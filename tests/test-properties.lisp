(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; Property and parity tests
;;;
;;; The rest of the suite asserts outputs for inputs a person thought of.
;;; This file asserts properties that must hold for inputs nobody thought
;;; of, and agreement between implementations that are supposed to be
;;; interchangeable. Those are the two things example-based tests
;;; structurally cannot cover, and both have already cost this codebase
;;; real bugs: a JSON round-trip that needed [["a",1]] to occur to
;;; someone, and a 1xx interim block that needed someone to imagine a CDN
;;; sending 103.
;;;
;;; Generation is seeded and the seeds are fixed, so a failure here is
;;; reproducible and a green run means the same thing twice. *FUZZ-SEEDS*
;;; doubles as the regression corpus: a seed that ever found a bug stays
;;; in the list with a comment naming what it found, so the input that
;;; broke us is replayed on every run forever after.
;;;
;;; A property test that fails prints the seed and the offending input.
;;; Reproduce a single case with:
;;;
;;;   (let ((*random-state* (sb-ext:seed-random-state SEED))) ...)
;;;
;;; ===========================================================================

(defparameter *fuzz-iterations* 200
  "Cases generated per property, per seed. Deliberately modest: with
   several seeds and a dozen properties this is already tens of
   thousands of parses per run, and the suite is expected to finish in
   seconds so that nobody is tempted to skip it. Raise it locally when
   hunting; the seeds are what make a raise reproducible.")

(defparameter *fuzz-seeds*
  '(1 2 3 20260807)
  "Seeds replayed on every run. Fixed rather than random so a green run
   is evidence about the same inputs each time — a suite that fuzzes
   from the clock is a suite whose failures nobody can reproduce.

   Add a seed here when it finds a bug, with a comment naming the bug.
   That turns the corpus into a regression list without needing a
   separate fixture file.")

;;; ---------------------------------------------------------------------------
;;; Generators
;;; ---------------------------------------------------------------------------

(defun gen-bytes (n)
  "N uniformly random bytes."
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v)
      (setf (aref v i) (random 256)))))

(defun gen-structured-request ()
  "Bytes that look enough like a request to get past the first few
   checks. Uniform random bytes almost never survive the request-line
   scan, so they exercise only the outermost guard; these carry real
   methods, spaces and CRLFs and reach the header parser, which is where
   the interesting surface is.

   Its cases spread across every verdict: most stop at the request line
   or the version, and the rest reach the header, method and path checks
   or are accepted whole. If a change to this generator pushes nearly
   everything into request-line, the property above is still passing but
   has stopped testing the parser."
  (let ((parts (list (nth (random 8) '("GET" "POST" "PUT" "DELETE"
                                       "HEAD" "OPTIONS" "PATCH" "PRI"))
                     " "
                     (nth (random 6) '("/" "/a" "*" "/x?y=1" "//" "/%zz"))
                     " "
                     (nth (random 5) '("HTTP/1.1" "HTTP/1.0" "HTTP/2.0"
                                       "HTTP/1." "hTTp/1.1"))
                     (string #\Return) (string #\Newline))))
    (dotimes (i (random 4))
      (declare (ignorable i))
      (push (format nil "~a:~a~a~a"
                    (nth (random 5) '("host" "x" "content-length"
                                      "transfer-encoding" ""))
                    (nth (random 5) '(" a" "" " 1" " chunked" " 99999999999"))
                    (string #\Return) (string #\Newline))
            (cdr (last parts))))
    (let ((s (apply #'concatenate 'string
                    (append parts (list (string #\Return) (string #\Newline))))))
      ;; Corrupt a byte sometimes, so the generator also produces things
      ;; that are structurally plausible and locally malformed.
      (let ((b (sb-ext:string-to-octets s :external-format :latin-1)))
        (when (and (plusp (length b)) (zerop (random 3)))
          (setf (aref b (random (length b))) (random 256)))
        b))))

(defun gen-json-value (&optional (depth 0))
  "A JSON value the serializer is expected to round-trip exactly.

   true is T, false is :FALSE, null is :NULL — false and null are
   keywords so they cannot be confused with NIL, which is the empty
   array. Getting this wrong in the generator produces a stream of
   \"unsupported type\" failures that look like serializer bugs.

   Floats are excluded on purpose: decimal printing and reading are not
   required to be inverse, so a float mismatch would be a property of
   the printer rather than of this parser, and the noise would bury real
   failures."
  (case (random (if (< depth 3) 8 5))
    (0 :null)
    (1 t)
    (2 :false)
    (3 (- (random 2000000) 1000000))
    (4 (gen-json-string))
    (5 (loop repeat (random 4) collect (gen-json-value (1+ depth))))
    (6 (web-skeleton:make-json-object (gen-json-pairs depth)))
    (7 nil)))                          ; the empty array

(defun gen-json-pairs (depth)
  "Object members with distinct keys. JSON-PARSE rejects a duplicate key
   rather than letting the last one win — a deliberate choice, since two
   readers disagreeing about which value survives is the JSON form of
   the parser disagreements this codebase refuses elsewhere. So a
   generator that emits duplicates is testing its own bug."
  (let ((seen (make-hash-table :test #'equal))
        (pairs nil))
    (dotimes (i (random 4) (nreverse pairs))
      (declare (ignorable i))
      (let ((key (gen-json-string)))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push (cons key (gen-json-value (1+ depth))) pairs))))))

(defparameter *json-string-alphabet*
  (concatenate 'string
               "ab\"\\/ {}[]:,0é☃𝄞"
               ;; Real control characters, built with CODE-CHAR rather
               ;; than written into the literal: "\\n" in a Lisp string
               ;; is a backslash followed by an n, which exercises the
               ;; backslash branch and never the newline one.
               (string #\Newline)      ; -> \n
               (string #\Return)       ; -> \r
               (string #\Tab)          ; -> \t
               (string (code-char 8))  ; -> \b
               (string (code-char 12)) ; -> \f
               (string (code-char 1))) ; -> , the generic branch
  "Characters a generated JSON string is drawn from. Covers every branch
   of the serializer's escape table plus a codepoint above the BMP, which
   has to survive as a surrogate pair and come back as one character.")

(defparameter *json-line-alphabet*
  (remove #\Return (remove #\Newline *json-string-alphabet*))
  "The same, minus the line terminators. PROP-CHUNKED-PARITY frames its
   payload as newline-terminated lines and compares a line-oriented
   decoder against a whole-buffer one: an embedded newline would split a
   line for the streaming decoder and not for the buffered one, and the
   resulting mismatch would look exactly like the chunked bug the
   property exists to find.")

(defun gen-json-string (&optional (alphabet *json-string-alphabet*))
  "A string exercising the escape paths: quotes, backslashes, control
   characters, non-ASCII, and characters above the BMP."
  (let ((out (make-string-output-stream)))
    (dotimes (i (random 12))
      (declare (ignorable i))
      (write-char (char alphabet (random (length alphabet))) out))
    (get-output-stream-string out)))

(defun gen-chunked (payload-lines)
  "Frame PAYLOAD-LINES (each already newline-terminated) into chunked
   transfer encoding, splitting at random boundaries so chunk edges fall
   in the middle of lines as often as not — the case where a line-
   oriented decoder and a whole-buffer decoder are most likely to
   disagree."
  (let* ((body (apply #'concatenate 'string payload-lines))
         (bytes (sb-ext:string-to-octets body :external-format :utf-8))
         (out (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0))
         (pos 0))
    (flet ((emit (string)
             (loop for c across (sb-ext:string-to-octets
                                 string :external-format :ascii)
                   do (vector-push-extend c out))))
      (loop while (< pos (length bytes))
            do (let* ((remaining (- (length bytes) pos))
                      (n (max 1 (min remaining (1+ (random 16))))))
                 (emit (format nil "~x~a~a" n (string #\Return) (string #\Newline)))
                 (loop for i from pos below (+ pos n)
                       do (vector-push-extend (aref bytes i) out))
                 (emit (format nil "~a~a" (string #\Return) (string #\Newline)))
                 (incf pos n)))
      (emit (format nil "0~a~a~a~a"
                    (string #\Return) (string #\Newline)
                    (string #\Return) (string #\Newline))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; ---------------------------------------------------------------------------
;;; Comparison helpers
;;; ---------------------------------------------------------------------------

(defun json-equal (a b)
  "Structural equality across the JSON value domain. EQUAL will not do:
   JSON-OBJECT is a struct, so two equal objects are not EQUAL."
  (cond
    ((and (web-skeleton:json-object-p a) (web-skeleton:json-object-p b))
     (let ((pa (web-skeleton:json-object-alist a))
           (pb (web-skeleton:json-object-alist b)))
       (and (= (length pa) (length pb))
            (every (lambda (x y)
                     (and (equal (car x) (car y))
                          (json-equal (cdr x) (cdr y))))
                   pa pb))))
    ((or (web-skeleton:json-object-p a) (web-skeleton:json-object-p b)) nil)
    ((and (consp a) (consp b))
     (and (= (length a) (length b)) (every #'json-equal a b)))
    ((or (consp a) (consp b)) nil)
    (t (equal a b))))

(defun fuzz-report (label seed input)
  "Print a failing case in a form that can be pasted back into a REPL."
  (format t "    seed ~a, input: ~s~%" seed input)
  label)

;;; ---------------------------------------------------------------------------
;;; Properties
;;; ---------------------------------------------------------------------------

(defun prop-request-parser-total (seed)
  "PARSE-REQUEST-BYTES either produces a request or signals
   HTTP-PARSE-ERROR. Any other condition escaping is a bug: the caller
   in MAIN.LISP answers HTTP-PARSE-ERROR with a status and everything
   else with a 500, so a stray TYPE-ERROR from a malformed request is
   the difference between a 400 and an internal error."
  (let ((bad nil))
    (dotimes (i *fuzz-iterations*)
      (let ((bytes (if (evenp i) (gen-bytes (random 64)) (gen-structured-request))))
        (handler-case
            (web-skeleton::parse-request-bytes bytes 0 (length bytes))
          (web-skeleton:http-parse-error () nil)
          (error (e)
            (unless bad
              (setf bad (type-of e))
              (fuzz-report nil seed bytes)
              (format t "    condition: ~a~%" e))))))
    bad))

(defun prop-json-roundtrip (seed)
  "JSON-PARSE of JSON-SERIALIZE is identity. This is the property that
   would have caught objects and arrays-of-pairs sharing a
   representation."
  (let ((bad nil))
    (dotimes (i *fuzz-iterations*)
      (declare (ignorable i))
      (let* ((v (gen-json-value))
             (text (handler-case (web-skeleton:json-serialize v)
                     (error (e) (setf bad :serialize-error)
                       (fuzz-report nil seed v)
                       (format t "    condition: ~a~%" e)
                       nil))))
        (when text
          (let ((back (handler-case (web-skeleton:json-parse text)
                        (error (e) (setf bad :parse-error)
                          (fuzz-report nil seed text)
                          (format t "    condition: ~a~%" e)
                          :failed))))
            (unless (or (eq back :failed) (json-equal v back) bad)
              (setf bad :mismatch)
              (fuzz-report nil seed text)
              (format t "    in:  ~s~%    out: ~s~%" v back))))))
    bad))

(defun prop-chunked-parity (seed)
  "DECODE-CHUNKED-BODY and STREAM-CHUNKED-LINES agree on every framing.
   The codebase repeatedly names parser disagreement as a smuggling
   primitive; on chunked framing these two are the pair that could
   disagree, so the property those comments promise is asserted here
   rather than assumed."
  (let ((bad nil))
    (dotimes (i *fuzz-iterations*)
      (declare (ignorable i))
      (let* ((lines (loop repeat (1+ (random 5))
                          collect (format nil "~a~a"
                                          (gen-json-string *json-line-alphabet*)
                                          #\Newline)))
             (framed (gen-chunked lines))
             (buffered (handler-case
                           (sb-ext:octets-to-string
                            (web-skeleton::decode-chunked-body
                             framed 0 (length framed))
                            :external-format :utf-8)
                         (error () :raised)))
             (streamed (handler-case
                           (let ((r (web-skeleton::make-stream-reader
                                     (make-mock-stream framed)))
                                 (acc nil))
                             (web-skeleton::stream-chunked-lines
                              r (lambda (line) (push line acc)))
                             (format nil "~{~a~%~}" (nreverse acc)))
                         (error () :raised))))
        (unless (equal buffered streamed)
          (unless bad
            (setf bad :disagreement)
            (fuzz-report nil seed framed)
            (format t "    buffered: ~s~%    streamed: ~s~%"
                    buffered streamed)))))
    bad))

(defun gen-chunk-payloads ()
  "Payloads for the encoder: two to five byte vectors, at least one of
   them guaranteed empty and placed wherever the generator likes rather
   than only ever last. An empty chunk is the case that matters — a
   terminator emitted for one truncates the message while producing bytes
   every reader accepts — so it is forced rather than left to chance.

   Bytes are printable ASCII: no CR, no LF. STREAM-CHUNKED-LINES treats
   CR, LF and CRLF as line terminators, so keeping them out of the
   payload makes chunk boundaries and line boundaries independent and
   leaves the framing as the thing under test. Content that *looks* like
   framing is covered deterministically in TEST-CHUNKED-ENCODER."
  (let* ((n (+ 2 (random 4)))
         (payloads (loop repeat n
                         collect (let* ((len (random 20))
                                        (v (make-array len :element-type
                                                       '(unsigned-byte 8))))
                                   (dotimes (j len v)
                                     (setf (aref v j) (+ 32 (random 95))))))))
    (setf (nth (random n) payloads)
          (make-array 0 :element-type '(unsigned-byte 8)))
    payloads))

(defun encode-chunk-stream (payloads)
  "Frame PAYLOADS and append the terminator, the way a producer would."
  (apply #'concatenate '(vector (unsigned-byte 8))
         (append (remove nil (mapcar #'web-skeleton::encode-chunk payloads))
                 (list (web-skeleton::chunked-terminator)))))

(defun prop-chunked-encoder-roundtrip (seed)
  "ENCODE-CHUNK's output is read identically by all three readers.

   The framework had three chunked readers and no writer, so nothing
   could generate well-formed chunked streams and the agreement between
   the readers could only be asserted in prose. CHUNKED-BODY-COMPLETE-P's
   docstring makes exactly such a claim — that it and DECODE-CHUNKED-BODY
   'agree on the completion point' — and until there was an encoder there
   was no way to check it.

   Three parts. The first two are acceptance; the third is the one worth
   having. Note the third is not 'the same offset': the predicate returns
   the *start* of the last-chunk header while the decoder stops *after*
   it, so the checkable claim is that they agree on the verdict at every
   prefix, and that the predicate's resume offset lands on the header."
  (let ((bad nil))
    (dotimes (i *fuzz-iterations*)
      (declare (ignorable i))
      (let* ((payloads (gen-chunk-payloads))
             (original (apply #'concatenate '(vector (unsigned-byte 8)) payloads))
             (framed (encode-chunk-stream payloads))
             (term-len (length (web-skeleton::chunked-terminator))))
        ;; 1. Buffered decode returns exactly what went in.
        (let ((decoded (handler-case
                           (web-skeleton::decode-chunked-body
                            framed 0 (length framed))
                         (error (e) (princ-to-string e)))))
          (unless (equalp decoded original)
            (unless bad
              (setf bad :buffered)
              (fuzz-report nil seed framed)
              (format t "    in:  ~s~%    out: ~s~%" original decoded))))
        ;; 2. The streaming reader sees the same content. With no CR or LF
        ;;    in the payload it accumulates one line and flushes it at the
        ;;    end — or none at all when every payload was empty.
        (let* ((acc nil)
               (streamed (handler-case
                             (progn
                               (web-skeleton::stream-chunked-lines
                                (web-skeleton::make-stream-reader
                                 (make-mock-stream framed))
                                (lambda (line) (push line acc)))
                               (nreverse acc))
                           (error (e) (princ-to-string e))))
               (expected (if (zerop (length original))
                             nil
                             (list (sb-ext:octets-to-string
                                    original :external-format :utf-8)))))
          (unless (equal streamed expected)
            (unless bad
              (setf bad :streamed)
              (fuzz-report nil seed framed)
              (format t "    expected: ~s~%    streamed: ~s~%"
                      expected streamed))))
        ;; 3. At every prefix, the predicate's verdict matches whether the
        ;;    decoder can actually finish. Too eager and the decoder runs
        ;;    on a truncated body; too reluctant and the read loop hangs.
        (loop for k from 0 to (length framed)
              do (let ((complete (and (web-skeleton::chunked-body-complete-p
                                       framed 0 k)
                                      t))
                       (decodes (handler-case
                                    (progn (web-skeleton::decode-chunked-body
                                            framed 0 k)
                                           t)
                                  (error () nil))))
                   (unless (eq complete decodes)
                     (unless bad
                       (setf bad :predicate)
                       (fuzz-report nil seed framed)
                       (format t "    prefix ~d: complete-p ~s, decodes ~s~%"
                               k complete decodes)))))
        ;; 3b. And the offset it hands back is the start of the last-chunk,
        ;;     not the end of the trailing CRLF — the "trailers are not
        ;;     consumed" half of the same docstring claim.
        (multiple-value-bind (complete resume)
            (web-skeleton::chunked-body-complete-p framed 0 (length framed))
          (unless (and complete
                       (= resume (- (length framed) term-len)))
            (unless bad
              (setf bad :resume)
              (fuzz-report nil seed framed)
              (format t "    complete ~s resume ~s want ~s~%"
                      complete resume (- (length framed) term-len)))))))
    bad))

(defparameter *size-line-bytes*
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code
       (concatenate 'string "aZ=\"x5;g" (list #\Space #\Tab #\Return #\Newline
                                              (code-char 0) (code-char #xE9))))
  "Bytes a generated chunk-size line is salted with: the separators the
   grammar names, the line terminators taken apart, a NUL, a byte above
   0x7F, and letters that are and are not hex digits.")

(defun gen-size-line ()
  "Bytes that may or may not be a chunk-size line, always followed by a
   CRLF so every reader finds an end. Built from the grammar's own parts
   — hex digits, whitespace, an extension — and then salted, so most
   cases sit one byte away from valid rather than nowhere near it."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (flet ((emit (b) (vector-push-extend b out))
           (salt () (aref *size-line-bytes* (random (length *size-line-bytes*)))))
      (when (zerop (random 8)) (emit (salt)))
      (dotimes (i (if (zerop (random 20)) 17 (random 5)))
        (declare (ignorable i))
        (emit (char-code (char "0123456789abcdefABCDEF" (random 22)))))
      (dotimes (i (random 3))
        (declare (ignorable i))
        (emit (if (zerop (random 2)) 32 9)))
      (when (zerop (random 2))
        (emit 59)
        (dotimes (i (random 7))
          (declare (ignorable i))
          (emit (salt))))
      (when (zerop (random 6))
        (let ((at (random (1+ (fill-pointer out)))))
          (emit 0)
          (replace out out :start1 (1+ at) :start2 at :end2 (1- (fill-pointer out)))
          (setf (aref out at) (salt))))
      (emit 13)
      (emit 10))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun prop-chunk-size-line-parity (seed)
  "Every chunked reader reads a size line the same way.

   PARSE-CHUNK-SIZE-BYTES reads the line for the walk and the buffered
   decoder; READER-READ-CRLF-LINE and PARSE-CHUNKED-SIZE-LINE read it for
   the blocking stream. Both are written to one grammar, and a line one
   accepts and the other refuses — or reads as a different size — is two
   readers framing the same bytes two ways. The line cap is part of the
   grammar, so each case runs under a small *MAX-HEADER-LINE-LENGTH* of
   its own and the cap is reached as often as not."
  (let ((bad nil))
    (dotimes (i *fuzz-iterations*)
      (declare (ignorable i))
      (let* ((line (gen-size-line))
             (web-skeleton::*max-header-line-length* (+ 4 (random 12)))
             (buffered (handler-case
                           (let ((size (web-skeleton::parse-chunk-size-bytes
                                        line 0 (length line))))
                             (or size :incomplete))
                         (web-skeleton::chunked-framing-error () :refused)))
             (streamed (handler-case
                           (let ((text (web-skeleton::reader-read-crlf-line
                                        (web-skeleton::make-stream-reader
                                         (make-mock-stream line)))))
                             (if text
                                 (web-skeleton::parse-chunked-size-line text)
                                 :eof))
                         (error () :refused))))
        (unless (eql buffered streamed)
          (unless bad
            (setf bad :disagreement)
            (fuzz-report nil seed line)
            (format t "    cap ~d: buffered ~s, streamed ~s~%"
                    web-skeleton::*max-header-line-length* buffered streamed)))))
    bad))

(defparameter *framing-bytes*
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code
       (concatenate 'string "01fgX;" (list #\Space #\Tab #\Return #\Newline
                                            (code-char 0))))
  "What a mutation writes into chunked framing: the bytes the framing is
   made of, and a few that are wrong in every position.")

(defun mutate-framing (bytes)
  "BYTES with one byte replaced, inserted or removed at a random place,
   the new byte drawn from *FRAMING-BYTES*."
  (mutate-bytes bytes *framing-bytes*))

(defun mutate-bytes (bytes alphabet)
  "BYTES with one byte replaced, inserted or removed at a random place,
   the new byte drawn from ALPHABET."
  (let* ((n (length bytes))
         (at (random (1+ n)))
         (b (aref alphabet (random (length alphabet)))))
    (coerce
     (ecase (if (zerop n) 1 (random 3))
       (0 (let ((v (copy-seq bytes)))
            (setf (aref v (min at (1- n))) b)
            v))
       (1 (concatenate '(vector (unsigned-byte 8))
                       (subseq bytes 0 at) (vector b) (subseq bytes at)))
       (2 (concatenate '(vector (unsigned-byte 8))
                       (subseq bytes 0 (min at (1- n)))
                       (subseq bytes (1+ (min at (1- n)))))))
     '(simple-array (unsigned-byte 8) (*)))))

(defun walk-verdict (buf start end resume collect)
  "Run CHUNKED-BODY-COMPLETE-P over BUF[START..END) from RESUME, handing
   each chunk to COLLECT. Returns (values VERDICT NEXT): :COMPLETE,
   :INCOMPLETE or :REFUSED, and where a later call resumes."
  (handler-case
      (multiple-value-bind (complete next)
          (web-skeleton::chunked-body-complete-p
           buf start end resume
           (lambda (b s e) (funcall collect (subseq b s e)) nil))
        (values (if complete :complete :incomplete) next))
    (web-skeleton::chunked-framing-error () (values :refused nil))))

(defun prop-chunk-walk-matches-decoder (seed)
  "CHUNKED-BODY-COMPLETE-P accepts exactly what DECODE-CHUNKED-BODY
   accepts, and ON-DATA hands over exactly what the decoder decodes.

   A consumer of ON-DATA — a relay forwarding a response as it arrives —
   is given each chunk once the walk has proved it, and the decoder never
   runs after that. So the walk is the only reader such a body meets, and
   a framing it waves through that the decoder refuses is a body a relay
   forwards and a buffered fetch answers 502.

   Valid bodies from the encoder are mutated by a byte, most of the time,
   so the corpus lives at the boundary of the grammar. Three claims:

     the whole buffer      walk complete exactly when the decoder succeeds,
                           and then ON-DATA's bytes are the decoded body; a
                           walk that refuses or waits faces a decoder that
                           refuses, waiting being truncation to a decoder
                           handed the whole body
     every split point     the walk run on a prefix and then on the whole
                           buffer from where it stopped hands over the same
                           bytes and reaches the same verdict as one pass
     refusal is final      a prefix the walk refuses is a body it refuses"
  (let ((bad nil))
    (flet ((report (kind framed fmt &rest args)
             (unless bad
               (setf bad kind)
               (fuzz-report nil seed framed)
               (apply #'format t fmt args))))
      (dotimes (i *fuzz-iterations*)
        (declare (ignorable i))
        (let* ((valid (encode-chunk-stream (gen-chunk-payloads)))
               (framed (if (zerop (random 4))
                           valid
                           (let ((m (mutate-framing valid)))
                             (if (zerop (random 3)) (mutate-framing m) m))))
               (n (length framed))
               (decoded (handler-case
                            (web-skeleton::decode-chunked-body framed 0 n)
                          (web-skeleton::chunked-framing-error () :refused)))
               (one-shot (make-array 0 :element-type '(unsigned-byte 8)
                                       :adjustable t :fill-pointer 0)))
          (flet ((into (acc) (lambda (v) (loop for b across v
                                               do (vector-push-extend b acc)))))
            (let ((verdict (walk-verdict framed 0 n 0 (into one-shot))))
              (cond
                ((eq verdict :complete)
                 (unless (and (not (eq decoded :refused))
                              (equalp (coerce one-shot '(vector (unsigned-byte 8)))
                                      (coerce decoded '(vector (unsigned-byte 8)))))
                   (report :whole framed "    walk complete, ~s handed over; decoder ~s~%"
                           one-shot decoded)))
                ((not (eq decoded :refused))
                 (report :whole framed "    walk ~s; decoder accepted ~s~%"
                         verdict decoded)))
              (loop for k from 0 to n
                    until bad
                    do (let ((split (make-array 0 :element-type '(unsigned-byte 8)
                                                  :adjustable t :fill-pointer 0)))
                         (multiple-value-bind (first next)
                             (walk-verdict framed 0 k 0 (into split))
                           (cond
                             ((eq first :refused)
                              (unless (eq verdict :refused)
                                (report :final framed
                                        "    prefix ~d refused, whole buffer ~s~%"
                                        k verdict)))
                             (t
                              (let ((second (walk-verdict framed 0 n next
                                                          (into split))))
                                (unless (and (eq second verdict)
                                             (equalp (coerce split '(vector (unsigned-byte 8)))
                                                     (coerce one-shot '(vector (unsigned-byte 8)))))
                                  (report :split framed
                                          "    split at ~d: ~s ~s, one pass ~s ~s~%"
                                          k second split verdict one-shot)))))))))))))
    bad))

(defun feed-in-pieces (framed cuts)
  "Feed FRAMED to CHUNKED-BODY-FEED as a receiving connection would: in
   the pieces CUTS marks, appended to a buffer from which every call's
   walked bytes are dropped. Returns (values VERDICT BYTES CALLS): the
   walk's :COMPLETE, :INCOMPLETE or :REFUSED once the input or the body
   ends, the data handed over, concatenated, and how many calls it took."
  (let ((buf (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (got (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (owed 0)
        (calls 0)
        (at 0))
    (dolist (cut (append cuts (list (length framed))))
      (loop for i from at below cut do (vector-push-extend (aref framed i) buf))
      (setf at cut)
      (incf calls)
      (handler-case
          (multiple-value-bind (complete next new-owed)
              (web-skeleton::chunked-body-feed
               buf 0 (fill-pointer buf) owed
               (lambda (b from to rest)
                 (declare (ignore rest))
                 (loop for i from from below to
                       do (vector-push-extend (aref b i) got))
                 nil))
            (when complete
              (return-from feed-in-pieces (values :complete got calls)))
            ;; Drop what the call walked, as the read path does.
            (replace buf buf :start2 next)
            (setf (fill-pointer buf) (- (fill-pointer buf) next)
                  owed new-owed))
        (web-skeleton::chunked-framing-error ()
          (return-from feed-in-pieces (values :refused got calls)))))
    (values :incomplete got calls)))

(defun prop-chunk-feed-matches-decoder (seed)
  "CHUNKED-BODY-FEED, which hands a received body over before its chunks
   have all landed, accepts exactly what DECODE-CHUNKED-BODY accepts and
   hands over exactly what it decodes — whatever pieces the body arrives
   in, and with each call's walked bytes dropped before the next.

   The feed is the reader a received request body meets and no decoder
   runs after it, so its verdict is where the request ends. The corpus is
   PROP-CHUNK-WALK-MATCHES-DECODER's — encoder output mutated by a byte or
   two — cut into random pieces, one byte at a time on some cases so a
   size line or a CRLF is split at every place it can be. The single-piece
   feed is checked against the decoder; every other cutting against the
   single piece."
  (let ((bad nil))
    (flet ((report (kind framed fmt &rest args)
             (unless bad
               (setf bad kind)
               (fuzz-report nil seed framed)
               (apply #'format t fmt args))))
      (dotimes (i *fuzz-iterations*)
        (declare (ignorable i))
        (let* ((valid (encode-chunk-stream (gen-chunk-payloads)))
               (framed (if (zerop (random 4))
                           valid
                           (let ((m (mutate-framing valid)))
                             (if (zerop (random 3)) (mutate-framing m) m))))
               (n (length framed))
               (decoded (handler-case
                            (web-skeleton::decode-chunked-body framed 0 n)
                          (web-skeleton::chunked-framing-error () :refused))))
          (multiple-value-bind (verdict got) (feed-in-pieces framed nil)
            (cond
              ((eq verdict :complete)
               (unless (and (not (eq decoded :refused)) (equalp got decoded))
                 (report :whole framed "    feed complete, ~s; decoder ~s~%"
                         got decoded)))
              ((not (eq decoded :refused))
               (report :whole framed "    feed ~s; decoder accepted ~s~%"
                       verdict decoded)))
            (let ((cuts (if (zerop (random 4))
                            (loop for k from 1 below n collect k)
                            (sort (remove-duplicates
                                   (loop repeat (random 6)
                                         collect (random (max 1 n))))
                                  #'<))))
              (multiple-value-bind (verdict2 got2) (feed-in-pieces framed cuts)
                (unless (and (eq verdict2 verdict) (equalp got2 got))
                  (report :pieces framed
                          "    cut at ~s: ~s ~s, one piece ~s ~s~%"
                          cuts verdict2 got2 verdict got))))))))
    bad))

(defun chunk-feed-in-partial-halts (framed cuts)
  "FEED-IN-PIECES, but with a consumer that takes each run only partway
   and halts there, as a received body's feed does when a pause falls
   inside a run: half of every run of two bytes or more, and the next
   call is handed the rest first. Returns (values VERDICT BYTES)."
  (let ((buf (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (got (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (owed 0)
        (at 0))
    (dolist (cut (append cuts (list (length framed))))
      (loop for i from at below cut do (vector-push-extend (aref framed i) buf))
      (setf at cut)
      ;; Called until a call makes no progress, since every call halts.
      (loop
        (let ((progress nil))
          (handler-case
              (multiple-value-bind (complete next new-owed)
                  (web-skeleton::chunked-body-feed
                   buf 0 (fill-pointer buf) owed
                   (lambda (b from to rest)
                     (declare (ignore rest))
                     (let ((upto (if (>= (- to from) 2) (+ from (floor (- to from) 2)) to)))
                       (loop for i from from below upto
                             do (vector-push-extend (aref b i) got))
                       (setf progress t)
                       (values :halt upto))))
                (when complete
                  (return-from chunk-feed-in-partial-halts (values :complete got)))
                (when (plusp next) (setf progress t))
                (replace buf buf :start2 next)
                (setf (fill-pointer buf) (- (fill-pointer buf) next)
                      owed new-owed))
            (web-skeleton::chunked-framing-error ()
              (return-from chunk-feed-in-partial-halts (values :refused got))))
          (unless progress (return)))))
    (values :incomplete got)))

(defun prop-chunk-feed-halts-short-of-a-run (seed)
  "A consumer of CHUNKED-BODY-FEED that halts partway into a run, with the
   UPTO it took the run to, is handed the rest of that run first by the
   next call, and nothing twice: the bytes it takes, in any pieces, are
   the decoded body, and its verdict is the one-pass feed's.

   That is what a received body's pause inside a run rests on, a
   multipart body paused at a part's end in the middle of a read."
  (let ((bad nil))
    (dotimes (i *fuzz-iterations*)
      (declare (ignorable i))
      (let* ((valid (encode-chunk-stream (gen-chunk-payloads)))
             (framed (if (zerop (random 4)) (mutate-framing valid) valid))
             (n (length framed))
             (cuts (sort (remove-duplicates
                          (loop repeat (random 6) collect (random (max 1 n))))
                         #'<)))
        (multiple-value-bind (verdict got) (feed-in-pieces framed nil)
          (multiple-value-bind (verdict2 got2) (chunk-feed-in-partial-halts framed cuts)
            (unless (and (eq verdict verdict2) (equalp got got2))
              (unless bad
                (setf bad :partial)
                (fuzz-report nil seed framed)
                (format t "    cut at ~s, halting halfway: ~s ~s; whole runs ~s ~s~%"
                        cuts verdict2 got2 verdict got)))))))
    bad))

;;; ---------------------------------------------------------------------------
;;; multipart/form-data
;;; ---------------------------------------------------------------------------

(defparameter *boundary-alphabet*
  "abcXYZ019'()+_,-./:=? "
  "Characters a generated boundary is drawn from: all of RFC 2046's
   punctuation, a few letters and digits, and the space it allows inside.")

(defun gen-boundary ()
  "A boundary RFC 2046 allows. Short ones half the time, since a boundary
   of a character or two is the one a part's content collides with by
   chance, and the collisions are the parser's hard cases."
  (let* ((n (if (zerop (random 2)) (1+ (random 3)) (+ 4 (random 67))))
         (s (make-string n)))
    (dotimes (i n)
      (setf (char s i) (char *boundary-alphabet* (random (length *boundary-alphabet*)))))
    (when (char= (char s (1- n)) #\Space)
      (setf (char s (1- n)) #\z))
    s))

(defun ascii-octets (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun octets-contain-p (bytes needle &key (start 0))
  (and (search needle bytes :start2 start) t))

(defun gen-part-content (boundary)
  "Content for a part that a valid body can carry: built from the pieces a
   delimiter is made of — CRLF, dashes, prefixes of the boundary — and
   random bytes, then refused and drawn again if it holds a line that
   begins with the boundary, which RFC 2046 forbids content to."
  (let* ((dash-boundary (ascii-octets (format nil "--~a" boundary)))
         (delimiter (concatenate '(vector (unsigned-byte 8)) #(13 10) dash-boundary)))
    (loop repeat 20
          do (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                                      :adjustable t :fill-pointer 0)))
               (dotimes (i (random 12))
                 (declare (ignorable i))
                 (let ((piece (ecase (random 6)
                                (0 #(13 10))
                                (1 #(45 45))
                                (2 (subseq delimiter 0 (random (length delimiter))))
                                (3 (subseq dash-boundary 0 (random (length dash-boundary))))
                                (4 (vector (random 256)))
                                (5 (gen-bytes (random 40))))))
                   (loop for b across piece do (vector-push-extend b out))))
               (let ((v (coerce out '(simple-array (unsigned-byte 8) (*)))))
                 (unless (or (octets-contain-p v delimiter)
                             (and (>= (length v) (length dash-boundary))
                                  (equalp (subseq v 0 (length dash-boundary)) dash-boundary)))
                   (return-from gen-part-content v)))))
    (ascii-octets "plain")))

(defparameter *form-names*
  '("title" "file" "a b" "C:\\path\\x.txt" "café" "" "x;y" "photos[]" "tab	here")
  "Names and filenames a generated part may carry: the backslash a Windows
   path has, a space, a ;, a tab and an empty name, all of which a quoted
   value holds as sent.")

(defun gen-param-value (value)
  "VALUE as a Content-Disposition parameter value: quoted, or bare when it
   is a token and the coin says so."
  (if (and (plusp (length value))
           (every #'web-skeleton::%token-char-p value)
           (zerop (random 2)))
      value
      (format nil "\"~a\"" value)))

(defun gen-multipart-form ()
  "A valid multipart/form-data body and what it says. Returns
   (values BOUNDARY PARTS BODY): PARTS a list of (NAME FILENAME
   CONTENT-TYPE CONTENT), in order.

   Every way the format lets the same parts be written turns up: a
   preamble, transport padding after a boundary, header names in any case,
   whitespace around a parameter's =, a bare or a quoted value, an extra
   header, a Content-Transfer-Encoding that leaves the content alone, an
   empty part written without a line of its own, and an epilogue."
  (let* ((boundary (gen-boundary))
         (crlf (format nil "~c~c" #\Return #\Newline))
         (parts (loop repeat (random 5)
                      collect (list (nth (random (length *form-names*)) *form-names*)
                                    (and (zerop (random 2))
                                         (nth (random (length *form-names*)) *form-names*))
                                    (and (zerop (random 3))
                                         (nth (random 3) '("text/plain" "image/png"
                                                           "application/octet-stream; x=\"y\"")))
                                    (if (zerop (random 4))
                                        (make-array 0 :element-type '(unsigned-byte 8))
                                        (gen-part-content boundary)))))
         (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((emit (thing)
             (loop for b across (if (stringp thing) (ascii-octets thing) thing)
                   do (vector-push-extend b out)))
           (padding () (subseq "  	 " 0 (if (zerop (random 3)) (random 4) 0))))
      (when (zerop (random 4))
        ;; A preamble, held to what content is held to, and the CRLF
        ;; before the first boundary.
        (emit (gen-part-content boundary))
        (emit crlf))
      (emit (format nil "--~a" boundary))
      (dolist (part parts)
        (destructuring-bind (name filename content-type content) part
          (emit (format nil "~a~a" (padding) crlf))
          (emit (format nil "~a: form-data;~a~a~a=~a~a~a"
                        (nth (random 3) '("Content-Disposition" "content-disposition"
                                          "CONTENT-DISPOSITION"))
                        (if (zerop (random 3)) "" " ")
                        "name"
                        (if (zerop (random 5)) " " "")
                        (gen-param-value name)
                        (if filename (format nil "; filename=~a" (gen-param-value filename)) "")
                        crlf))
          (when content-type
            (emit (format nil "Content-Type: ~a~a" content-type crlf)))
          (when (zerop (random 6))
            (emit (format nil "Content-Transfer-Encoding: ~a~a"
                          (nth (random 3) '("binary" "8bit" "7BIT")) crlf)))
          (when (zerop (random 6))
            (emit (format nil "X-Extra: ignored~a" crlf)))
          (if (and (zerop (length content)) (zerop (random 2)))
              ;; No line of content: the blank line is the delimiter's CRLF.
              (emit crlf)
              (progn (emit crlf) (emit content) (emit crlf)))
          (emit (format nil "--~a" boundary))))
      (emit "--")
      (emit (padding))
      (case (random 3)
        (0)
        (1 (emit crlf))
        (2 (emit crlf) (emit (gen-bytes (random 30))))))
    (values boundary
            (mapcar (lambda (p) (list (first p) (second p) (third p)
                                      (coerce (fourth p) 'list)))
                    parts)
            (coerce out '(simple-array (unsigned-byte 8) (*))))))

(defun multipart-in-pieces (body boundary cuts)
  "Parse BODY through MULTIPART-STEP in the pieces CUTS marks, as a body
   received a read at a time is parsed: each piece appended to what the
   last step left, and what a step read dropped. Returns (:PARTS list) of
   (NAME FILENAME CONTENT-TYPE CONTENT), or (:REFUSED status)."
  (handler-case
      (let ((parser (web-skeleton::make-multipart-parser
                     (ascii-octets boundary) :max-parts 1000))
            (buf (make-array 0 :element-type '(unsigned-byte 8)))
            (at 0)
            (parts '())
            (content nil))
        (dolist (cut (append cuts (list (length body))))
          (setf buf (concatenate '(simple-array (unsigned-byte 8) (*))
                                 buf (subseq body at cut))
                at cut)
          (let ((pos 0))
            (loop
              (multiple-value-bind (event next source from to)
                  (web-skeleton::multipart-step parser buf pos (length buf))
                (setf pos next)
                (ecase event
                  (:more (return))
                  (:part (setf content '()))
                  (:data (loop for i from from below to do (push (aref source i) content)))
                  (:part-end
                   (let ((part (web-skeleton::multipart-parser-part parser)))
                     (push (list (form-part-name part) (form-part-filename part)
                                 (form-part-content-type part) (reverse content))
                           parts))))))
            (setf buf (subseq buf pos))))
        (if (web-skeleton::multipart-parser-done-p parser)
            (list :parts (nreverse parts))
            (list :refused 400)))
    (http-parse-error (e) (list :refused (http-parse-error-status e)))
    (error (e) (list :escaped (type-of e)))))

(defun multipart-whole (body boundary)
  "PARSE-MULTIPART's reading of BODY, in MULTIPART-IN-PIECES' terms."
  (handler-case
      (list :parts
            (mapcar (lambda (p) (list (form-part-name p) (form-part-filename p)
                                      (form-part-content-type p)
                                      (coerce (form-part-body p) 'list)))
                    (parse-multipart
                     (web-skeleton::make-http-request
                      :method :post :path "/"
                      :headers (list (cons "content-type"
                                           (format nil "multipart/form-data; boundary=\"~a\""
                                                   boundary)))
                      :body body))))
    (http-parse-error (e) (list :refused (http-parse-error-status e)))))

(defparameter *multipart-mutation-bytes*
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code
       (format nil "~c~c-:;=\" a~c" #\Return #\Newline (code-char 0)))
  "What a mutation writes into a multipart body: the bytes its framing is
   made of, and a NUL.")

(defun prop-multipart-reads-alike-in-any-pieces (seed)
  "MULTIPART-STEP reads a multipart/form-data body the same however it
   arrives, and reads a valid one as what was sent.

     what was sent    a generated body parses to exactly its parts —
                      names, filenames, types and content, in order
     any pieces       the body cut anywhere, or a byte at a time, parses
                      as the whole body does
     a broken body    with a byte or two changed, inserted or removed, the
                      body is refused or read alike whole and in pieces,
                      and nothing but HTTP-PARSE-ERROR escapes

   A received form is parsed a read at a time, so where the reads fall
   is the client's choice. A body that reads one way in one piece and
   another way in two is a body whose fields depend on the network."
  (let ((bad nil))
    (flet ((report (kind body fmt &rest args)
             (unless bad
               (setf bad kind)
               (fuzz-report nil seed body)
               (apply #'format t fmt args))))
      (dotimes (i *fuzz-iterations*)
        (declare (ignorable i))
        (multiple-value-bind (boundary expected body) (gen-multipart-form)
          (let* ((broken (if (zerop (random 3))
                             (let ((m (mutate-bytes body *multipart-mutation-bytes*)))
                               (if (zerop (random 3)) (mutate-bytes m *multipart-mutation-bytes*) m))
                             body))
                 (n (length broken))
                 (whole (handler-case (multipart-whole broken boundary)
                          (error (e) (list :escaped (type-of e))))))
            (when (eq broken body)
              (unless (equal whole (list :parts expected))
                (report :sent body "    boundary ~s: read ~s, sent ~s~%"
                        boundary whole expected)))
            (when (eq (first whole) :escaped)
              (report :escaped broken "    boundary ~s: ~s escaped~%" boundary whole))
            (let ((cutting (case (random 8)
                             (0 (loop for k from 1 below n collect k))
                             (1 (and (< n 600) :every))
                             (t (sort (remove-duplicates
                                       (loop repeat (random 6) collect (random (max 1 n))))
                                      #'<)))))
              (if (eq cutting :every)
                  (loop for k from 0 to n
                        for got = (multipart-in-pieces broken boundary (list k))
                        unless (equal got whole)
                          do (report :split broken "    boundary ~s, split at ~d: ~s; whole ~s~%"
                                     boundary k got whole)
                             (return))
                  (let ((got (multipart-in-pieces broken boundary cutting)))
                    (unless (equal got whole)
                      (report :pieces broken "    boundary ~s, cut at ~s: ~s; whole ~s~%"
                              boundary cutting got whole)))))))))
    bad))

(defun drive-request-incrementally (bytes step)
  "Feed BYTES to a fresh connection STEP bytes per wake-up.
   Returns (values CALLS VERDICT CONN) — how many CONNECTION-ON-READ calls
   it took to stop being :CONTINUE, what that call answered, and the
   connection.

   The read-fn hands over at most one piece per wake-up and then answers
   :AGAIN, and the arming flag is what makes that true.
   CONNECTION-READ-AVAILABLE drains until :AGAIN, so a read-fn that simply
   kept returning bytes would deliver the whole request inside the first
   call and test nothing — which is exactly the shape every other inbound
   test in the suite has."
  (let* ((total (length bytes))
         (pos (list 0))
         (armed (list nil))
         (conn (web-skeleton::make-connection
                :fd -1
                :read-fn (lambda (buffer start max-bytes)
                           (if (or (not (car armed)) (>= (car pos) total))
                               :again
                               (let ((n (min step max-bytes
                                             (- total (car pos)))))
                                 (setf (car armed) nil)
                                 (replace buffer bytes
                                          :start1 start
                                          :start2 (car pos)
                                          :end2 (+ (car pos) n))
                                 (incf (car pos) n)
                                 n))))))
    (loop for calls from 1 to (+ total 2)
          do (setf (car armed) t)
             (let ((v (handler-case (web-skeleton::connection-on-read conn)
                        (error (e) (princ-to-string e)))))
               (when (or (not (eq v :continue)) (>= (car pos) total))
                 (return (values calls v conn))))
          finally (return (values (+ total 2) :runaway conn)))))

(defun prop-chunked-inbound-parity (seed)
  "The generated corpus, driven through the *request* path.

   The instrument parity needs. The inbound acceptance set has to be
   provably identical to the outbound one, and a disagreement is only ever
   visible when both sides read the same input — a hand-written table of
   cases cannot show it, because the two sides would be given different
   tables. ENCODE-CHUNK-STREAM produces the bytes,
   PROP-CHUNKED-ENCODER-ROUNDTRIP drives them through the three outbound
   readers, and this drives the same bytes through CONNECTION-ON-READ:
   the reader that decides where a *request* ends and what a handler
   receives.

   Driven twice: once with the whole request in the buffer, and once one
   byte per wake-up, which is what makes the parity literal — the outbound
   twin sweeps every prefix, so until the second drive existed `the same
   corpus, both directions` was true of the bytes and not of the driving.

   Two claims per case, and the first is the one that could not be made
   before. The request must complete on exactly these bytes — not one
   read later, which is what a boundary that stopped at the zero-size
   chunk header instead of past the trailer terminator would do — and the
   body a handler would receive must be the bytes that went in.

   An empty corpus is a real case: every payload can be empty, which
   frames as a bare terminator and decodes to zero bytes. The inbound
   path returns an empty vector there rather than NIL, which is why this
   compares against ORIGINAL rather than testing for a body at all."
  (let ((bad nil))
    (dotimes (i *fuzz-iterations*)
      (declare (ignorable i))
      (let* ((payloads (gen-chunk-payloads))
             (original (apply #'concatenate '(vector (unsigned-byte 8))
                              payloads))
             (framed (encode-chunk-stream payloads))
             (head (sb-ext:string-to-octets
                    (concatenate 'string
                                 "POST /u HTTP/1.1" *crlf*
                                 "Host: x" *crlf*
                                 "Transfer-Encoding: chunked" *crlf* *crlf*)
                    :external-format :ascii))
             (bytes (concatenate '(vector (unsigned-byte 8)) head framed))
             (conn (web-skeleton::make-connection
                    :fd -1
                    :read-fn (lambda (buffer start max-bytes)
                               (declare (ignore buffer start max-bytes))
                               :again))))
        (setf (web-skeleton::connection-read-buf conn) bytes
              (web-skeleton::connection-read-pos conn) (length bytes))
        (let ((verdict (handler-case
                           (web-skeleton::connection-on-read conn)
                         (error (e) (princ-to-string e)))))
          (cond
            ((not (eq verdict :dispatch))
             (unless bad
               (setf bad :verdict)
               (fuzz-report nil seed framed)
               (format t "    verdict ~s, want :DISPATCH~%" verdict)))
            (t
             ;; The boundary the framing produced must be the whole
             ;; request and nothing more — asserted here rather than only
             ;; through the body, because a boundary short by the trailer
             ;; terminator still decodes the right body while leaving two
             ;; bytes for the next parse to read as a request line.
             (let ((end (web-skeleton::connection-request-end conn)))
               (unless (= end (length bytes))
                 (unless bad
                   (setf bad :boundary)
                   (fuzz-report nil seed framed)
                   (format t "    request-end ~d, want ~d~%"
                           end (length bytes)))))
             (let ((body (handler-case
                             (web-skeleton:http-request-body
                              (web-skeleton::connection-parse-request conn))
                           (error (e) (princ-to-string e)))))
               (unless (equalp body original)
                 (unless bad
                   (setf bad :body)
                   (fuzz-report nil seed framed)
                   (format t "    in:  ~s~%    out: ~s~%" original body)))))))
        ;; 2. The same request delivered one byte per wake-up. This is the
        ;;    half that makes the parity claim literal — the outbound twin
        ;;    sweeps every prefix, and until this existed the inbound side
        ;;    was driven one-shot, so "the same corpus through both
        ;;    directions" was true of the bytes and not of the driving.
        ;;
        ;;    It is also the only place the across-reads design is
        ;;    exercised at all. DATA-BYTES is per-call rather than
        ;;    cumulative so that CONNECTION-BODY-COMPLETE-P can accumulate
        ;;    without double-counting, and CHUNK-SCAN-POS exists so the
        ;;    walk resumes with progress; a request that arrives in one
        ;;    read uses neither. If the accumulation ever broke, the
        ;;    symptom would be a 413 on a legal upload that happened to
        ;;    arrive in more than one read — which is every large upload,
        ;;    and was no test in this suite.
        (multiple-value-bind (calls verdict conn)
            (drive-request-incrementally bytes 1)
          (cond
            ;; Not one byte early and not one late: the dispatch has to
            ;; land on the wake-up that delivers the final byte. Early is
            ;; a boundary short of the trailer terminator; late is a
            ;; predicate that wants a byte the client will not send.
            ((/= calls (length bytes))
             (unless bad
               (setf bad :prefix)
               (fuzz-report nil seed framed)
               (format t "    dispatched after ~d of ~d bytes (~s)~%"
                       calls (length bytes) verdict)))
            ((not (eq verdict :dispatch))
             (unless bad
               (setf bad :incremental-verdict)
               (fuzz-report nil seed framed)
               (format t "    verdict ~s, want :DISPATCH~%" verdict)))
            (t
             (let ((end (web-skeleton::connection-request-end conn))
                   (decoded (web-skeleton::connection-body-decoded conn)))
               (unless (= end (length bytes))
                 (unless bad
                   (setf bad :incremental-boundary)
                   (fuzz-report nil seed framed)
                   (format t "    request-end ~d, want ~d~%"
                           end (length bytes))))
               ;; The accumulator, summed across as many calls as there
               ;; are bytes, must equal the decoded length exactly — one
               ;; chunk counted twice is a 413 waiting for a big enough
               ;; upload.
               (unless (= decoded (length original))
                 (unless bad
                   (setf bad :accumulator)
                   (fuzz-report nil seed framed)
                   (format t "    body-decoded ~d, want ~d~%"
                           decoded (length original)))))
             (let ((body (handler-case
                             (web-skeleton:http-request-body
                              (web-skeleton::connection-parse-request conn))
                           (error (e) (princ-to-string e)))))
               (unless (equalp body original)
                 (unless bad
                   (setf bad :incremental-body)
                   (fuzz-report nil seed framed)
                   (format t "    in:  ~s~%    out: ~s~%" original body)))))))
        ))
    bad))

(defun prop-byte-range-in-bounds (seed)
  "PARSE-BYTE-RANGE never returns a range outside [0, TOTAL). A range
   that escapes its resource becomes a SUBSEQ on the pre-built response
   buffer, so an out-of-bounds answer here is an out-of-bounds read
   there."
  (let ((bad nil))
    (dotimes (i *fuzz-iterations*)
      (declare (ignorable i))
      (let* ((total (random 5000))
             (spec (format nil "bytes=~a-~a"
                           (if (zerop (random 4)) ""
                               (- (random 12000) 2000))
                           (if (zerop (random 4)) ""
                               (- (random 12000) 2000)))))
        (multiple-value-bind (first last)
            (handler-case (web-skeleton::parse-byte-range spec total)
              (error () (values :raised nil)))
          (cond
            ((eq first :raised)
             (unless bad
               (setf bad :raised)
               (fuzz-report nil seed (list spec total))))
            ((integerp first)
             (unless (and (<= 0 first) (< first total)
                          (integerp last) (<= first last) (< last total))
               (unless bad
                 (setf bad :out-of-bounds)
                 (fuzz-report nil seed (list spec total))
                 (format t "    returned: ~s ~s (total ~d)~%"
                         first last total))))))))
    bad))

(defun prop-base64-roundtrip (seed)
  "BASE64-DECODE of BASE64-ENCODE is identity, and the URL-safe pair
   likewise. Encoders that produce something their own decoder rejects
   are a class of bug that only shows up on inputs of an awkward length,
   which is exactly what generation covers and hand-written vectors
   miss."
  (let ((bad nil))
    (dotimes (i *fuzz-iterations*)
      (declare (ignorable i))
      (let* ((data (gen-bytes (random 200))))
        (handler-case
            (progn
              (unless (equalp data (web-skeleton:base64-decode
                                    (web-skeleton:base64-encode data)))
                (unless bad
                  (setf bad :standard-mismatch)
                  (fuzz-report nil seed data)))
              (unless (equalp data (web-skeleton:base64url-decode
                                    (web-skeleton:base64url-encode data)))
                (unless bad
                  (setf bad :url-mismatch)
                  (fuzz-report nil seed data))))
          (error (e)
            (unless bad
              (setf bad :raised)
              (fuzz-report nil seed data)
              (format t "    condition: ~a~%" e))))))
    bad))

(defun prop-base64-canonical-only (seed)
  "Every accepted encoding is the only spelling of the bytes it yields.
   Mutating the final character of an encoding whose last group is short
   produces a different string that decoded to the same bytes before the
   canonical check existed; it must now be refused."
  (let ((bad nil))
    (dotimes (i *fuzz-iterations*)
      (declare (ignorable i))
      ;; Lengths where the final group is short — 3n is exact and has no
      ;; spare bits to mutate.
      (let* ((n (let ((k (1+ (random 60)))) (if (zerop (mod k 3)) (1+ k) k)))
             (data (gen-bytes n))
             (enc (web-skeleton:base64url-encode data))
             (last-char (char enc (1- (length enc))))
             (idx (position last-char web-skeleton::*base64url-alphabet*))
             ;; A twin sharing the emitted high bits but differing in the
             ;; padding bits below them. LOGXOR rather than LOGIOR: for a
             ;; canonical encoding IDX is always even — a 2-char final
             ;; group puts the last character at a multiple of 16, a
             ;; 3-char one at a multiple of 4, because in both cases the
             ;; low bits of that character are the zeroed pad bits. So
             ;; LOGIOR would also flip bit 0 on every input, but only by
             ;; a property of the encoder, which is the thing under test:
             ;; an encoder bug that produced an odd index would silently
             ;; turn this property off exactly when it started mattering.
             ;; LOGXOR flips bit 0 whatever IDX is.
             (twin (char web-skeleton::*base64url-alphabet* (logxor idx 1))))
        (unless (char= twin last-char)
          (let ((mutated (concatenate 'string
                                      (subseq enc 0 (1- (length enc)))
                                      (string twin))))
            (multiple-value-bind (ok)
                (handler-case (progn (web-skeleton:base64url-decode mutated) t)
                  (error () nil))
              (when (and ok
                         (equalp data (web-skeleton:base64url-decode mutated)))
                (unless bad
                  (setf bad :non-canonical-accepted)
                  (fuzz-report nil seed (list enc mutated)))))))))
    bad))

;;; ---------------------------------------------------------------------------
;;; Cross-transport parity
;;;
;;; "One definition of done, shared by the plain and TLS paths" is the
;;; stated design principle behind OUTBOUND-RESPONSE-COMPLETE-P. The
;;; buffered and streaming paths are both drivable from a byte vector, so
;;; the same fixture goes through both and the results must match.
;;;
;;; The HTTPS path needs no arm of its own: the TLS transport calls
;;; STREAM-RESPONSE-LINES with a byte source of its own, so there is no
;;; second reader of these bytes to disagree with — a stronger result
;;; than an arm that agreed.
;;; ---------------------------------------------------------------------------

(defun parity-fixture (interims body)
  "An upstream response with INTERIMS 1xx blocks in front of a final 200."
  (let ((crlf (coerce (list #\Return #\Linefeed) 'string)))
    (with-output-to-string (s)
      (dolist (code interims)
        (format s "HTTP/1.1 ~d ~a~a~a" code
                (if (= code 103) "Early Hints" "Continue") crlf crlf))
      (format s "HTTP/1.1 200 OK~aContent-Type: application/x-ndjson~a~a~a"
              crlf crlf crlf body))))

(defun read-fn-stream-lines (raw &key (method :GET))
  "Drive STREAM-RESPONSE-LINES over RAW through a READ-FN byte source.
   Returns (STATUS LINES).

   The TLS transport uses this very reader, so what is worth exercising
   here is the byte source. READ-SEQUENCE fills a whole buffer; a READ-FN
   hands over buffer-sized bites, so a record boundary can fall anywhere
   and the cross-read state (PREV-CR, the chunk phase flags) is exercised
   rather than bypassed.

   It calls the reader directly rather than resolving a symbol at run
   time, so it never answers :SKIPPED: a FIND-SYMBOL miss would turn
   these assertions into SKIPs, and the suite would stay green while
   losing them."
  (let ((pos 0)
        (lines nil))
    ;; A divergence in the reader shows up as a raise as often as a
    ;; wrong answer — a chunk-accounting slip reaches the end of the
    ;; bytes without the zero-size terminator and signals. Caught so
    ;; that lands as a failed CHECK carrying the message, rather than
    ;; a backtrace that ends the suite before the other arms run.
    (handler-case
        (list (web-skeleton::stream-response-lines
               nil
               (lambda (line) (push line lines))
               :method method
               :read-fn
               (lambda (buf len)
                 (if (>= pos (length raw))
                     :eof
                     (let ((n (min len (- (length raw) pos))))
                       (replace buf raw :start2 pos :end2 (+ pos n))
                       (incf pos n)
                       n))))
              (nreverse lines))
      (error (e) (list :raised (princ-to-string e))))))

(defun test-transport-parity ()
  "Same bytes through the buffered and streaming readers; same verdict."
  (dolist (interims '(() (100) (103) (103 103 100)))
    (let* ((body (format nil "{\"a\":1}~%{\"b\":2}~%"))
           (raw (ascii-bytes (parity-fixture interims body)))
           ;; Buffered: the completion predicate plus the status scan
           ;; the async path uses.
           (buffered-status
             (let ((start (web-skeleton::skip-interim-responses
                           raw 0 (length raw))))
               (web-skeleton::parse-response-status raw start (length raw))))
           ;; Streaming: the reader the http-fetch-stream path uses.
           (streamed-lines nil)
           (streamed-status
             (let ((stream (make-mock-stream raw)))
               (unwind-protect
                    (web-skeleton::stream-response-lines
                     stream (lambda (line) (push line streamed-lines)))
                 (close stream)))))
      (check (format nil "parity: ~d interim block(s), buffered status"
                     (length interims))
             buffered-status 200)
      (check (format nil "parity: ~d interim block(s), streamed status"
                     (length interims))
             streamed-status 200)
      (check (format nil "parity: ~d interim block(s), both agree"
                     (length interims))
             (eql buffered-status streamed-status) t)
      (check (format nil "parity: ~d interim block(s), body survives"
                     (length interims))
             (nreverse streamed-lines)
             (list "{\"a\":1}" "{\"b\":2}"))
      ;; The same reader over the other byte source. What it buys is
      ;; fill granularity — buffer-sized bites put a record boundary
      ;; anywhere, where a stream hands over the lot.
      (check (format nil "parity: ~d interim block(s), read-fn source agrees"
                     (length interims))
             (read-fn-stream-lines raw)
             (list 200 (list "{\"a\":1}" "{\"b\":2}")))))
  ;; Chunked framing through the read-fn source. Chunk framing is where a
  ;; boundary landing mid-token does the most damage — a chunk-size line
  ;; or a CRLF pair split across two fills — and where a stream-driven
  ;; fixture, which never splits anything, sees the least.
  (let* ((crlf (coerce (list #\Return #\Linefeed) 'string))
         (lines (list (format nil "{\"a\":1}~%") (format nil "{\"b\":2}~%")))
         ;; GEN-CHUNKED frames the payload and splits it at random chunk
         ;; boundaries, which is exactly the case a line-oriented decoder
         ;; is most likely to get wrong — so it is seeded rather than
         ;; hand-built, both for determinism and because hand-computing
         ;; chunk-size hex is how a fixture ends up testing the fixture.
         (body (let ((*random-state* (sb-ext:seed-random-state 20260814)))
                 (gen-chunked lines)))
         (raw (concatenate '(simple-array (unsigned-byte 8) (*))
                           (ascii-bytes
                            (format nil "HTTP/1.1 200 OK~a~
                                         Transfer-Encoding: chunked~a~a"
                                    crlf crlf crlf))
                           body))
         (via-read-fn (read-fn-stream-lines raw)))
    (check "parity: chunked body, read-fn source agrees with the stream"
           via-read-fn
           (let ((lines nil)
                 (stream (make-mock-stream raw)))
             (unwind-protect
                  (list (web-skeleton::stream-response-lines
                         stream (lambda (l) (push l lines)))
                        (nreverse lines))
               (close stream)))))
  ;; And the cap is the same number on both paths — one variable read by
  ;; every transport was the point of *max-interim-responses*.
  (check "parity: interim cap is one shared value"
         (integerp web-skeleton:*max-interim-responses*) t))

;;; ---------------------------------------------------------------------------
;;; Suite
;;; ---------------------------------------------------------------------------

(defun test-properties ()
  (format t "~%Properties and parity~%")
  (setf *tests-passed* 0 *tests-failed* 0 *failed-names* nil)
  (dolist (spec (list (cons "request parser is total" #'prop-request-parser-total)
                      (cons "json round-trips" #'prop-json-roundtrip)
                      (cons "chunked decoders agree" #'prop-chunked-parity)
                      (cons "chunked encoder round-trips"
                            #'prop-chunked-encoder-roundtrip)
                      (cons "chunked corpus through the request path"
                            #'prop-chunked-inbound-parity)
                      (cons "chunk-size lines read alike"
                            #'prop-chunk-size-line-parity)
                      (cons "chunk walk accepts what the decoder accepts"
                            #'prop-chunk-walk-matches-decoder)
                      (cons "chunk feed, in any pieces, matches the decoder"
                            #'prop-chunk-feed-matches-decoder)
                      (cons "chunk feed halted inside a run hands the rest over next"
                            #'prop-chunk-feed-halts-short-of-a-run)
                      (cons "multipart reads alike in any pieces"
                            #'prop-multipart-reads-alike-in-any-pieces)
                      (cons "byte ranges stay in bounds" #'prop-byte-range-in-bounds)
                      (cons "base64 round-trips" #'prop-base64-roundtrip)
                      (cons "base64 accepts only canonical" #'prop-base64-canonical-only)))
    (dolist (seed *fuzz-seeds*)
      (let ((*random-state* (sb-ext:seed-random-state seed)))
        (check (format nil "~a (seed ~a)" (car spec) seed)
               (funcall (cdr spec) seed)
               nil))))
  (test-transport-parity)
  (report-suite "properties")
  (zerop *tests-failed*))
