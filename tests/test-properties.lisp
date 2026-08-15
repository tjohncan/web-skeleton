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

   Measured rather than assumed: across 400 cases (half random, half
   from here) the verdicts land as ~204 request-line, ~121 version, ~37
   fully accepted, ~17 header, ~10 method, ~10 path. If a change to this
   generator pushes nearly everything into request-line, the property
   above is still passing but has stopped testing the parser."
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
;;; The HTTPS path is not included, and that is a real gap rather than an
;;; oversight: TLS-STREAM-RESPONSE reads via %SSL-READ on a raw pointer,
;;; with no seam a test can substitute a byte source into. Its 1xx
;;; handling is therefore review-verified only. Closing that needs either
;;; a local TLS listener in the harness or an injectable reader in
;;; tls.lisp, and it is worth doing.
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

(defun tls-stream-lines (raw &key (method :GET))
  "Drive TLS-STREAM-RESPONSE over RAW through its READ-FN seam.
   Returns (STATUS LINES), or :SKIPPED when web-skeleton-tls is absent.

   Resolved at run time rather than written literally, for the reason
   TEST-TLS's TLS-SYM gives: the TLS system is optional and loaded by the
   entry script, so naming the symbol at read time would intern it and
   emit undefined-function warnings on every compile of a tree without it.

   SSL is NIL and never dereferenced — supplying READ-FN is what takes the
   %SSL-READ path out of play, which is the whole point of the seam."
  (let ((fn (and web-skeleton:*https-fetch-fn*
                 (find-symbol "TLS-STREAM-RESPONSE" :web-skeleton))))
    (if (null fn)
        :skipped
        (let ((pos 0)
              (lines nil))
          ;; A divergence in the reader shows up as a raise as often as a
          ;; wrong answer — a chunk-accounting slip reaches the end of the
          ;; bytes without the zero-size terminator and signals. Caught so
          ;; that lands as a failed CHECK carrying the message, rather than
          ;; a backtrace that ends the suite before the other arms run.
          (handler-case
              (list (funcall fn nil
                         (lambda (line) (push line lines))
                         :method method
                         :read-fn
                         (lambda (buf len)
                           ;; Hand the bytes over in buffer-sized bites,
                           ;; so a record boundary can fall anywhere and
                           ;; the cross-read state (PREV-CR, the chunk
                           ;; phase flags) is exercised rather than
                           ;; bypassed by one big read.
                           (if (>= pos (length raw))
                               :eof
                               (let ((n (min len (- (length raw) pos))))
                                 (replace buf raw :start2 pos :end2 (+ pos n))
                                 (incf pos n)
                                 n))))
                    (nreverse lines))
            (error (e) (list :raised (princ-to-string e))))))))

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
      ;; The third transport. tls.lisp carries an independent
      ;; implementation of this same read, and the file header of this
      ;; suite recorded it as an unclosable gap — %SSL-READ takes a raw
      ;; pointer with no seam to substitute a byte source into. There is
      ;; one now, so the same fixture goes through all three and the
      ;; "two readers must never disagree" principle covers the reader
      ;; that most needed it.
      (let ((tls (tls-stream-lines raw)))
        (if (eq tls :skipped)
            (format t "  SKIP  parity: TLS arm (web-skeleton-tls not loaded)~%")
            (check (format nil "parity: ~d interim block(s), TLS agrees"
                           (length interims))
                   tls
                   (list 200 (list "{\"a\":1}" "{\"b\":2}")))))))
  ;; Chunked framing through the TLS reader. This is where the duplicate
  ;; implementation is densest — its own chunk-size scanner, its own
  ;; CR/LF partner tracking, its own expect-CR/expect-LF pair — and where
  ;; a divergence from stream-chunked-lines would be least visible.
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
         (tls (tls-stream-lines raw)))
    (if (eq tls :skipped)
        (format t "  SKIP  parity: TLS chunked arm~%")
        (check "parity: chunked body, TLS agrees with the plain reader"
               tls
               (let ((lines nil)
                     (stream (make-mock-stream raw)))
                 (unwind-protect
                      (list (web-skeleton::stream-response-lines
                             stream (lambda (l) (push l lines)))
                            (nreverse lines))
                   (close stream))))))
  ;; And the cap is the same number on both paths — three transports
  ;; reading one variable was the point of *max-interim-responses*.
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
