(in-package :web-skeleton)

;;; ===========================================================================
;;; JSON Parser and Serializer (RFC 8259)
;;;
;;; Parse: JSON string -> Lisp data
;;;   Objects  -> JSON-OBJECT struct wrapping an alist ((key . value) ...)
;;;   Arrays   -> lists   (value ...)
;;;   Strings  -> strings
;;;   Numbers  -> integers or floats
;;;   true     -> T
;;;   false    -> :FALSE
;;;   null     -> :NULL
;;;
;;; Serialize: Lisp data -> JSON string
;;;   JSON-OBJECT -> object
;;;   lists       -> arrays
;;;   strings     -> strings
;;;   integers    -> numbers
;;;   floats      -> numbers
;;;   T           -> true
;;;   :FALSE      -> false
;;;   :NULL       -> null
;;;   NIL         -> [] (NIL is the empty list in Common Lisp)
;;;
;;; Why objects are a distinct type rather than a bare alist
;;; -------------------------------------------------------
;;; With objects as bare alists, the serializer would have to guess which
;;; of the two a list is, by testing whether every element is a cons with
;;; a string car. That test cannot be made correct, because the two shapes
;;; are the same Lisp object:
;;;
;;;   [["a",1],["b",2]]  parses to  (("a" 1) ("b" 2))
;;;   {"a":1,"b":2}      parses to  (("a" . 1) ("b" . 2))
;;;
;;; ...and an alist whose values happen to be lists — (("items" . (1 2 3)))
;;; — is indistinguishable from an array of arrays. So an ordinary array of
;;; key/value pairs (Object.entries output, CSV-ish rows, header lists)
;;; would come back out as an object: [["a",1],["b",2]] re-serialized as
;;; {"a":[1],"b":[2]}. Well-formed, silently wrong, no error anywhere. The
;;; information is destroyed at parse time, so no heuristic downstream can
;;; recover it.
;;;
;;; Typed objects round-trip in both directions and keep {} and [] apart:
;;; {} is an empty JSON-OBJECT, [] is NIL, and null is :NULL — three
;;; distinct values that each serialize back to themselves.
;;;
;;; Consequence for hand-built data: a bare alist is an ARRAY of pairs.
;;; Wrap it in MAKE-JSON-OBJECT to emit an object. This fails loudly rather
;;; than silently — a dotted pair is not a valid array element, so
;;; (json-serialize '(("a" . 1))) raises the improper-list error below
;;; instead of quietly emitting the wrong shape.
;;;
;;; false and null are keywords to avoid ambiguity with NIL (empty list).
;;; ===========================================================================

(defparameter *json-max-depth* 256
  "Maximum nesting depth for JSON parsing. Prevents stack overflow on
   deeply nested input.")

(defparameter *json-max-string-length* (* 1 1024 1024)
  "Maximum decoded length of a single JSON string in characters.
   Default 1 MiB. Bounds the accumulator in JSON-PARSE-STRING so an
   attacker feeding a giant base64/hex-looking value at the outbound
   response boundary (bodies up to *MAX-OUTBOUND-RESPONSE-SIZE* =
   8 MiB) cannot force an 8 MiB per-string allocation. Raise for apps
   that need larger strings; do not disable.")

;;; ---------------------------------------------------------------------------
;;; The parser's condition
;;; ---------------------------------------------------------------------------

(define-condition json-parse-error (error)
  ((message :initarg :message :reader json-parse-error-message))
  (:report (lambda (condition stream)
             (format stream "json: ~a" (json-parse-error-message condition))))
  (:documentation
   "The text given to JSON-PARSE is not JSON, or is JSON past one of the
    parser's bounds: *JSON-MAX-DEPTH*, *JSON-MAX-STRING-LENGTH*, the digits
    a number may have. Or the bytes given to JSON-PARSE-BYTES are not
    UTF-8. MESSAGE says what was wrong and where.

    The parser signals this for the text or the bytes it refuses and
    nothing else, so a caller can catch it by name. That is what tells a
    body a client got wrong, which is the client's 400, from a fault in
    the caller's own code, which is a 500 and should stay one: a catch of
    ERROR around the parse takes both.

    Nothing answers it on the caller's behalf. One that escapes a handler
    is that handler's error like any other, because the text is not
    always the client's: an upstream's reply read in a fetch callback
    goes through the same parser, and its fault is not a bad request."))

;; Never returns, which the compiler is told so that a call reads to it as
;; ERROR does: nothing is expected back where one stands for a value.
(declaim (ftype (function (string &rest t) nil) json-parse-error))
(defun json-parse-error (format-string &rest args)
  "Signal a JSON-PARSE-ERROR with a formatted message."
  (error 'json-parse-error
         :message (apply #'format nil format-string args)))

;;; ---------------------------------------------------------------------------
;;; Object representation
;;; ---------------------------------------------------------------------------

(defstruct (json-object (:constructor make-json-object (&optional alist))
                        (:copier nil))
  "A JSON object: an ordered alist of (key . value) with string keys,
   wrapped so it is distinguishable from a JSON array at serialize time.
   ALIST preserves document order and may be read directly, but JSON-GET
   is the supported accessor — it also accepts a bare alist, so code that
   holds an object as a plain alist reads it the same way."
  (alist nil :type list))

;;; ---------------------------------------------------------------------------
;;; Parser internals
;;; ---------------------------------------------------------------------------

(defun json-skip-whitespace (str pos)
  "Advance past JSON whitespace. Returns new position."
  (loop while (and (< pos (length str))
                   (member (char str pos) '(#\Space #\Tab #\Newline #\Return)))
        do (incf pos))
  pos)

(defun json-parse-string (str pos)
  "Parse a JSON string starting at the opening quote at POS.
   Handles all escape sequences including \\uXXXX.
   Returns (values string new-pos).
   Caller is responsible for checking the opening quote.

   The string returned is a (SIMPLE-ARRAY CHARACTER (*)) of exactly its
   length, whatever kind of string STR is. One with no escape in it is
   its characters as they stand in STR: found by the scan here, and
   copied once. One with an escape is finished by
   JSON-PARSE-ESCAPED-STRING from the escape on."
  (incf pos)
  (let ((start pos)
        (len (length str)))
    (loop
      (when (>= pos len)
        (json-parse-error "unterminated string"))
      (let ((ch (char str pos)))
        (cond
          ((char= ch #\")
           ;; MAKE-STRING and not SUBSEQ, which answers a string of STR's
           ;; own element type: a base string in would be a base string
           ;; out, and no place for a character an application puts there.
           (let ((out (make-string (- pos start))))
             (replace out str :start2 start :end2 pos)
             (return (values out (1+ pos)))))
          ((char= ch #\\)
           (return (json-parse-escaped-string str start pos)))
          ;; RFC 8259 §7: control characters U+0000-U+001F must be escaped
          ((< (char-code ch) #x20)
           (json-parse-error "unescaped control character U+~4,'0X at ~d"
                             (char-code ch) pos))
          ;; *JSON-MAX-STRING-LENGTH*, as JSON-PARSE-ESCAPED-STRING holds
          ;; its accumulator to it: met at the first character past it.
          ((>= (- pos start) *json-max-string-length*)
           (json-parse-error "string exceeds ~d characters"
                             *json-max-string-length*))
          (t (incf pos)))))))

(defun json-parse-escaped-string (str start pos)
  "The JSON string whose first character is at START and whose first
   escape is at POS, everything between having been scanned already.
   Returns (values string new-pos), as JSON-PARSE-STRING does and for it.

   Decoded into a buffer as long as the string is written, which is room
   enough: an escape is never longer decoded than written. What was
   decoded is copied out at the end, a string of its own length.
   A buffer handed back as it stood, with its room to spare, would keep
   that room for as long as the parsed document was kept."
  (let* ((len (length str))
         ;; Where the string ends as written: the first quote no escape
         ;; covers, or the end of STR for a string never closed, which the
         ;; loop below is left to report.
         (close (loop with i = pos
                      do (cond ((>= i len) (return len))
                               ((char= (char str i) #\") (return i))
                               ((char= (char str i) #\\) (incf i 2))
                               (t (incf i)))))
         (out (make-string (min (- close start) *json-max-string-length*)))
         (n (- pos start)))
    (replace out str :start2 start :end2 pos)
    (flet ((push-char (c)
             ;; Bound the decoded string at *JSON-MAX-STRING-LENGTH*.
             ;; Without this cap an attacker-controlled response body
             ;; (up to *MAX-OUTBOUND-RESPONSE-SIZE* = 8 MiB) could force
             ;; an 8 MiB per-string allocation across many keys and
             ;; values.
             (when (>= n *json-max-string-length*)
               (json-parse-error "string exceeds ~d characters"
                                 *json-max-string-length*))
             (setf (schar out n) c)
             (incf n)))
    (loop
      (when (>= pos len)
        (json-parse-error "unterminated string"))
      (let ((ch (char str pos)))
        (cond
          ((char= ch #\")
           (return (values (subseq out 0 n) (1+ pos))))
          ((char= ch #\\)
           (incf pos)
           (when (>= pos len)
             (json-parse-error "unterminated escape"))
           (let ((esc (char str pos)))
             (case esc
               (#\n (push-char #\Newline))
               (#\t (push-char #\Tab))
               (#\r (push-char #\Return))
               (#\" (push-char #\"))
               (#\\ (push-char #\\))
               (#\/ (push-char #\/))
               (#\b (push-char (code-char 8)))
               (#\f (push-char (code-char 12)))
               (#\u
                ;; \uXXXX — exactly 4 hex digits required (RFC 8259 §7)
                (when (> (+ pos 5) len)
                  (json-parse-error "truncated \\uXXXX escape at ~d" (1- pos)))
                (let ((code (let ((hs (1+ pos)) (he (+ pos 5)))
                              (loop for k from hs below he
                                    for ch = (char str k)
                                    unless (or (char<= #\0 ch #\9)
                                               (char<= #\a ch #\f)
                                               (char<= #\A ch #\F))
                                    do (json-parse-error
                                        "invalid hex digit '~c' in \\u escape at ~d"
                                        ch (1- pos)))
                              (parse-integer str :start hs :end he :radix 16))))
                  ;; Handle surrogate pairs for characters above U+FFFF
                  (cond
                    ((<= #xD800 code #xDBFF)
                     ;; High surrogate — expect \uXXXX low surrogate
                     (let ((low-start (+ pos 5)))
                       (if (and (< (+ low-start 5) len)
                                (char= (char str low-start) #\\)
                                (char= (char str (1+ low-start)) #\u))
                           (let ((low (let ((ls (+ low-start 2))
                                          (le (+ low-start 6)))
                                      (loop for k from ls below le
                                            for ch = (char str k)
                                            unless (or (char<= #\0 ch #\9)
                                                       (char<= #\a ch #\f)
                                                       (char<= #\A ch #\F))
                                            do (json-parse-error
                                                "invalid hex digit '~c' in \\u escape at ~d"
                                                ch low-start))
                                      (parse-integer str :start ls :end le :radix 16))))
                             (unless (<= #xDC00 low #xDFFF)
                               (json-parse-error
                                "expected low surrogate after \\u~4,'0X at ~d"
                                code (1- pos)))
                             (let ((cp (+ #x10000
                                          (ash (- code #xD800) 10)
                                          (- low #xDC00))))
                               (push-char (code-char cp)))
                             (incf pos 10))
                           (json-parse-error
                            "lone high surrogate \\u~4,'0X at ~d"
                            code (1- pos)))))
                    ((<= #xDC00 code #xDFFF)
                     (json-parse-error "lone low surrogate \\u~4,'0X at ~d"
                                       code (1- pos)))
                    (t
                     (push-char (code-char code))
                     (incf pos 4)))))
               (t (json-parse-error "invalid escape \\~c at ~d" esc (1- pos)))))
           (incf pos))
          (t
           ;; RFC 8259 §7: control characters U+0000-U+001F must be escaped
           (when (< (char-code ch) #x20)
             (json-parse-error "unescaped control character U+~4,'0X at ~d"
                               (char-code ch) pos))
           (push-char ch)
           (incf pos))))))))

(defun json-parse-number (str pos)
  "Parse a JSON number starting at POS. Returns (values number new-pos)."
  (let ((start pos)
        (len (length str)))
    (when (and (< pos len) (char= (char str pos) #\-))
      (incf pos))
    ;; RFC 8259: integer part requires at least one digit
    (let ((digit-start pos))
      ;; Leading zeros not allowed (except bare 0)
      (when (and (< pos len) (char= (char str pos) #\0)
                 (< (1+ pos) len) (char<= #\0 (char str (1+ pos)) #\9))
        (json-parse-error "leading zeros not allowed at ~d" start))
      (loop while (and (< pos len) (char<= #\0 (char str pos) #\9))
            do (incf pos))
      (when (= pos digit-start)
        (json-parse-error "expected digit at ~d" pos))
      ;; Cap integer digits to prevent O(n²) bignum construction DoS
      (when (> (- pos digit-start) 300)
        (json-parse-error "integer too many digits at ~d" start)))
    (when (and (< pos len) (char= (char str pos) #\.))
      (incf pos)
      (let ((frac-start pos))
        (loop while (and (< pos len) (char<= #\0 (char str pos) #\9))
              do (incf pos))
        (when (= pos frac-start)
          (json-parse-error "no digits after decimal point at ~d" start))
        (when (> (- pos frac-start) 300)
          (json-parse-error "fractional part too many digits at ~d" start))))
    (when (and (< pos len) (member (char str pos) '(#\e #\E)))
      (incf pos)
      (when (and (< pos len) (member (char str pos) '(#\+ #\-)))
        (incf pos))
      (let ((exp-start pos))
        (loop while (and (< pos len) (char<= #\0 (char str pos) #\9))
              do (incf pos))
        (when (= pos exp-start)
          (json-parse-error "no digits in exponent at ~d" start))
        (when (> (- pos exp-start) 20)
          (json-parse-error "exponent too many digits at ~d" start))))
    (let ((num-str (subseq str start pos)))
      ;; Floats: read-from-string with *read-eval* NIL is safe here —
      ;; input is pre-validated to [0-9.eE+-] by the loop above.
      ;; Bind *read-default-float-format* to double-float so the CL reader
      ;; produces full 64-bit precision (default is single-float).
      ;;
      ;; The syntactic caps above (300 integer digits, 300 fraction
      ;; digits, 20 exponent digits) admit values like '1e9999' which
      ;; overflow IEEE 754 double. SBCL's reader signals a
      ;; floating-point-overflow from inside READ-FROM-STRING before
      ;; control returns — earlier than the post-read infinity check
      ;; could see it — so uncaught, an app would get a raw
      ;; SB-KERNEL:FLOATING-POINT-EXCEPTION rather than the 'number out
      ;; of range' error. Catch the arithmetic condition family and remap
      ;; to the same clean overflow error the infinity branch produces.
      (values (if (or (find #\. num-str) (find #\e num-str) (find #\E num-str))
                  (let* ((*read-default-float-format* 'double-float)
                         (*read-eval* nil)
                         (*read-base* 10)
                         ;; Broad catch: READ-FROM-STRING can signal
                         ;; ARITHMETIC-ERROR on overflow, but also
                         ;; READER-ERROR / SIMPLE-ERROR on shapes the
                         ;; pre-scan did not rule out. All of them
                         ;; route to the same 'out of range at N'
                         ;; error so an app sees one consistent
                         ;; failure shape instead of a raw SBCL
                         ;; condition leaking through.
                         (val (handler-case (read-from-string num-str)
                                (error ()
                                  (json-parse-error "number out of range at ~d"
                                                    start)))))
                    (when (or (sb-ext:float-infinity-p val)
                              (sb-ext:float-nan-p val))
                      (json-parse-error "number out of range at ~d" start))
                    val)
                  (parse-integer num-str))
              pos))))

(defun json-parse-value (str pos &optional (depth 0))
  "Parse a JSON value at POS. Returns (values value new-pos)."
  (when (> depth *json-max-depth*)
    (json-parse-error "nesting too deep (max ~d)" *json-max-depth*))
  (setf pos (json-skip-whitespace str pos))
  (when (>= pos (length str))
    (json-parse-error "unexpected end of input"))
  (let ((ch (char str pos)))
    (cond
      ((char= ch #\") (json-parse-string str pos))
      ((char= ch #\{) (json-parse-object str pos depth))
      ((char= ch #\[) (json-parse-array str pos depth))
      ((char= ch #\t)
       (unless (and (<= (+ pos 4) (length str))
                    (string= str "true" :start1 pos :end1 (+ pos 4)))
         (json-parse-error "invalid literal at ~d" pos))
       (values t (+ pos 4)))
      ((char= ch #\f)
       (unless (and (<= (+ pos 5) (length str))
                    (string= str "false" :start1 pos :end1 (+ pos 5)))
         (json-parse-error "invalid literal at ~d" pos))
       (values :false (+ pos 5)))
      ((char= ch #\n)
       (unless (and (<= (+ pos 4) (length str))
                    (string= str "null" :start1 pos :end1 (+ pos 4)))
         (json-parse-error "invalid literal at ~d" pos))
       (values :null (+ pos 4)))
      ;; ASCII digits only: DIGIT-CHAR-P is true of every Unicode digit,
      ;; U+0663 among them, which JSON's grammar does not admit.
      ((or (char<= #\0 ch #\9) (char= ch #\-))
       (json-parse-number str pos))
      (t (json-parse-error "unexpected character '~a' at ~d" ch pos)))))

(defun first-repeated-key (entries)
  "The entry of ENTRIES, conses whose cars are strings, given in the order
   they were read, whose car repeats an earlier entry's: the earliest such,
   or NIL.

   By sorting and comparing neighbours, not by a hash table of what was
   seen. The keys are a client's to choose, and SBCL's string hash takes
   no secret: on 2.2.9 a client can compute keys that all fall in one
   bucket and make each lookup walk every key before it, the square of
   their count. A merge sort costs its N log N comparisons whatever the
   keys. Used for JSON-PARSE's object keys and for multipart's header
   parameters."
  (when (rest entries)
    (let ((sorted (stable-sort (loop for entry in entries
                                     for i from 0
                                     collect (cons i entry))
                               #'string< :key #'cadr))
          (found nil))
      ;; Stable, so within a run of one key the entries keep the order they
      ;; were read in, and each after the first is a repeat. The earliest
      ;; repeat is the one a reader going in order meets first.
      (loop for (a b) on sorted
            while b
            when (and (string= (cadr a) (cadr b))
                      (or (null found) (< (car b) (car found))))
              do (setf found b))
      (cdr found))))

(defun json-parse-object (str pos &optional (depth 0))
  "Parse a JSON object at POS. Returns (values JSON-OBJECT new-pos).
   Caller (json-parse-value) has already dispatched on the opening '{'.
   An empty object is an empty JSON-OBJECT, not NIL — that is what keeps
   {} distinguishable from [] and from null.

   Duplicate keys raise. RFC 8259 §4 says names within an object
   SHOULD be unique and permits 'undefined' or 'implementation-
   dependent' behavior on duplicates, and several popular parsers
   silently pick last-wins. Our alist keeps every pair, so a naive
   (cdr (assoc ...)) consumer would see the FIRST value and a
   rewrite to (cdr (car ...)) would see the LAST, and the behavior
   of the application would swing on an invisible ordering detail.
   Rejecting is the honest option."
  (incf pos)
  (setf pos (json-skip-whitespace str pos))
  (when (>= pos (length str))
    (json-parse-error "unterminated object at ~d" pos))
  (when (char= (char str pos) #\})
    (return-from json-parse-object (values (make-json-object nil) (1+ pos))))
  (let ((pairs nil)
        ;; (key . position), newest first; checked for a repeat at the close
        ;; brace by FIRST-REPEATED-KEY.
        (keys nil))
    (loop
      (setf pos (json-skip-whitespace str pos))
      (when (>= pos (length str))
        (json-parse-error "unterminated object at ~d" pos))
      ;; Object keys must be quoted strings (RFC 8259 §4). This is the
      ;; one json-parse-string call site without an upstream dispatch,
      ;; so the check belongs here rather than inside json-parse-string.
      (unless (char= (char str pos) #\")
        (json-parse-error "expected string key at ~d" pos))
      (let ((key-pos pos))
        (multiple-value-bind (key new-pos) (json-parse-string str pos)
          (push (cons key key-pos) keys)
          (setf pos (json-skip-whitespace str new-pos))
          (when (>= pos (length str))
            (json-parse-error "unterminated object at ~d" pos))
          (unless (char= (char str pos) #\:)
            (json-parse-error "expected ':' at ~d" pos))
          (incf pos)
          (multiple-value-bind (val new-pos2)
              (json-parse-value str pos (1+ depth))
            (push (cons key val) pairs)
            (setf pos (json-skip-whitespace str new-pos2))
            (when (>= pos (length str))
              (json-parse-error "unterminated object at ~d" pos))
            (cond
              ((char= (char str pos) #\,) (incf pos))
              ((char= (char str pos) #\})
               (let ((repeat (first-repeated-key (nreverse keys))))
                 (when repeat
                   (json-parse-error "duplicate key ~s at ~d"
                                     (car repeat) (cdr repeat))))
               (return (values (make-json-object (nreverse pairs)) (1+ pos))))
              (t (json-parse-error "expected ',' or '}' at ~d" pos)))))))))

(defun json-parse-array (str pos &optional (depth 0))
  "Parse a JSON array at POS. Returns (values list new-pos).
   Caller (json-parse-value) has already dispatched on the opening '['."
  (incf pos)
  (setf pos (json-skip-whitespace str pos))
  (when (>= pos (length str))
    (json-parse-error "unterminated array at ~d" pos))
  (when (char= (char str pos) #\])
    (return-from json-parse-array (values nil (1+ pos))))
  (let ((items nil))
    (loop
      (multiple-value-bind (val new-pos) (json-parse-value str pos (1+ depth))
        (push val items)
        (setf pos (json-skip-whitespace str new-pos))
        (when (>= pos (length str))
          (json-parse-error "unterminated array at ~d" pos))
        (cond
          ((char= (char str pos) #\,) (incf pos))
          ((char= (char str pos) #\])
           (return (values (nreverse items) (1+ pos))))
          (t (json-parse-error "expected ',' or ']' at ~d" pos)))))))

;;; ---------------------------------------------------------------------------
;;; Public parser interface
;;; ---------------------------------------------------------------------------

(defun json-parse (str)
  "Parse a JSON string into Lisp data.
   Objects become JSON-OBJECT structs (read them with JSON-GET), arrays
   become lists. false -> :FALSE, null -> :NULL. {} is an empty
   JSON-OBJECT, [] is NIL, and null is :NULL — three distinct values,
   each of which serializes back to itself.

   Signals JSON-PARSE-ERROR for a string that is not JSON, or is past one
   of the parser's bounds, and no other condition for any string. A
   document held as bytes, a request's body, goes to JSON-PARSE-BYTES.

   A leading U+FEFF (UTF-8 BOM when decoded via UTF-8) is silently
   skipped per RFC 8259 §8.1. Windows text editors, older .NET
   APIs, and some Python encoders on Windows prepend one; rejecting
   it would make the parser more strict than the spec requires."
  (let ((start (if (and (> (length str) 0)
                        (char= (char str 0) (code-char #xFEFF)))
                   1
                   0)))
    (multiple-value-bind (val pos) (json-parse-value str start)
      (let ((end (json-skip-whitespace str pos)))
        (when (< end (length str))
          (json-parse-error "unexpected content at position ~d" end)))
      val)))

(defun json-parse-bytes (bytes)
  "Parse a JSON document from its bytes: decode them as UTF-8 and hand
   the text to JSON-PARSE, whose value this returns. A request's body and
   an upstream's reply are bytes.

   Bytes that are not UTF-8 signal JSON-PARSE-ERROR, as text that is not
   JSON does, so one catch by name answers a malformed body whichever it
   was. RFC 8259 §8.1 has a document sent between systems encoded as
   UTF-8, and the bytes are read as that: no other encoding is guessed
   at. The decoding is strict. An overlong form, an encoded surrogate and
   a character cut short at the end are each refused.

   BYTES is a vector of octets, or NIL. NIL is read as no bytes, and
   refused as the empty string is: it is what HTTP-REQUEST-BODY answers
   for a request that carried no body, which is the client's doing as a
   malformed one is. Any other argument, a string for one, is the
   caller's mistake and a TYPE-ERROR."
  (json-parse
   (if (null bytes)
       ""
       (handler-case (sb-ext:octets-to-string bytes :external-format :utf-8)
         (sb-int:character-decoding-error ()
           (json-parse-error "the bytes are not UTF-8"))))))

(defun json-get (obj key)
  "Look up KEY (string) in OBJ. Returns the value, or NIL if absent.

   OBJ may be a JSON-OBJECT (what JSON-PARSE produces) or a bare alist.
   Accepting both is deliberate: code that holds an object as a plain
   alist reads it the same way as a parsed one, and the framework's own
   readers (PARSE-JWKS, JWT-VERIFY) go only through this function, so
   they take either. Anything else — a string, a number, an array —
   returns NIL rather than raising, so walking a document whose shape
   differs from what the caller expected degrades to a miss instead of a
   type error deep inside ASSOC."
  (let ((alist (cond ((json-object-p obj) (json-object-alist obj))
                     ((listp obj) obj)
                     (t nil))))
    ;; Scanned rather than ASSOC'd, and the element test is what makes the
    ;; promise above true rather than aspirational. A JSON array parses to a
    ;; plain list, so the (LISTP OBJ) arm accepts one — and ASSOC on a list
    ;; whose elements are not conses is undefined per CLHS, which in SBCL
    ;; means a TYPE-ERROR raised out of the very call this docstring says
    ;; will answer NIL. PARSE-JWKS reaches it: a JWKS document that is an
    ;; array, or whose keys array holds arrays, escaped as a raw type error
    ;; from a function that gives every other malformed shape a clean
    ;; "JWKS: ..." message.
    ;;
    ;; The test is STRING-DESIGNATOR rather than STRINGP, and the wider
    ;; one is the right shape rather than a concession. STRING= is defined
    ;; on designators, so ASSOC :TEST #'STRING= matched symbol and
    ;; character keys too — a hand-built alist with symbol keys read
    ;; through this function, which the docstring above invites. STRINGP
    ;; would have dropped that silently while fixing the raise. What both
    ;; tests exclude is identical, and it is the shape that matters: a
    ;; number or a list in CAR position, which is what the elements of a
    ;; parsed array look like and exactly what STRING= raises on.
    (loop for entry in alist
          when (and (consp entry)
                    (typep (car entry) '(or string symbol character))
                    (string= (car entry) key))
            return (cdr entry))))

;;; ---------------------------------------------------------------------------
;;; Serializer
;;; ---------------------------------------------------------------------------

(defun proper-list-p (x)
  "Return T if X is a proper list — every CDR is a cons until NIL.
   Local to the JSON serializer: JSON arrays and objects require
   proper-list shape, and a one-cell LISTP check misses the deeper
   improper case. Input (1 2 . 3) has (CDR x) = (2 . 3) — a cons —
   so LISTP says T, but JSON-WRITE-ARRAY's LOOP would emit a
   trailing comma for the final non-NIL tail. Iterative walk so very
   long lists don't blow the stack. No circular-list detection — a
   circular structure fed to JSON-SERIALIZE is pathological regardless."
  (loop
    (cond ((null x) (return t))
          ((atom x) (return nil))
          (t (setf x (cdr x))))))

(defun json-serialize (value)
  "Serialize a Lisp value to a JSON string.
   A JSON-OBJECT serializes as an object; every other list serializes as
   an array. To emit an object from data you built yourself, wrap the
   alist: (json-serialize (make-json-object '((\"a\" . 1))))."
  (with-output-to-string (out)
    (json-write-value value out)))

(defun json-write-value (value stream)
  "Write VALUE as JSON to STREAM."
  (cond
    ((eq value t)      (write-string "true" stream))
    ((eq value :false) (write-string "false" stream))
    ((eq value :null)  (write-string "null" stream))
    ;; NIL is the empty list, so it emits []. An empty object is an empty
    ;; JSON-OBJECT and explicit null is :NULL, so each of the three
    ;; round-trips to itself.
    ((null value)      (write-string "[]" stream))
    ((stringp value)   (json-write-string value stream))
    ((integerp value)  (format stream "~d" value))
    ((floatp value)
     (when (typep value 'single-float)
       (error "json: single-float ~a loses precision; use ~ad0 for double-float"
              value value))
     (when (or (sb-ext:float-nan-p value)
               (sb-ext:float-infinity-p value))
       (error "json-serialize: cannot encode ~a" value))
     ;; Use write-to-string for shortest round-trippable representation.
     ;; CL's ~f produces fixed-point which loses precision at extremes.
     ;; With *read-default-float-format* as double-float, SBCL's printer
     ;; uses 'e' for exponents (matching the default format), never 'd'.
     (let* ((*read-default-float-format* 'double-float)
            (s (write-to-string (coerce value 'double-float))))
       (write-string s stream)))
    ;; Object — the only thing that emits {...}. No guessing.
    ((json-object-p value)
     (json-write-object (json-object-alist value) stream))
    ;; Improper lists reject up front — a dotted tail would emit a
    ;; trailing comma through json-write-array, producing invalid JSON.
    ;; This is also where a hand-built alist lands, objects being typed:
    ;; ("a" . 1) is not a valid array element, so the mistake is
    ;; caught here with a message naming the fix rather than emitting a
    ;; well-formed-but-wrong document.
    ((and (consp value) (not (proper-list-p value)))
     (if (and (stringp (car value)) (not (consp (cdr value))))
         (error "json: cannot serialize the dotted pair ~s. Objects are a ~
                 distinct type — wrap the alist in MAKE-JSON-OBJECT to emit ~
                 an object."
                value)
         (error "json: cannot serialize improper list ~s" value)))
    ;; Everything else that is a cons is an array. A bare alist is an
    ;; array of pairs; see the file header for why this cannot be
    ;; decided by inspection.
    ((consp value)
     (json-write-array value stream))
    (t (error "json-serialize: unsupported type ~a" (type-of value)))))

(defun json-write-string (str stream)
  "Write a JSON-escaped string to STREAM."
  (write-char #\" stream)
  (loop for ch across str
        do (case ch
             (#\\ (write-string "\\\\" stream))
             (#\" (write-string "\\\"" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (t (let ((code (char-code ch)))
                  (cond
                    ((= code 8)  (write-string "\\b" stream))
                    ((= code 12) (write-string "\\f" stream))
                    ((< code #x20) (format stream "\\u~4,'0x" code))
                    (t (write-char ch stream)))))))
  (write-char #\" stream))

(defun json-write-object (alist stream)
  "Write an alist as a JSON object to STREAM. Every key is a string, as
   JSON-PARSE makes them. Any other key signals with a message naming it,
   rather than a bare type error from inside JSON-WRITE-STRING."
  (write-char #\{ stream)
  (loop for (pair . rest) on alist
        do (unless (stringp (car pair))
             (error "json: an object key must be a string, not ~s" (car pair)))
           (json-write-string (car pair) stream)
           (write-char #\: stream)
           (json-write-value (cdr pair) stream)
        when rest do (write-char #\, stream))
  (write-char #\} stream))

(defun json-write-array (list stream)
  "Write a list as a JSON array to STREAM."
  (write-char #\[ stream)
  (loop for (item . rest) on list
        do (json-write-value item stream)
        when rest do (write-char #\, stream))
  (write-char #\] stream))
