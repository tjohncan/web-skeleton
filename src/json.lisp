(in-package :web-skeleton)

;;; ===========================================================================
;;; JSON Parser and Serializer (RFC 8259)
;;;
;;; Parse: JSON string -> Lisp data
;;;   Objects  -> alists  ((key . value) ...)
;;;   Arrays   -> lists   (value ...)
;;;   Strings  -> strings
;;;   Numbers  -> integers or floats
;;;   true     -> T
;;;   false    -> :FALSE
;;;   null     -> :NULL
;;;
;;; Serialize: Lisp data -> JSON string
;;;   alists   -> objects (when car of first element is a string)
;;;   lists    -> arrays
;;;   strings  -> strings
;;;   integers -> numbers
;;;   floats   -> numbers
;;;   T        -> true
;;;   :FALSE   -> false
;;;   :NULL    -> null
;;;   NIL      -> null
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
   Caller is responsible for checking the opening quote."
  (incf pos)
  (let ((out (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t))
        (len (length str)))
    (flet ((push-char (c)
             ;; Bound the per-string accumulator at
             ;; *JSON-MAX-STRING-LENGTH*. Without this cap an attacker-
             ;; controlled response body (up to *MAX-OUTBOUND-RESPONSE-
             ;; SIZE* = 8 MiB) could force an 8 MiB per-string allocation
             ;; across many keys and values.
             (when (>= (fill-pointer out) *json-max-string-length*)
               (error "json: string exceeds ~d characters"
                      *json-max-string-length*))
             (vector-push-extend c out)))
    (loop
      (when (>= pos len)
        (error "json: unterminated string"))
      (let ((ch (char str pos)))
        (cond
          ((char= ch #\")
           (return (values (coerce out 'string) (1+ pos))))
          ((char= ch #\\)
           (incf pos)
           (when (>= pos len)
             (error "json: unterminated escape"))
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
                  (error "json: truncated \\uXXXX escape at ~d" (1- pos)))
                (let ((code (let ((hs (1+ pos)) (he (+ pos 5)))
                              (loop for k from hs below he
                                    for ch = (char str k)
                                    unless (or (char<= #\0 ch #\9)
                                               (char<= #\a ch #\f)
                                               (char<= #\A ch #\F))
                                    do (error "json: invalid hex digit '~c' in \\u escape at ~d"
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
                                            do (error "json: invalid hex digit '~c' in \\u escape at ~d"
                                                      ch low-start))
                                      (parse-integer str :start ls :end le :radix 16))))
                             (unless (<= #xDC00 low #xDFFF)
                               (error "json: expected low surrogate after \\u~4,'0X at ~d"
                                      code (1- pos)))
                             (let ((cp (+ #x10000
                                          (ash (- code #xD800) 10)
                                          (- low #xDC00))))
                               (push-char (code-char cp)))
                             (incf pos 10))
                           (error "json: lone high surrogate \\u~4,'0X at ~d"
                                  code (1- pos)))))
                    ((<= #xDC00 code #xDFFF)
                     (error "json: lone low surrogate \\u~4,'0X at ~d"
                            code (1- pos)))
                    (t
                     (push-char (code-char code))
                     (incf pos 4)))))
               (t (error "json: invalid escape \\~c at ~d" esc (1- pos)))))
           (incf pos))
          (t
           ;; RFC 8259 §7: control characters U+0000-U+001F must be escaped
           (when (< (char-code ch) #x20)
             (error "json: unescaped control character U+~4,'0X at ~d"
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
        (error "json: leading zeros not allowed at ~d" start))
      (loop while (and (< pos len) (char<= #\0 (char str pos) #\9))
            do (incf pos))
      (when (= pos digit-start)
        (error "json: expected digit at ~d" pos))
      ;; Cap integer digits to prevent O(n²) bignum construction DoS
      (when (> (- pos digit-start) 300)
        (error "json: integer too many digits at ~d" start)))
    (when (and (< pos len) (char= (char str pos) #\.))
      (incf pos)
      (let ((frac-start pos))
        (loop while (and (< pos len) (char<= #\0 (char str pos) #\9))
              do (incf pos))
        (when (= pos frac-start)
          (error "json: no digits after decimal point at ~d" start))
        (when (> (- pos frac-start) 300)
          (error "json: fractional part too many digits at ~d" start))))
    (when (and (< pos len) (member (char str pos) '(#\e #\E)))
      (incf pos)
      (when (and (< pos len) (member (char str pos) '(#\+ #\-)))
        (incf pos))
      (let ((exp-start pos))
        (loop while (and (< pos len) (char<= #\0 (char str pos) #\9))
              do (incf pos))
        (when (= pos exp-start)
          (error "json: no digits in exponent at ~d" start))
        (when (> (- pos exp-start) 20)
          (error "json: exponent too many digits at ~d" start))))
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
      ;; could see it — so the friendly 'number out of range' error
      ;; never fired and apps got a raw SB-KERNEL:FLOATING-POINT-
      ;; EXCEPTION with 'an error of type FLOATING-POINT-OVERFLOW'.
      ;; Catch the arithmetic condition family and remap to the same
      ;; clean overflow error the infinity branch produces.
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
                                  (error "json: number out of range at ~d"
                                         start)))))
                    (when (or (sb-ext:float-infinity-p val)
                              (sb-ext:float-nan-p val))
                      (error "json: number out of range at ~d" start))
                    val)
                  (parse-integer num-str))
              pos))))

(defun json-parse-value (str pos &optional (depth 0))
  "Parse a JSON value at POS. Returns (values value new-pos)."
  (when (> depth *json-max-depth*)
    (error "json: nesting too deep (max ~d)" *json-max-depth*))
  (setf pos (json-skip-whitespace str pos))
  (when (>= pos (length str))
    (error "json: unexpected end of input"))
  (let ((ch (char str pos)))
    (cond
      ((char= ch #\") (json-parse-string str pos))
      ((char= ch #\{) (json-parse-object str pos depth))
      ((char= ch #\[) (json-parse-array str pos depth))
      ((char= ch #\t)
       (unless (and (<= (+ pos 4) (length str))
                    (string= str "true" :start1 pos :end1 (+ pos 4)))
         (error "json: invalid literal at ~d" pos))
       (values t (+ pos 4)))
      ((char= ch #\f)
       (unless (and (<= (+ pos 5) (length str))
                    (string= str "false" :start1 pos :end1 (+ pos 5)))
         (error "json: invalid literal at ~d" pos))
       (values :false (+ pos 5)))
      ((char= ch #\n)
       (unless (and (<= (+ pos 4) (length str))
                    (string= str "null" :start1 pos :end1 (+ pos 4)))
         (error "json: invalid literal at ~d" pos))
       (values :null (+ pos 4)))
      ((or (digit-char-p ch) (char= ch #\-))
       (json-parse-number str pos))
      (t (error "json: unexpected character '~a' at ~d" ch pos)))))

(defun json-parse-object (str pos &optional (depth 0))
  "Parse a JSON object at POS. Returns (values alist new-pos).
   Caller (json-parse-value) has already dispatched on the opening '{'.

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
  (when (>= pos (length str)) (error "json: unterminated object at ~d" pos))
  (when (char= (char str pos) #\})
    (return-from json-parse-object (values nil (1+ pos))))
  (let ((pairs nil)
        (seen (make-hash-table :test #'equal)))
    (loop
      (setf pos (json-skip-whitespace str pos))
      (when (>= pos (length str)) (error "json: unterminated object at ~d" pos))
      ;; Object keys must be quoted strings (RFC 8259 §4). This is the
      ;; one json-parse-string call site without an upstream dispatch,
      ;; so the check belongs here rather than inside json-parse-string.
      (unless (char= (char str pos) #\")
        (error "json: expected string key at ~d" pos))
      (let ((key-pos pos))
        (multiple-value-bind (key new-pos) (json-parse-string str pos)
          (when (gethash key seen)
            (error "json: duplicate key ~s at ~d" key key-pos))
          (setf (gethash key seen) t)
          (setf pos (json-skip-whitespace str new-pos))
          (when (>= pos (length str))
            (error "json: unterminated object at ~d" pos))
          (unless (char= (char str pos) #\:)
            (error "json: expected ':' at ~d" pos))
          (incf pos)
          (multiple-value-bind (val new-pos2)
              (json-parse-value str pos (1+ depth))
            (push (cons key val) pairs)
            (setf pos (json-skip-whitespace str new-pos2))
            (when (>= pos (length str))
              (error "json: unterminated object at ~d" pos))
            (cond
              ((char= (char str pos) #\,) (incf pos))
              ((char= (char str pos) #\})
               (return (values (nreverse pairs) (1+ pos))))
              (t (error "json: expected ',' or '}' at ~d" pos)))))))))

(defun json-parse-array (str pos &optional (depth 0))
  "Parse a JSON array at POS. Returns (values list new-pos).
   Caller (json-parse-value) has already dispatched on the opening '['."
  (incf pos)
  (setf pos (json-skip-whitespace str pos))
  (when (>= pos (length str)) (error "json: unterminated array at ~d" pos))
  (when (char= (char str pos) #\])
    (return-from json-parse-array (values nil (1+ pos))))
  (let ((items nil))
    (loop
      (multiple-value-bind (val new-pos) (json-parse-value str pos (1+ depth))
        (push val items)
        (setf pos (json-skip-whitespace str new-pos))
        (when (>= pos (length str)) (error "json: unterminated array at ~d" pos))
        (cond
          ((char= (char str pos) #\,) (incf pos))
          ((char= (char str pos) #\])
           (return (values (nreverse items) (1+ pos))))
          (t (error "json: expected ',' or ']' at ~d" pos)))))))

;;; ---------------------------------------------------------------------------
;;; Public parser interface
;;; ---------------------------------------------------------------------------

(defun json-parse (str)
  "Parse a JSON string into Lisp data.
   Objects become alists, arrays become lists.
   false -> :FALSE, null -> :NULL (to distinguish from NIL/empty list).

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
          (error "json: unexpected content at position ~d" end)))
      val)))

(defun json-get (obj key)
  "Look up KEY (string) in a JSON object (alist). Returns value or NIL."
  (cdr (assoc key obj :test #'string=)))

;;; ---------------------------------------------------------------------------
;;; Serializer
;;; ---------------------------------------------------------------------------

(defun proper-list-p (x)
  "Return T if X is a proper list — every CDR is a cons until NIL.
   Local to the JSON serializer: JSON arrays and objects require
   proper-list shape, and a one-cell LISTP check misses the deeper
   improper case. Input (1 2 . 3) has (CDR x) = (2 . 3) — a cons —
   so LISTP says T, but JSON-WRITE-ARRAY's LOOP would emit a
   trailing comma for the final non-NIL tail. ALSO used to guard
   the alist-detection EVERY call: per CLHS, EVERY on an improper
   list has undefined behavior. Iterative walk so very long lists
   don't blow the stack. No circular-list detection — a circular
   structure fed to JSON-SERIALIZE is pathological regardless."
  (loop
    (cond ((null x) (return t))
          ((atom x) (return nil))
          (t (setf x (cdr x))))))

(defun json-serialize (value)
  "Serialize a Lisp value to a JSON string.
   Alists with string keys serialize as objects; other lists as arrays."
  (with-output-to-string (out)
    (json-write-value value out)))

(defun json-write-value (value stream)
  "Write VALUE as JSON to STREAM."
  (cond
    ((eq value t)      (write-string "true" stream))
    ((eq value :false) (write-string "false" stream))
    ((eq value :null)  (write-string "null" stream))
    ;; NIL serializes as null. Empty {} and [] both parse to NIL,
    ;; so they round-trip to null. Use :NULL for explicit null if needed.
    ((null value)      (write-string "null" stream))
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
    ;; Improper lists reject up front — a dotted tail would emit a
    ;; trailing comma through json-write-array, producing invalid
    ;; JSON. Must come before the cons branches so EVERY on an
    ;; improper alist shape doesn't run with undefined behavior
    ;; (CLHS — EVERY requires proper lists).
    ((and (consp value) (not (proper-list-p value)))
     (error "json: cannot serialize improper list ~s" value))
    ;; Alist (object) — every element must be a cons with a string car.
    ;; Checking only the first element was a footgun: a mixed list like
    ;; '(("foo" . 1) "bar") would route to json-write-object and crash
    ;; mid-serialize. Checking every element falls back to the array
    ;; branch on mixed input, producing well-formed (if surprising) JSON.
    ((and (consp value)
          (every (lambda (el) (and (consp el) (stringp (car el)))) value))
     (json-write-object value stream))
    ;; List (array)
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
  "Write an alist as a JSON object to STREAM."
  (write-char #\{ stream)
  (loop for (pair . rest) on alist
        do (json-write-string (car pair) stream)
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
