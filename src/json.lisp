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
   Returns (values string new-pos)."
  (assert (char= (char str pos) #\"))
  (incf pos)
  (let ((out (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t))
        (len (length str)))
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
               (#\n (vector-push-extend #\Newline out))
               (#\t (vector-push-extend #\Tab out))
               (#\r (vector-push-extend #\Return out))
               (#\" (vector-push-extend #\" out))
               (#\\ (vector-push-extend #\\ out))
               (#\/ (vector-push-extend #\/ out))
               (#\b (vector-push-extend (code-char 8) out))
               (#\f (vector-push-extend (code-char 12) out))
               (#\u
                ;; \uXXXX — parse 4 hex digits
                (let ((code (parse-integer str :start (1+ pos)
                                               :end (min (+ pos 5) len)
                                               :radix 16)))
                  ;; Handle surrogate pairs for characters above U+FFFF
                  (cond
                    ((<= #xD800 code #xDBFF)
                     ;; High surrogate — expect \uXXXX low surrogate
                     (let ((low-start (+ pos 5)))
                       (if (and (< (+ low-start 5) len)
                                (char= (char str low-start) #\\)
                                (char= (char str (1+ low-start)) #\u))
                           (let ((low (parse-integer str :start (+ low-start 2)
                                                         :end (+ low-start 6)
                                                         :radix 16)))
                             (unless (<= #xDC00 low #xDFFF)
                               (error "json: expected low surrogate after \\u~4,'0X at ~d"
                                      code (1- pos)))
                             (let ((cp (+ #x10000
                                          (ash (- code #xD800) 10)
                                          (- low #xDC00))))
                               (vector-push-extend (code-char cp) out))
                             (incf pos 10))
                           (error "json: lone high surrogate \\u~4,'0X at ~d"
                                  code (1- pos)))))
                    ((<= #xDC00 code #xDFFF)
                     (error "json: lone low surrogate \\u~4,'0X at ~d"
                            code (1- pos)))
                    (t
                     (vector-push-extend (code-char code) out)
                     (incf pos 4)))))
               (t (error "json: invalid escape \\~c at ~d" esc (1- pos)))))
           (incf pos))
          (t
           (vector-push-extend ch out)
           (incf pos)))))))

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
                 (< (1+ pos) len) (digit-char-p (char str (1+ pos))))
        (error "json: leading zeros not allowed at ~d" start))
      (loop while (and (< pos len) (digit-char-p (char str pos)))
            do (incf pos))
      (when (= pos digit-start)
        (error "json: expected digit at ~d" pos)))
    (when (and (< pos len) (char= (char str pos) #\.))
      (incf pos)
      (let ((frac-start pos))
        (loop while (and (< pos len) (digit-char-p (char str pos)))
              do (incf pos))
        (when (= pos frac-start)
          (error "json: no digits after decimal point at ~d" start))))
    (when (and (< pos len) (member (char str pos) '(#\e #\E)))
      (incf pos)
      (when (and (< pos len) (member (char str pos) '(#\+ #\-)))
        (incf pos))
      (let ((exp-start pos))
        (loop while (and (< pos len) (digit-char-p (char str pos)))
              do (incf pos))
        (when (= pos exp-start)
          (error "json: no digits in exponent at ~d" start))))
    (let ((num-str (subseq str start pos)))
      ;; Floats: read-from-string with *read-eval* NIL is safe here —
      ;; input is pre-validated to [0-9.eE+-] by the loop above.
      ;; Bind *read-default-float-format* to double-float so the CL reader
      ;; produces full 64-bit precision (default is single-float).
      (values (if (or (find #\. num-str) (find #\e num-str) (find #\E num-str))
                  (let* ((*read-default-float-format* 'double-float)
                         (*read-eval* nil)
                         (val (read-from-string num-str)))
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
  "Parse a JSON object at POS. Returns (values alist new-pos)."
  (assert (char= (char str pos) #\{))
  (incf pos)
  (setf pos (json-skip-whitespace str pos))
  (when (>= pos (length str)) (error "json: unterminated object at ~d" pos))
  (when (char= (char str pos) #\})
    (return-from json-parse-object (values nil (1+ pos))))
  (let ((pairs nil))
    (loop
      (setf pos (json-skip-whitespace str pos))
      (when (>= pos (length str)) (error "json: unterminated object at ~d" pos))
      (multiple-value-bind (key new-pos) (json-parse-string str pos)
        (setf pos (json-skip-whitespace str new-pos))
        (when (>= pos (length str)) (error "json: unterminated object at ~d" pos))
        (unless (char= (char str pos) #\:)
          (error "json: expected ':' at ~d" pos))
        (incf pos)
        (multiple-value-bind (val new-pos2) (json-parse-value str pos (1+ depth))
          (push (cons key val) pairs)
          (setf pos (json-skip-whitespace str new-pos2))
          (when (>= pos (length str)) (error "json: unterminated object at ~d" pos))
          (cond
            ((char= (char str pos) #\,) (incf pos))
            ((char= (char str pos) #\})
             (return (values (nreverse pairs) (1+ pos))))
            (t (error "json: expected ',' or '}' at ~d" pos))))))))

(defun json-parse-array (str pos &optional (depth 0))
  "Parse a JSON array at POS. Returns (values list new-pos)."
  (assert (char= (char str pos) #\[))
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
   false -> :FALSE, null -> :NULL (to distinguish from NIL/empty list)."
  (multiple-value-bind (val pos) (json-parse-value str 0)
    (let ((end (json-skip-whitespace str pos)))
      (when (< end (length str))
        (error "json: unexpected content at position ~d" end)))
    val))

(defun json-get (obj key)
  "Look up KEY (string) in a JSON object (alist). Returns value or NIL."
  (cdr (assoc key obj :test #'string=)))

;;; ---------------------------------------------------------------------------
;;; Serializer
;;; ---------------------------------------------------------------------------

(defun json-serialize (value)
  "Serialize a Lisp value to a JSON string."
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
     (when (or (sb-ext:float-nan-p value)
               (sb-ext:float-infinity-p value))
       (error "json-serialize: cannot encode ~a" value))
     ;; Use write-to-string for shortest round-trippable representation.
     ;; CL's ~f produces fixed-point which loses precision at extremes.
     (let* ((*read-default-float-format* 'double-float)
            (s (write-to-string (coerce value 'double-float))))
       ;; CL may produce "1.0d0" — strip the d0 exponent marker
       (let ((d-pos (position #\d s)))
         (write-string (if d-pos
                           (if (string= s "0" :start1 (1+ d-pos))
                               (subseq s 0 d-pos)
                               (concatenate 'string
                                            (subseq s 0 d-pos) "e"
                                            (subseq s (1+ d-pos))))
                           s)
                       stream))))
    ;; Alist (object) — detected by first element being a cons with string car
    ((and (consp value) (consp (car value)) (stringp (caar value)))
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
