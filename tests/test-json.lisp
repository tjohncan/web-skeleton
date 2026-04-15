(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; JSON parser and serializer tests
;;; ===========================================================================

(defun test-json-parse ()
  (format t "~%JSON Parser~%")

  ;; Strings
  (check "string simple"
         (json-parse "\"hello\"") "hello")
  (check "string escapes"
         (json-parse "\"a\\nb\\tc\"")
         (concatenate 'string "a" (string #\Newline) "b" (string #\Tab) "c"))
  (check "string unicode escape"
         (json-parse "\"caf\\u00E9\"")
         (concatenate 'string "caf" (string (code-char #xE9))))
  (check "string slash escape"
         (json-parse "\"a\\/b\"") "a/b")
  (check "string surrogate pair"
         (json-parse "\"\\uD83D\\uDE00\"")
         (string (code-char #x1F600)))

  ;; Numbers
  (check "integer" (json-parse "4444") 4444)
  (check "negative" (json-parse "-17") -17)
  (check "zero" (json-parse "0") 0)

  ;; Booleans and null
  (check "true" (json-parse "true") t)
  (check "false" (json-parse "false") :false)
  (check "null" (json-parse "null") :null)

  ;; Arrays
  (check "empty array" (json-parse "[]") nil)
  (check "int array" (json-parse "[1,2,3]") '(1 2 3))
  (check "mixed array" (json-parse "[1,\"two\",true]") '(1 "two" t))
  (check "nested array" (json-parse "[[1],[2]]") '((1) (2)))

  ;; Objects
  (check "empty object" (json-parse "{}") nil)
  (check "simple object"
         (json-parse "{\"name\":\"ankle\",\"size\":4444}")
         '(("name" . "ankle") ("size" . 4444)))
  (check "nested object"
         (json-get (json-parse "{\"outer\":{\"inner\":\"deep\"}}") "outer")
         '(("inner" . "deep")))

  ;; Whitespace tolerance
  (check "whitespace"
         (json-parse "  { \"a\" : 1 , \"b\" : 2 }  ")
         '(("a" . 1) ("b" . 2)))

  ;; Real-world: JWT header
  (check "jwt header"
         (json-parse "{\"alg\":\"ES256\",\"typ\":\"JWT\"}")
         '(("alg" . "ES256") ("typ" . "JWT")))

  ;; Real-world: JWKS fragment
  (let ((jwks (json-parse "{\"keys\":[{\"kty\":\"EC\",\"crv\":\"P-256\",\"kid\":\"key-1\",\"x\":\"abc\",\"y\":\"def\"}]}")))
    (check "jwks keys array"
           (length (json-get jwks "keys")) 1)
    (check "jwks key kty"
           (json-get (first (json-get jwks "keys")) "kty") "EC")))

(defun test-json-parse-errors ()
  (format t "~%JSON Parser Errors~%")

  (flet ((signals-error-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    (check "leading zeros rejected"
           (signals-error-p (lambda () (json-parse "01"))) t)
    (check "negative leading zeros rejected"
           (signals-error-p (lambda () (json-parse "-01"))) t)
    (check "bare zero allowed"
           (json-parse "0") 0)
    (check "trailing content rejected"
           (signals-error-p (lambda () (json-parse "4444 abc"))) t)
    (check "trailing after array rejected"
           (signals-error-p (lambda () (json-parse "[1,2] 3"))) t)
    (check "lone high surrogate rejected"
           (signals-error-p (lambda () (json-parse "\"\\uD800\""))) t)
    (check "lone low surrogate rejected"
           (signals-error-p (lambda () (json-parse "\"\\uDC00\""))) t)
    (check "high surrogate + non-surrogate rejected"
           (signals-error-p (lambda () (json-parse "\"\\uD800\\u0041\""))) t)
    (check "empty string rejected"
           (signals-error-p (lambda () (json-parse ""))) t)
    (check "truncated object rejected"
           (signals-error-p (lambda () (json-parse "{"))) t)
    (check "truncated array rejected"
           (signals-error-p (lambda () (json-parse "[1,"))) t)
    ;; \uXXXX strict hex validation
    (check "\\u with sign rejected"
           (signals-error-p (lambda () (json-parse "\"\\u+041\""))) t)
    (check "\\u with negative rejected"
           (signals-error-p (lambda () (json-parse "\"\\u-001\""))) t)
    (check "\\u truncated rejected"
           (signals-error-p (lambda () (json-parse "\"\\u00\""))) t)
    ;; Exponent overflow — 1e9999 is syntactically valid per the 20
    ;; exponent-digit cap but overflows IEEE 754. The old path let
    ;; SBCL's reader raise a raw FLOATING-POINT-OVERFLOW out to the
    ;; caller; now it comes back as a friendly parser error.
    (check "exponent overflow rejected"
           (signals-error-p (lambda () (json-parse "1e9999"))) t)
    (check "negative exponent overflow rejected"
           (signals-error-p (lambda () (json-parse "-1e9999"))) t)
    ;; Duplicate keys — RFC 8259 §4 says SHOULD be unique; we say MUST.
    (check "duplicate key rejected"
           (signals-error-p
            (lambda () (json-parse "{\"a\":1,\"a\":2}")))
           t)
    (check "duplicate key rejected nested"
           (signals-error-p
            (lambda () (json-parse "{\"o\":{\"k\":1,\"k\":2}}")))
           t)
    (check "duplicate key across three rejected"
           (signals-error-p
            (lambda () (json-parse "{\"a\":1,\"b\":2,\"a\":3}")))
           t)))

(defun test-json-serialize ()
  (format t "~%JSON Serializer~%")

  ;; Primitives
  (check "ser string" (json-serialize "hello") "\"hello\"")
  (check "ser int" (json-serialize 4444) "4444")
  (check "ser true" (json-serialize t) "true")
  (check "ser false" (json-serialize :false) "false")
  (check "ser null" (json-serialize :null) "null")
  (check "ser nil" (json-serialize nil) "null")
  (check "ser float" (json-serialize 3.14d0) "3.14")
  (check "ser float large" (json-serialize 1.0d7) "1.0e7")
  (check "ser float small" (json-serialize 1.0d-4) "1.0e-4")
  (check "ser float zero" (json-serialize 0.0d0) "0.0")
  (check "ser float round-trip"
         (json-parse (json-serialize 3.14159d0)) 3.14159d0)

  ;; String escaping
  (check "ser escapes"
         (json-serialize (concatenate 'string "a" (string #\Newline)
                                      "b" (string #\Tab) "c"))
         "\"a\\nb\\tc\"")
  (check "ser quotes"
         (json-serialize "say \"hi\"")
         "\"say \\\"hi\\\"\"")

  ;; Arrays
  (check "ser array" (json-serialize '(1 2 3)) "[1,2,3]")
  (check "ser mixed array"
         (json-serialize '(1 "two" t))
         "[1,\"two\",true]")

  ;; Objects (alists with string keys)
  (check "ser object"
         (json-serialize '(("name" . "ankle") ("size" . 4444)))
         "{\"name\":\"ankle\",\"size\":4444}")

  ;; Nested
  (check "ser nested"
         (json-serialize '(("items" . (1 2 3)) ("count" . 3)))
         "{\"items\":[1,2,3],\"count\":3}")

  ;; Round-trip
  (let ((data '(("users" . ((("id" . 1) ("name" . "heel"))
                             (("id" . 2) ("name" . "ankle")))))))
    (check "round-trip"
           (json-parse (json-serialize data))
           data))

  ;; Float error cases
  (flet ((signals-error-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    (check "ser Infinity rejected"
           (signals-error-p
            (lambda ()
              (json-serialize sb-ext:double-float-positive-infinity))) t)))

(defun test-json ()
  (setf *tests-passed* 0
        *tests-failed* 0)
  (format t "~%=== JSON Tests ===~%")
  (test-json-parse)
  (test-json-parse-errors)
  (test-json-serialize)
  (format t "~%~d passed, ~d failed~%~%" *tests-passed* *tests-failed*)
  (zerop *tests-failed*))
