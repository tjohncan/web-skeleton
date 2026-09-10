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

  ;; Objects — a JSON-OBJECT struct, read via json-get or the alist slot.
  ;; An empty object is an EMPTY object, not NIL: that is what keeps {}
  ;; distinguishable from [] and from null.
  (check "empty object is an empty json-object"
         (json-object-alist (json-parse "{}")) nil)
  (check "empty object is not NIL"
         (null (json-parse "{}")) nil)
  (check "empty object is a json-object"
         (json-object-p (json-parse "{}")) t)
  (check "simple object"
         (json-object-alist (json-parse "{\"name\":\"ankle\",\"size\":4444}"))
         '(("name" . "ankle") ("size" . 4444)))
  (check "nested object"
         (json-object-alist
          (json-get (json-parse "{\"outer\":{\"inner\":\"deep\"}}") "outer"))
         '(("inner" . "deep")))
  (check "json-get reads a json-object"
         (json-get (json-parse "{\"a\":1}") "a") 1)
  ;; json-get still accepts a bare alist so handler code written against
  ;; the old representation keeps working.
  (check "json-get still accepts a bare alist"
         (json-get '(("a" . 1)) "a") 1)
  (check "json-get on a non-object returns nil, does not raise"
         (json-get "not-an-object" "a") nil)
  ;; The array case, which the check above does not reach: a string takes
  ;; JSON-GET's (T NIL) arm, so that assertion passed with or without the
  ;; element test — the docstring names an array explicitly and nothing
  ;; exercised one. ATTEMPT rather than a bare call because the defect is a
  ;; raise, and an uncaught raise ends the run instead of failing a check.
  (check "json-get on an array returns nil, does not raise"
         (attempt (json-get (json-parse "[1,2]") "a")) nil)
  (check "json-get on an array of objects returns nil, does not raise"
         (attempt (json-get (json-parse "[{\"a\":1}]") "a")) nil)
  ;; And the mixed shape PARSE-JWKS actually meets — an object whose value
  ;; is an array — walked one level down.
  (check "json-get through a nested array returns nil, does not raise"
         (attempt (json-get (json-get (json-parse "{\"keys\":[1]}") "keys") "a"))
         nil)
  ;; The other half of the element test, and the half a narrower one would
  ;; have cost silently. JSON-GET was ASSOC :TEST #'STRING=, and STRING= is
  ;; defined on string designators — so a hand-built alist with symbol keys
  ;; has always read through this function, which the docstring's invitation
  ;; to pass a bare alist is what makes reachable. Fixing the array raise
  ;; with STRINGP would have narrowed that away and returned NIL here, which
  ;; is the shape of regression nothing else in this file would have caught.
  (check "json-get answers a symbol key, as string= always has"
         (json-get '((foo . 1)) "FOO") 1)
  (check "json-get answers a character key too"
         (json-get '((#\a . 1)) "a") 1)

  ;; Whitespace tolerance
  (check "whitespace"
         (json-object-alist (json-parse "  { \"a\" : 1 , \"b\" : 2 }  "))
         '(("a" . 1) ("b" . 2)))

  ;; Real-world: JWT header
  (check "jwt header"
         (json-object-alist (json-parse "{\"alg\":\"ES256\",\"typ\":\"JWT\"}"))
         '(("alg" . "ES256") ("typ" . "JWT")))

  ;; Real-world: JWKS fragment. The "keys" value is an ARRAY of objects,
  ;; so it stays a list whose elements are JSON-OBJECTs — exactly the walk
  ;; PARSE-JWKS performs.
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
    ;; exponent-digit cap but overflows IEEE 754. A raw
    ;; FLOATING-POINT-OVERFLOW from SBCL's reader would be useless
    ;; to the app; parser error is what the caller should see.
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
  ;; NIL is the empty list, so it emits []. Explicit null is :NULL and an
  ;; empty object is an empty JSON-OBJECT — the three used to collapse
  ;; onto "null" and now each round-trips to itself.
  (check "ser nil is []" (json-serialize nil) "[]")
  (check "ser empty json-object is {}"
         (json-serialize (make-json-object nil)) "{}")
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

  ;; Improper lists must reject, not silently emit invalid JSON. A
  ;; one-cell LISTP check was the original guard — it only inspected
  ;; (CDR value), so (1 2 . 3) passed through and json-write-array
  ;; emitted "[1,2,]" (trailing comma). Guarded now by PROPER-LIST-P.
  (flet ((signals-error-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    (check "ser dotted pair rejected"
           (signals-error-p (lambda () (json-serialize '(1 . 2)))) t)
    (check "ser improper 3-list rejected"
           (signals-error-p (lambda () (json-serialize '(1 2 . 3)))) t)
    (check "ser improper alist-shape rejected"
           (signals-error-p
            (lambda () (json-serialize '(("a" . 1) ("b" . 2) . 3)))) t))

  ;; Objects — MAKE-JSON-OBJECT is what emits {...}. A bare alist is an
  ;; array of pairs now; see the A2 block below for why.
  (check "ser object"
         (json-serialize
          (make-json-object '(("name" . "ankle") ("size" . 4444))))
         "{\"name\":\"ankle\",\"size\":4444}")

  ;; Nested — an object whose value is an array.
  (check "ser nested"
         (json-serialize
          (make-json-object (list (cons "items" '(1 2 3))
                                  (cons "count" 3))))
         "{\"items\":[1,2,3],\"count\":3}")

  ;; Round-trip through parse: whatever json-parse produces must
  ;; re-serialize to the identical document.
  (let ((doc "{\"users\":[{\"id\":1,\"name\":\"heel\"},{\"id\":2,\"name\":\"ankle\"}]}"))
    (check "round-trip nested objects in an array"
           (json-serialize (json-parse doc)) doc))

  ;; ---- A2: arrays of pairs must stay arrays ----
  ;; The serializer used to decide "object vs array" by testing whether
  ;; every element was a cons with a string car. An array whose elements
  ;; are two-element arrays beginning with a string satisfies that test,
  ;; so [["a",1],["b",2]] came back out as {"a":[1],"b":[2]} — well-formed,
  ;; silently wrong, no error anywhere. That shape is not exotic: it is how
  ;; Object.entries(), tabular payloads, and header lists serialize.
  ;; Objects are a distinct type now, so no guessing happens.
  (dolist (doc '("[[\"a\",1],[\"b\",2]]"
                 "[[\"a\",1]]"
                 "[[\"k\",\"v\"],[\"k2\",\"v2\"],[\"k3\",\"v3\"]]"
                 "{\"a\":{},\"b\":[]}"
                 "{\"a\":null,\"b\":[]}"
                 "[[],{},null]"
                 "{}"
                 "[]"
                 "null"
                 "[{\"a\":1},{\"a\":2}]"
                 "{\"outer\":{\"inner\":[[\"x\",1]]}}"))
    (check (format nil "A2 round-trip ~a" doc)
           (json-serialize (json-parse doc)) doc))

  ;; A hand-built alist is an array of pairs, and a dotted pair is not a
  ;; valid array element — so the mistake surfaces as a loud error naming
  ;; the fix, never as a well-formed-but-wrong document.
  (flet ((error-text (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error (e) (princ-to-string e)))))
    (let ((msg (error-text (lambda () (json-serialize '(("a" . 1)))))))
      (check "ser bare alist raises"
             (not (null msg)) t)
      (check "ser bare alist error names MAKE-JSON-OBJECT"
             (not (null (search "MAKE-JSON-OBJECT" msg))) t)))

  ;; Float error cases
  (flet ((signals-error-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    (check "ser Infinity rejected"
           (signals-error-p
            (lambda ()
              (json-serialize sb-ext:double-float-positive-infinity))) t)
    (check "ser single-float rejected"
           (signals-error-p
            (lambda () (json-serialize 3.14)))
           t))

  ;; Leading UTF-8 BOM (U+FEFF) silently skipped per RFC 8259 §8.1.
  ;; Windows text editors and some encoders prepend one.
  (check "BOM stripped from JSON input"
         (json-object-alist
          (json-parse (format nil "~a{\"x\":1}" (string (code-char #xFEFF)))))
         '(("x" . 1)))
  (check "BOM-only input still errors cleanly"
         (handler-case
             (progn (json-parse (string (code-char #xFEFF))) nil)
           (error () t))
         t)

  ;; *JSON-MAX-STRING-LENGTH* caps the JSON-PARSE-STRING accumulator
  ;; so an attacker-controlled response body cannot force a multi-MiB
  ;; per-string allocation.
  (let ((saved web-skeleton:*json-max-string-length*)
        (at-cap   (make-string 32 :initial-element #\a))
        (over-cap (make-string 33 :initial-element #\a)))
    (setf web-skeleton:*json-max-string-length* 32)
    (unwind-protect
         (progn
           (check "json: string at cap accepted"
                  (json-parse (concatenate 'string "\"" at-cap "\""))
                  at-cap)
           (check "json: string over cap rejected"
                  (handler-case
                      (progn (json-parse
                              (concatenate 'string "\"" over-cap "\""))
                             nil)
                    (error () t))
                  t))
      (setf web-skeleton:*json-max-string-length* saved))))

(defun test-json ()
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
  (format t "~%=== JSON Tests ===~%")
  (test-json-parse)
  (test-json-parse-errors)
  (test-json-serialize)
  (report-suite "JSON")
  (zerop *tests-failed*))
