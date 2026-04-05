(in-package :web-skeleton)

;;; ===========================================================================
;;; Server tests — HTTP parser and response builder
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Helper
;;; ---------------------------------------------------------------------------

(defun crlf (&rest strings)
  "Join STRINGS with CRLF and append CRLFCRLF (header terminator)."
  (with-output-to-string (s)
    (loop for (str . rest) on strings
          do (write-string str s)
          when rest do (write-string *crlf* s))
    (write-string *crlf* s)
    (write-string *crlf* s)))

;;; ---------------------------------------------------------------------------
;;; HTTP request parser tests
;;; ---------------------------------------------------------------------------

(defun test-http-parser ()
  (format t "~%HTTP Parser~%")

  ;; Simple GET
  (let ((req (parse-request (crlf "GET / HTTP/1.1" "Host: localhost"))))
    (check "GET / method"    (http-request-method req) :GET)
    (check "GET / path"      (http-request-path req)   "/")
    (check "GET / version"   (http-request-version req) "1.1")
    (check "GET / host"      (get-header req "host")   "localhost"))

  ;; GET with query string
  (let ((req (parse-request (crlf "GET /search?q=lisp&page=1 HTTP/1.1"
                                  "Host: localhost"))))
    (check "query path"   (http-request-path req)  "/search")
    (check "query string" (http-request-query req) "q=lisp&page=1"))

  ;; POST with body
  (let* ((body "name=test&value=123")
         (raw (concatenate 'string
                "POST /submit HTTP/1.1" *crlf*
                "Host: localhost" *crlf*
                "Content-Length: " (write-to-string (length body)) *crlf*
                *crlf*
                body))
         (req (parse-request raw)))
    (check "POST method" (http-request-method req) :POST)
    (check "POST path"   (http-request-path req)   "/submit")
    (check "POST body"   (http-request-body req)   body))

  ;; Multiple headers with same name
  (let ((req (parse-request (crlf "GET / HTTP/1.1"
                                  "Host: localhost"
                                  "X-Custom: one"
                                  "X-Custom: two"))))
    (check "duplicate headers" (get-headers req "x-custom") '("one" "two")))

  ;; Case-insensitive header lookup
  (let ((req (parse-request (crlf "GET / HTTP/1.1" "Content-Type: text/html"))))
    (check "header case insensitive" (get-header req "CONTENT-TYPE") "text/html"))

  ;; All methods
  (dolist (method '("GET" "POST" "PUT" "DELETE" "HEAD" "OPTIONS" "PATCH"))
    (let ((req (parse-request (crlf (format nil "~a / HTTP/1.1" method)
                                    "Host: localhost"))))
      (check (format nil "method ~a" method)
             (http-request-method req)
             (intern method :keyword)))))

(defun test-http-parser-errors ()
  (format t "~%HTTP Parser — rejection~%")

  ;; No CRLFCRLF terminator
  (check-error "missing header terminator"
               (parse-request "GET / HTTP/1.1"))

  ;; Empty request line
  (check-error "empty request line"
               (parse-request (concatenate 'string *crlf* *crlf*)))

  ;; Bad method
  (check-error "invalid method"
               (parse-request (crlf "BOGUS / HTTP/1.1" "Host: localhost")))

  ;; Bad version
  (check-error "bad HTTP version"
               (parse-request (crlf "GET / HTTP/2.0" "Host: localhost")))

  ;; Malformed header
  (check-error "header without colon"
               (parse-request (crlf "GET / HTTP/1.1" "BadHeader")))

  ;; Path not starting with /
  (check-error "path without leading /"
               (parse-request (crlf "GET relative HTTP/1.1" "Host: localhost"))))

;;; ---------------------------------------------------------------------------
;;; HTTP response builder tests
;;; ---------------------------------------------------------------------------

(defun test-http-response ()
  (format t "~%HTTP Response~%")

  ;; Status reasons
  (check "200 reason" (status-reason 200) "OK")
  (check "404 reason" (status-reason 404) "Not Found")
  (check "unknown reason" (status-reason 999) "Unknown")

  ;; Text response
  (let ((resp (make-text-response 200 "hello")))
    (check "text response status" (http-response-status resp) 200)
    (check "text response body" (http-response-body resp) "hello"))

  ;; HTML response contains correct content-type
  (let* ((resp (make-html-response 200 "<h1>hi</h1>"))
         (bytes (format-response resp))
         (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
    (check "html content-type"
           (not (null (search "text/html" text)))
           t)
    (check "html body present"
           (not (null (search "<h1>hi</h1>" text)))
           t))

  ;; Error response
  (let ((resp (make-error-response 404)))
    (check "error status" (http-response-status resp) 404)
    (check "error body" (http-response-body resp) "404 Not Found")))

;;; ---------------------------------------------------------------------------
;;; Runner
;;; ---------------------------------------------------------------------------

(defun test-server ()
  (setf *tests-passed* 0
        *tests-failed* 0)
  (format t "~%=== Server Tests ===~%")
  (test-http-parser)
  (test-http-parser-errors)
  (test-http-response)
  (format t "~%~d passed, ~d failed~%~%" *tests-passed* *tests-failed*)
  (zerop *tests-failed*))
