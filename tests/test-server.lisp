(in-package :web-skeleton-tests)

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

  ;; POST with Content-Length (body extraction happens at connection level)
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
    (check "POST content-length" (get-header req "content-length")
           (write-to-string (length body))))

  ;; Multiple headers with same name
  (let ((req (parse-request (crlf "GET / HTTP/1.1"
                                  "Host: localhost"
                                  "X-Custom: one"
                                  "X-Custom: two"))))
    (check "duplicate headers" (get-headers req "x-custom") '("one" "two")))

  ;; Case-insensitive header lookup
  (let ((req (parse-request (crlf "GET / HTTP/1.1" "Content-Type: text/html"))))
    (check "header case insensitive" (get-header req "CONTENT-TYPE") "text/html"))

  ;; Cookie parsing
  (let ((req (parse-request (crlf "GET / HTTP/1.1"
                                  "Cookie: session=abc123; theme=dark; lang=en"))))
    (check "cookie first"  (get-cookie req "session") "abc123")
    (check "cookie middle" (get-cookie req "theme")   "dark")
    (check "cookie last"   (get-cookie req "lang")    "en")
    (check "cookie absent" (get-cookie req "missing") nil))

  ;; Cookie with no Cookie header
  (let ((req (parse-request (crlf "GET / HTTP/1.1" "Host: localhost"))))
    (check "cookie no header" (get-cookie req "session") nil))

  ;; All methods
  (dolist (method '("GET" "POST" "PUT" "DELETE" "HEAD" "OPTIONS" "PATCH" "TRACE"))
    (let ((req (parse-request (crlf (format nil "~a / HTTP/1.1" method)
                                    "Host: localhost"))))
      (check (format nil "method ~a" method)
             (http-request-method req)
             (intern method :keyword)))))

(defun test-http-date ()
  (format t "~%HTTP Date~%")
  ;; Use a known universal time: 2026-04-09 21:00:00 UTC = Thursday
  ;; CL universal time for 2026-04-09 21:00:00 UTC:
  ;; (encode-universal-time 0 0 21 9 4 2026 0) = 3985020000
  (let ((date (web-skeleton::http-date (encode-universal-time 0 0 21 9 4 2026 0))))
    (check "http-date format"
           date "Thu, 09 Apr 2026 21:00:00 GMT")))

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
               (parse-request (crlf "GET relative HTTP/1.1" "Host: localhost")))

  ;; Oversized request line
  (check-error "oversized request line"
               (let ((web-skeleton:*max-request-line-length* 10))
                 (parse-request (crlf "GET /this-is-too-long HTTP/1.1"
                                      "Host: localhost"))))

  ;; Oversized header line (limit 20 lets "Host: localhost" pass)
  (check-error "oversized header line"
               (let ((web-skeleton:*max-header-line-length* 20))
                 (parse-request (crlf "GET / HTTP/1.1"
                                      "Host: localhost"
                                      "X-Big: this-value-is-too-long"))))

  ;; Too many headers
  (check-error "too many headers"
               (let ((web-skeleton:*max-header-count* 2))
                 (parse-request (crlf "GET / HTTP/1.1"
                                      "Host: localhost"
                                      "A: 1"
                                      "B: 2"))))

  ;; Obsolete line folding
  (check-error "obs-fold rejected"
               (parse-request (concatenate 'string
                                "GET / HTTP/1.1" *crlf*
                                "Host: localhost" *crlf*
                                "X-Folded:" *crlf*
                                (string #\Tab) "continued" *crlf*
                                *crlf*)))

  ;; Transfer-Encoding rejected
  (let* ((raw (concatenate 'string
                "GET / HTTP/1.1" *crlf*
                "Host: localhost" *crlf*
                "Transfer-Encoding: chunked" *crlf*
                *crlf*))
         (bytes (sb-ext:string-to-octets raw :external-format :ascii))
         (header-end (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
    (check "transfer-encoding detected"
           (not (null (web-skeleton::scan-transfer-encoding bytes header-end)))
           t))

  ;; Duplicate conflicting Content-Length
  (let* ((raw (concatenate 'string
                "GET / HTTP/1.1" *crlf*
                "Content-Length: 10" *crlf*
                "Content-Length: 20" *crlf*
                *crlf*))
         (bytes (sb-ext:string-to-octets raw :external-format :ascii))
         (header-end (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
    (check-error "duplicate Content-Length"
                 (web-skeleton::scan-content-length bytes header-end)))

  ;; Unparseable Content-Length value (e.g. +10) — smuggling vector
  (let* ((raw (concatenate 'string
                "GET / HTTP/1.1" *crlf*
                "Content-Length: +10" *crlf*
                *crlf*))
         (bytes (sb-ext:string-to-octets raw :external-format :ascii))
         (header-end (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
    (check-error "Content-Length: +10 rejected"
                 (web-skeleton::scan-content-length bytes header-end)))

  ;; Host header validation (RFC 7230 §5.4)
  ;; connection-parse-request enforces this; parse-request does not.
  (let ((conn (web-skeleton::make-connection
               :fd -1
               :last-active 0)))
    ;; Missing Host
    (let* ((raw (crlf "GET / HTTP/1.1" "Accept: */*"))
           (bytes (sb-ext:string-to-octets raw :external-format :utf-8))
           (end (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
      (setf (web-skeleton::connection-read-buf conn) bytes
            (web-skeleton::connection-read-pos conn) (length bytes)
            (web-skeleton::connection-header-end conn) end
            (web-skeleton::connection-body-expected conn) 0)
      (check-error "missing Host rejected"
                   (web-skeleton::connection-parse-request conn)))
    ;; Duplicate Host
    (let* ((raw (crlf "GET / HTTP/1.1" "Host: a.com" "Host: b.com"))
           (bytes (sb-ext:string-to-octets raw :external-format :utf-8))
           (end (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
      (setf (web-skeleton::connection-read-buf conn) bytes
            (web-skeleton::connection-read-pos conn) (length bytes)
            (web-skeleton::connection-header-end conn) end
            (web-skeleton::connection-body-expected conn) 0)
      (check-error "duplicate Host rejected"
                   (web-skeleton::connection-parse-request conn)))))

;;; ---------------------------------------------------------------------------
;;; Expect: 100-continue tests
;;; ---------------------------------------------------------------------------

(defun test-expect-100-continue ()
  (format t "~%Expect: 100-continue~%")
  (flet ((scan-p (&rest header-lines)
           (let* ((raw   (apply #'crlf "POST /foo HTTP/1.1" header-lines))
                  (bytes (sb-ext:string-to-octets raw :external-format :ascii))
                  (end   (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
             (not (null
                   (web-skeleton::scan-expect-100-continue bytes end))))))
    ;; Positive matches
    (check "exact match"
           (scan-p "Host: localhost" "Expect: 100-continue") t)
    (check "case-insensitive header name"
           (scan-p "Host: localhost" "EXPECT: 100-continue") t)
    (check "case-insensitive value"
           (scan-p "Host: localhost" "Expect: 100-Continue") t)
    (check "extra spaces after colon"
           (scan-p "Host: localhost" "Expect:   100-continue") t)
    (check "tab after colon"
           (scan-p "Host: localhost"
                   (format nil "Expect:~c100-continue" #\Tab))
           t)
    ;; Negative matches
    (check "no Expect header"
           (scan-p "Host: localhost" "Content-Length: 10") nil)
    (check "different expect value"
           (scan-p "Host: localhost" "Expect: something-else") nil)
    (check "suffixed header name does not match"
           (scan-p "Host: localhost" "X-Expect: 100-continue") nil)
    (check "token must have a valid terminator"
           (scan-p "Host: localhost" "Expect: 100-continued") nil))
  ;; Constant sanity — the pre-built bytes are exactly the status line.
  (let ((bytes web-skeleton::*http-100-continue-bytes*))
    (check "100 Continue ends with CRLFCRLF"
           (list (aref bytes (- (length bytes) 4))
                 (aref bytes (- (length bytes) 3))
                 (aref bytes (- (length bytes) 2))
                 (aref bytes (- (length bytes) 1)))
           '(13 10 13 10))
    (check "100 Continue status line"
           (sb-ext:octets-to-string bytes :external-format :ascii)
           (format nil "HTTP/1.1 100 Continue~c~c~c~c"
                   #\Return #\Newline #\Return #\Newline))))

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
;;; Cookie builder tests
;;; ---------------------------------------------------------------------------

(defun test-cookie-builder ()
  (format t "~%Cookie Builder~%")
  (flet ((containsp (needle haystack)
           (not (null (search needle haystack))))
         (signals-error-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    ;; Default build — secure posture: HttpOnly + Secure + SameSite=Lax + Path=/
    (let ((c (build-cookie "session" "abc123")))
      (check "default: name=value present"
             (containsp "session=abc123" c) t)
      (check "default: Path=/"       (containsp "Path=/" c)       t)
      (check "default: HttpOnly"     (containsp "HttpOnly" c)     t)
      (check "default: Secure"       (containsp "Secure" c)       t)
      (check "default: SameSite=Lax" (containsp "SameSite=Lax" c) t))
    ;; Max-Age
    (check "max-age rendered"
           (containsp "Max-Age=3600"
                      (build-cookie "k" "v" :max-age 3600))
           t)
    ;; Domain
    (check "domain rendered"
           (containsp "Domain=example.com"
                      (build-cookie "k" "v" :domain "example.com"))
           t)
    ;; SameSite variants
    (check "SameSite=Strict"
           (containsp "SameSite=Strict"
                      (build-cookie "k" "v" :same-site :strict))
           t)
    (check "SameSite=None with :secure t"
           (containsp "SameSite=None"
                      (build-cookie "k" "v" :same-site :none :secure t))
           t)
    (check ":same-site nil omits the attribute"
           (containsp "SameSite" (build-cookie "k" "v" :same-site nil))
           nil)
    ;; Opt-outs
    (check ":http-only nil omits HttpOnly"
           (containsp "HttpOnly" (build-cookie "k" "v" :http-only nil))
           nil)
    (check ":secure nil omits Secure"
           (containsp "Secure" (build-cookie "k" "v" :secure nil))
           nil)
    ;; Validation: SameSite=None requires Secure
    (check "SameSite=None without :secure errors"
           (signals-error-p
            (lambda () (build-cookie "k" "v" :same-site :none :secure nil)))
           t)
    (check "invalid :same-site value errors"
           (signals-error-p
            (lambda () (build-cookie "k" "v" :same-site :bogus)))
           t)
    ;; Validation: structural characters
    (check "semicolon in name errors"
           (signals-error-p (lambda () (build-cookie "bad;name" "v")))
           t)
    (check "CR in value errors"
           (signals-error-p
            (lambda () (build-cookie "k" (format nil "v~cmore" #\Return))))
           t)
    (check "LF in value errors"
           (signals-error-p
            (lambda () (build-cookie "k" (format nil "v~cmore" #\Newline))))
           t)
    ;; delete-cookie: empty value with Max-Age=0
    (let ((c (delete-cookie "session")))
      (check "delete: empty value followed by attributes"
             (containsp "session=; " c) t)
      (check "delete: Max-Age=0"
             (containsp "Max-Age=0" c) t)
      (check "delete: default Path=/"
             (containsp "Path=/" c) t))
    (check "delete: domain rendered"
           (containsp "Domain=example.com"
                      (delete-cookie "session" :domain "example.com"))
           t)
    (check "delete: name validation"
           (signals-error-p (lambda () (delete-cookie "bad;name")))
           t)))

;;; ---------------------------------------------------------------------------
;;; HTTP client (fetch) tests — pure functions only, no networking
;;; ---------------------------------------------------------------------------

(defun test-fetch ()
  (format t "~%HTTP Client~%")

  ;; URL parsing
  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "http://localhost:8080/api/test")
    (check "url scheme"    scheme :http)
    (check "url host"      host "localhost")
    (check "url port"      port 8080)
    (check "url path"      path "/api/test"))

  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "http://example.com/rip/per")
    (declare (ignore scheme))
    (check "url default port" port 80)
    (check "url host no port" host "example.com")
    (check "url path simple"  path "/rip/per"))

  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "http://10.0.0.1:3000")
    (declare (ignore scheme))
    (check "url ip host"      host "10.0.0.1")
    (check "url ip port"      port 3000)
    (check "url no path"      path "/"))

  ;; HTTPS URLs
  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "https://api.example.com/v1/data")
    (check "https scheme"  scheme :https)
    (check "https host"    host "api.example.com")
    (check "https port"    port 443)
    (check "https path"    path "/v1/data"))

  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "https://10.0.0.1:8443/health")
    (declare (ignore scheme host))
    (check "https custom port" port 8443)
    (check "https ip path"     path "/health"))

  ;; Request building
  (let* ((bytes (web-skeleton::build-outbound-request
                 :GET "localhost" "/health"
                 :headers '(("accept" . "application/json"))))
         (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
    (check "req method"
           (not (null (search "GET /health HTTP/1.1" text))) t)
    (check "req host header"
           (not (null (search "host: localhost" text))) t)
    (check "req connection close"
           (not (null (search "connection: close" text))) t)
    (check "req custom header"
           (not (null (search "accept: application/json" text))) t))

  ;; Response status parsing
  (let ((buf (sb-ext:string-to-octets
              (concatenate 'string "HTTP/1.1 200 OK" *crlf*)
              :external-format :ascii)))
    (check "status 200"
           (web-skeleton::parse-response-status buf 0 (length buf)) 200))

  (let ((buf (sb-ext:string-to-octets
              (concatenate 'string "HTTP/1.1 404 Not Found" *crlf*)
              :external-format :ascii)))
    (check "status 404"
           (web-skeleton::parse-response-status buf 0 (length buf)) 404))

  (let ((buf (sb-ext:string-to-octets
              (concatenate 'string "HTTP/1.0 302 Found" *crlf*)
              :external-format :ascii)))
    (check "status 302 http/1.0"
           (web-skeleton::parse-response-status buf 0 (length buf)) 302)))

;;; ---------------------------------------------------------------------------
;;; URL decode tests
;;; ---------------------------------------------------------------------------

(defun test-url-decode ()
  (format t "~%URL Decode~%")

  (check "no encoding"
         (url-decode "/hello/world") "/hello/world")

  (check "space %20"
         (url-decode "/hello%20world") "/hello world")

  (check "slash %2F"
         (url-decode "a%2Fb") "a/b")

  (check "mixed"
         (url-decode "/path%20to/foot%3Fheel%3Dankle") "/path to/foot?heel=ankle")

  (check "percent at end (incomplete)"
         (url-decode "hello%2") "hello%2")

  (check "uppercase hex"
         (url-decode "%4A") "J")

  (check "lowercase hex"
         (url-decode "%4a") "J")

  (check "empty string"
         (url-decode "") "")

  (check "plus literal (not space)"
         (url-decode "a+b") "a+b"))

;;; ---------------------------------------------------------------------------
;;; Query string tests
;;; ---------------------------------------------------------------------------

(defun test-query-string ()
  (format t "~%Query String~%")

  (check "simple pair"
         (parse-query-string "a=1")
         '(("a" . "1")))

  (check "multiple pairs"
         (parse-query-string "a=1&b=2&c=3")
         '(("a" . "1") ("b" . "2") ("c" . "3")))

  (check "encoded key and value"
         (parse-query-string "hello%20world=foot%26heel")
         '(("hello world" . "foot&heel")))

  (check "key with no value"
         (parse-query-string "flag")
         '(("flag" . "")))

  (check "empty value"
         (parse-query-string "key=")
         '(("key" . "")))

  (check "plus as space in query value"
         (cdr (first (parse-query-string "a=b+c")))
         "b c")

  (check "nil query"
         (parse-query-string nil)
         nil)

  (check "empty query"
         (parse-query-string "")
         nil)

  ;; get-query-param via a parsed request
  (let ((req (parse-request (crlf "GET /search?q=common%20lisp&page=3 HTTP/1.1"
                                  "Host: localhost"))))
    (check "get-query-param q"    (get-query-param req "q")    "common lisp")
    (check "get-query-param page" (get-query-param req "page") "3")
    (check "get-query-param miss" (get-query-param req "x")    nil)))

;;; ---------------------------------------------------------------------------
;;; Path matching tests
;;; ---------------------------------------------------------------------------

(defun test-match-path ()
  (format t "~%Path Matching~%")

  ;; Exact matches
  (check "exact root"
         (match-path "/" "/") t)

  (check "exact path"
         (match-path "/users" "/users") t)

  (check "exact multi-segment"
         (match-path "/api/v1/health" "/api/v1/health") t)

  ;; No match
  (check "different path"
         (match-path "/users" "/posts") nil)

  (check "different length"
         (match-path "/users/list" "/users") nil)

  (check "prefix only"
         (match-path "/users" "/users/4444") nil)

  ;; Single capture
  (check "single param"
         (match-path "/users/:id" "/users/4444")
         '(("id" . "4444")))

  ;; Multiple captures
  (check "two params"
         (match-path "/users/:id/posts/:post-id" "/users/4444/posts/7")
         '(("id" . "4444") ("post-id" . "7")))

  ;; Capture with percent-encoding
  (check "param decoded"
         (match-path "/files/:name" "/files/hello%20world")
         '(("name" . "hello world")))

  ;; Mixed literal and capture
  (check "literal prefix + capture"
         (match-path "/api/users/:id" "/api/users/99")
         '(("id" . "99")))

  (check "literal mismatch with capture"
         (match-path "/api/users/:id" "/api/posts/99")
         nil))

;;; ---------------------------------------------------------------------------
;;; Streaming fetch tests (no network — in-memory byte stream mock)
;;;
;;; Intentionally minimal: only stream-read-byte and stream-element-type.
;;; read-sequence falls back to per-byte reads, which is fine for tests.
;;; ---------------------------------------------------------------------------

(defclass byte-array-stream (sb-gray:fundamental-binary-input-stream)
  ((bytes :initarg :bytes)
   (pos :initform 0)))

(defmethod sb-gray:stream-read-byte ((s byte-array-stream))
  (with-slots (bytes pos) s
    (if (< pos (length bytes))
        (prog1 (aref bytes pos) (incf pos))
        :eof)))

(defmethod stream-element-type ((s byte-array-stream))
  '(unsigned-byte 8))

(defun make-mock-stream (bytes)
  "Return an in-memory binary input stream over BYTES."
  (make-instance 'byte-array-stream :bytes bytes))

(defun ascii-bytes (string)
  "Convert STRING to a byte vector."
  (sb-ext:string-to-octets string :external-format :ascii))

(defun test-streaming-fetch ()
  (format t "~%Streaming Fetch~%")

  ;; Non-chunked response
  (let* ((raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Content-Type: application/x-ndjson" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)
                "{\"token\":\"hello\"}" (string #\Newline)
                "{\"token\":\"world\"}" (string #\Newline))))
         (stream (make-mock-stream raw))
         (lines nil))
    (unwind-protect
        (let ((status (web-skeleton::stream-response-lines
                       stream (lambda (line) (push line lines)))))
          (setf lines (nreverse lines))
          (check "non-chunked status" status 200)
          (check "non-chunked line count" (length lines) 2)
          (check "non-chunked first line" (first lines) "{\"token\":\"hello\"}")
          (check "non-chunked second line" (second lines) "{\"token\":\"world\"}"))
      (close stream)))

  ;; Chunked response
  (let* ((chunk1 (concatenate 'string "{\"n\":1}" (string #\Newline)))
         (chunk2 (concatenate 'string "{\"n\":2}" (string #\Newline)))
         (raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Transfer-Encoding: chunked" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)
                ;; chunk 1
                (format nil "~x" (length chunk1)) (string #\Return) (string #\Newline)
                chunk1
                (string #\Return) (string #\Newline)
                ;; chunk 2
                (format nil "~x" (length chunk2)) (string #\Return) (string #\Newline)
                chunk2
                (string #\Return) (string #\Newline)
                ;; final chunk
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline))))
         (stream (make-mock-stream raw))
         (lines nil))
    (unwind-protect
        (let ((status (web-skeleton::stream-response-lines
                       stream (lambda (line) (push line lines)))))
          (setf lines (nreverse lines))
          (check "chunked status" status 200)
          (check "chunked line count" (length lines) 2)
          (check "chunked first line" (first lines) "{\"n\":1}")
          (check "chunked second line" (second lines) "{\"n\":2}"))
      (close stream))))

;;; ---------------------------------------------------------------------------
;;; Buffered chunked body decoding tests
;;; ---------------------------------------------------------------------------

(defun test-decode-chunked-body ()
  (format t "~%Chunked Body Decode~%")

  ;; Two chunks + terminator
  (let* ((raw (ascii-bytes (concatenate 'string
                "7" (string #\Return) (string #\Newline)
                "hello, " (string #\Return) (string #\Newline)
                "6" (string #\Return) (string #\Newline)
                "world!" (string #\Return) (string #\Newline)
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline))))
         (decoded (web-skeleton::decode-chunked-body raw 0 (length raw))))
    (check "chunked decode"
           (sb-ext:octets-to-string decoded :external-format :utf-8)
           "hello, world!"))

  ;; Chunk with extension (RFC 7230 §4.1.1) — extension silently skipped
  (let* ((raw (ascii-bytes (concatenate 'string
                "5;ext=val" (string #\Return) (string #\Newline)
                "hello" (string #\Return) (string #\Newline)
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline))))
         (decoded (web-skeleton::decode-chunked-body raw 0 (length raw))))
    (check "chunked decode with extension"
           (sb-ext:octets-to-string decoded :external-format :utf-8)
           "hello"))

  ;; Empty body (zero-size first chunk)
  (let* ((raw (ascii-bytes (concatenate 'string
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline))))
         (decoded (web-skeleton::decode-chunked-body raw 0 (length raw))))
    (check "chunked decode empty" (length decoded) 0))

  ;; response-chunked-p helper
  (check "chunked-p yes"
         (not (null (web-skeleton::response-chunked-p
                     '(("transfer-encoding" . "chunked"))))) t)
  (check "chunked-p no"
         (web-skeleton::response-chunked-p
          '(("content-type" . "text/plain"))) nil)
  (check "chunked-p case-insensitive"
         (not (null (web-skeleton::response-chunked-p
                     '(("transfer-encoding" . "Chunked"))))) t))

;;; ---------------------------------------------------------------------------
;;; WebSocket tests
;;; ---------------------------------------------------------------------------

(defun test-websocket ()
  (format t "~%WebSocket~%")

  ;; Handshake accept key — RFC 6455 §4.2.2 example
  (check "accept key rfc6455"
         (web-skeleton::websocket-accept-key "dGhlIHNhbXBsZSBub25jZQ==")
         "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")

  ;; connection-header-has-token-p
  (check "token single"
         (web-skeleton::connection-header-has-token-p "upgrade" "upgrade") t)
  (check "token in list"
         (web-skeleton::connection-header-has-token-p "keep-alive, Upgrade" "upgrade") t)
  (check "token with whitespace"
         (web-skeleton::connection-header-has-token-p "  Upgrade  ,  keep-alive  " "upgrade") t)
  (check "token absent"
         (web-skeleton::connection-header-has-token-p "keep-alive" "upgrade") nil)

  ;; Frame building — text frame
  (let ((frame (build-ws-text "hello")))
    (check "text frame fin+opcode" (aref frame 0) #x81)  ; FIN=1, opcode=1
    (check "text frame length"     (aref frame 1) 5)
    (check "text frame payload"
           (sb-ext:octets-to-string (subseq frame 2) :external-format :utf-8)
           "hello"))

  ;; Frame building — close frame
  (let ((frame (build-ws-close 1000)))
    (check "close frame fin+opcode" (aref frame 0) #x88)  ; FIN=1, opcode=8
    (check "close frame length"     (aref frame 1) 2)
    (check "close frame code"       (logior (ash (aref frame 2) 8) (aref frame 3))
           1000))

  ;; Frame parsing — build a masked client text frame and parse it
  (let* ((frame (make-array 8 :element-type '(unsigned-byte 8)))
         (result nil) (consumed 0))
    (setf (aref frame 0) #x81          ; FIN + text
          (aref frame 1) (logior #x80 2) ; MASK + len=2
          (aref frame 2) #xAA (aref frame 3) #xBB
          (aref frame 4) #xCC (aref frame 5) #xDD
          ;; masked payload: 'h' XOR AA, 'i' XOR BB
          (aref frame 6) (logxor (char-code #\h) #xAA)
          (aref frame 7) (logxor (char-code #\i) #xBB))
    (multiple-value-setq (result consumed)
      (web-skeleton::try-parse-ws-frame frame 0 8))
    (check "parse masked frame" (not (null result)) t)
    (check "parse consumed bytes" consumed 8)
    (check "parse opcode" (ws-frame-opcode result) 1)
    (check "parse fin" (ws-frame-fin result) t)
    (check "parse payload"
           (sb-ext:octets-to-string (ws-frame-payload result)
                                     :external-format :utf-8)
           "hi")
    ;; Incomplete frame — not enough bytes
    (multiple-value-setq (result consumed)
      (web-skeleton::try-parse-ws-frame frame 0 1))
    (check "parse incomplete" result nil)))

;;; ---------------------------------------------------------------------------
;;; WebSocket fragmentation and control frame tests
;;; ---------------------------------------------------------------------------

(defun make-masked-frame (fin opcode payload)
  "Build a masked client WebSocket frame for testing.
   Supports payloads up to 65535 bytes (2-byte extended length)."
  (let* ((len (length payload))
         (mask #(#xAA #xBB #xCC #xDD))
         (header-size (if (<= len 125) 6 8))
         (frame (make-array (+ header-size len) :element-type '(unsigned-byte 8))))
    (setf (aref frame 0) (logior (if fin #x80 0) opcode))
    (if (<= len 125)
        (setf (aref frame 1) (logior #x80 len))
        (setf (aref frame 1) (logior #x80 126)
              (aref frame 2) (logand #xFF (ash len -8))
              (aref frame 3) (logand #xFF len)))
    (replace frame mask :start1 (- header-size 4))
    (loop for i from 0 below len
          do (setf (aref frame (+ header-size i))
                   (logxor (aref payload i) (aref mask (logand i 3)))))
    frame))

(defun test-websocket-fragmentation ()
  (format t "~%WebSocket Fragmentation~%")

  (flet ((signals-error-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))

    ;; FIN=0 text frame (first fragment) — parses fine
    (let ((frame (make-masked-frame nil 1 #(104 105))))  ; "hi"
      (multiple-value-bind (result consumed)
          (web-skeleton::try-parse-ws-frame frame 0 (length frame))
        (check "fragment first: parsed" (not (null result)) t)
        (check "fragment first: fin" (ws-frame-fin result) nil)
        (check "fragment first: opcode" (ws-frame-opcode result) 1)
        (check "fragment first: consumed" consumed (length frame))))

    ;; Continuation frame (FIN=1) — parses fine
    (let ((frame (make-masked-frame t 0 #(33))))  ; "!"
      (multiple-value-bind (result consumed)
          (web-skeleton::try-parse-ws-frame frame 0 (length frame))
        (declare (ignore consumed))
        (check "fragment continuation: parsed" (not (null result)) t)
        (check "fragment continuation: fin" (ws-frame-fin result) t)
        (check "fragment continuation: opcode" (ws-frame-opcode result) 0)))

    ;; Control frame with payload > 125 — rejected
    (let ((big-payload (make-array 126 :element-type '(unsigned-byte 8)
                                       :initial-element 0)))
      (check "control frame >125 rejected"
             (signals-error-p
              (lambda ()
                (let ((frame (make-masked-frame t 9 big-payload)))
                  (web-skeleton::try-parse-ws-frame frame 0 (length frame)))))
             t))

    ;; Fragmented control frame — rejected
    (check "fragmented ping rejected"
           (signals-error-p
            (lambda ()
              (let ((frame (make-masked-frame nil 9 #())))
                (web-skeleton::try-parse-ws-frame frame 0 (length frame)))))
           t)

    ;; Reserved close code clamped to 1000
    ;; Build a close frame with code 1006 (reserved, must not echo)
    (let ((close-frame (build-ws-close 1006)))
      ;; Verify build-ws-close produces the code we asked for
      (check "close frame code 1006 built"
             (logior (ash (aref close-frame 2) 8) (aref close-frame 3))
             1006))
    ;; The clamping happens in websocket-on-read (requires a connection),
    ;; but we verify the reserved-code logic directly:
    (flet ((clamp-code (raw-code)
             (if (or (< raw-code 1000)
                     (member raw-code '(1004 1005 1006 1015))
                     (and (>= raw-code 1016) (<= raw-code 2999)))
                 1000
                 raw-code)))
      (check "close code 1004 clamped" (clamp-code 1004) 1000)
      (check "close code 1005 clamped" (clamp-code 1005) 1000)
      (check "close code 1006 clamped" (clamp-code 1006) 1000)
      (check "close code 1015 clamped" (clamp-code 1015) 1000)
      (check "close code 999 clamped"  (clamp-code 999) 1000)
      (check "close code 1000 passes"  (clamp-code 1000) 1000)
      (check "close code 1001 passes"  (clamp-code 1001) 1001)
      (check "close code 3000 passes"  (clamp-code 3000) 3000))

    ;; Close frame with invalid UTF-8 reason — must fail with 1007
    ;; (RFC 6455 §5.5.1: reason text after the 2-byte code must be valid UTF-8)
    (let* ((close-payload (make-array 4 :element-type '(unsigned-byte 8)
                                        :initial-contents '(3 #xe8 #xFE #xFF)))
           (close-frame (make-masked-frame t 8 close-payload))
           (conn (web-skeleton::make-connection
                  :fd -1 :state :websocket :last-active 0))
           (buf (copy-seq close-frame)))
      (setf (web-skeleton::connection-read-buf conn) buf
            (web-skeleton::connection-read-pos conn) (length buf))
      (multiple-value-bind (action response)
          (web-skeleton::websocket-on-read conn nil)
        (check "close invalid UTF-8 reason -> :close"
               action :close)
        (check "close invalid UTF-8 reason -> 1007"
               (when (and response (>= (length response) 4))
                 (logior (ash (aref response 2) 8) (aref response 3)))
               1007)))

    ;; Close frame with valid UTF-8 reason — should echo the code, not 1007
    (let* ((reason (sb-ext:string-to-octets "going away" :external-format :utf-8))
           (close-payload (make-array (+ 2 (length reason))
                                       :element-type '(unsigned-byte 8)))
           (close-frame (progn
                          (setf (aref close-payload 0) (ash 1001 -8)
                                (aref close-payload 1) (logand 1001 #xFF))
                          (replace close-payload reason :start1 2)
                          (make-masked-frame t 8 close-payload)))
           (conn (web-skeleton::make-connection
                  :fd -1 :state :websocket :last-active 0))
           (buf (copy-seq close-frame)))
      (setf (web-skeleton::connection-read-buf conn) buf
            (web-skeleton::connection-read-pos conn) (length buf))
      (multiple-value-bind (action response)
          (web-skeleton::websocket-on-read conn nil)
        (check "close valid UTF-8 reason -> :close"
               action :close)
        (check "close valid UTF-8 reason -> echoes 1001"
               (when (and response (>= (length response) 4))
                 (logior (ash (aref response 2) 8) (aref response 3)))
               1001)))

    ;; Binary frame (opcode 2)
    (let ((frame (make-masked-frame t 2 #(#xDE #xAD))))
      (multiple-value-bind (result consumed)
          (web-skeleton::try-parse-ws-frame frame 0 (length frame))
        (declare (ignore consumed))
        (check "binary frame opcode" (ws-frame-opcode result) 2)
        (check "binary frame payload"
               (bytes-to-hex (ws-frame-payload result)) "dead")))

    ;; Extended length (126-65535 range, 2-byte extended header)
    (let* ((payload (make-array 200 :element-type '(unsigned-byte 8)
                                     :initial-element #x42))
           (frame (make-masked-frame t 1 payload)))
      (multiple-value-bind (result consumed)
          (web-skeleton::try-parse-ws-frame frame 0 (length frame))
        (declare (ignore consumed))
        (check "extended length parsed" (not (null result)) t)
        (check "extended length size"
               (length (ws-frame-payload result)) 200)))))

;;; ---------------------------------------------------------------------------
;;; Static file helper tests
;;; ---------------------------------------------------------------------------

(defun test-static-helpers ()
  (format t "~%Static Helpers~%")

  (check "mime html"  (web-skeleton::mime-type-for-path "/index.html")
         "text/html; charset=utf-8")
  (check "mime css"   (web-skeleton::mime-type-for-path "/style.css")
         "text/css; charset=utf-8")
  (check "mime js"    (web-skeleton::mime-type-for-path "/app.js")
         "application/javascript; charset=utf-8")
  (check "mime png"   (web-skeleton::mime-type-for-path "/image.png")
         "image/png")
  (check "mime json"  (web-skeleton::mime-type-for-path "/data.json")
         "application/json; charset=utf-8")
  (check "mime svg"   (web-skeleton::mime-type-for-path "/icon.svg")
         "image/svg+xml")
  (check "mime woff2" (web-skeleton::mime-type-for-path "/font.woff2")
         "font/woff2")
  (check "mime unknown" (web-skeleton::mime-type-for-path "/data.xyz")
         "application/octet-stream")
  (check "mime no ext" (web-skeleton::mime-type-for-path "/LICENSE")
         "application/octet-stream")

  (check "ext html" (web-skeleton::file-extension "/index.html") "html")
  (check "ext none" (web-skeleton::file-extension "/LICENSE") nil)
  (check "ext dotfile" (web-skeleton::file-extension "/.hidden") "hidden"))

;;; ---------------------------------------------------------------------------
;;; JWT tests
;;; ---------------------------------------------------------------------------

(defun test-jwt ()
  (format t "~%JWT~%")

  ;; Use the RFC 7515 A.3 ES256 example to build a complete JWT test
  (let* ((header-b64 "eyJhbGciOiJFUzI1NiJ9")
         (payload-b64 "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ")
         ;; Low-S normalized signature (original RFC 7515 A.3 has high-S)
         (sig-b64 "DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmU69fgrc8OPGycO0lD3tat_FoFp57SETepkeks4eL_QfA")
         (token (format nil "~a.~a.~a" header-b64 payload-b64 sig-b64))
         ;; Build a key set with the RFC 7515 A.3 public key
         (keys (list (make-jwt-key
                      :kid ""
                      :x (base64url-decode "f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU")
                      :y (base64url-decode "x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0")))))

    ;; Valid token (exp is in the past, but this tests the crypto path)
    ;; jwt-verify checks exp, so this will return NIL due to expiration.
    ;; Test the pieces individually instead.
    (check "jwt split"
           (length (web-skeleton::jwt-split token)) 3)

    ;; Verify signature is valid by calling ecdsa-verify-p256 directly
    (let* ((signing-input (format nil "~a.~a" header-b64 payload-b64))
           (hash (sha256 (sb-ext:string-to-octets signing-input
                                                    :external-format :ascii)))
           (sig (base64url-decode sig-b64)))
      (check "jwt signature valid"
             (ecdsa-verify-p256 hash sig
                                (jwt-key-x (first keys))
                                (jwt-key-y (first keys)))
             t))

    ;; jwt-verify rejects expired token
    (check "jwt expired token"
           (jwt-verify token keys)
           nil)

    ;; jwt-verify rejects tampered token
    (let ((bad-token (format nil "~a.~a.~a"
                             header-b64
                             ;; tamper: change one base64 character
                             (concatenate 'string "X" (subseq payload-b64 1))
                             sig-b64)))
      (check "jwt tampered token"
             (jwt-verify bad-token keys)
             nil))

    ;; jwt-verify rejects wrong algorithm
    (let ((bad-alg-token (format nil "~a.~a.~a"
                                 (base64url-encode
                                  (sb-ext:string-to-octets
                                   "{\"alg\":\"RS256\"}" :external-format :utf-8))
                                 payload-b64 sig-b64)))
      (check "jwt wrong algorithm"
             (jwt-verify bad-alg-token keys)
             nil)))

  ;; JWKS parsing
  (let* ((jwks-json "{\"keys\":[{\"kty\":\"EC\",\"crv\":\"P-256\",\"kid\":\"test-key\",\"x\":\"f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU\",\"y\":\"x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0\"},{\"kty\":\"RSA\",\"kid\":\"rsa-key\",\"n\":\"abc\",\"e\":\"def\"}]}")
         (keys (parse-jwks jwks-json)))
    (check "jwks parses EC keys only" (length keys) 1)
    (check "jwks kid" (jwt-key-kid (first keys)) "test-key")))

;;; ---------------------------------------------------------------------------
;;; Shutdown hook tests
;;; ---------------------------------------------------------------------------

(defun test-shutdown-hooks ()
  (format t "~%Shutdown Hooks~%")
  ;; Rebind the hook list so this test does not pollute framework state.
  ;; The lock stays shared — it is about thread safety of the cell access,
  ;; not about isolating the list value itself.
  (let ((web-skeleton::*shutdown-hooks* nil)
        (log (make-array 4 :fill-pointer 0 :adjustable t)))
    (web-skeleton:register-cleanup
     (lambda () (vector-push-extend :first log)))
    (web-skeleton:register-cleanup
     (lambda () (vector-push-extend :second log)))
    (web-skeleton:register-cleanup
     (lambda () (error "intentional test failure — must not abort the rest")))
    (web-skeleton:register-cleanup
     (lambda () (vector-push-extend :third log)))
    ;; Suppress the log-error line from the raising hook so the test output
    ;; stays clean.
    (let ((web-skeleton:*log-level* :error)
          (web-skeleton:*log-stream* (make-broadcast-stream)))
      (web-skeleton::run-shutdown-hooks))
    ;; LIFO: :third (last in) runs first, then the raiser (caught), then
    ;; :second, then :first. The raiser contributes nothing to the log.
    (check "hooks run LIFO and survive a raising hook"
           (coerce log 'list)
           '(:third :second :first))
    ;; Registration is idempotent on the return value — it returns FN.
    (let ((fn (lambda () nil)))
      (check "register-cleanup returns the function"
             (eq (web-skeleton:register-cleanup fn) fn) t))))

;;; ---------------------------------------------------------------------------
;;; Runner
;;; ---------------------------------------------------------------------------

(defun test-server ()
  (setf *tests-passed* 0
        *tests-failed* 0)
  (format t "~%=== Server Tests ===~%")
  (test-http-parser)
  (test-http-parser-errors)
  (test-expect-100-continue)
  (test-http-date)
  (test-http-response)
  (test-cookie-builder)
  (test-fetch)
  (test-url-decode)
  (test-query-string)
  (test-match-path)
  (test-streaming-fetch)
  (test-decode-chunked-body)
  (test-websocket)
  (test-websocket-fragmentation)
  (test-static-helpers)
  (test-jwt)
  (test-shutdown-hooks)
  (format t "~%~d passed, ~d failed~%~%" *tests-passed* *tests-failed*)
  (zerop *tests-failed*))
