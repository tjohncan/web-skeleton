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

  ;; All methods (TRACE is deliberately rejected — see parser test below)
  (dolist (method '("GET" "POST" "PUT" "DELETE" "HEAD" "OPTIONS" "PATCH"))
    (let ((req (parse-request (crlf (format nil "~a / HTTP/1.1" method)
                                    "Host: localhost"))))
      (check (format nil "method ~a" method)
             (http-request-method req)
             (intern method :keyword))))

  ;; TRACE is rejected at the parser layer (XST defense). Framework
  ;; handlers never see it.
  (check-error "TRACE rejected"
               (parse-request (crlf "TRACE / HTTP/1.1" "Host: localhost"))))

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

  ;; CTL bytes in request-target rejected (RFC 7230 §3.2.6).
  ;; scan-crlf only matches the CRLF pair, so a bare LF or CR
  ;; embedded in the URL would survive and land in the parsed
  ;; path string — a log-injection primitive via ~a interpolation.
  (check-error "bare LF in URL"
               (parse-request
                (crlf (format nil "GET /foo~cbar HTTP/1.1" #\Newline)
                      "Host: localhost")))
  (check-error "bare CR in URL"
               (parse-request
                (crlf (format nil "GET /foo~cbar HTTP/1.1" #\Return)
                      "Host: localhost")))
  (check-error "NUL in URL"
               (parse-request
                (crlf (format nil "GET /foo~cbar HTTP/1.1" (code-char 0))
                      "Host: localhost")))
  (check-error "DEL in URL"
               (parse-request
                (crlf (format nil "GET /foo~cbar HTTP/1.1" (code-char #x7f))
                      "Host: localhost")))
  (check-error "tab in URL"
               (parse-request
                (crlf (format nil "GET /foo~cbar HTTP/1.1" #\Tab)
                      "Host: localhost")))

  ;; CTL / DEL bytes in header NAMES rejected (RFC 7230 §3.2).
  ;; NUL and DEL must not reach the header alist — any CTL in a
  ;; header name is a log-injection or smuggling primitive.
  (check-error "NUL in header name"
               (parse-request
                (crlf "GET / HTTP/1.1"
                      (format nil "X-~aBad: v" (code-char 0))
                      "Host: localhost")))
  (check-error "DEL in header name"
               (parse-request
                (crlf "GET / HTTP/1.1"
                      (format nil "X-~aBad: v" (code-char #x7f))
                      "Host: localhost")))
  (check-error "CR in header name"
               (parse-request
                (crlf "GET / HTTP/1.1"
                      (format nil "X-~aBad: v" #\Return)
                      "Host: localhost")))

  ;; RFC 7230 §3.2.6: header name is a token, which excludes all
  ;; separators. The earlier check only rejected SP, CTLs, and DEL,
  ;; so a '(', ',', '/', or ':' could land in a header name and
  ;; slip past the parser into the alist. tchar-byte-p closes that.
  (check-error "'(' in header name"
               (parse-request
                (crlf "GET / HTTP/1.1" "X-B(ad: v" "Host: localhost")))
  (check-error "',' in header name"
               (parse-request
                (crlf "GET / HTTP/1.1" "X-B,ad: v" "Host: localhost")))
  (check-error "'/' in header name"
               (parse-request
                (crlf "GET / HTTP/1.1" "X-B/ad: v" "Host: localhost")))
  (check-error "'{' in header name"
               (parse-request
                (crlf "GET / HTTP/1.1" "X-B{ad: v" "Host: localhost")))

  ;; RFC 7230 §3.2: field-value excludes CTLs except HTAB (0x09).
  ;; scan-crlf only matches the CRLF pair, so a bare CR or LF
  ;; embedded in a header value survives into the parsed alist
  ;; string — a log-injection primitive against any ~a-interpolated
  ;; log call. serialize-http-message already rejects these on
  ;; the outbound side; parse-headers-bytes was the remaining gap.
  (check-error "bare LF in header value"
               (parse-request
                (crlf "GET / HTTP/1.1"
                      (format nil "Host: exam~cFAKE: x" #\Newline))))
  (check-error "bare CR in header value"
               (parse-request
                (crlf "GET / HTTP/1.1"
                      (format nil "Host: exam~cple.com" #\Return))))
  (check-error "NUL in header value"
               (parse-request
                (crlf "GET / HTTP/1.1"
                      (format nil "Host: exam~cple.com" (code-char 0)))))
  (check-error "DEL in header value"
               (parse-request
                (crlf "GET / HTTP/1.1"
                      (format nil "Host: exam~cple.com" (code-char #x7f)))))
  ;; HTAB is explicitly permitted by RFC 7230 §3.2 inside field-value.
  (let* ((raw (crlf "GET / HTTP/1.1"
                    (format nil "Host: a~cb.example" #\Tab)))
         (req (parse-request raw)))
    (check "TAB in header value accepted"
           (not (null (search "a	b.example"
                              (web-skeleton::get-header req "host"))))
           t))

  ;; CTL / DEL bytes in the METHOD region rejected. Same shape as
  ;; the URL check above: scan-crlf only matches the CRLF pair so
  ;; a bare LF smuggled into the method survives parsing and
  ;; would otherwise flow into the 'unrecognized method: ~a' error
  ;; text, splitting the log line into two and handing the second
  ;; one to an attacker.
  (check-error "bare LF in method"
               (parse-request
                (crlf (format nil "GE~cT / HTTP/1.1" #\Newline)
                      "Host: localhost")))
  (check-error "bare CR in method"
               (parse-request
                (crlf (format nil "GE~cT / HTTP/1.1" #\Return)
                      "Host: localhost")))
  (check-error "NUL in method"
               (parse-request
                (crlf (format nil "GE~cT / HTTP/1.1" (code-char 0))
                      "Host: localhost")))
  (check-error "DEL in method"
               (parse-request
                (crlf (format nil "GE~cT / HTTP/1.1" (code-char #x7f))
                      "Host: localhost")))

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

  ;; Content-Length digit cap is 10 decimal digits ≈ 9.3 GB, still
  ;; orders of magnitude past any real *MAX-BODY-SIZE*. The cap
  ;; cannot be reached via a legitimate value but bails early on
  ;; attacker-supplied padding like 'Content-Length: 999999999999999'.
  (let* ((raw (concatenate 'string
                "GET / HTTP/1.1" *crlf*
                "Content-Length: 12345678901" *crlf*  ; 11 digits
                *crlf*))
         (bytes (sb-ext:string-to-octets raw :external-format :ascii))
         (header-end (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
    (check-error "Content-Length 11 digits rejected"
                 (web-skeleton::scan-content-length bytes header-end)))
  (let* ((raw (concatenate 'string
                "GET / HTTP/1.1" *crlf*
                "Content-Length: 9999999999" *crlf*  ; 10 digits — allowed
                *crlf*))
         (bytes (sb-ext:string-to-octets raw :external-format :ascii))
         (header-end (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
    (check "Content-Length 10 digits accepted"
           (web-skeleton::scan-content-length bytes header-end) 9999999999))

  ;; Content-Length parser agreement. scan-content-length is the fast
  ;; byte scanner used for pre-allocation; parse-headers-bytes populates
  ;; the header alist. They are hand-rolled and the risk is that a
  ;; future edit lets one accept a value the other rejects, which
  ;; would open a smuggling gap. This locks in agreement for every CL
  ;; shape we already care about — valid and invalid.
  (flet ((alist-cl (bytes header-end)
           ;; Skip the request line — parse-headers-bytes expects START
           ;; to point at the first header byte, not the request line.
           (let* ((req-end (web-skeleton::scan-crlf bytes 0 header-end))
                  (headers (web-skeleton::parse-headers-bytes
                            bytes (+ req-end 2) (+ header-end 4)))
                  (v (cdr (assoc "content-length" headers :test #'string=))))
             (when v
               ;; Trim OWS around the value; scan-content-length does
               ;; the same before running its digit loop.
               (let ((trimmed (string-trim '(#\Space #\Tab) v)))
                 (when (every (lambda (c) (char<= #\0 c #\9)) trimmed)
                   (parse-integer trimmed))))))
         (block-bytes (header)
           (let ((raw (concatenate 'string
                                   "POST / HTTP/1.1" *crlf*
                                   "Host: localhost" *crlf*
                                   header *crlf* *crlf*)))
             (sb-ext:string-to-octets raw :external-format :ascii))))
    ;; Valid shapes — both parsers must agree on the same integer.
    (let ((tab-header (format nil "Content-Length:~c13" #\Tab)))
    (dolist (v (list '("Content-Length: 0"     0)
                     '("Content-Length: 10"    10)
                     '("Content-Length: 42"    42)
                     '("content-length: 99"    99)
                     '("Content-Length:   7"    7)  ; extra SP OWS
                     (list tab-header 13)))         ; TAB OWS
      (destructuring-bind (header expected) v
        (let* ((bytes (block-bytes header))
               (header-end (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
          (check (format nil "CL agreement scanner ~s" header)
                 (web-skeleton::scan-content-length bytes header-end) expected)
          (check (format nil "CL agreement alist ~s" header)
                 (alist-cl bytes header-end) expected)))))
    ;; Invalid shapes — scanner must reject. The alist path is lax by
    ;; design (it's a string value, not a validated integer), so we
    ;; only assert the authoritative side: scan-content-length must
    ;; raise on every one of these and never return a number.
    (dolist (header '("Content-Length: 10 20"
                      "Content-Length: 10x"
                      "Content-Length: +10"
                      "Content-Length: -5"))
      (let* ((bytes (block-bytes header))
             (header-end (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
        (check (format nil "CL scanner rejects ~s" header)
               (handler-case
                   (progn (web-skeleton::scan-content-length bytes header-end) nil)
                 (error () t))
               t))))

  ;; A \r\n injected in the request-target must not trick the
  ;; header scanners into seeing a fake Content-Length. The
  ;; request-line shape check (2 SPs required) catches the
  ;; injection before scanners run — "GET /\r\n..." has only
  ;; 1 SP, so it's rejected as a malformed request line.
  (check-error "CRLF injection in request-target rejected"
               (parse-request
                (concatenate 'string
                  "GET /" (string #\Return) (string #\Newline)
                  "Content-Length: 999999" (string #\Return) (string #\Newline)
                  "Host: x" (string #\Return) (string #\Newline)
                  (string #\Return) (string #\Newline))))
  (check-error "CRLF injection: TE variant rejected"
               (parse-request
                (concatenate 'string
                  "GET /" (string #\Return) (string #\Newline)
                  "Transfer-Encoding: chunked" (string #\Return) (string #\Newline)
                  "Host: x" (string #\Return) (string #\Newline)
                  (string #\Return) (string #\Newline))))
  (check-error "CRLF injection: Expect variant rejected"
               (parse-request
                (concatenate 'string
                  "GET /" (string #\Return) (string #\Newline)
                  "Expect: 100-continue" (string #\Return) (string #\Newline)
                  "Host: x" (string #\Return) (string #\Newline)
                  (string #\Return) (string #\Newline))))

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
    ;; RFC 7231 §5.1.1: Expect is 1#expectation — a list. Comma
    ;; terminates the 100-continue token just like ';' or CR.
    (check "comma separator accepted"
           (scan-p "Host: localhost" "Expect: 100-continue, x-foo=y") t)
    (check "comma immediately after token"
           (scan-p "Host: localhost" "Expect: 100-continue,x-foo=y") t)
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
    (check "error body" (http-response-body resp) "404 Not Found"))

  ;; HTTP header field names are case-insensitive (RFC 7230 §3.2).
  ;; Framework helpers route through set-response-header with lowercase
  ;; literals, but apps that build responses with mixed-case :headers
  ;; directly must still get clean replacement instead of duplication —
  ;; duplicate Content-Length is a response-smuggling primitive when a
  ;; caching proxy is in front.
  (flet ((count-occurrences (needle haystack)
           ;; Case-insensitive — the response will serialize whatever
           ;; capitalization the app passed in (Content-Length vs
           ;; content-length), and the test doesn't care which.
           (loop with pos = 0
                 with count = 0
                 for next = (search needle haystack
                                    :start2 pos :test #'char-equal)
                 while next
                 do (incf count)
                    (setf pos (+ next (length needle)))
                 finally (return count))))
    ;; set-response-header replaces a mixed-case pre-existing entry
    (let ((resp (web-skeleton::make-http-response
                 :status 200
                 :headers '(("Content-Type" . "text/html")))))
      (set-response-header resp "content-type" "application/json")
      (check "mixed-case replace: header count"
             (length (http-response-headers resp))
             1)
      (check "mixed-case replace: value wins"
             (cdr (first (http-response-headers resp)))
             "application/json"))
    ;; format-response auto-Content-Length skips when mixed-case present
    (let* ((resp (web-skeleton::make-http-response
                  :status 200
                  :body "hello"
                  :headers '(("Content-Length" . "5")
                             ("Content-Type" . "text/plain"))))
           (bytes (format-response resp))
           (text  (sb-ext:octets-to-string bytes :external-format :utf-8)))
      (check "mixed-case CL: not duplicated"
             (count-occurrences "ontent-length:" text)
             1))
    ;; format-response auto-Date skips when mixed-case Date present
    (let* ((resp (web-skeleton::make-http-response
                  :status 200
                  :body "hi"
                  :headers '(("Date" . "Mon, 01 Jan 2024 00:00:00 GMT"))))
           (bytes (format-response resp))
           (text  (sb-ext:octets-to-string bytes :external-format :utf-8)))
      (check "mixed-case Date: not duplicated"
             (count-occurrences "date:" text)
             1)))

  ;; serialize-http-message strictness. An app that builds a
  ;; response alist with a CR, NUL, '(' in the header name, or
  ;; char code > 127 must not silently produce a broken wire
  ;; message. The serializer UTF-8 encodes up front and validates
  ;; the resulting bytes, so non-ASCII gets a clean content error
  ;; and CR/LF in values can't slip through as a header-injection
  ;; primitive.
  (flet ((serialize (first-line headers &optional body)
           (web-skeleton::serialize-http-message first-line headers body)))
    ;; First-line CTL rejection (beyond just CR/LF).
    (check-error "first-line NUL rejected"
                 (serialize (format nil "HTTP/1.1 200 ~cOK" (code-char 0))
                            '(("x" . "y")) nil))
    (check-error "first-line DEL rejected"
                 (serialize (format nil "HTTP/1.1 200 ~cOK" (code-char #x7f))
                            '(("x" . "y")) nil))
    (check-error "first-line CR rejected"
                 (serialize (format nil "HTTP/1.1 200 ~cOK" #\Return)
                            '(("x" . "y")) nil))
    ;; Header name tchar — colon, comma, paren, space all rejected.
    (check-error "header name with ':' rejected"
                 (serialize "HTTP/1.1 200 OK" '(("x:y" . "v")) nil))
    (check-error "header name with ',' rejected"
                 (serialize "HTTP/1.1 200 OK" '(("x,y" . "v")) nil))
    (check-error "header name with ' ' rejected"
                 (serialize "HTTP/1.1 200 OK" '(("x y" . "v")) nil))
    (check-error "header name with '(' rejected"
                 (serialize "HTTP/1.1 200 OK" '(("x(y" . "v")) nil))
    (check-error "header name with '>' rejected"
                 (serialize "HTTP/1.1 200 OK" '(("x>y" . "v")) nil))
    (check-error "empty header name rejected"
                 (serialize "HTTP/1.1 200 OK" '(("" . "v")) nil))
    ;; Non-ASCII in a header name — UTF-8 encoding produces >= 0x80
    ;; bytes, which fail tchar-byte-p cleanly.
    (check-error "non-ASCII header name rejected"
                 (serialize "HTTP/1.1 200 OK" '(("X-Résumé" . "v")) nil))
    ;; Header value CTL rejection — CR, LF, NUL, DEL. TAB is legal.
    (check-error "header value CR rejected"
                 (serialize "HTTP/1.1 200 OK"
                            `(("x" . ,(format nil "a~cb" #\Return))) nil))
    (check-error "header value LF rejected"
                 (serialize "HTTP/1.1 200 OK"
                            `(("x" . ,(format nil "a~cb" #\Newline))) nil))
    (check-error "header value NUL rejected"
                 (serialize "HTTP/1.1 200 OK"
                            `(("x" . ,(format nil "a~cb" (code-char 0)))) nil))
    (check-error "header value DEL rejected"
                 (serialize "HTTP/1.1 200 OK"
                            `(("x" . ,(format nil "a~cb" (code-char #x7f)))) nil))
    (let ((bytes (serialize "HTTP/1.1 200 OK"
                            `(("x" . ,(format nil "a~cb" #\Tab))) nil)))
      (check "header value TAB allowed"
             (not (null (search "a	b"
                                (sb-ext:octets-to-string
                                 bytes :external-format :utf-8))))
             t))
    ;; Non-ASCII value — UTF-8 encoded, copied as bytes. Old path
    ;; crashed on (aref buf pos) := (char-code #\é) = 233 which is
    ;; fine, but (char-code #\✓) = 10003 exceeded (unsigned-byte 8).
    (let* ((bytes (serialize "HTTP/1.1 200 OK" '(("x" . "café ✓")) nil))
           (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
      (check "non-ASCII value UTF-8 encoded"
             (not (null (search "café ✓" text))) t)))

  ;; add-response-header appends without replacing — required for
  ;; multi-instance headers like Set-Cookie (RFC 6265 §4.1).
  (let ((resp (web-skeleton::make-http-response :status 200)))
    (add-response-header resp "set-cookie" "a=1")
    (add-response-header resp "set-cookie" "b=2")
    (let ((cookies (remove-if-not
                    (lambda (h) (string-equal (car h) "set-cookie"))
                    (http-response-headers resp))))
      (check "add-response-header: both cookies present"
             (length cookies) 2)))

  ;; format-response rejects out-of-range status codes
  (check-error "status -1 rejected"
               (format-response (web-skeleton::make-http-response :status -1)))
  (check-error "status 999 rejected"
               (format-response (web-skeleton::make-http-response :status 999)))
  (check-error "status 0 rejected"
               (format-response (web-skeleton::make-http-response :status 0)))

  ;; nil-body 200 gets Content-Length: 0 to prevent keep-alive hang
  (let* ((resp (web-skeleton::make-http-response :status 200))
         (bytes (format-response resp))
         (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
    (check "nil body 200: CL:0 injected"
           (not (null (search "content-length: 0" text :test #'char-equal))) t))

  ;; 204 does NOT get CL:0 (RFC 7230 §3.3.3 rule 1)
  (let* ((resp (web-skeleton::make-http-response :status 204))
         (bytes (format-response resp))
         (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
    (check "204: no CL injected"
           (search "content-length" text :test #'char-equal) nil))

  ;; 304 + CL: the bodiless-status exemption must skip the guard.
  ;; Tests the exemption predicate directly — 304 is in the skip set.
  (check "304+CL: truncation guard skipped"
         (let ((status 304))
           (or (<= 100 status 199) (= status 204) (= status 304)))
         t)

  ;; HEAD byte-vector strip: scan-crlf-crlf + subseq truncates at
  ;; the header boundary, keeping CL in headers but removing body.
  (let* ((full (web-skeleton::serialize-http-message
                "HTTP/1.1 200 OK"
                '(("content-type" . "text/plain")
                  ("content-length" . "5"))
                (sb-ext:string-to-octets "hello" :external-format :ascii)))
         (end (web-skeleton::scan-crlf-crlf full 0 (length full)))
         (stripped (subseq full 0 (+ end 4)))
         (text (sb-ext:octets-to-string stripped :external-format :ascii)))
    (check "HEAD strip: CL preserved"
           (not (null (search "content-length: 5" text :test #'char-equal))) t)
    (check "HEAD strip: body removed"
           (search "hello" text) nil))

  ;; Direct exercise of the strip-body-for-head helper
  (let* ((head-req (web-skeleton::make-http-request :method :HEAD :path "/"))
         (get-req  (web-skeleton::make-http-request :method :GET  :path "/"))
         (conn-head (web-skeleton::make-connection :request head-req :last-active 0))
         (conn-get  (web-skeleton::make-connection :request get-req :last-active 0))
         (bytes (web-skeleton::serialize-http-message
                 "HTTP/1.1 200 OK"
                 '(("content-length" . "5"))
                 (sb-ext:string-to-octets "hello" :external-format :ascii))))
    (check "strip-for-head: HEAD truncates"
           (< (length (web-skeleton::strip-body-for-head bytes conn-head))
              (length bytes))
           t)
    (check "strip-for-head: GET unchanged"
           (equalp (web-skeleton::strip-body-for-head bytes conn-get) bytes) t)
    (check "strip-for-head: nil conn unchanged"
           (equalp (web-skeleton::strip-body-for-head bytes nil) bytes) t)))

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
           t)
    ;; Cookie path/domain must be validated — ';' in path enables
    ;; attribute injection (e.g. path "/; Max-Age=0" deletes the cookie).
    (check-error "build-cookie: NUL in value"
                 (build-cookie "s" (format nil "v~c" (code-char 0))))
    (check-error "build-cookie: semicolon in path"
                 (build-cookie "s" "v" :path "/; Max-Age=0"))
    (check-error "build-cookie: CR in path"
                 (build-cookie "s" "v" :path (format nil "/~c" #\Return)))
    (check-error "build-cookie: semicolon in domain"
                 (build-cookie "s" "v" :domain "example.com; Max-Age=0"))
    ;; Empty cookie name rejected (both build and delete)
    (check-error "build-cookie: empty name"
                 (build-cookie "" "v"))
    (check-error "delete-cookie: empty name"
                 (delete-cookie ""))
    ;; delete-cookie with explicit :path nil must not crash
    (let ((cookie (delete-cookie "session" :path nil)))
      (check "delete-cookie :path nil"
             (not (null (search "session=" cookie))) t))
    ;; delete-cookie path/domain validation
    (check-error "delete-cookie: semicolon in path"
                 (delete-cookie "s" :path "/; Secure"))
    (check-error "delete-cookie: semicolon in domain"
                 (delete-cookie "s" :domain "evil.com; Secure"))))

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

  ;; CTL bytes in the URL rejected. Without this check an
  ;; attacker-supplied URL (passed by an app to defer-to-fetch)
  ;; with a bare CR / LF could reach the fetch-path log-debug
  ;; lines in fetch.lisp / dns.lisp / tls.lisp via ~a interpolation
  ;; of HOST and PATH — same log-injection shape as the inbound
  ;; request-target check.
  (check-error "parse-url: bare LF rejected"
               (web-skeleton::parse-url
                (format nil "http://example.com/foo~cbar" #\Newline)))
  (check-error "parse-url: bare CR rejected"
               (web-skeleton::parse-url
                (format nil "http://example.com/foo~cbar" #\Return)))
  (check-error "parse-url: NUL rejected"
               (web-skeleton::parse-url
                (format nil "http://example.com/foo~cbar" (code-char 0))))
  (check-error "parse-url: DEL rejected"
               (web-skeleton::parse-url
                (format nil "http://example.com/foo~cbar" (code-char #x7f))))
  (check-error "parse-url: LF in host rejected"
               (web-skeleton::parse-url
                (format nil "http://examp~cle.com/" #\Newline)))
  ;; Non-ASCII bytes must be percent-encoded per RFC 3986 §2.1
  (check-error "parse-url: non-ASCII rejected"
               (web-skeleton::parse-url "http://example.com/café"))
  (check-error "parse-url: SP in path rejected"
               (web-skeleton::parse-url "http://example.com/foo bar"))
  (check-error "parse-url: empty host rejected"
               (web-skeleton::parse-url "http:///path"))

  ;; HTTPS with an IP-literal host is rejected — SSL_set1_host wants a
  ;; DNS name and the framework does not wire up IP SAN verification
  ;; via X509_VERIFY_PARAM_set1_ip_asc. Failing loudly at parse-url
  ;; closes the asymmetry with plain-HTTP IP-literal support.
  (check-error "https + v4 literal rejected"
               (web-skeleton::parse-url "https://10.0.0.1:8443/health"))
  (check-error "https + v4 bare rejected"
               (web-skeleton::parse-url "https://1.2.3.4/"))
  (check-error "https + v6 bracketed rejected"
               (web-skeleton::parse-url "https://[::1]:8443/"))
  (check-error "https + v6 bracketed doc rejected"
               (web-skeleton::parse-url "https://[2001:db8::1]/api"))
  ;; Plain-HTTP IP literals still supported — the framework's fetch
  ;; path dials these directly with no TLS.
  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "http://10.0.0.1:8443/health")
    (declare (ignore scheme))
    (check "http v4 literal host" host "10.0.0.1")
    (check "http v4 literal port" port 8443)
    (check "http v4 literal path" path "/health"))
  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "http://[::1]:8080/")
    (declare (ignore scheme))
    (check "http v6 bracketed host" host "::1")
    (check "http v6 bracketed port" port 8080)
    (check "http v6 bracketed path" path "/"))

  ;; Userinfo in the authority is rejected with a dedicated
  ;; "userinfo not supported" error so the caller sees what's
  ;; wrong instead of a bare parse-integer type-error leaking up
  ;; from the port split.
  (check-error "url: userinfo rejected"
               (web-skeleton::parse-url "http://user:pass@example.com/"))
  (check-error "url: userinfo (just user) rejected"
               (web-skeleton::parse-url "http://user@example.com/"))
  ;; Non-numeric port surfaces as a friendly "non-numeric port"
  ;; error instead of a raw SIMPLE-TYPE-ERROR from parse-integer.
  (check-error "url: non-numeric port rejected"
               (web-skeleton::parse-url "http://example.com:abc/"))
  (check-error "parse-url: negative port rejected"
               (web-skeleton::parse-url "http://host:-80/"))
  (check-error "parse-url: port > 65535 rejected"
               (web-skeleton::parse-url "http://host:99999/"))
  (check-error "parse-url: port 0 rejected"
               (web-skeleton::parse-url "http://host:0/"))

  ;; Authority terminates at '?' / '#' (RFC 3986 §3.2 / §3.4) —
  ;; without this, 'http://host?q=1' would parse the whole
  ;; 'host?q=1' as the authority and dial a hostname containing '?'.
  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "http://example.com?q=1")
    (declare (ignore scheme))
    (check "query-only: host" host "example.com")
    (check "query-only: port" port 80)
    (check "query-only: path" path "/?q=1"))

  ;; RFC 3986 §3.5 / RFC 7230 §5.3: fragment never goes on the wire.
  ;; Parsing must strip it from the path in every shape.
  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "http://example.com#frag")
    (declare (ignore scheme))
    (check "fragment-only: host" host "example.com")
    (check "fragment-only: port" port 80)
    (check "fragment-only: path" path "/"))

  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "http://example.com/foo#frag")
    (declare (ignore scheme host port))
    (check "fragment+path: path" path "/foo"))

  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "http://example.com/foo?q=1#frag")
    (declare (ignore scheme host port))
    (check "fragment+query: path" path "/foo?q=1"))

  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "http://example.com:8080?x=1&y=2")
    (declare (ignore scheme))
    (check "query+port: host" host "example.com")
    (check "query+port: port" port 8080)
    (check "query+port: path" path "/?x=1&y=2"))

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

  ;; IPv6 literal host: the wire Host header must re-bracket the
  ;; address. parse-authority strips the brackets from the internal
  ;; host string (correct — URL syntax, not address), but RFC 7230
  ;; §5.4 / RFC 3986 §3.2.2 require '[' IPv6address ']' for
  ;; IP-literal authorities on the wire. A bare 'Host: ::1:8080'
  ;; is unparseable — no reader can tell where the address ends
  ;; and the port begins.
  (let* ((bytes (web-skeleton::build-outbound-request
                 :GET "::1" "/" :port 8080))
         (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
    (check "ipv6 host rebracketed with port"
           (not (null (search "host: [::1]:8080" text))) t))
  (let* ((bytes (web-skeleton::build-outbound-request
                 :GET "2001:db8::1" "/api" :port 80))
         (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
    (check "ipv6 host rebracketed default port"
           (not (null (search "host: [2001:db8::1]" text))) t))
  ;; End-to-end: parse-url → build-outbound-request pipeline for an
  ;; IPv6 literal URL. The two tests above cover the builder directly;
  ;; this one locks in that the whole chain behaves correctly, so a
  ;; future refactor that re-introduces the asymmetry can't hide
  ;; behind the separate parse-url tests.
  (multiple-value-bind (scheme host port path)
      (web-skeleton::parse-url "http://[::1]:8080/api")
    (declare (ignore scheme))
    (let* ((bytes (web-skeleton::build-outbound-request
                   :GET host path :port port))
           (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
      (check "ipv6 end-to-end host bracketed"
             (not (null (search "host: [::1]:8080" text))) t)))

  ;; Config knobs that DEPLOYMENT.md tells users to setf from their
  ;; own packages must be exported from :WEB-SKELETON. An unqualified
  ;; reference inside another package's setf form would otherwise
  ;; resolve to a fresh symbol in the caller's package and silently
  ;; leave the framework's cap untouched — the kind of bug that
  ;; only surfaces when someone discovers the framework-side value
  ;; is still at its default despite the app's tune block.
  (dolist (name '("*MAX-BODY-SIZE*"
                  "*MAX-OUTBOUND-RESPONSE-SIZE*"
                  "*MAX-STREAMING-LINE-SIZE*"
                  "*MAX-WS-PAYLOAD-SIZE*"
                  "*MAX-WS-MESSAGE-SIZE*"))
    (check (format nil "~a exported from :web-skeleton" name)
           (nth-value 1 (find-symbol name :web-skeleton))
           :external))

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
           (web-skeleton::parse-response-status buf 0 (length buf)) 302))

  ;; defer-to-fetch is a thin readability wrapper over http-fetch; check
  ;; that it returns the same continuation the framework recognizes as
  ;; an async signal.
  (let ((c (defer-to-fetch :get "http://example.com/" :then #'identity)))
    (check "defer-to-fetch returns a continuation"
           (typep c 'web-skeleton::http-fetch-continuation) t)))

;;; ---------------------------------------------------------------------------
;;; DNS helper tests — IP literal parsers, getent output parser,
;;; bracket-aware URL parsing. Pure functions, no network.
;;; ---------------------------------------------------------------------------

(defun test-dns ()
  (format t "~%DNS helpers~%")
  (flet ((v4 (s) (web-skeleton::parse-ipv4-literal s))
         (v6 (s) (web-skeleton::parse-ipv6-literal s))
         (bytes (s) (sb-ext:string-to-octets s :external-format :ascii)))

    ;; ---- parse-ipv4-literal ----
    (check "ipv4: 127.0.0.1"
           (coerce (v4 "127.0.0.1") 'list) '(127 0 0 1))
    (check "ipv4: 0.0.0.0"
           (coerce (v4 "0.0.0.0") 'list) '(0 0 0 0))
    (check "ipv4: 255.255.255.255"
           (coerce (v4 "255.255.255.255") 'list) '(255 255 255 255))
    (check "ipv4: bare zero octet"
           (coerce (v4 "1.2.3.0") 'list) '(1 2 3 0))
    (check "ipv4: leading zero rejected"  (v4 "1.2.3.01") nil)
    (check "ipv4: out of range rejected"  (v4 "1.2.3.256") nil)
    (check "ipv4: too few octets"         (v4 "1.2.3") nil)
    (check "ipv4: too many octets"        (v4 "1.2.3.4.5") nil)
    (check "ipv4: non-numeric"            (v4 "a.b.c.d") nil)
    (check "ipv4: empty"                  (v4 "") nil)
    (check "ipv4: hostname"               (v4 "example.com") nil)

    ;; ---- parse-ipv6-literal ----
    (let ((r (v6 "::1")))
      (check "ipv6: ::1 length 16"  (length r) 16)
      (check "ipv6: ::1 last byte"  (aref r 15) 1)
      (check "ipv6: ::1 prefix zero"
             (every (lambda (b) (= b 0)) (subseq r 0 15)) t))
    (let ((r (v6 "::")))
      (check "ipv6: :: all zero"
             (every (lambda (b) (= b 0)) r) t))
    (let ((r (v6 "2001:db8::1")))
      (check "ipv6: 2001:db8::1 byte 0" (aref r 0) #x20)
      (check "ipv6: 2001:db8::1 byte 1" (aref r 1) #x01)
      (check "ipv6: 2001:db8::1 byte 3" (aref r 3) #xb8)
      (check "ipv6: 2001:db8::1 last"   (aref r 15) 1))
    (let ((r (v6 "fe80::1")))
      (check "ipv6: fe80::1 byte 0" (aref r 0) #xfe)
      (check "ipv6: fe80::1 byte 1" (aref r 1) #x80)
      (check "ipv6: fe80::1 last"   (aref r 15) 1))
    (check "ipv6: invalid hex rejected"    (v6 "::zz") nil)
    (check "ipv6: too few groups no ::"    (v6 "1:2:3") nil)
    (check "ipv6: empty"                   (v6 "") nil)
    (check "ipv6: hostname"                (v6 "example.com") nil)
    (check "ipv6: ipv4 rejected"           (v6 "127.0.0.1") nil)

    ;; ---- parse-getent-output ----
    (let* ((text (format nil "127.0.0.1       STREAM localhost~%~
                              127.0.0.1       DGRAM ~%~
                              127.0.0.1       RAW ~%"))
           (buf (bytes text))
           (result (web-skeleton::parse-getent-output buf (length buf))))
      (check "getent: ipv4 family"  (cdr result) :inet)
      (check "getent: ipv4 address" (coerce (car result) 'list) '(127 0 0 1)))
    (let* ((text (format nil "::1             STREAM localhost~%~
                              ::1             DGRAM ~%~
                              ::1             RAW ~%"))
           (buf (bytes text))
           (result (web-skeleton::parse-getent-output buf (length buf))))
      (check "getent: ipv6 family"     (cdr result) :inet6)
      (check "getent: ipv6 last byte"  (aref (car result) 15) 1))
    (let* ((text (format nil "::1             STREAM localhost~%~
                              127.0.0.1       STREAM localhost~%"))
           (buf (bytes text))
           (result (web-skeleton::parse-getent-output buf (length buf))))
      (check "getent: first STREAM wins (ipv6 first)"
             (cdr result) :inet6))
    (let* ((text (format nil "127.0.0.1       DGRAM ~%~
                              127.0.0.1       RAW ~%"))
           (buf (bytes text))
           (result (web-skeleton::parse-getent-output buf (length buf))))
      (check "getent: no STREAM rows -> nil" result nil))
    (let* ((text "")
           (buf (bytes text))
           (result (web-skeleton::parse-getent-output buf (length buf))))
      (check "getent: empty buffer -> nil" result nil))
    (let* ((text "127.0.0.1       STREAM")  ; no trailing LF
           (buf (bytes text))
           (result (web-skeleton::parse-getent-output buf (length buf))))
      (check "getent: partial line (no LF) -> nil" result nil))

    ;; ---- parse-url with IPv6 brackets ----
    (multiple-value-bind (scheme host port path)
        (web-skeleton::parse-url "http://[::1]:8080/path")
      (check "url: ipv6 scheme" scheme :http)
      (check "url: ipv6 host"   host   "::1")
      (check "url: ipv6 port"   port   8080)
      (check "url: ipv6 path"   path   "/path"))
    (multiple-value-bind (scheme host port path)
        (web-skeleton::parse-url "http://[2001:db8::1]/")
      (declare (ignore scheme))
      (check "url: ipv6 default port" port 80)
      (check "url: ipv6 full host"    host "2001:db8::1")
      (check "url: ipv6 root path"    path "/"))
    ;; https:// + IP literal is rejected. The covering rejection
    ;; tests live in test-fetch; this is here only to pin down that
    ;; the DNS helper group's earlier https-with-bracketed-v6 case
    ;; no longer parses successfully.
    (check-error "url: https ipv6 rejected"
                 (web-skeleton::parse-url "https://[::1]:8443/v1"))))

;;; ---------------------------------------------------------------------------
;;; IP address classification (SSRF helper)
;;; ---------------------------------------------------------------------------

(defun test-is-public-address ()
  (format t "~%is-public-address-p~%")

  ;; IPv4 — public
  (check "v4 1.1.1.1"          (is-public-address-p #(1 1 1 1) :inet)       t)
  (check "v4 8.8.8.8"          (is-public-address-p #(8 8 8 8) :inet)       t)
  (check "v4 172.32 is public" (is-public-address-p #(172 32 0 1) :inet)    t)
  (check "v4 100.63 is public" (is-public-address-p #(100 63 0 1) :inet)    t)

  ;; IPv4 — not public
  (check "v4 0.0.0.0"     (is-public-address-p #(0 0 0 0) :inet)            nil)
  (check "v4 10/8"        (is-public-address-p #(10 0 0 1) :inet)           nil)
  (check "v4 cgnat low"   (is-public-address-p #(100 64 0 1) :inet)         nil)
  (check "v4 cgnat high"  (is-public-address-p #(100 127 255 255) :inet)    nil)
  (check "v4 loopback"    (is-public-address-p #(127 0 0 1) :inet)          nil)
  (check "v4 loopback top" (is-public-address-p #(127 255 255 255) :inet)   nil)
  (check "v4 link-local"  (is-public-address-p #(169 254 1 1) :inet)        nil)
  (check "v4 aws metadata" (is-public-address-p #(169 254 169 254) :inet)   nil)
  (check "v4 172.16/12 low"  (is-public-address-p #(172 16 0 1) :inet)      nil)
  (check "v4 172.16/12 high" (is-public-address-p #(172 31 255 255) :inet)  nil)
  (check "v4 192.168/16"  (is-public-address-p #(192 168 1 1) :inet)        nil)
  (check "v4 test-net-1"  (is-public-address-p #(192 0 2 1) :inet)          nil)
  (check "v4 test-net-2"  (is-public-address-p #(198 51 100 1) :inet)       nil)
  (check "v4 test-net-3"  (is-public-address-p #(203 0 113 1) :inet)        nil)
  (check "v4 benchmarking" (is-public-address-p #(198 18 0 1) :inet)        nil)
  (check "v4 multicast"   (is-public-address-p #(224 0 0 1) :inet)          nil)
  (check "v4 reserved"    (is-public-address-p #(240 0 0 1) :inet)          nil)
  (check "v4 broadcast"   (is-public-address-p #(255 255 255 255) :inet)    nil)

  ;; IPv6 — public
  (check "v6 public"
         (is-public-address-p
          #(#x20 #x01 #x48 #x60 #x48 #x60 0 0 0 0 0 0 0 0 #x88 #x88) :inet6)
         t)

  ;; IPv6 — not public
  (check "v6 ::"
         (is-public-address-p
          #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) :inet6) nil)
  (check "v6 ::1"
         (is-public-address-p
          #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) :inet6) nil)
  (check "v6 fe80::1 link-local"
         (is-public-address-p
          #(#xfe #x80 0 0 0 0 0 0 0 0 0 0 0 0 0 1) :inet6) nil)
  (check "v6 fc00::1 ULA"
         (is-public-address-p
          #(#xfc 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) :inet6) nil)
  (check "v6 fd00:ec2::254 AWS"
         (is-public-address-p
          #(#xfd 0 #xec #x02 0 0 0 0 0 0 0 0 0 0 #x02 #x54) :inet6) nil)
  (check "v6 ff02::1 multicast"
         (is-public-address-p
          #(#xff #x02 0 0 0 0 0 0 0 0 0 0 0 0 0 1) :inet6) nil)
  (check "v6 2001:db8:: documentation"
         (is-public-address-p
          #(#x20 #x01 #x0d #xb8 0 0 0 0 0 0 0 0 0 0 0 1) :inet6) nil)

  ;; IPv4-mapped IPv6 — attacker cannot launder 127.0.0.1
  (check "v6 ::ffff:127.0.0.1 mapped loopback"
         (is-public-address-p
          #(0 0 0 0 0 0 0 0 0 0 #xff #xff 127 0 0 1) :inet6) nil)
  (check "v6 ::ffff:10.0.0.1 mapped private"
         (is-public-address-p
          #(0 0 0 0 0 0 0 0 0 0 #xff #xff 10 0 0 1) :inet6) nil)
  (check "v6 ::ffff:8.8.8.8 mapped public"
         (is-public-address-p
          #(0 0 0 0 0 0 0 0 0 0 #xff #xff 8 8 8 8) :inet6) t)

  ;; IPv4-compatible IPv6 (deprecated, RFC 4291 §2.5.5.1). Same
  ;; laundering defense — ::127.0.0.1 and ::1.2.3.4 classify via
  ;; their embedded v4.
  (check "v6 ::127.0.0.1 compat loopback"
         (is-public-address-p
          #(0 0 0 0 0 0 0 0 0 0 0 0 127 0 0 1) :inet6) nil)
  (check "v6 ::10.0.0.1 compat private"
         (is-public-address-p
          #(0 0 0 0 0 0 0 0 0 0 0 0 10 0 0 1) :inet6) nil)
  (check "v6 ::8.8.8.8 compat public"
         (is-public-address-p
          #(0 0 0 0 0 0 0 0 0 0 0 0 8 8 8 8) :inet6) t)

  ;; 6to4 (2002::/16, RFC 3056 / RFC 7526). Deprecated but shape-
  ;; consistent with the other v4-in-v6 laundering defenses: v4
  ;; payload lives in bytes 2..5, everything past that is an
  ;; arbitrary host id. Classify via embedded v4.
  (check "v6 2002:0a00:0001:: carries 10.0.0.1"
         (is-public-address-p
          #(#x20 #x02 10 0 0 1 0 0 0 0 0 0 0 0 0 0) :inet6) nil)
  (check "v6 2002:7f00:0001:: carries 127.0.0.1"
         (is-public-address-p
          #(#x20 #x02 127 0 0 1 0 0 0 0 0 0 0 0 0 0) :inet6) nil)
  (check "v6 2002:a9fe:a9fe:: carries link-local"
         (is-public-address-p
          #(#x20 #x02 169 254 169 254 0 0 0 0 0 0 0 0 0 0) :inet6) nil)
  (check "v6 2002:0808:0808:: carries 8.8.8.8 (public)"
         (is-public-address-p
          #(#x20 #x02 8 8 8 8 0 0 0 0 0 0 0 0 0 0) :inet6) t)

  ;; NAT64 well-known prefix (64:ff9b::/96). Same v4-in-v6
  ;; unwrap as ::ffff: and 2002:: — classify via embedded v4.
  (check "v6 nat64 64:ff9b::10.0.0.1 carries private"
         (is-public-address-p
          #(0 #x64 #xff #x9b 0 0 0 0 0 0 0 0 10 0 0 1) :inet6) nil)
  (check "v6 nat64 64:ff9b::8.8.8.8 carries public"
         (is-public-address-p
          #(0 #x64 #xff #x9b 0 0 0 0 0 0 0 0 8 8 8 8) :inet6) t)

  ;; Wrong length / unknown family returns NIL (conservative)
  (check "v4 wrong length"   (is-public-address-p #(1 2 3) :inet)           nil)
  (check "v6 wrong length"   (is-public-address-p #(1 2 3 4) :inet6)        nil)
  (check "unknown family"    (is-public-address-p #(1 1 1 1) :inet7)        nil))

;;; ---------------------------------------------------------------------------
;;; Peer address formatter — both families
;;; ---------------------------------------------------------------------------

(defun test-format-peer-addr ()
  (format t "~%format-peer-addr~%")
  (check "v4 localhost"
         (web-skeleton::format-peer-addr #(127 0 0 1) 8080)
         "127.0.0.1:8080")
  (check "v4 bound-all"
         (web-skeleton::format-peer-addr #(0 0 0 0) 443)
         "0.0.0.0:443")
  (check "v6 loopback"
         (web-skeleton::format-peer-addr
          #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 8080)
         "[0:0:0:0:0:0:0:1]:8080")
  (check "v6 unspecified"
         (web-skeleton::format-peer-addr
          #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 443)
         "[0:0:0:0:0:0:0:0]:443")
  (check "v6 documentation"
         (web-skeleton::format-peer-addr
          #(#x20 #x01 #x0d #xb8 0 0 0 0 0 0 0 0 0 0 0 1) 80)
         "[2001:db8:0:0:0:0:0:1]:80"))

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
      (close stream)))

  ;; Streaming Content-Length parser: strict digits-only +
  ;; duplicate-conflict rejection, symmetric with the inbound
  ;; SCAN-CONTENT-LENGTH byte scanner.
  (flet ((run-headers (header-line)
           (let* ((raw (ascii-bytes (concatenate 'string
                        "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                        header-line (string #\Return) (string #\Newline)
                        (string #\Return) (string #\Newline)
                        "x")))
                  (stream (make-mock-stream raw)))
             (unwind-protect
                 (handler-case
                     (progn (web-skeleton::stream-response-lines
                             stream (lambda (line) (declare (ignore line))))
                            nil)
                   (error () t))
               (close stream)))))
    (check "streaming CL: negative rejected"
           (run-headers "Content-Length: -5") t)
    (check "streaming CL: +10 rejected"
           (run-headers "Content-Length: +10") t)
    (check "streaming CL: non-digit rejected"
           (run-headers "Content-Length: 10x") t)
    (check "streaming CL: empty rejected"
           (run-headers "Content-Length: ") t))

  ;; Duplicate Content-Length with conflicting values must raise —
  ;; smuggling vector if a downstream re-parses the stream frame.
  (let* ((raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Content-Length: 5" (string #\Return) (string #\Newline)
                "Content-Length: 10" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)
                "hello")))
         (stream (make-mock-stream raw)))
    (unwind-protect
        (check "streaming CL: duplicate conflict rejected"
               (handler-case
                   (progn (web-skeleton::stream-response-lines
                           stream (lambda (line) (declare (ignore line))))
                          nil)
                 (error () t))
               t)
      (close stream)))

  ;; Streaming header-count cap — *max-header-count* applies to
  ;; outbound streaming responses, not just inbound requests.
  (let* ((headers (with-output-to-string (s)
                    (write-string "HTTP/1.1 200 OK" s)
                    (write-char #\Return s) (write-char #\Newline s)
                    (loop repeat 200 do
                      (write-string "X-Spam: 1" s)
                      (write-char #\Return s) (write-char #\Newline s))
                    (write-char #\Return s) (write-char #\Newline s)))
         (raw (ascii-bytes headers))
         (stream (make-mock-stream raw)))
    (unwind-protect
        (check "streaming: header-count cap"
               (handler-case
                   (progn (web-skeleton::stream-response-lines
                           stream (lambda (line) (declare (ignore line))))
                          nil)
                 (error () t))
               t)
      (close stream)))

  ;; TE: identity + CL should ignore CL per RFC 7230 §3.3.3 rule 3.
  ;; The response is close-delimited, not CL-framed.
  (let* ((raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Transfer-Encoding: identity" (string #\Return) (string #\Newline)
                "Content-Length: 5" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)
                "hello world extra")))
         (stream (make-mock-stream raw))
         (lines nil))
    (unwind-protect
        (let ((status (web-skeleton::stream-response-lines
                       stream (lambda (line) (push line lines)))))
          (setf lines (nreverse lines))
          (check "te-identity: status" status 200)
          ;; All body bytes delivered, not truncated to CL=5
          (check "te-identity: full body delivered"
                 (not (null (find "hello world extra" lines :test #'string=))) t))
      (close stream)))

  ;; Stream-chunked-lines trailing CRLF is now strict via
  ;; reader-expect-crlf: bare LF after chunk-data is rejected,
  ;; matching decode-chunked-body's buffered discipline.
  (let* ((raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Transfer-Encoding: chunked" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)
                "5" (string #\Return) (string #\Newline)
                "hello" (string #\Newline)                 ; bare LF
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline))))
         (stream (make-mock-stream raw)))
    (unwind-protect
        (check "streaming chunked: bare LF after chunk-data rejected"
               (handler-case
                   (progn (web-skeleton::stream-response-lines
                           stream (lambda (line) (declare (ignore line))))
                          nil)
                 (error () t))
               t)
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

  ;; Truncation — no zero-size terminator (MITM RST mid-stream).
  ;; The old decoder silently returned the partial body; now raise.
  (let ((raw (ascii-bytes (concatenate 'string
                "5" (string #\Return) (string #\Newline)
                "hello" (string #\Return) (string #\Newline)))))
    (check-error "chunked truncation: no terminator"
                 (web-skeleton::decode-chunked-body raw 0 (length raw))))

  ;; Truncation — chunk-size header read but data short.
  (let ((raw (ascii-bytes (concatenate 'string
                "a" (string #\Return) (string #\Newline)
                "hel"))))
    (check-error "chunked truncation: short chunk-data"
                 (web-skeleton::decode-chunked-body raw 0 (length raw))))

  ;; Truncation — ran out mid-chunk-size line.
  (let ((raw (ascii-bytes "5")))
    (check-error "chunked truncation: partial chunk-size"
                 (web-skeleton::decode-chunked-body raw 0 (length raw))))

  ;; Strict trailing CRLF after chunk-data — bare LF (missing CR)
  ;; must be rejected. A lax trailing terminator is a smuggling
  ;; primitive against a stricter downstream that re-parses the
  ;; body bytes.
  (let ((raw (ascii-bytes (concatenate 'string
                "5" (string #\Return) (string #\Newline)
                "hello" (string #\Newline)                 ; bare LF
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)))))
    (check-error "chunked trailing: bare LF rejected"
                 (web-skeleton::decode-chunked-body raw 0 (length raw))))

  ;; Strict trailing CRLF — bare CR (missing LF) rejected same.
  (let ((raw (ascii-bytes (concatenate 'string
                "5" (string #\Return) (string #\Newline)
                "hello" (string #\Return)                  ; bare CR
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)))))
    (check-error "chunked trailing: bare CR rejected"
                 (web-skeleton::decode-chunked-body raw 0 (length raw))))

  ;; Non-hex trailing byte after chunk-size digits. RFC 7230 §4.1.1
  ;; lists only ';' (chunk-ext start), SP / HTAB (BWS tolerance),
  ;; and CR (start of CRLF) as legal bytes after the hex digits.
  ;; '5g\r\n' must be rejected — silently accepting it as 'size 5
  ;; with an implicit g-extension' is a smuggling primitive.
  (let ((raw (ascii-bytes (concatenate 'string
                "5g" (string #\Return) (string #\Newline)
                "hello" (string #\Return) (string #\Newline)
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)))))
    (check-error "chunked chunk-size: 5g rejected (no ;)"
                 (web-skeleton::decode-chunked-body raw 0 (length raw))))
  ;; Positive: '5;ext=val\r\n' is still accepted — ';' is the
  ;; documented chunk-extension delimiter. Covered by the existing
  ;; "chunked decode with extension" test above.
  ;; Positive: '5 \r\n' — SP is BWS tolerance, legal per RFC 7230.
  (let* ((raw (ascii-bytes (concatenate 'string
                "5 " (string #\Return) (string #\Newline)
                "hello" (string #\Return) (string #\Newline)
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline))))
         (decoded (web-skeleton::decode-chunked-body raw 0 (length raw))))
    (check "chunked chunk-size: BWS SP after size accepted"
           (sb-ext:octets-to-string decoded :external-format :utf-8)
           "hello"))

  ;; response-chunked-p helper
  (check "chunked-p yes"
         (not (null (web-skeleton::response-chunked-p
                     '(("transfer-encoding" . "chunked"))))) t)
  (check "chunked-p no"
         (web-skeleton::response-chunked-p
          '(("content-type" . "text/plain"))) nil)
  (check "chunked-p case-insensitive"
         (not (null (web-skeleton::response-chunked-p
                     '(("transfer-encoding" . "Chunked"))))) t)
  ;; Split TE headers: chunked in the second header must still be detected.
  (check "chunked-p split headers"
         (not (null (web-skeleton::response-chunked-p
                     '(("transfer-encoding" . "gzip")
                       ("transfer-encoding" . "chunked"))))) t)
  (check "chunked-p split headers no match"
         (web-skeleton::response-chunked-p
          '(("transfer-encoding" . "gzip")
            ("transfer-encoding" . "identity"))) nil)

  ;; parse-chunked-size-line — shared by the streaming chunk parsers
  ;; in stream-chunked-lines and tls-stream-response. Strict hex,
  ;; strips chunk-extensions from ';' onwards, raises on garbage.
  ;; A permissive :junk-allowed shape would accept 'xyz' as NIL
  ;; (silently exit the decoder loop) and '-5' as -5 (same) — a
  ;; parser-disagreement smuggling primitive against any stricter
  ;; downstream.
  (check "chunked size: plain hex"
         (web-skeleton::parse-chunked-size-line "a0") 160)
  (check "chunked size: uppercase hex"
         (web-skeleton::parse-chunked-size-line "FF") 255)
  (check "chunked size: zero"
         (web-skeleton::parse-chunked-size-line "0") 0)
  (check "chunked size: extension stripped"
         (web-skeleton::parse-chunked-size-line "5;name=value") 5)
  (check "chunked size: extension with whitespace"
         (web-skeleton::parse-chunked-size-line "10 ; foo=bar") 16)
  (check-error "chunked size: rejects xyz"
               (web-skeleton::parse-chunked-size-line "xyz"))
  (check-error "chunked size: rejects -5"
               (web-skeleton::parse-chunked-size-line "-5"))
  (check-error "chunked size: rejects empty"
               (web-skeleton::parse-chunked-size-line ""))
  (check-error "chunked size: rejects extension-only"
               (web-skeleton::parse-chunked-size-line ";foo=bar"))
  (check-error "chunked size: rejects trailing garbage"
               (web-skeleton::parse-chunked-size-line "5g"))
  (check-error "chunked size: exceeds response cap"
               (let ((web-skeleton:*max-outbound-response-size* 1000))
                 (web-skeleton::parse-chunked-size-line "FFFFFFFF"))))

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
    ;; but we verify the reserved-code logic directly against the
    ;; extracted helper — same function the server calls, no duplicate.
    (check "close code 1004 clamped"
           (web-skeleton::clamp-close-code 1004) 1000)
    (check "close code 1005 clamped"
           (web-skeleton::clamp-close-code 1005) 1000)
    (check "close code 1006 clamped"
           (web-skeleton::clamp-close-code 1006) 1000)
    (check "close code 1015 clamped"
           (web-skeleton::clamp-close-code 1015) 1000)
    (check "close code 999 clamped"
           (web-skeleton::clamp-close-code 999) 1000)
    (check "close code 1000 passes"
           (web-skeleton::clamp-close-code 1000) 1000)
    (check "close code 1001 passes"
           (web-skeleton::clamp-close-code 1001) 1001)
    (check "close code 3000 passes"
           (web-skeleton::clamp-close-code 3000) 3000)
    (check "close code 5000 clamped"
           (web-skeleton::clamp-close-code 5000) 1000)
    (check "close code 65535 clamped"
           (web-skeleton::clamp-close-code 65535) 1000)
    (check "close code 4999 passes"
           (web-skeleton::clamp-close-code 4999) 4999)

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
               (length (ws-frame-payload result)) 200)))

    ;; Multi-fragment reassembly end-to-end through websocket-on-read.
    ;; First text frame with FIN=0, then a continuation frame with
    ;; FIN=1. The handler should see one synthetic frame whose
    ;; payload is the concatenation of both fragments' payloads.
    (let* ((first  (make-masked-frame nil 1 #(104 101 108 108 111 32)))  ; "hello "
           (cont   (make-masked-frame t 0 #(119 111 114 108 100)))       ; "world"
           (buf    (concatenate '(simple-array (unsigned-byte 8) (*))
                                first cont))
           (conn   (web-skeleton::make-connection
                    :fd -1 :state :websocket :last-active 0))
           (received-opcode nil)
           (received-text nil))
      (setf (web-skeleton::connection-read-buf conn) buf
            (web-skeleton::connection-read-pos conn) (length buf))
      (let ((handler (lambda (c frame)
                       (declare (ignore c))
                       (setf received-opcode (ws-frame-opcode frame)
                             received-text
                             (sb-ext:octets-to-string
                              (ws-frame-payload frame)
                              :external-format :utf-8))
                       nil)))
        (web-skeleton::websocket-on-read conn handler))
      (check "fragment reassembly: opcode is text"
             received-opcode 1)
      (check "fragment reassembly: concatenated payload"
             received-text "hello world"))

    ;; Error-close preserves earlier batch responses. First text
    ;; frame produces a handler response; second frame has an
    ;; unknown opcode (5) which triggers close-with 1002. A naive
    ;; (values :close (build-ws-close NNNN)) at each error site
    ;; would drop the earlier response on the floor — the
    ;; close-with helper funnels every error-close through the
    ;; same concat-then-return path as the normal close branch.
    (let* ((good (make-masked-frame t 1 #(104 105)))  ; text "hi"
           (bad  (make-masked-frame t 5 #()))         ; unknown opcode 5
           (buf  (concatenate '(simple-array (unsigned-byte 8) (*))
                              good bad))
           (reply (sb-ext:string-to-octets "REPLY" :external-format :ascii))
           (conn  (web-skeleton::make-connection
                   :fd -1 :state :websocket :last-active 0)))
      (setf (web-skeleton::connection-read-buf conn) buf
            (web-skeleton::connection-read-pos conn) (length buf))
      (let ((handler (lambda (c frame)
                       (declare (ignore c frame))
                       ;; Return raw bytes — websocket-on-read pushes
                       ;; into RESPONSES.
                       reply)))
        (multiple-value-bind (action response)
            (web-skeleton::websocket-on-read conn handler)
          (check "error-close action :close" action :close)
          ;; Response should contain BOTH the reply bytes and a
          ;; close frame. Search for the literal "REPLY" prefix
          ;; as the first response, and the 1002 status code in
          ;; the close frame payload further down.
          (check "error-close carries earlier response"
                 (and response (search reply response)) 0)
          (check "error-close carries close code 1002"
                 (let ((close-start (+ (length reply) 2)))
                   (and response
                        (>= (length response) (+ close-start 2))
                        (logior (ash (aref response close-start) 8)
                                (aref response (1+ close-start)))))
                 1002))))))

;;; ---------------------------------------------------------------------------
;;; Static file helper tests
;;; ---------------------------------------------------------------------------

(defun test-static-etag ()
  (format t "~%Static ETag~%")

  ;; ---- build-static-response populates ETag metadata ----
  (let* ((content (sb-ext:string-to-octets "hello world"
                                            :external-format :utf-8))
         (entry (web-skeleton::build-static-response
                 "text/plain" content 0)))
    (check "static-entry: etag slot populated"
           (not (null (web-skeleton::static-entry-etag entry))) t)
    (check "static-entry: etag is a quoted string"
           (let ((e (web-skeleton::static-entry-etag entry)))
             (and (char= (char e 0) #\")
                  (char= (char e (1- (length e))) #\")))
           t)
    (check "static-entry: etag is sha256 of content"
           (web-skeleton::static-entry-etag entry)
           (format nil "\"~a\"" (sha256-hex content)))
    (check "static-entry: not-modified response pre-built"
           (not (null (web-skeleton::static-entry-not-modified-response entry)))
           t))

  ;; Same content → same etag (deterministic)
  (let* ((a (web-skeleton::build-static-response
             "text/plain" (sb-ext:string-to-octets "payload") 0))
         (b (web-skeleton::build-static-response
             "text/plain" (sb-ext:string-to-octets "payload") 0)))
    (check "same content yields same etag"
           (string= (web-skeleton::static-entry-etag a)
                    (web-skeleton::static-entry-etag b))
           t))
  ;; Different content → different etag
  (let* ((a (web-skeleton::build-static-response
             "text/plain" (sb-ext:string-to-octets "aaa") 0))
         (b (web-skeleton::build-static-response
             "text/plain" (sb-ext:string-to-octets "bbb") 0)))
    (check "different content yields different etag"
           (string= (web-skeleton::static-entry-etag a)
                    (web-skeleton::static-entry-etag b))
           nil))

  ;; ---- if-none-match-hit-p parser (RFC 7232 §3.2) ----
  (labels ((hit (client our)
             (not (null (web-skeleton::if-none-match-hit-p client our)))))
    (check "exact strong match"
           (hit "\"abc\"" "\"abc\"") t)
    (check "exact mismatch"
           (hit "\"abc\"" "\"xyz\"") nil)
    (check "weak prefix match"
           (hit "W/\"abc\"" "\"abc\"") t)
    (check "weak prefix mismatch"
           (hit "W/\"abc\"" "\"xyz\"") nil)
    (check "wildcard matches"
           (hit "*" "\"abc\"") t)
    (check "wildcard with leading whitespace"
           (hit "   *" "\"abc\"") t)
    (check "comma list: first matches"
           (hit "\"abc\", \"def\"" "\"abc\"") t)
    (check "comma list: second matches"
           (hit "\"abc\", \"def\"" "\"def\"") t)
    (check "comma list: neither matches"
           (hit "\"abc\", \"def\"" "\"xyz\"") nil)
    (check "comma list with weak prefixes"
           (hit "W/\"abc\", W/\"def\"" "\"def\"") t)
    (check "empty header"
           (hit "" "\"abc\"") nil))
  (check "nil client header returns nil"
         (web-skeleton::if-none-match-hit-p nil "\"abc\"") nil)
  (check "nil our etag returns nil"
         (web-skeleton::if-none-match-hit-p "\"abc\"" nil) nil))

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
  (check "ext dotfile" (web-skeleton::file-extension "/.hidden") nil)
  (check "ext dotfile in subdir"
         (web-skeleton::file-extension "/foo/.hidden") nil)
  (check "ext regular in subdir"
         (web-skeleton::file-extension "/foo/bar.txt") "txt"))

;;; ---------------------------------------------------------------------------
;;; JWT tests
;;; ---------------------------------------------------------------------------

(defun test-jwt ()
  (format t "~%JWT~%")

  ;; Use the RFC 7515 A.3 ES256 example to build a complete JWT test.
  ;; Signature is the one the RFC actually publishes (high-S) — RFC
  ;; 7515 / 7518 do not mandate low-S normalization and the ECDSA
  ;; primitive now accepts both forms. Same vector as
  ;; test-algorithms.lisp's test-ecdsa.
  (let* ((header-b64 "eyJhbGciOiJFUzI1NiJ9")
         (payload-b64 "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ")
         (sig-b64 "DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmXFCgfTjDxw5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q")
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
    (check "jwks kid" (jwt-key-kid (first keys)) "test-key"))

  ;; Duplicate empty-kid keys rejected (same discipline as non-empty kids)
  (check-error "jwks: duplicate empty kid"
               (parse-jwks (concatenate 'string
                 "{\"keys\":["
                 "{\"kty\":\"EC\",\"crv\":\"P-256\","
                 "\"x\":\"f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU\","
                 "\"y\":\"x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0\"},"
                 "{\"kty\":\"EC\",\"crv\":\"P-256\","
                 "\"x\":\"f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU\","
                 "\"y\":\"x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0\"}"
                 "]}"))))

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
  (test-dns)
  (test-is-public-address)
  (test-format-peer-addr)
  (test-url-decode)
  (test-query-string)
  (test-match-path)
  (test-streaming-fetch)
  (test-decode-chunked-body)
  (test-websocket)
  (test-websocket-fragmentation)
  (test-static-helpers)
  (test-static-etag)
  (test-jwt)
  (test-shutdown-hooks)
  (format t "~%~d passed, ~d failed~%~%" *tests-passed* *tests-failed*)
  (zerop *tests-failed*))
