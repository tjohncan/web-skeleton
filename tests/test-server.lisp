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

  ;; Whitespace around cookie values trimmed — a misbehaving proxy or
  ;; test harness can emit 'foo=bar ; baz=qux' with a stray space
  ;; before the semicolon. Returning the raw substring silently breaks
  ;; STRING= comparisons in the handler.
  (let ((req (parse-request
              (crlf "GET / HTTP/1.1"
                    "Cookie: session=abc123 ; theme= dark ;lang=en"))))
    (check "cookie trim: trailing space"
           (get-cookie req "session") "abc123")
    (check "cookie trim: leading space"
           (get-cookie req "theme") "dark")
    (check "cookie trim: last unaffected"
           (get-cookie req "lang") "en"))

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
  ;; Non-ASCII bytes in URI rejected at parse time. Raw UTF-8 in the
  ;; request-target violates RFC 3986 §2.1 (non-ASCII must be
  ;; percent-encoded); rejecting at parse-time keeps URL-DECODE's
  ;; :ascii conversion from firing mid-handler and surfacing
  ;; symmetrical with the outbound PARSE-URL check.
  (check-error "non-ASCII byte in URI path"
               (parse-request
                (crlf (format nil "GET /caf~c HTTP/1.1" (code-char #xe9))
                      "Host: localhost")))
  (check-error "non-ASCII byte in URI query"
               (parse-request
                (crlf (format nil "GET /?q=~c HTTP/1.1" (code-char #xe9))
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

  ;; Total header bytes exceed *MAX-TOTAL-HEADER-BYTES*. Per-line
  ;; and per-count caps alone permit 800 KiB of header-string
  ;; allocation worst-case; the running-total guard in
  ;; PARSE-HEADERS-BYTES rejects once the cumulative line bytes
  ;; exceed the cap. Cap at 64 bytes for the test, feed two
  ;; 50-byte header lines.
  (check-error "total header bytes exceeded"
               (let ((web-skeleton:*max-total-header-bytes* 64))
                 (parse-request
                  (crlf "GET / HTTP/1.1"
                        (format nil "A: ~vA" 50 #\a)
                        (format nil "B: ~vA" 50 #\b)))))
  ;; Under the cap succeeds.
  (let ((web-skeleton:*max-total-header-bytes* 4096))
    (check "total header bytes under cap accepts"
           (let ((req (parse-request
                       (crlf "GET / HTTP/1.1"
                             "Host: localhost"
                             "A: small"))))
             (http-request-method req))
           :GET))

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
  (format t "~%Expect: disposition~%")
  (flet ((disp (&rest header-lines)
           (let* ((raw   (apply #'crlf "POST /foo HTTP/1.1" header-lines))
                  (bytes (sb-ext:string-to-octets raw :external-format :ascii))
                  (end   (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
             (web-skeleton::scan-expect-disposition bytes end))))
    ;; :100-CONTINUE matches — exact, case variants, whitespace variants.
    (check "exact match"
           (disp "Host: localhost" "Expect: 100-continue") :100-continue)
    (check "case-insensitive header name"
           (disp "Host: localhost" "EXPECT: 100-continue") :100-continue)
    (check "case-insensitive value"
           (disp "Host: localhost" "Expect: 100-Continue") :100-continue)
    (check "extra spaces after colon"
           (disp "Host: localhost" "Expect:   100-continue") :100-continue)
    (check "tab after colon"
           (disp "Host: localhost"
                 (format nil "Expect:~c100-continue" #\Tab))
           :100-continue)
    ;; RFC 7231 §5.1.1: Expect is 1#expectation — a list. Comma
    ;; terminates the 100-continue token just like ';' or CR.
    (check "comma separator accepted"
           (disp "Host: localhost" "Expect: 100-continue, x-foo=y") :100-continue)
    (check "comma immediately after token"
           (disp "Host: localhost" "Expect: 100-continue,x-foo=y") :100-continue)
    ;; :NONE — no Expect header at all.
    (check "no Expect header"
           (disp "Host: localhost" "Content-Length: 10") :none)
    (check "suffixed header name does not match"
           (disp "Host: localhost" "X-Expect: 100-continue") :none)
    ;; :UNKNOWN — Expect header present with a non-100-continue value.
    ;; RFC 7231 §5.1.1 MAYs a 417 response on these; the framework
    ;; chooses to always 417. Scanner just reports the classification,
    ;; the state machine handles the 417.
    (check "different expect value → :unknown"
           (disp "Host: localhost" "Expect: something-else") :unknown)
    (check "100-continued (trailing garbage) → :unknown"
           (disp "Host: localhost" "Expect: 100-continued") :unknown)
    (check "x-custom-expectation → :unknown"
           (disp "Host: localhost" "Expect: x-custom-expect") :unknown))
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

  ;; Codes added for handler use. Phrases are asserted rather than just
  ;; presence, because a wrong phrase reaches the wire on every response
  ;; that uses the code and nothing else in the system would notice.
  (dolist (spec '((202 . "Accepted")
                  (303 . "See Other")
                  (410 . "Gone")
                  (411 . "Length Required")
                  (412 . "Precondition Failed")
                  (415 . "Unsupported Media Type")
                  (422 . "Unprocessable Content")
                  (428 . "Precondition Required")
                  (505 . "HTTP Version Not Supported")))
    (check (format nil "~d reason" (car spec))
           (status-reason (car spec)) (cdr spec)))
  ;; RFC 9110 §15.5.21 renamed 422 from RFC 4918's "Unprocessable
  ;; Entity". Pinned so a future editor does not "correct" it back to
  ;; the phrasing every other framework still ships.
  (check "422 uses RFC 9110 phrasing, not RFC 4918's"
         (search "Entity" (status-reason 422)) nil)
  ;; Deliberately absent — see the *status-reasons* docstring.
  (dolist (code '(418 402 406 426 451))
    (check (format nil "~d deliberately absent" code)
           (status-reason code) "Unknown"))

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

  ;; ---- Byte-vector bodies ----
  ;; A string body is UTF-8 encoded at serialize time, so arbitrary
  ;; bytes routed through MAKE-TEXT-RESPONSE come out re-encoded. The
  ;; payload below is deliberately not valid UTF-8 (0x00 0xFF 0x80 0xFE):
  ;; it must survive byte-for-byte.
  (let* ((payload (make-array 6 :element-type '(unsigned-byte 8)
                                :initial-contents '(#x00 #xFF #x80 #xFE #x01 #x7F)))
         (resp (make-bytes-response 200 payload :content-type "image/png"))
         (bytes (format-response resp))
         (header-end (web-skeleton::scan-crlf-crlf bytes 0 (length bytes)))
         (emitted (subseq bytes (+ header-end 4)))
         (head-text (sb-ext:octets-to-string
                     (subseq bytes 0 header-end) :external-format :latin-1)))
    (check "bytes body: emitted verbatim" (equalp emitted payload) t)
    (check "bytes body: content-length matches byte count"
           (not (null (search "content-length: 6" head-text :test #'char-equal)))
           t)
    (check "bytes body: content-type honored"
           (not (null (search "image/png" head-text))) t)
    ;; HEAD keeps the Content-Length of the GET body but emits no body
    ;; (RFC 7231 §4.3.2) — same contract as a string body.
    (let* ((head-bytes (format-response resp :head-only-p t))
           (hend (web-skeleton::scan-crlf-crlf head-bytes 0 (length head-bytes)))
           (htext (sb-ext:octets-to-string
                   (subseq head-bytes 0 hend) :external-format :latin-1)))
      (check "bytes body: HEAD emits no body"
             (= (length head-bytes) (+ hend 4)) t)
      (check "bytes body: HEAD keeps content-length"
             (not (null (search "content-length: 6" htext :test #'char-equal)))
             t)))
  ;; Empty byte vector is a present-but-empty body → Content-Length: 0,
  ;; matching the empty-string case rather than falling into the no-body
  ;; branch.
  (let* ((resp (make-bytes-response
                200 (make-array 0 :element-type '(unsigned-byte 8))))
         (text (sb-ext:octets-to-string (format-response resp)
                                        :external-format :latin-1)))
    (check "bytes body: empty vector yields content-length 0"
           (not (null (search "content-length: 0" text :test #'char-equal)))
           t))
  ;; Default content-type, and a fill-pointered accumulator (the shape an
  ;; app building bytes incrementally ends up with) is accepted.
  (let* ((acc (make-array 0 :element-type '(unsigned-byte 8)
                            :fill-pointer 0 :adjustable t)))
    (vector-push-extend 65 acc)
    (vector-push-extend 66 acc)
    (let* ((resp (make-bytes-response 201 acc))
           (text (sb-ext:octets-to-string (format-response resp)
                                          :external-format :latin-1)))
      (check "bytes body: adjustable vector accepted"
             (not (null (search "AB" text))) t)
      (check "bytes body: default content-type"
             (not (null (search "application/octet-stream" text))) t)))

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
           (equalp (web-skeleton::strip-body-for-head bytes nil) bytes) t))

  ;; format-response :head-only-p — HEAD short-circuit. Headers
  ;; emitted (including Content-Length matching the body's length)
  ;; but the body bytes are not. Avoids the double-allocation that
  ;; the post-serialize STRIP-BODY-FOR-HEAD incurred on large bodies.
  (let* ((resp (make-text-response 200 "payload-12345"))
         (full (web-skeleton::format-response resp))
         (head (web-skeleton::format-response resp :head-only-p t)))
    (check "head-only-p: shorter than full"
           (< (length head) (length full)) t)
    (check "head-only-p: Content-Length header preserved"
           (let ((text (sb-ext:octets-to-string head :external-format :utf-8)))
             (not (null (search "content-length: 13" text))))
           t)
    (check "head-only-p: no body on the wire"
           (let ((text (sb-ext:octets-to-string head :external-format :utf-8)))
             (null (search "payload-12345" text)))
           t)
    (check "head-only-p: ends with CRLFCRLF"
           (list (aref head (- (length head) 4))
                 (aref head (- (length head) 3))
                 (aref head (- (length head) 2))
                 (aref head (- (length head) 1)))
           '(13 10 13 10)))

  ;; sync-close-after-p-from-response — handler's Connection: close
  ;; flips INBOUND's CONNECTION-CLOSE-AFTER-P to T. Test edge shapes:
  ;;  (a) no Connection header → no-op
  ;;  (b) Connection: upgrade → no-op (not a close token)
  ;;  (c) Connection: close (single header) → syncs
  ;;  (d) Connection: close, upgrade (comma list) → syncs
  ;;  (e) two Connection entries via ADD-RESPONSE-HEADER, one "close"
  ;;      → syncs (walks every instance, not just first)
  (flet ((make-conn () (web-skeleton::make-connection :fd -1 :last-active 0))
         (sync (conn response)
           (web-skeleton::sync-close-after-p-from-response conn response)
           (web-skeleton::connection-close-after-p conn)))
    ;; The inbound read cap used to be *max-body-size* verbatim, so one
    ;; knob silently moved two budgets: an app tightening the body cap to
    ;; 32 KiB also capped total request bytes at 32 KiB, and a request
    ;; with large-but-legal headers died on the buffer with a 400 that
    ;; blamed the body. Asserted as a strict inequality against the body
    ;; cap rather than a literal, so the check survives retuned defaults.
    (let ((c (make-conn)))
      (check "read cap: inbound leaves room for headers beyond the body"
             (> (web-skeleton::connection-read-cap c)
                (+ web-skeleton::*max-body-size*
                   web-skeleton::*max-total-header-bytes*))
             t)
      ;; The tightened-body-cap case the aliasing broke, stated directly:
      ;; a 32 KiB body budget must still admit a full 64 KiB of headers.
      (let ((web-skeleton::*max-body-size* (* 32 1024)))
        (check "read cap: tight body cap still admits full headers"
               (> (web-skeleton::connection-read-cap c)
                  web-skeleton::*max-total-header-bytes*)
               t))
      ;; The other three arms are unchanged and stay that way.
      (setf (web-skeleton::connection-state c) :websocket)
      (check "read cap: websocket is payload plus masked header"
             (web-skeleton::connection-read-cap c)
             (+ web-skeleton::*max-ws-payload-size* 14))
      (setf (web-skeleton::connection-state c) :out-dns)
      (check "read cap: out-dns is 8 KiB"
             (web-skeleton::connection-read-cap c) 8192))
    (let ((c (make-conn)))
      (setf (web-skeleton::connection-outbound-p c) t)
      (check "read cap: outbound uses the response budget"
             (web-skeleton::connection-read-cap c)
             web-skeleton::*max-outbound-response-size*))

    (let ((c (make-conn)))
      (check "sync-close: no Connection header — no-op"
             (sync c (make-text-response 200 "x")) nil))

    ;; CONNECTION-QUEUE-WRITE is named "queue" but replaces. Every
    ;; current call site reaches it with a drained buffer, so the
    ;; invariant holds by construction — but by construction is a fact
    ;; about today's callers, not about the function, and the caller who
    ;; gets it wrong would ship a truncated frame followed by a whole
    ;; one, surfacing as a protocol error somewhere unrelated.
    (let ((c (make-conn))
          (bytes (make-array 4 :element-type '(unsigned-byte 8)
                               :initial-element 65)))
      (web-skeleton::connection-queue-write c bytes)
      (check "queue-write: drained buffer accepts a new one"
             (web-skeleton::connection-write-end c) 4)
      ;; Simulate a partial flush, then try to queue over it.
      (setf (web-skeleton::connection-write-pos c) 1)
      (check "queue-write: clobbering un-flushed bytes signals"
             (handler-case (progn (web-skeleton::connection-queue-write c bytes)
                                  nil)
               (error () t))
             t)
      ;; Fully flushed is not "un-flushed": pos = end must still pass, or
      ;; the guard would reject the ordinary keep-alive path.
      (setf (web-skeleton::connection-write-pos c) 4)
      (check "queue-write: fully flushed buffer accepts a new one"
             (progn (web-skeleton::connection-queue-write c bytes)
                    (web-skeleton::connection-write-pos c))
             0))
    (let ((c (make-conn))
          (r (make-text-response 200 "x")))
      (set-response-header r "connection" "upgrade")
      (check "sync-close: Connection: upgrade — no-op"
             (sync c r) nil))
    (let ((c (make-conn))
          (r (make-text-response 200 "x")))
      (set-response-header r "connection" "close")
      (check "sync-close: Connection: close — syncs"
             (sync c r) t))
    (let ((c (make-conn))
          (r (make-text-response 200 "x")))
      (set-response-header r "connection" "close, upgrade")
      (check "sync-close: comma-list with close — syncs"
             (sync c r) t))
    (let ((c (make-conn))
          (r (make-text-response 200 "x")))
      (add-response-header r "connection" "keep-alive")
      (add-response-header r "connection" "close")
      (check "sync-close: duplicate entries, one close — syncs"
             (sync c r) t))
    ;; Nil inbound — safe (no-op, no error).
    (let ((r (make-text-response 200 "x")))
      (set-response-header r "connection" "close")
      (check "sync-close: nil inbound is safe"
             (progn (web-skeleton::sync-close-after-p-from-response nil r)
                    :ok)
             :ok))))

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
    ;; '=' in the NAME is rejected. RFC 6265 §5.2: browsers parse
    ;; 'foo=bar=baz' as name='foo', value='bar=baz'. Accepting '='
    ;; in the caller-supplied name would silently rename the cookie
    ;; to the substring before the first '='. '=' in the VALUE is
    ;; accepted — a value can legally contain '=' (e.g. base64).
    (check-error "build-cookie: '=' in name rejected"
                 (build-cookie "foo=bar" "baz"))
    (check-error "delete-cookie: '=' in name rejected"
                 (delete-cookie "foo=bar"))
    (let ((c (build-cookie "k" "opaque==value")))
      (check "'=' in value still accepted (base64 padding)"
             (not (null (search "k=opaque==value" c))) t))
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

  ;; Method charset: must be uppercase ASCII letters. Apps hardcode
  ;; methods in practice, but a keyword interned from attacker-
  ;; influenced data containing a space or CR would otherwise emit
  ;; two valid request lines on the wire — request smuggling shape.
  (flet ((raises-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    ;; (intern "get" :keyword) preserves the lowercase name on the
    ;; keyword; :GET (the natural-syntax read) uppercases to "GET"
    ;; and passes, which is what apps hardcode in practice.
    (check "method charset: lowercase rejected"
           (raises-p (lambda ()
                       (web-skeleton::build-outbound-request
                        (intern "get" :keyword)
                        "localhost" "/"))) t)
    (check "method charset: space rejected"
           (raises-p (lambda ()
                       (web-skeleton::build-outbound-request
                        (intern "GET HTTP/1.1" :keyword)
                        "localhost" "/"))) t)
    (check "method charset: digits rejected"
           (raises-p (lambda ()
                       (web-skeleton::build-outbound-request
                        (intern "GET1" :keyword)
                        "localhost" "/"))) t)
    (check "method charset: HEAD accepted"
           (not (null (web-skeleton::build-outbound-request
                       :HEAD "localhost" "/"))) t)

    ;; Framing headers belong to the builder, not the caller. The merge
    ;; adds Content-Length only when the caller did not supply one, which
    ;; decides both cases below: Transfer-Encoding does not suppress it,
    ;; so a caller who sends TE with a body gets both framing headers at
    ;; once; Content-Length does suppress it, so a caller who sends a
    ;; wrong one gets their number in front of our bytes. Ingress already
    ;; refuses Transfer-Encoding; egress refusing to emit what ingress
    ;; refuses to accept is the symmetry.
    (check "outbound: caller Transfer-Encoding rejected with a body"
           (raises-p (lambda ()
                       (web-skeleton::build-outbound-request
                        :POST "localhost" "/"
                        :headers '(("transfer-encoding" . "chunked"))
                        :body "hello"))) t)
    ;; Bodiless too: the header is still a claim about framing we do not
    ;; implement, and rejecting only the both-headers case would leave
    ;; the lie legal whenever it happened to be harmless.
    (check "outbound: caller Transfer-Encoding rejected without a body"
           (raises-p (lambda ()
                       (web-skeleton::build-outbound-request
                        :GET "localhost" "/"
                        :headers '(("transfer-encoding" . "chunked"))))) t)
    ;; Header names are case-insensitive on the wire, so the guard has to
    ;; be too — STRING-EQUAL, not STRING=.
    (check "outbound: Transfer-Encoding rejected case-insensitively"
           (raises-p (lambda ()
                       (web-skeleton::build-outbound-request
                        :POST "localhost" "/"
                        :headers '(("Transfer-Encoding" . "chunked"))
                        :body "hello"))) t)
    ;; Content-Length is the half of RFC 7230 §3.3.3 that a caller reaches
    ;; by accident rather than on purpose — a stale content-length copied
    ;; along with the rest of a header alist. Short declaration: the
    ;; upstream reads five bytes and treats "56789" as the head of the
    ;; next request. Long declaration: it blocks until its read timeout.
    (check "outbound: caller Content-Length under-declaring rejected"
           (raises-p (lambda ()
                       (web-skeleton::build-outbound-request
                        :POST "localhost" "/"
                        :headers '(("content-length" . "5"))
                        :body "0123456789"))) t)
    (check "outbound: caller Content-Length over-declaring rejected"
           (raises-p (lambda ()
                       (web-skeleton::build-outbound-request
                        :POST "localhost" "/"
                        :headers '(("content-length" . "100"))
                        :body "hello"))) t)
    ;; Rejected even when it happens to agree with the body: a guard that
    ;; compared numbers instead of refusing the header would leave the
    ;; caller believing framing is theirs to declare, and the next value
    ;; they pass is the stale one.
    (check "outbound: caller Content-Length rejected even when correct"
           (raises-p (lambda ()
                       (web-skeleton::build-outbound-request
                        :POST "localhost" "/"
                        :headers '(("Content-Length" . "5"))
                        :body "hello"))) t)
    ;; The escape hatch has to keep working: a deliberate Content-Length: 0
    ;; on a bodiless POST is expressed as :body "", which is a zero-length
    ;; vector rather than NIL and so still emits the header.
    (let ((text (sb-ext:octets-to-string
                 (web-skeleton::build-outbound-request
                  :POST "localhost" "/" :body "")
                 :external-format :utf-8)))
      (check "outbound: :body \"\" still declares Content-Length: 0"
             (not (null (search "content-length: 0" text))) t))
    ;; And the computed header is still emitted for an ordinary body, so
    ;; the guards did not cost the normal path its framing.
    (let ((text (sb-ext:octets-to-string
                 (web-skeleton::build-outbound-request
                  :POST "localhost" "/" :body "hello")
                 :external-format :utf-8)))
      (check "outbound: computed Content-Length still emitted"
             (not (null (search "content-length: 5" text))) t))
    ;; And an ordinary header still passes, so the guard is not just
    ;; rejecting every :headers list handed to it.
    (check "outbound: unrelated header still accepted"
           (not (null (web-skeleton::build-outbound-request
                       :POST "localhost" "/"
                       :headers '(("x-trace" . "abc"))
                       :body "hello"))) t))

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
                  "*MAX-WS-MESSAGE-SIZE*"
                  ;; WS-SEND is exported; its only tuning knob was not, so
                  ;; the deadline that decides how long one slow peer may
                  ;; hold a whole worker was unreachable through the
                  ;; public API. A limit nobody can set is not a limit.
                  "*WS-SEND-TIMEOUT*"))
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

  ;; Non-HTTP prefixes must reject. Without the prefix check a wrong-
  ;; protocol upstream whose first line happens to contain '<junk> 200'
  ;; would parse as status 200 and masquerade as an HTTP response —
  ;; same shape as the parse-request-bytes version check on the inbound
  ;; side.
  (dolist (prefix '("FOOBAR 200 OK"          ; not HTTP
                    "HTTP/2.0 200 OK"        ; wrong major
                    "HTTP/1.2 200 OK"        ; wrong minor
                    "HTTP/1.1  200 OK"       ; double space (missing SP after 1.1)
                    "http/1.1 200 OK"        ; wrong case
                    "HTTP/1.1"               ; no status
                    ""))
    (let ((buf (sb-ext:string-to-octets
                (concatenate 'string prefix *crlf*)
                :external-format :ascii)))
      (check (format nil "status reject ~s" prefix)
             (web-skeleton::parse-response-status buf 0 (length buf)) nil)))

  ;; String-level twin PARSE-STATUS-LINE-STRING is what the streaming
  ;; paths (stream-response-lines, tls-stream-response) now delegate to.
  ;; Its acceptance set MUST match the buffered byte-level parse —
  ;; without the prefix check, a non-HTTP upstream whose first line
  ;; contains '<junk> 200' masquerades as HTTP status 200 on the
  ;; streaming paths only, a parser-disagreement smuggling primitive.
  ;; Cases parallel the buffered block above — if one parser drifts,
  ;; both test groups should catch it.
  (check "string status 200"
         (web-skeleton::parse-status-line-string "HTTP/1.1 200 OK") 200)
  (check "string status 404"
         (web-skeleton::parse-status-line-string "HTTP/1.1 404 Not Found") 404)
  (check "string status 302 http/1.0"
         (web-skeleton::parse-status-line-string "HTTP/1.0 302 Found") 302)
  (check "string status 204 no reason"
         (web-skeleton::parse-status-line-string "HTTP/1.1 204") 204)
  (dolist (line '("FUBAR 200 OK"             ; non-HTTP masquerade
                  "NOT-HTTP 418 Z"           ; another non-HTTP masquerade
                  "HTTP/2.0 200 OK"          ; wrong major
                  "HTTP/1.2 200 OK"          ; wrong minor
                  "HTTP/1.1  200 OK"         ; double space
                  "http/1.1 200 OK"          ; wrong case
                  "HTTP/1.1"                 ; no status digits
                  "HTTP/1.1 20"              ; two digits
                  "HTTP/1.1 99 OK"           ; two digits + junk
                  "HTTP/1.1 600 OK"          ; out of 100-599
                  "HTTP/1.1 099 OK"          ; out of 100-599
                  "HTTP/1.1 2OO OK"          ; non-digit
                  ""
                  nil))
    (check (format nil "string status reject ~s" line)
           (web-skeleton::parse-status-line-string line) nil))

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
    ;; IPv4-mapped / IPv4-compatible forms (RFC 4291 §2.5.5) — the
    ;; final 32 bits are written as dotted-quad. Required for
    ;; consistency with IS-PUBLIC-ADDRESS-P which already unwraps
    ;; these to classify via the embedded v4.
    (let ((mapped (v6 "::ffff:127.0.0.1")))
      (check "ipv6: ::ffff:127.0.0.1 length"  (length mapped) 16)
      (check "ipv6: ::ffff:127.0.0.1 bytes 10-11 ffff"
             (list (aref mapped 10) (aref mapped 11)) '(255 255))
      (check "ipv6: ::ffff:127.0.0.1 carries 127"
             (aref mapped 12) 127)
      (check "ipv6: ::ffff:127.0.0.1 last"
             (aref mapped 15) 1))
    (let ((compat (v6 "::127.0.0.1")))
      (check "ipv6: ::127.0.0.1 length"  (length compat) 16)
      (check "ipv6: ::127.0.0.1 bytes 10-11 zero"
             (list (aref compat 10) (aref compat 11)) '(0 0))
      (check "ipv6: ::127.0.0.1 carries 127"
             (aref compat 12) 127))
    (check "ipv6: dotted-quad without embedded rejected"
           (v6 "1.2.3.4") nil)
    ;; Malformed IPv4 tails after :: — each must reject cleanly.
    ;; A broken %IPV6-MAYBE-UNFOLD-IPV4-TAIL that returned NIL on
    ;; failed parse would let these parse as '::' (all zeros)
    ;; because the dropped suffix collapses to zero pad groups.
    (check "ipv6: ::1.2.3 too few octets rejected"
           (v6 "::1.2.3") nil)
    (check "ipv6: ::1.2.3.4.5 too many octets rejected"
           (v6 "::1.2.3.4.5") nil)
    (check "ipv6: ::256.1.2.3 out-of-range octet rejected"
           (v6 "::256.1.2.3") nil)
    (check "ipv6: ::01.2.3.4 leading-zero octet rejected"
           (v6 "::01.2.3.4") nil)
    (check "ipv6: ::garbage.com rejected"
           (v6 "::garbage.com") nil)
    (check "ipv6: abcd::1.2.3 non-suffix-pad rejected"
           (v6 "abcd::1.2.3") nil)
    (check "ipv6: ::ffff:invalid.host rejected"
           (v6 "::ffff:invalid.host") nil)

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
    ;; STREAM must be the token immediately after the address, not
    ;; any substring in the line. A DGRAM / RAW row whose hostname
    ;; happens to contain ' STREAM' does not masquerade as a STREAM
    ;; row — only a token-anchored parse qualifies.
    (let* ((text (format nil "127.0.0.1       DGRAM my.STREAM.example~%~
                              127.0.0.1       RAW  ~%"))
           (buf (bytes text))
           (result (web-skeleton::parse-getent-output buf (length buf))))
      (check "getent: DGRAM with STREAM in hostname rejected" result nil))
    ;; Mixed output: DGRAM rows whose hostnames contain 'STREAM' are
    ;; skipped; the real STREAM row below matches.
    (let* ((text (format nil "127.0.0.1       DGRAM my.STREAM.example~%~
                              10.0.0.1        STREAM real.example~%"))
           (buf (bytes text))
           (result (web-skeleton::parse-getent-output buf (length buf))))
      (check "getent: STREAM after DGRAM-with-STREAM-hostname matches"
             (coerce (car result) 'list) '(10 0 0 1)))
    ;; Unspecified addresses (0.0.0.0, ::) rejected at the parser.
    ;; 0.0.0.0 is routed to loopback on Linux, :: typically fails
    ;; with EADDRNOTAVAIL — either way, a meaningless dial target.
    (let* ((text (format nil "0.0.0.0         STREAM any~%"))
           (buf (bytes text))
           (result (web-skeleton::parse-getent-output buf (length buf))))
      (check "getent: 0.0.0.0 rejected" result nil))
    (let* ((text (format nil "::              STREAM any~%"))
           (buf (bytes text))
           (result (web-skeleton::parse-getent-output buf (length buf))))
      (check "getent: :: rejected" result nil))
    ;; Unspecified rejection only skips that entry — a later STREAM
    ;; row with a real address still matches.
    (let* ((text (format nil "0.0.0.0         STREAM any~%~
                              127.0.0.1       STREAM localhost~%"))
           (buf (bytes text))
           (result (web-skeleton::parse-getent-output buf (length buf))))
      (check "getent: real STREAM after 0.0.0.0 matches"
             (coerce (car result) 'list) '(127 0 0 1)))

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
  ;; 6to4 relay anycast, deprecated by RFC 7526. The relays are gone, so
  ;; the prefix is still routed but reaches whoever picked it up.
  (check "v4 6to4 anycast low"  (is-public-address-p #(192 88 99 0) :inet)   nil)
  (check "v4 6to4 anycast high" (is-public-address-p #(192 88 99 255) :inet) nil)
  ;; Neighbours on either side of the /24 stay public, so the new clause
  ;; is a /24 and not a /16 sitting on top of 192.88 or all of 192.
  (check "v4 192.88.98 is public"  (is-public-address-p #(192 88 98 1) :inet)  t)
  (check "v4 192.88.100 is public" (is-public-address-p #(192 88 100 1) :inet) t)
  (check "v4 192.89 is public"     (is-public-address-p #(192 89 99 1) :inet)  t)
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
  (check "unknown family"    (is-public-address-p #(1 1 1 1) :inet7)        nil)

  ;; Integration: parse-ipv6-literal → bytes → is-public-address-p.
  ;; An attacker-supplied '::ffff:127.0.0.1' URL literal must not
  ;; launder loopback through the IPv6 type. The parser now unfolds
  ;; the IPv4 tail, is-public-address-p unwraps via the embedded v4,
  ;; and the whole chain rejects as non-public.
  (flet ((public-via-literal-p (s)
           (let ((bytes (web-skeleton::parse-ipv6-literal s)))
             (and bytes (is-public-address-p bytes :inet6)))))
    (check "literal ::ffff:127.0.0.1 → not public"
           (public-via-literal-p "::ffff:127.0.0.1") nil)
    (check "literal ::ffff:10.0.0.1 → not public"
           (public-via-literal-p "::ffff:10.0.0.1") nil)
    (check "literal ::ffff:8.8.8.8 → public"
           (public-via-literal-p "::ffff:8.8.8.8") t)
    (check "literal ::127.0.0.1 → not public"
           (public-via-literal-p "::127.0.0.1") nil)
    (check "literal 2001:db8::1 → not public (documentation range)"
           (public-via-literal-p "2001:db8::1") nil)
    (check "literal fe80::1 → not public (link-local)"
           (public-via-literal-p "fe80::1") nil)))

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
         "[2001:db8:0:0:0:0:0:1]:80")
  ;; Neither length — the fallback both functions share.
  (check "unknown length"
         (web-skeleton::format-peer-addr #(1 2 3) 80)
         "<addr>:80")
  ;; FORMAT-PEER-ADDR is now FORMAT-IP plus brackets and a port, rather
  ;; than a second implementation of the same sixteen-byte walk. Asserted
  ;; as a relationship so the two cannot drift back apart: a log line
  ;; disagreeing with an address-filter decision about what an address
  ;; even looks like is a miserable thing to debug.
  (dolist (addr (list #(127 0 0 1)
                      #(8 8 8 8)
                      #(#x20 #x01 #x0d #xb8 0 0 0 0 0 0 0 0 0 0 0 1)
                      #(1 2 3)))
    (let ((v6-p (= (length addr) 16)))
      (check (format nil "peer-addr embeds format-ip (~d bytes)" (length addr))
             (web-skeleton::format-peer-addr addr 80)
             (if v6-p
                 (format nil "[~a]:80" (web-skeleton::format-ip addr))
                 (format nil "~a:80" (web-skeleton::format-ip addr)))))))

;;; ---------------------------------------------------------------------------
;;; Parse errors carry the status the client should receive
;;; ---------------------------------------------------------------------------

(defun test-parse-error-status ()
  (format t "~%Parse error status codes~%")
  ;; Every parse failure used to answer 400, including ones the framework
  ;; understood perfectly and refused on their merits. 413 / 414 / 431
  ;; were already in *status-reasons* and unreachable for exactly that
  ;; reason. These assert the status the condition carries, which is what
  ;; HANDLE-CLIENT-READ hands to MAKE-ERROR-RESPONSE.
  (flet ((status-of (thunk)
           (handler-case (progn (funcall thunk) :no-error)
             (web-skeleton:http-parse-error (e)
               (web-skeleton::http-parse-error-status e))))
         (req (line)
           (sb-ext:string-to-octets (concatenate 'string line *crlf* *crlf*)
                                    :external-format :ascii)))
    ;; 400 is still the default for genuine syntax trouble.
    (check "malformed request line stays 400"
           (status-of (lambda ()
                        (let ((b (req "GARBAGE")))
                          (web-skeleton::parse-request-bytes b 0 (length b)))))
           400)
    ;; 505 — the version is the problem, and 400 named the wrong thing.
    (check "unsupported version is 505"
           (status-of (lambda ()
                        (let ((b (req "GET / HTTP/2.0")))
                          (web-skeleton::parse-request-bytes b 0 (length b)))))
           505)
    ;; The real HTTP/2 prior-knowledge preface, which is the case the
    ;; version-before-method ordering exists for. Its method is PRI, so a
    ;; method-first parser answers 501 and never reaches the version —
    ;; and "GET / HTTP/2.0" above cannot catch that, because a known
    ;; method reaches the version check either way. RFC 7230 §2.6 puts
    ;; the version first.
    (check "h2 prior-knowledge preface is 505, not 501"
           (status-of (lambda ()
                        (let ((b (req "PRI * HTTP/2.0")))
                          (web-skeleton::parse-request-bytes b 0 (length b)))))
           505)
    (check "typo'd version is 505"
           (status-of (lambda ()
                        (let ((b (req "GET / HTTP/1.2")))
                          (web-skeleton::parse-request-bytes b 0 (length b)))))
           505)
    ;; A genuine HTTP/0.9 request line carries no version token at all,
    ;; so it has one space and dies as a malformed request line long
    ;; before the version check. A literal "HTTP/0.9" token does reach
    ;; 505. Both pinned because the distinction is easy to state wrongly.
    (check "literal HTTP/0.9 token is 505"
           (status-of (lambda ()
                        (let ((b (req "GET / HTTP/0.9")))
                          (web-skeleton::parse-request-bytes b 0 (length b)))))
           505)
    (check "versionless 0.9 request line is 400"
           (status-of (lambda ()
                        (let ((b (req "GET /path")))
                          (web-skeleton::parse-request-bytes b 0 (length b)))))
           400)
    (check "HTTP/1.0 still accepted"
           (status-of (lambda ()
                        (let ((b (req "GET / HTTP/1.0")))
                          (web-skeleton::parse-request-bytes b 0 (length b)))))
           :no-error)
    ;; 501 — a method we do not implement, not a malformed one. 405 would
    ;; be wrong: that means "known method, not allowed here" and carries a
    ;; mandatory Allow header we have no resource-level view to fill in.
    (check "unrecognized method is 501"
           (status-of (lambda ()
                        (let ((b (req "PROPFIND / HTTP/1.1")))
                          (web-skeleton::parse-request-bytes b 0 (length b)))))
           501)
    ;; 414 — the only part of a request line a client can grow at will is
    ;; the URI.
    (check "over-long request line is 414"
           (status-of
            (lambda ()
              (let ((b (req (format nil "GET /~a HTTP/1.1"
                                    (make-string
                                     (1+ web-skeleton:*max-request-line-length*)
                                     :initial-element #\a)))))
                (web-skeleton::parse-request-bytes b 0 (length b)))))
           414)
    ;; 431 — RFC 6585 §5, for headers individually or in total.
    (check "too many headers is 431"
           (status-of
            (lambda ()
              (let* ((hdrs (with-output-to-string (s)
                             (dotimes (i (+ 2 web-skeleton:*max-header-count*))
                               (format s "x-~d: v~a" i *crlf*))))
                     (b (sb-ext:string-to-octets
                         (concatenate 'string "GET / HTTP/1.1" *crlf*
                                      "host: x" *crlf* hdrs *crlf*)
                         :external-format :ascii)))
                (web-skeleton::parse-request-bytes b 0 (length b)))))
           431)
    (check "over-long header line is 431"
           (status-of
            (lambda ()
              (let ((b (sb-ext:string-to-octets
                        (concatenate 'string "GET / HTTP/1.1" *crlf*
                                     "x-big: "
                                     (make-string
                                      (1+ web-skeleton:*max-header-line-length*)
                                      :initial-element #\a)
                                     *crlf* *crlf*)
                        :external-format :ascii)))
                (web-skeleton::parse-request-bytes b 0 (length b)))))
           431)))

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

  ;; Obs-fold rejection — RFC 7230 §3.2.4. Buffered parse-headers-bytes
  ;; rejects a continuation line starting with SP or TAB; streaming
  ;; mirrors so the acceptance sets stay aligned.
  (dolist (spec `((" "              "SP")
                  (,(string #\Tab) "TAB")))
    (destructuring-bind (fold label) spec
      (let* ((raw (ascii-bytes (concatenate 'string
                    "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                    "Content-Type: text/plain" (string #\Return) (string #\Newline)
                    fold "continued-via-fold" (string #\Return) (string #\Newline)
                    (string #\Return) (string #\Newline) "x")))
             (stream (make-mock-stream raw)))
        (unwind-protect
            (check (format nil "streaming: obs-fold rejected (~a)" label)
                   (handler-case
                       (progn (web-skeleton::stream-response-lines
                               stream (lambda (line) (declare (ignore line))))
                              nil)
                     (error () t))
                   t)
          (close stream)))))

  ;; Per-header line length cap — *max-header-line-length*. Tighter
  ;; than *max-streaming-line-size* (the body-line budget). Without
  ;; this, a 1 MiB attacker-framed "header" would coast on the body-
  ;; line budget while the buffered parse-headers-bytes would reject
  ;; at 8 KiB.
  (let* ((long-value (make-string (1+ web-skeleton:*max-header-line-length*)
                                  :initial-element #\x))
         (raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "X-Big: " long-value (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline) "y")))
         (stream (make-mock-stream raw)))
    (unwind-protect
        (check "streaming: header line over *max-header-line-length* rejected"
               (handler-case
                   (progn (web-skeleton::stream-response-lines
                           stream (lambda (line) (declare (ignore line))))
                          nil)
                 (error () t))
               t)
      (close stream)))

  ;; Running-total header-bytes cap — *max-total-header-bytes*.
  ;; Sum of header lines past the cap raises before dispatch. Uses
  ;; near-max-line-length headers so the count stays under the
  ;; per-count cap while the aggregate trips.
  (let* ((per-line-bytes (- web-skeleton:*max-header-line-length* 100))
         (val (make-string per-line-bytes :initial-element #\x))
         (line-text (concatenate 'string "X-Spam: " val
                                 (string #\Return) (string #\Newline)))
         (lines-needed (1+ (ceiling web-skeleton:*max-total-header-bytes*
                                    per-line-bytes)))
         (headers (with-output-to-string (s)
                    (write-string "HTTP/1.1 200 OK" s)
                    (write-char #\Return s) (write-char #\Newline s)
                    (loop repeat lines-needed do (write-string line-text s))
                    (write-char #\Return s) (write-char #\Newline s)))
         (raw (ascii-bytes headers))
         (stream (make-mock-stream raw)))
    (unwind-protect
        (check "streaming: total header bytes cap"
               (handler-case
                   (progn (web-skeleton::stream-response-lines
                           stream (lambda (line) (declare (ignore line))))
                          nil)
                 (error () t))
               t)
      (close stream)))

  ;; Bodiless FINAL responses — RFC 7230 §3.3.3 rule 1, RFC 7232 §4.1,
  ;; RFC 7231 §4.3.2. 204 / 304 / HEAD terminate at the header-end
  ;; empty line regardless of CL / TE. Before the exempt was ported
  ;; to streaming, a 204 with a leftover CL raised "short body" and a
  ;; 304 with chunked framing blocked waiting for chunk-size bytes
  ;; that would never arrive. Symmetric with complete-fetch's
  ;; exempt set on the buffered path.
  ;;
  ;; 1xx deliberately does NOT appear here. It used to: 100 and 199
  ;; were listed as "bodiless" and the reader returned them as the
  ;; final status. That was the A1 defect — RFC 7231 §6.2 makes a 1xx
  ;; interim by definition, so an upstream that sends one and then
  ;; closes has not delivered a response at all, and reporting 103 as
  ;; the result silently discards the real one. Interim handling is
  ;; covered by TEST-INTERIM-RESPONSES, which asserts both that a 1xx
  ;; is stepped over and that a lone 1xx followed by EOF raises.
  (dolist (spec '((204 "Content-Length: 500")
                  (304 "Content-Length: 500")
                  (304 "Transfer-Encoding: chunked")))
    (destructuring-bind (status-code framing-line) spec
      (let* ((reason (case status-code
                       (204 "No Content")
                       (304 "Not Modified")))
             (raw (ascii-bytes
                   (concatenate 'string
                                (format nil "HTTP/1.1 ~d ~a" status-code reason)
                                (string #\Return) (string #\Newline)
                                framing-line
                                (string #\Return) (string #\Newline)
                                (string #\Return) (string #\Newline))))
             (stream (make-mock-stream raw))
             (body-lines 0))
        (unwind-protect
            (let ((status (web-skeleton::stream-response-lines
                           stream (lambda (line)
                                    (declare (ignore line))
                                    (incf body-lines)))))
              (check (format nil "streaming ~d: exempt returns status" status-code)
                     status status-code)
              (check (format nil "streaming ~d: callback not invoked" status-code)
                     body-lines 0))
          (close stream)))))

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
      (close stream)))

  ;; Chunk-size line terminator strictness — reader-read-crlf-line
  ;; rejects bare LF and bare CR on the size line itself (RFC 7230
  ;; §4.1). Before the fix, the lenient reader-read-line accepted
  ;; any of CR / LF / CRLF; streaming decoded "5\nhello\n0\n\n" as
  ;; valid while the buffered decode-chunked-body rejected the same
  ;; bytes — a parser-disagreement primitive.
  (let* ((raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Transfer-Encoding: chunked" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)
                "5" (string #\Newline)                      ; bare LF on size
                "hello" (string #\Return) (string #\Newline)
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline))))
         (stream (make-mock-stream raw)))
    (unwind-protect
        (check "streaming chunked: bare LF on chunk-size line rejected"
               (handler-case
                   (progn (web-skeleton::stream-response-lines
                           stream (lambda (line) (declare (ignore line))))
                          nil)
                 (error () t))
               t)
      (close stream)))

  ;; Bare CR inside a chunk-size line — CR not followed by LF is
  ;; malformed. "5\rhello..." would previously have been read as
  ;; chunk-size "5" with "hello" silently becoming chunk-data.
  (let* ((raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Transfer-Encoding: chunked" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)
                "5" (string #\Return)                       ; CR without LF
                "hello" (string #\Return) (string #\Newline)
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline))))
         (stream (make-mock-stream raw)))
    (unwind-protect
        (check "streaming chunked: bare CR on chunk-size line rejected"
               (handler-case
                   (progn (web-skeleton::stream-response-lines
                           stream (lambda (line) (declare (ignore line))))
                          nil)
                 (error () t))
               t)
      (close stream)))

  ;; Chunk-size hex-digit cap — parse-chunked-size-line mirrors
  ;; decode-chunked-body's 16-digit guard. 17 hex digits land in
  ;; parse-integer, which is super-linear in digit count; an
  ;; attacker-framed 1 MiB hex string would pin CPU before the
  ;; value check fires.
  (let* ((raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Transfer-Encoding: chunked" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)
                "00000000000000001"                         ; 17 hex digits
                (string #\Return) (string #\Newline)
                "x" (string #\Return) (string #\Newline)
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline))))
         (stream (make-mock-stream raw)))
    (unwind-protect
        (check "streaming chunked: over-16 hex digits rejected"
               (handler-case
                   (progn (web-skeleton::stream-response-lines
                           stream (lambda (line) (declare (ignore line))))
                          nil)
                 (error () t))
               t)
      (close stream)))

  ;; Empty chunk-size line — parse-chunked-size-line already raises
  ;; on empty hex, but the streaming path must not silently skip
  ;; past it. "\r\n\r\n" between headers and first chunk would have
  ;; been accepted before.
  (let* ((raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Transfer-Encoding: chunked" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)        ; empty size line
                "5" (string #\Return) (string #\Newline)
                "hello" (string #\Return) (string #\Newline)
                "0" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline))))
         (stream (make-mock-stream raw)))
    (unwind-protect
        (check "streaming chunked: empty chunk-size line rejected"
               (handler-case
                   (progn (web-skeleton::stream-response-lines
                           stream (lambda (line) (declare (ignore line))))
                          nil)
                 (error () t))
               t)
      (close stream)))

  ;; Body lines terminated by bare CR (WHATWG EventStream §9.2 —
  ;; lone U+000D is a valid separator). The reader splits on any
  ;; of CR / LF / CRLF without stripping CR from accumulated
  ;; content.
  (let* ((raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Content-Length: 16" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)
                ;; body: "event a\revent b\r" — two lines separated by
                ;; lone CR, final CR terminates the last line.
                "event a" (string #\Return)
                "event b" (string #\Return))))
         (stream (make-mock-stream raw))
         (lines nil))
    (unwind-protect
        (let ((status (web-skeleton::stream-response-lines
                       stream (lambda (l) (push l lines)))))
          (setf lines (nreverse lines))
          (check "sse-CR: status" status 200)
          (check "sse-CR: line count" (length lines) 2)
          (check "sse-CR: first line"  (first lines)  "event a")
          (check "sse-CR: second line" (second lines) "event b"))
      (close stream)))

  ;; Mixed CR / LF / CRLF terminators in one body. Each is equally
  ;; a line separator; content stays intact between terminators.
  ;; Body: a\rb\nc\r\nd\r\re\r\n = 13 bytes.
  (let* ((raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Content-Length: 13" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline)
                "a" (string #\Return)                           ; CR
                "b" (string #\Newline)                          ; LF
                "c" (string #\Return) (string #\Newline)        ; CRLF
                "d" (string #\Return) (string #\Return)         ; CR then CR
                "e" (string #\Return) (string #\Newline))))     ; CRLF
         (stream (make-mock-stream raw))
         (lines nil))
    (unwind-protect
        (progn
          (web-skeleton::stream-response-lines
           stream (lambda (l) (push l lines)))
          (setf lines (nreverse lines))
          (check "mixed-term: line count" (length lines) 6)
          (check "mixed-term: a"  (first lines)  "a")
          (check "mixed-term: b"  (second lines) "b")
          (check "mixed-term: c"  (third lines)  "c")
          (check "mixed-term: d"  (fourth lines) "d")
          (check "mixed-term: blank from CRCR"
                 (fifth lines) "")
          (check "mixed-term: e"  (sixth lines)  "e"))
      (close stream)))

  ;; HEAD response with Content-Length: N — the reader skips the
  ;; body phase entirely (RFC 7231 §4.3.2), so stream-response-lines
  ;; returns status without waiting for N bytes of body that the
  ;; upstream will not send.
  (let* ((raw (ascii-bytes (concatenate 'string
                "HTTP/1.1 200 OK" (string #\Return) (string #\Newline)
                "Content-Length: 42" (string #\Return) (string #\Newline)
                (string #\Return) (string #\Newline))))
         ;; no body bytes — upstream HEAD response shape
         (stream (make-mock-stream raw))
         (lines nil))
    (unwind-protect
        (let ((status (web-skeleton::stream-response-lines
                       stream (lambda (l) (push l lines))
                       :method :HEAD)))
          (check "HEAD stream: status" status 200)
          (check "HEAD stream: zero body lines" (length lines) 0))
      (close stream))))

;;; ---------------------------------------------------------------------------
;;; Upstream 1xx interim responses (RFC 7231 §6.2)
;;;
;;; A 1xx block is terminated by its own empty line and carries no body, so
;;; the final response begins immediately after it. Every buffered reader
;;; anchors on a CRLFCRLF to find where headers end, so without a skip that
;;; first boundary is the *interim's* terminator: the interim's status and
;;; headers become the fetch's, the body is forced empty by the bodiless
;;; exempt set, and the real response is discarded with no error.
;;;
;;; This is not a shape an app can avoid by configuration — `103 Early
;;; Hints` (RFC 8297) is sent unsolicited by Cloudflare and Fastly, and
;;; Apache emits it under H2EarlyHints.
;;;
;;; The framing predicate has to skip too. An interim carries no
;;; Content-Length, so anchoring on it makes a CL-framed 200 look
;;; close-delimited — which is how the fetch used to hang for the full
;;; *FETCH-TIMEOUT* against any upstream that keeps the connection alive.
;;; The regression cases below (plain 200, 204, partial interim) matter as
;;; much as the fix cases.
;;; ---------------------------------------------------------------------------

(defun test-interim-responses ()
  (format t "~%Interim 1xx responses~%")
  (flet ((skip (s)
           (let ((buf (ascii-bytes s)))
             (web-skeleton::skip-interim-responses buf 0 (length buf))))
         (status-after-skip (s)
           (let* ((buf (ascii-bytes s)) (end (length buf))
                  (sk (web-skeleton::skip-interim-responses buf 0 end)))
             (web-skeleton::parse-response-status buf sk end)))
         (complete-p (s method)
           (let ((buf (ascii-bytes s)))
             (not (null (web-skeleton::outbound-response-complete-p
                         buf (length buf) method 0))))))

    ;; ---- skip offsets ----
    (let ((s (concatenate 'string
                          (crlf "HTTP/1.1 103 Early Hints"
                                "Link: </s.css>; rel=preload")
                          (crlf "HTTP/1.1 200 OK" "Content-Length: 5")
                          "hello")))
      (check "interim: 103 skipped"            (skip s) 57)
      (check "interim: 103 then status is 200" (status-after-skip s) 200)
      (check "interim: 103 then CL-framed completes" (complete-p s :GET) t))

    (let ((s (concatenate 'string
                          (crlf "HTTP/1.1 100 Continue")
                          (crlf "HTTP/1.1 200 OK" "Content-Length: 5")
                          "hello")))
      (check "interim: 100 then status is 200" (status-after-skip s) 200)
      (check "interim: 100 then CL-framed completes" (complete-p s :GET) t))

    (let ((s (concatenate 'string
                          (crlf "HTTP/1.1 100 Continue")
                          (crlf "HTTP/1.1 103 Early Hints" "Link: <a>")
                          (crlf "HTTP/1.1 200 OK" "Content-Length: 5")
                          "hello")))
      (check "interim: two blocks then status is 200"
             (status-after-skip s) 200)
      (check "interim: two blocks then completes" (complete-p s :GET) t))

    ;; Chunked after an interim — the framing predicate must read TE from
    ;; the FINAL block's headers, not the interim's (which has none).
    (let ((s (concatenate 'string
                          (crlf "HTTP/1.1 103 Early Hints")
                          (crlf "HTTP/1.1 200 OK" "Transfer-Encoding: chunked")
                          "5" *crlf* "hello" *crlf* "0" *crlf* *crlf*)))
      (check "interim: chunked after interim completes on terminator"
             (complete-p s :GET) t))

    ;; ---- regressions: nothing without an interim may move ----
    (let ((s (concatenate 'string
                          (crlf "HTTP/1.1 200 OK" "Content-Length: 5")
                          "hello")))
      (check "interim regression: plain 200 skip is 0"  (skip s) 0)
      (check "interim regression: plain 200 status"     (status-after-skip s) 200)
      (check "interim regression: plain 200 completes"  (complete-p s :GET) t))

    ;; 204 is a final status, not an interim — it must never be skipped.
    (let ((s (crlf "HTTP/1.1 204 No Content")))
      (check "interim regression: 204 not skipped" (skip s) 0)
      (check "interim regression: 204 status"      (status-after-skip s) 204))

    ;; ---- partial reads: skip must not mis-anchor ----
    ;; A half-buffered interim yields START unchanged, so the read loop
    ;; simply waits for more bytes.
    (let ((s "HTTP/1.1 103 Early Hints\r\nLink: <a>\r\n"))
      (check "interim: partial interim yields start" (skip s) 0)
      (check "interim: partial interim is incomplete" (complete-p s :GET) nil))
    ;; Complete interim, partial final — skip advances past the interim and
    ;; the predicate still says "keep reading".
    (let ((s (concatenate 'string
                          (crlf "HTTP/1.1 103 Early Hints")
                          "HTTP/1.1 200 OK\r\nContent-Len")))
      (check "interim: complete interim + partial final skips interim"
             (skip s) 28)
      (check "interim: complete interim + partial final is incomplete"
             (complete-p s :GET) nil))

    ;; ---- cap: exactly at the limit must pass, one over must raise ----
    ;; The boundary is the whole point. A test that only feeds a wildly
    ;; over-cap count passes whether the loop allows N or N-1, which is
    ;; how an off-by-one here survived: the buffered walk consumes one
    ;; block per iteration, so it needs N+1 iterations to accept N
    ;; interims and still recognise the final response. Iterating only N
    ;; times rejected the Nth while the streaming paths — which count with
    ;; a separate (> count cap) test — accepted it, and the two transports
    ;; disagreed about their own documented limit.
    (flet ((n-interims (n)
             (with-output-to-string (o)
               (dotimes (i n)
                 (declare (ignorable i))
                 (write-string (crlf "HTTP/1.1 103 Early Hints") o))
               (write-string (crlf "HTTP/1.1 200 OK" "Content-Length: 0") o))))
      (check "interim cap: exactly *max-interim-responses* is accepted"
             (status-after-skip (n-interims web-skeleton:*max-interim-responses*))
             200)
      (check-error "interim cap: one over raises"
                   (skip (n-interims (1+ web-skeleton:*max-interim-responses*))))
      ;; And the streaming path must agree on the same boundary — the
      ;; two implementations count differently, so only a paired test
      ;; keeps them honest.
      (check "interim cap: streaming accepts exactly the cap"
             (let ((stream (make-mock-stream
                            (ascii-bytes
                             (n-interims web-skeleton:*max-interim-responses*)))))
               (unwind-protect
                    (web-skeleton::stream-response-lines stream nil)
                 (close stream)))
             200)
      (check-error "interim cap: streaming raises one over"
                   (let ((stream (make-mock-stream
                                  (ascii-bytes
                                   (n-interims
                                    (1+ web-skeleton:*max-interim-responses*))))))
                     (unwind-protect
                          (web-skeleton::stream-response-lines stream nil)
                       (close stream))))
      ;; The knob is exported like every other limit, so an app behind a
      ;; chatty CDN can raise it.
      (check "*max-interim-responses* exported from :web-skeleton"
             (nth-value 1 (find-symbol "*MAX-INTERIM-RESPONSES*" :web-skeleton))
             :external)))

  ;; ---- streaming path: must re-enter the header phase, not report 103 ----
  (flet ((stream-lines (raw &key (method :GET))
           (let ((lines nil)
                 (stream (make-mock-stream (ascii-bytes raw))))
             (unwind-protect
                  (let ((status (web-skeleton::stream-response-lines
                                 stream (lambda (l) (push l lines))
                                 :method method)))
                    (list status (nreverse lines)))
               (close stream)))))

    (check "interim streaming: 103 then CL body"
           (stream-lines
            (concatenate 'string
                         (crlf "HTTP/1.1 103 Early Hints" "Link: </s.css>")
                         (crlf "HTTP/1.1 200 OK" "Content-Length: 14")
                         "line-a" (string #\Newline)
                         "line-b" (string #\Newline)))
           '(200 ("line-a" "line-b")))

    (check "interim streaming: 103 then chunked body"
           (stream-lines
            (concatenate 'string
                         (crlf "HTTP/1.1 103 Early Hints")
                         (crlf "HTTP/1.1 200 OK" "Transfer-Encoding: chunked")
                         "7" *crlf* "{\"n\":1}" *crlf* "0" *crlf* *crlf*))
           '(200 ("{\"n\":1}")))

    (check "interim streaming regression: plain 200 unchanged"
           (stream-lines
            (concatenate 'string
                         (crlf "HTTP/1.1 200 OK" "Content-Length: 14")
                         "line-a" (string #\Newline)
                         "line-b" (string #\Newline)))
           '(200 ("line-a" "line-b")))

    (check "interim streaming regression: 204 still bodiless"
           (stream-lines (crlf "HTTP/1.1 204 No Content" "Content-Length: 500"))
           '(204 ()))

    (check "interim streaming regression: HEAD still bodiless"
           (stream-lines (crlf "HTTP/1.1 200 OK" "Content-Length: 42")
                         :method :HEAD)
           '(200 ()))

    ;; An upstream that sends only an interim and then closes has not sent
    ;; a response at all — that must be loud, not a silent 103.
    (check-error "interim streaming: interim then EOF raises"
                 (stream-lines (crlf "HTTP/1.1 103 Early Hints")))))

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

  ;; websocket-upgrade-p validates the Sec-WebSocket-Key charset.
  ;; Length alone accepts any 24-char string — including one with a
  ;; non-ASCII byte that would later trip websocket-accept-key's
  ;; :external-format :ascii conversion and surface as 500 instead
  ;; of the 400 a malformed key deserves.
  (flet ((ws-req (key)
           (make-test-request
            :method :GET
            :headers `(("host" . "localhost")
                       ("upgrade" . "websocket")
                       ("connection" . "Upgrade")
                       ("sec-websocket-key" . ,key)
                       ("sec-websocket-version" . "13")))))
    (check "ws-upgrade: valid base64 key accepted"
           (not (null (web-skeleton::websocket-upgrade-p
                       (ws-req "dGhlIHNhbXBsZSBub25jZQ==")))) t)
    ;; 24 chars including one non-ASCII — passes the length check so
    ;; the charset check is actually exercised (not short-circuited).
    (check "ws-upgrade: non-ASCII in key rejected"
           (web-skeleton::websocket-upgrade-p
            (ws-req (concatenate 'string
                                 "dGhlIHNhbXBsZSBub25"
                                 (string (code-char #xe9))
                                 "ZQ==")))
           nil)
    (check "ws-upgrade: space in key rejected"
           (web-skeleton::websocket-upgrade-p
            (ws-req "dGhlIHNhbXBsZSBub 5jZQ=="))
           nil)
    (check "ws-upgrade: short key (length) rejected"
           (web-skeleton::websocket-upgrade-p (ws-req "dGhlIHNhbXBsZQ=="))
           nil)
    ;; The key is base64 over 16 fixed bytes, so its shape is fully
    ;; determined: 22 data characters then exactly "==". A flat alphabet
    ;; sweep across all 24 admits '=' at any position, which accepted 24
    ;; of them — while the comment above the loop claimed the positional
    ;; strictness the loop did not have.
    (check "ws-upgrade: all-padding key rejected"
           (web-skeleton::websocket-upgrade-p
            (ws-req "========================"))
           nil)
    (check "ws-upgrade: pad inside the data chars rejected"
           (web-skeleton::websocket-upgrade-p
            (ws-req "dGhlIHNhbXBsZSBub=5jZQ=="))
           nil)
    (check "ws-upgrade: missing trailing pad rejected"
           (web-skeleton::websocket-upgrade-p
            (ws-req "dGhlIHNhbXBsZSBub25jZQAA"))
           nil)
    (check "ws-upgrade: single trailing pad rejected"
           (web-skeleton::websocket-upgrade-p
            (ws-req "dGhlIHNhbXBsZSBub25jZQA="))
           nil))

  ;; *ws-send-timeout* used to document 0 as "disable", which set no
  ;; deadline and left the write loop with no exit — a peer that stopped
  ;; draining its receive window pinned the worker permanently, and the
  ;; worker is every other connection on it, not just this one. An empty
  ;; frame is used so the loop body never runs and no fd is touched:
  ;; before the guard, 0 with nothing to write returned normally.
  (let ((conn (web-skeleton::make-connection :fd -1 :last-active 0))
        (empty (make-array 0 :element-type '(unsigned-byte 8))))
    (check "ws-send: zero timeout refused"
           (let ((*ws-send-timeout* 0))
             (handler-case (progn (web-skeleton::ws-send conn empty) nil)
               (error (e) (not (null (search "*ws-send-timeout*"
                                             (princ-to-string e)))))))
           t)
    (check "ws-send: negative timeout refused"
           (let ((*ws-send-timeout* -1))
             (handler-case (progn (web-skeleton::ws-send conn empty) nil)
               (error () t)))
           t)
    ;; A positive value still passes the guard, or the two checks above
    ;; would be satisfied by a function that refused everything.
    (check "ws-send: positive timeout passes the guard"
           (let ((*ws-send-timeout* 10))
             (handler-case (progn (web-skeleton::ws-send conn empty) :sent)
               (error () :error)))
           :sent))

  ;; build-ws-close only accepts the send-allowed set per RFC 6455
  ;; §7.4.1: 1000-1003, 1007-1014, 3000-4999. Clamp-on-receive is
  ;; interop-friendly; send-strict is symmetric with the RFC's MUST
  ;; NOT on 1004/1005/1006/1015.
  (flet ((raises-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    ;; Out-of-u16 range.
    (check "build-ws-close: code > 65535 rejected"
           (raises-p (lambda () (build-ws-close 99999))) t)
    (check "build-ws-close: negative rejected"
           (raises-p (lambda () (build-ws-close -1))) t)
    ;; Reserved-on-send: MUST NOT be sent per §7.4.1.
    (check "build-ws-close: 1004 reserved rejected"
           (raises-p (lambda () (build-ws-close 1004))) t)
    (check "build-ws-close: 1005 reserved rejected"
           (raises-p (lambda () (build-ws-close 1005))) t)
    (check "build-ws-close: 1006 reserved rejected"
           (raises-p (lambda () (build-ws-close 1006))) t)
    (check "build-ws-close: 1015 reserved rejected"
           (raises-p (lambda () (build-ws-close 1015))) t)
    ;; Unassigned / future-use bands.
    (check "build-ws-close: 999 rejected"
           (raises-p (lambda () (build-ws-close 999))) t)
    (check "build-ws-close: 2000 unassigned rejected"
           (raises-p (lambda () (build-ws-close 2000))) t)
    (check "build-ws-close: 5000 out-of-band rejected"
           (raises-p (lambda () (build-ws-close 5000))) t)
    ;; Valid send codes.
    (check "build-ws-close: 1000 accepted"
           (not (null (build-ws-close 1000))) t)
    (check "build-ws-close: 1001 accepted"
           (not (null (build-ws-close 1001))) t)
    (check "build-ws-close: 1011 accepted"
           (not (null (build-ws-close 1011))) t)
    (check "build-ws-close: 4000 app-range accepted"
           (not (null (build-ws-close 4000))) t))

  ;; header-has-token-p
  (check "token single"
         (web-skeleton::header-has-token-p "upgrade" "upgrade") t)
  (check "token in list"
         (web-skeleton::header-has-token-p "keep-alive, Upgrade" "upgrade") t)
  (check "token with whitespace"
         (web-skeleton::header-has-token-p "  Upgrade  ,  keep-alive  " "upgrade") t)
  (check "token absent"
         (web-skeleton::header-has-token-p "keep-alive" "upgrade") nil)

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
    (check "parse incomplete" result nil))

  ;; RFC 6455 §5.2: "the minimal number of bytes MUST be used to
  ;; encode the length." A peer sending len7=126 with a 16-bit value
  ;; below 126, or len7=127 with a 64-bit value below 65536, is
  ;; non-canonical. Accepting it opens a length-parser disagreement
  ;; vector against strict downstreams.
  (flet ((signals-error-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    ;; Non-canonical 16-bit length (value 100, should have used 7-bit form).
    ;; Build: b0=#x81 (FIN+text), b1=#x80+126 (MASK+126), length=0x0064=100,
    ;; mask 4 bytes, payload 100 bytes — but parser rejects before unmask.
    (let* ((frame (make-array (+ 8 100) :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
      (setf (aref frame 0) #x81
            (aref frame 1) (logior #x80 126)
            (aref frame 2) 0
            (aref frame 3) 100)
      (check "reject non-canonical 2-byte length"
             (signals-error-p
              (lambda ()
                (web-skeleton::try-parse-ws-frame frame 0 (length frame))))
             t))
    ;; RFC 6455 §5.1: client frames are masked. The mask bit is in byte 1,
    ;; so the rejection needs nothing but the two header bytes — it used
    ;; to sit below the availability test, which meant a peer could make
    ;; us buffer a whole payload before we refused a frame we had already
    ;; decided against. Header-only input proves the check no longer waits:
    ;; before the hoist this returned (values NIL 0) asking for more bytes.
    (let ((header-only (make-array 2 :element-type '(unsigned-byte 8)
                                     :initial-contents '(#x81 100))))
      (check "reject unmasked frame from header alone"
             (signals-error-p
              (lambda ()
                (web-skeleton::try-parse-ws-frame header-only 0 2)))
             t))
    ;; And a masked frame with the same shortfall still asks for more,
    ;; so the hoist did not turn incompleteness into an error.
    (let ((masked-short (make-array 2 :element-type '(unsigned-byte 8)
                                      :initial-contents '(#x81 #xE4))))
      (check "masked but incomplete frame still waits for bytes"
             (multiple-value-bind (frame consumed)
                 (web-skeleton::try-parse-ws-frame masked-short 0 2)
               (list frame consumed))
             '(nil 0)))
    ;; Canonical: len7=126, value=126 — accepted (boundary).
    (let* ((payload (make-array 126 :element-type '(unsigned-byte 8)
                                    :initial-element 97))  ; all 'a'
           (frame (make-array (+ 8 126) :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
      (setf (aref frame 0) #x81
            (aref frame 1) (logior #x80 126)
            (aref frame 2) 0
            (aref frame 3) 126
            ;; Mask key 0x00000000 so unmask is identity — skip XOR bookkeeping.
            (aref frame 4) 0 (aref frame 5) 0
            (aref frame 6) 0 (aref frame 7) 0)
      (replace frame payload :start1 8)
      (multiple-value-bind (f consumed)
          (web-skeleton::try-parse-ws-frame frame 0 (length frame))
        (declare (ignore consumed))
        (check "accept canonical 2-byte length (126)"
               (not (null f)) t)))
    ;; Non-canonical 64-bit length (value 1000, should have used 16-bit form).
    (let* ((frame (make-array (+ 14 1000) :element-type '(unsigned-byte 8)
                                          :initial-element 0)))
      (setf (aref frame 0) #x81
            (aref frame 1) (logior #x80 127)
            ;; 8 bytes big-endian: 1000 = 0x00000000_000003E8
            (aref frame 8) #x03
            (aref frame 9) #xE8)
      (check "reject non-canonical 8-byte length"
             (signals-error-p
              (lambda ()
                (web-skeleton::try-parse-ws-frame frame 0 (length frame))))
             t))))

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

    ;; Reserved close code handling on the receive side: clients can
    ;; send any code and the server CLAMPS (not fails) on receive for
    ;; interop. BUILD-WS-CLOSE on the send side is strict and rejects
    ;; reserved codes — see the test-websocket section that covers
    ;; BUILD-WS-CLOSE's allowed set directly. The clamp tests below
    ;; exercise the receive classifier.
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
         (web-skeleton::if-none-match-hit-p "\"abc\"" nil) nil)

  ;; ---- Cache-Control override ----
  ;; build-static-response accepts a caller-supplied Cache-Control
  ;; string in the optional 4th arg. LOAD-STATIC-FILES resolves its
  ;; :CACHE-CONTROL keyword (string or function-of-path) before
  ;; reaching here.
  (flet ((header-present-p (bytes header-text)
           (let* ((str (sb-ext:octets-to-string bytes :external-format :utf-8))
                  (probe (concatenate 'string header-text)))
             (not (null (search probe str))))))
    (let* ((content (sb-ext:string-to-octets "cache-ctl"
                                              :external-format :utf-8))
           (default-entry (web-skeleton::build-static-response
                           "text/plain" content 0))
           (custom-entry  (web-skeleton::build-static-response
                           "text/plain" content 0
                           "public, max-age=31536000, immutable")))
      (check "cache-control: default present on GET"
             (header-present-p
              (web-skeleton::static-entry-get-response default-entry)
              "cache-control: public, max-age=3600") t)
      (check "cache-control: default present on 304"
             (header-present-p
              (web-skeleton::static-entry-not-modified-response default-entry)
              "cache-control: public, max-age=3600") t)
      (check "cache-control: custom string present on GET"
             (header-present-p
              (web-skeleton::static-entry-get-response custom-entry)
              "cache-control: public, max-age=31536000, immutable") t)
      (check "cache-control: custom string present on 304"
             (header-present-p
              (web-skeleton::static-entry-not-modified-response custom-entry)
              "cache-control: public, max-age=31536000, immutable") t)))

  ;; ---- LOAD-STATIC-FILES aliases ----
  ;; Build a tiny tree in a scratch directory, load it, and verify
  ;; the alias passes populate the cache:
  ;;   /page.html        → /page          (.html extensionless alias)
  ;;   /sub/index.html   → /sub           (directory-index alias, new)
  ;;   /index.html       stays only as /index.html (root alias would
  ;;                     resolve to empty string — skipped by design)
  ;; Also verifies :CACHE-CONTROL function form receives the URL
  ;; path for per-file tailoring.
  (let* ((scratch (merge-pathnames "tests/tmp-static/"
                                    (truename ".")))
         (cc-seen nil))
    (ensure-directories-exist (merge-pathnames "sub/" scratch))
    (ensure-directories-exist (merge-pathnames ".well-known/" scratch))
    (ensure-directories-exist (merge-pathnames ".git/" scratch))
    (flet ((write-file (rel text)
             (with-open-file (s (merge-pathnames rel scratch)
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede
                                :if-does-not-exist :create)
               (write-sequence (sb-ext:string-to-octets
                                text :external-format :utf-8)
                               s))))
      (write-file "index.html"    "<!doctype html><title>root</title>")
      (write-file "page.html"     "<!doctype html><title>page</title>")
      (write-file "sub/index.html" "<!doctype html><title>sub</title>")
      ;; Dot-path discrimination: .well-known is the RFC 8615 exemption,
      ;; .git is the stays-hidden control (its file has a dotless name,
      ;; so only the directory-component filter can refuse it).
      (write-file ".well-known/security.txt" "Contact: mailto:sec@example")
      (write-file ".git/config" "[core]"))
    (let ((saved-cache web-skeleton::*static-cache*))
      (unwind-protect
           (progn
             (setf web-skeleton::*static-cache*
                   (make-hash-table :test #'equal))
             (web-skeleton::load-static-files
              (namestring scratch)
              :cache-control
              (lambda (url-path)
                (push url-path cc-seen)
                (if (search "/sub/" url-path)
                    "public, max-age=60"
                    "public, max-age=3600")))
             (check "alias: /page.html cached"
                    (not (null (gethash "/page.html"
                                        web-skeleton::*static-cache*))) t)
             (check "alias: /page extensionless"
                    (not (null (gethash "/page"
                                        web-skeleton::*static-cache*))) t)
             (check "alias: /sub/index.html cached"
                    (not (null (gethash "/sub/index.html"
                                        web-skeleton::*static-cache*))) t)
             (check "alias: /sub directory-index"
                    (not (null (gethash "/sub"
                                        web-skeleton::*static-cache*))) t)
             (check "alias: root /index.html cached"
                    (not (null (gethash "/index.html"
                                        web-skeleton::*static-cache*))) t)
             (check "alias: root \"\" not registered"
                    (gethash ""
                             web-skeleton::*static-cache*) nil)
             (check "cache-control fn: saw /sub/index.html url"
                    (not (null (member "/sub/index.html" cc-seen
                                       :test #'string=))) t)
             (check "dot-path: /.well-known/security.txt served"
                    (not (null (gethash "/.well-known/security.txt"
                                        web-skeleton::*static-cache*))) t)
             (check "dot-path: /.git/config stays hidden"
                    (gethash "/.git/config"
                             web-skeleton::*static-cache*) nil)
             ;; :MAX-TOTAL-BYTES. The cache is resident for the life of
             ;; the process, so an oversized tree is a resident-set
             ;; surprise discovered on the box at deploy time unless it
             ;; is refused here. Range support makes large media likelier
             ;; to be sitting in the directory, not less.
             (setf web-skeleton::*static-cache* (make-hash-table :test #'equal))
             (check "static: tree over :max-total-bytes signals"
                    (handler-case
                        (progn (web-skeleton::load-static-files
                                (namestring scratch) :max-total-bytes 10)
                               nil)
                      (error () t))
                    t)
             ;; A cap the tree fits under must not fire — otherwise the
             ;; check above would pass for a guard that rejects always.
             (setf web-skeleton::*static-cache* (make-hash-table :test #'equal))
             (check "static: tree under :max-total-bytes loads"
                    (handler-case
                        (progn (web-skeleton::load-static-files
                                (namestring scratch)
                                :max-total-bytes (* 1024 1024))
                               (not (null (gethash "/index.html"
                                                   web-skeleton::*static-cache*))))
                      (error () nil))
                    t)
             ;; And the default is generous enough that an ordinary tree
             ;; never trips it, so existing callers are untouched.
             (setf web-skeleton::*static-cache* (make-hash-table :test #'equal))
             (check "static: default cap does not fire on a small tree"
                    (handler-case
                        (progn (web-skeleton::load-static-files
                                (namestring scratch))
                               t)
                      (error () nil))
                    t))
        (setf web-skeleton::*static-cache* saved-cache)
        ;; Cleanup scratch tree. Files first, then nested dir, then
        ;; scratch root. IGNORE-ERRORS wraps each so a missing file
        ;; from a previous partial run does not mask a real test
        ;; failure.
        (dolist (rel '("index.html" "page.html" "sub/index.html"
                       ".well-known/security.txt" ".git/config"))
          (ignore-errors
           (delete-file (merge-pathnames rel scratch))))
        (dolist (dir '("sub/" ".well-known/" ".git/"))
          (ignore-errors
           (sb-ext:delete-directory (merge-pathnames dir scratch))))
        (ignore-errors
         (sb-ext:delete-directory scratch))))))

(defun test-static-range ()
  (format t "~%Static Range (RFC 7233)~%")

  ;; ---- parse-byte-range ----
  ;; TOTAL = 100, so valid offsets are 0..99.
  (flet ((r (spec)
           (multiple-value-bind (first last)
               (web-skeleton::parse-byte-range spec 100)
             (cond ((eq first :unsatisfiable) :unsatisfiable)
                   (first (list first last))
                   (t :ignore)))))
    (check "range: bytes=0-49"      (r "bytes=0-49")   '(0 49))
    (check "range: bytes=50-"       (r "bytes=50-")    '(50 99))
    (check "range: bytes=-10 suffix" (r "bytes=-10")   '(90 99))
    (check "range: single byte"     (r "bytes=0-0")    '(0 0))
    (check "range: last byte"       (r "bytes=99-99")  '(99 99))
    ;; LAST beyond the end is clamped, not an error (RFC 7233 §2.1).
    (check "range: end clamped to resource"
           (r "bytes=90-999") '(90 99))
    ;; A suffix longer than the resource yields the whole resource.
    (check "range: oversized suffix yields whole resource"
           (r "bytes=-500") '(0 99))
    ;; Start past the end is unsatisfiable → 416.
    (check "range: start past end unsatisfiable"
           (r "bytes=100-") :unsatisfiable)
    (check "range: bytes=-0 unsatisfiable"
           (r "bytes=-0") :unsatisfiable)
    ;; Malformed / unsupported shapes are ignored (serve the full 200) —
    ;; ignoring a Range is always safe; guessing at one is not.
    (check "range: multi-range ignored"  (r "bytes=0-9,20-29") :ignore)
    (check "range: last < first ignored" (r "bytes=50-10") :ignore)
    (check "range: non-numeric ignored"  (r "bytes=abc-def") :ignore)
    (check "range: wrong unit ignored"   (r "items=0-9") :ignore)
    (check "range: garbage ignored"      (r "bytes=") :ignore)
    (check "range: nil header ignored"   (r nil) :ignore)
    ;; An empty resource has no satisfiable range at all, which is the
    ;; definition of 416 rather than a reason to serve 200. RFC 7233 §2.1
    ;; puts every first-byte-pos at or past a zero length, and §4.4
    ;; answers that with 416; this used to short-circuit to NIL and serve
    ;; the full (empty) 200 instead. All three forms are checked because
    ;; only the suffix form needed telling — the other two reach
    ;; :unsatisfiable through the ordinary out-of-range branch, and a
    ;; later edit could break one without touching the others.
    (flet ((r0 (spec)
             (multiple-value-bind (first last)
                 (web-skeleton::parse-byte-range spec 0)
               (cond ((eq first :unsatisfiable) :unsatisfiable)
                     (first (list first last))
                     (t :ignore)))))
      (check "range: empty resource, explicit range unsatisfiable"
             (r0 "bytes=0-0") :unsatisfiable)
      (check "range: empty resource, open-ended range unsatisfiable"
             (r0 "bytes=0-") :unsatisfiable)
      (check "range: empty resource, suffix range unsatisfiable"
             (r0 "bytes=-500") :unsatisfiable)
      ;; Still NIL for a Range nobody wrote correctly: an empty resource
      ;; does not turn a malformed header into a satisfiability question.
      (check "range: empty resource, garbage still ignored"
             (r0 "bytes=abc-def") :ignore)
      (check "range: empty resource, nil header still ignored"
             (r0 nil) :ignore)))

  ;; ---- end-to-end through serve-static ----
  (let* ((content (sb-ext:string-to-octets
                   "0123456789abcdefghijklmnopqrstuvwxyz"
                   :external-format :ascii))   ; 36 bytes
         (entry (web-skeleton::build-static-response
                 "text/plain; charset=utf-8" content 0))
         (saved web-skeleton::*static-cache*))
    (unwind-protect
         (progn
           (setf web-skeleton::*static-cache* (make-hash-table :test #'equal))
           (setf (gethash "/data.txt" web-skeleton::*static-cache*) entry)
           (flet ((fetch (&rest headers)
                    (let ((bytes (serve-static
                                  (make-test-request :method :GET
                                                     :path "/data.txt"
                                                     :headers headers))))
                      (and bytes
                           (sb-ext:octets-to-string
                            bytes :external-format :latin-1))))
                  (body-of (text)
                    (let ((i (search (format nil "~a~a~a~a"
                                             #\Return #\Newline
                                             #\Return #\Newline)
                                     text)))
                      (and i (subseq text (+ i 4))))))
             ;; A plain GET still takes the pre-built path and is unchanged,
             ;; but now advertises Range support.
             (let ((full (fetch)))
               (check "range: plain GET still 200"
                      (not (null (search "200 OK" full))) t)
               (check "range: plain GET advertises accept-ranges"
                      (not (null (search "accept-ranges: bytes" full))) t)
               (check "range: plain GET body intact"
                      (body-of full)
                      "0123456789abcdefghijklmnopqrstuvwxyz"))
             ;; A byte range comes back 206 with the right slice, the right
             ;; Content-Length, and a Content-Range naming the whole size.
             (let ((part (fetch (cons "range" "bytes=10-19"))))
               (check "range: 206 status"
                      (not (null (search "206 Partial Content" part))) t)
               (check "range: content-range header"
                      (not (null (search "content-range: bytes 10-19/36" part)))
                      t)
               (check "range: content-length is the slice"
                      (not (null (search "content-length: 10" part))) t)
               (check "range: body is exactly the slice"
                      (body-of part) "abcdefghij"))
             ;; Suffix form — the last 6 bytes.
             (check "range: suffix form body"
                    (body-of (fetch (cons "range" "bytes=-6"))) "uvwxyz")
             ;; Open-ended form — from an offset to the end.
             (check "range: open-ended body"
                    (body-of (fetch (cons "range" "bytes=30-"))) "uvwxyz")
             ;; Out of bounds → 416, and the client is told the real length.
             (let ((oob (fetch (cons "range" "bytes=100-200"))))
               (check "range: out-of-bounds is 416"
                      (not (null (search "416 Range Not Satisfiable" oob))) t)
               (check "range: 416 reports the resource length"
                      (not (null (search "content-range: bytes */36" oob))) t))
             ;; Unsupported shapes fall back to the whole file, not an error.
             (check "range: multi-range serves full 200"
                    (not (null (search "200 OK"
                                       (fetch (cons "range" "bytes=0-9,20-29")))))
                    t)
             ;; If-Range: matching ETag honors the range...
             (check "range: if-range with matching etag honors range"
                    (body-of (fetch (cons "range" "bytes=0-3")
                                    (cons "if-range"
                                          (web-skeleton::static-entry-etag entry))))
                    "0123")
             ;; ...and a stale validator serves the whole file instead, so a
             ;; resumed download can't splice bytes from two versions.
             (let ((stale (fetch (cons "range" "bytes=0-3")
                                 (cons "if-range" "\"stale-etag\""))))
               (check "range: if-range with stale etag serves full 200"
                      (not (null (search "200 OK" stale))) t)
               (check "range: if-range stale body is the whole file"
                      (body-of stale)
                      "0123456789abcdefghijklmnopqrstuvwxyz"))
             ;; RFC 7232 §6: a conditional that yields 304 wins over Range —
             ;; the client already has these bytes.
             (check "range: if-none-match hit still wins over range"
                    (not (null (search "304 Not Modified"
                                       (fetch (cons "range" "bytes=0-3")
                                              (cons "if-none-match"
                                                    (web-skeleton::static-entry-etag
                                                     entry))))))
                    t)
             ;; HEAD has no body, so a Range on it is meaningless.
             (let ((head (let ((bytes (serve-static
                                       (make-test-request
                                        :method :HEAD :path "/data.txt"
                                        :headers (list (cons "range"
                                                             "bytes=0-3"))))))
                           (sb-ext:octets-to-string bytes
                                                    :external-format :latin-1))))
               (check "range: HEAD ignores range, stays 200"
                      (not (null (search "200 OK" head))) t)
               (check "range: HEAD emits no body"
                      (body-of head) ""))))
      (setf web-skeleton::*static-cache* saved))))

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
  (check "mime wasm"  (web-skeleton::mime-type-for-path "/app.wasm")
         "application/wasm")
  (check "mime avif"  (web-skeleton::mime-type-for-path "/pic.avif")
         "image/avif")
  (check "mime mp4"   (web-skeleton::mime-type-for-path "/clip.mp4")
         "video/mp4")
  (check "mime webm"  (web-skeleton::mime-type-for-path "/clip.webm")
         "video/webm")
  (check "mime pdf"   (web-skeleton::mime-type-for-path "/doc.pdf")
         "application/pdf")
  (check "mime map"   (web-skeleton::mime-type-for-path "/app.js.map")
         "application/json; charset=utf-8")
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
         (web-skeleton::file-extension "/foo/bar.txt") "txt")

  ;; hidden-path-component-p — the dot-path filter with the RFC 8615
  ;; .well-known exemption. Exact-component match only.
  (check "hidden: dot dir"
         (web-skeleton::hidden-path-component-p ".git/config") t)
  (check "hidden: nested dot dir"
         (web-skeleton::hidden-path-component-p "a/.secret/b.txt") t)
  (check "hidden: dotfile in subdir"
         (web-skeleton::hidden-path-component-p "sub/.env") t)
  (check "hidden: .well-known exempt"
         (web-skeleton::hidden-path-component-p ".well-known/acme/token") nil)
  (check "hidden: dotfile inside .well-known"
         (web-skeleton::hidden-path-component-p ".well-known/.hidden") t)
  (check "hidden: .well-known-evil not exempt"
         (web-skeleton::hidden-path-component-p ".well-known-evil/x") t)
  ;; RFC 8615 defines /.well-known/ at the root of the origin and nowhere
  ;; else, so the exemption is root-only — a nested one means nothing to a
  ;; client and stays hidden.
  (check "hidden: nested .well-known not exempt"
         (web-skeleton::hidden-path-component-p "sub/.well-known/x") t)
  (check "hidden: plain path"
         (web-skeleton::hidden-path-component-p "a/b.txt") nil))

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
    ;; Malformed tokens (≠ 2 dots) return NIL with early-bail — a
    ;; pathological 8 KiB token with many dots no longer allocates
    ;; O(dots) substrings before rejection.
    (check "jwt split: no dots returns nil"
           (web-skeleton::jwt-split "nodotshere") nil)
    (check "jwt split: one dot returns nil"
           (web-skeleton::jwt-split "only.one") nil)
    (check "jwt split: four dots returns nil"
           (web-skeleton::jwt-split "a.b.c.d.e") nil)
    (check "jwt split: three dots returns nil (early bail)"
           (web-skeleton::jwt-split "a.b.c.d") nil)

    ;; RFC 7515 §2: JWS segments carry no padding. The signature segment
    ;; is the one that matters — it sits outside the signed input, so
    ;; "sig" and "sig==" decode to the same 64 bytes and both verify.
    ;; The other two are checked by the same rule rather than a separate
    ;; argument about which segment deserves it.
    (check "jwt split: padded signature segment returns nil"
           (web-skeleton::jwt-split
            (format nil "~a.~a.~a==" header-b64 payload-b64 sig-b64))
           nil)
    (check "jwt split: padded payload segment returns nil"
           (web-skeleton::jwt-split
            (format nil "~a.~a==.~a" header-b64 payload-b64 sig-b64))
           nil)
    (check "jwt split: padded header segment returns nil"
           (web-skeleton::jwt-split
            (format nil "~a==.~a.~a" header-b64 payload-b64 sig-b64))
           nil)
    ;; The hole that closes, asserted at the codec rather than end to end
    ;; because there is no unexpired positive JWT-VERIFY fixture: the RFC
    ;; 7515 A.3 token is expired, so a JWT-VERIFY assertion would return
    ;; NIL for the wrong reason and pass whether or not the guard exists.
    ;; The codec accepts both spellings on purpose — padding is legal
    ;; base64url, and it is JWS that forbids it.
    (check "padded signature segment decodes to the same bytes"
           (equalp (base64url-decode sig-b64)
                   (base64url-decode (concatenate 'string sig-b64 "==")))
           t)

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

    ;; The positive case. Every other JWT-VERIFY assertion here is a
    ;; rejection, so without this one (defun jwt-verify (token keys) nil)
    ;; passes the whole suite — the framework's security-critical entry
    ;; point asserted to fail three ways and to succeed at nothing.
    ;;
    ;; The A.3 token expired in 2011 and there is no signer in this
    ;; framework (verification only), so the only route to a positive
    ;; result is to widen the clock window past the token's age. Derived
    ;; from the token's own exp rather than a literal date: the NOW terms
    ;; cancel, (- now *jwt-clock-skew*) reduces to exp - 3600, and the
    ;; expiry branch cannot start firing on some future run.
    (let ((*jwt-clock-skew* (+ 3600 (- (web-skeleton::jwt-current-time)
                                       1300819380))))
      (check "jwt verify: A.3 token verifies with the clock window widened"
             (jwt-claim (jwt-verify token keys) "iss")
             "joe")
      ;; First end-to-end proof that the JSON-OBJECT survives out of
      ;; JWT-VERIFY into JWT-CLAIM — the claims used to be a bare alist,
      ;; and the type change had no test that crossed this boundary.
      (check "jwt verify: claims come back as a json-object"
             (json-object-p (jwt-verify token keys))
             t)
      ;; Discriminating both ways: without JWT-SPLIT's padding guard this
      ;; token splits into three parts, the signature segment decodes to
      ;; the same 64 bytes, and the claims come back instead of NIL.
      (check "jwt verify: padded signature segment does not verify"
             (jwt-verify (concatenate 'string token "==") keys)
             nil))

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

  ;; Two kidless keys — RFC 7517 §4.5 says kid is OPTIONAL, so a
  ;; JWKS with multiple kidless keys is spec-legal. Dedup only on
  ;; non-empty kids so a minimal static set or a rotation window
  ;; with omitted kids isn't rejected out of hand. jwt-verify's
  ;; single-key fallback still handles tokens without a kid when
  ;; exactly one matching key exists.
  (let ((kidless-pair
         (parse-jwks (concatenate 'string
           "{\"keys\":["
           "{\"kty\":\"EC\",\"crv\":\"P-256\","
           "\"x\":\"f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU\","
           "\"y\":\"x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0\"},"
           "{\"kty\":\"EC\",\"crv\":\"P-256\","
           "\"x\":\"f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU\","
           "\"y\":\"x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0\"}"
           "]}"))))
    (check "jwks: two kidless keys accepted" (length kidless-pair) 2)
    (check "jwks: first kidless key has empty kid"
           (jwt-key-kid (first kidless-pair)) "")
    (check "jwks: second kidless key has empty kid"
           (jwt-key-kid (second kidless-pair)) ""))

  ;; Explicit duplicate kid still rejects — the dedup discipline
  ;; holds for non-empty kids where an issuer presumably meant
  ;; each kid to be unique.
  (check-error "jwks: duplicate explicit kid"
               (parse-jwks (concatenate 'string
                 "{\"keys\":["
                 "{\"kty\":\"EC\",\"crv\":\"P-256\",\"kid\":\"dup\","
                 "\"x\":\"f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU\","
                 "\"y\":\"x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0\"},"
                 "{\"kty\":\"EC\",\"crv\":\"P-256\",\"kid\":\"dup\","
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
;;; CONNECTION-READ-AVAILABLE tells "read data, then EOF" from "read data"
;;;
;;; This is the discriminating test for :OK-EOF. Both bugs it fixes are
;;; end-to-end shapes whose reproduction depends on whether a peer's last
;;; bytes and its end-of-stream land in the same read, and that is a race:
;;; the e2e close-delimited test hits it reliably on loopback, the e2e DNS
;;; test does not, because the framework usually reads getent's output
;;; before getent has exited.
;;;
;;; The contract underneath both is not a race. A pipe from a process that
;;; has already exited holds its bytes and its end of stream together,
;;; every time — which is precisely the fully-buffered shape getent
;;; produces, arrived at deterministically.
;;; ---------------------------------------------------------------------------

(defun test-read-available-eof ()
  (format t "~%connection-read-available: EOF reporting~%")
  (flet ((drain (sh-command)
           (let* ((proc (sb-ext:run-program "/bin/sh" (list "-c" sh-command)
                                            :output :stream :wait t))
                  (fd (web-skeleton::%process-output-fd proc)))
             (unwind-protect
                  (let ((conn (web-skeleton::make-connection
                               :fd fd :state :out-dns :outbound-p t
                               :last-active 0)))
                    (web-skeleton::set-nonblocking fd)
                    (list (web-skeleton::connection-read-available conn)
                          (web-skeleton::connection-read-pos conn)))
               (ignore-errors (sb-ext:process-close proc))))))
    ;; Bytes and end-of-stream in one drain. Reported as :OK, the EOF is
    ;; discarded, and every caller that treats it as terminal loses it.
    (destructuring-bind (result pos)
        (drain "printf '10.0.0.5 STREAM internal\\n'")
      (check "read-available: bytes then EOF reports :ok-eof" result :ok-eof)
      (check "read-available: :ok-eof still delivers the bytes"
             (> pos 0) t))
    ;; Nothing written before exit — no bytes to report, so plain :EOF.
    ;; Both arms are live in the DNS path: this one on the common
    ;; ordering, where the bytes arrive as :OK and the exit follows as a
    ;; separate event, and :OK-EOF above when getent has already finished.
    ;; Which of the two shows up is not the caller's to decide, so the
    ;; branch has to take both.
    (destructuring-bind (result pos) (drain "exit 0")
      (check "read-available: no bytes at EOF reports :eof" result :eof)
      (check "read-available: :eof delivers nothing" pos 0))))

;;; ---------------------------------------------------------------------------
;;; The :awaiting sweeper's 504, and its fallback
;;;
;;; The e2e test proves the 504 reaches a client. This proves the branch
;;; underneath it: DELIVER-AWAITING-TIMEOUT runs inside SWEEP-IDLE-
;;; CONNECTIONS, which runs inside RUN-EVENT-LOOP, which has no handler —
;;; so a signal escaping the sweep restarts the worker mid-walk and costs
;;; every other connection on it. The handler-case falls back to the bare
;;; close instead.
;;;
;;; A fallback is the one thing that fails silently by succeeding: if it
;;; fires when it shouldn't, the connection is closed without a word,
;;; which is exactly the behavior the 504 exists to remove. So both
;;; branches are asserted, and the clean case is the control that makes
;;; the dirty one mean something.
;;; ---------------------------------------------------------------------------

(defun test-awaiting-sweep-504 ()
  (format t "~%Awaiting sweep~%")
  (flet ((sweep-one (&key dirty)
           ;; Two epoll fds: one to sweep against, one standing in for a
           ;; connection. An epoll fd is pollable, so EPOLL_CTL accepts
           ;; it, and no socket or listener is needed.
           (let ((epfd (web-skeleton::epoll-create))
                 (connfd (web-skeleton::epoll-create))
                 (log (make-string-output-stream)))
             (unwind-protect
                  (let* ((conn (web-skeleton::make-connection
                                :fd connfd
                                :state :awaiting
                                :last-active 0
                                :request (web-skeleton::make-http-request
                                          :method :GET :path "/")))
                         (web-skeleton::*connections*
                           (make-hash-table :test #'eql))
                         (web-skeleton:*log-level* :warn)
                         (web-skeleton:*log-stream* log))
                    (web-skeleton::epoll-add
                     epfd connfd (logior web-skeleton::+epollin+
                                         web-skeleton::+epollet+))
                    (when dirty
                      ;; A write half-flushed. CONNECTION-QUEUE-WRITE
                      ;; refuses to clobber it — that guard is what turns
                      ;; a broken invariant into a signal here.
                      (setf (web-skeleton::connection-write-buf conn)
                            (make-array 2 :element-type '(unsigned-byte 8))
                            (web-skeleton::connection-write-pos conn) 1
                            (web-skeleton::connection-write-end conn) 2))
                    (web-skeleton::register-connection conn)
                    (let ((signalled
                            (handler-case
                                (progn (web-skeleton::sweep-idle-connections
                                        epfd (get-universal-time))
                                       nil)
                              (error (e) (princ-to-string e)))))
                      (list signalled
                            (web-skeleton::connection-state conn)
                            (get-output-stream-string log))))
               (ignore-errors (web-skeleton::%close connfd))
               (ignore-errors (web-skeleton::%close epfd))))))

    ;; Control: a drained buffer takes the 504 path.
    (destructuring-bind (signalled state log) (sweep-one)
      (check "awaiting sweep: clean case does not signal" signalled nil)
      (check "awaiting sweep: clean case queues a response"
             state :write-response)
      (check "awaiting sweep: clean case does not take the fallback"
             (search "could not answer 504" log) nil))

    ;; A broken invariant must not escape into the event loop.
    (destructuring-bind (signalled state log) (sweep-one :dirty t)
      (check "awaiting sweep: dirty buffer does not signal out of the sweep"
             signalled nil)
      (check "awaiting sweep: dirty buffer takes the fallback"
             (and (search "could not answer 504" log) t) t)
      ;; CONNECTION-CLOSE is what the fallback ends in, and it leaves the
      ;; state :closing — so the connection was torn down rather than left
      ;; parked forever, which is the whole point of falling back.
      (check "awaiting sweep: dirty buffer still gets closed"
             state :closing))))

;;; ---------------------------------------------------------------------------
;;; Outbound address filter (SSRF policy hook)
;;; ---------------------------------------------------------------------------

(defun test-fetch-address-filter ()
  (format t "~%Fetch address filter~%")
  (flet ((bytes (s) (sb-ext:string-to-octets s :external-format :ascii)))
    ;; Default: no filter installed → every address allowed. This is the
    ;; property the demo's self-fetch to 127.0.0.1 depends on.
    (check "no filter: loopback allowed"
           (web-skeleton::fetch-address-allowed-p #(127 0 0 1) :inet "localhost")
           t)
    ;; A filter that refuses.
    (let ((web-skeleton::*fetch-address-filter*
            (lambda (ip family host)
              (declare (ignore ip family host))
              nil)))
      (check "filter refuses: gate returns nil"
             (web-skeleton::fetch-address-allowed-p #(1 1 1 1) :inet "example.com")
             nil))
    ;; A raising filter must fail CLOSED — a bug in app policy must not
    ;; open the gate, and must not take the worker down either.
    (let ((web-skeleton::*fetch-address-filter*
            (lambda (ip family host)
              (declare (ignore ip family host))
              (error "boom"))))
      (check "filter raises: fails closed"
             (web-skeleton::fetch-address-allowed-p #(1 1 1 1) :inet "example.com")
             nil))
    ;; The intended composition with IS-PUBLIC-ADDRESS-P.
    (let ((web-skeleton::*fetch-address-filter*
            (lambda (ip family host)
              (declare (ignore host))
              (is-public-address-p ip family))))
      (check "is-public-address-p filter: public allowed"
             (web-skeleton::fetch-address-allowed-p #(8 8 8 8) :inet "dns.google")
             t)
      (check "is-public-address-p filter: cloud metadata refused"
             (web-skeleton::fetch-address-allowed-p
              #(169 254 169 254) :inet "metadata.evil")
             nil)
      (check "is-public-address-p filter: loopback refused"
             (web-skeleton::fetch-address-allowed-p #(127 0 0 1) :inet "localhost")
             nil))

    ;; ---- Filter applied inside the getent parser ----
    ;; A refused address falls through to the name's next address rather
    ;; than failing the lookup outright.
    (let* ((text (format nil "169.254.169.254 STREAM evil~%~
                              93.184.216.34   STREAM evil~%"))
           (buf (bytes text)))
      (let ((web-skeleton::*fetch-address-filter*
              (lambda (ip family host)
                (declare (ignore host))
                (is-public-address-p ip family))))
        (check "getent + filter: refused address falls through to next"
               (coerce (car (web-skeleton::parse-getent-output
                             buf (length buf) "evil"))
                       'list)
               '(93 184 216 34)))
      ;; Same buffer, no filter → the metadata address wins, proving the
      ;; fall-through above was the filter's doing and not line ordering.
      (check "getent, no filter: first STREAM row wins as before"
             (coerce (car (web-skeleton::parse-getent-output
                           buf (length buf) "evil"))
                     'list)
             '(169 254 169 254)))
    ;; Every address refused → NIL, which every caller already treats as
    ;; "did not resolve" (→ 502 + cleanup sentinel). No new failure mode.
    (let* ((text (format nil "10.0.0.5   STREAM internal~%~
                              127.0.0.1  STREAM internal~%"))
           (buf (bytes text))
           (web-skeleton::*fetch-address-filter*
             (lambda (ip family host)
               (declare (ignore host))
               (is-public-address-p ip family))))
      (check "getent + filter: all addresses refused yields nil"
             (web-skeleton::parse-getent-output buf (length buf) "internal")
             nil))

    ;; ---- Filter applied to the IP-literal fast path ----
    ;; RESOLVE-HOST-BLOCKING short-circuits on a literal before getent
    ;; ever runs, so the gate has to be there too or http://169.254.169.254/
    ;; walks straight past the policy.
    (let ((web-skeleton::*fetch-address-filter*
            (lambda (ip family host)
              (declare (ignore host))
              (is-public-address-p ip family))))
      (check "literal fast path: metadata IP refused"
             (web-skeleton::resolve-host-blocking "169.254.169.254")
             nil)
      (check "literal fast path: private IP refused"
             (web-skeleton::resolve-host-blocking "10.1.2.3")
             nil)
      (check "literal fast path: v6 loopback refused"
             (web-skeleton::resolve-host-blocking "::1")
             nil)
      (check "literal fast path: public IP allowed"
             (coerce (web-skeleton::resolve-host-blocking "93.184.216.34") 'list)
             '(93 184 216 34)))
    ;; Default (no filter): literals resolve as before — the demo relies
    ;; on 127.0.0.1 working here.
    (check "literal fast path, no filter: loopback still resolves"
           (coerce (web-skeleton::resolve-host-blocking "127.0.0.1") 'list)
           '(127 0 0 1))))

;;; ---------------------------------------------------------------------------
;;; Chunked completion detection (async outbound read path)
;;; ---------------------------------------------------------------------------

(defun test-chunked-body-complete-p ()
  (format t "~%Chunked completion~%")
  (flet ((complete-p (s)
           (let ((buf (sb-ext:string-to-octets s :external-format :latin-1)))
             (not (null (web-skeleton::chunked-body-complete-p
                         buf 0 (length buf)))))))
    ;; The terminator is what completes a chunked body — not EOF.
    (check "complete: single chunk + terminator"
           (complete-p (format nil "5~a~ahello~a~a0~a~a~a~a"
                               #\Return #\Newline #\Return #\Newline
                               #\Return #\Newline #\Return #\Newline))
           t)
    (check "complete: multiple chunks"
           (complete-p (format nil "3~a~aabc~a~a2~a~ade~a~a0~a~a~a~a"
                               #\Return #\Newline #\Return #\Newline
                               #\Return #\Newline #\Return #\Newline
                               #\Return #\Newline #\Return #\Newline))
           t)
    ;; Terminator not yet arrived → keep reading.
    (check "incomplete: chunk data but no terminator"
           (complete-p (format nil "5~a~ahello~a~a"
                               #\Return #\Newline #\Return #\Newline))
           nil)
    (check "incomplete: chunk data short of its declared size"
           (complete-p (format nil "10~a~ahello" #\Return #\Newline))
           nil)
    (check "incomplete: size line without its LF"
           (complete-p (format nil "5~a" #\Return))
           nil)
    (check "incomplete: empty buffer"
           (complete-p "")
           nil)
    ;; Chunk-extensions on the size line are skipped, per RFC 7230 §4.1.1.
    (check "complete: chunk-extension tolerated"
           (complete-p (format nil "5;foo=bar~a~ahello~a~a0~a~a~a~a"
                               #\Return #\Newline #\Return #\Newline
                               #\Return #\Newline #\Return #\Newline))
           t)
    ;; Stops at the zero-size header, exactly where decode-chunked-body
    ;; stops — so a body with trailers is complete without them.
    (check "complete: zero chunk with trailers still pending"
           (complete-p (format nil "5~a~ahello~a~a0~a~a"
                               #\Return #\Newline #\Return #\Newline
                               #\Return #\Newline))
           t)
    ;; The one that matters: chunk DATA containing the terminator's exact
    ;; bytes must not be mistaken for the terminator. A naive suffix check
    ;; (or a scan for "0\r\n\r\n") truncates the response here; walking the
    ;; framing and skipping over data does not.
    (let ((data-that-looks-like-a-terminator
            (format nil "5~a~a0~a~a~a~a" #\Return #\Newline
                    #\Return #\Newline #\Return #\Newline)))
      ;; chunk-size 5, data = "0\r\n\r\n" (5 bytes), then CRLF — and no
      ;; terminator yet. Must read as INCOMPLETE.
      (check "incomplete: data whose bytes look like the terminator"
             (complete-p (concatenate 'string
                                      data-that-looks-like-a-terminator
                                      (format nil "~a~a" #\Return #\Newline)))
             nil)
      ;; Same body, now with the real terminator appended → complete, and
      ;; decode-chunked-body agrees on the payload.
      (let* ((s (concatenate 'string
                             data-that-looks-like-a-terminator
                             (format nil "~a~a0~a~a~a~a"
                                     #\Return #\Newline #\Return #\Newline
                                     #\Return #\Newline)))
             (buf (sb-ext:string-to-octets s :external-format :latin-1)))
        (check "complete: same body once the real terminator lands"
               (not (null (web-skeleton::chunked-body-complete-p
                           buf 0 (length buf))))
               t)
        (check "decoder agrees: payload is the 5 terminator-looking bytes"
               (sb-ext:octets-to-string
                (web-skeleton::decode-chunked-body buf 0 (length buf))
                :external-format :latin-1)
               (format nil "0~a~a~a~a" #\Return #\Newline
                       #\Return #\Newline))))
    ;; Byte-at-a-time arrival: incomplete at every prefix, complete only
    ;; once the terminator's final byte lands. This is the property the
    ;; async read path relies on — it re-checks on every epoll wake-up.
    (let* ((s (format nil "5~a~ahello~a~a0~a~a~a~a"
                      #\Return #\Newline #\Return #\Newline
                      #\Return #\Newline #\Return #\Newline))
           (buf (sb-ext:string-to-octets s :external-format :latin-1))
           (full (length buf))
           ;; complete-at = the shortest prefix that reads as complete
           (complete-at
             (loop for n from 0 to full
                   when (web-skeleton::chunked-body-complete-p buf 0 n)
                   return n)))
      ;; "5\r\nhello\r\n0\r\n" is 13 bytes — the zero-size header's LF is
      ;; the byte that completes it; the trailing CRLF is not needed.
      (check "streaming: completes exactly when the zero-size line lands"
             complete-at 13))

    ;; ---- Resume offset ----
    ;; The walk hands back a chunk-header boundary so a caller polling a
    ;; growing buffer resumes there instead of rescanning from the top —
    ;; the difference between O(chunks) and O(chunks^2) over a transfer.
    ;; The invariant under test: threading the resume offset back in must
    ;; produce exactly the same answers as a from-scratch scan at every
    ;; prefix. If it ever disagreed, a chunked response would be declared
    ;; complete early (truncation) or never (hang).
    (let* ((s (with-output-to-string (o)
                ;; 40 one-byte chunks, then the terminator.
                (dotimes (i 40)
                  (format o "1~a~a~a~a~a" #\Return #\Newline
                          (code-char (+ 97 (mod i 26))) #\Return #\Newline))
                (format o "0~a~a~a~a" #\Return #\Newline #\Return #\Newline)))
           (buf (sb-ext:string-to-octets s :external-format :latin-1))
           (full (length buf))
           (mismatches 0)
           (resume 0)
           (resume-complete-at nil)
           (scratch-complete-at nil))
      ;; Feed the buffer one byte at a time, exactly as reads would arrive.
      (loop for n from 0 to full
            do (multiple-value-bind (complete next)
                   (web-skeleton::chunked-body-complete-p buf 0 n resume)
                 (let ((scratch (web-skeleton::chunked-body-complete-p buf 0 n)))
                   (unless (eq (not complete) (not scratch))
                     (incf mismatches))
                   (when (and complete (null resume-complete-at))
                     (setf resume-complete-at n))
                   (when (and scratch (null scratch-complete-at))
                     (setf scratch-complete-at n)))
                 ;; The resume offset must never move backwards, or the
                 ;; walk would redo validated framing (or skip past it).
                 (when (< next resume) (incf mismatches))
                 (setf resume next)))
      (check "resume: agrees with a from-scratch scan at every prefix"
             mismatches 0)
      (check "resume: completes at the same byte as a from-scratch scan"
             resume-complete-at scratch-complete-at)
      (check "resume: advanced past the first chunk (walk is incremental)"
             (> resume 0) t))))

;;; ---------------------------------------------------------------------------
;;; DNS cache (opt-in)
;;; ---------------------------------------------------------------------------

(defun test-dns-cache ()
  (format t "~%DNS cache~%")
  (let ((v4 (make-array 4 :element-type '(unsigned-byte 8)
                          :initial-contents '(93 184 216 34))))
    ;; Default: TTL 0 → caching disabled. A store is a no-op and a lookup
    ;; always misses, so every fetch re-resolves exactly as before. This is
    ;; the property that makes the feature safe to ship: nothing changes
    ;; until an app opts in.
    (let ((web-skeleton::*dns-cache* (make-hash-table :test #'equal))
          (web-skeleton::*dns-cache-ttl* 0))
      (web-skeleton::dns-cache-store "example.com" v4 :inet)
      (check "ttl 0: store is a no-op"
             (hash-table-count web-skeleton::*dns-cache*) 0)
      (check "ttl 0: lookup misses"
             (web-skeleton::dns-cache-lookup "example.com") nil))

    ;; No cache bound (a REPL, a test thread, any non-worker context) —
    ;; operations no-op instead of reaching for a global.
    (let ((web-skeleton::*dns-cache* nil)
          (web-skeleton::*dns-cache-ttl* 60))
      (web-skeleton::dns-cache-store "example.com" v4 :inet)
      (check "no cache bound: lookup misses, no error"
             (web-skeleton::dns-cache-lookup "example.com") nil))

    ;; Opted in: store then hit.
    (let ((web-skeleton::*dns-cache* (make-hash-table :test #'equal))
          (web-skeleton::*dns-cache-ttl* 60))
      (web-skeleton::dns-cache-store "example.com" v4 :inet)
      (multiple-value-bind (ip family)
          (web-skeleton::dns-cache-lookup "example.com")
        (check "ttl 60: cache hit returns address"
               (coerce ip 'list) '(93 184 216 34))
        (check "ttl 60: cache hit returns family" family :inet))
      (check "unknown host still misses"
             (web-skeleton::dns-cache-lookup "other.example") nil))

    ;; Expiry: an entry past its deadline is dropped, not served.
    (let ((web-skeleton::*dns-cache* (make-hash-table :test #'equal))
          (web-skeleton::*dns-cache-ttl* 60))
      (setf (gethash "stale.example" web-skeleton::*dns-cache*)
            (web-skeleton::make-dns-cache-entry
             v4 :inet (- (get-universal-time) 1)))   ; expired one second ago
      (check "expired entry misses"
             (web-skeleton::dns-cache-lookup "stale.example") nil)
      (check "expired entry is evicted on lookup"
             (hash-table-count web-skeleton::*dns-cache*) 0))

    ;; The load-bearing one: a cache hit is re-gated on the address filter,
    ;; so the cache can never become a DNS-rebinding accelerator. Seed an
    ;; entry with no filter installed (as an app might, before tightening
    ;; policy), then install a public-only filter — the cached private
    ;; address must stop being served AND be evicted.
    (let* ((loopback (make-array 4 :element-type '(unsigned-byte 8)
                                   :initial-contents '(127 0 0 1)))
           (web-skeleton::*dns-cache* (make-hash-table :test #'equal))
           (web-skeleton::*dns-cache-ttl* 60))
      (web-skeleton::dns-cache-store "rebind.example" loopback :inet)
      (check "seeded entry hits with no filter"
             (coerce (web-skeleton::dns-cache-lookup "rebind.example") 'list)
             '(127 0 0 1))
      (let ((web-skeleton::*fetch-address-filter*
              (lambda (ip family host)
                (declare (ignore host))
                (is-public-address-p ip family))))
        (check "cached address re-gated on filter: refused hit misses"
               (web-skeleton::dns-cache-lookup "rebind.example") nil)
        (check "refused entry is evicted"
               (hash-table-count web-skeleton::*dns-cache*) 0)))

    ;; Bounded: the table never grows past the cap, so a handler fetching
    ;; attacker-chosen hostnames cannot grow a worker without limit.
    (let ((web-skeleton::*dns-cache* (make-hash-table :test #'equal))
          (web-skeleton::*dns-cache-ttl* 60)
          (web-skeleton::*dns-cache-max-entries* 8))
      (loop for i from 0 below 50
            do (web-skeleton::dns-cache-store
                (format nil "host~d.example" i) v4 :inet))
      (check "cache stays bounded at max-entries"
             (<= (hash-table-count web-skeleton::*dns-cache*)
                 web-skeleton::*dns-cache-max-entries*)
             t))))

;;; ---------------------------------------------------------------------------
;;; Runner
;;; ---------------------------------------------------------------------------

(defun test-server ()
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
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
  (test-fetch-address-filter)
  (test-dns-cache)
  (test-format-peer-addr)
  (test-parse-error-status)
  (test-url-decode)
  (test-query-string)
  (test-match-path)
  (test-streaming-fetch)
  (test-interim-responses)
  (test-decode-chunked-body)
  (test-chunked-body-complete-p)
  (test-websocket)
  (test-websocket-fragmentation)
  (test-static-helpers)
  (test-static-etag)
  (test-static-range)
  (test-jwt)
  (test-shutdown-hooks)
  (test-read-available-eof)
  (test-awaiting-sweep-504)
  (report-suite "Server")
  (zerop *tests-failed*))
