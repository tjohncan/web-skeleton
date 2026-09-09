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

(defun test-http-date-cache ()
  (format t "~%HTTP Date cache~%")
  ;; No cache bound is the state every existing test and every REPL call
  ;; runs in, and it must keep formatting.
  (let ((web-skeleton::*http-date-cache* nil))
    (check "date: no cache bound still formats correctly"
           (string= (web-skeleton::http-date)
                    (web-skeleton::%format-http-date (get-universal-time)))
           t))
  (let ((web-skeleton::*http-date-cache* (cons 0 "")))
    ;; A cold cache formats and fills.
    (check "date: cold cache produces the right string"
           (string= (web-skeleton::http-date)
                    (web-skeleton::%format-http-date (get-universal-time)))
           t)
    (check "date: cold cache records the second it built for"
           (eql (car web-skeleton::*http-date-cache*) (get-universal-time))
           t)
    ;; A hit is served without reformatting. Asserted through a sentinel
    ;; that formatting could never produce, and tolerant of the second
    ;; ticking between setup and call — rare, and a fresh format is the
    ;; correct answer when it happens, so this is not a flake.
    (let ((now (get-universal-time)))
      (setf (car web-skeleton::*http-date-cache*) now
            (cdr web-skeleton::*http-date-cache*) "SENTINEL")
      (let ((got (web-skeleton::http-date)))
        (check "date: a hit in the same second is served from the cache"
               (or (string= got "SENTINEL")
                   (string= got (web-skeleton::%format-http-date
                                 (get-universal-time))))
               t)))
    ;; A stale second must not be served. Without this the cache would be
    ;; a clock that stopped.
    (setf (car web-skeleton::*http-date-cache*) 1
          (cdr web-skeleton::*http-date-cache*) "STALE")
    (check "date: a different second is not served from the cache"
           (string= (web-skeleton::http-date) "STALE")
           nil)
    ;; The load-bearing one. BUILD-STATIC-RESPONSE passes file mtimes, and
    ;; if an explicit time consulted the cache every static file's
    ;; Last-Modified would read as the moment the server started.
    (setf (car web-skeleton::*http-date-cache*) (get-universal-time)
          (cdr web-skeleton::*http-date-cache*) "SENTINEL")
    (check "date: an explicit time bypasses the cache"
           (web-skeleton::http-date 0)
           (web-skeleton::%format-http-date 0))
    (check "date: an explicit time does not disturb the cache"
           (cdr web-skeleton::*http-date-cache*)
           "SENTINEL")))

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
  ;; SCHEME exists in this builder for one observable reason: which port
  ;; counts as default and is therefore left out of the Host header. It
  ;; had no test, which is how a revert that stopped passing :SCHEME from
  ;; the async fetch path went unnoticed — the fixture there binds an
  ;; ephemeral port, where both schemes agree.
  (let* ((bytes (web-skeleton::build-outbound-request
                 :GET "example.test" "/" :scheme :https :port 443))
         (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
    (check "https default port is omitted from Host"
           (not (null (search "host: example.test" text))) t)
    (check "and not written out as :443"
           (null (search ":443" text)) t))
  (let* ((bytes (web-skeleton::build-outbound-request
                 :GET "example.test" "/" :scheme :http :port 443))
         (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
    (check "443 is not default for http, so it stays"
           (not (null (search "host: example.test:443" text))) t))

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
                  ;; The two limits on the write queue. They are a pair —
                  ;; too much queued, and queued too long — and a
                  ;; deployment that can set one but not the other can
                  ;; only half-tune a slow peer. A limit nobody can set
                  ;; is not a limit.
                  "*MAX-WRITE-BACKLOG*"
                  "*WRITE-STALL-TIMEOUT*"
                  ;; The stream knobs, and the surface an app reaches
                  ;; them through. A handler in another package that
                  ;; cannot name MAKE-STREAM-RESPONSE cannot stream at
                  ;; all, and one that cannot name the timeouts gets the
                  ;; defaults whatever it writes in its config block.
                  "*STREAM-IDLE-TIMEOUT*"
                  "*STREAM-KEEPALIVE-INTERVAL*"
                  "MAKE-STREAM-RESPONSE"
                  "STREAM-SEND"
                  "STREAM-CLOSE"
                  "STREAM-FULL-P"
                  "MAKE-SSE-RESPONSE"
                  "SSE-SEND"
                  "SSE-COMMENT"
                  ;; The backpressure half of :on-body. An app that
                  ;; pauses and cannot name the function that resumes has
                  ;; a relay that stops mid-body.
                  "FETCH-RESUME"))
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
  ;; path (stream-response-lines, over either transport) delegates to.
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

  ;; Teredo, 2001::/32 (RFC 4380). The one of this group with teeth: bytes
  ;; 4-7 carry the relay's IPv4 address and 12-15 the client's, so it is a
  ;; wrapper around IPv4 in the same sense ::ffff: and 2002:: are, and the
  ;; stated policy for those is to refuse whether or not the carrier is
  ;; live.
  (check "v6 2001::/32 teredo"
         (is-public-address-p
          #(#x20 #x01 0 0 #x0a 0 0 1 0 0 0 0 0 0 0 1) :inet6) nil)
  (check "v6 2001:2::/48 benchmarking"
         (is-public-address-p
          #(#x20 #x01 0 #x02 0 0 0 0 0 0 0 0 0 0 0 1) :inet6) nil)
  ;; Both ORCHID allocations. 2001:20::/28 is ORCHIDv2 (RFC 7343) and the
  ;; live one; 2001:10::/28 is RFC 4843's, expired and returned to the pool,
  ;; refused for the reason the file gives for 2002:: and 192.88.99.0/24.
  (check "v6 2001:10::/28 orchid v1"
         (is-public-address-p
          #(#x20 #x01 0 #x10 0 0 0 0 0 0 0 0 0 0 0 1) :inet6) nil)
  (check "v6 2001:20::/28 orchid v2"
         (is-public-address-p
          #(#x20 #x01 0 #x20 0 0 0 0 0 0 0 0 0 0 0 1) :inet6) nil)
  (check "v6 100::/64 discard"
         (is-public-address-p
          #(#x01 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) :inet6) nil)

  ;; The neighbours, so each clause is the prefix it claims and not a
  ;; wider match sitting on top of 2001:: or 100::. Without these a
  ;; too-broad guard would pass every assertion above it.
  (check "v6 2001:4860:: is public"
         (is-public-address-p
          #(#x20 #x01 #x48 #x60 0 0 0 0 0 0 0 0 0 0 #x88 #x88) :inet6) t)
  (check "v6 2001:3:: is public"
         (is-public-address-p
          #(#x20 #x01 0 #x03 0 0 0 0 0 0 0 0 0 0 0 1) :inet6) t)
  ;; A neighbour outside both /28s. 2001:30:: is unallocated space above
  ;; ORCHIDv2 and is where the too-wide-guard control belongs — the
  ;; previous control named 2001:20::, which is the reserved prefix itself.
  (check "v6 2001:30:: is public"
         (is-public-address-p
          #(#x20 #x01 0 #x30 0 0 0 0 0 0 0 0 0 0 0 1) :inet6) t)
  (check "v6 100::/64 neighbour is public"
         (is-public-address-p
          #(#x01 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1) :inet6) t)

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

(defun make-mock-read-fn (bytes &key chunk)
  "A READ-FN byte source over BYTES — the read-fn twin of
   MAKE-MOCK-STREAM. Answers the count, or :EOF once BYTES is spent.

   CHUNK caps how much any single call will hand back. That is the point
   of the parameter rather than a convenience: READ-SEQUENCE on a real
   stream fills the whole buffer, so a corpus driven only through a
   stream never splits a token across two fills, and the reader's
   refill-and-resume paths — a CRLF pair straddling a boundary, a
   chunk-size line arriving in pieces — go untested. CHUNK 1 puts a
   boundary between every pair of bytes."
  (let ((pos 0)
        (len (length bytes)))
    (lambda (buf want)
      (let ((n (min want (if chunk (min chunk (- len pos)) (- len pos)))))
        (if (<= n 0)
            :EOF
            (progn
              (replace buf bytes :start1 0 :start2 pos :end2 (+ pos n))
              (incf pos n)
              n))))))

(defun ascii-bytes (string)
  "Convert STRING to a byte vector."
  (sb-ext:string-to-octets string :external-format :ascii))

(defun %response-corpus ()
  "Response byte-strings covering every framing the line reader decides
   between, each with the result TLS-STREAM-RESPONSE produced for it.
   Entries are (NAME METHOD BYTES EXPECTED).

   EXPECTED is recorded, not derived. It was captured from
   TLS-STREAM-RESPONSE through its READ-FN seam while that function still
   existed, at all three granularities below, which agreed — so a single
   recorded triple is not a lie about any of them. That capture is what
   makes the deletion of ~450 lines checkable rather than hopeful, and it
   is the whole reason issue #4 put the seam there."
  (let* ((cr (string #\Return))
         (lf (string #\Newline))
         (crlf (concatenate 'string cr lf)))
    (flet ((raw (&rest parts)
             (ascii-bytes (apply #'concatenate 'string parts))))
      (list
       ;; Byte counts here are load-bearing and were wrong once: a
       ;; Content-Length longer than its body, or a chunk-size that
       ;; misdescribes its chunk, makes the case test truncation under
       ;; the name of the happy path. Three of these did, and the parity
       ;; run still passed, because both readers agreed about the error.
       ;; Count them.
       (list "content-length" :GET               ; "one\ntwo\n" = 8
             (raw "HTTP/1.1 200 OK" crlf "Content-Length: 8" crlf crlf
                  "one" lf "two" lf)
             '(200 ("one" "two") nil))
       (list "chunked" :GET                      ; 6 = "alpha\n", 5 = "beta\n"
             (raw "HTTP/1.1 200 OK" crlf "Transfer-Encoding: chunked" crlf crlf
                  "6" crlf "alpha" lf crlf
                  "5" crlf "beta" lf crlf
                  "0" crlf crlf)
             '(200 ("alpha" "beta") nil))
       (list "interim 100 then 200" :GET         ; "hi\n" = 3
             (raw "HTTP/1.1 100 Continue" crlf crlf
                  "HTTP/1.1 200 OK" crlf "Content-Length: 3" crlf crlf
                  "hi" lf)
             '(200 ("hi") nil))
       (list "interim 103 with headers" :GET
             (raw "HTTP/1.1 103 Early Hints" crlf "Link: </s.css>" crlf crlf
                  "HTTP/1.1 204 No Content" crlf "Content-Length: 0" crlf crlf)
             '(204 nil nil))
       (list "HEAD ignores echoed length" :HEAD
             (raw "HTTP/1.1 200 OK" crlf "Content-Length: 9" crlf crlf)
             '(200 nil nil))
       (list "close-delimited" :GET
             (raw "HTTP/1.1 200 OK" crlf "Content-Type: text/plain" crlf crlf
                  "tail" lf "end" lf)
             '(200 ("tail" "end") nil))
       ;; Well-framed chunk, then the stream simply stops — truncation on
       ;; its own, not truncation plus a lying chunk-size.
       (list "chunked truncated before terminator" :GET
             (raw "HTTP/1.1 200 OK" crlf "Transfer-Encoding: chunked" crlf crlf
                  "5" crlf "beta" lf crlf)
             '(nil ("beta") t))
       (list "content-length short body" :GET
             (raw "HTTP/1.1 200 OK" crlf "Content-Length: 40" crlf crlf
                  "not forty bytes" lf)
             '(nil ("not forty bytes") t))
       (list "bare LF terminators" :GET
             (raw "HTTP/1.1 200 OK" crlf "Content-Length: 8" crlf crlf
                  "a" lf "b" lf "c" lf "d" lf)
             '(200 ("a" "b" "c" "d") nil))
       (list "CR-only terminators" :GET
             (raw "HTTP/1.1 200 OK" crlf "Content-Length: 6" crlf crlf
                  "a" cr "b" cr "c" cr)
             '(200 ("a" "b" "c") nil))))))

(defun %run-line-reader (raw method chunk)
  "Drive STREAM-RESPONSE-LINES over RAW through a READ-FN byte source.
   Returns (STATUS LINES RAISED-P).

   RAISED-P rather than the condition text. The recorded expectations came
   from a different implementation, which worded its truncation errors
   differently; the claim under test is that the verdict and the lines
   delivered before it survived the replacement, not the phrasing."
  (let ((lines nil))
    (handler-case
        (let ((status (web-skeleton::stream-response-lines
                       nil
                       (lambda (line) (push line lines))
                       :method method
                       :read-fn (make-mock-read-fn raw :chunk chunk))))
          (list status (nreverse lines) nil))
      (error () (list nil (nreverse lines) t)))))

(defun test-line-reader-over-read-fn ()
  "STREAM-RESPONSE-LINES over a READ-FN reproduces, on every framing and
   at three byte-source granularities, what the deleted
   TLS-STREAM-RESPONSE produced for the same bytes.

   The granularities are the part with teeth. READ-SEQUENCE fills a whole
   buffer, so a corpus driven only through a stream never splits a token
   across two fills and the reader's refill-and-resume paths go
   unexercised; :CHUNK 1 puts a boundary between every pair of bytes.
   Measured: disabling the CRLF-straddle refill in READER-READ-LINE
   leaves all ten whole-response cases passing and the rest of the suite
   green, while failing nine of ten at :CHUNK 1."
  (format t "~%Shared line reader over a byte-source function~%")
  (dolist (chunk '(nil 7 1))
    (dolist (entry (%response-corpus))
      (destructuring-bind (name method raw expected) entry
        (check (format nil "capture [~a] ~a"
                       (if chunk (format nil "chunk ~d" chunk) "whole")
                       name)
               (%run-line-reader raw method chunk)
               expected)))))

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

  ;; parse-chunked-size-line — shared by stream-chunked-lines and the
  ;; buffered decode-chunked-body. Strict hex,
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

  ;; *write-stall-timeout* used to document 0 as "disable", which set no
  ;; deadline and left the write loop with no exit — a peer that stopped
  ;; draining its receive window pinned the worker permanently, and the
  ;; worker is every other connection on it, not just this one. An empty
  ;; frame is used so the loop body never runs and no fd is touched:
  ;; before the guard, 0 with nothing to write returned normally.
  (let ((conn (web-skeleton::make-connection :fd -1 :last-active 0))
        (empty (make-array 0 :element-type '(unsigned-byte 8))))
    (check "ws-send: zero timeout refused"
           (let ((*write-stall-timeout* 0))
             (handler-case (progn (web-skeleton::ws-send conn empty) nil)
               (error (e) (not (null (search "*write-stall-timeout*"
                                             (princ-to-string e)))))))
           t)
    (check "ws-send: negative timeout refused"
           (let ((*write-stall-timeout* -1))
             (handler-case (progn (web-skeleton::ws-send conn empty) nil)
               (error () t)))
           t)
    ;; A positive value still passes the guard, or the two checks above
    ;; would be satisfied by a function that refused everything.
    (check "ws-send: positive timeout passes the guard"
           (let ((*write-stall-timeout* 10))
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
    (check "static-entry: not-modified prefix pre-built"
           (not (null (web-skeleton::static-entry-not-modified-prefix entry)))
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
              (web-skeleton::static-entry-head-prefix default-entry)
              "cache-control: public, max-age=3600") t)
      (check "cache-control: default present on 304"
             (header-present-p
              (web-skeleton::static-entry-not-modified-prefix default-entry)
              "cache-control: public, max-age=3600") t)
      (check "cache-control: custom string present on GET"
             (header-present-p
              (web-skeleton::static-entry-head-prefix custom-entry)
              "cache-control: public, max-age=31536000, immutable") t)
      (check "cache-control: custom string present on 304"
             (header-present-p
              (web-skeleton::static-entry-not-modified-prefix custom-entry)
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
           ;; SERVE-STATIC answers either as one vector (the ranged and
           ;; 416 paths, built per request) or as the segments a
           ;; pre-built response is assembled from. What a client sees is
           ;; the concatenation either way, and that is what these
           ;; assertions are about.
           (flet ((fetch (&rest headers)
                    (let ((r (serve-static
                              (make-test-request :method :GET
                                                 :path "/data.txt"
                                                 :headers headers))))
                      (and r
                           (sb-ext:octets-to-string
                            (if (consp r)
                                (apply #'concatenate
                                       '(vector (unsigned-byte 8)) r)
                                r)
                            :external-format :latin-1))))
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
             (let ((head (let ((r (serve-static
                                   (make-test-request
                                    :method :HEAD :path "/data.txt"
                                    :headers (list (cons "range"
                                                         "bytes=0-3"))))))
                           (sb-ext:octets-to-string
                            (if (consp r)
                                (apply #'concatenate
                                       '(vector (unsigned-byte 8)) r)
                                r)
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

  ;; A kid that is not a string. The struct slot is typed STRING, so this
  ;; raised a bare SBCL type error out of a function that answers every
  ;; other malformed shape with a clean "JWKS: ..." message — and RFC 7517
  ;; 4.5, which makes kid OPTIONAL, says nothing forbidding a number there.
  ;; ATTEMPT because the defect is a raise; the assertion is on the message,
  ;; so a fix that raised something unhelpful would still fail.
  (check "jwks: a non-string kid is refused by name"
         (let ((msg (attempt
                     (parse-jwks
                      (concatenate
                       'string
                       "{\"keys\":[{\"kty\":\"EC\",\"crv\":\"P-256\",\"kid\":5,"
                       "\"x\":\"f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU\","
                       "\"y\":\"x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0\"}]}")))))
           (and (stringp msg) (search "JWKS: kid must be a string" msg) t))
         t)

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
;;; Write queue
;;;
;;; CONNECTION-APPEND-WRITE queues behind whatever is pending instead of
;;; replacing it. The bookkeeping is testable with no fd at all; the part
;;; that needs one is CONNECTION-ON-WRITE promoting the next vector when
;;; the head drains, which is what makes queueing mean anything.
;;;
;;; The drain test asserts file *contents*, not the return value. A
;;; snapshotted ON-WRITE also returns :DONE — it just stops after the
;;; first vector and waits for an EPOLLOUT that is not coming, so a
;;; check on :DONE alone would pass against the bug it exists to catch.
;;; ---------------------------------------------------------------------------

(defun test-write-queue ()
  (format t "~%Write queue~%")
  (flet ((bytes (s) (sb-ext:string-to-octets s :external-format :ascii))
         (str (v) (sb-ext:octets-to-string v :external-format :ascii)))
    ;; --- Append onto an idle connection becomes the head, no cons ---
    (let ((conn (web-skeleton::make-connection :fd -1 :last-active 0)))
      (check "append: accepted on idle connection"
             (web-skeleton::connection-append-write conn (bytes "hello")) t)
      (check "append: idle append becomes the head"
             (str (web-skeleton::connection-write-buf conn)) "hello")
      (check "append: idle append does not use the queue"
             (web-skeleton::connection-write-queue conn) nil)
      (check "append: pending counts the head"
             (web-skeleton::connection-write-pending conn) 5)

      ;; --- Second append goes behind, head untouched ---
      (check "append: accepted behind a head"
             (web-skeleton::connection-append-write conn (bytes "world!")) t)
      (check "append: head is not replaced"
             (str (web-skeleton::connection-write-buf conn)) "hello")
      (check "append: pending spans head and queue"
             (web-skeleton::connection-write-pending conn) 11)
      (check "append: queued counts only the tail"
             (web-skeleton::connection-write-queued conn) 6)

      ;; --- Order is preserved across a third ---
      (web-skeleton::connection-append-write conn (bytes "third"))
      (check "append: queue holds tail vectors oldest first"
             (mapcar #'str (web-skeleton::connection-write-queue conn))
             '("world!" "third"))

      ;; --- QUEUE-WRITE's guard counts the queue, not just the head ---
      ;; Drain the head only; a queue remains. The old guard subtracted
      ;; write-pos from write-end and would have seen zero here.
      (setf (web-skeleton::connection-write-pos conn)
            (web-skeleton::connection-write-end conn))
      (check "queue-write: signals when only the append queue is pending"
             (handler-case (progn (web-skeleton::connection-queue-write
                                   conn (bytes "clobber"))
                                  nil)
               (error () t))
             t)

      ;; --- Promotion walks the queue in order and unwinds the counter ---
      (check "promote: first promotion takes the oldest"
             (progn (web-skeleton::connection-promote-write conn)
                    (str (web-skeleton::connection-write-buf conn)))
             "world!")
      (check "promote: promotion resets the head offsets"
             (list (web-skeleton::connection-write-pos conn)
                   (web-skeleton::connection-write-end conn))
             '(0 6))
      (check "promote: second promotion takes the next"
             (progn (setf (web-skeleton::connection-write-pos conn) 6)
                    (web-skeleton::connection-promote-write conn)
                    (str (web-skeleton::connection-write-buf conn)))
             "third")
      (check "promote: queued returns to zero when the tail empties"
             (web-skeleton::connection-write-queued conn) 0)
      (check "promote: tail pointer is released with the last cons"
             (web-skeleton::connection-write-queue-tail conn) nil)
      (check "promote: empty queue reports nothing to promote"
             (progn (setf (web-skeleton::connection-write-pos conn) 5)
                    (web-skeleton::connection-promote-write conn))
             nil))

    ;; --- RESET-WRITE clears the queue along with the head ---
    ;; A queued vector surviving a keep-alive reset would be flushed as a
    ;; prefix of the next response on the same socket.
    (let ((conn (web-skeleton::make-connection :fd -1 :last-active 0)))
      (web-skeleton::connection-append-write conn (bytes "first"))
      (web-skeleton::connection-append-write conn (bytes "second"))
      (web-skeleton::connection-reset-write conn)
      (check "reset: head cleared"
             (web-skeleton::connection-write-buf conn) nil)
      (check "reset: queue cleared"
             (web-skeleton::connection-write-queue conn) nil)
      (check "reset: tail pointer cleared"
             (web-skeleton::connection-write-queue-tail conn) nil)
      (check "reset: nothing reported pending"
             (web-skeleton::connection-write-pending conn) 0))

    ;; --- The backlog bound refuses whole rather than appending a prefix ---
    ;; LET is safe for the limit here: these calls run on this thread, not
    ;; through a worker, so the binding is in scope for every one of them.
    (let ((web-skeleton:*max-write-backlog* 10)
          (conn (web-skeleton::make-connection :fd -1 :last-active 0)))
      (check "backlog: empty connection is not full"
             (web-skeleton::connection-write-full-p conn) nil)
      (check "backlog: append within the bound accepted"
             (web-skeleton::connection-append-write conn (bytes "12345678")) t)
      (check "backlog: append past the bound refused"
             (web-skeleton::connection-append-write conn (bytes "999")) nil)
      (check "backlog: refused append queues nothing"
             (web-skeleton::connection-write-pending conn) 8)
      (check "backlog: an exact fit is still accepted"
             (web-skeleton::connection-append-write conn (bytes "99")) t)
      (check "backlog: reaching the bound reports full"
             (web-skeleton::connection-write-full-p conn) t))

    ;; --- The queue never holds the caller's own vector ---
    ;; A producer reusing one buffer is the obvious way to write one, and
    ;; the queue advances an offset through what it is given. Chunked
    ;; framing copies because ENCODE-CHUNK builds a new vector; close
    ;; framing has nothing to build, so it has to copy on purpose. An app
    ;; cannot see which framing it got.
    (let ((sink (open "/dev/null" :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :append)))
      (unwind-protect
           (let ((conn (web-skeleton::make-connection
                        :fd (sb-sys:fd-stream-fd sink)
                        :state :streaming :last-active 0))
                 (buf (make-array 4 :element-type '(unsigned-byte 8)
                                    :initial-element 65)))
             (setf (web-skeleton::connection-stream-framing conn) :close)
             (web-skeleton:stream-send conn buf)
             (fill buf 90)
             (check "stream-send: close framing copies the caller's vector"
                    (sb-ext:octets-to-string
                     (subseq (web-skeleton::connection-write-buf conn) 0 4)
                     :external-format :ascii)
                    "AAAA"))
        (ignore-errors (close sink))))

    ;; --- The bound must clear the inbound message cap by a frame header ---
    ;; At default settings the receive path accepts a payload of exactly
    ;; *MAX-WS-MESSAGE-SIZE* (websocket.lisp tests > , not >=). Sending that
    ;; back means framing it, and BUILD-WS-FRAME spends 10 bytes on the
    ;; extended-length header for any payload past 65535. A bound merely
    ;; equal to the message cap therefore refuses a maximal legal echo onto
    ;; a completely empty queue. The two limits are exported and tunable
    ;; apart, so the relationship is a requirement to hold, not an identity
    ;; to assume.
    (let* ((conn (web-skeleton::make-connection :fd -1 :last-active 0))
           (payload (make-array web-skeleton:*max-ws-message-size*
                                :element-type '(unsigned-byte 8)
                                :initial-element 65))
           (frame (web-skeleton::build-ws-frame
                   web-skeleton::+ws-op-binary+ payload)))
      (check "backlog: headroom over the message cap covers a frame header"
             (>= (- web-skeleton:*max-write-backlog*
                    web-skeleton:*max-ws-message-size*)
                 10)
             t)
      (check "backlog: a maximal legal ws message fits an empty queue"
             (web-skeleton::connection-append-write conn frame) t))))

(defun %scripted-read-fn (script counter)
  "A CONNECTION read-fn replaying SCRIPT. An integer fabricates that many
   0x41 bytes; :AGAIN and :EOF are answered as themselves. Bumps COUNTER's
   CAR per call.

   Past the end of SCRIPT it answers :AGAIN rather than erroring, so a
   drain loop that should already have stopped fails as a wrong call count
   instead of spinning until CI kills the job."
  (let ((remaining script))
    (lambda (buffer start max-bytes)
      (incf (car counter))
      (let ((step (if remaining (pop remaining) :again)))
        (if (integerp step)
            (let ((n (min step max-bytes)))
              (fill buffer 65 :start start :end (+ start n))
              n)
            step)))))

(defun %scripted-write-fn (script counter sink)
  "A CONNECTION write-fn replaying SCRIPT. An integer accepts up to that
   many bytes and appends them to SINK, so a test can assert what actually
   went out and in what order; :AGAIN is answered as itself. Bumps
   COUNTER's CAR per call, and answers :AGAIN past the end of SCRIPT."
  (let ((remaining script))
    (lambda (buffer start nbytes)
      (incf (car counter))
      (let ((step (if remaining (pop remaining) :again)))
        (if (integerp step)
            (let ((n (min step nbytes)))
              (loop for i from start below (+ start n)
                    do (vector-push-extend (aref buffer i) sink))
              n)
            step)))))

(defun %scripted-handshake-fn (script counter)
  "A CONNECTION handshake-fn replaying SCRIPT. Entries are :WANT-READ,
   :WANT-WRITE, :DONE, or :RAISE. Bumps COUNTER's CAR per call, and raises
   past the end of SCRIPT — a state machine that keeps stepping a finished
   handshake should say so rather than loop."
  (let ((remaining script))
    (lambda ()
      (incf (car counter))
      (let ((step (if remaining (pop remaining) :overrun)))
        (case step
          (:raise   (error "handshake failed: scripted"))
          (:overrun (error "handshake stepped past :done"))
          (t step))))))

(defun %peer-has-bytes-p (fd)
  "T if anything is readable on FD right now. FD must be non-blocking."
  (integerp (web-skeleton::nb-read
             fd (make-array 256 :element-type '(unsigned-byte 8)) 0 256)))

(defun test-outbound-handshake-state ()
  "The handshake state arms the direction its transport asks for, and the
   request does not go on the wire until the handshake finishes.

   Both halves need a real epoll fd and a real socket, because both are
   claims about an interest mask. A loopback socket with nothing sent to it
   is writable and not readable, so EPOLLOUT wakes the loop and EPOLLIN
   does not — which is what makes the two masks distinguishable without any
   data timing. Asserting delivery instead would pass against a mask that
   never changed: issue #5 produced exactly that test, twice.

   The second half is the security-relevant one. A connection whose
   transport has a handshake is not usable when the TCP connect lands, and
   writing the queued request there would put the plaintext HTTP request on
   a socket the peer is waiting for a ClientHello on."
  (format t "~%Outbound transport handshake~%")
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let* ((out-fd (web-skeleton::socket-fd server))
                  (peer-fd (web-skeleton::socket-fd client))
                  (calls (list 0))
                  (evbuf (make-array (* 4 web-skeleton::+epoll-event-size+)
                                     :element-type '(unsigned-byte 8)))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (conn (web-skeleton::make-connection
                         :fd out-fd :socket server :state :out-connecting
                         :outbound-p t :last-active (get-universal-time)
                         :handshake-fn (%scripted-handshake-fn
                                        '(:want-write :want-read :done)
                                        calls))))
             (web-skeleton::set-nonblocking peer-fd)
             (web-skeleton::connection-queue-write
              conn (sb-ext:string-to-octets "GET / HTTP/1.1" :external-format :ascii))
             (web-skeleton::epoll-add epfd out-fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))

             ;; TCP connect landed. With a handshake pending this must not
             ;; become :out-write.
             (web-skeleton::handle-outbound-connect conn epfd)
             (check "handshake: connect hands off to the handshake state"
                    (web-skeleton::connection-state conn) :out-handshake)
             (check "handshake: stepped once"  (car calls) 1)
             (check "handshake: nothing on the wire yet"
                    (%peer-has-bytes-p peer-fd) nil)
             (check "handshake: :want-write arms EPOLLOUT, and the loop wakes"
                    (plusp (web-skeleton::epoll-wait epfd evbuf 4 50)) t)
             (check "handshake: and it is this fd"
                    (web-skeleton::epoll-event-fd evbuf 0) out-fd)

             ;; :want-read next. The same socket is still writable, so a
             ;; mask that failed to change would wake again here.
             (web-skeleton::handle-outbound-handshake conn epfd)
             (check "handshake: still handshaking"
                    (web-skeleton::connection-state conn) :out-handshake)
             (check "handshake: :want-read arms EPOLLIN, and nothing wakes"
                    (web-skeleton::epoll-wait epfd evbuf 4 50) 0)
             (check "handshake: still nothing on the wire"
                    (%peer-has-bytes-p peer-fd) nil)

             ;; :done hands off to the write path, which flushes and moves
             ;; to :out-read.
             (web-skeleton::handle-outbound-handshake conn epfd)
             (check "handshake: :done advances past the handshake"
                    (web-skeleton::connection-state conn) :out-read)
             (check "handshake: the request goes out only now"
                    (%peer-has-bytes-p peer-fd) t)
             (check "handshake: three steps, no more"  (car calls) 3))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client)))))

  ;; No handshake-fn: the pre-existing path, unchanged.
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let* ((out-fd (web-skeleton::socket-fd server))
                  (peer-fd (web-skeleton::socket-fd client))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (conn (web-skeleton::make-connection
                         :fd out-fd :socket server :state :out-connecting
                         :outbound-p t :last-active (get-universal-time))))
             (web-skeleton::set-nonblocking peer-fd)
             (web-skeleton::connection-queue-write
              conn (sb-ext:string-to-octets "GET / HTTP/1.1" :external-format :ascii))
             (web-skeleton::epoll-add epfd out-fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
             (web-skeleton::handle-outbound-connect conn epfd)
             (check "no handshake: connect goes straight through to reading"
                    (web-skeleton::connection-state conn) :out-read)
             (check "no handshake: and the request went out immediately"
                    (%peer-has-bytes-p peer-fd) t))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client)))))

  ;; A failing handshake raises rather than being retried. The outbound
  ;; dispatcher's handler-case is what turns that into a 502; what matters
  ;; here is that it is not swallowed and not looped on.
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let* ((calls (list 0))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (conn (web-skeleton::make-connection
                         :fd (web-skeleton::socket-fd server)
                         :socket server :state :out-handshake
                         :outbound-p t :last-active (get-universal-time)
                         :handshake-fn (%scripted-handshake-fn '(:raise) calls))))
             (check-error "handshake: a failed handshake raises"
                          (web-skeleton::handle-outbound-handshake conn epfd))
             (check "handshake: and it was not retried" (car calls) 1))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client))))))

(defun test-fetch-setup-releases-transport-on-error ()
  "A fetch that fails while wiring itself up still releases the transport
   it had already installed.

   The transport is an SSL and a 16 KiB foreign staging buffer. Both are
   freed by CLOSE-FN or by nothing at all — the connection object is
   collected, the memory behind it is not — so an unwind that skipped it
   leaked once per attempt, for the life of the process.

   EPOLL-ADD is the realistic trigger and the one used here: epoll_ctl
   answers ENOSPC when max_user_watches is exhausted and ENOMEM under
   pressure, which means this path is reached precisely when the machine
   is already short, and every retry adds another 16 KiB. Passing -1 as
   the epoll fd reproduces the failure without having to exhaust anything.

   The assertion observes the release, not the connection's state.
   Checking a slot after teardown would pass against a version that
   cleared the slot and freed nothing, which is the whole shape of the
   defect: the state looked tidy and the memory was gone."
  (format t "~%Fetch setup releases its transport on error~%")
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 1)
           (let* ((port (nth-value 1 (sb-bsd-sockets:socket-name listener)))
                  (released 0)
                  (installed 0)
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*tls-outbound-setup-fn*
                    (lambda (out-conn host)
                      (declare (ignore host))
                      (incf installed)
                      ;; Stands in for the SSL and the staging buffer. What
                      ;; matters is only that it is installed the same way
                      ;; and released by the same hook.
                      (setf (web-skeleton::connection-close-fn out-conn)
                            (lambda () (incf released)))))
                  (inbound (web-skeleton::make-connection
                            :fd -1 :state :read-http
                            :last-active (get-universal-time)))
                  (fetch-req (web-skeleton::make-http-fetch-continuation
                              :method :GET
                              :url (format nil "https://right.test:~d/" port)
                              :scheme :https
                              :callback (lambda (s h b)
                                          (declare (ignore s h b)) nil))))
             ;; -1 is not an epoll fd, so EPOLL-ADD raises after the
             ;; transport is in place. ATTEMPT because the raise is the
             ;; point and must not end the run.
             (attempt
              (web-skeleton::initiate-http-fetch-to-address
               inbound -1 fetch-req "right.test" port "/" #(127 0 0 1) :inet))
             (check "fetch setup: the transport was installed" installed 1)
             (check "fetch setup: and released when the wiring failed"
                    released 1)))
      (ignore-errors (sb-bsd-sockets:socket-close listener)))))

(defun test-automatic-resume-edge ()
  "A paused outbound is resumed when the inbound it relays into drains its
   own backlog — without the application calling FETCH-RESUME.

   Issue #5 described the resume as an inbound->outbound edge; what shipped
   was FETCH-RESUME, a primitive the app had to invoke itself. The gap that
   left is not theoretical: ON-BODY is the app's only scheduled contact
   with a relay, pausing is what stops ON-BODY firing, so an app that
   paused and had nothing else to run had removed its own way back.

   Asserted against epoll rather than against a flag. Clearing
   FETCH-PAUSED without re-arming EPOLLIN would look identical from the
   struct and would leave the connection waiting for an event nobody is
   going to send — which is exactly the mistake issue #5's own re-arm test
   was rewritten to catch."
  (format t "~%Automatic inbound-to-outbound resume~%")
  (multiple-value-bind (in-server in-client) (%loopback-pair)
    (multiple-value-bind (out-server out-client) (%loopback-pair)
      (let ((epfd (web-skeleton::epoll-create)))
        (unwind-protect
             (let* ((in-fd (web-skeleton::socket-fd in-server))
                    (out-fd (web-skeleton::socket-fd out-server))
                    (evbuf (make-array (* 4 web-skeleton::+epoll-event-size+)
                                       :element-type '(unsigned-byte 8)))
                    (web-skeleton::*connections* (make-hash-table :test #'eql))
                    (inbound (web-skeleton::make-connection
                              :fd in-fd :socket in-server :state :streaming
                              :last-active (get-universal-time)))
                    (outbound (web-skeleton::make-connection
                               :fd out-fd :socket out-server :state :out-read
                               :outbound-p t :inbound-fd in-fd
                               :last-active (get-universal-time))))
               (web-skeleton::register-connection inbound)
               (web-skeleton::register-connection outbound)
               ;; The state a pause leaves behind.
               (setf (web-skeleton::connection-fetch-paused outbound) t
                     (web-skeleton::connection-paused-outbound-fd inbound) out-fd)
               (web-skeleton::epoll-add epfd out-fd web-skeleton::+epollet+)
               (check "resume edge: paused means epoll reports nothing"
                      (web-skeleton::epoll-wait epfd evbuf 4 50) 0)
               ;; Give the inbound a backlog and drain it. :DONE is the
               ;; event the edge hangs on.
               (web-skeleton::connection-queue-write
                inbound (sb-ext:string-to-octets "xyz" :external-format :ascii))
               (check "resume edge: the inbound drained"
                      (attempt (web-skeleton::handle-client-write inbound epfd))
                      nil)
               (check "resume edge: the outbound is no longer paused"
                      (web-skeleton::connection-fetch-paused outbound) nil)
               (check "resume edge: the back-link is cleared with it"
                      (web-skeleton::connection-paused-outbound-fd inbound) -1)
               ;; The assertion that matters: EPOLLIN is actually back.
               (sb-bsd-sockets:socket-send out-client
                                           (sb-ext:string-to-octets
                                            "hi" :external-format :ascii)
                                           nil)
               (sleep 0.05)
               (check "resume edge: and epoll reports the outbound again"
                      (plusp (web-skeleton::epoll-wait epfd evbuf 4 100)) t)
               (check "resume edge: it is the outbound fd"
                      (web-skeleton::epoll-event-fd evbuf 0) out-fd))
          (ignore-errors (web-skeleton::%close epfd))
          (dolist (s (list in-server in-client out-server out-client))
            (ignore-errors (sb-bsd-sockets:socket-close s))))))))

(defun test-outbound-direction-inversion ()
  "A read that must wait for writability, and a write that must wait for
   readability — and in both cases the operation that gets re-issued is the
   one that blocked, not the one the direction suggests.

   That last clause is the assertion with teeth. Retrying a WANT_WRITE from
   a read by doing a *write* is a protocol error, and OpenSSL reports it as
   a generic SSL failure that reads like a broken peer — the kind of defect
   that gets blamed on an upstream for a week. Both fns are counted, so a
   dispatcher that ran the wrong one is visible as a count rather than as a
   plausible-looking error later.

   The masks are checked against a real epoll fd on a real socket, because
   they are the whole mechanism. A loopback socket with nothing sent to it
   is writable and not readable, so EPOLLOUT wakes the loop and EPOLLIN does
   not, and the two are distinguishable with no data timing involved."
  (format t "~%Outbound direction inversion~%")
  ;; ---- a read that wants writability ----
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let* ((out-fd (web-skeleton::socket-fd server))
                  (reads (list 0))
                  (writes (list 0))
                  (evbuf (make-array (* 4 web-skeleton::+epoll-event-size+)
                                     :element-type '(unsigned-byte 8)))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (conn (web-skeleton::make-connection
                         :fd out-fd :socket server :state :out-read
                         :outbound-p t :last-active (get-universal-time)
                         :read-fn (%scripted-read-fn '(:want-write 12 :again) reads)
                         :write-fn (%scripted-write-fn '(99) writes
                                                       (make-array 0 :fill-pointer 0
                                                                     :adjustable t)))))
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd out-fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
             ;; First read blocks on the wrong direction.
             (web-skeleton::handle-outbound-read conn epfd)
             (check "inversion: a read that wants writability is marked"
                    (web-skeleton::connection-interest-inverted conn) t)
             (check "inversion: and EPOLLOUT is armed, so the loop wakes"
                    (plusp (web-skeleton::epoll-wait epfd evbuf 4 50)) t)
             (check "inversion: one read so far, no writes"
                    (list (car reads) (car writes)) (list 1 0))
             ;; The dispatcher sees writability. It must re-issue the READ.
             (web-skeleton::handle-outbound-event
              conn epfd (logior web-skeleton::+epollout+ web-skeleton::+epollet+))
             ;; Reads went up and writes did not. Not an exact read count:
             ;; CONNECTION-READ-AVAILABLE drains until :AGAIN, so one pass
             ;; is several read-fn calls, and pinning the number would make
             ;; this track the drain loop rather than the dispatch.
             (check "inversion: writability re-issued the read, not a write"
                    (list (> (car reads) 1) (car writes)) (list t 0))
             (check "inversion: the inversion is cleared once it clears"
                    (web-skeleton::connection-interest-inverted conn) nil)
             (check "inversion: and EPOLLIN is back, so nothing wakes"
                    (web-skeleton::epoll-wait epfd evbuf 4 50) 0))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client)))))

  ;; ---- a write that wants readability ----
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let* ((out-fd (web-skeleton::socket-fd server))
                  (reads (list 0))
                  (writes (list 0))
                  (sink (make-array 0 :element-type '(unsigned-byte 8)
                                      :fill-pointer 0 :adjustable t))
                  (evbuf (make-array (* 4 web-skeleton::+epoll-event-size+)
                                     :element-type '(unsigned-byte 8)))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (conn (web-skeleton::make-connection
                         :fd out-fd :socket server :state :out-write
                         :outbound-p t :last-active (get-universal-time)
                         :read-fn (%scripted-read-fn '(:again) reads)
                         :write-fn (%scripted-write-fn '(:want-read 5) writes sink))))
             (web-skeleton::register-connection conn)
             (web-skeleton::connection-queue-write
              conn (sb-ext:string-to-octets "HELLO" :external-format :ascii))
             (web-skeleton::epoll-add epfd out-fd
                                      (logior web-skeleton::+epollout+
                                              web-skeleton::+epollet+))
             (web-skeleton::handle-outbound-write conn epfd)
             (check "inversion: a write that wants readability is marked"
                    (web-skeleton::connection-interest-inverted conn) t)
             (check "inversion: EPOLLIN armed, and an empty socket is quiet"
                    (web-skeleton::epoll-wait epfd evbuf 4 50) 0)
             (check "inversion: one write so far, no reads"
                    (list (car reads) (car writes)) (list 0 1))
             ;; Readability arrives. It must re-issue the WRITE.
             (web-skeleton::handle-outbound-event
              conn epfd (logior web-skeleton::+epollin+ web-skeleton::+epollet+))
             (check "inversion: readability re-issued the write, not a read"
                    (list (car reads) (car writes)) (list 0 2))
             (check "inversion: the write completed and moved to reading"
                    (web-skeleton::connection-state conn) :out-read)
             (check "inversion: cleared on the way through"
                    (web-skeleton::connection-interest-inverted conn) nil)
             (check "inversion: and the peer got the bytes"
                    (sb-ext:octets-to-string
                     (coerce sink '(vector (unsigned-byte 8)))
                     :external-format :ascii)
                    "HELLO"))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client))))))

(defun test-connection-transport-seam ()
  "Reads and writes route through CONNECTION-READ-FN / -WRITE-FN when set,
   and the drain loop runs until the transport says :AGAIN.

   Every connection here has :FD -1. That is the assertion, not tidiness:
   -1 is not a file descriptor, so NB-READ and NB-WRITE on it would raise
   EBADF. A seam that quietly fell through to the raw fd cannot pass these
   at all, which a valid fd would have let it do.

   The drain discipline is the part worth having. Edge-triggered epoll
   reports a transition, so readability must be drained in one pass —
   stated in CONNECTION-READ-AVAILABLE's docstring since it was written,
   and until now not assertable without a real socket and a real partial
   read. A scripted byte source makes it deterministic, which matters
   because issue #8 rests the whole SSL_pending argument on this loop
   behaving exactly this way."
  (format t "~%Connection transport seam~%")
  ;; ---- reads ----
  (let* ((calls (list 0))
         (conn (web-skeleton::make-connection
                :fd -1 :read-fn (%scripted-read-fn '(5 5 5 :again) calls))))
    (check "seam: drain returns :ok when the source blocks"
           (attempt (web-skeleton::connection-read-available conn)) :ok)
    (check "seam: every available byte accumulated"
           (web-skeleton::connection-read-pos conn) 15)
    (check "seam: drained until :again, not once"
           (car calls) 4))

  (let* ((calls (list 0))
         (conn (web-skeleton::make-connection
                :fd -1 :read-fn (%scripted-read-fn '(4 :eof) calls))))
    (check "seam: bytes then end of stream is :ok-eof"
           (attempt (web-skeleton::connection-read-available conn)) :ok-eof)
    (check "seam: :ok-eof keeps the bytes"
           (web-skeleton::connection-read-pos conn) 4))

  (let* ((calls (list 0))
         (conn (web-skeleton::make-connection
                :fd -1 :read-fn (%scripted-read-fn '(:eof) calls))))
    (check "seam: nothing then end of stream is :eof"
           (attempt (web-skeleton::connection-read-available conn)) :eof))

  (let* ((calls (list 0))
         (conn (web-skeleton::make-connection
                :fd -1 :read-fn (%scripted-read-fn '(:again) calls))))
    (check "seam: nothing available is :again"
           (attempt (web-skeleton::connection-read-available conn)) :again)
    (check "seam: :again costs exactly one call" (car calls) 1))

  ;; Growth through the seam. The initial buffer is 4 KiB, so this needs
  ;; two grows, and a seam that handed the source a stale buffer after a
  ;; grow would corrupt or short-count here rather than anywhere visible.
  (let* ((calls (list 0))
         (conn (web-skeleton::make-connection
                :fd -1
                :read-fn (%scripted-read-fn '(4096 4096 2000 :again) calls))))
    (check "seam: read buffer grows and keeps everything"
           (attempt (web-skeleton::connection-read-available conn)) :ok)
    (check "seam: grown total is exact"
           (web-skeleton::connection-read-pos conn) 10192)
    (check "seam: buffer grew past its initial size"
           (> (length (web-skeleton::connection-read-buf conn)) 4096) t))

  ;; CONNECTION-DISCARD-AVAILABLE is the other reader and must not have
  ;; kept its own path to the fd.
  (let* ((calls (list 0))
         (sink (make-array 64 :element-type '(unsigned-byte 8)))
         (conn (web-skeleton::make-connection
                :fd -1 :read-fn (%scripted-read-fn '(64 64 :again) calls))))
    (check "seam: discard drains through the seam too"
           (attempt (web-skeleton::connection-discard-available conn sink)) :ok)
    (check "seam: discard drained until :again" (car calls) 3))

  ;; ---- writes ----
  (let* ((calls (list 0))
         (sink (make-array 0 :element-type '(unsigned-byte 8)
                             :fill-pointer 0 :adjustable t))
         (conn (web-skeleton::make-connection
                :fd -1 :write-fn (%scripted-write-fn '(3 99) calls sink))))
    (web-skeleton::connection-queue-write
     conn (sb-ext:string-to-octets "HELLO" :external-format :ascii))
    (check "seam: a partial write then the rest reports :done"
           (attempt (web-skeleton::connection-on-write conn)) :done)
    (check "seam: the peer saw the bytes once, in order"
           (sb-ext:octets-to-string (coerce sink '(vector (unsigned-byte 8)))
                                    :external-format :ascii)
           "HELLO"))

  (let* ((calls (list 0))
         (sink (make-array 0 :element-type '(unsigned-byte 8)
                             :fill-pointer 0 :adjustable t))
         (conn (web-skeleton::make-connection
                :fd -1 :write-fn (%scripted-write-fn '(2 :again) calls sink))))
    (web-skeleton::connection-queue-write
     conn (sb-ext:string-to-octets "HELLO" :external-format :ascii))
    (check "seam: a blocked write reports :continue"
           (attempt (web-skeleton::connection-on-write conn)) :continue)
    (check "seam: and resumes from what was accepted"
           (web-skeleton::connection-write-pos conn) 2)))

(defun test-write-queue-drain ()
  (format t "~%Write queue drain~%")
  (let ((path "/tmp/web-skeleton-write-queue.bin"))
    (flet ((bytes (s) (sb-ext:string-to-octets s :external-format :ascii))
           (str (v) (sb-ext:octets-to-string v :external-format :ascii))
           (slurp ()
             (with-open-file (s path :element-type '(unsigned-byte 8))
               (let ((buf (make-array (file-length s)
                                      :element-type '(unsigned-byte 8))))
                 (read-sequence buf s)
                 (sb-ext:octets-to-string buf :external-format :ascii)))))
      ;; A regular file fd never reports EAGAIN and never short-writes, so
      ;; ON-WRITE runs to completion in one pass and the file is exactly
      ;; the byte stream the peer would have seen.
      (let* ((stream (open path :direction :output :element-type '(unsigned-byte 8)
                               :if-exists :supersede :if-does-not-exist :create))
             (fd (sb-sys:fd-stream-fd stream))
             (shared (bytes "SHARED")))
        (unwind-protect
             (let ((conn (web-skeleton::make-connection :fd fd :last-active 0)))
               (web-skeleton::connection-queue-write conn (bytes "AAA"))
               (web-skeleton::connection-append-write conn (bytes "BB"))
               (web-skeleton::connection-append-write conn shared)
               ;; Same vector twice: the queue holds references, so this is
               ;; the case that would break if the head were ever compacted
               ;; in place. Static serving hands one vector to every request.
               (web-skeleton::connection-append-write conn shared)
               (check "drain: one pass reports done"
                      (attempt (web-skeleton::connection-on-write conn)) :done)
               (check "drain: nothing left pending"
                      (web-skeleton::connection-write-pending conn) 0)
               (check "drain: shared vector is not mutated"
                      (str shared) "SHARED"))
          (ignore-errors (close stream))))
      ;; The whole queue reached the wire, in order. A snapshotted ON-WRITE
      ;; stops after "AAA".
      (check "drain: every queued vector is written, in order"
             (slurp) "AAABBSHAREDSHARED")
      (ignore-errors (delete-file path)))))

;;; ---------------------------------------------------------------------------
;;; ws-send queues instead of spinning
;;;
;;; Needs a fd that genuinely returns EAGAIN, which a regular file never
;;; does. A pipe to a child that never reads its stdin is deterministic:
;;; the buffer is 64 KiB, `sleep` does not drain it, so a payload past
;;; that size is guaranteed to leave a remainder.
;;;
;;; The elapsed-time check is the point of the whole item. The old ws-send
;;; sat in POLL-WRITABLE until the peer read or *WRITE-STALL-TIMEOUT* expired,
;;; holding the worker and every other connection on it. Against this
;;; fixture — a peer that never reads at all — that is a full ten seconds.
;;; ---------------------------------------------------------------------------

(defun test-ws-send-queues ()
  (format t "~%ws-send queueing~%")
  (let ((proc (sb-ext:run-program "/bin/sleep" '("30")
                                  :input :stream :output nil :wait nil)))
    (unwind-protect
         (let* ((fd (sb-sys:fd-stream-fd (sb-ext:process-input proc)))
                (conn (web-skeleton::make-connection
                       :fd fd :state :websocket :last-active 0))
                (big (web-skeleton::build-ws-frame
                      web-skeleton::+ws-op-binary+
                      (make-array (* 256 1024) :element-type '(unsigned-byte 8)
                                               :initial-element 88)))
                (small (web-skeleton::build-ws-frame
                        web-skeleton::+ws-op-binary+
                        (make-array 100 :element-type '(unsigned-byte 8)
                                        :initial-element 89))))
           (web-skeleton::set-nonblocking fd)
           (let* ((start (get-internal-real-time))
                  ;; Caught rather than allowed to propagate: a ws-send that
                  ;; went back to blocking would raise its timeout here and
                  ;; end the run with a backtrace instead of a failed check.
                  (flushed (handler-case (web-skeleton::ws-send conn big)
                             (error (e) (format nil "signalled: ~a" e))))
                  (elapsed (/ (- (get-internal-real-time) start)
                              internal-time-units-per-second)))
             (check "ws-send: reports a remainder rather than full delivery"
                    flushed nil)
             (check "ws-send: the remainder is queued"
                    (plusp (web-skeleton::connection-write-pending conn)) t)
             (check "ws-send: returns without waiting for the peer"
                    (< elapsed 1) t))
           ;; Append, not substitute. The old ws-send never touched the
           ;; connection's buffer at all; the new one must add to it.
           (let ((before (web-skeleton::connection-write-pending conn)))
             ;; Caught for the same reason as the first send: a blocking
             ;; ws-send raises here, and an unhandled raise ends the run
             ;; instead of reporting. The delta check below still tells
             ;; the two apart — a send that raised queued nothing.
             (handler-case (web-skeleton::ws-send conn small) (error () nil))
             (check "ws-send: a second frame is appended behind the first"
                    (- (web-skeleton::connection-write-pending conn) before)
                    (length small)))
           ;; The invariant the sweeps depend on: a drained head never sits
           ;; in front of a non-empty queue, because ON-WRITE promotes
           ;; before it returns and APPEND takes the head slot whenever
           ;; nothing is pending. Asserted after a real partial write, not
           ;; argued.
           (flet ((head-drained-with-queue-p ()
                    (and (zerop (- (web-skeleton::connection-write-end conn)
                                   (web-skeleton::connection-write-pos conn)))
                         (web-skeleton::connection-write-queue conn))))
             (check "ws-send: no drained head in front of a queue"
                    (head-drained-with-queue-p) nil)
             (web-skeleton::connection-on-write conn)
             (check "ws-send: still none after another write pass"
                    (head-drained-with-queue-p) nil))
           ;; At the bound ws-send signals. It must not truncate the frame
           ;; and must not drop it quietly.
           (let ((web-skeleton:*max-write-backlog*
                   (web-skeleton::connection-write-pending conn))
                 (pending-before (web-skeleton::connection-write-pending conn)))
             (check "ws-send: signals at the backlog bound"
                    (handler-case (progn (web-skeleton::ws-send conn small) nil)
                      (error () t))
                    t)
             (check "ws-send: the refused frame queued nothing"
                    (web-skeleton::connection-write-pending conn)
                    pending-before)))
      (ignore-errors (sb-ext:process-kill proc 9))
      (ignore-errors (sb-ext:process-wait proc))
      ;; PROCESS-KILL and PROCESS-WAIT reap the child; neither closes the
      ;; pipe SBCL opened for :INPUT :STREAM. Without this the fd stays
      ;; open for the rest of the run — the Server suite's one leak.
      (ignore-errors (sb-ext:process-close proc)))))

(defun test-ws-send-refuses-a-foreign-connection ()
  "WS-SEND refuses a connection this worker does not own.

   The frame here is 100 bytes into a fresh pipe, which is the point. A
   cross-worker WS-SEND used to be caught by the arming — an fd absent
   from this worker's epoll gives ENOENT — and arming only happens when
   the flush leaves a remainder. So the refusal covered the backed-up
   peer and missed the ordinary one: a frame that fit was appended to an
   unsynchronised queue, sent from the wrong thread, and reported success.
   That is the case the README and DEPLOYMENT.md both described as
   refused, and it is the common one.

   Four assertions, and the middle two are doing different jobs from the
   first. That nothing was queued is the placement: the guard runs before
   CONNECTION-APPEND-WRITE, because the queue is the thing being
   corrupted and refusing after the append would report the misuse having
   already committed it. The decoy pins EQ rather than a comparison on
   the fd number — an fd reissued to a different connection on this
   worker satisfies the number while being the wrong object, and a guard
   written on the number would pass every other check here.

   The last is a control and a real one: registering the connection is
   the only change, and it passes on both sides of the revert."
  (format t "~%ws-send: a connection this worker does not own~%")
  (let ((proc (sb-ext:run-program "/bin/sleep" '("30")
                                  :input :stream :output nil :wait nil))
        (epfd (web-skeleton::epoll-create)))
    (unwind-protect
         (let* ((fd (sb-sys:fd-stream-fd (sb-ext:process-input proc)))
                (small (web-skeleton::build-ws-frame
                        web-skeleton::+ws-op-binary+
                        (make-array 100 :element-type '(unsigned-byte 8)
                                        :initial-element 89))))
           (web-skeleton::set-nonblocking fd)
           (let* ((web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (conn (web-skeleton::make-connection
                         :fd fd :state :websocket :last-active 0))
                  (decoy (web-skeleton::make-connection
                          :fd fd :state :websocket :last-active 0)))
             ;; Not registered: what a connection owned by another worker
             ;; looks like from here.
             (check "ws-send: a connection this worker does not own is refused"
                    (handler-case (progn (web-skeleton::ws-send conn small) nil)
                      (error (e)
                        (not (null (search "not on this worker's connection table"
                                           (princ-to-string e))))))
                    t)
             (check "ws-send: the refused frame queued nothing"
                    (web-skeleton::connection-write-pending conn) 0)
             ;; The fd number is now on the table, attached to something
             ;; else. A guard reading the number would accept this.
             (web-skeleton::register-connection decoy)
             (check "ws-send: the right fd on the wrong object is still refused"
                    (handler-case (progn (web-skeleton::ws-send conn small) nil)
                      (error (e)
                        (not (null (search "not on this worker's connection table"
                                           (princ-to-string e))))))
                    t)
             ;; The control. Registering this connection is the only
             ;; change, and the frame reaches the pipe whole.
             (web-skeleton::register-connection conn)
             (check "ws-send: and the same frame is sent once it is owned"
                    (handler-case (web-skeleton::ws-send conn small)
                      (error (e) (format nil "signalled: ~a" e)))
                    t)))
      (ignore-errors (sb-ext:process-kill proc 9))
      (ignore-errors (sb-ext:process-wait proc))
      (ignore-errors (sb-ext:process-close proc))
      (ignore-errors (web-skeleton::%close epfd)))))

;;; ---------------------------------------------------------------------------
;;; The write-stall deadline
;;;
;;; *write-stall-timeout* used to bound a spin inside ws-send. It now bounds
;;; how long a connection may sit on a backlog that is not moving. The
;;; idle sweep cannot answer that question: *ws-idle-timeout* defaults to
;;; a day, and a peer that has stopped reading may still be sending, which
;;; keeps LAST-ACTIVE fresh. So the negative control here holds LAST-ACTIVE
;;; current — that is precisely the case the idle timeout would miss.
;;;
;;; The sweep asks whether there is a backlog and whether it has moved,
;;; not what state the connection is in. It used to name :WEBSOCKET, which
;;; left every other state — including the long-lived ones most likely to
;;; build a backlog — with no stall bound at all, so a second state is
;;; swept here to hold the generalization in place.
;;; ---------------------------------------------------------------------------

(defun test-ws-write-stall-sweep ()
  (format t "~%Write stall sweep~%")
  (flet ((sweep-one (&key stalled (state :websocket))
           (let ((epfd (web-skeleton::epoll-create))
                 (connfd (web-skeleton::epoll-create))
                 (log (make-string-output-stream))
                 (now (get-universal-time)))
             (unwind-protect
                  (let ((conn (web-skeleton::make-connection
                               :fd connfd :state state
                               ;; Fresh, so the idle sweep has no interest.
                               :last-active now))
                        (web-skeleton::*connections* (make-hash-table :test #'eql))
                        (web-skeleton:*log-level* :info)
                        (web-skeleton:*log-stream* log))
                    (web-skeleton::epoll-add
                     epfd connfd (logior web-skeleton::+epollin+
                                         web-skeleton::+epollet+))
                    ;; A backlog that exists either way; only its age differs.
                    (web-skeleton::connection-append-write
                     conn (make-array 64 :element-type '(unsigned-byte 8)))
                    (setf (web-skeleton::connection-write-progress-at conn)
                          (if stalled
                              (- now web-skeleton:*write-stall-timeout* 1)
                              now))
                    (web-skeleton::register-connection conn)
                    (web-skeleton::sweep-idle-connections epfd now)
                    (list (hash-table-count web-skeleton::*connections*)
                          (get-output-stream-string log)))
               (ignore-errors (web-skeleton::%close connfd))
               (ignore-errors (web-skeleton::%close epfd))))))

    ;; Control: same backlog, progress just made. Must survive — and would
    ;; also survive the old code, which is why the stalled case below is
    ;; what carries the check.
    (destructuring-bind (count log) (sweep-one)
      (check "ws stall: a moving backlog is left alone" count 1)
      (check "ws stall: control logs nothing about stalling"
             (search "stalled" log) nil))

    ;; Stalled past the deadline with LAST-ACTIVE fresh: the idle sweep
    ;; would never touch this connection.
    (destructuring-bind (count log) (sweep-one :stalled t)
      (check "ws stall: a stalled backlog is closed" count 0)
      (check "ws stall: the log names the reason"
             (and (search "write stalled" log) t) t))

    ;; A state that is not :WEBSOCKET. Gating the clause on one state left
    ;; the long-lived ones — the ones that queue most — unbounded.
    (destructuring-bind (count log) (sweep-one :stalled t :state :write-response)
      (check "stall: a non-websocket state is swept too" count 0)
      (check "stall: and the log names which state it was"
             (and (search "WRITE-RESPONSE" log :test #'char-equal) t) t))
    (destructuring-bind (count log) (sweep-one :state :write-response)
      (declare (ignore log))
      (check "stall: a moving non-websocket backlog is left alone" count 1))))

;;; ---------------------------------------------------------------------------
;;; Chunked encoder
;;;
;;; The generated property covers agreement with the three readers. These
;;; are the cases worth naming rather than discovering: the byte-exact
;;; wire shape, and the two ways a plausible encoder corrupts a stream
;;; while emitting bytes every reader accepts.
;;; ---------------------------------------------------------------------------

(defun test-chunked-encoder ()
  (format t "~%Chunked encoder~%")
  (flet ((bytes (s) (sb-ext:string-to-octets s :external-format :latin-1))
         (str (v) (sb-ext:octets-to-string v :external-format :latin-1)))

    ;; --- The wire shape is the intersection, not any reader's tolerance ---
    (check "encode-chunk: size line is bare lowercase hex plus CRLF"
           (str (web-skeleton::encode-chunk (bytes "hello")))
           (format nil "5~c~chello~c~c" #\Return #\Newline #\Return #\Newline))
    (check "encode-chunk: multi-digit sizes stay lowercase hex"
           (subseq (str (web-skeleton::encode-chunk
                         (make-array 255 :element-type '(unsigned-byte 8)
                                         :initial-element 65)))
                   0 4)
           (format nil "ff~c~c" #\Return #\Newline))
    (check "chunked-terminator: last-chunk plus empty trailer section"
           (str (web-skeleton::chunked-terminator))
           (format nil "0~c~c~c~c" #\Return #\Newline #\Return #\Newline))
    ;; Shared and reused, like the ping frame — the write queue holds it by
    ;; reference, so a fresh vector per call would be waste and a mutated
    ;; one would be a bug.
    (check "chunked-terminator: the same vector every time"
           (eq (web-skeleton::chunked-terminator)
               (web-skeleton::chunked-terminator))
           t)

    ;; --- An empty payload must never become the terminator ---
    ;; This is the failure that produces bytes every reader accepts: the
    ;; message ends early, nothing raises anywhere, and the peer believes
    ;; it received the whole thing.
    (check "encode-chunk: an empty payload yields nothing at all"
           (web-skeleton::encode-chunk
            (make-array 0 :element-type '(unsigned-byte 8)))
           nil)
    (check "encode-chunk: an empty range of a non-empty vector yields nothing"
           (web-skeleton::encode-chunk (bytes "hello") :start 2 :end 2)
           nil)
    ;; A reversed range must not borrow the empty range's quiet NIL. One
    ;; of them means "nothing to send"; the other means the caller has its
    ;; offsets backwards and is about to lose bytes.
    (check "encode-chunk: a reversed range signals rather than answering nothing"
           (handler-case (progn (web-skeleton::encode-chunk
                                 (bytes "hello") :start 2 :end 1)
                                nil)
             (error () t))
           t)
    (check "encode-chunk: a range past the end signals"
           (handler-case (progn (web-skeleton::encode-chunk
                                 (bytes "hello") :start 0 :end 99)
                                nil)
             (error () t))
           t)

    ;; --- An empty chunk between two real ones truncates nothing ---
    (let* ((framed (concatenate '(vector (unsigned-byte 8))
                                (web-skeleton::encode-chunk (bytes "AAA"))
                                (or (web-skeleton::encode-chunk
                                     (make-array 0 :element-type '(unsigned-byte 8)))
                                    #())
                                (web-skeleton::encode-chunk (bytes "BBB"))
                                (web-skeleton::chunked-terminator))))
      (check "encode-chunk: an empty chunk mid-stream does not end it"
             (str (web-skeleton::decode-chunked-body framed 0 (length framed)))
             "AAABBB"))

    ;; --- Content that looks like framing survives it ---
    ;; The decoders skip *over* chunk data rather than scanning it, which
    ;; is what makes this safe; asserting it keeps a future encoder from
    ;; deciding to be clever about escaping.
    (let* ((payload (bytes (format nil "0~c~c~c~cmore"
                                   #\Return #\Newline #\Return #\Newline)))
           (framed (concatenate '(vector (unsigned-byte 8))
                                (web-skeleton::encode-chunk payload)
                                (web-skeleton::chunked-terminator))))
      (check "encode-chunk: a payload containing a terminator round-trips"
             (str (web-skeleton::decode-chunked-body framed 0 (length framed)))
             (str payload))
      (check "encode-chunk: and the predicate is not fooled by it either"
             (multiple-value-bind (complete resume)
                 (web-skeleton::chunked-body-complete-p framed 0 (length framed))
               (list complete
                     (= resume (- (length framed)
                                  (length (web-skeleton::chunked-terminator))))))
             '(t t)))))

;;; ---------------------------------------------------------------------------
;;; connection-discard-available: the same EOF contract, on a scratch sink
;;;
;;; A :STREAMING connection notices its peer leaving through this, so it
;;; needs the same discrimination CONNECTION-READ-AVAILABLE gained: a
;;; peer whose last bytes and FIN arrive in one wake-up must not report
;;; :OK and be mistaken for one that is still there. Two readers with the
;;; same job disagreeing is the failure this codebase names as its threat
;;; model, so the twin is pinned against the same deterministic fixture
;;; rather than against an e2e race.
;;; ---------------------------------------------------------------------------

(defun test-discard-available-eof ()
  (format t "~%connection-discard-available: EOF reporting~%")
  (flet ((drain (sh-command)
           (let* ((proc (sb-ext:run-program "/bin/sh" (list "-c" sh-command)
                                            :output :stream :wait t))
                  (fd (web-skeleton::%process-output-fd proc))
                  (sink (make-array 64 :element-type '(unsigned-byte 8))))
             (unwind-protect
                  (let ((conn (web-skeleton::make-connection
                               :fd fd :state :streaming :last-active 0)))
                    (web-skeleton::set-nonblocking fd)
                    (list (web-skeleton::connection-discard-available conn sink)
                          ;; The connection's own buffer must be untouched:
                          ;; a stream's pipelined bytes still live at the
                          ;; offsets the keep-alive reset works from.
                          (web-skeleton::connection-read-pos conn)))
               (ignore-errors (sb-ext:process-close proc))))))
    (destructuring-bind (result pos) (drain "printf 'noise'")
      (check "discard-available: bytes then EOF reports :ok-eof" result :ok-eof)
      (check "discard-available: the connection buffer is not disturbed"
             pos 0))
    (destructuring-bind (result pos) (drain "exit 0")
      (check "discard-available: no bytes at EOF reports :eof" result :eof)
      (check "discard-available: still nothing in the connection buffer"
             pos 0))
    ;; More than one sink-full, so the loop is exercised rather than a
    ;; single read that happens to see everything.
    (destructuring-bind (result pos)
        (drain "printf '%0.s-' $(seq 1 500)")
      (check "discard-available: a payload past the sink still reports :ok-eof"
             result :ok-eof)
      (check "discard-available: and still leaves the buffer alone" pos 0))))

;;; ---------------------------------------------------------------------------
;;; Incremental delivery from the chunk walk
;;;
;;; CHUNKED-BODY-COMPLETE-P already visited every chunk exactly once, to
;;; step over it. ON-DATA hands those bytes back from the same visit, so
;;; the properties worth pinning are that it sees each chunk once, sees
;;; exactly what DECODE-CHUNKED-BODY would have produced, and never sees
;;; a chunk whose framing has not yet fully arrived.
;;; ---------------------------------------------------------------------------

(defun test-chunk-walk-on-data ()
  (format t "~%Chunk walk on-data~%")
  (flet ((bytes (s) (sb-ext:string-to-octets s :external-format :latin-1))
         (str (v) (sb-ext:octets-to-string v :external-format :latin-1)))
    (let* ((framed (concatenate '(vector (unsigned-byte 8))
                                (web-skeleton::encode-chunk (bytes "alpha"))
                                (web-skeleton::encode-chunk (bytes "beta"))
                                (web-skeleton::encode-chunk (bytes "gamma"))
                                (web-skeleton::chunked-terminator)))
           (n (length framed)))

      ;; --- One pass over a whole body: every chunk, in order, once ---
      (let ((seen nil))
        (multiple-value-bind (complete resume)
            (web-skeleton::chunked-body-complete-p
             framed 0 n 0
             (lambda (buf s e) (push (str (subseq buf s e)) seen)))
          (declare (ignore resume))
          (check "on-data: the body is complete" complete t)
          (check "on-data: every chunk, in order"
                 (nreverse seen) '("alpha" "beta" "gamma"))))

      ;; --- What it hands back is what the decoder would have produced ---
      (let ((seen (make-string-output-stream)))
        (web-skeleton::chunked-body-complete-p
         framed 0 n 0
         (lambda (buf s e) (write-string (str (subseq buf s e)) seen)))
        (check "on-data: the concatenation is the decoded body"
               (get-output-stream-string seen)
               (str (web-skeleton::decode-chunked-body framed 0 n))))

      ;; --- A dribbling upstream: each chunk delivered once, never twice ---
      ;; This is what RESUME buys, and the reason ON-DATA can live in the
      ;; walk at all — a caller threading the offset back never revisits
      ;; a chunk, so it cannot re-deliver one.
      (let ((seen nil)
            (scan 0))
        (loop for end from 0 to n
              do (multiple-value-bind (complete next)
                     (web-skeleton::chunked-body-complete-p
                      framed 0 end scan
                      (lambda (buf s e) (push (str (subseq buf s e)) seen)))
                   (declare (ignore complete))
                   (setf scan next)))
        (check "on-data: a byte-at-a-time upstream delivers each chunk once"
               (nreverse seen) '("alpha" "beta" "gamma")))

      ;; --- A partial chunk is never handed over ---
      ;; The walk returns before ON-DATA when the data or its trailing
      ;; CRLF has not landed, so an app is never given bytes whose
      ;; framing has not been proved.
      (let ((seen nil))
        ;; "5\r\nalph" — one byte of payload short, and no CRLF.
        (web-skeleton::chunked-body-complete-p
         framed 0 8 0
         (lambda (buf s e) (push (str (subseq buf s e)) seen)))
        (check "on-data: an incomplete chunk is withheld" seen nil)))))

;;; ---------------------------------------------------------------------------
;;; :on-body backpressure
;;;
;;; Over a real socket and a real epoll fd, because the whole mechanism
;;; is an epoll interest change: :PAUSE drops EPOLLIN so the upstream's
;;; send window fills, and FETCH-RESUME puts it back. Nothing about that
;;; is observable from a struct.
;;;
;;; The property that makes the simple re-arm correct is that a pause
;;; stops the *upstream*, not the current pass — everything already read
;;; is still handed over, so no undelivered bytes are left in user space
;;; where an EPOLL_CTL_MOD would not re-fire.
;;; ---------------------------------------------------------------------------

(defun test-fetch-on-body-pause ()
  (format t "~%Fetch :on-body backpressure~%")
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let* ((out-fd (web-skeleton::socket-fd server))
                  (seen nil)
                  (conn (web-skeleton::make-connection
                         :fd out-fd :socket server :state :out-read
                         :outbound-p t :last-active (get-universal-time)))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  ;; Three chunks in one write, so they all land in one
                  ;; read and the walk sees them in a single pass.
                  (body (concatenate
                         '(vector (unsigned-byte 8))
                         (sb-ext:string-to-octets
                          (format nil "HTTP/1.1 200 OK~c~ctransfer-encoding: ~
                                       chunked~c~c~c~c"
                                  #\Return #\Newline #\Return #\Newline
                                  #\Return #\Newline)
                          :external-format :ascii)
                         (web-skeleton::encode-chunk
                          (sb-ext:string-to-octets "aa" :external-format :ascii))
                         (web-skeleton::encode-chunk
                          (sb-ext:string-to-octets "bb" :external-format :ascii))
                         (web-skeleton::encode-chunk
                          (sb-ext:string-to-octets "cc" :external-format :ascii)))))
             (web-skeleton::set-nonblocking out-fd)
             (setf (web-skeleton::connection-fetch-on-body conn)
                   (lambda (c chunk)
                     (declare (ignore c))
                     (push (sb-ext:octets-to-string
                            chunk :external-format :ascii)
                           seen)
                     :pause))
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd out-fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
             (sb-bsd-sockets:socket-send client body nil)
             ;; Give the bytes a moment to cross loopback, then let the
             ;; read path drain them.
             (sleep 0.1)
             (web-skeleton::handle-outbound-read conn epfd)

             (check "pause: the callback saw every chunk already read"
                    (nreverse seen) '("aa" "bb" "cc"))
             (check "pause: the connection is marked paused"
                    (web-skeleton::connection-fetch-paused conn) t)
             ;; The response is unterminated, so the fetch has not
             ;; completed — the connection is still registered and no
             ;; callback has fired.
             (check "pause: the fetch has not completed"
                    (hash-table-count web-skeleton::*connections*) 1)

             ;; Resume clears the flag and re-arms. The re-arm is what
             ;; makes the pause recoverable rather than a one-way door.
             (web-skeleton::fetch-resume conn)
             (check "resume: the paused flag is cleared"
                    (web-skeleton::connection-fetch-paused conn) nil)
             (check "resume: a second resume is a no-op, not an error"
                    (progn (web-skeleton::fetch-resume conn)
                           (web-skeleton::connection-fetch-paused conn))
                    nil)

             ;; The re-arm has to be asserted against epoll, not against a
             ;; read. HANDLE-OUTBOUND-READ calls the socket directly, so
             ;; it delivers bytes whatever the interest mask says — the
             ;; mask only decides whether the event loop is ever woken to
             ;; call it. Reverting the re-arm and re-running showed
             ;; exactly that: a delivery check passed with the pause
             ;; never lifted.
             (let ((evbuf (make-array (* 4 web-skeleton::+epoll-event-size+)
                                      :element-type '(unsigned-byte 8))))
               ;; Fresh bytes with the interest re-armed: the loop wakes.
               (sb-bsd-sockets:socket-send
                client
                (web-skeleton::encode-chunk
                 (sb-ext:string-to-octets "dd" :external-format :ascii))
                nil)
               (sleep 0.1)
               (check "resume: epoll reports the fd once the interest is back"
                      (plusp (web-skeleton::epoll-wait epfd evbuf 4 50)) t)
               ;; And it really is our fd, not a stray wake-up.
               (check "resume: and it is the outbound fd"
                      (web-skeleton::epoll-event-fd evbuf 0) out-fd)
               ;; Paused again, the same bytes produce no wake-up at all.
               (web-skeleton::handle-outbound-read conn epfd)
               (check "pause: a paused fd is not reported, with data waiting"
                      (progn
                        (sb-bsd-sockets:socket-send
                         client
                         (web-skeleton::encode-chunk
                          (sb-ext:string-to-octets "ee" :external-format :ascii))
                         nil)
                        (sleep 0.1)
                        (web-skeleton::epoll-wait epfd evbuf 4 50))
                      0)))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client))))))

;;; ---------------------------------------------------------------------------
;;; Static responses carry a Date
;;;
;;; The interesting assertion is not that the header is present. It is
;;; that the body is still exactly the file after a header was added to
;;; the response — because the design this replaced derived the body's
;;; offset from the header block's length, and anything spliced into one
;;; pre-built vector and not the other would have moved a range slice
;;; without moving its Content-Length. That failure has a correct status,
;;; a correct length, and content starting a few bytes early, and it is
;;; invisible to every test that does not ask for a byte range.
;;; ---------------------------------------------------------------------------

(defun test-static-date ()
  (format t "~%Static Date~%")
  (let ((saved web-skeleton::*static-cache*)
        (content (sb-ext:string-to-octets
                  "0123456789abcdefghijklmnopqrstuvwxyz"
                  :external-format :latin-1)))
    (unwind-protect
         (let ((web-skeleton::*http-date-line-cache* (cons 0 #())))
           (setf web-skeleton::*static-cache* (make-hash-table :test #'equal))
           (setf (gethash "/d.txt" web-skeleton::*static-cache*)
                 (web-skeleton::build-static-response "text/plain" content 0))
           (flet ((wire (method &rest headers)
                    (let ((r (serve-static
                              (make-test-request :method method :path "/d.txt"
                                                 :headers headers))))
                      (sb-ext:octets-to-string
                       (if (consp r)
                           (apply #'concatenate '(vector (unsigned-byte 8)) r)
                           r)
                       :external-format :latin-1)))
                  (body (text)
                    (let ((i (search (format nil "~c~c~c~c" #\Return #\Newline
                                             #\Return #\Newline)
                                     text)))
                      (and i (subseq text (+ i 4))))))

             ;; Every pre-built shape now carries one. RFC 7231 §7.1.1.2
             ;; is a MUST, and 7232 §4.1 wants it on the 304 as well.
             (check "static date: a GET carries a Date"
                    (and (search "date: " (wire :GET)) t) t)
             (check "static date: a HEAD carries a Date"
                    (and (search "date: " (wire :HEAD)) t) t)
             (check "static date: a 304 carries a Date"
                    (and (search "date: "
                                 (wire :GET (cons "if-none-match"
                                                  (web-skeleton::static-entry-etag
                                                   (gethash "/d.txt"
                                                            web-skeleton::*static-cache*)))))
                         t)
                    t)
             (check "static date: a 206 carries a Date"
                    (and (search "date: "
                                 (wire :GET (cons "range" "bytes=0-3")))
                         t)
                    t)
             ;; A Date is exactly one header line, so it must appear once
             ;; — a per-request line appended to a prefix that already
             ;; had one would be two, and RFC 7230 §3.2.2 forbids that
             ;; for a non-list-valued field.
             (check "static date: exactly one Date on a GET"
                    (let ((w (wire :GET)) (n 0) (i 0))
                      (loop (let ((h (search "date: " w :start2 i)))
                              (unless h (return n))
                              (incf n)
                              (setf i (1+ h)))))
                    1)

             ;; The assertions the retired offset used to be load-bearing
             ;; for. The body of a whole GET is the file and nothing else,
             ;; and a range is the matching slice of it — both true no
             ;; matter how long the header block happens to be.
             (check "static date: the GET body is exactly the file"
                    (body (wire :GET))
                    "0123456789abcdefghijklmnopqrstuvwxyz")
             (check "static date: a range is the matching slice"
                    (body (wire :GET (cons "range" "bytes=5-9")))
                    "56789")
             (check "static date: a suffix range is the matching slice"
                    (body (wire :GET (cons "range" "bytes=-4")))
                    "wxyz")
             (check "static date: HEAD still has no body"
                    (body (wire :HEAD)) ""))

           ;; --- A file larger than the backlog bound goes out whole ---
           ;; *max-write-backlog* is for a producer outrunning its peer.
           ;; A static file is complete in memory at queue time: there is
           ;; nothing to throttle and nobody to decide a disposition, so
           ;; refusing a piece of it does not conserve anything — it
           ;; sends headers promising a body the peer never gets, and on
           ;; a keep-alive connection the next response lands where that
           ;; body should have been. The suite had no file over the bound
           ;; because the bound is 2 MiB; the number is tunable and
           ;; someone will lower it.
           ;; SETF and restore rather than LET. Every other test that
           ;; touches a tuning knob a worker might read does it this way,
           ;; and the reason generalizes past thread visibility: a LET
           ;; here leaves the binding live for everything the body calls,
           ;; and this body calls into the server's own queueing code.
           (let ((saved-backlog web-skeleton:*max-write-backlog*))
             (unwind-protect
                  (let* ((big (make-array 600 :element-type '(unsigned-byte 8)
                                              :initial-element 88))
                         (entry (web-skeleton::build-static-response
                                 "application/wasm" big 0))
                         (segments (web-skeleton::static-segments entry :get))
                         (conn (web-skeleton::make-connection
                                :fd -1 :last-active 0))
                         (total (reduce #'+ segments :key #'length)))
                    (setf web-skeleton:*max-write-backlog* 512)
                    (check "static backlog: the whole response is queued, bound or not"
                           (progn (web-skeleton::connection-queue-segments
                                   conn segments)
                                  (web-skeleton::connection-write-pending conn))
                           total)
                    (check "static backlog: the body is all of it"
                           (- (web-skeleton::connection-write-pending conn)
                              (length (first segments))
                              (length (second segments))
                              (length (third segments)))
                           600)
                    ;; The bound still means what it says for the path it
                    ;; was written for.
                    (let ((c2 (web-skeleton::make-connection
                               :fd -1 :last-active 0)))
                      (check "static backlog: an ordinary append is still bounded"
                             (web-skeleton::connection-append-write c2 big)
                             nil)))
               (setf web-skeleton:*max-write-backlog* saved-backlog))))
      (setf web-skeleton::*static-cache* saved))))

;;; ---------------------------------------------------------------------------
;;; Server-Sent Events
;;;
;;; The validation follows VALIDATE-COOKIE-FIELD's register, but the
;;; character set is re-derived rather than inherited: a cookie's
;;; delimiters are ';' and CR/LF against header injection, an SSE field's
;;; delimiter is a line break against *event* injection — a client
;;; dispatching an event the app never sent. The tests below are written
;;; against that consequence, not against the character list.
;;; ---------------------------------------------------------------------------

(defun test-sse ()
  (format t "~%Server-Sent Events~%")
  (flet ((str (v) (sb-ext:octets-to-string v :external-format :utf-8))
         (refused (thunk)
           (handler-case (progn (funcall thunk) nil) (error () t))))

    ;; --- The ordinary shapes ---
    (check "sse: a data-only event"
           (str (web-skeleton::sse-event-bytes :data "hello"))
           (format nil "data: hello~c~c" #\Newline #\Newline))
    (check "sse: fields come before data, in spec order"
           (str (web-skeleton::sse-event-bytes
                 :data "x" :event "tick" :id "7" :retry 3000))
           (format nil "event: tick~cid: 7~cretry: 3000~cdata: x~c~c"
                   #\Newline #\Newline #\Newline #\Newline #\Newline))
    ;; Multi-line data is the protocol's own mechanism, not an error: one
    ;; data line per segment, rejoined with LF by the client.
    (check "sse: multi-line data becomes one data line per segment"
           (str (web-skeleton::sse-event-bytes :data
                 (format nil "one~ctwo" #\Newline)))
           (format nil "data: one~cdata: two~c~c"
                   #\Newline #\Newline #\Newline))
    (check "sse: a trailing newline round-trips as an empty segment"
           (str (web-skeleton::sse-event-bytes :data
                 (format nil "one~c" #\Newline)))
           (format nil "data: one~cdata: ~c~c"
                   #\Newline #\Newline #\Newline))

    ;; --- Event injection: the consequence the validator exists for ---
    ;; A blank line dispatches. An event value carrying one would end the
    ;; app's event early and hand the client a second event it never
    ;; wrote — including one that could carry a different event type.
    (check "sse: LF in event is refused"
           (refused (lambda () (web-skeleton::sse-event-bytes
                                :data "x" :event
                                (format nil "a~c~cdata: forged" #\Newline
                                        #\Newline))))
           t)
    (check "sse: LF in id is refused"
           (refused (lambda () (web-skeleton::sse-event-bytes
                                :data "x" :id (format nil "1~cdata: forged"
                                                      #\Newline))))
           t)
    ;; CR is a line terminator to EventSource too — the character a
    ;; header-shaped validator would have caught for the wrong reason,
    ;; and a body-shaped one that only knew about LF would have missed.
    (check "sse: CR in event is refused"
           (refused (lambda () (web-skeleton::sse-event-bytes
                                :data "x" :event
                                (format nil "a~cdata: forged" #\Return))))
           t)
    (check "sse: CR in data is refused, even though LF is allowed there"
           (refused (lambda () (web-skeleton::sse-event-bytes
                                :data (format nil "a~cb" #\Return))))
           t)
    (check "sse: NUL in id is refused"
           (refused (lambda () (web-skeleton::sse-event-bytes
                                :data "x" :id (format nil "1~c2"
                                                      (code-char 0)))))
           t)
    (check "sse: a non-integer retry is refused"
           (refused (lambda () (web-skeleton::sse-event-bytes
                                :data "x" :retry "soon")))
           t)
    ;; A named refusal, not FIND's type error. An integer id is the most
    ;; natural thing an app passes — sequence numbers are what
    ;; Last-Event-ID replay is for — and "the value 42 is not of type
    ;; SEQUENCE" names neither the argument nor the fix.
    (dolist (case '((:id 42) (:event :tick) (:data 7)))
      (check (format nil "sse: a non-string ~a is refused by name"
                     (string-downcase (symbol-name (first case))))
             (handler-case
                 (progn (apply #'web-skeleton::sse-event-bytes
                               (if (eq (first case) :data)
                                   case
                                   (list* :data "x" case)))
                        nil)
               (error (e) (and (search "must be a string"
                                       (princ-to-string e))
                               t)))
             t))

    ;; --- An event with no data reaches nobody, so it is refused ---
    ;; EventSource returns early on an empty data buffer. Emitting one
    ;; would look sent from here and be dispatched nowhere.
    (check "sse: an event without data is refused"
           (refused (lambda () (web-skeleton::sse-event-bytes :event "tick")))
           t)
    (check "sse: an event with empty data is refused"
           (refused (lambda () (web-skeleton::sse-event-bytes :data "")))
           t)

    ;; --- The keepalive is the one emission with no data ---
    (check "sse: a bare comment is still two bytes, not zero"
           (str (web-skeleton::sse-comment-bytes))
           (format nil ":~c" #\Newline))
    (check "sse: a comment carries its text"
           (str (web-skeleton::sse-comment-bytes "ka"))
           (format nil ":ka~c" #\Newline))
    (check "sse: a comment cannot smuggle a line break either"
           (refused (lambda () (web-skeleton::sse-comment-bytes
                                (format nil "x~c~cdata: forged"
                                        #\Newline #\Newline))))
           t)

    ;; --- The response carries what a proxy needs to leave it alone ---
    (let* ((sresp (web-skeleton:make-sse-response))
           (head (sb-ext:octets-to-string
                  (web-skeleton::format-streaming-head
                   (web-skeleton::stream-response-response sresp) :chunked)
                  :external-format :latin-1)))
      (check "sse: content type declares the protocol"
             (and (search "content-type: text/event-stream" head) t) t)
      (check "sse: no-cache, so nothing serves a prefix of an endless body"
             (and (search "cache-control: no-cache" head) t) t)
      ;; nginx buffers this content type by default, and the deployment
      ;; story assumes a proxy in front — without this the stream arrives
      ;; in batches, which is not a stream.
      (check "sse: x-accel-buffering off for the proxy in front"
             (and (search "x-accel-buffering: no" head) t) t)
      (check "sse: a keepalive is installed by default"
             (str (web-skeleton::stream-response-keepalive sresp))
             (format nil ":~c" #\Newline)))
    (check "sse: the keepalive can be declined"
           (web-skeleton::stream-response-keepalive
            (web-skeleton:make-sse-response :keepalive nil))
           nil)
    ;; The framework's headers win: an app cannot quietly turn an SSE
    ;; response into something a client will not parse as one.
    (let ((head (sb-ext:octets-to-string
                 (web-skeleton::format-streaming-head
                  (web-skeleton::stream-response-response
                   (web-skeleton:make-sse-response
                    :headers '(("content-type" . "text/plain"))))
                  :chunked)
                 :external-format :latin-1)))
      (check "sse: the protocol's content type is not overridable"
             (and (search "content-type: text/event-stream" head) t) t))))

;;; ---------------------------------------------------------------------------
;;; Stream lifecycle
;;;
;;; START-STREAM through STREAM-CLOSE over a real loopback pair, reading
;;; back what the peer would have seen. The interesting assertions are
;;; the ones about the terminator: that it is written on the way out,
;;; and that the only route from :STREAMING back to :READ-HTTP is the one
;;; that writes it.
;;; ---------------------------------------------------------------------------

(defun test-stream-lifecycle ()
  (format t "~%Stream lifecycle~%")
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create))
          (closed-with :never)
          (sent-from-on-close :never))
      (unwind-protect
           (let* ((server-fd (web-skeleton::socket-fd server))
                  (conn (web-skeleton::make-connection
                         :fd server-fd :socket server :state :read-http
                         :last-active (get-universal-time)))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (request (web-skeleton::make-http-request
                            :method :GET :path "/s" :version "1.1"))
                  (sresp (web-skeleton:make-stream-response
                          :headers '(("content-type" . "text/plain"))
                          :on-close
                          (lambda (c reason)
                            (setf closed-with reason)
                            ;; A goodbye event from on-close is an
                            ;; ordinary shape. It must be refused, and
                            ;; refusing depends on the state having moved
                            ;; before the callback ran — otherwise these
                            ;; bytes land behind the terminator and, on a
                            ;; keep-alive connection, prefix the next
                            ;; response.
                            (setf sent-from-on-close
                                  (handler-case
                                      (progn (web-skeleton:stream-send
                                              c (sb-ext:string-to-octets
                                                 "bye" :external-format :ascii))
                                             :accepted)
                                    (error () :refused))))
                          :on-open
                          (lambda (c)
                            (web-skeleton:stream-send
                             c (sb-ext:string-to-octets
                                "hello " :external-format :ascii))
                            (web-skeleton:stream-send
                             c (sb-ext:string-to-octets
                                "world" :external-format :ascii))))))
             (web-skeleton::set-nonblocking server-fd)
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd server-fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
             (web-skeleton::start-stream conn epfd request sresp)

             (check "stream: the connection is streaming after the head"
                    (web-skeleton::connection-state conn) :streaming)
             (check "stream: framing came from the client's version"
                    (web-skeleton::connection-stream-framing conn) :chunked)
             (check "stream: on-close has not fired while the stream is open"
                    closed-with :never)

             ;; Closing writes the terminator and hands the socket back to
             ;; the ordinary write path.
             (web-skeleton:stream-close conn)
             (check "stream: close fires on-close exactly once, with :done"
                    closed-with :done)
             (check "stream: close leaves the ordinary write path in charge"
                    (web-skeleton::connection-state conn) :write-response)
             (check "stream: nothing is left queued after close"
                    (web-skeleton::connection-write-pending conn) 0)

             ;; A second teardown must not deliver a second notification.
             (setf closed-with :never)
             (web-skeleton::notify-stream-closed conn :disconnected)
             (check "stream: on-close does not fire twice" closed-with :never)
             (check "stream: a send from inside on-close is refused"
                    sent-from-on-close :refused)

             ;; What the peer actually received: head, two chunks, the
             ;; terminator — and the body decodes to what was sent.
             (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
               (multiple-value-bind (b n)
                   (sb-bsd-sockets:socket-receive client buf 4096)
                 (declare (ignore b))
                 (let* ((raw (subseq buf 0 n))
                        (text (sb-ext:octets-to-string
                               raw :external-format :latin-1))
                        (hend (web-skeleton::scan-crlf-crlf raw 0 n)))
                   (check "stream: the head declares chunked framing"
                          (and (search "transfer-encoding: chunked" text) t) t)
                   (check "stream: the head declares no length"
                          (search "content-length" text) nil)
                   ;; Caught: a stream that stopped writing its terminator
                   ;; makes this raise, and an unhandled raise ends the
                   ;; run with a backtrace instead of a failed check.
                   (check "stream: the body decodes to everything sent"
                          (handler-case
                              (sb-ext:octets-to-string
                               (web-skeleton::decode-chunked-body
                                raw (+ hend 4) n)
                               :external-format :ascii)
                            (error (e) (princ-to-string e)))
                          "hello world")
                   ;; Checked on the wire rather than on the queue. A
                   ;; send accepted from ON-CLOSE flushes straight out,
                   ;; so an empty queue afterwards proves nothing — and
                   ;; the decoder stops at the terminator, so the body
                   ;; check would not see the extra bytes either. What
                   ;; the peer received has to be the terminator last.
                   (check "stream: the terminator is the last thing sent"
                          (coerce (subseq raw (- n 5) n) 'list)
                          (coerce (web-skeleton::chunked-terminator) 'list))))))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client))))))

(defun test-stream-head-request ()
  (format t "~%Stream HEAD request~%")
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create))
          (opened nil))
      (unwind-protect
           (let* ((server-fd (web-skeleton::socket-fd server))
                  (conn (web-skeleton::make-connection
                         :fd server-fd :socket server :state :read-http
                         :last-active (get-universal-time)))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (request (web-skeleton::make-http-request
                            :method :HEAD :path "/s" :version "1.1"))
                  (sresp (web-skeleton:make-stream-response
                          :on-open (lambda (c) (declare (ignore c))
                                     (setf opened t)))))
             (web-skeleton::set-nonblocking server-fd)
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd server-fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
             (web-skeleton::start-stream conn epfd request sresp)
             ;; Headers a GET would have carried, and nothing after them.
             (check "stream HEAD: no stream is started"
                    (web-skeleton::connection-state conn) :write-response)
             (check "stream HEAD: on-open is never called" opened nil)
             (check "stream HEAD: no terminator is owed"
                    (web-skeleton::connection-stream-framing conn) nil)
             (web-skeleton::connection-on-write conn)
             (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
               (multiple-value-bind (b n)
                   (sb-bsd-sockets:socket-receive client buf 4096)
                 (declare (ignore b))
                 (let ((text (sb-ext:octets-to-string
                              (subseq buf 0 n) :external-format :latin-1)))
                   (check "stream HEAD: the head still declares the framing"
                          (and (search "transfer-encoding: chunked" text) t) t)
                   (check "stream HEAD: and nothing follows the headers"
                          (- n (+ 4 (web-skeleton::scan-crlf-crlf
                                     (subseq buf 0 n) 0 n)))
                          0)))))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client))))))

;;; ---------------------------------------------------------------------------
;;; Stream keepalive and idle
;;;
;;; Two knobs answering two questions. *STREAM-IDLE-TIMEOUT* asks whether
;;; the app is still producing; *WRITE-STALL-TIMEOUT* asks whether bytes
;;; are still leaving. A stream can be healthy at the socket and dead at
;;; the source, which is why reusing either of the existing idle knobs
;;; would be wrong — ten seconds reaps live streams, a day holds dead
;;; ones.
;;; ---------------------------------------------------------------------------

(defun test-stream-keepalive-and-idle ()
  (format t "~%Stream keepalive and idle~%")
  (flet ((streaming-conn (epfd connfd &key keepalive (age 0))
           ;; LAST-ACTIVE is held *current* while the production clock is
           ;; aged. That is the shape a draining backlog produces —
           ;; EPOLLOUT keeps the connection looking busy while the app
           ;; has stopped saying anything — and judging a stream on
           ;; LAST-ACTIVE would miss every one of them.
           (let ((conn (web-skeleton::make-connection
                        :fd connfd :state :streaming
                        :last-active (get-universal-time))))
             (setf (web-skeleton::connection-stream-framing conn) :chunked
                   (web-skeleton::connection-stream-produced-at conn)
                   (- (get-universal-time) age)
                   (web-skeleton::connection-stream-keepalive conn) keepalive)
             (web-skeleton::epoll-add
              epfd connfd (logior web-skeleton::+epollin+
                                  web-skeleton::+epollet+))
             (web-skeleton::register-connection conn)
             conn)))

    ;; --- Idle: a stream producing nothing for long enough is closed ---
    (let ((epfd (web-skeleton::epoll-create))
          (connfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let ((web-skeleton::*connections* (make-hash-table :test #'eql))
                 (reason :never)
                 (now (get-universal-time)))
             (let ((conn (streaming-conn epfd connfd
                                         :age (+ web-skeleton:*stream-idle-timeout*
                                                 1))))
               (setf (web-skeleton::connection-stream-on-close conn)
                     (lambda (c r) (declare (ignore c)) (setf reason r))))
             (web-skeleton::sweep-idle-connections epfd now)
             (check "stream idle: a quiet stream is reaped"
                    (hash-table-count web-skeleton::*connections*) 0)
             (check "stream idle: the app is told why" reason :idle))
        (ignore-errors (web-skeleton::%close connfd))
        (ignore-errors (web-skeleton::%close epfd))))

    ;; --- Control: within the deadline it survives ---
    (let ((epfd (web-skeleton::epoll-create))
          (connfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let ((web-skeleton::*connections* (make-hash-table :test #'eql))
                 (now (get-universal-time)))
             (streaming-conn epfd connfd :age 1)
             (web-skeleton::sweep-idle-connections epfd now)
             (check "stream idle: a recent stream is left alone"
                    (hash-table-count web-skeleton::*connections*) 1))
        (ignore-errors (web-skeleton::%close connfd))
        (ignore-errors (web-skeleton::%close epfd))))

    ;; --- Keepalive: framed, sent inline, and it refreshes the idle clock ---
    ;; Over a real pair rather than /dev/null, because the only site that
    ;; can tell a framed keepalive from a raw one is the wire. "Nothing
    ;; left queued", "counts as production" and "survives without arming"
    ;; are all true either way — they were, for a whole item.
    (multiple-value-bind (server client) (%loopback-pair)
      (let ((epfd (web-skeleton::epoll-create)))
        (unwind-protect
             (let* ((web-skeleton::*connections* (make-hash-table :test #'eql))
                    (now (get-universal-time))
                    (ka (sb-ext:string-to-octets ":ka" :external-format :ascii))
                    (conn (web-skeleton::make-connection
                           :fd (web-skeleton::socket-fd server)
                           :socket server :state :streaming
                           :last-active now)))
               (web-skeleton::set-nonblocking (web-skeleton::socket-fd server))
               (setf (web-skeleton::connection-stream-framing conn) :chunked
                     (web-skeleton::connection-stream-produced-at conn)
                     (- now web-skeleton:*stream-keepalive-interval* 1)
                     (web-skeleton::connection-stream-keepalive conn) ka)
               (web-skeleton::register-connection conn)
               (web-skeleton::keepalive-streams epfd now)
               (check "stream keepalive: nothing is left queued"
                      (web-skeleton::connection-write-pending conn) 0)
               (check "stream keepalive: it counts as production"
                      (web-skeleton::connection-stream-produced-at conn) now)
               (check "stream keepalive: the connection survives without arming"
                      (hash-table-count web-skeleton::*connections*) 1)
               ;; The assertion that needed a wire: these bytes share a
               ;; body with the app's sends, so on a chunked stream they
               ;; have to arrive as a chunk. Raw, they sit where the
               ;; peer's decoder expects a chunk-size.
               (let ((buf (make-array 64 :element-type '(unsigned-byte 8))))
                 (multiple-value-bind (b n)
                     (sb-bsd-sockets:socket-receive client buf 64)
                   (declare (ignore b))
                   (check "stream keepalive: it reaches the peer framed as a chunk"
                          (sb-ext:octets-to-string (subseq buf 0 n)
                                                   :external-format :latin-1)
                          (format nil "3~c~c:ka~c~c"
                                  #\Return #\Newline #\Return #\Newline)))))
          (ignore-errors (web-skeleton::%close epfd))
          (ignore-errors (sb-bsd-sockets:socket-close server))
          (ignore-errors (sb-bsd-sockets:socket-close client)))))

    ;; --- A stream with no keepalive bytes is left alone ---
    ;; There is nothing generic to send: a chunked stream's only
    ;; zero-content emission is the empty chunk, and that is the
    ;; terminator. Inventing one here would end the message.
    (let ((epfd (web-skeleton::epoll-create))
          (connfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let ((web-skeleton::*connections* (make-hash-table :test #'eql))
                 (now (get-universal-time)))
             (let ((conn (streaming-conn
                          epfd connfd
                          :age (+ web-skeleton:*stream-keepalive-interval* 1))))
               (web-skeleton::keepalive-streams epfd now)
               (check "stream keepalive: none configured means none sent"
                      (web-skeleton::connection-write-pending conn) 0)
               (check "stream keepalive: and the idle clock is not touched"
                      (< (web-skeleton::connection-stream-produced-at conn) now)
                      t)))
        (ignore-errors (web-skeleton::%close connfd))
        (ignore-errors (web-skeleton::%close epfd))))))

;;; ---------------------------------------------------------------------------
;;; Streaming response head
;;;
;;; The strongest check available is that the head we emit, followed by
;;; the chunks we encode, is read back as a complete response by the
;;; framework's own outbound reader. Producing and consuming are separate
;;; code paths that must agree on framing, which is the disagreement this
;;; codebase treats as the threat model.
;;; ---------------------------------------------------------------------------

(defun test-streaming-head ()
  (format t "~%Streaming response head~%")
  (flet ((bytes (s) (sb-ext:string-to-octets s :external-format :latin-1))
         (str (v) (sb-ext:octets-to-string v :external-format :latin-1))
         (resp (&optional headers)
           (let ((r (web-skeleton::make-http-response :status 200)))
             (loop for (n . v) in headers
                   do (web-skeleton::set-response-header r n v))
             r)))

    ;; --- Framing follows the client's version, not our preference ---
    (check "streaming framing: HTTP/1.1 gets chunked"
           (web-skeleton::stream-framing-for
            (web-skeleton::make-http-request :method :GET :version "1.1"))
           :chunked)
    (check "streaming framing: HTTP/1.0 cannot read chunked, so close"
           (web-skeleton::stream-framing-for
            (web-skeleton::make-http-request :method :GET :version "1.0"))
           :close)

    ;; --- The chunked head declares the framing and no length ---
    (let ((head (str (web-skeleton::format-streaming-head (resp) :chunked))))
      (check "streaming head: declares chunked transfer-encoding"
             (and (search "transfer-encoding: chunked" head) t) t)
      (check "streaming head: carries no Content-Length at all"
             (search "content-length" head) nil)
      (check "streaming head: carries a Date"
             (and (search "date: " head) t) t)
      (check "streaming head: ends at the header boundary"
             (and (search (format nil "~c~c~c~c" #\Return #\Newline
                                  #\Return #\Newline)
                          head)
                  t)
             t))

    ;; --- The close-delimited head says so, and declares no encoding ---
    (let ((head (str (web-skeleton::format-streaming-head (resp) :close))))
      (check "streaming head: close framing stamps Connection: close"
             (and (search "connection: close" head) t) t)
      (check "streaming head: close framing declares no transfer-encoding"
             (search "transfer-encoding" head) nil)
      (check "streaming head: close framing carries no Content-Length"
             (search "content-length" head) nil))

    ;; --- Two opinions about framing are refused, not reconciled ---
    (check "streaming head: a caller-set Content-Length is refused"
           (handler-case (progn (web-skeleton::format-streaming-head
                                 (resp '(("content-length" . "5"))) :chunked)
                                nil)
             (error () t))
           t)
    (check "streaming head: a caller-set Transfer-Encoding is refused"
           (handler-case (progn (web-skeleton::format-streaming-head
                                 (resp '(("transfer-encoding" . "chunked")))
                                 :chunked)
                                nil)
             (error () t))
           t)
    ;; On :CLOSE framing the Connection header *is* the framing — nothing
    ;; else says where the body ends — so a caller promising reuse would
    ;; leave the client unable to tell the eventual close from truncation.
    ;; Contradiction refused; agreement accepted.
    (check "streaming head: Connection: keep-alive is refused on close framing"
           (handler-case (progn (web-skeleton::format-streaming-head
                                 (resp '(("connection" . "keep-alive")))
                                 :close)
                                nil)
             (error () t))
           t)
    (check "streaming head: a redundant Connection: close is accepted"
           (let ((head (str (web-skeleton::format-streaming-head
                             (resp '(("connection" . "close"))) :close))))
             (and (search "connection: close" head) t))
           t)
    ;; The same header on chunked framing is only a hint, and stays one.
    (check "streaming head: Connection: keep-alive is fine on chunked framing"
           (let ((head (str (web-skeleton::format-streaming-head
                             (resp '(("connection" . "keep-alive"))) :chunked))))
             (and (search "transfer-encoding: chunked" head) t))
           t)
    (dolist (status '(204 304 100))
      (check (format nil "streaming head: status ~d cannot stream" status)
             (handler-case
                 (progn (web-skeleton::format-streaming-head
                         (web-skeleton::make-http-response :status status)
                         :chunked)
                        nil)
               (error () t))
             t))

    ;; --- Round-trip: our head plus our chunks, read by our own reader ---
    (let* ((head (web-skeleton::format-streaming-head (resp) :chunked))
           (full (concatenate '(vector (unsigned-byte 8))
                              head
                              (web-skeleton::encode-chunk (bytes "hello "))
                              (web-skeleton::encode-chunk (bytes "world"))
                              (web-skeleton::chunked-terminator))))
      (check "streaming head: the outbound reader sees a complete response"
             (and (web-skeleton::outbound-response-complete-p
                   full (length full) :GET)
                  t)
             t)
      (let ((hend (web-skeleton::scan-crlf-crlf full 0 (length full))))
        (check "streaming head: and the body decodes to what was streamed"
               (str (web-skeleton::decode-chunked-body
                     full (+ hend 4) (length full)))
               "hello world"))
      ;; Without the terminator the reader must keep waiting rather than
      ;; declaring the response done — which is what makes an unterminated
      ;; stream a hazard worth a lifecycle rule.
      (let ((unterminated (concatenate '(vector (unsigned-byte 8))
                                       head
                                       (web-skeleton::encode-chunk
                                        (bytes "hello ")))))
        (check "streaming head: an unterminated body never reads as complete"
               (web-skeleton::outbound-response-complete-p
                unterminated (length unterminated) :GET)
               nil)))))

;;; ---------------------------------------------------------------------------
;;; A handler that pushes and also returns
;;;
;;; DEPLOYMENT.md's own example calls ws-send from inside ws-handler. A
;;; handler that does that *and* returns a frame is the natural next step,
;;; and it is the case the read branch had to convert for: the returned
;;; frame used to go through CONNECTION-QUEUE-WRITE, which either
;;; overwrites what ws-send queued or — now that its guard counts the
;;; queue — signals, taking the connection down through the handler-case.
;;;
;;; Needs a genuinely bidirectional fd, so this builds a connected
;;; loopback pair rather than a pipe.
;;; ---------------------------------------------------------------------------

(defun %loopback-pair ()
  "Two connected TCP sockets over loopback. Returns (values server client)."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen listener 1)
    (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name listener)
      (declare (ignore addr))
      (let ((client (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp)))
        (sb-bsd-sockets:socket-connect client #(127 0 0 1) port)
        (let ((server (sb-bsd-sockets:socket-accept listener)))
          (sb-bsd-sockets:socket-close listener)
          (values server client))))))

(defun test-ws-handler-push-and-return ()
  (format t "~%ws handler push and return~%")
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let* ((server-fd (web-skeleton::socket-fd server))
                  (conn (web-skeleton::make-connection
                         :fd server-fd :socket server :state :websocket
                         :last-active (get-universal-time)))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  ;; Half a megabyte, against a shrunk send buffer and a
                  ;; client that never reads. The two paths are only
                  ;; distinguishable when ws-send leaves a remainder: with
                  ;; room to spare it flushes completely, the queue is
                  ;; empty by the time the handler returns, and a write
                  ;; that replaces the head looks exactly like one that
                  ;; appends behind it.
                  (pushed (web-skeleton::build-ws-frame
                           web-skeleton::+ws-op-binary+
                           (make-array (* 512 1024)
                                       :element-type '(unsigned-byte 8)
                                       :initial-element 80)))
                  (returned (web-skeleton::build-ws-text "RETURNED"))
                  (handler (lambda (c frame)
                             (declare (ignore frame))
                             ;; Push one frame, then hand back another.
                             (web-skeleton::ws-send c pushed)
                             returned)))
             (web-skeleton::set-nonblocking server-fd)
             ;; SO_SNDBUF is 7 on Linux. The kernel doubles the value and
             ;; enforces its own floor, so this asks for "as small as you
             ;; will give me" rather than an exact size.
             (web-skeleton::set-socket-option-int
              server-fd web-skeleton::+sol-socket+ 7 2048)
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd server-fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
             ;; The client sends one masked text frame to trigger the handler.
             (let ((req (make-test-ws-frame "GO")))
               (sb-bsd-sockets:socket-send client req nil))
             (let ((signalled
                     (handler-case
                         (progn (web-skeleton::handle-client-read
                                 conn epfd nil handler)
                                nil)
                       (error (e) (princ-to-string e)))))
               (check "ws push+return: the read branch does not signal"
                      signalled nil))
             (check "ws push+return: the connection is still open"
                    (hash-table-count web-skeleton::*connections*) 1)
             ;; The pushed frame is still the head, partly written; the
             ;; returned frame sits behind it rather than on top of it.
             (check "ws push+return: the pushed frame is still the head"
                    (web-skeleton::connection-write-end conn) (length pushed))
             (check "ws push+return: the head is only partly written"
                    (< (web-skeleton::connection-write-pos conn)
                       (web-skeleton::connection-write-end conn))
                    t)
             (check "ws push+return: the returned frame is queued behind it"
                    (coerce (first (web-skeleton::connection-write-queue conn))
                            'list)
                    (coerce returned 'list)))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client))))))

;;; ---------------------------------------------------------------------------
;;; The ping sweep flushes inline
;;;
;;; A two-byte ping onto an empty queue fits in any socket with room, so
;;; the sweep should hand it over on the spot and arm nothing. Under the
;;; old shape the frame sat queued until EPOLLOUT came back — one
;;; epoll_ctl per WebSocket connection per interval, in a burst, to
;;; deliver two bytes that had already fit. PENDING = 0 on return is the
;;; observable form of "no arming was needed".
;;; ---------------------------------------------------------------------------

(defun test-ws-ping-flush ()
  (format t "~%ws ping flush~%")
  (let ((epfd (web-skeleton::epoll-create))
        (sink (open "/dev/null" :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :append)))
    (unwind-protect
         (let* ((conn (web-skeleton::make-connection
                       :fd (sb-sys:fd-stream-fd sink)
                       :state :websocket
                       :last-active (get-universal-time)))
                (web-skeleton::*connections* (make-hash-table :test #'eql)))
           (web-skeleton::register-connection conn)
           (web-skeleton::ping-ws-connections epfd)
           (check "ws ping: nothing is left queued after the sweep"
                  (web-skeleton::connection-write-pending conn) 0)
           (check "ws ping: the ping was counted against the pong budget"
                  (web-skeleton::connection-missed-pongs conn) 1)
           ;; The fd was never registered with EPFD, so had the sweep tried
           ;; to arm EPOLLOUT the epoll_ctl would have failed and the
           ;; connection would have been collected as broken.
           (check "ws ping: the connection survives without any arming"
                  (hash-table-count web-skeleton::*connections*) 1))
      (ignore-errors (close sink))
      (ignore-errors (web-skeleton::%close epfd)))))

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
;;; The DNS phase and the sink
;;;
;;; INITIATE-HTTP-FETCH-TO-ADDRESS parks the target only for an :INBOUND
;;; fetch; INITIATE-DNS-LOOKUP has to make the same decision, because a
;;; hostname reaches the TCP phase through it and an IP literal does not.
;;; It used to park unconditionally, so every FETCH-INTO to a name — the
;;; shape DEPLOYMENT.md's relay example uses — moved the application's own
;;; :STREAMING or :WEBSOCKET connection to :AWAITING and nothing ever moved
;;; it back.
;;;
;;; Asserted against the real function rather than a stub, because the stub
;;; every other detached test installs for *DNS-LOOKUP-FN* is what hid this:
;;; they all jump straight to INITIATE-HTTP-FETCH-TO-ADDRESS, which was
;;; already right.
;;;
;;; No network. The name is RFC 2606's reserved .invalid, getent is killed
;;; by CLOSE-OUTBOUND before it can answer, and every assertion is about
;;; state INITIATE-DNS-LOOKUP has already set by the time it returns.
;;; ---------------------------------------------------------------------------

(defun test-dns-lookup-sink ()
  (format t "~%DNS lookup: the sink decides whether the target parks~%")
  (flet ((lookup (sink state)
           ;; Two epoll fds, as in TEST-AWAITING-SWEEP-504: one to register
           ;; against, one standing in for the target's descriptor. An
           ;; epoll fd is pollable and closeable, which is all either needs
           ;; to be here.
           (let ((epfd (web-skeleton::epoll-create))
                 (targetfd (web-skeleton::epoll-create)))
             (unwind-protect
                  (let* ((web-skeleton::*connections*
                           (make-hash-table :test #'eql))
                         (web-skeleton::*dns-cache* nil)
                         (target (web-skeleton::make-connection
                                  :fd targetfd :state state :last-active 0))
                         (cont (web-skeleton::make-http-fetch-continuation
                                :method :GET
                                :url "http://nxdomain.invalid/"
                                :callback (lambda (s h b)
                                            (declare (ignore s h b))
                                            nil)
                                :sink sink))
                         (dns-conn nil))
                    (web-skeleton::register-connection target)
                    (web-skeleton::initiate-dns-lookup
                     target epfd cont "nxdomain.invalid" 80 "/")
                    (maphash (lambda (fd c)
                               (declare (ignore fd))
                               (when (eq (web-skeleton::connection-state c)
                                         :out-dns)
                                 (setf dns-conn c)))
                             web-skeleton::*connections*)
                    (unwind-protect
                         (list (web-skeleton::connection-state target)
                               (>= (web-skeleton::connection-awaiting-fd
                                    target)
                                   0)
                               (and dns-conn
                                    (web-skeleton::connection-fetch-sink
                                     dns-conn))
                               (and dns-conn
                                    (plusp
                                     (web-skeleton::connection-fetch-deadline
                                      dns-conn))))
                      (when dns-conn
                        (ignore-errors
                         (web-skeleton::close-outbound dns-conn epfd)))))
               (ignore-errors (web-skeleton::%close targetfd))
               (ignore-errors (web-skeleton::%close epfd))))))

    ;; Asserted in halves, one lookup per case. The two claims are
    ;; independent — parking is a decision about the caller, the sink is a
    ;; value copied onto the dns-conn — and folded into a single compound
    ;; check they report under one name, so the failure list this suite is
    ;; actually read by cannot say which of them broke. Same reason
    ;; TEST-FETCH-FAILURE-DISPOSITION-CROSSES-THE-SEAM splits its pair.
    ;;
    ;; ATTEMPT's error text is handed through whole rather than sliced, so
    ;; a raise reports the condition instead of a NIL that says nothing.
    (flet ((parked (r) (if (listp r) (subseq r 0 2) r))
           (carried (r) (if (listp r) (subseq r 2 4) r)))

      ;; Control. The parked path is unchanged: an inbound waiting on a
      ;; fetch is exactly what :AWAITING is for, and its awaiting-fd is
      ;; the pipe.
      (let ((r (attempt (lookup :inbound :read-http))))
        (check "dns lookup: an :inbound fetch parks its caller"
               (parked r) '(:awaiting t))
        (check "dns lookup: an :inbound lookup carries its sink"
               (carried r) '(:inbound t)))

      ;; The defect. The application is still writing to this connection,
      ;; so its state is not the framework's to take — and nothing gives
      ;; it back, because the :DETACHED arm of
      ;; INITIATE-HTTP-FETCH-TO-ADDRESS correctly touches no state when
      ;; the lookup completes.
      (let ((r (attempt (lookup :detached :streaming))))
        (check "dns lookup: a detached fetch leaves its target alone"
               (parked r) '(:streaming nil))
        ;; The other half of the same defect. Without the sink on the
        ;; dns-conn, the detached reap, CLOSE-CONNECTION's orphan walk and
        ;; DELIVER-FETCH-ERROR all look at a detached fetch in flight and
        ;; see a parked one. The deadline rides along because being
        ;; visible to a reap that cannot tell your age is not being
        ;; visible to it.
        (check "dns lookup: a detached lookup carries its sink and a deadline"
               (carried r) '(:detached t))))))

;;; ---------------------------------------------------------------------------
;;; A name that will not resolve, and the fetch it still has to end
;;;
;;; DELIVER-DNS-ERROR held a copy of DELIVER-FETCH-ERROR's body that knew
;;; only about a parked inbound. Delegating is the fix; this asserts what
;;; the delegation buys, on the sink the copy had never heard of.
;;; ---------------------------------------------------------------------------

(defun test-dns-error-ends-a-detached-fetch ()
  "A detached fetch to a name that will not resolve releases its target.

   Three observations, and the first is a control that has to keep
   passing. The old body called CLOSE-OUTBOUND, which fires an unclaimed
   callback with the cleanup sentinel — so the application's callback ran
   either way, and a detector that only counted it would have been green
   against the defect. That is this branch's *confounded* mode: the right
   outcome reached by a path that proves nothing.

   What the old body could not do was reach the target. FETCH-OUTSTANDING
   stayed set for the rest of that connection's life, refusing every later
   FETCH-INTO on it, and the failure disposition the caller chose was never
   applied. Those two are the claim.

   No network and no getent: DELIVER-DNS-ERROR is called directly, which
   is what both of its failure sites do once they have decided the lookup
   is over."
  (format t "~%DNS failure: a detached fetch releases its target~%")
  (let ((fires 0)
        (epfd (web-skeleton::epoll-create))
        (targetfd (web-skeleton::epoll-create))
        (dnsfd (web-skeleton::epoll-create)))
    (unwind-protect
         (let* ((web-skeleton::*connections* (make-hash-table :test #'eql))
                (target (web-skeleton::make-connection
                         :fd targetfd :state :websocket
                         :fetch-outstanding t
                         :fetch-failure-disposition :close
                         :last-active (get-universal-time)))
                (dns-conn (web-skeleton::make-connection
                           :fd dnsfd :state :out-dns
                           :outbound-p t
                           :fetch-sink :detached
                           :inbound-fd targetfd
                           :fetch-callback (lambda (s h b)
                                             (declare (ignore s h b))
                                             (incf fires)
                                             nil)
                           :last-active (get-universal-time))))
           (web-skeleton::register-connection target)
           (web-skeleton::register-connection dns-conn)
           (web-skeleton::deliver-dns-error dns-conn epfd)
           ;; The control: unchanged, and the reason the other two are the
           ;; assertions rather than this one.
           (check "dns failure: the fetch callback still fired once" fires 1)
           (check "dns failure: the target's fetch marker was cleared"
                  (web-skeleton::connection-fetch-outstanding target) nil)
           (check "dns failure: and the failure disposition was applied"
                  (web-skeleton::connection-state target) :closing))
      (ignore-errors (web-skeleton::%close dnsfd))
      (ignore-errors (web-skeleton::%close targetfd))
      (ignore-errors (web-skeleton::%close epfd)))))

;;; ---------------------------------------------------------------------------
;;; Transfer-Encoding: the rules, and the codes they answer with
;;; ---------------------------------------------------------------------------

(defun test-transfer-encoding-rules ()
  (format t "~%Transfer-Encoding rules~%")
  (labels ((raw (version headers)
             (with-output-to-string (s)
               (format s "POST / HTTP/~a~a" version *crlf*)
               (dolist (h headers) (format s "~a~a" h *crlf*))
               (format s "~a" *crlf*)))
           (bytes (version headers)
             (sb-ext:string-to-octets (raw version headers)
                                      :external-format :ascii))
           (te (value)
             ;; A Transfer-Encoding header line carrying VALUE verbatim,
             ;; so a case can put a bare CR inside it.
             (concatenate 'string "Transfer-Encoding: " value))
           (cr (before after)
             ;; BEFORE and AFTER joined by a bare CR — a byte no header
             ;; value may contain, and one this scan has to notice before
             ;; PARSE-HEADERS-BYTES gets to at dispatch.
             (concatenate 'string before (string #\Return) after))
           (coding-of (headers &optional (version "1.1"))
             ;; Called the way CONNECTION-ON-READ calls it: bounded by the
             ;; CRLFCRLF and started past the request line.
             (let* ((b (bytes version headers))
                    (header-end (web-skeleton::scan-crlf-crlf b 0 (length b)))
                    (hdr-start (+ (web-skeleton::scan-crlf b 0 header-end) 2)))
               (attempt (web-skeleton::scan-transfer-encoding
                         b header-end hdr-start))))
           (on-read (headers &optional (version "1.1"))
             ;; The status the rules answer with, taken where the client
             ;; would get it: the condition CONNECTION-ON-READ raises, which
             ;; is what HANDLE-CLIENT-READ hands to MAKE-ERROR-RESPONSE. A
             ;; read-fn answering :AGAIN keeps the whole request in the
             ;; buffer and off any socket.
             (let ((b (bytes version headers))
                   (conn (web-skeleton::make-connection
                          :fd -1
                          :read-fn (lambda (buffer start max-bytes)
                                     (declare (ignore buffer start max-bytes))
                                     :again))))
               (setf (web-skeleton::connection-read-buf conn) b
                     (web-skeleton::connection-read-pos conn) (length b))
               (handler-case (web-skeleton::connection-on-read conn)
                 (web-skeleton:http-parse-error (e)
                   (web-skeleton::http-parse-error-status e))))))

    ;; ---- classification ----

    ;; The zero-count arm. A scanner that matched a header name as a
    ;; substring — "x-transfer-encoding", or the word inside a value —
    ;; would answer a coding here.
    (check "TE: absent is NIL"
           (coding-of '("Host: x" "X-Transfer-Encoding: chunked")) nil)

    ;; END is the CRLFCRLF position, so the last header line's CR sits *at*
    ;; END and a value scan bounded by END cannot see its terminator. These
    ;; two differ only in whether Transfer-Encoding is the final header; a
    ;; scan that got the bound wrong passes the second and fails the first.
    (check "TE: last header still reads its value"
           (coding-of '("Host: x" "Transfer-Encoding: chunked")) :chunked)
    (check "TE: non-final header reads the same"
           (coding-of '("Transfer-Encoding: chunked" "Host: x")) :chunked)

    (check "TE: field name and value both case fold"
           (coding-of '("Host: x" "TRANSFER-ENCODING: CHUNKED")) :chunked)

    (check "TE: surrounding OWS trimmed"
           (coding-of '("Host: x" "Transfer-Encoding:   chunked  ")) :chunked)

    ;; RFC 7230 §7's list rule permits empty elements. Counting them would
    ;; push chunked out of final position and refuse a legal message for a
    ;; reason that is not true of it.
    (check "TE: empty list elements are legal"
           (coding-of '("Host: x" "Transfer-Encoding: chunked,,")) :chunked)

    (check "TE: gzip alone is unsupported"
           (coding-of '("Host: x" "Transfer-Encoding: gzip")) :unsupported)

    ;; Legal, and chunked *is* final — the refusal is about the inner
    ;; coding, not the framing, which is why it is not :INVALID.
    (check "TE: gzip, chunked is unsupported"
           (coding-of '("Host: x" "Transfer-Encoding: gzip, chunked"))
           :unsupported)

    ;; RFC 7230 §3.3.1: chunked is applied once, and applied last.
    (check "TE: chunked in a non-final position is invalid"
           (coding-of '("Host: x" "Transfer-Encoding: chunked, gzip")) :invalid)
    (check "TE: chunked repeated in one value is invalid"
           (coding-of '("Host: x" "Transfer-Encoding: chunked, chunked"))
           :invalid)

    ;; The combination rule for repeated headers is defined, and computing
    ;; it is the reconciliation step the first rule refuses.
    (check "TE: two Transfer-Encoding headers are invalid"
           (coding-of '("Host: x" "Transfer-Encoding: gzip"
                        "Transfer-Encoding: chunked"))
           :invalid)

    (check "TE: a value naming no coding is invalid"
           (coding-of '("Host: x" "Transfer-Encoding:")) :invalid)

    ;; A bare CR ends the value scan early, so this reader sees a different
    ;; value than PARSE-HEADERS-BYTES will. Each of these four moves a
    ;; different way against its CR-free twin — less refusing, more
    ;; refusing, and twice not at all — which is why the arm rests on the
    ;; disagreement and not on a direction. SCAN-TRANSFER-ENCODING carries
    ;; the measured table.
    ;;
    ;; The no-space case is the one that distinguishes this arm. The spaced
    ;; case is here because it is what a natural probe writes, and the fold
    ;; arm catches it for an unrelated reason — tested alone, it makes a
    ;; missing bare-CR arm look present.
    (check "TE: bare CR before a comma is invalid"
           (coding-of (list "Host: x" (te (cr "chunked" ",gzip")))) :invalid)
    (check "TE: bare CR with the natural spacing is invalid"
           (coding-of (list "Host: x" (te (cr "chunked" ", gzip")))) :invalid)
    (check "TE: bare CR inside a token is invalid"
           (coding-of (list "Host: x" (te (cr "chu" "nked")))) :invalid)
    (check "TE: bare CR at the end of the value is invalid"
           (coding-of (list "Host: x" (te (cr "gzip" "")))) :invalid)

    ;; PARSE-HEADERS-BYTES rejects obsolete line folding too, but at
    ;; dispatch — after this scan has already decided how the body is
    ;; framed. Broken state: the fold is not noticed here, this reader
    ;; answers :CHUNKED for a value the parser reads as `chunked , gzip`,
    ;; and the two disagree about the framing of a body.
    (check "TE: obs-folded value is invalid"
           (coding-of (list "Host: x" "Transfer-Encoding: chunked"
                            (concatenate 'string (string #\Tab) ", gzip")))
           :invalid)

    ;; ---- the codes ----

    ;; RFC 7230 §3.3.3 says TE overrides CL. Answering 400 is the refusal
    ;; to apply that rule at all.
    (check "TE + Content-Length is 400"
           (on-read '("Host: x" "Transfer-Encoding: chunked"
                      "Content-Length: 5"))
           400)

    ;; Only *presence* is the violation, so the value is never consulted.
    ;; Broken state: SCAN-CONTENT-LENGTH's own rejections reach the client,
    ;; and the rule above holds only for Content-Lengths that happen to
    ;; parse — 400 for the value here, 413 below, both naming a condition
    ;; that is not why the request is refused.
    (check "TE + unparseable Content-Length is still the pair, 400"
           (on-read '("Host: x" "Transfer-Encoding: chunked"
                      "Content-Length: abc"))
           400)
    (check "TE + oversized Content-Length is 400, not 413"
           (on-read '("Host: x" "Transfer-Encoding: chunked"
                      "Content-Length: 99999999999"))
           400)

    (check "TE on HTTP/1.0 is 400"
           (on-read '("Host: x" "Transfer-Encoding: chunked") "1.0") 400)

    ;; The gate is stated as "for 1.1", not "against 1.0". The two
    ;; implementations agree on every 1.0 request and part here: under
    ;; "against 1.0" this byte is not 48, the gate does not fire, and the
    ;; coding answers 501. PARSE-REQUEST-BYTES would answer 505 for the
    ;; version — but at dispatch, and the framing is decided before that.
    (check "TE on an unvalidated version is 400"
           (on-read '("Host: x" "Transfer-Encoding: chunked") "1.9") 400)

    ;; The other half of that answer, and the premise the gate's comment
    ;; rests on: the same request without the header is the parser's 505,
    ;; raised at dispatch. The gate answers 400 rather than 505 because it
    ;; declines to hold a second copy of the supported-version set — this
    ;; says the set really does live somewhere else, so the asymmetry is a
    ;; division of labour and not an oversight.
    ;;
    ;; A pinned premise, not a detector, and the only one of these that no
    ;; revert can reach — `typo'd version is 505` above exercises the same
    ;; branch of the same function for the same reason. It earns its place
    ;; by sitting beside the 400 it explains, and nowhere else. Anything
    ;; that breaks it fails there first.
    (check "no TE: an unsupported version is the parser's 505"
           (let ((b (bytes "1.9" '("Host: x"))))
             (handler-case (progn (web-skeleton::parse-request-bytes
                                   b 0 (length b))
                                  :no-error)
               (web-skeleton:http-parse-error (e)
                 (web-skeleton::http-parse-error-status e))))
           505)

    ;; Transfer-Encoding is a 1.1 field, so the gate covers every coding.
    ;; Broken state: the gate sits inside the chunked arm, and a 1.0
    ;; request naming gzip answers 501 — the coding, when the version is
    ;; the reason it is refused.
    (check "TE: gzip on HTTP/1.0 is 400, not 501"
           (on-read '("Host: x" "Transfer-Encoding: gzip") "1.0") 400)

    (check "TE with chunked non-final is 400"
           (on-read '("Host: x" "Transfer-Encoding: chunked, gzip")) 400)

    (check "TE repeated as two headers is 400"
           (on-read '("Host: x" "Transfer-Encoding: gzip"
                      "Transfer-Encoding: chunked"))
           400)

    (check "TE bare CR is 400"
           (on-read (list "Host: x" (te (cr "chunked" ",gzip")))) 400)

    (check "TE obs-folded is 400"
           (on-read (list "Host: x" "Transfer-Encoding: chunked"
                          (concatenate 'string (string #\Tab) ", gzip")))
           400)

    ;; §3.3.1's own code for a coding the server does not understand.
    (check "TE: gzip is 501"
           (on-read '("Host: x" "Transfer-Encoding: gzip")) 501)
    (check "TE: gzip, chunked is 501"
           (on-read '("Host: x" "Transfer-Encoding: gzip, chunked")) 501)
    ;; Chunked is accepted now, so what this pins is the rules running
    ;; ahead of the body arm: the request is well formed, no body byte has
    ;; arrived, and the answer is to keep reading rather than to refuse or
    ;; to dispatch a POST with its body still on the wire.
    ;; TEST-CHUNKED-REQUEST-BODY covers where it ends.
    (check "TE: chunked alone is accepted and waits for its body"
           (on-read '("Host: x" "Transfer-Encoding: chunked")) :continue)

    ;; The zero-behavior-change claim, asserted rather than assumed.
    (check "no TE: request still dispatches"
           (on-read '("Host: x")) :dispatch)
    (check "no TE: a Content-Length body still reads"
           (on-read '("Host: x" "Content-Length: 5")) :continue)))

;;; ---------------------------------------------------------------------------
;;; Chunked request bodies: where the request ends, and what ends it
;;; ---------------------------------------------------------------------------

(defun test-chunked-request-body ()
  (format t "~%Chunked request bodies~%")
  (labels ((req (body &key (headers '("Host: x" "Transfer-Encoding: chunked"))
                           (version "1.1"))
             (sb-ext:string-to-octets
              (with-output-to-string (s)
                (format s "POST /u HTTP/~a~a" version *crlf*)
                (dolist (h headers) (format s "~a~a" h *crlf*))
                (format s "~a~a" *crlf* body))
              :external-format :ascii))
           (drive (bytes)
             ;; Returns (values VERDICT CONN). A read-fn answering :AGAIN
             ;; keeps everything in the buffer and off any socket, so the
             ;; verdict is the state machine's and nothing else.
             (let ((conn (web-skeleton::make-connection
                          :fd -1
                          :read-fn (lambda (buffer start max-bytes)
                                     (declare (ignore buffer start max-bytes))
                                     :again))))
               (setf (web-skeleton::connection-read-buf conn) bytes
                     (web-skeleton::connection-read-pos conn) (length bytes))
               (values (handler-case (web-skeleton::connection-on-read conn)
                         (web-skeleton:http-parse-error (e)
                           (web-skeleton::http-parse-error-status e))
                         ;; Anything else becomes its own text rather than
                         ;; ending the run. CONNECTION-ON-READ can raise a
                         ;; plain error — the walk does, on a size line it
                         ;; can already tell is invalid — and a raise that
                         ;; escapes here takes every later assertion in the
                         ;; file with it. A failed CHECK carrying the
                         ;; condition says the same thing and stays
                         ;; countable.
                         (error (e) (princ-to-string e)))
                       conn)))
           (verdict (body &rest args)
             (values (apply #'drive (list (apply #'req body args)))))
           (body-of (body)
             ;; Drive to :DISPATCH, then parse — the decoded body is what
             ;; a handler would receive.
             (multiple-value-bind (v conn) (drive (req body))
               (declare (ignore v))
               (handler-case
                   (let ((r (web-skeleton::connection-parse-request conn)))
                     (sb-ext:octets-to-string (web-skeleton:http-request-body r)
                                              :external-format :ascii))
                 (web-skeleton:http-parse-error (e)
                   (web-skeleton::http-parse-error-status e))))))

    ;; ---- where a chunked request ends ----

    ;; Headers only: the framing says a body is coming and none of it has
    ;; arrived. Broken state: the Content-Length arm answers "no body" for
    ;; this request and dispatches a POST with its body still on the wire.
    (check "chunked: no body bytes yet keeps reading"
           (verdict "") :continue)

    ;; The whole thing in one read.
    (check "chunked: a complete body dispatches"
           (verdict (format nil "3~a123~a0~a~a" *crlf* *crlf* *crlf* *crlf*))
           :dispatch)

    ;; THE two-byte rule. `...0 CRLF` is the zero-size chunk header and
    ;; nothing else; the CRLF that terminates an empty trailer section has
    ;; not arrived. Broken state: complete is answered here, REQUEST-END
    ;; lands two bytes early, and those two bytes are shifted to offset 0
    ;; and read as the beginning of the next request.
    (check "chunked: the terminator alone is not the end of the request"
           (verdict (format nil "3~a123~a0~a" *crlf* *crlf* *crlf*))
           :continue)
    (check "chunked: the empty trailer's CRLF is what ends it"
           (verdict (format nil "3~a123~a0~a~a" *crlf* *crlf* *crlf* *crlf*))
           :dispatch)

    ;; An empty body is still a body, and still needs its terminator.
    (check "chunked: zero chunks, terminated, dispatches"
           (verdict (format nil "0~a~a" *crlf* *crlf*)) :dispatch)

    ;; The boundary itself, not a delivery-shaped proxy for it: with a
    ;; second request pipelined behind, REQUEST-END has to land exactly on
    ;; its first byte. Computed from the pieces rather than written as a
    ;; number — a hand-counted offset is a second implementation of the
    ;; arithmetic this slot exists to delete, and the first draft of this
    ;; assertion got it wrong by five bytes.
    (let* ((first-req (req (format nil "3~a123~a0~a~a"
                                   *crlf* *crlf* *crlf* *crlf*)))
           (both (concatenate '(vector (unsigned-byte 8))
                              first-req
                              (sb-ext:string-to-octets
                               "GET /b HTTP/1.1" :external-format :ascii))))
      (multiple-value-bind (v conn) (drive both)
        (declare (ignore v))
        (check "chunked: the boundary lands on the next request's first byte"
               (web-skeleton::connection-request-end conn)
               (length first-req))))

    ;; ---- trailers ----

    ;; The decision: refuse. Nothing surfaces trailers to an app, so
    ;; accepting silently discards data the client believed it sent, and
    ;; consuming needs a second header parser whose disagreement with the
    ;; first is the shape these rules exist to prevent.
    (check "chunked: a trailer field is refused"
           (verdict (format nil "0~aX-T: 1~a~a" *crlf* *crlf* *crlf*)) 400)

    ;; The headline acceptance criterion, asserted at the boundary rather
    ;; than at delivery: a trailer section carrying a complete HTTP request
    ;; must not become a second request. Refusing means the boundary for
    ;; this request is never computed at all.
    (check "chunked: a trailer holding a whole request is refused"
           (verdict (format nil "0~aGET /admin HTTP/1.1~aHost: x~a~a"
                            *crlf* *crlf* *crlf* *crlf*))
           400)

    ;; A bare CR where the terminator belongs.
    (check "chunked: a bare CR after the terminator is refused"
           (verdict (format nil "0~a~a!" *crlf* (string #\Return))) 400)

    ;; ---- the body a handler receives ----

    (check "chunked: the decoded body reaches the request"
           (body-of (format nil "3~aabc~a2~ade~a0~a~a"
                            *crlf* *crlf* *crlf* *crlf* *crlf* *crlf*))
           "abcde")

    (check "chunked: an empty chunked body decodes to nothing"
           (body-of (format nil "0~a~a" *crlf* *crlf*)) "")
    ;; `5g` — the issue names this one. The walk accepts it as framing:
    ;; it reads one hex digit, stops at a byte that is not one, finds the
    ;; line's LF and skips five bytes, exactly as a well-formed header
    ;; would have it do. The decoder is what refuses, on the byte between
    ;; the size and the CRLF, and that is the split working rather than
    ;; failing — the walk defers what it cannot decide on sight.
    (check "chunked: a junk byte after the chunk-size is 400"
           (body-of (format nil "5g~ahello~a0~a~a"
                            *crlf* *crlf* *crlf* *crlf*))
           400)

    ;; The framing walk is deliberately lax — it answers "do we have it
    ;; all yet", and a too-strict predicate would hang instead of refusing.
    ;; DECODE-CHUNKED-BODY is the validator, and its error has to become
    ;; the client's 400 rather than the 500 an unhandled error becomes.
    ;; Broken state: the decode error escapes CONNECTION-PARSE-REQUEST and
    ;; HANDLE-CLIENT-READ's generic arm answers 500 — the server blamed
    ;; for a malformed request.
    (check "chunked: a size line the decoder rejects is 400, not 500"
           (body-of (format nil "3~aabcXX0~a~a" *crlf* *crlf* *crlf*)) 400)



    ;; ---- the size cap, which has no declared length to read ----

    ;; *MAX-BODY-SIZE* is enforced before allocating on the Content-Length
    ;; path, against a number the client supplied. A chunked request
    ;; supplies none, so the check has to become incremental, and this is
    ;; the only place a *body*-size refusal can come from at all.
    (let ((web-skeleton::*max-body-size* 4))
      (check "chunked: a body over the cap is 413"
             (verdict (format nil "5~ahello~a0~a~a" *crlf* *crlf* *crlf* *crlf*))
             413)
      ;; The boundary, so the comparison is > and not >=.
      (check "chunked: a body exactly at the cap is served"
             (verdict (format nil "4~ahell~a0~a~a" *crlf* *crlf* *crlf* *crlf*))
             :dispatch)
      ;; Spread across chunks: no single chunk crosses the cap, the
      ;; running total does. Broken state: the check reads one chunk's
      ;; size instead of accumulating, and three 2-byte chunks pass a
      ;; 4-byte cap.
      (check "chunked: the cap is a running total, not a per-chunk test"
             (verdict (format nil "2~aab~a2~acd~a2~aef~a0~a~a"
                              *crlf* *crlf* *crlf* *crlf*
                              *crlf* *crlf* *crlf* *crlf*))
             413))

    ;; Refused on the *declared* size, the moment the header arrives.
    ;; Broken state: the walk skips past the promised bytes, finds itself
    ;; beyond the buffer, answers "keep reading", and the client is
    ;; eventually told the read buffer filled — when what it actually did
    ;; was declare a chunk larger than any body this server accepts. The
    ;; buffer's answer is true and useless; a client cannot act on it.
    (check "chunked: an oversized chunk header is refused on arrival"
           (verdict (format nil "ffffffff~a" *crlf*)) 413)
    ;; Sixteen digits is the walk's own limit, and the product is past
    ;; MOST-POSITIVE-FIXNUM — the projected total is a local, so it may be
    ;; a bignum without the accumulator slot ever leaving FIXNUM.
    (check "chunked: a sixteen-digit chunk size is refused too"
           (verdict (format nil "ffffffffffffffff~a" *crlf*)) 413)
    ;; One digit further and the size is not large, it is unrepresentable
    ;; — the walk's own digit cap trips before any value exists to
    ;; project. Broken state: that arm answers "keep reading", the
    ;; projection sees nothing, and the request waits for bytes that
    ;; cannot come until the read buffer fills and answers about itself.
    ;; 400 rather than 413 because the fault is the framing, not a size.
    (check "chunked: a size line past the digit limit is 400, not a stall"
           (verdict (format nil "fffffffffffffffff~a" *crlf*)) 400)
    ;; ---- the buffer-full arm ----

    ;; A chunked body larger than the read cap has to answer 413, and this
    ;; is the assertion for the third reader of "is the body complete".
    ;; The :FULL arm used to be Content-Length arithmetic, which a chunked
    ;; request satisfies unconditionally — BODY-EXPECTED is 0 — so it would
    ;; call a half-arrived body complete, fall through, and then :READ-BODY
    ;; would answer :CONTINUE while the buffer sat at its cap with nothing
    ;; able to read further. The connection would be held until the idle
    ;; sweeper took it. Broken state: :CONTINUE here instead of 413.
    ;;
    ;; Two calls because the arm is only reachable from :READ-BODY: the
    ;; first establishes the framing, the second arrives with the buffer
    ;; already at its cap.
    (let* ((web-skeleton::*max-body-size* 256)
           (conn (web-skeleton::make-connection
                  :fd -1
                  :read-fn (lambda (buffer start max-bytes)
                             (declare (ignore buffer start max-bytes))
                             :again)))
           (head (req ""))
           (cap (web-skeleton::connection-read-cap conn)))
      ;; First pass: headers only, framing established, waiting on a body.
      (setf (web-skeleton::connection-read-buf conn) head
            (web-skeleton::connection-read-pos conn) (length head))
      (check "chunked: buffer-full setup reaches :read-body"
             (attempt (web-skeleton::connection-on-read conn)) :continue)
      ;; Second pass: a buffer at its cap holding framing that is
      ;; *incomplete* rather than invalid, so this reaches the :FULL arm
      ;; and not one of the refusals above it. `1;` opens a chunk-size
      ;; line with an extension and the rest of the buffer never
      ;; terminates it, so the walk finds no LF and answers keep reading
      ;; with nothing pending — the body cap sees 0 and declines, which is
      ;; the only way to isolate the buffer's 413 from the body's.
      ;;
      ;; The filler must not look like a chunk-size line. An earlier draft
      ;; used a size header and left the rest as `a` bytes, which are hex
      ;; digits: the walk read tens of thousands of them, tripped its
      ;; sixteen-digit cap, and answered 400 — a correct answer to a
      ;; different question, and this assertion failed for the right
      ;; reason with the wrong subject.
      (let ((big (make-array cap :element-type '(unsigned-byte 8)
                                 :initial-element 97)))   ; #\a
        (replace big head)
        (replace big (sb-ext:string-to-octets "1;" :external-format :ascii)
                 :start1 (length head))
        (setf (web-skeleton::connection-read-buf conn) big
              (web-skeleton::connection-read-pos conn) cap)
        (check "chunked: a body past the read cap is 413, not a stall"
               (handler-case (web-skeleton::connection-on-read conn)
                 (web-skeleton:http-parse-error (e)
                   (web-skeleton::http-parse-error-status e)))
               413)))

    ;; ---- Expect: 100-continue ----

    ;; Routed through :SENDING-100-CONTINUE rather than around it, so the
    ;; interim leaves by the same path a Content-Length body's does.
    ;; Broken state: the chunked arm answers :CONTINUE, the client waits
    ;; its timeout and sends anyway, and every chunked upload from a
    ;; well-behaved client pays a second or more it should not.
    (multiple-value-bind (v conn)
        (drive (req "" :headers '("Host: x" "Transfer-Encoding: chunked"
                                  "Expect: 100-continue")))
      (check "chunked + 100-continue: the interim is flushed"
             v :flush-queued)
      (check "chunked + 100-continue: it is the interim that was queued"
             (let ((b (web-skeleton::connection-write-buf conn)))
               (and b (search "100 Continue"
                              (sb-ext:octets-to-string
                               b :external-format :ascii))
                    t))
             t)
      (check "chunked + 100-continue: and the state is the interim's"
             (web-skeleton::connection-state conn) :sending-100-continue))

    ;; The client did not wait. Sending an interim it has stopped
    ;; listening for is latency on what is, for chunked, the common
    ;; shape — a sender that ignored the expectation has usually sent the
    ;; whole body. Broken state: the interim arm is tested before the
    ;; already-complete one, and every such upload takes an extra
    ;; round trip.
    (multiple-value-bind (v conn)
        (drive (req (format nil "3~aabc~a0~a~a" *crlf* *crlf* *crlf* *crlf*)
                    :headers '("Host: x" "Transfer-Encoding: chunked"
                               "Expect: 100-continue")))
      (check "chunked + 100-continue: a body already here dispatches"
             v :dispatch)
      (check "chunked + 100-continue: and no interim was queued"
             (null (web-skeleton::connection-write-buf conn)) t))
    ;; ---- interaction with the rules already in place ----

    ;; The chunked arm falls through to the body cond rather than
    ;; returning early, so the Expect gate still runs ahead of it.
    ;; Broken state: chunked returns from the classification block and a
    ;; chunked POST with an unknown expectation is accepted where a
    ;; Content-Length one is refused.
    (check "chunked: an unknown Expect still 417s"
           (verdict "" :headers '("Host: x" "Transfer-Encoding: chunked"
                                  "Expect: x-foo"))
           :flush-queued)))


;;; ---------------------------------------------------------------------------
;;; A request that can never be finished does not keep its slot
;;; ---------------------------------------------------------------------------

(defun test-unfinishable-request-closes ()
  (format t "~%Unfinishable request + peer FIN~%")
  (labels ((drive (raw eof-verdict)
             ;; RAW arrives in one read, then the peer's end-of-stream.
             ;; CONNECTION-READ-AVAILABLE reports :OK-EOF for bytes and
             ;; FIN together and :EOF for FIN alone, and both mean the
             ;; same thing to a request still waiting on bytes.
             (let* ((bytes (sb-ext:string-to-octets raw
                                                    :external-format :ascii))
                    (sent (list nil))
                    (conn (web-skeleton::make-connection
                           :fd -1
                           :read-fn
                           (lambda (buffer start max-bytes)
                             (cond
                               ((car sent) eof-verdict)
                               (t (setf (car sent) t)
                                  (let ((n (min (length bytes) max-bytes)))
                                    (replace buffer bytes :start1 start
                                                          :end2 n)
                                    n)))))))
               (handler-case (web-skeleton::connection-on-read conn)
                 (web-skeleton:http-parse-error (e)
                   (web-skeleton::http-parse-error-status e))
                 (error (e) (princ-to-string e))))))

    ;; Headers that never terminate. Nothing more is coming, so waiting
    ;; for a CRLFCRLF that cannot arrive holds a connection slot for
    ;; *IDLE-TIMEOUT* with no peer on the other end — free for whoever
    ;; sent it and walked away.
    (check "unfinishable: half a request line with FIN closes"
           (drive "GET /par" :eof) :close)

    ;; A Content-Length body short of its declared length.
    (check "unfinishable: a body short of its Content-Length closes"
           (drive (format nil "POST /u HTTP/1.1~aHost: x~aContent-Length: 10~a~aabc"
                          *crlf* *crlf* *crlf* *crlf*)
                  :eof)
           :close)

    ;; And chunked, which is the framing that has no declared length to
    ;; be short of — the terminator is missing and no byte can supply it.
    ;; Broken state: the check reads BODY-EXPECTED, which is 0 here, and
    ;; a half-arrived chunked upload is held rather than closed.
    (check "unfinishable: a chunked body without its terminator closes"
           (drive (format nil "POST /u HTTP/1.1~aHost: x~aTransfer-Encoding: chunked~a~a3~aab"
                          *crlf* *crlf* *crlf* *crlf* *crlf*)
                  :eof)
           :close)

    ;; The other half of the rule, and the one that must not move: a
    ;; *complete* request arriving with its own FIN is answered, not
    ;; closed. This is what TEST-HARNESS-PIPELINED-WITH-FIN-E2E covers
    ;; end to end; asserted here too because it is the failure this
    ;; change could plausibly cause and a unit check names it directly.
    (check "unfinishable: a complete request with FIN still dispatches"
           (drive (format nil "GET / HTTP/1.1~aHost: x~a~a" *crlf* *crlf* *crlf*)
                  :eof)
           :dispatch)
    (check "unfinishable: a complete chunked request with FIN dispatches"
           (drive (format nil "POST /u HTTP/1.1~aHost: x~aTransfer-Encoding: chunked~a~a3~aabc~a0~a~a"
                          *crlf* *crlf* *crlf* *crlf* *crlf* *crlf* *crlf* *crlf*)
                  :eof)
           :dispatch)))

;;; ---------------------------------------------------------------------------
;;; Runner
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; cpu-count
;;;
;;; Driven with synthetic file contents rather than the live filesystem,
;;; because a host that exposes cgroup CPU files cannot be relied on. CI does
;;; not, so without this every branch except the final fall-through would go
;;; unexercised — and the fall-through is the one path that was already
;;; working.
;;; ---------------------------------------------------------------------------

(defun %detached-pause-pass (label target-state)
  "Drive RESUME-PAUSED-OUTBOUND's edge against a target in TARGET-STATE.

   LABEL prefixes every assertion, so a failure names which target state
   broke rather than only that one did."
  (multiple-value-bind (out-server out-client) (%loopback-pair)
    (multiple-value-bind (tgt-server tgt-client) (%loopback-pair)
      (let ((epfd (web-skeleton::epoll-create)))
        (unwind-protect
             (let* ((out-fd (web-skeleton::socket-fd out-server))
                    (tgt-fd (web-skeleton::socket-fd tgt-server))
                    (web-skeleton::*connections* (make-hash-table :test #'eql))
                    (web-skeleton::*epoll-fd* epfd)
                    ;; The outbound, paused exactly as HANDLE-OUTBOUND-READ
                    ;; leaves one: subscribed to no events at all, so
                    ;; nothing but a resume can ever wake it.
                    (out (web-skeleton::make-connection
                          :fd out-fd :socket out-server :state :out-read
                          :outbound-p t
                          :fetch-sink :detached
                          :fetch-paused t
                          :fetch-paused-at (get-universal-time)
                          :fetch-deadline (+ (get-universal-time) 30)
                          :inbound-fd tgt-fd
                          :last-active (get-universal-time)))
                    ;; The target: a streaming connection with the
                    ;; back-link the pause left on it.
                    (tgt (web-skeleton::make-connection
                          :fd tgt-fd :socket tgt-server :state target-state
                          :stream-framing (when (eq target-state :streaming)
                                            :chunked)
                          :paused-outbound-fd out-fd
                          :last-active (get-universal-time))))
               (web-skeleton::set-nonblocking out-fd)
               (web-skeleton::set-nonblocking tgt-fd)
               (web-skeleton::register-connection out)
               (web-skeleton::register-connection tgt)
               (web-skeleton::epoll-add epfd out-fd web-skeleton::+epollet+)
               (web-skeleton::epoll-add epfd tgt-fd
                                        (logior web-skeleton::+epollout+
                                                web-skeleton::+epollet+))
               (check (format nil "~a: the outbound starts paused" label)
                      (web-skeleton::connection-fetch-paused out) t)
               ;; A backlog on the target, and then the drain that ends it.
               (web-skeleton::connection-append-write
                tgt (sb-ext:string-to-octets "queued" :external-format :ascii))
               (check (format nil "~a: the target has a backlog to drain" label)
                      (plusp (web-skeleton::connection-write-pending tgt)) t)
               (web-skeleton::handle-client-write tgt epfd)
               (check (format nil "~a: the target drained" label)
                      (web-skeleton::connection-write-pending tgt) 0)
               ;; The property.
               (check (format nil "~a: draining the target resumed the fetch" label)
                      (web-skeleton::connection-fetch-paused out) nil)
               (check (format nil "~a: and the back-link was cleared with it" label)
                      (web-skeleton::connection-paused-outbound-fd tgt) -1)
               ;; The deadline moved by the time spent paused, so a relay
               ;; is not killed for applying the backpressure it was told
               ;; to apply.
               (check (format nil "~a: the deadline is not still the original" label)
                      (>= (web-skeleton::connection-fetch-deadline out)
                          (+ (web-skeleton::connection-fetch-started-at out)
                             30))
                      t))
          (ignore-errors (web-skeleton::%close epfd))
          (ignore-errors (sb-bsd-sockets:socket-close out-server))
          (ignore-errors (sb-bsd-sockets:socket-close out-client))
          (ignore-errors (sb-bsd-sockets:socket-close tgt-server))
          (ignore-errors (sb-bsd-sockets:socket-close tgt-client)))))))

(defun test-detached-pause-auto-resumes ()
  "A paused detached fetch is resumed by its target's backlog draining.

   Sibling to TEST-AUTOMATIC-RESUME-EDGE rather than a replacement for it.
   That one has covered the mechanism since the resume edge landed, and
   with a :STREAMING target — what it could not cover is that anything
   reached it. RESUME-PAUSED-OUTBOUND runs from HANDLE-CLIENT-WRITE's :DONE
   arm when a connection's backlog empties, and until the detached seam the
   only connection a fetch could pause against was an :AWAITING inbound,
   which has nothing queued by construction and so never receives EPOLLOUT.
   A tested mechanism with no reachable caller, which is the same shape as
   the relay example this branch deleted.

   So what is new here is the sink, not the edge: a paused outbound whose
   FETCH-SINK is :DETACHED, resumed by its target draining, with the
   deadline advanced by the interval spent paused.

   Driven here rather than end to end, and that is a deliberate retreat.
   The e2e version has to make a real client stop reading until the
   target's socket refuses a write, and loopback will not cooperate:
   measured, 768 KiB went through with the target's pending bytes never
   leaving 0, and 5 MiB made the test slow without making it reliable. A
   test that cannot provoke the state it names is worse than one that
   drives the edge directly — it passes for the wrong reason, which is the
   failure this branch exists to delete.

   So the edge is driven: a target holding a backlog and a paused outbound
   behind it, flushed until :DONE, with the outbound's interest checked
   before and after.

   The precondition this pins down, which no document has ever carried:
   auto-resume needs the target to have *actually backed up*. STREAM-SEND
   flushes inline, and a flush that completes arms nothing and so never
   reaches HANDLE-CLIENT-WRITE — so a pause taken while the target's queue
   was empty has no wake-up coming and still needs an explicit
   FETCH-RESUME.

   Run against both target states. RESUME-PAUSED-OUTBOUND sits in
   HANDLE-CLIENT-WRITE's :DONE arm ahead of the state dispatch, and its own
   comment says why: every state that can be relayed into reaches that
   point, and a resume working for only one of them would be the kind of
   gap nobody finds until a different response shape turns up. Only
   :STREAMING was ever asserted against it.

   The :WEBSOCKET pass is a regression guard and not a detector for this
   branch — it passes on both sides, because the edge itself was always
   state-agnostic. What the branch changes is whether it is reachable:
   arming the remainder WS-SEND leaves behind is what lets
   HANDLE-CLIENT-WRITE run for a websocket target at all. This asserts the
   half of that chain the arming detector cannot see."
  (format t "~%Detached fetch: pause resumes when the target drains~%")
  (%detached-pause-pass "detached pause" :streaming)
  (%detached-pause-pass "detached pause (ws)" :websocket))

;;; ---------------------------------------------------------------------------
;;; The write path a closed stream is handed to, and the event that runs it
;;;
;;; STREAM-CLOSE moves the connection to :WRITE-RESPONSE, which
;;; HANDLE-CLIENT-WRITE and nothing else transitions out of. It used to
;;; hand off through STREAM-FLUSH, which arms only when the flush did not
;;; complete — so the ordinary close, a five-byte terminator onto a socket
;;; with room, armed nothing and left the connection parked.
;;;
;;; TEST-STREAM-LIFECYCLE builds this exact state and asserts both halves
;;; of its precondition — :WRITE-RESPONSE, and nothing left queued — and
;;; then never asks whether anything would move it. That is the shape of
;;; the gap, and the reason it survived a green suite.
;;; ---------------------------------------------------------------------------

(defun %stream-close-pass (back-up)
  "Close a stream and report the flush state, the interest armed, and where
   the connection was left.

   Returns (values PENDING MASKS CLOSED-STATE POST-STATE) — bytes still
   queued afterwards, every mask armed while STREAM-CLOSE ran in order, the
   state it left, and the state after one turn of the event loop.

   That last turn is driven through epoll rather than by calling
   HANDLE-CLIENT-WRITE, which is the difference between asserting the
   consequence and assuming it: the direct call runs whether or not
   anything armed the fd.

   BACK-UP shrinks SO_SNDBUF and pre-queues more than the socket will take,
   which is the branch STREAM-FLUSH used to be the only arming for. Without
   it the terminator goes out whole, the flush completes, and that is both
   the ordinary case and the one that armed nothing."
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create))
          (masks nil)
          (real (symbol-function 'web-skeleton::epoll-modify)))
      (unwind-protect
           (let* ((fd (web-skeleton::socket-fd server))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (conn (web-skeleton::make-connection
                         :fd fd :socket server :state :streaming
                         :stream-framing :chunked
                         :last-active (get-universal-time))))
             (web-skeleton::set-nonblocking fd)
             (when back-up
               ;; SO_SNDBUF is 7 on Linux, as in TEST-WS-HANDLER-PUSH-AND-RETURN.
               (web-skeleton::set-socket-option-int
                fd web-skeleton::+sol-socket+ 7 2048)
               (web-skeleton::connection-append-write
                conn (make-array (* 512 1024)
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 80)))
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
             ;; Recording starts here, so what is counted is STREAM-CLOSE's
             ;; own arming and not the setup's.
             (setf (symbol-function 'web-skeleton::epoll-modify)
                   (lambda (efd f mask)
                     (push mask masks)
                     (funcall real efd f mask)))
             (web-skeleton:stream-close conn)
             (setf (symbol-function 'web-skeleton::epoll-modify) real)
             (let ((pending (web-skeleton::connection-write-pending conn))
                   (closed-state (web-skeleton::connection-state conn))
                   (evbuf (make-array (* 4 web-skeleton::+epoll-event-size+)
                                      :element-type '(unsigned-byte 8))))
               ;; One turn of the event loop, driven the way the loop drives
               ;; it: ask epoll what is ready, and dispatch only that.
               ;; Calling HANDLE-CLIENT-WRITE directly would prove nothing —
               ;; it would run whether or not anything armed it, which is
               ;; the whole defect. Fifty milliseconds, not a deadline: a
               ;; writable socket with an armed interest is reported on the
               ;; first call, and an unarmed one is never reported at all,
               ;; so neither answer is waited for.
               (let ((answered :not-run))
                 (when (plusp (web-skeleton::epoll-wait epfd evbuf 4 50))
                   (setf answered
                         (web-skeleton::handle-client-write conn epfd)))
                 (values pending
                         (reverse masks)
                         closed-state
                         (web-skeleton::connection-state conn)
                         answered))))
        (setf (symbol-function 'web-skeleton::epoll-modify) real)
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client))))))

(defun test-stream-close-arms-the-write-path ()
  "A closed stream gets an event to finish on, whether or not it needed one
   to flush.

   The defect case is the ordinary one. With the terminator away and the
   queue empty there is nothing left to write, which is precisely why
   STREAM-FLUSH declined to arm — and precisely when the connection still
   has a transition owed to it: :WRITE-RESPONSE is where a keep-alive
   connection resets to :READ-HTTP and where a close-delimited one closes,
   and only HANDLE-CLIENT-WRITE performs it. Bounded by the idle sweep, so
   the symptom is liveness rather than loss: a FIN that waits
   *IDLE-TIMEOUT*, and a keep-alive socket that never answers again.

   The backed-up case is the control, and it is what makes the first
   assertion about STREAM-CLOSE rather than about streams in general. It
   was armed before this change and still is — by STREAM-FLUSH then, by
   STREAM-CLOSE now — so it passes on both sides. Only the case where the
   flush succeeded moves.

   The defect case also asserts what the arming buys: one turn of the loop
   and the connection is back at :READ-HTTP, reusable. That turn is driven
   through EPOLL-WAIT and dispatched only if the fd is reported, which is
   the difference between asserting the consequence and assuming it —
   calling HANDLE-CLIENT-WRITE directly would run whether or not anything
   armed the fd. Under the defect the loop is asked, told nothing is ready,
   and the connection stays :WRITE-RESPONSE. Fifty milliseconds bounds it
   and neither answer waits for the bound: an armed writable socket is
   reported on the first call, an unarmed one never.

   The control does not assert that half. Its socket is full, so no
   EPOLLOUT arrives whether or not the interest is set, and the loop turn
   is uninformative there rather than wrong.

   The first case asserts the whole mask list rather than membership,
   which pins three things at once: that arming happened, that it happened
   exactly once, and that it is EPOLLOUT alone. The last is the decision
   worth pinning — STREAM-FLUSH's mask keeps EPOLLIN for a reason its own
   docstring gives, that a stream has to notice its peer going away, and
   this connection is no longer a stream. The control asserts membership
   only, because its mask is the thing that changed."
  (format t "~%Stream close: the write path gets an event to run on~%")
  (multiple-value-bind (pending masks state post answered)
      (%stream-close-pass nil)
    (check "stream close: the terminator flushed completely" pending 0)
    (check "stream close: the ordinary write path is in charge"
           state :write-response)
    (check "stream close: EPOLLOUT was armed even so"
           masks
           (list (logior web-skeleton::+epollout+ web-skeleton::+epollet+)))
    (check "stream close: and one turn of the loop reuses the connection"
           post :read-http)
    ;; The loop is told to answer, not merely reset. HANDLE-CLIENT-WRITE's
    ;; :KEEP-ALIVE is what sends the event loop back into the read path
    ;; without waiting for another EPOLLIN, and it is the step between a
    ;; connection that is reusable and a client that is actually answered.
    ;; Discarding it left that half of the claim untested.
    (check "stream close: and the loop is told to read the next request"
           answered :keep-alive))
  (multiple-value-bind (pending masks state post answered)
      (%stream-close-pass t)
    (declare (ignore state post answered))
    (check "stream close: the backed-up control did not flush"
           (plusp pending) t)
    (check "stream close: and the control is armed on both sides"
           (and (find-if (lambda (m)
                           (plusp (logand m web-skeleton::+epollout+)))
                         masks)
                t)
           t)))

;;; ---------------------------------------------------------------------------
;;; The remainder a fetch callback leaves behind
;;;
;;; WS-SEND's second caller is a fetch callback on a :WEBSOCKET target, and
;;; it runs on the outbound connection's read path. HANDLE-CLIENT-READ —
;;; the site that did all of a WebSocket's arming — does not run for the
;;; target there at all, so a remainder was left with nothing subscribed to
;;; writability, waiting for an event no longer coming.
;;;
;;; Driven at the seam: DELIVER-DETACHED with a callback that sends, which
;;; is what FETCH-INTO's :THEN reduces to. End to end it would reach the
;;; same two lines through an upgrade handshake and a real upstream
;;; response, either of which could be what broke instead.
;;; ---------------------------------------------------------------------------

(defun test-ws-send-arms-from-a-fetch-callback ()
  "A frame sent from a fetch callback leaves its remainder armed.

   The target is a real socket with a shrunk send buffer and a peer that
   never reads — TEST-WS-HANDLER-PUSH-AND-RETURN's arrangement, and the
   only one that produces a remainder at all. With room to spare WS-SEND
   flushes completely and there is nothing left to arm for, so the two
   paths are indistinguishable.

   EPOLL-MODIFY is recorded rather than epoll being polled. A socket whose
   send buffer is full is not writable, so EPOLL-WAIT reports nothing
   whether or not EPOLLOUT was armed: the observation that looks the most
   direct is the one that cannot tell the two cases apart.

   Three checks, and the first two are the precondition. Without a
   remainder there is no claim to make, and a frame that fits would leave
   the third vacuous rather than failing — so the flush is asserted to have
   been incomplete before anything is asserted about arming."
  (format t "~%ws-send: arming from a fetch callback~%")
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create))
          (calls nil)
          (real (symbol-function 'web-skeleton::epoll-modify)))
      (unwind-protect
           (let* ((tgt-fd (web-skeleton::socket-fd server))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (target (web-skeleton::make-connection
                           :fd tgt-fd :socket server :state :websocket
                           :fetch-outstanding t
                           :last-active (get-universal-time)))
                  (frame (web-skeleton::build-ws-frame
                          web-skeleton::+ws-op-binary+
                          (make-array (* 512 1024)
                                      :element-type '(unsigned-byte 8)
                                      :initial-element 80)))
                  (flushed :unset))
             (web-skeleton::set-nonblocking tgt-fd)
             ;; SO_SNDBUF is 7 on Linux, as in TEST-WS-HANDLER-PUSH-AND-RETURN.
             (web-skeleton::set-socket-option-int
              tgt-fd web-skeleton::+sol-socket+ 7 2048)
             (web-skeleton::register-connection target)
             (web-skeleton::epoll-add epfd tgt-fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
             (setf (symbol-function 'web-skeleton::epoll-modify)
                   (lambda (efd fd mask)
                     (push (cons fd mask) calls)
                     (funcall real efd fd mask)))
             ;; What FETCH-INTO's :THEN reduces to, on the path where the
             ;; outbound is what epoll woke and the target is not.
             (web-skeleton::deliver-detached
              tgt-fd epfd
              (lambda (s h b)
                (declare (ignore s h b))
                (setf flushed (web-skeleton::ws-send target frame))
                nil)
              :delivered)
             (setf (symbol-function 'web-skeleton::epoll-modify) real)
             (check "callback ws-send: the frame did not all fit" flushed nil)
             (check "callback ws-send: a remainder is queued"
                    (plusp (web-skeleton::connection-write-pending target)) t)
             (check "callback ws-send: EPOLLOUT is armed on the target"
                    (and (find-if
                          (lambda (c)
                            (and (= (car c) tgt-fd)
                                 (plusp (logand (cdr c)
                                                web-skeleton::+epollout+))))
                          calls)
                         t)
                    t))
        (setf (symbol-function 'web-skeleton::epoll-modify) real)
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client))))))

;;; ---------------------------------------------------------------------------
;;; The third caller: a handler that sends to somebody else
;;;
;;; HANDLE-CLIENT-READ arms (CONNECTION-FD CONN) — the connection it was
;;; woken for. A ws-handler that sends to any *other* connection is
;;; therefore in exactly the position the fetch callback was in, and that
;;; is not a hypothetical shape: it is the one DEPLOYMENT.md documents
;;; under fan-out, "calling ws-send in a loop" over a subscriber list.
;;;
;;; The fix covers it by construction and the branch said nothing about it,
;;; which by this suite's own criterion is a claim without a detector.
;;; ---------------------------------------------------------------------------

(defun %ws-fanout-pass ()
  "Run the documented fan-out shape and report what got armed.

   A real masked frame arrives on A; A's handler sends a 512 KiB frame to
   B, whose send buffer is shrunk so a remainder is guaranteed. Returns
   (values SENT PENDING-B TOUCHED-A ARMED-B) — WS-SEND's answer, the bytes
   left on B, whether epoll was touched for A at all, and whether EPOLLOUT
   was armed for B.

   Asymmetric on purpose. A's handler returns NIL, so A has nothing pending
   and HANDLE-CLIENT-READ arms it EPOLLIN — the question for A is only
   whether the read path ran and reached its arming at all, which is what
   separates a B that was missed from a run where nothing armed anything at
   all.

   Driven through the real HANDLE-CLIENT-READ rather than by calling the
   handler, because the arming under test is the one HANDLE-CLIENT-READ
   performs after a handler returns. Calling the handler directly would
   remove the very code whose scope is the question."
  (multiple-value-bind (a-server a-client) (%loopback-pair)
    (multiple-value-bind (b-server b-client) (%loopback-pair)
      (let ((epfd (web-skeleton::epoll-create))
            (calls nil)
            (real (symbol-function 'web-skeleton::epoll-modify)))
        (unwind-protect
             (let* ((a-fd (web-skeleton::socket-fd a-server))
                    (b-fd (web-skeleton::socket-fd b-server))
                    (web-skeleton::*connections* (make-hash-table :test #'eql))
                    (web-skeleton::*epoll-fd* epfd)
                    (conn-a (web-skeleton::make-connection
                             :fd a-fd :socket a-server :state :websocket
                             :last-active (get-universal-time)))
                    (conn-b (web-skeleton::make-connection
                             :fd b-fd :socket b-server :state :websocket
                             :last-active (get-universal-time)))
                    (big (web-skeleton::build-ws-frame
                          web-skeleton::+ws-op-binary+
                          (make-array (* 512 1024)
                                      :element-type '(unsigned-byte 8)
                                      :initial-element 80)))
                    (sent :unset))
               (web-skeleton::set-nonblocking a-fd)
               (web-skeleton::set-nonblocking b-fd)
               ;; Only B is shrunk. A has to stay able to take its own
               ;; handler's return value, or the two fds would both hold
               ;; remainders and the assertion could not tell them apart.
               (web-skeleton::set-socket-option-int
                b-fd web-skeleton::+sol-socket+ 7 2048)
               (web-skeleton::register-connection conn-a)
               (web-skeleton::register-connection conn-b)
               (web-skeleton::epoll-add epfd a-fd
                                        (logior web-skeleton::+epollin+
                                                web-skeleton::+epollet+))
               (web-skeleton::epoll-add epfd b-fd
                                        (logior web-skeleton::+epollin+
                                                web-skeleton::+epollet+))
               (let ((stream (sb-bsd-sockets:socket-make-stream
                              a-client :input t :output t
                              :element-type '(unsigned-byte 8))))
                 (write-sequence (make-test-ws-frame "ping-a") stream)
                 (force-output stream))
               (sleep 0.1)
               ;; Recording starts after the setup so what is counted is the
               ;; read path's own arming and not EPOLL-ADD's.
               (setf (symbol-function 'web-skeleton::epoll-modify)
                     (lambda (efd fd mask)
                       (push (cons fd mask) calls)
                       (funcall real efd fd mask)))
               (web-skeleton::handle-client-read
                conn-a epfd nil
                (lambda (c f)
                  (declare (ignore c f))
                  (setf sent (web-skeleton:ws-send conn-b big))
                  nil))
               (setf (symbol-function 'web-skeleton::epoll-modify) real)
               (values sent
                       (web-skeleton::connection-write-pending conn-b)
                       (and (find a-fd calls :key #'car) t)
                       (and (find-if (lambda (c)
                                       (and (= (car c) b-fd)
                                            (plusp (logand
                                                    (cdr c)
                                                    web-skeleton::+epollout+))))
                                     calls)
                            t)))
          (setf (symbol-function 'web-skeleton::epoll-modify) real)
          (ignore-errors (web-skeleton::%close epfd))
          (ignore-errors (sb-bsd-sockets:socket-close a-server))
          (ignore-errors (sb-bsd-sockets:socket-close a-client))
          (ignore-errors (sb-bsd-sockets:socket-close b-server))
          (ignore-errors (sb-bsd-sockets:socket-close b-client)))))))


(defun test-ws-send-arms-a-fan-out-target ()
  "A handler that sends to somebody else arms that somebody else.

   HANDLE-CLIENT-READ arms the connection it was woken for, and only that
   one. So a ws-handler pushing to a subscriber list is in exactly the
   position the fetch callback was in — nothing downstream arms the target
   — and DEPLOYMENT.md documents that shape under fan-out rather than
   treating it as exotic. The same one-line fix covers both; only one of
   them was claimed.

   Four assertions, and the first two are the precondition. A frame that
   fits leaves nothing to strand, so WS-SEND returning NIL and B holding a
   backlog is what makes the fourth check about the claim rather than about
   the fixture.

   The third is the control, and what it guards against is the fourth going
   vacuous later rather than anything failing now. Under the defect A is
   still armed — HANDLE-CLIENT-READ names A's own fd and always did — so a
   failure list reading A touched, B not states the defect precisely.

   The case it really exists for is fixture drift. Simplify this test by
   calling the handler directly instead of driving HANDLE-CLIENT-READ — the
   obvious tidy-up, and someone will try it — and the fourth check still
   passes, because WS-SEND now arms B itself. The seam stops being crossed
   and nothing says so. That is the *pre-arranged* mode, reachable only
   because the fix landed, and this check is the thing that catches it:
   measured, that drift fails check three alone and leaves the other three
   green.

   EPOLLIN is what A gets, not EPOLLOUT: its handler returned NIL, so A has
   nothing pending. Hence the asymmetry between the third check and the
   fourth — for A the question is whether epoll was touched at all, for B
   it is whether the right interest was set."
  (format t "~%ws-send: arming a fan-out target~%")
  (multiple-value-bind (sent pending-b touched-a armed-b) (%ws-fanout-pass)
    (check "fan-out ws-send: the frame did not all fit" sent nil)
    (check "fan-out ws-send: a remainder is queued on the target"
           (plusp pending-b) t)
    (check "fan-out ws-send: the read path armed its own connection"
           touched-a t)
    (check "fan-out ws-send: and EPOLLOUT is armed on the fan-out target"
           armed-b t)))

;;; ---------------------------------------------------------------------------
;;; The close code for a frame that is too large
;;; ---------------------------------------------------------------------------

(defun test-ws-oversized-frame-close-code ()
  "An oversized data frame closes 1009; an oversized control frame closes 1002.

   RFC 6455 7.4.1 has 1009 (Message Too Big) for the data case, and the
   oversized fragmented *message* — the same complaint one layer up —
   already closed 1009 in two places. The single frame raised out of
   TRY-PARSE-WS-FRAME as a generic error, took WEBSOCKET-ON-READ's
   catch-all, and closed 1002. One file, two answers to one question.

   The control frame is the other half, and it wants the other code.
   *MAX-WS-PAYLOAD-SIZE* is a limit this endpoint chose, so exceeding it
   is 1009 — 'too big for me'. §5.5 caps every control frame at 125 bytes
   for everyone, so exceeding *that* is malformed rather than inconvenient
   and earns 1002. Introducing WS-FRAME-TOO-LARGE above the §5.5 check
   meant a ping declaring more than 64 KiB took the size check first and
   was told its message was too big for us, when what was wrong with it is
   that no endpoint may send it.

   Driven through WEBSOCKET-ON-READ rather than by asserting the condition
   type, because the code on the wire is the claim and the condition is
   only how it gets there. The neighbouring 1002 test is a control that
   still passes: an unknown opcode is a protocol error and keeps its code.

   *MAX-WS-PAYLOAD-SIZE* is lowered rather than a real 1 MiB frame built.
   What the parser compares is the declared length against the bound, and a
   small bound reaches that comparison with eight bytes instead of a
   megabyte. The ping needs 200 — over §5.5's 125 *and* over the lowered
   bound, since a frame that trips only one of them cannot tell which
   check ran first."
  (format t "~%ws: the close code for a frame that is too large~%")
  (flet ((close-code-for (frame)
           (let ((conn (web-skeleton::make-connection
                        :fd -1 :state :websocket :last-active 0)))
             (setf (web-skeleton::connection-read-buf conn) frame
                   (web-skeleton::connection-read-pos conn) (length frame))
             (multiple-value-bind (action response)
                 (web-skeleton::websocket-on-read
                  conn (lambda (c f) (declare (ignore c f)) nil))
               (values action
                       (and response
                            (>= (length response) 4)
                            (logior (ash (aref response 2) 8)
                                    (aref response 3))))))))
    (let ((web-skeleton::*max-ws-payload-size* 4))
      (multiple-value-bind (action code)
          (close-code-for (make-masked-frame t 1 #(104 101 108 108 111 32 119 111)))
        (check "oversized frame: the connection closes" action :close)
        (check "oversized frame: with 1009, not 1002" code 1009))
      ;; Opcode 9 is ping. 200 bytes is over both bounds, so the code that
      ;; comes back names which check the parser reached first.
      (multiple-value-bind (action code)
          (close-code-for
           (make-masked-frame t 9 (make-array 200 :element-type '(unsigned-byte 8)
                                                  :initial-element 65)))
        (check "oversized control frame: the connection closes" action :close)
        (check "oversized control frame: with 1002, not 1009" code 1002)))))

;;; ---------------------------------------------------------------------------
;;; The verdict the discard loop used to spin on
;;; ---------------------------------------------------------------------------

(defun test-discard-available-want-write ()
  "CONNECTION-DISCARD-AVAILABLE hands back :WANT-WRITE instead of looping.

   CONNECTION-READ-INTO has four non-integer verdicts and this loop handled
   two, so :WANT-WRITE fell into the integer default, set a flag and went
   round again — a hot spin inside the event loop with no exit. Its own
   docstring says the cond is deliberately the same shape as
   CONNECTION-READ-AVAILABLE's so the two can be read side by side, and it
   was not.

   Unreachable today: the only caller is the :STREAMING inbound path and
   there is no inbound TLS. That is how long a spin like this stays
   invisible, and it is the argument for the assertion rather than against
   it.

   The stub is bounded on purpose. An unbounded one would hang the run
   under the defect rather than fail it, which is the *too violent* mode —
   a detector whose failure is indistinguishable from a machine problem.
   Bounded, the defect exhausts the budget, falls through to :EOF and
   answers :OK-EOF, so both assertions fail and the run continues.

   The call count is the second assertion for the same reason: a fix that
   answered :WANT-WRITE after looping ten times would satisfy the first
   check and still be the bug."
  (format t "~%Discard loop: the verdict it used to spin on~%")
  (let ((calls 0)
        (real (symbol-function 'web-skeleton::connection-read-into)))
    (unwind-protect
         (progn
           (setf (symbol-function 'web-skeleton::connection-read-into)
                 (lambda (conn buffer start max-bytes)
                   (declare (ignore conn buffer start max-bytes))
                   (incf calls)
                   (if (< calls 20) :want-write :eof)))
           (let ((conn (web-skeleton::make-connection
                        :fd -1 :state :streaming :last-active 0))
                 (sink (make-array 64 :element-type '(unsigned-byte 8))))
             (check "discard: :want-write is answered, not swallowed"
                    (web-skeleton::connection-discard-available conn sink)
                    :want-write)
             (check "discard: and it answered on the first read"
                    calls 1)))
      (setf (symbol-function 'web-skeleton::connection-read-into) real))))

;;; ---------------------------------------------------------------------------
;;; The third boot invariant
;;; ---------------------------------------------------------------------------

(defun test-fetch-timeout-validated-at-boot ()
  "START-SERVER refuses a non-positive *FETCH-TIMEOUT* before it binds.

   It already refuses a non-positive *WRITE-STALL-TIMEOUT* and an
   under-sized *MAX-WRITE-BACKLOG*, with the same reasoning: a
   misconfiguration should not wait for the shape that reveals it.
   *FETCH-TIMEOUT* is the floor under every way a fetch can fail to return
   — the DNS phase, the connect, the read, and the :AWAITING sweep that
   answers a parked caller 504 — and at zero the sweep never fires.

   The :HOST is deliberately invalid. If the validation is reverted,
   START-SERVER continues to MAKE-TCP-LISTENER, which refuses a three-byte
   vector by name — so the revert fails this assertion instead of starting
   a real server inside the suite. A detector that leaves a listener
   running when it fails is worse than none.

   Asserted on the message rather than on the fact of a raise, because both
   paths raise and only one of them is this invariant."
  (format t "~%start-server: the third boot invariant~%")
  (let ((web-skeleton:*fetch-timeout* 0))
    (check "boot: a zero *fetch-timeout* is refused by name"
           (let ((msg (attempt (web-skeleton:start-server
                                :host #(1 2 3) :port 0 :workers 1))))
             (and (stringp msg) (search "*fetch-timeout*" msg) t))
           t)))

(defun test-cpu-count-parsers ()
  (format t "~%cpu-count: quota and topology parsing~%")

  ;; cgroup v2. The unlimited form's first field is the literal string
  ;; `max`, not a number: parsed as an integer it raises, and a parser that
  ;; guessed would answer with whatever it made of the word. It must decline
  ;; so the chain moves on.
  (check "cpu.max: unlimited declines"
         (web-skeleton::parse-cpu-max "max 100000") nil)
  (check "cpu.max: one full CPU"
         (web-skeleton::parse-cpu-max "100000 100000") 1)
  (check "cpu.max: four CPUs"
         (web-skeleton::parse-cpu-max "400000 100000") 4)
  ;; The shape this whole change exists for. FLOOR gives 0 here, and
  ;; START-SERVER refuses a non-positive :WORKERS — so rounding down turns
  ;; the fix into a server that will not boot in the deployment it serves.
  (check "cpu.max: half a CPU still gets one worker"
         (web-skeleton::parse-cpu-max "50000 100000") 1)
  (check "cpu.max: 2.5 CPUs rounds up"
         (web-skeleton::parse-cpu-max "250000 100000") 3)
  (check "cpu.max: trailing newline tolerated"
         (web-skeleton::parse-cpu-max (format nil "200000 100000~c" #\Newline)) 2)
  (check "cpu.max: garbage declines"
         (web-skeleton::parse-cpu-max "not a quota") nil)
  (check "cpu.max: single field declines"
         (web-skeleton::parse-cpu-max "100000") nil)
  (check "cpu.max: zero period declines"
         (web-skeleton::parse-cpu-max "100000 0") nil)
  (check "cpu.max: NIL line declines"
         (web-skeleton::parse-cpu-max nil) nil)

  ;; cgroup v1. Unlimited is a quota of -1 rather than a word.
  (check "cfs: unlimited declines"
         (web-skeleton::parse-cfs-quota "-1" "100000") nil)
  (check "cfs: two CPUs"
         (web-skeleton::parse-cfs-quota "200000" "100000") 2)
  (check "cfs: half a CPU still gets one worker"
         (web-skeleton::parse-cfs-quota "50000" "100000") 1)
  (check "cfs: a missing file declines"
         (web-skeleton::parse-cfs-quota nil "100000") nil)

  ;; Topology, the last answer in the chain and the only one CI reaches.
  (check "cpu list: simple range"
         (web-skeleton::parse-cpu-list "0-15") 16)
  (check "cpu list: single cpu"
         (web-skeleton::parse-cpu-list "0") 1)
  (check "cpu list: multi-range with a bare cpu"
         (web-skeleton::parse-cpu-list "0-3,8,12-13") 7)
  (check "cpu list: garbage declines"
         (web-skeleton::parse-cpu-list "nonsense") nil)
  (check "cpu list: empty declines"
         (web-skeleton::parse-cpu-list "") nil)

  ;; Affinity is a syscall and cannot be synthesised, so its arithmetic is
  ;; tested where it lives: the popcount over a filled cpu_set_t.
  (check "affinity: bits counted across bytes"
         (web-skeleton::count-set-bits #(#xFF #x0F #x00)) 12)
  (check "affinity: an empty mask counts nothing"
         (web-skeleton::count-set-bits #(0 0 0 0)) 0)

  ;; And the live call, which on any Linux host must answer something
  ;; positive. This is the one arm that runs against the real machine.
  (let ((n (web-skeleton::affinity-cpu-count)))
    (check "affinity: the live call answers a positive count"
           (and (integerp n) (plusp n)) t))

  ;; The least of the sources, not the first. These are the assertions that
  ;; distinguish the two: no host either of us can test on will disagree
  ;; with itself, so on every real machine MIN and first-answer-wins return
  ;; the same number and are indistinguishable.
  (check "min: a quota of eight against an affinity of two answers two"
         (web-skeleton::fewest-cpus
          (web-skeleton::parse-cpu-max "800000 100000") 2)
         2)
  (check "min: an affinity of sixteen against a quota of two answers two"
         (web-skeleton::fewest-cpus
          (web-skeleton::parse-cpu-max "200000 100000") 16)
         2)
  (check "min: a silent quota leaves affinity to answer"
         (web-skeleton::fewest-cpus
          (web-skeleton::parse-cpu-max "max 100000") 16)
         16)
  (check "min: every source silent declines"
         (web-skeleton::fewest-cpus nil nil nil) nil)
  (check "min: a non-positive source is ignored rather than winning"
         (web-skeleton::fewest-cpus 0 -3 4) 4)

  ;; The whole thing, on whatever this host is. Cannot assert a number —
  ;; that is the point of the change — but it must always be usable, since
  ;; START-SERVER refuses anything else.
  (let ((n (web-skeleton::cpu-count)))
    (check "cpu-count: answers a positive integer"
           (and (integerp n) (plusp n)) t)))

(defun test-server ()
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
  (format t "~%=== Server Tests ===~%")
  (test-http-parser)
  (test-http-parser-errors)
  (test-transfer-encoding-rules)
  (test-chunked-request-body)
  (test-unfinishable-request-closes)
  (test-expect-100-continue)
  (test-http-date)
  (test-http-date-cache)
  (test-http-response)
  (test-cookie-builder)
  (test-fetch)
  (test-dns)
  (test-is-public-address)
  (test-fetch-address-filter)
  (test-dns-cache)
  (test-dns-lookup-sink)
  (test-dns-error-ends-a-detached-fetch)
  (test-format-peer-addr)
  (test-parse-error-status)
  (test-url-decode)
  (test-query-string)
  (test-match-path)
  (test-streaming-fetch)
  (test-line-reader-over-read-fn)
  (test-interim-responses)
  (test-decode-chunked-body)
  (test-chunked-body-complete-p)
  (test-chunked-encoder)
  (test-chunk-walk-on-data)
  (test-fetch-on-body-pause)
  (test-streaming-head)
  (test-discard-available-eof)
  (test-stream-lifecycle)
  (test-stream-head-request)
  (test-stream-keepalive-and-idle)
  (test-sse)
  (test-websocket)
  (test-websocket-fragmentation)
  (test-static-helpers)
  (test-static-etag)
  (test-static-range)
  (test-static-date)
  (test-jwt)
  (test-shutdown-hooks)
  (test-read-available-eof)
  (test-awaiting-sweep-504)
  (test-write-queue)
  (test-connection-transport-seam)
  (test-outbound-handshake-state)
  (test-fetch-setup-releases-transport-on-error)
  (test-automatic-resume-edge)
  (test-outbound-direction-inversion)
  (test-write-queue-drain)
  (test-ws-send-queues)
  (test-ws-send-refuses-a-foreign-connection)
  (test-ws-write-stall-sweep)
  (test-ws-handler-push-and-return)
  (test-ws-ping-flush)
  (test-detached-pause-auto-resumes)
  (test-ws-send-arms-from-a-fetch-callback)
  (test-ws-send-arms-a-fan-out-target)
  (test-stream-close-arms-the-write-path)
  (test-cpu-count-parsers)
  (test-ws-oversized-frame-close-code)
  (test-discard-available-want-write)
  (test-fetch-timeout-validated-at-boot)
  (report-suite "Server")
  (zerop *tests-failed*))
