(in-package :web-skeleton)

;;; ===========================================================================
;;; HTTP Request Parser + Response Builder
;;;
;;; Pure logic — operates on strings/byte-arrays, does no I/O.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Limits
;;; ---------------------------------------------------------------------------

(defparameter *max-request-line-length* 8192
  "Maximum length of the HTTP request line in bytes.")

(defparameter *max-header-count* 100
  "Maximum number of headers allowed in a single request.")

(defparameter *max-header-line-length* 8192
  "Maximum length of a single header line in bytes.")

(defparameter *max-total-header-bytes* (* 64 1024)
  "Maximum cumulative header byte count per request, default 64 KiB.
   Individual lines are capped at *MAX-HEADER-LINE-LENGTH* (8 KiB)
   and the count at *MAX-HEADER-COUNT* (100), so the worst case
   without this cap is 800 KiB of header-string allocation per
   request. A running-total guard inside PARSE-HEADERS-BYTES rejects
   early once the cap is reached.")

(defparameter *max-body-size* (* 1 1024 1024)
  "Maximum request body size in bytes. Default 1MB.")

(defparameter *max-ws-payload-size* 65536
  "Maximum WebSocket frame payload size in bytes. Default 64KB.
   Applies to each individual frame on the read path — per-frame
   memory bound.")

(defparameter *max-ws-message-size* (* 1 1024 1024)
  "Maximum reassembled WebSocket message size in bytes. Default 1MB.
   Applies to the running total of a fragmented message (opcode
   TEXT/BINARY followed by one or more CONTINUATION frames, final
   fragment FIN=1). Separate from *MAX-WS-PAYLOAD-SIZE* so that
   fragmentation is actually useful at the application level —
   if the two caps were the same, a fragmented message could
   never carry more bytes than a single frame, which makes the
   whole fragmentation path pointless. Set higher for apps that
   stream large messages in fragments; the worst-case memory
   pressure is (MAX-CONNECTIONS * this-cap) bytes of in-flight
   reassembly buffers.")

;;; ---------------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------------

(define-condition http-parse-error (error)
  ((message :initarg :message :reader http-parse-error-message))
  (:report (lambda (condition stream)
             (format stream "HTTP parse error: ~a"
                     (http-parse-error-message condition)))))

(defun http-parse-error (format-string &rest args)
  "Signal an HTTP-PARSE-ERROR with a formatted message."
  (error 'http-parse-error
         :message (apply #'format nil format-string args)))

;;; ---------------------------------------------------------------------------
;;; HTTP request structure
;;; ---------------------------------------------------------------------------

(defstruct http-request
  (method   nil :type (or null keyword))    ; :GET, :POST, :PUT, etc.
  (path     ""  :type string)               ; "/fu/bar"
  (query    nil :type (or null string))     ; "a=1&b=2" or nil
  (version  "1.1" :type string)             ; "1.0" or "1.1"
  (headers  nil :type list)                 ; alist — ((name . value) ...)
  (body     nil :type (or null (simple-array (unsigned-byte 8) (*))))) ; raw body bytes or nil

;;; ---------------------------------------------------------------------------
;;; Header access
;;; ---------------------------------------------------------------------------

(defun get-header (request name)
  "Look up a header value by NAME (case-insensitive).
   Returns the value string, or NIL if not present."
  (cdr (assoc name (http-request-headers request)
              :test #'string-equal)))

(defun get-headers (request name)
  "Return a list of all values for headers matching NAME (case-insensitive).
   Handles the case where a client sends the same header multiple times."
  (loop for (n . v) in (http-request-headers request)
        when (string-equal n name) collect v))

(defun connection-header-has-token-p (header-value token)
  "Check if TOKEN appears in a comma-separated header value (RFC 7230 §3.2.6).
   Comparison is case-insensitive, tokens are trimmed of whitespace.
   Zero intermediate string allocation.

   Used for Transfer-Encoding: chunked, Connection: close/keep-alive,
   and Upgrade: websocket detection. The historical name survives
   from when this was websocket-specific; it is now a general HTTP
   token-list primitive, callable from any header-parsing site."
  (let ((len (length header-value))
        (token-len (length token)))
    (loop with pos = 0
          while (< pos len)
          do ;; Skip leading whitespace
             (loop while (and (< pos len)
                              (let ((ch (char header-value pos)))
                                (or (char= ch #\Space) (char= ch #\Tab))))
                   do (incf pos))
             ;; Find end of token (comma or end of string)
             (let ((end (or (position #\, header-value :start pos) len)))
               ;; Trim trailing whitespace
               (let ((trimmed-end end))
                 (loop while (and (> trimmed-end pos)
                                  (let ((ch (char header-value (1- trimmed-end))))
                                    (or (char= ch #\Space) (char= ch #\Tab))))
                       do (decf trimmed-end))
                 ;; Compare bounded region case-insensitively
                 (when (and (= (- trimmed-end pos) token-len)
                            (string-equal header-value token
                                         :start1 pos :end1 trimmed-end))
                   (return t))
                 ;; Move past comma
                 (setf pos (1+ end)))))))

(defun get-cookie (request name)
  "Extract the value of cookie NAME from the request's Cookie header.
   Returns the value string, or NIL if not found.
   Scans in-place — one allocation for the return value only.

   Parses the unquoted form only (cookie-pair = cookie-name \"=\"
   cookie-value per RFC 6265 §4.1.1). The spec's cookie-octet set
   excludes ';', '\"', and comma, so the quoted form (name=\"...\")
   is redundant — a spec-compliant quoted value can't contain any
   character that the unquoted parser would mishandle. Non-compliant
   servers (historically Tomcat and Jetty) that emit 'name=\"v;x\"'
   with literal semicolons inside quotes are unsupported by design —
   the framework does not condone non-compliant input."
  (let ((header (get-header request "cookie")))
    (when header
      (let ((name-len (length name))
            (len (length header))
            (pos 0))
        (loop
          ;; Skip whitespace after ;
          (loop while (and (< pos len)
                           (or (char= (char header pos) #\Space)
                               (char= (char header pos) #\Tab)))
                do (incf pos))
          (when (>= pos len) (return nil))
          ;; Check for name=
          (when (and (<= (+ pos name-len 1) len)
                     (string= header name :start1 pos :end1 (+ pos name-len))
                     (char= (char header (+ pos name-len)) #\=))
            (let* ((val-start (+ pos name-len 1))
                   (val-end (or (position #\; header :start val-start) len)))
              ;; Trim stray leading/trailing SP/TAB from the value.
              ;; RFC 6265 §4.1.1 cookie-octet excludes whitespace —
              ;; a spec-compliant value doesn't carry any — but
              ;; misbehaving proxies and test harnesses can inject
              ;; 'foo=bar ; baz=qux'. Silently including the trailing
              ;; space in the returned value is a footgun for apps
              ;; that compare via STRING=. Trim at read time.
              (return (string-trim '(#\Space #\Tab)
                                   (subseq header val-start val-end)))))
          ;; Skip to next pair
          (let ((semi (position #\; header :start pos)))
            (unless semi (return nil))
            (setf pos (1+ semi))))))))

;;; ---------------------------------------------------------------------------
;;; Set-Cookie builder (RFC 6265)
;;;
;;; Symmetric with GET-COOKIE. Kills the per-app HttpOnly / Secure /
;;; SameSite / Max-Age typo footgun — one misspelled attribute name in
;;; hand-rolled Set-Cookie string construction is a silent security
;;; regression nobody notices until audit time.
;;;
;;; Name and value are validated against CR, LF, and semicolon. Apps
;;; that need strict RFC 6265 §4.1.1 token validation can layer it on
;;; top; the framework only rejects characters that would break the
;;; header structure itself.
;;; ---------------------------------------------------------------------------

(defun validate-cookie-field (kind field)
  "Reject characters that would break the Set-Cookie header grammar
   or enable structural injection. CR / LF / NUL / ';' are rejected
   for every field. '=' is rejected for the cookie name only:
   browsers parse 'foo=bar=baz' as name='foo', value='bar=baz' per
   RFC 6265 §5.2, so accepting '=' in a caller-supplied name would
   silently rename the cookie to the substring before the first '='."
  (when (or (find #\Return field)
            (find #\Newline field)
            (find #\; field)
            (find (code-char 0) field))
    (error "build-cookie: ~a contains a forbidden character (NUL, CR, LF, or ';')"
           kind))
  (when (and (string= kind "name") (find #\= field))
    (error "build-cookie: name contains '=' — browsers would split ~
            the cookie at it")))

(defun build-cookie (name value &key (path "/") (http-only t) (secure t)
                                     (same-site :lax) max-age domain)
  "Build a Set-Cookie header value string.
   :SAME-SITE accepts :LAX (default), :STRICT, :NONE, or NIL (omit).
   :NONE requires :SECURE T — browsers reject the combination otherwise,
   and catching it here is friendlier than a silent client-side failure.
   :MAX-AGE is an integer (seconds) or NIL (session cookie, drops on
   browser close).
   :PATH defaults to '/'; :DOMAIN is omitted by default.
   Use ADD-RESPONSE-HEADER to attach the result — SET-RESPONSE-HEADER
   would replace a previously set cookie."
  (validate-cookie-field "name" name)
  (when (zerop (length name))
    (error "build-cookie: empty cookie name"))
  (validate-cookie-field "value" value)
  (when path (validate-cookie-field "path" path))
  (when domain (validate-cookie-field "domain" domain))
  (unless (or (null same-site)
              (member same-site '(:lax :strict :none)))
    (error "build-cookie: :same-site must be :lax, :strict, :none, or NIL"))
  (when (and (eq same-site :none) (not secure))
    (error "build-cookie: :same-site :none requires :secure t"))
  (with-output-to-string (out)
    (write-string name out)
    (write-char #\= out)
    (write-string value out)
    (when path
      (write-string "; Path=" out)
      (write-string path out))
    (when domain
      (write-string "; Domain=" out)
      (write-string domain out))
    (when max-age
      (format out "; Max-Age=~d" max-age))
    (when http-only
      (write-string "; HttpOnly" out))
    (when secure
      (write-string "; Secure" out))
    (when same-site
      (write-string "; SameSite=" out)
      (write-string (ecase same-site
                      (:lax    "Lax")
                      (:strict "Strict")
                      (:none   "None"))
                    out))))

(defun delete-cookie (name &key (path "/") domain)
  "Build a Set-Cookie header value that removes the cookie NAME.
   Empty value + Max-Age=0 so the browser drops it on receipt.
   :PATH and :DOMAIN must match how the cookie was originally set —
   browsers match cookies to Set-Cookie by (name, domain, path);
   HttpOnly / Secure / SameSite do not participate in matching."
  (validate-cookie-field "name" name)
  (when (zerop (length name))
    (error "delete-cookie: empty cookie name"))
  (when path (validate-cookie-field "path" path))
  (when domain (validate-cookie-field "domain" domain))
  (with-output-to-string (out)
    (write-string name out)
    (write-char #\= out)
    (when path
      (write-string "; Path=" out)
      (write-string path out))
    (when domain
      (write-string "; Domain=" out)
      (write-string domain out))
    (write-string "; Max-Age=0" out)))

;;; ---------------------------------------------------------------------------
;;; URL percent-decoding (RFC 3986)
;;; ---------------------------------------------------------------------------

(defun url-decode (string)
  "Decode percent-encoded characters in STRING (RFC 3986 §2.1).
   %XX sequences are replaced with the corresponding byte, decoded as UTF-8.
   Incomplete percent sequences (e.g. %2 at end of string) are passed through literally.
   '+' is passed through literally (this is path decoding, not form decoding)."
  (let* ((bytes (handler-case
                    (sb-ext:string-to-octets string :external-format :ascii)
                  (sb-int:character-encoding-error ()
                    (http-parse-error "non-ASCII character in URL"))))
         (len (length bytes))
         (out (make-array len :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop with i = 0
          while (< i len)
          do (let ((b (aref bytes i)))
               (if (and (= b 37)              ; '%'
                        (< (+ i 2) len))
                   (let ((hi (hex-digit-value (aref bytes (+ i 1))))
                         (lo (hex-digit-value (aref bytes (+ i 2)))))
                     (if (and hi lo)
                         (progn
                           (vector-push (logior (ash hi 4) lo) out)
                           (incf i 3))
                         (progn
                           (vector-push b out)
                           (incf i))))
                   (progn
                     (vector-push b out)
                     (incf i)))))
    (handler-case
        (sb-ext:octets-to-string (subseq out 0 (fill-pointer out))
                                  :external-format :utf-8)
      (sb-int:character-decoding-error ()
        (http-parse-error "invalid UTF-8 in percent-decoded value")))))

;;; ---------------------------------------------------------------------------
;;; Query string parsing
;;; ---------------------------------------------------------------------------

(defun form-decode (string)
  "Decode a form-encoded string (application/x-www-form-urlencoded).
   Like url-decode but + means space, per HTML spec §4.10.22.6."
  (url-decode (substitute #\Space #\+ string)))

(defun parse-query-string (query)
  "Parse a query string like \"a=1&b=2\" into an alist.
   Decodes per application/x-www-form-urlencoded (+ as space, %XX).
   Keys with no = get empty string values."
  (when (and query (> (length query) 0))
    (let ((pairs nil)
          (len (length query))
          (start 0))
      (loop
        (let* ((amp (or (position #\& query :start start) len))
               (segment-start start)
               (eq-pos (position #\= query :start segment-start :end amp)))
          (push (if eq-pos
                    (cons (form-decode (subseq query segment-start eq-pos))
                          (form-decode (subseq query (1+ eq-pos) amp)))
                    (cons (form-decode (subseq query segment-start amp)) ""))
                pairs)
          (if (>= amp len)
              (return (nreverse pairs))
              (setf start (1+ amp))))))))

(defun get-query-param (request name)
  "Look up query parameter NAME from the request's query string.
   Returns the decoded value string, or NIL if not present.
   Reparses the query string on each call; cache the result if calling repeatedly."
  (cdr (assoc name (parse-query-string (http-request-query request))
              :test #'string=)))

;;; ---------------------------------------------------------------------------
;;; Path matching
;;; ---------------------------------------------------------------------------

(defun split-path-segments (path)
  "Split \"/users/42\" into (\"users\" \"42\"). Leading slash is consumed."
  (let ((segments nil)
        (start (if (and (> (length path) 0) (char= (char path 0) #\/)) 1 0))
        (len (length path)))
    (loop
      (let ((slash (position #\/ path :start start)))
        (push (subseq path start (or slash len)) segments)
        (if slash
            (setf start (1+ slash))
            (return (nreverse segments)))))))

(defun match-path (pattern path)
  "Match PATH against PATTERN with :param segment captures.
   Literal segments must match exactly; segments starting with : capture.
   Empty segments are not captured (e.g. /users/ does not match /users/:id).
   Captured values are percent-decoded.

   Return contract is a deliberate tri-state:
     NIL      — PATH does not match PATTERN
     T        — PATH matches and PATTERN has no captures
     alist    — PATH matches and PATTERN has one or more captures

   Callers that write (cdr (assoc \"id\" (match-path ...))) must only
   do so for patterns they know carry a matching capture — the T
   branch is not an alist and will raise. The alternative (return
   () for capture-less match) would collapse 'matched without
   captures' into 'did not match' because () is NIL in Common Lisp,
   which is worse. Prefer a match-then-assoc idiom:
     (let ((b (match-path p path)))
       (when (and b (listp b)) (cdr (assoc \"id\" b))))"
  (let ((pat-segs  (split-path-segments pattern))
        (path-segs (split-path-segments path))
        (bindings nil))
    (when (= (length pat-segs) (length path-segs))
      (when (loop for pat in pat-segs
                  for seg in path-segs
                  always (if (and (> (length pat) 0)
                                  (char= (char pat 0) #\:))
                             (when (> (length seg) 0)
                               (push (cons (subseq pat 1) (url-decode seg))
                                     bindings)
                               t)
                             (string= pat (url-decode seg))))
        (if bindings (nreverse bindings) t)))))

;;; ---------------------------------------------------------------------------
;;; Byte-level scanning helpers
;;; ---------------------------------------------------------------------------

(declaim (inline scan-crlf))

(defun scan-crlf (buf start end)
  "Find next CRLF (13 10) in BUF[START..END). Returns position of CR, or NIL."
  (loop for i from start below (1- end)
        when (and (= (aref buf i) 13) (= (aref buf (1+ i)) 10))
        return i))

(defun scan-crlf-crlf (buf start end)
  "Find CRLFCRLF in BUF[START..END). Returns position of first CR, or NIL."
  (loop for i from start to (- end 4)
        when (and (= (aref buf i)       13)
                  (= (aref buf (+ i 1)) 10)
                  (= (aref buf (+ i 2)) 13)
                  (= (aref buf (+ i 3)) 10))
        return i))

;;; ---------------------------------------------------------------------------
;;; Byte-to-string conversion (allocation-minimal)
;;; ---------------------------------------------------------------------------

(defun bytes-to-string (buf start end)
  "Convert BUF[START..END) to a UTF-8 string. Single allocation."
  (handler-case
      (sb-ext:octets-to-string buf :start start :end end :external-format :utf-8)
    (sb-int:character-decoding-error ()
      (http-parse-error "invalid UTF-8 in request"))))

(defun bytes-to-lowercase-string (buf start end)
  "Convert BUF[START..END) to a lowercase ASCII string. Single allocation."
  (let ((str (make-string (- end start))))
    (loop for i from start below end
          for j from 0
          do (let ((b (aref buf i)))
               (setf (char str j)
                     (if (<= 65 b 90)       ; A-Z → a-z
                         (code-char (+ b 32))
                         (code-char b)))))
    str))

(defun trim-ows-bounds (buf start end)
  "Return (values trimmed-start trimmed-end) with leading/trailing OWS removed."
  (let ((s start) (e end))
    (loop while (and (< s e) (let ((b (aref buf s))) (or (= b 32) (= b 9))))
          do (incf s))
    (loop while (and (> e s) (let ((b (aref buf (1- e)))) (or (= b 32) (= b 9))))
          do (decf e))
    (values s e)))

;;; ---------------------------------------------------------------------------
;;; Byte-level HTTP method matching
;;; ---------------------------------------------------------------------------

(defun match-method-bytes (buf start end)
  "Match BUF[START..END) against known HTTP methods. Returns keyword or NIL.
   TRACE is intentionally unsupported: it echoes the request back in
   the response body (RFC 7231 §4.3.8), which is the classic
   cross-site-tracing (XST) sink for leaking cookies and auth headers
   through a compromised client-side script. Rejecting it at the
   parser layer means downstream handlers can never see it."
  (flet ((match-p (str)
           (let ((len (length str)))
             (and (= (- end start) len)
                  (loop for i from 0 below len
                        always (= (aref buf (+ start i))
                                  (char-code (char str i))))))))
    (cond
      ((match-p "GET")     :GET)
      ((match-p "POST")    :POST)
      ((match-p "PUT")     :PUT)
      ((match-p "DELETE")  :DELETE)
      ((match-p "HEAD")    :HEAD)
      ((match-p "OPTIONS") :OPTIONS)
      ((match-p "PATCH")   :PATCH))))

;;; ---------------------------------------------------------------------------
;;; RFC 7230 §3.2.6 tchar table
;;;
;;; One shared lookup used by both the inbound parser (header name
;;; validation) and the outbound serializer (header name validation
;;; before we copy bytes to the wire). Keeping the acceptance set
;;; symmetric on both edges means an app can never build a response
;;; header the framework would reject if it came back in as a request.
;;;
;;; tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "."
;;;       / "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
;;; ---------------------------------------------------------------------------

(declaim (type (simple-array bit (256)) *tchar-table*))
(defvar *tchar-table*
  (let ((tbl (make-array 256 :element-type 'bit :initial-element 0)))
    (loop for c from (char-code #\0) to (char-code #\9)
          do (setf (aref tbl c) 1))
    (loop for c from (char-code #\A) to (char-code #\Z)
          do (setf (aref tbl c) 1))
    (loop for c from (char-code #\a) to (char-code #\z)
          do (setf (aref tbl c) 1))
    (loop for ch across "!#$%&'*+-.^_`|~"
          do (setf (aref tbl (char-code ch)) 1))
    tbl)
  "256-entry bit table: 1 iff the byte index is a valid RFC 7230
   §3.2.6 token character. Used by parse-headers-bytes on ingress
   and serialize-http-message on egress.")

(declaim (inline tchar-byte-p))
(defun tchar-byte-p (byte)
  "T if BYTE is an RFC 7230 §3.2.6 tchar. BYTE must be in 0..255."
  (= 1 (aref *tchar-table* byte)))

;;; ---------------------------------------------------------------------------
;;; Byte-level header parser (single-pass)
;;; ---------------------------------------------------------------------------

(defun parse-headers-bytes (buf start end)
  "Parse headers from BUF[START..END) into an alist of (lowercase-name . value).
   Single-pass, handles obsolete line folding (RFC 7230 §3.2.4).
   Stops at the first empty line (CRLFCRLF boundary). Enforces
   *MAX-TOTAL-HEADER-BYTES* as a running-total guard — per-line and
   per-count caps alone permit worst-case 800 KiB of header-string
   allocation per request; the running total makes the actual bound
   match *MAX-TOTAL-HEADER-BYTES* (default 64 KiB)."
  (let ((headers nil)
        (count 0)
        (total 0)
        (pos start))
    (loop
      (let ((crlf (scan-crlf buf pos end)))
        (unless crlf (return))          ; no more complete lines
        (when (= crlf pos) (return))    ; empty line = end of headers
        (let ((line-len (- crlf pos)))
          (when (> line-len *max-header-line-length*)
            (http-parse-error "header line too long (~d bytes, max ~d)"
                              line-len *max-header-line-length*))
          (incf total line-len)
          (when (> total *max-total-header-bytes*)
            (http-parse-error "total header bytes exceed ~d"
                              *max-total-header-bytes*))
          (let ((first-byte (aref buf pos)))
            (if (and headers (or (= first-byte 32) (= first-byte 9)))
                ;; RFC 7230 §3.2.4: reject obsolete line folding
                (http-parse-error "obsolete line folding not accepted")
                ;; New header — find colon
                (let ((colon (position 58 buf :start pos :end crlf))) ; 58 = ':'
                  (unless colon
                    (http-parse-error "malformed header (no colon)"))
                  (when (= colon pos)
                    (http-parse-error "empty header name"))
                  ;; RFC 7230 §3.2.6: header name must be a token,
                  ;; i.e. each byte must be a tchar. The earlier
                  ;; loose check (<=0x20 or =0x7F) rejected CTLs and
                  ;; SP but let through separators like ':', '(',
                  ;; ',', '/', etc. — any of which hidden in a name
                  ;; is a smuggling primitive if it reaches assoc.
                  (loop for i from pos below colon
                        for b = (aref buf i)
                        unless (tchar-byte-p b)
                        do (http-parse-error "invalid byte in header name"))
                  (let ((name (bytes-to-lowercase-string buf pos colon)))
                    (multiple-value-bind (vs ve)
                        (trim-ows-bounds buf (1+ colon) crlf)
                      ;; RFC 7230 §3.2: field-value excludes CTLs
                      ;; except HTAB. scan-crlf only terminates on
                      ;; the CRLF pair, so a bare CR, bare LF, NUL,
                      ;; or other CTL embedded in a value survives
                      ;; into the alist — and from there into any
                      ;; ~a-interpolated log line. Same log-injection
                      ;; shape the request-target and method checks
                      ;; already close in parse-request-bytes, and
                      ;; symmetric with serialize-http-message's
                      ;; value reject on egress. The asymmetry was
                      ;; the bug — inbound accepted bytes the
                      ;; outbound side would refuse to emit.
                      (loop for i from vs below ve
                            for b = (aref buf i)
                            when (or (and (< b #x20) (/= b 9))
                                     (= b #x7F))
                            do (http-parse-error
                                "invalid byte 0x~2,'0x in header value" b))
                      ;; Check the count before pushing so the over-
                      ;; budget header is not allocated into the alist
                      ;; before we reject. Matches the pre-check
                      ;; pattern used elsewhere (e.g. JSON depth).
                      (incf count)
                      (when (> count *max-header-count*)
                        (http-parse-error "too many headers (~d, max ~d)"
                                          count *max-header-count*))
                      (push (cons name (if (= vs ve) ""
                                           (bytes-to-string buf vs ve)))
                            headers)))))))
        (setf pos (+ crlf 2))))
    (nreverse headers)))

;;; ---------------------------------------------------------------------------
;;; Byte-level request parser
;;; ---------------------------------------------------------------------------

(defun parse-request-bytes (buf start end)
  "Parse HTTP request directly from bytes BUF[START..END).
   END should be past the CRLFCRLF terminator.
   Returns an HTTP-REQUEST. Body is not extracted."
  (let ((req-end (scan-crlf buf start end)))
    (unless req-end
      (http-parse-error "incomplete request (no CRLF in request line)"))
    (let ((req-line-len (- req-end start)))
      (when (zerop req-line-len)
        (http-parse-error "empty request line"))
      (when (> req-line-len *max-request-line-length*)
        (http-parse-error "request line too long (~d bytes, max ~d)"
                          req-line-len *max-request-line-length*)))
    ;; Parse: METHOD SP URI SP VERSION
    (let ((sp1 (position 32 buf :start start :end req-end)))  ; 32 = space
      (unless sp1
        (http-parse-error "malformed request line"))
      (let ((sp2 (position 32 buf :start (1+ sp1) :end req-end)))
        (unless sp2
          (http-parse-error "malformed request line"))
        (when (position 32 buf :start (1+ sp2) :end req-end)
          (http-parse-error "malformed request line (extra spaces)"))
        ;; Reject CTL bytes (0x00-0x1F, 0x7F) in the method region.
        ;; Same discipline as the request-target check below: scan-crlf
        ;; only matches the CRLF pair, so a bare CR or LF smuggled
        ;; between start and the first space survives into
        ;; bytes-to-string and lands in the 'unrecognized method: ~a'
        ;; error text, which log-warn then splits across two lines —
        ;; a log-injection primitive identical in shape to the
        ;; request-target vector.
        (loop for i from start below sp1
              for b = (aref buf i)
              when (or (< b #x20) (= b #x7F))
              do (http-parse-error "control character in request method"))
        ;; Method
        (let ((method (match-method-bytes buf start sp1)))
          (unless method
            (http-parse-error "unrecognized method: ~a"
                              (bytes-to-string buf start sp1)))
          ;; Version — match "HTTP/1.0" or "HTTP/1.1" byte-by-byte
          (let* ((ver-start (1+ sp2))
                 (ver-len (- req-end ver-start))
                 (version
                   (when (and (= ver-len 8)
                              (= (aref buf ver-start)       72)  ; H
                              (= (aref buf (+ ver-start 1)) 84)  ; T
                              (= (aref buf (+ ver-start 2)) 84)  ; T
                              (= (aref buf (+ ver-start 3)) 80)  ; P
                              (= (aref buf (+ ver-start 4)) 47)  ; /
                              (= (aref buf (+ ver-start 5)) 49)  ; 1
                              (= (aref buf (+ ver-start 6)) 46)) ; .
                     (case (aref buf (+ ver-start 7))
                       (49 "1.1")    ; '1'
                       (48 "1.0"))))) ; '0'
            (unless version
              (http-parse-error "unsupported HTTP version"))
            ;; URI → path + query
            (let* ((uri-start (1+ sp1))
                   (qmark (position 63 buf :start uri-start :end sp2))  ; 63 = '?'
                   (path-end (or qmark sp2)))
              (when (= uri-start path-end)
                (http-parse-error "empty request path"))
              (unless (= (aref buf uri-start) 47)  ; 47 = '/'
                (http-parse-error "request path must start with /"))
              ;; Reject CTL bytes (0x00-0x1F, 0x7F) in the request-target
              ;; region (RFC 7230 §3.2.6: request-target uses pchar, which
              ;; excludes controls). Without this check a bare CR or LF
              ;; smuggled into the URL survives scan-crlf (which only
              ;; matches the CRLF pair) and ends up in the parsed PATH
              ;; string, giving an attacker a log-injection primitive via
              ;; any ~a-interpolated log call that names the path.
              ;; Non-ASCII bytes (>= 0x80) rejected at parse time too.
              ;; RFC 3986 §2.1 requires non-ASCII characters in URLs to
              ;; be percent-encoded — raw UTF-8 bytes are non-compliant
              ;; from a spec-adhering client. Without this check the
              ;; bytes survive into the parsed PATH/QUERY strings; a
              ;; later GET-QUERY-PARAM call trips URL-DECODE's
              ;; :external-format :ascii conversion and the handler
              ;; answers 400 at dispatch time with a misleading
              ;; "parse error" log line. Rejecting at parse-time keeps
              ;; the parser's contract honest and the error path
              ;; consistent with the outbound PARSE-URL check.
              (loop for i from uri-start below sp2
                    for b = (aref buf i)
                    when (or (< b #x20) (= b #x7F))
                    do (http-parse-error "control character in request-target")
                    when (>= b #x80)
                    do (http-parse-error "non-ASCII byte in request-target"))
              (let ((path (bytes-to-string buf uri-start path-end))
                    (query (when qmark
                             (bytes-to-string buf (1+ qmark) sp2))))
                (let ((headers (parse-headers-bytes buf (+ req-end 2) end)))
                  (make-http-request
                   :method method
                   :path path
                   :query query
                   :version version
                   :headers headers))))))))))

;;; ---------------------------------------------------------------------------
;;; String-level convenience interface
;;; ---------------------------------------------------------------------------

(defun parse-request (raw-data)
  "Parse a raw HTTP request header string into an HTTP-REQUEST struct.
   Convenience wrapper — converts to bytes and calls the byte-level parser."
  (let* ((bytes (sb-ext:string-to-octets raw-data :external-format :utf-8))
         (end (scan-crlf-crlf bytes 0 (length bytes))))
    (unless end
      (http-parse-error "incomplete headers (no CRLFCRLF terminator)"))
    (parse-request-bytes bytes 0 (+ end 4))))

;;; ===========================================================================
;;; HTTP Response Builder
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Status codes
;;; ---------------------------------------------------------------------------

(defparameter *status-reasons*
  '((100 . "Continue")
    (101 . "Switching Protocols")
    (200 . "OK")
    (201 . "Created")
    (204 . "No Content")
    (206 . "Partial Content")
    (301 . "Moved Permanently")
    (302 . "Found")
    (304 . "Not Modified")
    (307 . "Temporary Redirect")
    (308 . "Permanent Redirect")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (408 . "Request Timeout")
    (409 . "Conflict")
    (413 . "Payload Too Large")
    (414 . "URI Too Long")
    (417 . "Expectation Failed")
    (429 . "Too Many Requests")
    (431 . "Request Header Fields Too Large")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Timeout"))
  "Map of HTTP status codes to reason phrases.")

(defun status-reason (code)
  "Return the reason phrase for a status CODE, or \"Unknown\"."
  (or (cdr (assoc code *status-reasons*)) "Unknown"))

;;; ---------------------------------------------------------------------------
;;; Response structure
;;; ---------------------------------------------------------------------------

(defstruct http-response
  (status  200   :type integer)
  (headers nil   :type list)      ; alist — ((name . value) ...)
  (body    nil   :type (or null string)))

;;; ---------------------------------------------------------------------------
;;; HTTP message serialization (shared by response builder, static, and fetch)
;;; ---------------------------------------------------------------------------

(defun serialize-http-message (first-line headers body-bytes)
  "Serialize an HTTP message into a byte vector ready to write to a socket.
   FIRST-LINE: status line or request line string (without CRLF).
   HEADERS: alist of (name . value) string pairs.
   BODY-BYTES: byte vector or NIL.

   All strings are UTF-8 encoded to bytes before validation and
   write. A naive (char-code (char str i)) → (unsigned-byte 8)
   write would truncate char codes > 255 and crash on codes > 127
   whose UTF-8 form sets a high byte — an app that stuffs a UTF-8
   status reason or accidental non-ASCII header value into a
   response deserves a pointed content error, not a type-error
   from SETF AREF deep inside the serializer. Encoding up front
   means the per-byte checks below operate on the wire form, not
   the source string form.

   Header names are validated against the RFC 7230 §3.2.6 tchar
   table — the same table parse-headers-bytes uses on ingress.
   First-line and header-value bytes reject CTLs (0x00-0x1F, 0x7F)
   except HTAB (0x09, permitted in field values per RFC 7230 §3.2.6).
   Single pre-sized buffer — no intermediate allocations during write."
  (let* ((first-line-bytes (sb-ext:string-to-octets
                            first-line :external-format :utf-8))
         ;; Encode each header once; reuse byte vectors for sizing
         ;; and for the final copy. Consing one cons cell per header
         ;; is cheaper than encoding twice.
         (header-byte-specs
          (loop for (name . value) in headers
                collect (cons (sb-ext:string-to-octets
                               name :external-format :utf-8)
                              (sb-ext:string-to-octets
                               value :external-format :utf-8))))
         (header-size (+ (length first-line-bytes) 2   ; first line + CRLF
                         (loop for (nb . vb) in header-byte-specs
                               sum (+ (length nb) 2 (length vb) 2))
                         2))                            ; final CRLF
         (body-len (if body-bytes (length body-bytes) 0))
         (buf (make-array (+ header-size body-len)
                          :element-type '(unsigned-byte 8)))
         (pos 0))
    (labels ((reject-ctl-bytes (bytes where)
               ;; Reject CTLs (<0x20, =0x7F) except HTAB. HTAB is
               ;; legal inside field values (RFC 7230 §3.2.6); it
               ;; is not meaningful in a first line but is also
               ;; not a smuggling vector, and allowing it here
               ;; keeps the same primitive usable for both callers.
               (loop for i from 0 below (length bytes)
                     for b = (aref bytes i)
                     when (or (and (< b #x20) (/= b 9)) (= b #x7F))
                     do (error "HTTP ~a contains control byte 0x~2,'0x"
                               where b)))
             (put-bytes (src)
               (replace buf src :start1 pos)
               (incf pos (length src)))
             (put-crlf ()
               (setf (aref buf pos) 13 (aref buf (1+ pos)) 10)
               (incf pos 2)))
      (reject-ctl-bytes first-line-bytes "first line")
      (put-bytes first-line-bytes)
      (put-crlf)
      (dolist (spec header-byte-specs)
        (let ((name-bytes (car spec))
              (value-bytes (cdr spec)))
          (when (zerop (length name-bytes))
            (error "HTTP header has empty name"))
          ;; tchar on every byte of the name. UTF-8 encoding a
          ;; non-ASCII character produces bytes >= 0x80, all of
          ;; which fail tchar-byte-p — so a header name like
          ;; "X-Résumé" is rejected here cleanly instead of
          ;; emitting broken bytes on the wire.
          (loop for i from 0 below (length name-bytes)
                for b = (aref name-bytes i)
                unless (tchar-byte-p b)
                do (error "HTTP header name has invalid byte 0x~2,'0x" b))
          (reject-ctl-bytes value-bytes "header value")
          (put-bytes name-bytes)
          (setf (aref buf pos) 58 (aref buf (1+ pos)) 32)  ; ": "
          (incf pos 2)
          (put-bytes value-bytes)
          (put-crlf)))
      (put-crlf)
      (when body-bytes
        (replace buf body-bytes :start1 pos)))
    buf))

;;; ---------------------------------------------------------------------------
;;; Response building helpers
;;; ---------------------------------------------------------------------------

(defun set-response-header (response name value)
  "Set a header on RESPONSE. Replaces any existing header with the
   same name, compared case-insensitively per RFC 7230 §3.2. Apps
   that build responses with mixed-case header literals
   ('Content-Type') still get clean replacement instead of
   duplication — duplicate Content-Length or Content-Type is a
   response-smuggling primitive when a caching proxy is in front."
  (let ((key (string-downcase name)))
    (setf (http-response-headers response)
          (cons (cons key value)
                (remove key (http-response-headers response)
                        :key #'car :test #'string-equal))))
  response)

(defun add-response-header (response name value)
  "Add a header to RESPONSE without removing existing headers with the
   same name. Required for multi-instance headers like Set-Cookie —
   RFC 6265 §4.1 forbids comma-folding Set-Cookie, so each cookie
   must be a separate header. SET-RESPONSE-HEADER replaces; this appends."
  (push (cons (string-downcase name) value)
        (http-response-headers response))
  response)

(defun http-date (&optional (universal-time (get-universal-time)))
  "Return UTC time in RFC 7231 IMF-fixdate format.
   Defaults to current time if no argument given."
  (multiple-value-bind (sec min hour day month year dow)
      (decode-universal-time universal-time 0)
    (format nil "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d GMT"
            (nth dow '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            day
            (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                              "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            year hour min sec)))

(defun format-response (response &key connection-hint head-only-p)
  "Serialize an HTTP-RESPONSE into a byte vector ready to write to a socket.

   CONNECTION-HINT stamps a Connection header at serialize time:
     :CLOSE      — stamps 'Connection: close' when the server has
                   decided to close the socket after this response.
                   RFC 7230 §6.1 says a sender that wishes to close
                   SHOULD send the close option in the final message.
     :KEEP-ALIVE — stamps 'Connection: keep-alive' for an HTTP/1.0
                   client that negotiated keep-alive (HTTP/1.1's
                   default is keep-alive, so the header would be
                   redundant there).
     NIL         — do not stamp; caller didn't care or the default
                   framing is sufficient.
   The stamping happens in the emitted bytes only — never on the
   RESPONSE struct — so a cached struct reused across requests does
   not accumulate state. A caller-set Connection header always
   takes precedence; the hint is ignored when one is present.

   HEAD-ONLY-P skips the body in the emitted bytes while preserving
   the Content-Length header computed from the body's byte length
   (RFC 7231 §4.3.2). Without this, a HEAD response to a handler
   that returned a 10 MiB body would allocate the body into the
   serialization buffer only for STRIP-BODY-FOR-HEAD to truncate it
   back out — two large allocations for the cold HEAD path.
   Passing :HEAD-ONLY-P T encodes the body once for length, then
   skips emission. The byte-vector path (static files) still goes
   through STRIP-BODY-FOR-HEAD post-serialize."
  (let ((status (http-response-status response)))
    ;; Validate status up front so an out-of-range value short-
    ;; circuits before the body encode + header build below.
    (unless (<= 100 status 599)
      (error "HTTP status ~d out of range (must be 100-599)" status))
  (let* ((body   (http-response-body response))
         (body-bytes (when body
                       (sb-ext:string-to-octets body :external-format :utf-8)))
         (headers (http-response-headers response))
         (headers (cond
                   ;; Body present — add CL if not already set
                   ((and body-bytes
                         (not (assoc "content-length" headers
                                     :test #'string-equal)))
                    (cons (cons "content-length"
                                (write-to-string (length body-bytes)))
                          headers))
                   ;; No body, status requires CL:0 to prevent
                   ;; keep-alive clients from waiting forever
                   ((and (null body-bytes)
                         (not (or (<= 100 status 199)
                                  (= status 204) (= status 304)))
                         (not (assoc "content-length" headers
                                     :test #'string-equal)))
                    (cons (cons "content-length" "0") headers))
                   (t headers)))
         ;; Stamp the Connection header from the hint without touching
         ;; the caller's struct. App-set Connection header wins — if
         ;; the handler already put one in the alist, the hint is
         ;; ignored. :CLOSE maps to 'close' (RFC 7230 §6.1 SHOULD on
         ;; server-initiated close); :KEEP-ALIVE maps to 'keep-alive'
         ;; (only meaningful for HTTP/1.0 — 1.1 defaults to keep-alive
         ;; so the header would be redundant); NIL leaves framing to
         ;; the default for the request's HTTP version.
         (headers (if (and connection-hint
                           (not (assoc "connection" headers
                                       :test #'string-equal)))
                      (cons (cons "connection"
                                  (ecase connection-hint
                                    (:close      "close")
                                    (:keep-alive "keep-alive")))
                            headers)
                      headers))
         ;; RFC 7231 §7.1.1.2: origin server MUST send Date
         (headers (if (assoc "date" headers :test #'string-equal)
                      headers
                      (cons (cons "date" (http-date)) headers))))
    (serialize-http-message
     (format nil "HTTP/1.1 ~d ~a" status (status-reason status))
     headers
     ;; HEAD short-circuit: keep the computed Content-Length in the
     ;; headers alist (RFC 7231 §4.3.2 requires matching the GET
     ;; response's CL) but omit the body bytes from the emitted
     ;; serialization. Equivalent to STRIP-BODY-FOR-HEAD post-pass
     ;; without the large subseq allocation.
     (if head-only-p nil body-bytes)))))

;;; ---------------------------------------------------------------------------
;;; Convenience constructors
;;; ---------------------------------------------------------------------------

(defun make-text-response (status body &key (content-type "text/plain; charset=utf-8"))
  "Build a response with a text body."
  (let ((resp (make-http-response :status status :body body)))
    (set-response-header resp "content-type" content-type)
    resp))

(defun make-html-response (status body)
  "Build a response with an HTML body."
  (make-text-response status body :content-type "text/html; charset=utf-8"))

(defun make-error-response (status &optional message)
  "Build a plain-text error response."
  (let ((body (or message
                  (format nil "~d ~a" status (status-reason status)))))
    (make-text-response status body)))
