(in-package :web-skeleton)

;;; ===========================================================================
;;; HTTP Client (outbound fetch)
;;;
;;; Integrates with the event loop — outbound connections are registered
;;; with the same epoll fd and processed alongside inbound connections.
;;; Non-blocking I/O after connection. DNS resolution (get-host-by-name)
;;; and HTTPS (via TLS hook) block the worker thread.
;;;
;;; Usage from a handler:
;;;   (http-fetch :get "http://host/path"
;;;               :then (lambda (status headers body)
;;;                       (make-text-response 200 body)))
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Fetch request descriptor (returned by handler)
;;; ---------------------------------------------------------------------------

(defstruct http-fetch-continuation
  "Descriptor for an outbound HTTP request. Returned by http-fetch."
  (method   :GET   :type keyword)
  (url      ""     :type string)
  (headers  nil    :type list)
  (body     nil)
  (callback nil    :type function)
  ;; (BYTES) called per chunk as the response arrives, on the async
  ;; http:// path. NIL buffers the whole body as before.
  (on-body  nil    :type (or null function)))

(defparameter *fetch-timeout* 30
  "Seconds, per phase — not a total, except on the async http:// path
   where the :awaiting reap does bound the whole exchange. Elsewhere it
   bounds DNS, connect, and each individual socket read separately, so a
   trickling upstream never trips it. README Limitations has what that
   costs.")

;;; ---------------------------------------------------------------------------
;;; Outbound address policy (SSRF)
;;; ---------------------------------------------------------------------------

(defvar *fetch-address-filter* nil
  "Optional policy hook consulted for every address the outbound fetch
   machinery is about to dial. A function (IP FAMILY HOST) returning a
   generalized boolean: IP is the 4- or 16-byte address vector, FAMILY is
   :INET or :INET6, HOST is the hostname or IP literal from the URL.
   Returning NIL refuses the address.

   NIL — the default — allows every address. That is the historical
   behavior and the right one when fetch URLs come from config rather
   than from users (a fixed upstream, a self-fetch to 127.0.0.1).

   Set it when an app builds fetch URLs from user input. The framework
   resolves hostnames itself, so an app that resolves a name, approves
   the address, and then hands the *name* to DEFER-TO-FETCH is racing a
   second, independent resolution: an attacker's nameserver answers with
   a public address on the first lookup and 169.254.169.254 on the
   second (DNS rebinding). This hook runs on the resolution that is
   actually dialed, which is the only place that race can be closed. It
   also gates the IP-literal fast paths, so http://169.254.169.254/ is
   refused by the same policy — those skip DNS entirely and would
   otherwise walk straight past a resolver-only check.

   IS-PUBLIC-ADDRESS-P is the intended companion:

     (setf *fetch-address-filter*
           (lambda (ip family host)
             (declare (ignore host))
             (is-public-address-p ip family)))

   A refused address is skipped, not fatal: a hostname with several
   addresses falls through to the next one, and a lookup where none
   survive fails the fetch exactly as an unresolvable name does — 502 to
   the inbound caller, with the fetch callback firing its (NIL NIL NIL)
   cleanup sentinel exactly once. The filter runs on the worker thread
   inside the resolve path, so keep it cheap and non-blocking.")

(defun fetch-address-allowed-p (ip family host)
  "Gate IP (byte vector) / FAMILY (:INET or :INET6) / HOST (the name or
   literal from the URL) through *FETCH-ADDRESS-FILTER*. Returns T when
   no filter is installed or the filter accepts, NIL when it refuses.
   Refusals log at WARN — a blocked fetch is a policy event an operator
   wants to see, and a silent NIL would surface only as a puzzling 502.

   A raising filter refuses the address rather than propagating: a bug in
   app-supplied policy must fail closed (no dial) rather than fail open,
   and must not take the worker down either. The raise is logged at ERROR
   so the bug is not silently absorbed."
  (let ((filter *fetch-address-filter*))
    (cond
      ((null filter) t)
      ((handler-case (funcall filter ip family host)
         (error (e)
           (log-error "fetch: address filter raised on ~a (host ~a): ~a ~
                       — refusing address"
                      (format-ip ip) host e)
           nil))
       t)
      (t
       (log-warn "fetch: address ~a refused by *fetch-address-filter* (host ~a)"
                 (format-ip ip) host)
       nil))))

(defparameter *max-outbound-response-size* (* 8 1024 1024)
  "Maximum total bytes (headers + body together) for a buffered
   outbound HTTPS response read by TLS-READ-ALL. Default 8 MiB.
   Distinct from *MAX-BODY-SIZE*, which caps inbound request
   bodies — applying that 1 MiB inbound limit to outbound responses
   would reject legitimate 1 MiB HTTPS responses on principle.")

(defparameter *max-streaming-line-size* (* 1 1024 1024)
  "Maximum size of a single line in a streamed response (NDJSON,
   SSE, chunked text). Default 1 MiB. Applied by the streaming
   readers in FETCH-STREAM-PLAIN (via READER-READ-LINE /
   READER-READ-BYTES) and TLS-STREAM-RESPONSE. Distinct from
   *MAX-BODY-SIZE* (the inbound request-body cap) so tightening
   one does not move the other — an app tuning its request
   hardening should not incidentally break its NDJSON client.
   Distinct from *MAX-OUTBOUND-RESPONSE-SIZE* too: this cap is
   per-line, not per-response, because the streaming readers
   operate on one line at a time and never buffer the whole
   response.")

(defun http-fetch (method url &key headers body then on-body)
  "Create an outbound HTTP request descriptor.
   Return this from a handler to initiate a non-blocking outbound call.
   THEN is called with (status headers body) when the response arrives;
   it must return an HTTP response, byte vector, or another http-fetch-continuation.

   ON-BODY, if given, is called as (CONN BYTES) with each chunk of a
   chunked response as it arrives, on the async http:// path — CONN is
   the outbound connection, for FETCH-RESUME. The framing walk already
   proves each chunk whole, so this hands those bytes over instead of
   only stepping past them. THEN still fires exactly once at the end,
   with a NIL body: the bytes went out incrementally rather than being
   accumulated, and handing them over twice would double the memory the
   callback exists to avoid.

   Chunked framing only, and it degrades rather than disappearing. A
   Content-Length or close-delimited response has no chunk walk to hand
   bytes back from, so ON-BODY is never called and THEN receives the
   whole body the ordinary way — not incremental, but not lost. An app
   cannot choose which framing an upstream uses: the same origin will
   switch by response size or by whatever proxy is in front, so a relay
   has to be written to accept the body either way. Read the bytes from
   ON-BODY when they arrive there and from THEN's body when they do not.

   Chunk-granular, not line-granular, and deliberately. Line splitting
   already exists once, in READER-READ-BYTES on the blocking path, with
   CR/LF/CRLF handling and partial-line state carried across reads. A
   second implementation here would be two readers that could disagree
   about where a line ends, which is the disagreement this codebase
   treats as its threat model. An app that wants lines can split what it
   is given.

   Return :PAUSE from ON-BODY to stop reading the upstream — its send
   window fills and the pressure propagates back without anything being
   dropped. Resume with FETCH-RESUME. A value rather than a condition,
   for the reason CONNECTION-APPEND-WRITE refuses by return: applying
   backpressure is ordinary control flow and should not unwind through
   the middle of a read loop.

   :PAUSE stops the upstream, not the current pass. Whatever was already
   read stays in the read buffer and every chunk of it is still handed
   over before EPOLLIN is dropped — CONNECTION-READ-AVAILABLE drains to
   EAGAIN, so that can be a lot. This is deliberate and load-bearing: if
   the walk stopped early, the undelivered bytes would sit in user space
   where an EPOLL_CTL_MOD does not re-fire, and FETCH-RESUME could not be
   a simple re-arm. The last answer in a pass is the one that counts, so
   a caller may pause on one chunk and continue on a later one."
  (unless then
    (error "http-fetch requires :then callback"))
  (make-http-fetch-continuation :method method :url url
                                :headers headers :body body
                                :callback then :on-body on-body))

(defun defer-to-fetch (method url &key headers body then)
  "Readability alias for HTTP-FETCH at the handler call site. Returns an
   HTTP-FETCH-CONTINUATION which the framework recognizes as a signal to
   park the inbound connection and run the outbound call.
   Handlers that write (defer-to-fetch :post url :then ...) read as
   'this handler defers to an outbound fetch', which is exactly what is
   happening — the handler's return value encodes the next async step
   instead of producing a synchronous response."
  (http-fetch method url :headers headers :body body :then then))

;;; ---------------------------------------------------------------------------
;;; IP literal parsers
;;;
;;; Used both by PARSE-URL (to recognize bracketed IPv6 literals) and by
;;; INITIATE-HTTP-FETCH (to skip DNS entirely when the host is already an
;;; address). Returning NIL on any failure — callers fall through to the
;;; next case, never error.
;;; ---------------------------------------------------------------------------

(defun parse-ipv4-literal (str)
  "Parse STR as a strict dotted-quad IPv4 address. Returns a 4-byte
   vector on success, NIL on any failure. Rejects leading zeros (except
   the bare '0'), out-of-range octets, wrong segment count, non-digits."
  (unless (and (stringp str) (> (length str) 0))
    (return-from parse-ipv4-literal nil))
  (let ((parts '())
        (start 0)
        (len (length str)))
    (dotimes (i (1+ len))
      (when (or (= i len) (char= (char str i) #\.))
        (let ((piece (subseq str start i)))
          (when (zerop (length piece))
            (return-from parse-ipv4-literal nil))
          (when (and (> (length piece) 1) (char= (char piece 0) #\0))
            (return-from parse-ipv4-literal nil))
          (unless (every (lambda (c) (char<= #\0 c #\9)) piece)
            (return-from parse-ipv4-literal nil))
          (let ((val (parse-integer piece)))
            (unless (<= 0 val 255)
              (return-from parse-ipv4-literal nil))
            (push val parts)))
        (setf start (1+ i))))
    (when (= (length parts) 4)
      (make-array 4 :element-type '(unsigned-byte 8)
                    :initial-contents (nreverse parts)))))

(defun %ipv6-groups-to-bytes (groups)
  "Convert a list of eight 16-bit integers to a 16-byte vector (network
   byte order — big-endian)."
  (let ((out (make-array 16 :element-type '(unsigned-byte 8))))
    (loop for g in groups
          for i from 0 by 2
          do (setf (aref out i)       (logand #xFF (ash g -8))
                   (aref out (1+ i))  (logand #xFF g)))
    out))

(defun %parse-ipv6-group (s)
  "Parse 1-4 hex digits as an integer 0-65535, or return NIL."
  (and (<= 1 (length s) 4)
       (every (lambda (c)
                (or (char<= #\0 c #\9)
                    (char<= #\a c #\f)
                    (char<= #\A c #\F)))
              s)
       (parse-integer s :radix 16)))

(defun %split-ipv6-groups (s)
  "Split S on colons into a list of substrings. Empty S returns NIL."
  (when (zerop (length s))
    (return-from %split-ipv6-groups nil))
  (let ((groups '())
        (start 0))
    (dotimes (i (1+ (length s)) (nreverse groups))
      (when (or (= i (length s)) (char= (char s i) #\:))
        (push (subseq s start i) groups)
        (setf start (1+ i))))))

(defun %ipv6-maybe-unfold-ipv4-tail (groups)
  "If the last entry of GROUPS is a dotted-quad IPv4 literal, expand
   it to two 16-bit hex-string groups and return the new list.
   Otherwise return GROUPS unchanged. Supports the IPv4-mapped and
   IPv4-compatible IPv6 address forms (RFC 4291 §2.5.5) — e.g.
   '::ffff:127.0.0.1' — so PARSE-IPV6-LITERAL and IS-PUBLIC-ADDRESS-P
   agree on URL-literal classification.

   The last-contains-dot branch uses OR so a failed PARSE-IPV4-LITERAL
   falls back to GROUPS unchanged (the downstream %PARSE-IPV6-GROUP
   then rejects the dotted tail as non-hex). Returning NIL on a
   failed unfold would mask the rejection and let PARSE-IPV6-LITERAL
   silently accept malformed literals like '::1.2.3' as '::'."
  (let ((last (car (last groups))))
    (if (and last (find #\. last))
        (or (let ((v4 (parse-ipv4-literal last)))
              (when v4
                (let ((hi (logior (ash (aref v4 0) 8) (aref v4 1)))
                      (lo (logior (ash (aref v4 2) 8) (aref v4 3))))
                  (append (butlast groups)
                          (list (format nil "~x" hi)
                                (format nil "~x" lo))))))
            groups)
        groups)))

(defun parse-ipv6-literal (str)
  "Parse STR as an IPv6 literal. Returns a 16-byte vector on success,
   NIL on failure. Handles :: compression and the IPv4-mapped /
   IPv4-compatible tail form (::ffff:1.2.3.4 — RFC 4291 §2.5.5).
   Does not handle zone identifiers (fe80::1%eth0) — rare in URL
   contexts and v1 rejects them cleanly."
  (unless (and (stringp str) (> (length str) 0))
    (return-from parse-ipv6-literal nil))
  (let ((dcolon (search "::" str)))
    (cond
      ;; "::" compression present
      (dcolon
       (let* ((prefix (subseq str 0 dcolon))
              (suffix (subseq str (+ dcolon 2)))
              (pre (if (zerop (length prefix)) '() (%split-ipv6-groups prefix)))
              (suf (if (zerop (length suffix)) '() (%split-ipv6-groups suffix)))
              (suf (%ipv6-maybe-unfold-ipv4-tail suf))
              (pre-vals (mapcar #'%parse-ipv6-group pre))
              (suf-vals (mapcar #'%parse-ipv6-group suf))
              (filled (+ (length pre-vals) (length suf-vals))))
         (when (and (every #'identity pre-vals)
                    (every #'identity suf-vals)
                    (<= filled 7))
           (%ipv6-groups-to-bytes
            (append pre-vals
                    (make-list (- 8 filled) :initial-element 0)
                    suf-vals)))))
      ;; No "::" — must be exactly 8 groups (or 6 groups + IPv4 tail)
      (t
       (let* ((groups (%ipv6-maybe-unfold-ipv4-tail
                       (%split-ipv6-groups str)))
              (vals (mapcar #'%parse-ipv6-group groups)))
         (when (and (= (length vals) 8) (every #'identity vals))
           (%ipv6-groups-to-bytes vals)))))))

;;; ---------------------------------------------------------------------------
;;; URL parsing
;;; ---------------------------------------------------------------------------

(defun %parse-port (authority start)
  "Parse a decimal port number from AUTHORITY starting at START.
   Raises a clean error on non-numeric input instead of letting
   parse-integer's SIMPLE-TYPE-ERROR leak out of parse-url."
  (let ((p (handler-case (parse-integer authority :start start)
             (error ()
               (error "malformed URL authority: non-numeric port in ~a" authority)))))
    (unless (<= 1 p 65535)
      (error "malformed URL authority: port ~d out of range in ~a" p authority))
    p))

(defun parse-authority (authority default-port)
  "Split an HTTP authority string into (values HOST PORT). Handles
   bracketed IPv6 literals per RFC 3986 §3.2.2: '[::1]:8080' → host '::1',
   port 8080. For unbracketed authorities, splits on the first (only)
   colon between host and port.

   Userinfo (user:pass@host) is rejected explicitly. RFC 7230 §2.7.1
   deprecates the http(s) userinfo form and advises discarding it on
   sight. We do not forward credentials, and silently accepting
   user:pass@host would make the resulting Host header and any
   origin checks work against the wrong string."
  (when (find #\@ authority)
    (error "malformed URL authority: userinfo not supported in ~a"
           authority))
  (cond
    ;; Bracketed IPv6 literal
    ((and (> (length authority) 0) (char= (char authority 0) #\[))
     (let ((close (position #\] authority)))
       (unless close
         (error "malformed URL authority: unclosed '[' in ~a" authority))
       (let ((host (subseq authority 1 close))
             (rest (subseq authority (1+ close))))
         (if (and (> (length rest) 0) (char= (char rest 0) #\:))
             (values host (%parse-port rest 1))
             (values host default-port)))))
    ;; Plain host or IPv4 literal
    (t
     (let ((colon (position #\: authority)))
       (if colon
           (values (subseq authority 0 colon)
                   (%parse-port authority (1+ colon)))
           (values authority default-port))))))

(defun parse-url (url)
  "Parse a URL into (values scheme host port path).
   Supports http:// and https:// schemes. Host may be a plain name,
   an IPv4 literal, or a bracketed IPv6 literal ('[2001:db8::1]').
   Rejects URLs longer than *MAX-REQUEST-LINE-LENGTH* — a handler that
   proxies a user-controlled URL must not be able to hand us a 100 MB
   string that allocates through several intermediate buffers before
   anything else can push back. Also rejects CTL bytes in the URL:
   the fetch path log-debug lines in fetch.lisp / dns.lisp / tls.lisp
   interpolate HOST and PATH with ~a, so an attacker-supplied URL
   with bare CR / LF would otherwise give a log-injection primitive
   identical in shape to the inbound request-target check in
   parse-request-bytes."
  (when (> (length url) *max-request-line-length*)
    (error "URL too long (~d bytes, max ~d)"
           (length url) *max-request-line-length*))
  (loop for i from 0 below (length url)
        for code = (char-code (char url i))
        when (or (<= code #x20) (>= code #x7F))
        do (error "URL contains non-ASCII or control character"))
  (let ((scheme nil) (authority-start nil) (default-port nil))
    (cond
      ((and (>= (length url) 8) (string-equal url "https://" :end1 8))
       (setf scheme :https authority-start 8 default-port 443))
      ((and (>= (length url) 7) (string-equal url "http://" :end1 7))
       (setf scheme :http authority-start 7 default-port 80))
      (t (error "unsupported URL scheme: ~a" url)))
    ;; RFC 3986 §3.5: fragment is identified by '#' and is never
    ;; sent to the origin server. Cap the effective end of the URL
    ;; at the fragment delimiter once so both the authority scan
    ;; and the path slice respect the same boundary — leaving the
    ;; fragment glued to the path would emit a request-target
    ;; containing '#frag', which RFC 7230 §5.3 forbids.
    ;;
    ;; Authority ends at the first '/' or '?' within that cap
    ;; (RFC 3986 §3.2). A URL like 'http://host?q=1' has no path
    ;; segment at all, so scanning only for '/' would parse
    ;; "host?q=1" as the hostname and dial it.
    (let* ((end          (length url))
           (frag-start   (position #\# url))
           (request-end  (or frag-start end))
           (path-start
            (or (loop for i from authority-start below request-end
                      for ch = (char url i)
                      when (or (char= ch #\/) (char= ch #\?))
                      return i)
                request-end))
           (authority (subseq url authority-start path-start))
           (path
            (cond
              ((= path-start request-end) "/")
              ((char= (char url path-start) #\/)
               (subseq url path-start request-end))
              ;; No path segment — synthesize '/' so the origin-form
              ;; request-target is well-formed (HTTP/1.1 §5.3).
              (t
               (concatenate 'string "/"
                            (subseq url path-start request-end))))))
      (multiple-value-bind (host port) (parse-authority authority default-port)
        (when (zerop (length host))
          (error "malformed URL: empty host"))
        ;; Reject https:// with an IP-literal host. SSL_set1_host sets
        ;; a DNS name for peer verification; matching an IP SAN requires
        ;; X509_VERIFY_PARAM_set1_ip_asc, which we do not wire up. A
        ;; certificate with only a DNS SAN — which is the common case —
        ;; would fail the handshake anyway; failing loudly here closes
        ;; the asymmetry with plain-HTTP IP-literal support honestly
        ;; instead of producing an opaque SSL_connect error in the app.
        (when (and (eq scheme :https)
                   (or (parse-ipv4-literal host)
                       (parse-ipv6-literal host)))
          (error "https:// with IP-literal host not supported — ~
                  use a DNS name (IP SAN verification is not wired up)"))
        (values scheme host port path)))))

;;; ---------------------------------------------------------------------------
;;; Build outbound HTTP request bytes
;;; ---------------------------------------------------------------------------

(defun build-outbound-request (method host path &key (scheme :http) (port 80)
                                                      headers body)
  "Build an HTTP/1.1 request as a byte vector ready to write.

   IPv6 literal hosts are re-bracketed for the wire Host: value.
   parse-authority strips the brackets from the internal HOST
   string (correct — they're URL syntax, not part of the address),
   but RFC 7230 §5.4 requires the Host header to be identical to
   the authority component, and RFC 3986 §3.2.2 defines the IPv6
   authority grammar as '[' IPv6address ']' with the brackets
   mandatory. A bare 'Host: ::1:8080' is unparseable — no reader
   can tell where the address ends and the port begins — and
   strict upstreams 400 it. The re-bracket here keeps the
   parse-url → wire-form pipeline symmetric.

   METHOD must be a keyword whose symbol-name is uppercase ASCII.
   Apps hardcode methods in practice, but validating the charset
   closes a theoretical smuggling primitive — an intern'd keyword
   from attacker-influenced data containing a space or CRLF would
   otherwise emit a first line like 'GET HTTP/1.1 /path HTTP/1.1',
   i.e. two valid request lines to a permissive upstream parser.
   Symmetric with the MATCH-METHOD-BYTES acceptance set on ingress."
  (let ((method-str (symbol-name method)))
    ;; Validate method BEFORE the body-bytes UTF-8 encoding and
    ;; header-list build below — an invalid method throws away the
    ;; allocation otherwise. Check shape: uppercase ASCII letters
    ;; only, non-empty.
    (unless (and (> (length method-str) 0)
                 (every (lambda (c) (char<= #\A c #\Z)) method-str))
      (error "build-outbound-request: method must be uppercase ASCII ~
              letters, got ~s" method-str))
    ;; Framing headers are ours, not the caller's. RFC 7230 §3.3.3 gives
    ;; two ways for a request's declared framing to disagree with the
    ;; bytes that follow, and :headers can reach both.
    ;;
    ;; Transfer-Encoding: this function never chunk-encodes, so the claim
    ;; is false whatever the body is. The merge below adds Content-Length
    ;; only when the caller did not supply one — and Transfer-Encoding is
    ;; not Content-Length, so it does not suppress it. A caller passing
    ;; ("transfer-encoding" . "chunked") with a body therefore emits both
    ;; framing headers at once, which is the canonical smuggling shape,
    ;; pointed at the upstream.
    ;;
    ;; Content-Length: the same merge rule is what opens this one. A
    ;; caller-supplied Content-Length *does* suppress the computed one,
    ;; so ("content-length" . "5") alongside a ten-byte body puts a
    ;; five-byte promise in front of ten bytes and leaves "56789" sitting
    ;; in the upstream's buffer as the start of the next request. The
    ;; inverse just hangs the upstream until its read timeout. This is
    ;; the easier of the two to reach by accident, because it needs no
    ;; exotic header — only a stale content-length carried along with the
    ;; rest of a copied header alist.
    ;;
    ;; Ingress already refuses Transfer-Encoding on principle. Refusing
    ;; to emit what we refuse to accept is the same symmetry the tchar
    ;; table keeps between PARSE-HEADERS-BYTES and SERIALIZE-HTTP-MESSAGE,
    ;; and it is the same threat model the method-charset check above
    ;; already accepts — attacker-influenced data reaching :headers is
    ;; likelier than attacker-influenced data reaching :method.
    (when (assoc "transfer-encoding" headers :test #'string-equal)
      (error "build-outbound-request: Transfer-Encoding is not supported on ~
              outbound requests — the framework frames bodies with ~
              Content-Length"))
    (when (assoc "content-length" headers :test #'string-equal)
      (error "build-outbound-request: Content-Length is computed from :body. ~
              A caller-supplied one that disagrees with the body is a framing ~
              lie aimed at the upstream. Pass :body \"\" for a deliberate ~
              Content-Length: 0."))
  (let* ((default-port-p (or (and (eq scheme :http) (= port 80))
                             (and (eq scheme :https) (= port 443))))
         ;; Re-bracket IPv6 literals. An IPv6 address always contains
         ;; at least one ':'; no DNS name, IPv4 literal, or port-qualified
         ;; DNS name ever contains a ':' internally at this call point
         ;; (parse-authority has already split port off). Detecting
         ;; via colon is the minimal, allocation-free test.
         (ipv6-p (find #\: host))
         (wire-host (if ipv6-p (format nil "[~a]" host) host))
         (host-value (if default-port-p
                         wire-host
                         (format nil "~a:~d" wire-host port)))
         (body-bytes (etypecase body
                       (null nil)
                       (string (sb-ext:string-to-octets body
                                                         :external-format :utf-8))
                       ((simple-array (unsigned-byte 8) (*)) body)))
         (all-headers (flet ((has-header-p (name)
                              (assoc name headers :test #'string-equal)))
                       (append
                        (unless (has-header-p "host")
                          (list (cons "host" host-value)))
                        (unless (has-header-p "connection")
                          (list (cons "connection" "close")))
                        (unless (has-header-p "user-agent")
                          (list (cons "user-agent" "web-skeleton")))
                        headers
                        ;; Unconditional. The guard above refuses a
                        ;; caller-supplied Content-Length outright, so
                        ;; there is no case left where theirs wins and
                        ;; the test could only ever be true. Leaving it
                        ;; in would leave the shape of the old contract —
                        ;; "the caller may override framing" — sitting
                        ;; thirty lines below a comment saying framing
                        ;; headers are ours, and a reader who meets the
                        ;; merge first would re-derive the wrong rule.
                        ;; HOST / CONNECTION / USER-AGENT above stay
                        ;; conditional: those are real caller overrides.
                        (when body-bytes
                          (list (cons "content-length"
                                      (write-to-string
                                       (length body-bytes)))))))))
    (serialize-http-message
     (format nil "~a ~a HTTP/1.1" method-str path)
     all-headers body-bytes))))

;;; ---------------------------------------------------------------------------
;;; Parse response status line
;;; ---------------------------------------------------------------------------

(defun parse-status-line-string (line)
  "String-level twin of PARSE-RESPONSE-STATUS. LINE is the first
   response line with CRLF already stripped (how the streaming
   readers hand it over). Used by STREAM-RESPONSE-LINES and
   TLS-STREAM-RESPONSE so their acceptance set is identical to
   the buffered path's byte-level check — without this, a non-HTTP
   upstream whose first line is '<junk> 200 OK' ('FUBAR 200 OK',
   'NOT-HTTP 418 Z') parses as status 200 on the streaming paths
   while the buffered path correctly rejects, a parser-disagreement
   smuggling primitive. Requires a literal 'HTTP/1.0 ' or 'HTTP/1.1 '
   prefix (9 chars), then three digits in 100-599, then SP or
   end-of-line. Returns the status integer or NIL."
  (when (and line (>= (length line) 12))
    (let ((c0 (char line 0)) (c1 (char line 1)) (c2 (char line 2))
          (c3 (char line 3)) (c4 (char line 4)) (c5 (char line 5))
          (c6 (char line 6)) (c7 (char line 7)) (c8 (char line 8)))
      (when (and (char= c0 #\H) (char= c1 #\T) (char= c2 #\T)
                 (char= c3 #\P) (char= c4 #\/) (char= c5 #\1)
                 (char= c6 #\.) (or (char= c7 #\0) (char= c7 #\1))
                 (char= c8 #\Space))
        (let ((d1 (char line 9))
              (d2 (char line 10))
              (d3 (char line 11)))
          (when (and (digit-char-p d1) (digit-char-p d2) (digit-char-p d3)
                     (or (= (length line) 12)
                         (char= (char line 12) #\Space)))
            (let ((status (+ (* (digit-char-p d1) 100)
                             (* (digit-char-p d2) 10)
                             (digit-char-p d3))))
              (when (<= 100 status 599)
                status))))))))

(defun parse-response-status (buf start end)
  "Extract the integer status code from a response line in BUF[START..END).
   Expects 'HTTP/1.x NNN' or 'HTTP/1.x NNN reason' (RFC 7230 §3.1.2).
   Requires a byte-for-byte 'HTTP/1.0 ' or 'HTTP/1.1 ' prefix before
   reading digits. Without the prefix check a non-HTTP upstream whose
   first line happened to contain '<junk> 200' ('FOOBAR 200 OK',
   'NOT-HTTP 418 Z') would parse as status 200 — masquerading as HTTP
   to any app pattern-matching on the returned status. Symmetric with
   PARSE-REQUEST-BYTES' byte-level version check on the inbound side.
   Returns the status only if it is a 3-digit number in the 100-599
   range (RFC 7231 §6) followed by SP or CR. Rejects 4-digit codes,
   junk-terminated digits, and anything outside the defined ranges."
  (let ((crlf (scan-crlf buf start end)))
    (unless crlf (return-from parse-response-status nil))
    ;; HTTP/1.x prefix: 9 bytes (HTTP/1.0 or HTTP/1.1 + SP).
    (unless (and (>= (- crlf start) 9)
                 (= (aref buf start)       72)  ; H
                 (= (aref buf (+ start 1)) 84)  ; T
                 (= (aref buf (+ start 2)) 84)  ; T
                 (= (aref buf (+ start 3)) 80)  ; P
                 (= (aref buf (+ start 4)) 47)  ; /
                 (= (aref buf (+ start 5)) 49)  ; 1
                 (= (aref buf (+ start 6)) 46)  ; .
                 (or (= (aref buf (+ start 7)) 48)    ; 0
                     (= (aref buf (+ start 7)) 49))   ; 1
                 (= (aref buf (+ start 8)) 32))       ; SP
      (return-from parse-response-status nil))
    (let ((s (+ start 9)))
      ;; Need three digits plus a terminator byte (SP before reason,
      ;; or CR at end-of-line if the reason-phrase is omitted).
      (when (<= (+ s 3) crlf)
        (let ((d1 (- (aref buf s) 48))
              (d2 (- (aref buf (+ s 1)) 48))
              (d3 (- (aref buf (+ s 2)) 48))
              (after (aref buf (+ s 3))))
          (when (and (<= 0 d1 9) (<= 0 d2 9) (<= 0 d3 9)
                     (or (= after 32) (= after 13)))
            (let ((status (+ (* d1 100) (* d2 10) d3)))
              (when (<= 100 status 599)
                status))))))))

;;; ---------------------------------------------------------------------------
;;; Interim (1xx) responses
;;;
;;; RFC 7231 §6.2: "A client MUST be able to parse one or more 1xx responses
;;; received prior to a final response, even if the client does not expect
;;; one." A 1xx block is terminated by its own empty line and carries no
;;; body, so the *final* response begins immediately after it.
;;;
;;; Every buffered reader here anchors on a CRLFCRLF to find where headers
;;; end. Without a skip, that first boundary is the *interim's* terminator:
;;; the interim's status becomes the fetch's status, its headers become the
;;; fetch's headers, and — because 1xx is in the bodiless exempt set — the
;;; body is forced empty. The real response is discarded with no error and
;;; no log line.
;;;
;;; This is not a hypothetical shape an app can avoid by configuration.
;;; `103 Early Hints` (RFC 8297) is sent UNSOLICITED by CDNs — Cloudflare
;;; and Fastly both do it. Nothing the application does invites it, and
;;; the old caveat in this file ("Apps must not add Expect: 100-continue
;;; to outbound fetch headers") pushed a protocol obligation onto the
;;; application that it had no way to discharge.
;;;
;;; A 1xx must never be reported as a final status either: it is by
;;; definition interim, so "keep reading" is the only correct response to
;;; one. The framing predicate has to skip as well — otherwise the
;;; interim's (absent) Content-Length decides framing for the whole
;;; exchange, which is how a CL-framed 200 gets mistaken for a
;;; close-delimited response and hangs until *FETCH-TIMEOUT*.
;;; ---------------------------------------------------------------------------

(defparameter *max-interim-responses* 8
  "Maximum number of 1xx blocks accepted before the final response. Eight
   are allowed; the ninth signals. Bounded so an upstream cannot stream
   interim responses forever inside the response size cap — each block is
   cheap on its own, and without a cap a malicious or broken peer could
   hold a connection open indefinitely feeding blocks that are
   individually well-formed.

   Raise it for an app behind a chatty CDN. All three transports read this
   one variable, so they cannot drift on what the limit is.")

(defun skip-interim-responses (buf start end)
  "Return the offset of the final response's first byte in BUF[START..END),
   stepping over any 1xx interim blocks (RFC 7231 §6.2).

   Returns START unchanged when no complete block is buffered yet, so a
   partially-read interim is simply retried on the next wake-up rather than
   mis-anchoring. Signals when a *MAX-INTERIM-RESPONSES*+1'th interim block
   arrives; every caller already runs inside a handler-case that turns that
   into a 502 plus the fetch cleanup sentinel.

   The loop runs one more time than the cap on purpose: each pass consumes
   at most one interim, so N+1 passes are needed to consume N interims and
   still have a pass left to recognise the final response. Iterating only
   *MAX-INTERIM-RESPONSES* times would reject the Nth interim while the
   streaming paths — which count with a separate (> count cap) test —
   accepted it, and the two transports would disagree about the limit.

   Shared by the buffered plain-HTTP path, the framing predicate, and the
   TLS path so all three agree on where a response starts."
  (let ((pos start))
    (dotimes (i (1+ *max-interim-responses*)
                (error "upstream sent more than ~d interim responses"
                       *max-interim-responses*))
      (declare (ignorable i))
      (let ((header-end (scan-crlf-crlf buf pos end)))
        ;; No complete block from POS — caller keeps reading.
        (unless header-end (return pos))
        (let ((status (parse-response-status buf pos end)))
          ;; A status we cannot parse is not an interim; hand POS back and
          ;; let the caller's own "status line unparseable" path report it.
          (if (and status (<= 100 status 199))
              (setf pos (+ header-end 4))
              (return pos)))))))

;;; ---------------------------------------------------------------------------
;;; HTTPS hook — set by web-skeleton-tls when loaded
;;; ---------------------------------------------------------------------------

(defvar *https-fetch-fn* nil
  "When non-NIL, a function (conn epoll-fd fetch-req host port path) that
   performs a blocking HTTPS fetch.  Set by web-skeleton-tls on load.")

(defvar *https-stream-fn* nil
  "When non-NIL, a function (method host port path headers body on-line) that
   performs a blocking streaming HTTPS fetch.  Set by web-skeleton-tls on load.")

(defvar *dns-resolve-blocking-fn* nil
  "Synchronous DNS resolver — set by src/server/dns.lisp at load time.
   Called with (HOST) and returns (values IP FAMILY) or NIL. Used by
   FETCH-STREAM-PLAIN and TLS-CONNECT to share the same getent-based
   resolver as the async path without forward-referencing dns.lisp.")

;;; ---------------------------------------------------------------------------
;;; Blocking streaming fetch
;;;
;;; Reads the response body line by line, calling a callback per line.
;;; Designed for NDJSON/SSE streaming APIs (e.g. LLM token streams).
;;; Blocks the calling thread — call from ws-handler or HTTP handler.
;;; ---------------------------------------------------------------------------

(defun http-fetch-stream (method url &key headers body on-line)
  "Blocking streaming HTTP(S) fetch.
   Connects, sends request, calls (ON-LINE string) for each line of the
   response body. Returns the HTTP status code.
   Blocks the calling thread for the duration of the response."
  (multiple-value-bind (scheme host port path) (parse-url url)
    (if (eq scheme :https)
        (if *https-stream-fn*
            (funcall *https-stream-fn* method host port path headers body on-line)
            (error "HTTPS streaming not available — load web-skeleton-tls"))
        (fetch-stream-plain method host port path headers body on-line))))

(defun fetch-stream-plain (method host port path headers body on-line)
  "HTTP streaming fetch over plain TCP. Blocks the calling thread for
   the full request/response lifecycle, bounded by *FETCH-TIMEOUT* for
   each of DNS resolution, TCP connect, and socket-level read/write.
   DNS uses the shared getent resolver (same as the async path); the
   TCP connect uses a non-blocking connect + poll(2) so a black-holed
   peer cannot pin the worker for ~120s under tcp_syn_retries."
  (multiple-value-bind (ip family)
      (funcall *dns-resolve-blocking-fn* host)
    (unless ip
      (error "fetch-stream: failed to resolve ~a" host))
    (let ((socket (make-instance (if (eq family :inet)
                                     'sb-bsd-sockets:inet-socket
                                     'sb-bsd-sockets:inet6-socket)
                                 :type :stream :protocol :tcp)))
      (unwind-protect
          (progn
            (set-socket-timeout (socket-fd socket) *fetch-timeout*)
            (blocking-connect socket ip port *fetch-timeout*)
            (let ((stream (sb-bsd-sockets:socket-make-stream
                           socket :input t :output t
                           :element-type '(unsigned-byte 8)
                           :buffering :full)))
              (unwind-protect
                  (let ((request-bytes (build-outbound-request
                                       method host path
                                       :port port
                                       :headers headers :body body)))
                    (write-sequence request-bytes stream)
                    (force-output stream)
                    (stream-response-lines stream on-line :method method))
                (close stream))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

;;; ---------------------------------------------------------------------------
;;; Buffered stream reader
;;;
;;; Wraps a binary stream with a read-ahead buffer so callers can read
;;; line-by-line without issuing one syscall per byte.
;;; ---------------------------------------------------------------------------

(defstruct (stream-reader (:constructor make-stream-reader (stream)))
  (stream nil)
  (buf    (make-array 8192 :element-type '(unsigned-byte 8)))
  (pos    0 :type fixnum)
  (end    0 :type fixnum)
  ;; PREV-CR: CR-partner state for CONTENT reads through
  ;; READER-READ-BYTES only. Set when a CR emits a body line, consumed
  ;; when the next content byte is LF (CRLF collapses to one
  ;; terminator even when split across chunk-data boundaries).
  ;; READER-READ-LINE handles its own CRLF pair inline via lookahead
  ;; and does not touch this slot — a framing CR (chunk-size line,
  ;; header line) must not leak state into the next
  ;; READER-READ-BYTES or the content byte count falls short.
  (prev-cr nil :type boolean))

(defun reader-fill (r)
  "Refill the buffer. Returns bytes read (0 = EOF)."
  (setf (stream-reader-pos r) 0)
  (let ((n (read-sequence (stream-reader-buf r) (stream-reader-stream r))))
    (setf (stream-reader-end r) n)
    n))

(defun reader-read-line (r &key (max-size *max-streaming-line-size*))
  "Read a line from the buffered reader. Treats CR, LF, and CRLF as
   equivalent line terminators (WHATWG EventStream §9.2). The CRLF
   pair is consumed in one call via lookahead past the CR — the LF
   partner is taken from the current buffer, or from a refill when
   the pair straddles a boundary. Does not touch the struct's
   PREV-CR slot: that slot is owned by READER-READ-BYTES for
   cross-call content tracking, and setting it from framing reads
   (chunk-size lines, headers) would leak into the next
   READER-READ-BYTES call and mis-align the byte count against
   framing bytes. Returns a string, or NIL at EOF with no pending
   bytes; EOF mid-line flushes the partial line once.
   MAX-SIZE bounds the accumulated line length. Defaults to
   *MAX-STREAMING-LINE-SIZE* for body lines; header-phase callers
   pass a tighter *MAX-HEADER-LINE-LENGTH* so a 1 MiB attacker-
   framed 'header' is rejected on the same budget as the buffered
   parse-headers-bytes path rather than the body-line budget."
  (let ((line-buf (make-array 256 :element-type '(unsigned-byte 8)
                                  :fill-pointer 0 :adjustable t)))
    (loop
      (when (>= (stream-reader-pos r) (stream-reader-end r))
        (when (zerop (reader-fill r))
          (return (if (zerop (fill-pointer line-buf)) nil
                      (sb-ext:octets-to-string line-buf
                                               :external-format :utf-8)))))
      (let* ((buf (stream-reader-buf r))
             (start (stream-reader-pos r))
             (end (stream-reader-end r))
             ;; First CR or LF in the remaining buffer, whichever comes first.
             (term-idx (loop for i from start below end
                             when (or (= (aref buf i) 13)
                                      (= (aref buf i) 10))
                             return i)))
        (cond
          (term-idx
           (loop for i from start below term-idx
                 do (when (>= (fill-pointer line-buf) max-size)
                      (error "streaming response line too large (max ~d)"
                             max-size))
                    (vector-push-extend (aref buf i) line-buf))
           (setf (stream-reader-pos r) (1+ term-idx))
           ;; If we terminated on CR, consume a following LF as the
           ;; second half of a CRLF pair. Refill once if the LF is
           ;; past the current buffer so a pair split at a read
           ;; boundary still collapses to one terminator.
           (when (= (aref buf term-idx) 13)
             (when (>= (stream-reader-pos r) (stream-reader-end r))
               (reader-fill r))
             (when (and (< (stream-reader-pos r) (stream-reader-end r))
                        (= (aref (stream-reader-buf r)
                                 (stream-reader-pos r))
                           10))
               (incf (stream-reader-pos r))))
           (return (sb-ext:octets-to-string line-buf
                                            :external-format :utf-8)))
          (t
           (loop for i from start below end
                 do (when (>= (fill-pointer line-buf) max-size)
                      (error "streaming response line too large (max ~d)"
                             max-size))
                    (vector-push-extend (aref buf i) line-buf))
           (setf (stream-reader-pos r) end)))))))

(defun reader-expect-crlf (r)
  "Consume exactly two bytes from R and raise if they are not
   CR (0x0D) followed by LF (0x0A). Used by stream-chunked-lines
   to enforce the strict RFC 7230 §4.1 chunk-data terminator.

   READER-READ-LINE walks to the next LF and silently strips CR
   bytes from the accumulated content, so calling it on the
   trailing CRLF would tolerate a bare LF, a bare CR followed by
   anything, or even multi-byte garbage before the LF — all
   acceptance modes the buffered DECODE-CHUNKED-BODY rejects."
  (flet ((next-byte ()
           (when (>= (stream-reader-pos r) (stream-reader-end r))
             (when (zerop (reader-fill r))
               (error "chunked stream: truncated before chunk-data CRLF")))
           (prog1 (aref (stream-reader-buf r) (stream-reader-pos r))
             (incf (stream-reader-pos r)))))
    (unless (= (next-byte) 13)
      (error "chunked stream: expected CR after chunk-data"))
    (unless (= (next-byte) 10)
      (error "chunked stream: expected LF after chunk-data"))))

(defun reader-read-crlf-line (r &key (max-size *max-header-line-length*))
  "Strict-CRLF twin of READER-READ-LINE for chunk-size lines.
   Rejects bare CR and bare LF; requires a literal CR+LF pair to
   terminate. Symmetric with DECODE-CHUNKED-BODY's buffered strict
   CRLF discipline (RFC 7230 §4.1) — without this, a bare-LF or
   bare-CR chunk-size terminator would silently pass through
   READER-READ-LINE's WHATWG-style lenient parse, a parser-
   disagreement primitive. Returns the line string (terminator
   stripped), or NIL on clean EOF before any bytes. MAX-SIZE
   defaults to *MAX-HEADER-LINE-LENGTH*, mirroring the header-line
   budget — chunk-size lines with chunk-extensions rarely exceed
   this in practice."
  (let ((line-buf (make-array 64 :element-type '(unsigned-byte 8)
                                 :fill-pointer 0 :adjustable t))
        (saw-any nil))
    (loop
      (when (>= (stream-reader-pos r) (stream-reader-end r))
        (when (zerop (reader-fill r))
          (return (if saw-any
                      (error "chunked stream: truncated chunk-size line")
                      nil))))
      (let ((byte (aref (stream-reader-buf r) (stream-reader-pos r))))
        (incf (stream-reader-pos r))
        (setf saw-any t)
        (cond
          ((= byte 13)
           ;; CR — must be followed immediately by LF.
           (when (>= (stream-reader-pos r) (stream-reader-end r))
             (when (zerop (reader-fill r))
               (error "chunked stream: bare CR at EOF in chunk-size line")))
           (let ((next (aref (stream-reader-buf r) (stream-reader-pos r))))
             (unless (= next 10)
               (error "chunked stream: bare CR in chunk-size line"))
             (incf (stream-reader-pos r))
             (return (sb-ext:octets-to-string
                      line-buf :external-format :ascii))))
          ((= byte 10)
           (error "chunked stream: bare LF in chunk-size line"))
          (t
           (when (>= (fill-pointer line-buf) max-size)
             (error "chunked stream: chunk-size line too long (max ~d)"
                    max-size))
           (vector-push-extend byte line-buf)))))))

(defun reader-read-bytes (r count line-buf on-line)
  "Read COUNT bytes through the buffered reader, splitting into lines.
   Calls ON-LINE per complete line. Returns the number of bytes
   actually consumed — the caller compares against COUNT to detect
   truncation (EOF before the requested body was delivered, which
   is indistinguishable on the SBCL stream layer from SO_RCVTIMEO
   firing mid-stream).

   Terminators: CR, LF, CRLF all recognized (WHATWG EventStream
   §9.2). PREV-CR on the reader struct carries the CR-partner state
   across READER-READ-BYTES calls so a CRLF pair split at a chunk-
   data boundary collapses to one terminator instead of emitting a
   spurious empty line on the chunk-2 leading LF. READER-READ-LINE
   deliberately does not touch that slot — framing reads
   (chunk-size lines) must not set it, or the next
   READER-READ-BYTES would consume the framing LF as if it were a
   content CRLF-partner and the chunk-data byte count would fall
   short by one."
  (let ((remaining count))
    (loop while (> remaining 0) do
      (when (>= (stream-reader-pos r) (stream-reader-end r))
        (when (zerop (reader-fill r))
          (return)))
      (let* ((buf (stream-reader-buf r))
             (pos (stream-reader-pos r))
             (avail (- (stream-reader-end r) pos))
             (take (min avail remaining)))
        (loop for i from pos below (+ pos take)
              for byte = (aref buf i)
              do (cond
                   ((= byte 13)
                    (when on-line
                      (funcall on-line (sb-ext:octets-to-string
                                        (subseq line-buf 0 (fill-pointer line-buf))
                                        :external-format :utf-8)))
                    (setf (fill-pointer line-buf) 0)
                    (setf (stream-reader-prev-cr r) t))
                   ((= byte 10)
                    (cond
                      ((stream-reader-prev-cr r)
                       (setf (stream-reader-prev-cr r) nil))
                      (t
                       (when on-line
                         (funcall on-line (sb-ext:octets-to-string
                                           (subseq line-buf 0 (fill-pointer line-buf))
                                           :external-format :utf-8)))
                       (setf (fill-pointer line-buf) 0))))
                   (t
                    (setf (stream-reader-prev-cr r) nil)
                    (when (>= (fill-pointer line-buf)
                              *max-streaming-line-size*)
                      (error "streaming response line too large (max ~d)"
                             *max-streaming-line-size*))
                    (vector-push-extend byte line-buf))))
        (incf (stream-reader-pos r) take)
        (decf remaining take)))
    (- count remaining)))

;;; ---------------------------------------------------------------------------
;;; HTTP response streaming
;;; ---------------------------------------------------------------------------

(defun stream-response-lines (stream on-line &key (method :GET))
  "Read an HTTP response from a byte stream. Skip headers, call
   ON-LINE per body line. Handles chunked transfer encoding.
   Returns the status code.

   METHOD gates the body-framing discipline: for :HEAD, RFC 7231
   §4.3.2 guarantees an empty body even when the upstream echoes
   the GET-body Content-Length, so the body phase is skipped and
   the truncation checks are bypassed.

   Truncation discipline on the streaming path:
     - chunked: stream-chunked-lines raises if the zero-size
       terminator never arrives. Same contract as decode-chunked-body
       for the buffered path.
     - content-length (no TE): reader-read-bytes is called with the
       declared length; a short return means EOF (or SO_RCVTIMEO on
       the underlying blocking socket) before the full body was
       delivered — raise so the caller sees a loud error instead of
       a silently short stream. This is the plain-HTTP twin of the
       SSL_ERROR_SYSCALL discipline in tls.lisp's ssl-read-eof-or-raise.
     - close-delimited (no TE, no CL): by definition the connection
       close IS the end; we cannot distinguish a legitimate end from
       a premature RST without out-of-band framing, so treat clean
       EOF as complete. Apps that care about this case should use a
       framed path upstream."
  (let ((r (make-stream-reader stream))
        (status nil)
        (chunked nil)
        (te-present nil)
        (content-length nil)
        (interims 0))
    ;; Read status line + headers. Tighter MAX-SIZE for header lines
    ;; matches the buffered parse-headers-bytes budget so a 1 MiB
    ;; attacker-framed "header" can't coast on the body-line cap.
    ;; TOTAL-HEADER-BYTES enforces the aggregate cap symmetrically
    ;; with parse-headers-bytes' running-total guard.
    ;;
    ;; The whole header phase repeats over any 1xx interim block
    ;; (RFC 7231 §6.2): an interim is terminated by its own empty line
    ;; and carries no body, so the final response's status line is the
    ;; very next line. Without this the header loop stopped at the
    ;; interim's blank line, reported 103 as the status, took the
    ;; bodiless branch, and never called ON-LINE at all. Per-block
    ;; state is reset each pass so an interim's headers cannot leak
    ;; into the final response's framing decision.
    (loop
      (setf status nil chunked nil te-present nil content-length nil)
      (let ((header-count 0)
          (total-header-bytes 0))
      (loop for line = (reader-read-line r :max-size *max-header-line-length*)
            for first = t then nil
            while (and line (> (length line) 0))
            do (incf header-count)
               (when (> header-count *max-header-count*)
                 (error "streaming response: too many headers (~d)" header-count))
               (incf total-header-bytes (length line))
               (when (> total-header-bytes *max-total-header-bytes*)
                 (error "streaming response: total header bytes exceed ~d"
                        *max-total-header-bytes*))
               ;; RFC 7230 §3.2.4 — obsolete line folding. Buffered
               ;; parse-headers-bytes rejects; streaming mirrors so
               ;; the acceptance set doesn't drift. Skip the check on
               ;; the status line (it would have failed
               ;; parse-status-line-string anyway).
               (when (and (not first)
                          (or (char= (char line 0) #\Space)
                              (char= (char line 0) #\Tab)))
                 (error "streaming response: obsolete line folding not accepted"))
               (when first
                 (setf status (parse-status-line-string line)))
               (when (and (>= (length line) 18)
                          (string-equal line "transfer-encoding:"
                                        :end1 18))
                 (setf te-present t)
                 (let ((value (string-trim '(#\Space #\Tab) (subseq line 18))))
                   (when (header-has-token-p value "chunked")
                     (setf chunked t))))
               (when (and (>= (length line) 15)
                          (string-equal line "content-length:"
                                        :end1 15))
                 ;; Strict digits-only parse, symmetric with the
                 ;; inbound SCAN-CONTENT-LENGTH byte scanner so a
                 ;; future edit can't let one path accept a value
                 ;; the other rejects.
                 (let ((value (string-trim '(#\Space #\Tab) (subseq line 15))))
                   (unless (and (> (length value) 0)
                                (every (lambda (c) (char<= #\0 c #\9)) value))
                     (error "streaming response: malformed Content-Length ~s"
                            value))
                   (unless (<= (length value) 10)
                     (error "streaming response: Content-Length too many digits"))
                   (let ((n (parse-integer value)))
                     (when (and content-length (/= n content-length))
                       (error "streaming response: conflicting Content-Length ~
                               ~d vs ~d"
                              content-length n))
                     (setf content-length n))))))
      ;; Interim block — its empty line just ended, so go around and read
      ;; the next status line. A NIL status (unparseable, or EOF before
      ;; any line) is not an interim: fall through and let the post-loop
      ;; "no parseable status line" check report it.
      (if (and status (<= 100 status 199))
          (progn
            (incf interims)
            (when (> interims *max-interim-responses*)
              (error "streaming response: more than ~d interim responses"
                     *max-interim-responses*)))
          (return)))
    ;; Stream body lines
    (cond
      ;; Bodiless FINAL responses — RFC 7230 §3.3.3 rule 1, RFC 7232
      ;; §4.1, RFC 7231 §4.3.2. 204 / 304 are always terminated by
      ;; the empty-line header boundary regardless of CL / TE, and
      ;; HEAD responses carry no body even when the upstream echoes
      ;; the GET Content-Length. Skip body phase and its truncation
      ;; checks. Symmetric with complete-fetch's exempt set on the
      ;; buffered path — without this, a 204 with a leftover CL or
      ;; a 304 with chunked framing would either raise "short body"
      ;; or block forever waiting for framing that will never arrive.
      ;;
      ;; 1xx is deliberately absent: the header loop above only exits
      ;; on a NIL status or a status >= 200, so an interim can never
      ;; reach here. Testing for it would be dead code that reads as
      ;; though a 1xx were a legitimate final status — which is the
      ;; belief that produced the interim-response defect.
      ((or (eq method :HEAD)
           (and status (or (= status 204) (= status 304))))
       nil)
      (chunked
       (stream-chunked-lines r on-line))
      ((and content-length (not te-present))
       ;; Framed non-chunked. Track bytes against CL so SO_RCVTIMEO
       ;; or peer RST mid-body raises loud instead of quietly ending
       ;; the stream. RFC 7230 §3.3.3 order: TE wins over CL, so we
       ;; only reach this branch when the response has CL and no TE.
       (let* ((line-buf (make-array 4096 :element-type '(unsigned-byte 8)
                                         :fill-pointer 0 :adjustable t))
              (consumed (reader-read-bytes r content-length line-buf on-line)))
         (when (< consumed content-length)
           (error "streaming response short body: ~d of ~d bytes"
                  consumed content-length))
         ;; Flush any final unterminated line so the app sees the
         ;; last fragment even when the upstream omitted a trailing
         ;; newline — matches reader-read-line's EOF-with-partial
         ;; behavior.
         (when (> (fill-pointer line-buf) 0)
           (when on-line
             (funcall on-line (sb-ext:octets-to-string
                               line-buf :external-format :utf-8))))))
      (t
       ;; Close-delimited — trust EOF as the framing signal.
       (loop for line = (reader-read-line r)
             while line
             do (when on-line (funcall on-line line)))))
    (unless status
      (error "streaming response: no parseable status line"))
    status))

(defun parse-chunked-size-line (size-line)
  "Parse a chunked-transfer size token from SIZE-LINE. Strips
   chunk-extensions (RFC 7230 §4.1.1 — everything from ';' onwards),
   requires at least one hex digit, caps at 16 hex digits, and
   raises on parse failure. Returns the integer size (0 for the
   final chunk).

   Shared between stream-chunked-lines (plain streaming) and
   tls-stream-response (tls streaming) so both paths reject the
   same garbage inputs. Strict rejection matters here because a
   permissive parse ('xyz' → NIL, '-5' → -5) would silently exit
   the decoder loop as if the stream were complete — a parser-
   disagreement smuggling primitive against any stricter
   downstream that re-parses the body. The 16-digit cap mirrors
   decode-chunked-body's buffered-path guard: without it, a 1 MiB
   attacker-framed hex string lands in parse-integer, which is
   super-linear in digit count."
  (let* ((semi (position #\; size-line))
         (hex-end (or semi (length size-line)))
         (hex (string-trim '(#\Space #\Tab) (subseq size-line 0 hex-end))))
    (when (zerop (length hex))
      (error "chunked: empty chunk-size line"))
    (when (> (length hex) 16)
      (error "chunked: chunk-size too many hex digits (~d)" (length hex)))
    (loop for ch across hex
          unless (or (char<= #\0 ch #\9)
                     (char<= #\A (char-upcase ch) #\F))
          do (error "chunked: non-hex byte in chunk-size ~s" hex))
    (let ((n (parse-integer hex :radix 16)))
      (when (> n *max-outbound-response-size*)
        (error "chunked: chunk-size ~d exceeds response cap" n))
      n)))

(defun parse-chunked-size-bytes (bytes start end)
  "Byte-buffer entry point for PARSE-CHUNKED-SIZE-LINE. Used by
   tls-stream-response which accumulates the chunk-size line in a
   (unsigned-byte 8) fill-pointered buffer — this wrapper converts
   and calls the string-level parser so the strict acceptance set
   is identical on both sides of the tls/plaintext split."
  (parse-chunked-size-line
   (sb-ext:octets-to-string bytes :start start :end end
                                   :external-format :ascii)))

(defun stream-chunked-lines (r on-line)
  "Decode chunked transfer encoding via buffered reader R. Requires
   the zero-size chunk terminator (RFC 7230 §4.1) — a reader that
   runs out before seeing it raises, matching decode-chunked-body's
   truncation discipline so the async and streaming paths cannot
   diverge on incomplete input."
  (let ((line-buf (make-array 4096 :element-type '(unsigned-byte 8)
                                   :fill-pointer 0 :adjustable t))
        (terminated nil))
    (loop
      (let ((size-line (reader-read-crlf-line r)))
        (unless size-line
          (return))  ; stream EOF — handled by terminated check below
        ;; READER-READ-CRLF-LINE enforces strict CRLF (rejecting
        ;; bare CR and bare LF) + a *max-header-line-length* cap;
        ;; PARSE-CHUNKED-SIZE-LINE then rejects empty input, non-
        ;; hex bytes, and over-16 hex digits. Both layers mirror
        ;; DECODE-CHUNKED-BODY's buffered guards.
        (let ((chunk-size (parse-chunked-size-line size-line)))
          (when (zerop chunk-size)
            (setf terminated t)
            (return))
          (let ((consumed (reader-read-bytes r chunk-size line-buf on-line)))
            (when (< consumed chunk-size)
              (error "chunked stream: short chunk-data (~d of ~d bytes)"
                     consumed chunk-size)))
          ;; Strict trailing CRLF after chunk-data (RFC 7230 §4.1).
          ;; READER-READ-LINE would silently tolerate a bare LF or
          ;; garbage followed by LF — READER-EXPECT-CRLF requires
          ;; both bytes exactly, matching decode-chunked-body's
          ;; discipline on the buffered path.
          (reader-expect-crlf r))))
    (unless terminated
      (error "chunked: incomplete stream (no zero-size terminator)"))
    (when (> (fill-pointer line-buf) 0)
      (when on-line
        (funcall on-line (sb-ext:octets-to-string
                          line-buf :external-format :utf-8))))))

(defun connection-hint-for (inbound)
  "Compute the :CONNECTION-HINT value for FORMAT-RESPONSE based on
   INBOUND's parsed request and close-after-p state. Returns:
     :CLOSE      when CONNECTION-CLOSE-AFTER-P is T — the server has
                 decided to close and SHOULD stamp Connection: close
                 on the final message (RFC 7230 §6.1).
     :KEEP-ALIVE when the inbound is an HTTP/1.0 keep-alive session —
                 the only case where stamping Connection: keep-alive
                 on the wire is meaningful (HTTP/1.1's default is
                 keep-alive, so the header would be redundant).
     NIL         otherwise (typical HTTP/1.1 keep-alive).
   Used by COMPLETE-FETCH / DELIVER-FETCH-ERROR / the TLS happy and
   error paths / DELIVER-DNS-ERROR / HANDLE-CLIENT-READ so every
   response site echoes a consistent Connection header regardless
   of which path delivers."
  (let ((req (and inbound (connection-request inbound))))
    (cond ((not req) nil)
          ((connection-close-after-p inbound) :close)
          ((string= (http-request-version req) "1.0") :keep-alive)
          (t nil))))

(defun sync-close-after-p-from-response (inbound response)
  "Flip INBOUND's CONNECTION-CLOSE-AFTER-P to T when RESPONSE's
   headers carry any Connection: close token. Parallels the
   inbound-request parse in HANDLE-CLIENT-READ and the direct-
   dispatch sync there — without this step a fetch callback that
   sets Connection: close advertises the header on the wire but
   the server holds the socket open, framing-mismatch vs the
   client's expectation. Walks every 'connection' header via
   GET-HEADERS-equivalent (apps might ADD-RESPONSE-HEADER the
   same name twice) so any instance with 'close' wins.
   Byte-vector responses (pre-serialized from SERVE-STATIC or an
   app's hand-built bytes) are NOT scanned — the header bytes are
   opaque here and re-parsing them just to catch an embedded
   Connection: close would duplicate PARSE-HEADERS-BYTES. Apps
   that want close semantics on a byte-vector path must use an
   HTTP-RESPONSE struct instead, or close the connection
   explicitly via the handler-owned path. The self-heals gap
   when a byte-vector embeds Connection: close but the server
   doesn't close is bounded: the client eventually drops on
   idle, just later than the header advertised."
  (when (and inbound (typep response 'http-response))
    (let ((values (loop for (k . v) in (http-response-headers response)
                        when (string-equal k "connection") collect v)))
      (when (some (lambda (v) (header-has-token-p v "close"))
                  values)
        (setf (connection-close-after-p inbound) t)))))

(defun strip-body-for-head (bytes conn)
  "If CONN's parsed request method is HEAD, truncate BYTES at the
   CRLFCRLF header boundary so only status line + headers go on
   the wire (RFC 7231 §4.3.2). Returns BYTES unchanged when CONN
   is nil, its request is nil, the method is not HEAD, or no
   CRLFCRLF is found."
  (if (and conn
           (connection-request conn)
           (eq (http-request-method (connection-request conn)) :HEAD))
      (let ((end (scan-crlf-crlf bytes 0 (length bytes))))
        (if end (subseq bytes 0 (+ end 4)) bytes))
      bytes))

(defun awaiting-inbound-for (inbound-fd out-fd)
  "Return the inbound connection parked on OUT-FD, or NIL.

   The delivery paths resume an inbound by looking up a raw fd *number*
   that was recorded when the fetch was parked. Numbers are recycled: an
   inbound that closed while its outbound was still in flight frees its
   fd for the next accept, and delivering to whatever now answers to that
   number would queue one caller's response onto another caller's socket.

   Not reachable today — every route that closes an :AWAITING inbound goes
   through CLOSE-CONNECTION, which tears the paired outbound down with it.
   But that is a property of today's callers rather than of these
   functions, which is the reasoning CONNECTION-QUEUE-WRITE's own guard
   rejects, and the window widens with every long-lived parked state the
   roadmap adds.

   Refusing costs a delay, never a wrong delivery: an inbound this
   declines to answer is left parked, and the :AWAITING sweeper answers it
   504."
  (let ((inbound (lookup-connection inbound-fd)))
    (when (and inbound (= (connection-awaiting-fd inbound) out-fd))
      inbound)))

;;; ---------------------------------------------------------------------------
;;; Initiate outbound fetch
;;; ---------------------------------------------------------------------------

(defun initiate-fetch (conn epoll-fd fetch-req)
  "Start an outbound HTTP(S) request.
   CONN is the inbound connection to park.
   FETCH-REQ is the http-fetch-continuation descriptor.
   HTTP uses non-blocking epoll I/O. HTTPS dispatches to *https-fetch-fn*
   (blocking on the worker thread) — requires web-skeleton-tls."
  (handler-case
      (multiple-value-bind (scheme host port path)
          (parse-url (http-fetch-continuation-url fetch-req))
        (if (eq scheme :https)
            ;; HTTPS — blocking path via TLS hook
            (if *https-fetch-fn*
                (funcall *https-fetch-fn* conn epoll-fd fetch-req host port path)
                (error "HTTPS not available — load web-skeleton-tls"))
            ;; HTTP — non-blocking epoll path
            (initiate-http-fetch conn epoll-fd fetch-req host port path)))
    (error (e)
      (log-error "fetch setup failed: ~a" e)
      ;; Fire the cleanup sentinel so the app's :then closure runs
      ;; exactly once even on pre-connection errors (malformed URL,
      ;; DNS spawn failure, HTTPS not loaded).
      (handler-case
          (funcall (http-fetch-continuation-callback fetch-req) nil nil nil)
        (error (e2)
          (log-warn "fetch cleanup callback raised: ~a" e2)))
      (let ((error-response (strip-body-for-head
                             (format-response
                              (make-error-response 502)
                              :connection-hint (connection-hint-for conn))
                             conn)))
        (connection-queue-write conn error-response)
        (setf (connection-state conn) :write-response)
        (epoll-modify epoll-fd (connection-fd conn)
                     (logior +epollout+ +epollet+))))))

(defvar *dns-lookup-fn* nil
  "Async DNS dispatcher — set by src/server/dns.lisp at load time.
   Called with (CONN EPOLL-FD FETCH-REQ HOST PORT PATH) when HOST is
   not a numeric IP literal. The dispatcher parks CONN behind a getent
   subprocess and resumes via INITIATE-HTTP-FETCH-TO-ADDRESS on
   completion. The hook pattern keeps fetch.lisp free of forward
   references to dns.lisp.")

(defvar *handle-dns-ready-fn* nil
  "Async DNS read-ready handler — set by src/server/dns.lisp at load
   time. Called with (DNS-CONN EPOLL-FD) from HANDLE-OUTBOUND-EVENT
   when epoll reports EPOLLIN or EPOLLHUP on a :OUT-DNS connection.
   Second half of the DNS hook pair, mirroring *DNS-LOOKUP-FN*.")

(defun initiate-http-fetch-to-address (conn epoll-fd fetch-req host port path ip family)
  "Open a non-blocking TCP socket (FAMILY :INET or :INET6) to IP:PORT and
   send the outbound HTTP request. CONN is the parked inbound connection.
   HOST is the original hostname — used for the Host: header, not for
   routing. Called from INITIATE-HTTP-FETCH's numeric-IP fast path and
   from the DNS-ready callback in src/server/dns.lisp."
  (let ((socket (make-instance (if (eq family :inet)
                                   'sb-bsd-sockets:inet-socket
                                   'sb-bsd-sockets:inet6-socket)
                               :type :stream :protocol :tcp))
        (out-conn nil)
        (registered nil)
        (epoll-added nil)
        (done nil))
    (handler-case
        (unwind-protect
             (progn
               (set-nonblocking (socket-fd socket))
               (handler-case
                   (sb-bsd-sockets:socket-connect socket ip port)
                 (sb-bsd-sockets:socket-error (e)
                   ;; Non-blocking connect raises for EINPROGRESS (expected,
                   ;; the kernel completes via EPOLLOUT) and for immediate
                   ;; failures (EACCES, EADDRNOTAVAIL, EHOSTUNREACH cached).
                   ;; Re-raising the immediate failures here surfaces the
                   ;; real errno via the outer handler-case at INITIATE-FETCH
                   ;; instead of silently registering a doomed fd and
                   ;; waiting out *FETCH-TIMEOUT* in :awaiting.
                   ;; SOCKET-ERROR-ERRNO is internal to SB-BSD-SOCKETS —
                   ;; the :: accessor matches the one used in epoll.lisp's
                   ;; BLOCKING-CONNECT.
                   (let ((errno (sb-bsd-sockets::socket-error-errno e)))
                     (unless (or (= errno +einprogress+)
                                 (= errno +eagain+))
                       (error "connect: ~a" (errno-string errno))))))
               (let* ((out-fd (socket-fd socket))
                      (request-bytes (build-outbound-request
                                      (http-fetch-continuation-method fetch-req)
                                      host path
                                      :port port
                                      :headers (http-fetch-continuation-headers fetch-req)
                                      :body (http-fetch-continuation-body fetch-req))))
                 (setf out-conn (make-connection
                                 :fd out-fd
                                 :socket socket
                                 :state :out-connecting
                                 :outbound-p t
                                 :inbound-fd (connection-fd conn)
                                 :fetch-callback (http-fetch-continuation-callback fetch-req)
                                 :fetch-on-body (http-fetch-continuation-on-body fetch-req)
                                 :fetch-method (http-fetch-continuation-method fetch-req)
                                 :last-active (get-universal-time)))
                 (connection-queue-write out-conn request-bytes)
                 (register-connection out-conn)
                 (setf registered t)
                 (epoll-add epoll-fd out-fd (logior +epollout+ +epollet+))
                 (setf epoll-added t)
                 (setf (connection-state conn) :awaiting
                       (connection-awaiting-fd conn) out-fd)
                 (log-debug "fetch: fd ~d -> ~a :~d~a (outbound fd ~d, ~a)"
                            (connection-fd conn) host port path out-fd family)
                 (setf done t)))
          ;; Error path: if the error landed after REGISTER-CONNECTION but
          ;; before we finished wiring, tear down partial state. Symmetric
          ;; with ACCEPT-CONNECTION's flagged-unwind shape — an error
          ;; between register and epoll-add would otherwise leave a stale
          ;; entry in *CONNECTIONS* pointing at a bare fd that the epoll
          ;; fd has no record of, and the next sweep would trip over it.
          (unless done
            (when (and registered epoll-added out-conn)
              (ignore-errors
               (epoll-remove epoll-fd (connection-fd out-conn))))
            (when (and registered out-conn)
              (unregister-connection out-conn))))
      (error (e)
        (ignore-errors (sb-bsd-sockets:socket-close socket))
        (error e)))))

(defun initiate-http-fetch (conn epoll-fd fetch-req host port path)
  "Start a non-blocking outbound HTTP request. Fast path: HOST is a
   numeric IPv4 or IPv6 literal, skip DNS entirely and connect direct.
   Slow path: dispatch to *DNS-LOOKUP-FN* (provided by dns.lisp) which
   runs getent in a subprocess and resumes via
   INITIATE-HTTP-FETCH-TO-ADDRESS when the address is in hand.

   Both fast paths are gated on *FETCH-ADDRESS-FILTER*. They skip DNS,
   so a resolver-only check would let http://169.254.169.254/ — the most
   direct form of the attack the filter exists to stop — walk straight
   through. A refused literal raises, and INITIATE-FETCH's handler-case
   turns that into the same 502 + cleanup-sentinel path as any other
   pre-connection failure."
  (let* ((v4 (parse-ipv4-literal host))
         (v6 (unless v4 (parse-ipv6-literal host))))
    (cond
      (v4 (unless (fetch-address-allowed-p v4 :inet host)
            (error "fetch: address refused by policy: ~a" host))
          (initiate-http-fetch-to-address
           conn epoll-fd fetch-req host port path v4 :inet))
      (v6 (unless (fetch-address-allowed-p v6 :inet6 host)
            (error "fetch: address refused by policy: ~a" host))
          (initiate-http-fetch-to-address
           conn epoll-fd fetch-req host port path v6 :inet6))
      (*dns-lookup-fn*
       (funcall *dns-lookup-fn* conn epoll-fd fetch-req host port path))
      (t (error "no DNS resolver loaded")))))

;;; ---------------------------------------------------------------------------
;;; Outbound event handlers
;;; ---------------------------------------------------------------------------

(defun handle-outbound-event (conn epoll-fd flags)
  "Handle an epoll event on an outbound connection.

   Flag-dispatch order is deliberate: EPOLLIN is processed before
   EPOLLERR / EPOLLHUP. Linux delivers (EPOLLIN | EPOLLHUP) together
   when an upstream sends its final bytes and FINs in the same TCP
   segment — a common shape for close-delimited and chunked
   responses. The previous cond-first-match structure matched the
   HUP arm first and returned 502 even though a complete response
   was sitting in the kernel buffer. Now the read path runs first
   (draining to :eof and calling complete-fetch), and the HUP arm
   only fires as fatal if the connection is still alive after the
   drain — meaning the response really was incomplete.

   State-dispatch for the individual flag branches is permissive:
   stale EPOLLIN for a connection whose state advanced to :out-write
   earlier in the same batch, or stale EPOLLOUT for a connection
   that moved to :out-read, silently no-op instead of raising into
   the outer handler-case's 502 path."
  (handler-case
      (cond
        ;; DNS lookup: EPOLLIN or EPOLLHUP both mean drain-and-parse.
        ;; getent writes its output and then exits, which may surface
        ;; either as readable data (EPOLLIN) or as a pipe close hint
        ;; (EPOLLHUP). The read path handles EOF cleanly either way.
        ;; Dispatched via *HANDLE-DNS-READY-FN* so fetch.lisp has no
        ;; forward reference to dns.lisp.
        ((eq (connection-state conn) :out-dns)
         (funcall *handle-dns-ready-fn* conn epoll-fd))
        (t
         ;; 1. Drain readable bytes first so a batched IN+HUP sees
         ;;    the bytes before the HUP is treated as fatal.
         (when (logtest flags +epollin+)
           (case (connection-state conn)
             (:out-read (handle-outbound-read conn epoll-fd))
             ;; Stale EPOLLIN in another state — ignore silently.
             (otherwise nil)))
         ;; 2. Handle writable. Gated on (lookup-connection ...) so
         ;;    we skip if step 1 already completed and tore down
         ;;    the out-conn.
         (when (and (logtest flags +epollout+)
                    (lookup-connection (connection-fd conn)))
           (case (connection-state conn)
             (:out-connecting (handle-outbound-connect conn epoll-fd))
             (:out-write      (handle-outbound-write conn epoll-fd))
             (otherwise nil)))
         ;; 3. HUP/ERR after draining. If we still have a live
         ;;    :out-read connection, give it one more shot at
         ;;    completion — handle-outbound-read's nb-read will
         ;;    see :eof on a HUP'd fd and route to complete-fetch.
         ;;    If that still leaves the connection alive, the
         ;;    response really is incomplete and we deliver 502.
         (when (and (or (logtest flags +epollerr+)
                        (logtest flags +epollhup+))
                    (lookup-connection (connection-fd conn)))
           (cond
             ((eq (connection-state conn) :out-read)
              (handle-outbound-read conn epoll-fd)
              (when (lookup-connection (connection-fd conn))
                (deliver-fetch-error conn epoll-fd
                                     "connection error after partial read")))
             (t
              (deliver-fetch-error conn epoll-fd "connection error"))))))
    (error (e)
      (log-error "outbound error fd ~d: ~a" (connection-fd conn) e)
      ;; Skip the error-delivery if the connection was already torn
      ;; down (e.g. complete-fetch ran earlier in this call and
      ;; close-outbound unregistered the fd). Avoids double-delivery
      ;; of a 502 on top of an already-queued real response.
      (when (lookup-connection (connection-fd conn))
        (deliver-fetch-error conn epoll-fd "outbound request failed")))))

(defun handle-outbound-connect (conn epoll-fd)
  "Check if non-blocking connect succeeded, then start writing the request."
  (let ((err (get-socket-option-int (connection-fd conn)
                                     +sol-socket+ +so-error+)))
    (if (zerop err)
        (progn
          ;; Connect succeeded — start writing request (already queued)
          (setf (connection-state conn) :out-write)
          (handle-outbound-write conn epoll-fd))
        ;; Connect failed
        (deliver-fetch-error conn epoll-fd
                             (format nil "connect failed: errno ~d" err)))))

(defun handle-outbound-write (conn epoll-fd)
  "Flush the outbound HTTP request. When done, switch to reading the response."
  (let ((result (connection-on-write conn)))
    (case result
      (:done
       (setf (connection-state conn) :out-read
             (connection-read-pos conn) 0)
       (epoll-modify epoll-fd (connection-fd conn)
                    (logior +epollin+ +epollet+)))
      ;; :continue — more bytes to write
      )))

(defun fetch-resume (conn)
  "Resume reading an outbound response that ON-BODY paused.

   Re-arms EPOLLIN on the outbound connection. The bytes that arrived
   while it was paused are in the kernel, not in user space, so the
   edge-triggered re-arm does fire — which is exactly the case the
   framework documents as *not* holding for buffered user-space data.

   CONN is the outbound connection ON-BODY was called for. A no-op if it
   was never paused, so a producer that calls this on every pass rather
   than tracking state is not punished for it."
  (when (connection-fetch-paused conn)
    (setf (connection-fetch-paused conn) nil)
    (when *epoll-fd*
      (epoll-modify *epoll-fd* (connection-fd conn)
                    (logior +epollin+ +epollet+))))
  (values))

(defun handle-outbound-read (conn epoll-fd)
  "Read the outbound HTTP response. When complete, deliver to the inbound connection."
  (let ((result (connection-read-available conn)))
    (case result
      ((:eof :ok-eof)
       ;; Server closed — for a close-delimited response that IS the
       ;; framing. :OK-EOF is the same event with the last bytes attached;
       ;; see CONNECTION-READ-AVAILABLE for why they are told apart.
       (complete-fetch conn epoll-fd))
      (:full
       ;; Buffer hit *MAX-OUTBOUND-RESPONSE-SIZE*. Route through
       ;; DELIVER-FETCH-ERROR so the inbound gets a 502 and the
       ;; fetch callback's cleanup sentinel fires. A
       ;; Content-Length-framed response would otherwise hit
       ;; COMPLETE-FETCH's truncation guard and a chunked one would
       ;; hit DECODE-CHUNKED-BODY's missing-terminator raise, but
       ;; a Connection: close-framed response has neither guard —
       ;; routing all three shapes through deliver-fetch-error on
       ;; :full keeps the cap a hard boundary in every framing.
       (deliver-fetch-error
        conn epoll-fd
        (format nil "response exceeds ~d bytes (cap)"
                *max-outbound-response-size*)))
      (:again nil)  ; wait for more data
      (:ok
       ;; Got data — is the response framed-complete yet? OUTBOUND-
       ;; RESPONSE-COMPLETE-P is the one definition of "done", shared with
       ;; the TLS path. CHUNK-SCAN-POS carries the chunked walk's resume
       ;; offset across reads so a dribbling upstream is walked once, not
       ;; rescanned from the top on every wake-up.
       ;;
       ;; Completion follows the framing, not the socket's lifetime.
       ;; Waiting for EOF worked only because outbound requests default to
       ;; Connection: close, and even then it cost a round trip — the
       ;; whole response sits in our buffer while we wait for the
       ;; upstream's FIN. An app that overrides Connection to keep-alive
       ;; got an upstream that never closes, and the fetch hung until the
       ;; :awaiting reaper dropped the inbound with no response at all.
       ;;
       ;; Close-delimited responses (no CL, no TE) still complete via the
       ;; :eof branch above: for those, EOF genuinely is the framing.
       (let ((pause nil))
         (multiple-value-bind (complete next-scan)
             (outbound-response-complete-p
              (connection-read-buf conn)
              (connection-read-pos conn)
              (connection-fetch-method conn)
              (connection-chunk-scan-pos conn)
              (when (connection-fetch-on-body conn)
                (lambda (buf start end)
                  ;; SUBSEQ rather than the shared buffer: the app keeps
                  ;; whatever it is handed, and the read buffer is reused
                  ;; on the next wake-up.
                  ;; Last answer in a pass wins. A caller that pauses on
                  ;; one chunk and continues on the next ends the pass
                  ;; reading, which is the useful reading of a producer
                  ;; that drained its own backlog partway through.
                  (setf pause
                        (eq (funcall (connection-fetch-on-body conn)
                                     conn (subseq buf start end))
                            :pause)))))
           (setf (connection-chunk-scan-pos conn) next-scan)
           (cond
             (complete (complete-fetch conn epoll-fd))
             ;; Backpressure: leave the bytes in the kernel and let the
             ;; upstream's window fill, which is the disposition a relay
             ;; wants — the client has not misbehaved, it is on a slower
             ;; link. FETCH-RESUME has why the re-arm works.
             (pause
              (setf (connection-fetch-paused conn) t)
              (epoll-modify epoll-fd (connection-fd conn) +epollet+)))))))))

;;; ---------------------------------------------------------------------------
;;; Chunked body decoding (for buffered responses)
;;; ---------------------------------------------------------------------------

(defun response-chunked-p (headers)
  "Return T if HEADERS indicate chunked transfer encoding.
   Scans every Transfer-Encoding header in the alist — not just the
   first — because split headers are valid HTTP (RFC 7230 §3.2.2)
   and reverse proxies can produce them. Uses token-aware matching
   (RFC 7230 §3.2.6) so 'identity' or 'unchunked-foo' don't
   false-match on substring 'chunked'."
  (loop for (name . value) in headers
        thereis (and (string-equal name "transfer-encoding")
                     (header-has-token-p value "chunked"))))

(defun chunked-body-complete-p (buf start end &optional (resume start) on-data)
  "Return (values COMPLETE-P NEXT-RESUME) for the chunked body in
   BUF[START..END). COMPLETE-P is T once the zero-size chunk header has
   arrived. Walks the chunk framing, skipping *over* chunk data rather
   than scanning it, so the cost is proportional to the number of chunks
   and not to the bytes.

   RESUME is a NEXT-RESUME returned by an earlier call — an offset that
   is known to sit on a chunk-header boundary with everything before it
   already validated. A caller polling a growing buffer threads it back
   in, so each chunk is walked exactly once over the life of a transfer
   instead of the whole body being rescanned on every read. Without it,
   an upstream that dribbles N chunks costs O(N^2) header steps, which an
   adversarial (if reachable) upstream could turn into real CPU burn
   inside the response-size cap. Default RESUME = START scans from
   scratch, which is what a one-shot caller wants.

   This is a 'do we have it all yet?' predicate, not a validator, and it
   is deliberately permissive about malformed framing: DECODE-CHUNKED-BODY
   is the authority and rejects bad framing loudly when the response is
   delivered (which becomes a 502). NIL here only ever means 'keep
   reading' — so a too-strict predicate would hang, while a too-lax one
   merely reaches a loud decode error. Lean lax.

   It stops at the zero-size chunk header rather than at the trailing
   CRLF, which is exactly where DECODE-CHUNKED-BODY stops too (trailers
   are not consumed), so the two agree on the completion point.

   ON-DATA, when supplied, is called (BUF START END) once per chunk whose
   framing this walk has just proved whole — the same visit, handing the
   bytes back instead of only stepping over them. Never called twice for
   a chunk, because RESUME means the walk never revisits one."
  (let ((pos (max start resume)))
    (loop
      ;; BOUNDARY is the start of the chunk header about to be parsed:
      ;; everything before it is validated framing, so it is the offset
      ;; handed back for the next call to resume from.
      (let ((boundary pos)
            (size 0)
            (digits 0)
            (found nil))
        ;; chunk-size — at least one hex digit, capped like the decoder's.
        (loop
          (when (>= pos end) (return))
          (let ((digit (hex-digit-value (aref buf pos))))
            (unless digit (return))
            (incf digits)
            (when (> digits 16)
              (return-from chunked-body-complete-p (values nil boundary)))
            (setf size (+ (ash size 4) digit)
                  found t)
            (incf pos)))
        (unless found
          (return (values nil boundary)))
        ;; Skip any chunk-extensions; the size line's LF must have landed.
        (let ((lf (position 10 buf :start pos :end end)))
          (unless lf (return (values nil boundary)))
          (setf pos (1+ lf)))
        ;; Zero-size chunk header = end of body.
        (when (zerop size)
          (return (values t boundary)))
        ;; Skip the chunk data and its trailing CRLF. Note this jumps the
        ;; data rather than scanning it, which is what keeps a body whose
        ;; *contents* happen to contain "0\\r\\n\\r\\n" from being mistaken
        ;; for a terminator — a naive suffix check would truncate there.
        (let ((data-start pos))
          (incf pos size)
          (incf pos 2)
          (when (> pos end)
            (return (values nil boundary)))
          ;; The walk has already proved this chunk whole; ON-DATA is how
          ;; the bytes leave without a second pass over them.
          (when on-data
            (funcall on-data buf data-start (+ data-start size))))))))

(defun outbound-response-complete-p (buf end method &optional (chunk-scan 0) on-data)
  "Return (values COMPLETE-P NEXT-CHUNK-SCAN) for the outbound HTTP
   response accumulated in BUF[0..END), given the request METHOD. Thread
   NEXT-CHUNK-SCAN back in on the following call so a chunked body is
   walked once across a growing buffer (see CHUNKED-BODY-COMPLETE-P).

   One definition of 'the response is done', shared by the non-blocking
   plain-HTTP read path (HANDLE-OUTBOUND-READ) and the blocking TLS one
   (TLS-READ-ALL). They used to disagree: plain HTTP recognized the
   chunked terminator while TLS read to EOF, so the same upstream could
   be handled cleanly over http:// and stall over https://. A single
   predicate is the only way that divergence stays fixed.

   Framing, in RFC 7230 §3.3.3 order:
     HEAD           — no body ever (§4.3.2), even when the upstream
                      echoes the GET body's Content-Length. Done at
                      headers.
     Transfer-Encoding present — TE wins over Content-Length. Done at the
                      zero-size chunk header, but only once the encoding
                      is confirmed to actually be chunked: a TE that is
                      not chunked is framed by EOF, and completing early
                      there would truncate.
     Content-Length — done when that many body bytes have landed.
     Neither        — close-delimited. NIL forever: EOF *is* the framing,
                      and the caller's EOF branch completes it.

   Known corner, deliberately not optimized: if an upstream sends a
   Transfer-Encoding that is *not* chunked (already malformed — RFC 7230
   §3.3.1 requires chunked to be the final encoding) and its body bytes
   happen to walk as complete chunk framing, the confirming header parse
   re-runs on every subsequent read until EOF, because nothing remembers
   that the answer was already 'not chunked'. The cost is one header parse
   per read on a response that is broken anyway, and remembering the
   answer would mean a connection slot to carry it. Not worth the state."
  ;; Step over any 1xx interim blocks first. This has to happen HERE and
  ;; not only in COMPLETE-FETCH: the framing decision below reads
  ;; Transfer-Encoding / Content-Length out of the header region, and an
  ;; interim block carries neither. Anchoring on the interim would make a
  ;; CL-framed 200 look close-delimited, so the predicate would answer NIL
  ;; forever and the fetch would hang until *FETCH-TIMEOUT* on any upstream
  ;; that keeps the connection alive.
  (let* ((start (skip-interim-responses buf 0 end))
         (header-end (scan-crlf-crlf buf start end)))
    (unless header-end
      (return-from outbound-response-complete-p (values nil chunk-scan)))
    (let* ((body-start (+ header-end 4))
           (te-present (scan-transfer-encoding buf header-end start))
           (content-length (unless te-present
                             (scan-content-length buf header-end start))))
      (cond
        ((eq method :HEAD)
         (values t chunk-scan))
        (te-present
         (multiple-value-bind (complete next)
             (chunked-body-complete-p buf body-start end
                                      (max body-start chunk-scan)
                                      on-data)
           ;; The header parse only runs on the read that actually
           ;; completes — the cheap framing walk gates it.
           (values (and complete
                        (let ((first-crlf (scan-crlf buf start header-end)))
                          (and first-crlf
                               (response-chunked-p
                                (parse-headers-bytes buf (+ first-crlf 2)
                                                     (+ header-end 4)))
                               t)))
                   next)))
        (content-length
         (values (>= (- end body-start) content-length) chunk-scan))
        (t
         (values nil chunk-scan))))))

(defun decode-chunked-body (buf start end)
  "Decode chunked transfer encoding from BUF[START..END).
   Returns a byte vector with the chunk framing stripped.

   Raises on truncation: the zero-size chunk header (0 CRLF) is the
   only permitted exit. A response that runs out of bytes before
   the terminator (MITM RST, short read, upstream crash mid-body)
   is reported as an error so the caller's outer handler-case can
   convert it into a 502 or fire the fetch cleanup callback — the
   same discipline as the Content-Length truncation guard in
   COMPLETE-FETCH and HTTPS-FETCH, applied to the chunked path
   which has no Content-Length to compare against.

   Also requires strict CRLF after both chunk-size and chunk-data
   per RFC 7230 §4.1. Lax trailing CRLF is a smuggling primitive
   against a strict downstream that re-parses the body bytes."
  (let ((out (make-array (- end start) :element-type '(unsigned-byte 8)
                                        :fill-pointer 0))
        (pos start)
        (terminated nil))
    (loop
      ;; Parse chunk size (hex digits)
      (let ((size 0)
            (found nil)
            (digits 0))
        (loop while (< pos end)
              do (let ((digit (hex-digit-value (aref buf pos))))
                   (if digit
                       (progn (incf digits)
                              (when (> digits 16)
                                (error "chunked: chunk-size too many hex digits"))
                              (setf size (+ (ash size 4) digit)
                                    found t)
                              (incf pos))
                       (return))))
        (unless found
          ;; Ran out of bytes before seeing a chunk-size header.
          ;; Either the upstream closed mid-stream (truncation) or
          ;; the response never included the 0-chunk terminator.
          ;; Both are incomplete.
          (return))
        ;; RFC 7230 §4.1.1 lists only ';' (chunk-ext start), SP /
        ;; HTAB (BWS tolerance), and CR (start of CRLF) as legal
        ;; bytes after the hex digits of a chunk-size. Anything
        ;; else is rejected symmetrically with PARSE-CHUNKED-SIZE-LINE
        ;; on the streaming path — without this check '5g\r\n'
        ;; would silently parse as chunk-size 5 with 'g' as an
        ;; implicit extension and become a smuggling primitive
        ;; against a stricter downstream.
        (when (< pos end)
          (let ((b (aref buf pos)))
            (unless (or (= b 59)  ; ';'
                        (= b 32)  ; SP
                        (= b 9)   ; HTAB
                        (= b 13)) ; CR
              (error "chunked: invalid byte 0x~2,'0x after chunk-size" b))))
        ;; Require strict CRLF after the chunk-size (RFC 7230 §4.1).
        ;; Any chunk extensions between the hex digits and CRLF are
        ;; passed through untouched — we scan for the LF and verify
        ;; the preceding byte is CR so a bare-LF or truncated line
        ;; cannot slip through as "5junk<LF>data".
        (let ((eol pos))
          (loop while (and (< eol end) (/= (aref buf eol) 10))
                do (incf eol))
          (unless (and (< eol end)
                       (> eol pos)
                       (= (aref buf (1- eol)) 13))
            (error "chunked: expected CRLF after chunk-size"))
          (setf pos (1+ eol)))
        ;; Zero-size chunk = end (the only clean exit from this loop).
        (when (zerop size)
          (setf terminated t)
          (return))
        ;; Copy chunk data. Short-read here is truncation: we declared
        ;; SIZE bytes and need exactly that many.
        (when (> (+ pos size) end)
          (error "chunked: short chunk-data (~d of ~d bytes)"
                 (- end pos) size))
        (loop for i from pos below (+ pos size)
              do (vector-push-extend (aref buf i) out))
        (incf pos size)
        ;; Require strict CRLF after chunk-data (RFC 7230 §4.1).
        ;; The lax \r-or-\n-or-nothing accept-anything behaviour was
        ;; a smuggling primitive: a response shaped 5\r\nhellonext...
        ;; (no CRLF between the data and the next chunk-size) would
        ;; be decoded differently by web-skeleton and a strict
        ;; downstream that re-parsed the body bytes.
        (unless (and (<= (+ pos 2) end)
                     (= (aref buf pos) 13)
                     (= (aref buf (1+ pos)) 10))
          (error "chunked: expected CRLF after chunk-data"))
        (incf pos 2)))
    (unless terminated
      (error "chunked: incomplete response (no zero-size terminator)"))
    (subseq out 0 (fill-pointer out))))

;;; ---------------------------------------------------------------------------
;;; Deliver fetch result to the parked inbound connection
;;; ---------------------------------------------------------------------------

(defun complete-fetch (out-conn epoll-fd)
  "Parse the outbound response and deliver it to the parked inbound connection."
  (let* ((buf (connection-read-buf out-conn))
         (pos (connection-read-pos out-conn))
         ;; Step over any 1xx interim blocks (RFC 7231 §6.2) before
         ;; locating the header boundary. A CDN's unsolicited 103 Early
         ;; Hints would otherwise become the fetch's status and headers,
         ;; and the real response would be dropped silently — see
         ;; SKIP-INTERIM-RESPONSES.
         (start (skip-interim-responses buf 0 pos))
         (header-end (scan-crlf-crlf buf start pos)))
    ;; Gate on complete headers. DEPLOYMENT.md's fetch callback
    ;; contract promises the happy path fires with an integer
    ;; status and the cleanup sentinel is (NIL NIL NIL) — never
    ;; a zero status. Apps pattern-matching on (if status ...)
    ;; treat 0 as truthy, route to the happy branch, and blow up
    ;; interpreting the integer 0 as an HTTP status. Incomplete
    ;; headers go through DELIVER-FETCH-ERROR so the inbound
    ;; gets a 502 and the callback fires its cleanup sentinel.
    (unless header-end
      (deliver-fetch-error out-conn epoll-fd
                           "upstream response has no parseable headers")
      (return-from complete-fetch))
    (let* ((status (parse-response-status buf start pos))
           (body-start (+ header-end 4))
           (headers (let ((first-crlf (scan-crlf buf start header-end)))
                      (when first-crlf
                        (parse-headers-bytes buf (+ first-crlf 2)
                                             (+ header-end 4)))))
           (chunked-p (response-chunked-p headers))
           ;; RFC 7230 §3.3.3 rule 3: any TE present means CL is
           ;; ignored — read-until-close, not CL-framed.
           (te-present (scan-transfer-encoding buf header-end start))
           (content-length (unless te-present
                             (scan-content-length buf header-end start))))
    ;; Status line must parse too. PARSE-RESPONSE-STATUS returns
    ;; NIL on a malformed status line ('HTTP/1.1 ABC OK', status
    ;; digits out of range, missing version, etc.) — same 'never
    ;; a zero status' contract as the header-end gate above.
    (unless status
      (deliver-fetch-error out-conn epoll-fd
                           "upstream status line unparseable")
      (return-from complete-fetch))
    ;; Truncation guard: if the upstream declared a Content-Length
    ;; and we hit EOF before receiving that many body bytes, the
    ;; response is incomplete. Passing the partial body to the
    ;; callback would silently deliver truncated data to the app —
    ;; an especially ugly failure for MITM'd HTTPS fetches where
    ;; the attacker can RST at any point. Abort through
    ;; deliver-fetch-error so the callback fires its cleanup path
    ;; and the inbound gets a 502 instead of a short body.
    ;;
    ;; Skipped for 204/304 — these carry CL but have no body
    ;; (RFC 7230 §3.3.3 rule 1, RFC 7232 §4.1). Also skipped for
    ;; HEAD — RFC 7231 §4.3.2: upstream echoes the CL of the GET
    ;; body but MUST NOT send a body. The request method lives on
    ;; out-conn's fetch-method slot (set from the continuation in
    ;; initiate-http-fetch-to-address).
    ;;
    ;; 1xx is absent from this set, and from the body-end forcing
    ;; below, because SKIP-INTERIM-RESPONSES has already consumed every
    ;; complete interim: STATUS here is either >= 200 or NIL, and NIL
    ;; was rejected by the gate above. Testing for 1xx would be dead
    ;; code that reads as though an interim could be a final status —
    ;; the belief that produced the defect the skip exists to fix.
    (when (and content-length
               (not (or (= status 204) (= status 304)))
               (not (eq (connection-fetch-method out-conn) :HEAD))
               (< (- pos body-start) content-length))
      (deliver-fetch-error out-conn epoll-fd
                           (format nil "short body: ~d of ~d bytes"
                                   (- pos body-start) content-length))
      (return-from complete-fetch))
    ;; BODY-START is unconditionally (+ header-end 4) past the
    ;; header-end gate above — it is always a positive fixnum at
    ;; this point, so no further nil-guards on BODY-START are
    ;; needed.
    (let* ((body-end (if content-length
                         (min pos (+ body-start content-length))
                         pos))
           ;; 204/304 MUST NOT have a body (RFC 7230 §3.3.3 rule 1).
           ;; HEAD responses MUST NOT include a body (RFC 7231 §4.3.2)
           ;; regardless of what the upstream sent. Force empty on both.
           ;; 1xx cannot reach here — see the note on the guard above.
           (body-end (if (or (= status 204) (= status 304)
                             (eq (connection-fetch-method out-conn) :HEAD))
                         body-start
                         body-end))
           (raw-body (when (> body-end body-start)
                       (subseq buf body-start body-end)))
           ;; NIL only when ON-BODY *actually* delivered the bytes, which
           ;; is the chunked path and no other — ON-DATA is threaded into
           ;; CHUNKED-BODY-COMPLETE-P's walk and nowhere else. Keying this
           ;; on the callback merely being supplied dropped the body of
           ;; every Content-Length and close-delimited response: no chunks
           ;; delivered, NIL in :THEN, nothing raised. That is the majority
           ;; of responses in the wild, and the caller does not choose the
           ;; framing — the same origin switches by response size or by
           ;; whatever proxy is in front, so a relay would work against one
           ;; upstream and come back empty against the next.
           (body (cond
                   ((and (connection-fetch-on-body out-conn) chunked-p) nil)
                   ((and raw-body chunked-p)
                    (decode-chunked-body raw-body 0 (length raw-body)))
                   (t raw-body)))
           ;; Call the user's callback.
           (callback (connection-fetch-callback out-conn))
           (inbound-fd (connection-inbound-fd out-conn))
           ;; Captured before CLOSE-OUTBOUND nulls it — AWAITING-INBOUND-FOR
           ;; needs it to confirm the inbound it finds is the one that
           ;; parked on this outbound rather than a reuse of its number.
           (out-fd (connection-fd out-conn)))
      ;; Clear the slot so close-outbound's cleanup-firing path does
      ;; not re-invoke the callback on the happy path. We already
      ;; captured the actual callback into the CALLBACK local above.
      (setf (connection-fetch-callback out-conn) nil)
      (close-outbound out-conn epoll-fd)
      ;; Find and resume inbound connection. If the inbound vanished
      ;; between :awaiting parking and now (drain race, idle reap,
      ;; I/O error on the inbound fd), we can't deliver a response
      ;; anywhere — but the contract is still "callback fires exactly
      ;; once per fetch lifetime", so fire the cleanup sentinel here
      ;; before returning so app state gets released.
      (let ((inbound (awaiting-inbound-for inbound-fd out-fd)))
        (if inbound
            (handler-case
                (let ((response (funcall callback
                                         status (or headers nil)
                                         (or body nil))))
                  ;; Sync close-after-p from the callback's response BEFORE
                  ;; computing the connection-hint — a handler-set
                  ;; Connection: close flips close-after-p to T so the
                  ;; hint resolves to :CLOSE and the server stamps the
                  ;; header on the wire instead of leaving the framing
                  ;; implicit.
                  (sync-close-after-p-from-response inbound response)
                  ;; Queue the response on the inbound connection. On
                  ;; HEAD, FORMAT-RESPONSE emits headers only via
                  ;; :HEAD-ONLY-P — saves the full body encode + the
                  ;; downstream STRIP-BODY-FOR-HEAD subseq, which on a
                  ;; large response body doubled the peak allocation.
                  ;; Byte-vector responses still go through the strip
                  ;; because they're already-serialized blocks.
                  (let* ((head-p (and (connection-request inbound)
                                      (eq (http-request-method
                                           (connection-request inbound))
                                          :HEAD)))
                         (bytes (cond
                                  ((typep response '(simple-array (unsigned-byte 8) (*)))
                                   (strip-body-for-head response inbound))
                                  ((typep response 'http-fetch-continuation)
                                   ;; Chained fetch — initiate another outbound call
                                   (initiate-fetch inbound epoll-fd response)
                                   (return-from complete-fetch))
                                  (t (format-response
                                      response
                                      :connection-hint
                                      (connection-hint-for inbound)
                                      :head-only-p head-p)))))
                    (connection-queue-write inbound bytes)
                    (setf (connection-state inbound) :write-response
                          (connection-awaiting-fd inbound) -1
                          (connection-last-active inbound) (get-universal-time))
                    (epoll-modify epoll-fd (connection-fd inbound)
                                  (logior +epollout+ +epollet+))
                    (log-debug "fetch: resumed fd ~d" inbound-fd)))
              (error (e)
                (log-error "fetch callback error: ~a" e)
                (let ((err-bytes (strip-body-for-head
                                  (format-response
                                   (make-error-response 500)
                                   :connection-hint
                                   (connection-hint-for inbound))
                                  inbound)))
                  (connection-queue-write inbound err-bytes)
                  (setf (connection-state inbound) :write-response
                        (connection-awaiting-fd inbound) -1
                        (connection-last-active inbound) (get-universal-time))
                  (epoll-modify epoll-fd (connection-fd inbound)
                                (logior +epollout+ +epollet+)))))
            ;; Inbound vanished between :awaiting parking and now
            ;; (drain race, idle reap, I/O error). Fire the cleanup
            ;; sentinel so app state gets released — we can't deliver
            ;; a response anywhere but the contract is still
            ;; "callback fires once per fetch lifetime".
            (handler-case (funcall callback nil nil nil)
              (error (e)
                (log-warn "fetch cleanup callback raised: ~a" e)))))))))

(defun deliver-fetch-error (out-conn epoll-fd message)
  "Deliver a 502 error to the inbound connection and clean up."
  (log-warn "fetch error fd ~d: ~a" (connection-fd out-conn) message)
  (let ((inbound-fd (connection-inbound-fd out-conn))
        (out-fd (connection-fd out-conn)))
    (close-outbound out-conn epoll-fd)
    (let ((inbound (awaiting-inbound-for inbound-fd out-fd)))
      (when inbound
        (let ((err-bytes (strip-body-for-head
                         (format-response
                          (make-error-response 502)
                          :connection-hint (connection-hint-for inbound))
                         inbound)))
          (connection-queue-write inbound err-bytes)
          (setf (connection-state inbound) :write-response
                (connection-awaiting-fd inbound) -1
                (connection-last-active inbound) (get-universal-time))
          (epoll-modify epoll-fd (connection-fd inbound)
                       (logior +epollout+ +epollet+)))))))

(defun close-outbound (conn epoll-fd)
  "Close and unregister an outbound connection. Reaps any attached
   getent DNS subprocess via MAYBE-REAP-DNS-PROCESS — a no-op for
   TCP outbound connections that never had a DNS phase.

   If CONN still carries a :FETCH-CALLBACK, fire it once with
   (NIL NIL NIL) before teardown so application-level cleanup
   (releasing DB handles, closing metric spans, decrementing
   circuit breakers, returning rate-limit budget) runs exactly
   once per fetch. COMPLETE-FETCH clears the slot before calling
   close-outbound so the happy path does not double-fire. The
   callback is wrapped in HANDLER-CASE: a raising cleanup hook
   is logged at warn level and must not block teardown."
  (let ((fd (connection-fd conn)))
    (when (>= fd 0)
      (let ((callback (connection-fetch-callback conn)))
        (when callback
          (setf (connection-fetch-callback conn) nil)
          (handler-case (funcall callback nil nil nil)
            (error (e)
              (log-warn "fetch cleanup callback raised: ~a" e)))))
      (ignore-errors (epoll-remove epoll-fd fd))
      (unregister-connection conn)
      (maybe-reap-dns-process conn)
      (connection-close conn)
      (log-debug "fetch: closed outbound fd ~d" fd))))
