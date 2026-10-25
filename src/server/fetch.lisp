(in-package :web-skeleton)

;;; ===========================================================================
;;; HTTP Client (outbound fetch)
;;;
;;; Integrates with the event loop — outbound connections are registered
;;; with the same epoll fd and processed alongside inbound connections.
;;; Nothing on the event loop's path blocks the worker: a name is resolved
;;; by a getent subprocess whose pipe joins the same epoll
;;; (INITIATE-DNS-LOOKUP), and https:// runs as a transport under the same
;;; reads and writes (TLS-SETUP-OUTBOUND). The blocking pair —
;;; RESOLVE-HOST-BLOCKING and HTTPS-FETCH-STREAM — belongs to
;;; HTTP-FETCH-STREAM further down, which is a blocking entry point by
;;; design and says so there.
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
  ;; (CONN BYTES) called per chunk as the response arrives, over either
  ;; scheme — the framing decides, not the transport. NIL buffers the
  ;; whole body instead.
  (on-body  nil    :type (or null function))
  ;; :HTTP or :HTTPS, filled in by INITIATE-FETCH from the parsed URL.
  ;; It lives here rather than being re-derived because the DNS path
  ;; resumes several call frames later, carrying this struct and nothing
  ;; else, and a second PARSE-URL there would be a second chance to
  ;; disagree with the first about what the caller asked for.
  (scheme   :http  :type keyword)
  ;; :INBOUND when a handler returned this and an inbound will be parked on
  ;; it; :DETACHED when FETCH-INTO started it against a connection the app
  ;; already owns. Carried here for the same reason SCHEME is — the DNS
  ;; path resumes several frames later holding this struct and nothing
  ;; else, and re-deriving it there would be a second chance to disagree
  ;; with the first answer.
  (sink     :inbound :type keyword)
  ;; The rest is a fetch's life once started, which the application holds
  ;; this struct to ask about — FETCH-SEND and FETCH-FINISH take it.
  ;;
  ;; T when :BODY was :STREAM: the request body is the application's to
  ;; send, a piece at a time, and CONTENT-LENGTH is its declared length,
  ;; or NIL for a body sent chunked.
  (stream-body nil :type boolean)
  (content-length nil :type (or null (integer 0)))
  ;; The connection carrying the fetch: its :OUT-DNS connection while the
  ;; name resolves, then its outbound. NIL before it starts and once it has
  ;; ended.
  (carrier nil)
  ;; The connection it was started for, the one a detached fetch fetches
  ;; into or an inbound one is parked on; -1 before it starts.
  (target-fd -1 :type fixnum)
  ;; A streamed body's pieces sent before the outbound exists, framed and
  ;; oldest first, which become its write queue after the request head.
  (pending nil :type list)
  (pending-bytes 0 :type fixnum)
  ;; Body bytes the application has sent, unframed — the count a declared
  ;; Content-Length is held to.
  (sent 0 :type unsigned-byte)
  ;; FETCH-FINISH has been called: the body is complete.
  (finished nil :type boolean)
  ;; T once the fetch is over, however it ended. A streamed fetch's
  ;; FETCH-SEND answers :ENDED from then on.
  (ended nil :type boolean)
  ;; What :THEN returned, when it ran for a detached fetch, kept for a
  ;; request that is answered with the fetch afterwards (see ADOPT-FETCH).
  (answered nil :type boolean)
  (answer nil))

(defparameter *fetch-timeout* 30
  "Seconds, whole: the fetch path adds it to GET-UNIVERSAL-TIME, and
   START-SERVER refuses a fraction. A total on the HTTP-FETCH path, both
   schemes: the inbound
   parks in :AWAITING and the reaper bounds the whole exchange, handshake
   included, however many reads and writes it took.

   Per phase everywhere else, which means HTTP-FETCH-STREAM and the
   blocking setup paths only. There it bounds DNS, connect, and each
   individual socket read separately, so a trickling upstream never trips
   it. README Limitations has what that costs.

   A request body sent as it is produced (:BODY :STREAM) is bounded by its
   progress instead while it goes out: each piece the application sends,
   and each piece the upstream takes, moves the deadline out again
   (NOTE-REQUEST-PROGRESS), so an upload takes as long as it keeps moving.
   The wait for its answer is bounded by this from when the body has gone
   (BEGIN-RESPONSE-READ), as any fetch's is.

   The trigger for revisiting this docstring is not 'find the sentence
   about timeouts' but any change to which paths reach the event loop.
   SO_RCVTIMEO does nothing on a non-blocking socket, so a path that
   becomes non-blocking silently stops being bounded by the per-phase
   reading and starts being bounded by the :AWAITING timer instead — a
   change in what this number means, with nothing here to notice it.")

;;; ---------------------------------------------------------------------------
;;; Outbound address policy (SSRF)
;;; ---------------------------------------------------------------------------

(defvar *fetch-address-filter* nil
  "Optional policy hook consulted for every address the outbound fetch
   machinery is about to dial. A function (IP FAMILY HOST) returning a
   generalized boolean: IP is the 4- or 16-byte address vector, FAMILY is
   :INET or :INET6, HOST is the hostname or IP literal from the URL.
   Returning NIL refuses the address.

   NIL — the default — allows every address. That is the right setting
   when fetch URLs come from config rather than from users (a fixed
   upstream, a self-fetch to 127.0.0.1).

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
   inside the resolve path, so keep it cheap and non-blocking.

   It is asked about each address once a lookup, accepted or refused: for
   a name, once for each address getent prints, when getent's output has
   ended; for an IP literal, once. A resolution remembered under
   *DNS-CACHE-TTL* is asked about again, every address, on each fetch it
   serves, which is how a cached address never outlives a change of
   policy. A filter that writes an audit line or spends a rate budget is
   charged that often: once an address a lookup, and once an address a
   cache hit.")

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
         ;; SERIOUS-CONDITION, not ERROR: see HANDLE-CLIENT-READ's last catch.
         (serious-condition (e)
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
   outbound response, whatever its scheme. Default 8 MiB. It is the
   outbound arm of CONNECTION-READ-CAP, so the event loop stops growing
   the read buffer there, and PARSE-CHUNKED-SIZE-LINE refuses a declared
   chunk larger than it before any of it is read.
   Distinct from *MAX-BODY-SIZE*, which caps inbound request
   bodies — applying that 1 MiB inbound limit to outbound responses
   would reject legitimate 1 MiB HTTPS responses on principle.")

(defparameter *max-streaming-line-size* (* 1 1024 1024)
  "Maximum size of a single line in a streamed response (NDJSON,
   SSE, chunked text). Default 1 MiB. Applied by the streaming
   readers in FETCH-STREAM-PLAIN (via READER-READ-LINE /
   READER-READ-BYTES), whichever byte source STREAM-READER is
   wrapping. Distinct from
   *MAX-BODY-SIZE* (the inbound request-body cap) so tightening
   one does not move the other — an app tuning its request
   hardening should not incidentally break its NDJSON client.
   Distinct from *MAX-OUTBOUND-RESPONSE-SIZE* too: this cap is
   per-line, not per-response, because the streaming readers
   operate on one line at a time and never buffer the whole
   response.")

;;; An upstream's response head is held to these, a client's request head
;;; to the inbound knobs they default to. Apart, so an upstream that sends
;;; more than a client may, a CDN's long Set-Cookie or Link line or its
;;; hundred and first field, can be let through without widening what
;;; every client may send. NIL takes the inbound knob's value, so raising
;;; that one still raises both. Both upstream readers, buffered and
;;; streaming, take them from UPSTREAM-HEADER-LIMITS, so the two agree.

(defparameter *max-upstream-header-count* nil
  "Max header fields in an upstream's response head, or NIL, the default,
   for *MAX-HEADER-COUNT*'s value.")

(defparameter *max-upstream-header-line-length* nil
  "Max length of one field line in an upstream's response head, or NIL,
   the default, for *MAX-HEADER-LINE-LENGTH*'s value. The status line is
   allowed as much in RESPONSE-HEAD-BUDGET.")

(defparameter *max-upstream-total-header-bytes* nil
  "Max field-line bytes in an upstream's response head, or NIL, the
   default, for *MAX-TOTAL-HEADER-BYTES*'s value.")

(defun upstream-header-limits ()
  "(values COUNT LINE-LENGTH TOTAL-BYTES) an upstream's response head is
   held to: each *MAX-UPSTREAM-* knob, or the inbound knob it defaults to."
  (values (or *max-upstream-header-count* *max-header-count*)
          (or *max-upstream-header-line-length* *max-header-line-length*)
          (or *max-upstream-total-header-bytes* *max-total-header-bytes*)))

(defun http-fetch (method url &key headers body then on-body content-length)
  "Create an outbound HTTP request descriptor.
   Return this from a handler to initiate a non-blocking outbound call.
   THEN is called with (status headers body) when the response arrives,
   and answers the parked request with any response a handler can return:
   an HTTP-RESPONSE, a byte vector, or SERVE-STATIC's result. It may also
   return another http-fetch-continuation, to chain. Not a
   STREAM-RESPONSE: the parked request is answered whole, and a relay into
   a stream starts its fetch with FETCH-INTO from the stream's ON-OPEN.

   ON-BODY, if given, is called as (CONN BYTES) with each chunk of a
   chunked response as it arrives, over either scheme — CONN is the
   outbound connection, for FETCH-RESUME. The framing walk already
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

   :PAUSE takes effect at that chunk. No later chunk is handed over until
   the fetch resumes, however many the last read brought —
   CONNECTION-READ-AVAILABLE drains to EAGAIN, so that can be a lot. What
   the read brought stays in the buffer and is handed over first on the
   resume. A relay needs exactly this: it pauses because its own queue is
   behind, and every chunk delivered after that answer would land in the
   queue it is protecting. The recipe is to pause when STREAM-SEND or
   WS-SEND answers NIL, part of the chunk still queued, which is the one
   moment a drain is certain to come and resume the fetch.

   Return :STOP from ON-BODY to end the fetch. The outbound is closed,
   the connection being fetched into is left untouched, and THEN receives
   the abort sentinel — a NIL status — because a response the caller cut
   short is not a delivered one, and reporting a real status over it would
   be silent truncation: a partial body presented as a whole one. There is
   no resume; a stopped fetch is over.

   :STOP takes effect at that chunk too, and it outranks a response whose
   terminator arrived in the same read, so what THEN is told does not
   depend on where the upstream's bytes happened to be split.

   Only useful for a fetch started by FETCH-INTO, which is the one with a
   connection worth keeping. On the handler-returned path a client is
   parked with no answer owed by anything else, so a stop there is
   delivered as the same 502 that path gives every other response which
   never arrived.

   BODY is a string, a byte vector, or :STREAM. :STREAM makes the request
   body the application's to send as it has it, with FETCH-SEND, and to
   end with FETCH-FINISH — an upload forwarded upstream as it arrives,
   never held whole. CONTENT-LENGTH declares its length, and the body must
   then be exactly that many bytes; without it the body is sent chunked,
   which an upstream that needs a length up front (S3's PutObject among
   them) refuses. See FETCH-SEND for the rest of the contract."
  (unless then
    (error "http-fetch requires :then callback"))
  (when content-length
    (unless (eq body :stream)
      (error "http-fetch: :content-length declares a streamed body's length; ~
              a body given whole is measured, so pass :body :stream with it."))
    (unless (and (integerp content-length) (<= 0 content-length))
      (error "http-fetch: :content-length is ~s; it must be a non-negative ~
              integer of bytes."
             content-length)))
  (let ((streamed (eq body :stream)))
    (make-http-fetch-continuation :method method :url url
                                  :headers headers
                                  :body (if streamed nil body)
                                  :stream-body streamed
                                  :content-length content-length
                                  :callback then :on-body on-body)))

(defun fetch-started-p (fetch)
  "True once FETCH, a streamed fetch, has been started — by FETCH-INTO, or
   as a handler's answer. A fetch whose body was given whole is a
   description that may be started any number of times, and never counts
   as started."
  (and (http-fetch-continuation-stream-body fetch)
       (>= (http-fetch-continuation-target-fd fetch) 0)))

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
   parse-integer's SIMPLE-TYPE-ERROR leak out of parse-url.

   Digits and nothing else, as RFC 3986's port = *DIGIT: PARSE-INTEGER
   alone would take a sign and surrounding whitespace, so h:+80 and
   h: 80 would both be port 80."
  (let ((p (if (and (< start (length authority))
                    (loop for i from start below (length authority)
                          always (char<= #\0 (char authority i) #\9)))
               (parse-integer authority :start start)
               (error "malformed URL authority: non-numeric port in ~a" authority))))
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
         (cond ((zerop (length rest)) (values host default-port))
               ((char= (char rest 0) #\:) (values host (%parse-port rest 1)))
               ;; [::1]x: what follows the bracket is a port or nothing.
               (t (error "malformed URL authority: ~s after ']' in ~a"
                         rest authority))))))
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
                                                      headers body
                                                      streamed declared-length)
  "Build an HTTP/1.1 request as a byte vector ready to write.

   STREAMED builds the head of a request whose body follows it as the
   application sends it: framed by Content-Length when DECLARED-LENGTH is
   given, and chunked otherwise. BODY is ignored then; the head is all
   this builds.

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
    ;; Transfer-Encoding: the framework writes this one itself, for a
    ;; streamed body with no declared length, which FETCH-SEND frames to
    ;; match; for any other body a caller's claim is false. The merge below
    ;; adds Content-Length only when the caller did not supply one — and
    ;; Transfer-Encoding is not Content-Length, so it does not suppress
    ;; it. A caller passing
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
      (error "build-outbound-request: Transfer-Encoding is the framework's to ~
              write — a body given whole is framed with Content-Length, and a ~
              streamed one (:body :stream) is chunked unless it declares a ~
              :content-length"))
    (when (assoc "content-length" headers :test #'string-equal)
      (error "build-outbound-request: Content-Length is computed from :body, or ~
              declared with :content-length for a streamed body. A ~
              caller-supplied one that disagrees with the body is a framing ~
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
         (body-bytes (if streamed
                         nil
                         (etypecase body
                           (null nil)
                           (string (sb-ext:string-to-octets body
                                                             :external-format :utf-8))
                           ((simple-array (unsigned-byte 8) (*)) body))))
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
                        ;; there is no case where theirs wins and a test
                        ;; could only ever be true. A test would also
                        ;; carry the shape of a contract this function
                        ;; does not have — "the caller may override
                        ;; framing" — thirty lines below a comment saying
                        ;; framing headers are ours, and a reader who
                        ;; meets the merge first would re-derive the
                        ;; wrong rule.
                        ;; HOST / CONNECTION / USER-AGENT above stay
                        ;; conditional: those are real caller overrides.
                        (cond
                          ((and streamed declared-length)
                           (list (cons "content-length"
                                       (write-to-string declared-length))))
                          (streamed
                           (list (cons "transfer-encoding" "chunked")))
                          (body-bytes
                           (list (cons "content-length"
                                       (write-to-string
                                        (length body-bytes))))))))))
    (serialize-http-message
     (format nil "~a ~a HTTP/1.1" method-str path)
     all-headers body-bytes))))

;;; ---------------------------------------------------------------------------
;;; Parse a response head
;;;
;;; One reading of an upstream's head for every outbound path: COMPLETE-FETCH
;;; and OUTBOUND-RESPONSE-COMPLETE-P over the buffer the event loop fills,
;;; and STREAM-RESPONSE-LINES over the bytes READER-READ-HEAD collects. The
;;; streaming path reads the head as bytes for exactly this reason. A second
;;; reader of the same head, line by line over strings, would be a second
;;; answer to where a line ends, which bytes a value may hold and whether a
;;; repeated Content-Length conflicts, and a response one path took and the
;;; other refused is the disagreement this file is written against.
;;; ---------------------------------------------------------------------------

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

(defun parse-response-headers (buf start header-end)
  "The header alist of the response head in BUF whose status line begins
   at START and whose CRLFCRLF is at HEADER-END, or NIL when the head is a
   status line alone.

   An upstream's field value that is not valid UTF-8 is read as Latin-1
   rather than refused; BYTES-TO-STRING-OR-LATIN-1 says why the response
   side differs from the request side. Every other check is
   PARSE-HEADERS-BYTES' own, the same as a request head gets, against
   UPSTREAM-HEADER-LIMITS rather than the inbound knobs."
  (let ((first-crlf (scan-crlf buf start header-end)))
    (when first-crlf
      (multiple-value-bind (count line-length total-bytes) (upstream-header-limits)
        ;; Bound, the way RECEIVE-MULTIPART holds a part's head to its own
        ;; limits, for this parse only.
        (let ((*max-header-count* count)
              (*max-header-line-length* line-length)
              (*max-total-header-bytes* total-bytes))
          (parse-headers-bytes buf (+ first-crlf 2) (+ header-end 4)
                               :invalid-utf-8 :latin-1))))))

(defun response-head-budget ()
  "How long one response head may grow before it ends: a status line as
   long as a field line may be, the field lines' total and the CRLFs
   around them, all as UPSTREAM-HEADER-LIMITS has them. No head
   PARSE-RESPONSE-HEADERS would accept is longer, so a head still unended
   past this never will be accepted. The streaming reader
   (READER-READ-HEAD) and the buffered one (OUTBOUND-RESPONSE-HEAD) both
   refuse at this length, so the two cannot disagree about how long a
   head may be."
  (multiple-value-bind (count line-length total-bytes) (upstream-header-limits)
    (+ line-length total-bytes (* 2 (+ count 2)))))

(defun parse-response-head (buf start header-end)
  "Read the final response head in BUF that begins at START and whose
   CRLFCRLF is at HEADER-END. Returns
   (values STATUS HEADERS CHUNKED-P TE-PRESENT CONTENT-LENGTH).

   STATUS is NIL for a status line that is not HTTP's. TE-PRESENT is
   SCAN-TRANSFER-ENCODING's classification, and CONTENT-LENGTH is NIL
   whenever it is present: RFC 7230 §3.3.3 has any Transfer-Encoding
   override Content-Length. Signals for what PARSE-HEADERS-BYTES refuses
   and for conflicting Content-Lengths.

   What COMPLETE-FETCH and STREAM-RESPONSE-LINES both call, so the two
   cannot take different views of one head."
  (let* ((status (parse-response-status buf start (+ header-end 4)))
         (headers (parse-response-headers buf start header-end))
         (te-present (scan-transfer-encoding buf header-end start)))
    (values status
            headers
            (response-chunked-p headers)
            te-present
            (unless te-present
              (scan-content-length buf header-end start)))))

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
;;; and Fastly both do it. Nothing the application does invites it, and a
;;; caveat in its place ("Apps must not add Expect: 100-continue to
;;; outbound fetch headers") would push onto the application a protocol
;;; obligation it has no way to discharge.
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

(defun outbound-response-head (conn)
  "Find the final response head in outbound CONN's read buffer, resuming
   where the last read left off. Returns (START . HEADER-END), the head's
   first byte and the offset of its CRLFCRLF, or NIL while it is still
   arriving.

   Incremental, as a request's head search is. An interim already
   stepped over is not walked again, and the search for the head still
   arriving resumes three bytes before where the last one stopped, which
   is enough for a CRLFCRLF split across two reads. Searching from the top
   on every read would make an upstream that dribbles its head cost a
   scan of the whole buffer per wake-up.

   Signals, like SKIP-INTERIM-RESPONSES, past *MAX-INTERIM-RESPONSES*
   interims, and once a head has outgrown RESPONSE-HEAD-BUDGET without
   ending — at the length the streaming path refuses it too, rather than
   at the buffer's cap. The outbound handler turns either into a 502."
  (let ((buf (connection-read-buf conn))
        (end (connection-read-pos conn)))
    (loop
      (let* ((start (connection-out-head-start conn))
             (header-end (scan-crlf-crlf
                          buf (max start (- (connection-header-scan-pos conn) 3))
                          end)))
        (cond
          ((null header-end)
           (when (> (- end start) (response-head-budget))
             (error "upstream response head exceeds ~d bytes without ending"
                    (response-head-budget)))
           (setf (connection-header-scan-pos conn) end)
           (return nil))
          ((let ((status (parse-response-status buf start end)))
             (and status (<= 100 status 199)))
           (when (> (incf (connection-out-interims conn)) *max-interim-responses*)
             (error "upstream sent more than ~d interim responses"
                    *max-interim-responses*))
           (setf (connection-out-head-start conn) (+ header-end 4)
                 (connection-header-scan-pos conn) (+ header-end 4)))
          (t (return (cons start header-end))))))))

;;; ---------------------------------------------------------------------------
;;; HTTPS hook — set by web-skeleton-tls when loaded
;;; ---------------------------------------------------------------------------

(defvar *tls-outbound-setup-fn* nil
  "When non-NIL, a function (CONN HOST) that installs a TLS transport on
   an outbound connection: a handshake step, a byte source, a byte sink,
   and a release. Set by web-skeleton-tls on load.

   Internal, like *DNS-LOOKUP-FN* and for the same reason — it is how
   fetch.lisp reaches an optional system loaded after it, not something an
   application sets. *HTTPS-STREAM-FN* is the exported one, because
   HTTP-FETCH-STREAM is blocking by design.")

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
;;;
;;; Both schemes talk to a blocking socket carrying SO_RCVTIMEO and
;;; SO_SNDTIMEO, one system call at a time: read(2) and write(2) here,
;;; SSL_read and SSL_write in tls.lisp. Not through an SBCL fd-stream,
;;; whose refill and flush wait for the descriptor on their own clock —
;;; untimed unless the stream was made with :TIMEOUT — and go back to that
;;; wait when the call reports the would-block an expired socket timeout
;;; produces. Through one, an upstream that goes silent would hold the
;;; calling worker until it chose to close, and READ-SEQUENCE, which waits
;;; to fill its buffer, would hold every line of a slow stream until 8 KiB
;;; had arrived.
;;; ---------------------------------------------------------------------------

(defun call-retrying-eintr (thunk)
  "Call THUNK until it answers something other than :RETRY, and return
   that answer.

   :RETRY is what the blocking byte sources report for EINTR —
   PLAIN-BYTE-READER and PLAIN-WRITE-ALL here, and the SSL classifiers and
   handshake in tls.lisp: the call was interrupted before it moved any
   bytes, so re-issuing it unchanged is the whole of the correction.
   Nothing was lost and nothing is done twice.

   Bounded by *FETCH-TIMEOUT* across the whole sequence, and deliberately
   not by a count of retries. The difference is the whole of it. A counter
   turns a *correct* retry into a spurious failure after N signals, so it
   fails a healthy connection for something that is not its fault. A
   deadline fails only a fetch that has already outrun the budget the
   caller set, which is what setting that budget asked for.

   Leaving it unbounded would be wrong, because SO_RCVTIMEO restarts per
   syscall, so it bounds each call and never the sequence, and the
   signals here are routine: SBCL stops every other thread with one for
   each garbage collection, and each getent child ends with a SIGCHLD.
   Under steady collection a stalled upstream retried without a deadline
   would never time out. BLOCKING-WANT-OR-RAISE in tls.lisp has the same
   facts, and they are why EINTR reaches the TLS path at all.

   The deadline is taken on the first interruption rather than up front:
   the ordinary path never retries, and should not pay a clock read per
   call to be told so.

   The caller pins across this loop, not inside THUNK. OpenSSL requires a
   repeated call to present the same address and length, and a GC landing
   between the interrupted call and its retry would move the vector out
   from under exactly that requirement."
  (let ((deadline nil))
    (loop
      (let ((verdict (funcall thunk)))
        (unless (eq verdict :retry)
          (return verdict))
        (unless deadline
          (setf deadline (+ (get-internal-real-time)
                            (* *fetch-timeout*
                               internal-time-units-per-second))))
        ;; >= not >: at the deadline the budget is spent, and a zero
        ;; budget therefore means no retries rather than one.
        (when (>= (get-internal-real-time) deadline)
          (error "interrupted repeatedly: no progress in ~d seconds"
                 *fetch-timeout*))))))

(defun plain-io-failed (fd operation errno)
  "Classify a failed read(2) or write(2) on the blocking fetch socket FD:
   :RETRY for EINTR, and a raise for everything else.

   A would-block cannot mean what it means on the event loop's sockets.
   This one blocks, so read(2) and write(2) return EAGAIN only when
   SO_RCVTIMEO or SO_SNDTIMEO expired: the upstream sent nothing, or read
   nothing, for *FETCH-TIMEOUT*. That is raised as the timeout it is. Any
   other errno is FD-IO-ERROR naming FD, so a handler whose fetch failed
   answers 500 rather than being taken for its own client leaving."
  (cond
    ((= errno +eintr+) :retry)
    ((or (= errno +eagain+) (= errno +ewouldblock+))
     (error "fetch-stream: ~a timed out: nothing ~a in ~a second~:p (*fetch-timeout*)"
            operation (if (string= operation "read") "received" "accepted")
            *fetch-timeout*))
    (t (error 'fd-io-error :fd fd :operation operation :errno errno))))

(defun plain-byte-reader (fd)
  "Byte source over the blocking socket FD for STREAM-READER, the plain
   twin of SSL-BYTE-READER: one read(2) per fill, answering with however
   many bytes it returned, :EOF at a clean end of stream, or a raise from
   PLAIN-IO-FAILED.

   One call per fill is what lets a line out as soon as it arrives. A fill
   that waited for its buffer to be full would hold an NDJSON token stream
   back until its end."
  (lambda (buf len)
    ;; Pinned outside the retry loop, as SSL-BYTE-READER's is: an
    ;; interrupted call is re-issued with the same address.
    (sb-sys:with-pinned-objects (buf)
      (call-retrying-eintr
       (lambda ()
         (let ((n (%read fd (sb-sys:vector-sap buf) len)))
           (cond
             ((> n 0) n)
             ((zerop n) :eof)
             (t (plain-io-failed fd "read" (get-errno))))))))))

(defun plain-write-all (fd bytes)
  "Write all of BYTES to the blocking socket FD with write(2), the plain
   twin of TLS-WRITE-ALL. SO_SNDTIMEO bounds each call, so an upstream that
   stops reading a large request body fails the fetch rather than holding
   the caller; see PLAIN-IO-FAILED."
  (let ((pos 0)
        (len (length bytes)))
    (loop while (< pos len)
          do (incf pos
                   ;; Pinned across the retry loop, as TLS-WRITE-ALL's is.
                   (sb-sys:with-pinned-objects (bytes)
                     (call-retrying-eintr
                      (lambda ()
                        (let ((n (%write fd
                                         (sb-sys:sap+ (sb-sys:vector-sap bytes) pos)
                                         (- len pos))))
                          (if (> n 0)
                              n
                              (plain-io-failed fd "write" (get-errno)))))))))))

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
   each of DNS resolution, TCP connect, and each read and write on the
   socket. DNS uses the shared getent resolver (same as the async path);
   the TCP connect uses a non-blocking connect + poll(2) so a black-holed
   peer cannot pin the worker for ~120s under tcp_syn_retries. The request
   goes out through PLAIN-WRITE-ALL and the response comes back through
   PLAIN-BYTE-READER, read(2) and write(2) on the descriptor itself, for
   the reason this section's header gives."
  (multiple-value-bind (ip family fallbacks)
      (funcall *dns-resolve-blocking-fn* host)
    (unless ip
      (error "fetch-stream: failed to resolve ~a" host))
    (let ((socket (blocking-connect-to-any
                   (cons (cons ip family) fallbacks) port
                   :socket-setup (lambda (fd)
                                   (set-socket-timeout fd *fetch-timeout*)))))
      (unwind-protect
          (let ((fd (socket-fd socket)))
            (plain-write-all fd (build-outbound-request
                                 method host path
                                 :port port
                                 :headers headers :body body))
            (stream-response-lines nil on-line
                                   :method method
                                   :read-fn (plain-byte-reader fd)))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

;;; ---------------------------------------------------------------------------
;;; Buffered stream reader
;;;
;;; Wraps a binary stream with a read-ahead buffer so callers can read
;;; line-by-line without issuing one syscall per byte.
;;; ---------------------------------------------------------------------------

(defstruct (stream-reader
            (:constructor make-stream-reader (stream &optional read-fn)))
  (stream nil)
  ;; READ-FN, when present, replaces STREAM as the byte source:
  ;; (READ-FN BUF LEN) fills BUF and answers with the count, :EOF at a
  ;; clean end of stream, or raises. That is the contract
  ;; SSL-BYTE-READER already answers, and it is the whole reason one
  ;; line reader can serve both transports rather than TLS carrying a
  ;; second implementation of this file's job.
  (read-fn nil)
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
  "Refill the buffer. Returns bytes read (0 = EOF).

   0 is the contract, not 'fewer than asked for', and the two byte
   sources differ on exactly that: READ-SEQUENCE on a blocking stream
   fills the whole buffer or stops at end of stream, so a short return
   does mean EOF there, while a READ-FN answers as soon as any bytes are
   available and a short fill means nothing at all. Every caller loops
   and treats only 0 as terminal, so both work — but a caller that ever
   reads a short fill as EOF would be correct on one source and silently
   truncating on the other. Both transports read through a READ-FN,
   PLAIN-BYTE-READER and SSL-BYTE-READER; a stream is how a test hands
   this canned bytes.

   A READ-FN may also raise, which this function otherwise cannot. That
   is deliberate on the TLS side: SSL-READ-EOF-OR-RAISE answers :EOF for
   a benign close only and raises for everything else, which is what
   keeps a reset mid-body from arriving here as a clean end of stream."
  (setf (stream-reader-pos r) 0)
  (let* ((buf (stream-reader-buf r))
         (fn (stream-reader-read-fn r))
         (n (if fn
                (let ((v (funcall fn buf (length buf))))
                  (if (eq v :EOF) 0 v))
                (read-sequence buf (stream-reader-stream r)))))
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
   MAX-SIZE bounds the accumulated line length, *MAX-STREAMING-LINE-SIZE*
   by default.

   For body lines only. A response head is never read with this: its
   line endings are HTTP's, not EventStream's, and READER-READ-HEAD reads
   it as bytes for the buffered path's readers."
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

(defun reader-read-head (r)
  "Read one response head from R: the bytes from its status line through
   the CRLFCRLF that ends its header block, terminator included. Returns
   them as a fresh vector, or NIL at a clean end of stream before any
   byte.

   Bytes and not lines, so STREAM-RESPONSE-LINES can hand the head to the
   readers the buffered path uses; see \"Parse a response head\". The head
   ends where the buffered path says it ends, at the first CRLFCRLF, so a
   bare CR or LF ends no line here either.

   Signals at end of stream inside a head, which the buffered path also
   refuses, and once the head outgrows RESPONSE-HEAD-BUDGET, the length
   the buffered path refuses at too. The exact caps are
   PARSE-HEADERS-BYTES' to enforce. This bound only stops an upstream that
   never ends its head from growing the vector without limit."
  (let ((head (make-array 512 :element-type '(unsigned-byte 8)
                              :fill-pointer 0 :adjustable t))
        (limit (response-head-budget)))
    (loop
      (when (>= (stream-reader-pos r) (stream-reader-end r))
        (when (zerop (reader-fill r))
          (if (zerop (fill-pointer head))
              (return nil)
              (error "streaming response: end of stream inside the ~
                      response head"))))
      (let ((b (aref (stream-reader-buf r) (stream-reader-pos r)))
            (n (fill-pointer head)))
        (incf (stream-reader-pos r))
        (when (>= n limit)
          (error "streaming response: head longer than ~d bytes" limit))
        (vector-push-extend b head)
        (when (and (= b 10) (>= n 3)
                   (= (aref head (- n 1)) 13)
                   (= (aref head (- n 2)) 10)
                   (= (aref head (- n 3)) 13))
          (return (subseq head 0 (1+ n))))))))

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
   this in practice.

   The line is read one character per byte. A chunk-extension may carry
   any byte but CR and LF (a quoted value admits obs-text, RFC 7230
   §3.2.6), and PARSE-CHUNK-SIZE-BYTES skips such a byte on the buffered
   readers; a decoding that refused it would make this reader the one
   that disagrees."
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
                      line-buf :external-format :latin-1))))
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
   truncation, an end of stream before the requested body was
   delivered. An expired SO_RCVTIMEO is not one of those: the READ-FN
   raises it as a timeout.

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

(defun stream-response-lines (stream on-line &key (method :GET) read-fn)
  "Read an HTTP response from a byte stream. Read the head, then call
   ON-LINE per body line. Handles chunked transfer encoding.
   Returns the status code.

   READ-FN substitutes the byte source and STREAM is then ignored; see
   READER-FILL for the contract. The plain path supplies
   PLAIN-BYTE-READER and TLS supplies SSL-BYTE-READER, which is how this
   function serves both transports and why there is no second copy of it
   in tls.lisp.

   METHOD gates the body-framing discipline: for :HEAD, RFC 7231
   §4.3.2 guarantees an empty body even when the upstream echoes
   the GET-body Content-Length, so the body phase is skipped and
   the truncation checks are bypassed.

   Truncation discipline on the streaming path:
     - chunked: stream-chunked-lines raises if the zero-size
       terminator never arrives. Same contract as decode-chunked-body
       for the buffered path.
     - content-length (no TE): reader-read-bytes is called with the
       declared length; a short return means EOF before the full body
       was delivered — raise so the caller sees a loud error instead of
       a silently short stream. This is the plain-HTTP twin of the
       SSL_ERROR_SYSCALL discipline in tls.lisp's ssl-read-eof-or-raise.
       A read that times out raises from the READ-FN before it gets
       here.
     - close-delimited (no TE, no CL): by definition the connection
       close IS the end; we cannot distinguish a legitimate end from
       a premature RST without out-of-band framing, so treat clean
       EOF as complete. Apps that care about this case should use a
       framed path upstream."
  (let ((r (make-stream-reader stream read-fn))
        (status nil)
        (chunked nil)
        (te-present nil)
        (content-length nil)
        (interims 0))
    ;; Each head is read as bytes and handed to the readers the buffered
    ;; path uses — see "Parse a response head" — so the two paths cannot
    ;; disagree about a head, its limits included: PARSE-HEADERS-BYTES
    ;; enforces the per-line, per-count and total caps here as it does
    ;; there.
    ;;
    ;; The head is read again over any 1xx interim block (RFC 7231 §6.2):
    ;; an interim ends at its own CRLFCRLF and carries no body, so the
    ;; final response's head is the very next one. Without this the
    ;; interim's blank line would end the header phase, 103 would be
    ;; reported as the status, and ON-LINE would never be called. As in
    ;; SKIP-INTERIM-RESPONSES, an interim's fields are stepped over
    ;; unread, so nothing in one can reach the final response's framing.
    (loop
      (let ((head (reader-read-head r)))
        (setf status (and head (parse-response-status head 0 (length head))))
        ;; Refused before any body is read, as the buffered path refuses
        ;; it: the bytes after a status line that is not HTTP's are not
        ;; a body of anything.
        (unless status
          (error "streaming response: no parseable status line"))
        (unless (<= 100 status 199)
          (multiple-value-bind (final headers chunked-p te cl)
              (parse-response-head head 0 (- (length head) 4))
            (declare (ignore final headers))
            (setf chunked chunked-p te-present te content-length cl))
          (return))
        (incf interims)
        (when (> interims *max-interim-responses*)
          (error "streaming response: more than ~d interim responses"
                 *max-interim-responses*))))
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
      ;; 1xx is deliberately absent: the header loop above exits only
      ;; on a status >= 200, so an interim can never reach here. Testing
      ;; for it would be dead code that reads as though a 1xx were a
      ;; legitimate final status, which is the belief that lets an
      ;; interim stand in for the response.
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
    status))

(defun parse-chunked-size-line (size-line)
  "Parse a chunked-transfer size token from SIZE-LINE. Strips
   chunk-extensions (RFC 7230 §4.1.1 — everything from ';' onwards),
   requires at least one hex digit, caps at 16 hex digits, and
   raises on parse failure. Returns the integer size (0 for the
   final chunk).

   The streaming path's parser, reached from stream-chunked-lines over
   either transport. The buffered readers, DECODE-CHUNKED-BODY and
   CHUNKED-BODY-COMPLETE-P, read their size lines through
   PARSE-CHUNK-SIZE-BYTES instead, and accept the same lines this and
   READER-READ-CRLF-LINE accept together: one grammar in two
   implementations, one over a byte buffer and one over a line already
   read, and PROP-CHUNK-SIZE-LINE-PARITY holds them to it.

   Strict rejection matters because a permissive parse ('xyz' → NIL,
   '-5' → -5) would silently exit the decoder loop as if the stream
   were complete — a parser-disagreement smuggling primitive against
   any stricter downstream that re-parses the body. The 16-digit cap
   mirrors PARSE-CHUNK-SIZE-BYTES's: without it, a 1 MiB
   attacker-framed hex string lands in parse-integer, which is
   super-linear in digit count."
  ;; Trailing whitespace only: BWS may follow the digits, before a ';'
  ;; (RFC 7230 §4.1.1), but nothing precedes them. Trimmed on both sides,
  ;; ' 5' would parse here while DECODE-CHUNKED-BODY and
  ;; CHUNKED-BODY-COMPLETE-P refuse it — the same bytes framed two ways.
  (let* ((semi (position #\; size-line))
         (hex-end (or semi (length size-line)))
         (hex (string-right-trim '(#\Space #\Tab) (subseq size-line 0 hex-end))))
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
   Used by QUEUE-FINAL-RESPONSE, INITIATE-FETCH, COMPLETE-FETCH,
   DELIVER-FETCH-ERROR, DELIVER-AWAITING-TIMEOUT and START-STREAM, so
   every response site echoes a consistent Connection header whichever
   path delivers. A DNS failure and either scheme's fetch reach it
   through COMPLETE-FETCH or DELIVER-FETCH-ERROR."
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

(defun queue-final-response (conn response)
  "Queue RESPONSE on CONN as the whole answer to its request, in any of the
   shapes a handler or a fetch :THEN may answer with:

     HTTP-RESPONSE    serialized here, with CONN's connection hint, and
                      without its body for HEAD
     byte vector      a whole message already serialized, cut at its head
                      for HEAD
     list of segments byte vectors and WRITE-SPANs, written in order —
                      SERVE-STATIC's shape — whose producer has already
                      decided what HEAD gets, with CONN's Connection line
                      added ahead of the head's blank line (see
                      WITH-CONNECTION-LINE)

   Anything else signals, naming those shapes, rather than reaching a
   struct accessor inside FORMAT-RESPONSE and raising a type error about
   an internal. One function for HANDLE-CLIENT-READ and COMPLETE-FETCH, so
   a :THEN can answer with whatever a handler can — asking an upstream and
   then serving a file is the ordinary case — and the two cannot drift.

   A response struct is treated as immutable: a caller that caches a
   (make-error-response 404) across requests must not have HEAD strip its
   body slot or HTTP/1.0 keep-alive stamp a Connection header onto it.
   :CONNECTION-HINT stamps the header at serialize time and :HEAD-ONLY-P
   skips the body without allocating it; neither touches the struct.

   Segments are queued as one act, and outside *MAX-WRITE-BACKLOG*. That
   bound is for a producer outrunning its peer; held to it, a finished
   static file over the bound would have its body refused after its
   headers had gone out promising one.

   The caller moves the state and arms the descriptor; what follows the
   queueing differs between them."
  (let ((head-p (and (connection-request conn)
                     (eq (http-request-method (connection-request conn)) :HEAD))))
    (cond
      ((typep response 'http-response)
       (connection-queue-write conn (format-response
                                     response
                                     :connection-hint (connection-hint-for conn)
                                     :head-only-p head-p)))
      ((typep response '(simple-array (unsigned-byte 8) (*)))
       (connection-queue-write conn (strip-body-for-head response conn)))
      ((and (consp response)
            (every (lambda (segment)
                     (or (typep segment '(simple-array (unsigned-byte 8) (*)))
                         (write-span-p segment)))
                   response))
       (connection-queue-segments
        conn (with-connection-line response (connection-hint-for conn))))
      (t
       (error "~a is not a response: expected an HTTP-RESPONSE, a byte ~
               vector, or a list of segments (what SERVE-STATIC returns)"
              (type-of response))))))

(defun with-connection-line (segments hint)
  "SEGMENTS with the Connection header HINT calls for placed just ahead of
   their blank line, or SEGMENTS unchanged when HINT is NIL or there is no
   blank line to find.

   The blank line is the shared +HEADER-TERMINATOR+, which every head
   SERVE-STATIC builds ends with, so finding it is an EQ test and not a
   parse. A static head is built before anyone knows which connection it
   will go out on, so this is where the answer HTTP/1.0 keep-alive needs
   (keep-alive) and the one a closing connection owes (close) reach it,
   the same answer FORMAT-RESPONSE gives a dynamic response through
   CONNECTION-HINT-FOR. The lines are shared vectors, never written into,
   like the terminator itself."
  (let ((line (case hint
                (:close (load-time-value
                         (sb-ext:string-to-octets
                          (format nil "connection: close~c~c" #\Return #\Newline)
                          :external-format :ascii)
                         t))
                (:keep-alive (load-time-value
                              (sb-ext:string-to-octets
                               (format nil "connection: keep-alive~c~c"
                                       #\Return #\Newline)
                               :external-format :ascii)
                              t)))))
    (if (and line (member +header-terminator+ segments :test #'eq))
        (loop for segment in segments
              when (eq segment +header-terminator+) collect line
              collect segment)
        segments)))

(defun awaiting-inbound-for (inbound-fd out-fd)
  "Return the inbound connection parked on OUT-FD, or NIL.

   The delivery paths resume an inbound by looking up a raw fd *number*
   that was recorded when the fetch was parked. Numbers are recycled: an
   inbound that closed while its outbound was still in flight frees its
   fd for the next accept, and delivering to whatever now answers to that
   number would queue one caller's response onto another caller's socket.

   No caller reaches it: every route that closes an :AWAITING inbound goes
   through CLOSE-CONNECTION, which tears the paired outbound down with it.
   But that is a property of the callers rather than of these functions,
   which is the reasoning CONNECTION-QUEUE-WRITE's own guard rejects, and
   any long-lived parked state added later would widen the window.

   Refusing costs a delay, never a wrong delivery: an inbound this
   declines to answer is left parked, and the :AWAITING sweeper answers it
   504."
  (let ((inbound (lookup-connection inbound-fd)))
    (when (and inbound (= (connection-awaiting-fd inbound) out-fd))
      inbound)))

;;; ---------------------------------------------------------------------------
;;; Initiate outbound fetch
;;; ---------------------------------------------------------------------------

(defun fetch-into (connection continuation &key (failure-disposition :close))
  "Start CONTINUATION's outbound request against CONNECTION, a connection
   the application already owns. Returns T, or signals.

   CONNECTION is what the app's :ON-OPEN received, or what its WS-HANDLER
   was called with. CONTINUATION is an ordinary HTTP-FETCH continuation;
   HTTP-FETCH is a pure constructor either way.

   T means the fetch is initiated, not completed. Nothing is parked, the
   connection's state is untouched, and the app goes on writing to it. The
   callbacks are the ones HTTP-FETCH already takes and mean the same
   things: :THEN receives (status headers body) with a NIL status as the
   abort sentinel, and :ON-BODY receives (outbound-connection chunk) and
   may return :PAUSE or :STOP. A :THEN moves from a handler-returned fetch
   to this one unchanged. :THEN's return value is discarded here — there is
   no parked request for it to answer — except a streamed fetch's, below.

   :STOP is the verdict this path exists for. It closes the outbound,
   leaves CONNECTION alone, and fires :THEN with the abort sentinel, so an
   application that has stopped wanting a response can stop paying for it
   without giving up the connection it was producing into. The connection
   is free to start another fetch immediately; the outstanding marker is
   released before :THEN runs, on the same reasoning that lets a delivered
   fetch chain.

   FAILURE-DISPOSITION is what a *failed* fetch does to CONNECTION.
   :CLOSE, the default, is the framework's own disposition: a :STREAMING
   target is closed without its terminator, a :WEBSOCKET target gets a
   1011 close frame, and a connection receiving a body is closed, its
   plan's ON-ABORT hearing :UPSTREAM-FAILED. :KEEP suppresses that and
   leaves the connection to the application, and is accepted on every
   target but :STREAMING.

   A connection receiving a body is a target for the case RECEIVE-BODY's
   ON-DATA meets when what it was handed goes somewhere asynchronous: an
   upload forwarded a part at a time, each part a fetch. ON-DATA starts
   the fetch and answers :PAUSE; the fetch's :THEN resumes the body with
   BODY-RESUME, or refuses it with BODY-REFUSE. The fetch's result goes to
   :THEN alone, the connection being the one whose life bounds it — a
   client that leaves takes the fetch down with it.

   A streamed fetch (:BODY :STREAM) is the other shape for that case: one
   request, its body sent with FETCH-SEND as ON-DATA is handed it. A NIL
   from FETCH-SEND pauses the body, and the request draining resumes it,
   with no :THEN involved. Its :THEN's value is kept on the fetch rather
   than discarded, because the request being received may be answered with
   the fetch once its body has ended (ADOPT-FETCH), and an upstream that
   answered before then has answered that request too. FORWARD-BODY is the
   whole shape in one call.

   The asymmetry is not an oversight. On :STREAMING the disposition is a
   protocol obligation — the framework owns the terminator and writing one
   claims the body is complete — so :KEEP there is refused rather than
   honoured. On a WebSocket the framework has already disclaimed the
   framing, so the close is a policy, and its own justification is that an
   app will not handle a path it has never seen fail. An app that sends its
   own error frame is handling it, and :KEEP is how it says so.

   What :KEEP transfers is the failure path, including not leaving a dead
   socket parked until *WS-IDLE-TIMEOUT*, which defaults to a day. That is
   why :CLOSE stays the default: it is the safe answer for the app that has
   not thought about it.

   Signals, rather than returning NIL, in eight cases:

     the connection is not :STREAMING or :WEBSOCKET, nor receiving a body
       — nothing else owns its own write path or what it reads, so
       nothing else can consume a result;
     CONTINUATION is not an HTTP-FETCH continuation;
     FAILURE-DISPOSITION is neither :CLOSE nor :KEEP;
     FAILURE-DISPOSITION is :KEEP on a :STREAMING target;
     there is no event loop on this thread to drive the outbound, which is
       the case off a worker — the REPL, a unit test. FETCH-RESUME can
       degrade to a no-op there and this cannot: the socket would be
       opened and never driven, leaking a descriptor and a callback that
       never fires;
     a detached fetch is already outstanding on this connection. Two would
       race to apply the disposition below and whichever finished first
       would close the target out from under the other;
     CONNECTION is not on this worker's table. Delivery reaches the target
       by looking its fd up in *CONNECTIONS*, which is per-worker while fds
       are unique across the process — so a cross-worker fetch runs, and
       then finds nothing to deliver into, leaving the marker set for the
       life of the connection and the disposition unapplied;
     CONTINUATION is a streamed fetch already started. It is the
       application's handle on one exchange, and FETCH-SEND writes through
       it, so a second start would be two exchanges under one name.

   A fetch that passes them and still cannot be started signals what
   stopped it: a URL that does not parse, HTTPS without web-skeleton-tls,
   an IP literal *FETCH-ADDRESS-FILTER* refuses, or a dial the kernel
   refuses inside the call, which is where an IP literal and a name the
   DNS cache answers are dialed. A name that has to be looked up is dialed
   later, and a refusal there reaches :THEN as the abort sentinel.

   Signalling rather than returning NIL because a call from the wrong
   state is a programming error whose silent failure mode is a connection
   that produces nothing — which is the defect this function exists to
   remove, reintroduced by the function removing it.

   A signalling call has done nothing: no socket, no registration, no
   epoll entry, no state change on CONNECTION, and the callback does not
   fire. A fetch refused at the door has no lifetime for `exactly once` to
   apply within, and the caller is on the stack holding the connection and
   can act on the signal — unlike the parked-inbound path, where the
   callback fires the sentinel because there is a client owed an answer
   and nobody left to give it one.

   The corollary is what keeps T honest: :THEN can never run before this
   returns. Were a setup failure to deliver synchronously, an app that
   called FETCH-INTO and then wrote to the connection could have its
   failure frame reach the peer first, and could find the connection
   already closing on a call that returned T to say the fetch had begun."
  (unless (typep continuation 'http-fetch-continuation)
    (error "fetch-into: second argument is ~a, not an http-fetch ~
            continuation" (type-of continuation)))
  (unless (or (member (connection-state connection) '(:streaming :websocket))
              (receiving-body-p connection))
    (error "fetch-into: fd ~d is in state ~a; only :streaming and ~
            :websocket own their own write path, and only a connection ~
            receiving a body owns what it reads"
           (connection-fd connection) (connection-state connection)))
  (unless (member failure-disposition '(:close :keep))
    (error "fetch-into: :failure-disposition is ~s; expected :close or :keep"
           failure-disposition))
  ;; Refused here rather than honoured and then ignored at delivery, for
  ;; two reasons. The caller is on the stack at the call that made the
  ;; mistake, where an error is readable; DELIVER-DETACHED runs on a
  ;; teardown path with nothing left to answer. And a keyword that is
  ;; accepted and silently disregarded is worse than one that is refused —
  ;; the caller writes a failure path and believes it will run.
  ;;
  ;; Sound this early because the state cannot drift into mattering: a
  ;; :WEBSOCKET connection does not become :STREAMING, and a body being
  ;; received is not answered with a stream until it has been received.
  (when (and (eq failure-disposition :keep)
             (eq (connection-state connection) :streaming))
    (error "fetch-into: fd ~d is ~a; :failure-disposition :keep is for ~
            :websocket targets and bodies being received — on a :streaming ~
            one the disposition is the framing, and keeping it open would ~
            hand the client an unterminated body"
           (connection-fd connection) (connection-state connection)))
  (unless *epoll-fd*
    (error "fetch-into: no event loop on this thread. The outbound would ~
            be opened and never driven."))
  ;; And the right worker, not merely a worker. Placed after the check
  ;; above because the two are the same question at two resolutions, and
  ;; the coarser answer is the more useful one when both are true: told
  ;; "wrong worker" off a worker entirely, a caller would go looking for
  ;; the right one.
  ;;
  ;; This is the only one of the eight that needs the worker's table to
  ;; answer. A cross-worker call satisfies every other check —
  ;; the state is right, *EPOLL-FD* is bound, nothing is outstanding — so
  ;; the outbound is created and registered here and runs correctly.
  ;; Delivery is what fails: DELIVER-DETACHED and STOP-FETCH both reach
  ;; the target through (LOOKUP-CONNECTION TARGET-FD), and *CONNECTIONS*
  ;; is per-worker while fds are unique across the process, so that lookup
  ;; answers NIL. The marker is then set and never cleared, refusing every
  ;; later fetch on that connection for the rest of its life, and the
  ;; failure disposition never applies — the outcome DELIVER-DNS-ERROR
  ;; avoids by delegating, reached from another direction.
  ;;
  ;; EQ against the table, not a comparison on the fd number: an fd
  ;; reissued to a different connection on this worker would satisfy the
  ;; number while being the wrong object.
  (unless (eq (lookup-connection (connection-fd connection)) connection)
    (error "fetch-into: fd ~d is not on this worker's connection table. ~
            A connection can only be fetched into from the worker that ~
            owns it — delivery reaches the target by looking it up in that ~
            worker's table, so a cross-worker fetch would leave the marker ~
            set and the disposition unapplied."
           (connection-fd connection)))
  (when (connection-fetch-outstanding connection)
    (error "fetch-into: fd ~d already has a detached fetch outstanding"
           (connection-fd connection)))
  (setf (http-fetch-continuation-sink continuation) :detached)
  ;; Both marked before dialing and both cleared by the setup-failure path
  ;; below, so a refused call leaves the connection exactly as it found it.
  ;; :CLOSE is the resting value as well as the default, so restoring it is
  ;; restoring the connection rather than overwriting a choice — and the
  ;; slot is unread anyway while no fetch is outstanding.
  (setf (connection-fetch-outstanding connection) t
        (connection-fetch-failure-disposition connection) failure-disposition
        (connection-send-waiting connection) nil)
  (handler-case (initiate-fetch connection *epoll-fd* continuation)
    (error (e)
      (setf (connection-fetch-outstanding connection) nil
            (connection-fetch-failure-disposition connection) :close)
      (error e)))
  t)

(defun initiate-fetch (conn epoll-fd fetch-req)
  "Start an outbound HTTP(S) request.
   CONN is the inbound connection to park.
   FETCH-REQ is the http-fetch-continuation descriptor.

   Both schemes take the same non-blocking epoll path, and neither holds
   the worker for the exchange. What differs is a transport installed on
   the outbound connection — a handshake step, a byte source, a byte sink
   and a release — and nothing about the state machine that drives them.

   A streamed fetch starts once. Its struct is the application's handle on
   it from then on — FETCH-SEND writes through it — so a second start
   would be two exchanges answering to one name; an answer that names one
   already started goes to ADOPT-FETCH instead. Any other fetch is a
   description, and may be started as often as it is returned."
  (when (fetch-started-p fetch-req)
    (error "fetch to ~a has already been started; a fetch starts once"
           (http-fetch-continuation-url fetch-req)))
  (setf (http-fetch-continuation-target-fd fetch-req) (connection-fd conn))
  (handler-case
      (multiple-value-bind (scheme host port path)
          (parse-url (http-fetch-continuation-url fetch-req))
        (setf (http-fetch-continuation-scheme fetch-req) scheme)
        (when (and (eq scheme :https) (null *tls-outbound-setup-fn*))
          (error "HTTPS not available — load web-skeleton-tls"))
        (initiate-http-fetch conn epoll-fd fetch-req host port path))
    (error (e)
      ;; A detached fetch has no parked caller to answer and its initiator
      ;; is still on the stack, so the failure goes back to them as a
      ;; signal and the callback does not fire at all. See FETCH-INTO: a
      ;; fetch refused before it started has no lifetime for `exactly
      ;; once` to apply within, and delivering here would also mean :THEN
      ;; could run before FETCH-INTO returned.
      (when (eq (http-fetch-continuation-sink fetch-req) :detached)
        ;; Refused at the door, so not started: FETCH-INTO's caller may
        ;; start it again once it has dealt with the cause.
        (setf (http-fetch-continuation-target-fd fetch-req) -1)
        (error e))
      (setf (http-fetch-continuation-ended fetch-req) t)
      (log-error "fetch setup failed: ~a" e)
      ;; Fire the cleanup sentinel so the app's :then closure runs
      ;; exactly once even on pre-connection errors (malformed URL,
      ;; DNS spawn failure, HTTPS not loaded).
      ;;
      ;; Taken from the continuation rather than through
      ;; CLAIM-FETCH-CALLBACK, because no outbound connection was ever
      ;; built to hold it — this is the one ending that happens before a
      ;; fetch has anywhere to live. Nothing reaches FETCH-REQ again: it
      ;; arrived as this call's argument and is unreachable once the call
      ;; returns, so there is no second path to claim it away from.
      (finish-fetch (http-fetch-continuation-callback fetch-req) :aborted)
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

(defparameter *connect-fallback-delay* 2
  "Seconds a connect to one of a name's addresses is given while the name
   has more, before the next is dialed instead. RFC 8305's longest
   Connection Attempt Delay: an address that has not answered in that time
   has lost a SYN at least once, and a black-holed one, such as an IPv6
   route that goes nowhere, would otherwise hold the fetch to its whole
   *FETCH-TIMEOUT*. The last address is given the rest of that.

   Whole seconds, since the event loop's connect deadline is a time of
   day in seconds: START-SERVER refuses a fraction, RFC 8305's 250 ms
   included, rather than let it fail every fetch to a name with more than
   one address.")

(defun blocking-connect-to-any (addresses port &key socket-setup)
  "Connect, blocking, to the first of ADDRESSES, (IP . FAMILY) in order,
   that answers at PORT, and return its socket. The blocking paths'
   counterpart of FALL-BACK-TO-NEXT-ADDRESS: an address that fails gives
   way to the next, and so does one that has not answered within
   *CONNECT-FALLBACK-DELAY* while more remain. The last is given what is
   left of *FETCH-TIMEOUT*. Signals the last failure when none answers.

   SOCKET-SETUP, when given, is called with each socket's descriptor
   before its connect."
  (let ((deadline (+ (get-internal-real-time)
                     (* *fetch-timeout* internal-time-units-per-second)))
        (last-failure nil))
    (loop for ((ip . family) . more) on addresses
          for left = (/ (- deadline (get-internal-real-time))
                        internal-time-units-per-second)
          while (plusp left)
          do (let ((socket (handler-case
                               (make-instance (if (eq family :inet)
                                                  'sb-bsd-sockets:inet-socket
                                                  'sb-bsd-sockets:inet6-socket)
                                              :type :stream :protocol :tcp)
                             (error (e) (setf last-failure e) nil))))
               (when socket
                 (handler-case
                     (progn
                       (when socket-setup
                         (funcall socket-setup (socket-fd socket)))
                       (blocking-connect socket ip port
                                         (if more
                                             (min *connect-fallback-delay* left)
                                             left))
                       (return-from blocking-connect-to-any socket))
                   (error (e)
                     (setf last-failure e)
                     (ignore-errors (sb-bsd-sockets:socket-close socket))
                     (when more
                       (log-info "fetch: connect to ~a: ~a; trying the next address"
                                 (format-ip ip) e)))))))
    (if last-failure
        (error last-failure)
        (error "connect: no address answered in ~a seconds" *fetch-timeout*))))

(define-condition connect-failed-at-once (error)
  ((address :initarg :address :reader connect-failed-at-once-address)
   (why     :initarg :why     :reader connect-failed-at-once-why))
  (:report (lambda (c stream)
             (format stream "connect to ~a: ~a"
                     (format-ip (connect-failed-at-once-address c))
                     (connect-failed-at-once-why c))))
  (:documentation
   "An address could not be dialed at all: no socket of its family, or a
    connect the kernel refused before it began. The next address the name
    resolved to is tried; with none left, it fails the fetch as any setup
    error does."))

(defun initiate-http-fetch-to-address (conn epoll-fd fetch-req host port path ip family
                                       &key fallbacks started-at)
  "Open a non-blocking TCP connection to IP:PORT, FAMILY :INET or :INET6,
   and send the outbound HTTP request. CONN is the connection the fetch is
   for. HOST is the original hostname, for the Host: header and not for
   routing. Called from INITIATE-HTTP-FETCH's numeric-IP fast path, the DNS
   cache, and the DNS-ready callback in src/server/dns.lisp.

   FALLBACKS are the name's other addresses, (IP . FAMILY) in order. An
   address that cannot be dialed at all (CONNECT-FAILED-AT-ONCE) gives way
   to the next here. One whose connect fails later, or outlasts
   *CONNECT-FALLBACK-DELAY*, gives way to the next through
   FALL-BACK-TO-NEXT-ADDRESS, which the rest are kept on the outbound for.
   STARTED-AT is the first attempt's, so a fallback does not extend the
   fetch's deadline."
  (loop
    (handler-case
        (return (%dial-address conn epoll-fd fetch-req host port path ip family
                               :fallbacks fallbacks :started-at started-at))
      (connect-failed-at-once (e)
        (unless fallbacks
          (error e))
        (log-info "fetch: ~a; dialing the next address for ~a" e host)
        (destructuring-bind (next-ip . next-family) (pop fallbacks)
          (setf ip next-ip
                family next-family))))))

(defun %dial-address (conn epoll-fd fetch-req host port path ip family
                      &key fallbacks started-at)
  "One attempt of INITIATE-HTTP-FETCH-TO-ADDRESS: open a non-blocking TCP
   socket to IP:PORT and queue the request on a new outbound connection.
   Signals CONNECT-FAILED-AT-ONCE when the address cannot be dialed."
  (let ((socket (handler-case
                    (make-instance (if (eq family :inet)
                                       'sb-bsd-sockets:inet-socket
                                       'sb-bsd-sockets:inet6-socket)
                                   :type :stream :protocol :tcp)
                  (error (e)
                    (error 'connect-failed-at-once
                           :address ip
                           :why (format nil "no ~(~a~) socket: ~a" family e)))))
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
                       (error 'connect-failed-at-once
                              :address ip :why (errno-string errno))))))
               (let* ((out-fd (socket-fd socket))
                      (scheme (http-fetch-continuation-scheme fetch-req))
                      (streamed (http-fetch-continuation-stream-body fetch-req))
                      (request-bytes (build-outbound-request
                                      (http-fetch-continuation-method fetch-req)
                                      host path
                                      :scheme scheme
                                      :port port
                                      :headers (http-fetch-continuation-headers fetch-req)
                                      :body (http-fetch-continuation-body fetch-req)
                                      :streamed streamed
                                      :declared-length
                                      (http-fetch-continuation-content-length fetch-req))))
                 (setf out-conn (make-connection
                                 :fd out-fd
                                 :socket socket
                                 :state :out-connecting
                                 :outbound-p t
                                 :inbound-fd (connection-fd conn)
                                 :fetch-callback (http-fetch-continuation-callback fetch-req)
                                 :fetch-on-body (http-fetch-continuation-on-body fetch-req)
                                 :fetch-method (http-fetch-continuation-method fetch-req)
                                 :fetch-sink (http-fetch-continuation-sink fetch-req)
                                 :fetch-started-at (or started-at (get-universal-time))
                                 :fetch-deadline (+ (or started-at (get-universal-time))
                                                    *fetch-timeout*)
                                 :fallback-addresses fallbacks
                                 :connect-deadline (if fallbacks
                                                       (+ (get-universal-time)
                                                          *connect-fallback-delay*)
                                                       0)
                                 :fetch-continuation fetch-req
                                 :last-active (get-universal-time)))
                 ;; Before the request is queued and before epoll is armed:
                 ;; HANDLE-OUTBOUND-CONNECT keys on HANDSHAKE-FN, so no path
                 ;; may reach a write with the transport half installed.
                 (when (eq scheme :https)
                   (funcall *tls-outbound-setup-fn* out-conn host))
                 (connection-queue-write out-conn request-bytes)
                 ;; A streamed body's pieces sent before there was a socket to
                 ;; queue them on follow the head, in the order sent. They were
                 ;; held to the backlog bound as they came, so they join the
                 ;; queue without a second check.
                 (when streamed
                   (dolist (piece (http-fetch-continuation-pending fetch-req))
                     (%queue-tail out-conn piece))
                   (setf (http-fetch-continuation-pending fetch-req) nil
                         (http-fetch-continuation-pending-bytes fetch-req) 0))
                 (register-connection out-conn)
                 (setf registered t)
                 (epoll-add epoll-fd out-fd (logior +epollout+ +epollet+))
                 (setf epoll-added t)
                 ;; Only an :INBOUND fetch parks. A detached one leaves its
                 ;; target exactly as it found it — still :STREAMING or
                 ;; :WEBSOCKET, still being written to by the app — and is
                 ;; bounded by the outbound's own deadline instead of by
                 ;; the :AWAITING sweep.
                 (ecase (http-fetch-continuation-sink fetch-req)
                   (:inbound
                    (setf (connection-state conn) :awaiting
                          (connection-awaiting-fd conn) out-fd))
                   (:detached nil))
                 (log-debug "fetch: fd ~d -> ~a :~d~a (outbound fd ~d, ~a)"
                            (connection-fd conn) host port path out-fd family)
                 (setf (http-fetch-continuation-carrier fetch-req) out-conn
                       done t)))
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
              (unregister-connection out-conn))
            ;; Last, and unconditional on REGISTERED, because what it
            ;; releases was installed before any of the flags were set.
            ;; A TLS transport is an SSL and a 16 KiB foreign staging
            ;; buffer, and both are freed by CLOSE-FN or by nothing —
            ;; the connection object is collected, the memory behind it
            ;; is not. Unwinding here without this would leak both, and the
            ;; realistic trigger is EPOLL-ADD failing with ENOSPC or
            ;; ENOMEM: exactly when the machine is already short, once
            ;; per retry. After this the outer SOCKET-CLOSE is a no-op
            ;; on a socket already closed, which is why it stays wrapped.
            (when out-conn
              (ignore-errors (connection-close out-conn)))))
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
   responses. A cond whose first match wins would take the HUP arm
   and return 502 with a complete response sitting in the kernel
   buffer. The read path runs first instead (draining to :eof and
   calling complete-fetch), and the HUP arm
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
        ;; Paused: subscribed to nothing, but epoll reports an error or a
        ;; hang-up whatever the interest. Acting on one here would walk the
        ;; chunks the pause is holding back and hand them over while the
        ;; consumer is still behind. FETCH-RESUME's re-arm brings any such
        ;; condition back as an event, and it is handled then.
        ;;
        ;; Except a request still going out, which the pause does not hold:
        ;; a wake-up armed for its writes drives them, and reads nothing.
        ((connection-fetch-paused conn)
         (when (and (eq (connection-state conn) :out-send)
                    (logtest flags (logior +epollin+ +epollout+)))
           (handle-outbound-send conn epoll-fd)))
        (t
         ;; 1. Drain readable bytes first so a batched IN+HUP sees
         ;;    the bytes before the HUP is treated as fatal.
         (when (logtest flags +epollin+)
           (case (connection-state conn)
             (:out-read (handle-outbound-read conn epoll-fd))
             ;; A handshake blocked on :WANT-READ armed EPOLLIN, so this
             ;; is the wake-up it asked for.
             (:out-handshake (handle-outbound-handshake conn epoll-fd))
             ;; A write that asked to be woken on readability. Retried as a
             ;; write, because that is the operation the transport wants
             ;; re-issued — running the read the direction suggests is a
             ;; protocol error that surfaces looking like a broken peer.
             (:out-write (when (connection-interest-inverted conn)
                           (handle-outbound-write conn epoll-fd)))
             ;; Both directions, whichever woke it; once per event.
             (:out-send (handle-outbound-send conn epoll-fd))
             ;; Stale EPOLLIN in another state — ignore silently.
             (otherwise nil)))
         ;; 2. Handle writable. Gated on (lookup-connection ...) so
         ;;    we skip if step 1 already completed and tore down
         ;;    the out-conn.
         (when (and (logtest flags +epollout+)
                    (lookup-connection (connection-fd conn)))
           (case (connection-state conn)
             (:out-connecting (handle-outbound-connect conn epoll-fd))
             (:out-handshake  (handle-outbound-handshake conn epoll-fd))
             (:out-write      (handle-outbound-write conn epoll-fd))
             ;; The mirror of the EPOLLIN arm above.
             (:out-read       (when (connection-interest-inverted conn)
                                (handle-outbound-read conn epoll-fd)))
             ;; Already driven by the EPOLLIN arm when both came together.
             (:out-send       (unless (logtest flags +epollin+)
                                (handle-outbound-send conn epoll-fd)))
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
             ;; An upstream that answered and then hung up, or reset, before
             ;; the request was all sent: what it answered is read first.
             ((eq (connection-state conn) :out-send)
              (read-while-sending conn epoll-fd)
              (when (lookup-connection (connection-fd conn))
                (deliver-fetch-error conn epoll-fd
                                     "connection error while sending the request")))
             (t
              (deliver-fetch-error conn epoll-fd "connection error"))))))
    ;; SERIOUS-CONDITION, not ERROR, because :ON-BODY runs inside this: see
    ;; HANDLE-CLIENT-READ's last catch in main.lisp.
    (serious-condition (e)
      (let ((target (lookup-connection (connection-inbound-fd conn))))
        (if (and target
                 (typep e 'fd-io-error)
                 (eql (fd-io-error-fd e) (connection-fd target)))
            ;; :ON-BODY wrote to the connection it relays into, and that
            ;; connection's own descriptor failed: the client has gone, not
            ;; the upstream. Blamed on the upstream, a stream's ON-CLOSE
            ;; would hear :UPSTREAM-FAILED and the log would carry an ERROR
            ;; for a reset any client can cause. Closed as :DISCONNECTED
            ;; instead, which tears this outbound down with it, the way any
            ;; client leaving mid-relay does.
            (progn
              (log-debug "fd ~d: ~a — the relay's client is gone, closing"
                         (connection-fd target) e)
              (close-connection target epoll-fd :disconnected)
              (when (lookup-connection (connection-fd conn))
                (close-outbound conn epoll-fd)))
            (progn
              (log-error "outbound error fd ~d: ~a" (connection-fd conn) e)
              ;; Skip the error-delivery if the connection was already torn
              ;; down (e.g. complete-fetch ran earlier in this call and
              ;; close-outbound unregistered the fd). Avoids double-delivery
              ;; of a 502 on top of an already-queued real response.
              (when (lookup-connection (connection-fd conn))
                (deliver-fetch-error conn epoll-fd
                                     "outbound request failed"))))))))

(defun natural-interest (conn)
  "The epoll interest CONN's state implies: read when it is reading, write
   when it is writing. The one assumption the whole event loop is built on,
   named here because the inversion below is the only thing that departs
   from it and has to know what to depart from."
  (if (eq (connection-state conn) :out-read)
      (logior +epollin+ +epollet+)
      (logior +epollout+ +epollet+)))

(defun invert-interest (conn epoll-fd)
  "Arm the direction opposite to what CONN's state implies, because the
   transport asked to be woken that way before the *same* operation can be
   re-issued.

   The operation is not recorded anywhere, and does not need to be: the
   state already says which one is in flight, so the dispatcher retries by
   state rather than by which direction woke it. Recording it separately
   would be a second copy of a fact the state machine already holds, and
   the two could disagree."
  (setf (connection-interest-inverted conn) t)
  (epoll-modify epoll-fd (connection-fd conn)
                (if (eq (connection-state conn) :out-read)
                    (logior +epollout+ +epollet+)
                    (logior +epollin+ +epollet+))))

(defun restore-interest (conn epoll-fd)
  "Put the interest back where the state says it belongs, if an inversion
   moved it. Gated on the flag so the ordinary path — which never inverts —
   pays nothing: an EPOLL_CTL_MOD per wake-up to restore an interest that
   was never changed is a syscall for no reason."
  (when (connection-interest-inverted conn)
    (setf (connection-interest-inverted conn) nil)
    (epoll-modify epoll-fd (connection-fd conn) (natural-interest conn))))

(defun handle-outbound-connect (conn epoll-fd)
  "Check if non-blocking connect succeeded, then hand off to the handshake
   if the transport has one, or start writing the request if it does not."
  (let ((err (get-socket-option-int (connection-fd conn)
                                     +sol-socket+ +so-error+)))
    (cond
      ((not (zerop err))
       (let ((why (format nil "connect failed: ~a" (errno-string err))))
         (unless (fall-back-to-next-address conn epoll-fd why)
           (deliver-fetch-error conn epoll-fd why))))
      ;; A transport with a handshake is not usable just because the TCP
      ;; connect landed. Writing the request here would put plaintext on a
      ;; socket the peer is expecting a ClientHello on.
      ((connection-handshake-fn conn)
       (setf (connection-state conn) :out-handshake
             (connection-fallback-addresses conn) nil)
       (handle-outbound-handshake conn epoll-fd))
      ;; Connect succeeded — start writing the request, already queued.
      (t (setf (connection-fallback-addresses conn) nil)
         (begin-request conn epoll-fd)))))

(defun fall-back-to-next-address (out epoll-fd why)
  "OUT's connect failed, for WHY, or took past its CONNECT-DEADLINE: dial
   the next of the addresses its name resolved to, and hand the fetch to
   it. Returns T when another connection has the fetch, and NIL when there
   was no address left to dial, or no connection left for the fetch, for
   the caller to end the fetch through OUT as it would have.

   The fetch moves whole. The new connection gets its own request head,
   and a streamed body's pieces queued behind OUT's head, none of them
   written since nothing connected, are queued behind it in the same
   order. A parked inbound is parked on the new connection, and the
   fetch's deadline still runs from the first attempt. OUT is closed only
   once the new connection exists, and closed without ending the fetch:
   its callback is let go and the fetch's carrier is the new connection."
  (let ((rest (connection-fallback-addresses out))
        (fetch (connection-fetch-continuation out)))
    (when (and rest fetch (eq (connection-state out) :out-connecting))
      (let ((target (if (eq (connection-fetch-sink out) :inbound)
                        (awaiting-inbound-for (connection-inbound-fd out)
                                              (connection-fd out))
                        (lookup-connection (connection-inbound-fd out))))
            (streamed (http-fetch-continuation-stream-body fetch)))
        (when target
          (multiple-value-bind (scheme host port path)
              (parse-url (http-fetch-continuation-url fetch))
            (declare (ignore scheme))
            (when streamed
              (let ((pieces (copy-list (connection-write-queue out))))
                (setf (http-fetch-continuation-pending fetch) pieces
                      (http-fetch-continuation-pending-bytes fetch)
                      (reduce #'+ pieces :key #'segment-length))))
            (log-info "fetch: ~a to ~a; dialing the next address" why host)
            (destructuring-bind ((ip . family) . more) rest
              (handler-case
                  (initiate-http-fetch-to-address
                   target epoll-fd fetch host port path ip family
                   :fallbacks more
                   :started-at (connection-fetch-started-at out))
                (error (e)
                  (log-warn "fetch: no other address for ~a could be dialed: ~a"
                            host e)
                  (when streamed
                    (setf (http-fetch-continuation-pending fetch) nil
                          (http-fetch-continuation-pending-bytes fetch) 0))
                  (return-from fall-back-to-next-address nil))))
            (setf (connection-fetch-callback out) nil)
            (close-outbound out epoll-fd)
            t))))))

(defun begin-request (conn epoll-fd)
  "CONN's transport is usable: send the request queued on it.

   A request whose body is given whole goes out in :OUT-WRITE and is read
   for its answer once it has all gone. One whose body the application
   streams goes out in :OUT-SEND, which reads as it writes: the body may be
   long in coming, and an upstream may answer before it has all arrived —
   a 413 at the head, a 401, a failure partway — which a client that only
   listened afterwards would hear only once it had sent everything, or
   never. The response is read from the start, so its cursors are set
   here rather than at the change to :OUT-READ."
  (let ((fetch (connection-fetch-continuation conn)))
    (if (and fetch (http-fetch-continuation-stream-body fetch))
        (progn
          (setf (connection-state conn) :out-send
                (connection-read-pos conn) 0
                (connection-header-scan-pos conn) 0
                (connection-out-head-start conn) 0
                (connection-out-interims conn) 0
                (connection-interest-inverted conn) nil)
          (handle-outbound-send conn epoll-fd))
        (progn
          (setf (connection-state conn) :out-write)
          (handle-outbound-write conn epoll-fd)))))

(defun handle-outbound-handshake (conn epoll-fd)
  "Run one step of the transport handshake and arm whichever direction it
   asks for, or begin the request when it finishes (BEGIN-REQUEST).

   This is the one state in the outbound machine where readable does not
   mean read and writable does not mean write. A TLS handshake exchanges
   several messages in both directions and only the handshake itself knows
   which way it is currently blocked, so the step function answers with the
   direction and this arms it.

   Confining that to a state of its own is deliberate. The steady-state
   read and write paths are built on readable-means-read, one to one, and
   generalising them is a separate and much larger change; a handshake does
   not need it, because a single state that re-arms per step expresses the
   inversion completely for as long as it lasts.

   Re-arming on every step, rather than only on a change, is the cheap and
   correct choice: EPOLL_CTL_MOD on an unchanged mask costs one syscall per
   handshake message, and tracking the current mask to avoid it would add
   state whose only purpose is to be wrong once."
  (ecase (funcall (connection-handshake-fn conn))
    (:done (begin-request conn epoll-fd))
    (:want-read
     (epoll-modify epoll-fd (connection-fd conn)
                   (logior +epollin+ +epollet+)))
    (:want-write
     (epoll-modify epoll-fd (connection-fd conn)
                   (logior +epollout+ +epollet+)))))

(defun handle-outbound-write (conn epoll-fd)
  "Flush the outbound HTTP request. When done, switch to reading the response."
  (let ((result (connection-on-write conn)))
    (case result
      (:done
       ;; The state change makes EPOLLIN natural, so an inversion that was
       ;; outstanding is resolved by this same MOD rather than by a second.
       (setf (connection-state conn) :out-read
             (connection-read-pos conn) 0
             (connection-header-scan-pos conn) 0
             (connection-out-head-start conn) 0
             (connection-out-interims conn) 0
             (connection-interest-inverted conn) nil)
       (epoll-modify epoll-fd (connection-fd conn)
                    (logior +epollin+ +epollet+)))
      ;; Must receive before it can send again. The dispatcher returns
      ;; here because it retries by state, and the state is still :OUT-WRITE.
      (:want-read (invert-interest conn epoll-fd))
      ;; :continue — more bytes to write
      (t (restore-interest conn epoll-fd)))))

;;; ---------------------------------------------------------------------------
;;; A request body the application streams
;;; ---------------------------------------------------------------------------

(defun handle-outbound-send (conn epoll-fd)
  "Drive CONN in :OUT-SEND: write what is queued of the request, read what
   the upstream has answered so far, and arm what each direction waits on.

   Both directions are tried on every wake-up, whichever one woke it. With
   both in flight the state cannot say which operation a wake-up is for,
   which is what INVERT-INTEREST relies on in the other states: under
   TLS a write can be waiting on readability while a read waits on
   writability. Retrying both is right for either, costs one read that
   finds nothing on a wake-up meant for the write, and leaves no edge
   unwatched — readability is armed throughout, writability whenever a
   write or a read is waiting on it.

   The queue draining with the body unfinished lets go of a body the
   application paused on this fetch (RELEASE-SEND-WAITER). The queue
   draining with the body finished is the end of the request, and the
   fetch goes on in :OUT-READ.

   A pause :ON-BODY takes holds the reading alone. The request goes on
   going out, and readability is armed again by FETCH-RESUME, unless a
   write is waiting on it."
  (let ((fetch (connection-fetch-continuation conn))
        (want-write nil)
        (write-wants-read nil)
        (drained nil))
    (when (plusp (connection-write-pending conn))
      (let ((before (connection-write-pending conn)))
        (ecase (connection-on-write conn)
          (:continue (setf want-write t))
          ;; The wake-up for readability, armed below, re-issues it.
          (:want-read (setf write-wants-read t))
          (:done (setf drained t)))
        (when (< (connection-write-pending conn) before)
          (note-request-progress conn))))
    ;; The answer, unless :ON-BODY has paused it. A pause holds the
    ;; reading and not the request, which goes on going out.
    (unless (connection-fetch-paused conn)
      (case (read-while-sending conn epoll-fd)
        ;; Answered or failed: the fetch is over and closed.
        (:ended (return-from handle-outbound-send))
        (:want-write (setf want-write t))))
    (cond
      ((and (http-fetch-continuation-finished fetch)
            (zerop (connection-write-pending conn)))
       (begin-response-read conn epoll-fd))
      (t
       (when drained
         (release-send-waiter conn epoll-fd))
       (epoll-modify epoll-fd (connection-fd conn)
                     (logior +epollet+
                             (if (or (not (connection-fetch-paused conn))
                                     write-wants-read)
                                 +epollin+
                                 0)
                             (if want-write +epollout+ 0)))))))

(defun read-while-sending (conn epoll-fd)
  "Read what CONN's upstream has sent while its request is still going
   out, and walk it. Answers :ENDED when that ended the fetch — an answer
   complete before the request was, or a failure — :PAUSED when :ON-BODY
   paused it, :WANT-WRITE when the transport has to write before it can
   read again, or NIL to wait for more.

   The walk is :OUT-READ's, without its arming: HANDLE-OUTBOUND-SEND arms
   both directions at once. A read cut short by the buffer's cap is read
   again here, in this wake-up. :OUT-READ is woken for that by
   writability, which a socket with a request still filling it does not
   have at once."
  (when (connection-fetch-rewalk conn)
    (setf (connection-fetch-rewalk conn) nil)
    (let ((verdict (walk-outbound-response conn epoll-fd :buffered :arm nil)))
      (unless (eq verdict :reading)
        (return-from read-while-sending verdict))))
  ;; Sixteen reads at most, as a received body is read in at most sixteen
  ;; windows a wake-up: enough for a relay that frees what it hands on, and
  ;; a bound for one that does not.
  (loop repeat 16
        do (let ((result (connection-read-available conn)))
             (case result
               ;; End of stream: an answer framed by the close, or an
               ;; upstream that hung up on the request. COMPLETE-FETCH tells
               ;; the two apart.
               (:eof (complete-fetch conn epoll-fd) (return :ended))
               (:full
                (deliver-fetch-error
                 conn epoll-fd (format nil "response exceeds ~d bytes (cap)"
                                       *max-outbound-response-size*))
                (return :ended))
               (:again (return nil))
               (:want-write (return :want-write))
               ((:ok :ok-eof :ok-want-write)
                (multiple-value-bind (verdict wants)
                    (walk-outbound-response conn epoll-fd result :arm nil)
                  (cond
                    ((not (eq verdict :reading)) (return verdict))
                    ((eq wants :write) (return :want-write))
                    ;; Cut at the cap: read again.
                    ((eq wants :cut) nil)
                    (t (return nil)))))))
        ;; Still cut after every round: writability, when it comes, wakes
        ;; the read again.
        finally (return :want-write)))

(defun begin-response-read (conn epoll-fd)
  "CONN's streamed request has all gone out: read the rest of its answer
   in :OUT-READ.

   What arrived of the answer while the request was going out stays where
   it is, walked as far as it goes. The fetch's own deadline starts here,
   since what it bounds is the upstream answering, and a request answered
   with this fetch starts its wait for the answer here too. Before this
   point both were waiting on the body, which progress bounded."
  (setf (connection-state conn) :out-read
        (connection-interest-inverted conn) nil
        (connection-fetch-deadline conn) (+ (get-universal-time) *fetch-timeout*))
  (let ((in (awaiting-inbound-for (connection-inbound-fd conn) (connection-fd conn))))
    (when in
      (setf (connection-last-active in) (get-universal-time))))
  ;; A fetch :ON-BODY has paused reads nothing until FETCH-RESUME, which
  ;; wakes it from :OUT-READ as from any pause there.
  (epoll-modify epoll-fd (connection-fd conn)
                (if (connection-fetch-paused conn)
                    +epollet+
                    (logior +epollin+ +epollet+))))

(defun note-request-progress (out)
  "Some of OUT's streamed request moved — the application sent more, or
   the upstream took more — so its deadline moves out again. While a
   request body is going out the deadline is a bound on silence, not on
   the whole: an upload takes as long as it keeps moving."
  (setf (connection-fetch-deadline out) (+ (get-universal-time) *fetch-timeout*)))

(defun release-send-waiter (out epoll-fd)
  "OUT's streamed request body has drained. If its application was told
   to wait for that — FETCH-SEND answered NIL — the wait is over, and a
   body the application paused on it, still being received, is resumed.

   Only a body FETCH-SEND told to wait: a pause the application took for
   reasons of its own is the application's to end."
  (%release-target-waiter (connection-inbound-fd out) epoll-fd))

(defun %flush-request-body (out)
  "Write what OUT's queue holds, from FETCH-SEND or FETCH-FINISH.
   Returns T when the queue emptied.

   A remainder is left to OUT's own event handler, with writability and
   readability armed for it. So is a write that fails: the handler meets
   the same failure and ends the fetch as every upstream failure is ended,
   where raising here would raise into the callback that sent the bytes —
   an inbound body's ON-DATA — and blame that connection for its
   upstream's."
  (let ((drained (handler-case (eq (connection-on-write out) :done)
                   (error () nil))))
    (unless drained
      (ignore-errors
       (epoll-modify *epoll-fd* (connection-fd out)
                     (logior +epollin+ +epollout+ +epollet+))))
    drained))

(defun %check-fetch-owner (fetch who)
  "Signal unless FETCH may be written to from here: off a worker, or on a
   fetch another worker carries. As WS-SEND and BODY-RESUME refuse, and for
   the same reason — the queue and the epoll interest are the carrying
   worker's alone."
  (let ((carrier (http-fetch-continuation-carrier fetch)))
    (when carrier
      (unless *epoll-fd*
        (error "~a: no event loop on this thread. A streamed fetch is sent to ~
                from the worker that carries it — a body's callbacks, a ~
                fetch's :THEN, an :ON-TICK."
               who))
      (unless (eq (lookup-connection (connection-fd carrier)) carrier)
        (error "~a: the fetch to ~a is carried by another worker. Only that ~
                worker may send its body."
               who (http-fetch-continuation-url fetch))))))

(defun %mark-send-waiting (fetch)
  "Record, on the connection FETCH was started for, that its application
   has been told to wait for FETCH's body to drain."
  (let ((target (and (>= (http-fetch-continuation-target-fd fetch) 0)
                     (lookup-connection (http-fetch-continuation-target-fd fetch)))))
    (when target
      (setf (connection-send-waiting target) t))))

(defun %queue-request-piece (fetch piece &key (bounded t))
  "Put PIECE, framed, behind what FETCH's request has queued: on its
   outbound's write queue once there is one, held on FETCH until then.
   Returns the outbound, or NIL while there is none. Signals at
   *MAX-WRITE-BACKLOG* when BOUNDED."
  (let ((carrier (http-fetch-continuation-carrier fetch)))
    (if (and carrier (not (eq (connection-state carrier) :out-dns)))
        (progn
          (if bounded
              (unless (connection-append-write carrier piece)
                (error "fetch-send: the fetch to ~a is at *max-write-backlog* ~
                        (~d bytes pending, this piece is ~d); the upstream is ~
                        not keeping up, and FETCH-SEND answering NIL was the ~
                        signal to pause."
                       (http-fetch-continuation-url fetch)
                       (connection-write-pending carrier) (length piece)))
              (if (zerop (connection-write-pending carrier))
                  (connection-append-write carrier piece)
                  (%queue-tail carrier piece)))
          (note-request-progress carrier)
          carrier)
        (progn
          (when (and bounded
                     (> (+ (http-fetch-continuation-pending-bytes fetch) (length piece))
                        *max-write-backlog*))
            (error "fetch-send: the fetch to ~a has ~d bytes waiting for its ~
                    upstream to connect, and this piece of ~d would pass ~
                    *max-write-backlog*; FETCH-SEND answering NIL was the ~
                    signal to pause."
                   (http-fetch-continuation-url fetch)
                   (http-fetch-continuation-pending-bytes fetch) (length piece)))
          (setf (http-fetch-continuation-pending fetch)
                (append (http-fetch-continuation-pending fetch) (list piece)))
          (incf (http-fetch-continuation-pending-bytes fetch) (length piece))
          nil))))

(defun fetch-send (fetch bytes)
  "Send BYTES as the next piece of FETCH's request body, a fetch made with
   :BODY :STREAM. Returns T when everything sent so far has reached the
   kernel, NIL when some of it is waiting, or :ENDED when the fetch is
   over and nothing was sent.

   NIL is backpressure, the same answer STREAM-SEND and WS-SEND give and
   for the same reason: the upstream is not keeping up, and the producer
   should stop until it has. A producer that is a body being received —
   RECEIVE-BODY's ON-DATA, RECEIVE-MULTIPART's ON-PART-DATA — returns
   :PAUSE, and the body is resumed when FETCH's request drains, without
   anything more from the application. A send made before the upstream has
   connected answers NIL too: its bytes wait for the connection, and the
   resume comes once they have gone. Any other producer, an :ON-TICK for
   one, asks again with an empty BYTES, which sends nothing and answers as a
   send would: T once the upstream has caught up.

   :ENDED means the fetch ended while the body was being sent: the upstream
   answered early (a 413, a 401), failed, or :ON-BODY stopped it. Its :THEN
   has heard how. The bytes are dropped, so a producer may keep calling
   until it learns, and stop then.

   The bytes are copied, so BYTES may be reused at once. Chunked unless
   the fetch declared :CONTENT-LENGTH, each call one chunk.

   Signals:
     - FETCH's body was given whole, or has been finished (FETCH-FINISH);
     - BYTES would take the body past its declared :CONTENT-LENGTH;
     - the bytes waiting would pass *MAX-WRITE-BACKLOG*, which is what
       sending on through NIL answers comes to;
     - off a worker, or on a fetch another worker carries.
   A send before the fetch has started is held for it, on any thread."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (unless (http-fetch-continuation-stream-body fetch)
    (error "fetch-send: the fetch to ~a was given its body whole; one made ~
            with :body :stream has its body sent with FETCH-SEND"
           (http-fetch-continuation-url fetch)))
  (cond
    ((http-fetch-continuation-ended fetch) :ended)
    ((http-fetch-continuation-finished fetch)
     (error "fetch-send: the body of the fetch to ~a is finished; ~
             FETCH-FINISH has been called"
            (http-fetch-continuation-url fetch)))
    (t
     (let ((declared (http-fetch-continuation-content-length fetch))
           (sent (http-fetch-continuation-sent fetch)))
       (when (and declared (> (+ sent (length bytes)) declared))
         (error "fetch-send: ~d more bytes would take the body of the fetch ~
                 to ~a past the ~d it declared (~d sent)"
                (length bytes) (http-fetch-continuation-url fetch) declared sent))
       (%check-fetch-owner fetch "fetch-send")
       (let ((out (if (zerop (length bytes))
                      ;; Nothing to queue: the call is the question alone.
                      (let ((c (http-fetch-continuation-carrier fetch)))
                        (and c (not (eq (connection-state c) :out-dns)) c))
                      (%queue-request-piece
                       fetch (if declared (copy-seq bytes) (encode-chunk bytes))))))
         (incf (http-fetch-continuation-sent fetch) (length bytes))
         (if (and out
                  (eq (connection-state out) :out-send)
                  (%flush-request-body out))
             t
             (progn (%mark-send-waiting fetch) nil)))))))

(defun fetch-finish (fetch)
  "End FETCH's streamed request body. Returns T, or :ENDED when the fetch
   is already over.

   A chunked body gets its terminator; a body with a declared
   :CONTENT-LENGTH must have been sent in full, and signals otherwise —
   the upstream would wait for the rest, or read the next request out of
   it. Once the body has gone out the fetch reads the upstream's answer,
   under its *FETCH-TIMEOUT* from there.

   Calling it twice is calling it once. A body handler's ON-END that
   answers with FETCH finishes it if it has not been (see ADOPT-FETCH).
   Signals off a worker or on a fetch another worker carries, as
   FETCH-SEND does."
  (unless (http-fetch-continuation-stream-body fetch)
    (error "fetch-finish: the fetch to ~a was given its body whole"
           (http-fetch-continuation-url fetch)))
  (cond
    ((http-fetch-continuation-ended fetch) :ended)
    ((http-fetch-continuation-finished fetch) t)
    (t
     (let ((declared (http-fetch-continuation-content-length fetch))
           (sent (http-fetch-continuation-sent fetch)))
       (when (and declared (/= sent declared))
         (error "fetch-finish: the body of the fetch to ~a is ~d bytes of the ~
                 ~d it declared"
                (http-fetch-continuation-url fetch) sent declared))
       (%check-fetch-owner fetch "fetch-finish")
       (setf (http-fetch-continuation-finished fetch) t)
       (let ((out (if declared
                      (let ((c (http-fetch-continuation-carrier fetch)))
                        (and c (not (eq (connection-state c) :out-dns)) c))
                      ;; Five bytes, after everything else the body queued:
                      ;; held to no bound, as a terminator is never refused.
                      (%queue-request-piece fetch (chunked-terminator)
                                            :bounded nil))))
         (when (and out
                    (eq (connection-state out) :out-send)
                    (%flush-request-body out))
           (begin-response-read out *epoll-fd*)))
       t))))

(defun adopt-fetch (conn fetch)
  "Answer CONN's request with FETCH, a streamed fetch already started for
   it: what FETCH's :THEN returns becomes the request's response, as it
   does for a fetch a handler returns. The shape of an upload forwarded
   upstream as it arrives — the body's ON-END answers with the fetch the
   body went into, and the client hears what the upstream said.

   Returns :PARKED when the answer is still to come: CONN waits in
   :AWAITING on FETCH's carrier, as a handler's fetch parks it, and the
   fetch delivers to it from then on. Returns :ANSWER and the answer when
   FETCH has already ended — an upstream that answered before the body
   was all sent, or failed — since its :THEN ran then and its value was
   kept; a fetch that ended with no value kept answers 502.

   FETCH's body is finished first if it has not been: an answer is the
   statement that the body is done. Signals for a fetch started for
   another connection."
  (unless (= (http-fetch-continuation-target-fd fetch) (connection-fd conn))
    (error "a fetch started for fd ~d cannot answer the request on fd ~d"
           (http-fetch-continuation-target-fd fetch) (connection-fd conn)))
  (fetch-finish fetch)
  (setf (connection-fetch-outstanding conn) nil
        (connection-send-waiting conn) nil)
  (if (http-fetch-continuation-ended fetch)
      (values :answer (if (http-fetch-continuation-answered fetch)
                          (http-fetch-continuation-answer fetch)
                          (make-error-response 502)))
      (let ((carrier (http-fetch-continuation-carrier fetch)))
        ;; From here the fetch is a parked one: its result answers CONN, a
        ;; failure answers CONN 502, and the :AWAITING sweep bounds the wait
        ;; once the request has all gone out (see BEGIN-RESPONSE-READ).
        (setf (http-fetch-continuation-sink fetch) :inbound
              (connection-fetch-sink carrier) :inbound
              (connection-state conn) :awaiting
              (connection-awaiting-fd conn) (connection-fd carrier)
              (connection-last-active conn) (get-universal-time))
        :parked)))

(defun forward-body (fetch &key (max-size *max-body-size*) on-abort)
  "A body plan, for a body handler to return: send the request's body
   upstream as FETCH's streamed body, as it arrives, and answer the
   request with what FETCH's :THEN returns. An upload proxy in one form:

     (forward-body (http-fetch :put (object-url request)
                               :body :stream
                               :content-length (declared-length request)
                               :then (lambda (status headers body)
                                       (declare (ignore headers body))
                                       (if (eql status 200)
                                           (make-text-response 201 \"stored\")
                                           (make-error-response 502)))))

   The body is never held. FETCH starts with the body's first bytes, each
   run is sent as it arrives, and the body pauses whenever the upstream
   falls behind and resumes when it catches up, so the client is held to
   the upstream's pace. FETCH's body is finished when the request's is.

   An upstream that answers before the body is all sent — a 413, a 401 —
   has answered the client: as soon as :THEN returns, the rest of the body
   is refused with what it returned, if that is an HTTP-RESPONSE, or 502,
   whether or not the client is still sending. A failure is answered the
   same way, :THEN hearing it as the abort sentinel, and so is a fetch
   that cannot be started at all, a connect the kernel refuses at once
   among them. A :THEN that raises is a 500. ON-ABORT is RECEIVE-BODY's,
   and hears :REFUSED for a body refused in any of these ways. MAX-SIZE
   caps the body as RECEIVE-BODY's does.

   FETCH must be streamed (:BODY :STREAM) and not yet started."
  (unless (http-fetch-continuation-stream-body fetch)
    (error "forward-body: the fetch to ~a was given its body whole; it needs ~
            :body :stream"
           (http-fetch-continuation-url fetch)))
  (when (fetch-started-p fetch)
    (error "forward-body: the fetch to ~a has already been started"
           (http-fetch-continuation-url fetch)))
  (let ((then (http-fetch-continuation-callback fetch))
        (plan nil)
        ;; The connection FETCH was started for, from its first run on.
        (target nil))
    (labels ((answer-for (answer)
               (if (typep answer 'http-response)
                   answer
                   (make-error-response 502)))
             (start (conn)
               ;; T once FETCH has started for CONN. :KEEP, so a failure
               ;; reaches the client as :THEN's answer rather than as a
               ;; closed connection.
               (setf target conn)
               (handler-case (fetch-into conn fetch :failure-disposition :keep)
                 ;; FETCH-INTO signals a fetch that could not be started,
                 ;; having done nothing. Here it ends as a fetch that fails
                 ;; later does, and as a handler's fetch that fails to
                 ;; start does: :THEN hears the abort sentinel, and its
                 ;; answer refuses the body.
                 (error (e)
                   (log-error "fetch setup failed: ~a" e)
                   (finish-fetch (http-fetch-continuation-callback fetch) :aborted)
                   nil))))
      ;; :THEN, and then, if the body FETCH was started for is still
      ;; arriving, the rest of it refused with :THEN's answer: the fetch has
      ;; ended first, answered early or failed. Refused as it ends, rather
      ;; than at the body's next run, which a client that has stopped
      ;; sending for now may not send before *BODY-STALL-TIMEOUT* ends the
      ;; body unanswered. The answer starts as a 500, which is what it stays
      ;; when :THEN raises. Not in a drain, which closes the fetch on its way
      ;; to closing the connection, and ends the body as :SHUTDOWN.
      (setf (http-fetch-continuation-callback fetch)
            (lambda (status headers body)
              (let ((answer (make-error-response 500)))
                (unwind-protect (setf answer (funcall then status headers body))
                  (when (and target
                             (eq (connection-body-receiver target) plan)
                             (not (worker-stopping-p)))
                    (body-refuse target (answer-for answer)))))))
      (setf plan
            (receive-body
             :max-size max-size
             :on-data (lambda (conn bytes)
                        (when (or (fetch-started-p fetch) (start conn))
                          (let ((sent (fetch-send fetch bytes)))
                            (cond ((eq sent t) nil)
                                  ((null sent) :pause)
                                  (t (answer-for (http-fetch-continuation-answer fetch)))))))
             :on-end (lambda (conn)
                       (declare (ignore conn))
                       ;; A body that had no bytes never started FETCH;
                       ;; finished, it starts here as an ordinary answer, its
                       ;; body empty.
                       (fetch-finish fetch)
                       fetch)
             :on-abort on-abort)))))

(defun fetch-resume (conn)
  "Resume an outbound response that ON-BODY paused.

   The chunks the pause left in the buffer are handed over first, then
   reading goes on. The outbound is woken through writability, which its
   socket has at once — the request went out before the response began —
   so the resume does not wait on more bytes arriving. That matters
   twice. The chunks already read raise no readability edge of their own,
   and nor do bytes OpenSSL has decrypted and not yet handed over.
   HANDLE-OUTBOUND-READ walks the buffer, then reads.

   CONN is the outbound connection ON-BODY was called for. A no-op if it
   is not paused, and once it is closed, so a producer that calls this on
   every pass rather than tracking state is not punished for it."
  (when (and (connection-fetch-paused conn)
             (>= (connection-fd conn) 0))
    ;; The total runs on unpaused time. A relay applying backpressure is
    ;; deliberately not reading, so a deadline that kept running while
    ;; paused would kill a correct app for taking the framework's own
    ;; advice — which is the 504-on-a-healthy-upstream this seam prevents.
    ;;
    ;; The cost is that the deadline is not a bound on wall clock: a target
    ;; that trickles restarts WRITE-PROGRESS-AT on every byte it accepts,
    ;; so it never stalls and can extend this indefinitely. Memory stays
    ;; capped and the connection is visible in the census; the residual is
    ;; stated in README Limitations rather than papered over here.
    (when (plusp (connection-fetch-paused-at conn))
      (incf (connection-fetch-deadline conn)
            (max 0 (- (get-universal-time)
                      (connection-fetch-paused-at conn)))))
    (setf (connection-fetch-paused conn) nil
          (connection-fetch-paused-at conn) 0)
    ;; Drop the back-link with the pause it belongs to, so an inbound that
    ;; drains later does not resume something that resumed itself.
    (let ((in (lookup-connection (connection-inbound-fd conn))))
      (when (and in (= (connection-paused-outbound-fd in)
                       (connection-fd conn)))
        (setf (connection-paused-outbound-fd in) -1)))
    (when *epoll-fd*
      (if (eq (connection-state conn) :out-send)
          ;; Both directions, as :OUT-SEND always arms them. Writability
          ;; wakes it at once, and the wake-up walks what the pause held
          ;; before it reads on.
          (epoll-modify *epoll-fd* (connection-fd conn)
                        (logior +epollin+ +epollout+ +epollet+))
          (invert-interest conn *epoll-fd*))))
  (values))

(defun resume-paused-outbound (conn epoll-fd)
  "End a pause that CONN's own write backlog caused, now that it has
   drained. A no-op unless an outbound paused while relaying into CONN.

   This is the inbound->outbound edge FETCH-RESUME alone does not provide.
   Called where the backlog actually empties rather than on every
   write, because a relay whose client is keeping up never pauses at all
   and should not pay for the check twice per pass.

   The outbound is looked up rather than held: it can be closed and its fd
   reused between the pause and this call, and a stale pointer would
   resume a stranger. The FD-identity check is the same discipline
   CLOSE-OUTBOUND uses."
  (let ((out-fd (connection-paused-outbound-fd conn)))
    (when (>= out-fd 0)
      (setf (connection-paused-outbound-fd conn) -1)
      (let ((out (lookup-connection out-fd)))
        (when (and out
                   (connection-outbound-p out)
                   (connection-fetch-paused out)
                   (= (connection-inbound-fd out) (connection-fd conn)))
          (let ((*epoll-fd* epoll-fd))
            (fetch-resume out)))))))

(defun paused-behind-a-backlog-p (out)
  "T while the outbound OUT is paused and the connection it relays into
   still has bytes queued.

   That pause has a drain under way, and the drain is what ends it: the
   target's queue empties and RESUME-PAUSED-OUTBOUND resumes the fetch.
   The fetch's own deadline is the wrong bound on it, because the time is
   the client's to take — a slow link draining a full backlog legitimately
   outlasts *FETCH-TIMEOUT*. *WRITE-STALL-TIMEOUT* on the target is the
   right one: a queue that stops moving closes the target, and closing the
   target reaps OUT. A pause over an empty queue has no drain coming, so
   it is stranded rather than waiting, and the deadline still applies."
  (and (connection-fetch-paused out)
       (let ((target (lookup-connection (connection-inbound-fd out))))
         (and target (plusp (connection-write-pending target)) t))))

(defun drop-delivered-chunks (conn body-start next-scan)
  "Free the chunks :ON-BODY has been handed: shift CONN's read buffer
   down over BUF[BODY-START, NEXT-SCAN), keeping the head before it and
   the bytes after it, and move the read and walk cursors with them.

   A relay hands each chunk on as its framing is proved, and nothing
   reads those bytes again: COMPLETE-FETCH answers :THEN with a NIL body
   once the walk has reached the terminator. Kept, they would hold the
   whole upstream in memory for the life of the fetch, and a stream
   longer than *MAX-OUTBOUND-RESPONSE-SIZE* would fail at the cap however
   steadily it flowed. Dropped, the buffer holds the head and one
   unfinished chunk, and the cap bounds a chunk instead of the stream.

   The head stays because COMPLETE-FETCH reads the status and headers
   from it. NEXT-SCAN is a chunk boundary, so what is left begins with a
   chunk-size line and the walk resumes on it."
  (let* ((buf (connection-read-buf conn))
         (pos (connection-read-pos conn))
         (gone (- next-scan body-start)))
    (when (plusp gone)
      (replace buf buf :start1 body-start :start2 next-scan :end2 pos)
      (setf (connection-read-pos conn) (- pos gone)
            (connection-chunk-scan-pos conn) body-start)
      (connection-shrink-read-buf conn))))

(defun handle-outbound-read (conn epoll-fd)
  "Read the outbound HTTP response. When complete, deliver to the inbound
   connection.

   Chunks a pause left in the buffer are walked first, before anything
   more is read (see FETCH-REWALK). A pause halts the walk at the chunk it
   was given for, so the rest of that read is still here. Nothing would
   report it again — no readability edge fires for bytes already in user
   space — and a buffer they fill to the cap would answer the next read
   :FULL, a 502 for a relay doing what it was told."
  (when (connection-fetch-rewalk conn)
    (setf (connection-fetch-rewalk conn) nil)
    (unless (eq (walk-outbound-response conn epoll-fd :buffered) :reading)
      (return-from handle-outbound-read)))
  (let ((result (connection-read-available conn)))
    (case result
      (:eof
       ;; Read nothing, already at end of stream. For a close-delimited
       ;; response that IS the framing, and no bytes came with it, so
       ;; there is nothing to walk before completing.
       ;;
       ;; :OK-EOF is the same end of stream with the last bytes attached,
       ;; and it reads below rather than here: those bytes still have to
       ;; be walked and handed to ON-BODY before the fetch completes.
       ;; Sharing this arm would skip that walk, and a chunked response
       ;; whose last bytes and FIN landed in one read would reach the app
       ;; as a 200 with no body at all — not truncated, absent. Telling
       ;; the two verdicts apart is the entire reason
       ;; CONNECTION-READ-AVAILABLE reports them separately.
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
      (:again (restore-interest conn epoll-fd))  ; wait for more data
      ;; Nothing read; the transport wants to send first.
      (:want-write (invert-interest conn epoll-fd))
      ((:ok :ok-eof :ok-want-write)
       (walk-outbound-response conn epoll-fd result)))))

(defun walk-outbound-response (conn epoll-fd result &key (arm t))
  "Walk the response in CONN's read buffer, hand :ON-BODY its chunks, and
   act on what the walk and the verdicts decided. RESULT is what
   CONNECTION-READ-AVAILABLE answered for the read that brought the bytes,
   or :BUFFERED for chunks a pause left behind, walked with no read since.
   Answers :READING when the fetch reads on, :PAUSED, or :ENDED.

   ARM NIL leaves the epoll interest of a fetch that reads on to the
   caller, and says what it wanted as a second value: :WRITE for a
   transport that must write before reading again, :CUT for a read the
   buffer's cap cut short, or NIL. :OUT-SEND arms both directions itself.

   Is the response framed-complete yet? OUTBOUND-RESPONSE-COMPLETE-P is
   the one definition of \"done\", shared with the TLS path. CHUNK-SCAN-POS
   carries the chunked walk's resume offset across reads, so a dribbling
   upstream is walked once, not rescanned from the top on every wake-up.

   Completion follows the framing, not the socket's lifetime. Waiting for
   EOF would work only because outbound requests default to Connection:
   close, and even then it would cost a round trip, with the whole
   response sitting in our buffer while we waited for the upstream's FIN.
   An app that overrides Connection to keep-alive would get an upstream
   that never closes, and the fetch would hang until the :awaiting reaper
   dropped the inbound with no response at all. Close-delimited responses
   (no CL, no TE) never satisfy the test: for those, EOF genuinely is the
   framing, so they complete from the :OK-EOF arm below or from
   HANDLE-OUTBOUND-READ's :EOF arm.

   A verdict takes effect at the chunk it was given for. :PAUSE or :STOP
   halts the walk there, so no later chunk is handed over in the same
   pass — however many the read brought. A pass's chunks are bounded only
   by the read cap, and a consumer applying backpressure has to be able to
   stop at the chunk it decided on, or its own queue fills behind its
   back. What a pause leaves in the buffer is walked first when the fetch
   resumes."
  (let ((pause nil)
        (stop nil)
        ;; The drain stopped because the buffer reached the cap, not
        ;; because the transport ran dry, so more may be waiting — see the
        ;; CUT arm below.
        (cut (>= (connection-read-pos conn) (connection-read-cap conn))))
    (multiple-value-bind (complete next-scan chunked-body-start)
        (outbound-response-complete-p
         (connection-read-buf conn)
         (connection-read-pos conn)
         (connection-fetch-method conn)
         (connection-chunk-scan-pos conn)
         (when (connection-fetch-on-body conn)
           (lambda (buf start end)
             ;; SUBSEQ rather than the shared buffer: the app keeps
             ;; whatever it is handed, and the read buffer is reused on
             ;; the next wake-up.
             (case (funcall (connection-fetch-on-body conn)
                            conn (subseq buf start end))
               (:pause (setf pause t) :halt)
               (:stop  (setf stop t)  :halt)
               (t nil))))
         ;; Found incrementally; see OUTBOUND-RESPONSE-HEAD.
         (outbound-response-head conn))
      (setf (connection-chunk-scan-pos conn) next-scan)
      ;; A relay that is still reading frees what it has handed on; a fetch
      ;; ending in this pass needs nothing freed.
      (when (and chunked-body-start
                 (connection-fetch-on-body conn)
                 (not complete)
                 (not stop))
        (drop-delivered-chunks conn chunked-body-start next-scan))
      (cond
        ;; A halted walk is never complete, so at most one of the first
        ;; three holds.
        (stop (stop-fetch conn epoll-fd) :ended)
        (complete (complete-fetch conn epoll-fd :framing-complete t) :ended)
        ;; Ahead of :OK-EOF. A pause halts the walk with chunks still in
        ;; the buffer, and completing on the end of stream would judge a
        ;; body the app has not been handed yet. The resume walks them, and
        ;; reaches the terminator or the end of stream from there.
        (pause (pause-outbound conn epoll-fd) :paused)
        ;; Bytes walked and delivered, then end of stream.
        ((eq result :ok-eof) (complete-fetch conn epoll-fd) :ended)
        ;; Bytes arrived, then the transport asked to send. The response is
        ;; incomplete, so this same read is re-issued.
        ((eq result :ok-want-write)
         (if arm
             (progn (invert-interest conn epoll-fd) :reading)
             (values :reading :write)))
        ;; Buffered chunks walked with nothing halting them: the caller
        ;; reads next, in the same wake-up.
        ((eq result :buffered) :reading)
        ;; A drain cut short by the cap has to run again, and nothing else
        ;; would wake it. Bytes still in the kernel raise no new edge, and
        ;; under TLS OpenSSL can hold decrypted bytes the socket knows
        ;; nothing of. Writability is the wake-up: the request is sent, so
        ;; the socket is writable at once, and the :OUT-READ dispatcher
        ;; re-issues the read on an inverted interest. A relay has freed
        ;; what it handed on, so the read finds room. A buffered fetch
        ;; finds none and answers :FULL, a prompt 502 rather than a wait
        ;; for bytes that may never come.
        (cut (if arm
                 (progn (invert-interest conn epoll-fd) :reading)
                 (values :reading :cut)))
        (t (when arm (restore-interest conn epoll-fd)) :reading)))))

(defun pause-outbound (conn epoll-fd)
  "Stop CONN reading: the backpressure ON-BODY asked for.

   Every event is dropped from the interest, so the upstream's bytes stay
   in the kernel and its send window fills — the disposition a relay
   wants, since the client has not misbehaved, it is on a slower link.
   HANDLE-OUTBOUND-EVENT ignores the error and hang-up reports epoll sends
   regardless, so nothing is handed over until FETCH-RESUME. FETCH-REWALK
   is set because the walk halted at a chunk and what followed it in the
   read is still in the buffer.

   In :OUT-SEND the request may still be going out, and the pause is not
   on it: HANDLE-OUTBOUND-SEND, whose read this pause came from, arms its
   writes again before it returns."
  (setf (connection-fetch-paused conn) t
        (connection-fetch-paused-at conn) (get-universal-time)
        (connection-fetch-rewalk conn) t
        (connection-interest-inverted conn) nil)
  (epoll-modify epoll-fd (connection-fd conn) +epollet+)
  ;; The back-link RESUME-PAUSED-OUTBOUND reads; its docstring has why the
  ;; edge exists.
  (let ((in (lookup-connection (connection-inbound-fd conn))))
    (when in
      (setf (connection-paused-outbound-fd in) (connection-fd conn)))))

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

(defun outbound-response-complete-p (buf end method &optional (chunk-scan 0) on-data
                                                         (head nil head-p))
  "Return (values COMPLETE-P NEXT-CHUNK-SCAN CHUNKED-BODY-START) for the
   outbound HTTP response accumulated in BUF[0..END), given the request
   METHOD. Thread NEXT-CHUNK-SCAN back in on the following call so a
   chunked body is walked once across a growing buffer (see
   CHUNKED-BODY-COMPLETE-P).

   HEAD, when given, is OUTBOUND-RESPONSE-HEAD's answer for this buffer —
   (START . HEADER-END), or NIL for a head still arriving — and the
   search for it is not repeated here. HANDLE-OUTBOUND-READ passes it, so
   a connection's head is searched incrementally; a caller holding only a
   buffer leaves it out, and the head is found from the top.

   CHUNKED-BODY-START is where the body begins, given only when the
   response's Transfer-Encoding is exactly chunked, the one framing the
   walk is sure of. BUF[CHUNKED-BODY-START, NEXT-CHUNK-SCAN) is then
   chunks the walk has proved whole and ON-DATA has been handed, which a
   caller relaying them no longer needs: DROP-DELIVERED-CHUNKS frees it.

   One definition of 'the response is done', reached through
   HANDLE-OUTBOUND-READ for every outbound response, http:// and https://
   alike — TLS is a transport under that read, not a second reader. Two
   readers can disagree, one recognizing the chunked terminator and the
   other reading to EOF, and then the same upstream is handled cleanly
   over one scheme and stalls over the other. A single predicate is the
   only way that divergence cannot happen.

   Framing, in RFC 7230 §3.3.3 order:
     HEAD           — no body ever (§4.3.2), even when the upstream
                      echoes the GET body's Content-Length. Done at
                      headers.
     204 or 304     — no body ever (rule 1, RFC 9112 §6.3), whatever
                      Content-Length or Transfer-Encoding says; a 304's
                      Content-Length describes the representation, not
                      this response. Done at headers. COMPLETE-FETCH and
                      STREAM-RESPONSE-LINES read these two statuses the
                      same way, and an upstream that keeps the connection
                      open would otherwise be waited on for a body it
                      will never send, until the fetch timed out.
     Transfer-Encoding present — TE wins over Content-Length. Chunked, as
                      RESPONSE-CHUNKED-P reads the headers: done at the
                      zero-size chunk header. Any other coding is framed
                      by EOF (RFC 9112 §6.3), so NIL, and the bytes are
                      not walked at all: their first is not a chunk size,
                      and reading it as one would refuse a body that was
                      never chunk-framed. COMPLETE-FETCH and
                      STREAM-RESPONSE-LINES ask RESPONSE-CHUNKED-P the
                      same question, so all three agree on the framing.
     Content-Length — done when that many body bytes have landed.
     Neither        — close-delimited. NIL forever: EOF *is* the framing,
                      and the caller's EOF branch completes it.

   A Transfer-Encoding that is not the single token chunked has its
   headers parsed on every read to ask that question, since nothing
   remembers the answer between reads. Such a response is rare and usually
   malformed, and remembering would take a connection slot."
  ;; Step over any 1xx interim blocks first. This has to happen HERE and
  ;; not only in COMPLETE-FETCH: the framing decision below reads
  ;; Transfer-Encoding / Content-Length out of the header region, and an
  ;; interim block carries neither. Anchoring on the interim would make a
  ;; CL-framed 200 look close-delimited, so the predicate would answer NIL
  ;; forever and the fetch would hang until *FETCH-TIMEOUT* on any upstream
  ;; that keeps the connection alive.
  (let* ((start (if head-p
                    (car head)
                    (skip-interim-responses buf 0 end)))
         (header-end (if head-p
                         (cdr head)
                         (scan-crlf-crlf buf start end))))
    (unless header-end
      (return-from outbound-response-complete-p (values nil chunk-scan)))
    (let* ((body-start (+ header-end 4))
           (status (parse-response-status buf start end))
           (te-present (scan-transfer-encoding buf header-end start))
           (content-length (unless te-present
                             (scan-content-length buf header-end start))))
      (cond
        ((eq method :HEAD)
         (values t chunk-scan))
        ;; 1xx is absent for the reason COMPLETE-FETCH gives: the skip
        ;; above has consumed every complete interim, so a status here is
        ;; final or unparseable.
        ((and status (or (= status 204) (= status 304)))
         (values t chunk-scan))
        ((and te-present
              (not (eq te-present :chunked))
              (not (response-chunked-p
                    (parse-response-headers buf start header-end))))
         (values nil chunk-scan))
        (te-present
         (multiple-value-bind (complete next)
             (chunked-body-complete-p buf body-start end
                                      chunk-scan
                                      on-data)
           (values (and complete t)
                   next
                   (when (eq te-present :chunked) body-start))))
        (content-length
         (values (>= (- end body-start) content-length) chunk-scan))
        (t
         (values nil chunk-scan))))))

;;; ---------------------------------------------------------------------------
;;; Ending a fetch
;;;
;;; An application's fetch callback fires exactly once per fetch lifetime.
;;; Held by separate invocation sites, that would take each of them agreeing
;;; with the others about when to clear the slot and what a cleanup call
;;; looks like — several readers of "how does a fetch end", which is the
;;; shape this codebase refuses everywhere else.
;;;
;;; So it is two functions with one job each. CLAIM-FETCH-CALLBACK takes the
;;; callback off a connection so no later path can find it; FINISH-FETCH is
;;; the only place an application callback is invoked at all.
;;;
;;; They stay separate because on the delivery path the claim and the call
;;; are not adjacent: COMPLETE-FETCH must claim *before* CLOSE-OUTBOUND, or
;;; teardown fires the cleanup sentinel on a fetch that succeeded, and it
;;; must call *after*. Collapsing the two into one atomic step would move
;;; one of those across the other.
;;; ---------------------------------------------------------------------------

(defun claim-fetch-callback (conn)
  "Take CONN's fetch callback and clear the slot. Returns the callback, or
   NIL if it had already been claimed.

   Clearing is the whole point: a claimed callback is unreachable from the
   connection, so every later path that would have fired it — CLOSE-OUTBOUND
   on the way down, a second completion — finds nothing and cannot deliver
   it twice."
  (let ((callback (connection-fetch-callback conn)))
    (when callback
      (setf (connection-fetch-callback conn) nil))
    callback))

(defun finish-fetch (callback outcome &key status headers body)
  "Invoke CALLBACK once with the result of a fetch, and answer what it
   returned. The sole site from which an application fetch callback is
   called; NIL CALLBACK is a no-op, so a caller need not test a claim.

   OUTCOME is :DELIVERED — STATUS, HEADERS and BODY are the upstream's — or
   :ABORTED, which passes the (NIL NIL NIL) cleanup sentinel and ignores the
   other three. A NIL status is what tells an application the fetch did not
   complete, so :ABORTED never carries one.

   The two outcomes treat a raising callback differently, and deliberately.
   :DELIVERED lets it propagate, because the caller is answering an inbound
   request and has a 500 to give it. :ABORTED is running on a teardown path
   with nothing left to answer, so a raise there is logged and swallowed —
   a failing cleanup hook must not stop the rest of the teardown."
  (when callback
    ;; What the callback is given, and what happens if it raises, are two
    ;; questions. Keeping them apart leaves exactly one expression in this
    ;; file that calls an application callback at all, which is the property
    ;; worth being able to grep for.
    (multiple-value-bind (s h b)
        (ecase outcome
          (:delivered (values status headers body))
          (:aborted   (values nil nil nil)))
      (flet ((invoke () (funcall callback s h b)))
        (if (eq outcome :aborted)
            (handler-case (invoke)
              ;; SERIOUS-CONDITION, not ERROR: see HANDLE-CLIENT-READ's last
              ;; catch.
              (serious-condition (e)
                (log-warn "fetch cleanup callback raised: ~a" e)
                nil))
            (invoke))))))

(defun %release-target-waiter (target-fd epoll-fd)
  "End the wait FETCH-SEND set on the connection at TARGET-FD, resuming a
   body it is receiving that paused on it. See RELEASE-SEND-WAITER."
  (let ((target (lookup-connection target-fd)))
    (when (and target (connection-send-waiting target))
      (setf (connection-send-waiting target) nil)
      (when (and (receiving-body-p target)
                 (connection-body-paused target))
        (let ((*epoll-fd* epoll-fd))
          (body-resume target))))))

(defun deliver-detached (target-fd epoll-fd callback outcome
                         &key status headers body fetch)
  "Hand a detached fetch's result to CALLBACK, then decide what becomes of
   the connection it was fetching into.

   The framework decides rather than trusting the app to, because an app
   will not handle a path it has never seen fail: a :THEN written for the
   happy case leaves a failed stream open until *STREAM-IDLE-TIMEOUT*,
   which is the hang this whole seam exists to remove.

   The disposition is outcome-dependent, because writing a terminator is a
   claim about completeness. A clean STREAM-CLOSE on a failed upstream
   tells the client it received the whole body — silent truncation, which
   is why CHUNKED-TERMINATOR is a separate function in the first place.

     already closed       either      nothing
     :streaming           delivered   stream-close, terminator written
     :streaming           aborted     close, no terminator, peer sees truncation
     receiving a body     delivered   nothing; :THEN resumes or refuses it
     receiving a body     aborted     closed, ON-ABORT hearing
                                      :UPSTREAM-FAILED, unless the caller
                                      passed :FAILURE-DISPOSITION :KEEP
     :websocket           delivered   nothing; the app owns its framing
     :websocket           aborted     a 1011 close frame, unless the caller
                                      passed :FAILURE-DISPOSITION :KEEP

   The two aborted rows with an 'unless' are the ones a caller can
   override, and the asymmetry with :STREAMING is the argument. On
   :STREAMING the disposition is a protocol obligation — the framework
   owns the terminator — so FETCH-INTO refuses :KEEP there rather than
   honouring it. On a WebSocket the framing is already the app's, per the
   row above, and a body being received owes the client nothing until the
   application answers it, so there the close is a policy; and the
   policy's own justification is that an app will not handle a path it
   has never seen fail, which does not describe an app that asked to keep
   the connection so it could handle it.

   The table covers two of the three ways a detached fetch can end, and
   the third one deliberately never arrives here. A stopped fetch —
   :ON-BODY returned :STOP — goes through STOP-FETCH to CLOSE-OUTBOUND,
   which is neither of this function's two callers, so no disposition is
   applied to its target at all. That is the whole of what makes stopping
   coherent: the application asked for the ending, so the application
   decides what the connection does next. An implementer who read
   `:aborted closes the connection` and routed a stop through here would
   rebuild the ending that verdict exists to avoid.

   A callback that started another fetch suppresses all of it. That is how
   chaining works here: a handler-returned fetch chains by returning a
   continuation, and a detached one has no return value anyone reads, so it
   chains by calling FETCH-INTO again — and closing the stream the new
   fetch is about to produce into would make chaining impossible. The
   outstanding marker is cleared before the callback runs precisely so the
   callback can set it again.

   A raising callback is caught here rather than propagating as
   FINISH-FETCH's :DELIVERED contract otherwise allows, because that
   contract's reason — the caller has an inbound request and a 500 to
   answer it with — does not hold on this path. There is nothing to answer.

   Nothing yet. What the callback returns is kept on FETCH, a streamed
   fetch, because the request its body was being received for may be
   answered with it afterwards (ADOPT-FETCH): an upstream that answered
   before the body was all sent has answered for that request too. And a
   body that was waiting on FETCH's request to drain is let go once the
   disposition has been applied — a fetch that has ended will not drain."
  (let ((target (lookup-connection target-fd)))
    (when target
      (setf (connection-fetch-outstanding target) nil))
    (handler-case
        (let ((answer (finish-fetch callback outcome
                                    :status status :headers headers :body body)))
          (when fetch
            (setf (http-fetch-continuation-answer fetch) answer
                  (http-fetch-continuation-answered fetch) t)))
      ;; SERIOUS-CONDITION, not ERROR: see HANDLE-CLIENT-READ's last catch.
      (serious-condition (e)
        (log-warn "detached fetch callback raised for fd ~d: ~a" target-fd e))))
  ;; Looked up again: the callback may have closed the connection, and on
  ;; a reused fd number it may not even be the same one.
  (let ((target (lookup-connection target-fd)))
    (when (and target (not (connection-fetch-outstanding target)))
      (case (connection-state target)
        (:streaming
         (ecase outcome
           (:delivered
            (log-debug "detached fetch: closing stream fd ~d the app left open"
                       target-fd)
            (handler-case (stream-close target)
              (error (e)
                (log-warn "detached fetch: stream-close failed on fd ~d: ~a"
                          target-fd e)
                (close-connection target epoll-fd :upstream-failed))))
           (:aborted
            (log-warn "detached fetch failed for fd ~d — closing the stream ~
                       without a terminator" target-fd)
            (close-connection target epoll-fd :upstream-failed))))
        (:websocket
         (when (eq outcome :aborted)
           ;; A row a caller can override. FETCH-INTO refuses :KEEP on a
           ;; :STREAMING target, so reading the slot here needs no second
           ;; check: there it is always :CLOSE.
           (if (eq (connection-fetch-failure-disposition target) :keep)
               ;; Debug rather than warn: the failure itself was already
               ;; logged by DELIVER-FETCH-ERROR, which is the only route to
               ;; :ABORTED, so this line reports a disposition and not an
               ;; incident. An app that asked for :KEEP does not want its
               ;; log filled by being obeyed.
               (log-debug "detached fetch failed for ws fd ~d — left open ~
                           at the caller's request" target-fd)
               (progn
                 (log-warn "detached fetch failed for ws fd ~d — closing 1011"
                           target-fd)
                 ;; The append is refusable, and the state it is refused in
                 ;; is not an exotic one here: a target already at
                 ;; *MAX-WRITE-BACKLOG* is a plausible reason the relay
                 ;; failed in the first place, and it is the one condition
                 ;; under which this arm runs and its own guard is false.
                 ;; Hanging the entire teardown on the frame being accepted
                 ;; left that connection :WEBSOCKET with a full queue, no
                 ;; close frame, and nothing marking it for teardown — to be
                 ;; collected a *WRITE-STALL-TIMEOUT* later by the stall
                 ;; sweep, which is a timeout standing in for a decision
                 ;; this arm had already reached.
                 ;;
                 ;; So the frame is a courtesy and the close is not.
                 ;; STREAM-CLOSE decides it the same way when there is no
                 ;; room for the terminator, and the :STREAMING arm above
                 ;; closes on :ABORTED without attempting to say anything at
                 ;; all.
                 (if (connection-append-write target (build-ws-close 1011))
                     (progn
                       (setf (connection-state target) :closing)
                       (ignore-errors
                        (epoll-modify epoll-fd (connection-fd target)
                                      (logior +epollout+ +epollet+))))
                     (progn
                       (log-warn "detached fetch: no room for a close frame ~
                                  on ws fd ~d — closing without one"
                                 target-fd)
                       (close-connection target epoll-fd :upstream-failed)))))))
        ;; A body being received: the fetch was something its ON-DATA was
        ;; waiting on. Delivered, the application's :THEN has resumed or
        ;; refused it, or will. Aborted, the body is ended unless the
        ;; caller kept it — its :THEN saw the sentinel and owns the outcome.
        ((:read-body :sending-100-continue)
         (when (and (eq outcome :aborted)
                    (receiving-body-p target)
                    (eq (connection-fetch-failure-disposition target) :close))
           (log-warn "detached fetch failed for fd ~d — ending the body it ~
                      was received for" target-fd)
           (close-connection target epoll-fd :upstream-failed))))))
  (%release-target-waiter target-fd epoll-fd))

(defun complete-fetch (out-conn epoll-fd &key framing-complete)
  "Parse the outbound response and deliver it to the parked inbound connection.

   FRAMING-COMPLETE is what OUTBOUND-RESPONSE-COMPLETE-P said: the response
   ended where its own framing says it ends. NIL means the caller is
   completing on end of stream instead, which is authoritative for a
   close-delimited response and truncation for every other framing."
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
    (multiple-value-bind (status headers chunked-p te-present content-length)
        (parse-response-head buf start header-end)
      (declare (ignore te-present))
    (let ((body-start (+ header-end 4)))
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
    ;; code that reads as though an interim could be a final status,
    ;; which is the belief that lets an interim stand in for the
    ;; response.
    (when (and content-length
               (not (or (= status 204) (= status 304)))
               (not (eq (connection-fetch-method out-conn) :HEAD))
               (< (- pos body-start) content-length))
      (deliver-fetch-error out-conn epoll-fd
                           (format nil "short body: ~d of ~d bytes"
                                   (- pos body-start) content-length))
      (return-from complete-fetch))
    ;; A relayed body cut short. Under :ON-BODY the chunks were handed on
    ;; as they arrived and DROP-DELIVERED-CHUNKS freed them, so decoding
    ;; what is left cannot judge the body: what is left may be nothing,
    ;; which would read as an empty body and a success. FRAMING-COMPLETE
    ;; NIL means the walk never reached the terminator, so the body ended
    ;; early, and it gets the 502 every other truncation here gets.
    (when (and (connection-fetch-on-body out-conn)
               chunked-p
               (not framing-complete)
               (not (or (= status 204) (= status 304)))
               (not (eq (connection-fetch-method out-conn) :HEAD)))
      (deliver-fetch-error out-conn epoll-fd
                           "chunked body ended before its terminator")
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
           ;; NIL only when ON-BODY *actually* delivered every byte, which
           ;; takes all three conjuncts: a callback, chunked framing — the
           ;; walk ON-DATA is threaded into is CHUNKED-BODY-COMPLETE-P's
           ;; and no other — and a walk that reached the terminator.
           ;;
           ;; Keyed on the callback merely being supplied, this would drop
           ;; the body of every Content-Length and close-delimited
           ;; response: no chunks delivered, NIL in :THEN, nothing raised.
           ;; That is the majority of responses in the wild, and the caller
           ;; does not choose the framing — the same origin switches by
           ;; response size or by whatever proxy sits in front, so a relay
           ;; would work against one upstream and come back empty against
           ;; the next.
           ;;
           ;; Keyed on callback-and-chunked, it would still suppress the
           ;; guard on a body that stopped early: chunks delivered,
           ;; upstream RSTs before the terminator, and :THEN fires 200 with
           ;; NIL as though the response were whole. Requiring the walk to
           ;; have finished falls through to DECODE-CHUNKED-BODY instead,
           ;; whose missing-terminator raise becomes the same 502 every
           ;; other truncation on this path produces.
           (body (cond
                   ((and (connection-fetch-on-body out-conn) chunked-p
                         framing-complete)
                    nil)
                   ((and raw-body chunked-p)
                    (decode-chunked-body raw-body 0 (length raw-body)))
                   (t raw-body)))
           ;; Call the user's callback.
           ;; Claimed here, which is before CLOSE-OUTBOUND below. That
           ;; ordering is the guarantee: teardown looks for a callback to
           ;; fire the cleanup sentinel with, and on a fetch that succeeded
           ;; it must find none.
           (callback (claim-fetch-callback out-conn))
           (inbound-fd (connection-inbound-fd out-conn))
           ;; Captured before CLOSE-OUTBOUND nulls it — AWAITING-INBOUND-FOR
           ;; needs it to confirm the inbound it finds is the one that
           ;; parked on this outbound rather than a reuse of its number.
           ;; Captured before teardown for the same reason as OUT-FD.
           (sink (connection-fetch-sink out-conn))
           (out-fd (connection-fd out-conn))
           (fetch (connection-fetch-continuation out-conn)))
      (close-outbound out-conn epoll-fd)
      (when (eq sink :detached)
        (deliver-detached inbound-fd epoll-fd callback :delivered
                          :status status :headers (or headers nil)
                          :body (or body nil)
                          :fetch fetch)
        (return-from complete-fetch))
      ;; Find and resume inbound connection. If the inbound vanished
      ;; between :awaiting parking and now (drain race, idle reap,
      ;; I/O error on the inbound fd), we can't deliver a response
      ;; anywhere — but the contract is still "callback fires exactly
      ;; once per fetch lifetime", so fire the cleanup sentinel here
      ;; before returning so app state gets released.
      (let ((inbound (awaiting-inbound-for inbound-fd out-fd)))
        (if inbound
            (handler-case
                (let ((response (finish-fetch callback :delivered
                                              :status status
                                              :headers (or headers nil)
                                              :body (or body nil))))
                  ;; A streamed fetch already started for this request
                  ;; answers it as a handler's fetch would: parked on until
                  ;; it has, or answered with what it answered already.
                  (loop while (and (typep response 'http-fetch-continuation)
                                   (fetch-started-p response))
                        do (multiple-value-bind (verdict answer)
                               (adopt-fetch inbound response)
                             (if (eq verdict :parked)
                                 (return-from complete-fetch)
                                 (setf response answer))))
                  ;; Sync close-after-p from the response that answers,
                  ;; BEFORE computing the connection-hint — a handler-set
                  ;; Connection: close flips close-after-p to T so the
                  ;; hint resolves to :CLOSE and the server stamps the
                  ;; header on the wire instead of leaving the framing
                  ;; implicit.
                  (sync-close-after-p-from-response inbound response)
                  (cond
                    ;; Chained fetch — initiate another outbound call
                    ((typep response 'http-fetch-continuation)
                     (initiate-fetch inbound epoll-fd response)
                     (return-from complete-fetch))
                    ;; A stream answers a request before its body exists,
                    ;; and this request is parked until the fetch answers
                    ;; it whole. Named, so the 500 says how to get one.
                    ((typep response 'stream-response)
                     (error "a fetch :then answered with a STREAM-RESPONSE; ~
                             a parked request is answered whole. To relay ~
                             an upstream into a stream, return the stream ~
                             from the handler and start the fetch with ~
                             FETCH-INTO from its :ON-OPEN.")))
                  ;; Every shape a handler may answer with, through the
                  ;; function HANDLE-CLIENT-READ uses, so the two accept
                  ;; the same things.
                  (queue-final-response inbound response)
                  (setf (connection-state inbound) :write-response
                        (connection-awaiting-fd inbound) -1
                        (connection-last-active inbound) (get-universal-time))
                  (epoll-modify epoll-fd (connection-fd inbound)
                                (logior +epollout+ +epollet+))
                  (log-debug "fetch: resumed fd ~d" inbound-fd))
              ;; SERIOUS-CONDITION, not ERROR: see HANDLE-CLIENT-READ's last
              ;; catch.
              (serious-condition (e)
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
            (finish-fetch callback :aborted))))))))

(defun deliver-fetch-error (out-conn epoll-fd message)
  "Deliver a 502 error to the inbound connection and clean up."
  (log-warn "fetch error fd ~d: ~a" (connection-fd out-conn) message)
  (let ((inbound-fd (connection-inbound-fd out-conn))
        (sink (connection-fetch-sink out-conn))
        (out-fd (connection-fd out-conn)))
    ;; A detached target is mid-response: its head went out long ago and a
    ;; body is in flight. Serializing a 502 onto it would put a complete
    ;; HTTP message inside an established chunked body, which is the
    ;; framing confusion this codebase refuses on principle. It gets the
    ;; abort outcome and DELIVER-DETACHED's disposition instead.
    (when (eq sink :detached)
      (let ((callback (claim-fetch-callback out-conn))
            (fetch (connection-fetch-continuation out-conn)))
        (close-outbound out-conn epoll-fd)
        (deliver-detached inbound-fd epoll-fd callback :aborted :fetch fetch))
      (return-from deliver-fetch-error))
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
      ;; A closed fetch is not a paused one, so FETCH-RESUME on it is the
      ;; no-op its contract promises rather than an epoll_ctl on a dead fd.
      (setf (connection-fetch-paused conn) nil
            (connection-fetch-rewalk conn) nil)
      ;; The fetch is over if this was what carried it. A resolved name's
      ;; pipe is closed after the outbound it led to has taken the fetch
      ;; on, and that close ends nothing.
      (let ((fetch (connection-fetch-continuation conn)))
        (when (and fetch (eq (http-fetch-continuation-carrier fetch) conn))
          (setf (http-fetch-continuation-carrier fetch) nil
                (http-fetch-continuation-ended fetch) t)))
      (finish-fetch (claim-fetch-callback conn) :aborted)
      (ignore-errors (epoll-remove epoll-fd fd))
      (unregister-connection conn)
      (maybe-reap-dns-process conn)
      (connection-close conn)
      (log-debug "fetch: closed outbound fd ~d" fd))))

(defun stop-fetch (out-conn epoll-fd)
  "End OUT-CONN because the application asked it to, and leave the
   connection it was fetching into alone.

   The third way a detached fetch can end, and the only one that both
   reports the truth and keeps the target. COMPLETE-FETCH reports
   :DELIVERED — a real status and the partial body, which tells an
   application that a response it cut short arrived whole, which is silent
   truncation again. DELIVER-FETCH-ERROR reports the truth and then takes
   the target with it through DELIVER-DETACHED's disposition. Stopping wants
   the honest report without the disposition, which is neither of them.

   It ends through CLOSE-OUTBOUND rather than around it. The exactly-once
   contract, the epoll removal, the DNS reap and the abort sentinel are all
   already there, and (>= fd 0) already makes it idempotent — a stopped
   fetch reports its ending like every other one, so an app that stops one
   still gets to release whatever it was holding.

   What CLOSE-OUTBOUND does not do is clear the target's outstanding
   marker: DELIVER-DETACHED clears it, and a stopped fetch reaches
   DELIVER-DETACHED by neither of its two callers. Left set, FETCH-INTO
   would refuse every later fetch on that connection for the rest of its
   life, and `stop this upstream, keep this connection` would hand back a
   connection stripped of the one thing the application kept it for.

   Cleared before CLOSE-OUTBOUND rather than after, for the reason
   DELIVER-DETACHED clears it before its callback runs: the sentinel fires
   from inside CLOSE-OUTBOUND, and a :THEN that starts the next fetch has
   to find the slot free.

   A parked fetch is not stopped, it is failed. :ON-BODY is available on
   the handler-returned path too, where a client is parked in :AWAITING
   with no response owed to it by anything else — closing the outbound
   alone would leave that client to be reaped having been answered nothing,
   so the stop becomes the same 502 every other ending that produces no
   response on that path already produces. The application still gets the
   abort sentinel, from CLOSE-OUTBOUND, on the way through."
  (if (eq (connection-fetch-sink out-conn) :detached)
      (let ((target-fd (connection-inbound-fd out-conn)))
        (let ((target (lookup-connection target-fd)))
          (when target
            (setf (connection-fetch-outstanding target) nil)))
        (log-debug "fetch: stopped by :on-body, fd ~d" (connection-fd out-conn))
        (close-outbound out-conn epoll-fd)
        ;; A stopped fetch will not drain, so a body FETCH-SEND told to wait
        ;; on it is not kept waiting.
        (%release-target-waiter target-fd epoll-fd))
      (deliver-fetch-error out-conn epoll-fd
                           ":on-body returned :stop on a parked fetch")))
