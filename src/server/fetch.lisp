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
  (callback nil    :type function))

(defparameter *fetch-timeout* 30
  "Seconds for blocking fetch I/O timeout and :awaiting connection reaping.")

(defun http-fetch (method url &key headers body then)
  "Create an outbound HTTP request descriptor.
   Return this from a handler to initiate a non-blocking outbound call.
   THEN is called with (status headers body) when the response arrives;
   it must return an HTTP response, byte vector, or another http-fetch-continuation."
  (unless then
    (error "http-fetch requires :then callback"))
  (make-http-fetch-continuation :method method :url url
                                :headers headers :body body
                                :callback then))

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

(defun parse-ipv6-literal (str)
  "Parse STR as an IPv6 literal. Returns a 16-byte vector on success,
   NIL on failure. Handles :: compression. Does not handle IPv4-mapped
   addresses (::ffff:1.2.3.4) or zone identifiers (fe80::1%eth0) — both
   are rare in URL contexts and v1 rejects them cleanly."
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
      ;; No "::" — must be exactly 8 groups
      (t
       (let* ((groups (%split-ipv6-groups str))
              (vals (mapcar #'%parse-ipv6-group groups)))
         (when (and (= (length vals) 8) (every #'identity vals))
           (%ipv6-groups-to-bytes vals)))))))

;;; ---------------------------------------------------------------------------
;;; URL parsing
;;; ---------------------------------------------------------------------------

(defun parse-authority (authority default-port)
  "Split an HTTP authority string into (values HOST PORT). Handles
   bracketed IPv6 literals per RFC 3986 §3.2.2: '[::1]:8080' → host '::1',
   port 8080. For unbracketed authorities, splits on the first (only)
   colon between host and port."
  (cond
    ;; Bracketed IPv6 literal
    ((and (> (length authority) 0) (char= (char authority 0) #\[))
     (let ((close (position #\] authority)))
       (unless close
         (error "malformed URL authority: unclosed '[' in ~a" authority))
       (let ((host (subseq authority 1 close))
             (rest (subseq authority (1+ close))))
         (if (and (> (length rest) 0) (char= (char rest 0) #\:))
             (values host (parse-integer rest :start 1))
             (values host default-port)))))
    ;; Plain host or IPv4 literal
    (t
     (let ((colon (position #\: authority)))
       (if colon
           (values (subseq authority 0 colon)
                   (parse-integer authority :start (1+ colon)))
           (values authority default-port))))))

(defun parse-url (url)
  "Parse a URL into (values scheme host port path).
   Supports http:// and https:// schemes. Host may be a plain name,
   an IPv4 literal, or a bracketed IPv6 literal ('[2001:db8::1]').
   Rejects URLs longer than *MAX-REQUEST-LINE-LENGTH* — a handler that
   proxies a user-controlled URL must not be able to hand us a 100 MB
   string that allocates through several intermediate buffers before
   anything else can push back."
  (when (> (length url) *max-request-line-length*)
    (error "URL too long (~d bytes, max ~d)"
           (length url) *max-request-line-length*))
  (let ((scheme nil) (authority-start nil) (default-port nil))
    (cond
      ((and (>= (length url) 8) (string-equal url "https://" :end1 8))
       (setf scheme :https authority-start 8 default-port 443))
      ((and (>= (length url) 7) (string-equal url "http://" :end1 7))
       (setf scheme :http authority-start 7 default-port 80))
      (t (error "unsupported URL scheme: ~a" url)))
    (let* ((path-start (or (position #\/ url :start authority-start)
                           (length url)))
           (authority (subseq url authority-start path-start))
           (path (if (= path-start (length url)) "/" (subseq url path-start))))
      (multiple-value-bind (host port) (parse-authority authority default-port)
        (values scheme host port path)))))

;;; ---------------------------------------------------------------------------
;;; Build outbound HTTP request bytes
;;; ---------------------------------------------------------------------------

(defun build-outbound-request (method host path &key (scheme :http) (port 80)
                                                      headers body)
  "Build an HTTP/1.1 request as a byte vector ready to write."
  (let* ((method-str (symbol-name method))
         (default-port-p (or (and (eq scheme :http) (= port 80))
                             (and (eq scheme :https) (= port 443))))
         (host-value (if default-port-p
                         host
                         (format nil "~a:~d" host port)))
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
                        (when (and body-bytes
                                   (not (has-header-p "content-length")))
                          (list (cons "content-length"
                                      (write-to-string
                                       (length body-bytes)))))))))
    (serialize-http-message
     (format nil "~a ~a HTTP/1.1" method-str path)
     all-headers body-bytes)))

;;; ---------------------------------------------------------------------------
;;; Parse response status line
;;; ---------------------------------------------------------------------------

(defun parse-response-status (buf start end)
  "Extract the integer status code from a response line in BUF[START..END).
   Expects 'HTTP/1.x NNN' or 'HTTP/1.x NNN reason' (RFC 7230 §3.1.2).
   Returns the status only if it is a 3-digit number in the 100-599
   range (RFC 7231 §6) followed by SP or CR. Rejects 4-digit codes,
   junk-terminated digits, and anything outside the defined ranges."
  (let ((crlf (scan-crlf buf start end)))
    (unless crlf (return-from parse-response-status nil))
    (let ((sp (position 32 buf :start start :end crlf)))
      (unless sp (return-from parse-response-status nil))
      (let ((s (1+ sp)))
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
                  status)))))))))

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
                    (stream-response-lines stream on-line))
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
  (end    0 :type fixnum))

(defun reader-fill (r)
  "Refill the buffer. Returns bytes read (0 = EOF)."
  (setf (stream-reader-pos r) 0)
  (let ((n (read-sequence (stream-reader-buf r) (stream-reader-stream r))))
    (setf (stream-reader-end r) n)
    n))

(defun reader-read-line (r)
  "Read a line from the buffered reader. Returns a string, or NIL at EOF."
  (let ((line-buf (make-array 256 :element-type '(unsigned-byte 8)
                                  :fill-pointer 0 :adjustable t)))
    (loop
      (when (>= (stream-reader-pos r) (stream-reader-end r))
        (when (zerop (reader-fill r))
          (return (if (zerop (fill-pointer line-buf)) nil
                      (sb-ext:octets-to-string line-buf
                                               :external-format :utf-8)))))
      (let* ((buf (stream-reader-buf r))
             (pos (stream-reader-pos r))
             (end (stream-reader-end r))
             (lf  (position 10 buf :start pos :end end)))
        (if lf
            (progn
              (loop for i from pos below lf
                    for b = (aref buf i)
                    unless (= b 13)
                    do (when (> (fill-pointer line-buf) *max-body-size*)
                         (error "streaming response line too large"))
                       (vector-push-extend b line-buf))
              (setf (stream-reader-pos r) (1+ lf))
              (return (sb-ext:octets-to-string line-buf
                                               :external-format :utf-8)))
            (progn
              (loop for i from pos below end
                    for b = (aref buf i)
                    unless (= b 13)
                    do (when (> (fill-pointer line-buf) *max-body-size*)
                         (error "streaming response line too large"))
                       (vector-push-extend b line-buf))
              (setf (stream-reader-pos r) end)))))))

(defun reader-read-bytes (r count line-buf on-line)
  "Read COUNT bytes through the buffered reader, splitting into lines.
   Calls ON-LINE per complete line."
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
                   ((= byte 10)
                    (when on-line
                      (funcall on-line (sb-ext:octets-to-string
                                        (subseq line-buf 0 (fill-pointer line-buf))
                                        :external-format :utf-8)))
                    (setf (fill-pointer line-buf) 0))
                   ((= byte 13) nil)
                   (t (when (> (fill-pointer line-buf) *max-body-size*)
                        (error "streaming response line too large"))
                      (vector-push-extend byte line-buf))))
        (incf (stream-reader-pos r) take)
        (decf remaining take)))))

;;; ---------------------------------------------------------------------------
;;; HTTP response streaming
;;; ---------------------------------------------------------------------------

(defun stream-response-lines (stream on-line)
  "Read an HTTP response from a byte stream. Skip headers, call ON-LINE
   per body line. Handles chunked transfer encoding. Returns the status code."
  (let ((r (make-stream-reader stream))
        (status nil)
        (chunked nil))
    ;; Read status line + headers
    (loop for line = (reader-read-line r)
          for first = t then nil
          while (and line (> (length line) 0))
          do (when first
               (let ((sp (position #\Space line)))
                 (when (and sp (< (+ sp 3) (length line)))
                   (setf status (parse-integer line :start (1+ sp)
                                                    :end (+ sp 4)
                                                    :junk-allowed t)))))
             (when (and (>= (length line) 18)
                        (string-equal line "transfer-encoding:"
                                      :end1 18))
               (let ((value (string-trim '(#\Space #\Tab) (subseq line 18))))
                 (when (connection-header-has-token-p value "chunked")
                   (setf chunked t)))))
    ;; Stream body lines
    (if chunked
        (stream-chunked-lines r on-line)
        (loop for line = (reader-read-line r)
              while line
              do (when on-line (funcall on-line line))))
    (or status 0)))

(defun stream-chunked-lines (r on-line)
  "Decode chunked transfer encoding via buffered reader R."
  (let ((line-buf (make-array 4096 :element-type '(unsigned-byte 8)
                                   :fill-pointer 0 :adjustable t)))
    (loop
      (let* ((size-line (reader-read-line r))
             (chunk-size (when (and size-line (> (length size-line) 0))
                           (parse-integer size-line :radix 16 :junk-allowed t))))
        (unless (and chunk-size (> chunk-size 0))
          (when (> (fill-pointer line-buf) 0)
            (when on-line
              (funcall on-line (sb-ext:octets-to-string
                                line-buf :external-format :utf-8))))
          (return))
        (reader-read-bytes r chunk-size line-buf on-line)
        ;; Consume trailing CRLF
        (reader-read-line r)))))

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
      (let ((error-response (format-response (make-error-response 502))))
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
                               :type :stream :protocol :tcp)))
    (handler-case
        (progn
          (set-nonblocking (socket-fd socket))
          (handler-case
              (sb-bsd-sockets:socket-connect socket ip port)
            (sb-bsd-sockets:socket-error () nil))
          (let* ((out-fd (socket-fd socket))
                 (request-bytes (build-outbound-request
                                (http-fetch-continuation-method fetch-req)
                                host path
                                :port port
                                :headers (http-fetch-continuation-headers fetch-req)
                                :body (http-fetch-continuation-body fetch-req)))
                 (out-conn (make-connection
                            :fd out-fd
                            :socket socket
                            :state :out-connecting
                            :outbound-p t
                            :inbound-fd (connection-fd conn)
                            :fetch-callback (http-fetch-continuation-callback fetch-req)
                            :last-active (get-universal-time))))
            (connection-queue-write out-conn request-bytes)
            (register-connection out-conn)
            (epoll-add epoll-fd out-fd (logior +epollout+ +epollet+))
            (setf (connection-state conn) :awaiting
                  (connection-awaiting-fd conn) out-fd)
            (log-debug "fetch: fd ~d -> ~a :~d~a (outbound fd ~d, ~a)"
                       (connection-fd conn) host port path out-fd family)))
      (error (e)
        (let* ((fd (sb-bsd-sockets:socket-file-descriptor socket))
               (stale (when (>= fd 0) (lookup-connection fd))))
          (when stale (unregister-connection stale)))
        (ignore-errors (sb-bsd-sockets:socket-close socket))
        (error e)))))

(defun initiate-http-fetch (conn epoll-fd fetch-req host port path)
  "Start a non-blocking outbound HTTP request. Fast path: HOST is a
   numeric IPv4 or IPv6 literal, skip DNS entirely and connect direct.
   Slow path: dispatch to *DNS-LOOKUP-FN* (provided by dns.lisp) which
   runs getent in a subprocess and resumes via
   INITIATE-HTTP-FETCH-TO-ADDRESS when the address is in hand."
  (let* ((v4 (parse-ipv4-literal host))
         (v6 (unless v4 (parse-ipv6-literal host))))
    (cond
      (v4 (initiate-http-fetch-to-address
           conn epoll-fd fetch-req host port path v4 :inet))
      (v6 (initiate-http-fetch-to-address
           conn epoll-fd fetch-req host port path v6 :inet6))
      (*dns-lookup-fn*
       (funcall *dns-lookup-fn* conn epoll-fd fetch-req host port path))
      (t (error "no DNS resolver loaded")))))

;;; ---------------------------------------------------------------------------
;;; Outbound event handlers
;;; ---------------------------------------------------------------------------

(defun handle-outbound-event (conn epoll-fd flags)
  "Handle an epoll event on an outbound connection."
  (handler-case
      (cond
        ;; DNS lookup: EPOLLIN or EPOLLHUP both mean drain-and-parse.
        ;; getent writes its output and then exits, which may surface
        ;; either as readable data (EPOLLIN) or as a pipe close hint
        ;; (EPOLLHUP). The read path handles EOF cleanly either way —
        ;; treating HUP as an error here would lose the final output.
        ;; Dispatched via *HANDLE-DNS-READY-FN* so fetch.lisp has no
        ;; forward reference to dns.lisp.
        ((eq (connection-state conn) :out-dns)
         (funcall *handle-dns-ready-fn* conn epoll-fd))
        ((or (logtest flags +epollerr+) (logtest flags +epollhup+))
         (deliver-fetch-error conn epoll-fd "connection error"))
        ((logtest flags +epollout+)
         (ecase (connection-state conn)
           (:out-connecting (handle-outbound-connect conn epoll-fd))
           (:out-write      (handle-outbound-write conn epoll-fd))))
        ((logtest flags +epollin+)
         (ecase (connection-state conn)
           (:out-read (handle-outbound-read conn epoll-fd)))))
    (error (e)
      (log-error "outbound error fd ~d: ~a" (connection-fd conn) e)
      (deliver-fetch-error conn epoll-fd "outbound request failed"))))

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

(defun handle-outbound-read (conn epoll-fd)
  "Read the outbound HTTP response. When complete, deliver to the inbound connection."
  (let ((result (connection-read-available conn)))
    (case result
      (:eof
       ;; Server closed connection — response is complete (Connection: close)
       (complete-fetch conn epoll-fd))
      (:full
       ;; Buffer full — response truncated at *max-body-size*
       (log-warn "fetch: response truncated at ~d bytes on fd ~d"
                 (connection-read-pos conn) (connection-fd conn))
       (complete-fetch conn epoll-fd))
      (:again nil)  ; wait for more data
      (:ok
       ;; Got data — check if we have a complete response
       (let* ((buf (connection-read-buf conn))
              (pos (connection-read-pos conn))
              (header-end (scan-crlf-crlf buf 0 pos)))
         (when header-end
           ;; Have complete headers — check if body is complete.
           ;; RFC 7230 §3.3.3: Transfer-Encoding takes precedence over CL.
           (let* ((body-start (+ header-end 4))
                  (chunked (scan-transfer-encoding buf header-end))
                  (content-length (unless chunked
                                    (scan-content-length buf header-end))))
             (if content-length
                 ;; Have Content-Length (no TE) — complete when body received
                 (when (>= (- pos body-start) content-length)
                   (complete-fetch conn epoll-fd))
                 ;; Chunked or no CL — wait for EOF (Connection: close)
                 nil))))))))

;;; ---------------------------------------------------------------------------
;;; Chunked body decoding (for buffered responses)
;;; ---------------------------------------------------------------------------

(defun response-chunked-p (headers)
  "Return T if HEADERS indicate chunked transfer encoding.
   Uses token-aware matching (RFC 7230 §3.2.6) so 'identity' or
   'unchunked-foo' don't false-match on substring 'chunked'."
  (let ((te (cdr (assoc "transfer-encoding" headers :test #'string-equal))))
    (and te (connection-header-has-token-p te "chunked"))))

(defun decode-chunked-body (buf start end)
  "Decode chunked transfer encoding from BUF[START..END).
   Returns a byte vector with the chunk framing stripped."
  (let ((out (make-array (- end start) :element-type '(unsigned-byte 8)
                                        :fill-pointer 0))
        (pos start))
    (loop
      ;; Parse chunk size (hex digits)
      (let ((size 0)
            (found nil))
        (loop while (< pos end)
              for byte = (aref buf pos)
              do (let ((digit (hex-digit-value byte)))
                   (if digit
                       (progn (setf size (+ (ash size 4) digit)
                                    found t)
                              (incf pos))
                       (return))))
        (unless found (return))
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
        ;; Zero-size chunk = end
        (when (zerop size) (return))
        ;; Copy chunk data
        (let ((chunk-end (min (+ pos size) end)))
          (loop for i from pos below chunk-end
                do (vector-push-extend (aref buf i) out))
          (setf pos chunk-end))
        ;; Skip trailing CRLF after chunk data
        (when (and (< pos end) (= (aref buf pos) 13)) (incf pos))
        (when (and (< pos end) (= (aref buf pos) 10)) (incf pos))))
    (subseq out 0 (fill-pointer out))))

;;; ---------------------------------------------------------------------------
;;; Deliver fetch result to the parked inbound connection
;;; ---------------------------------------------------------------------------

(defun complete-fetch (out-conn epoll-fd)
  "Parse the outbound response and deliver it to the parked inbound connection."
  (let* ((buf (connection-read-buf out-conn))
         (pos (connection-read-pos out-conn))
         (header-end (scan-crlf-crlf buf 0 pos))
         (status (when header-end (parse-response-status buf 0 pos)))
         (body-start (when header-end (+ header-end 4)))
         ;; Parse response headers
         (headers (when header-end
                    (let ((first-crlf (scan-crlf buf 0 header-end)))
                      (when first-crlf
                        (parse-headers-bytes buf (+ first-crlf 2)
                                             (+ header-end 4))))))
         ;; Extract body, capped at Content-Length when present and not chunked
         ;; (RFC 7230 §3.3.3: TE takes precedence over CL)
         (chunked-p (response-chunked-p headers))
         (content-length (when (and header-end (not chunked-p))
                           (scan-content-length buf header-end)))
         (body-end (if (and body-start content-length)
                       (min pos (+ body-start content-length))
                       pos))
         (raw-body (when (and body-start (> body-end body-start))
                     (subseq buf body-start body-end)))
         (body (if (and raw-body chunked-p)
                   (decode-chunked-body raw-body 0 (length raw-body))
                   raw-body))
         ;; Call the user's callback
         (callback (connection-fetch-callback out-conn))
         (inbound-fd (connection-inbound-fd out-conn)))
    ;; Clean up outbound connection
    (close-outbound out-conn epoll-fd)
    ;; Find and resume inbound connection
    (let ((inbound (lookup-connection inbound-fd)))
      (when inbound
        (handler-case
            (let ((response (funcall callback
                                     (or status 0) (or headers nil)
                                     (or body nil))))
              ;; Queue the response on the inbound connection
              (let ((bytes (cond
                             ((typep response '(simple-array (unsigned-byte 8) (*)))
                              response)
                             ((typep response 'http-fetch-continuation)
                              ;; Chained fetch — initiate another outbound call
                              (initiate-fetch inbound epoll-fd response)
                              (return-from complete-fetch))
                             (t (format-response response)))))
                (connection-queue-write inbound bytes)
                (setf (connection-state inbound) :write-response
                      (connection-awaiting-fd inbound) -1
                      (connection-last-active inbound) (get-universal-time))
                (epoll-modify epoll-fd (connection-fd inbound)
                             (logior +epollout+ +epollet+))
                (log-debug "fetch: resumed fd ~d" inbound-fd)))
          (error (e)
            (log-error "fetch callback error: ~a" e)
            (let ((err-bytes (format-response (make-error-response 500))))
              (connection-queue-write inbound err-bytes)
              (setf (connection-state inbound) :write-response
                    (connection-awaiting-fd inbound) -1
                    (connection-last-active inbound) (get-universal-time))
              (epoll-modify epoll-fd (connection-fd inbound)
                           (logior +epollout+ +epollet+)))))))))

(defun deliver-fetch-error (out-conn epoll-fd message)
  "Deliver a 502 error to the inbound connection and clean up."
  (log-warn "fetch error fd ~d: ~a" (connection-fd out-conn) message)
  (let ((inbound-fd (connection-inbound-fd out-conn)))
    (close-outbound out-conn epoll-fd)
    (let ((inbound (lookup-connection inbound-fd)))
      (when inbound
        (let ((err-bytes (format-response (make-error-response 502))))
          (connection-queue-write inbound err-bytes)
          (setf (connection-state inbound) :write-response
                (connection-awaiting-fd inbound) -1
                (connection-last-active inbound) (get-universal-time))
          (epoll-modify epoll-fd (connection-fd inbound)
                       (logior +epollout+ +epollet+)))))))

(defun close-outbound (conn epoll-fd)
  "Close and unregister an outbound connection. Reaps any attached
   getent DNS subprocess via MAYBE-REAP-DNS-PROCESS — a no-op for
   TCP outbound connections that never had a DNS phase."
  (let ((fd (connection-fd conn)))
    (when (>= fd 0)
      (ignore-errors (epoll-remove epoll-fd fd))
      (unregister-connection conn)
      (maybe-reap-dns-process conn)
      (connection-close conn)
      (log-debug "fetch: closed outbound fd ~d" fd))))
