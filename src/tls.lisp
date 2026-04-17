(in-package :web-skeleton)

;;; ===========================================================================
;;; TLS via libssl FFI (OpenSSL 1.1+)
;;;
;;; Provides blocking TLS connections for outbound HTTPS in http-fetch.
;;; Loaded by the web-skeleton-tls ASDF system — optional, not part of core.
;;;
;;; On load:
;;;   1. Opens libssl.so and libcrypto.so
;;;   2. Initializes OpenSSL
;;;   3. Creates a shared SSL_CTX with system CA roots
;;;   4. Registers the HTTPS fetch handler with the core framework
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Load shared libraries
;;; ---------------------------------------------------------------------------

(eval-when (:load-toplevel :execute)
  ;; Try versioned SONAMEs first, fall through to the unversioned link.
  ;; Production containers (debian:slim, alpine, distroless/cc) ship
  ;; only libssl.so.3 or libssl.so.1.1 — the bare libssl.so symlink
  ;; comes with the -dev package, which slim images don't install.
  ;; Loading the unversioned name first would have worked on a dev
  ;; workstation and silently failed in the container.
  (labels ((try-load (candidates)
             "Walk CANDIDATES and return the first SONAME that loads,
              or NIL if none did."
             (dolist (name candidates)
               (when (ignore-errors
                      (sb-alien:load-shared-object name :dont-save t)
                      t)
                 (return name)))))
    (let ((crypto (try-load '("libcrypto.so.3" "libcrypto.so.1.1" "libcrypto.so")))
          (ssl    (try-load '("libssl.so.3"    "libssl.so.1.1"    "libssl.so"))))
      (unless (and crypto ssl)
        (error "web-skeleton-tls: failed to load libssl (tried .so.3, .so.1.1, .so).~%~
                Production containers ship libssl.so.3 or libssl.so.1.1; ~
                dev hosts also ship the unversioned libssl.so symlink via ~
                libssl-dev (Debian/Ubuntu), openssl-devel (RHEL), or ~
                openssl-dev (Alpine). Install whichever matches your distro."))
      (log-info "tls: loaded ~a and ~a" crypto ssl))))

;;; ---------------------------------------------------------------------------
;;; FFI bindings
;;; ---------------------------------------------------------------------------

;;; Initialization
(sb-alien:define-alien-routine ("OPENSSL_init_ssl" %openssl-init-ssl)
    sb-alien:int
  (opts sb-alien:unsigned-long)
  (settings (* t)))

;;; Context
(sb-alien:define-alien-routine ("TLS_client_method" %tls-client-method)
    (* t))

(sb-alien:define-alien-routine ("SSL_CTX_new" %ssl-ctx-new) (* t)
  (method (* t)))

(sb-alien:define-alien-routine ("SSL_CTX_free" %ssl-ctx-free) sb-alien:void
  (ctx (* t)))

(sb-alien:define-alien-routine ("SSL_CTX_set_default_verify_paths"
                                %ssl-ctx-set-default-verify-paths) sb-alien:int
  (ctx (* t)))

(sb-alien:define-alien-routine ("SSL_CTX_set_verify" %ssl-ctx-set-verify)
    sb-alien:void
  (ctx (* t))
  (mode sb-alien:int)
  (callback (* t)))

;;; Connection
(sb-alien:define-alien-routine ("SSL_new" %ssl-new) (* t)
  (ctx (* t)))

(sb-alien:define-alien-routine ("SSL_free" %ssl-free) sb-alien:void
  (ssl (* t)))

(sb-alien:define-alien-routine ("SSL_set_fd" %ssl-set-fd) sb-alien:int
  (ssl (* t))
  (fd sb-alien:int))

(sb-alien:define-alien-routine ("SSL_connect" %ssl-connect) sb-alien:int
  (ssl (* t)))

(sb-alien:define-alien-routine ("SSL_shutdown" %ssl-shutdown) sb-alien:int
  (ssl (* t)))

(sb-alien:define-alien-routine ("SSL_get_error" %ssl-get-error) sb-alien:int
  (ssl (* t))
  (ret sb-alien:int))

;;; I/O
(sb-alien:define-alien-routine ("SSL_read" %ssl-read) sb-alien:int
  (ssl (* t))
  (buf (* t))
  (num sb-alien:int))

(sb-alien:define-alien-routine ("SSL_write" %ssl-write) sb-alien:int
  (ssl (* t))
  (buf (* t))
  (num sb-alien:int))

;;; SNI
(sb-alien:define-alien-routine ("SSL_ctrl" %ssl-ctrl) sb-alien:long
  (ssl (* t))
  (cmd sb-alien:int)
  (larg sb-alien:long)
  (parg (* t)))

;;; Hostname verification (OpenSSL 1.1.0+)
(sb-alien:define-alien-routine ("SSL_set1_host" %ssl-set1-host) sb-alien:int
  (ssl (* t))
  (hostname sb-alien:c-string))

;;; Constants
(defconstant +ssl-verify-peer+ 1)
(defconstant +ssl-ctrl-set-tlsext-hostname+ 55)
(defconstant +ssl-ctrl-set-min-proto-version+ 123)
(defconstant +tls1-2-version+ #x0303)
(defconstant +ssl-error-syscall+ 5)
(defconstant +ssl-error-zero-return+ 6)

;;; ---------------------------------------------------------------------------
;;; SSL_CTX — shared context, created once
;;; ---------------------------------------------------------------------------

(defvar *ssl-ctx* nil
  "Shared SSL_CTX for outbound TLS connections. Created on first use.")

(defvar *ssl-ctx-lock* (sb-thread:make-mutex :name "ssl-ctx-init"))

(defun ensure-ssl-ctx ()
  "Create the shared SSL_CTX if not already created. Thread-safe.
   Always takes the mutex rather than double-checked locking: on
   weakly-ordered architectures (aarch64 Graviton, Ampere, Apple
   Silicon), a thread could observe the *SSL-CTX* pointer set before
   the SSL_CTX_new + set_default_verify_paths + set_verify init
   sequence is fully visible. Init happens once per process, so the
   mutex cost is irrelevant — correctness on every supported target
   beats a micro-optimisation on x86 alone."
  (sb-thread:with-mutex (*ssl-ctx-lock*)
    (unless *ssl-ctx*
      ;; Initialize OpenSSL
      (unless (= 1 (%openssl-init-ssl 0 (sb-sys:int-sap 0)))
        (error "OPENSSL_init_ssl failed"))
      ;; Create context with modern TLS client method
      (let ((ctx (%ssl-ctx-new (%tls-client-method))))
        (when (sb-sys:sap= (sb-alien:alien-sap ctx) (sb-sys:int-sap 0))
          (error "SSL_CTX_new failed"))
        ;; Require TLS 1.2+ (RFC 8996 deprecates 1.0/1.1). SSL_CTX_ctrl
        ;; returns 1 on success, 0 on failure for SET_MIN_PROTO_VERSION.
        ;; A silent failure on OpenSSL 1.1.1 (still shipped by long-tail
        ;; LTS distros, where the default floor is TLS 1.0) would leave
        ;; the client willing to negotiate 1.0 against a misconfigured
        ;; peer; OpenSSL 3.0's default security level already forbids
        ;; 1.0/1.1 so this check is redundant there but free to keep.
        (unless (= 1 (%ssl-ctrl ctx +ssl-ctrl-set-min-proto-version+
                                +tls1-2-version+ (sb-sys:int-sap 0)))
          (error "SSL_CTX set min proto version failed"))
        ;; Load system CA certificates
        (when (zerop (%ssl-ctx-set-default-verify-paths ctx))
          (log-warn "tls: could not load system CA certificates"))
        ;; Enable peer certificate verification
        (%ssl-ctx-set-verify ctx +ssl-verify-peer+ (sb-sys:int-sap 0))
        (setf *ssl-ctx* ctx)
        (log-info "tls: SSL context initialized")))
    *ssl-ctx*))

;;; ---------------------------------------------------------------------------
;;; TLS connection lifecycle
;;; ---------------------------------------------------------------------------

(defun tls-connect (hostname port)
  "Open a blocking TLS connection to HOSTNAME:PORT.
   Returns (values ssl-ptr socket) on success. DNS resolution and the
   TCP connect are both bounded by *FETCH-TIMEOUT*; DNS goes through
   the shared *DNS-RESOLVE-BLOCKING-FN* getent resolver (same one the
   async HTTP path uses) so both v4 and v6 addresses are handled and
   there is exactly one DNS primitive in the framework."
  (let ((ctx (ensure-ssl-ctx))
        (ssl nil)
        (socket nil))
    (multiple-value-bind (ip family)
        (funcall *dns-resolve-blocking-fn* hostname)
      (unless ip
        (error "tls-connect: failed to resolve ~a" hostname))
      (setf socket (make-instance (if (eq family :inet)
                                      'sb-bsd-sockets:inet-socket
                                      'sb-bsd-sockets:inet6-socket)
                                  :type :stream :protocol :tcp))
      (handler-case
          (progn
            (set-socket-timeout (sb-bsd-sockets:socket-file-descriptor socket)
                                *fetch-timeout*)
            (blocking-connect socket ip port *fetch-timeout*)
            ;; Create SSL object
            (setf ssl (%ssl-new ctx))
            (when (sb-sys:sap= (sb-alien:alien-sap ssl) (sb-sys:int-sap 0))
              (error "SSL_new failed"))
            ;; Set SNI hostname (must be null-terminated — SSL_ctrl uses
            ;; strlen). SSL_set_tlsext_host_name returns 1 on success,
            ;; 0 on failure. A silent failure here meant the handshake
            ;; proceeded without SNI and the upstream typically rejected
            ;; with an opaque 'unknown certificate' further down the
            ;; stack — raising loud at the call site is much easier to
            ;; diagnose than debugging the eventual cert mismatch.
            (let ((hostname-bytes (concatenate '(simple-array (unsigned-byte 8) (*))
                                                (sb-ext:string-to-octets hostname
                                                                         :external-format :ascii)
                                                #(0))))
              (sb-sys:with-pinned-objects (hostname-bytes)
                (unless (= 1 (%ssl-ctrl ssl +ssl-ctrl-set-tlsext-hostname+ 0
                                        (sb-sys:vector-sap hostname-bytes)))
                  (error "SSL_set_tlsext_host_name failed for ~a" hostname))))
            ;; Enable hostname verification (OpenSSL 1.1.0+)
            (when (zerop (%ssl-set1-host ssl hostname))
              (error "SSL_set1_host failed"))
            ;; Attach to socket fd
            (unless (= 1 (%ssl-set-fd ssl (sb-bsd-sockets:socket-file-descriptor socket)))
              (error "SSL_set_fd failed"))
            ;; TLS handshake
            (let ((result (%ssl-connect ssl)))
              (unless (= result 1)
                (error "SSL_connect failed: error ~d"
                       (%ssl-get-error ssl result))))
            (values ssl socket))
        (error (e)
          (when ssl
            (ignore-errors (%ssl-free ssl)))
          (ignore-errors (sb-bsd-sockets:socket-close socket))
          (error "tls-connect ~a:~d failed: ~a" hostname port e))))))

(defun tls-write-all (ssl bytes)
  "Write all BYTES through the SSL connection. Blocks until complete."
  (let ((pos 0)
        (len (length bytes)))
    (loop while (< pos len)
          do (sb-sys:with-pinned-objects (bytes)
               (let ((n (%ssl-write ssl
                                    (sb-sys:sap+ (sb-sys:vector-sap bytes) pos)
                                    (- len pos))))
                 (when (<= n 0)
                   (error "SSL_write failed: error ~d" (%ssl-get-error ssl n)))
                 (incf pos n))))))

(defun ssl-read-eof-or-raise (ssl n)
  "Classify a non-positive SSL_read return. Returns :EOF if the peer
   cleanly closed the stream, raises otherwise so the outer
   handler-case converts the error into a 502 and fires the fetch
   callback's cleanup sentinel.

   SSL_ERROR_SYSCALL conflates at least four distinct conditions
   and must NOT be treated uniformly as clean EOF:
     errno = 0                 — unexpected EOF with no close_notify.
                                 Benign for HTTP/1.0-style legacy
                                 servers that drop the TCP connection
                                 as their framing signal. Treat as
                                 clean EOF.
     errno = EAGAIN/EWOULDBLOCK — SO_RCVTIMEO fired (the
                                 *fetch-timeout* we install on the
                                 socket in tls-connect). This is the
                                 behavior DEPLOYMENT.md promises the
                                 framework enforces; silent-EOF here
                                 made that promise a lie for
                                 close-delimited HTTPS responses and
                                 for http-fetch-stream over HTTPS.
     errno = ECONNRESET / EPIPE / ETIMEDOUT / other
                              — real transport failure, including
                                 the nasty MITM-RST-mid-stream case
                                 where an attacker truncates a
                                 response and the app sees 'success'.
                              Loud raise.
   Other SSL errors (WANT_READ / WANT_WRITE / SSL / etc) also raise."
  (let ((err (%ssl-get-error ssl n)))
    (cond
      ((= err +ssl-error-zero-return+) :eof)
      ((= err +ssl-error-syscall+)
       (let ((errno (get-errno)))
         (cond
           ((zerop errno) :eof)
           ((or (= errno +eagain+) (= errno +ewouldblock+))
            (error "SSL_read: timed out (~a)" (errno-string errno)))
           (t
            (error "SSL_read: transport error ~a" (errno-string errno))))))
      (t (error "SSL_read failed: error ~d" err)))))

(defun tls-read-all (ssl)
  "Read the complete HTTP response through the SSL connection.
   Returns the raw response as a byte vector. Bounded by
   *MAX-OUTBOUND-RESPONSE-SIZE* (headers + body together) — the
   inbound *MAX-BODY-SIZE* cap was the wrong knob here, since a
   legitimate 1 MB HTTPS response with a few hundred bytes of
   headers exceeds the inbound-request budget on principle."
  (let ((chunks nil)
        (total 0)
        (buf (make-array 8192 :element-type '(unsigned-byte 8))))
    (loop
      (sb-sys:with-pinned-objects (buf)
        (let ((n (%ssl-read ssl (sb-sys:vector-sap buf) (length buf))))
          (cond
            ((> n 0)
             (when (> (+ total n) *max-outbound-response-size*)
               (error "HTTPS response too large (~d bytes, max ~d)"
                      (+ total n) *max-outbound-response-size*))
             (incf total n)
             (push (subseq buf 0 n) chunks))
            ((zerop n)
             (ssl-read-eof-or-raise ssl n)
             (return))
            (t
             (ssl-read-eof-or-raise ssl n)
             (return))))))
    ;; Concatenate chunks
    (let ((result (make-array total :element-type '(unsigned-byte 8)))
          (offset 0))
      (dolist (chunk (nreverse chunks))
        (replace result chunk :start1 offset)
        (incf offset (length chunk)))
      result)))

(defun tls-close (ssl socket)
  "Shut down a TLS connection and close the socket."
  (ignore-errors (%ssl-shutdown ssl))
  (ignore-errors (%ssl-free ssl))
  (ignore-errors (sb-bsd-sockets:socket-close socket)))

;;; ---------------------------------------------------------------------------
;;; Blocking HTTPS fetch — called by the core framework via *https-fetch-fn*
;;; ---------------------------------------------------------------------------

(defun https-fetch (conn epoll-fd fetch-req host port path)
  "Perform a blocking HTTPS fetch and deliver the result to CONN.
   Called by initiate-fetch when the URL scheme is :https.

   The fetch callback fires exactly once per call — either with real
   (status headers body) arguments on the happy path, or with
   (nil nil nil) as a cleanup sentinel in every error path
   (tls-connect failure, handshake error, parse error, truncation).
   Apps get a single defined moment to release DB handles, close
   metric spans, or decrement rate-limit counters regardless of how
   the fetch ends. CALLBACK-FIRED is flipped just before the happy-
   path funcall so that if the user callback itself raises, the
   outer handler-case does not re-invoke it."
  (let ((callback (http-fetch-continuation-callback fetch-req))
        (callback-fired nil))
    (handler-case
        (multiple-value-bind (ssl socket)
            (tls-connect host port)
          (unwind-protect
              (let ((method (http-fetch-continuation-method fetch-req)))
                ;; Build and send the HTTP request
                (let ((request-bytes (build-outbound-request
                                     method host path
                                     :scheme :https :port port
                                     :headers (http-fetch-continuation-headers fetch-req)
                                     :body (http-fetch-continuation-body fetch-req))))
                  (tls-write-all ssl request-bytes))
                ;; Read the complete response. The parsing discipline
                ;; here mirrors COMPLETE-FETCH on the plain path:
                ;; header-end and status must both be present before
                ;; the callback fires the happy-path branch, otherwise
                ;; we raise and let the outer handler-case convert to
                ;; 502 + cleanup sentinel. A 'status = 0' happy-path
                ;; callback is a DEPLOYMENT.md contract violation —
                ;; apps pattern-matching on (if status ...) treat the
                ;; integer 0 as truthy and blow up interpreting it as
                ;; an HTTP status.
                (let* ((response-buf (tls-read-all ssl))
                       (buf-len (length response-buf))
                       (header-end (scan-crlf-crlf response-buf 0 buf-len)))
                  (unless header-end
                    (error "https: upstream response has no parseable headers"))
                  (let* ((status (parse-response-status response-buf 0 buf-len))
                         (headers
                          (let ((first-crlf (scan-crlf response-buf 0 header-end)))
                            (when first-crlf
                              (parse-headers-bytes response-buf
                                                   (+ first-crlf 2)
                                                   (+ header-end 4)))))
                         (body-start (+ header-end 4))
                         ;; RFC 7230 §3.3.3: TE takes precedence over CL
                         (chunked-p (response-chunked-p headers))
                         ;; RFC 7230 §3.3.3 rule 3: any TE present means
                         ;; CL is ignored — read-until-close, not CL-framed.
                         (te-present (scan-transfer-encoding response-buf
                                                             header-end))
                         (content-length (unless te-present
                                           (scan-content-length response-buf
                                                                header-end))))
                    (unless status
                      (error "https: upstream status line unparseable"))
                    ;; Truncation guard: an upstream that declares a
                    ;; Content-Length and then closes short must not be
                    ;; allowed to hand us a silently-truncated body. The
                    ;; MITM case is the nasty one — attacker RSTs
                    ;; mid-stream and the app receives short data with no
                    ;; indication. Signal an error so the outer
                    ;; handler-case converts it into a 502 and fires
                    ;; the cleanup sentinel.
                    ;;
                    ;; Skipped for 1xx/204/304 (carry CL but MUST NOT
                    ;; have a body per RFC 7230 §3.3.3 rule 1 / RFC 7232
                    ;; §4.1) and for HEAD (RFC 7231 §4.3.2 — upstream
                    ;; echoes the GET-body CL but MUST NOT send a body).
                    ;; Twin of the exemption in fetch.lisp COMPLETE-FETCH
                    ;; on the plain-HTTP path.
                    (when (and content-length
                               (not (or (<= 100 status 199) (= status 204) (= status 304)))
                               (not (eq method :HEAD))
                               (< (- buf-len body-start) content-length))
                      (error "https: short body (~d of ~d bytes)"
                             (- buf-len body-start) content-length))
                    (let* ((body-end (if content-length
                                         (min buf-len (+ body-start content-length))
                                         buf-len))
                           ;; 1xx/204/304 MUST NOT have a body (RFC 7230 §3.3.3 rule 1).
                           ;; HEAD MUST NOT include a body (RFC 7231 §4.3.2).
                           ;; Force empty regardless of what the upstream sent.
                           (body-end (if (or (<= 100 status 199) (= status 204) (= status 304)
                                             (eq method :HEAD))
                                         body-start
                                         body-end))
                           (raw-body (when (> body-end body-start)
                                       (subseq response-buf body-start body-end)))
                           (body (if (and raw-body chunked-p)
                                     (decode-chunked-body raw-body 0 (length raw-body))
                                     raw-body)))
                    ;; Mark the callback as fired before the funcall so
                    ;; that a raising user callback doesn't get invoked
                    ;; a second time with nil sentinels in the outer
                    ;; handler-case's cleanup branch.
                    (setf callback-fired t)
                    (let ((response (funcall callback
                                             status (or headers nil)
                                             (or body nil))))
                      ;; Deliver to inbound connection
                      (let ((bytes (cond
                                     ((typep response '(simple-array (unsigned-byte 8) (*)))
                                      response)
                                     ((typep response 'http-fetch-continuation)
                                      ;; Chained fetch
                                      (initiate-fetch conn epoll-fd response)
                                      (return-from https-fetch))
                                     (t (format-response response)))))
                        (setf bytes (strip-body-for-head bytes conn))
                        (connection-queue-write conn bytes)
                        (setf (connection-state conn) :write-response
                              (connection-last-active conn) (get-universal-time))
                        (epoll-modify epoll-fd (connection-fd conn)
                                      (logior +epollout+ +epollet+))
                        (log-debug "fetch: https ~a:~d~a -> fd ~d"
                                   host port path (connection-fd conn))))))))
            (tls-close ssl socket)))
      (error (e)
        (log-error "https fetch failed: ~a" e)
        ;; Fire the cleanup sentinel in every pre-delivery error
        ;; path so the app's :then closure runs exactly once.
        ;; Wrapped in its own handler-case — a raising cleanup hook
        ;; must not block the 502 from reaching the inbound.
        (unless callback-fired
          (handler-case (funcall callback nil nil nil)
            (error (e2)
              (log-warn "fetch cleanup callback raised: ~a" e2))))
        (let ((err-bytes (strip-body-for-head
                         (format-response (make-error-response 502))
                         conn)))
          (connection-queue-write conn err-bytes)
          (setf (connection-state conn) :write-response
                (connection-last-active conn) (get-universal-time))
          (epoll-modify epoll-fd (connection-fd conn)
                        (logior +epollout+ +epollet+)))))))

;;; ---------------------------------------------------------------------------
;;; Blocking streaming HTTPS fetch
;;; ---------------------------------------------------------------------------

(defun https-fetch-stream (method host port path headers body on-line)
  "Blocking streaming HTTPS fetch. Calls ON-LINE per response body line."
  (multiple-value-bind (ssl socket)
      (tls-connect host port)
    (unwind-protect
        (progn
          (tls-write-all ssl (build-outbound-request method host path
                                                     :scheme :https :port port
                                                     :headers headers :body body))
          (tls-stream-response ssl on-line :method method))
      (tls-close ssl socket))))

(defun tls-stream-response (ssl on-line &key (method :GET))
  "Read HTTP response via SSL, skip headers, call ON-LINE per body line.
   Handles chunked transfer encoding. Returns the status code.

   METHOD gates the body-framing discipline: for :HEAD, RFC 7231
   §4.3.2 guarantees an empty body even when the upstream echoes
   the GET-body Content-Length, so the body phase is skipped
   entirely and the post-loop truncation checks are bypassed.

   Truncation discipline on the TLS streaming path (twin of the
   plain-path STREAM-RESPONSE-LINES):
     - chunked: TERMINATED is set when the zero-size chunk header
       arrives. The post-loop check raises if the outer loop exited
       without seeing it (mid-body close, MITM RST, zero-byte SSL
       read classified as benign :eof by ssl-read-eof-or-raise).
     - content-length (no TE): BODY-CONSUMED counts body bytes as
       they are processed, and the post-loop check raises if the
       count is short of the declared length. The SSL-layer
       classifier treats an errno=0 close as benign :eof, so the
       CL comparison is the only thing standing between a MITM
       mid-body close on a CL-framed HTTPS stream and the app
       receiving a silently truncated response.
     - close-delimited (no TE, no CL): the connection close IS
       the framing signal; clean EOF is treated as complete."
  (let ((buf (make-array 8192 :element-type '(unsigned-byte 8)))
        (line-buf (make-array 4096 :element-type '(unsigned-byte 8)
                                   :fill-pointer 0 :adjustable t))
        (status nil)
        (chunked nil)
        (te-present nil)
        (content-length nil)
        (body-consumed 0)
        (terminated nil)
        (in-headers t)
        (header-count 0)
        ;; WHATWG EventStream §9.2: CR, LF, and CRLF are equivalent
        ;; line terminators. PREV-CR carries across SSL-read
        ;; iterations so a CRLF pair split at a TLS record boundary
        ;; collapses to one terminator. Scoped to content-phase use —
        ;; header and chunk-size phases still use the lenient strip-CR
        ;; behavior (headers and framing are strict CRLF in practice
        ;; and a lone CR there is malformed either way).
        (prev-cr nil)
        (first-line t)
        ;; Chunked state
        (in-chunk-size nil)
        (chunk-remaining 0)
        (expect-cr nil)    ; awaiting CR after chunk-data
        (expect-lf nil)    ; awaiting LF after chunk-data
        (chunk-size-buf (make-array 20 :element-type '(unsigned-byte 8)
                                       :fill-pointer 0 :adjustable t)))
    (flet ((emit-line ()
             (let ((line (sb-ext:octets-to-string
                          (subseq line-buf 0 (fill-pointer line-buf))
                          :external-format :utf-8)))
               (setf (fill-pointer line-buf) 0)
               line))
           (emit-body-line ()
             (let ((line (sb-ext:octets-to-string
                          (subseq line-buf 0 (fill-pointer line-buf))
                          :external-format :utf-8)))
               (setf (fill-pointer line-buf) 0)
               (when on-line (funcall on-line line)))))
      (loop
        (when terminated (return))
        (when (and content-length (not te-present)
                   (>= body-consumed content-length))
          (return))
        (sb-sys:with-pinned-objects (buf)
          (let ((n (%ssl-read ssl (sb-sys:vector-sap buf) (length buf))))
            (cond
              ((zerop n)
               (ssl-read-eof-or-raise ssl n)
               (return))
              ((< n 0)
               ;; Non-positive return must NOT be treated as clean EOF
               ;; unconditionally — SSL_ERROR_SYSCALL conflates timeout,
               ;; transport error, and benign HTTP/1.0 connection-close.
               ;; Same discipline as tls-read-all: the helper either
               ;; returns :EOF (benign) or raises so the outer
               ;; unwind-protect tears down and the caller sees the
               ;; error instead of a silently-truncated NDJSON/SSE
               ;; stream.
               (ssl-read-eof-or-raise ssl n)
               (return))
              (t
               (loop for i from 0 below n
                     for byte = (aref buf i)
                     do (when (>= (fill-pointer line-buf) *max-streaming-line-size*)
                          (error "streaming response line too large (~d bytes, max ~d)"
                                 (fill-pointer line-buf)
                                 *max-streaming-line-size*))
                        (cond
                          ;; Header phase
                          (in-headers
                           (cond
                             ((= byte 10)
                              (let ((line (emit-line)))
                                (if (zerop (length line))
                                    (progn
                                      (setf in-headers nil)
                                      (when chunked (setf in-chunk-size t))
                                      ;; HEAD: RFC 7231 §4.3.2 — no body
                                      ;; regardless of CL / chunked framing.
                                      ;; Return status directly and skip
                                      ;; the post-loop truncation checks.
                                      (when (eq method :HEAD)
                                        (return-from tls-stream-response
                                          (or status
                                              (error "https streaming: no parseable status line")))))
                                    (progn
                                      (incf header-count)
                                      (when (> header-count *max-header-count*)
                                        (error "https streaming: too many headers (~d)"
                                               header-count))
                                      (when first-line
                                        (let ((sp (position #\Space line)))
                                          (when (and sp (<= (+ sp 4) (length line)))
                                            (let ((d1 (char line (+ sp 1)))
                                                  (d2 (char line (+ sp 2)))
                                                  (d3 (char line (+ sp 3))))
                                              (when (and (digit-char-p d1)
                                                         (digit-char-p d2)
                                                         (digit-char-p d3)
                                                         (or (= (+ sp 4) (length line))
                                                             (char= (char line (+ sp 4)) #\Space)))
                                                (let ((code (+ (* 100 (digit-char-p d1))
                                                               (* 10 (digit-char-p d2))
                                                               (digit-char-p d3))))
                                                  (when (<= 100 code 599)
                                                    (setf status code)))))))
                                        (setf first-line nil))
                                      (when (and (>= (length line) 18)
                                                 (string-equal line "transfer-encoding:"
                                                               :end1 18))
                                        (setf te-present t)
                                        (let ((value (string-trim '(#\Space #\Tab)
                                                                   (subseq line 18))))
                                          (when (connection-header-has-token-p value "chunked")
                                            (setf chunked t))))
                                      ;; Capture Content-Length for
                                      ;; the non-chunked truncation
                                      ;; check. Strict digits-only
                                      ;; parse — same discipline as
                                      ;; SCAN-CONTENT-LENGTH on the
                                      ;; inbound side so a malformed
                                      ;; value does not silently
                                      ;; disable the check.
                                      (when (and (>= (length line) 15)
                                                 (string-equal line "content-length:"
                                                               :end1 15))
                                        (let ((value (string-trim '(#\Space #\Tab)
                                                                   (subseq line 15))))
                                          (unless (and (> (length value) 0)
                                                       (every (lambda (c)
                                                                (char<= #\0 c #\9))
                                                              value))
                                            (error "https streaming: malformed Content-Length ~s"
                                                   value))
                                          (unless (<= (length value) 10)
                                            (error "https streaming: Content-Length too many digits"))
                                          (let ((n (parse-integer value)))
                                            (when (and content-length (/= n content-length))
                                              (error "https streaming: conflicting Content-Length ~d vs ~d"
                                                     content-length n))
                                            (setf content-length n))))))))
                             ((= byte 13) nil)
                             (t (vector-push-extend byte line-buf))))
                          ;; Chunked body — reading chunk size (separate
                          ;; buffer so body content in line-buf is not
                          ;; corrupted across chunk boundaries). Strict
                          ;; parse: parse-chunked-size-bytes strips
                          ;; chunk-extensions (RFC 7230 §4.1.1 — anything
                          ;; from ';' onwards), requires at least one
                          ;; hex digit, and raises on parse failure.
                          ;; The prior :junk-allowed t + NIL → final-
                          ;; chunk behavior was a parser-disagreement
                          ;; smuggling primitive between us and any
                          ;; stricter downstream.
                          ((and chunked in-chunk-size)
                           (cond
                             ((= byte 10)
                              (when (> (fill-pointer chunk-size-buf) 0)
                                (let ((size (parse-chunked-size-bytes
                                             chunk-size-buf 0
                                             (fill-pointer chunk-size-buf))))
                                  (setf (fill-pointer chunk-size-buf) 0)
                                  (cond
                                    ((zerop size)
                                     ;; Final chunk — mark terminated
                                     ;; so the outer loop's guard
                                     ;; exits cleanly. The post-loop
                                     ;; check then passes, and a
                                     ;; stream that closed mid-body
                                     ;; without reaching this point
                                     ;; will raise.
                                     (setf terminated t)
                                     (return))
                                    (t (setf chunk-remaining size
                                             in-chunk-size nil))))))
                             ((= byte 13) nil)
                             (t (when (>= (fill-pointer chunk-size-buf) 256)
                                 (error "chunked stream: chunk-size line too long"))
                               (vector-push-extend byte chunk-size-buf))))
                          ;; Strict CRLF after chunk-data (RFC 7230 4.1).
                          ;; Symmetric with decode-chunked-body and
                          ;; reader-expect-crlf on the plain paths.
                          (expect-cr
                           (unless (= byte 13)
                             (error "chunked stream: expected CR after chunk-data"))
                           (setf expect-cr nil
                                 expect-lf t))
                          (expect-lf
                           (unless (= byte 10)
                             (error "chunked stream: expected LF after chunk-data"))
                           (setf expect-lf nil
                                 in-chunk-size t))
                          ;; Chunked body — reading chunk data. Content-
                          ;; phase terminators are CR / LF / CRLF
                          ;; (WHATWG EventStream §9.2); PREV-CR carries
                          ;; a CR's LF-partner across subsequent bytes
                          ;; so the LF does not emit a second line.
                          (chunked
                           (when (> chunk-remaining 0)
                             (decf chunk-remaining)
                             (cond
                               ((= byte 13)
                                (emit-body-line)
                                (setf prev-cr t))
                               ((= byte 10)
                                (cond
                                  (prev-cr (setf prev-cr nil))
                                  (t (emit-body-line))))
                               (t
                                (setf prev-cr nil)
                                (vector-push-extend byte line-buf))))
                           (when (zerop chunk-remaining)
                             (setf expect-cr t)))
                          ;; Non-chunked body. BODY-CONSUMED tracks
                          ;; every byte that flows through the body
                          ;; cond, so the post-loop CL check can
                          ;; compare against declared length. For
                          ;; close-delimited responses (no CL set),
                          ;; the count is still maintained but never
                          ;; compared. CR / LF / CRLF treated as
                          ;; equivalent terminators (same as chunked
                          ;; and the plain-path reader).
                          (t
                           (incf body-consumed)
                           (cond
                             ((= byte 13)
                              (emit-body-line)
                              (setf prev-cr t))
                             ((= byte 10)
                              (cond
                                (prev-cr (setf prev-cr nil))
                                (t (emit-body-line))))
                             (t
                              (setf prev-cr nil)
                              (vector-push-extend byte line-buf)))
                           (when (and content-length (not te-present)
                                      (>= body-consumed content-length))
                             ;; Hit declared length — stop processing
                             ;; and let the outer loop exit via its
                             ;; pre-read guard so any trailing bytes
                             ;; in the current buffer are discarded.
                             (return)))))))))))
    ;; Post-loop truncation checks. Raise loud on either framing
    ;; shape that came up short — the outer UNWIND-PROTECT in
    ;; HTTPS-FETCH-STREAM closes the TLS session and the error
    ;; propagates to the app so a silently-truncated NDJSON / SSE
    ;; stream is no longer presented as 'success with short body'.
    (when (and chunked (not terminated))
      (error "https streaming: chunked response missing zero-size terminator"))
    (when (and content-length (not te-present)
               (< body-consumed content-length))
      (error "https streaming: short body (~d of ~d bytes)"
             body-consumed content-length))
    ;; Flush any remaining unterminated line
    (when (> (fill-pointer line-buf) 0)
      (when on-line
        (funcall on-line (sb-ext:octets-to-string
                          (subseq line-buf 0 (fill-pointer line-buf))
                          :external-format :utf-8))))
    (unless status
      (error "https streaming: no parseable status line"))
    status))

;;; ===========================================================================
;;; Crypto primitives — EVP digest + ECDSA verify
;;;
;;; At load time, this file swaps web-skeleton's public SHA-1, SHA-256,
;;; and ECDSA-VERIFY-P256 symbols for libssl-backed implementations via
;;; SETF SYMBOL-FUNCTION. The pure-Lisp originals stay reachable as
;;; SHA1-LISP / SHA256-LISP / ECDSA-VERIFY-P256-LISP for framework-dev
;;; verification via TEST-PURE-LISP-CRYPTO.
;;;
;;; SHA uses the EVP_MD_CTX interface — the modern, non-deprecated path.
;;; We deliberately avoid the one-shot SHA1() / SHA256() symbols, which
;;; are marked OSSL_DEPRECATEDIN_3_0 in OpenSSL 3's headers.
;;;
;;; ECDSA uses d2i_PUBKEY (not deprecated in 3.0) to parse a hand-built
;;; SubjectPublicKeyInfo, then EVP_PKEY_verify against a DER-encoded
;;; SEQUENCE { r, s } signature. HMAC-SHA256 is not accelerated
;;; directly — it's pure-Lisp, but its internal SHA-256 calls route
;;; through the function cell and pick up the libssl swap for free.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; FFI bindings (digest)
;;; ---------------------------------------------------------------------------

(sb-alien:define-alien-routine ("EVP_MD_CTX_new" %evp-md-ctx-new) (* t))

(sb-alien:define-alien-routine ("EVP_MD_CTX_free" %evp-md-ctx-free) sb-alien:void
  (ctx (* t)))

(sb-alien:define-alien-routine ("EVP_sha1"   %evp-sha1)   (* t))
(sb-alien:define-alien-routine ("EVP_sha256" %evp-sha256) (* t))

(sb-alien:define-alien-routine ("EVP_DigestInit_ex" %evp-digest-init-ex)
    sb-alien:int
  (ctx  (* t))
  (md   (* t))
  (impl (* t)))

(sb-alien:define-alien-routine ("EVP_DigestUpdate" %evp-digest-update)
    sb-alien:int
  (ctx  (* t))
  (data (* t))
  (len  sb-alien:unsigned-long))

(sb-alien:define-alien-routine ("EVP_DigestFinal_ex" %evp-digest-final-ex)
    sb-alien:int
  (ctx    (* t))
  (md     (* t))
  (outlen (* t)))

;;; ---------------------------------------------------------------------------
;;; FFI bindings (ECDSA verify via EVP_PKEY)
;;; ---------------------------------------------------------------------------

(sb-alien:define-alien-routine ("d2i_PUBKEY" %d2i-pubkey) (* t)
  (a      (* t))
  (pp     (* t))
  (length sb-alien:long))

(sb-alien:define-alien-routine ("EVP_PKEY_free" %evp-pkey-free) sb-alien:void
  (pkey (* t)))

(sb-alien:define-alien-routine ("EVP_PKEY_CTX_new" %evp-pkey-ctx-new) (* t)
  (pkey   (* t))
  (engine (* t)))

(sb-alien:define-alien-routine ("EVP_PKEY_CTX_free" %evp-pkey-ctx-free)
    sb-alien:void
  (ctx (* t)))

(sb-alien:define-alien-routine ("EVP_PKEY_verify_init" %evp-pkey-verify-init)
    sb-alien:int
  (ctx (* t)))

(sb-alien:define-alien-routine ("EVP_PKEY_verify" %evp-pkey-verify) sb-alien:int
  (ctx    (* t))
  (sig    (* t))
  (siglen sb-alien:unsigned-long)
  (tbs    (* t))
  (tbslen sb-alien:unsigned-long))

;;; ---------------------------------------------------------------------------
;;; SHA via EVP_MD_CTX
;;; ---------------------------------------------------------------------------

(defun %libssl-digest (evp-md data out-len)
  "Compute a digest via OpenSSL's EVP interface. EVP-MD is the alien
   pointer returned by %EVP-SHA1 or %EVP-SHA256. DATA is the input byte
   vector. OUT-LEN is the expected digest size in bytes (20 for SHA-1,
   32 for SHA-256). Returns a fresh OUT-LEN byte vector."
  (let ((ctx (%evp-md-ctx-new)))
    (when (sb-sys:sap= (sb-alien:alien-sap ctx) (sb-sys:int-sap 0))
      (error "EVP_MD_CTX_new failed"))
    (unwind-protect
         (progn
           (unless (= 1 (%evp-digest-init-ex ctx evp-md (sb-sys:int-sap 0)))
             (error "EVP_DigestInit_ex failed"))
           (sb-sys:with-pinned-objects (data)
             (unless (= 1 (%evp-digest-update ctx
                                              (sb-sys:vector-sap data)
                                              (length data)))
               (error "EVP_DigestUpdate failed")))
           (let ((out (make-array out-len :element-type '(unsigned-byte 8))))
             (sb-sys:with-pinned-objects (out)
               ;; NULL out-len argument = skip writing the length back;
               ;; we already know the size for a fixed-digest algorithm.
               (unless (= 1 (%evp-digest-final-ex
                             ctx
                             (sb-sys:vector-sap out)
                             (sb-sys:int-sap 0)))
                 (error "EVP_DigestFinal_ex failed")))
             out))
      (%evp-md-ctx-free ctx))))

(defun sha1-libssl (data)
  "libssl-accelerated SHA-1. Matches SHA1-LISP's contract — byte-vector
   in, 20-byte digest out."
  (%libssl-digest (%evp-sha1) data 20))

(defun sha256-libssl (data)
  "libssl-accelerated SHA-256. Matches SHA256-LISP's contract — byte
   vector in, 32-byte digest out."
  (%libssl-digest (%evp-sha256) data 32))

;;; ---------------------------------------------------------------------------
;;; ECDSA P-256 verify via d2i_PUBKEY + EVP_PKEY_verify
;;; ---------------------------------------------------------------------------

(defparameter *p256-spki-prefix*
  (make-array 27 :element-type '(unsigned-byte 8)
              :initial-contents '(#x30 #x59 #x30 #x13
                                   #x06 #x07 #x2A #x86 #x48 #xCE #x3D #x02 #x01
                                   #x06 #x08 #x2A #x86 #x48 #xCE #x3D #x03 #x01 #x07
                                   #x03 #x42 #x00 #x04))
  "Fixed 27-byte DER prefix for a NIST P-256 SubjectPublicKeyInfo with
   an uncompressed public-key point. Callers append X (32 bytes) and
   Y (32 bytes) for a total 91-byte SPKI that d2i_PUBKEY parses as an
   EVP_PKEY. Byte map:
     30 59                               ; SEQUENCE, 89 bytes content
       30 13                             ; SEQUENCE, 19 bytes content
         06 07 2A 86 48 CE 3D 02 01      ; OID 1.2.840.10045.2.1 ecPublicKey
         06 08 2A 86 48 CE 3D 03 01 07   ; OID 1.2.840.10045.3.1.7 prime256v1
       03 42                             ; BIT STRING, 66 bytes content
         00                              ; 0 unused bits
         04                              ; uncompressed point marker
         (caller appends 32-byte X, 32-byte Y)")

(defun build-p256-spki (x y)
  "Assemble a 91-byte DER SubjectPublicKeyInfo for a P-256 public key
   from raw 32-byte X and Y coordinates. Fresh byte vector each call."
  (let ((out (make-array 91 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace out *p256-spki-prefix*)
    (replace out x :start1 27 :end1 59)
    (replace out y :start1 59 :end1 91)
    out))

(defun %der-integer-bounds (sig-bytes offset)
  "Return (values START LEN PAD-P) for the minimal DER INTEGER
   encoding of the 32-byte big-endian value at SIG-BYTES[OFFSET..OFFSET+32).
   Strips leading zeros per X.690 8.3.2 and adds a 0x00 pad when
   the first significant byte has bit 7 set (sign preservation)."
  (let ((start offset)
        (end (+ offset 32)))
    ;; Strip leading zeros, keep at least one byte
    (loop while (and (< (1+ start) end)
                     (zerop (aref sig-bytes start)))
          do (incf start))
    (let* ((pad (logbitp 7 (aref sig-bytes start)))
           (raw-len (- end start))
           (len (if pad (1+ raw-len) raw-len)))
      (values start len pad))))

(defun der-encode-ecdsa-signature (sig-bytes)
  "DER-encode a raw 64-byte ECDSA signature (r || s) as a minimal
   ASN.1 SEQUENCE { INTEGER r, INTEGER s }. Strips leading zeros
   and adds sign-preservation padding per X.690 8.3.2."
  (multiple-value-bind (r-start r-len r-pad) (%der-integer-bounds sig-bytes 0)
    (multiple-value-bind (s-start s-len s-pad) (%der-integer-bounds sig-bytes 32)
      (let* ((content-len (+ 2 r-len 2 s-len))
             (out (make-array (+ 2 content-len) :element-type '(unsigned-byte 8)))
             (pos 0))
        (setf (aref out pos) #x30) (incf pos)         ; SEQUENCE tag
        (setf (aref out pos) content-len) (incf pos)   ; SEQUENCE length
        ;; r
        (setf (aref out pos) #x02) (incf pos)          ; INTEGER tag
        (setf (aref out pos) r-len) (incf pos)         ; r length
        (when r-pad (setf (aref out pos) #x00) (incf pos))
        (replace out sig-bytes :start1 pos :start2 r-start :end2 32)
        (incf pos (- 32 r-start))
        ;; s
        (setf (aref out pos) #x02) (incf pos)          ; INTEGER tag
        (setf (aref out pos) s-len) (incf pos)         ; s length
        (when s-pad (setf (aref out pos) #x00) (incf pos))
        (replace out sig-bytes :start1 pos :start2 s-start :end2 64)
        out))))

(defun ecdsa-verify-p256-libssl (hash sig-bytes pubkey-x pubkey-y)
  "libssl-accelerated ECDSA P-256 signature verification. Matches
   ECDSA-VERIFY-P256-LISP's contract — all inputs are byte vectors,
   returns T on valid, NIL on any failure. Builds a DER SubjectPublicKeyInfo
   from the raw (X, Y) coordinates, parses it to an EVP_PKEY via
   d2i_PUBKEY, DER-encodes the raw r||s signature, then calls
   EVP_PKEY_verify. Cleanup via unwind-protect regardless of path.
   OpenSSL handles r/s range and invalid-curve point rejection
   internally, so those checks are not duplicated here. Both (r, s)
   and (r, n-s) are accepted — RFC 7515 / 7518 do not mandate low-S."
  ;; Length-gate like the pure-Lisp path. DER-ENCODE-ECDSA-SIGNATURE
  ;; reads bytes 0..63 from sig-bytes unconditionally, so anything
  ;; other than a 64-byte input must be rejected here before the
  ;; DER builder sees it — otherwise an 80-byte signature would be
  ;; silently truncated to the first 64 bytes and verified against
  ;; whatever that prefix happened to decode to.
  (unless (= (length sig-bytes) 64)
    (return-from ecdsa-verify-p256-libssl nil))
  (unless (and (= (length pubkey-x) 32) (= (length pubkey-y) 32))
    (return-from ecdsa-verify-p256-libssl nil))
  (let ((spki (build-p256-spki pubkey-x pubkey-y))
        (sig-der (der-encode-ecdsa-signature sig-bytes))
        (pkey nil)
        (ctx nil))
    (unwind-protect
         (block verify
           (sb-sys:with-pinned-objects (spki sig-der hash)
             ;; d2i_PUBKEY wants `const unsigned char **` — a pointer to
             ;; a pointer. We allocate PP on the alien stack, seed it
             ;; with the SAP of the SPKI buffer, and pass its address.
             (sb-alien:with-alien ((pp (* t)))
               (setf pp (sb-alien:sap-alien (sb-sys:vector-sap spki) (* t)))
               (setf pkey (%d2i-pubkey (sb-sys:int-sap 0)
                                        (sb-alien:addr pp)
                                        (length spki))))
             (when (sb-sys:sap= (sb-alien:alien-sap pkey) (sb-sys:int-sap 0))
               (setf pkey nil)
               (return-from verify nil))
             (setf ctx (%evp-pkey-ctx-new pkey (sb-sys:int-sap 0)))
             (when (sb-sys:sap= (sb-alien:alien-sap ctx) (sb-sys:int-sap 0))
               (setf ctx nil)
               (return-from verify nil))
             (unless (= 1 (%evp-pkey-verify-init ctx))
               (return-from verify nil))
             (= 1 (%evp-pkey-verify ctx
                                    (sb-sys:vector-sap sig-der)
                                    (length sig-der)
                                    (sb-sys:vector-sap hash)
                                    (length hash)))))
      (when ctx  (%evp-pkey-ctx-free ctx))
      (when pkey (%evp-pkey-free pkey)))))

;;; ---------------------------------------------------------------------------
;;; Registration — hook into the core framework
;;; ---------------------------------------------------------------------------

(eval-when (:load-toplevel :execute)
  (setf *https-fetch-fn* #'https-fetch)
  (setf *https-stream-fn* #'https-fetch-stream)
  ;; Swap the pure-Lisp crypto primitives for libssl-backed versions.
  ;; SHA1-LISP / SHA256-LISP / ECDSA-VERIFY-P256-LISP remain reachable
  ;; internally; TEST-PURE-LISP-CRYPTO uses them to re-verify the
  ;; pure-Lisp paths on a libssl-enabled machine. HMAC-SHA256 is not
  ;; swapped directly — it's pure-Lisp, but its internal SHA-256 calls
  ;; route through the function cell and pick up this swap for free.
  (setf (symbol-function 'sha1)              #'sha1-libssl
        (symbol-function 'sha256)            #'sha256-libssl
        (symbol-function 'ecdsa-verify-p256) #'ecdsa-verify-p256-libssl)
  (log-info "tls: HTTPS fetch enabled")
  (log-info "tls: crypto swapped to libssl (sha1, sha256, ecdsa-p256)"))
