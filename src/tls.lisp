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
  ;;
  ;; NOT using :dont-save t. The function-cell swaps at the bottom of
  ;; this file (sha1 / sha256 / ecdsa-verify-p256 → libssl-backed
  ;; versions) survive SAVE-LISP-AND-DIE, so the shared library MUST
  ;; be reachable when the dumped image is restored or the first call
  ;; to any swapped function crashes with "undefined alien function"
  ;; deep in the request path. The SBCL default (record the SONAME in
  ;; sb-sys:*shared-objects* and re-dlopen on restore) is exactly the
  ;; contract we need: if libssl is missing at image-restore time,
  ;; SBCL errors loudly at startup rather than deferring to a
  ;; mysterious crash on the hot path. Loading web-skeleton-tls
  ;; commits the image to having libssl at runtime — that's an
  ;; implied contract of the crypto swap, and removing :dont-save t
  ;; makes the contract explicit.
  (labels ((try-load (candidates)
             "Walk CANDIDATES and return the first SONAME that loads,
              or NIL if none did."
             (dolist (name candidates)
               (when (ignore-errors
                      (sb-alien:load-shared-object name)
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

;;; Additional trust anchors. Bound for the integration test, which needs a
;;; CA it generated to be trusted alongside the system roots — additive, so
;;; it does not weaken verification, which is the only reason it is
;;; acceptable to point at the shared context.
(sb-alien:define-alien-routine ("SSL_CTX_load_verify_locations"
                                %ssl-ctx-load-verify-locations) sb-alien:int
  (ctx (* t))
  (ca-file sb-alien:c-string)
  (ca-path sb-alien:c-string))

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

;;; Context-level ctrl. SSL_CTX_ctrl and SSL_ctrl are separate libssl
;;; entry points over different struct types — the header macro
;;; SSL_CTX_set_min_proto_version(ctx, v) expands to SSL_CTX_ctrl, and
;;; passing an SSL_CTX* to SSL_ctrl instead reads and writes ssl_st
;;; field offsets inside the smaller ssl_ctx_st allocation: undefined
;;; behavior that can silently pass, hard-fail, or corrupt the context
;;; depending on libssl version and heap layout. Never mix the two.
(sb-alien:define-alien-routine ("SSL_CTX_ctrl" %ssl-ctx-ctrl) sb-alien:long
  (ctx (* t))
  (cmd sb-alien:int)
  (larg sb-alien:long)
  (parg (* t)))

;;; Hostname verification (OpenSSL 1.1.0+)
(sb-alien:define-alien-routine ("SSL_set1_host" %ssl-set1-host) sb-alien:int
  (ssl (* t))
  (hostname sb-alien:c-string))

;;; SNI read-back. Bound for the same reason the GET twin of the
;;; min-proto-version ctrl is: SSL_set_tlsext_host_name reports success
;;; without the name necessarily being what a later handshake will send,
;;; and reading it off the SSL is the only check that distinguishes a
;;; configured SNI from a call that returned 1.
(sb-alien:define-alien-routine ("SSL_get_servername" %ssl-get-servername)
    sb-alien:c-string
  (ssl (* t))
  (type sb-alien:int))

;;; Constants
(defconstant +ssl-verify-peer+ 1)
(defconstant +ssl-ctrl-set-tlsext-hostname+ 55)
(defconstant +ssl-ctrl-set-min-proto-version+ 123)
(defconstant +ssl-ctrl-get-min-proto-version+ 130
  "GET twin of the SET ctrl above. Used by the test suite to read the
   floor back off the context — a wrong-entry-point SET (the SSL_ctrl /
   SSL_CTX_ctrl mixup) can return 1 while writing to a garbage offset,
   and only the read-back exposes that the floor never landed.")
(defconstant +tls1-2-version+ #x0303)
(defconstant +ssl-error-want-read+ 2)
(defconstant +ssl-error-want-write+ 3)
(defconstant +ssl-error-syscall+ 5)
(defconstant +ssl-error-zero-return+ 6)
(defconstant +tlsext-nametype-host-name+ 0
  "The only SNI name type OpenSSL implements; SSL_get_servername takes it.")

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
      (let ((ctx (%ssl-ctx-new (%tls-client-method)))
            (ready nil))
        (when (sb-sys:sap= (sb-alien:alien-sap ctx) (sb-sys:int-sap 0))
          (error "SSL_CTX_new failed"))
        ;; Free the context on any failure below. Nothing cached it yet —
        ;; *SSL-CTX* is only set on the success path — so without this
        ;; every retry of a failing init leaks another SSL_CTX, and the
        ;; CA check is a failure an app retries on every single fetch.
        (unwind-protect
             (progn
               ;; Require TLS 1.2+ (RFC 8996 deprecates 1.0/1.1). SSL_CTX_ctrl
               ;; returns 1 on success, 0 on failure for SET_MIN_PROTO_VERSION.
               ;; A silent failure on OpenSSL 1.1.1 (still shipped by long-tail
               ;; LTS distros, where the default floor is TLS 1.0) would leave
               ;; the client willing to negotiate 1.0 against a misconfigured
               ;; peer; OpenSSL 3.0's default security level already forbids
               ;; 1.0/1.1 so this check is redundant there but free to keep.
               (unless (= 1 (%ssl-ctx-ctrl ctx +ssl-ctrl-set-min-proto-version+
                                           +tls1-2-version+ (sb-sys:int-sap 0)))
                 (error "SSL_CTX set min proto version failed"))
               ;; Load system CA certificates. Raising rather than warning:
               ;; SSL_VERIFY_PEER is set two lines down, so with no trust
               ;; anchors every handshake fails anyway — the old warning was
               ;; already fail-closed, it just made the operator derive that
               ;; from one startup line and an unrelated-looking stream of
               ;; handshake errors afterwards. This is init refusing to hand
               ;; back a context that cannot do the one thing it is for,
               ;; which is what the two checks above it already do.
               ;;
               ;; Safe to raise here because ENSURE-SSL-CTX is called from
               ;; TLS-CONNECT, not at startup: a plain-HTTP server on a
               ;; distroless image still boots, and only an actual HTTPS
               ;; fetch — which was going to fail regardless — now says why.
               (when (zerop (%ssl-ctx-set-default-verify-paths ctx))
                 (error "tls: no system CA certificates found, so every HTTPS ~
                         fetch would fail certificate verification. Install a ~
                         CA bundle (ca-certificates), or point SSL_CERT_FILE / ~
                         SSL_CERT_DIR at one."))
               ;; Enable peer certificate verification
               (%ssl-ctx-set-verify ctx +ssl-verify-peer+ (sb-sys:int-sap 0))
               (setf *ssl-ctx* ctx
                     ready t)
               (log-info "tls: SSL context initialized"))
          (unless ready
            (%ssl-ctx-free ctx)))))
    *ssl-ctx*))

;;; ---------------------------------------------------------------------------
;;; TLS connection lifecycle
;;; ---------------------------------------------------------------------------

(defun tls-client-ssl (hostname fd)
  "Build an SSL for an outbound client connection on FD, with SNI and
   hostname verification set for HOSTNAME. Returns the SSL pointer, or
   frees it and raises.

   One implementation, shared by the blocking TLS-CONNECT and the
   non-blocking handshake path, because these are the two settings whose
   absence a successful handshake does not report. Without SNI a
   multi-tenant upstream answers with the wrong certificate and the failure
   arrives as an opaque verify error several layers down. Without
   SSL_set1_host the chain is checked for validity but never for whose it
   is, so any certificate a trusted CA ever issued will do. Two copies of
   this would be two chances to omit one, and the omission is silent in
   both directions.

   The read-back is not ceremony. SSL_set_tlsext_host_name answers 1
   without that guaranteeing the name is what a later handshake sends, so
   asking the SSL what its servername is is the only check that separates a
   configured SNI from a call that returned 1 — the same discipline the
   min-proto-version ctrl already gets, and for the same reason."
  (let ((ctx (ensure-ssl-ctx))
        (ssl nil))
    (handler-case
        (progn
          (setf ssl (%ssl-new ctx))
          (when (sb-sys:sap= (sb-alien:alien-sap ssl) (sb-sys:int-sap 0))
            (error "SSL_new failed"))
          ;; Null-terminated: SSL_ctrl reads this with strlen.
          (let ((hostname-bytes
                  (concatenate '(simple-array (unsigned-byte 8) (*))
                               (sb-ext:string-to-octets
                                hostname :external-format :ascii)
                               #(0))))
            (sb-sys:with-pinned-objects (hostname-bytes)
              (unless (= 1 (%ssl-ctrl ssl +ssl-ctrl-set-tlsext-hostname+ 0
                                      (sb-sys:vector-sap hostname-bytes)))
                (error "SSL_set_tlsext_host_name failed for ~a" hostname))))
          (let ((sni (%ssl-get-servername ssl +tlsext-nametype-host-name+)))
            (unless (equal sni hostname)
              (error "SNI did not take: set ~s, SSL reports ~s"
                     hostname sni)))
          (when (zerop (%ssl-set1-host ssl hostname))
            (error "SSL_set1_host failed for ~a" hostname))
          (unless (= 1 (%ssl-set-fd ssl fd))
            (error "SSL_set_fd failed"))
          ssl)
      (error (e)
        (when ssl (ignore-errors (%ssl-free ssl)))
        (error "tls-client-ssl ~a: ~a" hostname e)))))

(defun ssl-handshake-stepper (ssl)
  "A handshake step function for CONNECTION-HANDSHAKE-FN. Each call runs
   SSL_connect once and answers :DONE, :WANT-READ, :WANT-WRITE, or raises.

   SSL_connect on a non-blocking socket is resumable: it is called again,
   unchanged, until it stops asking. Which direction it is blocked on is
   not the caller's to guess — a handshake sends and receives several
   times, and the direction changes between calls — so the answer carries
   it and the state machine arms what it is told.

   Only the two WANT codes are continuable. Anything else is a failed
   handshake, and treating one as retryable would spin the event loop on a
   connection that is never going to complete."
  (lambda ()
    (let ((result (%ssl-connect ssl)))
      (if (= result 1)
          :done
          (let ((err (%ssl-get-error ssl result)))
            (cond
              ((= err +ssl-error-want-read+)  :want-read)
              ((= err +ssl-error-want-write+) :want-write)
              (t (error "SSL_connect failed: error ~d" err))))))))

(defun tls-connect (hostname port)
  "Open a blocking TLS connection to HOSTNAME:PORT.
   Returns (values ssl-ptr socket) on success. DNS resolution and the
   TCP connect are both bounded by *FETCH-TIMEOUT*; DNS goes through
   the shared *DNS-RESOLVE-BLOCKING-FN* getent resolver (same one the
   async HTTP path uses) so both v4 and v6 addresses are handled and
   there is exactly one DNS primitive in the framework."
  ;; No CTX binding here any more: TLS-CLIENT-SSL calls ENSURE-SSL-CTX,
  ;; which is idempotent and mutex-guarded, so touching it twice would
  ;; only be a second place to get the ordering wrong.
  (let ((ssl nil)
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
            (setf ssl (tls-client-ssl
                       hostname
                       (sb-bsd-sockets:socket-file-descriptor socket)))
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

(defun ssl-write-error-raise (ssl n)
  "Classify a non-positive SSL_write return and raise with the same
   errno discipline as SSL-READ-EOF-OR-RAISE: distinguish a
   SO_SNDTIMEO expiry (errno = EAGAIN/EWOULDBLOCK) from a real
   transport failure so operators chasing timeouts can tell them
   apart in the log. Write has no benign-EOF case — every
   non-positive return is an error."
  (let ((err (%ssl-get-error ssl n)))
    (cond
      ((= err +ssl-error-syscall+)
       (let ((errno (get-errno)))
         (cond
           ((or (= errno +eagain+) (= errno +ewouldblock+))
            (error "SSL_write: timed out (~a)" (errno-string errno)))
           (t
            (error "SSL_write: transport error ~a" (errno-string errno))))))
      (t (error "SSL_write failed: error ~d" err)))))

(defun tls-write-all (ssl bytes)
  "Write all BYTES through the SSL connection. Blocks until complete.
   Surfaces SO_SNDTIMEO as a distinct error from transport failures
   via SSL-WRITE-ERROR-RAISE — symmetric with SSL-READ-EOF-OR-RAISE
   on the read path."
  (let ((pos 0)
        (len (length bytes)))
    (loop while (< pos len)
          do (sb-sys:with-pinned-objects (bytes)
               (let ((n (%ssl-write ssl
                                    (sb-sys:sap+ (sb-sys:vector-sap bytes) pos)
                                    (- len pos))))
                 (when (<= n 0)
                   (ssl-write-error-raise ssl n))
                 (incf pos n))))))

(defun ssl-read-eof-or-raise (ssl n &optional (err (%ssl-get-error ssl n)))
  "Classify a non-positive SSL_read return. Returns :EOF if the peer
   cleanly closed the stream, raises otherwise so the outer
   handler-case converts the error into a 502 and fires the fetch
   callback's cleanup sentinel.

   ERR defaults to asking SSL_get_error here, and is passed in by a caller
   that has already asked. SSL_get_error may consult errno, so the answer
   belongs to the SSL_read it followed; taking it as an argument keeps a
   caller that needed to branch on WANT_READ first from having to ask a
   second time and hope nothing moved in between.

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
  (progn
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

(defun tls-read-all (ssl &key (method :GET))
  "Read the HTTP response through the SSL connection and return it as a
   byte vector. Bounded by *MAX-OUTBOUND-RESPONSE-SIZE* (headers + body
   together) — the inbound *MAX-BODY-SIZE* cap is the wrong knob here,
   since a legitimate 1 MB HTTPS response with a few hundred bytes of
   headers exceeds the inbound-request budget on principle.

   Stops as soon as the response is framed-complete, via the same
   OUTBOUND-RESPONSE-COMPLETE-P the non-blocking plain-HTTP path uses:
   Content-Length satisfied, chunked terminator seen, or (for HEAD)
   headers done. METHOD is the request method, needed for that last case.

   Reading to EOF unconditionally — which this did — worked only because
   BUILD-OUTBOUND-REQUEST sends Connection: close by default, and it was
   never free:

     * It cost a round trip on *every* HTTPS fetch. The complete response
       is already in hand; we were waiting for the peer's close_notify to
       tell us something the framing had already said.
     * An upstream that keeps the connection open — because the caller
       passed its own Connection header — pinned this worker thread until
       SO_RCVTIMEO fired (*FETCH-TIMEOUT*, 30s by default). HTTPS fetch is
       blocking, so that is a worker, not merely a parked connection.
     * It left the two transports disagreeing about when a response ends:
       plain HTTP recognized the chunked terminator, TLS did not. The same
       upstream behaved differently over http:// and https://.

   Close-delimited responses (no Content-Length, no Transfer-Encoding)
   still read to EOF, because for those EOF genuinely is the framing."
  (let* ((cap 8192)
         (out (make-array cap :element-type '(unsigned-byte 8)))
         (len 0)
         (chunk-scan 0)
         (buf (make-array 8192 :element-type '(unsigned-byte 8))))
    (loop
      ;; Framed-complete? Ask before reading again, so a response whose
      ;; last byte arrived on the previous pass does not wait on a read
      ;; that has nothing left to deliver.
      (multiple-value-bind (complete next-scan)
          (outbound-response-complete-p out len method chunk-scan)
        (setf chunk-scan next-scan)
        (when complete (return)))
      (sb-sys:with-pinned-objects (buf)
        (let ((n (%ssl-read ssl (sb-sys:vector-sap buf) (length buf))))
          (cond
            ((> n 0)
             (when (> (+ len n) *max-outbound-response-size*)
               (error "HTTPS response too large (~d bytes, max ~d)"
                      (+ len n) *max-outbound-response-size*))
             ;; Grow geometrically and copy in one REPLACE — a
             ;; VECTOR-PUSH-EXTEND per byte would dominate the read.
             (when (> (+ len n) cap)
               (loop while (< cap (+ len n)) do (setf cap (* cap 2)))
               (let ((bigger (make-array cap :element-type '(unsigned-byte 8))))
                 (replace bigger out :end2 len)
                 (setf out bigger)))
             (replace out buf :start1 len :end2 n)
             (incf len n))
            (t
             ;; :EOF (benign close) or a raise — SSL-READ-EOF-OR-RAISE
             ;; decides which, and a benign EOF is what completes a
             ;; close-delimited response.
             (ssl-read-eof-or-raise ssl n)
             (return))))))
    (subseq out 0 len)))

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
                (let* ((response-buf (tls-read-all ssl :method method))
                       (buf-len (length response-buf))
                       ;; Step over any 1xx interim blocks (RFC 7231 §6.2)
                       ;; before locating the header boundary — a CDN's
                       ;; unsolicited 103 Early Hints would otherwise supply
                       ;; the status and headers this fetch reports, and the
                       ;; real response would be dropped without a word.
                       ;; Same helper the plain-HTTP path uses, so the two
                       ;; transports cannot drift on where a response starts.
                       (start (skip-interim-responses response-buf 0 buf-len))
                       (header-end (scan-crlf-crlf response-buf start buf-len)))
                  (unless header-end
                    (error "https: upstream response has no parseable headers"))
                  (let* ((status (parse-response-status response-buf start buf-len))
                         (headers
                          (let ((first-crlf (scan-crlf response-buf start header-end)))
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
                                                             header-end start))
                         (content-length (unless te-present
                                           (scan-content-length response-buf
                                                                header-end start))))
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
                    ;; Skipped for 204/304 (carry CL but MUST NOT
                    ;; have a body per RFC 7230 §3.3.3 rule 1 / RFC 7232
                    ;; §4.1) and for HEAD (RFC 7231 §4.3.2 — upstream
                    ;; echoes the GET-body CL but MUST NOT send a body).
                    ;; Twin of the exemption in fetch.lisp COMPLETE-FETCH
                    ;; on the plain-HTTP path, 1xx included: the skip
                    ;; above has consumed every complete interim, so
                    ;; STATUS is >= 200 here and testing for 1xx would be
                    ;; dead code implying an interim could be final.
                    (when (and content-length
                               (not (or (= status 204) (= status 304)))
                               (not (eq method :HEAD))
                               (< (- buf-len body-start) content-length))
                      (error "https: short body (~d of ~d bytes)"
                             (- buf-len body-start) content-length))
                    (let* ((body-end (if content-length
                                         (min buf-len (+ body-start content-length))
                                         buf-len))
                           ;; 204/304 MUST NOT have a body (RFC 7230 §3.3.3 rule 1).
                           ;; HEAD MUST NOT include a body (RFC 7231 §4.3.2).
                           ;; Force empty regardless of what the upstream sent.
                           ;; 1xx cannot reach here — see the guard above.
                           (body-end (if (or (= status 204) (= status 304)
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
                      ;; Sync close-after-p from the callback's response
                      ;; before format-response — a handler-set
                      ;; Connection: close should zero out the hint too.
                      (sync-close-after-p-from-response conn response)
                      ;; Deliver to inbound connection. :HEAD-ONLY-P
                      ;; short-circuits the body encode on HEAD (matches
                      ;; the plain COMPLETE-FETCH path); byte-vector
                      ;; responses still strip post-serialize.
                      (let* ((head-p (and (connection-request conn)
                                          (eq (http-request-method
                                               (connection-request conn))
                                              :HEAD)))
                             (bytes (cond
                                      ((typep response '(simple-array (unsigned-byte 8) (*)))
                                       (strip-body-for-head response conn))
                                      ((typep response 'http-fetch-continuation)
                                       ;; Chained fetch
                                       (initiate-fetch conn epoll-fd response)
                                       (return-from https-fetch))
                                      (t (format-response
                                          response
                                          :connection-hint
                                          (connection-hint-for conn)
                                          :head-only-p head-p)))))
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
                         (format-response
                          (make-error-response 502)
                          :connection-hint (connection-hint-for conn))
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
  "Blocking streaming HTTPS fetch. Calls ON-LINE per response body line.

   The response is read by STREAM-RESPONSE-LINES, the same function the
   plain-HTTP path uses, with SSL-BYTE-READER supplying the bytes. This
   used to be TLS-STREAM-RESPONSE: 386 lines reimplementing interim 1xx
   handling, header caps, chunk framing, :HEAD gating and three
   truncation disciplines the shared reader already had. Two readers of
   the same bytes is the shape this codebase spends the most effort
   refusing, and the largest instance of it was here."
  (multiple-value-bind (ssl socket)
      (tls-connect host port)
    (unwind-protect
        (progn
          (tls-write-all ssl (build-outbound-request method host path
                                                     :scheme :https :port port
                                                     :headers headers :body body))
          (stream-response-lines nil on-line
                                 :method method
                                 :read-fn (ssl-byte-reader ssl)))
      (tls-close ssl socket))))

(defun ssl-connection-reader (ssl)
  "A CONNECTION read-fn over SSL. Fills BUFFER[START..START+MAX-BYTES) and
   answers NB-READ's contract: bytes read, :AGAIN, :EOF, or a raise.

   WANT_READ becomes :AGAIN, and that one mapping is what makes
   CONNECTION-READ-AVAILABLE's existing drain loop enforce the SSL_pending
   discipline for free. SSL_read hands back whatever it has already
   decrypted before it goes near the socket and returns at most one record
   per call, so a loop that runs until :AGAIN empties OpenSSL's buffer as
   well as the kernel's. Stopping earlier leaves decrypted bytes in user
   space with nothing left on the fd, and an edge-triggered epoll has no
   reason to wake again — the connection hangs holding its own answer.

   WANT_WRITE is deliberately not :AGAIN. It means the SSL wants to send
   before it can read — a renegotiation or a post-handshake message — and
   what it is asking for is another SSL_READ once the socket is *writable*.
   The event loop outside :OUT-HANDSHAKE is built on readable-means-read,
   so there is nowhere to put that request; answering :AGAIN would park the
   connection waiting for a readability event that is not coming, turning a
   condition we can name into a hang we cannot. Raising is the honest
   answer until the direction inversion lands.

   Everything else defers to SSL-READ-EOF-OR-RAISE, with the error code
   passed along, so the four-way reading of SSL_ERROR_SYSCALL stays in one
   place and this function cannot drift from the blocking path's idea of
   what a clean end of stream is."
  (lambda (buffer start max-bytes)
    (sb-sys:with-pinned-objects (buffer)
      (let ((n (%ssl-read ssl
                          (sb-sys:sap+ (sb-sys:vector-sap buffer) start)
                          max-bytes)))
        (if (> n 0)
            n
            (let ((err (%ssl-get-error ssl n)))
              (cond
                ((= err +ssl-error-want-read+) :again)
                ((= err +ssl-error-want-write+)
                 (error "SSL_read returned WANT_WRITE: the retry it wants ~
                         is another SSL_read once the socket is writable, ~
                         which this state machine cannot express. See ~
                         SSL-CONNECTION-READER."))
                (t (ssl-read-eof-or-raise ssl n err)))))))))

(defun ssl-byte-reader (ssl)
  "Byte source over SSL for STREAM-READER: fill BUF, answer with the
   count, :EOF on a benign close, or raise.

   The classification lives here rather than at the call site because a
   caller supplying its own source has no SSL pointer to hand
   SSL-READ-EOF-OR-RAISE — and that function's four-way reading of
   SSL_ERROR_SYSCALL is not something a second site should restate.

   The raise is load-bearing rather than incidental: READER-FILL treats
   only 0 as end of stream, so answering :EOF for a reset mid-body would
   deliver a truncated response as a clean one."
  (lambda (buf len)
    (sb-sys:with-pinned-objects (buf)
      (let ((n (%ssl-read ssl (sb-sys:vector-sap buf) len)))
        (if (> n 0)
            n
            ;; Returns :EOF for a benign close, raises for everything
            ;; else. SSL_ERROR_SYSCALL conflates four conditions and only
            ;; one of them is an ordinary end of stream.
            (ssl-read-eof-or-raise ssl n))))))

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
