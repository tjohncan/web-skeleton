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
      (%openssl-init-ssl 0 (sb-sys:int-sap 0))
      ;; Create context with modern TLS client method
      (let ((ctx (%ssl-ctx-new (%tls-client-method))))
        (when (sb-sys:sap= (sb-alien:alien-sap ctx) (sb-sys:int-sap 0))
          (error "SSL_CTX_new failed"))
        ;; Require TLS 1.2+ (RFC 8996 deprecates 1.0/1.1)
        (%ssl-ctrl ctx +ssl-ctrl-set-min-proto-version+
                   +tls1-2-version+ (sb-sys:int-sap 0))
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
            ;; Set SNI hostname (must be null-terminated — SSL_ctrl uses strlen)
            (let ((hostname-bytes (concatenate '(simple-array (unsigned-byte 8) (*))
                                                (sb-ext:string-to-octets hostname
                                                                         :external-format :ascii)
                                                #(0))))
              (sb-sys:with-pinned-objects (hostname-bytes)
                (%ssl-ctrl ssl +ssl-ctrl-set-tlsext-hostname+ 0
                           (sb-sys:vector-sap hostname-bytes))))
            ;; Enable hostname verification (OpenSSL 1.1.0+)
            (when (zerop (%ssl-set1-host ssl hostname))
              (error "SSL_set1_host failed"))
            ;; Attach to socket fd
            (%ssl-set-fd ssl (sb-bsd-sockets:socket-file-descriptor socket))
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
            ((zerop n) (return))  ; clean shutdown
            (t
             (let ((err (%ssl-get-error ssl n)))
               ;; 6 = SSL_ERROR_ZERO_RETURN (peer closed)
               ;; 5 = SSL_ERROR_SYSCALL (may be EOF)
               (when (or (= err +ssl-error-zero-return+)
                         (= err +ssl-error-syscall+))
                 (return))
               (error "SSL_read failed: error ~d" err)))))))
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
              (progn
                ;; Build and send the HTTP request
                (let ((request-bytes (build-outbound-request
                                     (http-fetch-continuation-method fetch-req)
                                     host path
                                     :scheme :https :port port
                                     :headers (http-fetch-continuation-headers fetch-req)
                                     :body (http-fetch-continuation-body fetch-req))))
                  (tls-write-all ssl request-bytes))
                ;; Read the complete response
                (let* ((response-buf (tls-read-all ssl))
                       (buf-len (length response-buf))
                       (header-end (scan-crlf-crlf response-buf 0 buf-len))
                       (status (when header-end
                                 (parse-response-status response-buf 0 buf-len)))
                       (headers (when header-end
                                  (let ((first-crlf (scan-crlf response-buf 0
                                                               header-end)))
                                    (when first-crlf
                                      (parse-headers-bytes response-buf
                                                           (+ first-crlf 2)
                                                           (+ header-end 4))))))
                       (body-start (when header-end (+ header-end 4)))
                       ;; RFC 7230 §3.3.3: TE takes precedence over CL
                       (chunked-p (response-chunked-p headers))
                       (content-length (when (and header-end (not chunked-p))
                                         (scan-content-length response-buf header-end))))
                  ;; Truncation guard: an upstream that declares a
                  ;; Content-Length and then closes short must not be
                  ;; allowed to hand us a silently-truncated body. The
                  ;; MITM case is the nasty one — attacker RSTs
                  ;; mid-stream and the app receives short data with no
                  ;; indication. Signal an error so the outer
                  ;; handler-case converts it into a 502 and fires
                  ;; the cleanup sentinel.
                  (when (and content-length body-start
                             (< (- buf-len body-start) content-length))
                    (error "https: short body (~d of ~d bytes)"
                           (- buf-len body-start) content-length))
                  (let* ((body-end (if (and body-start content-length)
                                       (min buf-len (+ body-start content-length))
                                       buf-len))
                         (raw-body (when (and body-start (> body-end body-start))
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
                                             (or status 0) (or headers nil)
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
                        (connection-queue-write conn bytes)
                        (setf (connection-state conn) :write-response
                              (connection-last-active conn) (get-universal-time))
                        (epoll-modify epoll-fd (connection-fd conn)
                                      (logior +epollout+ +epollet+))
                        (log-debug "fetch: https ~a:~d~a -> fd ~d"
                                   host port path (connection-fd conn)))))))
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
        (let ((err-bytes (format-response (make-error-response 502))))
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
          (tls-stream-response ssl on-line))
      (tls-close ssl socket))))

(defun tls-stream-response (ssl on-line)
  "Read HTTP response via SSL, skip headers, call ON-LINE per body line.
   Handles chunked transfer encoding. Returns the status code."
  (let ((buf (make-array 8192 :element-type '(unsigned-byte 8)))
        (line-buf (make-array 4096 :element-type '(unsigned-byte 8)
                                   :fill-pointer 0 :adjustable t))
        (status nil)
        (chunked nil)
        (in-headers t)
        (first-line t)
        ;; Chunked state
        (in-chunk-size nil)
        (chunk-remaining 0)
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
        (sb-sys:with-pinned-objects (buf)
          (let ((n (%ssl-read ssl (sb-sys:vector-sap buf) (length buf))))
            (when (<= n 0) (return))
            (loop for i from 0 below n
                  for byte = (aref buf i)
                  do (when (> (fill-pointer line-buf) *max-body-size*)
                       (error "streaming response line too large"))
                     (cond
                       ;; Header phase
                       (in-headers
                        (cond
                          ((= byte 10)
                           (let ((line (emit-line)))
                             (if (zerop (length line))
                                 (progn
                                   (setf in-headers nil)
                                   (when chunked (setf in-chunk-size t)))
                                 (progn
                                   (when first-line
                                     (let ((sp (position #\Space line)))
                                       (when (and sp (< (+ sp 3) (length line)))
                                         (setf status (parse-integer line :start (1+ sp)
                                                                          :end (+ sp 4)
                                                                          :junk-allowed t))))
                                     (setf first-line nil))
                                   (when (and (>= (length line) 18)
                                              (string-equal line "transfer-encoding:"
                                                            :end1 18))
                                     (let ((value (string-trim '(#\Space #\Tab)
                                                                (subseq line 18))))
                                       (when (connection-header-has-token-p value "chunked")
                                         (setf chunked t))))))))
                          ((= byte 13) nil)
                          (t (vector-push-extend byte line-buf))))
                       ;; Chunked body — reading chunk size (separate buffer
                       ;; to avoid corrupting body content in line-buf)
                       ((and chunked in-chunk-size)
                        (cond
                          ((= byte 10)
                           ;; Skip empty lines (chunk-terminator CRLFs)
                           (when (> (fill-pointer chunk-size-buf) 0)
                             (let* ((size-str (sb-ext:octets-to-string
                                              (subseq chunk-size-buf 0
                                                      (fill-pointer chunk-size-buf))
                                              :external-format :ascii))
                                    (size (parse-integer size-str :radix 16
                                                                  :junk-allowed t)))
                               (setf (fill-pointer chunk-size-buf) 0)
                               (if (and size (> size 0))
                                   (setf chunk-remaining size
                                         in-chunk-size nil)
                                   (return)))))  ; final chunk
                          ((= byte 13) nil)
                          (t (vector-push-extend byte chunk-size-buf))))
                       ;; Chunked body — reading chunk data
                       (chunked
                        (when (> chunk-remaining 0)
                          (decf chunk-remaining)
                          (cond
                            ((= byte 10) (emit-body-line))
                            ((= byte 13) nil)
                            (t (vector-push-extend byte line-buf))))
                        (when (zerop chunk-remaining)
                          (setf in-chunk-size t)))
                       ;; Non-chunked body
                       (t
                        (cond
                          ((= byte 10) (emit-body-line))
                          ((= byte 13) nil)
                          (t (vector-push-extend byte line-buf))))))))))
    ;; Flush any remaining unterminated line
    (when (> (fill-pointer line-buf) 0)
      (when on-line
        (funcall on-line (sb-ext:octets-to-string
                          (subseq line-buf 0 (fill-pointer line-buf))
                          :external-format :utf-8))))
    (or status 0)))

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
  (let ((out (make-array 91 :element-type '(unsigned-byte 8))))
    (replace out *p256-spki-prefix*)
    (replace out x :start1 27 :end1 59)
    (replace out y :start1 59 :end1 91)
    out))

(defun der-encode-ecdsa-signature (sig-bytes)
  "DER-encode a raw 64-byte ECDSA signature (r || s, each 32 bytes
   big-endian) as an ASN.1 SEQUENCE { INTEGER r, INTEGER s }. Returns
   a fresh 70- to 72-byte vector depending on whether r or s has its
   high bit set — each such case needs a 0x00 prefix to keep the
   ASN.1 INTEGER positive."
  (let* ((r-pad (logbitp 7 (aref sig-bytes 0)))
         (s-pad (logbitp 7 (aref sig-bytes 32)))
         (r-len (if r-pad 33 32))
         (s-len (if s-pad 33 32))
         (content-len (+ 2 r-len 2 s-len))
         (out (make-array (+ 2 content-len) :element-type '(unsigned-byte 8)))
         (pos 0))
    (setf (aref out pos) #x30) (incf pos)         ; SEQUENCE tag
    (setf (aref out pos) content-len) (incf pos)  ; SEQUENCE length
    (setf (aref out pos) #x02) (incf pos)         ; INTEGER tag (r)
    (setf (aref out pos) r-len) (incf pos)        ; r length
    (when r-pad (setf (aref out pos) #x00) (incf pos))
    (replace out sig-bytes :start1 pos :start2 0 :end2 32)
    (incf pos 32)
    (setf (aref out pos) #x02) (incf pos)         ; INTEGER tag (s)
    (setf (aref out pos) s-len) (incf pos)        ; s length
    (when s-pad (setf (aref out pos) #x00) (incf pos))
    (replace out sig-bytes :start1 pos :start2 32 :end2 64)
    out))

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
  ;; reads bytes 0..63 from sig-bytes unconditionally, so an 80-byte
  ;; sig would have been silently truncated to the first 64 rather
  ;; than rejected. Match the behavior of the Lisp implementation
  ;; exactly: anything other than 64 bytes fails verification before
  ;; we touch the DER builder or libssl.
  (unless (= (length sig-bytes) 64)
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
