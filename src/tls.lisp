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
  (handler-case
      (progn
        (sb-alien:load-shared-object "libcrypto.so" :dont-save t)
        (sb-alien:load-shared-object "libssl.so" :dont-save t))
    (error (e)
      (error "web-skeleton-tls: failed to load libssl — ~a~%~
              Install libssl-dev (Debian/Ubuntu), openssl-devel (RHEL), ~
              or openssl-dev (Alpine)." e))))

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
  "Create the shared SSL_CTX if not already created. Thread-safe."
  (or *ssl-ctx*
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
        *ssl-ctx*)))

;;; ---------------------------------------------------------------------------
;;; TLS connection lifecycle
;;; ---------------------------------------------------------------------------

(defun tls-connect (hostname port)
  "Open a blocking TLS connection to HOSTNAME:PORT.
   Returns (values ssl-ptr socket) on success."
  (let* ((ctx (ensure-ssl-ctx))
         (socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp))
         (ssl nil))
    (handler-case
        (progn
          ;; Blocking TCP connect
          (let ((addr (sb-bsd-sockets:host-ent-address
                       (sb-bsd-sockets:get-host-by-name hostname))))
            (sb-bsd-sockets:socket-connect socket addr port))
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
        (error "tls-connect ~a:~d failed: ~a" hostname port e)))))

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
   Returns the raw response as a byte vector."
  (let ((chunks nil)
        (total 0)
        (buf (make-array 8192 :element-type '(unsigned-byte 8))))
    (loop
      (sb-sys:with-pinned-objects (buf)
        (let ((n (%ssl-read ssl (sb-sys:vector-sap buf) (length buf))))
          (cond
            ((> n 0)
             (when (> (+ total n) *max-body-size*)
               (error "HTTPS response too large (~d bytes, max ~d)"
                      (+ total n) *max-body-size*))
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
   Called by initiate-fetch when the URL scheme is :https."
  (handler-case
      (multiple-value-bind (ssl socket)
          (tls-connect host port)
        (unwind-protect
            (progn
              ;; Build and send the HTTP request
              (let ((request-bytes (build-outbound-request
                                   (http-fetch-request-method fetch-req)
                                   host path
                                   :scheme :https :port port
                                   :headers (http-fetch-request-headers fetch-req)
                                   :body (http-fetch-request-body fetch-req))))
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
                                       (scan-content-length response-buf header-end)))
                     (body-end (if (and body-start content-length)
                                   (min buf-len (+ body-start content-length))
                                   buf-len))
                     (raw-body (when (and body-start (> body-end body-start))
                                 (subseq response-buf body-start body-end)))
                     (body (if (and raw-body chunked-p)
                               (decode-chunked-body raw-body 0 (length raw-body))
                               raw-body))
                     (callback (http-fetch-request-callback fetch-req)))
                ;; Call the user's callback
                (let ((response (funcall callback
                                         (or status 0) (or headers nil)
                                         (or body nil))))
                  ;; Deliver to inbound connection
                  (let ((bytes (cond
                                 ((typep response '(simple-array (unsigned-byte 8) (*)))
                                  response)
                                 ((typep response 'http-fetch-request)
                                  ;; Chained fetch
                                  (initiate-fetch conn epoll-fd response)
                                  (return-from https-fetch))
                                 (t (format-response response)))))
                    (connection-queue-write conn bytes)
                    (setf (connection-state conn) :write-response)
                    (epoll-modify epoll-fd (connection-fd conn)
                                 (logior +epollout+ +epollet+))
                    (log-debug "fetch: https ~a:~d~a -> fd ~d"
                               host port path (connection-fd conn))))))
          (tls-close ssl socket)))
    (error (e)
      (log-error "https fetch failed: ~a" e)
      (let ((err-bytes (format-response (make-error-response 502))))
        (connection-queue-write conn err-bytes)
        (setf (connection-state conn) :write-response)
        (epoll-modify epoll-fd (connection-fd conn)
                     (logior +epollout+ +epollet+))))))

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
                                                            :end1 18)
                                              (search "chunked" line :start2 18 :test #'char-equal))
                                     (setf chunked t))))))
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
    (or status 0)))

;;; ---------------------------------------------------------------------------
;;; Registration — hook into the core framework
;;; ---------------------------------------------------------------------------

(eval-when (:load-toplevel :execute)
  (setf *https-fetch-fn* #'https-fetch)
  (setf *https-stream-fn* #'https-fetch-stream)
  (log-info "tls: HTTPS fetch enabled"))
