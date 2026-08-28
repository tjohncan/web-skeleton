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

;;; Staging a write into foreign memory is a byte copy on the write path,
;;; so it goes through libc rather than a SAP loop.
(sb-alien:define-alien-routine ("memcpy" %memcpy) (* t)
  (dest (* t))
  (src (* t))
  (n sb-alien:unsigned-long))

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
(defconstant +ssl-write-stage-size+ 16384
  "Bytes staged per SSL_write, one TLS record. Capping here costs nothing:
   CONNECTION-ON-WRITE loops until :AGAIN or empty, and OpenSSL would split
   a larger write into records of about this size anyway.")
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

(defun call-retrying-eintr (thunk)
  "Call THUNK until it answers something other than :RETRY, and return
   that answer.

   :RETRY is what the classifiers below report for EINTR: the call was
   interrupted before it moved any bytes, so re-issuing it unchanged is
   the whole of the correction. Nothing was lost and nothing is done
   twice.

   Unbounded on purpose. Every call THUNK makes is itself bounded by
   SO_RCVTIMEO / SO_SNDTIMEO, so a peer that has gone quiet still fails
   on time; only an unending stream of signals could extend it, and a
   counter here would answer that with a different wrong answer rather
   than a right one.

   The caller pins across this loop, not inside THUNK. OpenSSL requires a
   repeated call to present the same address and length, and a GC landing
   between the interrupted call and its retry would move the vector out
   from under exactly that requirement."
  (loop
    (let ((verdict (funcall thunk)))
      (unless (eq verdict :retry)
        (return verdict)))))

(defun ssl-write-retry-or-raise (ssl n &optional
                                       (errno (get-errno))
                                       (err (%ssl-get-error ssl n)))
  "Classify a non-positive SSL_write return: :RETRY if the call was
   interrupted before it moved any bytes, and otherwise raise, with the
   same errno discipline as SSL-READ-EOF-OR-RAISE — a SO_SNDTIMEO expiry
   (errno = EAGAIN/EWOULDBLOCK) told apart from a real transport failure
   so operators chasing timeouts can tell them apart in the log. Write
   has no benign-EOF case: every non-positive return is an error or a
   retry.

   ERRNO before ERR, and the ordering is load-bearing: &OPTIONAL defaults
   evaluate left to right, so errno is taken before SSL_get_error runs.
   SSL_get_error is a foreign call and can set errno itself, so reading it
   afterwards reports what that call did rather than what SSL_write did --
   and errno is the entire basis of the split below."
  (cond
    ((= err +ssl-error-syscall+)
     (cond
       ((= errno +eintr+) :retry)
       ((or (= errno +eagain+) (= errno +ewouldblock+))
        (error "SSL_write: timed out (~a)" (errno-string errno)))
       (t
        (error "SSL_write: transport error ~a" (errno-string errno)))))
    (t (error "SSL_write failed: error ~d" err))))

(defun tls-write-all (ssl bytes)
  "Write all BYTES through the SSL connection. Blocks until complete.
   Surfaces SO_SNDTIMEO as a distinct error from transport failures
   via SSL-WRITE-ERROR-RAISE — symmetric with SSL-READ-EOF-OR-RAISE
   on the read path."
  (let ((pos 0)
        (len (length bytes)))
    (loop while (< pos len)
          do (incf pos
                   ;; Pinned across the retry loop rather than across one
                   ;; call. A retry must present the same address, and a GC
                   ;; between the interrupted call and its retry would move
                   ;; BYTES. A *partial* write is not a retry — it advances
                   ;; POS and issues a fresh call — which is why the pin can
                   ;; end and restart at that boundary and not inside one.
                   (sb-sys:with-pinned-objects (bytes)
                     (call-retrying-eintr
                      (lambda ()
                        (let ((n (%ssl-write
                                  ssl
                                  (sb-sys:sap+ (sb-sys:vector-sap bytes) pos)
                                  (- len pos))))
                          (if (> n 0)
                              n
                              (ssl-write-retry-or-raise ssl n))))))))))

(defun ssl-read-eof-or-raise (ssl n &optional
                                      (errno (get-errno))
                                      (err (%ssl-get-error ssl n)))
  "Classify a non-positive SSL_read return. Returns :EOF if the peer
   cleanly closed the stream, raises otherwise so the outer
   handler-case converts the error into a 502 and fires the fetch
   callback's cleanup sentinel.

   ERRNO is declared before ERR and that ordering is the point, not a
   style: &OPTIONAL defaults evaluate left to right, so errno is taken
   before SSL_get_error is called. SSL_get_error is a foreign call and can
   set errno itself, so reading errno afterwards reports what *it* did
   rather than what SSL_read did. That is not hypothetical -- it is how a
   plain EAGAIN came back from this codebase reading as EBADF, and errno
   is the whole basis of the SSL_ERROR_SYSCALL split below.

   Both are passed in by a caller that already sampled them, which is what
   a caller must do if it needed to branch on WANT_READ first.

   ARGUMENT ORDER IS A TRAP, and it belongs up here rather than at the
   bottom because the next caller added is where it costs something.
   ERRNO sits ahead of ERR. A caller written against the older shape and
   passing ERR positionally now passes it as ERRNO, silently — and errno
   is the whole basis of the split below, where a wrong value turns a
   transport failure into a clean end of stream. Pass both or neither.

   :AGAIN IS THE CALLER'S TO INTERPRET, and that is the re-derivation this
   function needed once the socket stopped being blocking. EAGAIN used to
   mean exactly one thing here — SO_RCVTIMEO fired — and the error message
   said so. SO_RCVTIMEO does nothing on a non-blocking socket, so there
   EAGAIN means only what it says: nothing to read yet. On a blocking
   socket with the timeout installed it still cannot mean anything else,
   because a blocking read does not return would-block unless the receive
   timeout expired.

   One classification, two readings, each made where the socket's mode is
   known: SSL-CONNECTION-READER passes :AGAIN to the event loop, and
   SSL-BLOCKING-READ-EOF-OR-RAISE turns it into the loud timeout
   DEPLOYMENT.md promises. Deciding it here would mean guessing at a fact
   this function cannot see.

   SSL_ERROR_SYSCALL still conflates several conditions and still must NOT
   be read uniformly as clean EOF:
     errno = 0                  — end of stream with no close_notify.
                                  Benign, and load-bearing: it is the
                                  framing signal for HTTP/1.0-style
                                  servers that never send one.
     errno = EAGAIN/EWOULDBLOCK — would block. :AGAIN, per above.
     errno = EINTR              — a signal arrived before the call moved
                                  any bytes. Nothing is wrong with the
                                  connection and nothing was lost, so the
                                  correction is to issue it again:
                                  :RETRY. CALL-RETRYING-EINTR is the loop,
                                  and the pin belongs outside it.
     errno = anything else      — real transport failure: ECONNRESET,
                                  EPIPE, ETIMEDOUT. This is the MITM
                                  RST-mid-stream case, where an attacker
                                  truncates a response and a silent EOF
                                  here delivers it as success. Loud
                                  raise, always.

   EINTR IS REACHABLE, and not by the route a reader will assume. It is
   *un*reachable on the event loop's sockets: those are non-blocking, so
   a read answers EAGAIN rather than entering the interruptible sleep
   where EINTR is generated. The blocking fetch path is three things at
   once. BLOCKING-CONNECT restores blocking mode deliberately, so the
   call can sleep. SET-SOCKET-TIMEOUT installs SO_RCVTIMEO so
   *FETCH-TIMEOUT* bounds it, and per signal(7) a blocking socket call
   carrying a receive timeout fails with EINTR when interrupted
   *regardless of SA_RESTART* — the handler flag that would otherwise
   restart it does not apply. And the framework supplies the signal
   itself: DNS resolution spawns a getent child per lookup, so SIGCHLD is
   ordinary on a path that has just resolved a name.

   Blocking socket, receive timeout installed, and a signal this process
   generates for itself. Do not delete this arm on the grounds that
   non-blocking sockets never see EINTR: that is true, and is not where
   this one comes from.

   WANT_READ and WANT_WRITE reaching here is a caller bug: both are
   continuable and belong to whoever knows how to continue them."
  (cond
    ((= err +ssl-error-zero-return+) :eof)
    ((= err +ssl-error-syscall+)
     (cond
       ((zerop errno) :eof)
       ((or (= errno +eagain+) (= errno +ewouldblock+)) :again)
       ((= errno +eintr+) :retry)
       (t (error "SSL_read: transport error ~a" (errno-string errno)))))
    ((or (= err +ssl-error-want-read+) (= err +ssl-error-want-write+))
     (error "SSL_read: ~a reached the classifier; it is continuable and ~
             belongs to the caller that knows how to continue it"
            (if (= err +ssl-error-want-read+) "WANT_READ" "WANT_WRITE")))
    (t (error "SSL_read failed: error ~d" err))))

(defun ssl-blocking-read-eof-or-raise (ssl n &optional
                                            (errno (get-errno))
                                            (err (%ssl-get-error ssl n)))
  "SSL-READ-EOF-OR-RAISE for a socket still in blocking mode with
   SO_RCVTIMEO installed.

   The only difference is what :AGAIN means there, and it is not a
   difference of degree: a blocking read does not return would-block
   unless the receive timeout expired, so :AGAIN is the timeout and gets
   the loud error DEPLOYMENT.md promises. Silence here made that promise
   a lie once, for close-delimited HTTPS responses and for
   http-fetch-stream over HTTPS.

   :RETRY passes straight through. An interrupted call means the same
   thing on either kind of socket, and the caller's CALL-RETRYING-EINTR
   is what acts on it. Only :AGAIN reads differently here.

   A wrapper rather than a flag on the classifier: that one answers what
   the transport reported, this one answers what it means on this kind of
   socket, and neither has to know the other's business."
  (let ((verdict (ssl-read-eof-or-raise ssl n errno err)))
    (if (eq verdict :again)
        (error "SSL_read: timed out (~a)" (errno-string errno))
        verdict)))

(defun tls-close (ssl socket)
  "Shut down a TLS connection and close the socket."
  (ignore-errors (%ssl-shutdown ssl))
  (ignore-errors (%ssl-free ssl))
  (ignore-errors (sb-bsd-sockets:socket-close socket)))

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

(defun tls-setup-outbound (conn host)
  "Install the TLS transport on an outbound CONN. Called by INITIATE-FETCH
   once the socket exists and the non-blocking connect is under way, before
   the request is queued or epoll is armed.

   This is the function items 3 through 8 were built for. Everything it
   installs is a closure over one SSL: a handshake step, a byte source, a
   byte sink, and a release. Nothing about the outbound state machine knows
   that TLS exists — it asks a connection to read, to write, to take one
   more handshake step, and to let go.

   HOST rather than the connection's peer address, because the certificate
   is checked against the name the caller asked for. Checking it against
   the address DNS produced would verify that whoever answered holds a
   certificate for themselves, which is not a check.

   CLOSE-FN releases in the order the peer needs: the staging buffer, then
   a best-effort close_notify, then the SSL. CONNECTION-CLOSE runs it
   before the descriptor goes, which is what makes the shutdown possible at
   all — it has nothing to send over afterwards. Best-effort because a peer
   that has already vanished must not stop us freeing anything."
  (let ((ssl (tls-client-ssl host (connection-fd conn))))
    (multiple-value-bind (write-fn release-staging) (ssl-connection-writer ssl)
      (setf (connection-handshake-fn conn) (ssl-handshake-stepper ssl)
            (connection-read-fn conn)      (ssl-connection-reader ssl)
            (connection-write-fn conn)     write-fn
            (connection-close-fn conn)
            (lambda ()
              (funcall release-staging)
              (ignore-errors (%ssl-shutdown ssl))
              (%ssl-free ssl))))
    conn))

(defun ssl-connection-writer (ssl)
  "Returns (values WRITE-FN RELEASE-FN) for a CONNECTION over SSL.

   WRITE-FN answers NB-WRITE's contract: bytes written, :AGAIN, or a raise.

   The bytes are copied into a malloc'd staging buffer and SSL_write is
   issued from there, never from the Lisp vector. OpenSSL requires that a
   write retried after WANT_WRITE present the same address and length as
   the call that failed, and SB-SYS:WITH-PINNED-OBJECTS pins only for its
   own dynamic extent — a partial write returns to the event loop, the pin
   is gone, and a GC before the retry may move the vector. Foreign memory
   does not move, so the requirement is met by where the bytes live rather
   than by anything the caller has to keep true.

   The alternative was SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER, and it was
   rejected on a rule rather than a preference: it would make TLS write
   correctness depend on the write queue never mutating what it holds — an
   invariant that lives in connection.lisp, is documented there for an
   entirely different reason (static responses share one vector across
   connections), and would be broken by anyone who later decided to compact
   the queue as an optimisation. They would break static serving and TLS
   writes together, and only one of those has a comment warning them.

   RELEASE-FN frees the staging buffer and is idempotent. It belongs on the
   connection's CLOSE-FN; a seam that allocates without a matching release
   is a leak by construction.

   WANT_READ from a write is refused for the same reason WANT_WRITE is
   refused from a read: the retry it asks for is another SSL_write once the
   socket is *readable*, and the event loop outside :OUT-HANDSHAKE cannot
   express that."
  (let ((stage (sb-alien:make-alien (sb-alien:unsigned 8)
                                    +ssl-write-stage-size+))
        (pending 0))
    (values
     (lambda (buffer start nbytes)
       (let ((n (min nbytes +ssl-write-stage-size+)))
         ;; A retry must be the same call. The queue head does not move
         ;; while a write is outstanding, so a mismatch means something
         ;; changed the plan mid-write and OpenSSL would reject it anyway —
         ;; better to say which invariant broke than to hand it on.
         (when (and (plusp pending) (/= n pending))
           (error "SSL_write retry changed length: staged ~d, now asked ~d"
                  pending n))
         (sb-sys:with-pinned-objects (buffer)
           (%memcpy (sb-alien:alien-sap stage)
                    (sb-sys:sap+ (sb-sys:vector-sap buffer) start)
                    n))
         (let* ((w (%ssl-write ssl (sb-alien:alien-sap stage) n))
                ;; Sampled here, before SSL_get_error. errno belongs to the
                ;; call that just returned, and SSL_get_error is itself a
                ;; foreign call that may set it — reading errno afterwards
                ;; reports whatever that did, which is how a plain EAGAIN
                ;; came back looking like EBADF.
                (errno (if (> w 0) 0 (get-errno))))
           (if (> w 0)
               (progn (setf pending 0) w)
               (let ((err (%ssl-get-error ssl w)))
                 (cond
                   ((= err +ssl-error-want-write+) (setf pending n) :again)
                   ;; Mirror of the read side: arm readability, re-issue
                   ;; the write.
                   ((= err +ssl-error-want-read+) (setf pending n) :want-read)
                   ;; errno is in the message for the same reason the fd is
                   ;; in EPOLL-WAIT's: SSL_ERROR_SYSCALL is a pointer at the
                   ;; socket layer and says nothing on its own.
                   ;; OpenSSL reports a would-block on some paths as
                   ;; SSL_ERROR_SYSCALL with errno EAGAIN rather than as
                   ;; WANT_WRITE. It is the same condition and the same
                   ;; answer; treating it as an error made a full socket
                   ;; buffer look like a transport failure.
                   ((and (= err +ssl-error-syscall+)
                         (or (= errno +eagain+) (= errno +ewouldblock+)))
                    (setf pending n)
                    :again)
                   (t (error "SSL_write failed: error ~d (errno ~d: ~a)"
                             err errno (errno-string errno)))))))))
     (lambda ()
       (when stage
         (sb-alien:free-alien stage)
         (setf stage nil))))))

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
    ;; The retry loop is here to keep a shared classifier's contract
    ;; total, not because EINTR is expected on this path -- these sockets
    ;; are non-blocking and answer EAGAIN instead. But
    ;; SSL-READ-EOF-OR-RAISE can now answer :RETRY, and NB-READ's contract
    ;; has no such value: leaking it would reach CONNECTION-READ-AVAILABLE
    ;; as an unrecognised verdict, which is a worse failure than the raise
    ;; it replaced. Consuming it here costs one loop and cannot be wrong.
    (sb-sys:with-pinned-objects (buffer)
      (call-retrying-eintr
       (lambda ()
         (let* ((n (%ssl-read ssl
                              (sb-sys:sap+ (sb-sys:vector-sap buffer) start)
                              max-bytes))
                ;; Before SSL_get_error, which can set errno itself.
                (errno (if (> n 0) 0 (get-errno))))
           (if (> n 0)
               n
               (let ((err (%ssl-get-error ssl n)))
                 (cond
                   ((= err +ssl-error-want-read+) :again)
                   ;; The state machine can express this now: the caller
                   ;; arms writability and re-issues the read. It must be
                   ;; the read -- retrying as a write is a protocol error
                   ;; that surfaces looking like a broken peer.
                   ((= err +ssl-error-want-write+) :want-write)
                   (t (ssl-read-eof-or-raise ssl n errno err)))))))))))

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
    ;; Pinned outside the retry loop: a re-issued read must present the
    ;; same address, and this is the blocking path, which is the one that
    ;; can be interrupted.
    (sb-sys:with-pinned-objects (buf)
      (call-retrying-eintr
       (lambda ()
         (let* ((n (%ssl-read ssl (sb-sys:vector-sap buf) len))
                ;; Sampled before SSL-READ-EOF-OR-RAISE asks SSL_get_error.
                (errno (if (> n 0) 0 (get-errno))))
           (if (> n 0)
               n
               ;; :EOF for a benign close, :RETRY for an interrupted call,
               ;; a raise for everything else. SSL_ERROR_SYSCALL conflates
               ;; several conditions and only one is an ordinary end of
               ;; stream.
               (ssl-blocking-read-eof-or-raise ssl n errno))))))))

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
  ;; Swap the pure-Lisp crypto primitives for libssl-backed versions.
  ;; SHA1-LISP / SHA256-LISP / ECDSA-VERIFY-P256-LISP remain reachable
  ;; internally; TEST-PURE-LISP-CRYPTO uses them to re-verify the
  ;; pure-Lisp paths on a libssl-enabled machine. HMAC-SHA256 is not
  ;; swapped directly — it's pure-Lisp, but its internal SHA-256 calls
  ;; route through the function cell and pick up this swap for free.
  (setf (symbol-function 'sha1)              #'sha1-libssl
        (symbol-function 'sha256)            #'sha256-libssl
        (symbol-function 'ecdsa-verify-p256) #'ecdsa-verify-p256-libssl)
  (log-info "tls: crypto swapped to libssl (sha1, sha256, ecdsa-p256)")
  ;; Registration goes last, after every swap above has returned, so that
  ;; a hook being set means the whole file succeeded and not merely that
  ;; execution reached this form. TLS-LOADED-P reads *HTTPS-STREAM-FN* as
  ;; exactly that signal, and it was reading it before the swaps ran —
  ;; a raise from one of them (an OpenSSL without EVP_MD_CTX_new, say)
  ;; would have left TLS reporting itself loaded with sha1, sha256 and
  ;; ecdsa-verify-p256 still pure-Lisp. TEST-PURE-LISP-CRYPTO exists to
  ;; re-verify those on a libssl machine, so a half-swap could have read
  ;; as a pass.
  (setf *tls-outbound-setup-fn* #'tls-setup-outbound)
  (setf *https-stream-fn* #'https-fetch-stream)
  (log-info "tls: HTTPS fetch enabled"))
