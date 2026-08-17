(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; TLS tests — skipped when web-skeleton-tls is not loaded
;;; ===========================================================================

(defun test-tls ()
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
  (format t "~%=== TLS Tests ===~%")
  (if (not (tls-loaded-p))
      (progn
        (format t "~%  SKIP  TLS not loaded (libssl not found)~%")
        (format t "~%0 passed, 0 failed (skipped)~%~%")
        t)
      (progn
        (test-tls-registration)
        (test-ssl-ctx-init)
        (test-tls-client-ssl)
        (report-suite "TLS")
        (zerop *tests-failed*))))

(defun test-tls-client-ssl ()
  "TLS-CLIENT-SSL applies SNI, and the SSL says so when asked.

   This exists because the function was extracted out of TLS-CONNECT so the
   blocking and non-blocking handshakes could share one copy, and nothing
   in this tree exercises TLS-CONNECT — there is no HTTPS integration test,
   which makes an extraction from it a change with no detector. The two
   settings it carries are exactly the two whose absence a successful
   handshake does not report, so a silent regression would surface as
   somebody's traffic being verified against the wrong certificate.

   The read-back is the assertion with teeth.
   SSL_set_tlsext_host_name answers 1 without that guaranteeing the name is
   what a handshake will send, so checking the return value proves nothing a
   wrong-offset or wrong-entry-point call would fail — the same trap the
   min-proto-version ctrl already has a GET twin for.

   No connection is needed: SSL_set_fd only attaches a descriptor to the
   BIO, and nothing here handshakes."
  (format t "~%TLS client SSL setup~%")
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp))
        (ssl nil))
    (unwind-protect
         (let ((host "example.invalid"))
           (setf ssl (funcall (tls-sym "TLS-CLIENT-SSL")
                              host
                              (sb-bsd-sockets:socket-file-descriptor socket)))
           (check "tls-client-ssl returns an SSL" (not (null ssl)) t)
           (check "tls-client-ssl: SNI reads back off the SSL"
                  (funcall (tls-sym "%SSL-GET-SERVERNAME")
                           ssl
                           (symbol-value (tls-sym "+TLSEXT-NAMETYPE-HOST-NAME+")))
                  host))
      (when ssl (ignore-errors (funcall (tls-sym "%SSL-FREE") ssl)))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun test-tls-registration ()
  (format t "~%TLS Registration~%")
  (check "https-fetch-fn set"
         (not (null web-skeleton:*https-fetch-fn*)) t)
  (check "https-stream-fn set"
         (not (null web-skeleton:*https-stream-fn*)) t))

(defun test-ssl-ctx-init ()
  ;; Smoke the shared-context init path for real. Registration checks
  ;; alone can't catch an FFI-level mistake — a binding that names the
  ;; wrong libssl entry point (the historical SSL_ctrl / SSL_CTX_ctrl
  ;; mixup) or passes the wrong struct type loads cleanly and only
  ;; misbehaves when the call executes. ENSURE-SSL-CTX runs the whole
  ;; init sequence (OPENSSL_init_ssl, SSL_CTX_new, TLS 1.2 floor via
  ;; SSL_CTX_ctrl, CA paths, verify mode) with zero network, so it
  ;; belongs in the default suite whenever libssl is present.
  ;; Idempotent — the context is created once per process and cached,
  ;; so running it here just means the first later HTTPS fetch finds
  ;; it warm.
  ;;
  ;; The read-back check is the one with teeth. Empirically (OpenSSL
  ;; 3.5.4): the mixed-up SET returns 1 — success by every visible
  ;; measure — while the floor lands at a garbage offset, so only
  ;; reading min_proto_version back off the context distinguishes a
  ;; correct init from a lucky one.
  (format t "~%SSL context init~%")
  (check "ensure-ssl-ctx initializes shared context"
         (handler-case (and (funcall (tls-sym "ENSURE-SSL-CTX")) t)
           (error (e) (format nil "error: ~a" e)))
         t)
  (check "TLS 1.2 floor reads back off the context"
         (handler-case
             (funcall (tls-sym "%SSL-CTX-CTRL")
                      (funcall (tls-sym "ENSURE-SSL-CTX"))
                      (symbol-value
                       (tls-sym "+SSL-CTRL-GET-MIN-PROTO-VERSION+"))
                      0 (sb-sys:int-sap 0))
           (error (e) (format nil "error: ~a" e)))
         (symbol-value (tls-sym "+TLS1-2-VERSION+"))))
