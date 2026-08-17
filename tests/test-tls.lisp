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
        (test-tls-end-to-end)
        (test-tls-ssl-pending)
        (report-suite "TLS")
        (zerop *tests-failed*))))

;;; ---------------------------------------------------------------------------
;;; End-to-end TLS
;;;
;;; The first coverage in this project that puts real TLS bytes on a real
;;; socket, and it exists for one setting in particular. SSL_set1_host is
;;; what makes a certificate be checked for *whose* it is rather than only
;;; for whether a trusted CA signed it; drop it and every certificate any
;;; trusted CA ever issued is accepted for every host. Nothing detects that
;;; from inside the process — the handshake succeeds, the fetch succeeds, and
;;; the only symptom is that a MITM with any valid certificate now works.
;;;
;;; Isolating it needs a certificate that is trusted and wrong. Hence a
;;; generated CA, loaded additively into the shared context, signing two
;;; leaves: one for the name we ask for and one for a name we do not. The
;;; pair is the point. A self-signed certificate would fail for the wrong
;;; reason (untrusted issuer) and would still fail with set1_host removed,
;;; so it proves nothing about the setting.
;;;
;;; openssl s_server is the peer. Writing a TLS server against these
;;; bindings would mean testing this FFI surface with itself, and a bug
;;; common to both sides would cancel out.
;;; ---------------------------------------------------------------------------

(defparameter *tls-fixture-script* "
set -e
mkdir -p \"$1\"
cd \"$1\"
openssl req -x509 -newkey rsa:2048 -nodes -keyout ca.key -out ca.pem -subj /CN=ws-test-ca -days 2 -sha256 >/dev/null 2>&1
for n in right wrong; do
  printf 'subjectAltName=DNS:%s.test\\n' \"$n\" > \"$n.ext\"
  openssl req -newkey rsa:2048 -nodes -keyout \"$n.key\" -out \"$n.csr\" -subj \"/CN=$n.test\" >/dev/null 2>&1
  openssl x509 -req -in \"$n.csr\" -CA ca.pem -CAkey ca.key -CAcreateserial -out \"$n.pem\" -days 2 -sha256 -extfile \"$n.ext\" >/dev/null 2>&1
done
i=0
while [ $i -lt 1024 ]; do printf 'ABCDEFGHIJKLMNOPQRSTUVWXYZ012345ABCDEFGHIJKLMNOPQRSTUVWXYZ012345\\n'; i=$((i+1)); done > body.txt
printf 'TAIL-MARKER\\n' >> body.txt
{ printf 'HTTP/1.0 200 OK\\r\\nContent-Length: %d\\r\\n\\r\\n' \"$(wc -c < body.txt)\"; cat body.txt; } > response.txt
"
  "Generates a CA and two leaves, one per hostname.

   SAN, not CN: modern OpenSSL ignores commonName for hostname
   verification entirely, so a CN-only certificate would fail both cases
   and the pair would prove nothing.

   One command per line, no backslash continuations. A backslash before a
   newline inside a Common Lisp string escapes the newline and is consumed,
   so the shell never receives the continuation and each command silently
   splits in two — which is how the first version of this failed.")

(defun %pick-free-port ()
  "Bind port 0, read the number, close, return it.

   This is the close-then-rebind race START-SERVER was taught to avoid, and
   it is safe here for a reason that does not generalise: the rebinder is
   openssl s_server, which does not set SO_REUSEPORT, so a collision fails
   its bind loudly instead of silently sharing the port. %WAIT-FOR-ACCEPT
   turns that into a named failure. Do not copy this into the harness."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (unwind-protect
         (progn (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
                (nth-value 1 (sb-bsd-sockets:socket-name socket)))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun %wait-for-accept (port &key (timeout 10))
  "Poll PORT until it accepts a connection. Returns T, or NIL on timeout."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (when (> (get-internal-real-time) deadline) (return nil))
      (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                              :type :stream :protocol :tcp)))
        (handler-case
            (progn (sb-bsd-sockets:socket-connect s #(127 0 0 1) port)
                   (sb-bsd-sockets:socket-close s)
                   (return t))
          (error ()
            (ignore-errors (sb-bsd-sockets:socket-close s))
            (sleep 0.05)))))))

(defun %call-with-tls-peer (dir leaf fn &key relay-file)
  "Run openssl s_server in DIR presenting LEAF's certificate, call FN with
   the port, then stop it. FN gets NIL if the server never came up.

   With no RELAY-FILE it runs -www, whose canned status page is a few
   hundred bytes. RELAY-FILE instead feeds a file on s_server's stdin,
   which relay mode sends to the client as-is — the way to arrange a
   response bigger than one TLS record, and bigger than the read buffer.

   Not -HTTP, which looked like the obvious way to serve a large file and
   is not: measured, it truncates at around 10 KB against a reader that
   consumes in 4 KiB steps driven by epoll, while delivering the whole file
   to s_client. The cause is s_server's, not ours — a clean close_notify
   arrives mid-body — but a fixture that stops early for reasons belonging
   to the fixture cannot support an assertion about our drain loop."
  (let* ((port (%pick-free-port))
         ;; Bare name with :SEARCH T, matching dns.lisp's getent call. The
         ;; fixture script runs openssl through /bin/sh and so resolves it
         ;; via PATH; hardcoding /usr/bin/openssl here would make the two
         ;; halves disagree about where openssl is on any machine that keeps
         ;; it elsewhere — Homebrew, Nix, /usr/local in a slim image. Cert
         ;; generation would succeed, the peer would not come up, and the
         ;; failure would read as a TLS problem.
         (proc (sb-ext:run-program
                "openssl"
                (append
                 (list "s_server" "-accept" (princ-to-string port)
                       "-cert" (format nil "~a/~a.pem" dir leaf)
                       "-key"  (format nil "~a/~a.key" dir leaf)
                       "-quiet")
                 (if relay-file (list "-naccept" "1") (list "-www")))
                :wait nil :output nil :error nil
                :directory dir :search t
                :input (and relay-file (pathname relay-file)))))
    (unwind-protect
         ;; Relay mode serves exactly one connection, so it cannot be
         ;; probed: a probe that connects consumes the connection the test
         ;; needs and s_server exits before the test arrives. The port goes
         ;; through unprobed and %TLS-CONNECT-RETRYING is the readiness
         ;; check instead.
         (funcall fn (if relay-file port (and (%wait-for-accept port) port)))
      (ignore-errors (sb-ext:process-kill proc 15))
      (ignore-errors (sb-ext:process-wait proc)))))

(defun %tls-connect-retrying (host port &key (attempts 60))
  "TLS-CONNECT, retrying while the peer is still coming up.

   Needed because the relay-mode peer cannot be probed for readiness
   without spending the one connection it will serve. Retrying the real
   connect asks the same question without consuming the answer."
  (loop for i from 1 to attempts
        do (handler-case
               (return (funcall (tls-sym "TLS-CONNECT") host port))
             (error (e)
               (when (= i attempts) (error e))
               (sleep 0.05)))))

(defun test-tls-end-to-end ()
  "A real handshake against a real peer, and a trusted-but-wrong
   certificate rejected.

   The second assertion is the one with no substitute. It is the only check
   in this project that would notice SSL_set1_host going missing, and the
   failure it guards against is silent by construction: without it the
   handshake succeeds, the response arrives, and nothing anywhere reports
   that the certificate was issued for somebody else.

   DNS is overridden rather than mocked around, because https:// to an IP
   literal is refused by design — the certificate has to be checked against
   a name, so the test needs one. A LET suffices: TLS-CONNECT is blocking
   and reads this special on this thread."
  (format t "~%TLS end to end~%")
  (let ((dir (format nil "/tmp/ws-tls-~36r" (random (expt 36 10)))))
    (unwind-protect
         (progn
           (sb-ext:run-program "/bin/sh" (list "-c" *tls-fixture-script* "sh" dir)
                               :wait t :output nil :error nil)
           (unless (probe-file (format nil "~a/right.pem" dir))
             (check "tls e2e: fixture certificates generated" nil t)
             (return-from test-tls-end-to-end))
           ;; Additive: the system roots stay trusted, this CA joins them.
           (check "tls e2e: test CA loaded into the shared context"
                  (funcall (tls-sym "%SSL-CTX-LOAD-VERIFY-LOCATIONS")
                           (funcall (tls-sym "ENSURE-SSL-CTX"))
                           (format nil "~a/ca.pem" dir)
                           nil)
                  1)
           (let ((web-skeleton::*dns-resolve-blocking-fn*
                   (lambda (host)
                     (declare (ignore host))
                     (values #(127 0 0 1) :inet))))
             ;; Right name: the handshake completes and the shared line
             ;; reader reads a real TLS response off a real socket, which is
             ;; the first time it has done so in this suite.
             (%call-with-tls-peer
              dir "right"
              (lambda (port)
                (check "tls e2e: peer came up (right name)" (not (null port)) t)
                (when port
                  (multiple-value-bind (ssl socket)
                      (funcall (tls-sym "TLS-CONNECT") "right.test" port)
                    (unwind-protect
                         (let ((lines nil))
                           (funcall (tls-sym "TLS-WRITE-ALL") ssl
                                    (web-skeleton::build-outbound-request
                                     :GET "right.test" "/" :scheme :https
                                     :port port))
                           (check "tls e2e: shared reader reads a real TLS response"
                                  (web-skeleton::stream-response-lines
                                   nil (lambda (l) (push l lines))
                                   :read-fn (funcall (tls-sym "SSL-BYTE-READER")
                                                     ssl))
                                  200))
                      (ignore-errors
                       (funcall (tls-sym "TLS-CLOSE") ssl socket)))))))
              ;; WANT_READ becomes :AGAIN, asserted on its own because the
              ;; large-response test never reaches it: a peer that sends
              ;; everything and closes ends the drain on close_notify, so
              ;; the mapping is invisible there — removing it changed
              ;; nothing in a full suite run. Here nothing has been
              ;; requested yet, so the socket is genuinely empty and
              ;; SSL_read has no answer but "not yet".
              (%call-with-tls-peer
               dir "right"
               (lambda (port)
                 (check "tls e2e: peer came up (want-read)" (not (null port)) t)
                 (when port
                   (multiple-value-bind (ssl socket)
                       (funcall (tls-sym "TLS-CONNECT") "right.test" port)
                     (unwind-protect
                          (let ((fd (web-skeleton::socket-fd socket))
                                (buf (make-array 4096
                                                 :element-type '(unsigned-byte 8))))
                            (web-skeleton::set-nonblocking fd)
                            (check "tls e2e: an empty TLS socket answers :again"
                                   (funcall
                                    (funcall (tls-sym "SSL-CONNECTION-READER") ssl)
                                    buf 0 4096)
                                   :again))
                      (ignore-errors
                       (funcall (tls-sym "TLS-CLOSE") ssl socket)))))))
             ;; Wrong name, same CA. Trusted issuer, wrong subject.
             (%call-with-tls-peer
              dir "wrong"
              (lambda (port)
                (check "tls e2e: peer came up (wrong name)" (not (null port)) t)
                (when port
                  (check-error
                   "tls e2e: a trusted certificate for another host is refused"
                   (funcall (tls-sym "TLS-CONNECT") "right.test" port)))))))
      (ignore-errors
       (sb-ext:run-program "/bin/rm" (list "-rf" dir) :wait t
                                                      :output nil :error nil)))))

(defun test-tls-ssl-pending ()
  "A TLS response far larger than the read buffer arrives complete, through
   CONNECTION-READ-AVAILABLE over an SSL byte source, driven only by
   edge-triggered epoll.

   This is the trap issue #8 calls the one that will bite, and until now it
   was an argument rather than a test. SSL_read returns at most one record
   per call and hands back already-decrypted bytes before it touches the
   socket. Consume part of a record and the remainder sits in OpenSSL's
   buffer with nothing left on the fd, so an edge-triggered epoll has no
   transition to report and never fires again. The connection then hangs
   holding its own answer.

   The shape of the assertion is what makes it a test of that and not of
   something easier. Reads are driven exclusively from EPOLL-WAIT, and the
   loop stops the moment epoll goes quiet. A drain that leaves bytes inside
   OpenSSL therefore loses them for good, exactly as it would in the event
   loop, and the body comes up short. Calling CONNECTION-READ-AVAILABLE in a
   plain loop instead would find those bytes on the next call and pass
   against the bug.

   64 KiB of body, against a 4 KiB starting buffer: OpenSSL sends bulk data
   in records up to 16 KiB, so most SSL_read calls here return a partial
   record and leave the rest pending. One record larger than one buffer is
   the condition; this arranges several."
  (format t "~%TLS SSL_pending discipline~%")
  (let ((dir (format nil "/tmp/ws-tls-~36r" (random (expt 36 10)))))
    (unwind-protect
         (progn
           (sb-ext:run-program "/bin/sh" (list "-c" *tls-fixture-script* "sh" dir)
                               :wait t :output nil :error nil)
           (unless (probe-file (format nil "~a/response.txt" dir))
             (check "ssl-pending: fixture generated" nil t)
             (return-from test-tls-ssl-pending))
           (funcall (tls-sym "%SSL-CTX-LOAD-VERIFY-LOCATIONS")
                    (funcall (tls-sym "ENSURE-SSL-CTX"))
                    (format nil "~a/ca.pem" dir) nil)
           (let ((web-skeleton::*dns-resolve-blocking-fn*
                   (lambda (host) (declare (ignore host))
                     (values #(127 0 0 1) :inet)))
                 (expected (with-open-file (s (format nil "~a/body.txt" dir)
                                              :element-type '(unsigned-byte 8))
                             (file-length s))))
             (check "ssl-pending: fixture body is bigger than one record"
                    (> expected 16384) t)
             (%call-with-tls-peer
              dir "right"
              (lambda (port)
                (check "ssl-pending: peer came up" (not (null port)) t)
                (when port
                  (multiple-value-bind (ssl socket)
                      (%tls-connect-retrying "right.test" port)
                    (let ((epfd (web-skeleton::epoll-create)))
                      (unwind-protect
                           (let* ((fd (web-skeleton::socket-fd socket))
                                  (reads (list 0))
                                  (conn (web-skeleton::make-connection
                                         :fd fd :socket socket :state :out-read
                                         :outbound-p t
                                         :last-active (get-universal-time)
                                         :read-fn
                                         (let ((inner (funcall
                                                       (tls-sym "SSL-CONNECTION-READER")
                                                       ssl)))
                                           (lambda (b s m)
                                             (incf (car reads))
                                             (funcall inner b s m)))))
                                  (evbuf (make-array
                                          (* 4 web-skeleton::+epoll-event-size+)
                                          :element-type '(unsigned-byte 8)))
                                  (verdicts nil))
                             ;; No request is sent: relay mode pushes the
                             ;; canned response as soon as the handshake
                             ;; completes, and the read path is what is
                             ;; under test.
                             (web-skeleton::set-nonblocking fd)
                             (web-skeleton::epoll-add
                              epfd fd (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
                             ;; Reads happen only when epoll says so. If a
                             ;; drain ever leaves bytes inside OpenSSL, epoll
                             ;; goes quiet and this loop ends short.
                             (loop repeat 200
                                   while (not (member (first verdicts)
                                                      '(:eof :ok-eof)))
                                   do (if (plusp (web-skeleton::epoll-wait
                                                  epfd evbuf 4 1000))
                                          (push (web-skeleton::connection-read-available
                                                 conn)
                                                verdicts)
                                          (return)))
                             (let* ((got (web-skeleton::connection-read-pos conn))
                                    (raw (sb-ext:octets-to-string
                                          (subseq (web-skeleton::connection-read-buf conn)
                                                  0 got)
                                          :external-format :latin-1))
                                    (blank (search (format nil "~c~c~c~c"
                                                           #\Return #\Newline
                                                           #\Return #\Newline)
                                                   raw)))
                               (check "ssl-pending: stream ended, not stalled"
                                      (and (member (first verdicts) '(:eof :ok-eof))
                                           t)
                                      t)
                               ;; The discriminating pair. Many SSL_read
                               ;; calls were needed, and they happened
                               ;; *inside* the drain rather than one per
                               ;; epoll wake-up — which is the whole
                               ;; SSL_pending property. A drain that
                               ;; returned after one SSL_read would need
                               ;; one wake-up per record, and the records
                               ;; it had already decrypted would never
                               ;; produce one.
                               ;; Loose on purpose. The exact count depends
                               ;; on OpenSSL's record sizing and on the
                               ;; buffer's doubling schedule, and pinning it
                               ;; would make this test track both. What
                               ;; matters is that it is many, not one.
                               (check "ssl-pending: many SSL_read calls were needed"
                                      (> (car reads) 5) t)
                               (check "ssl-pending: and they ran inside the drain"
                                      (> (car reads) (* 2 (length verdicts))) t)
                               (check "ssl-pending: headers arrived"
                                      (and blank t) t)
                               (check "ssl-pending: the whole body arrived"
                                      (and blank (= (- got (+ blank 4)) expected))
                                      t)
                               (check "ssl-pending: and its last line is intact"
                                      (and (search "TAIL-MARKER" raw) t) t)))
                        (ignore-errors (web-skeleton::%close epfd))
                        (ignore-errors
                         (funcall (tls-sym "TLS-CLOSE") ssl socket)))))))
              :relay-file (format nil "~a/response.txt" dir))))
      (ignore-errors
       (sb-ext:run-program "/bin/rm" (list "-rf" dir) :wait t
                                                      :output nil :error nil)))))

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
