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
        (test-tls-connection-write)
        (test-tls-write-retry-after-gc)
        (test-ssl-read-classification)
        (test-ssl-eintr-retry)
        (test-https-fetch-async-e2e)
        (test-https-fetch-on-body-e2e)
        (test-https-does-not-hold-the-worker)
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

(defun %port-listening-p (port)
  "T when some socket is in LISTEN state on PORT, per /proc/net/tcp and
   /proc/net/tcp6.

   Not %LISTEN-SOCKET-COUNT, for two reasons. That one reads IPv4 only,
   and openssl s_server binds the IPv6 wildcard, so it never appears
   there. And it counts distinct inodes because its caller is asking how
   many workers joined an SO_REUSEPORT listen group — a harder question
   than this one, which needs only whether the port is up yet. The
   seq_file resume that makes counting delicate is harmless here: a row
   skipped on one pass is read on the next poll.

   Field 1 is LOCAL_ADDRESS as HEXIP:HEXPORT, field 3 is the state, and
   0A is TCP_LISTEN."
  (let ((hex (format nil "~4,'0X" port)))
    (dolist (path '("/proc/net/tcp" "/proc/net/tcp6") nil)
      (with-open-file (in path :if-does-not-exist nil)
        (when in
          (read-line in nil nil)          ; column header
          (loop for line = (read-line in nil nil)
                while line
                do (let ((fields (%split-ws line)))
                     (when (>= (length fields) 4)
                       (let* ((local (second fields))
                              (colon (position #\: local)))
                         (when (and colon
                                    (string= (fourth fields) "0A")
                                    (string= hex local :start2 (1+ colon)))
                           (return-from %port-listening-p t)))))))))))

(defun %wait-for-listener (port &key (timeout 10))
  "Poll the kernel's listen table until PORT has a listening socket.
   Returns T, or NIL on timeout.

   The readiness check for a peer that must not be probed. %WAIT-FOR-ACCEPT
   answers the same question by connecting, which relay mode cannot afford:
   s_server -naccept 1 serves exactly one connection, so the probe spends
   the connection the test came for.

   Sleeping a fixed interval instead makes the test a race. The framework's
   own outbound dial does not retry — %TLS-CONNECT-RETRYING is the test
   client's lever, not the event loop's — so a fetch that beats s_server's
   bind gets ECONNREFUSED, and it surfaces as a 502 from the relay rather
   than as anything naming the fixture. Reading /proc/net/tcp asks whether
   the socket is listening without touching it."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (when (%port-listening-p port) (return t))
      (when (> (get-internal-real-time) deadline) (return nil))
      (sleep 0.05))))

(defvar *tls-peer-process* nil
  "The s_server process, bound inside %CALL-WITH-TLS-PEER. A special rather
   than another parameter on FN, so the one test that needs to signal the
   peer can reach it without every other call site growing an argument it
   ignores.")

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
         (let ((*tls-peer-process* proc))
           (funcall fn (if relay-file port (and (%wait-for-accept port) port))))
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
                            ;; The subject raises as readily as it answers,
                            ;; so a raise is converted into a value the CHECK
                            ;; can report. Uncaught, it ends the run
                            ;; mid-file: no failure list, no totals, and 35
                            ;; later assertions never execute — which makes
                            ;; this detector unreadable by the full-list
                            ;; discipline every revert here is read under.
                            ;; Same handler-case, same reason, as the parity
                            ;; arm in test-properties.lisp.
                            (check "tls e2e: an empty TLS socket answers :again"
                                   (handler-case
                                       (funcall
                                        (funcall (tls-sym "SSL-CONNECTION-READER")
                                                 ssl)
                                        buf 0 4096)
                                     (error (e) (princ-to-string e)))
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
                                          ;; A raising read becomes a verdict
                                          ;; rather than the end of the suite;
                                          ;; the assertions below then report
                                          ;; on what was actually collected.
                                          (let ((v (handler-case
                                                       (web-skeleton::connection-read-available
                                                        conn)
                                                     (error (e)
                                                       (list :raised
                                                             (princ-to-string e))))))
                                            (push v verdicts)
                                            (when (consp v) (return)))
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

(defconstant +so-sndbuf+ 7
  "SO_SNDBUF. Defined here rather than in epoll.lisp because nothing the
   framework does needs it — only this test, which has to make a socket
   small enough to block.")

(defun test-tls-write-retry-after-gc ()
  "A blocked SSL_write, a full GC, and then the retry — the exact sequence
   the foreign staging buffer exists for.

   OpenSSL requires a write retried after WANT_WRITE to present the same
   address and length as the call that failed. WITH-PINNED-OBJECTS pins only
   for its own dynamic extent, so a partial write that returns to the event
   loop is unpinned in between and a GC may move the vector. Staging into
   malloc'd memory removes the requirement rather than satisfying it: the
   bytes live somewhere that cannot move.

   The peer is SIGSTOPped, which is the whole reason this is reliable rather
   than a race. A stopped process cannot read, so its receive buffer fills
   and stays full; and unlike a peer that has finished or exited, it cannot
   close the connection underneath the retry. Racing a live peer instead
   produced exactly that — SSL_write failing with SSL_ERROR_SYSCALL because
   the far end had gone away mid-test.

   Small vectors, not one large one: a full GC is free to relocate a 1 KiB
   vector and would leave a multi-megabyte one where it is.

   Measured: with staging removed, the retry raises \"SSL_write failed:
   error 1\" — SSL_ERROR_SSL, OpenSSL's bad-write-retry — three runs of
   three. That is this ruling demonstrated rather than argued."
  (format t "~%TLS write retry across a GC~%")
  (let ((dir (format nil "/tmp/ws-tls-~36r" (random (expt 36 10)))))
    (unwind-protect
         (progn
           (sb-ext:run-program "/bin/sh" (list "-c" *tls-fixture-script* "sh" dir)
                               :wait t :output nil :error nil)
           (unless (probe-file (format nil "~a/right.pem" dir))
             (check "tls retry: fixture generated" nil t)
             (return-from test-tls-write-retry-after-gc))
           (funcall (tls-sym "%SSL-CTX-LOAD-VERIFY-LOCATIONS")
                    (funcall (tls-sym "ENSURE-SSL-CTX"))
                    (format nil "~a/ca.pem" dir) nil)
           (let ((web-skeleton::*dns-resolve-blocking-fn*
                   (lambda (h) (declare (ignore h))
                     (values #(127 0 0 1) :inet))))
             (%call-with-tls-peer
              dir "right"
              (lambda (port)
                (check "tls retry: peer came up" (not (null port)) t)
                (when port
                  (multiple-value-bind (ssl socket)
                      (funcall (tls-sym "TLS-CONNECT") "right.test" port)
                    (multiple-value-bind (write-fn release-fn)
                        (funcall (tls-sym "SSL-CONNECTION-WRITER") ssl)
                      (let* ((fd (web-skeleton::socket-fd socket))
                             (peer *tls-peer-process*)
                             (conn (web-skeleton::make-connection
                                    :fd fd :socket socket :state :out-write
                                    :outbound-p t
                                    :last-active (get-universal-time)
                                    :write-fn write-fn :close-fn release-fn)))
                        (unwind-protect
                             (progn
                               ;; Frozen, so it cannot read and cannot close.
                               (sb-ext:process-kill peer 19)
                               (web-skeleton::set-socket-option-int
                                fd web-skeleton::+sol-socket+ +so-sndbuf+ 2048)
                               (web-skeleton::set-nonblocking fd)
                               (loop repeat 4000
                                     do (web-skeleton::connection-append-write
                                         conn
                                         (make-array 1024
                                                     :element-type '(unsigned-byte 8)
                                                     :initial-element 88)))
                               (check "tls retry: the socket actually blocked"
                                      (attempt (web-skeleton::connection-on-write conn))
                                      :continue)
                               ;; The whole point.
                               (sb-ext:gc :full t)
                               (check "tls retry: and the retry survives a full GC"
                                      (attempt (web-skeleton::connection-on-write conn))
                                      :continue))
                          (ignore-errors (sb-ext:process-kill peer 18))
                          (ignore-errors (web-skeleton::connection-close conn))
                          (ignore-errors
                           (funcall (tls-sym "%SSL-FREE") ssl))))))))))) 
      (ignore-errors
       (sb-ext:run-program "/bin/rm" (list "-rf" dir) :wait t
                                                      :output nil :error nil)))))

(defun test-tls-connection-write ()
  "A request written through the connection write queue over real TLS, and
   the staging buffer released afterwards.

   The round trip is the correctness assertion. Counting bytes out would
   pass against a staging copy that dropped or reordered them; a peer that
   parses the request and answers 200 could not.

   The large payload is there for the chunking. It exceeds
   +SSL-WRITE-STAGE-SIZE+ several times over, so CONNECTION-ON-WRITE has to
   come back for more and each pass restages — which is the path the
   foreign buffer exists to make safe.

   Not covered, and it is the same gap as WANT_WRITE on the read side: the
   WANT_WRITE retry itself. Provoking it needs a peer that stops reading
   while its socket buffer fills, and a retry that never happens cannot
   demonstrate that the address held still across it. Review-verified."
  (format t "~%TLS connection write path~%")
  (let ((dir (format nil "/tmp/ws-tls-~36r" (random (expt 36 10)))))
    (unwind-protect
         (progn
           (sb-ext:run-program "/bin/sh" (list "-c" *tls-fixture-script* "sh" dir)
                               :wait t :output nil :error nil)
           (unless (probe-file (format nil "~a/right.pem" dir))
             (check "tls write: fixture generated" nil t)
             (return-from test-tls-connection-write))
           (funcall (tls-sym "%SSL-CTX-LOAD-VERIFY-LOCATIONS")
                    (funcall (tls-sym "ENSURE-SSL-CTX"))
                    (format nil "~a/ca.pem" dir) nil)
           (let ((web-skeleton::*dns-resolve-blocking-fn*
                   (lambda (h) (declare (ignore h))
                     (values #(127 0 0 1) :inet))))
             (%call-with-tls-peer
              dir "right"
              (lambda (port)
                (check "tls write: peer came up" (not (null port)) t)
                (when port
                  (multiple-value-bind (ssl socket)
                      (funcall (tls-sym "TLS-CONNECT") "right.test" port)
                    (unwind-protect
                         (multiple-value-bind (write-fn release-fn)
                             (funcall (tls-sym "SSL-CONNECTION-WRITER") ssl)
                           (let* ((writes (list 0))
                                  (releases (list 0))
                                  (conn (web-skeleton::make-connection
                                         :fd (web-skeleton::socket-fd socket)
                                         :socket socket :state :out-write
                                         :outbound-p t
                                         :last-active (get-universal-time)
                                         :write-fn
                                         (lambda (b st n)
                                           (incf (car writes))
                                           (funcall write-fn b st n))
                                         ;; Counted, so the assertion below
                                         ;; can observe the release running
                                         ;; rather than the slot being
                                         ;; cleared. CONNECTION-CLOSE nulls
                                         ;; the slot before calling it, so a
                                         ;; version that deleted only the
                                         ;; call would still look tidy.
                                         :close-fn
                                         (lambda ()
                                           (incf (car releases))
                                           (funcall release-fn))))
                                  ;; Padding rides in a header value, so the
                                  ;; request stays well-formed and the peer
                                  ;; still has to parse it.
                                  (pad (make-string 60000 :initial-element #\X))
                                  (req (sb-ext:string-to-octets
                                        (format nil "GET / HTTP/1.0~c~cX-Pad: ~a~c~c~c~c"
                                                #\Return #\Newline pad
                                                #\Return #\Newline
                                                #\Return #\Newline)
                                        :external-format :ascii)))
                             (check "tls write: payload exceeds the staging buffer"
                                    (> (length req)
                                       (* 3 (symbol-value
                                             (tls-sym "+SSL-WRITE-STAGE-SIZE+"))))
                                    t)
                             (web-skeleton::connection-queue-write conn req)
                             (check "tls write: the queue drains to :done"
                                    (handler-case
                                        (web-skeleton::connection-on-write conn)
                                      (error (e) (princ-to-string e)))
                                    :done)
                             (check "tls write: it took several staged passes"
                                    (> (car writes) 3) t)
                             ;; The peer parsed what arrived. Nothing about a
                             ;; byte count proves that.
                             (let ((lines nil))
                               (check "tls write: and the peer answered it"
                                      (handler-case
                                          (web-skeleton::stream-response-lines
                                           nil (lambda (l) (push l lines))
                                           :read-fn (funcall
                                                     (tls-sym "SSL-BYTE-READER")
                                                     ssl))
                                        (error (e) (princ-to-string e)))
                                      200))
                             ;; Release is the connection's job, once.
                             (web-skeleton::connection-close conn)
                             (check "tls write: close ran the release"
                                    (car releases) 1)
                             (check "tls write: and cleared the slot with it"
                                    (web-skeleton::connection-close-fn conn) nil)
                             (check "tls write: and a second close is safe"
                                    (handler-case
                                        (progn (web-skeleton::connection-close conn)
                                               :ok)
                                      (error (e) (princ-to-string e)))
                                    :ok)
                             (check "tls write: which did not release twice"
                                    (car releases) 1)))
                      (ignore-errors
                       (funcall (tls-sym "%SSL-FREE") ssl)))))))))
      (ignore-errors
       (sb-ext:run-program "/bin/rm" (list "-rf" dir) :wait t
                                                      :output nil :error nil)))))

(defun test-https-does-not-hold-the-worker ()
  "Issue #8's headline criterion: with one worker, a request relaying from
   an https:// upstream must not delay a concurrent request to a fast
   endpoint.

   The stall is a SIGSTOPped peer, and that choice is what makes this a
   test rather than a race. A frozen process completes the TCP connect —
   the kernel does that — and then answers nothing, so the handshake parks
   in :OUT-HANDSHAKE for as long as we like, deterministically. Timing the
   fast request against a peer that merely happens to be slow would be a
   measurement of the machine.

   Before this branch the relay held the worker for the whole exchange,
   and /fast would not have been answered until the upstream finished.
   With one worker there is no other thread to rescue it: whether the fast
   request is served at all is the entire question."
  (format t "~%HTTPS relay does not hold the worker~%")
  (let ((dir (format nil "/tmp/ws-tls-~36r" (random (expt 36 10))))
        (saved-dns web-skeleton::*dns-lookup-fn*))
    (unwind-protect
         (progn
           (sb-ext:run-program "/bin/sh" (list "-c" *tls-fixture-script* "sh" dir)
                               :wait t :output nil :error nil)
           (unless (probe-file (format nil "~a/right.pem" dir))
             (check "no-hold: fixture generated" nil t)
             (return-from test-https-does-not-hold-the-worker))
           (funcall (tls-sym "%SSL-CTX-LOAD-VERIFY-LOCATIONS")
                    (funcall (tls-sym "ENSURE-SSL-CTX"))
                    (format nil "~a/ca.pem" dir) nil)
           (setf web-skeleton::*dns-lookup-fn*
                 (lambda (conn epoll-fd fetch-req host port path)
                   (web-skeleton::initiate-http-fetch-to-address
                    conn epoll-fd fetch-req host port path #(127 0 0 1) :inet)))
           (%call-with-tls-peer
            dir "right"
            (lambda (peer-port)
              (check "no-hold: peer came up" (not (null peer-port)) t)
              (when peer-port
                (let ((peer *tls-peer-process*))
                  (with-test-server
                      (:handler
                       (lambda (req)
                         (if (search "/fast" (http-request-path req))
                             (make-text-response 200 "fast")
                             (http-fetch
                              :get (format nil "https://right.test:~d/" peer-port)
                              :then (lambda (status headers body)
                                      (declare (ignore status headers body))
                                      (make-text-response 200 "relayed"))))))
                    (let ((port *test-port*)
                          (relay-done nil))
                      ;; Frozen before anything connects, so the handshake
                      ;; cannot complete until we say so.
                      (sb-ext:process-kill peer 19)
                      (let ((relay (sb-thread:make-thread
                                    (lambda ()
                                      (let ((*test-port* port))
                                        (setf relay-done
                                              (attempt
                                               (nth-value
                                                0 (test-http-request :get "/relay"))))))
                                    :name "https-relay")))
                        (unwind-protect
                             (progn
                               ;; Let the relay reach the stalled handshake.
                               (sleep 0.5)
                               (check "no-hold: the relay is still in flight"
                                      relay-done nil)
                               (let* ((start (get-internal-real-time))
                                      (status (attempt
                                               (nth-value
                                                0 (test-http-request :get "/fast"))))
                                      (elapsed (/ (- (get-internal-real-time) start)
                                                  internal-time-units-per-second)))
                                 (check "no-hold: the fast request was answered"
                                        status 200)
                                 ;; Generous on purpose. The claim is "not
                                 ;; blocked behind a stalled TLS handshake",
                                 ;; not a latency budget; a worker that was
                                 ;; held would not answer at all until we
                                 ;; released the peer, seconds later.
                                 (check "no-hold: and answered promptly"
                                        (< elapsed 2) t)))
                          (ignore-errors (sb-ext:process-kill peer 18))
                          (ignore-errors
                           (sb-thread:join-thread relay :timeout 15))))
                      ;; And the relay still completes once the peer moves.
                      (check "no-hold: the stalled relay finished afterwards"
                             relay-done 200))))))))
      (setf web-skeleton::*dns-lookup-fn* saved-dns)
      (ignore-errors
       (sb-ext:run-program "/bin/rm" (list "-rf" dir) :wait t
                                                      :output nil :error nil)))))

(defun test-ssl-eintr-retry ()
  "EINTR is a retry, not a transport failure, on both classifiers — and
   the loop that acts on it actually loops.

   An interrupted call moved no bytes and left the connection intact, so
   reporting it as a transport error fails a fetch that nothing is wrong
   with. It reached the application as a 502 with the callback's cleanup
   sentinel.

   Reachable on the blocking fetch path specifically, and not by the
   route a reader assumes. Those sockets are blocking by BLOCKING-CONNECT's
   deliberate choice and carry SO_RCVTIMEO so *FETCH-TIMEOUT* bounds them,
   and per signal(7) a blocking socket call carrying a receive timeout
   fails with EINTR when interrupted regardless of SA_RESTART. The
   framework then supplies the signal: a getent child per DNS lookup, on a
   path that has just resolved a name.

   Asserted directly rather than provoked, for the same reason the
   ECONNRESET branch above is: arranging a real signal to land inside an
   in-flight SSL_read is not something this fixture can do reliably, and a
   test that only sometimes reaches its branch only sometimes catches a
   regression in it. Stated rather than implied.

   The count in the loop check is the assertion with teeth. A helper that
   called its thunk once and returned whatever it got would satisfy the
   value check and none of the purpose."
  (format t "~%SSL EINTR retry~%")
  (let ((classify (tls-sym "SSL-READ-EOF-OR-RAISE"))
        (blocking (tls-sym "SSL-BLOCKING-READ-EOF-OR-RAISE"))
        (wclass   (tls-sym "SSL-WRITE-RETRY-OR-RAISE"))
        (retrying (tls-sym "CALL-RETRYING-EINTR"))
        (syscall  (symbol-value (tls-sym "+SSL-ERROR-SYSCALL+")))
        (eagain   (symbol-value (find-symbol "+EAGAIN+" :web-skeleton)))
        (eintr    (symbol-value (find-symbol "+EINTR+" :web-skeleton)))
        (econnreset 104))
    ;; Read side, and the blocking wrapper must pass it through rather
    ;; than convert it into the loud receive-timeout error — reporting a
    ;; blown 30-second deadline milliseconds into the budget is the
    ;; misdiagnosis BLOCKING-CONNECT's poll loop already exists to prevent.
    (check "eintr: SYSCALL with EINTR is a retry, not an error"
           (attempt (funcall classify nil -1 eintr syscall)) :retry)
    (check "eintr: the blocking wrapper passes a retry through"
           (attempt (funcall blocking nil -1 eintr syscall)) :retry)
    ;; Write side, plus the two neighbours it must not have swallowed.
    (check "eintr: SSL_write with EINTR is a retry"
           (attempt (funcall wclass nil -1 eintr syscall)) :retry)
    (check "eintr: SSL_write with EAGAIN is still the timeout"
           (and (search "timed out"
                        (attempt (funcall wclass nil -1 eagain syscall)))
                t)
           t)
    (check "eintr: SSL_write with ECONNRESET still raises"
           (stringp (attempt (funcall wclass nil -1 econnreset syscall))) t)
    ;; And the loop.
    (let ((calls 0))
      (check "eintr: the retry loop runs until the answer is not :retry"
             (funcall retrying
                      (lambda ()
                        (incf calls)
                        (if (< calls 3) :retry 7)))
             7)
      (check "eintr: and it called through three times to get there"
             calls 3))))

(defun test-https-fetch-async-e2e ()
  "An https:// fetch through the event loop, end to end, on one worker.

   This is what items 3 through 8 were for. Until now every one of them
   was mechanism with tests and no production caller: the seam, the
   handshake state, the byte source, the byte sink, the direction
   inversion and the errno discipline had never all been wired at once,
   let alone driven by a real request. Here a handler calls HTTP-FETCH on
   an https:// URL, and the worker returns to the event loop between every
   step of it.

   DNS is overridden through *DNS-LOOKUP-FN*, the hook dns.lisp fills, and
   SETF globally rather than bound: the worker reading it lives in a
   thread WITH-TEST-SERVER spawned, and dynamic bindings do not cross
   MAKE-THREAD. Restored in the UNWIND-PROTECT.

   A name, not an address, because https:// to an IP literal is refused by
   design — a certificate has to be checked against something, and an
   address is not it. That refusal is the reason this test needs a
   resolver at all."
  (format t "~%HTTPS fetch through the event loop~%")
  (let ((dir (format nil "/tmp/ws-tls-~36r" (random (expt 36 10))))
        (saved-dns web-skeleton::*dns-lookup-fn*))
    (unwind-protect
         (progn
           (sb-ext:run-program "/bin/sh" (list "-c" *tls-fixture-script* "sh" dir)
                               :wait t :output nil :error nil)
           (unless (probe-file (format nil "~a/right.pem" dir))
             (check "https async: fixture generated" nil t)
             (return-from test-https-fetch-async-e2e))
           (funcall (tls-sym "%SSL-CTX-LOAD-VERIFY-LOCATIONS")
                    (funcall (tls-sym "ENSURE-SSL-CTX"))
                    (format nil "~a/ca.pem" dir) nil)
           (setf web-skeleton::*dns-lookup-fn*
                 (lambda (conn epoll-fd fetch-req host port path)
                   (web-skeleton::initiate-http-fetch-to-address
                    conn epoll-fd fetch-req host port path #(127 0 0 1) :inet)))
           (%call-with-tls-peer
            dir "right"
            (lambda (port)
              (check "https async: peer came up" (not (null port)) t)
              (when port
                (let ((upstream :never))
                  (with-test-server
                      (:handler
                       (lambda (req)
                         (declare (ignore req))
                         (http-fetch
                          :get (format nil "https://right.test:~d/" port)
                          :then (lambda (status headers body)
                                  (declare (ignore headers))
                                  (setf upstream
                                        (list status
                                              (if (and body (plusp (length body)))
                                                  :present :empty)))
                                  (make-text-response 200 "relayed")))))
                    (multiple-value-bind (status headers body)
                        (test-http-request :get "/relay")
                      (declare (ignore headers))
                      (check "https async: the inbound request was answered"
                             status 200)
                      (check "https async: and answered from the relay"
                             body "relayed")))
                  ;; The upstream half, asserted separately: a 200 to the
                  ;; client proves the handler ran, not that TLS worked.
                  (check "https async: the upstream answered over TLS"
                         upstream (list 200 :present)))))))
      (setf web-skeleton::*dns-lookup-fn* saved-dns)
      (ignore-errors
       (sb-ext:run-program "/bin/rm" (list "-rf" dir) :wait t
                                                      :output nil :error nil)))))

(defun test-https-fetch-on-body-e2e ()
  "An :ON-BODY relay over https:// sees the chunk boundaries the upstream
   framed, not the ones TLS records happened to land on.

   The two layers have no reason to agree. OpenSSL emits records on its
   own schedule and hands back whatever a single SSL_read decrypted, so
   the byte runs arriving at the walk are cut differently over TLS than
   over plain TCP — split mid-chunk, or several chunks at once. The
   framing has to be reconstructed from the stream either way, and the
   app is promised the same chunks regardless of which transport it
   named.

   Asserted against *CHUNKED-CORPUS*, which is also what the plain-TCP
   test asserts and what this upstream is built from. That shared
   definition is the point: two literals could agree today and drift
   apart later without either test noticing.

   Measured, this does *not* reach the :OK-EOF branch, and the docstring
   said it did until the claim was checked. s_server's close_notify
   arrives as its own wake-up, so the walk completes on :OK while the
   stream is still open — confirmed by reintroducing the :OK-EOF defect,
   which leaves this test green. TEST-FETCH-OK-EOF-WALKS-THE-BYTES is
   where that branch is pinned. This one covers the boundaries."
  (format t "~%HTTPS fetch :on-body chunk boundaries~%")
  (let ((dir (format nil "/tmp/ws-tls-~36r" (random (expt 36 10))))
        (saved-dns web-skeleton::*dns-lookup-fn*))
    (unwind-protect
         (progn
           (sb-ext:run-program "/bin/sh" (list "-c" *tls-fixture-script* "sh" dir)
                               :wait t :output nil :error nil)
           (unless (probe-file (format nil "~a/right.pem" dir))
             (check "https on-body: fixture generated" nil t)
             (return-from test-https-fetch-on-body-e2e))
           (with-open-file (out (format nil "~a/chunked.txt" dir)
                                :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
             (write-sequence (sb-ext:string-to-octets (%chunked-corpus-response)
                                                      :external-format :ascii)
                             out))
           (funcall (tls-sym "%SSL-CTX-LOAD-VERIFY-LOCATIONS")
                    (funcall (tls-sym "ENSURE-SSL-CTX"))
                    (format nil "~a/ca.pem" dir) nil)
           ;; SETF globally, not bound: the worker that reads this lives in
           ;; a thread WITH-TEST-SERVER spawned, and dynamic bindings do not
           ;; cross MAKE-THREAD. Restored in the UNWIND-PROTECT.
           (setf web-skeleton::*dns-lookup-fn*
                 (lambda (conn epoll-fd fetch-req host port path)
                   (web-skeleton::initiate-http-fetch-to-address
                    conn epoll-fd fetch-req host port path #(127 0 0 1) :inet)))
           (%call-with-tls-peer
            dir "right"
            (lambda (port)
              (let ((chunks nil)
                    (final :never)
                    (fires 0))
                (check "https on-body: the peer is listening"
                       (%wait-for-listener port) t)
                (with-test-server
                    (:handler
                     (lambda (req)
                       (declare (ignore req))
                       (http-fetch
                        :get (format nil "https://right.test:~d/" port)
                        :on-body (lambda (conn chunk)
                                   (declare (ignore conn))
                                   (push (sb-ext:octets-to-string
                                          chunk :external-format :ascii)
                                         chunks))
                        :then (lambda (status headers body)
                                (declare (ignore headers))
                                (incf fires)
                                (setf final
                                      (list status (if body :present :nil)))
                                (make-text-response 200 "relayed")))))
                  (multiple-value-bind (status headers body)
                      (test-http-request :get "/relay")
                    (declare (ignore headers))
                    (check "https on-body: the relay answered" status 200)
                    (check "https on-body: and its own body came through"
                           body "relayed")))
                ;; The upstream half, asserted separately: a 200 to the
                ;; client proves the handler ran, not that TLS worked.
                (check "https on-body: the upstream answered over TLS"
                       (first final) 200)
                ;; The claim.
                (check "https on-body: chunk boundaries survive TLS framing"
                       (reverse chunks) *chunked-corpus*)
                (check "https on-body: :then got no body to re-deliver"
                       (second final) :nil)
                (check "https on-body: :then fires exactly once" fires 1)))
            :relay-file (format nil "~a/chunked.txt" dir)))
      (setf web-skeleton::*dns-lookup-fn* saved-dns)
      (ignore-errors
       (sb-ext:run-program "/bin/rm" (list "-rf" dir) :wait t
                                                      :output nil :error nil)))))

(defun test-ssl-read-classification ()
  "Every branch of the SSL_read classifier, and the blocking wrapper's one
   difference from it.

   This code decides whether a truncated HTTPS response reaches an
   application as success. It has carried that decision since the TLS path
   was written and has never had a direct assertion on it — only the
   end-to-end tests that happen to route through it on the happy path,
   which exercise exactly one of its branches.

   Both optionals are supplied at every call, so SSL is never dereferenced
   and NIL is safe to pass. That is not a trick: it is the same shape a
   real caller uses, because a caller that had to branch on WANT_READ
   first has already sampled both.

   The ECONNRESET case is the MITM one. Provoking a real reset mid-body
   would need a peer that can be made to send RST rather than FIN at a
   chosen moment, which openssl s_server gives no way to arrange; the
   branch is asserted directly instead, and that is stated rather than
   implied."
  (format t "~%SSL_read classification~%")
  (let ((classify (tls-sym "SSL-READ-EOF-OR-RAISE"))
        (blocking (tls-sym "SSL-BLOCKING-READ-EOF-OR-RAISE"))
        (syscall  (symbol-value (tls-sym "+SSL-ERROR-SYSCALL+")))
        (zero-ret (symbol-value (tls-sym "+SSL-ERROR-ZERO-RETURN+")))
        (want-rd  (symbol-value (tls-sym "+SSL-ERROR-WANT-READ+")))
        (want-wr  (symbol-value (tls-sym "+SSL-ERROR-WANT-WRITE+")))
        (eagain   (symbol-value (find-symbol "+EAGAIN+" :web-skeleton)))
        (econnreset 104))
    ;; A clean close_notify is the only unambiguous end of stream.
    (check "classify: ZERO_RETURN is a clean end of stream"
           (attempt (funcall classify nil 0 0 zero-ret)) :eof)
    ;; errno 0 is end-of-stream without close_notify. Benign, and the
    ;; framing signal HTTP/1.0-style servers actually use.
    (check "classify: SYSCALL with errno 0 is end of stream"
           (attempt (funcall classify nil -1 0 syscall)) :eof)
    ;; The re-derivation. This used to be "the receive timeout fired",
    ;; which is true only on a blocking socket.
    (check "classify: SYSCALL with EAGAIN is would-block, not an error"
           (attempt (funcall classify nil -1 eagain syscall)) :again)
    ;; The one that must never be quiet.
    (check "classify: SYSCALL with ECONNRESET raises"
           (stringp (attempt (funcall classify nil -1 econnreset syscall))) t)
    ;; ATTEMPT wraps the whole expression, not just the call. A revert
    ;; that answers :EOF here would make SEARCH raise a type error
    ;; *outside* a narrower wrapper, and the run would end instead of
    ;; reporting -- which is the failure mode this suite spent a round
    ;; removing.
    (check "classify: and the message names the transport failure"
           ;; Two ATTEMPTs, and both are needed. The inner one turns the
           ;; expected raise into its text so SEARCH has something to look
           ;; at; the outer one catches SEARCH itself when a revert answers
           ;; a keyword instead, so the check fails rather than ending the
           ;; run.
           (attempt (and (search "transport error"
                                 (attempt (funcall classify nil -1 econnreset syscall)))
                         t))
           t)
    ;; Continuable codes are the caller's, and arriving here means a caller
    ;; forgot. Loud, because silence would look like a transport failure.
    (check "classify: WANT_READ here is a caller bug, and says so"
           ;; Two ATTEMPTs, and both are needed. The inner one turns the
           ;; expected raise into its text so SEARCH has something to look
           ;; at; the outer one catches SEARCH itself when a revert answers
           ;; a keyword instead, so the check fails rather than ending the
           ;; run.
           (attempt (and (search "continuable"
                                 (attempt (funcall classify nil -1 0 want-rd)))
                         t))
           t)
    (check "classify: WANT_WRITE likewise"
           ;; Two ATTEMPTs, and both are needed. The inner one turns the
           ;; expected raise into its text so SEARCH has something to look
           ;; at; the outer one catches SEARCH itself when a revert answers
           ;; a keyword instead, so the check fails rather than ending the
           ;; run.
           (attempt (and (search "continuable"
                                 (attempt (funcall classify nil -1 0 want-wr)))
                         t))
           t)
    ;; The blocking wrapper differs in exactly one place.
    (check "blocking: EAGAIN is the receive timeout, and raises"
           ;; Two ATTEMPTs, and both are needed. The inner one turns the
           ;; expected raise into its text so SEARCH has something to look
           ;; at; the outer one catches SEARCH itself when a revert answers
           ;; a keyword instead, so the check fails rather than ending the
           ;; run.
           (attempt (and (search "timed out"
                                 (attempt (funcall blocking nil -1 eagain syscall)))
                         t))
           t)
    (check "blocking: end of stream still passes through"
           (attempt (funcall blocking nil -1 0 syscall)) :eof)
    (check "blocking: and a transport failure is still loud"
           ;; Two ATTEMPTs, and both are needed. The inner one turns the
           ;; expected raise into its text so SEARCH has something to look
           ;; at; the outer one catches SEARCH itself when a revert answers
           ;; a keyword instead, so the check fails rather than ending the
           ;; run.
           (attempt (and (search "transport error"
                                 (attempt (funcall blocking nil -1 econnreset syscall)))
                         t))
           t)))

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
  (check "tls-outbound-setup-fn set"
         (not (null (symbol-value (tls-sym "*TLS-OUTBOUND-SETUP-FN*")))) t)
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
