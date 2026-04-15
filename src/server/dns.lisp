(in-package :web-skeleton)

;;; RESOLVE-HOST-BLOCKING uses *FETCH-TIMEOUT* (defined in fetch.lisp,
;;; which loads before this file) as the subprocess deadline. Forward-
;;; declared special so the compiler doesn't warn on first-compile
;;; orderings where fetch.lisp isn't yet in the image.
(declaim (special *fetch-timeout*))

;;; ===========================================================================
;;; Async DNS resolution via a getent subprocess
;;;
;;; Hostname resolution happens out-of-band in a `getent ahosts` subprocess
;;; whose stdout pipe is registered with the event loop's epoll fd. The
;;; connection state machine gains a :out-dns state for the duration of
;;; the lookup. When the pipe becomes readable (getent wrote something or
;;; exited), we drain and parse the first STREAM line's address, clean up
;;; the process, and resume via INITIATE-HTTP-FETCH-TO-ADDRESS.
;;;
;;; Why getent + subprocess instead of a pure-Lisp DNS client: semantic
;;; parity with blocking GET-HOST-BY-NAME. /etc/hosts, NSS modules,
;;; Docker's embedded DNS, mDNS, LDAP, nsswitch.conf — whatever the
;;; system knows, we inherit automatically. The cost is one fork+exec
;;; per lookup (~ms), which is dominated by the DNS round trip itself
;;; even on cache hits.
;;;
;;; Why a subprocess instead of GET-HOST-BY-NAME in a thread:
;;; SB-EXT:RUN-PROGRAM is fork+exec. No Lisp code runs in the child
;;; between fork and exec, so the "don't fork from a multi-threaded
;;; SBCL" hazard is not in play — the child is a fresh process with
;;; fresh memory and fresh locks.
;;;
;;; IPv4 and IPv6: `getent ahosts` returns both families in the
;;; system's preferred order, already filtered by AI_ADDRCONFIG so
;;; addresses for an unreachable family never appear. First STREAM
;;; line wins. The socket family is picked to match the address.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; fd extraction from the SBCL stream
;;;
;;; sb-impl::fd-stream-fd is the internal reader for the struct slot.
;;; Using it directly via double-colon access is the stable pattern —
;;; sb-sys:fd-stream-fd is not always exported in every SBCL version.
;;; ---------------------------------------------------------------------------

(defun %process-output-fd (process)
  "Return the integer file descriptor of PROCESS's stdout stream."
  (sb-impl::fd-stream-fd (sb-ext:process-output process)))

;;; ---------------------------------------------------------------------------
;;; Parsing getent ahosts output
;;;
;;; Line format:
;;;   <address> STREAM [<hostname>]
;;;   <address> DGRAM
;;;   <address> RAW
;;; We only accept STREAM rows; DGRAM and RAW are redundant for our
;;; TCP use case and pollute the answer set. First parseable STREAM
;;; line wins — order respects system preference.
;;; ---------------------------------------------------------------------------

(defun parse-getent-output (buf end)
  "Scan BUF[0..END) for the first `<address> STREAM ...` line with a
   parseable IPv4 or IPv6 address. Returns (IP . FAMILY) where FAMILY
   is :INET or :INET6, or NIL if no complete parseable line yet.
   Safe to call on partial buffers — returns NIL until at least one
   line with a newline terminator has been received."
  (let ((line-start 0))
    (loop while (< line-start end) do
      (let ((lf (position 10 buf :start line-start :end end)))
        (unless lf (return nil))
        (let ((line (handler-case
                        (sb-ext:octets-to-string
                         buf :start line-start :end lf
                             :external-format :ascii)
                      (error () nil))))
          (when (and line (search " STREAM" line))
            (let* ((sp (position #\Space line))
                   (addr-str (and sp (subseq line 0 sp))))
              (when addr-str
                (let ((v4 (parse-ipv4-literal addr-str)))
                  (when v4 (return (cons v4 :inet))))
                (let ((v6 (parse-ipv6-literal addr-str)))
                  (when v6 (return (cons v6 :inet6))))))))
        (setf line-start (1+ lf))))))

;;; ---------------------------------------------------------------------------
;;; Kick off a lookup
;;;
;;; The process-reaping helper MAYBE-REAP-DNS-PROCESS lives in
;;; src/server/connection.lisp so that both CLOSE-OUTBOUND (fetch.lisp,
;;; loads before this file) and CLOSE-CONNECTION (main.lisp, loads after)
;;; can call it without a forward reference.
;;; ---------------------------------------------------------------------------

(defun initiate-dns-lookup (conn epoll-fd fetch-req host port path)
  "Spawn `getent ahosts HOST`, register its stdout pipe with epoll, park
   CONN behind a new outbound :out-dns connection carrying the DNS-THEN
   closure. When the pipe becomes readable, HANDLE-DNS-READY parses the
   output and fires DNS-THEN, which opens the TCP socket and transitions
   to :out-connecting — the existing outbound flow takes over from there."
  ;; -- terminates getent's option parsing so a hostname that begins
  ;; with '-' cannot be misread as a flag. Numeric IP literals never
  ;; reach this path (parse-ipv4-literal / parse-ipv6-literal catch
  ;; them earlier) so this is strictly a habit-of-care hardening.
  (let ((process (sb-ext:run-program "getent"
                                      (list "ahosts" "--" host)
                                      :output :stream
                                      :wait nil
                                      :search t)))
    (unless process
      (error "dns: failed to spawn getent for ~a" host))
    (let ((dns-conn nil)
          (success nil))
      (unwind-protect
           (let ((out-fd (%process-output-fd process)))
             (setf dns-conn
                   (make-connection
                    :fd out-fd
                    :state :out-dns
                    :outbound-p t
                    :inbound-fd (connection-fd conn)
                    :dns-process process
                    ;; Carry the fetch callback on the dns-conn so
                    ;; close-outbound fires it on any DNS teardown
                    ;; path that doesn't chain to a successful
                    ;; initiate-http-fetch-to-address. The success
                    ;; path in handle-dns-ready explicitly clears
                    ;; this slot before calling close-outbound so
                    ;; the callback isn't fired twice when dns-then
                    ;; creates a fresh outbound with the same
                    ;; callback attached.
                    :fetch-callback (http-fetch-continuation-callback fetch-req)
                    :dns-then (lambda (ip family)
                                (initiate-http-fetch-to-address
                                 conn epoll-fd fetch-req
                                 host port path ip family))
                    :last-active (get-universal-time)))
             (set-nonblocking out-fd)
             (register-connection dns-conn)
             (epoll-add epoll-fd out-fd (logior +epollin+ +epollet+))
             (setf (connection-state conn) :awaiting
                   (connection-awaiting-fd conn) out-fd)
             (log-debug "dns: fd ~d -> getent ahosts ~a (pipe fd ~d)"
                        (connection-fd conn) host out-fd)
             (setf success t))
        ;; Error path: partial setup. Unregister the dns-conn if we
        ;; created it, reap the getent child, let the outer error
        ;; handler in initiate-fetch deliver the 502.
        (unless success
          (when dns-conn
            (ignore-errors
             (epoll-remove epoll-fd (connection-fd dns-conn)))
            (unregister-connection dns-conn))
          (ignore-errors
           (when (sb-ext:process-alive-p process)
             (sb-ext:process-kill process 9)))
          (ignore-errors (sb-ext:process-close process)))))))

;;; ---------------------------------------------------------------------------
;;; Completion paths
;;; ---------------------------------------------------------------------------

(defun deliver-dns-error (dns-conn epoll-fd)
  "DNS lookup failed — no usable address, getent gave up, or a parse
   error. Reply to the parked inbound with a 502 and tear down the
   dns-conn through CLOSE-OUTBOUND, which fires the fetch callback
   with (NIL NIL NIL) so app-level cleanup runs."
  (let ((inbound-fd (connection-inbound-fd dns-conn)))
    (close-outbound dns-conn epoll-fd)
    (let ((inbound (lookup-connection inbound-fd)))
      (when inbound
        (let ((err-bytes (format-response (make-error-response 502))))
          (connection-queue-write inbound err-bytes)
          (setf (connection-state inbound) :write-response
                (connection-awaiting-fd inbound) -1
                (connection-last-active inbound) (get-universal-time))
          (epoll-modify epoll-fd (connection-fd inbound)
                        (logior +epollout+ +epollet+)))))))

(defun handle-dns-ready (dns-conn epoll-fd)
  "Called from HANDLE-OUTBOUND-EVENT when epoll reports readability
   (EPOLLIN or EPOLLHUP) on a :out-dns pipe fd. Drain what is
   available, try to parse an address, fire DNS-THEN on success,
   deliver a 502 on EOF-without-answer."
  (let ((result (connection-read-available dns-conn)))
    (case result
      ((:ok :eof)
       (let ((parsed (parse-getent-output
                      (connection-read-buf dns-conn)
                      (connection-read-pos dns-conn))))
         (cond
           (parsed
            (let ((dns-then (connection-dns-then dns-conn)))
              ;; DNS succeeded — dns-then will chain to
              ;; initiate-http-fetch-to-address which creates a
              ;; fresh outbound carrying the same fetch callback.
              ;; Clear the slot on the dns-conn so close-outbound
              ;; doesn't fire the callback here (that would be a
              ;; double-fire against the fresh outbound's own
              ;; teardown path).
              (setf (connection-fetch-callback dns-conn) nil)
              (close-outbound dns-conn epoll-fd)
              (destructuring-bind (ip . family) parsed
                (funcall dns-then ip family))))
           ((eq result :eof)
            (log-warn "dns: no usable address in getent output")
            (deliver-dns-error dns-conn epoll-fd))
           ;; :OK and incomplete — next epoll wake will bring more.
           )))
      (:again nil)
      (:full
       (log-warn "dns: getent output exceeded read buffer")
       (deliver-dns-error dns-conn epoll-fd)))))

;;; ---------------------------------------------------------------------------
;;; Synchronous resolver for blocking fetch paths
;;;
;;; FETCH-STREAM-PLAIN and TLS-CONNECT are blocking by construction —
;;; the whole request/response lifecycle holds the worker thread. They
;;; cannot park in epoll the way the async HTTP path does. But they
;;; should still share the same resolver so semantics match: /etc/hosts,
;;; NSS, Docker's embedded DNS, mDNS — whatever the system knows, both
;;; blocking and async fetches inherit automatically. RESOLVE-HOST-BLOCKING
;;; spawns the same `getent ahosts` subprocess and drains its output
;;; synchronously via SB-EXT:PROCESS-WAIT. Numeric literals short-circuit
;;; the subprocess via the same parser as the async path.
;;; ---------------------------------------------------------------------------

(defun resolve-host-blocking (host)
  "Synchronously resolve HOST via `getent ahosts`.
   Returns (values IP FAMILY) on success or NIL on failure. FAMILY is
   :INET or :INET6. Numeric IPv4 and IPv6 literals skip the subprocess
   via the same fast path as the async resolver. Used by the blocking
   fetch setup paths so there is a single DNS primitive across the
   framework.

   Bounded by *FETCH-TIMEOUT* via a deadline poll: the subprocess is
   spawned with :WAIT NIL, the caller sleeps in short slices until
   process exit or deadline, and we SIGKILL the child on expiry. A
   libc resolver hang (unresponsive nameserver, slow NSS module, hung
   mDNS responder) no longer pins the worker thread indefinitely —
   the promise that *FETCH-TIMEOUT* covers each of DNS, connect, and
   I/O on the blocking path is now actually kept."
  (let ((v4 (parse-ipv4-literal host)))
    (when v4 (return-from resolve-host-blocking (values v4 :inet))))
  (let ((v6 (parse-ipv6-literal host)))
    (when v6 (return-from resolve-host-blocking (values v6 :inet6))))
  (handler-case
      (let ((process (sb-ext:run-program "getent"
                                          (list "ahosts" "--" host)
                                          :output :stream
                                          :wait nil
                                          :search t)))
        (unless process
          (return-from resolve-host-blocking nil))
        (unwind-protect
             (let ((deadline (+ (get-internal-real-time)
                                (* *fetch-timeout*
                                   internal-time-units-per-second))))
               ;; Wait for exit, sliced so shutdown stays responsive.
               (loop until (or (not (sb-ext:process-alive-p process))
                               (>= (get-internal-real-time) deadline))
                     do (sleep 0.05))
               (cond
                 ;; Timed out — kill the child and report no result.
                 ((sb-ext:process-alive-p process)
                  (ignore-errors (sb-ext:process-kill process 9))
                  (ignore-errors (sb-ext:process-wait process))
                  nil)
                 ;; Exited non-zero — unresolved.
                 ((not (zerop (sb-ext:process-exit-code process)))
                  nil)
                 ;; Exited clean — drain stdout and parse.
                 (t
                  (let* ((stream (sb-ext:process-output process))
                         (buf (make-array 1024
                                          :element-type '(unsigned-byte 8)
                                          :fill-pointer 0 :adjustable t)))
                    (loop for byte = (read-byte stream nil nil)
                          while byte
                          do (vector-push-extend byte buf))
                    (let ((parsed (parse-getent-output buf (length buf))))
                      (when parsed
                        (values (car parsed) (cdr parsed))))))))
          (ignore-errors (sb-ext:process-close process))))
    (error () nil)))

;;; ---------------------------------------------------------------------------
;;; Registration
;;;
;;; Install our dispatchers into fetch.lisp's hook slots so initiate-
;;; http-fetch, fetch-stream-plain, and tls-connect can reach us without
;;; compile-time forward references. The hook pattern mirrors src/tls.lisp's
;;; registration of *HTTPS-FETCH-FN*.
;;; ---------------------------------------------------------------------------

(eval-when (:load-toplevel :execute)
  (setf *dns-lookup-fn*           #'initiate-dns-lookup
        *handle-dns-ready-fn*     #'handle-dns-ready
        *dns-resolve-blocking-fn* #'resolve-host-blocking))
