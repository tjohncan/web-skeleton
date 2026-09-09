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
;;; sb-sys:fd-stream-fd has been an external symbol on SBCL 2.2+
;;; (the framework targets modern SBCL on Linux). Previous code here
;;; reached into sb-impl:: for the struct-slot reader; that works
;;; but uses an internal symbol that has no export contract and
;;; could be renamed across releases. sb-sys: is the stable path.
;;; ---------------------------------------------------------------------------

(defun %process-output-fd (process)
  "Return the integer file descriptor of PROCESS's stdout stream."
  (sb-sys:fd-stream-fd (sb-ext:process-output process)))

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

(defun %unspecified-address-p (bytes)
  "T if BYTES are all zero — the unspecified IPv4 0.0.0.0 or IPv6 ::.
   Meaningless as a destination: 0.0.0.0 is routed to loopback on
   Linux, :: is typically rejected with EADDRNOTAVAIL. Either way,
   spending a connect syscall on it is wasted work."
  (every #'zerop bytes))

(defun parse-getent-output (buf end &optional host)
  "Scan BUF[0..END) for the first `<address> STREAM ...` line with a
   parseable IPv4 or IPv6 address that policy accepts. Returns
   (IP . FAMILY) where FAMILY is :INET or :INET6, or NIL if no complete
   parseable line yet. Safe to call on partial buffers — returns NIL
   until at least one line with a newline terminator has been received.

   Token boundary: STREAM must appear as the token immediately after
   the address, not as a substring anywhere in the line. A substring
   match (`(search \" STREAM\" line)`) would classify a DGRAM row
   whose hostname happens to contain ' STREAM' ('127.0.0.1 DGRAM
   my.STREAM.example') as a STREAM match.

   Rejects the unspecified addresses (0.0.0.0, ::) — dialing them is
   meaningless and some systems quietly route 0.0.0.0 to loopback.

   HOST is the name being resolved; it is used only by the address
   filter and its log line. Every candidate address is gated on
   *FETCH-ADDRESS-FILTER* before selection. This is the point at which
   an address gets *chosen*, so a refusal here means a multi-homed name
   whose first address is refused simply falls through to the next one,
   and a name with no acceptable address returns NIL — indistinguishable
   to every caller from a name that did not resolve. Gating here rather
   than at the connect syscall introduces no new failure mode and keeps
   the fallback behavior for free."
  (let ((line-start 0))
    (loop while (< line-start end) do
      (let ((lf (position 10 buf :start line-start :end end)))
        (unless lf (return nil))
        (let ((line (handler-case
                        (sb-ext:octets-to-string
                         buf :start line-start :end lf
                             :external-format :ascii)
                      (error () nil))))
          (when line
            (let* ((sp (position #\Space line))
                   (addr-str (and sp (subseq line 0 sp)))
                   ;; First non-whitespace token after the address.
                   (tok-start
                    (and sp (position-if-not
                              (lambda (c)
                                (or (char= c #\Space) (char= c #\Tab)))
                              line :start sp)))
                   (stream-token-p
                    (and tok-start
                         (>= (- (length line) tok-start) 6)
                         (string= line "STREAM"
                                  :start1 tok-start
                                  :end1 (+ tok-start 6))
                         (or (= (length line) (+ tok-start 6))
                             (let ((after (char line (+ tok-start 6))))
                               (or (char= after #\Space)
                                   (char= after #\Tab)))))))
              (when (and stream-token-p addr-str)
                (let ((v4 (parse-ipv4-literal addr-str)))
                  (when (and v4 (not (%unspecified-address-p v4))
                             (fetch-address-allowed-p
                              v4 :inet (or host addr-str)))
                    (return (cons v4 :inet))))
                (let ((v6 (parse-ipv6-literal addr-str)))
                  (when (and v6 (not (%unspecified-address-p v6))
                             (fetch-address-allowed-p
                              v6 :inet6 (or host addr-str)))
                    (return (cons v6 :inet6))))))))
        (setf line-start (1+ lf))))))

;;; ---------------------------------------------------------------------------
;;; Optional per-worker resolution cache
;;;
;;; Every hostname fetch otherwise costs a fork+exec of getent (~ms). For
;;; an app that pounds one upstream, that is a subprocess per request for
;;; an answer that did not change.
;;;
;;; The framework supplies the mechanism and the app supplies the policy:
;;; getent reports no TTL, so *there is no honest expiry the framework
;;; could pick on the app's behalf*. Hence *DNS-CACHE-TTL* defaults to 0
;;; (caching off, historical behavior, every fetch re-resolves) and an app
;;; that wants caching names the number of seconds it is willing to trust
;;; a resolution for. That number is a real trade — how fast an upstream's
;;; failover must be noticed — and it belongs to whoever owns the upstream.
;;;
;;; The cache is per-worker: workers share nothing in the hot path, so
;;; each keeps its own table and no lock is needed. A worker's first fetch
;;; to a host pays the subprocess; the rest of that TTL window does not.
;;; ---------------------------------------------------------------------------

(defparameter *dns-cache-ttl* 0
  "Seconds a successful hostname → address resolution is cached per
   worker. 0 (the default) disables caching entirely: every fetch to a
   hostname re-runs getent, which is the framework's historical behavior
   and the only one that is correct without knowing the app's tolerance
   for stale addresses.

   Set it to the number of seconds you are willing to keep dialing a
   remembered address after DNS has changed. Small values (30-60) suit an
   upstream behind a load balancer that can fail over; larger values suit
   a pinned host. getent surfaces no TTL, so the framework cannot infer
   this — that is exactly why the default is off rather than some invented
   number.

   The cache holds successes only, and never outlives a policy change:
   *FETCH-ADDRESS-FILTER* is re-consulted on every cache hit, so a cached
   address cannot become a way around the filter.")

(defparameter *dns-cache-max-entries* 256
  "Maximum hostnames cached per worker. On overflow, expired entries are
   swept and the table is cleared if that is not enough. The cache is a
   latency optimization, not a source of truth — dropping it costs one
   getent per host — and an unbounded table would let a handler that
   fetches attacker-chosen hostnames grow a worker's memory without
   limit.")

(defvar *dns-cache* nil
  "Per-worker hostname → DNS-CACHE-ENTRY table, bound by RUN-WORKER
   alongside *CONNECTIONS*. NIL outside a worker thread (a REPL, a test),
   which simply means no caching happens there — every cache operation
   no-ops on a NIL table rather than reaching for a global.")

(defstruct (dns-cache-entry
            (:constructor make-dns-cache-entry (ip family expires-at)))
  (ip     nil)
  (family :inet :type keyword)
  (expires-at 0 :type integer))

(defun dns-cache-lookup (host)
  "Return (values IP FAMILY) for HOST from the per-worker cache, or NIL on
   a miss, an expired entry, a disabled cache, or a thread with no cache
   bound.

   A hit is re-gated on *FETCH-ADDRESS-FILTER* before it is handed back.
   This is the load-bearing part: without it, caching would be a DNS-
   rebinding accelerator — an address admitted while no filter was
   installed, or under a policy that has since changed, would keep being
   dialed from memory with nothing left to refuse it. Re-running a cheap
   predicate costs nothing next to the connect that follows. A now-refused
   entry is dropped rather than merely skipped, so the filter does not get
   re-invoked on it for the rest of its TTL."
  (when (and *dns-cache* (> *dns-cache-ttl* 0))
    (let ((entry (gethash host *dns-cache*)))
      (when entry
        (cond
          ((>= (get-universal-time) (dns-cache-entry-expires-at entry))
           (remhash host *dns-cache*)
           nil)
          ((not (fetch-address-allowed-p (dns-cache-entry-ip entry)
                                         (dns-cache-entry-family entry)
                                         host))
           (remhash host *dns-cache*)
           nil)
          (t
           (log-debug "dns: cache hit ~a -> ~a" host
                      (format-ip (dns-cache-entry-ip entry)))
           (values (dns-cache-entry-ip entry)
                   (dns-cache-entry-family entry))))))))

(defun dns-cache-store (host ip family)
  "Record HOST → IP/FAMILY for *DNS-CACHE-TTL* seconds. No-op when
   caching is disabled or no cache is bound.

   Successful resolutions only. A failed lookup is usually transient — a
   nameserver blip, a service still coming up — and caching the failure
   would stretch an outage well past its cause."
  (when (and *dns-cache* (> *dns-cache-ttl* 0))
    (when (>= (hash-table-count *dns-cache*) *dns-cache-max-entries*)
      (let ((now (get-universal-time))
            (dead nil))
        (maphash (lambda (k v)
                   (when (>= now (dns-cache-entry-expires-at v))
                     (push k dead)))
                 *dns-cache*)
        (dolist (k dead) (remhash k *dns-cache*))
        (when (>= (hash-table-count *dns-cache*) *dns-cache-max-entries*)
          (log-debug "dns: cache full (~d), clearing"
                     (hash-table-count *dns-cache*))
          (clrhash *dns-cache*))))
    (setf (gethash host *dns-cache*)
          (make-dns-cache-entry ip family
                                (+ (get-universal-time) *dns-cache-ttl*)))
    (log-debug "dns: cached ~a -> ~a for ~ds"
               host (format-ip ip) *dns-cache-ttl*)))

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
   to :out-connecting — the existing outbound flow takes over from there.

   A per-worker cache hit (opt-in, see *DNS-CACHE-TTL*) skips all of that:
   no subprocess, no :out-dns connection, no epoll registration — straight
   to the TCP phase on the remembered address, exactly as a numeric literal
   would. The hit has already been re-gated on *FETCH-ADDRESS-FILTER* by
   DNS-CACHE-LOOKUP."
  (multiple-value-bind (cached-ip cached-family) (dns-cache-lookup host)
    (when cached-ip
      (return-from initiate-dns-lookup
        (initiate-http-fetch-to-address conn epoll-fd fetch-req
                                        host port path
                                        cached-ip cached-family))))
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
                    ;; The sink travels with the fetch from its first
                    ;; frame, for the reason SCHEME travels on the
                    ;; continuation: the DNS phase is the one stretch of a
                    ;; fetch's life that lives on a connection of its own,
                    ;; and every path that has to find it again keys on
                    ;; this slot — SWEEP-IDLE-CONNECTIONS' detached reap,
                    ;; CLOSE-CONNECTION's orphan walk when the target goes
                    ;; first, and DELIVER-FETCH-ERROR's choice of ending.
                    ;; Left at the :INBOUND default, a detached fetch whose
                    ;; name is still resolving is reachable by none of
                    ;; them: the getent child and its pipe outlive the
                    ;; connection they were opened for.
                    :fetch-sink (http-fetch-continuation-sink fetch-req)
                    ;; The DNS phase gets its own *FETCH-TIMEOUT*, and the
                    ;; TCP phase gets another when INITIATE-HTTP-FETCH-TO-
                    ;; ADDRESS starts one, so a detached fetch to a name is
                    ;; bounded at twice the parked path's budget rather
                    ;; than at one. Stated rather than threaded through a
                    ;; new continuation slot: the parked path has the
                    ;; :AWAITING sweep over the whole exchange and a
                    ;; detached one has nothing at all, so what matters
                    ;; here is that it is bounded.
                    :fetch-started-at (get-universal-time)
                    :fetch-deadline (+ (get-universal-time) *fetch-timeout*)
                    ;; Carried so HANDLE-DNS-READY can name the host when
                    ;; it runs the getent output past the address filter.
                    :dns-host host
                    :dns-then (lambda (ip family)
                                (initiate-http-fetch-to-address
                                 conn epoll-fd fetch-req
                                 host port path ip family))
                    :last-active (get-universal-time)))
             (set-nonblocking out-fd)
             (register-connection dns-conn)
             (epoll-add epoll-fd out-fd (logior +epollin+ +epollet+))
             ;; Only an :INBOUND fetch parks, exactly as in
             ;; INITIATE-HTTP-FETCH-TO-ADDRESS — the two are the same
             ;; decision made at two points on one path, and this one used
             ;; to make it unconditionally.
             ;;
             ;; A detached fetch is dialing on behalf of a connection the
             ;; application already owns and is still writing to. Moving
             ;; that connection to :AWAITING takes its state away, and
             ;; nothing hands it back: DNS resumes through
             ;; INITIATE-HTTP-FETCH-TO-ADDRESS, whose :DETACHED arm
             ;; correctly touches no state at all. So the connection stayed
             ;; :AWAITING for the rest of its life — STREAM-SEND signalling
             ;; on it, WEBSOCKET-ON-READ never running for it, and the
             ;; :AWAITING sweep serializing a whole HTTP/1.1 504 into the
             ;; middle of an established chunked body a *FETCH-TIMEOUT*
             ;; later. The IP-literal fast path escaped it only because it
             ;; skips this function.
             (ecase (http-fetch-continuation-sink fetch-req)
               (:inbound
                (setf (connection-state conn) :awaiting
                      (connection-awaiting-fd conn) out-fd))
               (:detached nil))
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
   error. Ends the fetch the way every other outbound failure ends: the
   parked inbound is answered 502, a detached one gets its callback's
   abort sentinel and DELIVER-DETACHED's disposition, and CLOSE-OUTBOUND
   reaps the getent child either way.

   Delegates rather than restating. The two were the same six lines, and
   the copy here had drifted: it knew only about a parked inbound, so a
   FETCH-INTO to a name that would not resolve left the target's
   FETCH-OUTSTANDING set for the rest of that connection's life — refusing
   every later fetch on it — and never applied the failure disposition the
   caller chose. A dns-conn is an outbound connection like any other; what
   is particular about its failure is only where in the lookup it
   happened, and all three of its callers already log that."
  (deliver-fetch-error dns-conn epoll-fd "DNS lookup failed"))

(defun handle-dns-ready (dns-conn epoll-fd)
  "Called from HANDLE-OUTBOUND-EVENT when epoll reports readability
   (EPOLLIN or EPOLLHUP) on a :out-dns pipe fd. Drain what is
   available, try to parse an address, fire DNS-THEN on success,
   deliver a 502 on EOF-without-answer."
  (let ((result (connection-read-available dns-conn)))
    (case result
      ((:ok :ok-eof :eof)
       (let ((parsed (parse-getent-output
                      (connection-read-buf dns-conn)
                      (connection-read-pos dns-conn)
                      (connection-dns-host dns-conn))))
         (cond
           (parsed
            (let ((dns-then (connection-dns-then dns-conn))
                  (chained nil))
              ;; CHAINED is what separates "the lookup failed" from "the
              ;; lookup succeeded and tidying up after it did not", and the
              ;; two stopped being the same question when DELIVER-DNS-ERROR
              ;; started delegating. It used to know only about a parked
              ;; inbound, so a raise from the cleanup below found no
              ;; :AWAITING match and did nothing. Now it reaches
              ;; DELIVER-DETACHED with :ABORTED — which would apply the
              ;; target's failure disposition, closing a WebSocket or
              ;; abandoning a stream, while the outbound DNS-THEN just
              ;; opened is alive and still running the fetch it was asked
              ;; for. Delegation is what made a failure to close a pipe
              ;; into a failure of the fetch.
              (handler-case
                  (destructuring-bind (ip . family) parsed
                    ;; Remember the resolution before chaining to TCP: the
                    ;; name resolved, which is true regardless of whether
                    ;; the connect that follows succeeds. PARSE-GETENT-
                    ;; OUTPUT already ran this address past the address
                    ;; filter, so nothing refused ever enters the cache.
                    (dns-cache-store (connection-dns-host dns-conn) ip family)
                    (funcall dns-then ip family)
                    (setf chained t))
                (error (e)
                  ;; dns-then failed — callback still on dns-conn, so
                  ;; close-outbound fires the cleanup sentinel.
                  (log-warn "dns: chain to TCP failed: ~a" e)
                  (deliver-dns-error dns-conn epoll-fd)))
              (when chained
                ;; The new outbound carries the callback now. Clear it here
                ;; so close-outbound doesn't double-fire.
                (setf (connection-fetch-callback dns-conn) nil)
                ;; Logged, not delivered. Everything this fetch needs has
                ;; already moved to the new outbound; what is left is a
                ;; resolved pipe and its child. Failing to reap them leaks
                ;; an fd, which is worth a line in the log and is not worth
                ;; ending a live fetch over.
                (handler-case (close-outbound dns-conn epoll-fd)
                  (error (e)
                    (log-warn "dns: closing the resolved pipe on fd ~d ~
                               failed: ~a"
                              (connection-fd dns-conn) e))))))
           ((member result '(:eof :ok-eof))
            ;; No parseable STREAM row, or every address the name
            ;; resolved to was refused by *FETCH-ADDRESS-FILTER*. Both
            ;; are "no address we are willing to dial" — same 502.
            ;;
            ;; :OK-EOF used to be missing here, and its absence was a
            ;; race rather than a certainty — which is why the branch
            ;; mostly worked and the bug was hard to see.
            ;;
            ;; getent writes its output in one go and the EOF appears
            ;; when it exits. Usually it has not exited by the time we
            ;; drain, so the bytes come back :OK, the exit arrives as a
            ;; later event, and this branch fires on a clean :EOF — which
            ;; is the common ordering and the reason the old code was
            ;; right most of the time. When getent has already exited,
            ;; both land in one read; reported as :OK, that fell straight
            ;; past here to wait for a wake-up the closed pipe would
            ;; never deliver, and the parked inbound sat until the
            ;; sweeper took it.
            ;;
            ;; So neither arm is dead. :EOF is the common path and
            ;; :OK-EOF is the coalesced one, and the branch has to accept
            ;; both because which one arrives is not ours to decide.
            (log-warn "dns: no usable address for ~a in getent output"
                      (or (connection-dns-host dns-conn) "<host>"))
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
  ;; Literal fast paths are gated on *FETCH-ADDRESS-FILTER* for the same
  ;; reason INITIATE-HTTP-FETCH's are: they skip DNS, so a resolver-only
  ;; check would let https://169.254.169.254/ straight through. A refused
  ;; literal returns NIL — the same "did not resolve" answer every caller
  ;; already handles (raise → 502 + cleanup sentinel).
  (let ((v4 (parse-ipv4-literal host)))
    (when v4
      (return-from resolve-host-blocking
        (when (fetch-address-allowed-p v4 :inet host)
          (values v4 :inet)))))
  (let ((v6 (parse-ipv6-literal host)))
    (when v6
      (return-from resolve-host-blocking
        (when (fetch-address-allowed-p v6 :inet6 host)
          (values v6 :inet6)))))
  ;; Per-worker cache (opt-in — see *DNS-CACHE-TTL*). A hit skips the
  ;; subprocess and its deadline poll entirely. This is the only way an
  ;; HTTPS fetch can avoid the fork+exec: PARSE-URL refuses https:// with
  ;; an IP-literal host, so an app cannot hand us a pre-resolved address
  ;; the way it can for plain HTTP.
  (multiple-value-bind (cached-ip cached-family) (dns-cache-lookup host)
    (when cached-ip
      (return-from resolve-host-blocking (values cached-ip cached-family))))
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
                          do (when (>= (fill-pointer buf) 8192)
                               (error "dns: getent output exceeds 8KB"))
                             (vector-push-extend byte buf))
                    (let ((parsed (parse-getent-output buf (length buf) host)))
                      (when parsed
                        (dns-cache-store host (car parsed) (cdr parsed))
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
