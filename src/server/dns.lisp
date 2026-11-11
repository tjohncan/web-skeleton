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
;;; exited), we drain it, and once getent's output has ended we parse its
;;; STREAM lines' addresses, clean up the process, and resume via
;;; INITIATE-HTTP-FETCH-TO-ADDRESS.
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
;;; system's preferred order (RFC 6724). AI_ADDRCONFIG drops a family
;;; only when this host has no address of it configured; it knows
;;; nothing of whether the destination answers on that family. So every
;;; STREAM address the address filter accepts is kept, in that order. The
;;; first is dialed, and a connect that is refused, unreachable, or
;;; unanswered for *CONNECT-FALLBACK-DELAY* gives way to the next
;;; (FALL-BACK-TO-NEXT-ADDRESS). A name that sorts an IPv6 address first,
;;; localhost as ::1, still reaches a service bound to IPv4 alone. The
;;; socket family is picked to match each address.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; fd extraction from the SBCL stream
;;;
;;; sb-sys:fd-stream-fd is an external symbol on SBCL 2.2+ (the
;;; framework targets modern SBCL on Linux). The struct-slot reader
;;; in sb-impl:: works too, but it is an internal symbol with no
;;; export contract and could be renamed across releases. sb-sys: is
;;; the stable path.
;;; ---------------------------------------------------------------------------

(defun %process-output-fd (process)
  "Return the integer file descriptor of PROCESS's stdout stream."
  (sb-sys:fd-stream-fd (sb-ext:process-output process)))

(defun spawn-getent (host)
  "Start `getent ahosts -- HOST` with its output on a pipe, and return the
   process without waiting for it, as SB-EXT:RUN-PROGRAM does.

   The one place a lookup's child is started, for the parked path and the
   blocking one alike, so what is asked of getent is said once.

   The pipe carries getent's standard error along with its output, which
   is what RUN-PROGRAM gives a child unless told otherwise. A complaint
   on it is not an address line, and PARSE-GETENT-OUTPUT passes over it;
   and the pipe ends when getent holds neither, which is when it exits.

   The -- ends getent's option parsing, so a hostname that begins with
   '-' cannot be read as a flag. Numeric literals never get this far,
   PARSE-IPV4-LITERAL and PARSE-IPV6-LITERAL taking them first, so it is a
   habit of care more than a hole closed."
  (sb-ext:run-program "getent" (list "ahosts" "--" host)
                      :output :stream
                      :wait nil
                      :search t))

;;; ---------------------------------------------------------------------------
;;; Parsing getent ahosts output
;;;
;;; Line format:
;;;   <address> STREAM [<hostname>]
;;;   <address> DGRAM
;;;   <address> RAW
;;; We only accept STREAM rows; DGRAM and RAW are redundant for our
;;; TCP use case and pollute the answer set. STREAM lines are kept in the
;;; order printed, which is the system's preference, the first dialed and
;;; the rest fallen back on.
;;; ---------------------------------------------------------------------------

(defun %unspecified-address-p (bytes)
  "T if BYTES are all zero — the unspecified IPv4 0.0.0.0 or IPv6 ::.
   Meaningless as a destination: 0.0.0.0 is routed to loopback on
   Linux, :: is typically rejected with EADDRNOTAVAIL. Either way,
   spending a connect syscall on it is wasted work."
  (every #'zerop bytes))

(defun parse-getent-output (buf end &optional host)
  "Scan BUF[0..END) for `<address> STREAM ...` lines with a parseable IPv4
   or IPv6 address that policy accepts. Returns (IP . FAMILY) for the first,
   where FAMILY is :INET or :INET6, or NIL if no complete parseable line
   yet. Safe to call on partial buffers: only lines ended by a newline are
   read, so it returns NIL until one has been received.

   Third value: every accepted address, (IP . FAMILY) in the order getent
   printed them, the first included and each once. A connect that fails
   moves on to the next of them (FALL-BACK-TO-NEXT-ADDRESS), so a caller
   that wants them all waits for getent's output to end.

   Second value: how many parseable addresses policy refused, each counted
   once. NIL with a positive count is a policy decision; NIL with zero is a
   name that did not resolve. Callers must keep giving both the same answer to the client —
   see below — but they should not give an operator the same answer, and
   without the count they could not tell the two apart.

   Token boundary: STREAM must appear as the token immediately after
   the address, not as a substring anywhere in the line. A substring
   match (`(search \" STREAM\" line)`) would classify a DGRAM row
   whose hostname happens to contain ' STREAM' ('127.0.0.1 DGRAM
   my.STREAM.example') as a STREAM match.

   Rejects the unspecified addresses (0.0.0.0, ::) — dialing them is
   meaningless and some systems quietly route 0.0.0.0 to loopback.

   HOST is the name being resolved; it is used only by the address
   filter and its log line. Every candidate address is gated on
   *FETCH-ADDRESS-FILTER* before selection, once: getent can print an
   address on more than one STREAM line, and the filter is asked about it
   the first time. This is the point at which
   an address gets *chosen*, so a refusal here means a multi-homed name
   whose first address is refused simply falls through to the next one,
   and a name with no acceptable address returns NIL — indistinguishable
   to every caller from a name that did not resolve. Gating here rather
   than at the connect syscall introduces no new failure mode and keeps
   the fallback behavior for free."
  (let ((line-start 0)
        ;; Kept so a caller can tell "nothing resolved" from "everything
        ;; that resolved was refused". Both return NIL, deliberately — the
        ;; client must not learn which — but an operator reading a log is a
        ;; different audience, and sending them to debug a resolver when
        ;; their own filter said no costs an afternoon.
        (refused '())
        (accepted '()))
    (flet ((consider (address host)
             ;; ADDRESS to the filter, unless an earlier line printed it.
             (unless (or (member address accepted :test #'equalp)
                         (member address refused :test #'equalp))
               (if (fetch-address-allowed-p (car address) (cdr address) host)
                   (push address accepted)
                   (push address refused)))))
      (loop while (< line-start end) do
        (let ((lf (position 10 buf :start line-start :end end)))
          (unless lf (loop-finish))
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
                    (when (and v4 (not (%unspecified-address-p v4)))
                      (consider (cons v4 :inet) (or host addr-str))))
                  (let ((v6 (parse-ipv6-literal addr-str)))
                    (when (and v6 (not (%unspecified-address-p v6)))
                      (consider (cons v6 :inet6) (or host addr-str))))))))
          (setf line-start (1+ lf)))
            finally (let ((all (reverse accepted)))
                      (return (values (first all) (length refused) all)))))))

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
;;; (caching off, every fetch re-resolves) and an app
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
   hostname re-runs getent, the only behavior that is correct without
   knowing the app's tolerance for stale addresses.

   Set it to the number of seconds you are willing to keep dialing a
   remembered address after DNS has changed. Small values (30-60) suit an
   upstream behind a load balancer that can fail over; larger values suit
   a pinned host. getent surfaces no TTL, so the framework cannot infer
   this — that is exactly why the default is off rather than some invented
   number.

   The cache holds successes only, and never outlives a policy change:
   *FETCH-ADDRESS-FILTER* is re-consulted on every cache hit, so a cached
   address cannot become a way around the filter.

   Whole seconds, since an entry's expiry is a time of day in seconds;
   START-SERVER refuses a fraction.")

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
            (:constructor make-dns-cache-entry (ip family expires-at
                                                &optional fallbacks)))
  (ip     nil)
  (family :inet :type keyword)
  (expires-at 0 :type integer)
  ;; The name's other addresses, (IP . FAMILY) in order, to fall back on.
  (fallbacks nil :type list))

(defun dns-cache-lookup (host)
  "Return (values IP FAMILY FALLBACKS) for HOST from the per-worker cache,
   or NIL on a miss, an expired entry, a disabled cache, or a thread with
   no cache bound. FALLBACKS are the name's other addresses that the
   filter still accepts.

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
                   (dns-cache-entry-family entry)
                   (remove-if-not (lambda (address)
                                    (fetch-address-allowed-p (car address)
                                                             (cdr address)
                                                             host))
                                  (dns-cache-entry-fallbacks entry)))))))))

(defun dns-cache-store (host ip family &optional fallbacks)
  "Record HOST → IP/FAMILY, and the name's other addresses FALLBACKS, for
   *DNS-CACHE-TTL* seconds. No-op when caching is disabled or no cache is
   bound.

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
                                (+ (get-universal-time) *dns-cache-ttl*)
                                fallbacks))
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
  (multiple-value-bind (cached-ip cached-family cached-fallbacks)
      (dns-cache-lookup host)
    (when cached-ip
      (return-from initiate-dns-lookup
        (initiate-http-fetch-to-address conn epoll-fd fetch-req
                                        host port path
                                        cached-ip cached-family
                                        :fallbacks cached-fallbacks))))
  (let ((process (spawn-getent host)))
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
                    :fetch-continuation fetch-req
                    ;; Carried so HANDLE-DNS-READY can name the host when
                    ;; it runs the getent output past the address filter.
                    :dns-host host
                    :dns-then (lambda (ip family &optional fallbacks)
                                (initiate-http-fetch-to-address
                                 conn epoll-fd fetch-req
                                 host port path ip family
                                 :fallbacks fallbacks))
                    :last-active (get-universal-time)))
             (set-nonblocking out-fd)
             (register-connection dns-conn)
             (epoll-add epoll-fd out-fd (logior +epollin+ +epollet+))
             ;; Only an :INBOUND fetch parks, exactly as in
             ;; INITIATE-HTTP-FETCH-TO-ADDRESS — the two are the same
             ;; decision made at two points on one path, and they have to
             ;; make it the same way.
             ;;
             ;; A detached fetch is dialing on behalf of a connection the
             ;; application already owns and is still writing to. Moving
             ;; that connection to :AWAITING takes its state away, and
             ;; nothing hands it back: DNS resumes through
             ;; INITIATE-HTTP-FETCH-TO-ADDRESS, whose :DETACHED arm
             ;; correctly touches no state at all. Parked here, the
             ;; connection would stay :AWAITING for the rest of its life —
             ;; STREAM-SEND signalling on it, WEBSOCKET-ON-READ never
             ;; running for it, and the :AWAITING sweep serializing a whole
             ;; HTTP/1.1 504 into the middle of an established chunked body
             ;; a *FETCH-TIMEOUT* later. An IP-literal fetch never comes
             ;; here, since its fast path skips this function.
             (ecase (http-fetch-continuation-sink fetch-req)
               (:inbound
                (setf (connection-state conn) :awaiting
                      (connection-awaiting-fd conn) out-fd))
               (:detached nil))
             (log-debug "dns: fd ~d -> getent ahosts ~a (pipe fd ~d)"
                        (connection-fd conn) host out-fd)
             (setf (http-fetch-continuation-carrier fetch-req) dns-conn
                   success t))
        ;; Error path: partial setup. Unregister the dns-conn if we
        ;; created it, release the getent child, let the outer error
        ;; handler in initiate-fetch deliver the 502.
        (unless success
          (when dns-conn
            (ignore-errors
             (epoll-remove epoll-fd (connection-fd dns-conn)))
            (unregister-connection dns-conn))
          (release-dns-process process))))))

;;; ---------------------------------------------------------------------------
;;; Completion paths
;;; ---------------------------------------------------------------------------

(defun deliver-dns-error (dns-conn epoll-fd &optional (reason "DNS lookup failed"))
  "A fetch to a name ended in its lookup — no usable address, getent gave
   up, or a parse error — or just after it, at an address the lookup gave
   that could not be dialed. Ends the fetch the way every other outbound
   failure ends: the parked inbound is answered 502, a detached one gets
   its callback's abort sentinel and DELIVER-DETACHED's disposition, and
   CLOSE-OUTBOUND reaps the getent child either way.

   REASON is what the log says ended the fetch. The default is for a
   lookup that failed; a caller whose lookup worked passes what failed
   instead, so an operator is not sent to debug a resolver that worked.

   Delegates rather than restating. A copy that knew only about a parked
   inbound would leave a FETCH-INTO to a name that does not resolve with
   the target's FETCH-OUTSTANDING set for the rest of that connection's
   life — refusing every later fetch on it — and would never apply the
   failure disposition the caller chose. A dns-conn is an outbound
   connection like any other; what is particular about its failure is
   only where in the lookup it happened, and all three of its callers
   already log that."
  (deliver-fetch-error dns-conn epoll-fd reason))

(defun handle-dns-ready (dns-conn epoll-fd)
  "Called from HANDLE-OUTBOUND-EVENT when epoll reports readability
   (EPOLLIN or EPOLLHUP) on a :out-dns pipe fd. Drain what is available,
   and once getent's output has ended, parse it: fire DNS-THEN on an
   address, or deliver a 502 without one.

   Parsed at the end and not before, so *FETCH-ADDRESS-FILTER* is asked
   about each address once a lookup. The output usually arrives in one
   read and getent's exit in another, and a parse at each would ask
   twice, with nothing to gain: an address is dialed only once all of
   them are known."
  (let ((result (connection-read-available dns-conn)))
    (case result
      ;; The output still to end: the next epoll wake brings the rest, or
      ;; the exit that ends it.
      ((:ok :again) nil)
      ;; :OK-EOF ends the output as surely as :EOF, and leaving it out
      ;; would be a race rather than a certainty — a branch that mostly
      ;; works, with a bug that is hard to see.
      ;;
      ;; getent writes its output in one go and the EOF appears when it
      ;; exits. Usually it has not exited by the time we drain, so the
      ;; bytes come back :OK, the exit arrives as a later event, and this
      ;; arm runs on a clean :EOF — the common ordering, under which an
      ;; arm taking :EOF alone is right. When getent has already exited,
      ;; both land in one read; reported as :OK, they would wait for a
      ;; wake-up the closed pipe will never deliver, and the parked inbound
      ;; would sit until the sweeper took it.
      ;;
      ;; So neither is dead. :EOF is the common path and :OK-EOF is the
      ;; coalesced one, and the arm has to take both because which one
      ;; arrives is not ours to decide.
      ((:ok-eof :eof)
       (multiple-value-bind (parsed refused all)
           (parse-getent-output
            (connection-read-buf dns-conn)
            (connection-read-pos dns-conn)
            (connection-dns-host dns-conn))
         (cond
           ;; At the end of getent's output, so the addresses to fall back
           ;; on are all there. getent writes its answer at once and ends
           ;; it by exiting, so waiting for the end costs the exit and no
           ;; more.
           (parsed
            (let ((dns-then (connection-dns-then dns-conn))
                  (chained nil))
              ;; CHAINED is what separates "the lookup failed" from "the
              ;; lookup succeeded and tidying up after it did not". The
              ;; two are different questions because DELIVER-DNS-ERROR
              ;; delegates: it reaches DELIVER-DETACHED with :ABORTED,
              ;; which would apply the target's failure disposition,
              ;; closing a WebSocket or abandoning a stream, while the
              ;; outbound DNS-THEN just opened is alive and still running
              ;; the fetch it was asked for. Without CHAINED, a failure to
              ;; close a pipe would be a failure of the fetch.
              (handler-case
                  (destructuring-bind (ip . family) parsed
                    ;; Remember the resolution before chaining to TCP: the
                    ;; name resolved, which is true regardless of whether
                    ;; the connect that follows succeeds. PARSE-GETENT-
                    ;; OUTPUT already ran these addresses past the address
                    ;; filter, so nothing refused ever enters the cache.
                    (dns-cache-store (connection-dns-host dns-conn) ip family
                                     (rest all))
                    (funcall dns-then ip family (rest all))
                    (setf chained t))
                (error (e)
                  ;; dns-then failed — callback still on dns-conn, so
                  ;; close-outbound fires the cleanup sentinel. The
                  ;; resolver worked, so the fetch ends with what failed
                  ;; after it, most often an address the kernel would not
                  ;; dial, and not with "DNS lookup failed".
                  (log-warn "dns: chain to TCP failed: ~a" e)
                  (deliver-dns-error dns-conn epoll-fd (princ-to-string e))))
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
           (t
            ;; No parseable STREAM row, or every address the name
            ;; resolved to was refused by *FETCH-ADDRESS-FILTER*. Both
            ;; are "no address we are willing to dial" — same 502.
            (if (plusp refused)
                ;; The resolver worked. Policy is what said no, and an
                ;; operator told "DNS lookup failed" here goes and debugs
                ;; their resolver instead of reading their own filter.
                (log-warn "dns: every address for ~a was refused by ~
                           *fetch-address-filter* (~d candidate~:p) — policy, ~
                           not a resolution failure"
                          (or (connection-dns-host dns-conn) "<host>") refused)
                (log-warn "dns: no usable address for ~a in getent output"
                          (or (connection-dns-host dns-conn) "<host>")))
            (deliver-dns-error dns-conn epoll-fd
                               (if (plusp refused)
                                   "address refused by policy"
                                   "DNS lookup failed"))))))
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
;;; spawns the same `getent ahosts` subprocess and reads its output to
;;; the end, waiting on the pipe with poll(2) where the async path waits
;;; on it with epoll. Numeric literals short-circuit the subprocess via
;;; the same parser as the async path.
;;; ---------------------------------------------------------------------------

(defun read-getent-output (process buffer)
  "Read PROCESS's output into BUFFER until it ends. Returns how many
   bytes that was, or NIL when the output had not ended by the time
   BUFFER was full or *FETCH-TIMEOUT* had passed.

   Read as it arrives, with poll(2) to wait on the pipe in between, so
   the wait ends when the output does. Waiting for the child to exit
   first, and reading afterwards, has two costs. The exit can only be
   asked about at intervals, and each lookup then takes an interval
   longer than getent did. And a pipe holds only so much unread, 64 KiB
   by default: a child with more than that to say would block writing
   it, never exit, and be waited on for the whole timeout.

   NIL once BUFFER is full, the end not yet seen, as the parked path's
   read buffer answers :FULL: whether the end comes next cannot be known
   without room to read into."
  (let ((fd (%process-output-fd process))
        (end 0)
        (deadline (+ (get-internal-real-time)
                     (round (* *fetch-timeout*
                               internal-time-units-per-second)))))
    (set-nonblocking fd)
    (loop
      (when (= end (length buffer))
        (return nil))
      (let ((n (nb-read fd buffer end (- (length buffer) end))))
        (case n
          (:eof (return end))
          ;; POLL-READABLE answers NIL for a signal as for a timeout, and
          ;; the exit of a getent child raises one, SIGCHLD. So what is
          ;; left of the deadline is worked out again each time round, as
          ;; BLOCKING-CONNECT does, and a wait cut short only reads again.
          (:again
           (let ((remaining-ms
                   (round (* 1000 (- deadline (get-internal-real-time)))
                          internal-time-units-per-second)))
             (when (<= remaining-ms 0)
               (return nil))
             (poll-readable fd remaining-ms)))
          (t (incf end n)))))))

(defun resolve-host-blocking (host)
  "Synchronously resolve HOST via `getent ahosts`.
   Returns (values IP FAMILY FALLBACKS) on success or NIL on failure.
   FAMILY is :INET or :INET6, and FALLBACKS the name's other addresses,
   (IP . FAMILY) in order, for BLOCKING-CONNECT-TO-ANY to fall back on.
   Numeric IPv4 and IPv6 literals skip the subprocess
   via the same fast path as the async resolver. Used by the blocking
   fetch setup paths so there is a single DNS primitive across the
   framework.

   The answer is getent's output up to its end, as on the async path,
   read by READ-GETENT-OUTPUT as it arrives: a lookup takes as long as
   getent does. Bounded by *FETCH-TIMEOUT* and +GETENT-OUTPUT-LIMIT+.
   Output that has not ended within both is no answer, and the child is
   sent SIGKILL. A libc resolver hang (unresponsive nameserver, slow NSS
   module, hung mDNS responder) cannot pin the worker thread
   indefinitely, so *FETCH-TIMEOUT* does cover each of DNS, connect, and
   I/O on the blocking path.

   getent's exit status is not asked for, here or on the async path.
   Given one name, it prints an address line only when the name
   resolved, so the output says everything the status would, and a
   lookup need not wait for the exit to have it."
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
  (multiple-value-bind (cached-ip cached-family cached-fallbacks)
      (dns-cache-lookup host)
    (when cached-ip
      (return-from resolve-host-blocking
        (values cached-ip cached-family cached-fallbacks))))
  (handler-case
      (let ((process (spawn-getent host)))
        (unless process
          (return-from resolve-host-blocking nil))
        (unwind-protect
             (let* ((buf (make-array +getent-output-limit+
                                     :element-type '(unsigned-byte 8)))
                    (end (read-getent-output process buf)))
               (when end
                 (multiple-value-bind (parsed refused all)
                     (parse-getent-output buf end host)
                   (declare (ignore refused))
                   (when parsed
                     (dns-cache-store host (car parsed) (cdr parsed) (rest all))
                     (values (car parsed) (cdr parsed) (rest all))))))
          ;; The lookup is over, with an answer or without one, and the
          ;; child may not have exited: its output ends a moment before it
          ;; does, and one given up on is still running. RELEASE-DNS-PROCESS
          ;; ends it and closes it only once it is dead, as it does for the
          ;; async path's child.
          (release-dns-process process)))
    (error () nil)))

;;; ---------------------------------------------------------------------------
;;; Registration
;;;
;;; Install our dispatchers into fetch.lisp's hook slots so initiate-
;;; http-fetch, fetch-stream-plain, and tls-connect can reach us without
;;; compile-time forward references. The hook pattern mirrors src/tls.lisp's
;;; registration of *HTTPS-STREAM-FN*.
;;; ---------------------------------------------------------------------------

(eval-when (:load-toplevel :execute)
  (setf *dns-lookup-fn*           #'initiate-dns-lookup
        *handle-dns-ready-fn*     #'handle-dns-ready
        *dns-resolve-blocking-fn* #'resolve-host-blocking))
