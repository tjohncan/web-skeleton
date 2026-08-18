(in-package :web-skeleton)

;;; ===========================================================================
;;; Worker pool and server entry point
;;;
;;; N worker threads, each with its own listener socket (SO_REUSEPORT),
;;; epoll fd, and connection table.  The kernel distributes incoming
;;; connections across workers.  Zero shared state in the hot path.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; TCP listener
;;; ---------------------------------------------------------------------------

(defun make-tcp-listener (host port)
  "Create a TCP socket, bind to HOST:PORT, listen with a backlog of 128.
   HOST is a 4-byte IPv4 vector or a 16-byte IPv6 vector; the family
   is dispatched accordingly. Sets SO_REUSEADDR, SO_REUSEPORT, and
   non-blocking. Returns the socket."
  (let* ((family (case (length host)
                   (4  :inet)
                   (16 :inet6)
                   (t (error "make-tcp-listener: :host must be a 4-byte v4 ~
                              or 16-byte v6 vector, got ~a" host))))
         (socket (make-instance (if (eq family :inet)
                                    'sb-bsd-sockets:inet-socket
                                    'sb-bsd-sockets:inet6-socket)
                                :type :stream
                                :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (set-socket-option-int (socket-fd socket)
                           +sol-socket+ +so-reuseport+ 1)
    (sb-bsd-sockets:socket-bind socket host port)
    (sb-bsd-sockets:socket-listen socket 128)
    (set-nonblocking (socket-fd socket))
    socket))

;;; ---------------------------------------------------------------------------
;;; Per-worker connection table
;;;
;;; Each worker thread binds *connections* in its own dynamic scope.
;;; All connection functions use whatever binding is current —
;;; no locks, no shared state.
;;; ---------------------------------------------------------------------------

(defvar *connections* nil
  "Maps file descriptor → connection object. Bound per-worker.")

(defun register-connection (conn)
  (setf (gethash (connection-fd conn) *connections*) conn))

(defun unregister-connection (conn)
  (remhash (connection-fd conn) *connections*))

(defun lookup-connection (fd)
  (gethash fd *connections*))

;;; ---------------------------------------------------------------------------
;;; Connection lifecycle — idle timeout and WebSocket ping/pong
;;;
;;; Three mechanisms:
;;;   1. HTTP idle timeout — close connections that never finish a request
;;;   2. WebSocket ping/pong — detect dead connections (client vanished)
;;;   3. WebSocket idle timeout — close inactive but alive connections
;;; ---------------------------------------------------------------------------

(defparameter *max-connections* 10000
  "Maximum connections per worker. New accepts are refused when full.")

(defparameter *connection-limit-response*
  (let* ((body (sb-ext:string-to-octets
                (format nil "Service Unavailable: connection limit reached~%")
                :external-format :ascii))
         (crlf (coerce '(#\Return #\Linefeed) 'string)))
    (concatenate
     '(simple-array (unsigned-byte 8) (*))
     (sb-ext:string-to-octets
      (concatenate 'string
                   "HTTP/1.1 503 Service Unavailable" crlf
                   ;; Two seconds: long enough that the retry is not part
                   ;; of the same burst that caused the refusal, short
                   ;; enough that a momentary spike does not turn into a
                   ;; visible stall for a client that did nothing wrong.
                   "Retry-After: 2" crlf
                   "Content-Type: text/plain; charset=utf-8" crlf
                   (format nil "Content-Length: ~d" (length body)) crlf
                   "Connection: close" crlf
                   crlf)
      :external-format :ascii)
     body))
  "The refusal sent when a worker is at *MAX-CONNECTIONS*, built once.

   No Date header, for the reason BUILD-STATIC-RESPONSE omits one: bytes
   frozen at load time cannot carry a per-request timestamp, and a stale
   Date is worse than none. Building it per refusal would put header
   construction on the one path that exists because the worker is already
   out of room.")

(defconstant +refusal-drain-size+ 2048
  "Bytes REFUSE-CONNECTION clears per read while draining a refused peer.
   Sized to hold an ordinary request in one or two passes; the drain is
   capped at four of them regardless.")

(defvar *stream-drain-buf* nil
  "Scratch buffer a :STREAMING connection's reads are discarded into,
   bound per worker by RUN-WORKER.

   A stream watches EPOLLIN only to notice the peer leaving; whatever
   the peer sends while a response streams is not a request this
   connection can answer. Kept out of the connection's own read buffer
   so a long stream cannot grow it, and so the offsets the keep-alive
   reset works from are the ones the original request left.")

(defvar *refusal-drain-buf* nil
  "Scratch buffer REFUSE-CONNECTION drains into, bound per worker by
   RUN-WORKER alongside *EPOLL-CTL-BUF* and *POLL-BUF*.

   The bytes read into it are discarded — the drain exists so that
   close(2) sends FIN rather than RST, not to look at what the peer
   said — so one buffer per worker is enough and its contents never
   need clearing between refusals.

   Per-worker rather than global for the reason every other scratch
   buffer here is: workers share nothing in the hot path, and a shared
   sink would be two threads writing one array. Nothing reads it, so
   that would in fact be harmless, which is exactly the kind of thing
   that stops being true after an edit nobody connected to it.

   NIL outside a worker — the REPL, the test suite driving
   REFUSE-CONNECTION directly — where the per-call allocation this
   replaces is still what happens.")

(defparameter *idle-timeout* 10
  "Seconds before an idle HTTP connection is closed. 0 to disable.")

(defparameter *ws-idle-timeout* 86400
  "Seconds before an inactive WebSocket connection is closed. 0 to disable.
   Inactivity = no text or binary frames from the client (pongs don't count).")

(defparameter *ws-ping-interval* 30
  "Seconds between server-initiated WebSocket pings.")

(defparameter *ws-max-missed-pongs* 3
  "Close a WebSocket connection after this many consecutive unanswered pings.")

(defun deliver-awaiting-timeout (conn epoll-fd)
  "Answer 504 on an inbound parked in :AWAITING past *FETCH-TIMEOUT*, then
   hand it to the write path to close.

   504 rather than 502 because the condition is specifically that the
   upstream did not answer in time. DELIVER-FETCH-ERROR's 502s are right
   for their cases — connect refused, short body, unparseable status, all
   genuinely a bad response from the gateway — and this is the other one.
   Answering everything with a single code is what *STATUS-REASONS*
   already declines to do.

   The paired outbound goes first, through CLOSE-OUTBOUND, because that
   is what fires the fetch callback's (NIL NIL NIL) sentinel. Skipping it
   would leak the app's cleanup on exactly the path where the fetch never
   returned.

   The write buffer is drained by construction: a connection reaches
   :AWAITING only from dispatch, after its request was fully read and
   before any response was queued, and both the keep-alive reset and the
   100-continue flush zero the buffer on the way. CONNECTION-QUEUE-WRITE
   enforces that rather than trusting it, which is why the caller catches
   — a signal here would take down a worker mid-sweep."
  (let ((out-fd (connection-awaiting-fd conn)))
    (when (>= out-fd 0)
      (let ((out-conn (lookup-connection out-fd)))
        (when out-conn
          (close-outbound out-conn epoll-fd)))))
  (setf (connection-close-after-p conn) t
        (connection-awaiting-fd conn) -1)
  (let ((bytes (strip-body-for-head
                (format-response (make-error-response 504)
                                 :connection-hint (connection-hint-for conn))
                conn)))
    (connection-queue-write conn bytes)
    (setf (connection-state conn) :write-response
          (connection-last-active conn) (get-universal-time))
    (epoll-modify epoll-fd (connection-fd conn)
                  (logior +epollout+ +epollet+))))

(defun sweep-idle-connections (epoll-fd now)
  "Close connections that have been idle too long.
   HTTP uses *idle-timeout*. WebSocket uses *ws-idle-timeout*.
   An :AWAITING connection is answered 504 first — see
   DELIVER-AWAITING-TIMEOUT.

   Also closes any connection whose write backlog has not moved for
   *WRITE-STALL-TIMEOUT*, whatever state it is in. That is a different
   question from idleness and the idle timeouts cannot answer it: they
   are measured on LAST-ACTIVE, which a peer that has stopped reading
   keeps fresh merely by continuing to send, and they are long by design
   on exactly the long-lived states most likely to build a backlog."
  (let ((idle nil)
        (stalled nil))
    (maphash (lambda (fd conn)
               (declare (ignore fd))
               ;; Outbound connections are cleaned up via their paired
               ;; inbound's :awaiting timeout — see close-connection
               (unless (connection-outbound-p conn)
                 (let* ((state (connection-state conn))
                        (timeout (cond
                                   ((eq state :websocket) *ws-idle-timeout*)
                                   ;; A stream and a parked inbound are
                                   ;; different judgments. *FETCH-TIMEOUT*
                                   ;; asks how long an upstream may take to
                                   ;; answer; this asks how long an app may
                                   ;; produce nothing before its stream is
                                   ;; presumed dead. Reusing either
                                   ;; *IDLE-TIMEOUT* or *WS-IDLE-TIMEOUT*
                                   ;; would be wrong in opposite directions
                                   ;; — ten seconds reaps healthy streams, a
                                   ;; day holds dead ones.
                                   ((eq state :streaming) *stream-idle-timeout*)
                                   ((eq state :awaiting)  *fetch-timeout*)
                                   (t                     *idle-timeout*)))
                        ;; A stream is judged on when its app last
                        ;; produced, not on when the connection last saw
                        ;; an event. HANDLE-CLIENT-WRITE bumps LAST-ACTIVE
                        ;; on EPOLLOUT entry, so a draining backlog would
                        ;; keep refreshing the deadline of a stream whose
                        ;; producer stopped — the conflation
                        ;; WRITE-PROGRESS-AT was added to break, one level
                        ;; up and in a new knob.
                        (since (if (eq state :streaming)
                                   (connection-stream-produced-at conn)
                                   (connection-last-active conn))))
                   (cond
                     ;; Stall is checked first: a connection can be both,
                     ;; and closing it for the reason that is actually
                     ;; wrong with it makes the log line worth reading.
                     ;; Measured from the last forward progress on the
                     ;; backlog, never from LAST-ACTIVE — HANDLE-CLIENT-WRITE
                     ;; bumps that on EPOLLOUT entry, which would refresh
                     ;; the deadline of precisely the stuck connection.
                     ;; Any connection sitting on a backlog, not just a
                     ;; WebSocket one. This used to name :WEBSOCKET, which
                     ;; made the state most likely to build a backlog —
                     ;; a long-lived stream — the one state with no stall
                     ;; bound, falling through to an idle test measured on
                     ;; LAST-ACTIVE, the clock this exists to avoid. Asking
                     ;; "is there a backlog and has it moved" needs no list
                     ;; of states to keep up to date.
                     ;; :AWAITING is the one exclusion, and not because of
                     ;; its state so much as because it already has a more
                     ;; specific answer: DELIVER-AWAITING-TIMEOUT's 504 is
                     ;; the documented floor under every way a fetch can
                     ;; strand, and a stall close would take that away and
                     ;; hand the client a bare disconnect instead. The
                     ;; state is also supposed to have nothing pending, so
                     ;; excluding it costs nothing that should ever exist.
                     ((and (not (eq state :awaiting))
                           (plusp *write-stall-timeout*)
                           (plusp (connection-write-pending conn))
                           (> (- now (connection-write-progress-at conn))
                              *write-stall-timeout*))
                      (push conn stalled))
                     ((and (> timeout 0)
                           (> (- now since) timeout))
                      (push conn idle))))))
             *connections*)
    (dolist (conn stalled)
      (log-info "write stalled ~ds fd ~d (~a, ~d bytes pending) — closing"
                *write-stall-timeout* (connection-fd conn)
                (connection-state conn)
                (connection-write-pending conn))
      (close-connection conn epoll-fd :stalled))
    (dolist (conn idle)
      (log-debug "idle timeout fd ~d (~a)"
                 (connection-fd conn) (connection-state conn))
      ;; Collect-then-act: nothing here queues a write inside the MAPHASH
      ;; above, because DELIVER-AWAITING-TIMEOUT tears down the paired
      ;; outbound and that mutates the table being walked.
      (if (eq (connection-state conn) :awaiting)
          ;; A failure to answer must not be worse than not trying. If
          ;; anything in the 504 path signals — a write buffer that was
          ;; not drained after all, a serializer refusing a header — fall
          ;; back to the bare close this used to do unconditionally,
          ;; rather than letting it escape into RUN-EVENT-LOOP, which has
          ;; no handler and would restart the worker mid-sweep.
          (handler-case (deliver-awaiting-timeout conn epoll-fd)
            (error (e)
              (log-warn "awaiting timeout: could not answer 504 on fd ~d: ~a"
                        (connection-fd conn) e)
              (close-connection conn epoll-fd :idle)))
          (close-connection conn epoll-fd :idle)))))

(defun keepalive-streams (epoll-fd now)
  "Send the keepalive bytes of any :STREAMING connection that has gone
   quiet for *STREAM-KEEPALIVE-INTERVAL*.

   Only connections whose STREAM-RESPONSE supplied keepalive bytes are
   touched, because there is nothing generic to send: a chunked stream's
   only zero-content emission is the empty chunk, and that is the
   terminator. Keeping a stream warm is necessarily the business of
   whatever protocol is riding on top of it.

   Skipped when anything is already queued — a connection with bytes
   waiting is not quiet, and adding to a backlog that is not draining
   would help nothing. The write goes out inline for the same reason the
   ping does: a keepalive is small, an idle socket takes it whole, and
   arming EPOLLOUT to deliver bytes that already fit is a wake-up for
   nothing. Failures close the connection rather than escaping into
   RUN-EVENT-LOOP, which has no handler."
  (when (plusp *stream-keepalive-interval*)
    (let ((broken nil))
      (maphash
       (lambda (fd conn)
         (declare (ignore fd))
         (when (and (eq (connection-state conn) :streaming)
                    (connection-stream-keepalive conn)
                    (zerop (connection-write-pending conn))
                    (>= (- now (connection-stream-produced-at conn))
                        *stream-keepalive-interval*))
           (handler-case
               (progn
                 ;; Counts as production, which is the point: a stream
                 ;; emitting keepalives is never reaped by
                 ;; *STREAM-IDLE-TIMEOUT*. Measured on the same clock the
                 ;; idle sweep reads, so the two cannot drift apart.
                 (setf (connection-stream-produced-at conn) now)
                 ;; Framed, not appended raw. These bytes land in the same
                 ;; body the app's sends land in, so on a chunked stream a
                 ;; raw comment line sits exactly where the peer's decoder
                 ;; expects a chunk-size — and the keepalive whose whole
                 ;; job is to stop a quiet stream being dropped becomes
                 ;; what drops it. FRAME-STREAM-BYTES is the one place
                 ;; that decision is made, so this writer cannot drift
                 ;; from STREAM-SEND again.
                 (let ((framed (frame-stream-bytes
                                conn (connection-stream-keepalive conn))))
                   (when (and framed (connection-append-write conn framed))
                     (unless (eq (connection-on-write conn) :done)
                       (epoll-modify epoll-fd (connection-fd conn)
                                     (logior +epollin+ +epollout+
                                             +epollet+))))))
             (error (e)
               (log-debug "stream keepalive failed fd ~d: ~a"
                          (connection-fd conn) e)
               (push conn broken)))))
       *connections*)
      (dolist (conn broken)
        (close-connection conn epoll-fd :disconnected)))))

(defun ping-ws-connections (epoll-fd)
  "Send pings to WebSocket connections and close dead ones.
   Dead = exceeded *ws-max-missed-pongs* consecutive unanswered pings.
   Skips connections with a write in progress (they're clearly not dead)."
  (let ((dead nil)
        (broken nil)
        (ping-frame (build-ws-ping)))
    (maphash (lambda (fd conn)
               (declare (ignore fd))
               (when (eq (connection-state conn) :websocket)
                 (cond
                   ;; Dead — too many missed pongs
                   ((>= (connection-missed-pongs conn) *ws-max-missed-pongs*)
                    (push conn dead))
                   ;; Nothing outstanding — send ping. Counts the append
                   ;; queue, not just the head: a connection with frames
                   ;; queued behind the one in flight is as clearly alive
                   ;; as one mid-write.
                   ((zerop (connection-write-pending conn))
                    (incf (connection-missed-pongs conn))
                    ;; Return ignored, and provably so: the guard above is
                    ;; that nothing is pending, and two bytes cannot
                    ;; overrun a bound the docs require to clear 1 MiB.
                    (connection-append-write conn ping-frame)
                    ;; Write it here rather than arming EPOLLOUT and coming
                    ;; back. A two-byte ping onto an empty queue is taken
                    ;; whole by any socket with room, so the arm-and-wake
                    ;; round trip was one epoll_ctl per connection per
                    ;; interval, issued as a single burst with the event
                    ;; loop serving nobody, to deliver two bytes that had
                    ;; already fit. Only a socket already backed up needs
                    ;; the arm.
                    ;;
                    ;; A peer that has gone away makes this write signal
                    ;; (EPIPE), and PING-WS-CONNECTIONS runs from the
                    ;; maintenance tick inside RUN-EVENT-LOOP, which has no
                    ;; handler and would restart the worker. Collected and
                    ;; closed alongside the unresponsive ones instead.
                    (handler-case
                        (unless (eq (connection-on-write conn) :done)
                          (epoll-modify epoll-fd (connection-fd conn)
                                        (logior +epollout+ +epollet+)))
                      (error (e)
                        (log-debug "ws ping write failed fd ~d: ~a"
                                   (connection-fd conn) e)
                        (push conn broken)))))))
             *connections*)
    (dolist (conn dead)
      (log-info "ws dead (missed ~d pongs) fd ~d"
                (connection-missed-pongs conn) (connection-fd conn))
      (close-connection conn epoll-fd))
    (dolist (conn broken)
      (close-connection conn epoll-fd))))

;;; ---------------------------------------------------------------------------
;;; Shutdown
;;; ---------------------------------------------------------------------------

(sb-ext:defglobal *shutdown* nil
  "Set to T to signal all workers to exit.
   defglobal (not defvar) to avoid per-thread bindings and ensure
   a single shared value cell across all worker threads.")

(defparameter *drain-timeout* 5
  "Seconds to wait for connections to drain during graceful shutdown.")

(defparameter *shutdown-poll-interval* 1
  "Seconds between shutdown-signal checks in the main thread's wait loop
   and each worker's event-loop epoll timeout.
   Default 1 second balances wake-up overhead against shutdown
   responsiveness. Test harnesses bind this to a small value (e.g. 0.05)
   so teardown doesn't wait a full second per call. Float accepted —
   the worker converts to ms for epoll_wait.

   It does not set the periodic-maintenance cadence. RUN-EVENT-LOOP gates
   the idle sweep on a hardcoded one second and the WebSocket ping on
   *WS-PING-INTERVAL*; this only bounds how often the loop can wake to
   check them. Lowering it makes shutdown prompt without making either
   scan run more often — the harness sets it to 0.05 and the sweep still
   runs at 1 Hz.")

(defconstant +max-events+ 64
  "Maximum events to process per epoll_wait call. Internal — not a
   tunable. A larger batch increases worst-case latency for the
   tail of the batch without meaningfully improving throughput; a
   smaller batch adds syscall overhead. 64 is the historical sweet
   spot that epoll-oriented servers have converged on.")

;;; ---------------------------------------------------------------------------
;;; User-extensible cleanup hooks
;;;
;;; Apps with background work (session reapers, cache flushers, metrics
;;; exporters) register zero-argument cleanup functions via REGISTER-CLEANUP.
;;; Hooks run inside start-server's unwind-protect after worker drain, each
;;; wrapped in HANDLER-CASE so one raising hook cannot prevent the rest from
;;; firing. This is the integration seam for primitives that own a live
;;; thread — e.g. a store with an expiry reaper.
;;; ---------------------------------------------------------------------------

(defvar *shutdown-hooks* nil
  "List of zero-argument cleanup functions, head = most recently registered.
   DEFVAR rather than DEFGLOBAL so tests can rebind it locally.
   Workers spawned via SB-THREAD:MAKE-THREAD see the top-level binding —
   dynamic bindings are not carried across thread creation.")

(defvar *shutdown-hooks-lock* (sb-thread:make-mutex :name "shutdown-hooks")
  "Serializes REGISTER-CLEANUP across concurrent threads.")

(defun register-cleanup (fn)
  "Register FN (a zero-argument function) to run during graceful shutdown.
   Hooks run after workers have drained, before START-SERVER returns.
   Thread-safe — callable from any thread, before or during server run.
   Each invocation is wrapped in HANDLER-CASE, so raising from a hook does
   not abort the rest. Returns FN."
  (sb-thread:with-mutex (*shutdown-hooks-lock*)
    (push fn *shutdown-hooks*))
  fn)

(defun run-shutdown-hooks ()
  "Invoke each registered cleanup hook in LIFO order, catching errors.
   Called from START-SERVER's unwind-protect — do not call directly.
   Copies the hook list under the mutex before iterating so a hook that
   registers another hook does not mutate the list we're walking."
  (let ((hooks (sb-thread:with-mutex (*shutdown-hooks-lock*)
                 (copy-list *shutdown-hooks*))))
    (dolist (fn hooks)
      (handler-case (funcall fn)
        (error (e)
          (log-error "shutdown hook error: ~a" e))))))

;;; ---------------------------------------------------------------------------
;;; Request dispatch
;;; ---------------------------------------------------------------------------

(defun dispatch-request (request handler)
  "Route an HTTP request via HANDLER. Returns (values response upgrade-p).
   If the handler returns :UPGRADE, validates the WebSocket handshake."
  (let ((response (if handler
                      (funcall handler request)
                      (make-error-response 501))))
    (cond
      ;; Handler signals WebSocket upgrade
      ((eq response :upgrade)
       (if (websocket-upgrade-p request)
           (progn
             (log-info "~a ~a -> 101 upgrade"
                       (http-request-method request)
                       (http-request-path request))
             (values (make-websocket-handshake-response request) t))
           (progn
             (log-warn "~a ~a -> 400 bad upgrade"
                       (http-request-method request)
                       (http-request-path request))
             ;; RFC 6455 §4.4: MUST include supported version on rejection
             (let ((resp (make-error-response 400)))
               (set-response-header resp "sec-websocket-version" "13")
               (values resp nil)))))
      ;; Outbound fetch request — handler needs an external call
      ((typep response 'http-fetch-continuation)
       (log-debug "~a ~a -> fetch ~a"
                  (http-request-method request)
                  (http-request-path request)
                  (http-fetch-continuation-url response))
       (values response nil))
      ;; Streaming response — the handler has headers now and a body it
      ;; will produce over time. Passed through for START-STREAM, which
      ;; is the only thing that knows how to frame one.
      ((typep response 'stream-response)
       (log-debug "~a ~a -> stream"
                  (http-request-method request)
                  (http-request-path request))
       (values response nil))
      ;; A response already in pieces: a list of byte vectors to write in
      ;; order. SERVE-STATIC's shape, so a pre-built file can carry a
      ;; current Date without the vector it was built from being rebuilt
      ;; or rewritten. The producer owns HEAD here — SERVE-STATIC decides
      ;; which segments a HEAD gets, because only it knows where its own
      ;; body begins, and STRIP-BODY-FOR-HEAD cannot find a boundary it
      ;; was not given.
      ((and (consp response)
            (typep (car response) '(simple-array (unsigned-byte 8) (*))))
       (log-debug "~a ~a -> static"
                  (http-request-method request)
                  (http-request-path request))
       (values response nil))
      ;; Pre-formatted response (e.g., static file — already bytes).
      ;; HEAD truncation happens centrally in HANDLE-CLIENT-READ via
      ;; STRIP-BODY-FOR-HEAD on the way to CONNECTION-QUEUE-WRITE, so
      ;; both byte-vector and HTTP-RESPONSE paths funnel through one
      ;; strip site instead of two.
      ((typep response '(simple-array (unsigned-byte 8) (*)))
       (log-debug "~a ~a -> static"
                  (http-request-method request)
                  (http-request-path request))
       (values response nil))
      ;; Normal HTTP response — must be an HTTP-RESPONSE struct. The
      ;; previous catch-all let a handler that forgot MAKE-TEXT-RESPONSE
      ;; (returning a raw string or alist) fall through to FORMAT-RESPONSE
      ;; where it tripped a deep SIMPLE-TYPE-ERROR from the struct
      ;; accessor, which the outer handler-case converted to a 500.
      ;; Correct outcome, terrible message — apps debugging their own
      ;; handler bugs had to trace into HTTP-RESPONSE-STATUS to realize
      ;; the issue was upstream. Check here and raise a pointed error.
      ((typep response 'http-response)
       (log-debug "~a ~a -> ~d"
                  (http-request-method request)
                  (http-request-path request)
                  (http-response-status response))
       (values response nil))
      (t
       (error "handler returned ~a; expected an HTTP-RESPONSE, ~
               HTTP-FETCH-CONTINUATION, byte vector, or :UPGRADE"
              (type-of response))))))

;;; ---------------------------------------------------------------------------
;;; Accept a new connection
;;; ---------------------------------------------------------------------------

(defun refuse-connection (client-socket)
  "Answer 503 on a connection the worker has no room for, then close.

   A bare close leaves the client unable to tell \"server full\" from
   \"server broken\" — both are a socket that opens and immediately shuts —
   and gives it nothing to back off against. One pre-built write makes
   the condition name itself.

   Delivery is best effort, and both limits are structural.

   The write is a single non-blocking attempt. Blocking here would stall
   the accept loop, which is the thing the connection limit exists to
   protect. The response is under 200 bytes and an empty send buffer
   takes it whole, so a short write means the peer is already gone.

   The drain is bounded, and what it buys is a graceful close rather
   than delivery. close(2) on a socket still holding unread data makes
   Linux send RST instead of FIN. The response has already gone out by
   then, so the bytes do arrive; what the peer loses is the ordinary
   end-of-stream, getting a reset on the read that should have returned
   it — and a client that reports a connection error in place of the 503
   has learned nothing, which is the state this function exists to get
   out of.

   It shortens the odds rather than settling the matter, and cannot do
   better. The refusal happens at accept time, which may precede the
   peer's request arriving at all, so the drain clears only what is
   already queued. Waiting for the rest would mean blocking in the
   accept loop, and draining to EOF would mean parking a peer there is
   by definition no room for. Four buffers clears a request already in
   hand, which is the ordinary case; bytes still in flight, or a client
   midway through a large upload, still earn a reset — and are still no
   worse off than the bare close they used to get."
  (let ((fd (ignore-errors (socket-fd client-socket))))
    (when (and fd (>= fd 0))
      (ignore-errors (set-nonblocking fd))
      (ignore-errors
       (nb-write fd *connection-limit-response* 0
                 (length *connection-limit-response*)))
      (let ((sink (or *refusal-drain-buf*
                      (make-array +refusal-drain-size+
                                  :element-type '(unsigned-byte 8)))))
        (dotimes (i 4)
          (declare (ignorable i))
          ;; NB-READ returns :AGAIN once the queue is empty and :EOF once
          ;; the peer is done; either way there is nothing left to clear.
          (unless (integerp (ignore-errors (nb-read fd sink 0 (length sink))))
            (return))))))
  (ignore-errors (sb-bsd-sockets:socket-close client-socket)))

(defun accept-connection (listener-socket epoll-fd)
  "Accept a pending connection and register it with epoll.
   Returns T if a connection was accepted, NIL if none pending (EAGAIN).
   Refuses the connection with a 503 if the per-worker limit is reached."
  (let ((client-socket (handler-case
                          (sb-bsd-sockets:socket-accept listener-socket)
                        (error (e)
                          (log-error "socket-accept failed: ~a" e)
                          (sleep 0.1) ; backoff to avoid log-spin on EMFILE
                          (return-from accept-connection nil)))))
    (unless client-socket
      (return-from accept-connection nil))
    ;; Enforce per-worker connection limit
    (when (and (> *max-connections* 0)
               (>= (hash-table-count *connections*) *max-connections*))
      (log-warn "connection limit reached (~d), refusing new accept"
                *max-connections*)
      (refuse-connection client-socket)
      (return-from accept-connection t))
    (handler-case
        (let ((conn (make-client-connection client-socket))
              (registered nil)
              (done nil))
          (unwind-protect
               (progn
                 (register-connection conn)
                 (setf registered t)
                 (epoll-add epoll-fd (connection-fd conn)
                            (logior +epollin+ +epollet+))
                 (log-debug "accepted fd ~d ~a" (connection-fd conn)
                            (or (connection-remote-addr conn) ""))
                 (setf done t))
            ;; If EPOLL-ADD raised after REGISTER-CONNECTION succeeded,
            ;; the connection is in *CONNECTIONS* but the kernel never
            ;; saw the fd — a stale entry that the idle sweeper would
            ;; later trip over. Drop it so the table stays honest.
            (when (and registered (not done))
              (unregister-connection conn)))
          t)
      (error (e)
        (log-error "accept failed: ~a" e)
        (ignore-errors (sb-bsd-sockets:socket-close client-socket))
        nil))))

;;; ---------------------------------------------------------------------------
;;; Close and clean up a connection
;;; ---------------------------------------------------------------------------

(defun close-connection (conn epoll-fd &optional (reason :closed))
  "Remove from epoll, unregister, close fd.

   REASON is passed to a streaming connection's ON-CLOSE callback, which
   fires here rather than at each call site so that no way of tearing a
   stream down can forget it — the same argument as the fetch cleanup
   sentinel below. Callers that know why they are closing say so;
   :CLOSED is the honest answer for the ones that do not.

   If CONN is :awaiting, also closes its outbound connection via
   CLOSE-OUTBOUND — which fires the app's fetch cleanup callback if
   the outbound was still carrying one, so DB handles / metrics /
   rate-limit counters get a defined moment to run their teardown
   even on inbound-driven aborts (drain, idle timeout, I/O error).
   MAYBE-REAP-DNS-PROCESS runs on every cleaned-up connection so a
   half-finished DNS lookup never leaks."
  (let ((fd (connection-fd conn)))
    (when (>= fd 0)
      ;; Before anything else is torn down, so the app's callback still
      ;; sees a live connection to read state off. NOTIFY-STREAM-CLOSED
      ;; is a no-op once fired, so a stream ended normally through
      ;; STREAM-CLOSE does not hear about it twice.
      (notify-stream-closed conn reason)
      ;; If parked waiting for a fetch, close the orphaned outbound too.
      (when (eq (connection-state conn) :awaiting)
        (let ((out-fd (connection-awaiting-fd conn)))
          (when (>= out-fd 0)
            (let ((out-conn (lookup-connection out-fd)))
              (when out-conn
                (close-outbound out-conn epoll-fd))))))
      (ignore-errors (epoll-remove epoll-fd fd))
      (unregister-connection conn)
      (maybe-reap-dns-process conn)
      (connection-close conn)
      (log-debug "closed fd ~d" fd))))

;;; ---------------------------------------------------------------------------
;;; Graceful drain — flush in-progress writes, close cleanly
;;; ---------------------------------------------------------------------------

(defun drain-connections (listener-socket epoll-fd event-buf)
  "Gracefully drain all active connections during shutdown.
   Stops accepting, sends WebSocket close frames, lets writes flush,
   force-closes anything remaining after *drain-timeout*."
  ;; Stop accepting new connections
  (ignore-errors (epoll-remove epoll-fd (socket-fd listener-socket)))
  (let ((count (hash-table-count *connections*)))
    (when (zerop count)
      (return-from drain-connections))
    (log-info "draining ~d connection~:p" count))
  ;; Phase 1: initiate shutdown on each connection. Outbound
  ;; connections and inbound connections that haven't completed a
  ;; request are closed immediately. WebSocket connections get a
  ;; 1001 close frame and are flipped to :closing so the event
  ;; loop finishes flushing the close before tearing them down.
  ;; Outbounds and inbounds are split into two lists so outbounds
  ;; can be routed through close-outbound (which fires the fetch
  ;; callback); close-connection on a direct outbound wouldn't
  ;; because its :awaiting branch only handles paired-outbound
  ;; teardown, not the outbound itself.
  (let ((outbounds-to-close nil)
        (inbounds-to-close nil))
    (maphash (lambda (fd conn)
               (declare (ignore fd))
               (cond
                 ((connection-outbound-p conn)
                  (push conn outbounds-to-close))
                 ;; A stream will not end on its own, so waiting for it
                 ;; would spend the whole drain timeout. Closed without a
                 ;; terminator on purpose: the client reads a truncated
                 ;; chunked body, which is exactly what happened, and a
                 ;; terminator would claim a complete response the app
                 ;; never finished sending. The app hears :SHUTDOWN.
                 ((member (connection-state conn)
                          '(:read-http :read-body :awaiting :streaming))
                  (push conn inbounds-to-close))
                 ((eq (connection-state conn) :websocket)
                  ;; Must not clobber an in-flight write. The race:
                  ;; ping-ws-connections queued a 2-byte ping whose
                  ;; first byte was flushed, write-pos=1 write-end=2
                  ;; (EAGAIN on byte 2). SIGTERM arrives, drain runs,
                  ;; connection-queue-write overwrites write-buf with
                  ;; the close frame bytes and resets write-pos to 0.
                  ;; What hits the wire: partial ping + close frame
                  ;; bytes as one contiguous buffer, and the client
                  ;; interprets the close frame's FIN+opcode byte as
                  ;; the ping's length, then waits forever. Same
                  ;; discipline ping-ws-connections itself uses
                  ;; (queue only when nothing is in flight). If a
                  ;; write IS in flight, transition to :closing and
                  ;; let handle-client-write tear the connection
                  ;; down after the partial write finishes — the
                  ;; peer sees a truncated frame rather than a
                  ;; corrupt one.
                  ;; Pending counts the append queue as well as the head,
                  ;; so a connection with frames stacked behind the one in
                  ;; flight takes the same branch rather than having the
                  ;; close frame overwrite the head out from under them.
                  (cond
                    ((plusp (connection-write-pending conn))
                     (setf (connection-state conn) :closing))
                    (t
                     (connection-queue-write conn (build-ws-close 1001))
                     (setf (connection-state conn) :closing)
                     (epoll-modify epoll-fd (connection-fd conn)
                                   (logior +epollout+ +epollet+)))))
                 ;; :write-response, :ws-upgrade, :closing — let them finish
                 (t nil)))
             *connections*)
    ;; Close outbounds first so their fetch callbacks fire. Inbounds
    ;; afterward — any that were in :awaiting find their paired
    ;; outbound already gone and short-circuit cleanly.
    (dolist (conn outbounds-to-close)
      (close-outbound conn epoll-fd))
    (dolist (conn inbounds-to-close)
      (close-connection conn epoll-fd :shutdown)))
  ;; Phase 2: flush remaining writes until drained or timeout
  (let ((deadline (+ (get-universal-time) *drain-timeout*)))
    (loop
      (when (zerop (hash-table-count *connections*))
        (log-info "all connections drained")
        (return))
      (when (> (get-universal-time) deadline)
        (log-warn "drain timeout — force-closing ~d connection~:p"
                  (hash-table-count *connections*))
        (return))
      (let ((n (epoll-wait epoll-fd event-buf +max-events+ 200)))
        (loop for i from 0 below n
              do (let* ((fd (epoll-event-fd event-buf i))
                        (flags (epoll-event-flags event-buf i))
                        (conn (lookup-connection fd)))
                   (when conn
                     (cond
                       ((or (logtest flags +epollerr+)
                            (logtest flags +epollhup+))
                        (close-connection conn epoll-fd))
                       ((logtest flags +epollout+)
                        (handle-client-write conn epoll-fd))))))))))

;;; ---------------------------------------------------------------------------
;;; Handle readable event on a client fd
;;; ---------------------------------------------------------------------------

(defun start-stream (conn epoll-fd request sresp)
  "Send the head of a streaming response and hand CONN to the app.

   The head goes out through CONNECTION-QUEUE-WRITE, whose guard is a
   real check here: nothing should be pending on a connection that has
   just finished parsing a request, and a stream is the last thing that
   should be appended behind someone else's half-written response.

   Arming happens once, after ON-OPEN returns, from what is actually
   pending — the same reason HANDLE-CLIENT-READ's :websocket branch
   centralized it. ON-OPEN may send nothing, may send and flush
   completely, may leave a remainder, or may close the stream outright,
   and each of those wants a different interest."
  (let* ((framing (stream-framing-for request))
         (head (format-streaming-head (stream-response-response sresp) framing
                                      :connection-hint (connection-hint-for conn))))
    (connection-queue-write conn head)
    (cond
      ;; HEAD: RFC 7231 §4.3.2 wants the headers a GET would have
      ;; carried, which is what the head already is. No body follows, so
      ;; no stream starts, no terminator is owed, and the connection is
      ;; reusable exactly as it would be for any other response.
      ((eq (http-request-method request) :HEAD)
       (setf (connection-state conn) :write-response)
       (epoll-modify epoll-fd (connection-fd conn)
                     (logior +epollout+ +epollet+)))
      (t
       (when (eq framing :close)
         ;; Close-delimited framing *is* the end of the connection.
         (setf (connection-close-after-p conn) t))
       (setf (connection-stream-framing conn) framing
             (connection-stream-on-close conn) (stream-response-on-close sresp)
             (connection-stream-keepalive conn) (stream-response-keepalive sresp)
             (connection-state conn) :streaming
             (connection-last-active conn) (get-universal-time)
             (connection-stream-produced-at conn) (get-universal-time))
       (let ((on-open (stream-response-on-open sresp)))
         (when on-open
           (funcall on-open conn)))
       (epoll-modify
        epoll-fd (connection-fd conn)
        (if (eq (connection-state conn) :streaming)
            (logior +epollin+ +epollet+
                    (if (plusp (connection-write-pending conn)) +epollout+ 0))
            ;; ON-OPEN closed the stream already — a one-shot producer
            ;; that had everything to say up front. The terminator is
            ;; queued and the ordinary write path takes it from here.
            (logior +epollout+ +epollet+)))))))

(defun handle-client-read (conn epoll-fd handler ws-handler)
  "Handle EPOLLIN on a client connection.
   State dispatch is permissive: a stale EPOLLIN for a connection
   whose state moved to :write-response / :ws-upgrade / :closing /
   :sending-100-continue earlier in the same epoll batch silently
   no-ops instead of raising into the outer handler-case and
   queueing a 500 over the legitimate response that's already in
   the write buffer. The race is reachable when one event in a
   batch completes a fetch (setting an inbound's state to
   :write-response) and a later event in the same batch is a
   stale EPOLLIN for that inbound."
  (handler-case
      (case (connection-state conn)
        ;; HTTP request accumulation
        ((:read-http :read-body)
         (let ((result (connection-on-read conn)))
           (case result
             (:dispatch
              ;; Full request — parse and dispatch
              (setf (connection-last-active conn) (get-universal-time))
              (let ((request (connection-parse-request conn)))
                (log-debug "~a ~a~@[?~a~] HTTP/~a ~a"
                           (http-request-method request)
                           (http-request-path request)
                           (http-request-query request)
                           (http-request-version request)
                           (or (connection-remote-addr conn) ""))
                ;; Determine keep-alive: HTTP/1.1 default is keep-alive,
                ;; HTTP/1.0 default is close. Connection header overrides.
                ;; RFC 7230 §6.1 + §3.2.2: Connection is a list-valued
                ;; header, and multiple occurrences are semantically
                ;; equivalent to one comma-joined value. Walk
                ;; GET-HEADERS rather than GET-HEADER so a split
                ;; Connection: keep-alive / Connection: close pair
                ;; from a cooperating proxy is read as "close" instead
                ;; of silently dropping the second value.
                (let ((conn-values (get-headers request "connection")))
                  (setf (connection-close-after-p conn)
                        (cond
                          ((some (lambda (v)
                                   (header-has-token-p v "close"))
                                 conn-values)
                           t)
                          ((some (lambda (v)
                                   (header-has-token-p v "keep-alive"))
                                 conn-values)
                           nil)
                          ((string= (http-request-version request) "1.0") t)
                          (t nil))))
                (multiple-value-bind (response upgrade-p)
                    (dispatch-request request handler)
                  ;; Sync close-after-p from a handler-set Connection: close
                  ;; header. Without this step an HTTP/1.1 handler that
                  ;; explicitly asks to close the socket would advertise
                  ;; 'Connection: close' on the wire while the server
                  ;; happily held the socket open — framing mismatch vs
                  ;; the client's expectation. Shared helper with the
                  ;; fetch-callback path (fetch.lisp / tls.lisp) walks
                  ;; every 'connection' header via GET-HEADERS-equivalent,
                  ;; symmetric with the inbound-side walk ten lines up.
                  ;; A stream's Connection header lives on the response it
                  ;; carries, not on the wrapper — an app that asks to
                  ;; close after a stream has to be heard the same way as
                  ;; one asking after an ordinary response.
                  (sync-close-after-p-from-response
                   conn (if (typep response 'stream-response)
                            (stream-response-response response)
                            response))
                  (cond
                    ;; Outbound fetch — park and initiate
                    ((typep response 'http-fetch-continuation)
                     (initiate-fetch conn epoll-fd response))
                    ;; Streaming response — send the head, hand over
                    ((typep response 'stream-response)
                     (start-stream conn epoll-fd request response))
                    ;; Normal response — queue for writing.
                    ;; Handler-returned response structs are treated as
                    ;; immutable: a caller that caches a (make-error-response
                    ;; 404) across requests must not have HEAD strip the
                    ;; body slot or HTTP/1.0 keep-alive stamp a Connection
                    ;; header onto it. :CONNECTION-HINT stamps the
                    ;; Connection header at serialize time, :HEAD-ONLY-P
                    ;; skips body emission for HEAD without allocating —
                    ;; neither touches the struct. The byte-vector path
                    ;; (pre-built static files) still goes through
                    ;; STRIP-BODY-FOR-HEAD since the bytes are already
                    ;; fully serialized and we have no encode step to
                    ;; short-circuit.
                    ;; Segments — a complete response in pieces, queued
                    ;; as one act and outside the backlog bound. Routing
                    ;; them through CONNECTION-APPEND-WRITE subjected a
                    ;; finished in-memory response to a limit meant for a
                    ;; producer outrunning its peer, and a static file
                    ;; over *MAX-WRITE-BACKLOG* had its body refused
                    ;; while its headers went out promising one.
                    ((consp response)
                     (connection-queue-segments conn response)
                     (setf (connection-state conn) :write-response)
                     (epoll-modify epoll-fd (connection-fd conn)
                                   (logior +epollout+ +epollet+)))
                    (t
                     (let* ((head-p (eq (http-request-method request) :HEAD))
                            (bytes
                             (if (typep response
                                        '(simple-array (unsigned-byte 8) (*)))
                                 (strip-body-for-head response conn)
                                 (format-response
                                  response
                                  :connection-hint (connection-hint-for conn)
                                  :head-only-p head-p))))
                       (connection-queue-write conn bytes)
                       (setf (connection-state conn)
                             (if upgrade-p :ws-upgrade :write-response))
                       (epoll-modify epoll-fd (connection-fd conn)
                                    (logior +epollout+ +epollet+))))))))
             (:flush-queued
              ;; connection-on-read queued response bytes that must flush
              ;; before further reads happen. Covers two cases:
              ;;   :sending-100-continue — interim 100, body read resumes
              ;;                           once the flush completes.
              ;;   :write-response       — terminal 417 on unknown Expect,
              ;;                           close-after-p=T closes after.
              ;; Either way the next step is EPOLLOUT; handle-client-write
              ;; flushes and state-based dispatch takes it from there.
              (epoll-modify epoll-fd (connection-fd conn)
                            (logior +epollout+ +epollet+)))
             (:close
              (close-connection conn epoll-fd))
             ;; :continue — just wait for more data
             )))
        ;; WebSocket frame processing
        (:websocket
         (let ((result (connection-on-read conn)))
           (case result
             (:websocket
              (multiple-value-bind (response close-frame)
                  (handler-case
                      (websocket-on-read conn ws-handler)
                    (error (e)
                      ;; Handler error — close directly without a close frame.
                      ;; A ws-send may have put part of a frame on the wire
                      ;; and left the rest queued, and this teardown drops
                      ;; the queue; appending a close frame instead would
                      ;; land it where the peer is still reading payload
                      ;; and be taken as continuation of the partial one.
                      (log-warn "ws handler error fd ~d: ~a"
                                (connection-fd conn) e)
                      (close-connection conn epoll-fd)
                      (return-from handle-client-read)))
                ;; Every branch appends rather than replaces. The handler
                ;; may already have queued frames through WS-SEND, and
                ;; CONNECTION-QUEUE-WRITE would either clobber them or,
                ;; now that its guard counts the queue, signal.
                (cond
                  ;; Close requested — send close frame back, then shut down
                  ((eq response :close)
                   ;; Best-effort: a full queue means the peer already is
                   ;; not draining, and the teardown is what matters.
                   (when close-frame
                     (connection-append-write conn close-frame))
                   ;; Mark as closing so on-write knows to disconnect
                   (setf (connection-state conn) :closing))
                  ;; Response frame(s) to send
                  (response
                   (unless (connection-append-write conn response)
                     ;; The handler produced a frame for a peer already at
                     ;; *MAX-WRITE-BACKLOG*. Dropping it would leave the
                     ;; app's view of the stream and the peer's permanently
                     ;; different with nothing raised anywhere, so the
                     ;; connection goes instead.
                     (log-warn "ws backlog full fd ~d (~d bytes pending) — closing"
                               (connection-fd conn)
                               (connection-write-pending conn))
                     (close-connection conn epoll-fd)
                     (return-from handle-client-read))
                   (setf (connection-state conn) :websocket))
                  ;; No response. If the buffer is at capacity (no frames
                  ;; were consumed), a partial frame exceeds our limit —
                  ;; close with 1009 to prevent a spin loop.
                  (t
                   (when (>= (connection-read-pos conn)
                             (length (connection-read-buf conn)))
                     (log-warn "ws buffer full, no parseable frames fd ~d"
                               (connection-fd conn))
                     (connection-append-write conn (build-ws-close 1009))
                     (setf (connection-state conn) :closing))))
                ;; One arming decision for all of them. What is pending
                ;; now decides the interest, not which branch ran: the
                ;; handler's own WS-SEND calls may have flushed everything
                ;; already, or left a remainder the event loop has to
                ;; finish. Arming EPOLLIN when nothing is pending also
                ;; re-arms the edge trigger, in case the kernel still holds
                ;; data that did not fit the read buffer earlier.
                (epoll-modify epoll-fd (connection-fd conn)
                              (logior (if (plusp (connection-write-pending conn))
                                          +epollout+
                                          +epollin+)
                                      +epollet+))))
             (:close
              (close-connection conn epoll-fd))
             ;; :continue — wait for more data
             )))
        ;; A stream watches EPOLLIN for one thing: the peer leaving.
        (:streaming
         (let ((result (connection-discard-available
                        conn (or *stream-drain-buf*
                                 (make-array 4096
                                             :element-type '(unsigned-byte 8))))))
           (case result
             ;; End of stream from the client's side. Producing into a
             ;; socket with nobody on the far end is the thing this
             ;; branch exists to stop, and noticing it needs :OK-EOF —
             ;; a peer whose last bytes and FIN land in one wake-up
             ;; would otherwise report :OK and be missed entirely.
             ((:eof :ok-eof)
              (log-debug "stream peer disconnected fd ~d" (connection-fd conn))
              (close-connection conn epoll-fd :disconnected))
             ;; The peer sent something. It cannot be answered on this
             ;; connection — a pipelined request behind a stream would
             ;; have to wait for the stream to end, and the stream may
             ;; not end — so the bytes are discarded and the connection
             ;; is marked not to be reused. A client that gets a clean
             ;; close retries; one whose request vanished silently
             ;; waits forever.
             (:ok
              (log-debug "stream peer sent data mid-stream fd ~d — ~
                          will not reuse" (connection-fd conn))
              (setf (connection-close-after-p conn) t))
             ;; :again — spurious wake-up, nothing to do.
             (t nil))))
        ;; Parked for outbound fetch — ignore reads, data stays in kernel buffer
        (:awaiting nil)
        ;; Stale EPOLLIN for a state that isn't currently reading.
        ;; Silently ignored so a late notification can't escalate
        ;; into a 500 via the handler-case fallback below.
        (otherwise
         (log-debug "stale EPOLLIN fd ~d in state ~a — ignoring"
                    (connection-fd conn) (connection-state conn))))
    (http-parse-error (e)
      (log-warn "parse error fd ~d: ~a" (connection-fd conn)
                (http-parse-error-message e))
      ;; Answer before closing so the client gets a proper HTTP response.
      ;; The status comes from the condition: 400 unless the raise site
      ;; knew better (413 for a body over the cap, 414 for a request line
      ;; over its cap, 431 for headers, 501 for a method or transfer
      ;; coding we do not implement, 505 for a version we do not speak).
      ;; Runs through STRIP-BODY-FOR-HEAD for symmetry with the 500 path
      ;; below — typically a no-op here because CONNECTION-REQUEST isn't
      ;; set until PARSE-REQUEST-BYTES fully succeeds, but harmless and
      ;; correct on any future path that raises HTTP-PARSE-ERROR after
      ;; the request is parsed.
      (handler-case
          (let ((resp (make-error-response (http-parse-error-status e))))
            (set-response-header resp "connection" "close")
            (let ((err-bytes (strip-body-for-head
                              (format-response resp) conn)))
              (connection-queue-write conn err-bytes)
              (setf (connection-state conn) :write-response
                    (connection-close-after-p conn) t)
              (epoll-modify epoll-fd (connection-fd conn)
                           (logior +epollout+ +epollet+))))
        (error ()
          (close-connection conn epoll-fd))))
    (error (e)
      (log-warn "error fd ~d: ~a" (connection-fd conn) e)
      ;; Send 500 before closing so the client gets a proper HTTP response
      (handler-case
          (let ((resp (make-error-response 500)))
            (set-response-header resp "connection" "close")
            (let ((err-bytes (strip-body-for-head
                              (format-response resp) conn)))
              (connection-queue-write conn err-bytes)
              (setf (connection-state conn) :write-response
                    (connection-close-after-p conn) t)
              (epoll-modify epoll-fd (connection-fd conn)
                           (logior +epollout+ +epollet+))))
        (error ()
          (close-connection conn epoll-fd))))))

;;; ---------------------------------------------------------------------------
;;; Handle writable event on a client fd
;;; ---------------------------------------------------------------------------

(defun handle-client-write (conn epoll-fd)
  "Handle EPOLLOUT on a client connection.
   Inner state dispatch is permissive for the same reason as
   HANDLE-CLIENT-READ: a stale EPOLLOUT can arrive for a state
   that isn't currently writing, and raising into the handler-case
   below would overwrite a legitimate in-flight response with a 500.

   Bumps LAST-ACTIVE on entry so a legitimate slow client (mobile
   3G pulling a 5 MiB response at 200 KB/s) is not reaped by the
   idle sweeper midway through the download. EPOLLOUT firing means
   the kernel has room in the socket buffer; treating that as
   activity is the correct semantics for a slow-pipe client, and
   dispatch-time + keep-alive-reset bumps alone would not survive
   a response that takes longer than *IDLE-TIMEOUT* to flush."
  (setf (connection-last-active conn) (get-universal-time))
  (handler-case
      (let ((result (connection-on-write conn)))
        (case result
          (:done
           ;; All bytes sent — next action depends on state
           (case (connection-state conn)
             (:write-response
              (if (connection-close-after-p conn)
                  (close-connection conn epoll-fd)
                  ;; Keep-alive — reset for next request,
                  ;; preserving any pipelined bytes already buffered
                  (let* ((consumed (connection-request-end conn))
                         (buffered (connection-read-pos conn))
                         (extra (- buffered consumed)))
                    (when (> extra 0)
                      (replace (connection-read-buf conn)
                               (connection-read-buf conn)
                               :start1 0 :start2 consumed :end2 buffered))
                    ;; Shrink the read buffer back to the default 4 KiB
                    ;; when a prior large body grew it and the shifted
                    ;; pipelined bytes fit in the default. 10k idle
                    ;; keep-alives × 1 MiB grown buffers would otherwise
                    ;; sit on ~10 GiB after a burst of large POSTs.
                    ;; Heuristic: a uniform-large-POST workload will
                    ;; shrink-then-grow each cycle — fine for bursty
                    ;; traffic (the common case), measurable if traffic
                    ;; happens to be uniform-large.
                    (let ((buf (connection-read-buf conn)))
                      (when (and (> (length buf) 4096)
                                 (< extra 4096))
                        (let ((fresh (make-array 4096
                                                  :element-type '(unsigned-byte 8))))
                          (when (> extra 0)
                            (replace fresh buf :end2 extra))
                          (setf (connection-read-buf conn) fresh))))
                    (connection-reset-write conn)
                    (setf (connection-read-pos conn) (max extra 0)
                          (connection-request conn) nil
                          (connection-body-expected conn) 0
                          (connection-header-end conn) 0
                          (connection-close-after-p conn) nil
                          (connection-state conn) :read-http
                          (connection-last-active conn) (get-universal-time))
                    (epoll-modify epoll-fd (connection-fd conn)
                                 (logior +epollin+ +epollet+))
                    ;; Edge-triggered: data may already be waiting from a
                    ;; pipelined request. Signal the event loop to try reading.
                    (return-from handle-client-write :keep-alive))))
             (:ws-upgrade
              ;; WebSocket handshake sent — preserve any data past the HTTP request
              (let* ((http-end (connection-request-end conn))
                     (buffered (connection-read-pos conn))
                     (extra (- buffered http-end)))
                (when (> extra 0)
                  (replace (connection-read-buf conn) (connection-read-buf conn)
                           :start1 0 :start2 http-end :end2 buffered))
                (connection-reset-write conn)
                (setf (connection-read-pos conn) (max extra 0)
                      (connection-request conn) nil
                      (connection-body-expected conn) 0
                      (connection-header-end conn) 0
                      (connection-state conn) :websocket))
              (epoll-modify epoll-fd (connection-fd conn)
                           (logior +epollin+ +epollet+))
              ;; Edge-triggered: buffered frame data from the upgrade
              ;; request is in user-space, not the kernel. MOD won't
              ;; re-fire. Signal the event loop to read immediately.
              (return-from handle-client-write :keep-alive))
             (:websocket
              ;; WebSocket frame response sent — back to reading
              (epoll-modify epoll-fd (connection-fd conn)
                           (logior +epollin+ +epollet+)))
             (:streaming
              ;; Backlog drained; the stream stays open. Drop EPOLLOUT
              ;; and keep EPOLLIN, which is the only event a stream with
              ;; nothing queued has any use for. STREAM-FLUSH arms it
              ;; again the moment a send fails to complete.
              (epoll-modify epoll-fd (connection-fd conn)
                            (logior +epollin+ +epollet+)))
             (:closing
              ;; Close frame sent — disconnect
              (close-connection conn epoll-fd))
             (:sending-100-continue
              ;; 100 Continue flushed — switch to reading the body.
              ;; Returning :keep-alive tells the main loop to re-enter
              ;; handle-client-read immediately in case the body is
              ;; already buffered (edge-triggered epoll won't re-fire
              ;; on user-space bytes).
              (connection-reset-write conn)
              (setf (connection-state conn) :read-body)
              (epoll-modify epoll-fd (connection-fd conn)
                            (logior +epollin+ +epollet+))
              (return-from handle-client-write :keep-alive))
             ;; Stale EPOLLOUT for a state that isn't currently
             ;; writing (e.g. a race where read-side completed
             ;; earlier in the same batch and flipped the state
             ;; before this event was dispatched). Silently ignore.
             (otherwise
              (log-debug "stale EPOLLOUT :done fd ~d in state ~a — ignoring"
                         (connection-fd conn) (connection-state conn)))))
          ;; :continue — more bytes to write
          (:continue nil)))
    (error (e)
      (log-error "write error fd ~d: ~a" (connection-fd conn) e)
      (close-connection conn epoll-fd))))

;;; ---------------------------------------------------------------------------
;;; Event loop
;;; ---------------------------------------------------------------------------

(defun run-event-loop (listener-socket epoll-fd handler ws-handler)
  "Main event loop. Runs until *shutdown* is set."
  (let ((listener-fd    (socket-fd listener-socket))
        (last-ping-time  (get-universal-time))
        (last-sweep-time (get-universal-time))
        (event-buf (make-epoll-event-buf +max-events+)))
    (loop
      (when *shutdown*
        (drain-connections listener-socket epoll-fd event-buf)
        (return))
      (let ((n (epoll-wait epoll-fd event-buf +max-events+
                           (max 10 (round (* *shutdown-poll-interval* 1000))))))
        (loop for i from 0 below n
              do (block handle-event
                   (let ((fd    (epoll-event-fd event-buf i))
                         (flags (epoll-event-flags event-buf i)))
                     (cond
                       ;; New connection on the listener
                       ((= fd listener-fd)
                        ;; Edge-triggered: accept in a loop until none pending
                        (loop (unless (accept-connection listener-socket epoll-fd)
                                (return))))
                       ;; Event on a connection (inbound or outbound)
                       (t
                        (let ((conn (lookup-connection fd)))
                          (when conn
                            (if (connection-outbound-p conn)
                                ;; Outbound fetch connection
                                (handle-outbound-event conn epoll-fd flags)
                                ;; Inbound client connection. Process
                                ;; EPOLLIN before EPOLLHUP so a
                                ;; fire-and-close HTTP/1.0 client
                                ;; (sends request, immediately FINs)
                                ;; gets the request processed instead
                                ;; of losing it to the HUP arm.
                                (progn
                                  ;; Readable
                                  (when (logtest flags +epollin+)
                                    (handle-client-read conn epoll-fd handler ws-handler))
                                  ;; Writable (rebind from table — fd may
                                  ;; have been reused since the batch start)
                                  (when (logtest flags +epollout+)
                                    (let ((live (lookup-connection fd)))
                                      (when live
                                        (when (eq (handle-client-write live epoll-fd)
                                                  :keep-alive)
                                          ;; Keep-alive reset — read immediately in case
                                          ;; the next request is already buffered
                                          ;; (edge-triggered epoll won't re-notify)
                                          (when (lookup-connection fd)
                                            (handle-client-read live epoll-fd
                                                                handler ws-handler))))))
                                  ;; Error/hangup — close only if the
                                  ;; read/write path didn't already
                                  ;; tear the connection down.
                                  (when (or (logtest flags +epollerr+)
                                           (logtest flags +epollhup+))
                                    (let ((live (lookup-connection fd)))
                                      (when live
                                        (close-connection live epoll-fd))))))))))))))
      ;; Periodic maintenance — both scans gated on elapsed wall
      ;; clock so a busy epoll loop doesn't walk the connection
      ;; table multiple times per second. 1 s is fine: idle
      ;; timeouts are measured in 10 s+ units, so sub-second sweep
      ;; granularity is pure waste.
      (let ((now (get-universal-time)))
        (when (>= (- now last-sweep-time) 1)
          (sweep-idle-connections epoll-fd now)
          ;; Rides the same 1 s gate rather than taking a third timer.
          ;; The interval is per connection and measured from that
          ;; connection's own last activity, so this needs to be asked
          ;; often enough, not on a schedule of its own — and a second
          ;; walk of the table per second would be the waste the gate
          ;; above exists to prevent.
          (keepalive-streams epoll-fd now)
          (setf last-sweep-time now))
        (when (>= (- now last-ping-time) *ws-ping-interval*)
          (ping-ws-connections epoll-fd)
          (setf last-ping-time now))))))

;;; ---------------------------------------------------------------------------
;;; Worker
;;; ---------------------------------------------------------------------------

(defun run-worker (host port worker-id handler ws-handler &optional listener)
  "Run a single worker: own listener, own epoll fd, own connections.
   Automatically restarts on unhandled errors (with backoff).

   LISTENER, when supplied, is an already-bound socket this worker
   adopts instead of binding one of its own. START-SERVER hands one to
   worker 0 when :PORT was 0, so that the kernel-assigned port is held
   by a real listener from the first instant it is knowable — see
   START-SERVER for why closing it and rebinding is not survivable.

   Adopted for the first pass only. A restart after a crash binds
   normally, and lands on the right port because by then PORT is the
   concrete number START-SERVER resolved — so the restarted worker
   rejoins its siblings rather than appearing somewhere new.

   The epoll fd is logged at startup because a worker was once seen to
   fail with EBADF on its own, and the number is what distinguished a
   descriptor closed underneath it from a wrong one arriving. That is
   settled now, and the answer was the worse of the two: CONNECTION-CLOSE
   closed descriptors with %CLOSE and left SB-BSD-SOCKETS' finalizer armed
   on a number the kernel had already reissued, so a later GC closed it
   under its new owner. An epoll fd is a bare number with nothing owning
   it, which made a worker the ideal victim. It reached shipping servers,
   not only test trees. See CONNECTION-CLOSE; the log line stays because
   it is what made the failure legible."
  (loop
    (handler-case
        (with-worker-urandom
        (let ((*connections* (make-hash-table :test #'eql))
              ;; Per-worker DNS cache. Workers share nothing in the hot
              ;; path, so each keeps its own table and no lock is needed.
              ;; Inert unless the app opts in via *DNS-CACHE-TTL*; a
              ;; worker restart drops its cache, which is harmless — the
              ;; next fetch to each host pays one getent again.
              (*dns-cache* (make-hash-table :test #'equal))
              (*epoll-ctl-buf* (make-array +epoll-event-size+
                                           :element-type '(unsigned-byte 8)))
              (*poll-buf* (make-array 8 :element-type '(unsigned-byte 8)))
              (*refusal-drain-buf*
                (make-array +refusal-drain-size+
                            :element-type '(unsigned-byte 8)))
              (*stream-drain-buf*
                (make-array 4096 :element-type '(unsigned-byte 8)))
              ;; CAR is the second the string was built for. 0 can never
              ;; be the current universal time, so the first response of
              ;; the worker's life formats and the rest of that second
              ;; read.
              (*http-date-cache* (cons 0 ""))
              ;; Same shape, different consumer: the pre-built static
              ;; responses want the date as a finished header line rather
              ;; than a string to hand a serializer.
              (*http-date-line-cache*
                (cons 0 (make-array 0 :element-type '(unsigned-byte 8)))))
          ;; Split the listener and epoll-fd bindings so a failure of
          ;; EPOLL-CREATE (EMFILE, ENOMEM) still tears down the bound
          ;; listener socket — a shared let* would leak it because the
          ;; cleanup form references EPOLL-FD which is never bound on
          ;; that path.
          ;; SHIFTF rather than a plain read: an adopted listener belongs
          ;; to this worker's first pass only. Reusing it after a restart
          ;; would hand the new event loop a socket the crashed pass
          ;; already closed in its own cleanup below.
          (let ((listener (or (shiftf listener nil)
                              (make-tcp-listener host port))))
            (unwind-protect
                 (let* ((epoll-fd (epoll-create))
                        ;; Bound per worker alongside the other share-nothing
                        ;; slots, so app-facing writers can arm EPOLLOUT
                        ;; without an epoll fd being threaded through an
                        ;; exported signature. STREAM-SEND may be called from
                        ;; a fetch callback or a producer the read branch
                        ;; never returns through, and a remainder left queued
                        ;; with nothing armed waits for an event that is not
                        ;; coming.
                        (*epoll-fd* epoll-fd))
                   (log-info "worker ~d started (epoll fd ~d)"
                             worker-id epoll-fd)
                   (epoll-add epoll-fd (socket-fd listener)
                              (logior +epollin+ +epollet+))
                   (unwind-protect
                       (run-event-loop listener epoll-fd handler ws-handler)
                     ;; Cleanup on worker crash or normal exit. Split the
                     ;; table into outbounds and everything else — outbounds
                     ;; go through CLOSE-OUTBOUND so their fetch callbacks
                     ;; fire even when the worker dies with requests in
                     ;; flight, matching the contract that every fetch's
                     ;; :then closure runs exactly once. Non-outbounds get
                     ;; the raw CONNECTION-CLOSE; we can't deliver anything
                     ;; to their clients at this point and the epoll fd is
                     ;; about to be torn down anyway. The collect step
                     ;; avoids mutating the hash table during MAPHASH — we
                     ;; only call UNREGISTER-CONNECTION (via close-outbound)
                     ;; in the dolist after the walk.
                     (let ((outbounds nil))
                       (maphash (lambda (fd conn)
                                  (declare (ignore fd))
                                  (if (connection-outbound-p conn)
                                      (push conn outbounds)
                                      (progn
                                        (maybe-reap-dns-process conn)
                                        (connection-close conn))))
                                *connections*)
                       (dolist (conn outbounds)
                         (close-outbound conn epoll-fd)))
                     (%close epoll-fd)))
              ;; Runs whether EPOLL-CREATE succeeded or raised.
              (sb-bsd-sockets:socket-close listener)))
          ;; Normal exit (shutdown requested)
          (log-info "worker ~d stopped" worker-id)
          (return)))
      (error (e)
        (log-error "worker ~d crashed: ~a — restarting" worker-id e)
        ;; 1-second backoff, sliced into *shutdown-poll-interval*
        ;; chunks so a SIGTERM arriving during the backoff is noticed
        ;; within one slice rather than after the full second.
        (let ((until (+ (get-internal-real-time)
                        internal-time-units-per-second)))
          (loop until (or *shutdown*
                          (>= (get-internal-real-time) until))
                do (sleep *shutdown-poll-interval*)))
        (when *shutdown* (return))))))

;;; ---------------------------------------------------------------------------
;;; CPU count
;;; ---------------------------------------------------------------------------

(defun cpu-count ()
  "Return the number of online CPU cores.
   Parses /sys/devices/system/cpu/online. Handles both the simple
   '0-N' shape and the multi-range 'A-B,C,D-E' shape produced by
   hotplugged or heterogeneous topologies (Intel E-cores offline,
   VMs with non-contiguous CPU masks, etc.). The old one-shot
   `dash + parse-integer` parser fell back to 1 on any comma,
   silently wasting cores on exactly the machines where we cared
   about parallelism most."
  (handler-case
      (with-open-file (s "/sys/devices/system/cpu/online")
        (let ((line (read-line s)))
          (loop with total = 0
                with start = 0
                with len = (length line)
                while (< start len)
                do (let* ((comma (or (position #\, line :start start) len))
                          (dash  (position #\- line :start start :end comma)))
                     (if dash
                         (let ((lo (parse-integer line :start start :end dash))
                               (hi (parse-integer line :start (1+ dash)
                                                       :end comma)))
                           (incf total (1+ (- hi lo))))
                         (progn
                           ;; Single-CPU token — still parse to validate.
                           (parse-integer line :start start :end comma)
                           (incf total)))
                     (setf start (1+ comma)))
                finally (return (max 1 total)))))
    (error ()
      (log-warn "cpu-count: could not parse topology, defaulting to 1 worker")
      1)))

;;; ---------------------------------------------------------------------------
;;; Server entry point
;;; ---------------------------------------------------------------------------

(defun start-server (&key (host #(127 0 0 1)) (port 8081) (workers (cpu-count))
                          handler ws-handler on-listen)
  "Start the server with WORKERS event loops on HOST:PORT.
   HOST is a 4-byte vector (default #(127 0 0 1) = localhost only;
   use #(0 0 0 0) to listen on all interfaces).
   HANDLER: function (request) -> response or :UPGRADE.
   WS-HANDLER: function (connection frame) -> bytes or NIL.
   Each worker gets its own listener socket (SO_REUSEPORT), epoll fd,
   and connection table. Ctrl-C shuts down all workers.

   PORT may be 0, in which case the kernel assigns one and ON-LISTEN —
   a function of one argument, called once, on the calling thread, after
   every worker thread has been spawned — receives the port actually
   bound. It is called for a fixed port too, so a caller need not know
   which kind it asked for. This function does not return until
   shutdown, so a callback is the only way to answer the question.

   A caller that binds port 0 itself, closes, and passes the number here
   has a race. Every listener sets SO_REUSEPORT, so a second process
   handed that number in the gap does not fail its bind: both hold the
   port and the kernel splits traffic between them, silently — measured
   on this repo at 40 requests split 17/23, with nothing in either log.
   Resolving here means the port is never free between being chosen and
   being served."
  (unless (and (integerp workers) (plusp workers))
    (error "start-server: :workers must be a positive integer, got ~a"
           workers))
  ;; WS-SEND checks this too, but only WS-SEND does, and a handler that
  ;; returns a frame instead of pushing one never goes through it — that
  ;; path appends directly. A zero here would therefore leave the primary
  ;; documented shape with no time bound on an undrained queue at all,
  ;; while three docs promise there is no setting that disables it.
  (unless (and (realp *write-stall-timeout*) (plusp *write-stall-timeout*))
    (error "start-server: *write-stall-timeout* is ~s; it must be positive. ~
            It is the only deadline on a write queue the peer may never ~
            drain — the idle timeouts are measured on last activity, which ~
            a peer that stops reading keeps fresh by continuing to send."
           *write-stall-timeout*))
  (setf *shutdown* nil)
  ;; Save the previous SIGPIPE and SIGTERM handlers so start-server can
  ;; be called from inside a host SBCL image (a REPL, a test runner, an
  ;; orchestrator) without permanently stealing the signals.
  ;;   SIGPIPE -> :ignore so writes to a broken peer return EPIPE
  ;;   SIGTERM -> set *shutdown*, matching Ctrl-C's path
  ;;
  ;; Two nested unwind-protects, one per signal: SIGPIPE in the
  ;; outer, SIGTERM in the inner. If SIGTERM install raises, the
  ;; outer cleanup still restores SIGPIPE. If both succeed and the
  ;; body raises (MAKE-THREAD exhaustion, etc.), both cleanups run.
  ;; A single parallel let would leak SIGPIPE if the SIGTERM init
  ;; form raised before the body was entered.
  (let ((prev-sigpipe (sb-sys:enable-interrupt sb-unix:sigpipe :ignore)))
    (unwind-protect
         (let ((prev-sigterm (sb-sys:enable-interrupt sb-unix:sigterm
                               (lambda (signal info context)
                                 (declare (ignore signal info context))
                                 (setf *shutdown* t)))))
           (unwind-protect
                ;; Resolve port 0 here, once, rather than letting the
                ;; workers each pass it to bind(2). Two things break if
                ;; they do: N workers on port 0 land on N *different*
                ;; ephemeral ports, and RUN-WORKER's restart loop rebinds,
                ;; so a crashed worker would come back somewhere its
                ;; siblings are not. Neither shows up as an error — the
                ;; server keeps serving on whichever port the caller
                ;; happens to have been told about.
                ;;
                ;; The socket bound here is handed to worker 0 rather than
                ;; closed and rebound: a port that is closed and rebound is
                ;; free for an instant, and the docstring above is about
                ;; what gets into that instant.
                (let* ((listener0 (when (zerop port)
                                    (make-tcp-listener host port)))
                       (port (if listener0
                                 (nth-value 1 (sb-bsd-sockets:socket-name
                                               listener0))
                                 port)))
                  (log-info "starting ~d worker~:p on ~a"
                            workers (format-peer-addr host port))
                  ;; Accumulate threads incrementally rather than via LOOP
                  ;; COLLECT: if MAKE-THREAD raises on iteration N, the
                  ;; partial list in THREADS still covers workers 0..N-1 so
                  ;; the drain cleanup can signal and join them.
                  (let ((threads nil))
                    (unwind-protect
                         (progn
                           (dotimes (i workers)
                             (let ((id i)
                                   (adopted (when (zerop i) listener0)))
                               (push (sb-thread:make-thread
                                      (lambda ()
                                        (run-worker host port id
                                                    handler ws-handler
                                                    adopted))
                                      :name (format nil "web-skeleton-~d" i))
                                     threads)
                               ;; Ownership passes to the thread only once
                               ;; the thread exists. Clearing LISTENER0
                               ;; after a successful MAKE-THREAD — never
                               ;; before — is what keeps the cleanup below
                               ;; and worker 0 from both closing it, while
                               ;; still closing it if MAKE-THREAD raised.
                               (when adopted (setf listener0 nil))))
                           ;; After the spawn loop, so a caller that
                           ;; connects the moment it learns the port finds
                           ;; workers on their way up rather than a bound
                           ;; socket with nobody behind it. The listener is
                           ;; already accepting into its backlog either way.
                           (when on-listen (funcall on-listen port))
                           (handler-case
                               ;; Main thread waits for interrupt or SIGTERM
                               (loop (sleep *shutdown-poll-interval*)
                                     (when *shutdown*
                                       (log-info "shutting down")
                                       (return)))
                             (sb-sys:interactive-interrupt ()
                               (format t "~%")
                               (log-info "interrupted — shutting down"))))
                      ;; Signal workers to drain and stop. Runs on normal
                      ;; exit, interactive-interrupt, AND a MAKE-THREAD
                      ;; raise partway through the spawn loop.
                      (setf *shutdown* t)
                      ;; Non-NIL only if MAKE-THREAD raised before worker 0
                      ;; adopted it, in which case nothing else will ever
                      ;; close it.
                      (when listener0
                        (ignore-errors
                         (sb-bsd-sockets:socket-close listener0)))
                      (dolist (thread threads)
                        (ignore-errors
                         (sb-thread:join-thread thread
                                                :timeout (+ *drain-timeout* 3))))
                      ;; Workers have drained; run any app-registered cleanup
                      ;; before returning. Hooks own their own error handling —
                      ;; one raising hook cannot block the rest, cannot block
                      ;; the "stopped" log, and cannot prevent START-SERVER
                      ;; from returning to its caller.
                      (run-shutdown-hooks)
                      (log-info "stopped"))))
             ;; Inner cleanup: restore SIGTERM
             (ignore-errors
              (sb-sys:enable-interrupt sb-unix:sigterm prev-sigterm))))
      ;; Outer cleanup: restore SIGPIPE (runs even if SIGTERM install raised)
      (ignore-errors
       (sb-sys:enable-interrupt sb-unix:sigpipe prev-sigpipe)))))
