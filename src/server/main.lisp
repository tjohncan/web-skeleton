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
  "Maximum connections per worker. New accepts are dropped when full.")

(defparameter *idle-timeout* 10
  "Seconds before an idle HTTP connection is closed. 0 to disable.")

(defparameter *ws-idle-timeout* 86400
  "Seconds before an inactive WebSocket connection is closed. 0 to disable.
   Inactivity = no text or binary frames from the client (pongs don't count).")

(defparameter *ws-ping-interval* 30
  "Seconds between server-initiated WebSocket pings.")

(defparameter *ws-max-missed-pongs* 3
  "Close a WebSocket connection after this many consecutive unanswered pings.")

(defun sweep-idle-connections (epoll-fd now)
  "Close connections that have been idle too long.
   HTTP uses *idle-timeout*. WebSocket uses *ws-idle-timeout*."
  (let ((idle nil))
    (maphash (lambda (fd conn)
               (declare (ignore fd))
               ;; Outbound connections are cleaned up via their paired
               ;; inbound's :awaiting timeout — see close-connection
               (unless (connection-outbound-p conn)
                 (let* ((state (connection-state conn))
                        (timeout (cond
                                   ((eq state :websocket) *ws-idle-timeout*)
                                   ((eq state :awaiting)  *fetch-timeout*)
                                   (t                     *idle-timeout*))))
                   (when (and (> timeout 0)
                              (> (- now (connection-last-active conn)) timeout))
                     (push conn idle)))))
             *connections*)
    (dolist (conn idle)
      (log-debug "idle timeout fd ~d (~a)"
                 (connection-fd conn) (connection-state conn))
      (close-connection conn epoll-fd))))

(defun ping-ws-connections (epoll-fd)
  "Send pings to WebSocket connections and close dead ones.
   Dead = exceeded *ws-max-missed-pongs* consecutive unanswered pings.
   Skips connections with a write in progress (they're clearly not dead)."
  (let ((dead nil)
        (ping-frame (build-ws-ping)))
    (maphash (lambda (fd conn)
               (declare (ignore fd))
               (when (eq (connection-state conn) :websocket)
                 (cond
                   ;; Dead — too many missed pongs
                   ((>= (connection-missed-pongs conn) *ws-max-missed-pongs*)
                    (push conn dead))
                   ;; No write in progress — send ping
                   ((>= (connection-write-pos conn) (connection-write-end conn))
                    (incf (connection-missed-pongs conn))
                    (connection-queue-write conn ping-frame)
                    (epoll-modify epoll-fd (connection-fd conn)
                                 (logior +epollout+ +epollet+))))))
             *connections*)
    (dolist (conn dead)
      (log-info "ws dead (missed ~d pongs) fd ~d"
                (connection-missed-pongs conn) (connection-fd conn))
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
   and each worker's event-loop epoll timeout. Also governs the worker's
   periodic-maintenance cadence (idle-connection sweep, WebSocket ping).
   Default 1 second balances wake-up overhead against shutdown
   responsiveness. Test harnesses bind this to a small value (e.g. 0.05)
   so teardown doesn't wait a full second per call. Float accepted —
   the worker converts to ms for epoll_wait.")

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
      ;; Pre-formatted response (e.g., static file — already bytes)
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

(defun accept-connection (listener-socket epoll-fd)
  "Accept a pending connection and register it with epoll.
   Returns T if a connection was accepted, NIL if none pending (EAGAIN).
   Drops the connection if the per-worker limit is reached."
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
      (log-warn "connection limit reached (~d), dropping new accept"
                *max-connections*)
      (ignore-errors (sb-bsd-sockets:socket-close client-socket))
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

(defun close-connection (conn epoll-fd)
  "Remove from epoll, unregister, close fd.
   If CONN is :awaiting, also closes its outbound connection via
   CLOSE-OUTBOUND — which fires the app's fetch cleanup callback if
   the outbound was still carrying one, so DB handles / metrics /
   rate-limit counters get a defined moment to run their teardown
   even on inbound-driven aborts (drain, idle timeout, I/O error).
   MAYBE-REAP-DNS-PROCESS runs on every cleaned-up connection so a
   half-finished DNS lookup never leaks."
  (let ((fd (connection-fd conn)))
    (when (>= fd 0)
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
                 ((member (connection-state conn) '(:read-http :read-body :awaiting))
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
                  (cond
                    ((< (connection-write-pos conn) (connection-write-end conn))
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
      (close-connection conn epoll-fd)))
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
                (let ((conn-header (get-header request "connection")))
                  (setf (connection-close-after-p conn)
                        (cond
                          ((and conn-header
                                (connection-header-has-token-p conn-header "close"))
                           t)
                          ((and conn-header
                                (connection-header-has-token-p conn-header "keep-alive"))
                           nil)
                          ((string= (http-request-version request) "1.0") t)
                          (t nil))))
                (multiple-value-bind (response upgrade-p)
                    (dispatch-request request handler)
                  (cond
                    ;; Outbound fetch — park and initiate
                    ((typep response 'http-fetch-continuation)
                     (initiate-fetch conn epoll-fd response))
                    ;; Normal response — queue for writing
                    (t
                     ;; HTTP/1.0 keep-alive: echo the header so the client
                     ;; knows the connection will persist
                     (when (and (not (connection-close-after-p conn))
                                (string= (http-request-version request) "1.0")
                                (typep response 'http-response))
                       (set-response-header response "connection" "keep-alive"))
                     ;; HEAD responses: set Content-Length but strip body
                     (when (and (eq (http-request-method request) :HEAD)
                                (typep response 'http-response)
                                (http-response-body response))
                       (let ((body (sb-ext:string-to-octets
                                    (http-response-body response)
                                    :external-format :utf-8)))
                         (unless (assoc "content-length"
                                        (http-response-headers response)
                                        :test #'string-equal)
                           (set-response-header response "content-length"
                                                (write-to-string (length body)))))
                       (setf (http-response-body response) nil))
                     (let ((bytes (if (typep response
                                            '(simple-array (unsigned-byte 8) (*)))
                                      response
                                      (format-response response))))
                       (connection-queue-write conn bytes)
                       (setf (connection-state conn)
                             (if upgrade-p :ws-upgrade :write-response))
                       (epoll-modify epoll-fd (connection-fd conn)
                                    (logior +epollout+ +epollet+))))))))
             (:send-continue
              ;; connection-on-read queued the 100 Continue response and
              ;; set state :sending-100-continue. Flip to EPOLLOUT so
              ;; handle-client-write flushes it; the body arrives next.
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
                      ;; ws-send may have left partial bytes on the wire,
                      ;; corrupting the stream. A close frame would be
                      ;; misinterpreted as continuation of the partial frame.
                      (log-warn "ws handler error fd ~d: ~a"
                                (connection-fd conn) e)
                      (close-connection conn epoll-fd)
                      (return-from handle-client-read)))
                (cond
                  ;; Close requested — send close frame back, then shut down
                  ((eq response :close)
                   (when close-frame
                     (connection-queue-write conn close-frame)
                     (epoll-modify epoll-fd (connection-fd conn)
                                  (logior +epollout+ +epollet+)))
                   ;; Mark as closing so on-write knows to disconnect
                   (setf (connection-state conn) :closing))
                  ;; Response frame(s) to send
                  (response
                   (connection-queue-write conn response)
                   (setf (connection-state conn) :websocket)
                   (epoll-modify epoll-fd (connection-fd conn)
                                (logior +epollout+ +epollet+)))
                  ;; No response — re-arm edge trigger in case kernel
                  ;; still has data we couldn't buffer earlier.
                  ;; But if buffer is at capacity (no frames were consumed),
                  ;; a partial frame exceeds our limit — close with 1009
                  ;; to prevent a spin loop.
                  (t
                   (if (>= (connection-read-pos conn)
                           (length (connection-read-buf conn)))
                       (progn
                         (log-warn "ws buffer full, no parseable frames fd ~d"
                                   (connection-fd conn))
                         (connection-queue-write conn (build-ws-close 1009))
                         (setf (connection-state conn) :closing)
                         (epoll-modify epoll-fd (connection-fd conn)
                                      (logior +epollout+ +epollet+)))
                       (epoll-modify epoll-fd (connection-fd conn)
                                    (logior +epollin+ +epollet+)))))))
             (:close
              (close-connection conn epoll-fd))
             ;; :continue — wait for more data
             )))
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
      ;; Send 400 before closing so the client gets a proper HTTP response
      (handler-case
          (let ((resp (make-error-response 400)))
            (set-response-header resp "connection" "close")
            (let ((err-bytes (format-response resp)))
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
            (let ((err-bytes (format-response resp)))
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
                    (setf (connection-read-pos conn) (max extra 0)
                          (connection-write-buf conn) nil
                          (connection-write-pos conn) 0
                          (connection-write-end conn) 0
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
                (setf (connection-read-pos conn) (max extra 0)
                      (connection-write-buf conn) nil
                      (connection-write-pos conn) 0
                      (connection-write-end conn) 0
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
             (:closing
              ;; Close frame sent — disconnect
              (close-connection conn epoll-fd))
             (:sending-100-continue
              ;; 100 Continue flushed — switch to reading the body.
              ;; Returning :keep-alive tells the main loop to re-enter
              ;; handle-client-read immediately in case the body is
              ;; already buffered (edge-triggered epoll won't re-fire
              ;; on user-space bytes).
              (setf (connection-write-buf conn) nil
                    (connection-write-pos conn) 0
                    (connection-write-end conn) 0
                    (connection-state conn) :read-body)
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
                           (round (* *shutdown-poll-interval* 1000)))))
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
                                  ;; Writable (check still alive after read)
                                  (when (and (logtest flags +epollout+)
                                             (lookup-connection fd))
                                    (when (eq (handle-client-write conn epoll-fd)
                                              :keep-alive)
                                      ;; Keep-alive reset — read immediately in case
                                      ;; the next request is already buffered
                                      ;; (edge-triggered epoll won't re-notify)
                                      (when (lookup-connection fd)
                                        (handle-client-read conn epoll-fd
                                                            handler ws-handler))))
                                  ;; Error/hangup — close only if the
                                  ;; read/write path didn't already
                                  ;; tear the connection down.
                                  (when (and (or (logtest flags +epollerr+)
                                                 (logtest flags +epollhup+))
                                             (lookup-connection fd))
                                    (close-connection conn epoll-fd))))))))))))
      ;; Periodic maintenance — both scans gated on elapsed wall
      ;; clock so a busy epoll loop doesn't walk the connection
      ;; table multiple times per second. 1 s is fine: idle
      ;; timeouts are measured in 10 s+ units, so sub-second sweep
      ;; granularity is pure waste.
      (let ((now (get-universal-time)))
        (when (>= (- now last-sweep-time) 1)
          (sweep-idle-connections epoll-fd now)
          (setf last-sweep-time now))
        (when (>= (- now last-ping-time) *ws-ping-interval*)
          (ping-ws-connections epoll-fd)
          (setf last-ping-time now))))))

;;; ---------------------------------------------------------------------------
;;; Worker
;;; ---------------------------------------------------------------------------

(defun run-worker (host port worker-id handler ws-handler)
  "Run a single worker: own listener, own epoll fd, own connections.
   Automatically restarts on unhandled errors (with backoff)."
  (loop
    (handler-case
        (let ((*connections* (make-hash-table :test #'eql))
              (*epoll-ctl-buf* (make-array +epoll-event-size+
                                           :element-type '(unsigned-byte 8)))
              (*poll-buf* (make-array 8 :element-type '(unsigned-byte 8))))
          (let* ((listener (make-tcp-listener host port))
                 (epoll-fd (epoll-create)))
            (log-info "worker ~d started (epoll fd ~d)" worker-id epoll-fd)
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
              (sb-bsd-sockets:socket-close listener)
              (%close epoll-fd)))
          ;; Normal exit (shutdown requested)
          (log-info "worker ~d stopped" worker-id)
          (return))
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
                for comma = (or (position #\, line :start start) len)
                for dash  = (position #\- line :start start :end comma)
                do (if dash
                       (let ((lo (parse-integer line :start start :end dash))
                             (hi (parse-integer line :start (1+ dash)
                                                     :end comma)))
                         (incf total (1+ (- hi lo))))
                       (progn
                         ;; Single-CPU token — still parse to validate.
                         (parse-integer line :start start :end comma)
                         (incf total)))
                   (setf start (1+ comma))
                finally (return (max 1 total)))))
    (error ()
      (log-warn "cpu-count: could not parse topology, defaulting to 1 worker")
      1)))

;;; ---------------------------------------------------------------------------
;;; Server entry point
;;; ---------------------------------------------------------------------------

(defun start-server (&key (host #(127 0 0 1)) (port 8081) (workers (cpu-count))
                          handler ws-handler)
  "Start the server with WORKERS event loops on HOST:PORT.
   HOST is a 4-byte vector (default #(127 0 0 1) = localhost only;
   use #(0 0 0 0) to listen on all interfaces).
   HANDLER: function (request) -> response or :UPGRADE.
   WS-HANDLER: function (connection frame) -> bytes or NIL.
   Each worker gets its own listener socket (SO_REUSEPORT), epoll fd,
   and connection table. Ctrl-C shuts down all workers."
  (setf *shutdown* nil)
  ;; Save the previous SIGPIPE and SIGTERM handlers so start-server can
  ;; be called from inside a host SBCL image (a REPL, a test runner, an
  ;; orchestrator) without permanently stealing the signals.
  ;;   SIGPIPE -> :ignore so writes to a broken peer return EPIPE
  ;;   SIGTERM -> set *shutdown*, matching Ctrl-C's path
  ;;
  ;; The restore lives in an OUTER unwind-protect wrapping the entire
  ;; thread lifecycle — it MUST NOT be nested inside the same unwind-
  ;; protect that drains the worker threads, because a MAKE-THREAD
  ;; raise partway through the spawn loop (thread exhaustion,
  ;; setrlimit, etc.) would blow past both the drain and the restore
  ;; and leave the host image with the framework's SIGPIPE / SIGTERM
  ;; handlers installed forever.
  (let ((prev-sigpipe (sb-sys:enable-interrupt sb-unix:sigpipe :ignore))
        (prev-sigterm (sb-sys:enable-interrupt sb-unix:sigterm
                        (lambda (signal info context)
                          (declare (ignore signal info context))
                          (setf *shutdown* t)))))
    (unwind-protect
         (progn
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
                      (let ((id i))
                        (push (sb-thread:make-thread
                               (lambda ()
                                 (run-worker host port id
                                             handler ws-handler))
                               :name (format nil "web-skeleton-~d" i))
                              threads)))
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
      ;; Hand the signals back to whoever had them before we started.
      ;; Each restore gets its own IGNORE-ERRORS — a single wrapper
      ;; would short-circuit SIGPIPE if SIGTERM restore raised,
      ;; leaking the framework's SIGPIPE handler into the host image.
      (ignore-errors
       (sb-sys:enable-interrupt sb-unix:sigterm prev-sigterm))
      (ignore-errors
       (sb-sys:enable-interrupt sb-unix:sigpipe prev-sigpipe)))))
