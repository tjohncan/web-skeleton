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
   Sets SO_REUSEADDR, SO_REUSEPORT, and non-blocking. Returns the socket."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
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

(defparameter *max-events* 64
  "Maximum events to process per epoll_wait call.")

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
      ;; Normal HTTP response
      (t
       (log-debug "~a ~a -> ~d"
                  (http-request-method request)
                  (http-request-path request)
                  (http-response-status response))
       (values response nil)))))

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
        (let ((conn (make-client-connection client-socket)))
          (register-connection conn)
          (epoll-add epoll-fd (connection-fd conn)
                     (logior +epollin+ +epollet+))
          (log-debug "accepted fd ~d ~a" (connection-fd conn)
                     (or (connection-remote-addr conn) ""))
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
   If CONN is :awaiting, also closes its outbound connection to prevent
   use-after-close on fd reuse. MAYBE-REAP-DNS-PROCESS runs on every
   cleaned-up connection so a half-finished DNS lookup never leaks."
  (let ((fd (connection-fd conn)))
    (when (>= fd 0)
      ;; If parked waiting for a fetch, close the orphaned outbound too
      (when (eq (connection-state conn) :awaiting)
        (let ((out-fd (connection-awaiting-fd conn)))
          (when (>= out-fd 0)
            (let ((out-conn (lookup-connection out-fd)))
              (when out-conn
                (ignore-errors (epoll-remove epoll-fd out-fd))
                (unregister-connection out-conn)
                (maybe-reap-dns-process out-conn)
                (connection-close out-conn)
                (log-debug "closed orphaned outbound fd ~d" out-fd))))))
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
  ;; Phase 1: initiate shutdown on each connection
  (let ((to-close nil))
    (maphash (lambda (fd conn)
               (declare (ignore fd))
               (cond
                 ;; Outbound connections — close immediately
                 ((connection-outbound-p conn)
                  (push conn to-close))
                 ;; HTTP connections still reading — nothing to drain
                 ((member (connection-state conn) '(:read-http :read-body :awaiting))
                  (push conn to-close))
                 ;; WebSocket — send close frame (1001 = going away)
                 ((eq (connection-state conn) :websocket)
                  (connection-queue-write conn (build-ws-close 1001))
                  (setf (connection-state conn) :closing)
                  (epoll-modify epoll-fd (connection-fd conn)
                               (logior +epollout+ +epollet+)))
                 ;; :write-response, :ws-upgrade, :closing — let them finish
                 (t nil)))
             *connections*)
    (dolist (conn to-close)
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
      (let ((n (epoll-wait epoll-fd event-buf *max-events* 200)))
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
  "Handle EPOLLIN on a client connection."
  (handler-case
      (ecase (connection-state conn)
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
                                        :test #'string=)
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
        (:awaiting nil))
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
  "Handle EPOLLOUT on a client connection."
  (handler-case
      (let ((result (connection-on-write conn)))
        (case result
          (:done
           ;; All bytes sent — next action depends on state
           (ecase (connection-state conn)
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
              (return-from handle-client-write :keep-alive))))
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
  (let ((listener-fd (socket-fd listener-socket))
        (last-ping-time (get-universal-time))
        (event-buf (make-epoll-event-buf *max-events*)))
    (loop
      (when *shutdown*
        (drain-connections listener-socket epoll-fd event-buf)
        (return))
      (let ((n (epoll-wait epoll-fd event-buf *max-events*
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
                                ;; Inbound client connection
                                (progn
                                  ;; Error or hangup — close and skip
                                  (when (or (logtest flags +epollerr+)
                                            (logtest flags +epollhup+))
                                    (close-connection conn epoll-fd)
                                    (return-from handle-event))
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
                                                            handler ws-handler))))))))))))))
      ;; Periodic maintenance
      (let ((now (get-universal-time)))
        (sweep-idle-connections epoll-fd now)
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
              (*epoll-ctl-buf* (make-array 12 :element-type '(unsigned-byte 8)))
              (*poll-buf* (make-array 8 :element-type '(unsigned-byte 8))))
          (let* ((listener (make-tcp-listener host port))
                 (epoll-fd (epoll-create)))
            (log-info "worker ~d started (epoll fd ~d)" worker-id epoll-fd)
            (epoll-add epoll-fd (socket-fd listener)
                       (logior +epollin+ +epollet+))
            (unwind-protect
                (run-event-loop listener epoll-fd handler ws-handler)
              ;; Cleanup: reap any in-flight DNS subprocesses first so
              ;; a worker crash doesn't leak zombie getent children,
              ;; then close all connections, listener, epoll fd.
              (maphash (lambda (fd conn)
                         (declare (ignore fd))
                         (maybe-reap-dns-process conn)
                         (connection-close conn))
                       *connections*)
              (sb-bsd-sockets:socket-close listener)
              (%close epoll-fd)))
          ;; Normal exit (shutdown requested)
          (log-info "worker ~d stopped" worker-id)
          (return))
      (error (e)
        (log-error "worker ~d crashed: ~a — restarting in 1s" worker-id e)
        (sleep 1)
        (when *shutdown* (return))))))

;;; ---------------------------------------------------------------------------
;;; CPU count
;;; ---------------------------------------------------------------------------

(defun cpu-count ()
  "Return the number of online CPU cores.
   Parses the 0-N format from /sys/devices/system/cpu/online.
   Falls back to 1 for exotic topologies (comma-separated ranges, etc.)."
  (handler-case
      (with-open-file (s "/sys/devices/system/cpu/online")
        (let* ((line (read-line s))
               (dash (position #\- line)))
          (if dash
              (1+ (parse-integer (subseq line (1+ dash))))
              1)))
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
  ;; Ignore SIGPIPE — writing to a broken connection must return EPIPE,
  ;; not kill the process. SBCL typically ignores it, but not guaranteed.
  (sb-sys:enable-interrupt sb-unix:sigpipe :ignore)
  ;; SIGTERM triggers graceful shutdown (same as Ctrl-C)
  (sb-sys:enable-interrupt sb-unix:sigterm
    (lambda (signal info context)
      (declare (ignore signal info context))
      (setf *shutdown* t)))
  (log-info "starting ~d worker~:p on ~{~d~^.~}:~d" workers (coerce host 'list) port)
  (let ((threads (loop for i from 0 below workers
                       collect (sb-thread:make-thread
                                (let ((id i))
                                  (lambda () (run-worker host port id handler ws-handler)))
                                :name (format nil "web-skeleton-~d" i)))))
    (unwind-protect
        (handler-case
            ;; Main thread waits for interrupt or SIGTERM
            (loop (sleep *shutdown-poll-interval*)
                  (when *shutdown*
                    (log-info "shutting down")
                    (return)))
          (sb-sys:interactive-interrupt ()
            (format t "~%")
            (log-info "interrupted — shutting down")))
      ;; Signal workers to drain and stop
      (setf *shutdown* t)
      (dolist (thread threads)
        (ignore-errors (sb-thread:join-thread thread
                                              :timeout (+ *drain-timeout* 3))))
      ;; Workers have drained; run any app-registered cleanup before
      ;; returning. Hooks own their own error handling — one raising
      ;; hook cannot block the rest, cannot block the "stopped" log,
      ;; and cannot prevent START-SERVER from returning to its caller.
      (run-shutdown-hooks)
      (log-info "stopped"))))
