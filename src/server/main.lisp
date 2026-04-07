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

(defun make-tcp-listener (port)
  "Create a TCP socket, bind to 0.0.0.0:PORT, listen with a backlog of 128.
   Sets SO_REUSEADDR, SO_REUSEPORT, and non-blocking. Returns the socket."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (set-socket-option-int (socket-fd socket)
                           +sol-socket+ +so-reuseport+ 1)
    (sb-bsd-sockets:socket-bind socket #(0 0 0 0) port)
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

(defparameter *idle-timeout* 30
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
               (let* ((ws-p (eq (connection-state conn) :websocket))
                      (timeout (if ws-p *ws-idle-timeout* *idle-timeout*)))
                 (when (and (> timeout 0)
                            (> (- now (connection-last-active conn)) timeout))
                   (push conn idle))))
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

(defvar *shutdown* nil
  "Set to T to signal all workers to exit.")

(defparameter *drain-timeout* 5
  "Seconds to wait for connections to drain during graceful shutdown.")

(defparameter *max-events* 64
  "Maximum events to process per epoll_wait call.")

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
             (values (make-error-response 400) nil))))
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
   Returns T if a connection was accepted, NIL if none pending (EAGAIN)."
  (let ((client-socket (sb-bsd-sockets:socket-accept listener-socket)))
    (unless client-socket
      (return-from accept-connection nil))
    (handler-case
        (let ((conn (make-client-connection client-socket)))
          (register-connection conn)
          (epoll-add epoll-fd (connection-fd conn)
                     (logior +epollin+ +epollet+))
          (log-debug "accepted fd ~d" (connection-fd conn))
          t)
      (error (e)
        (log-error "accept failed: ~a" e)
        (ignore-errors (sb-bsd-sockets:socket-close client-socket))
        nil))))

;;; ---------------------------------------------------------------------------
;;; Close and clean up a connection
;;; ---------------------------------------------------------------------------

(defun close-connection (conn epoll-fd)
  "Remove from epoll, unregister, close fd."
  (let ((fd (connection-fd conn)))
    (when (>= fd 0)
      (ignore-errors (epoll-remove epoll-fd fd))
      (unregister-connection conn)
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
               (case (connection-state conn)
                 ;; HTTP connections still reading — nothing to drain
                 ((:read-http :read-body)
                  (push conn to-close))
                 ;; WebSocket — send close frame (1001 = going away)
                 (:websocket
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
              (let ((request (connection-parse-request conn)))
                (log-debug "~a ~a~@[?~a~] HTTP/~a"
                           (http-request-method request)
                           (http-request-path request)
                           (http-request-query request)
                           (http-request-version request))
                (multiple-value-bind (response upgrade-p)
                    (dispatch-request request handler)
                  ;; Queue the response for writing
                  (let ((bytes (if (typep response
                                         '(simple-array (unsigned-byte 8) (*)))
                                   response
                                   (format-response response))))
                    (connection-queue-write conn bytes)
                    (setf (connection-state conn)
                          (if upgrade-p :ws-upgrade :write-response))
                    (epoll-modify epoll-fd (connection-fd conn)
                                 (logior +epollout+ +epollet+))))))
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
                  (websocket-on-read conn ws-handler)
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
                  ;; No response needed (shouldn't normally happen)
                  (t nil))))
             (:close
              (close-connection conn epoll-fd))
             ;; :continue — wait for more data
             ))))
    (http-parse-error (e)
      (log-warn "parse error fd ~d: ~a" (connection-fd conn)
                (http-parse-error-message e))
      (close-connection conn epoll-fd))
    (error (e)
      (log-error "error fd ~d: ~a" (connection-fd conn) e)
      (close-connection conn epoll-fd))))

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
              ;; Normal HTTP — close (no keep-alive yet)
              (close-connection conn epoll-fd))
             (:ws-upgrade
              ;; WebSocket handshake sent — preserve any data past the HTTP request
              (let* ((http-end (+ (connection-header-end conn) 4))
                     (buffered (connection-read-pos conn))
                     (extra (- buffered http-end)))
                (when (> extra 0)
                  (replace (connection-read-buf conn) (connection-read-buf conn)
                           :start1 0 :start2 http-end :end2 buffered))
                (setf (connection-read-pos conn) (max extra 0)
                      (connection-state conn) :websocket))
              (epoll-modify epoll-fd (connection-fd conn)
                           (logior +epollin+ +epollet+)))
             (:websocket
              ;; WebSocket frame response sent — back to reading
              (epoll-modify epoll-fd (connection-fd conn)
                           (logior +epollin+ +epollet+)))
             (:closing
              ;; Close frame sent — disconnect
              (close-connection conn epoll-fd))))
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
      (let ((n (epoll-wait epoll-fd event-buf *max-events* 1000)))
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
                       ;; Event on a client connection
                       (t
                        (let ((conn (lookup-connection fd)))
                          (when conn
                            ;; Error or hangup — close and skip to next event
                            (when (or (logtest flags +epollerr+)
                                      (logtest flags +epollhup+))
                              (close-connection conn epoll-fd)
                              (return-from handle-event))
                            ;; Readable
                            (when (logtest flags +epollin+)
                              (handle-client-read conn epoll-fd handler ws-handler))
                            ;; Writable (check conn still alive after read handling)
                            (when (and (logtest flags +epollout+)
                                       (lookup-connection fd))
                              (handle-client-write conn epoll-fd))))))))))
      ;; Periodic maintenance
      (let ((now (get-universal-time)))
        (sweep-idle-connections epoll-fd now)
        (when (>= (- now last-ping-time) *ws-ping-interval*)
          (ping-ws-connections epoll-fd)
          (setf last-ping-time now))))))

;;; ---------------------------------------------------------------------------
;;; Worker
;;; ---------------------------------------------------------------------------

(defun run-worker (port worker-id handler ws-handler)
  "Run a single worker: own listener, own epoll fd, own connections."
  (let ((*connections* (make-hash-table :test #'eql))
        (*epoll-ctl-buf* (make-array 12 :element-type '(unsigned-byte 8))))
    (let* ((listener (make-tcp-listener port))
           (epoll-fd (epoll-create)))
      (log-info "worker ~d started (epoll fd ~d)" worker-id epoll-fd)
      (epoll-add epoll-fd (socket-fd listener)
                 (logior +epollin+ +epollet+))
      (unwind-protect
          (run-event-loop listener epoll-fd handler ws-handler)
        ;; Cleanup: close all connections, listener, epoll fd
        (maphash (lambda (fd conn)
                   (declare (ignore fd))
                   (connection-close conn))
                 *connections*)
        (sb-bsd-sockets:socket-close listener)
        (%close epoll-fd)
        (log-info "worker ~d stopped" worker-id)))))

;;; ---------------------------------------------------------------------------
;;; CPU count
;;; ---------------------------------------------------------------------------

(defun cpu-count ()
  "Return the number of online CPU cores."
  (handler-case
      (with-open-file (s "/sys/devices/system/cpu/online")
        (let* ((line (read-line s))
               (dash (position #\- line)))
          (if dash
              (1+ (parse-integer (subseq line (1+ dash))))
              1)))
    (error () 1)))

;;; ---------------------------------------------------------------------------
;;; Server entry point
;;; ---------------------------------------------------------------------------

(defun start-server (&key (port 8081) (workers (cpu-count))
                          handler ws-handler)
  "Start the server with WORKERS event loops on PORT.
   HANDLER: function (request) -> response or :UPGRADE.
   WS-HANDLER: function (connection frame) -> bytes or NIL.
   Each worker gets its own listener socket (SO_REUSEPORT), epoll fd,
   and connection table. Ctrl-C shuts down all workers."
  (setf *shutdown* nil)
  ;; SIGTERM triggers graceful shutdown (same as Ctrl-C)
  (sb-sys:enable-interrupt sb-unix:sigterm
    (lambda (signal info context)
      (declare (ignore signal info context))
      (setf *shutdown* t)))
  (log-info "starting ~d worker~:p on port ~d" workers port)
  (let ((threads (loop for i from 0 below workers
                       collect (sb-thread:make-thread
                                (let ((id i))
                                  (lambda () (run-worker port id handler ws-handler)))
                                :name (format nil "web-skeleton-~d" i)))))
    (unwind-protect
        (handler-case
            ;; Main thread waits for interrupt or SIGTERM
            (loop (sleep 1)
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
      (log-info "stopped"))))
