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
;;; Test page
;;; ---------------------------------------------------------------------------

(defparameter *test-page*
  "<!DOCTYPE html>
<html>
<head>
<meta charset=\"utf-8\">
<title>web-skeleton</title>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body { font-family: monospace; background: #111; color: #eee; padding: 2em; }
  h1 { margin-bottom: 1em; color: #0f0; }
  #log { background: #000; padding: 1em; height: 400px; overflow-y: auto;
         border: 1px solid #333; margin-bottom: 1em; white-space: pre-wrap; }
  .status { color: #0ff; }
  .sent { color: #888; }
  .recv { color: #0f0; }
  .err { color: #f00; }
  form { display: flex; gap: 0.5em; }
  input { flex: 1; padding: 0.5em; font-family: monospace; font-size: 1em;
          background: #222; color: #eee; border: 1px solid #333; }
  button { padding: 0.5em 1.5em; font-family: monospace; font-size: 1em;
           background: #333; color: #eee; border: 1px solid #555; cursor: pointer; }
</style>
</head>
<body>
<h1>web-skeleton</h1>
<div id=\"log\"></div>
<form id=\"form\">
  <input id=\"msg\" type=\"text\" placeholder=\"type a message...\" autofocus>
  <button type=\"submit\">send</button>
</form>
<script>
  const log = document.getElementById('log');
  const form = document.getElementById('form');
  const msg = document.getElementById('msg');

  function appendLog(text, cls) {
    const line = document.createElement('div');
    line.className = cls;
    line.textContent = text;
    log.appendChild(line);
    log.scrollTop = log.scrollHeight;
  }

  appendLog('[status] connecting...', 'status');
  const ws = new WebSocket('ws://' + location.host + '/ws');

  ws.onopen = function() {
    appendLog('[status] connected', 'status');
  };
  ws.onmessage = function(e) {
    appendLog('[recv] ' + e.data, 'recv');
  };
  ws.onclose = function() {
    appendLog('[status] disconnected', 'status');
  };
  ws.onerror = function() {
    appendLog('[error] connection error', 'err');
  };

  form.onsubmit = function(e) {
    e.preventDefault();
    const text = msg.value;
    if (!text) return;
    ws.send(text);
    appendLog('[sent] ' + text, 'sent');
    msg.value = '';
  };
</script>
</body>
</html>")

;;; ---------------------------------------------------------------------------
;;; Per-worker connection table
;;;
;;; Each worker thread binds *connections* in its own dynamic scope.
;;; All connection functions use whatever binding is current —
;;; no locks, no shared state.
;;; ---------------------------------------------------------------------------

(defvar *connections* (make-hash-table :test #'eql)
  "Maps file descriptor → connection object. Bound per-worker.")

(defun register-connection (conn)
  (setf (gethash (connection-fd conn) *connections*) conn))

(defun unregister-connection (conn)
  (remhash (connection-fd conn) *connections*))

(defun lookup-connection (fd)
  (gethash fd *connections*))

;;; ---------------------------------------------------------------------------
;;; Shutdown flag
;;; ---------------------------------------------------------------------------

(defvar *shutdown* nil
  "Set to T to signal all workers to exit.")

;;; ---------------------------------------------------------------------------
;;; Request routing
;;; ---------------------------------------------------------------------------

(defun route-request (conn)
  "Route a parsed HTTP request. Returns (values response upgrade-p).
   UPGRADE-P is true if this is a WebSocket upgrade."
  (let* ((request (connection-request conn))
         (method  (http-request-method request))
         (path    (http-request-path request)))
    (cond
      ;; Serve test page
      ((and (eq method :GET) (string= path "/"))
       (log-debug "~a ~a -> 200" method path)
       (values (make-html-response 200 *test-page*) nil))
      ;; WebSocket upgrade
      ((and (eq method :GET) (string= path "/ws")
            (websocket-upgrade-p request))
       (log-info "~a ~a -> 101 switching protocols" method path)
       (values (make-websocket-handshake-response request) t))
      ;; Everything else
      (t
       (log-debug "~a ~a -> 404" method path)
       (values (make-error-response 404) nil)))))

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
;;; Handle readable event on a client fd
;;; ---------------------------------------------------------------------------

(defun handle-client-read (conn epoll-fd)
  "Handle EPOLLIN on a client connection."
  (handler-case
      (ecase (connection-state conn)
        ;; HTTP request accumulation
        ((:read-http :read-body)
         (let ((result (connection-on-read conn)))
           (case result
             (:dispatch
              ;; Full request — parse and route
              (let ((request (connection-parse-request conn)))
                (log-debug "~a ~a~@[?~a~] HTTP/~a"
                           (http-request-method request)
                           (http-request-path request)
                           (http-request-query request)
                           (http-request-version request))
                (multiple-value-bind (response upgrade-p)
                    (route-request conn)
                  ;; Queue the response for writing
                  (let ((bytes (format-response response)))
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
                  (websocket-on-read conn)
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
              ;; WebSocket handshake sent — switch to reading frames
              (setf (connection-read-pos conn) 0
                    (connection-state conn) :websocket)
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

(defparameter *max-events* 64
  "Maximum events to process per epoll_wait call.")

(defun run-event-loop (listener-socket epoll-fd)
  "Main event loop. Runs until *shutdown* is set."
  (let ((listener-fd (socket-fd listener-socket)))
    (loop
      (when *shutdown* (return))
      (let ((events (epoll-wait epoll-fd *max-events* 1000)))
        (dolist (event events)
          (block handle-event
            (let ((fd     (car event))
                  (flags  (cdr event)))
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
                       (handle-client-read conn epoll-fd))
                     ;; Writable (check conn still alive after read handling)
                     (when (and (logtest flags +epollout+)
                                (lookup-connection fd))
                       (handle-client-write conn epoll-fd)))))))))))))

;;; ---------------------------------------------------------------------------
;;; Worker
;;; ---------------------------------------------------------------------------

(defun run-worker (port worker-id)
  "Run a single worker: own listener, own epoll fd, own connections."
  (let ((*connections* (make-hash-table :test #'eql)))
    (let* ((listener (make-tcp-listener port))
           (epoll-fd (epoll-create)))
      (log-info "worker ~d started (epoll fd ~d)" worker-id epoll-fd)
      (epoll-add epoll-fd (socket-fd listener)
                 (logior +epollin+ +epollet+))
      (unwind-protect
          (run-event-loop listener epoll-fd)
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

(defun start-server (&key (port 8081) (workers (cpu-count)))
  "Start the server with WORKERS event loops on PORT.
   Each worker gets its own listener socket (SO_REUSEPORT), epoll fd,
   and connection table. Ctrl-C shuts down all workers."
  (setf *shutdown* nil)
  (log-info "starting ~d worker~:p on port ~d" workers port)
  (let ((threads (loop for i from 0 below workers
                       collect (sb-thread:make-thread
                                (let ((id i))
                                  (lambda () (run-worker port id)))
                                :name (format nil "web-skeleton-~d" i)))))
    (unwind-protect
        (handler-case
            ;; Main thread waits for interrupt
            (loop (sleep 1))
          (sb-sys:interactive-interrupt ()
            (format t "~%")
            (log-info "interrupted — shutting down")))
      ;; Signal workers to stop and wait for them
      (setf *shutdown* t)
      (dolist (thread threads)
        (ignore-errors (sb-thread:join-thread thread :timeout 5)))
      (log-info "stopped"))))

(defun main ()
  (start-server :port 8081))
