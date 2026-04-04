(in-package :web-skeleton)

;;; ===========================================================================
;;; Connection object
;;;
;;; Bundles all per-connection state into a single struct. When we move
;;; to epoll + thread pool, each connection object gets passed to a
;;; worker — no global state, no shared mutation between connections.
;;; ===========================================================================

(defstruct connection
  (socket   nil)                              ; the raw sb-bsd-sockets socket
  (stream   nil)                              ; the I/O stream wrapping the socket
  (state    :http :type keyword)              ; :http or :websocket
  (request  nil :type (or null http-request)) ; parsed request (during HTTP phase)
  (created  0   :type integer))               ; internal-real-time at creation

(defun make-client-connection (client-socket)
  "Wrap a newly accepted socket into a connection object."
  (let ((stream (sb-bsd-sockets:socket-make-stream
                 client-socket :input t :output t
                 :element-type '(unsigned-byte 8))))
    (make-connection :socket client-socket
                     :stream stream
                     :created (get-internal-real-time))))

(defun connection-close (conn)
  "Close a connection's socket. Safe to call multiple times."
  (when (connection-socket conn)
    (ignore-errors (sb-bsd-sockets:socket-close (connection-socket conn)))
    (setf (connection-socket conn) nil
          (connection-stream conn) nil)))
