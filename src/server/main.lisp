(in-package :web-skeleton)

;;; ===========================================================================
;;; TCP listener and server loop
;;;
;;; All socket I/O lives here. Parsing and response building (http.lisp)
;;; are pure logic on strings with no I/O of their own.
;;;
;;; TODO: replace blocking I/O with epoll + non-blocking sockets.
;;; The connection struct (connection.lisp) bundles per-connection state.
;;; When we add epoll, the accept loop registers connections with the
;;; epoll fd, and worker threads pick up ready connections — but the
;;; handle-connection / websocket-loop functions stay the same.
;;; ===========================================================================

(defun make-tcp-listener (port)
  "Create a TCP socket, bind to 0.0.0.0:PORT, listen with a backlog of 128.
   Returns the listening socket."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(0 0 0 0) port)
    (sb-bsd-sockets:socket-listen socket 128)
    socket))

;;; ---------------------------------------------------------------------------
;;; Reading HTTP requests from a connection
;;; ---------------------------------------------------------------------------

(defun read-until-headers-complete (stream buffer)
  "Read from STREAM into BUFFER until we find CRLFCRLF (end of headers).
   Returns the total number of bytes read, or NIL on clean disconnect."
  (let ((total 0)
        (terminator (sb-ext:string-to-octets +crlf-crlf+
                                              :external-format :ascii)))
    (loop
      (let ((byte (read-byte stream nil nil)))
        (unless byte
          (if (zerop total)
              (return-from read-until-headers-complete nil)
              (http-parse-error "connection closed before headers complete")))
        (when (>= total (length buffer))
          (http-parse-error "headers exceed buffer size (~d bytes)" (length buffer)))
        (setf (aref buffer total) byte)
        (incf total)
        (when (and (>= total 4)
                   (equalp (subseq buffer (- total 4) total) terminator))
          (return total))))))

(defun read-request (conn)
  "Read a complete HTTP request from CONN.
   Sets conn's request field and returns the request, or NIL on disconnect."
  (let* ((stream (connection-stream conn))
         (header-buf (make-array 65536 :element-type '(unsigned-byte 8)))
         (header-bytes (read-until-headers-complete stream header-buf)))
    (unless header-bytes
      (return-from read-request nil))
    (let* ((raw-headers (sb-ext:octets-to-string
                         header-buf :end header-bytes
                         :external-format :utf-8))
           (header-end (find-header-end raw-headers))
           (cl-start (search "content-length:" (string-downcase raw-headers)))
           (content-length
             (when cl-start
               (let* ((val-start (+ cl-start (length "content-length:")))
                      (val-end (or (position #\Return raw-headers :start val-start)
                                   (length raw-headers)))
                      (val (string-trim '(#\Space #\Tab)
                                        (subseq raw-headers val-start val-end))))
                 (parse-integer val :junk-allowed t)))))
      (when (and content-length (> content-length 0))
        (when (> content-length *max-body-size*)
          (http-parse-error "body too large (~d bytes, max ~d)"
                            content-length *max-body-size*))
        (let* ((body-buf (make-array content-length
                                     :element-type '(unsigned-byte 8)))
               (already-read (- header-bytes (+ header-end 4)))
               (remaining (- content-length already-read)))
          (when (> already-read 0)
            (replace body-buf header-buf
                     :start2 (+ header-end 4)
                     :end2 header-bytes))
          (when (> remaining 0)
            (let ((got (read-sequence body-buf stream
                                      :start already-read
                                      :end content-length)))
              (when (< got content-length)
                (http-parse-error "incomplete body (got ~d of ~d bytes)"
                                  (+ already-read got) content-length))))
          (let ((body-str (sb-ext:octets-to-string
                           body-buf :external-format :utf-8)))
            (setf raw-headers (concatenate 'string raw-headers body-str)))))
      (let ((request (parse-request raw-headers)))
        (setf (connection-request conn) request)
        request))))

(defun send-response (conn response)
  "Write an HTTP-RESPONSE to CONN's stream."
  (let ((bytes (format-response response)))
    (write-sequence bytes (connection-stream conn))
    (force-output (connection-stream conn))))

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
;;; Request routing
;;; ---------------------------------------------------------------------------

(defun handle-request (conn)
  "Route a parsed HTTP request on CONN. Sends the response.
   Returns :WEBSOCKET if the connection was upgraded, :DONE otherwise."
  (let* ((request (connection-request conn))
         (method  (http-request-method request))
         (path    (http-request-path request)))
    (cond
      ;; Serve test page
      ((and (eq method :GET) (string= path "/"))
       (let ((response (make-html-response 200 *test-page*)))
         (send-response conn response)
         (log-debug "~a ~a -> ~d" method path (http-response-status response))
         :done))
      ;; WebSocket upgrade
      ((and (eq method :GET) (string= path "/ws")
            (websocket-upgrade-p request))
       (let ((response (make-websocket-handshake-response request)))
         (send-response conn response)
         (log-info "~a ~a -> ~d switching protocols" method path 101)
         (setf (connection-state conn) :websocket)
         :websocket))
      ;; Everything else
      (t
       (let ((response (make-error-response 404)))
         (send-response conn response)
         (log-debug "~a ~a -> ~d" method path 404)
         :done)))))

;;; ---------------------------------------------------------------------------
;;; Per-connection handler
;;; ---------------------------------------------------------------------------

(defun handle-connection (conn)
  "Handle a single connection from accept to close.
   This is the function a worker thread would call in the epoll model."
  (handler-case
      (let ((request (read-request conn)))
        (if request
            (progn
              (log-debug "~a ~a~@[?~a~] HTTP/~a"
                         (http-request-method request)
                         (http-request-path request)
                         (http-request-query request)
                         (http-request-version request))
              (let ((result (handle-request conn)))
                (when (eq result :websocket)
                  (websocket-loop (connection-stream conn)))))
            (log-debug "client disconnected")))
    (http-parse-error (e)
      (log-warn "parse error: ~a" (http-parse-error-message e)))
    (error (e)
      (log-error "connection error: ~a" e))))

;;; ---------------------------------------------------------------------------
;;; Server loop
;;; ---------------------------------------------------------------------------

(defun start-server (&key (port 8081))
  "Start the TCP server on PORT. Accepts connections, parses requests,
   routes them, and sends responses. Ctrl-C shuts down cleanly."
  (let ((listener (make-tcp-listener port)))
    (log-info "listening on port ~d" port)
    (unwind-protect
        (handler-case
            (loop
              (let ((conn (make-client-connection
                           (sb-bsd-sockets:socket-accept listener))))
                (log-debug "connection accepted")
                (unwind-protect
                    (handle-connection conn)
                  (connection-close conn)
                  (log-debug "connection closed"))))
          (sb-sys:interactive-interrupt ()
            (format t "~%")
            (log-info "interrupted — shutting down")))
      (sb-bsd-sockets:socket-close listener)
      (log-info "stopped"))))

(defun main ()
  (start-server :port 8081))
