(in-package :web-skeleton-demo)

;;; ===========================================================================
;;; Demo application — test page + WebSocket echo
;;;
;;; Shows how to use the web-skeleton framework:
;;;   - Define an HTTP handler that routes requests
;;;   - Define a WebSocket handler that processes messages
;;;   - Pass both to start-server
;;; ===========================================================================

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
;;; HTTP handler
;;; ---------------------------------------------------------------------------

(defun handle-request (request)
  "Demo HTTP handler. Serves the test page and upgrades /ws."
  (let ((method (http-request-method request))
        (path   (http-request-path request)))
    (cond
      ((and (eq method :GET) (string= path "/"))
       (make-html-response 200 *test-page*))
      ((and (eq method :GET) (string= path "/ws"))
       :upgrade)
      (t
       (make-error-response 404)))))

;;; ---------------------------------------------------------------------------
;;; WebSocket handler
;;; ---------------------------------------------------------------------------

(defun handle-ws-message (conn frame)
  "Demo WebSocket handler. Echoes text messages back."
  (declare (ignore conn))
  (when (= (ws-frame-opcode frame) +ws-op-text+)
    (build-ws-text
     (sb-ext:octets-to-string (ws-frame-payload frame)
                               :external-format :utf-8))))

;;; ---------------------------------------------------------------------------
;;; Entry points
;;; ---------------------------------------------------------------------------

(defun start-demo (&key (port 8081))
  "Start the demo server."
  (start-server :port port
                :handler #'handle-request
                :ws-handler #'handle-ws-message))

(defun main ()
  (start-demo))
