(in-package :web-skeleton-demo)

;;; ===========================================================================
;;; Demo application — test page + WebSocket echo + async fetch demo
;;;
;;; Shows how to use the web-skeleton framework:
;;;   - Define an HTTP handler that routes requests
;;;   - Define a WebSocket handler that processes messages
;;;   - Return a DEFER-TO-FETCH continuation for async outbound calls
;;;   - Pass handler and ws-handler to start-server
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Self-address for the /demo-fetch example
;;; ---------------------------------------------------------------------------

(defvar *demo-host* "127.0.0.1"
  "Host the /demo-fetch endpoint uses to self-fetch over HTTP.")

(defvar *demo-port* 8081
  "Port the demo server is listening on. START-DEMO sets this so the
   /demo-fetch endpoint can build a self-referential URL.")

;;; ---------------------------------------------------------------------------
;;; HTTP handler
;;; ---------------------------------------------------------------------------

(defun handle-request (request)
  "Demo HTTP handler. Upgrades /ws, demonstrates async fetch at
   /demo-fetch, serves static files for everything else."
  (let ((method (http-request-method request))
        (path   (http-request-path request)))
    (cond
      ((and (eq method :GET) (string= path "/ws"))
       ;; Production apps must validate the Origin header before upgrading.
       ;; See DEPLOYMENT.md "WebSocket origin validation".
       :upgrade)
      ((and (eq method :GET) (string= path "/demo-fetch"))
       (handle-demo-fetch request))
      (t
       (or (serve-static request)
           (make-error-response 404))))))

(defun handle-demo-fetch (request)
  "Runnable reference for the async http-fetch pattern. Handlers that
   need to reach an upstream API return a DEFER-TO-FETCH continuation
   instead of blocking. The framework parks the inbound connection,
   runs the outbound call on the same epoll loop, then calls the
   :THEN callback with (status headers body-bytes) on success, or
   with (NIL NIL NIL) as a cleanup sentinel if the fetch aborts
   before delivering a response. Whatever the happy-path callback
   returns becomes the final response to the original caller;
   cleanup-path return values are discarded.

   This endpoint demonstrates the pattern by self-fetching the demo's
   own /robots.txt over HTTP. A real app would target an upstream API."
  (declare (ignore request))
  (defer-to-fetch
   :get (format nil "http://~a:~d/robots.txt" *demo-host* *demo-port*)
   :then (lambda (status headers body-bytes)
           (declare (ignore headers))
           (if status
               ;; Happy path — upstream responded, shape a demo body.
               (make-text-response
                200
                (format nil "fetched /robots.txt~%  status: ~d~%  body: ~a"
                        status
                        (if body-bytes
                            (sb-ext:octets-to-string
                             body-bytes :external-format :utf-8)
                            "(empty)")))
               ;; Cleanup path — fetch never completed (inbound
               ;; vanished, drain, worker crash). Nothing to release
               ;; in this demo; return NIL and let the framework
               ;; discard the value.
               nil))))

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
  (setf *demo-port* port)
  (load-static-files "demo/static/"
                     :substitutions
                     '(("robots.txt" ("are smart" . "robots are cool and smart"))))
  (start-server :port port
                :handler #'handle-request
                :ws-handler #'handle-ws-message))

(defun main ()
  (start-demo))
