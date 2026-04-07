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
;;; HTTP handler
;;; ---------------------------------------------------------------------------

(defun handle-request (request)
  "Demo HTTP handler. Upgrades /ws, serves static files for everything else."
  (let ((method (http-request-method request))
        (path   (http-request-path request)))
    (cond
      ((and (eq method :GET) (string= path "/ws"))
       :upgrade)
      (t
       (or (serve-static request)
           (make-error-response 404))))))

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
  (load-static-files "demo/static/")
  (start-server :port port
                :handler #'handle-request
                :ws-handler #'handle-ws-message))

(defun main ()
  (start-demo))
