(defpackage :web-skeleton-test-harness
  (:use :cl :web-skeleton)
  (:export #:with-test-server
           #:*test-port*
           #:test-http-request
           #:make-test-request
           #:make-test-ws-frame))

(in-package :web-skeleton-test-harness)

;;; ===========================================================================
;;; Test harness for web-skeleton
;;;
;;; Optional ASDF system providing two styles of handler testing:
;;;
;;;   1. End-to-end via WITH-TEST-SERVER — spins a live single-worker
;;;      server on an ephemeral port in a background thread, binds
;;;      *TEST-PORT* for the body, tears down via the shutdown-hook path
;;;      on scope exit. Use TEST-HTTP-REQUEST from inside the body to
;;;      make real HTTP round-trips.
;;;
;;;   2. Unit-style via MAKE-TEST-REQUEST / MAKE-TEST-WS-FRAME — build
;;;      request / frame structs directly and pass them to your handlers.
;;;      No network, no background thread. Fastest path when the test is
;;;      purely about handler logic.
;;;
;;; The harness is its own ASDF system (parallel to web-skeleton-tls) so
;;; downstream apps can depend on it in their test build without pulling
;;; in the framework's own test suite.
;;; ===========================================================================

(defvar *test-port* nil
  "Port the live test server is listening on inside WITH-TEST-SERVER.")

;;; ---------------------------------------------------------------------------
;;; Ephemeral port discovery + readiness
;;; ---------------------------------------------------------------------------

(defun find-free-port ()
  "Bind a temporary socket to port 0, let the kernel pick a free port,
   then close and return it. There is a tiny race between close and the
   caller's rebind — another process could theoretically grab the port
   in the gap — but in practice this is reliable for localhost tests.
   If it ever flakes in CI, the fix is to teach START-SERVER to accept
   port 0 and propagate the assigned port back to the caller."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
           (multiple-value-bind (host port)
               (sb-bsd-sockets:socket-name socket)
             (declare (ignore host))
             port))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun wait-for-port (port &key (timeout 5))
  "Poll PORT every 50ms until it accepts a TCP connection, or TIMEOUT
   seconds elapse. Signals an error on timeout."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (when (> (get-internal-real-time) deadline)
        (error "test server on 127.0.0.1:~d did not become ready within ~ds"
               port timeout))
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp)))
        (handler-case
            (progn
              (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
              (sb-bsd-sockets:socket-close socket)
              (return))
          (error ()
            (ignore-errors (sb-bsd-sockets:socket-close socket))
            (sleep 0.05)))))))

;;; ---------------------------------------------------------------------------
;;; Live test server
;;; ---------------------------------------------------------------------------

(defmacro with-test-server ((&key handler ws-handler) &body body)
  "Spin a single-worker server on an ephemeral port, bind *TEST-PORT* for
   BODY, tear down on scope exit (signal + bounded join + fallback
   terminate).
   Shutdown hooks are isolated: REGISTER-CLEANUP calls made from inside
   BODY (directly or via a handler) fire during this server's teardown
   and do not leak into the caller's framework state. The outer
   *SHUTDOWN-HOOKS* list is saved on entry and restored on exit."
  `(call-with-test-server ,handler ,ws-handler (lambda () ,@body)))

(defun call-with-test-server (handler ws-handler thunk)
  (let ((port (find-free-port))
        (saved-hooks web-skeleton::*shutdown-hooks*)
        (saved-drain web-skeleton:*drain-timeout*)
        (saved-poll  web-skeleton:*shutdown-poll-interval*))
    ;; Global SETF (not a LET binding) for the shutdown-related specials:
    ;; the workers that read them are spawned by START-SERVER into fresh
    ;; threads that inherit nothing from our dynamic environment, and
    ;; SB-THREAD:MAKE-THREAD has no :initial-bindings shortcut in modern
    ;; SBCL. A serial test runner makes the global mutation safe.
    ;; *SHUTDOWN-POLL-INTERVAL* shrinks the main-loop sleep and worker
    ;; epoll_wait timeout so teardown takes ~50ms instead of ~1s per test.
    (setf web-skeleton::*shutdown-hooks* nil
          web-skeleton::*shutdown* nil
          web-skeleton:*drain-timeout* 1
          web-skeleton:*shutdown-poll-interval* 0.05)
    (unwind-protect
         (let ((server-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (start-server :host #(127 0 0 1)
                                  :port port
                                  :workers 1
                                  :handler handler
                                  :ws-handler ws-handler))
                  :name "web-skeleton-test-server")))
           (unwind-protect
                (progn
                  (wait-for-port port)
                  (let ((*test-port* port))
                    (funcall thunk)))
             ;; Teardown: signal shutdown, join with a bounded timeout,
             ;; fall back to TERMINATE-THREAD if the graceful path
             ;; hangs (e.g. a misbehaving handler holding a connection).
             (setf web-skeleton::*shutdown* t)
             (handler-case
                 (sb-thread:join-thread server-thread :timeout 10)
               (error ()
                 (ignore-errors
                  (sb-thread:terminate-thread server-thread))
                 (ignore-errors
                  (sb-thread:join-thread server-thread))))))
      (setf web-skeleton::*shutdown-hooks* saved-hooks
            web-skeleton:*drain-timeout* saved-drain
            web-skeleton:*shutdown-poll-interval* saved-poll))))

;;; ---------------------------------------------------------------------------
;;; HTTP client for end-to-end tests
;;; ---------------------------------------------------------------------------

(defun test-http-request (method path &key headers body)
  "Make a real HTTP request to the live test server on *TEST-PORT*.
   Returns (values STATUS HEADERS BODY-STRING).
   METHOD is a keyword (:GET, :POST, :PUT, ...). PATH is the request path
   (with optional query string). HEADERS is an alist of (name . value);
   Host and Connection: close are filled in automatically unless the
   caller provides them. BODY is a string, byte vector, or NIL; the
   Content-Length header is appended when BODY is non-nil."
  (unless *test-port*
    (error "test-http-request: must be called inside WITH-TEST-SERVER"))
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
           (let* ((stream (sb-bsd-sockets:socket-make-stream
                           socket :input t :output t
                           :element-type '(unsigned-byte 8)))
                  (body-bytes (etypecase body
                                (null nil)
                                (string (sb-ext:string-to-octets
                                         body :external-format :utf-8))
                                ((simple-array (unsigned-byte 8) (*)) body)))
                  (all-headers (append
                                (unless (assoc "host" headers
                                               :test #'string-equal)
                                  (list (cons "host" "localhost")))
                                (unless (assoc "connection" headers
                                               :test #'string-equal)
                                  (list (cons "connection" "close")))
                                headers
                                (when body-bytes
                                  (list (cons "content-length"
                                              (write-to-string
                                               (length body-bytes)))))))
                  (request-bytes (web-skeleton::serialize-http-message
                                  (format nil "~a ~a HTTP/1.1"
                                          (symbol-name method) path)
                                  all-headers
                                  body-bytes)))
             (write-sequence request-bytes stream)
             (force-output stream)
             (parse-test-response stream)))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun parse-test-response (stream)
  "Drain a complete HTTP response from STREAM and return
   (values STATUS HEADERS BODY-STRING). BODY-STRING is NIL when empty."
  (let ((buf (make-array 8192 :element-type '(unsigned-byte 8)
                              :fill-pointer 0 :adjustable t)))
    (loop for byte = (handler-case (read-byte stream nil nil)
                       (error () nil))
          while byte
          do (vector-push-extend byte buf))
    (let* ((end (fill-pointer buf))
           (header-end (web-skeleton::scan-crlf-crlf buf 0 end)))
      (unless header-end
        (error "test harness: incomplete response from test server"))
      (let* ((status (web-skeleton::parse-response-status buf 0 end))
             (first-crlf (web-skeleton::scan-crlf buf 0 header-end))
             (headers (when first-crlf
                        (web-skeleton::parse-headers-bytes
                         buf (+ first-crlf 2) (+ header-end 4))))
             (body-start (+ header-end 4))
             (body-bytes (when (> end body-start)
                           (subseq buf body-start end)))
             (body-string (when body-bytes
                            (handler-case
                                (sb-ext:octets-to-string
                                 body-bytes :external-format :utf-8)
                              (error () nil)))))
        (values status headers body-string)))))

;;; ---------------------------------------------------------------------------
;;; Unit-style request and frame builders
;;; ---------------------------------------------------------------------------

(defun make-test-request (&key (method :GET) (path "/") query headers body)
  "Build an HTTP-REQUEST struct directly, bypassing the byte parser.
   For unit-testing handlers without the whole network stack.
   HEADERS is an alist of (name . value); names are lowercased to match
   what the parser would produce. BODY is a string, byte vector, or NIL."
  (web-skeleton::make-http-request
   :method method
   :path path
   :query query
   :version "1.1"
   :headers (mapcar (lambda (h)
                      (cons (string-downcase (car h)) (cdr h)))
                    headers)
   :body (etypecase body
           (null nil)
           (string (sb-ext:string-to-octets body :external-format :utf-8))
           ((simple-array (unsigned-byte 8) (*)) body))))

(defun make-test-ws-frame (text &key (opcode 1) (fin t))
  "Build a masked client WebSocket frame for testing ws-handler logic.
   TEXT is the payload string (UTF-8 encoded). Default is a complete
   text frame (opcode=1, FIN=1); pass :OPCODE / :FIN to build other
   shapes (continuation, control frames, non-final fragments).
   Returns a byte vector ready to feed to WEB-SKELETON::TRY-PARSE-WS-FRAME
   or to write into a connection's read buffer for a full round-trip test."
  (let* ((payload (sb-ext:string-to-octets text :external-format :utf-8))
         (len (length payload))
         (mask #(#xAA #xBB #xCC #xDD))
         (header-size (cond ((<= len 125)   6)
                            ((<= len 65535) 8)
                            (t              14)))
         (frame (make-array (+ header-size len)
                            :element-type '(unsigned-byte 8))))
    (setf (aref frame 0) (logior (if fin #x80 0) opcode))
    (cond
      ((<= len 125)
       (setf (aref frame 1) (logior #x80 len)))
      ((<= len 65535)
       (setf (aref frame 1) (logior #x80 126)
             (aref frame 2) (logand #xFF (ash len -8))
             (aref frame 3) (logand #xFF len)))
      (t
       (setf (aref frame 1) (logior #x80 127))
       (loop for i from 0 below 8
             do (setf (aref frame (+ 2 i))
                      (logand #xFF (ash len (* -8 (- 7 i))))))))
    (replace frame mask :start1 (- header-size 4))
    (loop for i from 0 below len
          do (setf (aref frame (+ header-size i))
                   (logxor (aref payload i) (aref mask (logand i 3)))))
    frame))
