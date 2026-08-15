(defpackage :web-skeleton-test-harness
  (:use :cl :web-skeleton)
  (:export #:with-test-server
           #:*test-port*
           #:test-http-request
           #:make-test-request
           #:make-test-ws-frame
           ;; Bounded reading — a test that reads a socket needs a deadline
           #:read-until-bounded
           #:*test-read-timeout*))

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
   then close and return it.

   There is a race between that close and the caller's rebind, and it is
   not theoretical — it has cost two investigations. What this docstring
   used to get wrong was the symptom. It assumed a collision would show
   up as a bind failure; every listener sets SO_REUSEPORT, so the second
   bind *succeeds* and the kernel load-balances between two servers on
   one port. Measured, with two suites racing: 40 requests split 17/23,
   no error, no warning, nothing in either log.

   What that looks like from inside a test is a request answered by the
   other suite's handler, or accepted into the backlog of a server that
   is mid-teardown and never answers. Ten seconds later, a read deadline
   expires in a test that has nothing to do with whatever was being
   changed at the time.

   The durable fix is the one this docstring already named: teach
   START-SERVER to accept port 0 and report the bound port back, so the
   listener exists before the number can be handed to anyone else. Until
   then CALL-WITH-TEST-SERVER probes for a nonce, which does not close
   the window but does turn a ten-minute mystery into a named failure.

   Operationally: one suite at a time on a machine."
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

(defun check-owns-port (nonce)
  "Confirm the server answering *TEST-PORT* is the one this test started.

   Cannot close FIND-FREE-PORT's window — the durable fix for that is a
   START-SERVER that reports its own bound port. What it does is name the
   failure. With SO_REUSEPORT two servers can hold one port and the
   kernel splits traffic between them silently, so the symptom is a
   request answered by someone else's handler, or a connection accepted
   by a server that is shutting down and will never reply. Left alone
   that surfaces ten seconds later as a read deadline in an unrelated
   test, and costs an afternoon.

   Probing once per server start catches roughly half of any given
   collision, which across a suite of forty is plenty — and one clear
   line beats one silent hang.

   Raises only on positive evidence: a 200 whose body is somebody else's.
   Anything else is inconclusive and passes, because some tests lower
   *MAX-CONNECTIONS* far enough that this probe itself draws the
   framework's own 503, and a check that breaks the tests it is meant to
   protect is worse than the hang it replaces."
  (multiple-value-bind (status headers body)
      (handler-case (test-http-request :get "/__nonce")
        (error () (values :error nil nil)))
    (declare (ignore headers))
    (when (and (eql status 200) (not (equal body nonce)))
      (error "test server on port ~d is not ours: expected nonce ~s, got ~s. ~
              Another process is bound to the same port — every listener ~
              sets SO_REUSEPORT, so a collision shares the port instead of ~
              failing the bind, and the kernel splits traffic between the ~
              two. Run one suite at a time; see FIND-FREE-PORT."
             *test-port* nonce body))))

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
         (let* ((nonce (format nil "~36r~36r" (random (expt 36 8))
                               (get-internal-real-time)))
                (server-thread
                  (sb-thread:make-thread
                   (lambda ()
                     (start-server :host #(127 0 0 1)
                                   :port port
                                   :workers 1
                                   ;; Wrapped so every server answers one
                                   ;; reserved path with its own nonce,
                                   ;; whatever the test's handler does.
                                   ;; The app handler never sees the probe.
                                   :handler
                                   (lambda (req)
                                     (if (string= (http-request-path req)
                                                  "/__nonce")
                                         (make-text-response 200 nonce)
                                         (if handler
                                             (funcall handler req)
                                             (make-error-response 501))))
                                   :ws-handler ws-handler))
                   :name "web-skeleton-test-server")))
           (unwind-protect
                (progn
                  (wait-for-port port)
                  (let ((*test-port* port))
                    (check-owns-port nonce)
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
;;; Bounded reads
;;;
;;; READ-BYTE on a socket stream has no deadline. A server that answers
;;; late, answers partially, or never answers therefore does not fail a
;;; test — it stops the suite, and CI kills the job ten minutes later with
;;; a log ending at the name of the test that started and nothing said
;;; about what it was waiting for.
;;;
;;; Every read this harness performs goes through a deadline, and
;;; READ-UNTIL-BOUNDED is exported so that reads a downstream app performs
;;; can too.
;;; ---------------------------------------------------------------------------

(defvar *test-read-timeout* 10
  "Seconds any harness read waits before giving up and returning what
   arrived.

   A diagnostic backstop, not a latency assertion: a healthy response
   lands in milliseconds, so any value a working server cannot reach will
   do. Raise it around a deliberately slow handler with a LET — unlike the
   server's own specials, this one is read on the calling thread, so a
   binding is seen.")

(defun call-with-read-deadline (seconds thunk)
  "Run THUNK on its own thread and abandon it after SECONDS.
   Returns (values RESULT COMPLETED-P).

   A thread is the mechanism because there is no other one: READ-BYTE on
   an fd-stream cannot be interrupted from outside, so the only way to
   stop waiting is to stop looking at the thread that is waiting. The
   abandoned thread may still be blocked in a read when this returns —
   which is why anything it was filling has to be visible to the caller
   rather than returned by the thunk."
  (let* ((completed nil)
         (result nil)
         (thread (sb-thread:make-thread
                  (lambda ()
                    (setf result (funcall thunk)
                          completed t))
                  :name "bounded-reader")))
    (sb-thread:join-thread thread :timeout seconds :default nil)
    (unless completed
      (ignore-errors (sb-thread:terminate-thread thread)))
    (values result completed)))

(defun read-until-bounded (stream &key until into
                                       (seconds *test-read-timeout*))
  "Read bytes from STREAM until UNTIL is satisfied, the stream ends, or
   SECONDS elapse. Returns (values BUFFER REASON).

   BUFFER is a fill-pointered (UNSIGNED-BYTE 8) vector holding everything
   that arrived — always, including on the deadline and on a reset. That
   is the point: a test that times out should be able to assert against
   the bytes it did get, and say what was missing.

   REASON is one of:
     :SATISFIED — UNTIL returned true
     :EOF       — the peer closed cleanly
     :ERROR     — the read failed, in practice a reset
     :DEADLINE  — SECONDS elapsed with the stream still open

   :EOF and :ERROR are kept apart because an ordinary end of stream and a
   reset are different outcomes, and at least one test turns on which one
   it got (a refused connection is supposed to close, not reset).

   UNTIL is called as (UNTIL BUFFER FILL) after each byte, so it must be
   cheap — a scan of the whole buffer per byte is quadratic, which for
   test-sized payloads is fine and for a large stream is not. NIL means
   read until the stream ends, which is the right choice whenever the
   response is close-delimited and the wrong one whenever it is not:
   a keep-alive response never reaches EOF, so it needs a predicate or it
   will sit here until the deadline.

   :INTO supplies the buffer instead of allocating one, for a caller
   accumulating across several calls."
  (let ((buf (or into
                 (make-array 8192 :element-type '(unsigned-byte 8)
                                  :fill-pointer 0 :adjustable t))))
    (multiple-value-bind (reason completed)
        (call-with-read-deadline
         seconds
         (lambda ()
           (handler-case
               (loop
                 (when (and until (funcall until buf (fill-pointer buf)))
                   (return :satisfied))
                 (let ((byte (read-byte stream nil nil)))
                   (unless byte (return :eof))
                   (vector-push-extend byte buf)))
             (error () :error))))
      (values buf (if completed reason :deadline)))))

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
   (values STATUS HEADERS BODY-STRING). BODY-STRING is NIL when empty.

   Reads to the end of the stream, bounded by *TEST-READ-TIMEOUT*.
   End-of-stream is the right framing here because TEST-HTTP-REQUEST sends
   Connection: close unless the caller overrides it — a caller who does
   override it wants READ-UNTIL-BOUNDED with an :UNTIL predicate instead,
   because a kept-alive response never reaches EOF and this will spend the
   whole deadline discovering that."
  (multiple-value-bind (buf reason) (read-until-bounded stream)
    (let* ((end (fill-pointer buf))
           (header-end (web-skeleton::scan-crlf-crlf buf 0 end)))
      (unless header-end
        (error "test harness: ~a after ~d byte~:p~@[ — got: ~s~]"
               (ecase reason
                 (:deadline (format nil "no complete response within ~as"
                                    *test-read-timeout*))
                 (:eof      "peer closed before a complete response")
                 (:error    "read failed before a complete response")
                 (:satisfied "predicate satisfied before a complete response"))
               end
               (when (plusp end)
                 (sb-ext:octets-to-string
                  (subseq buf 0 (min end 200)) :external-format :latin-1))))
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
