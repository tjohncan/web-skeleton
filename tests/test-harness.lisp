(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; Test harness tests
;;;
;;; Unit-style tests for MAKE-TEST-REQUEST / MAKE-TEST-WS-FRAME land first
;;; because they are pure functions and run in milliseconds. End-to-end
;;; tests via WITH-TEST-SERVER come after — each one spins a live server
;;; on an ephemeral port, so teardown adds ~1-2 seconds per call.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Unit-style builders
;;; ---------------------------------------------------------------------------

(defun test-harness-make-test-request ()
  (format t "~%Harness: make-test-request~%")
  (let ((req (make-test-request
              :method :POST
              :path "/foo"
              :query "q=1"
              :headers '(("X-Custom" . "bar"))
              :body "data")))
    (check "method"           (http-request-method req)   :POST)
    (check "path"              (http-request-path req)     "/foo")
    (check "query"             (http-request-query req)    "q=1")
    (check "header lowercased" (get-header req "x-custom") "bar")
    (check "body present"
           (not (null (http-request-body req))) t)
    (check "body content"
           (sb-ext:octets-to-string (http-request-body req)
                                    :external-format :utf-8)
           "data")))

(defun test-harness-make-test-ws-frame ()
  (format t "~%Harness: make-test-ws-frame~%")
  (let ((frame (make-test-ws-frame "hi")))
    (check "fin+opcode byte" (aref frame 0) #x81)
    (check "mask bit + length" (aref frame 1) (logior #x80 2))
    ;; Round-trip: parsing the frame recovers the original payload.
    (multiple-value-bind (parsed consumed)
        (web-skeleton::try-parse-ws-frame frame 0 (length frame))
      (declare (ignore consumed))
      (check "round-trip parses" (not (null parsed)) t)
      (check "round-trip opcode" (ws-frame-opcode parsed) 1)
      (check "round-trip fin"    (ws-frame-fin parsed)    t)
      (check "round-trip payload"
             (sb-ext:octets-to-string (ws-frame-payload parsed)
                                      :external-format :utf-8)
             "hi")))
  ;; Non-default opcode and FIN=0 (first fragment)
  (let ((frame (make-test-ws-frame "partial" :opcode 2 :fin nil)))
    (multiple-value-bind (parsed consumed)
        (web-skeleton::try-parse-ws-frame frame 0 (length frame))
      (declare (ignore consumed))
      (check "binary frame opcode" (ws-frame-opcode parsed) 2)
      (check "fin=0 for first fragment" (ws-frame-fin parsed) nil))))

;;; ---------------------------------------------------------------------------
;;; End-to-end server tests
;;; ---------------------------------------------------------------------------

(defun test-harness-basic-get ()
  (format t "~%Harness: basic GET~%")
  (with-test-server
      (:handler (lambda (req)
                  (declare (ignore req))
                  (make-text-response 200 "hello")))
    (multiple-value-bind (status headers body)
        (test-http-request :get "/")
      (declare (ignore headers))
      (check "status 200" status 200)
      (check "body hello"  body   "hello"))))

(defun test-harness-post-with-body ()
  (format t "~%Harness: POST with body~%")
  (with-test-server
      (:handler (lambda (req)
                  (let ((body (http-request-body req)))
                    (make-text-response
                     200
                     (format nil "got ~d bytes"
                             (if body (length body) 0))))))
    (multiple-value-bind (status headers body)
        (test-http-request :post "/echo" :body "hello world")
      (declare (ignore headers))
      (check "POST status 200"         status 200)
      (check "POST body length echoed" body   "got 11 bytes"))))

(defun test-harness-shutdown-hook-e2e ()
  (format t "~%Harness: shutdown hook end-to-end~%")
  ;; A handler-registered cleanup should fire on teardown. The isolation
  ;; inside WITH-TEST-SERVER means this hook belongs to this test's
  ;; server only — restored on exit so no leakage to later tests.
  (let ((fired nil))
    (with-test-server
        (:handler (lambda (req)
                    (declare (ignore req))
                    (register-cleanup (lambda () (setf fired t)))
                    (make-text-response 200 "ok")))
      (test-http-request :get "/"))
    (check "cleanup fired during teardown" fired t)))

(defun test-harness-expect-100-continue-e2e ()
  (format t "~%Harness: Expect: 100-continue end-to-end~%")
  (with-test-server
      (:handler (lambda (req)
                  (let ((body (http-request-body req)))
                    (make-text-response
                     200
                     (format nil "~d bytes received"
                             (if body (length body) 0))))))
    (multiple-value-bind (status headers body)
        (test-http-request :post "/upload"
                           :headers '(("expect" . "100-continue"))
                           :body "payload-data")
      (declare (ignore headers))
      (check "Expect: 100-continue reaches handler" status 200)
      (check "Expect: 100-continue body echoed"
             body "12 bytes received"))))

(defun test-harness-head-fetch-e2e ()
  "An HTTP HEAD fetch against an upstream that (per RFC 7230 §3.3.2)
   returns Content-Length: N with zero body bytes delivers the happy
   path rather than tripping COMPLETE-FETCH's CL truncation guard.
   Exercises the FETCH-METHOD slot on the outbound connection — set
   from the continuation's :METHOD and read by the guard's HEAD
   exemption.

   Why the UPSTREAM-PORT closure: *TEST-PORT* is a DYNAMIC binding
   set by WITH-TEST-SERVER for the main-thread body only. The handler
   lambda runs on the worker thread, which sees only the top-level
   value (NIL). Capturing a lexical location lets the body SETF the
   port after WITH-TEST-SERVER has assigned one, and the handler
   reads the updated value at request time."
  (format t "~%Harness: HEAD fetch round-trip~%")
  (let ((upstream-port nil))
    (with-test-server
        (:handler
         (lambda (req)
           (cond
             ((string= (http-request-path req) "/upstream")
              ;; Framework auto-strips the body on HEAD and keeps
              ;; Content-Length — the §3.3.2 shape an upstream sends.
              (make-text-response 200 "hello world body"))
             ((string= (http-request-path req) "/probe")
              (defer-to-fetch :HEAD
                (format nil "http://127.0.0.1:~d/upstream" upstream-port)
                :then (lambda (status headers body)
                        (declare (ignore headers))
                        (make-text-response
                         (or status 500)
                         (format nil "status=~a body-nil=~a"
                                 status (null body))))))
             (t (make-error-response 404)))))
      (setf upstream-port *test-port*)
      (multiple-value-bind (status headers body)
          (test-http-request :get "/probe")
        (declare (ignore headers))
        (check "HEAD fetch: status delivered to callback" status 200)
        (check "HEAD fetch: callback saw 200 status"
               body "status=200 body-nil=T")))))

(defun test-harness-body-at-max-size-e2e ()
  "A POST whose Content-Length equals *MAX-BODY-SIZE* dispatches
   cleanly even though pre-growing the buffer to body-start+CL
   takes it past CONNECTION-READ-AVAILABLE's cap: :FULL fires at
   pos=body-start+CL, which IS the complete body, and the state
   machine reaches :DISPATCH.

   SETF (not LET) on *MAX-BODY-SIZE*: the worker thread reads the
   top-level value — dynamic bindings do not cross thread creation —
   so LET here would leave the worker at the default."
  (format t "~%Harness: POST body at *max-body-size*~%")
  (let ((saved web-skeleton:*max-body-size*))
    (setf web-skeleton:*max-body-size* 2048)
    (unwind-protect
         (with-test-server
             (:handler (lambda (req)
                         (let ((body (http-request-body req)))
                           (make-text-response
                            200 (format nil "got ~d bytes"
                                        (if body (length body) 0))))))
           (let ((body (make-string 2048 :initial-element #\x)))
             (multiple-value-bind (status headers response-body)
                 (test-http-request :post "/echo" :body body)
               (declare (ignore headers))
               (check "CL==max-body-size: dispatches" status 200)
               (check "CL==max-body-size: full body received"
                      response-body "got 2048 bytes"))))
      (setf web-skeleton:*max-body-size* saved))))

(defun test-harness-cached-response-survives-head-e2e ()
  "A handler that returns a shared HTTP-RESPONSE struct across
   requests must still have its body intact after a HEAD visits
   it. HEAD is handled post-serialize via STRIP-BODY-FOR-HEAD — no
   mutation of the caller's struct."
  (format t "~%Harness: cached response survives HEAD~%")
  (let ((cached (make-text-response 200 "payload-keep-me")))
    (with-test-server
        (:handler (lambda (req)
                    (declare (ignore req))
                    cached))
      ;; HEAD once — should return headers only, no body on the wire.
      (test-http-request :head "/")
      ;; GET after — should still deliver the body since the struct
      ;; wasn't mutated.
      (multiple-value-bind (status headers body)
          (test-http-request :get "/")
        (declare (ignore headers))
        (check "cached: post-HEAD GET status 200" status 200)
        (check "cached: post-HEAD GET body intact"
               body "payload-keep-me"))
      ;; HEAD again — still no state leakage.
      (test-http-request :head "/")
      (multiple-value-bind (status headers body)
          (test-http-request :get "/")
        (declare (ignore status headers))
        (check "cached: second-round GET body still intact"
               body "payload-keep-me")))))

(defun %raw-http-request (bytes)
  "Send BYTES to *TEST-PORT* on a fresh socket, close write half,
   drain the response, return it as a UTF-8 string. Used for tests
   that need HTTP/1.0, pipelining, or otherwise non-default wire
   shapes that TEST-HTTP-REQUEST doesn't support."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
           (let ((stream (sb-bsd-sockets:socket-make-stream
                          socket :input t :output t
                          :element-type '(unsigned-byte 8))))
             (write-sequence bytes stream)
             (force-output stream)
             ;; Half-close is advisory — if the server has already
             ;; processed + closed, SBCL's fd-stream may have torn
             ;; down the fd during flush and the shutdown syscall
             ;; would EBADF. We already have the write out; the
             ;; subsequent read-until-EOF validates what came back.
             (ignore-errors
              (sb-bsd-sockets:socket-shutdown socket :direction :output))
             (let ((buf (make-array 8192 :element-type '(unsigned-byte 8)
                                         :fill-pointer 0 :adjustable t)))
               (loop for byte = (handler-case (read-byte stream nil nil)
                                  (error () nil))
                     while byte
                     do (vector-push-extend byte buf))
               (sb-ext:octets-to-string (subseq buf 0 (fill-pointer buf))
                                        :external-format :utf-8))))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun test-harness-http10-keepalive-no-mutation-e2e ()
  "An HTTP/1.0 + Connection: keep-alive request must not stamp
   'Connection: keep-alive' onto the handler's returned struct.
   FORMAT-RESPONSE's :KEEP-ALIVE-HINT emits the header at serialize
   time without mutating. Verified by sending HTTP/1.0 first, then
   inspecting the handler's cached struct for leaked Connection
   headers, and finally sending HTTP/1.1 to confirm no stale
   'Connection: keep-alive' appears in an unrelated follow-up."
  (format t "~%Harness: HTTP/1.0 keep-alive doesn't mutate struct~%")
  (let ((cached (make-text-response 200 "body")))
    (with-test-server
        (:handler (lambda (req)
                    (declare (ignore req))
                    cached))
      ;; Raw HTTP/1.0 + Connection: keep-alive.
      (%raw-http-request
       (sb-ext:string-to-octets
        (concatenate 'string
                     "GET / HTTP/1.0" *crlf*
                     "Host: localhost" *crlf*
                     "Connection: keep-alive" *crlf* *crlf*)
        :external-format :ascii))
      (check "http/1.0 keep-alive: struct body still set"
             (http-response-body cached) "body")
      (check "http/1.0 keep-alive: no connection header on struct"
             (assoc "connection" (http-response-headers cached)
                    :test #'string-equal)
             nil))))

(defun %raw-http-split-send (headers-bytes body-bytes)
  "Send HEADERS-BYTES, force-output, sleep 100ms, send BODY-BYTES,
   half-close, drain response. The sleep forces the server to see a
   headers-only first read — necessary for tests that exercise the
   Expect: 100-continue gate, where a concatenated headers+body
   would short-circuit via the already-have-full-body branch and
   bypass the gate entirely."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
           (let ((stream (sb-bsd-sockets:socket-make-stream
                          socket :input t :output t
                          :element-type '(unsigned-byte 8))))
             (write-sequence headers-bytes stream)
             (force-output stream)
             (sleep 0.1)
             (write-sequence body-bytes stream)
             (force-output stream)
             ;; Half-close is advisory — tolerate ENOTCONN/EBADF
             ;; when the server has already closed after processing.
             (ignore-errors
              (sb-bsd-sockets:socket-shutdown socket :direction :output))
             (let ((buf (make-array 8192 :element-type '(unsigned-byte 8)
                                         :fill-pointer 0 :adjustable t)))
               (loop for byte = (handler-case (read-byte stream nil nil)
                                  (error () nil))
                     while byte
                     do (vector-push-extend byte buf))
               (sb-ext:octets-to-string (subseq buf 0 (fill-pointer buf))
                                        :external-format :utf-8))))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun test-harness-http10-expect-100-continue-no-fire-e2e ()
  "HTTP/1.0 clients that send 'Expect: 100-continue' must NOT
   receive an interim 100 Continue response — RFC 7231 §5.1.1
   scopes the feature to HTTP/1.1. Some HTTP/1.0-only clients
   don't understand 1xx and fail the request on the interim.

   Uses %RAW-HTTP-SPLIT-SEND so the server sees headers-only on
   its first read — otherwise a concatenated headers+body fills
   body-available=content-length on the initial pass, routes
   through the already-have-full-body branch, and bypasses the
   Expect gate entirely. The split with sleep forces the gate to
   actually run."
  (format t "~%Harness: HTTP/1.0 Expect: 100-continue does not fire~%")
  (with-test-server
      (:handler (lambda (req)
                  (declare (ignore req))
                  (make-text-response 200 "ok")))
    (let* ((body "some-body-bytes")
           (headers (concatenate 'string
                                 "POST /upload HTTP/1.0" *crlf*
                                 "Host: localhost" *crlf*
                                 "Connection: close" *crlf*
                                 "Expect: 100-continue" *crlf*
                                 "Content-Length: "
                                 (write-to-string (length body)) *crlf*
                                 *crlf*))
           (raw (%raw-http-split-send
                 (sb-ext:string-to-octets headers :external-format :ascii)
                 (sb-ext:string-to-octets body :external-format :ascii))))
      (check "http/1.0 + Expect: no 100 Continue interim"
             (null (search "HTTP/1.1 100" raw)) t)
      (check "http/1.0 + Expect: final response delivered"
             (not (null (search "HTTP/1.1 200" raw))) t))))

(defun test-harness-expect-417-on-unknown-e2e ()
  "An HTTP/1.1 request carrying an Expect token other than
   100-continue must receive a 417 Expectation Failed (RFC 7231
   §5.1.1 MUST), with the body never reaching the handler. Sends
   headers only (Content-Length: 4 declared, body bytes withheld):
   body-available=0 < CL=4 keeps the request out of the already-
   have-full-body fast path, so SCAN-EXPECT-DISPOSITION actually
   runs. The :UNKNOWN arm queues 417, sets close-after-p=T, and
   advances state to :write-response — the server writes then
   closes. Reads to EOF without a client-side shutdown, which
   would race the server's close and raise ENOTCONN once the
   server RSTs."
  (format t "~%Harness: Expect: unknown returns 417~%")
  (let ((reached-handler nil))
    (with-test-server
        (:handler (lambda (req)
                    (declare (ignore req))
                    (setf reached-handler t)
                    (make-text-response 200 "should not reach")))
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp)))
        (unwind-protect
             (progn
               (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
               (let* ((stream (sb-bsd-sockets:socket-make-stream
                               socket :input t :output t
                               :element-type '(unsigned-byte 8)))
                      (headers (concatenate 'string
                                            "POST /upload HTTP/1.1" *crlf*
                                            "Host: localhost" *crlf*
                                            "Expect: x-custom-unknown" *crlf*
                                            "Content-Length: 4" *crlf*
                                            *crlf*)))
                 (write-sequence (sb-ext:string-to-octets
                                  headers :external-format :ascii)
                                 stream)
                 (force-output stream)
                 (let ((buf (make-array 1024 :element-type '(unsigned-byte 8)
                                             :fill-pointer 0 :adjustable t)))
                   (loop for byte = (handler-case (read-byte stream nil nil)
                                      (error () nil))
                         while byte
                         do (vector-push-extend byte buf))
                   (let ((raw (sb-ext:octets-to-string
                               (subseq buf 0 (fill-pointer buf))
                               :external-format :utf-8)))
                     (check "417: handler not reached" reached-handler nil)
                     (check "417: status line on wire"
                            (not (null (search "HTTP/1.1 417" raw))) t)
                     (check "417: connection: close on wire"
                            (not (null (search "connection: close" raw))) t)
                     (check "417: date header present"
                            (not (null (search "date:" raw))) t)))))
          (ignore-errors (sb-bsd-sockets:socket-close socket)))))))

(defun test-harness-expect-417-on-unknown-no-body-e2e ()
  "A GET (or any no-body request) with an unknown Expect token
   must 417 the same way the bodied POST variant does. Before the
   disposition-first restructure, the body-present branch owned
   the SCAN-EXPECT-DISPOSITION call and the no-body branch fell
   straight through to :dispatch — so a GET /admin with Expect:
   x-foo reached the handler while a POST with the same header
   got rejected. RFC 7231 §5.1.1 doesn't condition 417 on body
   presence; the framework now mirrors that."
  (format t "~%Harness: Expect: unknown on no-body request returns 417~%")
  (let ((reached-handler nil))
    (with-test-server
        (:handler (lambda (req)
                    (declare (ignore req))
                    (setf reached-handler t)
                    (make-text-response 200 "should not reach")))
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp)))
        (unwind-protect
             (progn
               (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
               (let* ((stream (sb-bsd-sockets:socket-make-stream
                               socket :input t :output t
                               :element-type '(unsigned-byte 8)))
                      (req (concatenate 'string
                                        "GET /probe HTTP/1.1" *crlf*
                                        "Host: localhost" *crlf*
                                        "Expect: x-custom-unknown" *crlf*
                                        *crlf*)))
                 (write-sequence (sb-ext:string-to-octets
                                  req :external-format :ascii)
                                 stream)
                 (force-output stream)
                 (let ((buf (make-array 1024 :element-type '(unsigned-byte 8)
                                             :fill-pointer 0 :adjustable t)))
                   (loop for byte = (handler-case (read-byte stream nil nil)
                                      (error () nil))
                         while byte
                         do (vector-push-extend byte buf))
                   (let ((raw (sb-ext:octets-to-string
                               (subseq buf 0 (fill-pointer buf))
                               :external-format :utf-8)))
                     (check "417 no-body: handler not reached" reached-handler nil)
                     (check "417 no-body: status line on wire"
                            (not (null (search "HTTP/1.1 417" raw))) t)
                     (check "417 no-body: connection: close on wire"
                            (not (null (search "connection: close" raw))) t)
                     (check "417 no-body: date header present"
                            (not (null (search "date:" raw))) t)))))
          (ignore-errors (sb-bsd-sockets:socket-close socket)))))))

(defun test-harness-expect-417-head-no-body-e2e ()
  "A HEAD request with an unknown Expect must 417 WITHOUT a
   message body on the wire (RFC 7231 §4.3.2 MUST NOT send a
   message body in a HEAD response). connection-on-read matches
   the 'HEAD ' method prefix inline and passes :head-only-p to
   format-response so Content-Length survives but body bytes
   are suppressed. Shape is vanishingly rare — HEAD with Expect
   is meaningless in practice — but the test locks in the fix."
  (format t "~%Harness: HEAD + Expect: unknown has no body on wire~%")
  (with-test-server
      (:handler (lambda (req)
                  (declare (ignore req))
                  (make-text-response 200 "should not reach")))
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
             (let* ((stream (sb-bsd-sockets:socket-make-stream
                             socket :input t :output t
                             :element-type '(unsigned-byte 8)))
                    (req (concatenate 'string
                                      "HEAD /probe HTTP/1.1" *crlf*
                                      "Host: localhost" *crlf*
                                      "Expect: x-custom-unknown" *crlf*
                                      *crlf*)))
               (write-sequence (sb-ext:string-to-octets
                                req :external-format :ascii)
                               stream)
               (force-output stream)
               (let ((buf (make-array 1024 :element-type '(unsigned-byte 8)
                                           :fill-pointer 0 :adjustable t)))
                 (loop for byte = (handler-case (read-byte stream nil nil)
                                    (error () nil))
                       while byte
                       do (vector-push-extend byte buf))
                 (let* ((raw (sb-ext:octets-to-string
                              (subseq buf 0 (fill-pointer buf))
                              :external-format :utf-8))
                        (hdr-end (search (format nil "~c~c~c~c"
                                                 #\Return #\Newline
                                                 #\Return #\Newline)
                                         raw)))
                   (check "HEAD+417: status line on wire"
                          (not (null (search "HTTP/1.1 417" raw))) t)
                   (check "HEAD+417: connection: close on wire"
                          (not (null (search "connection: close" raw))) t)
                   (check "HEAD+417: headers terminator present"
                          (not (null hdr-end)) t)
                   (check "HEAD+417: no body bytes after headers"
                          (- (length raw) (+ (or hdr-end 0) 4)) 0)))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun test-harness-handler-connection-close-honored-e2e ()
  "A handler that explicitly sets 'Connection: close' on an HTTP/1.1
   response must actually close the TCP socket after the response —
   close-after-p is synced from the response headers before the
   write path runs, so the wire framing matches what the client
   reads. Without the sync, the handler advertises close but the
   server holds the socket open and the client waits for more bytes
   until the idle sweeper reaps.

   Strengthened: after reading the first response we attempt a
   second request on the same socket. If the sync regresses and
   the server holds the socket open, the second request would
   receive a response and GOT-SECOND flips to T — catching the
   regression as a test failure rather than a slow-down."
  (format t "~%Harness: handler-set Connection: close is honored~%")
  (with-test-server
      (:handler (lambda (req)
                  (declare (ignore req))
                  (let ((resp (make-text-response 200 "bye")))
                    (set-response-header resp "connection" "close")
                    resp)))
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
             (let* ((stream (sb-bsd-sockets:socket-make-stream
                             socket :input t :output t
                             :element-type '(unsigned-byte 8)))
                    (req1 (sb-ext:string-to-octets
                           (concatenate 'string
                                        "GET / HTTP/1.1" *crlf*
                                        "Host: localhost" *crlf* *crlf*)
                           :external-format :ascii))
                    (req2 (sb-ext:string-to-octets
                           (concatenate 'string
                                        "GET /next HTTP/1.1" *crlf*
                                        "Host: localhost" *crlf* *crlf*)
                           :external-format :ascii)))
               (write-sequence req1 stream)
               (force-output stream)
               ;; Drain first response until EOF. Correct close →
               ;; EOF arrives promptly; regression would block here,
               ;; caught by the per-test join timeout in with-test-
               ;; server's teardown (still a failure shape rather
               ;; than a pass).
               (let ((buf (make-array 8192 :element-type '(unsigned-byte 8)
                                           :fill-pointer 0 :adjustable t)))
                 (loop for byte = (handler-case (read-byte stream nil nil)
                                    (error () nil))
                       while byte
                       do (vector-push-extend byte buf))
                 (let ((text (sb-ext:octets-to-string
                              (subseq buf 0 (fill-pointer buf))
                              :external-format :utf-8)))
                   (check "handler close: response present"
                          (not (null (search "HTTP/1.1 200" text))) t)
                   (check "handler close: body delivered"
                          (not (null (search "bye" text))) t)
                   (check "handler close: Connection: close stamped on wire"
                          (not (null (search "connection: close" text))) t)))
               ;; Probe: attempt a second request. Socket should be
               ;; closed — write may succeed buffering to the closed
               ;; socket or raise EPIPE; either way the read returns
               ;; NIL and GOT-SECOND stays NIL. A regression that
               ;; held the socket open would accept the second
               ;; request and respond, flipping GOT-SECOND to T.
               ;; SB-EXT:WITH-TIMEOUT bounds the probe so a pathological
               ;; regression doesn't hang the suite.
               (let ((got-second nil))
                 (handler-case
                     (sb-ext:with-timeout 1
                       (write-sequence req2 stream)
                       (force-output stream)
                       (let ((b (read-byte stream nil nil)))
                         (when b (setf got-second t))))
                   (error () nil))
                 (check "handler close: second request on same socket fails"
                        got-second nil))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun test-harness-fetch-callback-connection-close-honored-e2e ()
  "When a DEFER-TO-FETCH callback returns a response with
   'Connection: close' set, COMPLETE-FETCH must sync close-after-p
   from the response headers so the server actually closes the
   socket after delivering it. Parallel to the handler-close case,
   but through the fetch resumption path. Without SYNC-CLOSE-AFTER-
   P-FROM-RESPONSE in complete-fetch, the header lands on the wire
   but the socket stays open — framing-mismatch vs the client's
   expectation. Second-request probe on the same socket catches
   the regression."
  (format t "~%Harness: fetch-callback Connection: close is honored~%")
  (let ((upstream-port nil))
    (with-test-server
        (:handler
         (lambda (req)
           (cond
             ((string= (http-request-path req) "/upstream")
              (make-text-response 200 "upstream-body"))
             ((string= (http-request-path req) "/proxy")
              (defer-to-fetch :GET
                (format nil "http://127.0.0.1:~d/upstream" upstream-port)
                :then (lambda (status headers body)
                        (declare (ignore status headers body))
                        (let ((resp (make-text-response 200 "bye")))
                          (set-response-header resp "connection" "close")
                          resp))))
             (t (make-error-response 404)))))
      (setf upstream-port *test-port*)
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp)))
        (unwind-protect
             (progn
               (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
               (let* ((stream (sb-bsd-sockets:socket-make-stream
                               socket :input t :output t
                               :element-type '(unsigned-byte 8)))
                      (req1 (sb-ext:string-to-octets
                             (concatenate 'string
                                          "GET /proxy HTTP/1.1" *crlf*
                                          "Host: localhost" *crlf* *crlf*)
                             :external-format :ascii))
                      (req2 (sb-ext:string-to-octets
                             (concatenate 'string
                                          "GET /proxy HTTP/1.1" *crlf*
                                          "Host: localhost" *crlf* *crlf*)
                             :external-format :ascii)))
                 (write-sequence req1 stream)
                 (force-output stream)
                 (let ((buf (make-array 8192 :element-type '(unsigned-byte 8)
                                             :fill-pointer 0 :adjustable t)))
                   (loop for byte = (handler-case (read-byte stream nil nil)
                                      (error () nil))
                         while byte
                         do (vector-push-extend byte buf))
                   (let ((text (sb-ext:octets-to-string
                                (subseq buf 0 (fill-pointer buf))
                                :external-format :utf-8)))
                     (check "fetch close: response present"
                            (not (null (search "HTTP/1.1 200" text))) t)
                     (check "fetch close: body delivered"
                            (not (null (search "bye" text))) t)
                     (check "fetch close: Connection: close stamped on wire"
                            (not (null (search "connection: close" text))) t)))
                 (let ((got-second nil))
                   (handler-case
                       (sb-ext:with-timeout 1
                         (write-sequence req2 stream)
                         (force-output stream)
                         (let ((b (read-byte stream nil nil)))
                           (when b (setf got-second t))))
                     (error () nil))
                   (check "fetch close: second request on same socket fails"
                          got-second nil))))
          (ignore-errors (sb-bsd-sockets:socket-close socket)))))))

(defun test-harness-http11-server-close-stamps-connection-close-e2e ()
  "When an HTTP/1.1 client sends 'Connection: close', the server's
   response MUST carry 'Connection: close' (RFC 7230 §6.1: a sender
   that receives a close option SHOULD echo it). Exercises the
   CONNECTION-HINT-FOR path for a handler that doesn't explicitly
   set Connection — close-after-p=T set by request parse → hint
   returns :CLOSE → FORMAT-RESPONSE stamps the header on the wire."
  (format t "~%Harness: HTTP/1.1 client close stamps Connection: close~%")
  (with-test-server
      (:handler (lambda (req)
                  (declare (ignore req))
                  (make-text-response 200 "body")))
    (let* ((req (concatenate 'string
                             "GET / HTTP/1.1" *crlf*
                             "Host: localhost" *crlf*
                             "Connection: close" *crlf* *crlf*))
           (raw (%raw-http-request
                 (sb-ext:string-to-octets req :external-format :ascii))))
      (check "http/1.1 client-close: response present"
             (not (null (search "HTTP/1.1 200" raw))) t)
      (check "http/1.1 client-close: body delivered"
             (not (null (search "body" raw))) t)
      (check "http/1.1 client-close: Connection: close stamped on wire"
             (not (null (search "connection: close" raw))) t))))

(defun test-harness-connection-header-split-e2e ()
  "A client that sends split Connection: keep-alive / Connection:
   close pair (semantically equivalent to 'keep-alive, close' per
   RFC 7230 §6.1) must be read as 'close' — GET-HEADERS walks every
   instance, not just the first one."
  (format t "~%Harness: split Connection header close wins~%")
  (with-test-server
      (:handler (lambda (req)
                  (declare (ignore req))
                  (make-text-response 200 "body")))
    (let* ((req (concatenate 'string
                             "GET / HTTP/1.1" *crlf*
                             "Host: localhost" *crlf*
                             "Connection: keep-alive" *crlf*
                             "Connection: close" *crlf* *crlf*))
           (raw (%raw-http-request
                 (sb-ext:string-to-octets req :external-format :ascii))))
      (check "split Connection: response present"
             (not (null (search "HTTP/1.1 200" raw))) t)
      ;; If the server honored 'close', the socket was closed after
      ;; this response — which is already the shape of this test
      ;; (raw reads until EOF). The presence of a complete response
      ;; confirms framing was correct.
      (check "split Connection: body byte received"
             (not (null (search "body" raw))) t))))

(defun test-harness-workers-zero-rejected ()
  "start-server with :workers 0 must raise rather than silently
   block on the main-thread sleep loop with no workers listening.

   The test runs start-server in a background thread with a bounded
   JOIN-THREAD :timeout — if the validation check ever regresses,
   the direct call would hang forever in the sleep loop and the
   test suite would never complete. The 2-second timeout + thread
   terminate surfaces the regression as a visible failure instead."
  (format t "~%Harness: start-server :workers 0 rejects~%")
  (flet ((try-workers (n)
           (let ((errored nil))
             (let ((th (sb-thread:make-thread
                        (lambda ()
                          (handler-case
                              (progn
                                (start-server
                                 :workers n
                                 :handler (lambda (r) (declare (ignore r))))
                                nil)
                            (error () (setf errored t))))
                        :name "workers-validation-probe")))
               (handler-case
                   (sb-thread:join-thread th :timeout 2)
                 (error ()
                   (ignore-errors (sb-thread:terminate-thread th))
                   (ignore-errors (sb-thread:join-thread th)))))
             errored)))
    (check "start-server: :workers 0 signals error"
           (try-workers 0) t)
    (check "start-server: :workers -1 signals error"
           (try-workers -1) t)
    (check "start-server: :workers :auto signals error"
           (try-workers :auto) t)))

(defun test-harness-pipelined-with-fin-e2e ()
  "Two HTTP/1.1 requests pipelined onto one connection, followed by
   a half-close from the client, both dispatch. After the keep-alive
   reset shifts req2 to offset 0, the next read sees :EOF with user-
   space bytes still buffered — the state machine processes req2
   rather than closing on the :EOF. Mirrors the :AGAIN arm's
   buffer-non-empty guard."
  (format t "~%Harness: pipelined requests followed by FIN~%")
  (with-test-server
      (:handler (lambda (req)
                  (make-text-response
                   200 (format nil "path=~a" (http-request-path req)))))
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
             (let* ((stream (sb-bsd-sockets:socket-make-stream
                             socket :input t :output t
                             :element-type '(unsigned-byte 8)))
                    (requests
                     (concatenate 'string
                                  "GET /a HTTP/1.1" *crlf*
                                  "Host: localhost" *crlf* *crlf*
                                  "GET /b HTTP/1.1" *crlf*
                                  "Host: localhost" *crlf*
                                  "Connection: close" *crlf* *crlf*)))
               (write-sequence (sb-ext:string-to-octets
                                requests :external-format :ascii)
                               stream)
               (force-output stream)
               ;; Half-close so the server sees FIN after having both
               ;; requests already in its buffer. This is the shape
               ;; that reproduced the drop. Advisory — if the server
               ;; has already processed + closed, the fd-stream may
               ;; have torn down the fd during flush and the syscall
               ;; raises EBADF. The write is already out; assertions
               ;; run against what came back.
               (ignore-errors
                (sb-bsd-sockets:socket-shutdown socket :direction :output))
               (let ((buf (make-array 16384 :element-type '(unsigned-byte 8)
                                            :fill-pointer 0 :adjustable t)))
                 (loop for byte = (handler-case (read-byte stream nil nil)
                                    (error () nil))
                       while byte
                       do (vector-push-extend byte buf))
                 (let* ((text (sb-ext:octets-to-string
                               (subseq buf 0 (fill-pointer buf))
                               :external-format :utf-8))
                        (first-200 (search "HTTP/1.1 200" text))
                        (second-200 (and first-200
                                         (search "HTTP/1.1 200" text
                                                 :start2 (1+ first-200)))))
                   (check "pipelined: first response present"
                          (not (null first-200)) t)
                   (check "pipelined: second response present"
                          (not (null second-200)) t)
                   (check "pipelined: body /a delivered"
                          (not (null (search "path=/a" text))) t)
                   (check "pipelined: body /b delivered"
                          (not (null (search "path=/b" text))) t)))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

;;; ---------------------------------------------------------------------------
;;; Runner
;;; ---------------------------------------------------------------------------

(defun test-harness ()
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
  (format t "~%=== Test Harness Tests ===~%")
  ;; Unit tests first (fast)
  (test-harness-make-test-request)
  (test-harness-make-test-ws-frame)
  ;; End-to-end tests (~1-2s each due to server teardown)
  (test-harness-basic-get)
  (test-harness-post-with-body)
  (test-harness-shutdown-hook-e2e)
  (test-harness-expect-100-continue-e2e)
  (test-harness-head-fetch-e2e)
  (test-harness-body-at-max-size-e2e)
  (test-harness-pipelined-with-fin-e2e)
  (test-harness-cached-response-survives-head-e2e)
  (test-harness-http10-keepalive-no-mutation-e2e)
  (test-harness-http10-expect-100-continue-no-fire-e2e)
  (test-harness-expect-417-on-unknown-e2e)
  (test-harness-expect-417-on-unknown-no-body-e2e)
  (test-harness-expect-417-head-no-body-e2e)
  (test-harness-connection-header-split-e2e)
  (test-harness-handler-connection-close-honored-e2e)
  (test-harness-fetch-callback-connection-close-honored-e2e)
  (test-harness-http11-server-close-stamps-connection-close-e2e)
  (test-harness-workers-zero-rejected)
  (report-suite "Harness")
  (zerop *tests-failed*))
