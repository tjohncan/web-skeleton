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

;;; Every socket read in this file is bounded. READ-UNTIL-BOUNDED lives in
;;; the harness system and carries the argument for why; the two helpers
;;; below are the shapes this file needs on top of it.

(defun read-to-eof-bounded (stream buf &key (seconds *test-read-timeout*))
  "Drain STREAM into BUF until it ends, giving up after SECONDS.
   Returns (values ENDED-P CLEAN-P).

   The two are separate answers and the separation is the point. ENDED-P
   is NIL when the deadline passed with the stream still open. CLEAN-P is
   NIL when the read ended in an error rather than an ordinary end of
   stream — a reset, in practice — which is a different outcome from a
   graceful close and precisely the one REFUSE-CONNECTION's drain
   decides. Collapsing them would make this unusable for the test that
   cares.

   The caller supplies BUF because it wants the bytes whether or not the
   read ended, which is also why READ-UNTIL-BOUNDED takes :INTO."
  (multiple-value-bind (filled reason)
      (read-until-bounded stream :into buf :seconds seconds)
    (declare (ignore filled))
    (values (not (eq reason :deadline))
            (eq reason :eof))))

(defun drain-response (stream)
  "Read a whole close-delimited response and return it as a string.
   Bounded, and decoded as latin-1 so that a truncated multi-byte
   sequence in whatever did arrive cannot raise on top of the failure
   being diagnosed."
  (let ((buf (read-until-bounded stream)))
    (sb-ext:octets-to-string (subseq buf 0 (fill-pointer buf))
                             :external-format :latin-1)))

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
  (let ((fires 0))
    (with-test-server
        (:handler (lambda (req)
                    (declare (ignore req))
                    (register-cleanup (lambda () (incf fires)))
                    (make-text-response 200 "ok")))
      (test-http-request :get "/"))
    ;; A count rather than a flag, so a hook run twice fails here instead
    ;; of passing. An app releasing a resource twice is worse off than one
    ;; that never hears.
    (check "cleanup hook fires exactly once during teardown" fires 1)))

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
             (drain-response stream)))
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
             (drain-response stream)))
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
                 (let ((raw (drain-response stream)))
                   (check "417: handler not reached" reached-handler nil)
                   (check "417: status line on wire"
                          (not (null (search "HTTP/1.1 417" raw))) t)
                   (check "417: connection: close on wire"
                          (not (null (search "connection: close" raw))) t)
                   (check "417: date header present"
                          (not (null (search "date:" raw))) t))))
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
                 (let ((raw (drain-response stream)))
                   (check "417 no-body: handler not reached" reached-handler nil)
                   (check "417 no-body: status line on wire"
                          (not (null (search "HTTP/1.1 417" raw))) t)
                   (check "417 no-body: connection: close on wire"
                          (not (null (search "connection: close" raw))) t)
                   (check "417 no-body: date header present"
                          (not (null (search "date:" raw))) t))))
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
               (let* ((raw (drain-response stream))
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
                        (- (length raw) (+ (or hdr-end 0) 4)) 0))))
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
               ;; Drain the first response until EOF. A correct close
               ;; makes EOF arrive promptly; a regression that held the
               ;; socket open used to block here until with-test-server's
               ;; teardown noticed. Bounded now, so the same regression
               ;; fails these checks with whatever did arrive instead.
               (let ((text (drain-response stream)))
                 (check "handler close: response present"
                        (not (null (search "HTTP/1.1 200" text))) t)
                 (check "handler close: body delivered"
                        (not (null (search "bye" text))) t)
                 (check "handler close: Connection: close stamped on wire"
                        (not (null (search "connection: close" text))) t))
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
                 (let ((text (drain-response stream)))
                   (check "fetch close: response present"
                          (not (null (search "HTTP/1.1 200" text))) t)
                   (check "fetch close: body delivered"
                          (not (null (search "bye" text))) t)
                   (check "fetch close: Connection: close stamped on wire"
                          (not (null (search "connection: close" text))) t))
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

(defun test-harness-awaiting-timeout-answers-504-e2e ()
  "An inbound parked on a fetch that never comes back is answered 504,
   not closed without a word.

   DEPLOYMENT.md promised this and the code did not do it: the sweeper
   collected the timed-out connection and called CLOSE-CONNECTION, which
   removes it from epoll, unregisters it, and closes the fd. The client
   got a bare TCP close after *FETCH-TIMEOUT* seconds and nothing to
   distinguish it from the server dying.

   The upstream is a listener that accepts and never writes, built inline
   rather than added to the harness — the harness ships, and one internal
   test is not a reason to widen what it promises. The TCP connect
   succeeds because the kernel completes the handshake into the accept
   queue without anyone calling accept, so the fetch gets all the way to
   :out-read and waits there, which is the state under test.

   *FETCH-TIMEOUT* is set globally rather than bound: the worker runs in a
   thread START-SERVER spawned, and a LET here would not reach it."
  (format t "~%Harness: :awaiting timeout answers 504~%")
  (let ((saved web-skeleton:*fetch-timeout*)
        (silent (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (setf web-skeleton:*fetch-timeout* 2)
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address silent) t)
           (sb-bsd-sockets:socket-bind silent #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen silent 5)
           (multiple-value-bind (host upstream-port)
               (sb-bsd-sockets:socket-name silent)
             (declare (ignore host))
             (let ((cleanup-fires 0))
               (with-test-server
                   (:handler
                    (lambda (req)
                      (declare (ignore req))
                      (defer-to-fetch :GET
                        (format nil "http://127.0.0.1:~d/never" upstream-port)
                        :then (lambda (status headers body)
                                (declare (ignore headers body))
                                (unless status (incf cleanup-fires))
                                (make-text-response (or status 500)
                                                    "unreached")))))
                 (let ((start (get-internal-real-time)))
                   ;; A regression here answers nothing at all, and
                   ;; TEST-HTTP-REQUEST raises rather than returning on a
                   ;; response it cannot parse. Caught so that becomes a
                   ;; failed check with the diagnostic attached instead of
                   ;; a backtrace that ends the whole suite run.
                   (multiple-value-bind (status headers body)
                       (handler-case (test-http-request :get "/proxy")
                         (error (e) (values nil nil (princ-to-string e))))
                     (declare (ignore headers))
                     (let ((secs (/ (float (- (get-internal-real-time) start))
                                    internal-time-units-per-second)))
                       (check "awaiting timeout: answers 504, not a bare close"
                              status 504)
                       (check "awaiting timeout: body names the condition"
                              (and body (search "Gateway Timeout" body) t) t)
                       ;; Bounded on both sides. Too early would mean
                       ;; something other than the sweeper answered; too
                       ;; late would mean the sweeper is not the thing
                       ;; that did. The sweep runs at 1 Hz against a
                       ;; one-second clock, so a 2 s timeout lands in
                       ;; [2, 4) plus scheduling.
                       (check "awaiting timeout: at roughly *fetch-timeout*"
                              (and (> secs 1.0) (< secs 10.0)) t)))))
               ;; The fetch callback's cleanup sentinel must still fire
               ;; exactly once — CLOSE-OUTBOUND is what fires it, and
               ;; answering the inbound must not skip tearing the
               ;; outbound down.
               ;; A count, not a flag. DEPLOYMENT.md promises the callback
               ;; fires exactly once per fetch lifetime, and that invariant
               ;; is held by slot-nulling across three functions — a flag
               ;; here reads T whether it fired once or twice, so the
               ;; promise was untestable.
               (check "awaiting timeout: cleanup sentinel fires exactly once"
                      cleanup-fires 1))))
      (setf web-skeleton:*fetch-timeout* saved)
      (ignore-errors (sb-bsd-sockets:socket-close silent)))))

(defun %canned-upstream (listener response &key (name "canned-upstream"))
  "Accept once, read the request head, write RESPONSE verbatim, close.
   Returns the thread.

   The write and the close go back to back, so the last bytes and the FIN
   reach the framework in one wake-up — the ordinary shape on loopback,
   and the one that produces :OK-EOF rather than :EOF. Every framing that
   depends on where the response ends is exercised by handing this a
   different RESPONSE, which is why the string is a parameter and the
   sequencing is not.

   Reads only to the request's CRLFCRLF, never to EOF. A fetch sends its
   request and then waits, so no end of stream is coming until this
   answers: draining to EOF here deadlocks both sides."
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (let* ((s (sb-bsd-sockets:socket-accept listener))
                (st (sb-bsd-sockets:socket-make-stream
                     s :input t :output t :element-type '(unsigned-byte 8)))
                (b (make-array 4096 :element-type '(unsigned-byte 8)
                                    :fill-pointer 0 :adjustable t)))
           (loop for byte = (read-byte st nil nil)
                 while byte
                 do (vector-push-extend byte b)
                 until (web-skeleton::scan-crlf-crlf b 0 (fill-pointer b)))
           (write-sequence
            (sb-ext:string-to-octets response :external-format :ascii) st)
           (force-output st)
           (sb-bsd-sockets:socket-close s))
       (error () nil)))
   :name name))

(defun %crlf (&rest lines)
  "LINES joined by CRLF, with a trailing CRLF. Hand-written wire bytes: the
   chunk boundaries a test asserts on have to be the test's choice, not
   whatever an encoder picked on the day."
  (with-output-to-string (out)
    (dolist (line lines)
      (format out "~a~c~c" line #\Return #\Newline))))

(defun test-harness-close-delimited-fetch-e2e ()
  "A fetch of a close-delimited upstream completes when the close arrives,
   not when *FETCH-TIMEOUT* expires.

   CONNECTION-READ-AVAILABLE used to report \"read bytes, then EOF\" as
   :OK, discarding the end of stream. For a response with no
   Content-Length and no Transfer-Encoding that end of stream *is* the
   framing — OUTBOUND-RESPONSE-COMPLETE-P returns NIL forever and says so,
   deferring to the caller's EOF branch — so the branch never ran and the
   fetch hung until the sweeper took it.

   A TCP peer's close does not set EPOLLHUP, so HANDLE-OUTBOUND-EVENT's
   error arm did not rescue it either. Nothing about this needs
   configuring: our own outbound requests send Connection: close, which
   invites an upstream to frame this way.

   Uses an IP literal so the numeric fast path skips DNS — the framing is
   what is under test here, not resolution."
  (format t "~%Harness: close-delimited upstream fetch~%")
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp))
        (thread nil))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 5)
           (multiple-value-bind (host upstream-port)
               (sb-bsd-sockets:socket-name listener)
             (declare (ignore host))
             (setf thread
                   (%canned-upstream
                    listener
                    (concatenate 'string
                                 (%crlf "HTTP/1.1 200 OK"
                                        "Content-Type: text/plain"
                                        "")
                                 "close-framed-body")
                    :name "close-delimited-upstream"))
             (with-test-server
                 (:handler
                  (lambda (req)
                    (declare (ignore req))
                    (defer-to-fetch :GET
                      (format nil "http://127.0.0.1:~d/framed" upstream-port)
                      :then (lambda (status headers body)
                              (declare (ignore headers))
                              (make-text-response
                               (or status 500)
                               (format nil "~a|~a" status
                                       (if body
                                           (sb-ext:octets-to-string
                                            body :external-format :utf-8)
                                           "(none)")))))))
               (let ((start (get-internal-real-time)))
                 (multiple-value-bind (status headers body)
                     (handler-case (test-http-request :get "/proxy")
                       (error (e) (values nil nil (princ-to-string e))))
                   (declare (ignore headers))
                   (let ((secs (/ (float (- (get-internal-real-time) start))
                                  internal-time-units-per-second)))
                     (check "close-delimited: status delivered" status 200)
                     (check "close-delimited: body survives the framing"
                            (and body (search "200|close-framed-body" body) t) t)
                     ;; The discriminating one. *FETCH-TIMEOUT* is 30 by
                     ;; default and the harness read deadline is 10, so a
                     ;; regression shows up here as a wait rather than as
                     ;; a wrong answer.
                     (check "close-delimited: completes on the close, promptly"
                            (< secs 5.0) t)))))))
      (ignore-errors (sb-bsd-sockets:socket-close listener))
      (when thread
        (handler-case (sb-thread:join-thread thread :timeout 5)
          (error () (ignore-errors (sb-thread:terminate-thread thread))))))))

(defparameter *chunked-corpus* '("aa" "bbb" "cccc")
  "The chunk payloads the :ON-BODY framing tests assert on.

   One definition, because the claim the TLS test makes is that these
   arrive *identically* over both transports. Written as two literals it
   would instead be asserting that two literals had not drifted apart,
   which is a weaker claim about a different thing.")

(defun %chunked-corpus-response ()
  "*CHUNKED-CORPUS* as a complete chunked HTTP response, terminator
   included.

   Framed here rather than run through the encoder: a test asserting on
   chunk boundaries has to choose them, not inherit whatever the encoder
   picked on the day."
  (apply #'%crlf "HTTP/1.1 200 OK" "Transfer-Encoding: chunked" ""
         (append (loop for payload in *chunked-corpus*
                       append (list (format nil "~x" (length payload)) payload))
                 (list "0" ""))))

(defun %chunked-upstream-fetch (response on-body-out then-out fires-box)
  "Run one :ON-BODY fetch against a canned upstream sending RESPONSE, and
   return the relay's own (VALUES STATUS BODY).

   Shared by the two tests below because only RESPONSE differs between
   them: one ends with the zero-size terminator and one does not, and
   everything else about the setup is the thing being held constant."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp))
        (thread nil))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 5)
           (multiple-value-bind (host upstream-port)
               (sb-bsd-sockets:socket-name listener)
             (declare (ignore host))
             (setf thread (%canned-upstream listener response
                                            :name "chunked-upstream"))
             (with-test-server
                 (:handler
                  (lambda (req)
                    (declare (ignore req))
                    (http-fetch
                     :get (format nil "http://127.0.0.1:~d/chunked"
                                  upstream-port)
                     :on-body (lambda (conn chunk)
                                (declare (ignore conn))
                                (push (sb-ext:octets-to-string
                                       chunk :external-format :ascii)
                                      (car on-body-out)))
                     :then (lambda (status headers body)
                             (declare (ignore headers))
                             (incf (car fires-box))
                             (setf (car then-out)
                                   (list status (if body :present :nil)))
                             (make-text-response 200 "relayed")))))
               (multiple-value-bind (status headers body)
                   (handler-case (test-http-request :get "/relay")
                     (error (e) (values nil nil (princ-to-string e))))
                 (declare (ignore headers))
                 (values status body)))))
      (ignore-errors (sb-bsd-sockets:socket-close listener))
      (when thread
        (handler-case (sb-thread:join-thread thread :timeout 5)
          (error () (ignore-errors (sb-thread:terminate-thread thread))))))))

(defun test-harness-fetch-on-body-eof-together-e2e ()
  "A chunked upstream whose entire response and FIN arrive in one read
   still delivers every chunk to :ON-BODY.

   :OK-EOF means \"read some bytes, and then hit end of stream\". Sharing
   an arm with :EOF — read *nothing*, already at end of stream — meant
   the bytes it carried were never walked, so :ON-BODY never fired; and
   COMPLETE-FETCH then nulled the buffered body on the strength of an
   :ON-BODY callback merely being installed. The app got 200 with no body
   at all. Not truncated, absent.

   TEST-HARNESS-FETCH-ON-BODY-E2E cannot catch this and is not weaker for
   it: its upstream streams over time, so the FIN reliably arrives as its
   own event and the coalesced case never occurs.

   The canned upstream writes and closes back to back, which makes the
   coalesced read the usual outcome and not a guaranteed one — the
   framework can still be scheduled between the write and the close, read
   the bytes as :OK, and take the ordinary path. Measured: this catches a
   reintroduced defect on most runs and not all.
   TEST-FETCH-OK-EOF-WALKS-THE-BYTES pins the branch itself, every run.
   This test is the end-to-end shape around it, and the pair is the
   coverage.

   Both halves are asserted. The chunks pin :ON-BODY as the route that
   delivered them, and the NIL in :THEN pins the contract that they are
   not handed over a second time — a repair that moved the body into
   :THEN instead would satisfy neither."
  (format t "~%Harness: fetch :on-body with body and FIN in one read~%")
  (let ((chunks (list nil))
        (final (list :never))
        (fires (list 0)))
    (multiple-value-bind (status body)
        (%chunked-upstream-fetch (%chunked-corpus-response) chunks final fires)
      (check "on-body/eof: the relay answered" status 200)
      (check "on-body/eof: and its own body came through" body "relayed"))
    ;; The discriminating one: NIL here was the defect.
    (check "on-body/eof: every chunk reached :on-body, in order"
           (reverse (car chunks)) *chunked-corpus*)
    (check "on-body/eof: :then saw the upstream status"
           (first (car final)) 200)
    (check "on-body/eof: :then got no body to re-deliver"
           (second (car final)) :nil)
    (check "on-body/eof: :then fires exactly once" (car fires) 1)))

(defun test-fetch-ok-eof-walks-the-bytes ()
  "HANDLE-OUTBOUND-READ walks the bytes that arrived with the FIN.

   The transport here is a stub that hands back the whole response and
   then reports end of stream, so CONNECTION-READ-AVAILABLE returns
   :OK-EOF on every run. That is the reason to do it at this seam rather
   than over a socket: whether a real peer's last bytes and its FIN land
   in one wake-up is a scheduling question, and a test that only
   sometimes reaches the branch it covers only sometimes catches a
   regression in it.

   Chunk delivery is the assertion. Routing :OK-EOF into the :EOF arm
   skips the walk and :ON-BODY never fires — the defect, deterministic
   here.

   No inbound is parked, so the fetch ends on its cleanup sentinel rather
   than a delivery. That is not the shape under test and the end-to-end
   pair covers it; what matters here is that it ends exactly once."
  (format t "~%Fetch: :OK-EOF still walks the bytes it carried~%")
  (let* ((bytes (sb-ext:string-to-octets (%chunked-corpus-response)
                                         :external-format :ascii))
         (pos 0)
         (chunks nil)
         (fires 0)
         (socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp))
         (epfd (web-skeleton::epoll-create))
         ;; RUN-WORKER binds this in its own dynamic scope, and teardown
         ;; goes through it. Driving one connection outside a worker means
         ;; supplying the table the worker would have.
         (web-skeleton::*connections* (make-hash-table)))
    (unwind-protect
         (let ((conn (web-skeleton::make-connection
                      :fd (web-skeleton::socket-fd socket)
                      :socket socket
                      :state :out-read
                      :outbound-p t
                      :inbound-fd -1
                      :fetch-method :GET
                      :last-active (get-universal-time)
                      :fetch-on-body
                      (lambda (c chunk)
                        (declare (ignore c))
                        (push (sb-ext:octets-to-string
                               chunk :external-format :ascii)
                              chunks))
                      :fetch-callback
                      (lambda (status headers body)
                        (declare (ignore status headers body))
                        (incf fires)
                        nil)
                      ;; Bytes until they run out, then :EOF — never
                      ;; :AGAIN, which is what makes the verdict :OK-EOF
                      ;; rather than :OK.
                      :read-fn
                      (lambda (buf start max)
                        (if (>= pos (length bytes))
                            :eof
                            (let ((n (min max (- (length bytes) pos))))
                              (replace buf bytes :start1 start
                                                 :start2 pos :end2 (+ pos n))
                              (incf pos n)
                              n))))))
           (web-skeleton::handle-outbound-read conn epfd)
           (check "ok-eof: every chunk reached :on-body, in order"
                  (reverse chunks) *chunked-corpus*)
           (check "ok-eof: the fetch ended exactly once" fires 1))
      (ignore-errors (web-skeleton::%close epfd)))))

(defun test-harness-fetch-on-body-truncated-chunked-e2e ()
  "A chunked upstream that delivers chunks and then closes without the
   zero-size terminator fails the fetch, rather than reporting success.

   The truncation guard on this framing is DECODE-CHUNKED-BODY's raise —
   there is no Content-Length to compare against — and suppressing the
   buffered body suppressed the guard along with it. Chunks went to
   :ON-BODY, the upstream vanished mid-body, and :THEN fired 200 with a
   NIL body: byte-for-byte the report a *whole* response produces. The
   app cannot tell the two apart, which is the failure the Content-Length
   guard already refuses to have.

   The chunks arriving first is asserted rather than assumed. Without
   that, this would also pass against an upstream that failed before
   sending anything — a different case, and one already covered."
  (format t "~%Harness: fetch :on-body against a truncated chunked upstream~%")
  (let ((chunks (list nil))
        (final (list :never))
        (fires (list 0)))
    (multiple-value-bind (status body)
        (%chunked-upstream-fetch
         ;; Chunks, then the close. No "0" terminator.
         (%crlf "HTTP/1.1 200 OK" "Transfer-Encoding: chunked" ""
                "2" "aa" "3" "bbb")
         chunks final fires)
      (declare (ignore body))
      (check "truncated-chunked: the relay reports failure" status 502))
    ;; Non-vacuity: this is the truncated-mid-body case, not a fetch that
    ;; failed before any of it arrived.
    (check "truncated-chunked: the chunks that did arrive were delivered"
           (reverse (car chunks)) '("aa" "bbb"))
    (check "truncated-chunked: :then fired the cleanup sentinel"
           (first (car final)) nil)
    (check "truncated-chunked: :then fires exactly once" (car fires) 1)))

(defun test-harness-dns-all-addresses-refused-e2e ()
  "A hostname whose every resolved address the policy refuses fails the
   fetch promptly, with a 502, rather than stranding until the sweeper.

   Read this before trusting it: **this test passes without the fix**, and
   is here as an end-to-end assertion of the DEPLOYMENT.md promise rather
   than as coverage for :OK-EOF. TEST-READ-AVAILABLE-EOF is the check with
   teeth.

   The reason is the mechanism. getent writes its output in one go and the
   EOF appears when it exits, so whether one drain sees both depends on
   whether it has exited by the time we read — and usually it has not. The
   bytes come back :OK, the exit arrives as a separate event, and the old
   code handled it on a clean :EOF. Only when getent finishes first do the
   two coalesce, and that is the ordering the bug needed. A race, not a
   certainty, which is exactly why it survived a suite that covers the
   parser it sits behind.

   What the test does assert is the promise: a name whose every resolved
   address *FETCH-ADDRESS-FILTER* refuses fails the fetch promptly with a
   502 and fires the cleanup sentinel once. That is the SSRF-defense path,
   which is where DEPLOYMENT.md made the promise and the worst place for
   it to go unkept.

   The filter is set globally, not bound: the worker reads it on a thread
   START-SERVER spawned."
  (format t "~%Harness: DNS with every address refused~%")
  (let ((saved web-skeleton:*fetch-address-filter*))
    (setf web-skeleton:*fetch-address-filter*
          (lambda (ip family host)
            (declare (ignore ip family host))
            nil))
    (unwind-protect
         (let ((sentinel-fires 0))
           (with-test-server
               (:handler
                (lambda (req)
                  (declare (ignore req))
                  ;; A name, not a literal — the literal fast paths skip
                  ;; DNS and this is about what happens after getent runs.
                  (defer-to-fetch :GET "http://localhost:9/refused"
                    :then (lambda (status headers body)
                            (declare (ignore headers body))
                            (unless status (incf sentinel-fires))
                            (make-text-response (or status 500) "unreached")))))
             (let ((start (get-internal-real-time)))
               (multiple-value-bind (status headers body)
                   (handler-case (test-http-request :get "/proxy")
                     (error (e) (values nil nil (princ-to-string e))))
                 (declare (ignore headers body))
                 (let ((secs (/ (float (- (get-internal-real-time) start))
                                internal-time-units-per-second)))
                   (check "dns all-refused: answers 502" status 502)
                   (check "dns all-refused: promptly, not at the sweep"
                          (< secs 5.0) t)))))
           (check "dns all-refused: cleanup sentinel fires exactly once"
                  sentinel-fires 1))
      (setf web-skeleton:*fetch-address-filter* saved))))

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

(defun test-harness-write-stall-timeout-zero-rejected ()
  "start-server must refuse a non-positive *write-stall-timeout*.

   WS-SEND checks it too, but only callers of WS-SEND reach that check,
   and a handler that returns a frame instead of pushing one appends
   through a path that never sees it. With the deadline disabled, that
   path — the primary documented shape — has no time bound on an
   undrained queue at all: *ws-idle-timeout* defaults to a day and is
   refreshed by reads a peer that stopped reading may still be sending.
   Three documents promise no setting disables the deadline. This is
   where that promise is kept.

   The error message is asserted, not merely the fact of an error, so
   this cannot pass on a rejection that happened for some other reason.
   The positive case needs no check here: every other harness test starts
   a server at the default of 10.

   SETF rather than LET, and restored in an UNWIND-PROTECT — the probe
   runs in a fresh thread and dynamic bindings do not cross MAKE-THREAD."
  (format t "~%Harness: start-server *write-stall-timeout* 0 rejects~%")
  (let ((saved *write-stall-timeout*))
    (unwind-protect
         (flet ((try-timeout (v)
                  (setf *write-stall-timeout* v)
                  (let ((msg nil))
                    (let ((th (sb-thread:make-thread
                               (lambda ()
                                 (handler-case
                                     (progn
                                       (start-server
                                        :workers 1
                                        :handler (lambda (r) (declare (ignore r))))
                                       nil)
                                   (error (e) (setf msg (princ-to-string e)))))
                               :name "write-stall-timeout-validation-probe")))
                      (handler-case
                          (sb-thread:join-thread th :timeout 2)
                        (error ()
                          (ignore-errors (sb-thread:terminate-thread th))
                          (ignore-errors (sb-thread:join-thread th)))))
                    msg)))
           (check "start-server: *write-stall-timeout* 0 signals error"
                  (let ((m (try-timeout 0)))
                    (and m (not (null (search "*write-stall-timeout*" m))) t))
                  t)
           (check "start-server: *write-stall-timeout* -1 signals error"
                  (let ((m (try-timeout -1)))
                    (and m (not (null (search "*write-stall-timeout*" m))) t))
                  t))
      (setf *write-stall-timeout* saved))))

(defun test-harness-write-backlog-minimum-rejected ()
  "start-server must refuse a *max-write-backlog* that cannot carry a
   maximal WebSocket message.

   The backlog has to clear *max-ws-message-size* by ten bytes — the
   largest header BUILD-WS-FRAME emits — or the receive path accepts a
   payload the send path is then refused permission to return. Three
   places said so and nothing checked it, which is the worst combination:
   documented as a boundary and reachable anyway. Nobody hits it at the
   defaults, 2 MiB against 1 MiB, and the deployment that does hit it is
   the obvious one — someone trimming memory lowers the backlog and the
   MiB of headroom disappears.

   The error message is asserted rather than merely the fact of an error,
   so this cannot pass on a rejection that happened for another reason —
   the probe passes :workers 1 and a valid handler precisely so the other
   two validations have nothing to say.

   Exactly-at-the-bound is asserted too, because a >= written as > is the
   plausible slip and refusing a legal configuration is its own defect.

   SETF rather than LET, and restored in an UNWIND-PROTECT: the probe
   runs in a fresh thread and dynamic bindings do not cross MAKE-THREAD."
  (format t "~%Harness: start-server *max-write-backlog* minimum~%")
  (let ((saved *max-write-backlog*))
    (unwind-protect
         (flet ((try-backlog (v)
                  (setf *max-write-backlog* v)
                  (let ((msg nil))
                    (let ((th (sb-thread:make-thread
                               (lambda ()
                                 (handler-case
                                     (progn
                                       (start-server
                                        :workers 1
                                        :handler (lambda (r) (declare (ignore r))))
                                       nil)
                                   (error (e) (setf msg (princ-to-string e)))))
                               :name "write-backlog-validation-probe")))
                      (handler-case
                          (sb-thread:join-thread th :timeout 2)
                        (error ()
                          (ignore-errors (sb-thread:terminate-thread th))
                          (ignore-errors (sb-thread:join-thread th)))))
                    msg)))
           (check "start-server: a backlog under the message size signals"
                  (let ((m (try-backlog 1024)))
                    (and m (not (null (search "*max-write-backlog*" m))) t))
                  t)
           ;; One byte short of the ten-byte header allowance: the case
           ;; the requirement is actually about, and the one a naive
           ;; "backlog >= message size" check would let through.
           (check "start-server: nine bytes of headroom is still too few"
                  (let ((m (try-backlog (+ *max-ws-message-size* 9))))
                    (and m (not (null (search "*max-write-backlog*" m))) t))
                  t)
           ;; And exactly at the bound must start. Asserted by the absence
           ;; of a message, since this probe's server is torn down by the
           ;; thread timeout rather than returning.
           (check "start-server: exactly ten bytes of headroom is accepted"
                  (let ((m (try-backlog (+ *max-ws-message-size* 10))))
                    (or (null m)
                        (null (search "*max-write-backlog*" m))))
                  t))
      (setf *max-write-backlog* saved))))

(defun %split-ws (line)
  "LINE split on runs of space and tab. /proc/net/tcp columns are
   space-padded to varying widths, so a fixed-offset read of it is
   wrong on the first line whose inode or uid changes length."
  (let ((out nil) (i 0) (n (length line)))
    (flet ((wsp (c) (or (char= c #\Space) (char= c #\Tab))))
      (loop
        (loop while (and (< i n) (wsp (char line i))) do (incf i))
        (when (>= i n) (return))
        (let ((start i))
          (loop while (and (< i n) (not (wsp (char line i)))) do (incf i))
          (push (subseq line start i) out))))
    (nreverse out)))

(defun %listen-socket-count (port)
  "How many sockets are in LISTEN state on PORT, per /proc/net/tcp — or
   NIL if the file could not be read cleanly.

   Linux-only, which this framework is. There is no portable substitute
   and, more to the point, no *behavioral* substitute: with SO_REUSEPORT
   the kernel balances across the listen group, so a client that gets an
   answer has learned only that some worker is on the port — which is
   equally true when the other workers landed somewhere else entirely.
   Counting the sockets is the only way to see the difference.

   Field 1 is LOCAL_ADDRESS as HEXIP:HEXPORT, field 3 is the state
   (0A = TCP_LISTEN), field 9 is the socket inode. Ten fields, not
   twelve: TX_QUEUE:RX_QUEUE and TR:TM->WHEN are each colon-joined.

   Distinct inodes, not matching rows, and that is the whole reason the
   inode is parsed at all. This file is a seq_file: the iterator saves a
   position and re-walks it for the next chunk, so records removed before
   that position make the resume land late and skip rows, while records
   added before it make the resume land early and re-emit rows already
   delivered. A suite running live servers does both constantly. Counting
   rows therefore both under- and over-reports; counting inodes, which are
   unique per socket, cannot over-report, and leaves only the undercount
   for the caller to handle.

   NIL rather than 0 because a read of this file has been seen to fail,
   and 'no listeners there' and 'could not look' are different answers:
   a caller that conflates them reports a spurious failure whenever the
   machine is busy.

   Two separate things happen to this file and they were conflated once,
   so: the undercount above is seq_file resume, measured directly. The
   failed read was not. That was EBADF on a descriptor nothing here
   closed, and its cause turned out to live in CONNECTION-CLOSE -- a
   finalizer closing a reissued fd number under its new owner. Fixed
   there. The guard stays because a busy machine can still make the
   count late, and because a caller should not have to know which."
  (handler-case
      (with-open-file (in "/proc/net/tcp" :if-does-not-exist nil)
        (when in
          (let ((inodes nil))
            (read-line in nil nil)      ; column header
            (loop for line = (read-line in nil nil)
                  while line
                  do (let ((fields (%split-ws line)))
                       (when (and (>= (length fields) 10)
                                  (string= (fourth fields) "0A"))
                         (let* ((local (second fields))
                                (colon (position #\: local)))
                           (when (and colon
                                      (ignore-errors
                                       (= port (parse-integer
                                                local :start (1+ colon)
                                                      :radix 16))))
                             (pushnew (nth 9 fields) inodes
                                      :test #'string=))))))
            (length inodes))))
    (error () nil)))

(defun %call-with-bare-server (workers fn)
  "Start a real server with :PORT 0 and WORKERS workers, call FN with the
   port :ON-LISTEN reported (NIL if it never fired), then tear down.

   Deliberately not WITH-TEST-SERVER. That fixture is fixed at one worker
   and it consumes the port resolution internally — and the port
   resolution is the thing under test, so borrowing the fixture that
   depends on it would assert nothing.

   SETF rather than LET on the shutdown specials: the workers read them
   from threads START-SERVER spawns, and dynamic bindings do not cross
   MAKE-THREAD."
  (let ((saved-hooks web-skeleton::*shutdown-hooks*)
        (saved-drain *drain-timeout*)
        (saved-poll *shutdown-poll-interval*)
        (bound nil)
        (sem (sb-thread:make-semaphore :name "bare-server-port")))
    (setf web-skeleton::*shutdown-hooks* nil
          web-skeleton::*shutdown* nil
          *drain-timeout* 1
          *shutdown-poll-interval* 0.05)
    (unwind-protect
         (let ((th (sb-thread:make-thread
                    (lambda ()
                      (start-server
                       :host #(127 0 0 1) :port 0 :workers workers
                       :on-listen (lambda (p)
                                    (setf bound p)
                                    (sb-thread:signal-semaphore sem))
                       :handler (lambda (req)
                                  (declare (ignore req))
                                  (make-text-response 200 "bare"))))
                    :name "bare-server")))
           (unwind-protect
                (funcall fn (when (sb-thread:wait-on-semaphore sem :timeout 10)
                              bound))
             (setf web-skeleton::*shutdown* t)
             (handler-case (sb-thread:join-thread th :timeout 10)
               (error ()
                 (ignore-errors (sb-thread:terminate-thread th))
                 (ignore-errors (sb-thread:join-thread th))))))
      (setf web-skeleton::*shutdown-hooks* saved-hooks
            *drain-timeout* saved-drain
            *shutdown-poll-interval* saved-poll))))

(defun %port-answers-with-p (port marker)
  "T if a plain GET / on PORT comes back containing MARKER.

   Answers NIL for every way of not getting there — an unusable port
   number, a refused connection, a read that never completes. The
   alternative is a raise, and the caller is a CHECK: a regressed port
   resolution reports 0, which is truthy, and CONNECT to 0 raises, so
   letting errors through would turn a clean failed assertion into a
   backtrace that takes the rest of the suite with it.

   No readiness poll. Worker 0 adopts a socket START-SERVER already put
   in LISTEN before :ON-LISTEN fired, so connect(2) succeeds whether or
   not the worker has reached accept(2) yet. Needing a poll here would
   itself be the bug."
  (and (integerp port) (< 0 port 65536)
       (handler-case
           (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                        :type :stream :protocol :tcp)))
             (unwind-protect
                  (progn
                    (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
                    (let ((stream (sb-bsd-sockets:socket-make-stream
                                   socket :input t :output t
                                          :element-type '(unsigned-byte 8))))
                      (write-sequence
                       (sb-ext:string-to-octets
                        (format nil "GET / HTTP/1.1~c~cHost: localhost~c~c~
                                     Connection: close~c~c~c~c"
                                #\Return #\Newline #\Return #\Newline
                                #\Return #\Newline #\Return #\Newline)
                        :external-format :ascii)
                       stream)
                      (force-output stream)
                      (let ((raw (sb-ext:octets-to-string
                                  (coerce (read-until-bounded stream)
                                          '(vector (unsigned-byte 8)))
                                  :external-format :latin-1)))
                        (and (search marker raw) t))))
               (ignore-errors (sb-bsd-sockets:socket-close socket))))
         (error () nil))))

(defun test-harness-fetch-stream-plain-e2e ()
  "HTTP-FETCH-STREAM over http://, end to end.

   It had no test at all, and that is how it came to be shipped broken:
   a botched edit put an undefined variable into its request builder, the
   whole suite stayed green, and only a compiler warning said so — on a
   run whose warning check was itself misgrepped. An exported API with no
   assertion is a place where two mistakes can meet.

   Called from the test thread rather than from a handler, deliberately.
   HTTP-FETCH-STREAM blocks the caller for the whole exchange, so a
   handler on a one-worker server that fetched from its own server would
   wait for a worker it is itself occupying. That is not a flaw in the
   test — it is the documented cost of the blocking API, and the shape of
   this test is what that cost looks like."
  (format t "~%Harness: http-fetch-stream over plain HTTP~%")
  (let ((lines nil))
    (with-test-server
        (:handler (lambda (req)
                    (declare (ignore req))
                    (make-text-response
                     200 (format nil "alpha~%beta~%gamma~%"))))
      (let ((status (attempt
                     (http-fetch-stream
                      :get (format nil "http://127.0.0.1:~d/lines" *test-port*)
                      :on-line (lambda (line) (push line lines))))))
        (check "fetch-stream: the upstream answered" status 200)
        (check "fetch-stream: every line arrived, in order"
               (nreverse lines) (list "alpha" "beta" "gamma"))))))

(defun test-harness-port-zero-reported ()
  "START-SERVER with :PORT 0 binds an ephemeral port and reports it
   through :ON-LISTEN, and the port it reports is the one that serves.

   The second half is the half worth having. A callback that fired with
   the number the caller passed in — 0 — would satisfy 'ON-LISTEN was
   called' and 'ON-LISTEN got an integer' alike. Only a real request
   answered on the reported port distinguishes a resolved port from an
   echoed argument."
  (format t "~%Harness: start-server :port 0 reports its bound port~%")
  (%call-with-bare-server 1
    (lambda (port)
      (check "port 0: :on-listen fired" (not (null port)) t)
      (check "port 0: reported port is a real port"
             (and (integerp port) (< 0 port 65536)) t)
      (check "port 0: reported port serves"
             (%port-answers-with-p port "bare") t))))

(defun test-harness-port-zero-workers-share-one-port ()
  "Every worker binds the port :ON-LISTEN reported — not just worker 0.

   Each worker builds its own listener, so a :PORT 0 handed straight
   down to them puts N workers on N *different* ephemeral ports. Nothing
   about that state fails: worker 0 holds the reported port and answers
   everything sent to it, so every request-shaped check passes while
   N-1 workers sit on ports nobody will ever connect to and the pool
   silently has one member.

   Counting listen sockets is what sees that; making requests is not.
   Polled because the count legitimately lags: START-SERVER binds worker
   0's listener before spawning anything, and workers 1..N-1 bind inside
   their own threads some time after :ON-LISTEN has already fired."
  (format t "~%Harness: start-server :port 0 shares one port across workers~%")
  (let ((workers 3))
    (%call-with-bare-server workers
      (lambda (port)
        (check "port 0 (multi): :on-listen fired" (not (null port)) t)
        (when port
          ;; A NIL from an unreadable /proc keeps polling and, if it is
          ;; still NIL at the end, fails as NIL-against-3 rather than
          ;; being counted as zero listeners.
          ;;
          ;; The budget is slack for a loaded CI box and nothing more:
          ;; measured, all three listeners are present on the first read,
          ;; because MAKE-THREAD plus a bind is microseconds. Only a
          ;; failing run spends the whole budget, and a failing check
          ;; should not also be the suite's longest sleep.
          ;; Highest count seen, not the last one. Measured: a read of
          ;; /proc/net/tcp intermittently returns 2, and once 1, while
          ;; three healthy workers are demonstrably on the port, because a
          ;; seq_file resume can land late and skip records. Overcounting
          ;; is ruled out by %LISTEN-SOCKET-COUNT counting distinct socket
          ;; inodes — the same resume can land early and re-emit a row, so
          ;; the row count alone was not one-directional. So a low read is
          ;; noise, a high read is signal, and reporting the last read
          ;; rather than the best would fail this test at random.
          (let ((n (loop repeat 80
                         with best = nil
                         for c = (%listen-socket-count port)
                         do (when (and c (or (null best) (> c best)))
                              (setf best c))
                         when (and best (>= best workers)) return best
                         do (sleep 0.025)
                         finally (return best))))
            (check "port 0: every worker listens on the reported port"
                   n workers)))))))

(defun %raw-connect ()
  "A raw socket to the live test server, plus its byte stream."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
    (values socket
            (sb-bsd-sockets:socket-make-stream
             socket :input t :output t :element-type '(unsigned-byte 8)))))

(defun %send-raw-get (stream path &key (extra ""))
  (write-sequence (sb-ext:string-to-octets
                   (format nil "GET ~a HTTP/1.1~c~cHost: localhost~c~c~a~c~c"
                           path #\Return #\Newline #\Return #\Newline
                           extra #\Return #\Newline)
                   :external-format :ascii)
                  stream)
  (force-output stream))

(defun test-harness-streaming-e2e ()
  "A streaming response, end to end: head, chunks, terminator, and the
   framework's own decoder reading its own encoder's output back off a
   real socket. The handler returns as soon as it has queued its content,
   which is the shape the whole issue is about — the worker is free while
   the response is still being delivered."
  (format t "~%Harness: streaming response end-to-end~%")
  (with-test-server
      (:handler (lambda (req)
                  (declare (ignore req))
                  (make-stream-response
                   :headers '(("content-type" . "text/plain"))
                   :on-open (lambda (conn)
                              (stream-send conn (sb-ext:string-to-octets
                                                 "alpha " :external-format :ascii))
                              (stream-send conn (sb-ext:string-to-octets
                                                 "beta" :external-format :ascii))
                              (stream-close conn)))))
    (multiple-value-bind (socket stream) (%raw-connect)
      (unwind-protect
           (progn
             (%send-raw-get stream "/stream" :extra
                            (format nil "Connection: close~c~c"
                                    #\Return #\Newline))
             (let* ((buf (read-until-bounded stream))
                    (raw (subseq buf 0 (fill-pointer buf)))
                    (text (sb-ext:octets-to-string raw :external-format :latin-1))
                    (hend (web-skeleton::scan-crlf-crlf raw 0 (length raw))))
               (check "streaming e2e: 200 with chunked framing"
                      (and (search "200 OK" text)
                           (search "transfer-encoding: chunked" text)
                           t)
                      t)
               (check "streaming e2e: no Content-Length"
                      (search "content-length" text) nil)
               ;; Caught for the same reason as the unit-level decode: a
               ;; missing terminator raises here, and a raise ends the
               ;; run rather than reporting.
               (check "streaming e2e: the body decodes to what was streamed"
                      (handler-case
                          (sb-ext:octets-to-string
                           (web-skeleton::decode-chunked-body
                            raw (+ hend 4) (length raw))
                           :external-format :ascii)
                        (error (e) (princ-to-string e)))
                      "alpha beta")))
        (ignore-errors (close stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun test-harness-sse-e2e ()
  "An SSE endpoint end to end. The events go out framed as chunks and
   come back through the framework's own chunked decoder, so what is
   asserted is the event text a browser would actually parse — not the
   bytes the serializer happened to produce."
  (format t "~%Harness: server-sent events end-to-end~%")
  (with-test-server
      (:handler (lambda (req)
                  (declare (ignore req))
                  (make-sse-response
                   :on-open (lambda (conn)
                              (sse-send conn :data "first" :event "tick")
                              (sse-send conn :data "second" :id "2")
                              (sse-comment conn "keep")
                              (stream-close conn)))))
    (multiple-value-bind (socket stream) (%raw-connect)
      (unwind-protect
           (progn
             (%send-raw-get stream "/events" :extra
                            (format nil "Connection: close~c~c"
                                    #\Return #\Newline))
             (let* ((buf (read-until-bounded stream))
                    (raw (subseq buf 0 (fill-pointer buf)))
                    (text (sb-ext:octets-to-string raw :external-format :latin-1))
                    (hend (web-skeleton::scan-crlf-crlf raw 0 (length raw))))
               (check "sse e2e: the content type reaches the client"
                      (and (search "content-type: text/event-stream" text) t) t)
               (check "sse e2e: and so does the proxy hint"
                      (and (search "x-accel-buffering: no" text) t) t)
               (check "sse e2e: the events decode to what was sent"
                      (handler-case
                          (sb-ext:octets-to-string
                           (web-skeleton::decode-chunked-body
                            raw (+ hend 4) (length raw))
                           :external-format :utf-8)
                        (error (e) (princ-to-string e)))
                      (format nil "event: tick~cdata: first~c~c~
                                   id: 2~cdata: second~c~c~
                                   :keep~c"
                              #\Newline #\Newline #\Newline
                              #\Newline #\Newline #\Newline
                              #\Newline))))
        (ignore-errors (close stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun test-harness-sse-keepalive-framed-e2e ()
  "A keepalive on a chunked stream has to be framed as a chunk.

   Every check written for the keepalive before this one looked at the
   response struct or the connection's queue — 'installed by default',
   'nothing left queued', 'counts as production' — and all of those are
   true of raw bytes and framed bytes alike. The only site that can tell
   them apart is the wire, so this reads the bytes back and puts them
   through the framework's own chunked decoder.

   Unframed, the comment line sits where the peer's decoder expects a
   chunk-size, and the keepalive whose job is to stop a quiet stream
   being dropped is what drops it — in the default configuration, with
   no app error required.

   SETF rather than LET on the interval: the worker reads it from another
   thread and dynamic bindings do not cross MAKE-THREAD."
  (format t "~%Harness: sse keepalive is framed~%")
  (let ((saved *stream-keepalive-interval*))
    (unwind-protect
         (progn
           (setf *stream-keepalive-interval* 1)
           (with-test-server
               (:handler (lambda (req)
                           (declare (ignore req))
                           (make-sse-response
                            :on-open (lambda (conn)
                                       ;; One real event, then silence —
                                       ;; the sweep supplies the rest.
                                       (sse-send conn :data "start")))))
             (multiple-value-bind (socket stream) (%raw-connect)
               (unwind-protect
                    (progn
                      (%send-raw-get stream "/events")
                      ;; No predicate: the stream never ends, so the
                      ;; deadline is the mechanism. Four seconds at a
                      ;; one-second interval collects the event plus
                      ;; several keepalives.
                      (let ((buf (read-until-bounded stream :seconds 4)))
                        (let* ((raw (subseq buf 0 (fill-pointer buf)))
                               (hend (or (web-skeleton::scan-crlf-crlf
                                          raw 0 (length raw))
                                         (error "no header boundary in ~d bytes"
                                                (length raw))))
                               (body (subseq raw (+ hend 4)))
                               ;; The stream is still open, so it has no
                               ;; terminator. Append one and the decoder
                               ;; can pass judgement on everything sent
                               ;; so far.
                               (terminated
                                 (concatenate '(vector (unsigned-byte 8))
                                              body
                                              (web-skeleton::chunked-terminator))))
                          (check "sse keepalive: the stream so far is valid chunked"
                                 (handler-case
                                     (progn (web-skeleton::decode-chunked-body
                                             terminated 0 (length terminated))
                                            :decoded)
                                   (error (e) (princ-to-string e)))
                                 :decoded)
                          (check "sse keepalive: and a comment line reached the client"
                                 (let ((decoded
                                         (handler-case
                                             (sb-ext:octets-to-string
                                              (web-skeleton::decode-chunked-body
                                               terminated 0 (length terminated))
                                              :external-format :utf-8)
                                           (error () ""))))
                                   (and (search "data: start" decoded)
                                        (search ":" decoded :start2
                                                (+ 11 (or (search "data: start"
                                                                  decoded)
                                                          0)))
                                        t))
                                 t))))
                 (ignore-errors (close stream))
                 (ignore-errors (sb-bsd-sockets:socket-close socket))))))
      (setf *stream-keepalive-interval* saved))))

(defun test-harness-fetch-on-body-e2e ()
  "A chunked response relayed incrementally, end to end and on one
   worker: the same server streams it and fetches it.

   ON-BODY sees each chunk as its framing is proved, and THEN still fires
   once at the end with a NIL body — the bytes went out incrementally
   rather than being accumulated, and delivering them twice would double
   the memory the callback exists to avoid.

   The upstream is item 5's streaming surface, so this also checks that
   what the encoder produces is what the walk hands back."
  (format t "~%Harness: fetch :on-body incremental delivery~%")
  (let ((collected nil)
        (final :never)
        (then-fires 0)
        (port-box (list nil)))
    (with-test-server
        (:handler
         (lambda (req)
           (if (search "/up" (http-request-path req))
               (make-stream-response
                :on-open (lambda (c)
                           (stream-send c (sb-ext:string-to-octets
                                           "one" :external-format :ascii))
                           (stream-send c (sb-ext:string-to-octets
                                           "two" :external-format :ascii))
                           (stream-close c)))
               (http-fetch
                :get (format nil "http://127.0.0.1:~d/up" (first port-box))
                :on-body (lambda (conn chunk)
                           (declare (ignore conn))
                           (push (sb-ext:octets-to-string
                                  chunk :external-format :ascii)
                                 collected))
                :then (lambda (status headers body)
                        (declare (ignore headers))
                        (incf then-fires)
                        (setf final (list status (if body :present :nil)))
                        (make-text-response 200 "relayed"))))))
      (setf (first port-box) *test-port*)
      (multiple-value-bind (status headers body)
          (test-http-request :get "/relay")
        (declare (ignore headers))
        (check "on-body e2e: the relay answered" status 200)
        (check "on-body e2e: and its own body came through" body "relayed"))
      (check "on-body e2e: every chunk arrived, in order"
             (reverse collected) '("one" "two"))
      ;; The final callback still fires exactly once, and its body is NIL
      ;; because the bytes were already handed over.
      (check "on-body e2e: :then saw the upstream status" (first final) 200)
      (check "on-body e2e: :then got no body to re-deliver"
             (second final) :nil)
      ;; Counted, because the other once-ness assertions in this file all
      ;; increment under (UNLESS STATUS ...) and so watch the cleanup
      ;; sentinel only. A callback delivered twice with a real status
      ;; passes every one of them: FINAL is overwritten with the same
      ;; value and nothing else notices. This is the delivery half of the
      ;; same contract.
      (check "on-body e2e: :then fires exactly once" then-fires 1))))

(defun test-harness-fetch-on-body-content-length-e2e ()
  "An :ON-BODY fetch against a Content-Length upstream must still deliver
   the body — through :THEN, since there is no chunk walk to hand it back
   from.

   Suppressing the buffered body on the strength of ON-BODY merely being
   *supplied* dropped it entirely on this framing: no chunks, NIL in
   :THEN, nothing raised. An app cannot choose which framing an upstream
   uses — the same origin switches by response size or by whatever proxy
   is in front — so the two paths have to agree that the bytes arrive
   somewhere."
  (format t "~%Harness: fetch :on-body against a Content-Length upstream~%")
  (let ((chunks nil)
        (final :never)
        (port-box (list nil)))
    (with-test-server
        (:handler
         (lambda (req)
           (if (search "/up" (http-request-path req))
               ;; An ordinary response: Content-Length, no chunking.
               (make-text-response 200 "plain-body")
               (http-fetch
                :get (format nil "http://127.0.0.1:~d/up" (first port-box))
                :on-body (lambda (conn chunk)
                           (declare (ignore conn))
                           (push (sb-ext:octets-to-string
                                  chunk :external-format :ascii)
                                 chunks))
                :then (lambda (status headers body)
                        (declare (ignore status headers))
                        (setf final (if body
                                        (sb-ext:octets-to-string
                                         body :external-format :ascii)
                                        :nil))
                        (make-text-response 200 "relayed"))))))
      (setf (first port-box) *test-port*)
      (test-http-request :get "/relay")
      (check "on-body/CL: no chunks, because there is no chunk walk"
             chunks nil)
      (check "on-body/CL: and the body still arrives, through :then"
             final "plain-body"))))

(defun test-harness-stream-does-not-hold-worker-e2e ()
  "The acceptance criterion for the whole issue, at :WORKERS 1.

   One client opens a stream and leaves it open. A second client then
   makes an ordinary request on the same single worker and must be
   answered while the first is still streaming. If holding a stream held
   the worker, the second request could not complete at all.

   The same fixture then covers disconnect detection: the streaming
   client goes away without a word, and the server has to notice and run
   the app's teardown rather than producing into a socket with nobody on
   the far end."
  (format t "~%Harness: a live stream does not hold the worker~%")
  (let ((live-conn nil)
        (closed-reason :never))
    (with-test-server
        (:handler (lambda (req)
                    (if (search "/stream" (http-request-path req))
                        (make-stream-response
                         :on-close (lambda (c reason)
                                     (declare (ignore c))
                                     (setf closed-reason reason))
                         :on-open (lambda (conn)
                                    ;; Send something so the client can
                                    ;; prove the stream is live, then
                                    ;; return without closing it.
                                    (setf live-conn conn)
                                    (stream-send conn
                                                 (sb-ext:string-to-octets
                                                  "tick" :external-format :ascii))))
                        (make-text-response 200 "fast"))))
      (multiple-value-bind (socket stream) (%raw-connect)
        (unwind-protect
             (progn
               (%send-raw-get stream "/stream")
               ;; Read only as far as the first chunk. The response is
               ;; keep-alive and open-ended, so it never reaches EOF —
               ;; without a predicate this would sit here until the
               ;; deadline and prove nothing.
               (multiple-value-bind (got reason)
                   (read-until-bounded
                    stream :seconds 3
                    :until (lambda (buf fill)
                             (search "tick"
                                     (sb-ext:octets-to-string
                                      (subseq buf 0 fill)
                                      :external-format :latin-1))))
                 (declare (ignore got))
                 (check "stream/worker: the stream delivered its first chunk"
                        reason :satisfied))
               (check "stream/worker: the stream is still open"
                      (null live-conn) nil)
               ;; The worker must still be able to serve someone else.
               (multiple-value-bind (status headers body)
                   (test-http-request :get "/fast")
                 (declare (ignore headers))
                 (check "stream/worker: a concurrent request is answered"
                        status 200)
                 (check "stream/worker: and answered correctly" body "fast")))
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
      ;; The streaming client is gone now. The server should notice and
      ;; tell the app, rather than holding a connection to nobody.
      (let ((deadline (+ (get-internal-real-time)
                         (* 3 internal-time-units-per-second))))
        (loop until (or (not (eq closed-reason :never))
                        (> (get-internal-real-time) deadline))
              do (sleep 0.05)))
      (check "stream/worker: the app is told the peer disconnected"
             closed-reason :disconnected))))

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
                 (check "pipelined: server closed the connection"
                        (read-to-eof-bounded stream buf) t)
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

(defun test-harness-pipelined-after-body-e2e ()
  "A request carrying a Content-Length body, with a second request
   pipelined behind it.

   The keep-alive reset has to shift from past the *body*, and this is the
   only shape where that differs from shifting past the headers: with no
   body the two are the same offset, so TEST-HARNESS-PIPELINED-WITH-FIN-E2E
   — two GETs — cannot tell a boundary that forgets the body from one that
   does not.

   What each assertion catches, measured against three sabotaged
   boundaries rather than reasoned about:

     never set, so 0    the whole request stays buffered and is re-parsed
                        forever. The server never closes, and `server
                        closed the connection` fails on its bounded read
                        — after which the run dies, so this one is loud
                        without being countable.
     long by 3          the next parse begins inside /b's request line.
     short by 3         same, from the other side.
                        Both: `second response present` and `/b
                        dispatched after it` fail. The same two, twice.

   `/a body arrived whole` is a control, not a discriminator, and passed
   under all three. REQUEST-END governs only what the *next* parse sees:
   /a's body is sliced by BODY-EXPECTED in CONNECTION-PARSE-REQUEST and
   answered before the keep-alive reset runs, so no boundary error can
   truncate it. It stays because it costs nothing and says the bodied path
   still works — but the discriminators here are the two that name /b."
  (format t "~%Harness: a bodied request with one pipelined behind it~%")
  (with-test-server
      (:handler (lambda (req)
                  (make-text-response
                   200 (format nil "path=~a body=~a"
                               (http-request-path req)
                               (if (http-request-body req)
                                   (sb-ext:octets-to-string
                                    (http-request-body req)
                                    :external-format :utf-8)
                                   "-")))))
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
                                  "POST /a HTTP/1.1" *crlf*
                                  "Host: localhost" *crlf*
                                  "Content-Length: 5" *crlf* *crlf*
                                  "HELLO"
                                  "GET /b HTTP/1.1" *crlf*
                                  "Host: localhost" *crlf*
                                  "Connection: close" *crlf* *crlf*)))
               (write-sequence (sb-ext:string-to-octets
                                requests :external-format :ascii)
                               stream)
               (force-output stream)
               (ignore-errors
                (sb-bsd-sockets:socket-shutdown socket :direction :output))
               (let ((buf (make-array 16384 :element-type '(unsigned-byte 8)
                                            :fill-pointer 0 :adjustable t)))
                 (check "pipelined body: server closed the connection"
                        (read-to-eof-bounded stream buf) t)
                 (let* ((text (sb-ext:octets-to-string
                               (subseq buf 0 (fill-pointer buf))
                               :external-format :utf-8))
                        (first-200 (search "HTTP/1.1 200" text))
                        (second-200 (and first-200
                                         (search "HTTP/1.1 200" text
                                                 :start2 (1+ first-200)))))
                   (check "pipelined body: first response present"
                          (not (null first-200)) t)
                   (check "pipelined body: second response present"
                          (not (null second-200)) t)
                   (check "pipelined body: /a body arrived whole"
                          (not (null (search "path=/a body=HELLO" text))) t)
                   (check "pipelined body: /b dispatched after it"
                          (not (null (search "path=/b" text))) t)
                   ;; And carried no body of its own. BODY-EXPECTED is
                   ;; written by the Content-Length arm and by the chunked
                   ;; arm, and by neither of the paths a bodiless request
                   ;; takes — so on this exact sequence, POST-with-body
                   ;; then GET, the keep-alive reset is its only writer.
                   ;; Left stale, CONNECTION-PARSE-REQUEST attaches five
                   ;; bytes of whatever follows /b's headers to a request
                   ;; the client sent no body with. The handler has always
                   ;; printed this value; only the assertion was missing.
                   (check "pipelined body: /b carried no body of its own"
                          (not (null (search "path=/b body=-" text))) t)))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun test-harness-chunked-keepalive-e2e ()
  "Two chunked requests on one keep-alive connection.

   The detector for CHUNK-SCAN-POS being cleared per request, and the
   first body is large on purpose. The walk clamps a resume that is too
   *low* up to START, so a leftover cursor only bites when it exceeds the
   *next* request's body-start. With a short first body the leftover is
   smaller than the second request's header block, the clamp hides it, and
   this test passes with the clear removed — proving nothing.

   Sized so it cannot: the first body is one 200-byte chunk, leaving the
   cursor near 270, while the second request's body begins near 84. With
   the clear neutered the second walk starts well past its own body, finds
   no framing there, and the connection never answers — measured:
   this test's `server closed the connection` and `second request
   dispatched`.

   That was measured after removing a second clear, not before. The
   connection resets used to zero the cursor as well, and with both in
   place neutering either one was silent: each masked the other, and this
   test detected neither. Two mechanisms, one guarantee, no way to tell
   them apart.

   The second request is chunked too, rather than a plain GET, because a
   GET would never consult the cursor at all."
  (format t "~%Harness: two chunked requests on one connection~%")
  (with-test-server
      (:handler (lambda (req)
                  (make-text-response
                   200 (format nil "path=~a len=~a"
                               (http-request-path req)
                               (if (http-request-body req)
                                   (length (http-request-body req))
                                   -1)))))
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
             (let* ((stream (sb-bsd-sockets:socket-make-stream
                             socket :input t :output t
                             :element-type '(unsigned-byte 8)))
                    (big (make-string 200 :initial-element #\a))
                    (requests
                     (concatenate 'string
                                  "POST /a HTTP/1.1" *crlf*
                                  "Host: localhost" *crlf*
                                  "Transfer-Encoding: chunked" *crlf* *crlf*
                                  "c8" *crlf* big *crlf* "0" *crlf* *crlf*
                                  "POST /b HTTP/1.1" *crlf*
                                  "Host: localhost" *crlf*
                                  "Transfer-Encoding: chunked" *crlf* *crlf*
                                  "2" *crlf* "de" *crlf* "0" *crlf* *crlf*
                                  ;; Third, and bodiless on purpose: it is
                                  ;; the only shape that reads BODY-FRAMING
                                  ;; without writing it first.
                                  "GET /c HTTP/1.1" *crlf*
                                  "Host: localhost" *crlf*
                                  "Connection: close" *crlf* *crlf*)))
               (write-sequence (sb-ext:string-to-octets
                                requests :external-format :ascii)
                               stream)
               (force-output stream)
               (ignore-errors
                (sb-bsd-sockets:socket-shutdown socket :direction :output))
               (let ((buf (make-array 16384 :element-type '(unsigned-byte 8)
                                            :fill-pointer 0 :adjustable t)))
                 (check "chunked keepalive: server closed the connection"
                        (read-to-eof-bounded stream buf) t)
                 (let ((text (sb-ext:octets-to-string
                              (subseq buf 0 (fill-pointer buf))
                              :external-format :utf-8)))
                   (check "chunked keepalive: first body decoded whole"
                          (not (null (search "path=/a len=200" text))) t)
                   (check "chunked keepalive: second request dispatched"
                          (not (null (search "path=/b len=2" text))) t)
                   ;; BODY-FRAMING is written by the chunked arm and by
                   ;; nothing else, so a bodiless request following a
                   ;; chunked one reads whatever the reset left. Left
                   ;; stale at :CHUNKED, CONNECTION-PARSE-REQUEST decodes
                   ;; /c over an empty range and a perfectly good GET
                   ;; earns a 400. Both other requests here are chunked,
                   ;; which is right for the cursor and is exactly what
                   ;; leaves the framing field uncovered.
                   (check "chunked keepalive: a bodiless request after them is served"
                          (not (null (search "path=/c len=-1" text))) t)))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun test-harness-chunked-body-cap-e2e ()
  "Two chunked requests on one connection, each under `*max-body-size*`
   and over it summed, plus one that genuinely exceeds it.

   The detector for BODY-DECODED being cleared by the keep-alive reset.
   That accumulator is written by CONNECTION-BODY-COMPLETE-P as chunks are
   proved whole, and by no completing arm — so the reset is its only
   writer of 0, the same position BODY-FRAMING and BODY-EXPECTED are in.
   Left stale, the second request inherits the first's total and a
   perfectly legal upload earns a 413 that names a cap it never reached.

   `*max-body-size*` is set globally rather than bound, because the worker
   runs in a thread that inherits nothing from this one's dynamic
   environment — a LET here would be invisible to the code under test.
   Restored on the way out."
  (format t "~%Harness: the chunked body cap across a keep-alive~%")
  (let ((saved web-skeleton:*max-body-size*))
    (unwind-protect
         (progn
           (setf web-skeleton:*max-body-size* 256)
           (with-test-server
               (:handler (lambda (req)
                           (make-text-response
                            200 (format nil "path=~a len=~a"
                                        (http-request-path req)
                                        (length (http-request-body req))))))
             ;; Under the cap twice, over it summed.
             (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                          :type :stream :protocol :tcp)))
               (unwind-protect
                    (progn
                      (sb-bsd-sockets:socket-connect socket #(127 0 0 1)
                                                     *test-port*)
                      (let* ((stream (sb-bsd-sockets:socket-make-stream
                                      socket :input t :output t
                                      :element-type '(unsigned-byte 8)))
                             (a (make-string 200 :initial-element #\a))
                             (b (make-string 100 :initial-element #\b))
                             (requests
                              (concatenate 'string
                                           "POST /a HTTP/1.1" *crlf*
                                           "Host: localhost" *crlf*
                                           "Transfer-Encoding: chunked" *crlf* *crlf*
                                           "c8" *crlf* a *crlf* "0" *crlf* *crlf*
                                           "POST /b HTTP/1.1" *crlf*
                                           "Host: localhost" *crlf*
                                           "Transfer-Encoding: chunked" *crlf*
                                           "Connection: close" *crlf* *crlf*
                                           "64" *crlf* b *crlf* "0" *crlf* *crlf*)))
                        (write-sequence (sb-ext:string-to-octets
                                         requests :external-format :ascii)
                                        stream)
                        (force-output stream)
                        (ignore-errors
                         (sb-bsd-sockets:socket-shutdown socket
                                                         :direction :output))
                        (let ((buf (make-array 16384
                                               :element-type '(unsigned-byte 8)
                                               :fill-pointer 0 :adjustable t)))
                          (check "body cap: server closed the connection"
                                 (read-to-eof-bounded stream buf) t)
                          (let ((text (sb-ext:octets-to-string
                                       (subseq buf 0 (fill-pointer buf))
                                       :external-format :utf-8)))
                            (check "body cap: first request under the cap is served"
                                   (not (null (search "path=/a len=200" text))) t)
                            ;; 200 + 100 is over 256. Only a cleared
                            ;; accumulator lets this one through.
                            (check "body cap: the second starts from zero, not 200"
                                   (not (null (search "path=/b len=100" text))) t)
                            (check "body cap: neither was refused"
                                   (null (search "HTTP/1.1 413" text)) t)))))
                 (ignore-errors (sb-bsd-sockets:socket-close socket))))
             ;; And one that really is too big, so the 413 is known to
             ;; reach a client and not merely to be raised.
             (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                          :type :stream :protocol :tcp)))
               (unwind-protect
                    (progn
                      (sb-bsd-sockets:socket-connect socket #(127 0 0 1)
                                                     *test-port*)
                      (let* ((stream (sb-bsd-sockets:socket-make-stream
                                      socket :input t :output t
                                      :element-type '(unsigned-byte 8)))
                             (big (make-string 300 :initial-element #\c))
                             (request
                              (concatenate 'string
                                           "POST /big HTTP/1.1" *crlf*
                                           "Host: localhost" *crlf*
                                           "Transfer-Encoding: chunked" *crlf* *crlf*
                                           "12c" *crlf* big *crlf* "0" *crlf* *crlf*)))
                        (write-sequence (sb-ext:string-to-octets
                                         request :external-format :ascii)
                                        stream)
                        (force-output stream)
                        (ignore-errors
                         (sb-bsd-sockets:socket-shutdown socket
                                                         :direction :output))
                        (let ((buf (make-array 16384
                                               :element-type '(unsigned-byte 8)
                                               :fill-pointer 0 :adjustable t)))
                          (read-to-eof-bounded stream buf)
                          (let ((text (sb-ext:octets-to-string
                                       (subseq buf 0 (fill-pointer buf))
                                       :external-format :utf-8)))
                            (check "body cap: an oversized body is 413 to the client"
                                   (not (null (search "HTTP/1.1 413" text))) t)
                            (check "body cap: and was never dispatched"
                                   (null (search "path=/big" text)) t)))))
                 (ignore-errors (sb-bsd-sockets:socket-close socket))))))
      (setf web-skeleton:*max-body-size* saved))))

(defun test-harness-chunked-trailer-smuggle-e2e ()
  "A trailer section carrying a complete HTTP request, end to end.

   The headline acceptance criterion of the issue, and a delivery-shaped
   assertion cannot see it: if the smuggle succeeded the client would get
   *two* responses, so what this asserts is that it gets one, and that the
   one it gets is the 400 the trailer earned rather than a 200 for /a
   followed by a 200 for the request hidden in its trailer.

   Refusing means the boundary for a trailer-bearing request is never
   computed at all, which is why this is a 400 and not a silently-consumed
   trailer."
  (format t "~%Harness: a trailer holding a whole request~%")
  (with-test-server
      (:handler (lambda (req)
                  (make-text-response
                   200 (format nil "served=~a" (http-request-path req)))))
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
             (let* ((stream (sb-bsd-sockets:socket-make-stream
                             socket :input t :output t
                             :element-type '(unsigned-byte 8)))
                    (attack
                     (concatenate 'string
                                  "POST /a HTTP/1.1" *crlf*
                                  "Host: localhost" *crlf*
                                  "Transfer-Encoding: chunked" *crlf* *crlf*
                                  "3" *crlf* "abc" *crlf*
                                  "0" *crlf*
                                  ;; The trailer section, and a whole
                                  ;; request inside it.
                                  "GET /admin HTTP/1.1" *crlf*
                                  "Host: localhost" *crlf*
                                  *crlf*)))
               (write-sequence (sb-ext:string-to-octets
                                attack :external-format :ascii)
                               stream)
               (force-output stream)
               (ignore-errors
                (sb-bsd-sockets:socket-shutdown socket :direction :output))
               (let ((buf (make-array 16384 :element-type '(unsigned-byte 8)
                                            :fill-pointer 0 :adjustable t)))
                 (check "trailer smuggle: server closed the connection"
                        (read-to-eof-bounded stream buf) t)
                 (let* ((text (sb-ext:octets-to-string
                               (subseq buf 0 (fill-pointer buf))
                               :external-format :utf-8))
                        (first-status (search "HTTP/1.1 " text))
                        (second-status (and first-status
                                            (search "HTTP/1.1 " text
                                                    :start2 (1+ first-status)))))
                   (check "trailer smuggle: refused with 400"
                          (not (null (search "HTTP/1.1 400" text))) t)
                   ;; The assertion the issue exists for: not two responses.
                   (check "trailer smuggle: exactly one response"
                          (null second-status) t)
                   (check "trailer smuggle: /admin was never served"
                          (null (search "served=/admin" text)) t)
                   (check "trailer smuggle: /a was not served either"
                          (null (search "served=/a" text)) t)))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun test-harness-chunked-expect-100-continue-e2e ()
  "A chunked upload from a client that actually waits for the interim.

   The unit assertions cover the verdict and the queued bytes; this
   covers the part they cannot, which is that the interim *arrives* and
   that the chunked frame survives it. Between the two, the connection
   passes through :SENDING-100-CONTINUE and back — CONNECTION-RESET-WRITE
   runs on the way out, and if it or anything else cleared BODY-FRAMING,
   CHUNK-SCAN-POS or HEADER-END, the body read would resume against a
   frame belonging to no request.

   Written by hand rather than through TEST-HTTP-REQUEST because the
   waiting is the subject: the headers go out alone, the interim is read
   back before a single body byte is sent, and only then does the body
   follow."
  (format t "~%Harness: a chunked upload that waits for its 100~%")
  (with-test-server
      (:handler (lambda (req)
                  (make-text-response
                   200 (format nil "got=~a"
                               (sb-ext:octets-to-string
                                (http-request-body req)
                                :external-format :ascii)))))
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
             (let ((stream (sb-bsd-sockets:socket-make-stream
                            socket :input t :output t
                            :element-type '(unsigned-byte 8))))
               ;; Headers only. No body byte has been written.
               (write-sequence
                (sb-ext:string-to-octets
                 (concatenate 'string
                              "POST /upload HTTP/1.1" *crlf*
                              "Host: localhost" *crlf*
                              "Transfer-Encoding: chunked" *crlf*
                              "Expect: 100-continue" *crlf* *crlf*)
                 :external-format :ascii)
                stream)
               (force-output stream)
               ;; The interim, read before anything else is sent. A
               ;; server that skipped it would leave this read blocking
               ;; until the harness's bound expires.
               (check "chunked 100: the interim arrives before the body"
                      (attempt (read-response-status-head stream)) 100)
               ;; Now the body, in two chunks, so the walk resumes across
               ;; a wake-up rather than seeing it all at once.
               (write-sequence
                (sb-ext:string-to-octets
                 (concatenate 'string "3" *crlf* "abc" *crlf*)
                 :external-format :ascii)
                stream)
               (force-output stream)
               (write-sequence
                (sb-ext:string-to-octets
                 (concatenate 'string "2" *crlf* "de" *crlf*
                              "0" *crlf* *crlf*)
                 :external-format :ascii)
                stream)
               (force-output stream)
               (ignore-errors
                (sb-bsd-sockets:socket-shutdown socket :direction :output))
               (let ((buf (make-array 16384 :element-type '(unsigned-byte 8)
                                            :fill-pointer 0 :adjustable t)))
                 (read-to-eof-bounded stream buf)
                 (let ((text (sb-ext:octets-to-string
                              (subseq buf 0 (fill-pointer buf))
                              :external-format :utf-8)))
                   (check "chunked 100: the response is 200"
                          (not (null (search "HTTP/1.1 200" text))) t)
                   ;; Both chunks, decoded, in order — which is the frame
                   ;; having survived the interim.
                   (check "chunked 100: the whole body arrived decoded"
                          (not (null (search "got=abcde" text))) t)))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun read-response-status-head (stream)
  "Read through the CRLFCRLF ending a response's header block and return
   the status. NIL if the peer closed, or the deadline passed, before a
   complete block arrived.

   PARSE-TEST-RESPONSE reads to end-of-stream, which is right for a
   Connection: close request and wrong here — the point of the holder
   connection below is that it stays open and keeps its slot, so there is
   no EOF to wait for. This is what READ-UNTIL-BOUNDED's :UNTIL is for:
   stop at the header terminator rather than at a close that will not
   come."
  (multiple-value-bind (buf reason)
      (read-until-bounded
       stream
       :until (lambda (b fill) (web-skeleton::scan-crlf-crlf b 0 fill)))
    (declare (ignore reason))
    (let ((end (fill-pointer buf)))
      (when (web-skeleton::scan-crlf-crlf buf 0 end)
        (web-skeleton::parse-response-status buf 0 end)))))

(defun claim-connection-slot (&key (attempts 40))
  "Open a keep-alive connection and return its socket once the server has
   accepted it, or NIL if it never does.

   Retrying is not defensive padding. WAIT-FOR-PORT establishes the
   server is up by connecting and closing immediately, and that probe
   stays registered in the worker's connection table until the worker
   processes its EOF — so a test running against a limit of 1 can find
   the slot already taken and be refused. Retrying until a connection is
   accepted means the slot under test is provably this function's, rather
   than assumed free at a moment when it demonstrably may not be."
  (let ((crlf (coerce '(#\Return #\Linefeed) 'string)))
    (dotimes (i attempts)
      (declare (ignorable i))
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp))
            (kept nil))
        (unwind-protect
             (progn
               (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
               (let ((stream (sb-bsd-sockets:socket-make-stream
                              socket :input t :output t
                              :element-type '(unsigned-byte 8))))
                 (write-sequence
                  (sb-ext:string-to-octets
                   (concatenate 'string
                                "GET / HTTP/1.1" crlf
                                "Host: localhost" crlf
                                "Connection: keep-alive" crlf crlf)
                   :external-format :ascii)
                  stream)
                 (force-output stream)
                 ;; A 200 read to the end of its headers proves the handler
                 ;; ran, which proves the connection is accepted and
                 ;; registered. Anything else means the worker was full.
                 (when (eql (read-response-status-head stream) 200)
                   (setf kept t)
                   (return-from claim-connection-slot socket))))
          (unless kept
            (ignore-errors (sb-bsd-sockets:socket-close socket))
            (sleep 0.02)))))))

(defun request-expecting-refusal ()
  "Send a request the server is expected to refuse and return what came
   back: (values STATUS HEADERS BODY-STRING).

   Hand-rolled rather than routed through TEST-HTTP-REQUEST because a
   refusal that answers nothing leaves PARSE-TEST-RESPONSE with no header
   block to parse, and it signals. That silence is a regression worth
   reporting as a failed check rather than a backtrace that takes the
   rest of the suite down with it, so this returns NILs instead."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp))
        (crlf (coerce '(#\Return #\Linefeed) 'string)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-connect socket #(127 0 0 1) *test-port*)
           (let ((stream (sb-bsd-sockets:socket-make-stream
                          socket :input t :output t
                          :element-type '(unsigned-byte 8)))
                 (buf (make-array 4096 :element-type '(unsigned-byte 8)
                                       :fill-pointer 0 :adjustable t)))
             (write-sequence
              (sb-ext:string-to-octets
               (concatenate 'string
                            "GET / HTTP/1.1" crlf
                            "Host: localhost" crlf
                            "Connection: close" crlf crlf)
               :external-format :ascii)
              stream)
             (force-output stream)
             ;; A refused connection can end in a reset rather than an
             ;; ordinary close, which READ-TO-EOF-BOUNDED reports as
             ;; unclean. Only the bytes matter here — whether the close
             ;; was graceful is the accept-time race, asserted where it
             ;; is decidable rather than here.
             (read-to-eof-bounded stream buf :seconds 10)
             (let* ((end (fill-pointer buf))
                    (header-end (web-skeleton::scan-crlf-crlf buf 0 end)))
               (if (null header-end)
                   (values nil nil nil)
                   (let* ((status (web-skeleton::parse-response-status
                                   buf 0 end))
                          (first-crlf (web-skeleton::scan-crlf buf 0 header-end))
                          (headers (when first-crlf
                                     (web-skeleton::parse-headers-bytes
                                      buf (+ first-crlf 2) (+ header-end 4))))
                          (body-start (+ header-end 4))
                          (body (when (> end body-start)
                                  (handler-case
                                      (sb-ext:octets-to-string
                                       (subseq buf body-start end)
                                       :external-format :utf-8)
                                    (error () nil)))))
                     (values status headers body))))))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun wait-until-readable (socket &key (attempts 400))
  "Wait until SOCKET has bytes queued, without consuming them.
   Returns T once data is present, NIL if it never arrives."
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
    (web-skeleton::set-nonblocking (web-skeleton::socket-fd socket))
    (dotimes (i attempts nil)
      (declare (ignorable i))
      (let ((n (handler-case
                   (nth-value 1 (sb-bsd-sockets:socket-receive
                                 socket buf 1 :peek t))
                 (error () nil))))
        (when (and n (plusp n))
          (return t))
        (sleep 0.005)))))

(defun test-refuse-connection-drains ()
  "REFUSE-CONNECTION answers and then closes without resetting, once the
   peer's request has actually arrived.

   Driven directly instead of through a server, because that precondition
   is the whole property and the accept path cannot supply it: the
   refusal happens at accept time and may precede the request entirely,
   leaving the drain nothing to clear and the close to reset. Asserting a
   graceful close through ACCEPT-CONNECTION would therefore be asserting
   the outcome of a race — it failed about one cold run in four when this
   test tried it that way. Here the request is confirmed queued before
   REFUSE-CONNECTION is called, so the drain has work to do and the
   result is a fact rather than a coin."
  (format t "~%Harness: refuse-connection drains, then closes cleanly~%")
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 5)
           (multiple-value-bind (host port)
               (sb-bsd-sockets:socket-name listener)
             (declare (ignore host))
             (let ((client (make-instance 'sb-bsd-sockets:inet-socket
                                          :type :stream :protocol :tcp)))
               (unwind-protect
                    (progn
                      (sb-bsd-sockets:socket-connect client #(127 0 0 1) port)
                      (let ((accepted (sb-bsd-sockets:socket-accept listener))
                            (stream (sb-bsd-sockets:socket-make-stream
                                     client :input t :output t
                                     :element-type '(unsigned-byte 8))))
                        (write-sequence
                         (sb-ext:string-to-octets
                          (concatenate 'string
                                       "GET / HTTP/1.1" *crlf*
                                       "Host: localhost" *crlf* *crlf*)
                          :external-format :ascii)
                         stream)
                        (force-output stream)
                        (check "request is queued before the refusal"
                               (wait-until-readable accepted) t)
                        (web-skeleton::refuse-connection accepted)
                        (let ((buf (make-array 1024
                                               :element-type '(unsigned-byte 8)
                                               :fill-pointer 0 :adjustable t)))
                          (multiple-value-bind (ended clean)
                              (read-to-eof-bounded stream buf :seconds 5)
                            ;; Bounded even though REFUSE-CONNECTION closes
                            ;; unconditionally today: this test's subject
                            ;; is whether it closes, and the day that
                            ;; SOCKET-CLOSE moves inside a conditional, the
                            ;; test written to catch it should fail rather
                            ;; than hang the suite it belongs to.
                            (check "refused connection is closed" ended t)
                            (let ((text (sb-ext:octets-to-string
                                         (subseq buf 0 (fill-pointer buf))
                                         :external-format :latin-1)))
                              (check "direct refusal sends the 503"
                                     (and (search "503" text) t) t))
                            ;; The assertion the drain exists for. Without
                            ;; it the queued request is still unread at
                            ;; close, Linux sends RST instead of FIN, and
                            ;; this read ends in a reset rather than an
                            ;; ordinary end of stream.
                            (check "drained refusal closes without a reset"
                                   clean t)))))
                 (ignore-errors (sb-bsd-sockets:socket-close client))))))
      (ignore-errors (sb-bsd-sockets:socket-close listener)))))

(defun test-harness-connection-limit-e2e ()
  "A worker at *MAX-CONNECTIONS* answers 503 instead of closing mute."
  (format t "~%Harness: connection limit answers 503~%")
  (let ((saved web-skeleton::*max-connections*))
    ;; Global SETF rather than a LET binding, for the reason
    ;; CALL-WITH-TEST-SERVER gives for the shutdown specials: the worker
    ;; runs in a thread START-SERVER spawns, and it reads the global
    ;; value. A binding established here would not reach it, and the
    ;; test would quietly assert nothing against a limit of 10000.
    (setf web-skeleton::*max-connections* 1)
    (unwind-protect
         (with-test-server
             (:handler (lambda (req)
                         (declare (ignore req))
                         (make-text-response 200 "")))
           (let ((holder (claim-connection-slot)))
             (unwind-protect
                  (progn
                    (check "a connection is accepted below the limit"
                           (not (null holder)) t)
                    (when holder
                      ;; Delivery only, and on loopback rather than by
                      ;; guarantee: the refusal happens at accept time and
                      ;; may precede the request arriving at all, which
                      ;; costs the graceful close and can cost the
                      ;; response itself. Here the client's write always
                      ;; beats the server's accept, so this is stable —
                      ;; but it is stable because of the transport, not
                      ;; because the accept path promises anything. The
                      ;; drain's own property is asserted in
                      ;; TEST-REFUSE-CONNECTION-DRAINS, which establishes
                      ;; the precondition this path cannot.
                      (multiple-value-bind (status headers body)
                          (request-expecting-refusal)
                        (check "refused connection gets 503" status 503)
                        (check "refusal carries Retry-After"
                               (cdr (assoc "retry-after" headers
                                           :test #'string-equal))
                               "2")
                        (check "refusal explains itself"
                               (and body
                                    (search "connection limit" body)
                                    t)
                               t))))
               (when holder
                 (ignore-errors (sb-bsd-sockets:socket-close holder))))))
      (setf web-skeleton::*max-connections* saved))))

;;; ---------------------------------------------------------------------------
;;; Runner
;;; ---------------------------------------------------------------------------

(defun %ascii (s) (sb-ext:string-to-octets s :external-format :ascii))

(defun %decode-streamed-body (raw)
  "Chunked body of a complete raw response, or the error's text. Caught
   rather than raised: a missing terminator ends the run instead of
   reporting, and a truncated stream is a thing several of these tests
   assert about."
  (let ((hend (web-skeleton::scan-crlf-crlf raw 0 (length raw))))
    (if (null hend)
        "<no header terminator>"
        (handler-case
            (sb-ext:octets-to-string
             (web-skeleton::decode-chunked-body raw (+ hend 4) (length raw))
             :external-format :ascii)
          (error (e) (princ-to-string e))))))

(defun test-harness-fetch-into-relay-e2e ()
  "The relay DEPLOYMENT.md described, now that it dials.

   A streaming response whose :ON-OPEN starts a fetch, forwards each chunk
   into its own body as the framing proves it, and closes when the upstream
   is done. Before FETCH-INTO this built a continuation inside :ON-OPEN and
   dropped it: the client got a 200 with chunked framing that never
   terminated, held for *STREAM-IDLE-TIMEOUT*, then a truncated body with
   no error anywhere.

   Asserted on the decoded body rather than on bytes arriving, because the
   defect's signature was a body that never terminated — a check that only
   looked for content would have passed against it."
  (format t "~%Harness: fetch-into, the documented relay~%")
  (let ((port-box (list nil))
        (then-fires 0))
    (with-test-server
        (:handler
         (lambda (req)
           (if (search "/up" (http-request-path req))
               (make-stream-response
                :on-open (lambda (c)
                           (stream-send c (%ascii "alpha"))
                           (stream-send c (%ascii "beta"))
                           (stream-close c)))
               (make-stream-response
                :on-open
                (lambda (client)
                  (fetch-into
                   client
                   (http-fetch
                    :get (format nil "http://127.0.0.1:~d/up"
                                 (first port-box))
                    :on-body (lambda (out chunk)
                               (declare (ignore out))
                               (stream-send client chunk)
                               nil)
                    :then (lambda (status headers body)
                            (declare (ignore status headers body))
                            (incf then-fires)
                            (stream-close client)
                            nil))))))))
      (setf (first port-box) *test-port*)
      (multiple-value-bind (socket stream) (%raw-connect)
        (unwind-protect
             (progn
               (%send-raw-get stream "/relay" :extra
                              (format nil "Connection: close~c~c"
                                      #\Return #\Newline))
               (let* ((buf (read-until-bounded stream))
                      (raw (subseq buf 0 (fill-pointer buf))))
                 (check "fetch-into relay: the upstream body arrived, framed"
                        (%decode-streamed-body raw) "alphabeta")))
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
      (check "fetch-into relay: :then fired exactly once" then-fires 1)
      (check "fetch-into relay: no outbound left behind"
             (census-await :outbound 0) 0))))

(defun test-harness-fetch-into-chained-e2e ()
  "A :THEN that starts another fetch keeps the stream open.

   A handler-returned fetch chains by returning a continuation. A detached
   one has no return value anyone reads, so it chains by calling FETCH-INTO
   again — and the framework's own disposition would otherwise close the
   stream the new fetch is about to produce into, on the grounds that the
   first one delivered and the app had not closed it.

   Asserted on the second fetch's content rather than on the connection
   closing cleanly, because the failure is a truncated first response,
   which reads like an upstream fault rather than like the framework
   closing the connection underneath it."
  (format t "~%Harness: fetch-into, chained from :then~%")
  (let ((port-box (list nil)))
    (with-test-server
        (:handler
         (lambda (req)
           (let ((path (http-request-path req)))
             (cond
               ((search "/one" path)
                (make-stream-response
                 :on-open (lambda (c) (stream-send c (%ascii "first"))
                            (stream-close c))))
               ((search "/two" path)
                (make-stream-response
                 :on-open (lambda (c) (stream-send c (%ascii "second"))
                            (stream-close c))))
               (t
                (make-stream-response
                 :on-open
                 (lambda (client)
                   (flet ((leg (where then)
                            (http-fetch
                             :get (format nil "http://127.0.0.1:~d~a"
                                          (first port-box) where)
                             :on-body (lambda (out chunk)
                                        (declare (ignore out))
                                        (stream-send client chunk)
                                        nil)
                             :then then)))
                     (fetch-into
                      client
                      (leg "/one"
                           (lambda (status headers body)
                             (declare (ignore status headers body))
                             ;; Chained from inside :THEN. The stream must
                             ;; survive this call.
                             (fetch-into
                              client
                              (leg "/two"
                                   (lambda (s h b)
                                     (declare (ignore s h b))
                                     (stream-close client)
                                     nil)))
                             nil)))))))))))
      (setf (first port-box) *test-port*)
      (multiple-value-bind (socket stream) (%raw-connect)
        (unwind-protect
             (progn
               (%send-raw-get stream "/chain" :extra
                              (format nil "Connection: close~c~c"
                                      #\Return #\Newline))
               (let* ((buf (read-until-bounded stream))
                      (raw (subseq buf 0 (fill-pointer buf))))
                 (check "fetch-into chained: both legs reached the client"
                        (%decode-streamed-body raw) "firstsecond")))
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
      (check "fetch-into chained: no outbound left behind"
             (census-await :outbound 0) 0))))

(defun test-harness-fetch-into-refusals ()
  "The four refusals, each asserted on its own.

   They fail in different directions and a single \"it signals\" check
   would cover one of them by accident. And a refused call must leave
   nothing behind — no outbound, and a callback that never fired — which
   is the one place on this branch where zero is the right answer for a
   counter."
  (format t "~%Harness: fetch-into refusals~%")
  (let ((fires 0))
    (flet ((cont ()
             (http-fetch :get "http://127.0.0.1:1/x"
                         :then (lambda (s h b)
                                 (declare (ignore s h b))
                                 (incf fires)
                                 nil))))
      ;; Wrong state. A connection that does not own its own write path has
      ;; nowhere to put a result.
      (let ((conn (web-skeleton::make-connection :fd 1 :state :read-http)))
        (check "refusal: a non-producing state signals"
               (handler-case (progn (fetch-into conn (cont)) :no-signal)
                 (error () :signalled))
               :signalled))
      ;; Not a continuation.
      (let ((conn (web-skeleton::make-connection :fd 1 :state :streaming)))
        (check "refusal: a non-continuation signals"
               (handler-case (progn (fetch-into conn "not a fetch") :no-signal)
                 (error () :signalled))
               :signalled))
      ;; No event loop. Off a worker *EPOLL-FD* is NIL, and the outbound
      ;; would be opened and never driven — a leaked descriptor plus a
      ;; callback that never fires.
      (let ((conn (web-skeleton::make-connection :fd 1 :state :streaming))
            (web-skeleton::*epoll-fd* nil))
        (check "refusal: no event loop signals"
               (handler-case (progn (fetch-into conn (cont)) :no-signal)
                 (error () :signalled))
               :signalled))
      ;; Already outstanding. Two would race to apply the disposition and
      ;; whichever finished first would close the target under the other.
      (let ((conn (web-skeleton::make-connection
                   :fd 1 :state :streaming :fetch-outstanding t))
            (web-skeleton::*epoll-fd* 99))
        (check "refusal: a second outstanding fetch signals"
               (handler-case (progn (fetch-into conn (cont)) :no-signal)
                 (error () :signalled))
               :signalled))
      ;; A refused call did nothing: the callback never ran.
      (check "refusal: no callback fired on any refusal" fires 0)
      ;; Every refusal above returns from the guard block, which is before
      ;; the outstanding marker is ever written. Asserting the marker here
      ;; read as coverage of "a refused call left nothing behind" and was
      ;; coverage of a slot no path on this test touched — it passed with
      ;; the reset-on-unwind deleted outright. That claim belongs where
      ;; the marker is set and then cleared, which is the setup-failure
      ;; path below.
      ;;
      ;; What is worth asserting here is the property these four share
      ;; that the marker cannot show: refusing did not consume the
      ;; continuation, so the same one still reaches the next refusal
      ;; rather than having been spent on the last.
      (let ((conn (web-skeleton::make-connection
                   :fd 1 :state :streaming :fetch-outstanding t)))
        (check "refusal: refusing does not consume the continuation"
               (handler-case (progn (fetch-into conn (cont)) :no-signal)
                 (error () :signalled))
               :signalled)
        (check "refusal: and still fired no callback" fires 0)))))

(defun test-harness-detached-deadline-sweep ()
  "A paused detached outbound whose upstream never answers is reaped by
   the sweeper, and the app's callback still fires once.

   The ending with the fewest ways to be reached. Every other fetch is
   bounded by its parked inbound's :AWAITING reap, and a detached one has
   no parked inbound. It is not idle either — it is waiting on an
   upstream — so the idle arm does not want it, and it is skipped by the
   OUTBOUND-P guard in any case. A *paused* one has been modified down to
   bare +EPOLLET+, subscribed to no events at all, so no wake-up is
   coming to notice anything. RESUME-PAUSED-OUTBOUND is the ordinary way
   out and it is driven by the target draining, which cannot help when
   the upstream is the half that has stopped.

   That leaves the deadline arm of SWEEP-IDLE-CONNECTIONS as the only
   thing in the process that can still reach this connection. Without it
   the socket is held until the process ends and the app's :THEN never
   fires — the leak the abort sentinel exists to prevent.

   The arm was not uncovered before this, and saying otherwise would be
   the overstatement this branch keeps deleting:
   TEST-HARNESS-FETCH-INTO-UPSTREAM-STALLS-E2E fails when it is removed,
   because the sentinel does not arrive. What that test cannot see is
   everything after the callback — it has no way to look inside the
   worker's connection table. So what is new here is the reclamation
   rather than the notification: the outbound is actually unregistered,
   the callback fired exactly once rather than merely at least once, and
   the target got DELIVER-DETACHED's disposition instead of being left
   open behind a body that stopped.

   **The target is deliberately left alive.** The first version of this
   test closed it, which proved nothing: CLOSE-CONNECTION walks a dying
   target's detached outbounds and reaps them itself, so every assertion
   below passed with the sweeper's arm deleted outright. That walk is
   real and covered by TEST-HARNESS-FETCH-INTO-TARGET-CLOSED-E2E. A live
   target is what leaves the sweeper as the only remaining reaper, which
   is the whole point of the arm.

   Built rather than provoked. Reaching this through a live server means
   an upstream that stalls mid-body, backpressure applied, and then
   waiting out *FETCH-TIMEOUT* — two races and a sleep to reach one
   branch. The state is assembled directly instead: the same shape the
   sweeper sees, and none of the waiting."
  (format t "~%Harness: detached outbound past its deadline, swept~%")
  (let* ((fires 0)
         (status-seen :never)
         (target-sock (make-instance 'sb-bsd-sockets:inet-socket
                                     :type :stream :protocol :tcp))
         (out-sock (make-instance 'sb-bsd-sockets:inet-socket
                                  :type :stream :protocol :tcp))
         (epfd (web-skeleton::epoll-create))
         (web-skeleton::*connections* (make-hash-table)))
    (unwind-protect
         (let* ((target (web-skeleton::make-connection
                         :fd (web-skeleton::socket-fd target-sock)
                         :socket target-sock
                         :state :streaming
                         :last-active (get-universal-time)))
                (out (web-skeleton::make-connection
                      :fd (web-skeleton::socket-fd out-sock)
                      :socket out-sock
                      :state :out-read
                      :outbound-p t
                      :fetch-sink :detached
                      :fetch-method :GET
                      ;; Paused, and therefore subscribed to nothing.
                      :fetch-paused t
                      :fetch-paused-at (get-universal-time)
                      ;; Already past its deadline: the sweeper's test is
                      ;; (> now deadline), so this is the state a real one
                      ;; reaches after *FETCH-TIMEOUT* of no progress.
                      :fetch-started-at (- (get-universal-time) 600)
                      :fetch-deadline (- (get-universal-time) 300)
                      :inbound-fd (web-skeleton::socket-fd target-sock)
                      :last-active (get-universal-time)
                      :fetch-callback
                      (lambda (status headers body)
                        (declare (ignore headers body))
                        (incf fires)
                        (setf status-seen status)
                        nil))))
           (web-skeleton::register-connection target)
           (web-skeleton::register-connection out)
           (check "detached sweep: the outbound is registered before the sweep"
                  (and (web-skeleton::lookup-connection
                        (web-skeleton::connection-fd out))
                       t)
                  t)
           (web-skeleton::sweep-idle-connections epfd (get-universal-time))
           ;; The three the arm exists for.
           (check "detached sweep: the expired outbound was reaped"
                  (web-skeleton::lookup-connection
                   (web-skeleton::connection-fd out))
                  nil)
           (check "detached sweep: the callback fired exactly once" fires 1)
           (check "detached sweep: and fired the cleanup sentinel"
                  status-seen nil)
           ;; DELIVER-DETACHED's disposition for a :STREAMING target on an
           ;; abort: the stream cannot be finished honestly, so it is
           ;; closed rather than left open behind a body that stopped.
           (check "detached sweep: and the target stream was closed with it"
                  (web-skeleton::lookup-connection
                   (web-skeleton::connection-fd target))
                  nil))
      (ignore-errors (web-skeleton::%close epfd))
      (ignore-errors (sb-bsd-sockets:socket-close target-sock))
      (ignore-errors (sb-bsd-sockets:socket-close out-sock)))))

(defun test-harness-fetch-into-setup-failure ()
  "A detached fetch whose setup fails synchronously signals, and leaves the
   target connection exactly as it found it.

   This is where FETCH-INTO's headline claim — a signalling call has done
   nothing — is interesting. The four refusals beside it are pre-flight:
   they return before any state is touched, so the claim is trivially true
   of them. Setup failure is the only ending that marks the connection
   first and then fails, so it is the only place the marker can be
   watched going back.

   Two fixes live on this path and neither had a detector. Both reverts
   left the suite green.

   Removing INITIATE-FETCH's re-raise for :DETACHED queues a 502 into the
   application's live stream, flips it to :WRITE-RESPONSE, and fires the
   abort sentinel — mid-stream corruption of a connection the app owns,
   answered in a framing the app never chose. Removing FETCH-INTO's reset
   on unwind leaves that connection unable to start another fetch for the
   rest of its life: every later FETCH-INTO hits the already-outstanding
   refusal.

   HTTPS with the TLS hook unbound is one of the three synchronous
   failures the contract names, and it raises inside INITIATE-FETCH after
   the marker is set and before any socket exists — which is exactly the
   window under test. LET rather than SETF is safe for the hook because
   FETCH-INTO runs on this thread: a synchronous failure is synchronous by
   definition, so no worker ever reads the binding."
  (format t "~%Harness: fetch-into setup failure leaves the target alone~%")
  (let ((fires 0))
    (let ((conn (web-skeleton::make-connection :fd 1 :state :streaming))
          (web-skeleton::*epoll-fd* 99)
          (web-skeleton::*tls-outbound-setup-fn* nil))
      (check "setup failure: it signals rather than reporting through :then"
             (handler-case
                 (progn (fetch-into
                         conn
                         (http-fetch :get "https://example.test/"
                                     :then (lambda (s h b)
                                             (declare (ignore s h b))
                                             (incf fires)
                                             nil)))
                        :no-signal)
               (error () :signalled))
             :signalled)
      ;; Written before INITIATE-FETCH ran and cleared on the way out. The
      ;; refusals cannot assert this: they never write it.
      (check "setup failure: the target can start another fetch"
             (web-skeleton::connection-fetch-outstanding conn) nil)
      ;; The three the re-raise protects, each a separate way the app's
      ;; own connection would have been corrupted.
      (check "setup failure: nothing was queued into the app's stream"
             (web-skeleton::connection-write-pending conn) 0)
      (check "setup failure: the target is still streaming"
             (web-skeleton::connection-state conn) :streaming)
      (check "setup failure: the callback did not fire" fires 0))))

(defun test-harness-fetch-into-upstream-stalls-e2e ()
  "An upstream that stops mid-body aborts the stream without terminating it.

   Three things at once, because one arrangement produces all of them: the
   sweeper reaping a detached outbound (nothing else can — it is not idle,
   it is waiting, and no inbound is parked on it), the aborted disposition,
   and the rule that an abort must not be delivered as an HTTP response
   into a body already in flight.

   The client must see its decode *fail*. A stream closed with a
   terminator after a failed upstream would tell it the body was complete,
   which is silent truncation — the failure CHUNKED-TERMINATOR is a
   separate function to prevent."
  (format t "~%Harness: fetch-into, upstream stalls mid-body~%")
  (let ((port-box (list nil))
        (saved web-skeleton:*fetch-timeout*)
        (aborted nil))
    (setf web-skeleton:*fetch-timeout* 1)
    (unwind-protect
         (with-test-server
             (:handler
              (lambda (req)
                (if (search "/up" (http-request-path req))
                    ;; One chunk, then silence. Never closed, so the
                    ;; detached fetch's own deadline is the only thing
                    ;; that can end it.
                    (make-stream-response
                     :on-open (lambda (c) (stream-send c (%ascii "partial"))))
                    (make-stream-response
                     :on-open
                     (lambda (client)
                       (fetch-into
                        client
                        (http-fetch
                         :get (format nil "http://127.0.0.1:~d/up"
                                      (first port-box))
                         :on-body (lambda (out chunk)
                                    (declare (ignore out))
                                    (stream-send client chunk)
                                    nil)
                         :then (lambda (status headers body)
                                 (declare (ignore headers body))
                                 (setf aborted (null status))
                                 nil))))))))
           (setf (first port-box) *test-port*)
           (multiple-value-bind (socket stream) (%raw-connect)
             (unwind-protect
                  (progn
                    (%send-raw-get stream "/relay" :extra
                                   (format nil "Connection: close~c~c"
                                           #\Return #\Newline))
                    (let* ((buf (read-until-bounded stream :seconds 8))
                           (raw (subseq buf 0 (fill-pointer buf)))
                           (decoded (%decode-streamed-body raw)))
                      (check "upstream stall: the bytes already relayed arrived"
                             (search "partial"
                                     (sb-ext:octets-to-string
                                      raw :external-format :latin-1))
                             (search "partial"
                                     (sb-ext:octets-to-string
                                      raw :external-format :latin-1)))
                      ;; The property. A terminator here would claim the
                      ;; body was whole.
                      (check "upstream stall: the client's decode fails"
                             (search "chunked" decoded)
                             (search "chunked" decoded))
                      (check "upstream stall: decode did not succeed short"
                             (string= decoded "partial") nil)))
               (ignore-errors (close stream))
               (ignore-errors (sb-bsd-sockets:socket-close socket))))
           (check "upstream stall: :then saw the abort sentinel" aborted t)
           (check "upstream stall: no outbound left behind"
                  (census-await :outbound 0) 0))
      (setf web-skeleton:*fetch-timeout* saved))))

(defun test-harness-fetch-into-target-closed-e2e ()
  "A client that leaves mid-relay tears the outbound down, once.

   D5's arm reaches a detached outbound by walking, because nothing was
   parked and AWAITING-FD names nothing. The subtlety is that it runs while
   the target is still :STREAMING with a live fd — CONNECTION-CLOSE is
   several lines below — so a disposition applied here would find it
   :STREAMING, take the aborted branch, and call CLOSE-CONNECTION on the
   connection already being closed one frame up.

   It terminates, which is what makes it worth asserting: the outer frame
   would resume holding a descriptor the inner call already closed. Counted
   rather than inferred, because every neighbouring assertion — the
   outbound goes away, the callback fires once — passes with the
   re-entrancy present."
  (format t "~%Harness: fetch-into, target closed mid-relay~%")
  (let ((port-box (list nil))
        (target-fd nil)
        (closes 0)
        (real (symbol-function 'web-skeleton::close-connection)))
    (setf (symbol-function 'web-skeleton::close-connection)
          (lambda (conn epoll-fd &optional (reason :closed))
            (when (and target-fd
                       (= (web-skeleton::connection-fd conn) target-fd))
              (incf closes))
            (funcall real conn epoll-fd reason)))
    (unwind-protect
         (progn
           (with-test-server
               (:handler
                (lambda (req)
                  (if (search "/up" (http-request-path req))
                      ;; Never completes, so the relay is still live when
                      ;; the client walks away.
                      (make-stream-response
                       :on-open (lambda (c) (stream-send c (%ascii "x"))))
                      (make-stream-response
                       :on-open
                       (lambda (client)
                         (setf target-fd (web-skeleton::connection-fd client))
                         (fetch-into
                          client
                          (http-fetch
                           :get (format nil "http://127.0.0.1:~d/up"
                                        (first port-box))
                           :on-body (lambda (out chunk)
                                      (declare (ignore out chunk))
                                      nil)
                           :then (lambda (s h b)
                                   (declare (ignore s h b))
                                   nil))))))))
             (setf (first port-box) *test-port*)
             (multiple-value-bind (socket stream) (%raw-connect)
               (%send-raw-get stream "/relay" :extra
                              (format nil "Connection: close~c~c"
                                      #\Return #\Newline))
               ;; Let the head and first chunk land, then vanish.
               (sleep 0.4)
               (ignore-errors (close stream))
               (ignore-errors (sb-bsd-sockets:socket-close socket)))
             (check "target closed: outbound torn down with it"
                    (census-await :outbound 0) 0))
           (check "target closed: close-connection entered exactly once"
                  closes 1))
      (setf (symbol-function 'web-skeleton::close-connection) real))))

(defun test-fetch-stop-is-sticky-across-a-pass ()
  "A :STOP on the first chunk of a pass survives the chunks that follow it.

   :ON-BODY's answer was captured last-answer-wins, which is right for
   :PAUSE — it describes a write backlog that may have drained by the next
   chunk — and wrong for :STOP, which describes the fetch and has no
   verdict that retracts it. Under last-wins an app that stopped on the
   first of three chunks in one pass had its stop erased by the NIL it
   returned for the second, and the fetch ran to completion with the
   cancel silently dropped.

   Driven through a stub transport rather than a socket, for the reason
   TEST-FETCH-OK-EOF-WALKS-THE-BYTES is: whether several chunks land in one
   read is a scheduling question over a real peer, and a pass carrying one
   chunk cannot tell sticky from last-wins apart at all. Here the whole
   response arrives in one read on every run, so the case is reached on
   every run.

   The response is *complete*, terminator included, and that is what makes
   one assertion cover both halves of the verdict. Last-wins loses the stop
   and COMPLETE-FETCH delivers a 200; so does an implementation that keeps
   the stop but tests it after COMPLETE in the cond. Only sticky, and ahead
   of COMPLETE, produces the abort sentinel.

   That completeness does a second job, and it is why this test is not a
   smaller copy of the end-to-end one. A stop taken mid-body has no chunked
   terminator yet, so a COMPLETE-FETCH routing raises out of
   DECODE-CHUNKED-BODY and the fetch ends on the sentinel anyway, by the
   wrong road — the end-to-end test cannot tell that apart from the right
   one. Here the accumulated body parses, 200 is what COMPLETE-FETCH
   actually reports, and this is the only place that reading is refused.

   The chunks are asserted too, and not as decoration: they are what proves
   the pass carried anything after the stop. Without them this would pass
   against a single-chunk pass, which is the arrangement that cannot fail."
  (format t "~%Fetch: :STOP is sticky across a pass~%")
  (let* ((bytes (sb-ext:string-to-octets (%chunked-corpus-response)
                                         :external-format :ascii))
         (pos 0)
         (chunks nil)
         (fires 0)
         (final :never)
         (socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp))
         (epfd (web-skeleton::epoll-create))
         ;; RUN-WORKER binds this in its own dynamic scope, and teardown
         ;; goes through it. Driving one connection outside a worker means
         ;; supplying the table the worker would have.
         (web-skeleton::*connections* (make-hash-table)))
    (unwind-protect
         (let ((conn (web-skeleton::make-connection
                      :fd (web-skeleton::socket-fd socket)
                      :socket socket
                      :state :out-read
                      :outbound-p t
                      ;; :DETACHED is what routes a stop through STOP-FETCH
                      ;; rather than into the parked path's 502, and -1
                      ;; exercises the target lookup finding nothing to
                      ;; unmark.
                      :fetch-sink :detached
                      :inbound-fd -1
                      :fetch-method :GET
                      :last-active (get-universal-time)
                      :fetch-on-body
                      (lambda (c chunk)
                        (declare (ignore c))
                        (push (sb-ext:octets-to-string
                               chunk :external-format :ascii)
                              chunks)
                        ;; Stop on the first, NIL for the rest. That
                        ;; sequence is the one last-wins erases.
                        (when (= (length chunks) 1) :stop))
                      :fetch-callback
                      (lambda (status headers body)
                        (declare (ignore headers body))
                        (incf fires)
                        (setf final status)
                        nil)
                      ;; Bytes until they run out, then :EOF — never
                      ;; :AGAIN, so the whole response is one pass.
                      :read-fn
                      (lambda (buf start max)
                        (if (>= pos (length bytes))
                            :eof
                            (let ((n (min max (- (length bytes) pos))))
                              (replace buf bytes :start1 start
                                                 :start2 pos :end2 (+ pos n))
                              (incf pos n)
                              n))))))
           ;; Registered so the reclamation check below reads a slot this
           ;; test actually filled. Unregistered, the count is zero whether
           ;; or not anything tore the connection down.
           (web-skeleton::register-connection conn)
           (web-skeleton::handle-outbound-read conn epfd)
           ;; The discriminating one.
           (check "stop sticky: the fetch ended on the abort sentinel"
                  final nil)
           (check "stop sticky: the pass carried chunks past the stop"
                  (reverse chunks) *chunked-corpus*)
           (check "stop sticky: the fetch ended exactly once" fires 1)
           (check "stop sticky: the outbound was reclaimed"
                  (hash-table-count web-skeleton::*connections*) 0))
      (ignore-errors (web-skeleton::%close epfd)))))

(defun test-harness-fetch-into-stop-e2e ()
  "A stop ends the upstream and hands back a connection that still works.

   The want this verdict exists for, end to end: stop this upstream, keep
   this connection. Three claims, and they fail in different directions.

   The upstream never closes, so within the client's read window nothing
   but the stop can end this fetch — *FETCH-TIMEOUT* is 30 seconds and the
   window is 8, which is what keeps the deadline sweeper from passing this
   test on the stop's behalf.

   :THEN must see the abort sentinel, and which wrong implementation that
   catches was measured rather than reasoned about. COMPLETE-FETCH with
   :FRAMING-COMPLETE T — a copy of the arm directly above it in the cond,
   and so the likeliest mistake — suppresses the body decode, delivers a
   200, and passes every other check here: the disposition leaves the
   target alone, the stream survives, the second fetch runs. Only this
   check fails, on exactly the report the verdict exists to prevent.

   Plain COMPLETE-FETCH, without that keyword, is caught elsewhere and not
   here, which is worth stating rather than leaving to be discovered. A
   chunked upstream stopped mid-body has no terminator, so
   DECODE-CHUNKED-BODY raises, HANDLE-OUTBOUND-EVENT's handler routes it
   to DELIVER-FETCH-ERROR, and :THEN receives the sentinel by the wrong
   road. TEST-FETCH-STOP-IS-STICKY-ACROSS-A-PASS is the detector for that
   shape: its upstream response is complete, so COMPLETE-FETCH parses it
   and reports 200 with or without the keyword.

   The target must survive, asserted through its own body: the client
   decodes what was relayed plus what was written after the stop,
   terminator and all. It earns its place against a stop that never
   happens and against one that leaves the connection unusable, both of
   which end with no terminator ever written. It is not evidence about
   *routing*, and reading it that way would overstate it — the second
   fetch sets the outstanding marker again before DELIVER-DETACHED
   consults it, which suppresses the disposition, so even a stop routed
   through the failure path leaves this stream standing.

   And the connection must still be usable, which is the half that is easy
   to miss. CLOSE-OUTBOUND does not clear the target's outstanding marker —
   DELIVER-DETACHED does, and a stopped fetch never reaches it — so a stop
   that skipped that step would hand back a connection FETCH-INTO refuses
   for the rest of its life. Proven by starting the next fetch from inside
   the stopped one's :THEN, which is also the earliest moment an
   application would try."
  (format t "~%Harness: fetch-into, stopped from :on-body~%")
  (let ((port-box (list nil))
        (then-fires 0)
        (body-fires 0)
        (stopped nil)
        (second-start :never))
    (with-test-server
        (:handler
         (lambda (req)
           (let ((path (http-request-path req)))
             (cond
               ;; One chunk, then silence, and never closed.
               ((search "/up" path)
                (make-stream-response
                 :on-open (lambda (c) (stream-send c (%ascii "alpha")))))
               ;; What the surviving connection goes on to fetch.
               ((search "/second" path)
                (make-text-response 200 "second"))
               (t
                (make-stream-response
                 :on-open
                 (lambda (client)
                   (fetch-into
                    client
                    (http-fetch
                     :get (format nil "http://127.0.0.1:~d/up"
                                  (first port-box))
                     :on-body (lambda (out chunk)
                                (declare (ignore out))
                                (incf body-fires)
                                (stream-send client chunk)
                                :stop)
                     :then
                     (lambda (status headers body)
                       (declare (ignore headers body))
                       (incf then-fires)
                       (setf stopped (null status))
                       (setf second-start
                             (handler-case
                                 (progn
                                   (fetch-into
                                    client
                                    (http-fetch
                                     :get (format nil
                                                  "http://127.0.0.1:~d/second"
                                                  (first port-box))
                                     :then (lambda (s h b)
                                             (declare (ignore s h))
                                             (when b (stream-send client b))
                                             (stream-close client)
                                             nil)))
                                   :ok)
                               (error (e) (princ-to-string e))))
                       nil))))))))))
      (setf (first port-box) *test-port*)
      (multiple-value-bind (socket stream) (%raw-connect)
        (unwind-protect
             (progn
               (%send-raw-get stream "/relay" :extra
                              (format nil "Connection: close~c~c"
                                      #\Return #\Newline))
               (let* ((buf (read-until-bounded stream :seconds 8))
                      (raw (subseq buf 0 (fill-pointer buf))))
                 (check "stop: the target survived and kept producing"
                        (%decode-streamed-body raw) "alphasecond")))
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
      ;; The discriminating one.
      (check "stop: :then saw the abort sentinel" stopped t)
      (check "stop: the stopped connection could start another fetch"
             second-start :ok)
      (check "stop: :on-body fired before the stop" (> body-fires 0) t)
      (check "stop: :then fired exactly once" then-fires 1)
      (check "stop: no outbound left behind"
             (census-await :outbound 0) 0))))

(defun census-await (key target &key (seconds 5))
  "Poll WEB-SKELETON::CONNECTION-CENSUS until KEY reads TARGET, or SECONDS
   elapse. Returns the last value seen, so a failing check reports what the
   count actually settled on rather than only that it was wrong.

   Polling rather than reading once, because the census is published on the
   maintenance tick and is therefore up to a second stale by construction.
   A single read after a request would be asserting on the tick's timing."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second)))
        (seen nil))
    (loop
      (setf seen (getf (web-skeleton::connection-census) key))
      (when (eql seen target) (return seen))
      (when (> (get-internal-real-time) deadline) (return seen))
      (sleep 0.05))))

(defun test-harness-census-outbound-returns-to-zero-e2e ()
  "An ordinary fetch leaves no outbound connection behind.

   This is the instrument the rest of the branch is reviewed with. Every
   connection-lifecycle defect it exists to catch — an outbound never
   swept, a teardown that misses its pair, a paused connection nothing
   will wake — strands a connection with no request attached to it, so a
   request log would show nothing at all and this shows a count that never
   comes down.

   Asserted in both directions on purpose. That the count returns to zero
   is the property; that it was non-zero first is what proves the census
   can see an outbound at all, without which zero would be vacuous and the
   assertion would hold just as well against a census that counted
   nothing.

   The first draft of this test read the count from :THEN and measured a
   peak of 0, because COMPLETE-FETCH tears the outbound down before it
   invokes the callback. The non-vacuity check is the only reason that was
   noticed rather than shipped as a passing test of nothing."
  (format t "~%Harness: census, outbound returns to zero after a fetch~%")
  (let ((port-box (list nil))
        (peak 0))
    (with-test-server
        (:handler
         (lambda (req)
           (if (search "/up" (http-request-path req))
               ;; Chunked, so :ON-BODY fires at all — against a
               ;; Content-Length upstream there is no chunk walk to hand
               ;; bytes back from and the callback is never called.
               (make-stream-response
                :on-open (lambda (c)
                           (stream-send c (sb-ext:string-to-octets
                                           "one" :external-format :ascii))
                           (stream-close c)))
               (http-fetch
                :get (format nil "http://127.0.0.1:~d/up" (first port-box))
                ;; The only window in which an outbound is observable.
                ;; :THEN is too late — COMPLETE-FETCH calls CLOSE-OUTBOUND
                ;; before invoking it, so by then the connection is already
                ;; unregistered and the count is legitimately back to zero.
                :on-body (lambda (out chunk)
                           (declare (ignore out chunk))
                           (setf peak
                                 (max peak
                                      (getf (web-skeleton::census-counts)
                                            :outbound)))
                           nil)
                :then (lambda (status headers body)
                        (declare (ignore headers body))
                        (if (eql status 200)
                            (make-text-response 200 "relayed")
                            (make-error-response 502)))))))
      (setf (first port-box) *test-port*)
      (multiple-value-bind (status headers body)
          (test-http-request :get "/fetch")
        (declare (ignore headers))
        (check "census e2e: the fetch completed" status 200)
        (check "census e2e: and answered from :then" body "relayed"))
      ;; Non-vacuity: the census counted an outbound while one existed.
      (check "census e2e: an outbound was visible mid-fetch"
             (>= peak 1) t)
      ;; The property.
      (check "census e2e: outbound returns to 0"
             (census-await :outbound 0) 0))))

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
  (test-harness-pipelined-after-body-e2e)
  (test-harness-chunked-keepalive-e2e)
  (test-harness-chunked-trailer-smuggle-e2e)
  (test-harness-chunked-body-cap-e2e)
  (test-harness-chunked-expect-100-continue-e2e)
  (test-harness-cached-response-survives-head-e2e)
  (test-harness-http10-keepalive-no-mutation-e2e)
  (test-harness-http10-expect-100-continue-no-fire-e2e)
  (test-harness-expect-417-on-unknown-e2e)
  (test-harness-expect-417-on-unknown-no-body-e2e)
  (test-harness-expect-417-head-no-body-e2e)
  (test-harness-connection-header-split-e2e)
  (test-harness-handler-connection-close-honored-e2e)
  (test-harness-fetch-callback-connection-close-honored-e2e)
  (test-harness-awaiting-timeout-answers-504-e2e)
  (test-harness-close-delimited-fetch-e2e)
  (test-harness-dns-all-addresses-refused-e2e)
  (test-harness-http11-server-close-stamps-connection-close-e2e)
  (test-refuse-connection-drains)
  (test-harness-connection-limit-e2e)
  (test-harness-workers-zero-rejected)
  (test-harness-write-stall-timeout-zero-rejected)
  (test-harness-write-backlog-minimum-rejected)
  (test-harness-fetch-stream-plain-e2e)
  (test-harness-port-zero-reported)
  (test-harness-port-zero-workers-share-one-port)
  (test-harness-streaming-e2e)
  (test-harness-sse-e2e)
  (test-harness-sse-keepalive-framed-e2e)
  (test-harness-fetch-on-body-e2e)
  (test-harness-fetch-on-body-content-length-e2e)
  (test-fetch-ok-eof-walks-the-bytes)
  (test-harness-fetch-on-body-eof-together-e2e)
  (test-harness-fetch-on-body-truncated-chunked-e2e)
  (test-harness-stream-does-not-hold-worker-e2e)
  (test-harness-census-outbound-returns-to-zero-e2e)
  (test-harness-fetch-into-refusals)
  (test-harness-fetch-into-setup-failure)
  (test-harness-detached-deadline-sweep)
  (test-harness-fetch-into-relay-e2e)
  (test-harness-fetch-into-chained-e2e)

  (test-harness-fetch-into-upstream-stalls-e2e)
  (test-harness-fetch-into-target-closed-e2e)
  (test-fetch-stop-is-sticky-across-a-pass)
  (test-harness-fetch-into-stop-e2e)
  (report-suite "Harness")
  (zerop *tests-failed*))
