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
  (test-refuse-connection-drains)
  (test-harness-connection-limit-e2e)
  (test-harness-workers-zero-rejected)
  (report-suite "Harness")
  (zerop *tests-failed*))
