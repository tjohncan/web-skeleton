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

(defun test-harness-large-body-grows-the-buffer-e2e ()
  "A body many times the size of a connection's first read buffer arrives
   whole over a real socket.

   The buffer starts at 4 KiB and is grown by CONNECTION-READ-AVAILABLE as
   bytes arrive, never in advance to the length the client declared. This
   is the path every large upload takes, through as many partial reads as
   the kernel happens to deliver, and nothing else drives it end to end.
   The digest is compared rather than the length alone, because a buffer
   that grew in the wrong place would lose bytes at a doubling boundary
   and could still report the right count."
  (format t "~%Harness: a large body grows the read buffer~%")
  (let* ((n (* 600 1024))
         (body (let ((s (make-string n)))
                 (dotimes (i n s)
                   (setf (char s i) (code-char (+ 97 (mod (* i 7) 26)))))))
         (want (format nil "~d ~a" n
                       (sha256-hex (sb-ext:string-to-octets
                                    body :external-format :ascii)))))
    (with-test-server
        (:handler (lambda (req)
                    (let ((b (http-request-body req)))
                      (make-text-response
                       200 (format nil "~d ~a" (length b) (sha256-hex b))))))
      (multiple-value-bind (status headers response-body)
          (test-http-request :post "/upload" :body body)
        (declare (ignore headers))
        (check "large body: dispatches" status 200)
        (check "large body: every byte arrived, in order" response-body want)))))

(defun test-harness-split-head-then-pipelined-e2e ()
  "A short request pipelined behind one whose head arrived in two pieces is
   answered.

   The first head's search records how far it got when the first piece
   held no terminator. The keep-alive reset then moves the pipelined
   request to offset 0, where its whole head sits before that position.
   Broken state: the reset leaves the position as it was, the search for
   the second head starts past its terminator, and the second request
   waits for an end it has already sent — here until the read deadline.

   The pause between the two writes is what makes the first head two
   wake-ups rather than one; without it this is an ordinary pipelined
   pair and passes whatever the reset does."
  (format t "~%Harness: a split head, then a pipelined request~%")
  (with-test-server (:handler (lambda (req)
                                (make-text-response
                                 200 (http-request-path req))))
    (let* ((socket (connect-to-test-server))
           (stream (sb-bsd-sockets:socket-make-stream
                    socket :input t :output t :element-type '(unsigned-byte 8))))
      (flet ((put (s)
               (write-sequence (sb-ext:string-to-octets s :external-format :ascii)
                               stream)
               (force-output stream)))
        (unwind-protect
             (progn
               (put (concatenate 'string
                                 "GET /first HTTP/1.1" *crlf*
                                 "Host: localhost" *crlf*
                                 "X-Pad: " (make-string 100 :initial-element #\a)))
               (sleep 0.2)
               (put (concatenate 'string
                                 *crlf* *crlf*
                                 "GET /b HTTP/1.1" *crlf*
                                 "Host: x" *crlf* *crlf*))
               (let* ((buf (read-until-bounded
                            stream :seconds 3
                            :until (lambda (buf fill)
                                     ;; Both bodies, the second one last.
                                     (and (>= fill 2)
                                          (= (aref buf (- fill 2)) 47)     ; /
                                          (= (aref buf (- fill 1)) 98))))) ; b
                      (text (sb-ext:octets-to-string buf :external-format :latin-1)))
                 (check "split head: the first request is answered"
                        (not (null (search "/first" text))) t)
                 (check "split head: and the pipelined one behind it"
                        (let ((tail (search "HTTP/1.1 200" text :from-end t)))
                          (and tail (plusp tail) (search "/b" text :start2 tail) t))
                        t)))
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket)))))))

(defun test-harness-full-buffer-answered-promptly-e2e ()
  "A request that fills the read buffer without finishing is answered as
   the buffer fills, not when the idle sweep gets to it.

   The client sends everything at once and then only reads. The read that
   fills the buffer leaves the rest in the kernel, and nothing more will
   arrive to raise another edge, so the answer has to come from that read.
   Broken state: the read deadline below expires with nothing received,
   because the server is waiting for an event that will not come and will
   close the connection, unanswered, at *IDLE-TIMEOUT*.

   The answer is the 413 and then a lingering close: a FIN after the
   response, and the rest of the request read and discarded. Closed at
   once instead, with the rest unread, Linux answers with a reset, which
   a client on Linux survives — the 413 arrived first and stays readable
   — but which discards anything unsent and reads as a failure to a peer
   that treats it as one. The read ending in :EOF rather than :ERROR is
   what tells the two apart.

   Budgets are set with SETF, not LET: the worker reads the global values."
  (format t "~%Harness: a full buffer is answered when it fills~%")
  (let ((saved (list web-skeleton:*max-body-size*
                     web-skeleton:*max-total-header-bytes*
                     web-skeleton:*max-request-line-length*
                     web-skeleton:*max-header-count*)))
    (setf web-skeleton:*max-body-size* 8192
          web-skeleton:*max-total-header-bytes* 1024
          web-skeleton:*max-request-line-length* 1024
          web-skeleton:*max-header-count* 10)
    (unwind-protect
         (with-test-server (:handler (lambda (req)
                                       (declare (ignore req))
                                       (make-text-response 200 "unreached")))
           (let* ((socket (connect-to-test-server))
                  (stream (sb-bsd-sockets:socket-make-stream
                           socket :input t :output t
                           :element-type '(unsigned-byte 8))))
             (unwind-protect
                  (progn
                    ;; 12 KiB of one-byte chunks against a 10 KiB cap: the
                    ;; decoded body is 2000 bytes, well inside the body cap,
                    ;; so it is the buffer that fills.
                    (write-sequence
                     (sb-ext:string-to-octets
                      (concatenate 'string
                                   "POST /u HTTP/1.1" *crlf*
                                   "Host: localhost" *crlf*
                                   "Transfer-Encoding: chunked" *crlf*
                                   *crlf*
                                   (with-output-to-string (s)
                                     (dotimes (i 2000)
                                       (format s "1~aX~a" *crlf* *crlf*))))
                      :external-format :ascii)
                     stream)
                    (force-output stream)
                    (multiple-value-bind (buf reason)
                        (read-until-bounded stream :seconds 4)
                      (check "full buffer e2e: the connection ends promptly"
                             (not (eq reason :deadline)) t)
                      (check "full buffer e2e: the 413 arrives"
                             (eql 0 (search "HTTP/1.1 413"
                                            (sb-ext:octets-to-string
                                             buf :external-format :latin-1)))
                             t)
                      (check "full buffer e2e: and ends with a FIN, not a reset"
                             reason :eof)))
               (ignore-errors (close stream))
               (ignore-errors (sb-bsd-sockets:socket-close socket)))))
      (setf web-skeleton:*max-body-size* (first saved)
            web-skeleton:*max-total-header-bytes* (second saved)
            web-skeleton:*max-request-line-length* (third saved)
            web-skeleton:*max-header-count* (fourth saved)))))

(defun test-harness-body-at-max-size-e2e ()
  "A POST whose Content-Length equals *MAX-BODY-SIZE* dispatches: the cap
   is a ceiling the body may reach, and the comparison is > rather than >=.

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

(defun test-harness-a-head-with-no-fields-e2e ()
  "A request line and CRLFCRLF with nothing between, on the wire: answered
   at HTTP/1.0, which requires no field, and refused 400 at HTTP/1.1,
   which requires Host. OPTIONS / HTTP/1.0 alone is HAProxy's default
   health probe, so a server that refuses this shape fails its checks."
  (format t "~%Harness: a request head with no header fields~%")
  (with-test-server
      (:handler (lambda (req)
                  (make-text-response 200 (format nil "~a ~a"
                                                  (http-request-method req)
                                                  (http-request-path req)))))
    (flet ((exchange (line)
             (%exchange-or-why (%ascii (format nil "~a~a~a" line *crlf* *crlf*))))
           (status-line (text)
             (subseq text 0 (min 12 (length text)))))
      (let ((answer (exchange "GET /probe HTTP/1.0")))
        (check "no fields: GET at 1.0 is answered" (status-line answer) "HTTP/1.1 200")
        (check "no fields: GET reaches the handler"
               (not (null (search "GET /probe" answer))) t))
      (let ((answer (exchange "OPTIONS / HTTP/1.0")))
        (check "no fields: OPTIONS at 1.0 is answered" (status-line answer) "HTTP/1.1 200")
        (check "no fields: OPTIONS reaches the handler"
               (not (null (search "OPTIONS /" answer))) t))
      (check "no fields: at 1.1 it is refused for its missing Host"
             (status-line (exchange "GET / HTTP/1.1")) "HTTP/1.1 400"))))

(defun test-harness-content-length-spellings-e2e ()
  "Every spelling of a Content-Length field is read alike by both of its
   readers, or refused with the connection closed.

   SCAN-CONTENT-LENGTH takes the body's length from the raw head when its
   CRLFCRLF arrives, and PARSE-HEADERS-BYTES reads the same field again at
   dispatch. A spelling one of them reads and the other does not is what
   request smuggling is made of, so each case carries five body bytes with
   GET /second behind them in the same write.
   - Read alike: the body is the five bytes, and /second is answered.
   - Refused: one 400, and /second never answered.
   - A name that is not Content-Length frames no body, so its five bytes
     begin the next request, refused 501 and closed."
  (format t "~%Harness: Content-Length spellings, read alike or refused~%")
  (with-test-server
      (:handler (lambda (req)
                  (make-text-response 200 (format nil "~a ~d"
                                                  (http-request-path req)
                                                  (length (or (http-request-body req) ""))))))
    (flet ((shown (field)
             ;; FIELD for a check's name, its CR, LF and TAB written out.
             (with-output-to-string (s)
               (loop for c across field
                     do (case c
                          (#\Return (write-string "<CR>" s))
                          (#\Newline (write-string "<LF>" s))
                          (#\Tab (write-string "<TAB>" s))
                          (t (write-char c s))))))
           (outcome (field)
             ;; (statuses first-body-length second-answered-p)
             (let* ((resp (%exchange-or-why
                           (%ascii (format nil "POST /first HTTP/1.1~aHost: x~a~a~a~a~
                                                helloGET /second HTTP/1.1~aHost: x~a~a"
                                           *crlf* *crlf* field *crlf* *crlf*
                                           *crlf* *crlf* *crlf*))))
                    (statuses (loop for at = (search "HTTP/1.1 " resp)
                                      then (search "HTTP/1.1 " resp :start2 (1+ at))
                                    while at
                                    collect (subseq resp (+ at 9) (min (length resp) (+ at 12))))))
               (list statuses
                     (cond ((search "/first 5" resp) 5)
                           ((search "/first 0" resp) 0))
                     (not (null (search "/second 0" resp)))))))
      (dolist (field (list "Content-Length: 5" "Content-Length:5" "content-length: 5"
                           "CONTENT-LENGTH: 5" "Content-Length: 05" "Content-Length: 5 "
                           (format nil "Content-Length:~c5" #\Tab)
                           (format nil "Content-Length: 5~c" #\Tab)
                           (format nil "Content-Length: 5~aContent-Length: 5" *crlf*)))
        (check (format nil "CL read alike: ~a" (shown field))
               (outcome field) '(("200" "200") 5 t)))
      (dolist (field (list "Content-Length : 5" (format nil "Content-Length~c: 5" #\Tab)
                           "Content-Length: +5" "Content-Length: -5" "Content-Length: 0x5"
                           "Content-Length: 5x" "Content-Length: 5,5" "Content-Length: 5, 5"
                           "Content-Length: 5, 6" "Content-Length: 5 5" "Content-Length: "
                           (format nil "X-Pad: y~a Content-Length: 5" *crlf*)))
        (check (format nil "CL refused, nothing behind it read: ~a" (shown field))
               (outcome field) '(("400") nil nil)))
      (dolist (field '("X-Content-Length: 5" "Content-Lengthx: 5"))
        (check (format nil "not a CL, no body: ~a" (shown field))
               (outcome field) '(("200" "501") 0 nil))))))

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
      ;; One connection, kept alive, and no half-close: a FIN beside the
      ;; request would have the server answer it Connection: close.
      (multiple-value-bind (socket stream) (%raw-connect)
        (unwind-protect
             (flet ((ask (request)
                      ;; REQUEST sent; its answer, head and four-byte body, as
                      ;; text, read no further than that.
                      (write-sequence (%ascii request) stream)
                      (force-output stream)
                      (let ((buf (read-until-bounded
                                  stream :seconds 5
                                  :until (lambda (b fill)
                                           (let ((hend (web-skeleton::scan-crlf-crlf b 0 fill)))
                                             (and hend (>= fill (+ hend 4 4))))))))
                        (string-downcase
                         (sb-ext:octets-to-string (subseq buf 0 (fill-pointer buf))
                                                  :external-format :latin-1)))))
               (let ((first (ask (concatenate 'string
                                              "GET / HTTP/1.0" *crlf*
                                              "Host: localhost" *crlf*
                                              "Connection: keep-alive" *crlf* *crlf*))))
                 (check "http/1.0 keep-alive: the hint is on the wire"
                        (not (null (search "connection: keep-alive" first))) t))
               (let ((second (ask (concatenate 'string
                                               "GET / HTTP/1.1" *crlf*
                                               "Host: localhost" *crlf* *crlf*))))
                 (check "http/1.0 keep-alive: the connection serves the next request"
                        (not (null (search "http/1.1 200" second))) t)
                 (check "http/1.0 keep-alive: which carries no stale hint"
                        (search "connection: keep-alive" second) nil)))
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
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
   100-continue receives a 417 Expectation Failed, with the body never
   reaching the handler. RFC 9110 §10.1.1 lets a server answer 417
   (MAY); the framework always does, so this pins its choice. Sends
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
   is meaningless in practice — but the test pins it."
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
               ;; makes EOF arrive promptly. The drain is bounded, so a
               ;; regression that held the socket open fails these
               ;; checks with whatever did arrive, rather than blocking
               ;; here until with-test-server's teardown notices.
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
               ;; regression doesn't hang the suite. The TIMEOUT it signals
               ;; is not an ERROR, so it is named in the catch; a catch on
               ;; ERROR alone would let it end the run instead.
               (let ((got-second nil))
                 (handler-case
                     (sb-ext:with-timeout 1
                       (write-sequence req2 stream)
                       (force-output stream)
                       (let ((b (read-byte stream nil nil)))
                         (when b (setf got-second t))))
                   ((or error sb-ext:timeout) () nil))
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
                     ;; TIMEOUT named beside ERROR, as in the test above.
                     ((or error sb-ext:timeout) () nil))
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
   answers: draining to EOF here deadlocks both sides.

   RESPONSE goes out one byte per character, as Latin-1, so a test can
   put any byte on the wire by writing the character with that code."
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
            (sb-ext:string-to-octets response :external-format :latin-1) st)
           (force-output st)
           (sb-bsd-sockets:socket-close s))
       (error () nil)))
   :name name))

(defun %crlf-lines (&rest lines)
  "LINES joined by CRLF, with a trailing CRLF. Hand-written wire bytes: the
   chunk boundaries a test asserts on have to be the test's choice, not
   whatever an encoder picked on the day."
  (with-output-to-string (out)
    (dolist (line lines)
      (format out "~a~c~c" line #\Return #\Newline))))

(defun test-harness-close-delimited-fetch-e2e ()
  "A fetch of a close-delimited upstream completes when the close arrives,
   not when *FETCH-TIMEOUT* expires.

   CONNECTION-READ-AVAILABLE reports \"read bytes, then EOF\" as :OK-EOF,
   not :OK. For a response with no Content-Length and no
   Transfer-Encoding that end of stream *is* the framing —
   OUTBOUND-RESPONSE-COMPLETE-P returns NIL forever and says so,
   deferring to the caller's EOF branch — so a read that discarded it
   would never run the branch, and the fetch would hang until the
   sweeper took it.

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
                                 (%crlf-lines "HTTP/1.1 200 OK"
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

(defun test-harness-fetch-latin-1-header-e2e ()
  "An upstream header in Latin-1 reaches the fetch's :THEN, and so does
   the response it came with.

   The byte is E9: é in ISO-8859-1, and not UTF-8 on its own. It sits in
   a Content-Disposition filename, which is where legacy servers send it.
   Read strictly, that one header would fail the whole fetch — a 502 and
   the cleanup sentinel — over a value the application may never read."
  (format t "~%Harness: an upstream's Latin-1 header does not fail the fetch~%")
  (let* ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                  :type :stream :protocol :tcp))
         (thread nil)
         (disposition (format nil "attachment; filename=\"r~csum.pdf\""
                              (code-char #xE9))))
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
                                 (%crlf-lines "HTTP/1.1 200 OK"
                                        (format nil "Content-Disposition: ~a"
                                                disposition)
                                        "Content-Length: 2"
                                        "")
                                 "ok")
                    :name "latin-1-upstream"))
             (with-test-server
                 (:handler
                  (lambda (req)
                    (declare (ignore req))
                    (defer-to-fetch :GET
                      (format nil "http://127.0.0.1:~d/file" upstream-port)
                      :then (lambda (status headers body)
                              (let ((value (cdr (assoc "content-disposition"
                                                       headers
                                                       :test #'string-equal))))
                                (make-text-response
                                 200
                                 (format nil "~a|~a|~a"
                                         status
                                         (equal value disposition)
                                         (if body
                                             (sb-ext:octets-to-string
                                              body :external-format :utf-8)
                                             "(none)"))))))))
               (multiple-value-bind (status headers body)
                   (handler-case (test-http-request :get "/proxy")
                     (error (e) (values nil nil (princ-to-string e))))
                 (declare (ignore headers))
                 (check "latin-1 header: the client is answered 200" status 200)
                 (check "latin-1 header: :THEN got the status, the value and the body"
                        body "200|T|ok")))))
      (ignore-errors (sb-bsd-sockets:socket-close listener))
      (when thread
        (handler-case (sb-thread:join-thread thread :timeout 5)
          (error () (ignore-errors (sb-thread:terminate-thread thread))))))))

(defun test-harness-accept-failure-is-retried-e2e ()
  "A connection queued behind a failed accept is answered without another
   connection arriving.

   The listener is edge-triggered: its event fires when a connection
   arrives, not while one waits. Once the server is up, the next
   SOCKET-ACCEPT is made to fail as EMFILE would, and one client
   connects. The kernel has queued it, the accept that should take it
   fails, and nothing else will ever raise an edge for it. Only the
   maintenance tick's retry answers it, so it is answered within a few
   seconds or, without the retry, not until the client gives up."
  (format t "~%Harness: a backlog left by a failed accept is retried~%")
  (with-test-server
      (:handler (lambda (req)
                  (declare (ignore req))
                  (make-text-response 200 "ok")))
    (let ((failed nil))
      (sb-int:encapsulate 'sb-bsd-sockets:socket-accept 'fail-once
                          (lambda (f &rest args)
                            (if failed
                                (apply f args)
                                (progn (setf failed t)
                                       (error "simulated EMFILE")))))
      (unwind-protect
           (let ((start (get-internal-real-time)))
             (multiple-value-bind (status headers body)
                 (attempt (test-http-request :get "/"))
               (declare (ignore headers))
               (check "accept retry: the queued client is answered"
                      (list status body) (list 200 "ok"))
               (check "accept retry: within a few seconds, by the tick"
                      (< (/ (- (get-internal-real-time) start)
                            internal-time-units-per-second)
                         5)
                      t))
             (check "control: the accept did fail first" failed t))
        (sb-int:unencapsulate 'sb-bsd-sockets:socket-accept 'fail-once)))))

(defun test-harness-client-errors-log-once-a-minute-e2e ()
  "A flood of requests a client gets wrong logs a line, not a flood.

   Every log line holds *LOG-LOCK*, the one lock all workers share, across
   a format and a flush, and a malformed request costs a client one write.
   LOG-THROTTLED logs the first of a kind at once and then at most once a
   minute per worker, with a count of the ones held back. The first half
   drives it directly; the second sends twenty requests with an unknown
   method to a real server and counts the lines its worker wrote."
  (format t "~%Harness: a flood of bad requests logs a line, not a flood~%")
  (let ((out (make-string-output-stream)))
    (let ((web-skeleton::*log-stream* out)
          (web-skeleton::*log-level* :info)
          (web-skeleton::*log-throttle* (make-hash-table :test #'eq)))
      (dotimes (i 5)
        (web-skeleton::log-throttled :warn :test-kind "bad thing ~d" i))
      (web-skeleton::log-throttled :warn :other-kind "other thing")
      ;; A minute on, as far as the throttle can tell.
      (decf (car (gethash :test-kind web-skeleton::*log-throttle*)) 61)
      (web-skeleton::log-throttled :warn :test-kind "bad thing ~d" 5))
    (let ((text (get-output-stream-string out)))
      (check "throttle: the first of a kind logs"
             (and (search "bad thing 0" text) t) t)
      (check "throttle: the four after it do not"
             (search "bad thing 1" text) nil)
      (check "throttle: another kind logs on its own"
             (and (search "other thing" text) t) t)
      (check "throttle: a minute on, one line carries the count"
             (and (search "bad thing 5 (and 4 more like it" text) t) t)))
  (let ((out (make-string-output-stream)))
    (let ((web-skeleton::*log-stream* out)
          (web-skeleton::*log-level* :info)
          (web-skeleton::*log-throttle* nil))
      (dotimes (i 3) (web-skeleton::log-throttled :warn :k "line ~d" i)))
    (check "throttle: outside a worker every line logs"
           (count #\Newline (get-output-stream-string out)) 3))
  (let ((saved web-skeleton::*log-stream*)
        (log (make-string-output-stream))
        (statuses nil))
    (unwind-protect
         (progn
           ;; SETF, not bound: the worker that logs is another thread.
           (setf web-skeleton::*log-stream* log)
           (with-test-server
               (:handler (lambda (req)
                           (declare (ignore req))
                           (make-text-response 200 "ok")))
             (dotimes (i 20)
               (push (attempt (values (test-http-request :bogus "/")))
                     statuses))))
      (setf web-skeleton::*log-stream* saved))
    (let ((text (get-output-stream-string log)))
      (check "control: every request was refused with 501"
             (every (lambda (s) (eql s 501)) statuses) t)
      (check "twenty malformed requests log one parse-error line"
             (loop with n = 0 and start = 0
                   for at = (search "parse error fd" text :start2 start)
                   while at do (incf n) (setf start (1+ at))
                   finally (return n))
             1))))

(defun %piecemeal-upstream (listener pieces &key (pause 0.005) (hold 6))
  "Accept once, read the request head, write PIECES (strings, one byte per
   character) one at a time with PAUSE seconds between them, then hold the
   connection open for HOLD seconds before closing. Returns the thread."
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
           (dolist (piece pieces)
             (write-sequence (sb-ext:string-to-octets piece :external-format :latin-1) st)
             (force-output st)
             (sleep pause))
           (sleep hold)
           (sb-bsd-sockets:socket-close s))
       (error () nil)))
   :name "piecemeal-upstream"))

(defun %pieces (string size)
  "STRING cut into consecutive pieces of SIZE characters."
  (loop for i from 0 below (length string) by size
        collect (subseq string i (min (length string) (+ i size)))))

(defun %fetch-through (pieces &key (hold 6))
  "Fetch from a %PIECEMEAL-UPSTREAM sending PIECES. Returns
   (values STATUS SECONDS) as the client of the relay saw them."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp))
        (thread nil))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 5)
           (multiple-value-bind (host port) (sb-bsd-sockets:socket-name listener)
             (declare (ignore host))
             (setf thread (%piecemeal-upstream listener pieces :hold hold))
             (with-test-server
                 (:handler (lambda (req)
                             (declare (ignore req))
                             (defer-to-fetch :GET (format nil "http://127.0.0.1:~d/h" port)
                               :then (lambda (status headers body)
                                       (declare (ignore headers body))
                                       (make-text-response (or status 502) "relayed")))))
               (let ((start (get-internal-real-time)))
                 (values (attempt (values (test-http-request :get "/relay")))
                         (/ (- (get-internal-real-time) start)
                            internal-time-units-per-second))))))
      (ignore-errors (sb-bsd-sockets:socket-close listener))
      (when thread
        (handler-case (sb-thread:join-thread thread :timeout 10)
          (error () (ignore-errors (sb-thread:terminate-thread thread))))))))

(defun test-harness-outbound-head-budget-and-search-e2e ()
  "An upstream's response head is searched once and refused at the
   streaming path's length.

   Two halves. An upstream that sends a head longer than
   RESPONSE-HEAD-BUDGET and then holds the connection open is answered 502
   at once, where waiting for the buffer's 8 MiB cap or for the upstream
   to close would keep the fetch alive. An upstream that dribbles a 20 KiB
   head in 200 pieces is searched incrementally: the bytes SCAN-CRLF-CRLF
   walks stay near the head's size, where a search from the top on every
   read would walk the buffer once per piece, about 4 MiB."
  (format t "~%Harness: an upstream head is budgeted and searched once~%")
  ;; A head that never ends.
  (let ((head (with-output-to-string (s)
                (format s "HTTP/1.1 200 OK~c~c" #\Return #\Newline)
                (dotimes (i 100)
                  (format s "X-Pad-~3,'0d: ~a~c~c" i (make-string 1000 :initial-element #\p)
                          #\Return #\Newline)))))
    (multiple-value-bind (status seconds) (%fetch-through (%pieces head 4096))
      (check "head budget: an unending head is answered 502" status 502)
      (check "head budget: at once, not when the upstream gives up"
             (< seconds 4) t)))
  ;; A head that ends, sent in pieces.
  (let* ((head (with-output-to-string (s)
                 (format s "HTTP/1.1 200 OK~c~cContent-Length: 2~c~c" #\Return #\Newline
                         #\Return #\Newline)
                 (dotimes (i 50)
                   (format s "X-Pad-~3,'0d: ~a~c~c" i (make-string 390 :initial-element #\p)
                           #\Return #\Newline))
                 (format s "~c~cok" #\Return #\Newline)))
         (scanned 0))
    (sb-int:encapsulate 'web-skeleton::scan-crlf-crlf 'count-scan
                        (lambda (f buf start end)
                          (incf scanned (max 0 (- end start)))
                          (funcall f buf start end)))
    (unwind-protect
         (multiple-value-bind (status seconds)
             (%fetch-through (%pieces head 100) :hold 0)
           (declare (ignore seconds))
           (check "head search: the dribbled head is read and relayed" status 200)
           (check "head search: the bytes walked stay near the head's size"
                  (< scanned (* 200 1024)) t))
      (sb-int:unencapsulate 'web-skeleton::scan-crlf-crlf 'count-scan))))

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
  (apply #'%crlf-lines "HTTP/1.1 200 OK" "Transfer-Encoding: chunked" ""
         (append (loop for payload in *chunked-corpus*
                       append (list (format nil "~x" (length payload)) payload))
                 (list "0" ""))))

(defun %chunked-upstream-fetch (response on-body-out then-out fires-box
                                &optional verdict)
  "Run one :ON-BODY fetch against a canned upstream sending RESPONSE, and
   return the relay's own (VALUES STATUS BODY).

   Shared by the three tests below because only RESPONSE and VERDICT differ
   between them: one response ends with the zero-size terminator and one
   does not, and everything else about the setup is the thing being held
   constant.

   VERDICT is what :ON-BODY answers — NIL for the two framing tests, :STOP
   for the one that asks what a stop does on this path. This is the
   *parked* path, where a client is waiting in :AWAITING, so the fetch is
   handler-returned rather than detached and the two endings differ."
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
                                      (car on-body-out))
                                verdict)
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

   :OK-EOF means \"read some bytes, and then hit end of stream\". An arm
   shared with :EOF — read *nothing*, already at end of stream — would
   never walk the bytes it carried, so :ON-BODY would never fire, and
   COMPLETE-FETCH would then null the buffered body on the strength of an
   :ON-BODY callback merely being installed. The app would get 200 with
   no body at all. Not truncated, absent.

   TEST-HARNESS-FETCH-ON-BODY-E2E cannot catch this and is not weaker for
   it: its upstream streams over time, so the FIN reliably arrives as its
   own event and the coalesced case never occurs.

   The canned upstream writes and closes back to back, which makes the
   coalesced read the usual outcome and not a guaranteed one — the
   framework can still be scheduled between the write and the close, read
   the bytes as :OK, and take the ordinary path. So this catches a
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
      (ignore-errors (web-skeleton::%close epfd))
      ;; The fetch's teardown closes the socket; this is for a run that
      ;; never reached it. A closed socket takes a second close as a no-op.
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun test-harness-fetch-on-body-truncated-chunked-e2e ()
  "A chunked upstream that delivers chunks and then closes without the
   zero-size terminator fails the fetch, rather than reporting success.

   There is no Content-Length to compare against, and under :ON-BODY the
   delivered chunks have been freed, so the guard is COMPLETE-FETCH's own:
   a chunked relay that reaches end of stream without the walk finding
   its terminator is a 502. Without it, chunks would go to :ON-BODY, the
   upstream would vanish mid-body, and :THEN would fire 200 with a NIL
   body: byte-for-byte the report a *whole* response produces. The app
   could not tell the two apart, which is the failure the Content-Length
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
         (%crlf-lines "HTTP/1.1 200 OK" "Transfer-Encoding: chunked" ""
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

(defun %relay-corpus (chunks size)
  "CHUNKS chunks of SIZE bytes each, chunk I filled with the letter I mod
   26, as a list of strings. Distinct letters make a lost, repeated or
   reordered chunk show in the relayed text, where a byte count would not."
  (loop for i below chunks
        collect (make-string size :initial-element
                             (code-char (+ 97 (mod i 26))))))

(defun test-harness-fetch-on-body-relays-past-the-cap-e2e ()
  "An :ON-BODY fetch relays a chunked response many times the size of
   *MAX-OUTBOUND-RESPONSE-SIZE*, with the read buffer held to the cap.

   A relay hands each chunk on as its framing is proved, so the chunks
   it has handed on are freed. Kept, they would hold the whole upstream
   in memory, and the read would fail at the cap however steadily the
   stream flowed: a feed relayed into a stream or a WebSocket, which is
   what :ON-BODY is for, would die once it had carried the cap's worth.

   The cap is lowered to 64 KiB for the run and the upstream sends five
   times that, so the claim is about the cap and not about a size."
  (format t "~%Harness: an :on-body relay carries more than the cap~%")
  (let* ((saved-cap web-skeleton:*max-outbound-response-size*)
         (corpus (%relay-corpus 40 8192))
         (expected (apply #'concatenate 'string corpus))
         (listener (make-instance 'sb-bsd-sockets:inet-socket
                                  :type :stream :protocol :tcp))
         (thread nil)
         (received (make-string-output-stream))
         (peak (list 0))
         (final (list :never)))
    (unwind-protect
         (progn
           (setf web-skeleton:*max-outbound-response-size* (* 64 1024))
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
                                 (%crlf-lines "HTTP/1.1 200 OK"
                                        "Transfer-Encoding: chunked" "")
                                 (apply #'%crlf-lines
                                        (append
                                         (loop for c in corpus
                                               append (list (format nil "~x" (length c))
                                                            c))
                                         (list "0" ""))))
                    :name "relay-upstream"))
             (with-test-server
                 (:handler
                  (lambda (req)
                    (declare (ignore req))
                    (http-fetch
                     :get (format nil "http://127.0.0.1:~d/feed" upstream-port)
                     :on-body (lambda (out chunk)
                                (setf (car peak)
                                      (max (car peak)
                                           (length (web-skeleton::connection-read-buf
                                                    out))))
                                (write-string (sb-ext:octets-to-string
                                               chunk :external-format :latin-1)
                                              received)
                                nil)
                     :then (lambda (status headers body)
                             (declare (ignore headers))
                             (setf (car final) (list status body))
                             (make-text-response 200 "relayed")))))
               (let ((status (handler-case (test-http-request :get "/relay")
                               (error () nil))))
                 (check "relay past the cap: the client is answered 200" status 200)))))
      (setf web-skeleton:*max-outbound-response-size* saved-cap)
      (ignore-errors (sb-bsd-sockets:socket-close listener))
      (when thread
        (handler-case (sb-thread:join-thread thread :timeout 5)
          (error () (ignore-errors (sb-thread:terminate-thread thread))))))
    (let ((got (get-output-stream-string received)))
      (check "relay past the cap: every chunk was relayed, in order"
             (string= got expected) t)
      (check "relay past the cap: five times the cap went through"
             (length got) (* 5 64 1024)))
    (check "relay past the cap: :then fired 200 with a NIL body"
           (car final) (list 200 nil))
    (check "relay past the cap: the read buffer stayed within the cap"
           (<= (car peak) (* 64 1024)) t)))

(defun test-harness-fetch-into-relays-past-the-cap-e2e ()
  "FETCH-INTO relays a stream many times *MAX-OUTBOUND-RESPONSE-SIZE* into
   a client's own stream, end to end.

   The detached relay DEPLOYMENT.md documents, at a size the cap would end:
   five times a 64 KiB cap, from the framework's own chunked stream through
   FETCH-INTO into the client's. The relayed body is decoded and compared
   whole, so a relay cut off at the cap fails here as a short body."
  (format t "~%Harness: a fetch-into relay carries more than the cap~%")
  (let* ((saved-cap web-skeleton:*max-outbound-response-size*)
         (corpus (%relay-corpus 40 8192))
         (port-box (list nil))
         (then-status (list :never)))
    (unwind-protect
         (progn
           (setf web-skeleton:*max-outbound-response-size* (* 64 1024))
           (with-test-server
               (:handler
                (lambda (req)
                  (if (search "/up" (http-request-path req))
                      (make-stream-response
                       :on-open (lambda (c)
                                  (dolist (piece corpus)
                                    (stream-send c (%ascii piece)))
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
                                   (declare (ignore headers body))
                                   (setf (car then-status) status)
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
                        (check "fetch-into past the cap: the whole body arrived, framed"
                               (equal (%decode-streamed-body raw)
                                      (apply #'concatenate 'string corpus))
                               t)))
                 (ignore-errors (close stream))
                 (ignore-errors (sb-bsd-sockets:socket-close socket))))
             (check "fetch-into past the cap: :then saw the upstream's 200"
                    (car then-status) 200)))
      (setf web-skeleton:*max-outbound-response-size* saved-cap))))

(defun test-harness-fetch-on-body-stop-parked-e2e ()
  "A :STOP from a handler-returned fetch answers the parked client 502.

   :ON-BODY belongs to HTTP-FETCH, not to FETCH-INTO, so the verdict added
   for detached fetches is reachable on this path too — where there is a
   client parked in :AWAITING and no response owed to it by anything else.
   Ending through CLOSE-OUTBOUND alone, which is right for a detached
   fetch, would leave that client to be reaped having been answered
   nothing: a request that hangs for *FETCH-TIMEOUT* and then closes.

   So STOP-FETCH routes this path to DELIVER-FETCH-ERROR instead, and the
   client gets the same 502 every other ending that produces no response
   here already produces. Defining it was not optional — a verdict on a
   shared callback cannot have behaviour on half its callers — but it is
   not what the verdict is for, and the docstring says so.

   The upstream response is *complete*, terminator and all, which is what
   makes the 502 mean something. Against a truncated one this test would
   be indistinguishable from its neighbour above, which reaches the same
   502 by the framing guard rather than by the stop.

   :THEN still fires the cleanup sentinel exactly once, because the stop
   goes through CLOSE-OUTBOUND on the way and that is where it fires."
  (format t "~%Harness: fetch :on-body :stop on a parked fetch~%")
  (let ((chunks (list nil))
        (final (list :never))
        (fires (list 0)))
    (multiple-value-bind (status body)
        (%chunked-upstream-fetch (%chunked-corpus-response)
                                 chunks final fires :stop)
      (declare (ignore body))
      ;; The discriminating one. Ending at CLOSE-OUTBOUND leaves the client
      ;; parked with nothing, and this reads NIL after the harness deadline
      ;; rather than 502.
      (check "stop parked: the parked client is answered 502" status 502))
    ;; Non-vacuity: the stop happened mid-delivery, not before anything
    ;; arrived. Without this the check above would also pass against a
    ;; fetch that failed at the door.
    (check "stop parked: the chunk that triggered it was delivered"
           (first (reverse (car chunks))) (first *chunked-corpus*))
    (check "stop parked: :then fired the cleanup sentinel"
           (first (car final)) nil)
    (check "stop parked: :then fires exactly once" (car fires) 1)))

(defun test-harness-dns-all-addresses-refused-e2e ()
  "A hostname whose every resolved address the policy refuses fails the
   fetch promptly, with a 502, rather than stranding until the sweeper.

   Read this before trusting it: **this test passes without the :OK-EOF
   arm**, and is here as an end-to-end assertion of the DEPLOYMENT.md
   promise rather than as coverage for :OK-EOF. TEST-READ-AVAILABLE-EOF is
   the check with teeth.

   The reason is the mechanism. getent writes its output in one go and the
   EOF appears when it exits, so whether one drain sees both depends on
   whether it has exited by the time we read — and usually it has not. The
   bytes come back :OK, the exit arrives as a separate event, and a branch
   taking only :EOF handles it. Only when getent finishes first do the two
   coalesce, and that is the ordering that needs the arm. A race, not a
   certainty, so a missing arm can survive a suite that covers the parser
   it sits behind.

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

(defun test-harness-address-filter-asked-once-e2e ()
  "*FETCH-ADDRESS-FILTER* is asked about each address of a name once a
   lookup, and once again on each fetch a cached resolution serves: the
   count its docstring gives a filter with a side effect, an audit line or
   a rate budget, to be charged.

   getent's output usually arrives in one read and its exit in another,
   and a lookup that parsed the output at each would ask twice; getent can
   also print an address on two lines. The fetch is to localhost, so the
   name's addresses are whatever this host's getent prints for it, one or
   two, and the check is that none is asked about twice in one fetch, the
   cache off and on.

   The filter and *DNS-CACHE-TTL* are set globally, not bound: the worker
   reads them on a thread START-SERVER spawned."
  (format t "~%Harness: the address filter is asked once an address~%")
  (let ((saved-filter web-skeleton:*fetch-address-filter*)
        (saved-ttl web-skeleton:*dns-cache-ttl*)
        (lock (sb-thread:make-mutex :name "filter-calls"))
        (calls nil))
    (setf web-skeleton:*fetch-address-filter*
          (lambda (ip family host)
            (declare (ignore family host))
            (sb-thread:with-mutex (lock)
              (push (web-skeleton::format-ip ip) calls))
            t))
    (unwind-protect
         (dolist (ttl '(0 60))
           (setf web-skeleton:*dns-cache-ttl* ttl)
           (let ((port nil))
             (with-test-server
                 (:handler
                  (lambda (req)
                    (if (string= (http-request-path req) "/up")
                        (make-text-response 200 "up")
                        (defer-to-fetch :get (format nil "http://localhost:~d/up" port)
                          :then (lambda (status headers body)
                                  (declare (ignore headers body))
                                  (make-text-response (or status 502) "fetched"))))))
               (setf port *test-port*)
               (let ((fetches
                       (loop repeat 3
                             collect (progn
                                       (sb-thread:with-mutex (lock) (setf calls nil))
                                       (let ((status (test-http-request :get "/go")))
                                         (list status
                                               (sb-thread:with-mutex (lock)
                                                 (reverse calls))))))))
                 (check (format nil "filter, cache ~:[off~;on~]: three fetches answered"
                                (plusp ttl))
                        (mapcar #'first fetches) '(200 200 200))
                 ;; Against the same lists with any repeat taken out, so a
                 ;; failure prints what was asked.
                 (let ((asked (mapcar #'second fetches)))
                   (check (format nil "filter, cache ~:[off~;on~]: each address asked once a fetch"
                                  (plusp ttl))
                          (list (notany #'null asked) asked)
                          (list t (mapcar (lambda (a)
                                            (remove-duplicates a :test #'string=
                                                                 :from-end t))
                                          asked))))))))
      (setf web-skeleton:*fetch-address-filter* saved-filter
            web-skeleton:*dns-cache-ttl* saved-ttl))))

(defun %proc-pids ()
  "Every pid with a directory under /proc.

   Listed with * and filtered for digits afterwards, because SBCL's
   pathnames have no character classes: /proc/[0-9]*/ matches nothing at
   all, and a zombie count built on it passes on every build ever made.
   Symlinks are left unresolved so /proc/self is skipped as a name rather
   than counted twice as a pid."
  (loop for p in (directory "/proc/*/stat" :resolve-symlinks nil)
        for dir = (pathname-directory p)
        for name = (car (last dir))
        when (and (= (length dir) 3)
                  (stringp name)
                  (plusp (length name))
                  (every #'digit-char-p name))
          collect (parse-integer name)))

(defun %proc-stat-fields (line)
  "(STATE PPID) from a /proc/PID/stat LINE. Read from after the last ')',
   because the command name before it is in parentheses and may contain
   anything, a ')' or a space included."
  (let ((rp (position #\) line :from-end t)))
    (when rp
      (let ((fields (%split-ws (subseq line (1+ rp)))))
        (list (first fields)
              (ignore-errors (parse-integer (second fields))))))))

(defun %zombie-children ()
  "Pids of this process's children that have exited and not been
   collected: state Z, and this process as the parent."
  (let ((self (sb-unix:unix-getpid)))
    (loop for pid in (%proc-pids)
          for line = (ignore-errors
                      (with-open-file (s (format nil "/proc/~d/stat" pid)
                                         :if-does-not-exist nil)
                        (and s (read-line s nil nil))))
          for (state ppid) = (and line (%proc-stat-fields line))
          when (and (equal state "Z") (eql ppid self))
            collect pid)))

(defun test-harness-hostname-fetches-leave-no-zombie-e2e ()
  "Two hundred fetches by hostname leave no zombie behind.

   Each one spawns a getent child, and its lookup ends when the answer
   arrives, which may be before the child has exited. Closing the process
   then makes a zombie on an SBCL before 2.4.11. The framework closes a
   child only once it has seen it exit, collecting it on the worker's tick.

   On 2.4.11 and later SBCL collects a closed child itself, so there this
   passes whether or not the framework waits for the exit, and is a
   control: two hundred real
   lookups through the whole path, and nothing left behind. On an older
   SBCL it is a detector, and not a perfect one. A zombie needs getent to
   be still running at the moment its lookup ends, which is a race, so a
   build that closes early can come through a lucky run clean.
   TEST-DNS-CHILD-CLOSED-ONLY-ONCE-DEAD holds the child alive on purpose
   and is the check that never misses, on any SBCL.

   Any answer is accepted from the fetch itself. What is under test is the
   child, and a lookup that resolved and then failed to connect ended the
   same way one that succeeded did."
  (format t "~%Harness: hostname fetches leave no zombie~%")
  ;; The counter's premises. Without the first, a listing that matches
  ;; nothing reads as no zombies forever; without the second, a parse
  ;; thrown by a command name does.
  (check "hostname fetches: the /proc walk finds this process"
         (not (null (member (sb-unix:unix-getpid) (%proc-pids))))
         t)
  (check "hostname fetches: a stat line reads past a name holding ') '"
         (%proc-stat-fields "42 (odd) S name) Z 7 1 1 0")
         '("Z" 7))
  (let ((upstream-port nil)
        (before (%zombie-children)))
    (with-test-server
        (:handler
         (lambda (req)
           (if (string= (http-request-path req) "/upstream")
               (make-text-response 200 "up")
               (defer-to-fetch :GET (format nil "http://localhost:~d/upstream"
                                            upstream-port)
                 :then (lambda (status headers body)
                         (declare (ignore headers body))
                         (make-text-response (or status 502) "via name"))))))
      (setf upstream-port *test-port*)
      (let ((answered (loop repeat 200
                            count (member (ignore-errors
                                           (values (test-http-request :get "/via")))
                                          '(200 502)))))
        (check "hostname fetches: all two hundred were answered" answered 200))
      ;; Collected on the worker's tick, so give it two.
      (let ((deadline (+ (get-internal-real-time)
                         (* 3 internal-time-units-per-second)))
            (left nil))
        (loop do (setf left (set-difference (%zombie-children) before))
              while (and left (< (get-internal-real-time) deadline))
              do (sleep 0.1))
        (check "hostname fetches: no zombie children left behind"
               (length left) 0)))))

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
   instance, not just the first one.

   Sent with no half-close: a FIN beside the request would have the
   server answer it Connection: close whatever its headers said."
  (format t "~%Harness: split Connection header close wins~%")
  (with-test-server
      (:handler (lambda (req)
                  (declare (ignore req))
                  (make-text-response 200 "body")))
    (multiple-value-bind (socket stream) (%raw-connect)
      (unwind-protect
           (progn
             (write-sequence (%ascii (concatenate 'string
                                                  "GET / HTTP/1.1" *crlf*
                                                  "Host: localhost" *crlf*
                                                  "Connection: keep-alive" *crlf*
                                                  "Connection: close" *crlf* *crlf*))
                             stream)
             (force-output stream)
             ;; To the end of the stream, which only the server's close
             ;; brings: a connection kept open ends the read at its
             ;; deadline instead.
             (multiple-value-bind (buf reason) (read-until-bounded stream :seconds 5)
               (let ((raw (sb-ext:octets-to-string (subseq buf 0 (fill-pointer buf))
                                                   :external-format :latin-1)))
                 (check "split Connection: response present"
                        (not (null (search "HTTP/1.1 200" raw))) t)
                 (check "split Connection: body byte received"
                        (not (null (search "body" raw))) t)
                 ;; The property, in both places it shows: what the server
                 ;; says, and what it does.
                 (check "split Connection: close answered on the wire"
                        (not (null (search "connection: close" raw))) t)
                 (check "split Connection: and the connection closed"
                        reason :eof))))
        (ignore-errors (close stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

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
                                ;; Port 0: see the backlog probe below.
                                (start-server
                                 :port 0
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
                                       ;; Port 0, so a start that got past
                                       ;; the check never binds the default
                                       ;; port, which SO_REUSEPORT would
                                       ;; share with whatever serves there.
                                       (start-server
                                        :port 0
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
                                       ;; Port 0, so a start that got past
                                       ;; the check never binds the default
                                       ;; port, which SO_REUSEPORT would
                                       ;; share with whatever serves there.
                                       (start-server
                                        :port 0
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
           ;; of any message, since this probe's server is torn down by the
           ;; thread timeout rather than returning: a start that failed for
           ;; another reason is no start either.
           (check "start-server: exactly ten bytes of headroom is accepted"
                  (try-backlog (+ *max-ws-message-size* 10))
                  nil))
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

   NIL rather than 0 when the read fails, because 'no listeners there'
   and 'could not look' are different answers: a caller that conflates
   them reports a spurious failure whenever the read does.

   The two are separate failures and easy to conflate. The undercount
   above is seq_file resume. A read that fails
   outright is not: EBADF on a descriptor nothing here closed means
   something else closed it, which is the shape CONNECTION-CLOSE's
   comment describes, a finalizer closing a reissued fd number under its
   new owner. The guard is kept all the same: a busy machine can still
   make the count late, and a caller should not have to know which
   failure it met."
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
        (saved-poll *worker-wake-interval*)
        (bound nil)
        (sem (sb-thread:make-semaphore :name "bare-server-port")))
    (setf web-skeleton::*shutdown-hooks* nil
          web-skeleton::*shutdown* nil
          *drain-timeout* 1
          *worker-wake-interval* 0.05)
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
            *worker-wake-interval* saved-poll))))

(defun test-harness-a-worker-left-behind-stays-with-its-run ()
  "A worker its run's join gave up on does not come back to serve beside
   the next run on the same port.

   Server A's handler is inside a slow request when shutdown is asked for,
   so A's join gives up on that worker and START-SERVER returns, saying
   so. Server B starts on the same port. When A's handler returns, its
   worker finds *SHUTDOWN* cleared by B and, keyed on the flag alone, went
   back to serving: listener open, old handler, and SO_REUSEPORT splitting
   connections between it and B. Every request made after A's handler
   returned is asserted to reach B, and the log to have named the worker
   left running.

   Started by hand rather than through WITH-TEST-SERVER, which owns one
   server at a time and resolves its own port; this needs two servers on
   one port, in sequence."
  (format t "~%Harness: a worker left behind by its run stays with that run~%")
  (let ((saved-hooks web-skeleton::*shutdown-hooks*)
        (saved-drain *drain-timeout*)
        (saved-poll *worker-wake-interval*)
        (saved-stream web-skeleton::*log-stream*)
        (log (make-string-output-stream))
        (port nil)
        (a-up (sb-thread:make-semaphore :name "server-a"))
        (b-up (sb-thread:make-semaphore :name "server-b"))
        (threads nil))
    ;; Global SETF: the workers read these from threads START-SERVER spawns.
    (setf web-skeleton::*shutdown-hooks* nil
          web-skeleton::*shutdown* nil
          *drain-timeout* 0
          *worker-wake-interval* 0.05
          web-skeleton::*log-stream* log)
    (flet ((serve (port* marker semaphore &optional slow)
             (sb-thread:make-thread
              (lambda ()
                (start-server
                 :host #(127 0 0 1) :port port* :workers 1
                 :on-listen (lambda (p)
                              (setf port p)
                              (sb-thread:signal-semaphore semaphore))
                 :handler (lambda (req)
                            (when (and slow (string= (http-request-path req)
                                                     "/slow"))
                              (sleep slow))
                            (make-text-response 200 marker))))
              :name (format nil "server-~a" marker)))
           (body-from (p)
             (let ((*test-port* p))
               (nth-value 2 (handler-case (test-http-request :get "/")
                              (error (e) (values nil nil (princ-to-string e))))))))
      (unwind-protect
           (let ((slow-start nil))
             ;; A's join gives up after *DRAIN-TIMEOUT* + 3 seconds; its
             ;; handler sleeps past that.
             (push (serve 0 "A" a-up 4.5) threads)
             (check "control: server A came up"
                    (not (null (sb-thread:wait-on-semaphore a-up :timeout 10))) t)
             (setf slow-start (get-internal-real-time))
             (push (sb-thread:make-thread
                    (lambda ()
                      (let ((*test-port* port))
                        (ignore-errors (test-http-request :get "/slow"))))
                    :name "slow-client")
                   threads)
             (sleep 0.3)
             (setf web-skeleton::*shutdown* t)
             ;; Server A returns once its join has given up.
             (let ((a (car (last threads))))
               (handler-case (sb-thread:join-thread a :timeout 10)
                 (error () nil)))
             (push (serve port "B" b-up) threads)
             (check "control: server B came up on the same port"
                    (not (null (sb-thread:wait-on-semaphore b-up :timeout 10))) t)
             ;; Past the end of A's slow handler, with room for its worker
             ;; to come back to its loop.
             (loop until (> (/ (- (get-internal-real-time) slow-start)
                               internal-time-units-per-second)
                            5.5)
                   do (sleep 0.1))
             (let ((bodies (loop repeat 20 collect (body-from port))))
               (check "every request after A's handler returned reached B"
                      (count "B" bodies :test #'equal) 20)))
        (setf web-skeleton::*shutdown* t)
        (dolist (th threads)
          (handler-case (sb-thread:join-thread th :timeout 10)
            (error () (ignore-errors (sb-thread:terminate-thread th)))))
        (setf web-skeleton::*shutdown-hooks* saved-hooks
              *drain-timeout* saved-drain
              *worker-wake-interval* saved-poll
              web-skeleton::*log-stream* saved-stream)))
    (check "the log named the worker A left running"
           (not (null (search "still running" (get-output-stream-string log))))
           t)))

(defun test-harness-drain-closes-a-keep-alive-response-it-let-finish ()
  "A shutdown that finds a keep-alive response in flight lets it finish,
   then closes the connection, and returns well inside *DRAIN-TIMEOUT*.

   A drain that let such a response finish and left the connection's
   keep-alive alone would have the last byte reset it to reading a next
   request. The drain never closes a connection that is reading, so
   shutdown would sit out the whole *DRAIN-TIMEOUT* for a response
   delivered at once, and log a force-close.

   The handler is still running when shutdown is asked for, so its
   response is queued after the drain has begun: the shape a SIGTERM
   during a request produces."
  (format t "~%Harness: the drain closes a keep-alive response it let finish~%")
  (let ((saved-hooks web-skeleton::*shutdown-hooks*)
        (saved-drain *drain-timeout*)
        (saved-poll *worker-wake-interval*)
        (saved-stream web-skeleton::*log-stream*)
        (log (make-string-output-stream))
        (port nil)
        (up (sb-thread:make-semaphore :name "drain-server"))
        (reply nil))
    (setf web-skeleton::*shutdown-hooks* nil
          web-skeleton::*shutdown* nil
          *drain-timeout* 5
          *worker-wake-interval* 0.05
          web-skeleton::*log-stream* log)
    (unwind-protect
         (let ((server (sb-thread:make-thread
                        (lambda ()
                          (start-server
                           :host #(127 0 0 1) :port 0 :workers 1
                           :on-listen (lambda (p)
                                        (setf port p)
                                        (sb-thread:signal-semaphore up))
                           :handler (lambda (req)
                                      (declare (ignore req))
                                      (sleep 1)
                                      (make-text-response 200 "answered"))))
                        :name "drain-server"))
               (client nil))
           (check "control: the server came up"
                  (not (null (sb-thread:wait-on-semaphore up :timeout 10))) t)
           ;; HTTP/1.1 and no Connection: close — a keep-alive request — and
           ;; the socket held open after the response, as a client reusing
           ;; it would.
           (setf client
                 (sb-thread:make-thread
                  (lambda ()
                    (let* ((*test-port* port)
                           (sock (connect-to-test-server))
                           (stream (sb-bsd-sockets:socket-make-stream
                                    sock :input t :output t
                                    :element-type '(unsigned-byte 8))))
                      (unwind-protect
                           (progn
                             (write-sequence (sb-ext:string-to-octets
                                              (crlf "GET /slow HTTP/1.1" "Host: x")
                                              :external-format :ascii)
                                             stream)
                             (force-output stream)
                             (setf reply (sb-ext:octets-to-string
                                          (read-until-bounded stream :seconds 8)
                                          :external-format :latin-1)))
                        (ignore-errors (close stream))
                        (ignore-errors (sb-bsd-sockets:socket-close sock)))))
                  :name "drain-client"))
           (sleep 0.4)
           (let ((start (get-internal-real-time)))
             (setf web-skeleton::*shutdown* t)
             (handler-case (sb-thread:join-thread server :timeout 10)
               (error () (ignore-errors (sb-thread:terminate-thread server))))
             (let ((secs (/ (float (- (get-internal-real-time) start))
                            internal-time-units-per-second)))
               (check "the server stopped well inside *drain-timeout*"
                      (< secs 3.0) t)))
           (handler-case (sb-thread:join-thread client :timeout 10)
             (error () (ignore-errors (sb-thread:terminate-thread client))))
           (check "the response in flight was delivered whole"
                  (and reply (search "HTTP/1.1 200" reply) (search "answered" reply) t)
                  t))
      (setf web-skeleton::*shutdown* t
            web-skeleton::*shutdown-hooks* saved-hooks
            *drain-timeout* saved-drain
            *worker-wake-interval* saved-poll
            web-skeleton::*log-stream* saved-stream))
    (check "and nothing was force-closed"
           (search "force-closing" (get-output-stream-string log))
           nil)))

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

(defun %scripted-upstream (listener script &key (read-request t) stop
                                              (name "scripted-upstream"))
  "Accept once and play SCRIPT on the connection, then close. Returns the
   thread.

   SCRIPT is a list of steps: a string is written and flushed at once, a
   number is that many seconds of silence. READ-REQUEST NIL skips reading
   the request head, for an upstream that never reads at all; otherwise
   the head is read to its CRLFCRLF first, as %CANNED-UPSTREAM reads it
   and for its reason.

   STOP, a cons, ends a silence early once its car is true. A test whose
   fetch has already given up has no use for the rest of the script, and
   the suite should not wait it out."
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (let* ((s (sb-bsd-sockets:socket-accept listener))
                (st (sb-bsd-sockets:socket-make-stream
                     s :input t :output t :element-type '(unsigned-byte 8)))
                (b (make-array 4096 :element-type '(unsigned-byte 8)
                                    :fill-pointer 0 :adjustable t)))
           (unwind-protect
                (progn
                  (when read-request
                    (loop for byte = (read-byte st nil nil)
                          while byte
                          do (vector-push-extend byte b)
                          until (web-skeleton::scan-crlf-crlf b 0 (fill-pointer b))))
                  (dolist (step script)
                    (if (stringp step)
                        (progn
                          (write-sequence (sb-ext:string-to-octets
                                           step :external-format :ascii)
                                          st)
                          (force-output st))
                        (let ((until (+ (get-internal-real-time)
                                        (* step internal-time-units-per-second))))
                          (loop until (or (and stop (car stop))
                                          (>= (get-internal-real-time) until))
                                do (sleep 0.05))))))
             (ignore-errors (sb-bsd-sockets:socket-close s))))
       (error () nil)))
   :name name))

(defun %call-with-scripted-upstream (script fn &key (read-request t))
  "Call FN with the port of a listener playing SCRIPT, and tear both down,
   cutting short whatever silence the script has left once FN returns."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp))
        (stop (list nil))
        (thread nil))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 5)
           (setf thread (%scripted-upstream listener script
                                            :read-request read-request
                                            :stop stop))
           (funcall fn (nth-value 1 (sb-bsd-sockets:socket-name listener))))
      (setf (car stop) t)
      (ignore-errors (sb-bsd-sockets:socket-close listener))
      (when thread
        (handler-case (sb-thread:join-thread thread :timeout 10)
          (error () (ignore-errors (sb-thread:terminate-thread thread))))))))

(defun %timed (seconds thunk)
  "Run THUNK on its own thread for at most SECONDS. Returns (VALUES OUTCOME
   ELAPSED): what THUNK returned, or the text of what it raised, or :HUNG;
   and the seconds it took. The bound is the test's, so a blocking call
   that never returns fails a check instead of the run."
  (let* ((start (get-internal-real-time))
         (outcome :hung)
         (thread (sb-thread:make-thread
                  (lambda ()
                    (setf outcome (handler-case (funcall thunk)
                                    (error (e) (princ-to-string e)))))
                  :name "timed-call")))
    (handler-case (sb-thread:join-thread thread :timeout seconds :default nil)
      (error () nil))
    (when (sb-thread:thread-alive-p thread)
      (ignore-errors (sb-thread:terminate-thread thread)))
    (values outcome
            (/ (float (- (get-internal-real-time) start))
               internal-time-units-per-second))))

(defun test-harness-fetch-stream-plain-read-timeout-e2e ()
  "HTTP-FETCH-STREAM over http:// gives up on an upstream that accepts,
   takes the request and says nothing, after *FETCH-TIMEOUT*.

   SO_RCVTIMEO is what bounds the read, and it bounds read(2) only. The
   upstream here closes after six seconds, so a read that waits on
   anything else finishes then, with a different error, and the elapsed
   time is what tells the two apart."
  (format t "~%Harness: http-fetch-stream over plain HTTP, a silent upstream~%")
  (%call-with-scripted-upstream
   '(6)
   (lambda (port)
     (multiple-value-bind (outcome secs)
         (%timed 12 (lambda ()
                      (let ((web-skeleton:*fetch-timeout* 1))
                        (http-fetch-stream
                         :get (format nil "http://127.0.0.1:~d/silent" port)
                         :on-line #'identity))))
       (check "silent upstream: the fetch raised a read timeout"
              (and (stringp outcome) (search "read timed out" outcome) t) t)
       (check "silent upstream: within the timeout, not at the upstream's close"
              (< secs 4.0) t)))))

(defun test-harness-fetch-stream-plain-lines-arrive-as-sent-e2e ()
  "HTTP-FETCH-STREAM over http:// hands each line to :ON-LINE as it
   arrives, not when the response ends.

   Five NDJSON lines a third of a second apart, the shape of a token
   stream, in both framings a streaming upstream uses. The first line must
   arrive while the last is still unsent; a reader that waits to fill an
   8 KiB buffer gets all five at once, at the end."
  (format t "~%Harness: http-fetch-stream over plain HTTP, lines as they arrive~%")
  (flet ((paced (framing)
           (let ((lines (loop for i from 1 to 5
                              collect (format nil "{\"token\":~d}~c" i #\Newline))))
             (ecase framing
               (:close
                (cons (%crlf-lines "HTTP/1.1 200 OK"
                             "Content-Type: application/x-ndjson" "")
                      (loop for line in lines append (list line 0.3))))
               (:chunked
                (append
                 (list (%crlf-lines "HTTP/1.1 200 OK"
                              "Content-Type: application/x-ndjson"
                              "Transfer-Encoding: chunked" ""))
                 (loop for line in lines
                       append (list (%crlf-lines (format nil "~x" (length line)) line)
                                    0.3))
                 (list (%crlf-lines "0" "")))))))
         (arrivals (port)
           ;; (SECONDS . LINE) for each line, in the order they came.
           (let ((start (get-internal-real-time))
                 (seen nil))
             (values
              (%timed 10 (lambda ()
                           (http-fetch-stream
                            :get (format nil "http://127.0.0.1:~d/tokens" port)
                            :on-line (lambda (line)
                                       (push (cons (/ (float (- (get-internal-real-time)
                                                                start))
                                                      internal-time-units-per-second)
                                                   line)
                                             seen)))))
              (reverse seen)))))
    (dolist (framing '(:close :chunked))
      (%call-with-scripted-upstream
       (paced framing)
       (lambda (port)
         (multiple-value-bind (status seen) (arrivals port)
           (check (format nil "~(~a~): the upstream answered" framing) status 200)
           (check (format nil "~(~a~): all five lines, in order" framing)
                  (mapcar #'cdr seen)
                  (loop for i from 1 to 5 collect (format nil "{\"token\":~d}" i)))
           ;; The last line goes out at 1.2 s, the close at 1.5 s.
           (check (format nil "~(~a~): the first line came before the last was sent"
                          framing)
                  (and seen (< (car (first seen)) 0.9) t)
                  t)))))))

(defun test-harness-fetch-bodiless-status-kept-alive-e2e ()
  "A fetch whose upstream answers 304 or 204 and keeps the connection open
   completes at the response's blank line.

   Neither status has a body, whatever its headers say — a 304's
   Content-Length describes the representation — so nothing more is coming
   and the end of the connection is not either. The upstream holds the
   connection for five seconds; a fetch that waited for a body or a close
   finishes then, when it finishes at all, and the elapsed time is what
   tells the two apart."
  (format t "~%Harness: a 304 or 204 from an upstream that keeps the connection~%")
  (dolist (case (list (list "304 with a Content-Length"
                            (%crlf-lines "HTTP/1.1 304 Not Modified" "ETag: \"v1\""
                                   "Content-Length: 1234" "")
                            304)
                      (list "204 with no framing at all"
                            (%crlf-lines "HTTP/1.1 204 No Content" "")
                            204)))
    (destructuring-bind (name head status) case
      (%call-with-scripted-upstream
       (list head 5)
       (lambda (port)
         (with-test-server
             (:handler
              (lambda (req)
                (declare (ignore req))
                (defer-to-fetch :GET (format nil "http://127.0.0.1:~d/x" port)
                  :headers '(("connection" . "keep-alive")
                             ("if-none-match" . "\"v1\""))
                  :then (lambda (s h b)
                          (declare (ignore h b))
                          (make-text-response 200 (format nil "upstream ~a" s))))))
           (let ((start (get-internal-real-time)))
             (multiple-value-bind (code headers body)
                 (handler-case (test-http-request :get "/relay")
                   (error (e) (values nil nil (princ-to-string e))))
               (declare (ignore headers))
               (let ((secs (/ (float (- (get-internal-real-time) start))
                              internal-time-units-per-second)))
                 (check (format nil "~a: :then saw the upstream's status" name)
                        (list code body)
                        (list 200 (format nil "upstream ~d" status)))
                 (check (format nil "~a: at the blank line, not the close" name)
                        (< secs 2.5) t))))))))))

(defun test-harness-fetch-non-chunked-coding-e2e ()
  "A response whose Transfer-Encoding does not include chunked is framed
   by the close, and HTTP-FETCH hands :THEN its bytes as they came, as
   HTTP-FETCH-STREAM reads the same response.

   RFC 9112 §6.3 frames such a response by the connection's close, and the
   Transfer-Encoding overrides any Content-Length. There is no chunk size
   to find: a reader that walks the body as chunks refuses it at its first
   byte. Neither coding is decoded here, so the body arrives unchanged.

   The buffered predicate is asked on its own as well, about a gzip body's
   first bytes, which the ASCII scripts below cannot send."
  (format t "~%Harness: a transfer coding other than chunked ends at the close~%")
  (let ((buf (concatenate '(vector (unsigned-byte 8))
                          (sb-ext:string-to-octets
                           (%crlf-lines "HTTP/1.1 200 OK" "Transfer-Encoding: gzip"
                                  "Connection: close" "")
                           :external-format :ascii)
                          #(#x1F #x8B #x08 #x00))))
    (check "non-chunked coding: a gzip body waits for the close"
           (attempt (web-skeleton::outbound-response-complete-p
                     buf (length buf) :get))
           nil))
  (dolist (case (list (list "gzip"
                            (%crlf-lines "HTTP/1.1 200 OK" "Transfer-Encoding: gzip"
                                   "Connection: close" "")
                            "{\"a\":1}")
                      ;; A length shorter than the body, so a reader that
                      ;; honoured it instead of reading to the close would
                      ;; answer "hel" and fail here, where an equal length
                      ;; could not tell the two apart.
                      (list "identity beside a Content-Length"
                            (%crlf-lines "HTTP/1.1 200 OK" "Transfer-Encoding: identity"
                                   "Content-Length: 3" "Connection: close" "")
                            "hello")))
    (destructuring-bind (name head body) case
      (%call-with-scripted-upstream
       (list head body)
       (lambda (port)
         (with-test-server
             (:handler
              (lambda (req)
                (declare (ignore req))
                (defer-to-fetch :GET (format nil "http://127.0.0.1:~d/x" port)
                  :then (lambda (s h b)
                          (declare (ignore h))
                          (make-text-response
                           200 (format nil "~a ~a" s
                                       (if b
                                           (sb-ext:octets-to-string
                                            b :external-format :latin-1)
                                           "")))))))
           (multiple-value-bind (code headers reply)
               (handler-case (test-http-request :get "/relay")
                 (error (e) (values nil nil (princ-to-string e))))
             (declare (ignore headers))
             (check (format nil "~a: http-fetch delivers the body read to the close"
                            name)
                    (list code reply)
                    (list 200 (format nil "200 ~a" body)))))))
      (%call-with-scripted-upstream
       (list head body)
       (lambda (port)
         (let* ((lines nil)
                (status (%timed 10 (lambda ()
                                     (http-fetch-stream
                                      :get (format nil "http://127.0.0.1:~d/x" port)
                                      :on-line (lambda (line) (push line lines)))))))
           (check (format nil "~a: http-fetch-stream reads it the same way" name)
                  (list status (reverse lines))
                  (list 200 (list body)))))))))

(defun test-harness-upstream-header-limits-e2e ()
  "An upstream's response head is held to the *MAX-UPSTREAM-* knobs, a
   client's request head to the inbound ones, and both upstream readers
   agree.

   Each head is past an inbound default: a field line over 8 KiB, a
   hundred and one fields, field lines over 64 KiB in all. At the
   defaults HTTP-FETCH answers 502 and HTTP-FETCH-STREAM raises; with the
   upstream knob raised, both read the response. Raised, it does not widen
   what a client may send, and left NIL it follows the inbound knob, so a
   deployment that raised that one for its upstreams keeps them.

   SETF, not LET: the buffered fetch is read on the worker and the
   streaming one on %TIMED's thread, and a binding crosses neither."
  (format t "~%Harness: an upstream's head is held to its own limits~%")
  (let* ((long-line (concatenate 'string "X-Long: " (make-string 9000 :initial-element #\a)))
         (many-fields (loop for i from 1 to 101 collect (format nil "X-F~d: v" i)))
         (big-lines (loop for i from 1 to 10
                          collect (format nil "X-B~d: ~a" i
                                          (make-string 7500 :initial-element #\b))))
         (saved (list web-skeleton:*max-upstream-header-count*
                      web-skeleton:*max-upstream-header-line-length*
                      web-skeleton:*max-upstream-total-header-bytes*
                      web-skeleton:*max-header-line-length*)))
    (labels ((head (lines)
               (apply #'%crlf-lines "HTTP/1.1 200 OK"
                      (append lines (list "Connection: close" ""))))
             (buffered (head)
               ;; HTTP-FETCH's answer to the client: (code body).
               (let ((result nil))
                 (%call-with-scripted-upstream
                  (list head "ok")
                  (lambda (port)
                    (with-test-server
                        (:handler
                         (lambda (req)
                           (declare (ignore req))
                           (defer-to-fetch :GET (format nil "http://127.0.0.1:~d/x" port)
                             :then (lambda (s h b)
                                     (declare (ignore s h))
                                     (make-text-response
                                      200 (if b
                                              (sb-ext:octets-to-string
                                               b :external-format :latin-1)
                                              ""))))))
                      (multiple-value-bind (code headers reply)
                          (handler-case (test-http-request :get "/relay")
                            (error (e) (values nil nil (princ-to-string e))))
                        (declare (ignore headers))
                        (setf result (list code (if (eql code 200) reply :refused)))))))
                 result))
             (streamed (head)
               ;; HTTP-FETCH-STREAM's answer: (status lines), or :REFUSED
               ;; when it raised.
               (let ((result nil))
                 (%call-with-scripted-upstream
                  (list head "ok")
                  (lambda (port)
                    (let* ((lines nil)
                           (status (%timed 10 (lambda ()
                                                (http-fetch-stream
                                                 :get (format nil "http://127.0.0.1:~d/x" port)
                                                 :on-line (lambda (line) (push line lines)))))))
                      (setf result (if (integerp status)
                                       (list status (reverse lines))
                                       :refused)))))
                 result))
             (reset ()
               (setf web-skeleton:*max-upstream-header-count* nil
                     web-skeleton:*max-upstream-header-line-length* nil
                     web-skeleton:*max-upstream-total-header-bytes* nil
                     web-skeleton:*max-header-line-length* (fourth saved))))
      (unwind-protect
           (progn
             (dolist (case (list (list "a field line over 8 KiB" (list long-line)
                                       '*max-upstream-header-line-length* 16384)
                                 (list "101 fields" many-fields
                                       '*max-upstream-header-count* 200)
                                 (list "field lines over 64 KiB" big-lines
                                       '*max-upstream-total-header-bytes* (* 128 1024))))
               (destructuring-bind (name lines knob value) case
                 (reset)
                 (check (format nil "upstream head, ~a: refused at the defaults" name)
                        (list (buffered (head lines)) (streamed (head lines)))
                        '((502 :refused) :refused))
                 (setf (symbol-value knob) value)
                 (check (format nil "upstream head, ~a: read with ~(~a~) raised" name knob)
                        (list (buffered (head lines)) (streamed (head lines)))
                        '((200 "ok") (200 ("ok"))))))
             ;; Raised for the upstream, the client's limit stays put.
             (reset)
             (setf web-skeleton:*max-upstream-header-line-length* 16384)
             (with-test-server
                 (:handler (lambda (req) (declare (ignore req))
                             (make-text-response 200 "ok")))
               (check "upstream head: a client's long line is still refused"
                      (let ((answer (%exchange-or-why
                                     (%ascii (concatenate 'string
                                                          "GET / HTTP/1.1" *crlf*
                                                          "Host: x" *crlf*
                                                          long-line *crlf* *crlf*)))))
                        (and (stringp answer) (search "HTTP/1.1 431" answer) t))
                      t))
             ;; Left NIL, the upstream limit is the inbound knob's.
             (reset)
             (setf web-skeleton:*max-header-line-length* 16384)
             (check "upstream head: NIL follows the inbound knob"
                    (buffered (head (list long-line)))
                    '(200 "ok")))
        (setf web-skeleton:*max-upstream-header-count* (first saved)
              web-skeleton:*max-upstream-header-line-length* (second saved)
              web-skeleton:*max-upstream-total-header-bytes* (third saved)
              web-skeleton:*max-header-line-length* (fourth saved))))
    ;; Checked at the start, on the calling thread, so a LET reaches it.
    ;; The invalid :HOST, as in the other boot checks, keeps a reverted
    ;; check from starting a server.
    (dolist (name '(*max-upstream-header-count*
                    *max-upstream-header-line-length*
                    *max-upstream-total-header-bytes*))
      (check (format nil "boot: ~(~a~) as a string is refused by name" name)
             (progv (list name) (list "100")
               (let ((msg (attempt (web-skeleton:start-server
                                    :host #(1 2 3) :port 0 :workers 1))))
                 (and (stringp msg)
                      (search (string-downcase (symbol-name name)) msg)
                      t)))
             t))))

(defun test-harness-serves-without-tls-e2e ()
  "The framework starts and serves with web-skeleton-tls absent, the build
   the demo's image and demo/deploy/run.lisp support.

   The suite runs with TLS loaded, so its absence is made here: the two
   hooks it registers unset, and SHA1, SHA256 and ECDSA-VERIFY-P256 back on
   the pure-Lisp versions it swaps out. A live server then answers a plain
   request; a WebSocket upgrade, whose accept key is SHA-1's; an ES256
   token, whose hash and signature check are SHA-256's and the pure-Lisp
   verifier's; and a static file, whose ETag is the SHA-256 of its bytes.
   An https:// fetch is refused for want of TLS, with a 502, and :THEN
   hears the cleanup sentinel.

   SETF, not LET: the workers read the hooks, and a function cell is
   global already."
  (format t "~%Harness: the framework serves with web-skeleton-tls absent~%")
  (let ((saved-setup web-skeleton::*tls-outbound-setup-fn*)
        (saved-stream web-skeleton:*https-stream-fn*)
        (saved-sha1 (symbol-function 'sha1))
        (saved-sha256 (symbol-function 'sha256))
        (saved-ecdsa (symbol-function 'web-skeleton::ecdsa-verify-p256))
        (saved-cache web-skeleton::*static-cache*)
        (then-status :never))
    (unwind-protect
         (progn
           (setf web-skeleton::*tls-outbound-setup-fn* nil
                 web-skeleton:*https-stream-fn* nil
                 (symbol-function 'sha1) #'web-skeleton::sha1-lisp
                 (symbol-function 'sha256) #'web-skeleton::sha256-lisp
                 (symbol-function 'web-skeleton::ecdsa-verify-p256)
                 #'web-skeleton::ecdsa-verify-p256-lisp)
           (check "without TLS: the suite's own test reads it as absent"
                  (tls-loaded-p) nil)
           ;; Built with the pure-Lisp SHA-256 in the cell, as a load
           ;; without TLS builds it.
           (setf web-skeleton::*static-cache* (make-hash-table :test #'equal)
                 (gethash "/hello.txt" web-skeleton::*static-cache*)
                 (web-skeleton::build-static-response
                  "text/plain; charset=utf-8"
                  (sb-ext:string-to-octets "hello" :external-format :ascii) 0))
           (let ((key (%es256-test-key))
                 (token (%es256-token "{\"alg\":\"ES256\",\"kid\":\"test\"}"
                                      "{\"sub\":\"no-tls\"}")))
             (with-test-server
                 (:handler
                  (lambda (req)
                    (let ((path (http-request-path req)))
                      (cond
                        ((string= path "/ws") :upgrade)
                        ((string= path "/jwt")
                         (let ((claims (jwt-verify (get-header req "x-token")
                                                   (list key))))
                           (make-text-response
                            200 (if claims (jwt-claim claims "sub") "refused"))))
                        ((string= path "/out")
                         (defer-to-fetch :GET "https://example.test/"
                           :then (lambda (s h b)
                                   (declare (ignore h b))
                                   (setf then-status s)
                                   (make-text-response 200 "fetched"))))
                        ((serve-static req))
                        (t (make-text-response 200 "plain")))))
                  :ws-handler (lambda (conn frame)
                                (declare (ignore conn frame))
                                nil))
               (multiple-value-bind (status headers body) (test-http-request :get "/")
                 (declare (ignore headers))
                 (check "without TLS: a plain request is answered"
                        (list status body) '(200 "plain")))
               (multiple-value-bind (socket stream) (%raw-connect)
                 (unwind-protect
                      (let ((head (string-downcase (%ws-client-upgrade stream))))
                        ;; RFC 6455 §1.3's sample key and its accept value.
                        (check "without TLS: the upgrade's accept key is SHA-1's"
                               (list (not (null (search "http/1.1 101" head)))
                                     (not (null (search "s3pplmbitxaq9kygzzhzrbk+xoo=" head))))
                               '(t t)))
                   (ignore-errors (close stream))
                   (ignore-errors (sb-bsd-sockets:socket-close socket))))
               (multiple-value-bind (status headers body)
                   (test-http-request :get "/jwt" :headers (list (cons "x-token" token)))
                 (declare (ignore headers))
                 (check "without TLS: an ES256 token verifies"
                        (list status body) '(200 "no-tls")))
               (multiple-value-bind (status headers body) (test-http-request :get "/hello.txt")
                 (check "without TLS: a static file is served with its SHA-256 ETag"
                        (list status body (cdr (assoc "etag" headers :test #'string-equal)))
                        '(200 "hello"
                          "\"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824\"")))
               (multiple-value-bind (status headers body) (test-http-request :get "/out")
                 (declare (ignore headers body))
                 (check "without TLS: an https:// fetch is refused 502"
                        (list status then-status) '(502 nil))))))
      (setf web-skeleton::*tls-outbound-setup-fn* saved-setup
            web-skeleton:*https-stream-fn* saved-stream
            (symbol-function 'sha1) saved-sha1
            (symbol-function 'sha256) saved-sha256
            (symbol-function 'web-skeleton::ecdsa-verify-p256) saved-ecdsa
            web-skeleton::*static-cache* saved-cache))))

(defun test-harness-fetch-stream-plain-write-timeout-e2e ()
  "HTTP-FETCH-STREAM over http:// gives up on an upstream that never reads
   the request, after *FETCH-TIMEOUT*.

   The body is larger than both ends' socket buffers together, so the
   write blocks once they fill. SO_SNDTIMEO bounds write(2) only; the
   upstream closes after six seconds, and the elapsed time tells a write
   that timed out from one that waited for the close."
  (format t "~%Harness: http-fetch-stream over plain HTTP, an upstream that never reads~%")
  (%call-with-scripted-upstream
   '(6)
   (lambda (port)
     (let ((body (make-array (* 24 1024 1024) :element-type '(unsigned-byte 8)
                                              :initial-element 97)))
       (multiple-value-bind (outcome secs)
           (%timed 12 (lambda ()
                        (let ((web-skeleton:*fetch-timeout* 1))
                          (http-fetch-stream
                           :post (format nil "http://127.0.0.1:~d/sink" port)
                           :body body
                           :on-line #'identity))))
         (check "non-reading upstream: the fetch raised a write timeout"
                (and (stringp outcome) (search "write timed out" outcome) t) t)
         (check "non-reading upstream: within the timeout, not at the close"
                (< secs 4.0) t))))
   :read-request nil))

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
          ;; The budget is slack for a loaded CI runner and nothing more:
          ;; all three listeners are normally present on the first read,
          ;; because MAKE-THREAD plus a bind is microseconds. Only a
          ;; failing run spends the whole budget, and a failing check
          ;; should not also be the suite's longest sleep.
          ;; Highest count seen, not the last one. A read of
          ;; /proc/net/tcp can return fewer listeners than there are,
          ;; while three healthy workers are on the port, because a
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
  "A raw socket to the live test server, plus its byte stream.

   Through CONNECT-TO-TEST-SERVER so the client family follows whatever
   WITH-TEST-SERVER bound, rather than assuming IPv4."
  (let ((socket (connect-to-test-server)))
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

   The other keepalive checks look at the response struct or the
   connection's queue — 'installed by default', 'nothing left queued',
   'counts as production' — and all of those are
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
                      (let* ((buf (read-until-bounded stream :seconds 4))
                             (raw (subseq buf 0 (fill-pointer buf)))
                             (hend (web-skeleton::scan-crlf-crlf raw 0 (length raw))))
                        ;; A check rather than a raise, so a head that never
                        ;; came fails here and the run goes on.
                        (check "sse keepalive: the head arrived"
                               (not (null hend)) t)
                        (when hend
                          (let* ((body (subseq raw (+ hend 4)))
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
                                   t)))))
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

   The upstream is the framework's own streaming surface, so this also
   checks that what the encoder produces is what the walk hands back."
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
               ;; The worker must still be able to serve someone else.
               (multiple-value-bind (status headers body)
                   (test-http-request :get "/fast")
                 (declare (ignore headers))
                 (check "stream/worker: a concurrent request is answered"
                        status 200)
                 (check "stream/worker: and answered correctly" body "fast"))
               ;; Asked after a request the one worker served, so a stream it
               ;; closed after the first chunk would be closed by now: ON-CLOSE
               ;; has not fired, and the connection is still streaming.
               (check "stream/worker: the stream is still open"
                      (list closed-reason
                            (and live-conn (web-skeleton::connection-state live-conn)))
                      '(:never :streaming)))
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

(defun %worker-ids-in (text)
  "Every worker= value in TEXT, in the order they appear.

   Collected as strings rather than parsed, so an id that came back NIL is
   captured as \"NIL\" and fails the assertion below. Parsing here would raise
   instead, and a raise ends a run rather than reporting one.

   The semicolon is why the handler writes one. These responses are
   pipelined, so the first body is followed immediately by the second
   response's status line with nothing in between: a scan for the end of the
   value ran off \"worker=0\" straight into \"HTTP\" and collected 0HTTP."
  (let ((out nil) (start 0))
    (loop
      (let ((i (search "worker=" text :start2 start)))
        (unless i (return (nreverse out)))
        (let* ((from (+ i 7))
               (to (or (position #\; text :start from) (length text))))
          (push (subseq text from to) out)
          (setf start from))))))

(defun test-worker-id-is-public-and-holds-still-per-connection ()
  "*WORKER-ID* is readable by an application, it differs between workers, and
   one connection sees one worker for its whole life.

   It is exported for a single job: attributing work to the worker that did
   it. Three claims have to hold for that to be worth anything, and they fail
   in different ways, so each is asserted separately.

   That it is readable at all. That the three workers do not all report the
   same number — asserted through :ON-TICK rather than through requests,
   because every worker ticks whether or not it has a connection, while which
   worker accepts a connection is the kernel's business and a test that
   waited for three different ones to answer would be a coin flip dressed as
   an assertion.

   And that it holds still. Two requests go down one socket below and must
   come back carrying one id. A server that migrated a connection between
   workers would answer twice and differently, and every per-connection thing
   an application hung off the id would be quietly wrong.

   The pair recorded in the tick is the fourth claim, and the cheapest one to
   get wrong: the id :ON-TICK is handed and the id bound on the thread it
   runs on must be the same number. The demo indexes one array by the
   argument and reads the special in a handler, so a disagreement would show
   up as a fan-out writing one worker's slot from another worker's thread.

   NIL off a worker is the other half of the contract, and it is an answer
   rather than an absence: the thread running this test is on no worker, and
   a caller that prints the id should say so instead of guessing zero."
  (format t "~%server: the worker a connection is on~%")
  (check "off a worker it is NIL, not 0" *worker-id* nil)
  (let ((seen nil)
        (lock (sb-thread:make-mutex)))
    (with-test-server
        (:workers 3
         :on-tick (lambda (id)
                    (sb-thread:with-mutex (lock)
                      (pushnew (cons id *worker-id*) seen :test #'equal)))
         :handler (lambda (req)
                    (declare (ignore req))
                    (make-text-response
                     200 (format nil "worker=~a;" *worker-id*))))
      (multiple-value-bind (socket stream) (%raw-connect)
        (unwind-protect
             (progn
               (write-sequence
                (sb-ext:string-to-octets
                 (concatenate 'string
                              "GET /one HTTP/1.1" *crlf*
                              "Host: localhost" *crlf* *crlf*
                              "GET /two HTTP/1.1" *crlf*
                              "Host: localhost" *crlf*
                              "Connection: close" *crlf* *crlf*)
                 :external-format :ascii)
                stream)
               (force-output stream)
               (ignore-errors
                (sb-bsd-sockets:socket-shutdown socket :direction :output))
               (let ((buf (make-array 16384 :element-type '(unsigned-byte 8)
                                            :fill-pointer 0 :adjustable t)))
                 (read-to-eof-bounded stream buf)
                 (let* ((text (sb-ext:octets-to-string
                               (subseq buf 0 (fill-pointer buf))
                               :external-format :utf-8))
                        (ids (%worker-ids-in text)))
                   ;; The control. Without it the two assertions after it
                   ;; pass on a server that answered nothing at all: one
                   ;; empty list equals another, and every id in it is a
                   ;; digit for the reason that there are none.
                   (check "both requests were answered" (length ids) 2)
                   (check "an application can read it at all"
                          (and ids (every (lambda (s)
                                            (and (plusp (length s))
                                                 (every #'digit-char-p s)))
                                          ids))
                          t)
                   (check "one socket, one worker, both times"
                          (and (= (length ids) 2)
                               (string= (first ids) (second ids)))
                          t)
                   (check "and it indexes a worker this server actually has"
                          (and ids
                               (every (lambda (s)
                                        (and (every #'digit-char-p s)
                                             (< -1 (parse-integer s) 3)))
                                      ids))
                          t))))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
      ;; Polled rather than slept: the tick rides *WORKER-WAKE-INTERVAL* and
      ;; this test has no business guessing what a reader set it to.
      (let ((deadline (+ (get-universal-time) 8)))
        (loop until (or (>= (length (sb-thread:with-mutex (lock) seen)) 3)
                        (> (get-universal-time) deadline))
              do (sleep 0.05))))
    (let ((pairs (sb-thread:with-mutex (lock) (copy-list seen))))
      (check "three workers, three different ids"
             (let ((ids (mapcar #'cdr pairs)))
               ;; Sorted only when every id is a number, so a worker whose
               ;; id is NIL fails this check rather than raising out of SORT.
               (if (every #'realp ids) (sort ids #'<) ids))
             '(0 1 2))
      (check "the id :ON-TICK is handed is the one bound on its thread"
             (and pairs (every (lambda (p) (eql (car p) (cdr p))) pairs))
             t))))

(defun test-connection-serial-does-not-repeat-when-an-fd-does ()
  "Every accepted connection gets a number this worker will not hand out
   again, which is the thing a file descriptor cannot promise.

   An fd is unique among the sockets open at this instant and no longer than
   that. The kernel gives the next accept the lowest free descriptor, so the
   fd of a connection that closed a moment ago belongs to a stranger now, and
   anything that labelled a peer by fd would label two peers the same. The
   demo puts a name on every line of a public broadcast; a name that silently
   changes hands is worse than no name.

   Three connections are opened and closed one after another below, which is
   precisely the shape that reuses an fd. Whether a given run actually reuses
   one is the kernel's business and is reported rather than asserted — what
   is asserted is that the serials did not repeat regardless, because that is
   the claim the field makes.

   Collected from :ON-TICK rather than from a handler: an HTTP handler is
   given a request and never sees the connection it arrived on, so the tick,
   which runs with this worker's table bound, is the only place an
   application can look at one.

   Keyed on the connection object, and that is load-bearing. Pairs of
   (fd . serial) deduplicated by EQUAL would collapse a server handing every
   connection the same serial to one pair per fd, and turn the control
   counting observations red with the property it is meant to be
   independent of. A mutation that turns the control red cannot be read: it
   says the observation broke, not that the property did. Here the count of
   connections seen does not depend on the serials at all.

   The fd is captured when the connection is seen rather than read back
   later, because CONNECTION-CLOSE sets it to -1. The serial it leaves alone.

   There is no assertion that the serials increase: comparing a sorted list
   with a sorted copy of itself is true of any list whatever. Distinct
   serials, and a highest serial no lower than the number of connections,
   are the claims a repeating or constant counter actually fails."
  (format t "~%server: a connection number that is not an fd~%")
  (let ((seen nil)
        (lock (sb-thread:make-mutex))
        (saved *worker-wake-interval*))
    (unwind-protect
         (progn
           ;; Set before the workers start so they pick it up on their first
           ;; pass rather than after one second of the old value.
           (setf *worker-wake-interval* 0.05)
           (with-test-server
               (:workers 1
                :on-tick
                (lambda (id)
                  (declare (ignore id))
                  (maphash
                   (lambda (fd conn)
                     (sb-thread:with-mutex (lock)
                       (unless (assoc conn seen :test #'eq)
                         (push (cons conn fd) seen))))
                   web-skeleton::*connections*))
                :handler (lambda (req)
                           (declare (ignore req))
                           (make-text-response 200 "ok")))
             ;; Connecting is enough: a connection is registered at accept,
             ;; and no request has to be made for it to exist. Held open for
             ;; a few ticks so the hook above is certain to have seen it.
             (dotimes (i 3)
               (multiple-value-bind (socket stream) (%raw-connect)
                 (declare (ignore stream))
                 (sleep 0.25)
                 (ignore-errors (sb-bsd-sockets:socket-close socket))
                 ;; And closed before the next one opens, so the fd it was
                 ;; using is free when the next accept asks for one.
                 (sleep 0.15)))))
      (setf *worker-wake-interval* saved))
    (let* ((pairs (sb-thread:with-mutex (lock) (copy-list seen)))
           (serials (mapcar (lambda (p) (connection-serial (car p))) pairs))
           (fds (mapcar #'cdr pairs)))
      (format t "  (observed ~d connection~:p across ~d distinct fd~:p)~%"
              (length pairs) (length (remove-duplicates fds)))
      ;; The control, and it counts connection objects, so nothing about the
      ;; serials can move it. Every assertion after it is true of an empty
      ;; list, so without it the test passes on a server that accepted
      ;; nothing and a hook that never ran. At least three rather than
      ;; exactly three: WITH-TEST-SERVER probes the port to know it is up,
      ;; and those are accepted connections like any other.
      (check "at least the three opened here were seen"
             (>= (length pairs) 3) t)
      (check "no serial was handed out twice"
             (length (remove-duplicates serials)) (length serials))
      (check "and they count accepts, so the highest is at least the count"
             (and serials (>= (reduce #'max serials) (length serials))) t))))

(defun test-connection-serial-survives-a-worker-restart ()
  "A worker that crashes and restarts keeps counting its accepts, so no two
   connections the server accepts share a name.

   The serial names a connection, and the demo prints that name on a public
   line. Bound beside the other per-worker slots, it began again at one on
   every restart, so a crashed worker's next peer took the name of its first.

   The crash is real rather than simulated. The hook duplicates /dev/null over
   the worker's own epoll fd, once; the next pass's epoll_wait fails with
   EINVAL, because that number no longer names an epoll instance, nothing in
   RUN-EVENT-LOOP catches it, and RUN-WORKER takes the restart path it takes
   for any error that escapes — the path its docstring records a shipping
   server taking. So this is also the first test that watches a worker come
   back at all.

   Duplicated over, not closed. RUN-WORKER's cleanup closes the epoll fd
   itself, so closing it here first would free the number for whatever opens
   a descriptor next and leave the cleanup to close that instead — the reuse
   race any close by number runs. dup2 keeps the number owned until the
   cleanup's own close.

   The restart is detected by the connection table, not assumed from the
   close. RUN-WORKER binds a fresh table on every pass, so the hook seeing a
   second distinct table is proof a restart happened; counting ticks after the
   close would not be, because a worker that somehow survived the close would
   go on ticking too. The log line is a second, independent witness."
  (format t "~%server: a serial survives the worker restarting~%")
  (let ((seen nil)
        (tables nil)
        (lock (sb-thread:make-mutex))
        (crash nil)
        (log (make-string-output-stream))
        (saved-stream web-skeleton::*log-stream*))
    ;; Global SETF: the worker is a thread START-SERVER spawns and reads the
    ;; global value, so a binding here would capture nothing.
    (setf web-skeleton::*log-stream* log)
    (unwind-protect
         (with-test-server
             (:workers 1
              :on-tick
              (lambda (id)
                (declare (ignore id))
                (sb-thread:with-mutex (lock)
                  (let ((table web-skeleton::*connections*))
                    (unless (member table tables :test #'eq)
                      (push table tables))
                    (maphash (lambda (fd conn)
                               (unless (assoc conn seen :test #'eq)
                                 (push (cons conn fd) seen)))
                             table))
                  (when crash
                    (setf crash nil)
                    (with-open-file (devnull "/dev/null")
                      (unless (= (sb-alien:alien-funcall
                                  (sb-alien:extern-alien
                                   "dup2" (function sb-alien:int sb-alien:int
                                                    sb-alien:int))
                                  (sb-sys:fd-stream-fd devnull)
                                  web-skeleton::*epoll-fd*)
                                 web-skeleton::*epoll-fd*)
                        (error "dup2 over the epoll fd failed"))))))
              :handler (lambda (req)
                         (declare (ignore req))
                         (make-text-response 200 "ok")))
           (flet ((hold-one ()
                    ;; Retried, because across the restart there is a second
                    ;; with no listener and a refused connect is expected.
                    ;; Held open for a few ticks so the hook is certain to
                    ;; see it, then closed.
                    (let ((deadline (+ (get-universal-time) 8)))
                      (loop
                        (let ((socket (ignore-errors (%raw-connect))))
                          (when socket
                            (sleep 0.25)
                            (ignore-errors (sb-bsd-sockets:socket-close socket))
                            (sleep 0.15)
                            (return t)))
                        (when (> (get-universal-time) deadline) (return nil))
                        (sleep 0.1))))
                  (tables-seen ()
                    (sb-thread:with-mutex (lock) (length tables))))
             (check "control: a connection was accepted before the crash"
                    (hold-one) t)
             (let ((before (sb-thread:with-mutex (lock) (copy-list seen))))
               (sb-thread:with-mutex (lock) (setf crash t))
               ;; The restart backs off a second before rebinding; wait for
               ;; the second table rather than guessing at the second.
               (let ((deadline (+ (get-universal-time) 8)))
                 (loop until (or (>= (tables-seen) 2)
                                 (> (get-universal-time) deadline))
                       do (sleep 0.05)))
               (check "control: the worker crashed and came back"
                      (tables-seen) 2)
               (check "control: a connection was accepted after the restart"
                      (hold-one) t)
               (let* ((after (sb-thread:with-mutex (lock)
                               (remove-if (lambda (p)
                                            (assoc (car p) before :test #'eq))
                                          seen)))
                      (before-serials
                        (mapcar (lambda (p) (connection-serial (car p))) before))
                      (after-serials
                        (mapcar (lambda (p) (connection-serial (car p))) after)))
                 (format t "  (serials before the crash ~a, after ~a)~%"
                         (sort (copy-list before-serials) #'<)
                         (sort (copy-list after-serials) #'<))
                 (check "every serial after the restart is above every one before"
                        (and before-serials after-serials
                             (> (reduce #'min after-serials)
                                (reduce #'max before-serials)))
                        t)))))
      (setf web-skeleton::*log-stream* saved-stream))
    (check "control: the log names the crash"
           (not (null (search "crashed" (get-output-stream-string log))))
           t)))

(defun test-harness-pipelined-after-body-e2e ()
  "A request carrying a Content-Length body, with a second request
   pipelined behind it.

   The keep-alive reset has to shift from past the *body*, and this is the
   only shape where that differs from shifting past the headers: with no
   body the two are the same offset, so TEST-HARNESS-PIPELINED-WITH-FIN-E2E
   — two GETs — cannot tell a boundary that forgets the body from one that
   does not.

   What each assertion catches, against three sabotaged boundaries:

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
                   ;; the client sent no body with. The handler prints this
                   ;; value, and this is the assertion that reads it.
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
   no framing there, and the connection never answers, which fails this
   test's `server closed the connection` and `second request
   dispatched`.

   That holds because the cursor has one clear. With a second — the
   connection resets zeroing it as well — neutering either one would be
   silent: each would mask the other, and this test would detect neither.
   Two mechanisms, one guarantee, no way to tell them apart.

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

   The point of refusing trailers, and a delivery-shaped assertion cannot
   see it: if the smuggle succeeded the client would get
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
                   ;; The assertion that matters most: not two responses.
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
                            ;; unconditionally: this test's subject
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
                               t))
                      ;; The same refusal from an operator's side. One event,
                      ;; two call sites — the accept turned away and the 503
                      ;; sent for it — and either can be deleted without the
                      ;; other noticing, so each gets its own line. Polled for
                      ;; both at once, so the wait cannot end on half.
                      (let ((k nil) (deadline (+ (get-universal-time) 6)))
                        (loop until (or (and k
                                             (>= (getf k :refused 0) 1)
                                             (>= (getf k :server-error 0) 1))
                                        (> (get-universal-time) deadline))
                              do (setf k (getf (web-skeleton:connection-census)
                                               :counters))
                                 (sleep 0.05))
                        (check "the census counts the accept it refused"
                               (>= (getf k :refused 0) 1) t)
                        (check "and the 503 as a response, though no handler ran"
                               (>= (getf k :server-error 0) 1) t))))
               (when holder
                 (ignore-errors (sb-bsd-sockets:socket-close holder))))))
      (setf web-skeleton::*max-connections* saved))))

(defun test-harness-refusals-are-reported-not-logged-each ()
  "A flood of refusals at *MAX-CONNECTIONS* is logged once, and counted
   in full by the census.

   Each refusal logged its own WARN, and every log line holds the one lock
   all workers share across a format and a flush: a flood arrives exactly
   when a worker is out of room, and a line per refusal had every worker
   queueing on logging. The first refusal is logged at once and the rest
   are reported at most once a minute, so in a test well inside that
   minute fifty refusals are one line. The census is what counts them."
  (format t "~%Harness: refusals are reported, not logged one by one~%")
  (let ((saved web-skeleton::*max-connections*)
        (saved-stream web-skeleton::*log-stream*)
        (log (make-string-output-stream)))
    (setf web-skeleton::*max-connections* 1
          web-skeleton::*log-stream* log)
    (unwind-protect
         (with-test-server
             (:handler (lambda (req)
                         (declare (ignore req))
                         (make-text-response 200 "")))
           (let ((holder (claim-connection-slot)))
             (unwind-protect
                  (progn
                    (check "control: the one slot is taken" (not (null holder)) t)
                    (when holder
                      (let ((refused (loop repeat 50
                                           count (eql (values (request-expecting-refusal))
                                                      503))))
                        (check "control: fifty connections were refused" refused 50))
                      (let ((k nil) (deadline (+ (get-universal-time) 6)))
                        (loop until (or (and k (>= (getf k :refused 0) 50))
                                        (> (get-universal-time) deadline))
                              do (setf k (getf (web-skeleton:connection-census)
                                               :counters))
                                 (sleep 0.05))
                        (check "the census counts every refusal"
                               (>= (getf k :refused 0) 50) t))))
               (when holder
                 (ignore-errors (sb-bsd-sockets:socket-close holder))))))
      (setf web-skeleton::*max-connections* saved
            web-skeleton::*log-stream* saved-stream))
    (let* ((text (get-output-stream-string log))
           (lines (loop with start = 0
                        for i = (search "connection limit reached" text :start2 start)
                        while i count t do (setf start (1+ i)))))
      (check "and the log says so once, not fifty times" lines 1))))

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
  "The relay DEPLOYMENT.md describes, end to end — and through the real
   resolver.

   A streaming response whose :ON-OPEN starts a fetch, forwards each chunk
   into its own body as the framing proves it, and closes when the upstream
   is done. A continuation built inside :ON-OPEN and dropped would leave
   the client a 200 with chunked framing that never terminated, held for
   *STREAM-IDLE-TIMEOUT*, then a truncated body with no error anywhere.

   Asserted on the decoded body rather than on bytes arriving, because
   that failure's signature is a body that never terminates — a check that
   only looked for content would pass against it.

   The upstream is dialled by **name**, which is what makes this the only
   e2e in the suite to reach INITIATE-DNS-LOOKUP on its success path. Every
   other FETCH-INTO here dials an IP literal and takes INITIATE-HTTP-FETCH's
   PARSE-IPV4-LITERAL fast path straight past it, so a defect in that
   function would live behind a green suite.

   Both ends are moved together rather than one end being assumed. The
   listener binds whatever RESOLVE-HOST-BLOCKING answers for the same name
   the fetch will dial, and CONNECT-TO-TEST-SERVER dispatches the client
   socket on that vector's length exactly as MAKE-TCP-LISTENER dispatches
   the bind. So the family is whatever the host's resolver prefers, and
   agreement is by construction rather than by assumption: a host whose
   /etc/hosts orders `::1 localhost` first binds and dials v6, one that
   orders IPv4 first binds and dials v4, and neither can reach a listener
   that is not there. Pinning either end to a literal is what would make
   this environment-dependent.

   That the name resolves at all is asserted rather than skipped on. A
   machine without it cannot run the rest, and a test that quietly does not
   run is worse than one that fails saying why."
  (format t "~%Harness: fetch-into, the documented relay~%")
  (let* ((port-box (list nil))
         (then-fires 0)
         ;; The framework's own resolver, so the listener and the fetch
         ;; cannot disagree about the family: whatever this answers is what
         ;; INITIATE-DNS-LOOKUP will answer for the same name a moment
         ;; later, out of the same getent.
         (host-vec (web-skeleton::resolve-host-blocking "localhost")))
    ;; Asserted rather than skipped on. A machine where this name does not
    ;; resolve cannot run the rest, and a test that quietly does not run is
    ;; the failure mode this suite spends its docstrings avoiding — so it
    ;; fails here, by name, ahead of the assertions that would fail
    ;; confusingly.
    (check "fetch-into relay: the name the fetch will dial resolves"
           (and host-vec t) t)
    (with-test-server
        (:host host-vec
         :handler
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
                    :get (format nil "http://localhost:~d/up"
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

(defun test-harness-fetch-into-relays-to-a-slow-client-e2e ()
  "A relay into a client slower than its upstream delivers the whole body,
   pausing the way DEPLOYMENT.md says to: when STREAM-SEND answers NIL.

   The upstream sends a hundred chunks of 8 KiB at once, and the client
   says nothing for a second before it reads. The target's send buffer and
   the client's receive buffer are both shrunk to a few KiB, and
   *MAX-WRITE-BACKLOG* to 64 KiB, so the relay is far ahead of the client
   almost at once. The pause has to take effect at the chunk it was given
   for. A pass carries many chunks, and each one handed over after the
   verdict would land in the queue the pause is protecting, until
   STREAM-SEND refused one and the fetch failed with the stream left
   unterminated.

   Asserted on the decoded body, on how the stream ended, and on the pause
   count. The count is the non-vacuity check: with no pause at all, a body
   arriving whole would show only that the buffers were larger than this
   arranged."
  (format t "~%Harness: fetch-into, relaying to a client that reads slowly~%")
  (let* ((pauses 0)
         (ended :never)
         (chunk-size 8192)
         (payloads (loop for i below 100
                         collect (make-string chunk-size
                                              :initial-element
                                              (code-char (+ 65 (mod i 26))))))
         (expected (apply #'concatenate 'string payloads))
         (script (append (list (%crlf-lines "HTTP/1.1 200 OK"
                                      "Transfer-Encoding: chunked" ""))
                         (loop for p in payloads
                               collect (%crlf-lines (format nil "~x" chunk-size) p))
                         (list (%crlf-lines "0" ""))))
         (saved-backlog web-skeleton:*max-write-backlog*)
         (saved-ws-message web-skeleton:*max-ws-message-size*))
    ;; Global, not bound: the worker threads read them, and a LET here
    ;; would be invisible to them. START-SERVER refuses a backlog that
    ;; cannot hold a maximal WebSocket message, so that cap comes down too.
    (setf web-skeleton:*max-write-backlog* (* 64 1024)
          web-skeleton:*max-ws-message-size* (* 32 1024))
    (unwind-protect
         (%call-with-scripted-upstream
          script
          (lambda (up-port)
            (with-test-server
                (:handler
                 (lambda (req)
                   (declare (ignore req))
                   (make-stream-response
                    :on-open
                    (lambda (client)
                      ;; The kernel doubles a send buffer and keeps its own
                      ;; floor, so this asks for as small as it will give.
                      (web-skeleton::set-socket-option-int
                       (web-skeleton::connection-fd client)
                       web-skeleton::+sol-socket+ +so-sndbuf+ 4096)
                      (fetch-into
                       client
                       (http-fetch
                        :get (format nil "http://127.0.0.1:~d/body" up-port)
                        :on-body (lambda (out chunk)
                                   (declare (ignore out))
                                   (unless (stream-send client chunk)
                                     (incf pauses)
                                     :pause))
                        :then (lambda (status headers body)
                                (declare (ignore headers body))
                                (when status (stream-close client))
                                nil))))
                    :on-close (lambda (c reason)
                                (declare (ignore c))
                                (setf ended reason)))))
              (let ((socket (make-instance (if (= (length *test-host*) 16)
                                               'sb-bsd-sockets:inet6-socket
                                               'sb-bsd-sockets:inet-socket)
                                           :type :stream :protocol :tcp)))
                ;; The receive buffer, set before the connect so the window the
                ;; server sees is the small one from the start.
                (web-skeleton::set-socket-option-int
                 (sb-bsd-sockets:socket-file-descriptor socket)
                 web-skeleton::+sol-socket+ +so-rcvbuf+ 4096)
                (sb-bsd-sockets:socket-connect socket *test-host* *test-port*)
                (let ((stream (sb-bsd-sockets:socket-make-stream
                               socket :input t :output t
                               :element-type '(unsigned-byte 8))))
                  (unwind-protect
                       (progn
                         (%send-raw-get stream "/relay" :extra
                                        (format nil "Connection: close~c~c"
                                                #\Return #\Newline))
                         ;; Behind from the start.
                         (sleep 1)
                         (let* ((buf (read-until-bounded stream :seconds 20))
                                (raw (subseq buf 0 (fill-pointer buf)))
                                (body (%decode-streamed-body raw)))
                           (check "slow client: the whole body arrived, framed and in order"
                                  (list (length body) (equal body expected))
                                  (list (length expected) t))))
                    (ignore-errors (close stream))
                    (ignore-errors (sb-bsd-sockets:socket-close socket)))))
              (check "slow client: the relay paused on the client's queue"
                     (plusp pauses) t)
              (check "slow client: and the stream ended as a finished one"
                     ended :done))))
      (setf web-skeleton:*max-write-backlog* saved-backlog
            web-skeleton:*max-ws-message-size* saved-ws-message))))

(defun test-harness-fetch-into-dns-failure-e2e ()
  "A detached fetch to a name that will not resolve, end to end, through the
   real resolver.

   One of two e2e tests that reach INITIATE-DNS-LOOKUP at all. Every other
   e2e FETCH-INTO dials an IP literal, which takes INITIATE-HTTP-FETCH's
   fast path on PARSE-IPV4-LITERAL and never enters the function, so a
   defect there would live behind a green suite. Every unit detector for
   that function drives the seam; this
   one spawns getent.

   RFC 2606 reserves .invalid, so the lookup fails on every machine and
   reaches no network.

   The success half is TEST-HARNESS-FETCH-INTO-RELAY-E2E, which dials a
   resolvable name against a listener bound to whatever that same name
   resolves to. Two tests rather than one because the halves want opposite
   things from the resolver: this one wants a name that fails everywhere,
   and that one wants a name that succeeds everywhere.

   The assertion is the *second* fetch. :THEN receives the abort sentinel
   and immediately starts another FETCH-INTO on the same connection, which
   FETCH-INTO refuses on either of two guards if the DNS phase mishandled
   the target: on state, if the lookup parked a :STREAMING connection into
   :AWAITING and nothing moved it back; and on FETCH-OUTSTANDING, if the
   failure path never released the marker. So a body arriving at all is
   both defects not having happened — asserted by their consequence, which
   is the thing an application would actually hit, rather than by their
   mechanism, which the unit detectors already cover.

   Chaining from an aborted :THEN is a supported shape, not a trick: the
   new marker suppresses DELIVER-DETACHED's disposition on the way out, the
   same interaction TEST-HARNESS-FETCH-INTO-CHAINED-E2E covers from a
   delivered one."
  (format t "~%Harness: fetch-into, a name that will not resolve~%")
  (let ((port-box (list nil))
        (abort-status :unset)
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
                    :get "http://nxdomain.invalid/"
                    :then
                    (lambda (status headers body)
                      (declare (ignore headers body))
                      (incf then-fires)
                      (setf abort-status status)
                      (fetch-into
                       client
                       (http-fetch
                        :get (format nil "http://127.0.0.1:~d/up"
                                     (first port-box))
                        :on-body (lambda (out chunk)
                                   (declare (ignore out))
                                   (stream-send client chunk)
                                   nil)
                        :then (lambda (s h b)
                                (declare (ignore s h b))
                                (stream-close client)
                                nil)))
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
                 (check "dns failure e2e: the chained fetch's body arrived, framed"
                        (%decode-streamed-body raw) "alphabeta")))
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
      (check "dns failure e2e: :then saw the abort sentinel" abort-status nil)
      (check "dns failure e2e: :then fired once for the failed lookup"
             then-fires 1)
      (check "dns failure e2e: no outbound left behind"
             (census-await :outbound 0) 0))))

(defparameter +v6-loopback+ #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
  "IPv6 loopback as START-SERVER's :HOST wants it — sixteen bytes, which is
   what MAKE-TCP-LISTENER dispatches :INET6 on.")

(defun %ipv6-loopback-available-p ()
  "Whether ::1 can be bound here at all.

   Asked by binding rather than by reading configuration: a container with
   IPv6 disabled in the kernel, or with the loopback address absent, fails
   at the bind and that is the only answer that matters."
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:inet6-socket
                                   :type :stream :protocol :tcp)))
        (unwind-protect
             (progn (sb-bsd-sockets:socket-bind socket +v6-loopback+ 0) t)
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
    (error () nil)))

(defun test-harness-ipv6-listener-e2e ()
  "The listener binds IPv6, and the harness can reach it.

   MAKE-TCP-LISTENER dispatches the socket family on the host vector's
   length, and README documents all four forms. This is the test that
   passes it sixteen bytes, through the harness's :HOST plumbing.

   Not a detector for a defect in the framework, and it should not be read
   as one: the v6 bind is right with or without it. It is a detector for
   the *harness* code, which nothing else exercises —
   CONNECT-TO-TEST-SERVER dispatching the client family from the same
   vector MAKE-TCP-LISTENER dispatches the bind from. Pin that branch to
   INET-SOCKET and this is the only test that notices.

   The other e2e tests cannot reach it. Their :HOST is either the literal
   four-byte loopback or whatever RESOLVE-HOST-BLOCKING answers for
   `localhost`, and that is IPv4 on any machine ordering IPv4 first in
   /etc/hosts, the GitHub ubuntu runner among them. Depending on the
   resolver to hand us a v6 address is exactly the environment dependence
   the relay test exists to avoid, so the address is named outright here.

   Skipped, visibly, where ::1 cannot be bound — a kernel with IPv6 off, or
   a container without the loopback address. Same shape as the TLS suite
   skipping without libssl: a printed SKIP rather than a silent pass, so a
   run that did not execute this says so."
  (format t "~%Harness: the listener binds IPv6~%")
  (if (not (%ipv6-loopback-available-p))
      (skip "ipv6 listener: ::1 cannot be bound on this machine")
      (with-test-server (:host +v6-loopback+
                         :handler (lambda (req)
                                    (declare (ignore req))
                                    (make-text-response 200 "v6-ok")))
        (multiple-value-bind (status headers body)
            (test-http-request :get "/v6")
          (declare (ignore headers))
          (check "ipv6 listener: the bound address is the v6 one"
                 (length *test-host*) 16)
          (check "ipv6 listener: it answered over IPv6" status 200)
          (check "ipv6 listener: and the body is ours" body "v6-ok")))))

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
  "The six refusals, each asserted on its own.

   They fail in different directions and a single \"it signals\" check
   would cover one of them by accident. And a refused call must leave
   nothing behind — no outbound, and a callback that never fired — which
   is the one place among the FETCH-INTO tests where zero is the right
   answer for a counter.

   The two disposition refusals bring a seventh check that is not a
   refusal: the same argument on the target it is meant for has to get
   past them. A guard that refuses everything passes every test written
   only about what it rejects."
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
      ;; On this worker's table, or the table guard ahead of this one
      ;; refuses it first; and asserted on the text, so that guard cannot
      ;; pass for this one.
      (let ((conn (web-skeleton::make-connection
                   :fd 1 :state :streaming :fetch-outstanding t))
            (web-skeleton::*epoll-fd* 99)
            (web-skeleton::*connections* (make-hash-table :test #'eql)))
        (web-skeleton::register-connection conn)
        (check "refusal: a second outstanding fetch signals"
               (handler-case (progn (fetch-into conn (cont)) :no-signal)
                 (error (e) (and (search "already has" (princ-to-string e)) :outstanding)))
               :outstanding))
      ;; The three disposition refusals assert on the error's *text* where
      ;; the four above assert only that something signalled. Not a style
      ;; difference: these three fail into each other. With no event loop
      ;; bound, an implementation missing a disposition guard signals
      ;; anyway — from the guard below it — so ":signalled" would be
      ;; satisfied by the wrong refusal and the check would pass against
      ;; the defect. The text names which guard fired.
      ;;
      ;; No event loop is also what keeps this cheap: every call here stops
      ;; at a guard, so nothing dials, registers, or leaks a descriptor.
      (flet ((refusal-text (disposition state)
               (let ((conn (web-skeleton::make-connection :fd 1 :state state))
                     (web-skeleton::*epoll-fd* nil))
                 (handler-case
                     (progn (fetch-into conn (cont)
                                        :failure-disposition disposition)
                            :no-signal)
                   (error (e) (princ-to-string e)))))
             (mentions (text want)
               ;; STRINGP first. Without it a call that did not signal
               ;; returns a keyword, SEARCH raises a type error outside any
               ;; check, and the run ends with no failure list instead of
               ;; reporting one.
               (and (stringp text) (search want text) t)))
        ;; An unrecognized value. Refused rather than defaulted: a caller
        ;; who wrote :KEPT and got the framework's own disposition asked
        ;; for an opt-out and silently did not receive one.
        (check "refusal: an unknown :failure-disposition names itself"
               (mentions (refusal-text :kept :websocket) "failure-disposition")
               t)
        ;; :KEEP on a :STREAMING target. Refused at the call rather than
        ;; ignored at delivery: there the disposition is the framing, and
        ;; honouring it would hand the client a body with no terminator.
        (check "refusal: :keep on a stream names the disposition"
               (mentions (refusal-text :keep :streaming) "failure-disposition")
               t)
        ;; And the same call on the target it is for gets past both guards
        ;; and stops at the next one. Without this the two checks above
        ;; would pass against a FETCH-INTO that refused :KEEP everywhere,
        ;; which is a different function with the same test results.
        (check "refusal: :keep on a websocket reaches the later guards"
               (mentions (refusal-text :keep :websocket) "no event loop")
               t))
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
      ;; What is worth asserting here is the property these share that the
      ;; marker cannot show: refusing did not consume the continuation. One
      ;; continuation, held, goes through two refusals in turn and comes
      ;; out of both as it went in, its sink untouched, so the second
      ;; refusal is the guard's own and not one for a fetch already spent.
      (let* ((same (cont))
             (sink-before (web-skeleton::http-fetch-continuation-sink same))
             (wrong-state (web-skeleton::make-connection :fd 1 :state :read-http))
             (outstanding (web-skeleton::make-connection
                           :fd 1 :state :streaming :fetch-outstanding t))
             (web-skeleton::*epoll-fd* 99)
             (web-skeleton::*connections* (make-hash-table :test #'eql)))
        (web-skeleton::register-connection outstanding)
        (check "refusal: refusing does not consume the continuation"
               (list (handler-case (progn (fetch-into wrong-state same) :no-signal)
                       (error (e) (and (search "state" (princ-to-string e)) :wrong-state)))
                     (handler-case (progn (fetch-into outstanding same) :no-signal)
                       (error (e) (and (search "already has" (princ-to-string e))
                                       :outstanding)))
                     (eq (web-skeleton::http-fetch-continuation-sink same) sink-before))
               '(:wrong-state :outstanding t))
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

   The arm has other coverage, and saying otherwise would be an
   overstatement: TEST-HARNESS-FETCH-INTO-UPSTREAM-STALLS-E2E fails when
   it is removed, because the sentinel does not arrive. What that test
   cannot see is everything after the callback — it has no way to look
   inside the worker's connection table. So what this one adds is the
   reclamation rather than the notification: the outbound is actually
   unregistered, the callback fired exactly once rather than merely at
   least once, and the target got DELIVER-DETACHED's disposition instead
   of being left open behind a body that stopped.

   **The target is deliberately left alive.** Closing it would prove
   nothing: CLOSE-CONNECTION walks a dying target's detached outbounds and
   reaps them itself, so every assertion below would pass with the
   sweeper's arm deleted outright. That walk is
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

(defun test-harness-detached-deadline-spares-a-draining-pause ()
  "A paused detached outbound past its deadline is left alone while the
   target it relays into still has a backlog draining.

   The pause is waiting on that drain, and the drain is the client's to
   take: a slow link emptying a full queue can outlast *FETCH-TIMEOUT*, and
   reaping the relay then cuts a healthy body short. The target's
   *WRITE-STALL-TIMEOUT* bounds it instead — a queue that stops moving
   closes the target, and the target's teardown reaps the outbound.
   TEST-HARNESS-DETACHED-DEADLINE-SWEEP is the other half: the same
   fixture over an empty queue, where nothing is coming and the deadline
   still reaps it.

   The target's own fate is asserted too. Its queue moved a moment ago,
   so neither the stall arm nor the stream's idle clock may take it,
   though it produced nothing for longer than *STREAM-IDLE-TIMEOUT*."
  (format t "~%Harness: a paused relay with a draining target, past its deadline~%")
  (let* ((fires 0)
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
                         :stream-framing :chunked
                         ;; Produced nothing for an hour: only the moving
                         ;; queue keeps this stream from being idle.
                         :stream-produced-at (- (get-universal-time) 3600)
                         :last-active (get-universal-time)))
                (out (web-skeleton::make-connection
                      :fd (web-skeleton::socket-fd out-sock)
                      :socket out-sock
                      :state :out-read
                      :outbound-p t
                      :fetch-sink :detached
                      :fetch-method :GET
                      :fetch-paused t
                      :fetch-paused-at (- (get-universal-time) 120)
                      :fetch-started-at (- (get-universal-time) 600)
                      :fetch-deadline (- (get-universal-time) 300)
                      :inbound-fd (web-skeleton::socket-fd target-sock)
                      :last-active (get-universal-time)
                      :fetch-callback
                      (lambda (status headers body)
                        (declare (ignore status headers body))
                        (incf fires)
                        nil))))
           (web-skeleton::register-connection target)
           (web-skeleton::register-connection out)
           ;; The backlog, queued now, so its progress clock is fresh.
           (web-skeleton::connection-append-write
            target (sb-ext:string-to-octets "draining" :external-format :ascii))
           (check "draining pause: the target has a backlog"
                  (plusp (web-skeleton::connection-write-pending target)) t)
           (web-skeleton::sweep-idle-connections epfd (get-universal-time))
           (check "draining pause: the outbound is still registered"
                  (and (web-skeleton::lookup-connection
                        (web-skeleton::connection-fd out))
                       t)
                  t)
           (check "draining pause: and its callback has not fired" fires 0)
           (check "draining pause: the target stream is still open"
                  (and (web-skeleton::lookup-connection
                        (web-skeleton::connection-fd target))
                       t)
                  t))
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
                             (not (null (search "partial"
                                                (sb-ext:octets-to-string
                                                 raw :external-format :latin-1))))
                             t)
                      ;; The property. A terminator here would claim the
                      ;; body was whole, and the decode would succeed;
                      ;; failing, it answers its error's text.
                      (check "upstream stall: the client's decode fails"
                             (not (null (search "chunked" decoded)))
                             t)
                      (check "upstream stall: decode did not succeed short"
                             (string= decoded "partial") nil)))
               (ignore-errors (close stream))
               (ignore-errors (sb-bsd-sockets:socket-close socket))))
           (check "upstream stall: :then saw the abort sentinel" aborted t)
           (check "upstream stall: no outbound left behind"
                  (census-await :outbound 0) 0))
      (setf web-skeleton:*fetch-timeout* saved))))

(defun test-harness-fetch-into-refuses-malformed-chunks-e2e ()
  "A relay refuses an upstream body that a buffered fetch refuses.

   :ON-BODY is handed each chunk as the walk proves it, and nothing reads
   a relayed body after that: COMPLETE-FETCH does not decode what has
   already been delivered. The walk is the only judge such a body meets,
   so it has to refuse what DECODE-CHUNKED-BODY refuses — here, chunk data
   followed by two bytes that are not its CRLF. Accepted, the relay would
   forward `hello` as a whole body, terminator and all, where a buffered
   fetch of the same upstream answers 502, and it would drop the two bytes
   it took for a CRLF.

   The upstream is a byte-vector response from the test server itself, so
   the framing reaches the wire exactly as written."
  (format t "~%Harness: fetch-into refuses framing the decoder refuses~%")
  (let ((port-box (list nil))
        (handed nil)
        (outcome nil))
    (with-test-server
        (:handler
         (lambda (req)
           (if (search "/up" (http-request-path req))
               (%ascii (format nil "HTTP/1.1 200 OK~a~
                                    Transfer-Encoding: chunked~a~
                                    Connection: close~a~a~
                                    5~ahelloXX0~a~a"
                               *crlf* *crlf* *crlf* *crlf*
                               *crlf* *crlf* *crlf*))
               (make-stream-response
                :on-open
                (lambda (client)
                  (fetch-into
                   client
                   (http-fetch
                    :get (format nil "http://127.0.0.1:~d/up" (first port-box))
                    :on-body (lambda (out chunk)
                               (declare (ignore out))
                               (push chunk handed)
                               (stream-send client chunk)
                               nil)
                    :then (lambda (status headers body)
                            (declare (ignore headers body))
                            (setf outcome (if status :delivered :aborted))
                            nil))))))))
      (setf (first port-box) *test-port*)
      (multiple-value-bind (socket stream) (%raw-connect)
        (unwind-protect
             (progn
               (%send-raw-get stream "/relay" :extra
                              (format nil "Connection: close~a" *crlf*))
               (let* ((buf (read-until-bounded stream :seconds 5))
                      (raw (subseq buf 0 (fill-pointer buf))))
                 (check "malformed upstream: none of its body reached the client"
                        (search "hello" (sb-ext:octets-to-string
                                         raw :external-format :latin-1))
                        nil)
                 (check "malformed upstream: the client's body does not decode"
                        (string= (%decode-streamed-body raw) "hello")
                        nil)))
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
      (check "malformed upstream: :on-body was handed nothing" handed nil)
      (check "malformed upstream: :then saw the abort sentinel" outcome :aborted)
      (check "malformed upstream: no outbound left behind"
             (census-await :outbound 0) 0))))

(defun %raw-exchange (bytes &key (seconds 5) (half-close t))
  "Send BYTES on a fresh connection to the test server and return all it
   answered before closing, as a latin-1 string. HALF-CLOSE NIL keeps the
   write side open, for a client that is still owed something — the one
   that sent Expect: 100-continue and is waiting to be told to go on."
  (multiple-value-bind (socket stream) (%raw-connect)
    (unwind-protect
         (progn
           (write-sequence bytes stream)
           (force-output stream)
           (when half-close
             (ignore-errors
              (sb-bsd-sockets:socket-shutdown socket :direction :output)))
           (let ((buf (read-until-bounded stream :seconds seconds)))
             (sb-ext:octets-to-string (subseq buf 0 (fill-pointer buf))
                                      :external-format :latin-1)))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun %post-bytes (path body &key (headers '()) chunked (version "1.1"))
  "A POST of BODY to PATH, framed by Content-Length, or as one chunk and
   the terminator when CHUNKED, with HEADERS added. Bytes, ready to send."
  (%ascii
   (with-output-to-string (s)
     (format s "POST ~a HTTP/~a~aHost: localhost~a" path version *crlf* *crlf*)
     (if chunked
         (format s "Transfer-Encoding: chunked~a" *crlf*)
         (format s "Content-Length: ~d~a" (length body) *crlf*))
     (dolist (h headers) (format s "~a~a" h *crlf*))
     (format s "~a" *crlf*)
     (if chunked
         (format s "~x~a~a~a0~a~a" (length body) *crlf* body *crlf* *crlf* *crlf*)
         (format s "~a" body)))))

(defun test-harness-body-handler-e2e ()
  "A body handler, end to end: asked at the head with the body not yet
   read, and obeyed — the default, a cap of the request's own either side
   of *MAX-BODY-SIZE*, or an answer before the body.

   *MAX-BODY-SIZE* is 16 for the run, so a 40-byte body is over the
   default and under the 64 /big chooses. The handler answers the path and
   the body's length, which says both which request it was handed and how
   much of it arrived.

   SETF rather than LET for *MAX-BODY-SIZE*: the worker reads it, and
   dynamic bindings do not cross MAKE-THREAD."
  (format t "~%Harness: body handler, end to end~%")
  (let ((saved web-skeleton:*max-body-size*)
        (lock (sb-thread:make-mutex :name "body-handler-seen"))
        (seen nil))
    (setf web-skeleton:*max-body-size* 16)
    (unwind-protect
         (with-test-server
             (:handler
              (lambda (req)
                (make-text-response
                 200 (format nil "~a:~d" (http-request-path req)
                             (length (or (http-request-body req) #())))))
              :body-handler
              (lambda (req)
                (sb-thread:with-mutex (lock)
                  (push (list (http-request-path req)
                              (http-request-body req)
                              (get-header req "x-probe"))
                        seen))
                (let ((path (http-request-path req)))
                  (cond ((string= path "/big") (buffer-body :max-size 64))
                        ((string= path "/small") (buffer-body :max-size 4))
                        ((string= path "/deny") (make-text-response 401 "no"))
                        ((string= path "/raise") (error "deliberate: body handler"))
                        ((string= path "/junk") 42)
                        (t nil)))))
           (let ((forty (make-string 40 :initial-element #\x))
                 (ten (make-string 10 :initial-element #\y)))
             ;; ---- what the body handler is handed ----
             (check "body handler: NIL reads the body as without one"
                    (and (search "/plain:10" (%raw-exchange
                                              (%post-bytes "/plain" ten
                                                           :headers '("X-Probe: p1"))))
                         t)
                    t)
             (check "body handler: handed the parsed head, and no body"
                    (sb-thread:with-mutex (lock)
                      (find "p1" seen :key #'third :test #'equal))
                    '("/plain" nil "p1"))
             (check "body handler: not asked about a request without a body"
                    (progn
                      (%raw-exchange (%ascii (format nil "GET /nobody HTTP/1.1~a~
                                                          Host: x~aConnection: close~a~a"
                                                     *crlf* *crlf* *crlf* *crlf*)))
                      (sb-thread:with-mutex (lock)
                        (find "/nobody" seen :key #'first :test #'equal)))
                    nil)
             ;; ---- caps either side of the default ----
             (check "buffer-body: a cap above *max-body-size* takes a larger body"
                    (and (search "200 OK" (%raw-exchange (%post-bytes "/big" forty)))
                         (search "/big:40" (%raw-exchange (%post-bytes "/big" forty)))
                         t)
                    t)
             (check "buffer-body: and a chunked one"
                    (and (search "/big:40" (%raw-exchange
                                            (%post-bytes "/big" forty :chunked t)))
                         t)
                    t)
             (check "control: the same body without the plan is over the default"
                    (and (search "413" (%raw-exchange (%post-bytes "/plain" forty)))
                         t)
                    t)
             (check "buffer-body: a cap below *max-body-size* refuses, with 413"
                    (and (search "413" (%raw-exchange (%post-bytes "/small" ten)))
                         t)
                    t)
             (check "buffer-body: and refuses a chunked body as it arrives"
                    (and (search "413" (%raw-exchange
                                        (%post-bytes "/small" ten :chunked t)))
                         t)
                    t)
             ;; ---- an answer before the body ----
             ;; The client asked to be told whether to send its body and was
             ;; told no: the answer arrives with the body never sent, without
             ;; a 100 Continue ahead of it, and the server closes after it.
             (let ((answer (%raw-exchange
                            (%ascii (format nil "POST /deny HTTP/1.1~aHost: x~a~
                                                 Content-Length: 4096~a~
                                                 Expect: 100-continue~a~a"
                                            *crlf* *crlf* *crlf* *crlf* *crlf*))
                            :half-close nil)))
               (check "answered before the body: the client is told 401, first"
                      (search "HTTP/1.1 401" answer) 0)
               (check "answered before the body: no 100 Continue invited it"
                      (search "100 Continue" answer) nil)
               (check "answered before the body: and the connection closes after it"
                      (and (search "connection: close" answer) t) t))
             (check "answered before the body: a body sent anyway changes nothing"
                    (and (search "HTTP/1.1 401" (%raw-exchange (%post-bytes "/deny" ten)))
                         t)
                    t)
             ;; ---- a body handler that fails ----
             (check "body handler raising: the request is answered 500"
                    (and (search "HTTP/1.1 500" (%raw-exchange (%post-bytes "/raise" ten)))
                         t)
                    t)
             (check "body handler answering nonsense: 500"
                    (and (search "HTTP/1.1 500" (%raw-exchange (%post-bytes "/junk" ten)))
                         t)
                    t)
             ;; ---- Expect: 100-continue, accepted ----
             ;; The head arrives alone, so the interim really is waited for:
             ;; it goes out once the body handler has accepted the body.
             (let ((answer (%raw-http-split-send
                            (%ascii (format nil "POST /big HTTP/1.1~aHost: x~a~
                                                 Content-Length: 40~a~
                                                 Expect: 100-continue~a~
                                                 Connection: close~a~a"
                                            *crlf* *crlf* *crlf* *crlf* *crlf* *crlf*))
                            (%ascii forty))))
               (check "accepted with Expect: 100 Continue, then the answer"
                      (let ((interim (search "HTTP/1.1 100 Continue" answer))
                            (final (search "/big:40" answer)))
                        (and interim final (< interim final) t))
                      t))
             ;; ---- the decision is the request's, not the connection's ----
             ;; Two requests on one connection: /big's cap admits its forty
             ;; bytes, and the request after it, which no plan covers, is held
             ;; to the default again.
             (let ((answer (%raw-exchange
                            (concatenate '(vector (unsigned-byte 8))
                                         (%post-bytes "/big" forty)
                                         (%post-bytes "/plain" (subseq forty 0 20))))))
               (check "keep-alive: the first request had its plan's cap"
                      (and (search "/big:40" answer) t) t)
               (check "keep-alive: the next one is held to the default again"
                      (and (search "413" answer)
                           (< (search "/big:40" answer) (search "413" answer))
                           t)
                      t))))
      (setf web-skeleton:*max-body-size* saved))))

(defun %pseudo-random-bytes (n seed)
  "N bytes from a fixed linear-congruential sequence: the same body every
   run, and no run of it that looks like chunked framing by design."
  (let ((v (make-array n :element-type '(unsigned-byte 8)))
        (x seed))
    (dotimes (i n v)
      (setf x (mod (+ (* x 1103515245) 12345) 2147483648)
            (aref v i) (ldb (byte 8 16) x)))))

(defun test-harness-receive-body-e2e ()
  "A received body, end to end: handed to ON-DATA as it arrives, answered
   by ON-END, ended by ON-ABORT exactly once on every way it can fail.

   The body handler receives every path that starts /recv, accumulating
   what ON-DATA is handed and answering with its length and SHA-256, so a
   body that arrives reordered, short or doubled answers wrongly. The
   other paths each fail one way, and ON-ABORT records the path and the
   reason, so each failure is checked against the request that caused it.

   The large bodies are what the window is for. A 4 MiB Content-Length
   body is more than a wake-up hands over, so it goes through the yield; a
   3 MiB body sent as a single chunk can only arrive whole if chunks are
   handed over before they have landed. Both are checked for the read
   buffer staying at the head and one window."
  (format t "~%Harness: received bodies, end to end~%")
  (let* ((lock (sb-thread:make-mutex :name "receive-e2e"))
         (aborts nil)
         (handed (make-hash-table :test #'equal))
         (widest (make-hash-table :test #'equal))
         (port-box (list nil)))
    (flet ((note (table path value fn)
             (sb-thread:with-mutex (lock)
               (setf (gethash path table) (funcall fn (gethash path table 0) value))))
           (aborted (path)
             (sb-thread:with-mutex (lock)
               (loop for (p . r) in aborts when (string= p path) collect r)))
           (await-abort (path)
             (loop repeat 100
                   until (sb-thread:with-mutex (lock) (assoc path aborts :test #'string=))
                   do (sleep 0.05))))
      (with-test-server
          (:handler
           (lambda (req)
             (make-text-response 200 (format nil "handler:~a" (http-request-path req))))
           :body-handler
           (lambda (req)
             (let ((path (http-request-path req)))
               (flet ((recv (&key (max-size (* 64 1024 1024)) data end)
                        (let ((acc (make-array 0 :element-type '(unsigned-byte 8)
                                                 :adjustable t :fill-pointer 0))
                              (calls 0))
                          (receive-body
                           :max-size max-size
                           :on-data
                           (lambda (conn bytes)
                             ;; From the second piece on, the buffer is the
                             ;; head and one window.
                             (when (> (incf calls) 1)
                               (note widest path
                                     (length (web-skeleton::connection-read-buf conn))
                                     #'max))
                             (note handed path (length bytes) #'+)
                             (loop for b across bytes do (vector-push-extend b acc))
                             (when data (funcall data bytes)))
                           :on-end
                           (lambda (conn)
                             (declare (ignore conn))
                             (if end
                                 (funcall end acc)
                                 (make-text-response
                                  200 (format nil "~a:~d:~a" path (length acc)
                                              (sha256-hex acc)))))
                           :on-abort
                           (lambda (conn reason)
                             (declare (ignore conn))
                             (sb-thread:with-mutex (lock)
                               (push (cons path reason) aborts)))))))
                 (cond
                   ((string= path "/cap") (recv :max-size 1000))
                   ((string= path "/refuse")
                    (recv :data (lambda (b) (declare (ignore b))
                                  (make-text-response 415 "not this"))))
                   ((string= path "/raise-data")
                    (recv :data (lambda (b) (declare (ignore b))
                                  (error "deliberate: on-data"))))
                   ((string= path "/raise-end")
                    (recv :end (lambda (acc) (declare (ignore acc))
                                 (error "deliberate: on-end"))))
                   ((string= path "/end-upgrade")
                    (recv :end (lambda (acc) (declare (ignore acc)) :upgrade)))
                   ((string= path "/end-ws-upgrade")
                    (recv :end (lambda (acc) (declare (ignore acc)) (ws-upgrade))))
                   ((string= path "/unbounded") (recv :max-size nil))
                   ((string= path "/fetch")
                    (recv :end (lambda (acc)
                                 (http-fetch
                                  :get (format nil "http://127.0.0.1:~d/plain"
                                               (first port-box))
                                  :then (lambda (status headers body)
                                          (declare (ignore headers))
                                          (make-text-response
                                           200 (format nil "fetched:~d:~a:~d" status
                                                       (sb-ext:octets-to-string
                                                        body :external-format :ascii)
                                                       (length acc))))))))
                   ((eql 0 (search "/recv" path)) (recv))
                   (t nil))))))
        (setf (first port-box) *test-port*)
        (labels ((head (path &rest lines)
                   (%ascii (format nil "POST ~a HTTP/1.1~aHost: x~a~{~a~a~}~a"
                                   path *crlf* *crlf*
                                   (loop for l in lines append (list l *crlf*))
                                   *crlf*)))
                 (bytes (&rest parts)
                   (apply #'concatenate '(vector (unsigned-byte 8)) parts))
                 (answer-of (&rest parts)
                   (%raw-exchange (apply #'bytes parts) :seconds 15)))
          ;; ---- received whole, in order, through every window ----
          (let* ((body (%pseudo-random-bytes (* 4 1024 1024) 7))
                 (answer (answer-of (head "/recv-cl" "Content-Length: 4194304"
                                          "Connection: close")
                                    body)))
            (check "received: a 4 MiB body arrives whole and in order"
                   (and (search (format nil "/recv-cl:4194304:~a" (sha256-hex body))
                                answer)
                        t)
                   t)
            (check "received: the buffer held the head and one window"
                   (<= (gethash "/recv-cl" widest 0)
                       (+ 200 web-skeleton::+receive-window+))
                   t))
          (let* ((body (%pseudo-random-bytes (* 3 1024 1024) 11))
                 (answer (answer-of (head "/recv-one-chunk"
                                          "Transfer-Encoding: chunked"
                                          "Connection: close")
                                    (%ascii (format nil "~x~a" (length body) *crlf*))
                                    body
                                    (%ascii (format nil "~a0~a~a" *crlf* *crlf* *crlf*)))))
            (check "received: one 3 MiB chunk is handed over as it lands"
                   (and (search (format nil "/recv-one-chunk:3145728:~a"
                                        (sha256-hex body))
                                answer)
                        t)
                   t)
            (check "received: and the buffer still held one window"
                   (<= (gethash "/recv-one-chunk" widest 0)
                       (+ 200 web-skeleton::+receive-window+))
                   t))
          ;; ---- Expect: 100-continue, and keep-alive after ----
          (let ((answer (%raw-http-split-send
                         (head "/recv-continue" "Content-Length: 5"
                               "Expect: 100-continue" "Connection: close")
                         (%ascii "hello"))))
            (check "received: the interim, then ON-END's answer"
                   (let ((interim (search "100 Continue" answer))
                         (final (search "/recv-continue:5:" answer)))
                     (and interim final (< interim final) t))
                   t))
          (let ((answer (answer-of (head "/recv-keep" "Content-Length: 3")
                                   (%ascii "abc")
                                   (%ascii (format nil "GET /after HTTP/1.1~aHost: x~a~
                                                        Connection: close~a~a"
                                                   *crlf* *crlf* *crlf* *crlf*)))))
            (check "received: the request after a received body is served"
                   (and (search "/recv-keep:3:" answer)
                        (search "handler:/after" answer)
                        t)
                   t))
          ;; ---- ON-END answering with a fetch ----
          (check "received: ON-END may answer with a fetch"
                 (and (search "fetched:200:handler:/plain:4"
                              (answer-of (head "/fetch" "Content-Length: 4"
                                               "Connection: close")
                                         (%ascii "abcd")))
                      t)
                 t)
          ;; ---- refused, each with its reason ----
          (check "cap: a declared length over it is 413"
                 (and (search "HTTP/1.1 413"
                              (answer-of (head "/cap" "Content-Length: 1001")
                                         (%ascii (make-string 1001 :initial-element #\c))))
                      t)
                 t)
          (check "cap: the declared length's ON-ABORT heard :too-large, once"
                 (aborted "/cap") '(:too-large))
          (sb-thread:with-mutex (lock) (setf aborts nil))
          (check "cap: a chunked body passing it is 413"
                 (and (search "HTTP/1.1 413"
                              (answer-of (head "/cap" "Transfer-Encoding: chunked")
                                         (%ascii (format nil "3e8~a~a~a3e8~a~a~a0~a~a"
                                                         *crlf* (make-string 1000 :initial-element #\c)
                                                         *crlf* *crlf*
                                                         (make-string 1000 :initial-element #\d)
                                                         *crlf* *crlf* *crlf*))))
                      t)
                 t)
          (check "cap: and nothing past the cap was handed over"
                 (<= (gethash "/cap" handed 0) 1000) t)
          (check "cap: the chunked body's ON-ABORT heard :too-large, once"
                 (aborted "/cap") '(:too-large))
          (check "refused: ON-DATA's answer is the client's"
                 (and (search "HTTP/1.1 415"
                              (answer-of (head "/refuse" "Content-Length: 4")
                                         (%ascii "abcd")))
                      t)
                 t)
          (check "refused: ON-ABORT heard :refused" (aborted "/refuse") '(:refused))
          (check "raised in ON-DATA: 500"
                 (and (search "HTTP/1.1 500"
                              (answer-of (head "/raise-data" "Content-Length: 4")
                                         (%ascii "abcd")))
                      t)
                 t)
          (check "raised in ON-DATA: ON-ABORT heard :error" (aborted "/raise-data")
                 '(:error))
          (check "raised in ON-END: 500"
                 (and (search "HTTP/1.1 500"
                              (answer-of (head "/raise-end" "Content-Length: 4"
                                               "Connection: close")
                                         (%ascii "abcd")))
                      t)
                 t)
          (check "raised in ON-END: ON-ABORT heard :error" (aborted "/raise-end")
                 '(:error))
          ;; A request whose body was read is no handshake, whichever way
          ;; ON-END asks to switch protocols.
          (check "ON-END answering :upgrade, or a WS-UPGRADE: 500 each"
                 (loop for path in '("/end-upgrade" "/end-ws-upgrade")
                       collect (and (search "HTTP/1.1 500"
                                            (answer-of (head path "Content-Length: 4"
                                                             "Connection: close")
                                                       (%ascii "abcd")))
                                    t))
                 '(t t))
          (check "malformed: chunked framing that is wrong is 400"
                 (and (search "HTTP/1.1 400"
                              (answer-of (head "/recv-malformed"
                                               "Transfer-Encoding: chunked")
                                         (%ascii (format nil "5~ahelloXX" *crlf*))))
                      t)
                 t)
          (check "malformed: ON-ABORT heard :malformed" (aborted "/recv-malformed")
                 '(:malformed))
          ;; ---- the client leaving ----
          (multiple-value-bind (socket stream) (%raw-connect)
            (write-sequence (bytes (head "/recv-gone" "Content-Length: 1000")
                                   (%ascii "0123456789"))
                            stream)
            (force-output stream)
            (sleep 0.2)
            (ignore-errors (close stream))
            (ignore-errors (sb-bsd-sockets:socket-close socket)))
          (await-abort "/recv-gone")
          (check "gone: ON-ABORT heard :disconnected" (aborted "/recv-gone")
                 '(:disconnected))
          (check "gone: what had arrived was handed over"
                 (gethash "/recv-gone" handed 0) 10)
          ;; A declared length no buffer could hold is no reason to refuse a
          ;; body that is never buffered.
          (multiple-value-bind (socket stream) (%raw-connect)
            (write-sequence (bytes (head "/unbounded" "Content-Length: 21474836480")
                                   (%ascii "0123456789"))
                            stream)
            (force-output stream)
            (sleep 0.2)
            (ignore-errors (close stream))
            (ignore-errors (sb-bsd-sockets:socket-close socket)))
          (await-abort "/unbounded")
          (check "unbounded: a 20 GiB declaration is received, not refused"
                 (list (gethash "/unbounded" handed 0) (aborted "/unbounded"))
                 '(10 (:disconnected)))
          ;; ---- exactly once ----
          (check "once: no body that ended well heard ON-ABORT"
                 (loop for path in '("/recv-cl" "/recv-one-chunk" "/recv-continue"
                                     "/recv-keep" "/fetch")
                       always (null (aborted path)))
                 t)))
      ;; A body still arriving when the server drains: the drain closes it,
      ;; and ON-ABORT hears :SHUTDOWN. The connection is opened inside the
      ;; server's extent and the check made once WITH-TEST-SERVER's
      ;; teardown has drained it.
      (let ((socket nil) (stream nil))
        (unwind-protect
             (progn
               (with-test-server
                   (:handler (lambda (req) (declare (ignore req))
                               (make-text-response 200 "ok"))
                    :body-handler
                    (lambda (req)
                      (declare (ignore req))
                      (receive-body
                       :on-data (lambda (conn bytes) (declare (ignore conn bytes)) nil)
                       :on-end (lambda (conn) (declare (ignore conn))
                                 (make-text-response 200 "done"))
                       :on-abort (lambda (conn reason)
                                   (declare (ignore conn))
                                   (sb-thread:with-mutex (lock)
                                     (push (cons "/drain" reason) aborts))))))
                 (multiple-value-setq (socket stream) (%raw-connect))
                 (write-sequence (%ascii (format nil "POST /drain HTTP/1.1~aHost: x~a~
                                                      Content-Length: 1000~a~a0123"
                                                 *crlf* *crlf* *crlf* *crlf*))
                                 stream)
                 (force-output stream)
                 (sleep 0.2))
               (check "shutdown: a body still arriving hears :shutdown"
                      (aborted "/drain") '(:shutdown)))
          (when stream (ignore-errors (close stream)))
          (when socket (ignore-errors (sb-bsd-sockets:socket-close socket))))))))

(defun test-harness-receive-body-paused-e2e ()
  "A received body forwarded a part at a time: ON-DATA collects a part,
   starts a fetch carrying it with FETCH-INTO, and pauses; the fetch's
   :THEN resumes the body with BODY-RESUME; ON-END answers with one last
   fetch for the remainder. The shape of an upload ferried on to object
   storage as it arrives.

   The upstream is the test server's own /store, which records each part.
   What is checked is what the shape depends on: every part arrives, in
   order, the parts together are the body, and while the body is paused
   ON-DATA is never called — the pause holds."
  (format t "~%Harness: a received body paused on the fetches it starts~%")
  (let* ((lock (sb-thread:make-mutex :name "paused-e2e"))
         (stored nil)
         (pauses 0)
         (while-paused 0)
         (paused nil)
         (port-box (list nil))
         (part-size (* 256 1024))
         (body (%pseudo-random-bytes (+ (* 4 part-size) 100) 23)))
    (with-test-server
        (:handler
         (lambda (req)
           (if (string= (http-request-path req) "/store")
               (progn
                 (sb-thread:with-mutex (lock)
                   (push (copy-seq (http-request-body req)) stored))
                 (make-text-response 200 "stored"))
               (make-error-response 404)))
         :body-handler
         (lambda (req)
           (when (string= (http-request-path req) "/upload")
             (let ((part (make-array 0 :element-type '(unsigned-byte 8)
                                       :adjustable t :fill-pointer 0))
                   (parts 0))
               (flet ((store (bytes then)
                        (http-fetch :put (format nil "http://127.0.0.1:~d/store"
                                                 (first port-box))
                                    :body (coerce bytes '(simple-array (unsigned-byte 8) (*)))
                                    :then then)))
                 (receive-body
                  :max-size nil
                  :on-data
                  (lambda (conn bytes)
                    (when paused (incf while-paused))
                    (loop for b across bytes do (vector-push-extend b part))
                    (when (>= (fill-pointer part) part-size)
                      (let ((whole (subseq part 0 part-size))
                            (rest (subseq part part-size)))
                        (setf (fill-pointer part) 0)
                        (loop for b across rest do (vector-push-extend b part))
                        (incf parts)
                        (incf pauses)
                        (setf paused t)
                        (fetch-into conn
                                    (store whole
                                           (lambda (status headers b)
                                             (declare (ignore headers b))
                                             (setf paused nil)
                                             (if (eql status 200)
                                                 (body-resume conn)
                                                 (body-refuse conn (make-error-response 502)))
                                             nil)))
                        :pause)))
                  :on-end
                  ;; What is left is stored a part at a time too, each part's
                  ;; :THEN chaining the next, so the parts fall on the same
                  ;; boundaries however the reads happened to cut the body.
                  (lambda (conn)
                    (declare (ignore conn))
                    (labels ((next (from)
                               (let ((to (min (fill-pointer part) (+ from part-size))))
                                 (incf parts)
                                 (store (subseq part from to)
                                        (lambda (status headers b)
                                          (declare (ignore headers b))
                                          (if (and (eql status 200)
                                                   (< to (fill-pointer part)))
                                              (next to)
                                              (make-text-response
                                               (or status 502)
                                               (format nil "parts:~d" parts))))))))
                      (next 0)))))))))
      (setf (first port-box) *test-port*)
      (let ((answer (%raw-exchange
                     (concatenate '(vector (unsigned-byte 8))
                                  (%ascii (format nil "POST /upload HTTP/1.1~aHost: x~a~
                                                       Content-Length: ~d~a~
                                                       Connection: close~a~a"
                                                  *crlf* *crlf* (length body) *crlf*
                                                  *crlf* *crlf*))
                                  body)
                     :seconds 20)))
        (check "paused: the upload is answered after its last part is stored"
               (and (search "parts:5" answer) t) t)
        (let ((parts (reverse (sb-thread:with-mutex (lock) stored))))
          (check "paused: every part reached the upstream"
                 (length parts) 5)
          (check "paused: in order, together the body"
                 (sha256-hex (apply #'concatenate '(vector (unsigned-byte 8)) parts))
                 (sha256-hex body)))
        (check "paused: the body paused for the parts it forwarded"
               (<= 1 pauses 4) t)
        (check "paused: ON-DATA was not called while a pause held"
               while-paused 0)))))

(defun test-harness-receive-body-refused-from-a-callback-e2e ()
  "A received body ended from outside ON-DATA: by BODY-REFUSE from a
   fetch's :THEN, and by a failed fetch under each failure disposition.

   The fetches go to a port with nothing listening, so each one fails
   to connect. Under :CLOSE, the default, the body is ended and ON-ABORT
   hears :UPSTREAM-FAILED; under :KEEP the body is left paused, its :THEN
   sees the abort sentinel and answers with BODY-REFUSE, and ON-ABORT
   hears :REFUSED."
  (format t "~%Harness: a received body refused from a fetch's :then~%")
  (let* ((lock (sb-thread:make-mutex :name "refuse-e2e"))
         (aborts nil)
         (dead-port (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                                            :type :stream :protocol :tcp)))
                      (sb-bsd-sockets:socket-bind s #(127 0 0 1) 0)
                      (prog1 (nth-value 1 (sb-bsd-sockets:socket-name s))
                        (sb-bsd-sockets:socket-close s)))))
    (flet ((aborted (path)
             (loop repeat 100
                   until (sb-thread:with-mutex (lock) (assoc path aborts :test #'string=))
                   do (sleep 0.05))
             (sb-thread:with-mutex (lock)
               (loop for (p . r) in aborts when (string= p path) collect r))))
      (with-test-server
          (:handler (lambda (req) (declare (ignore req)) (make-error-response 404))
           :body-handler
           (lambda (req)
             (let ((path (http-request-path req)))
               (receive-body
                :on-data
                (lambda (conn bytes)
                  (declare (ignore bytes))
                  (fetch-into
                   conn
                   (http-fetch :get (format nil "http://127.0.0.1:~d/" dead-port)
                               :then (lambda (status headers b)
                                       (declare (ignore headers b))
                                       (when (and (null status)
                                                  (string= path "/keep"))
                                         (body-refuse conn (make-text-response
                                                            503 "upstream down")))
                                       nil))
                   :failure-disposition (if (string= path "/keep") :keep :close))
                  :pause)
                :on-end (lambda (conn) (declare (ignore conn))
                          (make-text-response 200 "unexpected"))
                :on-abort (lambda (conn reason)
                            (declare (ignore conn))
                            (sb-thread:with-mutex (lock)
                              (push (cons path reason) aborts)))))))
        (%raw-exchange (%post-bytes "/close" "abcd") :seconds 5)
        (check "failed fetch, :close: the body is ended as :upstream-failed"
               (aborted "/close") '(:upstream-failed))
        (let ((answer (%raw-exchange (%post-bytes "/keep" "abcd") :seconds 5)))
          (check "failed fetch, :keep: :then's BODY-REFUSE answers the client"
                 (and (search "HTTP/1.1 503" answer) t) t))
        (check "failed fetch, :keep: and ON-ABORT hears :refused"
               (aborted "/keep") '(:refused))))))

(defun test-harness-paused-body-waits-on-its-fetch-e2e ()
  "A received body paused on a fetch it started is bounded by that fetch,
   not by the stall clock.

   *BODY-STALL-TIMEOUT* is 1 for the run and the upstream takes three
   seconds to answer, so a stall clock still running through the pause
   would close the body before the part was stored. The body is answered
   instead, once the part is stored and the body resumed — which also
   restarts the clock, so the rest of the body is timed afresh."
  (format t "~%Harness: a paused body waits on its fetch~%")
  (let ((saved web-skeleton:*body-stall-timeout*)
        (listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp))
        (thread nil))
    (setf web-skeleton:*body-stall-timeout* 1)
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 5)
           (let ((port (nth-value 1 (sb-bsd-sockets:socket-name listener))))
             (setf thread (%piecemeal-upstream
                           listener
                           (list (format nil "HTTP/1.1 200 OK~c~cContent-Length: 2~c~c~
                                              Connection: close~c~c~c~c"
                                         #\Return #\Newline #\Return #\Newline
                                         #\Return #\Newline #\Return #\Newline)
                                 "ok")
                           :pause 3 :hold 0))
             (with-test-server
                 (:handler (lambda (req) (declare (ignore req)) (make-error-response 404))
                  :body-handler
                  (lambda (req)
                    (declare (ignore req))
                    (let ((sent nil))
                      (receive-body
                       :on-data
                       (lambda (conn bytes)
                         (declare (ignore bytes))
                         (unless sent
                           (setf sent t)
                           (fetch-into
                            conn
                            (http-fetch :get (format nil "http://127.0.0.1:~d/" port)
                                        :then (lambda (status headers b)
                                                (declare (ignore status headers b))
                                                (body-resume conn)
                                                nil)))
                           :pause))
                       :on-end (lambda (conn) (declare (ignore conn))
                                 (make-text-response 200 "stored"))))))
               (let* ((start (get-internal-real-time))
                      (answer (%raw-exchange (%post-bytes "/u" "abcd"
                                                          :headers '("Connection: close"))
                                             :seconds 10))
                      (elapsed (/ (- (get-internal-real-time) start)
                                  internal-time-units-per-second)))
                 (check "waits: the body outlasted the stall clock on its fetch"
                        (and (search "stored" answer) t) t)
                 (check "precondition: the fetch took longer than the stall timeout"
                        (> elapsed 2) t)))))
      (setf web-skeleton:*body-stall-timeout* saved)
      (ignore-errors (sb-bsd-sockets:socket-close listener))
      (when thread
        (handler-case (sb-thread:join-thread thread :timeout 10)
          (error () (ignore-errors (sb-thread:terminate-thread thread))))))))

(defun %multipart-body (boundary parts &key (close t))
  "A multipart/form-data body as a browser writes one: PARTS, each
   (NAME FILENAME CONTENT) with CONTENT a string or bytes and FILENAME NIL
   for a text field, divided by BOUNDARY. CLOSE NIL leaves the close
   delimiter off, for a body that ends too soon. Bytes."
  (let ((pieces '()))
    (flet ((emit (x)
             (push (if (stringp x) (sb-ext:string-to-octets x :external-format :utf-8) x)
                   pieces)))
      (dolist (part parts)
        (destructuring-bind (name filename content) part
          (emit (format nil "--~a~a" boundary *crlf*))
          (emit (format nil "Content-Disposition: form-data; name=\"~a\"~@[; filename=\"~a\"~]~a"
                        name filename *crlf*))
          (when filename
            (emit (format nil "Content-Type: application/octet-stream~a" *crlf*)))
          (emit *crlf*)
          (emit content)
          (emit *crlf*)))
      (when close
        (emit (format nil "--~a--~a" boundary *crlf*))))
    (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) (nreverse pieces))))

(defun %form-post (path boundary body &key chunk-size (headers '()))
  "A POST of the multipart BODY to PATH: framed by Content-Length, or
   chunked in CHUNK-SIZE pieces. Bytes, ready to send."
  (concatenate
   '(simple-array (unsigned-byte 8) (*))
   (%ascii (format nil "POST ~a HTTP/1.1~aHost: x~a~
                        Content-Type: multipart/form-data; boundary=~a~a~
                        ~:[Content-Length: ~d~;Transfer-Encoding: chunked~*~]~a~
                        ~{~a~a~}~a"
                   path *crlf* *crlf* boundary *crlf*
                   chunk-size (length body) *crlf*
                   (loop for h in headers append (list h *crlf*))
                   *crlf*))
   (if chunk-size
       (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
              (append
               (loop for at from 0 below (length body) by chunk-size
                     for to = (min (length body) (+ at chunk-size))
                     collect (%ascii (format nil "~x~a" (- to at) *crlf*))
                     collect (subseq body at to)
                     collect (%ascii *crlf*))
               (list (%ascii (format nil "0~a~a" *crlf* *crlf*)))))
       body)))

(defun test-harness-receive-multipart-e2e ()
  "A form received as it arrives, end to end: its text fields collected
   and handed to :ON-END, its file streamed and never held, and every way
   such a body can be refused answered with its status and heard once by
   :ON-ABORT with its reason.

   The body handler receives every path that starts /form. :ON-PART
   collects a part with no filename and streams one with a filename,
   whose runs are kept and hashed at :ON-PART-END; :ON-END answers with
   the fields and each file's name, length and SHA-256, so a file that
   arrives reordered, short or doubled answers wrongly. The other paths
   each fail one way, and :ON-ABORT records the path and the reason."
  (format t "~%Harness: a form received as it arrives~%")
  (let* ((lock (sb-thread:make-mutex :name "multipart-e2e"))
         (aborts nil)
         (seen nil)
         (widest 0)
         (boundary "----FormBoundary7MA4YWxkTrZu0gW")
         (file (%pseudo-random-bytes (* 3 1024 1024) 41))
         ;; The answer is UTF-8, and %RAW-EXCHANGE reads it as Latin-1.
         (expected (sb-ext:octets-to-string
                    (sb-ext:string-to-octets
                     (format nil "fields:title=hello,note=Zoë;files:upload:~d:~a"
                             (length file) (sha256-hex file))
                     :external-format :utf-8)
                    :external-format :latin-1)))
    (flet ((aborted (path)
             (loop repeat 100
                   until (sb-thread:with-mutex (lock) (assoc path aborts :test #'string=))
                   do (sleep 0.05))
             (sb-thread:with-mutex (lock)
               (loop for (p . r) in aborts when (string= p path) collect r)))
           (parts-seen (path)
             (sb-thread:with-mutex (lock)
               (reverse (loop for (p . n) in seen when (string= p path) collect n))))
           (answer-of (bytes) (%raw-exchange bytes :seconds 20)))
      (with-test-server
          (:handler
           (lambda (req)
             (if (string= (http-request-path req) "/parsed")
                 ;; The buffered path: the body read whole, then parsed.
                 ;; A raise is the request's answer, with its status.
                 (make-text-response
                  200 (format nil "parsed:~{~a~^,~}"
                              (mapcar (lambda (p) (format nil "~a=~a" (form-part-name p)
                                                          (length (form-part-body p))))
                                      (parse-multipart req))))
                 (make-text-response 200 (format nil "handler:~a" (http-request-path req)))))
           :body-handler
           (lambda (req)
             (let ((path (http-request-path req)))
               (when (eql 0 (search "/form" path))
                 (let ((files '())
                       (current nil)
                       (runs 0))
                   (receive-multipart
                    req
                    :max-size (* 64 1024 1024)
                    :max-parts (if (string= path "/form-few") 2 1000)
                    :max-collected-size (if (string= path "/form-small") 100 (* 1024 1024))
                    :on-part
                    (lambda (conn part)
                      (declare (ignore conn))
                      (sb-thread:with-mutex (lock) (push (cons path (form-part-name part)) seen))
                      (cond
                        ((and (string= path "/form-strict")
                              (string= (form-part-name part) "evil"))
                         (make-text-response 422 "unexpected field"))
                        ((form-part-filename part)
                         (setf current (make-array 0 :element-type '(unsigned-byte 8)
                                                     :adjustable t :fill-pointer 0))
                         :stream)
                        (t nil)))
                    :on-part-data
                    (lambda (conn part bytes)
                      (declare (ignore part))
                      ;; From the second run on, the buffer is the head and
                      ;; one window; the first can arrive in the buffer a
                      ;; read filled before the plan was decided.
                      (when (> (incf runs) 1)
                        (sb-thread:with-mutex (lock)
                          (setf widest (max widest (length (web-skeleton::connection-read-buf conn))))))
                      (loop for b across bytes do (vector-push-extend b current))
                      nil)
                    :on-part-end
                    (lambda (conn part)
                      (declare (ignore conn))
                      (cond
                        ((form-part-filename part)
                         (push (format nil "~a:~d:~a" (form-part-name part)
                                       (fill-pointer current) (sha256-hex current))
                               files)
                         nil)
                        ;; A field that decides whether the rest is wanted.
                        ((and (string= (form-part-name part) "token")
                              (not (equal (form-part-text part) "good")))
                         (make-text-response 403 "bad token"))
                        (t nil)))
                    :on-end
                    (lambda (conn parts)
                      (declare (ignore conn))
                      (make-text-response
                       200 (format nil "fields:~{~a~^,~};files:~{~a~^,~}"
                                   (mapcar (lambda (p) (format nil "~a=~a" (form-part-name p)
                                                               (form-part-text p)))
                                           parts)
                                   (reverse files))))
                    :on-abort
                    (lambda (conn reason)
                      (declare (ignore conn))
                      (sb-thread:with-mutex (lock) (push (cons path reason) aborts)))))))))
        (let ((form (%multipart-body boundary `(("title" nil "hello")
                                                 ("upload" "data.bin" ,file)
                                                 ("note" nil "Zoë")))))
          ;; ---- received whole, in order, the file never held ----
          (check "form: fields collected, the file streamed whole and in order"
                 (and (search expected (answer-of (%form-post "/form" boundary form
                                                              :headers '("Connection: close"))))
                      t)
                 t)
          (check "form: the read buffer held the head and one window"
                 (<= widest (+ 400 web-skeleton::+receive-window+)) t)
          (check "form: and chunked, in pieces that cut the parts anywhere"
                 (and (search expected (answer-of (%form-post "/form" boundary form
                                                              :chunk-size 50003
                                                              :headers '("Connection: close"))))
                      t)
                 t)
          (check "form: nothing aborted the bodies that ended well" (aborted "/form") nil))
        ;; ---- keep-alive after a received form ----
        (let ((answer (answer-of
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (%form-post "/form-keep" boundary
                                                (%multipart-body boundary '(("a" nil "1"))))
                                    (%ascii (format nil "GET /after HTTP/1.1~aHost: x~a~
                                                         Connection: close~a~a"
                                                    *crlf* *crlf* *crlf* *crlf*))))))
          (check "form: the request after a received form is served"
                 (and (search "fields:a=1;files:" answer) (search "handler:/after" answer) t)
                 t))
        ;; ---- refused, each with its reason ----
        (check "few: more parts than :max-parts is 413"
               (and (search "HTTP/1.1 413"
                            (answer-of (%form-post "/form-few" boundary
                                                   (%multipart-body boundary '(("a" nil "1") ("b" nil "2")
                                                                               ("c" nil "3"))))))
                    t)
               t)
        (check "few: :on-abort heard :too-large" (aborted "/form-few") '(:too-large))
        (check "small: collected fields past :max-collected-size are 413"
               (and (search "HTTP/1.1 413"
                            (answer-of (%form-post "/form-small" boundary
                                                   (%multipart-body
                                                    boundary `(("a" nil ,(make-string 200 :initial-element #\a)))))))
                    t)
               t)
        (check "small: :on-abort heard :too-large" (aborted "/form-small") '(:too-large))
        (check "strict: :on-part's answer refuses the body"
               (and (search "HTTP/1.1 422"
                            (answer-of (%form-post "/form-strict" boundary
                                                   (%multipart-body boundary '(("token" nil "good")
                                                                               ("evil" nil "x"))))))
                    t)
               t)
        (check "strict: :on-abort heard :refused" (aborted "/form-strict") '(:refused))
        (check "token: a field's :on-part-end refuses the body before the file"
               (and (search "HTTP/1.1 403"
                            (answer-of (%form-post "/form-token" boundary
                                                   (%multipart-body boundary
                                                                    `(("token" nil "bad")
                                                                      ("upload" "big.bin" ,file))))))
                    t)
               t)
        (check "token: :on-abort heard :refused" (aborted "/form-token") '(:refused))
        (check "token: and the file part was never begun"
               (parts-seen "/form-token") '("token"))
        (check "malformed: a line beginning with the boundary that is not a delimiter is 400"
               (and (search "HTTP/1.1 400"
                            (answer-of (%form-post "/form-bad" boundary
                                                   (concatenate
                                                    '(simple-array (unsigned-byte 8) (*))
                                                    (%multipart-body boundary '(("a" nil "1")) :close nil)
                                                    (%ascii (format nil "--~aX~a" boundary *crlf*))))))
                    t)
               t)
        (check "malformed: :on-abort heard :malformed" (aborted "/form-bad") '(:malformed))
        (check "truncated: a body that ends before its close delimiter is 400"
               (and (search "HTTP/1.1 400"
                            (answer-of (%form-post "/form-short" boundary
                                                   (%multipart-body boundary '(("a" nil "1")) :close nil))))
                    t)
               t)
        (check "truncated: :on-abort heard :malformed" (aborted "/form-short") '(:malformed))
        ;; ---- not a form: refused at the head ----
        (let ((answer (%raw-exchange
                       (%ascii (format nil "POST /form-type HTTP/1.1~aHost: x~a~
                                            Content-Type: text/plain~a~
                                            Content-Length: 4096~aExpect: 100-continue~a~a"
                                       *crlf* *crlf* *crlf* *crlf* *crlf* *crlf*))
                       :half-close nil)))
          (check "type: a body that is not multipart/form-data is 415, before it is read"
                 (list (search "HTTP/1.1 415" answer) (search "100 Continue" answer))
                 '(0 nil)))
        (check "type: no plan was accepted, so :on-abort heard nothing"
               (sb-thread:with-mutex (lock) (assoc "/form-type" aborts :test #'string=))
               nil)
        ;; ---- buffered, and parsed by :handler ----
        (check "parsed: a buffered form, parsed in :handler"
               (and (search "parsed:a=1,f=4"
                            (%raw-exchange (%form-post "/parsed" boundary
                                                       (%multipart-body boundary '(("a" nil "1")
                                                                                   ("f" "f.txt" "four")))
                                                       :headers '("Connection: close"))))
                    t)
               t)
        (check "parsed: a body that is not a form is answered 415 from the raise"
               (and (search "HTTP/1.1 415" (%raw-exchange (%post-bytes "/parsed" "a=1"
                                                                       :headers '("Content-Type: text/plain"))))
                    t)
               t)
        (check "parsed: a malformed form is answered 400 from the raise"
               (and (search "HTTP/1.1 400"
                            (%raw-exchange (%form-post "/parsed" boundary
                                                       (%multipart-body boundary '(("a" nil "1"))
                                                                        :close nil))))
                    t)
               t)))))

(defun test-harness-receive-multipart-paused-e2e ()
  "A form whose file is forwarded upstream a piece at a time as it
   arrives, the shape of a file ferried on to object storage: when a full
   piece has been collected, a fetch carries it with FETCH-INTO and the
   body pauses. The fetch's :THEN sends the next full piece if one is
   already collected, and otherwise resumes the body; the file's
   :ON-PART-END sends what is left. A text field after the file is read
   once that last piece is stored. However the reads cut the body, the
   pieces fall on the same boundaries, and a read that brings several of
   them pauses inside itself.

   The upstream is the test server's own /store. What is checked: every
   piece arrives, in order, together the file; the field after the file
   reaches :ON-END; and no callback is called while a pause holds."
  (format t "~%Harness: a form's file forwarded upstream as it arrives~%")
  (let* ((lock (sb-thread:make-mutex :name "multipart-paused-e2e"))
         (stored nil)
         (paused nil)
         (while-paused 0)
         (port-box (list nil))
         (piece-size (* 128 1024))
         (boundary "x7YqL")
         (file (%pseudo-random-bytes (+ (* 5 piece-size) 777) 43)))
    (with-test-server
        (:handler
         (lambda (req)
           (if (string= (http-request-path req) "/store")
               (progn
                 (sb-thread:with-mutex (lock)
                   (push (copy-seq (http-request-body req)) stored))
                 (make-text-response 200 "stored"))
               (make-error-response 404)))
         :body-handler
         (lambda (req)
           (when (string= (http-request-path req) "/upload")
             (let ((piece (make-array 0 :element-type '(unsigned-byte 8)
                                        :adjustable t :fill-pointer 0))
                   (file-ended nil)
                   (pieces 0))
               (labels ((take (k)
                          ;; The first K bytes collected, removed.
                          (let ((head (subseq piece 0 k))
                                (rest (subseq piece k)))
                            (setf (fill-pointer piece) 0)
                            (loop for b across rest do (vector-push-extend b piece))
                            (coerce head '(simple-array (unsigned-byte 8) (*)))))
                        (next-piece ()
                          ;; A full piece, or once the file has ended what is left.
                          (let ((n (fill-pointer piece)))
                            (cond ((>= n piece-size) (take piece-size))
                                  ((and file-ended (plusp n)) (take n)))))
                        (send (conn bytes)
                          (incf pieces)
                          (fetch-into conn
                                      (http-fetch :put (format nil "http://127.0.0.1:~d/store"
                                                               (first port-box))
                                                  :body bytes
                                                  :then (lambda (status headers b)
                                                          (declare (ignore headers b))
                                                          (if (eql status 200)
                                                              (let ((more (next-piece)))
                                                                (if more
                                                                    (send conn more)
                                                                    (progn (setf paused nil)
                                                                           (body-resume conn))))
                                                              (progn (setf paused nil)
                                                                     (body-refuse conn (make-error-response 502))))
                                                          nil))))
                        (send-if-due (conn)
                          (let ((due (next-piece)))
                            (when due
                              (setf paused t)
                              (send conn due)
                              :pause)))
                        (note-call ()
                          (when paused (incf while-paused))))
                 (receive-multipart
                  req
                  :max-size nil
                  :on-part (lambda (conn part)
                             (declare (ignore conn))
                             (note-call)
                             (if (form-part-filename part) :stream nil))
                  :on-part-data (lambda (conn part bytes)
                                  (declare (ignore part))
                                  (note-call)
                                  (loop for b across bytes do (vector-push-extend b piece))
                                  (send-if-due conn))
                  :on-part-end (lambda (conn part)
                                 (note-call)
                                 (when (form-part-filename part)
                                   (setf file-ended t)
                                   (send-if-due conn)))
                  :on-end (lambda (conn parts)
                            (declare (ignore conn))
                            (note-call)
                            (make-text-response
                             200 (format nil "pieces:~d;fields:~{~a~^,~}" pieces
                                         (mapcar #'form-part-text parts))))))))))
      (setf (first port-box) *test-port*)
      (let* ((form (%multipart-body boundary `(("before" nil "1")
                                                ("file" "f.bin" ,file)
                                                ("after" nil "2"))))
             (answer (%raw-exchange (%form-post "/upload" boundary form
                                                :headers '("Connection: close"))
                                    :seconds 20)))
        (check "paused form: answered after the file's last piece, with the field after it"
               (and (search "pieces:6;fields:1,2" answer) t) t)
        (let ((pieces (reverse (sb-thread:with-mutex (lock) stored))))
          (check "paused form: every piece reached the upstream" (length pieces) 6)
          (check "paused form: in order, together the file"
                 (sha256-hex (apply #'concatenate '(vector (unsigned-byte 8)) pieces))
                 (sha256-hex file)))
        (check "paused form: no callback was called while a pause held" while-paused 0)))))

(defun test-harness-body-refused-from-inside-its-callback-e2e ()
  "A callback that calls BODY-REFUSE itself, where returning its response
   was the documented way, ends the body there: the client gets that
   response, :ON-ABORT hears :REFUSED once, and nothing more of the body is
   handed over — RECEIVE-BODY's :ON-DATA and RECEIVE-MULTIPART's
   :ON-PART-DATA alike, the second with more parts in the same read."
  (format t "~%Harness: a body refused from inside its own callback~%")
  (let ((lock (sb-thread:make-mutex :name "refuse-inside-e2e"))
        (aborts nil)
        (calls nil))
    (flet ((aborted (path)
             (loop repeat 100
                   until (sb-thread:with-mutex (lock) (assoc path aborts :test #'string=))
                   do (sleep 0.05))
             (sb-thread:with-mutex (lock)
               (loop for (p . r) in aborts when (string= p path) collect r))))
      (with-test-server
          (:handler (lambda (req) (declare (ignore req)) (make-error-response 404))
           :body-handler
           (lambda (req)
             (let ((path (http-request-path req)))
               (flet ((abort-note (conn reason)
                        (declare (ignore conn))
                        (sb-thread:with-mutex (lock) (push (cons path reason) aborts)))
                      (call-note (what)
                        (sb-thread:with-mutex (lock) (push (cons path what) calls))))
                 (if (string= path "/raw")
                     (receive-body
                      :on-data (lambda (conn bytes)
                                 (declare (ignore bytes))
                                 (call-note :data)
                                 (body-refuse conn (make-text-response 403 "refused inside"))
                                 nil)
                      :on-end (lambda (conn) (declare (ignore conn))
                                (call-note :end)
                                (make-text-response 200 "unexpected"))
                      :on-abort #'abort-note)
                     (receive-multipart
                      req
                      :on-part (lambda (conn part) (declare (ignore conn))
                                 (call-note (form-part-name part))
                                 :stream)
                      :on-part-data (lambda (conn part bytes)
                                      (declare (ignore part bytes))
                                      (body-refuse conn (make-text-response 403 "refused inside"))
                                      nil)
                      :on-end (lambda (conn parts) (declare (ignore conn parts))
                                (call-note :end)
                                (make-text-response 200 "unexpected"))
                      :on-abort #'abort-note))))))
        (check "inside, receive-body: the client gets the callback's response"
               (and (search "HTTP/1.1 403" (%raw-exchange (%post-bytes "/raw" "abcdef")))
                    t)
               t)
        (check "inside, receive-body: :on-abort heard :refused, once" (aborted "/raw") '(:refused))
        (check "inside, receive-body: nothing was handed over after it"
               (sb-thread:with-mutex (lock)
                 (loop for (p . w) in calls when (string= p "/raw") collect w))
               '(:data))
        (let ((boundary "b0"))
          (check "inside, receive-multipart: the client gets the callback's response"
                 (and (search "HTTP/1.1 403"
                              (%raw-exchange (%form-post "/mp" boundary
                                                         (%multipart-body boundary
                                                                          '(("one" "1.txt" "first")
                                                                            ("two" "2.txt" "second"))))))
                      t)
                 t))
        (check "inside, receive-multipart: :on-abort heard :refused, once" (aborted "/mp") '(:refused))
        (check "inside, receive-multipart: the next part in the same read was never begun"
               (sb-thread:with-mutex (lock)
                 (loop for (p . w) in calls when (string= p "/mp") collect w))
               '("one"))))))

(defun %upload-upstream (&key (pace 0) early-status close-after stall-after silent release)
  "A listener on an ephemeral port playing an upstream that receives an
   upload, once. Returns (values PORT THREAD); the thread's result is
   (HEAD BODY), the request head as a string and the body decoded.

   It reads the head, then the body — by its Content-Length, or chunked to
   its terminator — at most 16 KiB a read and PACE seconds between reads,
   and answers 200 with got:<length>:<sha-256> of what it received.
   EARLY-STATUS answers with that status as soon as the head has arrived
   and then reads on, as an upstream refusing an upload does. CLOSE-AFTER
   hangs up once that many bytes of body have arrived, answering nothing.
   STALL-AFTER stops reading once that many have, holding the connection
   open and answering nothing until the CAR of RELEASE is true, or ten
   seconds have passed, and hangs up then. SILENT reads the whole body and
   then holds on in the same way, answering nothing."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen listener 1)
    (values
     (nth-value 1 (sb-bsd-sockets:socket-name listener))
     (sb-thread:make-thread
      (lambda ()
        (unwind-protect
             (handler-case
                 (let* ((s (sb-bsd-sockets:socket-accept listener))
                        (buf (make-array 0 :element-type '(unsigned-byte 8)
                                           :adjustable t :fill-pointer 0))
                        (piece (make-array 16384 :element-type '(unsigned-byte 8)))
                        (head-end nil)
                        (head nil)
                        (resume 0))
                   (labels ((receive ()
                              (multiple-value-bind (b n) (sb-bsd-sockets:socket-receive s piece nil)
                                (declare (ignore b))
                                (when (or (null n) (zerop n))
                                  (return-from receive nil))
                                (loop for i below n do (vector-push-extend (aref piece i) buf))
                                n))
                            (answer (status text)
                              (let ((bytes (%ascii (format nil "HTTP/1.1 ~d X~aContent-Length: ~d~a~
                                                                Connection: close~a~a~a"
                                                           status *crlf* (length text) *crlf*
                                                           *crlf* *crlf* text))))
                                (sb-bsd-sockets:socket-send s bytes nil)))
                            (hold-until-released ()
                              (loop repeat 200 until (car release) do (sleep 0.05))
                              (sb-bsd-sockets:socket-close s))
                            (finish (body)
                              ;; The body has all arrived.
                              (cond (early-status)
                                    (silent (hold-until-released))
                                    (t (answer 200 (format nil "got:~d:~a" (length body)
                                                           (sha256-hex body)))))
                              (list head body)))
                     (block %upload-upstream-read
                       ;; The head.
                       (loop until head-end
                             do (unless (receive)
                                  (return-from %upload-upstream-read nil))
                                (setf head-end (web-skeleton::scan-crlf-crlf buf 0 (fill-pointer buf))))
                       (setf head (sb-ext:octets-to-string buf :end (+ head-end 4)
                                                               :external-format :latin-1))
                       (when early-status
                         (answer early-status "refused"))
                       ;; The body.
                       (let* ((start (+ head-end 4))
                              (declared (let ((at (search "content-length: " head :test #'char-equal)))
                                          (and at (parse-integer head :start (+ at 16) :junk-allowed t)))))
                         (setf resume start)
                         (loop
                           (let* ((end (fill-pointer buf))
                                  (got (- end start)))
                             (when (and close-after (>= got close-after))
                               (sb-bsd-sockets:socket-close s)
                               (return-from %upload-upstream-read (list head nil)))
                             (when (and stall-after (>= got stall-after))
                               (hold-until-released)
                               (return-from %upload-upstream-read (list head nil)))
                             (cond
                               (declared
                                (when (>= got declared)
                                  (return-from %upload-upstream-read
                                    (finish (coerce (subseq buf start (+ start declared))
                                                    '(simple-array (unsigned-byte 8) (*)))))))
                               (t
                                (let ((walk (coerce buf '(simple-array (unsigned-byte 8) (*)))))
                                  (multiple-value-bind (complete next)
                                      (web-skeleton::chunked-body-complete-p walk start end resume)
                                    (setf resume next)
                                    (when complete
                                      (return-from %upload-upstream-read
                                        (finish (web-skeleton::decode-chunked-body
                                                 walk start end))))))))
                             (when (plusp pace) (sleep pace))
                             (unless (receive)
                               (return-from %upload-upstream-read (list head nil)))))))))
               (error () nil))
          (ignore-errors (sb-bsd-sockets:socket-close listener))))
      :name "upload-upstream"))))

(defun test-harness-streamed-request-body-e2e ()
  "A request body forwarded upstream as it arrives, end to end: FORWARD-BODY
   piping a received body into a streamed fetch, the client answered with
   what the upstream said.

   Each case has an upstream of its own, %UPLOAD-UPSTREAM, which checks
   what it was sent. A body declared by Content-Length is forwarded with
   the same length, one sent chunked is forwarded chunked, and both arrive
   whole and in order. An upstream that reads slowly holds the client to
   its pace — the body pauses, the upload's queue never passes
   *MAX-WRITE-BACKLOG*, and the body still arrives whole. One that answers
   before the body is sent answers the client, and one that hangs up
   partway is a 502, both at once to a client that has sent part of its
   body and stopped, its body refused; a :THEN that raises there is a 500.
   An upstream that cannot be dialed at all is a 502 as well, :THEN
   hearing the abort sentinel. A client that hangs up partway leaves the
   upstream a request that ended short, never one that reads as whole, and
   a body still going upstream when the server drains ends as :SHUTDOWN."
  (format t "~%Harness: a request body forwarded upstream as it arrives~%")
  (let* ((lock (sb-thread:make-mutex :name "streamed-e2e"))
         (aborts nil)
         (thens nil)
         (most-queued 0)
         (pauses 0)
         (upstream-port (list nil))
         (body (%pseudo-random-bytes (* 3 1024 1024) 61)))
    (labels ((then (status headers b)
               (declare (ignore headers))
               ;; The upstream's status passes through, so a client can be
               ;; seen to hear it; the abort sentinel is a 502.
               (if (eql status 200)
                   (make-text-response 200 (sb-ext:octets-to-string b :external-format :latin-1))
                   (make-error-response (or status 502))))
             (then-for (path)
               ;; THEN, noting the status it heard for PATH.
               (lambda (status headers b)
                 (sb-thread:with-mutex (lock) (push (cons path status) thens))
                 (then status headers b)))
             (thens-for (path)
               (sb-thread:with-mutex (lock)
                 (loop for (p . s) in thens when (string= p path) collect s)))
             (upstream-url () (format nil "http://127.0.0.1:~d/up" (first upstream-port)))
             (aborts-for (path)
               (sb-thread:with-mutex (lock)
                 (loop for (p . r) in aborts when (string= p path) collect r)))
             (aborted (path)
               ;; As ABORTS-FOR, once an abort has had its moment to land.
               (loop repeat 100
                     until (aborts-for path)
                     do (sleep 0.05))
               (aborts-for path))
             (through (path &key chunk-size (pace 0) early-status close-after)
               ;; (values ANSWER (HEAD BODY)) for BODY posted to PATH, chunked
               ;; in CHUNK-SIZE pieces if given, with a fresh upstream that
               ;; takes the rest.
               (multiple-value-bind (port thread)
                   (%upload-upstream :pace pace :early-status early-status
                                     :close-after close-after)
                 (setf (first upstream-port) port)
                 (let ((answer (%exchange-or-why (%form-post-bytes path body
                                                                :chunk-size chunk-size
                                                                :headers '("Connection: close"))
                                              :seconds 30)))
                   (values answer
                           (handler-case (sb-thread:join-thread thread :timeout 20)
                             (error () nil))))))
             (part-then-wait (path part &rest upstream-args)
               ;; What the client hears for the head of BODY posted to PATH
               ;; and its first PART bytes, sending nothing more, with a
               ;; fresh upstream given UPSTREAM-ARGS. The client keeps its
               ;; side open, so the body is neither complete nor abandoned:
               ;; an answer has to come of the upstream alone.
               (multiple-value-bind (port thread) (apply #'%upload-upstream upstream-args)
                 (setf (first upstream-port) port)
                 (let* ((request (%form-post-bytes path body :headers '("Connection: close")))
                        (head-end (+ (web-skeleton::scan-crlf-crlf request 0 (length request)) 4)))
                   (prog1 (%exchange-or-why (subseq request 0 (+ head-end part))
                                            :half-close nil :seconds 10)
                     (handler-case (sb-thread:join-thread thread :timeout 20)
                       (error () nil)))))))
      (with-test-server
          (:handler (lambda (req) (declare (ignore req)) (make-error-response 404))
           :body-handler
           (lambda (req)
             (let ((path (http-request-path req)))
               (flet ((note-abort (conn reason)
                        (declare (ignore conn))
                        (sb-thread:with-mutex (lock) (push (cons path reason) aborts))))
                 (cond
                   ((string= path "/unreachable")
                    (forward-body (http-fetch :put "http://255.255.255.255:9/up" :body :stream
                                              :then (then-for path))
                                  :max-size nil :on-abort #'note-abort))
                   ((string= path "/early-raise")
                    (forward-body (http-fetch :put (upstream-url) :body :stream
                                              :then (lambda (status headers b)
                                                      (declare (ignore status headers b))
                                                      (error "a :then that raises")))
                                  :max-size nil :on-abort #'note-abort))
                   ((string= path "/length")
                    (forward-body (http-fetch :put (upstream-url) :body :stream
                                              :content-length (parse-integer
                                                               (get-header req "content-length"))
                                              :then #'then)
                                  :max-size nil :on-abort #'note-abort))
                   ;; By hand rather than by FORWARD-BODY, to watch the
                   ;; upload's queue and count the pauses.
                   ((string= path "/slow")
                    (let ((put (http-fetch :put (upstream-url) :body :stream :then #'then)))
                      (receive-body
                       :max-size nil
                       :on-data (lambda (conn bytes)
                                  (unless (fetch-started-p put) (fetch-into conn put))
                                  (let ((sent (fetch-send put bytes))
                                        (carrier (web-skeleton::http-fetch-continuation-carrier put)))
                                    (when carrier
                                      (setf most-queued
                                            (max most-queued
                                                 (web-skeleton::connection-write-pending carrier))))
                                    (cond ((eq sent t) nil)
                                          ((null sent) (incf pauses) :pause)
                                          (t (make-error-response 502)))))
                       :on-end (lambda (conn) (declare (ignore conn)) put)
                       :on-abort #'note-abort)))
                   (t
                    (forward-body (http-fetch :put (upstream-url) :body :stream :then #'then)
                                  :max-size nil :on-abort #'note-abort)))))))
        (let ((expected (format nil "got:~d:~a" (length body) (sha256-hex body))))
          ;; ---- forwarded whole, framed as it arrived ----
          (multiple-value-bind (answer seen) (through "/length")
            (check "length: the upstream was told the length the client declared"
                   (and seen (search (format nil "content-length: ~d" (length body))
                                     (first seen) :test #'char-equal)
                        t)
                   t)
            (check "length: and received the body whole and in order"
                   (and seen (equalp (second seen) body)) t)
            (check "length: the client heard what the upstream answered"
                   (and (search "HTTP/1.1 200" answer) (search expected answer) t) t))
          (multiple-value-bind (answer seen) (through "/chunked" :chunk-size 99991)
            (check "chunked: forwarded chunked"
                   (and seen (search "transfer-encoding: chunked" (first seen) :test #'char-equal) t)
                   t)
            (check "chunked: whole and in order, and the client heard the upstream"
                   (list (and seen (equalp (second seen) body)) (and (search expected answer) t))
                   '(t t)))
          ;; ---- a slow upstream holds the client to its pace ----
          (multiple-value-bind (answer seen) (through "/slow" :pace 0.002)
            (check "slow: the body still arrives whole"
                   (list (and seen (equalp (second seen) body)) (and (search expected answer) t))
                   '(t t))
            (check "slow: the body paused for the upstream" (plusp pauses) t)
            (check "slow: and the upload's queue stayed within *max-write-backlog*"
                   (<= most-queued web-skeleton:*max-write-backlog*) t))
          ;; ---- an upstream that answers first ----
          ;; Each client sends part of its body and then nothing, so the
          ;; answer cannot wait for a run of the body to carry it, nor the
          ;; body be complete before it, however much the sockets hold.
          (let ((answer (part-then-wait "/early" (* 64 1024) :early-status 413)))
            (check "early: the upstream's answer reached a client that had stopped sending"
                   (and (search "HTTP/1.1 413" answer) t) t))
          (check "early: and the rest of the body was refused" (aborted "/early") '(:refused))
          (let ((answer (part-then-wait "/early-raise" (* 64 1024) :early-status 413)))
            (check "early, :then raising: the client is answered 500"
                   (and (search "HTTP/1.1 500" answer) t) t))
          (check "early, :then raising: and the rest of the body was refused"
                 (aborted "/early-raise") '(:refused))
          ;; ---- an upstream that hangs up partway ----
          (let ((answer (part-then-wait "/fail" (* 320 1024) :close-after (* 256 1024))))
            (check "fail: an upstream that hangs up partway is a 502 to a client that had stopped sending"
                   (and (search "HTTP/1.1 502" answer) t) t))
          (check "fail: and the rest of the body was refused" (aborted "/fail") '(:refused))
          ;; ---- an upstream that cannot be dialed at all ----
          ;; A TCP connect to a broadcast address fails in the connect
          ;; itself, with ENETUNREACH, inside FETCH-INTO.
          (let ((answer (%exchange-or-why (%form-post-bytes "/unreachable" (subseq body 0 1000)
                                                            :headers '("Connection: close"))
                                          :seconds 10)))
            (check "unreachable: a connect refused at once is a 502"
                   (and (search "HTTP/1.1 502" answer) t) t))
          (check "unreachable: :then heard the abort sentinel, once"
                 (thens-for "/unreachable") '(nil))
          (check "unreachable: and the body was refused" (aborted "/unreachable") '(:refused))
          ;; ---- a client that leaves partway ----
          ;; The upstream must not be left holding what reads as a whole
          ;; upload: no chunked terminator, and short of a declared length.
          (multiple-value-bind (port thread) (%upload-upstream)
            (setf (first upstream-port) port)
            (multiple-value-bind (socket stream) (%raw-connect)
              (unwind-protect
                   (let ((request (%form-post-bytes "/gone" body
                                                    :headers '("Connection: close"))))
                     (handler-case
                         (progn (write-sequence request stream
                                                :end (floor (length request) 2))
                                (force-output stream))
                       (error () nil)))
                (ignore-errors (close stream :abort t))
                (ignore-errors (sb-bsd-sockets:socket-close socket))))
            (let ((seen (sb-thread:join-thread thread :timeout 20 :default :timed-out)))
              (check "gone: the upstream's request ended short of a whole body"
                     (and (not (eq seen :timed-out)) (null (second seen))) t)))
          (check "gone: and the body's end was heard as the client leaving"
                 (aborted "/gone") '(:disconnected))
          (check "nothing aborted the bodies forwarded whole"
                 (list (aborts-for "/length") (aborts-for "/chunked") (aborts-for "/slow"))
                 '(nil nil nil))))
      ;; ---- a body still going upstream when the server drains ----
      ;; The drain closes the fetch on its way to closing the connection,
      ;; and the body ends as the drain ends every body still arriving, not
      ;; as refused by the fetch's end. The check is made once
      ;; WITH-TEST-SERVER's teardown has drained it.
      (let ((release (list nil)) (thread nil) (socket nil) (stream nil))
        (unwind-protect
             (progn
               (with-test-server
                   (:handler (lambda (req) (declare (ignore req)) (make-error-response 404))
                    :body-handler
                    (lambda (req)
                      (declare (ignore req))
                      (forward-body (http-fetch :put (upstream-url) :body :stream :then #'then)
                                    :max-size nil
                                    :on-abort (lambda (conn reason)
                                                (declare (ignore conn))
                                                (sb-thread:with-mutex (lock)
                                                  (push (cons "/drain" reason) aborts))))))
                 (multiple-value-bind (port upstream)
                     (%upload-upstream :stall-after 1 :release release)
                   (setf (first upstream-port) port
                         thread upstream))
                 (multiple-value-setq (socket stream) (%raw-connect))
                 (write-sequence (%ascii (format nil "POST /drain HTTP/1.1~aHost: x~a~
                                                      Content-Length: 1000~a~a0123456789"
                                                 *crlf* *crlf* *crlf* *crlf*))
                                 stream)
                 (force-output stream)
                 (sleep 0.3))
               (check "drain: a body still going upstream hears :shutdown"
                      (aborted "/drain") '(:shutdown)))
          (setf (car release) t)
          (when thread (handler-case (sb-thread:join-thread thread :timeout 10) (error () nil)))
          (when stream (ignore-errors (close stream)))
          (when socket (ignore-errors (sb-bsd-sockets:socket-close socket))))))))

(defun test-harness-form-file-forwarded-e2e ()
  "One file of a form forwarded upstream as it arrives, end to end: its
   runs sent with FETCH-SEND from :ON-PART-DATA, :ON-END answering with
   the fetch, and the client answered with what the upstream said once it
   has.

   The form has a text field before the file and one after it. Both reach
   :ON-END collected while the file goes upstream unheld. The upstream,
   %UPLOAD-UPSTREAM, reads slowly, so the body pauses on the upload inside
   a run of the file and is resumed by the upload draining, and the field
   after the file is read only after that. A file with no content still
   makes its request, started at :ON-PART-END: the upstream gets an empty
   body, and the client its answer."
  (format t "~%Harness: a form's file forwarded upstream as it arrives~%")
  (let* ((lock (sb-thread:make-mutex :name "form-forward-e2e"))
         (fields nil)
         (pauses 0)
         (aborts nil)
         (upstream-port (list nil))
         (boundary "----FormBoundaryForwardQ3x")
         (file (%pseudo-random-bytes (* 2 1024 1024) 71)))
    (labels ((then (status headers b)
               (declare (ignore headers))
               (if (eql status 200)
                   (make-text-response 200 (sb-ext:octets-to-string b :external-format :latin-1))
                   (make-error-response 502)))
             (through (request &rest upstream-args)
               (multiple-value-bind (port thread) (apply #'%upload-upstream upstream-args)
                 (setf (first upstream-port) port)
                 (let ((answer (%exchange-or-why request :seconds 30)))
                   (values answer
                           (handler-case (sb-thread:join-thread thread :timeout 20)
                             (error () nil))))))
             (form (path content)
               (%form-post path boundary
                           (%multipart-body boundary `(("title" nil "before")
                                                       ("upload" "a.bin" ,content)
                                                       ("note" nil "after")))
                           :headers '("Connection: close"))))
      (with-test-server
          (:handler (lambda (req) (declare (ignore req)) (make-error-response 404))
           :body-handler
           (lambda (req)
             (let ((path (http-request-path req))
                   (put (http-fetch :put (format nil "http://127.0.0.1:~d/up" (first upstream-port))
                                    :body :stream :then #'then)))
               (receive-multipart
                req
                :max-size nil
                :on-part (lambda (conn part)
                           (declare (ignore conn))
                           (if (form-part-filename part) :stream nil))
                :on-part-data (lambda (conn part bytes)
                                (declare (ignore part))
                                (unless (fetch-started-p put)
                                  (fetch-into conn put))
                                (let ((sent (fetch-send put bytes)))
                                  (cond ((eq sent t) nil)
                                        ((null sent)
                                         (sb-thread:with-mutex (lock) (incf pauses))
                                         :pause)
                                        (t (make-error-response 502)))))
                :on-part-end (lambda (conn part)
                               ;; A file with no content never started it.
                               (when (and (form-part-filename part)
                                          (not (fetch-started-p put)))
                                 (fetch-into conn put))
                               nil)
                :on-end (lambda (conn parts)
                          (declare (ignore conn))
                          (sb-thread:with-mutex (lock)
                            (push (cons path (loop for p in parts
                                                   collect (cons (form-part-name p)
                                                                 (form-part-text p))))
                                  fields))
                          put)
                :on-abort (lambda (conn reason)
                            (declare (ignore conn))
                            (sb-thread:with-mutex (lock) (push (cons path reason) aborts)))))))
        (flet ((fields-of (path)
                 (sb-thread:with-mutex (lock) (cdr (assoc path fields :test #'string=)))))
          (multiple-value-bind (answer seen) (through (form "/file" file) :pace 0.002)
            (check "file: the upstream received the file, and only the file, whole"
                   (and seen (equalp (second seen) file)) t)
            (check "file: the client heard the upstream's answer"
                   (and (search (format nil "got:~d:~a" (length file) (sha256-hex file)) answer)
                        t)
                   t)
            (check "file: the fields either side of it reached :on-end"
                   (fields-of "/file") '(("title" . "before") ("note" . "after")))
            (check "file: the body paused on the upload" (plusp pauses) t))
          (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
            (multiple-value-bind (answer seen) (through (form "/empty" empty))
              (check "empty: the upstream got a request with an empty body"
                     (and seen (equalp (second seen) empty)) t)
              (check "empty: and the client its answer"
                     (and (search (format nil "got:0:~a" (sha256-hex empty)) answer) t) t)
              (check "empty: the fields reached :on-end"
                     (fields-of "/empty") '(("title" . "before") ("note" . "after")))))
          (check "nothing aborted either form" aborts nil))))))

(defun test-harness-streamed-request-body-timing-e2e ()
  "A streamed request body is timed on its progress: *FETCH-TIMEOUT*
   bounds the body standing still, not the upload.

   *FETCH-TIMEOUT* is 1 for the run. A client that sends its body a piece
   every quarter of a second, for three and a half seconds, is forwarded
   whole and answered 200: each piece moved the deadline out. An upstream
   that takes the head and a little of the body and then reads no more,
   holding the connection open, has the fetch ended once the body has
   stood still past the deadline, and the client is answered 502 while the
   upstream is still holding on. One that takes the whole body and never
   answers is a fetch whose answer did not come: the wait for it starts
   when the request has gone, and the client is answered 504.

   SETF rather than LET for *FETCH-TIMEOUT*: the worker reads it, and
   dynamic bindings do not cross MAKE-THREAD."
  (format t "~%Harness: a streamed request body is timed on its progress~%")
  (let ((saved web-skeleton:*fetch-timeout*)
        (upstream-port (list nil)))
    (setf web-skeleton:*fetch-timeout* 1)
    (unwind-protect
         (with-test-server
             (:handler (lambda (req) (declare (ignore req)) (make-error-response 404))
              :body-handler
              (lambda (req)
                (declare (ignore req))
                (forward-body
                 (http-fetch :put (format nil "http://127.0.0.1:~d/up" (first upstream-port))
                             :body :stream
                             :then (lambda (status headers b)
                                     (declare (ignore headers))
                                     (if (eql status 200)
                                         (make-text-response
                                          200 (sb-ext:octets-to-string b :external-format :latin-1))
                                         (make-error-response 502))))
                 :max-size nil)))
           (flet ((seconds-since (start)
                    (/ (float (- (get-internal-real-time) start))
                       internal-time-units-per-second)))
             ;; ---- a body that keeps moving outlasts the deadline ----
             (let* ((piece (%pseudo-random-bytes 1024 91))
                    (pieces 14)
                    (whole (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                                  (loop repeat pieces collect piece))))
               (multiple-value-bind (port thread) (%upload-upstream)
                 (setf (first upstream-port) port)
                 (multiple-value-bind (socket stream) (%raw-connect)
                   (unwind-protect
                        (let ((start (get-internal-real-time)))
                          (let* ((answer
                                   (handler-case
                                       (progn
                                         (write-sequence
                                          (%ascii (format nil "POST /up HTTP/1.1~aHost: x~a~
                                                               Content-Length: ~d~a~
                                                               Connection: close~a~a"
                                                          *crlf* *crlf* (length whole) *crlf*
                                                          *crlf* *crlf*))
                                          stream)
                                         (force-output stream)
                                         (loop repeat pieces
                                               do (sleep 0.25)
                                                  (write-sequence piece stream)
                                                  (force-output stream))
                                         (let ((got (read-until-bounded stream :seconds 10)))
                                           (sb-ext:octets-to-string (subseq got 0 (fill-pointer got))
                                                                    :external-format :latin-1)))
                                     (error (e) (format nil "no answer: ~a" e))))
                                 (secs (seconds-since start))
                                 (seen (handler-case (sb-thread:join-thread thread :timeout 10)
                                         (error () nil))))
                            (check "moving: the upload outlasted *fetch-timeout* several times over"
                                   (> secs 3) t)
                            (check "moving: and arrived whole" (and seen (equalp (second seen) whole)) t)
                            (check "moving: and the client heard the upstream"
                                   (and (search "HTTP/1.1 200" answer)
                                        (search (format nil "got:~d:~a" (length whole)
                                                        (sha256-hex whole))
                                                answer)
                                        t)
                                   t)))
                     (ignore-errors (close stream))
                     (ignore-errors (sb-bsd-sockets:socket-close socket))))))
             ;; ---- an upstream that stops reading ----
             ;; More body than the sockets on the way can hold, so it stands
             ;; still once the upstream has stopped.
             (let ((body (%pseudo-random-bytes (* 8 1024 1024) 92))
                   (release (list nil)))
               (multiple-value-bind (port thread)
                   (%upload-upstream :stall-after (* 64 1024) :release release)
                 (setf (first upstream-port) port)
                 (let* ((start (get-internal-real-time))
                        (answer (%exchange-or-why (%form-post-bytes "/up" body
                                                                 :headers '("Connection: close"))
                                               :seconds 20))
                        (secs (seconds-since start)))
                   (setf (first release) t)
                   (handler-case (sb-thread:join-thread thread :timeout 10)
                     (error () nil))
                   (check "stalled: the client was answered 502"
                          (and (search "HTTP/1.1 502" answer) t) t)
                   (check "stalled: once the body stood still, with the upstream holding on"
                          (< secs 8) t))))
             ;; ---- an upstream that takes the body and never answers ----
             ;; The request has gone, so this is the wait for an answer that
             ;; does not come: a 504, as for any fetch that goes unanswered.
             (let ((body (%pseudo-random-bytes (* 64 1024) 93))
                   (release (list nil)))
               (multiple-value-bind (port thread) (%upload-upstream :silent t :release release)
                 (setf (first upstream-port) port)
                 (let* ((start (get-internal-real-time))
                        (answer (%exchange-or-why (%form-post-bytes "/up" body
                                                                 :headers '("Connection: close"))
                                               :seconds 20))
                        (secs (seconds-since start)))
                   (setf (first release) t)
                   (let ((seen (handler-case (sb-thread:join-thread thread :timeout 10)
                                 (error () nil))))
                     (check "silent: the upstream had the whole body"
                            (and seen (equalp (second seen) body)) t))
                   (check "silent: the client was answered 504"
                          (and (search "HTTP/1.1 504" answer) t) t)
                   (check "silent: with the upstream holding on" (< secs 8) t))))))
      (setf web-skeleton:*fetch-timeout* saved))))

(defun test-harness-then-answers-with-a-started-fetch-e2e ()
  "A fetch's :THEN may answer with a streamed fetch started earlier for the
   same request, and the request is answered with what that fetch
   answered, as when a handler or ON-END answers with one.

   The body's first run goes to fetch A, which is finished with it, and
   the body waits until A is over; the rest goes to fetch B. ON-END
   answers with B, and B's :THEN answers with A, whose own :THEN ran when
   A ended. A's answer asks to close the connection, and the client, which
   did not ask, sees it closed once the answer is sent: the request's
   framing follows the answer that answers it."
  (format t "~%Harness: a fetch's :then answers with a fetch started earlier~%")
  (let ((ports (list nil nil))
        (body (%pseudo-random-bytes (* 512 1024) 95)))
    (flet ((url (which) (format nil "http://127.0.0.1:~d/up" (nth which ports))))
      (with-test-server
          (:handler (lambda (req) (declare (ignore req)) (make-error-response 404))
           :body-handler
           (lambda (req)
             (declare (ignore req))
             (let* ((a (http-fetch :put (url 0) :body :stream
                                   :then (lambda (status headers b)
                                           (declare (ignore headers))
                                           (let ((r (make-text-response
                                                     (if (eql status 200) 200 502)
                                                     (format nil "a:~a"
                                                             (and b (sb-ext:octets-to-string
                                                                     b :external-format :latin-1))))))
                                             (set-response-header r "connection" "close")
                                             r))))
                    (b (http-fetch :put (url 1) :body :stream
                                   :then (lambda (status headers b)
                                           (declare (ignore status headers b))
                                           a))))
               (receive-body
                :max-size nil
                :on-data (lambda (conn bytes)
                           (cond
                             ((not (fetch-started-p a))
                              (fetch-into conn a)
                              (fetch-send a bytes)
                              (fetch-finish a)
                              :pause)
                             (t
                              (unless (fetch-started-p b) (fetch-into conn b))
                              (if (eq (fetch-send b bytes) t) nil :pause))))
                :on-end (lambda (conn)
                          (declare (ignore conn))
                          (fetch-finish b)
                          b)))))
        (multiple-value-bind (port-a thread-a) (%upload-upstream)
          (multiple-value-bind (port-b thread-b) (%upload-upstream)
            (setf ports (list port-a port-b))
            (let* ((start (get-internal-real-time))
                   (reply (%exchange-or-why (%form-post-bytes "/two" body) :seconds 6 :half-close nil))
                   (secs (/ (float (- (get-internal-real-time) start))
                            internal-time-units-per-second))
                   (seen-a (handler-case (sb-thread:join-thread thread-a :timeout 10)
                             (error () nil)))
                   (seen-b (handler-case (sb-thread:join-thread thread-b :timeout 10)
                             (error () nil))))
              (check "two: A and B between them had the whole body, in order"
                     (and seen-a seen-b
                          (equalp (concatenate '(simple-array (unsigned-byte 8) (*))
                                               (second seen-a) (second seen-b))
                                  body))
                     t)
              (check "two: the client was answered with what A answered"
                     (and seen-a
                          (search (format nil "a:got:~d:~a" (length (second seen-a))
                                          (sha256-hex (second seen-a)))
                                  reply)
                          t)
                     t)
              (check "two: and the connection closed after it, as A's answer asked"
                     (< secs 4) t))))))))

(defun %answering-upstream (body-done &key (middle-after (* 1024 1024)))
  "A listener on an ephemeral port playing an upstream that answers an
   upload as it receives it, once. Returns (values PORT THREAD); the
   thread's result is the request body it received, decoded.

   It reads the head and answers at once with a chunked 200 and its first
   chunk, first. Once MIDDLE-AFTER bytes of body have arrived it sends a
   chunk middle. It reads the chunked body to its terminator, sets the CAR
   of BODY-DONE, and sends a chunk second, and the end."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen listener 1)
    (values
     (nth-value 1 (sb-bsd-sockets:socket-name listener))
     (sb-thread:make-thread
      (lambda ()
        (unwind-protect
             (handler-case
                 (let ((s (sb-bsd-sockets:socket-accept listener))
                       (buf (make-array 0 :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0))
                       (piece (make-array 16384 :element-type '(unsigned-byte 8)))
                       (head-end nil))
                   (flet ((receive ()
                            (multiple-value-bind (b n) (sb-bsd-sockets:socket-receive s piece nil)
                              (declare (ignore b))
                              (and n (plusp n)
                                   (loop for i below n
                                         do (vector-push-extend (aref piece i) buf)
                                         finally (return t)))))
                          (say (text)
                            (sb-bsd-sockets:socket-send s (%ascii text) nil)))
                     (block %answering-upstream-read
                       (loop until head-end
                             do (unless (receive) (return-from %answering-upstream-read nil))
                                (setf head-end (web-skeleton::scan-crlf-crlf
                                                buf 0 (fill-pointer buf))))
                       (say (format nil "HTTP/1.1 200 OK~aTransfer-Encoding: chunked~a~
                                         Connection: close~a~a5~afirst~a"
                                    *crlf* *crlf* *crlf* *crlf* *crlf* *crlf*))
                       (let ((start (+ head-end 4))
                             (resume (+ head-end 4))
                             (middle-sent nil))
                         (loop
                           (when (and (not middle-sent)
                                      (>= (- (fill-pointer buf) start) middle-after))
                             (setf middle-sent t)
                             (say (format nil "6~amiddle~a" *crlf* *crlf*)))
                           (let ((walk (coerce buf '(simple-array (unsigned-byte 8) (*)))))
                             (multiple-value-bind (complete next)
                                 (web-skeleton::chunked-body-complete-p
                                  walk start (length walk) resume)
                               (setf resume next)
                               (when complete
                                 (setf (car body-done) t)
                                 (say (format nil "6~asecond~a0~a~a"
                                              *crlf* *crlf* *crlf* *crlf*))
                                 (sb-bsd-sockets:socket-close s)
                                 (return-from %answering-upstream-read
                                   (web-skeleton::decode-chunked-body
                                    walk start (length walk))))))
                           (sleep 0.002)
                           (unless (receive)
                             (return-from %answering-upstream-read nil)))))))
               (error () nil))
          (ignore-errors (sb-bsd-sockets:socket-close listener))))
      :name "answering-upstream"))))

(defun test-harness-answer-paused-while-the-request-goes-out-e2e ()
  "An upstream that answers while the request body is still going out, and
   an :ON-BODY that pauses on the answer's first chunk: the pause holds the
   answer, and the request goes on going out.

   The upstream answers at the head, sends a second chunk partway through
   the body, and sends its last only once it has the whole body, and an
   :ON-TICK resumes the fetch only once the upstream has said so. So the
   body must reach the upstream while the fetch is paused, and nothing else
   lets the test finish: a pause that held the request as well would leave
   the upload waiting on a drain that never comes. The chunk sent partway
   arrives while the request's writes are still being driven, and is not
   read until the resume. Once resumed, the rest of the answer is read, and
   the client hears :THEN."
  (format t "~%Harness: an answer paused while the request goes out~%")
  (let* ((saved web-skeleton:*worker-wake-interval*)
         (lock (sb-thread:make-mutex :name "paused-answer"))
         (body (%pseudo-random-bytes (* 4 1024 1024) 99))
         (body-done (list nil))
         (out-box (list nil))
         (resumed nil)
         (chunks '())
         (upstream-port (list nil)))
    (setf web-skeleton:*worker-wake-interval* 0.05)
    (unwind-protect
         (with-test-server
             (:handler (lambda (req) (declare (ignore req)) (make-error-response 404))
              :body-handler
              (lambda (req)
                (declare (ignore req))
                (forward-body
                 (http-fetch :put (format nil "http://127.0.0.1:~d/up" (first upstream-port))
                             :body :stream
                             :on-body (lambda (conn chunk)
                                        (sb-thread:with-mutex (lock)
                                          (push (cons (sb-ext:octets-to-string
                                                       chunk :external-format :latin-1)
                                                      resumed)
                                                chunks))
                                        (unless (first out-box)
                                          (setf (first out-box) conn)
                                          :pause))
                             :then (lambda (status headers b)
                                     (declare (ignore headers b))
                                     (make-text-response 200 (format nil "status:~a" status))))
                 :max-size nil))
              :on-tick (lambda (worker-id)
                         (declare (ignore worker-id))
                         (when (and (first out-box) (car body-done) (not resumed))
                           (setf resumed t)
                           (fetch-resume (first out-box)))))
           (multiple-value-bind (port thread) (%answering-upstream body-done)
             (setf (first upstream-port) port)
             (let ((answer (%exchange-or-why (%form-post-bytes "/up" body
                                                               :headers '("Connection: close"))
                                             :seconds 30))
                   (seen (sb-thread:join-thread thread :timeout 20 :default nil)))
               (check "paused: the body reached the upstream while the answer was held"
                      (and seen (equalp seen body)) t)
               (check "paused: and the answer was read to its end, the rest only once resumed"
                      (sb-thread:with-mutex (lock) (reverse chunks))
                      '(("first" . nil) ("middle" . t) ("second" . t)))
               (check "paused: and the client heard :then"
                      (and (search "status:200" answer) t) t))))
      (setf web-skeleton:*worker-wake-interval* saved))))

(defun test-harness-streamed-request-body-from-a-tick-e2e ()
  "A request body produced on the worker rather than received: a handler
   answers with a streamed fetch, and :ON-TICK writes the fetch's body a
   piece at a time, stopping when FETCH-SEND answers NIL. On a later tick it
   asks again with an empty send, which sends nothing and answers T once the
   upstream has caught up. The client is parked on the fetch throughout and
   is answered with what the upstream said.

   The upstream reads slowly, so the producer is told to wait and then to
   go on, and between the two every piece arrives once and in order.

   SETF rather than LET for *WORKER-WAKE-INTERVAL*, which paces the ticks:
   the worker reads it, and dynamic bindings do not cross MAKE-THREAD."
  (format t "~%Harness: a request body produced on the worker's tick~%")
  (let* ((saved web-skeleton:*worker-wake-interval*)
         (lock (sb-thread:make-mutex :name "tick-producer"))
         (box (list nil))
         (polls '())
         (piece-size (* 64 1024))
         (pieces 48)
         (whole (%pseudo-random-bytes (* piece-size pieces) 97))
         (next 0)
         (upstream-port (list nil))
         (none (make-array 0 :element-type '(unsigned-byte 8))))
    (setf web-skeleton:*worker-wake-interval* 0.05)
    (unwind-protect
         (with-test-server
             (:handler (lambda (req)
                         (declare (ignore req))
                         (setf (first box)
                               (http-fetch :put (format nil "http://127.0.0.1:~d/up"
                                                        (first upstream-port))
                                           :body :stream
                                           :content-length (length whole)
                                           :then (lambda (status headers b)
                                                   (declare (ignore headers))
                                                   (if (eql status 200)
                                                       (make-text-response
                                                        200 (sb-ext:octets-to-string
                                                             b :external-format :latin-1))
                                                       (make-error-response 502))))))
              :on-tick (lambda (worker-id)
                         (declare (ignore worker-id))
                         (let ((put (first box)))
                           (when (and put (fetch-started-p put) (< next pieces))
                             (let ((ready (fetch-send put none)))
                               (sb-thread:with-mutex (lock) (push ready polls))
                               (loop while (and (eq ready t) (< next pieces))
                                     do (setf ready (fetch-send
                                                     put (subseq whole (* next piece-size)
                                                                 (* (1+ next) piece-size))))
                                        (incf next))
                               (when (= next pieces)
                                 (fetch-finish put)))))))
           (multiple-value-bind (port thread) (%upload-upstream :pace 0.005)
             (setf (first upstream-port) port)
             (let ((answer (%exchange-or-why
                            (%ascii (format nil "GET /export HTTP/1.1~aHost: x~a~
                                                 Connection: close~a~a"
                                            *crlf* *crlf* *crlf* *crlf*))
                            :seconds 30))
                   (seen (sb-thread:join-thread thread :timeout 20 :default nil)))
               (check "tick: the upstream had the whole body, in order"
                      (and seen (equalp (second seen) whole)) t)
               (check "tick: and the client was answered with what it said"
                      (and (search (format nil "got:~d:~a" (length whole) (sha256-hex whole))
                                   answer)
                           t)
                      t)
               (check "tick: the producer was told to wait, and then to go on"
                      (sb-thread:with-mutex (lock)
                        (list (and (member nil polls) t) (and (member t polls) t)))
                      '(t t)))))
      (setf web-skeleton:*worker-wake-interval* saved))))

(defun %black-hole (port)
  "A listener on 127.0.0.2:PORT that answers no connect. Its accept queue
   is full, held by a connection never accepted, so the kernel drops each
   SYN that arrives, and a connect to it waits as one to a black-holed
   route does. Returns (values LISTENER FILLER), for the caller to close."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp))
        (filler (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener #(127 0 0 2) port)
    (sb-bsd-sockets:socket-listen listener 0)
    (sb-bsd-sockets:socket-connect filler #(127 0 0 2) port)
    (values listener filler)))

(defun test-harness-fetch-falls-back-to-the-next-address-e2e ()
  "A name that resolves to more than one address is dialed at the next
   when one does not answer: a fetch to a name whose first address is
   127.0.0.2, where nothing answers, and whose second is the upstream's.

   Refused, the next address is dialed at once, and so it is when the
   connect fails in the call itself. Black-holed (%BLACK-HOLE),
   it is dialed once *CONNECT-FALLBACK-DELAY* has passed, and an upload
   streamed into the fetch meanwhile, queued on the first attempt, goes
   with it and arrives whole. HTTP-FETCH-STREAM's blocking connect falls
   back the same way.

   The resolution is the test's: *DNS-LOOKUP-FN* and
   *DNS-RESOLVE-BLOCKING-FN* answer every name with those two addresses,
   in that order, as getent would for a name listed twice in /etc/hosts."
  (format t "~%Harness: a fetch falls back to a name's next address~%")
  (let ((saved-lookup web-skeleton::*dns-lookup-fn*)
        (saved-blocking web-skeleton::*dns-resolve-blocking-fn*)
        (next-addresses (list (cons (coerce #(127 0 0 1)
                                            '(simple-array (unsigned-byte 8) (*)))
                                    :inet)))
        ;; A cell, so a case can change what the name resolves to first.
        (first-address (list (coerce #(127 0 0 2) '(simple-array (unsigned-byte 8) (*)))))
        (upstream-port (list nil))
        (body (%pseudo-random-bytes (* 256 1024) 101)))
    (setf web-skeleton::*dns-lookup-fn*
          (lambda (conn epoll-fd fetch-req host port path)
            (web-skeleton::initiate-http-fetch-to-address
             conn epoll-fd fetch-req host port path (car first-address) :inet
             :fallbacks next-addresses))
          web-skeleton::*dns-resolve-blocking-fn*
          (lambda (host)
            (declare (ignore host))
            (values (car first-address) :inet next-addresses)))
    (unwind-protect
         (with-test-server
             (:handler (lambda (req)
                         (if (string= (http-request-path req) "/plain")
                             (make-text-response 200 "plain")
                             (http-fetch :put (format nil "http://dual.test:~d/up"
                                                      (first upstream-port))
                                         :body "hello"
                                         :then (lambda (status headers b)
                                                 (declare (ignore headers))
                                                 (make-text-response
                                                  (or status 502)
                                                  (if b (sb-ext:octets-to-string
                                                         b :external-format :latin-1)
                                                      "none"))))))
              :body-handler
              (lambda (req)
                (when (string= (http-request-path req) "/upload")
                  (forward-body
                   (http-fetch :put (format nil "http://dual.test:~d/up" (first upstream-port))
                               :body :stream
                               :content-length (length body)
                               :then (lambda (status headers b)
                                       (declare (ignore headers))
                                       (make-text-response
                                        (or status 502)
                                        (if b (sb-ext:octets-to-string
                                               b :external-format :latin-1)
                                            "none"))))
                   :max-size nil))))
           ;; ---- refused: the next address at once ----
           (multiple-value-bind (port thread) (%upload-upstream)
             (setf (first upstream-port) port)
             (let ((answer (%exchange-or-why
                            (%ascii (format nil "GET /fetch HTTP/1.1~aHost: x~a~
                                                 Connection: close~a~a"
                                            *crlf* *crlf* *crlf* *crlf*))
                            :seconds 10)))
               (sb-thread:join-thread thread :timeout 10 :default nil)
               (check "refused: answered from the name's next address"
                      (and (search "HTTP/1.1 200" answer)
                           (search (format nil "got:5:~a" (sha256-hex (%ascii "hello")))
                                   answer)
                           t)
                      t)))
           ;; ---- unreachable at once: the next address, from the same call ----
           ;; A TCP connect to a broadcast address fails in the connect
           ;; itself, with ENETUNREACH, so no connection is ever made for it.
           (multiple-value-bind (port thread) (%upload-upstream)
             (setf (first upstream-port) port
                   (car first-address) (coerce #(255 255 255 255)
                                               '(simple-array (unsigned-byte 8) (*))))
             (let ((answer (%exchange-or-why
                            (%ascii (format nil "GET /fetch HTTP/1.1~aHost: x~a~
                                                 Connection: close~a~a"
                                            *crlf* *crlf* *crlf* *crlf*))
                            :seconds 10)))
               (sb-thread:join-thread thread :timeout 10 :default nil)
               (setf (car first-address) (coerce #(127 0 0 2)
                                                 '(simple-array (unsigned-byte 8) (*))))
               (check "unreachable at once: answered from the name's next address"
                      (and (search "HTTP/1.1 200" answer) t)
                      t)))
           ;; ---- black-holed: the next address once its budget is spent ----
           (multiple-value-bind (port thread) (%upload-upstream)
             (setf (first upstream-port) port)
             (multiple-value-bind (listener filler) (%black-hole port)
               (unwind-protect
                    (let* ((start (get-internal-real-time))
                           (answer (%exchange-or-why
                                    (%form-post-bytes "/upload" body
                                                      :headers '("Connection: close"))
                                    :seconds 20))
                           (secs (/ (float (- (get-internal-real-time) start))
                                    internal-time-units-per-second))
                           (seen (sb-thread:join-thread thread :timeout 10 :default nil)))
                      (check "black hole: the upload reached the next address whole"
                             (and seen (equalp (second seen) body)) t)
                      (check "black hole: and the client heard its answer"
                             (and (search (format nil "got:~d:~a" (length body)
                                                  (sha256-hex body))
                                          answer)
                                  t)
                             t)
                      (check "black hole: once the attempt's budget had passed, not the fetch's"
                             (< (float web-skeleton::*connect-fallback-delay*) secs 10.0)
                             t))
                 (ignore-errors (sb-bsd-sockets:socket-close filler))
                 (ignore-errors (sb-bsd-sockets:socket-close listener)))))
           ;; ---- the blocking path falls back the same way ----
           (check "blocking: http-fetch-stream answered from the next address"
                  (handler-case
                      (http-fetch-stream :get (format nil "http://dual.test:~d/plain"
                                                      *test-port*)
                                         :on-line #'identity)
                    (error (e) (princ-to-string e)))
                  200))
      (setf web-skeleton::*dns-lookup-fn* saved-lookup
            web-skeleton::*dns-resolve-blocking-fn* saved-blocking))))

(defun test-harness-a-response-is-written-from-its-read-e2e ()
  "An ordinary keep-alive response is written from the read that produced
   its request: one change of epoll interest per request, the keep-alive
   reset's re-arming of readability, where arming writability and coming
   back to write costs two. Pipelined requests keep the writability path,
   and each is answered, in order.

   Counted at EPOLL-MODIFY, wrapped for the run and restored after."
  (format t "~%Harness: a response is written from its read~%")
  (let ((real (symbol-function 'web-skeleton::epoll-modify))
        (mods 0)
        (lock (sb-thread:make-mutex :name "inline-count")))
    (unwind-protect
         (progn
           (setf (symbol-function 'web-skeleton::epoll-modify)
                 (lambda (&rest args)
                   (sb-thread:with-mutex (lock) (incf mods))
                   (apply real args)))
           (with-test-server
               (:handler (lambda (req)
                           (make-text-response 200 (http-request-path req))))
             (multiple-value-bind (socket stream) (%raw-connect)
               (unwind-protect
                    (labels ((ask (&rest paths)
                               ;; All of PATHS in one write; their answers,
                               ;; bodies only, in order.
                               (write-sequence
                                (%ascii (format nil "~{GET ~a HTTP/1.1~aHost: x~a~a~}"
                                                (loop for p in paths
                                                      append (list p *crlf* *crlf* *crlf*))))
                                stream)
                               (force-output stream)
                               (loop for p in paths
                                     collect (let ((tail 0))
                                               (loop until (= tail 4)
                                                     do (let ((b (read-byte stream)))
                                                          (setf tail
                                                                (cond ((and (member tail '(0 2)) (= b 13)) (1+ tail))
                                                                      ((and (member tail '(1 3)) (= b 10)) (1+ tail))
                                                                      ((= b 13) 1)
                                                                      (t 0)))))
                                               (let ((body (make-array (length p)
                                                                       :element-type '(unsigned-byte 8))))
                                                 (read-sequence body stream)
                                                 (sb-ext:octets-to-string body :external-format :ascii)))))
                             (bounded (thunk)
                               ;; THUNK's reads under a deadline, as every read
                               ;; in this file is, and a close or a reset
                               ;; answered as its text: a dropped response
                               ;; fails a check rather than hanging the suite,
                               ;; and a close fails one rather than ending it.
                               (multiple-value-bind (result completed)
                                   (web-skeleton-test-harness::call-with-read-deadline
                                    20 (lambda ()
                                         (handler-case (funcall thunk)
                                           (error (e) (princ-to-string e)))))
                                 (if completed result "no answer before the deadline"))))
                      (check "inline: the warm-up is answered"
                             (bounded (lambda () (dotimes (i 20) (ask "/warm")) :ok))
                             :ok)
                      (sb-thread:with-mutex (lock) (setf mods 0))
                      (check "inline: two hundred requests are answered"
                             (bounded (lambda () (dotimes (i 200) (ask "/one")) :ok))
                             :ok)
                      (check "inline: one change of interest per keep-alive request"
                             (sb-thread:with-mutex (lock) (/ mods 200)) 1)
                      (check "pipelined: three in one write, answered in order"
                             (bounded (lambda () (ask "/a" "/bb" "/ccc")))
                             '("/a" "/bb" "/ccc")))
                 (ignore-errors (close stream))
                 (ignore-errors (sb-bsd-sockets:socket-close socket))))))
      (setf (symbol-function 'web-skeleton::epoll-modify) real))))

(defun %exchange-or-why (bytes &rest args)
  "%RAW-EXCHANGE, except that a connection reset or closed while the request
   was still being written answers as the text of that error, so a check
   reports it rather than the run ending on it."
  (handler-case (apply #'%raw-exchange bytes args)
    (error (e) (format nil "no answer: ~a" e))))

(defun %form-post-bytes (path body &key chunk-size (headers '()))
  "A POST of BODY, bytes, to PATH: framed by Content-Length, or chunked in
   CHUNK-SIZE pieces. Bytes, ready to send."
  (concatenate
   '(simple-array (unsigned-byte 8) (*))
   (%ascii (format nil "POST ~a HTTP/1.1~aHost: x~a~
                        ~:[Content-Length: ~d~;Transfer-Encoding: chunked~*~]~a~
                        ~{~a~a~}~a"
                   path *crlf* *crlf* chunk-size (length body) *crlf*
                   (loop for h in headers append (list h *crlf*))
                   *crlf*))
   (if chunk-size
       (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
              (append
               (loop for at from 0 below (length body) by chunk-size
                     for to = (min (length body) (+ at chunk-size))
                     collect (%ascii (format nil "~x~a" (- to at) *crlf*))
                     collect (subseq body at to)
                     collect (%ascii *crlf*))
               (list (%ascii (format nil "0~a~a" *crlf* *crlf*)))))
       body)))

(defun test-harness-body-stall-e2e ()
  "A body a body handler accepted is timed on its progress, by
   *BODY-STALL-TIMEOUT*; a body nobody accepted stays on *IDLE-TIMEOUT*,
   a budget for the whole request.

   *IDLE-TIMEOUT* is 1 and *BODY-STALL-TIMEOUT* 2 for the bodies that keep
   arriving. A body arriving four bytes every 0.7 s takes about 3.5 s,
   well past the idle budget and never 2 s without a byte: accepted, it is
   served; not accepted, it is cut off.

   A body that stops arriving is closed by the stall clock, and not before
   the idle budget would have: the test that the clock really is a
   different one. The sweep runs once a second on whole-second clocks, so
   a close comes between T and T + 2 seconds after the last byte, and with
   the stall budget at 2 the two windows overlap. The bodies that stop
   arriving have it at 4: an idle close lands before 3 s, a stall close
   after 4 s.

   SETF for both timeouts, for the reason TEST-HARNESS-BODY-HANDLER-E2E
   gives."
  (format t "~%Harness: an accepted body is timed on its progress~%")
  (let ((saved-idle web-skeleton:*idle-timeout*)
        (saved-stall web-skeleton:*body-stall-timeout*)
        (received-abort nil))
    (setf web-skeleton:*idle-timeout* 1
          web-skeleton:*body-stall-timeout* 2)
    (unwind-protect
         (with-test-server
             (:handler
              (lambda (req)
                (make-text-response
                 200 (format nil "~a:~d" (http-request-path req)
                             (length (or (http-request-body req) #())))))
              :body-handler
              (lambda (req)
                (let ((path (http-request-path req)))
                  (cond
                    ((string= path "/accepted") (buffer-body :max-size 64))
                    ((string= path "/received")
                     (receive-body
                      :on-data (lambda (conn bytes) (declare (ignore conn bytes)) nil)
                      :on-end (lambda (conn) (declare (ignore conn))
                                (make-text-response 200 "done"))
                      :on-abort (lambda (conn reason) (declare (ignore conn))
                                  (setf received-abort reason))))
                    (t nil)))))
           (flet ((trickle (path)
                    ;; Head first, then the body four bytes every 0.7 s.
                    ;; Writes after the server has closed fail, and that is
                    ;; one of the outcomes under test, so they are let fail.
                    (multiple-value-bind (socket stream) (%raw-connect)
                      (unwind-protect
                           (progn
                             (write-sequence
                              (%ascii (format nil "POST ~a HTTP/1.1~aHost: x~a~
                                                   Content-Length: 20~a~
                                                   Connection: close~a~a"
                                              path *crlf* *crlf* *crlf* *crlf* *crlf*))
                              stream)
                             (force-output stream)
                             (ignore-errors
                              (dotimes (i 5)
                                (sleep 0.7)
                                (write-sequence (%ascii "abcd") stream)
                                (force-output stream)))
                             (let ((buf (read-until-bounded stream :seconds 5)))
                               (sb-ext:octets-to-string
                                (subseq buf 0 (fill-pointer buf))
                                :external-format :latin-1)))
                        (ignore-errors (close stream))
                        (ignore-errors (sb-bsd-sockets:socket-close socket))))))
             (check "accepted: a slow body that keeps arriving is served"
                    (and (search "/accepted:20" (trickle "/accepted")) t)
                    t)
             (check "control: the same body nobody accepted is cut off"
                    (search "/plain:20" (trickle "/plain"))
                    nil)
             ;; The clock is the request's. On a connection whose first
             ;; request had its body accepted, the next one — which nobody
             ;; accepted — is back on the whole-request budget. Kept, the
             ;; first request's clock would let the second trickle a body
             ;; past *IDLE-TIMEOUT*, a slow-drip budget handed to a request
             ;; that never asked for one.
             (multiple-value-bind (socket stream) (%raw-connect)
               (unwind-protect
                    (progn
                      (write-sequence
                       (%ascii (format nil "POST /accepted HTTP/1.1~aHost: x~a~
                                            Content-Length: 4~a~aabcd"
                                       *crlf* *crlf* *crlf* *crlf*))
                       stream)
                      (force-output stream)
                      (let ((first-answer
                              (read-until-bounded
                               stream :seconds 5
                                      :until (lambda (buf fill)
                                               (search (%ascii "/accepted:4") buf
                                                       :end2 fill)))))
                        (check "keep-alive: the first request, accepted, is served"
                               (and (search (%ascii "/accepted:4") first-answer) t)
                               t))
                      (write-sequence
                       (%ascii (format nil "POST /plain HTTP/1.1~aHost: x~a~
                                            Content-Length: 20~a~a"
                                       *crlf* *crlf* *crlf* *crlf*))
                       stream)
                      (force-output stream)
                      (ignore-errors
                       (dotimes (i 5)
                         (sleep 0.7)
                         (write-sequence (%ascii "abcd") stream)
                         (force-output stream)))
                      (let ((rest (read-until-bounded stream :seconds 5)))
                        (check "keep-alive: the next, not accepted, is cut off"
                               (search (%ascii "/plain:20") rest)
                               nil)))
                 (ignore-errors (close stream))
                 (ignore-errors (sb-bsd-sockets:socket-close socket)))))
           ;; Four bytes, then nothing. Closed by the stall clock: after
           ;; *BODY-STALL-TIMEOUT* of quiet and not at the idle budget.
           ;; The budget at 4 from here on, so the windows are apart: see
           ;; the docstring.
           (setf web-skeleton:*body-stall-timeout* 4)
           (multiple-value-bind (socket stream) (%raw-connect)
             (unwind-protect
                  (let ((start (get-internal-real-time)))
                    (write-sequence
                     (%ascii (format nil "POST /accepted HTTP/1.1~aHost: x~a~
                                          Content-Length: 20~a~aabcd"
                                     *crlf* *crlf* *crlf* *crlf*))
                     stream)
                    (force-output stream)
                    (multiple-value-bind (buf reason)
                        (read-until-bounded stream :seconds 10)
                      (let ((elapsed (/ (- (get-internal-real-time) start)
                                        internal-time-units-per-second)))
                        (check "stalled: the connection is closed, with nothing sent"
                               (list (and (member reason '(:eof :error)) t)
                                     (fill-pointer buf))
                               '(t 0))
                        (check "stalled: not at the idle budget, but after the stall"
                               (and (>= elapsed 3.5) (< elapsed 9) t)
                               t))))
               (ignore-errors (close stream))
               (ignore-errors (sb-bsd-sockets:socket-close socket))))
           ;; A received body is timed the same way, and its ON-ABORT is
           ;; told why it ended.
           (multiple-value-bind (socket stream) (%raw-connect)
             (unwind-protect
                  (progn
                    (write-sequence
                     (%ascii (format nil "POST /received HTTP/1.1~aHost: x~a~
                                          Content-Length: 20~a~aabcd"
                                     *crlf* *crlf* *crlf* *crlf*))
                     stream)
                    (force-output stream)
                    (read-until-bounded stream :seconds 10)
                    (check "stalled: a received body's ON-ABORT hears :stalled"
                           received-abort :stalled))
               (ignore-errors (close stream))
               (ignore-errors (sb-bsd-sockets:socket-close socket)))))
      (setf web-skeleton:*idle-timeout* saved-idle
            web-skeleton:*body-stall-timeout* saved-stall))))

(defun test-harness-fetch-into-target-closed-e2e ()
  "A client that leaves mid-relay tears the outbound down, once.

   CLOSE-CONNECTION's detached-outbound arm reaches its outbound by
   walking, because nothing was
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

(defun %stop-verdict-pass (&optional target-disposition)
  "One HANDLE-OUTBOUND-READ pass over a complete chunked response whose
   :ON-BODY returns :STOP on the first chunk and NIL on the rest.

   Returns (values FINAL CHUNKS FIRES REMAINING TARGET): the status the
   fetch callback was handed (:NEVER if it never ran), the chunk payloads
   in delivery order, how many times the callback fired, the connections
   still registered when the pass returned, and the target if one was
   asked for.

   TARGET-DISPOSITION, when given, puts a registered :WEBSOCKET target
   carrying it behind the outbound. Without it the outbound's INBOUND-FD is
   -1, which is the arrangement the first test wants: no target to
   survive, and a REMAINING of zero that means the outbound alone.

   Shared by the two tests below because only the assertions differ. The
   arrangement is the whole fixture — a pass carrying several chunks with a
   stop on the first — and written twice it would be two fixtures free to
   drift apart while each claimed to hold everything but its own question
   constant.

   A stub transport rather than a socket, for the reason
   TEST-FETCH-OK-EOF-WALKS-THE-BYTES uses one: whether several chunks land
   in one read is a scheduling question over a real peer, and both claims
   here need a multi-chunk pass on every run, not on most of them.

   The response is complete, terminator included. That is what lets the
   sentinel alone separate a stopped fetch from a delivered one — a
   COMPLETE-FETCH routing parses this body and reports 200, where against a
   truncated one it would raise and reach the abort sentinel by the error
   path instead, which is indistinguishable from the right answer
   downstream."
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
         (web-skeleton::*connections* (make-hash-table))
         ;; With TARGET-DISPOSITION given, a :WEBSOCKET target carrying it
         ;; is registered and the outbound points at it. Fd 4242 rather
         ;; than a real socket: nothing here writes to it, and the two
         ;; things a disposition would do to it — a queued close frame, a
         ;; state change — are both readable off the struct.
         (target (when target-disposition
                   (let ((c (web-skeleton::make-connection
                             :fd 4242
                             :state :websocket
                             :fetch-outstanding t
                             :fetch-failure-disposition target-disposition
                             :last-active (get-universal-time))))
                     (web-skeleton::register-connection c)
                     c))))
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
                      :inbound-fd (if target (web-skeleton::connection-fd target) -1)
                      :fetch-method :GET
                      :last-active (get-universal-time)
                      :fetch-on-body
                      (lambda (c chunk)
                        (declare (ignore c))
                        (push (sb-ext:octets-to-string
                               chunk :external-format :ascii)
                              chunks)
                        ;; Stop on the first, NIL for the rest: a walk that
                        ;; carried on past the verdict would hand those
                        ;; over and record their answers.
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
           ;; Registered so the reclamation reading means something.
           ;; Unregistered, the count is zero whether or not anything tore
           ;; the connection down.
           (web-skeleton::register-connection conn)
           (web-skeleton::handle-outbound-read conn epfd)
           ;; REMAINING counts the target too when there is one, so the
           ;; first test's "reclaimed" reading is taken without one.
           (values final (reverse chunks) fires
                   (hash-table-count web-skeleton::*connections*)
                   target))
      (ignore-errors (web-skeleton::%close epfd))
      ;; The stop's teardown closes the socket; this is for a pass that
      ;; never reached it. A closed socket takes a second close as a no-op.
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun test-fetch-stop-ends-the-pass-at-its-chunk ()
  "A :STOP on the first chunk of a pass ends the fetch there: no later
   chunk is handed over, though the whole response is in the buffer, and
   :THEN gets the abort sentinel.

   Two checks, each against its own wrong implementation. The chunks are
   the one against a walk that carries on past the verdict: it would hand
   over all three. The sentinel is the one against a stop that loses to the
   response completing: the terminator is in the buffer, and a walk left
   to reach it would have COMPLETE-FETCH report 200.

   %STOP-VERDICT-PASS has why the fixture is shaped as it is, and in
   particular why its response is complete — which is also why this is not
   a smaller copy of the end-to-end test, whose stopped upstream never
   sends a terminator at all."
  (format t "~%Fetch: :STOP ends the pass at its chunk~%")
  (multiple-value-bind (final chunks fires remaining)
      (%stop-verdict-pass)
    (check "stop: the fetch ended on the abort sentinel" final nil)
    (check "stop: no chunk after the stop was handed over"
           chunks (list (first *chunked-corpus*)))
    (check "stop: the fetch ended exactly once" fires 1)
    (check "stop: the outbound was reclaimed" remaining 0)))

(defun %ws-disposition-pass (disposition)
  "Run DELIVER-DETACHED's aborted arm against a :WEBSOCKET target carrying
   DISPOSITION, and report what became of it.

   Returns (values STATE QUEUED FIRES) — the target's state afterwards, the
   bytes sitting on its write path, and the callback count.

   At the seam rather than end to end, because the claim is about one arm
   of one case statement and an end-to-end WebSocket relay would reach it
   through a handshake, a frame decoder and an upstream failure, any of
   which could be what broke. Fd 4242 and no socket: nothing here writes,
   the epoll arm is already wrapped in IGNORE-ERRORS, and both observable
   consequences — a queued close frame, a state change — are slots.

   The callback count is returned because it must not move. :KEEP changes
   what happens to the connection and nothing about the fetch contract, so
   an implementation that suppressed the delivery along with the close
   would be a different and worse bug than the one being prevented."
  (let* ((fires 0)
         (epfd (web-skeleton::epoll-create))
         (web-skeleton::*connections* (make-hash-table)))
    (unwind-protect
         (let ((target (web-skeleton::make-connection
                        :fd 4242
                        :state :websocket
                        :fetch-outstanding t
                        :fetch-failure-disposition disposition
                        :last-active (get-universal-time))))
           (web-skeleton::register-connection target)
           (web-skeleton::deliver-detached
            4242 epfd
            (lambda (s h b) (declare (ignore s h b)) (incf fires) nil)
            :aborted)
           (values (web-skeleton::connection-state target)
                   (web-skeleton::connection-write-buf target)
                   fires))
      (ignore-errors (web-skeleton::%close epfd)))))

(defun test-fetch-failure-disposition-crosses-the-seam ()
  "FETCH-INTO writes the disposition and DELIVER-DETACHED reads it. This is
   the wire between them.

   The ends are covered elsewhere and the wire is not. The refusal tests
   reach FETCH-INTO's guards and stop there, which is before the write.
   The two disposition fixtures set the slot themselves through
   MAKE-CONNECTION's initarg, so they check the read against a value no
   production code put there. Delete the disposition clause from
   FETCH-INTO's setf and the slot keeps its :CLOSE default — :KEEP
   accepted, validated, and silently ignored — and every other test stays
   green.

   That mode is *pre-arranged*. A fixture performs the step
   production is responsible for performing, then checks the consumer of
   that step. It is not vacuous, it can fail; not partial, it covers its
   claim; not confounded, it passes for the right reason. What it cannot do
   is fail for any defect in the step it performed itself, and both ends
   read as covered while the middle is not. The rule it produces: when a
   value crosses a seam, revert the write as well as the read.

   INITIATE-FETCH is stubbed because the write is the last thing FETCH-INTO
   does before dialing. :KEEP is the only value worth asserting here —
   :CLOSE is the slot's resting value, so a write that never happened is
   indistinguishable from one that did.

   Two checks rather than one, and not for redundancy: the first names the
   write and the second names its effect, so a failure list says whether
   FETCH-INTO stopped writing or DELIVER-DETACHED stopped reading."
  (format t "~%Fetch: the disposition survives the trip from fetch-into~%")
  (let ((real (symbol-function 'web-skeleton::initiate-fetch)))
    (setf (symbol-function 'web-skeleton::initiate-fetch)
          (lambda (conn epoll-fd fetch-req)
            (declare (ignore conn epoll-fd fetch-req))
            t))
    (unwind-protect
         (let* ((epfd (web-skeleton::epoll-create))
                (web-skeleton::*connections* (make-hash-table))
                (web-skeleton::*epoll-fd* epfd))
           (unwind-protect
                (let ((target (web-skeleton::make-connection
                               :fd 4242
                               :state :websocket
                               :last-active (get-universal-time))))
                  (web-skeleton::register-connection target)
                  (fetch-into target
                              (http-fetch :get "http://127.0.0.1:1/x"
                                          :then (lambda (s h b)
                                                  (declare (ignore s h b))
                                                  nil))
                              :failure-disposition :keep)
                  (check "disposition seam: fetch-into wrote it to the target"
                         (web-skeleton::connection-fetch-failure-disposition
                          target)
                         :keep)
                  (web-skeleton::deliver-detached
                   4242 epfd
                   (lambda (s h b) (declare (ignore s h b)) nil)
                   :aborted)
                  (check "disposition seam: and the failed fetch left it open"
                         (web-skeleton::connection-state target) :websocket))
             (ignore-errors (web-skeleton::%close epfd))))
      (setf (symbol-function 'web-skeleton::initiate-fetch) real))))

(defun test-fetch-into-refuses-a-foreign-connection ()
  "FETCH-INTO refuses a connection this worker does not own.

   The seventh guard, and the only one that cannot be answered from the
   connection alone. A cross-worker call satisfies every other check — the
   state is right, *EPOLL-FD* is bound, nothing is outstanding — so the
   outbound is created and registered here and runs correctly. Delivery is
   what fails: DELIVER-DETACHED and STOP-FETCH both reach the target
   through (LOOKUP-CONNECTION TARGET-FD), and *CONNECTIONS* is per-worker
   while fds are unique across the process, so that lookup answers NIL.

   The consequence: FETCH-OUTSTANDING is set and never cleared, so every
   later FETCH-INTO on that connection is refused for the rest of its
   life, and the failure disposition never applies, so a :STREAMING
   target sits to
   *STREAM-IDLE-TIMEOUT* when its upstream fails.

   Two calls differing only in registration. The refused one is the claim.
   The registered one guards against over-refusal — a guard that refuses
   everything passes every test written only about what it rejects, which
   is the trap the sibling refusal test names in its own docstring.

   It is not a control in this suite's usual sense and should not be read
   as one: it does not pass on both sides. Against main's source the first
   call is accepted, which sets FETCH-OUTSTANDING, so the second is refused
   by the sixth guard and this assertion fails too — as a cascade from the
   defect rather than independently of it. Both failing is the correct
   result there, and the reason is worth knowing before someone reads two
   failures as two defects.

   INITIATE-FETCH is stubbed because the control has to get *past* the
   guards without dialing. The assertion is on the message rather than on
   the fact of a raise, since six other guards also raise and only one of
   them is this one."
  (format t "~%Fetch-into: a connection this worker does not own~%")
  (let ((real (symbol-function 'web-skeleton::initiate-fetch))
        (epfd (web-skeleton::epoll-create)))
    (setf (symbol-function 'web-skeleton::initiate-fetch)
          (lambda (conn epoll-fd fetch-req)
            (declare (ignore conn epoll-fd fetch-req))
            t))
    (unwind-protect
         (let* ((web-skeleton::*connections* (make-hash-table))
                (web-skeleton::*epoll-fd* epfd)
                (conn (web-skeleton::make-connection
                       :fd 4242
                       :state :streaming
                       :last-active (get-universal-time))))
           ;; Not registered: what a connection owned by another worker
           ;; looks like from here.
           (check "fetch-into: a connection this worker does not own is refused"
                  (let ((msg (attempt
                              (fetch-into conn
                                          (http-fetch
                                           :get "http://127.0.0.1:1/x"
                                           :then (lambda (s h b)
                                                   (declare (ignore s h b))
                                                   nil))))))
                    (and (stringp msg)
                         (search "not on this worker's connection table" msg)
                         t))
                  t)
           ;; The control. Registering the same connection is the only
           ;; difference, and the call then reaches INITIATE-FETCH.
           (web-skeleton::register-connection conn)
           (check "fetch-into: and the same call is accepted once it is owned"
                  (attempt
                   (fetch-into conn
                               (http-fetch
                                :get "http://127.0.0.1:1/x"
                                :then (lambda (s h b)
                                        (declare (ignore s h b))
                                        nil))))
                  t))
      (setf (symbol-function 'web-skeleton::initiate-fetch) real)
      (ignore-errors (web-skeleton::%close epfd)))))

(defun test-fetch-failure-disposition-both-directions ()
  "A failed detached fetch closes a WebSocket under :CLOSE and does not
   under :KEEP.

   Both directions, and the second one is not the interesting half. A
   parameter that is read but never obeyed passes the :KEEP checks alone —
   the connection survives because nothing was ever going to close it — so
   the :CLOSE half is what proves the default still applies and that the
   opt-out is an opt-out rather than a removal.

   The close frame is compared against BUILD-WS-CLOSE's own output rather
   than against hand-written bytes. What is being asserted is that a 1011
   close was queued, not what a close frame looks like; the second question
   has its own tests and one definition."
  (format t "~%Fetch: :failure-disposition, both directions~%")
  (multiple-value-bind (state queued fires) (%ws-disposition-pass :close)
    (check "disposition :close: the target is closing" state :closing)
    (check "disposition :close: a 1011 close frame was queued"
           (and queued
                (equalp queued (web-skeleton::build-ws-close 1011))
                t)
           t)
    (check "disposition :close: the callback still fired once" fires 1))
  (multiple-value-bind (state queued fires) (%ws-disposition-pass :keep)
    (check "disposition :keep: the target is still a websocket"
           state :websocket)
    (check "disposition :keep: nothing was written to it" queued nil)
    (check "disposition :keep: the callback still fired once" fires 1)))

(defun %ws-refused-close-pass ()
  "Run DELIVER-DETACHED's aborted :WEBSOCKET arm against a target already at
   *MAX-WRITE-BACKLOG*, and report what became of it.

   Returns (values REGISTERED FD PENDING FIRES) — whether the target is
   still in *CONNECTIONS*, its descriptor, the bytes still queued on it,
   and the callback count.

   A real descriptor rather than %WS-DISPOSITION-PASS's invented 4242,
   because this is the one case whose outcome is CLOSE-CONNECTION actually
   running, and it closes what it is handed. An epoll fd is owned and
   closeable, which is all this one has to be; the UNWIND-PROTECT's own
   close of it becomes a harmless EBADF afterwards.

   The bound is lowered rather than two megabytes being queued against it.
   What the arm sees either way is CONNECTION-APPEND-WRITE answering NIL,
   and it is reached here through the real refusal — a genuine backlog
   measured against the real bound — rather than by stubbing the one
   function whose answer is the entire precondition.

   Filled to exactly the bound, so the refusal does not quietly depend on
   how long a 1011 frame happens to be. PENDING is returned for the same
   reason: it is the guard separating \"the close did not happen\" from
   \"the frame was never refused in the first place\", the second of which
   would be a green assertion about a path this test is not about."
  (let* ((fires 0)
         (epfd (web-skeleton::epoll-create))
         (targetfd (web-skeleton::epoll-create))
         (web-skeleton::*connections* (make-hash-table))
         (web-skeleton::*max-write-backlog* 8))
    (unwind-protect
         (let ((target (web-skeleton::make-connection
                        :fd targetfd
                        :state :websocket
                        :fetch-outstanding t
                        :fetch-failure-disposition :close
                        :last-active (get-universal-time))))
           (web-skeleton::register-connection target)
           (web-skeleton::connection-append-write
            target (make-array 8 :element-type '(unsigned-byte 8)
                                 :initial-element 0))
           (web-skeleton::deliver-detached
            targetfd epfd
            (lambda (s h b) (declare (ignore s h b)) (incf fires) nil)
            :aborted)
           (values (and (web-skeleton::lookup-connection targetfd) t)
                   (web-skeleton::connection-fd target)
                   (web-skeleton::connection-write-pending target)
                   fires))
      (ignore-errors (web-skeleton::%close targetfd))
      (ignore-errors (web-skeleton::%close epfd)))))

(defun test-fetch-aborted-ws-at-the-backlog-bound ()
  "A failed detached fetch tears its WebSocket target down even when there
   is no room left to say why.

   The aborted :WEBSOCKET arm queues a 1011 close and then, under :CLOSE,
   has to mark the connection for teardown. Marking it was conditional on
   the frame being accepted, which reads as caution and is not: a target at
   *MAX-WRITE-BACKLOG* is the one that most needs ending, and it is also a
   plausible reason the relay feeding it failed at all. Refused, the
   connection stayed :WEBSOCKET with a full queue and nothing marking it,
   and the stall sweep collected it a *WRITE-STALL-TIMEOUT* later — a
   timeout doing the work of a decision this arm had already reached.

   The descriptor is the assertion and the state is not. CONNECTION-CLOSE
   sets :CLOSING on its way out and so does the accepted-frame branch, so
   state alone cannot tell a connection that was torn down from one merely
   marked. An fd of -1 and an empty *CONNECTIONS* can.

   The callback count is here for the reason it is in
   %WS-DISPOSITION-PASS: what becomes of the socket must not change what
   the fetch contract already promised, and a teardown that swallowed the
   delivery would be a worse bug than the one being fixed."
  (format t "~%Fetch: an aborted ws target at its backlog bound~%")
  (multiple-value-bind (registered fd pending fires) (%ws-refused-close-pass)
    ;; First that the fixture reached the condition at all. Eight bytes
    ;; against a bound of eight leaves room for nothing, so the close frame
    ;; was refused whole, and what follows is this arm's guard being false
    ;; rather than some unrelated path being green.
    (check "backlog bound: the close frame was refused, not queued"
           pending 8)
    (check "backlog bound: the target was unregistered" registered nil)
    (check "backlog bound: its descriptor was closed" fd -1)
    (check "backlog bound: the callback still fired once" fires 1)))

(defun test-fetch-stop-ignores-the-failure-disposition ()
  "A stopped fetch's target survives under :CLOSE as well as :KEEP.

   The interaction, and the inference it refuses is both wrong and
   reasonable: :STOP delivers the abort sentinel, :FAILURE-DISPOSITION
   governs what an aborted fetch does to the target, its default is
   :CLOSE — so returning :STOP must close the connection, which is the
   opposite of what the verdict is for.

   It does not, because the sentinel and the disposition are separate
   facts. A stop ends through STOP-FETCH and CLOSE-OUTBOUND, which reach
   neither of DELIVER-DETACHED's two callers, so no disposition is
   consulted under either setting.

   :CLOSE is the discriminating half. An implementation that routed a stop
   through the disposition would pass this at :KEEP and close the target at
   the default — the wrong answer arriving at the setting nobody changed,
   which is the shape that ships."
  (format t "~%Fetch: a stop is not governed by :failure-disposition~%")
  (dolist (disposition '(:close :keep))
    (multiple-value-bind (final chunks fires remaining target)
        (%stop-verdict-pass disposition)
      (declare (ignore final chunks fires remaining))
      (check (format nil "stop vs ~a: the target survived" disposition)
             (web-skeleton::connection-state target) :websocket)
      (check (format nil "stop vs ~a: nothing was written to it" disposition)
             (web-skeleton::connection-write-buf target) nil)
      (check (format nil "stop vs ~a: and it can fetch again" disposition)
             (web-skeleton::connection-fetch-outstanding target) nil))))

(defun test-harness-fetch-into-stop-e2e ()
  "A stop ends the upstream and hands back a connection that still works.

   The want this verdict exists for, end to end: stop this upstream, keep
   this connection. Three claims, and they fail in different directions.

   The upstream never closes, so within the client's read window nothing
   but the stop can end this fetch — *FETCH-TIMEOUT* is 30 seconds and the
   window is 8, which is what keeps the deadline sweeper from passing this
   test on the stop's behalf.

   :THEN must see the abort sentinel, which is what catches this wrong
   implementation: COMPLETE-FETCH with :FRAMING-COMPLETE T — a copy of the
   arm directly above it in the cond, and so the likeliest mistake —
   suppresses the body decode, delivers a
   200, and passes every other check here: the disposition leaves the
   target alone, the stream survives, the second fetch runs. Only this
   check fails, on exactly the report the verdict exists to prevent.

   Plain COMPLETE-FETCH, without that keyword, is caught elsewhere and not
   here, which is worth stating rather than leaving to be discovered. A
   stopped walk has not reached a terminator, so COMPLETE-FETCH refuses
   the relay as ended early and routes it to DELIVER-FETCH-ERROR, and
   :THEN receives the sentinel by the wrong road. That road applies the
   failure disposition, so TEST-FETCH-STOP-IGNORES-THE-FAILURE-DISPOSITION
   is the detector for it: at :CLOSE the target does not survive.

   The target must survive, asserted through its own body: the client
   decodes what was relayed plus what was written after the stop,
   terminator and all. It earns its place against a stop that never
   happens and against one that leaves the connection unusable, both of
   which end with no terminator ever written. It is not evidence about
   *routing*, and reading it that way would overstate it — the second
   fetch sets the outstanding marker again before DELIVER-DETACHED
   consults it, which suppresses the disposition, so even a stop routed
   through the failure path leaves this stream standing.

   A counted :ON-BODY assertion lived here and is gone. The callback
   increments unconditionally and returns :STOP unconditionally, and a stop
   can only happen through it, so a check that it fired was true in every
   run where the sentinel check could pass — green under every revert that
   leaves the callback wired, which is all of them. Its name claimed an
   ordering its expression never checked, which is this project's
   prose-versus-code failure occurring inside the suite. The decode above
   already proves the callback fired and relayed, so nothing was lost with
   it. The mode is *entailed*: an assertion that cannot fail in any run
   where a neighbour passes, because the neighbour's precondition is the
   thing it checks.

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

   The count is not read from :THEN. COMPLETE-FETCH tears the outbound
   down before it invokes the callback, so a peak read there is 0, and
   only the non-vacuity check would say so rather than pass on nothing."
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
  (test-harness-large-body-grows-the-buffer-e2e)
  (test-harness-split-head-then-pipelined-e2e)
  (test-harness-full-buffer-answered-promptly-e2e)
  (test-harness-pipelined-with-fin-e2e)
  (test-worker-id-is-public-and-holds-still-per-connection)
  (test-connection-serial-does-not-repeat-when-an-fd-does)
  (test-connection-serial-survives-a-worker-restart)
  (test-harness-pipelined-after-body-e2e)
  (test-harness-chunked-keepalive-e2e)
  (test-harness-chunked-trailer-smuggle-e2e)
  (test-harness-chunked-body-cap-e2e)
  (test-harness-chunked-expect-100-continue-e2e)
  (test-harness-cached-response-survives-head-e2e)
  (test-harness-a-head-with-no-fields-e2e)
  (test-harness-content-length-spellings-e2e)
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
  (test-harness-fetch-latin-1-header-e2e)
  (test-harness-accept-failure-is-retried-e2e)
  (test-harness-client-errors-log-once-a-minute-e2e)
  (test-harness-outbound-head-budget-and-search-e2e)
  (test-harness-dns-all-addresses-refused-e2e)
  (test-harness-address-filter-asked-once-e2e)
  (test-harness-hostname-fetches-leave-no-zombie-e2e)
  (test-harness-http11-server-close-stamps-connection-close-e2e)
  (test-refuse-connection-drains)
  (test-harness-connection-limit-e2e)
  (test-harness-refusals-are-reported-not-logged-each)
  (test-harness-workers-zero-rejected)
  (test-harness-write-stall-timeout-zero-rejected)
  (test-harness-write-backlog-minimum-rejected)
  (test-harness-fetch-stream-plain-e2e)
  (test-harness-fetch-stream-plain-read-timeout-e2e)
  (test-harness-fetch-stream-plain-lines-arrive-as-sent-e2e)
  (test-harness-fetch-stream-plain-write-timeout-e2e)
  (test-harness-fetch-bodiless-status-kept-alive-e2e)
  (test-harness-fetch-non-chunked-coding-e2e)
  (test-harness-upstream-header-limits-e2e)
  (test-harness-serves-without-tls-e2e)
  (test-harness-a-worker-left-behind-stays-with-its-run)
  (test-harness-drain-closes-a-keep-alive-response-it-let-finish)
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
  (test-harness-fetch-on-body-relays-past-the-cap-e2e)
  (test-harness-fetch-into-relays-past-the-cap-e2e)
  (test-harness-fetch-on-body-stop-parked-e2e)
  (test-harness-stream-does-not-hold-worker-e2e)
  (test-harness-census-outbound-returns-to-zero-e2e)
  (test-harness-fetch-into-refusals)
  (test-harness-fetch-into-setup-failure)
  (test-harness-detached-deadline-sweep)
  (test-harness-detached-deadline-spares-a-draining-pause)
  (test-harness-fetch-into-relay-e2e)
  (test-harness-fetch-into-relays-to-a-slow-client-e2e)
  (test-harness-fetch-into-chained-e2e)
  (test-harness-fetch-into-dns-failure-e2e)
  (test-harness-ipv6-listener-e2e)

  (test-harness-fetch-into-upstream-stalls-e2e)
  (test-harness-fetch-into-refuses-malformed-chunks-e2e)
  (test-harness-body-handler-e2e)
  (test-harness-receive-body-e2e)
  (test-harness-receive-body-paused-e2e)
  (test-harness-receive-body-refused-from-a-callback-e2e)
  (test-harness-paused-body-waits-on-its-fetch-e2e)
  (test-harness-receive-multipart-e2e)
  (test-harness-receive-multipart-paused-e2e)
  (test-harness-body-refused-from-inside-its-callback-e2e)
  (test-harness-streamed-request-body-e2e)
  (test-harness-form-file-forwarded-e2e)
  (test-harness-streamed-request-body-timing-e2e)
  (test-harness-then-answers-with-a-started-fetch-e2e)
  (test-harness-streamed-request-body-from-a-tick-e2e)
  (test-harness-answer-paused-while-the-request-goes-out-e2e)
  (test-harness-fetch-falls-back-to-the-next-address-e2e)
  (test-harness-a-response-is-written-from-its-read-e2e)
  (test-harness-body-stall-e2e)
  (test-harness-fetch-into-target-closed-e2e)
  (test-fetch-stop-ends-the-pass-at-its-chunk)
  (test-fetch-stop-ignores-the-failure-disposition)
  (test-fetch-failure-disposition-both-directions)
  (test-fetch-aborted-ws-at-the-backlog-bound)
  (test-fetch-failure-disposition-crosses-the-seam)
  (test-fetch-into-refuses-a-foreign-connection)
  (test-harness-fetch-into-stop-e2e)
  (report-suite "Harness")
  (zerop *tests-failed*))
