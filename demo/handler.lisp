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

(defvar *instance* nil
  "This process, named. Four hex characters, minted at START-DEMO or handed
   in by a deployer.

   The framework has no such thing and should not grow one. A deployer
   already has an identity for a process — a container id, a hostname, a task
   name — and a second one minted underneath it would be an identity
   competing with the real one, which is the same argument that keeps a
   metrics format out of the framework. So the application mints it, and
   demo/deploy lets WS_INSTANCE override it with whatever the deployment
   already calls this box.

   Fresh on every start when it is random, which is what makes it useful
   across a restart: the codes differ, so two runs of the same box are two
   names. That is also why a start time is not in the handle below — it would
   be answering a question this already answers.")

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
      ((and (eq method :GET) (string= path "/census"))
       (handle-census request))
      ((and (eq method :GET) (string= path "/bench"))
       (handle-bench request))
      ((and (eq method :GET) (string= path "/lab/base64"))
       (handle-lab-base64 request))
      ((and (eq method :GET) (string= path "/lab/hash"))
       (handle-lab-hash request))
      ((and (eq method :GET) (string= path "/lab/jwt"))
       (handle-lab-jwt request))
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

(defun %port-already-served-p (port)
  "T if something already answers on PORT of the loopback interface.

   Every listener this framework opens sets SO_REUSEPORT, which is what lets
   one worker per core hold the same port. The cost is that a *second server
   process* binding it does not fail either — it joins. Both then serve, the
   kernel splits arriving connections between them by 4-tuple hash, and
   nothing anywhere reports it: no error, no warning, no log line on either
   side.

   For this demo that surfaces as the room quietly dividing. Each process has
   its own bulletin, so a tab only ever hears the tabs the kernel hashed the
   same way, and which group a tab lands in is invisible from inside it. It
   costs an afternoon to diagnose and one connect() to prevent.

   The check races anything started in the gap before the bind, which is
   fine: the case that actually happens is a previous run that did not die."
  (handler-case
      (let ((probe (make-instance 'sb-bsd-sockets:inet-socket
                                  :type :stream :protocol :tcp)))
        (unwind-protect
             (progn (sb-bsd-sockets:socket-connect probe #(127 0 0 1) port)
                    t)
          (ignore-errors (sb-bsd-sockets:socket-close probe))))
    (error () nil)))

;;; ---------------------------------------------------------------------------
;;; The bench — refusals, run against the real parser
;;;
;;; A browser physically cannot send a malformed request: fetch() normalises
;;; everything, and XMLHttpRequest refuses the header names that would matter.
;;; So the framing argument is the one part of the README a reader has to take
;;; entirely on faith, and the point of this panel is that they do not have to.
;;;
;;; The server hands each case's exact bytes to PARSE-REQUEST — the same
;;; exported entry point an application would use — and reports what it
;;; actually said. Nothing is described, simulated, or remembered: change the
;;; parser and the panel changes on the next click.
;;;
;;; No socket, no thread, no subprocess. One parse per click, on bytes fixed
;;; at compile time, which is what keeps a public page from being a button
;;; marked "load the server".
;;;
;;; Scope worth stating: PARSE-REQUEST validates a header block. The framing
;;; rules that refuse Transfer-Encoding alongside Content-Length live in the
;;; connection read path, because they are about a body arriving on a live
;;; socket rather than about a block of headers — so this panel exhibits the
;;; refusals reachable from outside the framework, not every refusal there is.

(defun %peer-handle (conn)
  "A name for the connection CONN, for one public line of chatter.

   Three parts, and it takes all three. The instance says which process, so
   two servers behind one proxy do not both call somebody 1:38. The worker
   says which loop, because the serial only counts within a worker and two
   of them reach 38 independently. The serial says which connection, and it
   has to be the serial rather than the fd: an fd is unique only among the
   sockets open at this instant, and the kernel hands the lowest free one to
   the next accept, so a peer labelled by fd inherits the label of whoever
   held it a moment ago.

   Not the address. This is a public broadcast to strangers, and the source
   IP of a stranger is not the page's to publish. What this does reveal is
   which worker someone landed on and roughly how many connections that
   worker has taken, both of which the census already shows everyone."
  (format nil "~a:~a:~d"
          (or *instance* "????")
          (or *worker-id* "?")
          (connection-serial conn)))

(defun %crlf (&rest lines)
  (format nil "~{~a~c~c~}~c~c"
          (loop for l in lines append (list l #\Return #\Newline))
          #\Return #\Newline))

(defparameter *bench-cases*
  (list
   (list :id "fold"
         :title "obsolete line folding"
         :bytes (%crlf "GET /x HTTP/1.1" "Host: h" "X-Note: one" "  two")
         :why "RFC 7230 deprecated folding because two readers disagree about
               where a value ends. Refused by both readers here rather than
               unfolded by one of them.")
   (list :id "version"
         :title "a version token that is not HTTP/1.0 or 1.1"
         :bytes (%crlf "GET /x HTTP/9.9" "Host: h")
         :why "505 rather than a guess. A server that treats an unknown
               version as 1.1 is deciding on the client's behalf what
               framing rules apply to the bytes after it.")
   (list :id "absolute"
         :title "absolute-form request target"
         :bytes (%crlf "GET http://elsewhere/x HTTP/1.1" "Host: h")
         :why "Only origin-form is accepted. One accepted shape is one shape
               to get wrong, and behind a reverse proxy this form does not
               arrive. A boundary, written down in Limitations.")
   (list :id "ctl"
         :title "a control byte in a header value"
         :bytes (format nil "GET /x HTTP/1.1~c~cHost: h~c~cX-Note: a~cb~c~c~c~c"
                        #\Return #\Newline #\Return #\Newline
                        (code-char 7) #\Return #\Newline #\Return #\Newline)
         :why "Checked against the same table the serializer uses on the way
               out, so the framework will not accept a byte it would refuse
               to emit.")
   (list :id "ok"
         :title "a well-formed request, for contrast"
         :bytes (%crlf "GET /x HTTP/1.1" "Host: h" "Accept: */*")
         :why "The control. A panel where everything is refused proves only
               that something is refusing.")))

(defun %bench-response (c)
  "The response the server builds for this case, or NIL if it is accepted.

   The same three calls CONNECTION-ON-READ makes on its parse-error path —
   MAKE-ERROR-RESPONSE, a Connection: close header, FORMAT-RESPONSE — so
   these are the framework's own bytes rather than the page's drawing of
   them. Drawing them is what the lab panel next door refuses to do, and the
   appendix has no business doing it either.

   Built once at load time rather than per click, and that is load-bearing
   rather than thrift. FORMAT-RESPONSE counts what it serializes, and
   *COUNTERS* is NIL off a worker: at load there are no workers, so a
   response nobody will ever receive stays out of the census. Per click it
   would add a 4xx to the numbers on the other tab for traffic that never
   happened, which is the page disagreeing with the server about what the
   server did."
  (let ((status (handler-case (progn (parse-request (getf c :bytes)) nil)
                  (http-parse-error (e) (or (http-parse-error-status e) 400))
                  (error () 400))))
    (when status
      (let ((resp (make-error-response status)))
        (set-response-header resp "connection" "close")
        (sb-ext:octets-to-string (format-response resp)
                                 :external-format :latin-1)))))

(defparameter *bench-responses*
  (mapcar (lambda (c) (cons (getf c :id) (%bench-response c))) *bench-cases*)
  "Case id to the response bytes for it, as a string. NIL for a case the
   parser accepts, where what happens next is the application's business and
   not this panel's to invent.")

(defun %bench-case-json (c)
  (let ((resp (cdr (assoc (getf c :id) *bench-responses* :test #'string=))))
    (make-json-object
     (list (cons "id"    (getf c :id))
           (cons "title" (getf c :title))
           (cons "bytes" (getf c :bytes))
           (cons "response" (or resp ""))
           (cons "why"   (substitute #\Space #\Newline (getf c :why)))))))

(defun %run-bench-case (c)
  "Hand the bytes to the real parser and report what it said, verbatim."
  (handler-case
      (progn (parse-request (getf c :bytes))
             "accepted")
    (http-parse-error (e)
      (format nil "~d  ~a" (or (http-parse-error-status e) 400)
              (http-parse-error-message e)))
    (error (e) (format nil "~a" e))))

(defun handle-bench (request)
  "With no :case, the catalogue. With one, that case run against the parser.

   The catalogue carries the bytes, so what the page displays and what the
   server parses are the same string — a panel that held its own copy could
   show one thing and run another, which is the failure this whole page is
   an argument against."
  (let ((wanted (get-query-param request "case")))
    (if wanted
        (let ((c (find wanted *bench-cases* :key (lambda (x) (getf x :id))
                                            :test #'string=)))
          (if c
              (make-text-response
               200 (json-serialize
                    (make-json-object
                     (list (cons "id" (getf c :id))
                           (cons "result" (%run-bench-case c)))))
               :content-type "application/json")
              (make-error-response 404)))
        (make-text-response
         200 (json-serialize (mapcar #'%bench-case-json *bench-cases*))
         :content-type "application/json"))))

;;; ---------------------------------------------------------------------------
;;; The lab — ordinary requests, shown whole
;;;
;;; Three GETs a visitor might actually want done: base64 either way, a
;;; digest, and a JWT taken apart. Useful on their own, which is the point —
;;; a demo page gets opened a second time if one of its tabs does a job. All
;;; three already live in src/algorithms, so the demo reaches for the
;;; framework rather than carrying a copy of anything.
;;;
;;; Every answer carries the same envelope: which worker ran it, how long the
;;; server spent inside it, and the request as the server parsed it.
;;;
;;; The echo is the part worth explaining. fetch() does not expose the bytes
;;; the browser put on the wire, so a panel drawing its own picture of the
;;; request would be showing a reconstruction and calling it the thing —
;;; which is the failure the rest of this page is an argument against. The
;;; server writes back what it received. It goes to the caller who sent it
;;; and to nobody else, which is why this may carry headers when /census
;;; deliberately carries nothing about anyone.

(defparameter *lab-input-max* 2048
  "Longest accepted lab input. The framework bounds the request line well
   below this already; the cap is here so an over-long value is answered with
   a sentence the page can show rather than a connection-level refusal it
   cannot explain.")

(defparameter *unix-epoch* (encode-universal-time 0 0 0 1 1 1970 0)
  "2208988800. Unix seconds plus this is a Lisp universal time.")

(defun %now-us ()
  "Microseconds, for timing one handler.

   Not GET-INTERNAL-REAL-TIME: on SBCL 2.6 it does not advance between
   adjacent calls, and two hundred FORMATs between two reads still measured
   zero. Every lab request reported itself as instantaneous, which reads as
   a broken field rather than as a fast server.

   This was CLOCK_MONOTONIC, which is the right clock and was the wrong
   call. SB-UNIX:CLOCK-GETTIME is external on some SBCLs and internal on
   others, so the demo compiled on the machine it was written on and failed
   to READ on a Debian image — not a worse number, a build that stopped
   before it started. Reaching into another package's internals to measure
   microseconds on a demo page was not a trade worth making.

   GET-TIME-OF-DAY is external, has been for over a decade, and has the
   resolution. What it costs is that it is wall time, so an interval
   measured across a clock step is not an interval; the subtraction that
   uses this clamps at zero, so a backward step reports nothing rather than
   a negative count of microseconds. For a handler that runs in tens of
   microseconds, beside a round trip this page does not control, that is the
   cheaper of the two wrongs."
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ (* sec 1000000) usec)))

(defun %lab-request-echo (request)
  "The request as the parser holds it, written back in the form it arrived.

   Rebuilt from the struct rather than kept as raw bytes, deliberately: what
   this shows is the request that was actually dispatched. A header the
   parser dropped is absent here, which is the honest answer to the question
   the panel is asking."
  (with-output-to-string (s)
    (format s "~a ~a~@[?~a~] HTTP/~a~c~c"
            (symbol-name (http-request-method request))
            (http-request-path request)
            (http-request-query request)
            (http-request-version request)
            #\Return #\Newline)
    (loop for (name . value) in (http-request-headers request)
          do (format s "~a: ~a~c~c" name value #\Return #\Newline))
    (format s "~c~c" #\Return #\Newline)))

(defun %lab-json (request started pairs &key error (status 200))
  "The envelope every lab endpoint answers with.

   STARTED is a %NOW-US taken at the top of the handler, so
   SERVER_US is the server's own time inside the request and the page can
   subtract it from the round trip it measured to see what was network and
   browser. Two clocks, each reported by the side that owns it, rather than
   one number asked to mean both."
  (let ((us (max 0 (- (%now-us) started)))
        (resp nil))
    (setf resp
          (make-text-response
           status
           (json-serialize
            (make-json-object
             (append
              (list (cons "worker" (or *worker-id* :null))
                    (cons "server_us" us)
                    (cons "request" (%lab-request-echo request)))
              (if error
                  (list (cons "error" error))
                  (list (cons "result" (make-json-object pairs)))))))
           :content-type "application/json"))
    ;; Also a header, so the worker appears in the raw response the page
    ;; renders rather than only in a body it had to parse to find it.
    (set-response-header resp "x-worker"
                         (if *worker-id* (princ-to-string *worker-id*) "none"))
    resp))

(defun %lab-input (request name &key (required t))
  "A query parameter, length-checked. The second value is a message when the
   first is unusable, so a caller tests one thing and reports the other."
  (let ((v (get-query-param request name)))
    (cond ((null v)
           (if required
               (values nil (format nil "missing query parameter ~a" name))
               (values nil nil)))
          ((> (length v) *lab-input-max*)
           (values nil (format nil "~a is ~:d characters and the cap is ~:d"
                               name (length v) *lab-input-max*)))
          (t (values v nil)))))

(defun %lab-b64 (op s)
  "Run OP over S. The second value is a note when the answer needs one."
  (flet ((in () (sb-ext:string-to-octets s :external-format :utf-8))
         (out (bytes)
           ;; A decode can produce bytes that are not text in any encoding.
           ;; Hex beats replacement characters presented as the answer.
           (handler-case
               (values (sb-ext:octets-to-string bytes :external-format :utf-8)
                       nil)
             (error ()
               (values (bytes-to-hex bytes)
                       "the decoded bytes are not valid UTF-8, shown as hex")))))
    (cond ((string= op "encode")     (values (base64-encode (in)) nil))
          ((string= op "encode-url") (values (base64url-encode (in)) nil))
          ((string= op "decode")     (out (base64-decode s)))
          ((string= op "decode-url") (out (base64url-decode s)))
          (t (error "op must be encode, decode, encode-url or decode-url, not ~s"
                    op)))))

(defun handle-lab-base64 (request)
  "base64 and base64url, both directions."
  (let ((started (%now-us)))
    (multiple-value-bind (s err) (%lab-input request "s")
      (if err
          (%lab-json request started nil :error err :status 400)
          (let ((op (or (get-query-param request "op") "encode")))
            (handler-case
                (multiple-value-bind (out note) (%lab-b64 op s)
                  (%lab-json request started
                             (append (list (cons "op" op)
                                           (cons "in" s)
                                           (cons "out" out))
                                     (when note (list (cons "note" note))))))
              (error (e)
                (%lab-json request started nil
                           :error (format nil "~a" e) :status 400))))))))

(defun %lab-hash (alg s key)
  (let ((bytes (sb-ext:string-to-octets s :external-format :utf-8)))
    (cond ((string= alg "sha256") (sha256-hex bytes))
          ((string= alg "sha1")   (sha1-hex bytes))
          ((string= alg "hmac-sha256")
           (unless (and key (plusp (length key)))
             (error "hmac-sha256 needs a key; give one in the key field"))
           (bytes-to-hex
            (hmac-sha256 (sb-ext:string-to-octets key :external-format :utf-8)
                         bytes)))
          (t (error "alg must be sha256, sha1 or hmac-sha256, not ~s" alg)))))

(defun handle-lab-hash (request)
  "SHA-256, SHA-1, or HMAC-SHA256 over the text given."
  (let ((started (%now-us)))
    (multiple-value-bind (s err) (%lab-input request "s")
      (if err
          (%lab-json request started nil :error err :status 400)
          (multiple-value-bind (key key-err)
              (%lab-input request "key" :required nil)
            (if key-err
                (%lab-json request started nil :error key-err :status 400)
                (let ((alg (or (get-query-param request "alg") "sha256")))
                  (handler-case
                      (%lab-json request started
                                 (list (cons "alg" alg)
                                       (cons "in" s)
                                       (cons "bytes_in"
                                             (length (sb-ext:string-to-octets
                                                      s :external-format :utf-8)))
                                       (cons "hex" (%lab-hash alg s key))))
                    (error (e)
                      (%lab-json request started nil
                                 :error (format nil "~a" e) :status 400))))))))))

(defun %utc-string (unix)
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (+ unix *unix-epoch*) 0)
    (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month date hour min sec)))

(defun %relative (unix now)
  "How far UNIX is from NOW, in the largest unit that is not silly."
  (let* ((d (- unix now))
         (a (abs d)))
    (multiple-value-bind (n unit)
        (cond ((< a 60)    (values a "second"))
              ((< a 3600)  (values (round a 60) "minute"))
              ((< a 86400) (values (round a 3600) "hour"))
              (t           (values (round a 86400) "day")))
      (format nil "~d ~a~p ~a" n unit n (if (minusp d) "ago" "from now")))))

(defun %jwt-segments (token)
  "Split TOKEN on dots, keeping empty segments — a JWS whose signature is
   empty still has three parts, and collapsing that would report it as
   malformed for the wrong reason."
  (let ((out nil) (start 0))
    (loop
      (let ((dot (position #\. token :start start)))
        (push (subseq token start (or dot (length token))) out)
        (if dot (setf start (1+ dot)) (return))))
    (nreverse out)))

(defun %jwt-claim-times (payload now)
  "The three registered time claims, decoded. Absent ones stay absent."
  (loop for name in '("iat" "nbf" "exp")
        for v = (json-get payload name)
        when (integerp v)
          collect (cons name (format nil "~a  (~a)"
                                     (%utc-string v) (%relative v now)))))

(defun handle-lab-jwt (request)
  "Take a JWT apart. Decoded, never verified, and the answer says so."
  (let ((started (%now-us)))
    (multiple-value-bind (token err) (%lab-input request "token")
      (if err
          (%lab-json request started nil :error err :status 400)
          (handler-case
              (let ((parts (%jwt-segments token)))
                (unless (= (length parts) 3)
                  (error "a JWS has three dot-separated parts and this has ~d"
                         (length parts)))
                (flet ((seg (i)
                         (json-parse (sb-ext:octets-to-string
                                      (base64url-decode (nth i parts))
                                      :external-format :utf-8))))
                  (let* ((header (seg 0))
                         (payload (seg 1))
                         (sig (base64url-decode (third parts)))
                         (now (- (get-universal-time) *unix-epoch*))
                         (times (%jwt-claim-times payload now)))
                    (%lab-json
                     request started
                     (list (cons "header" header)
                           (cons "payload" payload)
                           (cons "signature_bytes" (length sig))
                           (cons "times" (make-json-object times))
                           (cons "verified" :false)
                           (cons "note"
                                 "decoded, not verified. Verifying needs the issuer's key, which a public box has no business being handed."))))))
            (error (e)
              (%lab-json request started nil
                         :error (format nil "~a" e) :status 400)))))))

;;; ---------------------------------------------------------------------------
;;; Vitals — what the server can say about itself
;;;
;;; Aggregate only. Counts and states, never anything about one visitor: no
;;; paths, no addresses, no headers, no message contents. The page is public.

(defvar *started-at* nil
  "Universal time START-DEMO ran. An application fact, not a framework one —
   the framework has no opinion about when this app began.")

(defun %states-json (states)
  "A :STATES plist into a JSON object with string keys.

   Walks the plist rather than naming the states it expects. CONNECTION-CENSUS
   calls :STATES diagnostic and keys it by the connection state machine's own
   keywords, and asks consumers to render unknown keys generically — a panel
   matching a closed list would silently stop showing whatever was added."
  (make-json-object
   (loop for (state n) on states by #'cddr
         collect (cons (string-downcase (symbol-name state)) n))))

;; :COUNTERS is a plist like :STATES, so it is rendered the same way — walked
;; rather than named. The census docstring says new keys will appear here and
;; asks consumers not to match a closed list; a panel that named the counters
;; it knew about would go quietly blank on the first one that was added.
(defun %plist-json (plist)
  (make-json-object
   (loop for (k v) on plist by #'cddr
         collect (cons (substitute #\_ #\- (string-downcase (symbol-name k)))
                       v))))

(defun %worker-json (slot)
  "One census slot, or an empty one for a worker that has not published yet."
  (make-json-object
   (list (cons "total"    (if slot (getf slot :total 0) 0))
         (cons "states"   (%states-json (and slot (getf slot :states))))
         (cons "counters" (%plist-json (and slot (getf slot :counters)))))))

(defun handle-census (request)
  "Server-derived vitals as JSON, for the sternum and limbs panels."
  (declare (ignore request))
  (let* ((c (connection-census))
         (json (json-serialize
                (make-json-object
                 (list (cons "uptime" (if *started-at*
                                          (- (get-universal-time) *started-at*)
                                          0))
                       (cons "workers" (or (getf c :workers) 0))
                       (cons "total"   (or (getf c :total) 0))
                       ;; The floor under fan-out, shown because it is the
                       ;; cost of sharing nothing and the page may as well
                       ;; say what it is rather than apologise for it.
                       (cons "cadence_ms"
                             (round (* *worker-wake-interval* 1000)))
                       (cons "counters" (%plist-json (getf c :counters)))
                       (cons "per_worker"
                             (mapcar #'%worker-json (getf c :per-worker))))))))
    (make-text-response 200 json :content-type "application/json")))

;;; ---------------------------------------------------------------------------
;;; The bulletin — a live broadcast with no history
;;;
;;; Everyone connected sees what is posted while they are connected. Nothing
;;; is replayed on connect, so the shared structure is not a log: it is a
;;; hand-off buffer, holding a line between the worker that received it and
;;; every other worker's next tick. Its lifetime is one tick, not "recent
;;; history", which is why a few seconds and a couple of hundred entries are
;;; generous bounds rather than tight ones.
;;;
;;; The whole ring lives under one key and is replaced atomically. Assigning
;;; a sequence number and inserting the line have to be one step: two steps
;;; race, because a worker could take seq 5 while another takes 6 and inserts
;;; it, a fan-out could see 6 and advance past it, and 5 would then be
;;; inserted behind a watermark that had already gone by — delivered to
;;; nobody, silently.

(defparameter *bulletin-window* 5
  "Seconds a line stays in the hand-off buffer. Only has to outlast one
   fan-out tick on every worker; the rest is slack for a busy one.")

(defparameter *bulletin-max* 256
  "Hard cap on buffered lines whatever the window says. The pair is the
   same shape as the framework's own *MAX-WRITE-BACKLOG* and
   *WRITE-STALL-TIMEOUT*: how much may pile up, and for how long.")

(defparameter *bulletin-line-max* 500
  "Longest line accepted. Public box, strangers' text.")

(defvar *bulletin* nil
  "Shared store holding the ring under :RING, newest first.")

(defvar *fanned* nil
  "Per-worker vector of the last sequence that worker has fanned out. Each
   worker writes only its own slot, so no lock — the same share-nothing
   shape as the framework's connection census.")

(defun %bulletin-trim (ring now)
  "Drop what is older than the window or past the cap. RING is newest first,
   so both bounds are a prefix take."
  (loop for entry in ring
        for kept from 0 below *bulletin-max*
        while (<= (- now (third entry)) *bulletin-window*)
        collect entry))

(defun bulletin-post (text handle)
  "Append TEXT from HANDLE under a fresh sequence number. Atomic: the read of
   the last sequence, the increment, the insert and the trim are one update.

   HANDLE is carried rather than looked up later because by the time a line
   is fanned out the connection that sent it may be gone, and the worker
   doing the fanning is usually not the worker that took it in."
  (let ((now (get-universal-time)))
    (store-update *bulletin* :ring
                  (lambda (ring)
                    (let ((next (1+ (if ring (first (first ring)) 0))))
                      (cons (list next text now handle)
                            (%bulletin-trim ring now)))))
    nil))

(defun bulletin-since (seq)
  "Lines newer than SEQ, oldest first. The ring descends by sequence, so the
   walk stops at the first entry already seen rather than scanning it all."
  (let ((ring (store-get *bulletin* :ring)))
    (nreverse
     (loop for entry in ring
           while (> (first entry) seq)
           collect entry))))

(defun %utc-clock (universal)
  "HH:MM:SSZ, in UTC.

   Stamped by the server that took the line rather than by each browser when
   it arrives, so everyone reading the bulletin sees one time for a post
   instead of their own. UTC rather than anywhere's local time for the same
   reason: a shared broadcast wants a shared clock, and a page that guessed
   at a visitor's zone would print a different time to each of them for the
   same event.

   Seconds and no date. Nothing here outlives the window by more than a
   handful of seconds, so a date would be the same on every line."
  (multiple-value-bind (sec min hour) (decode-universal-time universal 0)
    (format nil "~2,'0d:~2,'0d:~2,'0dZ" hour min sec)))

(defun %bulletin-payload (entry)
  "Wire form: sequence, TAB, UTC clock, TAB, sender handle, TAB, the line.

   Three tabs now, and the line itself still cannot contain one —
   %SANITIZE-LINE strips every byte below 32, TAB among them. So a client
   splits on the first three and whatever is left is the text, however many
   tabs somebody tried to type into it.

   The sequence stays on the wire even though the page stopped printing it
   in every line. It is the mechanism — the number each worker compares
   against to know what it has not handed out yet — it costs nothing to
   send, and somebody reading the socket should be able to see the thing the
   workers actually compare. What it is not is something to read a thousand
   times: it counts from server start and only goes up, so on a process that
   stays up for months it becomes seven digits of noise in front of every
   sentence."
  (build-ws-text (format nil "~d~a~a~a~a~a~a"
                         (first entry) #\Tab
                         (%utc-clock (third entry)) #\Tab
                         (or (fourth entry) "?") #\Tab
                         (second entry))))

(defun bulletin-tick (worker-id)
  "Fan out to the connections THIS worker owns. Runs on that worker's event
   loop, which is the only place WS-SEND to them is legal."
  (let ((new (bulletin-since (aref *fanned* worker-id))))
    (when new
      (let ((payloads (mapcar #'%bulletin-payload new)))
        (map-worker-websockets
         (lambda (conn)
           ;; One peer at *MAX-WRITE-BACKLOG* must not cost everyone behind
           ;; it the batch. MAP-WORKER-WEBSOCKETS deliberately does not
           ;; decide this; an application that broadcasts does.
           (handler-case
               (dolist (p payloads) (ws-send conn p))
             (error () nil)))))
      (setf (aref *fanned* worker-id)
            (reduce #'max new :key #'first)))))

;;; ---------------------------------------------------------------------------
;;; WebSocket handler
;;; ---------------------------------------------------------------------------

(defun handle-ws-message (conn frame)
  "Post to the bulletin. Returns NIL: the sender sees their own line through
   the same fan-out as everyone else, one tick later.

   Echoing it back immediately would feel faster and would be a lie — the
   sender would see an ordering nobody else sees. The demo is here to show
   the mechanism, latency included."
  (cond
    ((= (ws-frame-opcode frame) +ws-op-binary+)
     (%ws-command conn frame))
    ((= (ws-frame-opcode frame) +ws-op-text+)
     (let ((text (sb-ext:octets-to-string (ws-frame-payload frame)
                                          :external-format :utf-8)))
       (bulletin-post (%sanitize-line text) (%peer-handle conn)))
     nil)
    (t nil)))

(defun %ws-command (conn frame)
  "Answer a control frame, on the asking connection only.

   Binary rather than text, and that is what keeps the two channels apart.
   The bulletin box sends text and only text, so nothing a visitor can type
   reaches this, and nothing answered here can be mistaken for a posted line.

   The answer is text with no TAB in it. A bulletin line always has exactly
   one, put there by %BULLETIN-PAYLOAD and impossible to post because
   %SANITIZE-LINE strips it — so the client tells the two apart by
   construction rather than by sniffing a prefix that a line could imitate.

   Which worker, and who this connection is, are the only things to ask so
   far. Both are worth asking because both hold still for the life of the
   socket, and because the worker is not the one that will serve the next
   request from the same browser: one connection, one worker, for as long as
   it is open. The handle comes back so a page can recognise its own lines
   in a broadcast it shares with strangers."
  (let ((cmd (sb-ext:octets-to-string (ws-frame-payload frame)
                                      :external-format :utf-8)))
    (build-ws-text
     (if (string= cmd "worker")
         (format nil "worker ~a ~a" (or *worker-id* "none")
                 (%peer-handle conn))
         (format nil "unknown command ~a" (%sanitize-line cmd))))))

(defun %sanitize-line (text)
  "Cap the length and strip control characters.

   The cap is because this is a public box and the text is strangers'. The
   control strip is not about rendering — the client uses textContent, so
   nothing here can become markup — it is about the wire format: a TAB
   separates the sequence number from the line, and a posted TAB would split
   a line into a sequence number the client would then believe.

   Space is 32, so the test below never catches it and needs no exception."
  (let ((clean (remove-if (lambda (c) (< (char-code c) 32)) text)))
    (if (> (length clean) *bulletin-line-max*)
        (subseq clean 0 *bulletin-line-max*)
        clean)))

;;; ---------------------------------------------------------------------------
;;; Entry points
;;; ---------------------------------------------------------------------------

(defun start-demo (&key (host #(127 0 0 1)) (port 8081) (workers 4)
                        (instance (bytes-to-hex (random-bytes 2))))
  "Start the demo server.

   HOST defaults to loopback, which is what you want on a laptop and wrong
   inside a container: nothing outside the container can reach a socket bound
   to its own loopback, so demo/deploy passes #(0 0 0 0) and lets the
   published port and the reverse proxy decide who may actually arrive.

   WORKERS is pinned rather than left to CPU-COUNT. The page's subject is
   fan-out across workers, and a small box reporting two of them is a dull
   exhibit; a fixed number also makes what the census shows reproducible.

   INSTANCE names this process in the handles the bulletin shows. Random by
   default so two runs are two names; pass the one your deployment already
   uses if it has one."
  (when (%port-already-served-p port)
    (error "start-demo: something is already answering on port ~d.~%~
            SO_REUSEPORT means a second server binds it rather than failing, ~
            and both would serve — the kernel splitting connections between ~
            them, each with its own bulletin, so the room divides in two with ~
            no error anywhere. Stop the other one first." port))
  (setf *demo-port* port)
  ;; The fan-out can only be as prompt as the workers wake: a worker with no
  ;; traffic is asleep in epoll_wait, and :ON-TICK does not run until it
  ;; returns. The default second is fine for a server and far too slow for a
  ;; bulletin — at 0.05 a posted line lands in about 50ms, and the cost is
  ;; twenty syscall returns per worker per second doing a sequence compare.
  ;;
  ;; Set here rather than left alone because the page displays it: the cadence
  ;; is what sharing nothing costs, and the number belongs on screen rather
  ;; than in an apology.
  (setf *worker-wake-interval* 0.05)
  (setf *instance* instance)
  (setf *started-at* (get-universal-time)
        *bulletin* (make-store :test #'eql)
        *fanned* (make-array workers :initial-element 0))
  (load-static-files "demo/static/"
                     :substitutions
                     '(("robots.txt" ("are smart" . "robots are cool and smart"))))
  (start-server :host host
                :port port
                :workers workers
                :handler #'handle-request
                :ws-handler #'handle-ws-message
                :on-tick #'bulletin-tick))

(defun main ()
  (start-demo))
