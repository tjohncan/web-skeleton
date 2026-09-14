(in-package :web-skeleton-demo)

;;; ===========================================================================
;;; Demo application — a live bulletin, a census, a request lab, a bench
;;;
;;; Shows how to use the web-skeleton framework, on the exported surface only:
;;;   - An HTTP handler that routes requests and upgrades /ws
;;;   - A WebSocket handler that posts, answers control frames, and budgets
;;;   - :ON-TICK and MAP-WORKER-WEBSOCKETS to fan out across workers
;;;   - A DEFER-TO-FETCH continuation for an async outbound call
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; What this process is called, and what it hangs off
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

(defvar *backlink-url* nil
  "An absolute URL to whatever this demo hangs off, or NIL for nothing.

   NIL is the default and the honest one: this repository has no parent, and
   a framework that shipped a link to its author's site would be shipping an
   opinion about where it runs. A deployer who has a hub says so, and the
   page grows a way back to it; a deployer who does not gets a page with no
   dangling link on it.

   Rendered once into the static cache at startup rather than per request.
   It is fixed for the life of the process, and the page it lands on is a
   cached file.")

;;; ---------------------------------------------------------------------------
;;; Self-address for the /demo-fetch example
;;; ---------------------------------------------------------------------------

(defvar *demo-host* "127.0.0.1"
  "Host the /demo-fetch endpoint uses to self-fetch over HTTP.")

(defvar *demo-port* 8081
  "Port the demo server is listening on. START-DEMO sets this so the
   /demo-fetch endpoint can build a self-referential URL.")

;;; ---------------------------------------------------------------------------
;;; Same-origin check for the /ws upgrade
;;; ---------------------------------------------------------------------------

(defun %origin-host (origin)
  "The host of an Origin header value, or NIL if it carries none. An Origin is
   scheme://host[:port] with no path (RFC 6454); anything else — the literal
   \"null\" a sandboxed or file:// browser sends, or junk — has no host here
   and returns NIL, which the caller treats as not same-origin."
  (let ((sep (search "://" origin)))
    (when sep
      (let* ((rest (subseq origin (+ sep 3)))
             (end  (position-if (lambda (c) (or (char= c #\:) (char= c #\/)))
                                rest)))
        (if end (subseq rest 0 end) rest)))))

(defun %host-only (host-header)
  "HOST-HEADER without its :port, for comparison against an Origin's host."
  (let ((colon (position #\: host-header)))
    (if colon (subseq host-header 0 colon) host-header)))

(defun %ws-origin-allowed-p (request)
  "T if the /ws upgrade may proceed. A same-origin check against cross-site
   WebSocket hijacking (DEPLOYMENT.md, \"WebSocket origin validation\"): a
   browser sends an Origin its page's script cannot forge, so a socket opened
   from another site carries that site's Origin and is refused. A request with
   no Origin — curl, wscat, a probe — is not a browser and not the attack this
   stops; it is allowed, and the proxy's per-address limits are what bound it.
   A present-but-foreign or malformed Origin, the literal \"null\" included,
   is refused. Host is what the proxy forwards, so the comparison holds behind
   it and on a laptop alike."
  (let ((origin (get-header request "origin")))
    (or (null origin)
        (let ((oh (%origin-host origin))
              (hh (get-header request "host")))
          (and oh hh (string-equal oh (%host-only hh)))))))

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
       ;; Same-origin check before the upgrade. A socket opened from another
       ;; site carries that site's Origin and is refused, which is what stops
       ;; a page using its visitors' browsers to post into the room from
       ;; addresses the proxy's per-address limits count as many clients. A
       ;; request with no Origin is allowed and left to those limits. See
       ;; DEPLOYMENT.md, "WebSocket origin validation".
       (if (%ws-origin-allowed-p request)
           :upgrade
           (make-error-response 403 "cross-origin WebSocket upgrade refused")))
      ((and (eq method :GET) (string= path "/demo-fetch"))
       (handle-demo-fetch request))
      ((and (eq method :GET) (string= path "/healthz"))
       (handle-healthz request))
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
;;; The port guard
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
;;; Peer handles
;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------
;;; The backlink
;;; ---------------------------------------------------------------------------

(defun %escape-html-attr (s)
  "The five characters that can end an attribute or start a tag.

   The URL is the operator's, not a visitor's, so this is not defending
   against an attacker — it is defending against a URL with an ampersand in
   its query string, which is ordinary, and against the stray quote that
   would otherwise end the attribute and put the rest of the URL into the
   markup as attributes of its own."
  (with-output-to-string (out)
    (loop for c across s do
      (case c
        (#\& (write-string "&amp;"  out))
        (#\< (write-string "&lt;"   out))
        (#\> (write-string "&gt;"   out))
        (#\" (write-string "&quot;" out))
        (#\' (write-string "&#39;"  out))
        (t    (write-char c out))))))

(defun %backlink-display (url)
  "URL without its scheme or its trailing slash.

   What someone wants to read on a link is where it goes, and https:// is
   not where anything goes. Display only: the href keeps the whole URL."
  (let* ((s (cond ((and (> (length url) 8) (string= "https://" url :end2 8))
                   (subseq url 8))
                  ((and (> (length url) 7) (string= "http://" url :end2 7))
                   (subseq url 7))
                  (t url)))
         (n (length s)))
    (if (and (plusp n) (char= (char s (1- n)) #\/))
        (subseq s 0 (1- n))
        s)))

(defun %backlink-html ()
  "The anchor, or the empty string when there is nowhere to go.

   Empty rather than absent-by-template: the placeholder has to be replaced
   either way, or a page with no backlink configured would display the word
   __BACKLINK__ to everyone who visited it.

   Anything that is not an http or https URL is refused and said out loud.
   The value is the operator's own, so this is not an attack surface — but
   a javascript: URL in an href is a foot-gun regardless of who loaded it,
   and refusing the shape costs one comparison."
  (cond
    ((null *backlink-url*) "")
    ((not (or (and (> (length *backlink-url*) 8)
                   (string= "https://" *backlink-url* :end2 8))
              (and (> (length *backlink-url*) 7)
                   (string= "http://" *backlink-url* :end2 7))))
     (log-warn "backlink ignored: ~s is not an http or https URL"
               *backlink-url*)
     "")
    (t
     (format nil "<a class=\"backlink\" href=\"~a\">&#8627; ~a</a>"
             (%escape-html-attr *backlink-url*)
             (%escape-html-attr (%backlink-display *backlink-url*))))))

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

   Built once by START-DEMO, before START-SERVER, rather than per click, and
   that is load-bearing rather than thrift. FORMAT-RESPONSE counts what it
   serializes, and *COUNTERS* is NIL off a worker: START-DEMO runs before any
   worker exists, so a response nobody will ever receive stays out of the
   census. Per click it would add a 4xx to the numbers on the other tab for
   traffic that never happened, which is the page disagreeing with the server
   about what the server did.

   One consequence shows in the bytes: their Date header is this process's
   start, not the time of a click, and the panel's label says so. Building
   them at load instead would freeze that Date at a compiled image's build
   time, days stale by the time it runs; doing it in START-DEMO makes 'at
   startup' true. A live refusal carries the moment it was sent."
  (let ((status (handler-case (progn (parse-request (getf c :bytes)) nil)
                  (http-parse-error (e) (or (http-parse-error-status e) 400))
                  (error () 400))))
    (when status
      (let ((resp (make-error-response status)))
        (set-response-header resp "connection" "close")
        (sb-ext:octets-to-string (format-response resp)
                                 :external-format :latin-1)))))

(defvar *bench-responses* nil
  "Case id to the response bytes for it, as a string. NIL for a case the
   parser accepts, where what happens next is the application's business and
   not this panel's to invent. Populated by START-DEMO — see %BENCH-RESPONSE
   for why there and not at load.")

(defun %build-bench-responses ()
  "Serialize each refusal's response once, off any worker. START-DEMO calls
   this before START-SERVER, so *COUNTERS* is unbound and these stay out of
   the census, and the Date they carry is this process's start rather than a
   compiled image's build date."
  (mapcar (lambda (c) (cons (getf c :id) (%bench-response c))) *bench-cases*))

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

(defparameter *lab-redacted-headers*
  '("cookie" "authorization" "proxy-authorization")
  "Headers whose value the echo replaces. The ones that carry a credential.")

(defparameter *lab-input-max* 2048
  "Longest accepted lab input, in characters. The framework's own request-line
   cap is higher — *MAX-REQUEST-LINE-LENGTH*, 8 KB — and it is a connection-
   level 414 the page cannot explain to a visitor, so this lower cap catches an
   over-long value first with a sentence the page can show. It is a soft guard,
   not the boundary: a value whose percent-encoded or multi-byte form is long
   enough still meets the framework's 414 first, and that is fine — the 414 is
   correct, just wordless.")

(defparameter *unix-epoch* (encode-universal-time 0 0 0 1 1 1970 0)
  "2208988800. Unix seconds plus this is a Lisp universal time.")

(defun %now-us ()
  "Microseconds of wall time: for timing a handler, refilling a post budget,
   and stamping a bulletin line.

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
   measured across a clock step is not an interval.

   So every subtraction of two of these clamps at zero: the lab's handler
   timer, and the post budget's refill. A backward step then reads as no
   time having passed, never as negative time. Unclamped, the budget turned
   a one-hour backward step into minus three thousand six hundred posts,
   and a connection that had posted before the step stayed locked out for
   the hour it took to earn them back.

   The bulletin's stamps are not clamped, and say so where they are taken:
   a stamp is the time the server believed it was, and a clock that stepped
   back is something a stamp should show rather than paper over. The
   sequence number is what orders posts."
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ (* sec 1000000) usec)))

(defun %lab-request-echo (request)
  "The request as the parser holds it, written back in the form it arrived.

   Rebuilt from the struct rather than kept as raw bytes, deliberately: what
   this shows is the request that was actually dispatched. A header the
   parser dropped is absent here, which is the honest answer to the question
   the panel is asking.

   One exception, and it is stated rather than hidden: the value of a header
   that carries a credential is replaced, its name kept. The echo goes only to
   the caller who sent it, so this is not guarding one visitor from another —
   it is guarding a cookie scoped to a parent domain, which a browser sends
   here without being asked, from turning up in a screenshot, a copied bug
   report, or anything else on the page that can read the response."
  (with-output-to-string (s)
    (format s "~a ~a~@[?~a~] HTTP/~a~c~c"
            (symbol-name (http-request-method request))
            (http-request-path request)
            (http-request-query request)
            (http-request-version request)
            #\Return #\Newline)
    (loop for (name . value) in (http-request-headers request)
          do (format s "~a: ~a~c~c" name
                     (if (member name *lab-redacted-headers* :test #'string-equal)
                         "(redacted)"
                         value)
                     #\Return #\Newline))
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
              ;; The framework's splitter, not one of the demo's own. This
              ;; used to carry its own, which accepted a token with base64
              ;; padding that JWT-SPLIT refuses — two readers of one token
              ;; disagreeing about whether it was one, on a page whose
              ;; subject is that disagreement. RFC 7515 omits the padding, so
              ;; JWT-SPLIT is the one that is right.
              (let ((parts (jwt-split token)))
                (unless parts
                  (error "not a JWS compact serialization: it needs exactly ~
                          three base64url segments, with no = padding"))
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

(defparameter *healthz-stale-after* 5
  "Seconds a worker may go without a loop pass before /healthz calls it
   wedged. At a 0.05 wake interval a healthy worker passes a hundred times in
   that; one that has not passed at all is stuck in something.")

(defvar *last-tick* nil
  "Per-worker vector of the universal time each worker last passed its loop.
   Written only by its own worker, from BULLETIN-TICK; read by whichever
   worker answers /healthz. The same shape as *FANNED*, and for the same
   reason needs no lock: one writer per slot, and a read that is a tick
   stale costs nothing.")

(defun handle-healthz (request)
  "200 when every worker has passed its loop recently, 503 naming the ones
   that have not.

   This is what makes the container healthcheck mean something. The census
   used to be the healthcheck, and a probe lands on one of four workers: a
   wedged worker failed about one probe in four, and a check that wants three
   failures in a row almost never saw it. Here any live worker answers for
   all of them, because the ticks it reads are every worker's own, so one
   stuck loop fails every probe, whichever worker takes it."
  (declare (ignore request))
  (let* ((now (get-universal-time))
         (stale (loop for at across *last-tick*
                      for i from 0
                      when (> (- now at) *healthz-stale-after*)
                        collect i)))
    (if stale
        (make-text-response
         503 (format nil "wedged worker~p: ~{~d~^, ~}~%" (length stale) stale))
        (make-text-response
         200 (format nil "ok: ~d workers passing their loops~%"
                     (length *last-tick*))))))

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

(sb-ext:defglobal *bulletin-latest* 0
  "The newest sequence posted, written under the store's lock by BULLETIN-POST
   and read without it by every worker's tick.

   It is what lets an idle tick cost nothing. Every worker passes its loop
   twenty times a second, and reading the ring means taking the store's lock
   — eighty acquisitions a second on four workers, for a bulletin nobody has
   posted to. The tick compares this against its own watermark first and
   takes the lock only when there is something newer.

   Unlocked, and safe to be: a fixnum read is one word, so a reader sees the
   old value or the new one and never half of each. Old by one post means one
   tick late, which the next tick corrects.

   DEFGLOBAL rather than DEFVAR for the reason the framework's census is: one
   shared value cell, never a per-thread binding a worker could shadow.")

(defvar *fanned* nil
  "Per-worker vector of the last sequence that worker has fanned out. Each
   worker writes only its own slot, so no lock — the same share-nothing
   shape as the framework's connection census.")

(defun %bulletin-trim (ring now)
  "Drop what is older than the window or past the cap. RING is newest first,
   so both bounds are a prefix take.

   NOW and the entry times are microseconds; *BULLETIN-WINDOW* is seconds,
   because it is a knob somebody sets and nobody sets a hand-off window in
   microseconds. The conversion lives here, at the one comparison."
  (loop for entry in ring
        for kept from 0 below *bulletin-max*
        while (<= (- now (third entry)) (* *bulletin-window* 1000000))
        collect entry))

(defun bulletin-post (text handle)
  "Append TEXT from HANDLE under a fresh sequence number. Atomic: the read of
   the last sequence, the increment, the insert and the trim are one update.

   HANDLE is carried rather than looked up later because by the time a line
   is fanned out the connection that sent it may be gone, and the worker
   doing the fanning is usually not the worker that took it in.

   The time stored is microseconds, not universal time. Whole seconds were
   enough while the page showed only a clock, and are not enough for a stamp
   with a fraction in it — every line posted in one second would claim the
   same instant, which for a page about the order things happen in is the
   one thing the stamp must not do."
  ;; The stamp is taken inside the update, under the lock, and not before
  ;; it. Taken before, two posts racing on different workers could be stamped
  ;; in one order and sequenced in the other. Taken here, a race cannot do
  ;; that — but the stamp is wall time, so the clock stepping backward still
  ;; can, and a stamp is left to show it rather than clamped to the one
  ;; before. Clamping would print a time that never happened, and after a
  ;; bogus jump forward would pin every stamp there until real time caught up.
  ;; The sequence is what orders posts; the stamp says when.
  (store-update *bulletin* :ring
                (lambda (ring)
                  (let ((now (%now-us))
                        (next (1+ (if ring (first (first ring)) 0))))
                    (setf *bulletin-latest* next)
                    (cons (list next text now handle)
                          (%bulletin-trim ring now)))))
  nil)

(defun bulletin-since (seq)
  "Lines newer than SEQ, oldest first. The ring descends by sequence, so the
   walk stops at the first entry already seen rather than scanning it all."
  (let ((ring (store-get *bulletin* :ring)))
    (nreverse
     (loop for entry in ring
           while (> (first entry) seq)
           collect entry))))

(defun %utc-stamp (us)
  "US, a microsecond count since the Unix epoch, as
   yyyy-MM-dd HH:MM:SS.ffffff in UTC.

   Stamped by the server that took the line rather than by each browser when
   it arrives, so everyone reading the bulletin sees one time for a post
   instead of their own. UTC rather than anywhere's local time for the same
   reason: a shared broadcast wants a shared clock, and a page that guessed
   at a visitor's zone would print a different time to each of them for one
   event.

   No zone marker. Every stamp here is UTC and nothing else ever will be, so
   a Z on the end of every line is a column of Zs.

   The fraction is why the ring stores microseconds rather than universal
   time: GET-UNIVERSAL-TIME counts whole seconds, so a stamp built from one
   can carry six digits after the point and every one of them is a zero."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (+ (floor us 1000000) *unix-epoch*) 0)
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d.~6,'0d"
            year month date hour min sec (mod us 1000000))))

(defun %bulletin-payload (entry)
  "Wire form: sequence, TAB, UTC stamp, TAB, sender handle, TAB, the line.

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
                         (%utc-stamp (third entry)) #\Tab
                         (or (fourth entry) "?") #\Tab
                         (second entry))))

(defun bulletin-tick (worker-id)
  "Fan out to the connections THIS worker owns. Runs on that worker's event
   loop, which is the only place WS-SEND to them is legal.

   Also records that this worker passed its loop, for /healthz, and does it
   first so a tick with nothing to send still counts as a pass.

   Takes the store's lock only when *BULLETIN-LATEST* says there is something
   newer than this worker has sent — the :ON-TICK docstring asks a hook not to
   block the loop it runs on, and a lock taken twenty times a second for
   nothing is the slow version of doing exactly that."
  (setf (aref *last-tick* worker-id) (get-universal-time))
  (let ((latest *bulletin-latest*))
    (when (> latest (aref *fanned* worker-id))
      (let ((new (bulletin-since (aref *fanned* worker-id))))
        (cond
          (new
           (let ((payloads (mapcar #'%bulletin-payload new)))
             (map-worker-websockets
              (lambda (conn)
                ;; One peer at *MAX-WRITE-BACKLOG* must not cost everyone
                ;; behind it the batch. MAP-WORKER-WEBSOCKETS deliberately
                ;; does not decide this; an application that broadcasts does.
                (handler-case
                    (dolist (p payloads) (ws-send conn p))
                  (error () nil)))))
           (setf (aref *fanned* worker-id)
                 (reduce #'max new :key #'first)))
          ;; Newer lines existed and are already gone: this worker went
          ;; longer than the window without a pass, and the trim took what
          ;; it had not yet sent. Nothing to send — but left behind, the
          ;; watermark stays under LATEST, and every tick after this takes the
          ;; lock to find the ring empty, until somebody posts again. Catching
          ;; up to what was read is safe: a post landing after that read has a
          ;; higher sequence and is caught on the next pass.
          (t
           (setf (aref *fanned* worker-id) latest)))))))

;;; ---------------------------------------------------------------------------
;;; WebSocket handler
;;; ---------------------------------------------------------------------------

(defparameter *post-burst* 5
  "Posts one connection may make back to back before the budget applies.")

(defparameter *post-refill-per-second* 1
  "Posts a connection earns back per second, up to *POST-BURST*.")

(defstruct (post-budget (:conc-name budget-))
  (tokens 0)
  (at 0)
  (warned nil))

(defvar *post-budgets* nil
  "Per-worker vector of weak EQ hash tables, connection to POST-BUDGET.

   Per worker because a connection is only ever handled by the worker that
   owns it, so each table has one reader and one writer, and the lock SBCL
   puts on a weak table — it synchronizes them for the collector's sake — is
   never contended. Weak on the key because nothing tells an application a
   WebSocket closed: an entry
   for a connection that has gone is dropped when the connection is
   collected, so the table needs neither a registry nor a close notification
   to stay the size of the live population.")

(defun %post-allowed-p (conn)
  "Spend one post from CONN's budget, or refuse. A token bucket: *POST-BURST*
   posts at once, then *POST-REFILL-PER-SECOND*.

   Second value is T the first time a refusal lands after an allowed post, so
   the sender is told once per run of refusals rather than once per frame —
   an answer to every dropped frame would be a write for every write, handed
   to the one client already sending faster than it should."
  (let ((table (and *worker-id* (aref *post-budgets* *worker-id*))))
    (if (null table)
        (values t nil)
        (let* ((now (%now-us))
               (b (or (gethash conn table)
                      (setf (gethash conn table)
                            (make-post-budget :tokens *post-burst* :at now))))
               ;; Clamped: see %NOW-US. A clock stepped backward makes this
               ;; negative, and a negative refill is a lockout.
               (elapsed (max 0 (- now (budget-at b))))
               (tokens (min *post-burst*
                            (+ (budget-tokens b)
                               (* (/ elapsed 1000000)
                                  *post-refill-per-second*)))))
          (setf (budget-at b) now)
          (if (>= tokens 1)
              (progn (setf (budget-tokens b) (- tokens 1)
                           (budget-warned b) nil)
                     (values t nil))
              (let ((first-refusal (not (budget-warned b))))
                (setf (budget-tokens b) tokens
                      (budget-warned b) t)
                (values nil first-refusal)))))))

(defun handle-ws-message (conn frame)
  "Post to the bulletin. Returns NIL: the sender sees their own line through
   the same fan-out as everyone else, one tick later.

   Echoing it back immediately would feel faster and would be a lie — the
   sender would see an ordering nobody else sees. The demo is here to show
   the mechanism, latency included.

   Posts are budgeted per connection. A proxy's rate limit counts HTTP
   requests, and a WebSocket is one request: every frame after the upgrade
   goes past it uncounted. Unbudgeted, one socket could post as fast as it
   could write, and each post takes the store's lock and goes out to every
   socket on every worker — one sender multiplied by the whole room.

   The budget limits a socket, and a new socket gets a new one — right for a
   real client whose connection dropped and came back, and also a way past the
   budget for one that closes and reopens on purpose. Either way the
   application cannot tell them apart: behind a proxy every peer arrives from
   the proxy's address, so \"the same client\" is not a question it can answer.

   Bounding a client is the proxy's, and takes both its limits, not one.
   limit_conn caps the sockets one address holds at once. limit_req caps how
   fast it opens new ones — which is what bounds the reopen-for-a-fresh-budget
   path, since that path never holds enough at once for limit_conn to see. The
   ceiling is then limit_req's rate times a socket's five posts — about fifty
   a second, after a one-time burst — not unbounded, and each post only makes
   the bulletin scroll. demo/deploy's nginx sample sets both."
  (cond
    ((= (ws-frame-opcode frame) +ws-op-binary+)
     (%ws-command conn frame))
    ((= (ws-frame-opcode frame) +ws-op-text+)
     (multiple-value-bind (allowed tell) (%post-allowed-p conn)
       (cond (allowed
              (let ((text (sb-ext:octets-to-string (ws-frame-payload frame)
                                                   :external-format :utf-8)))
                (bulletin-post (%sanitize-line text) (%peer-handle conn)))
              nil)
             ;; No TAB, so the page reads this as the server talking rather
             ;; than as a posted line, same as a control answer.
             (tell (build-ws-text "posting too fast; lines are being dropped"))
             (t nil))))
    (t nil)))

(defun %ws-command (conn frame)
  "Answer a control frame, on the asking connection only.

   Binary rather than text, and that is what keeps the two channels apart.
   The bulletin box sends text and only text, so nothing a visitor can type
   reaches this, and nothing answered here can be mistaken for a posted line.

   The answer is text with no TAB in it. A bulletin line always has three,
   put there by %BULLETIN-PAYLOAD and impossible to post because
   %SANITIZE-LINE strips them — so the client tells the two apart by
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
   nothing here can become markup — it is about the wire format: TABs separate
   the sequence, the stamp, the handle and the line, and a posted TAB would add
   a field boundary the client parses as one of those, shifting the handle or
   the stamp onto whatever the sender typed.

   Space is 32, so the test below never catches it and needs no exception."
  (let ((clean (remove-if (lambda (c) (< (char-code c) 32)) text)))
    (if (> (length clean) *bulletin-line-max*)
        (subseq clean 0 *bulletin-line-max*)
        clean)))

;;; ---------------------------------------------------------------------------
;;; Entry points
;;; ---------------------------------------------------------------------------

(defun start-demo (&key (host #(127 0 0 1)) (port 8081) (workers 4)
                        (instance (bytes-to-hex (random-bytes 2)))
                        backlink)
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
   uses if it has one.

   BACKLINK is an absolute URL to whatever this demo hangs off, or NIL. A
   page served from somebody's collection of things should offer a way back
   to the collection; a page served from nowhere in particular should not
   invent one."
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
  (setf *instance* instance
        *backlink-url* backlink)
  (setf *started-at* (get-universal-time)
        *bulletin* (make-store :test #'eql)
        *fanned* (make-array workers :initial-element 0)
        ;; Zero, not now: a worker that never starts should read as wedged
        ;; from the first probe, and the image's start period covers the
        ;; milliseconds before a healthy one passes its loop.
        *last-tick* (make-array workers :initial-element 0)
        *post-budgets* (let ((v (make-array workers)))
                         (dotimes (i workers v)
                           (setf (aref v i)
                                 (make-hash-table :test 'eq :weakness :key)))))
  (setf *bulletin-latest* 0)
  ;; The appendix's refusal responses, serialized here rather than at load so
  ;; their Date is this process's start and not a compiled image's build date,
  ;; and still off any worker so they stay out of the census. See
  ;; %BENCH-RESPONSE.
  (setf *bench-responses* (%build-bench-responses))
  ;; Backquoted rather than quoted now: the backlink is not known until
  ;; this call, and the cache is built once from what it says here.
  (load-static-files "demo/static/"
                     :substitutions
                     `(("robots.txt" ("are smart" . "robots are cool and smart"))
                       ("index.html" ("__BACKLINK__" . ,(%backlink-html)))))
  (start-server :host host
                :port port
                :workers workers
                :handler #'handle-request
                :ws-handler #'handle-ws-message
                :on-tick #'bulletin-tick))

(defun main ()
  (start-demo))
