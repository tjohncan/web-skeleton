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
      ((and (eq method :GET) (string= path "/census"))
       (handle-census request))
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

(defun %worker-json (slot)
  "One census slot, or an empty one for a worker that has not published yet."
  (make-json-object
   (list (cons "total"  (if slot (getf slot :total 0) 0))
         (cons "states" (%states-json (and slot (getf slot :states)))))))

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

(defun bulletin-post (text)
  "Append TEXT under a fresh sequence number. Atomic: the read of the last
   sequence, the increment, the insert and the trim are one update."
  (let ((now (get-universal-time)))
    (store-update *bulletin* :ring
                  (lambda (ring)
                    (let ((next (1+ (if ring (first (first ring)) 0))))
                      (cons (list next text now)
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

(defun %bulletin-payload (entry)
  (build-ws-text (format nil "~d~a~a" (first entry) #\Tab (second entry))))

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
  (declare (ignore conn))
  (when (= (ws-frame-opcode frame) +ws-op-text+)
    (let ((text (sb-ext:octets-to-string (ws-frame-payload frame)
                                         :external-format :utf-8)))
      (bulletin-post (%sanitize-line text))))
  nil)

(defun %sanitize-line (text)
  "Cap the length and strip control characters. The client renders with
   textContent, so this is belt-and-braces rather than the only guard."
  (let ((clean (remove-if (lambda (c)
                            (and (< (char-code c) 32)
                                 (not (char= c #\Space))))
                          text)))
    (if (> (length clean) *bulletin-line-max*)
        (subseq clean 0 *bulletin-line-max*)
        clean)))

;;; ---------------------------------------------------------------------------
;;; Entry points
;;; ---------------------------------------------------------------------------

(defun start-demo (&key (port 8081) (workers 4))
  "Start the demo server.

   WORKERS is pinned rather than left to CPU-COUNT. The page's subject is
   fan-out across workers, and a small box reporting two of them is a dull
   exhibit; a fixed number also makes what the census shows reproducible."
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
  (setf *started-at* (get-universal-time)
        *bulletin* (make-store :test #'eql)
        *fanned* (make-array workers :initial-element 0))
  (load-static-files "demo/static/"
                     :substitutions
                     '(("robots.txt" ("are smart" . "robots are cool and smart"))))
  (start-server :port port
                :workers workers
                :handler #'handle-request
                :ws-handler #'handle-ws-message
                :on-tick #'bulletin-tick))

(defun main ()
  (start-demo))
