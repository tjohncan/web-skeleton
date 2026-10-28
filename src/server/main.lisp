(in-package :web-skeleton)

;;; ===========================================================================
;;; Worker pool and server entry point
;;;
;;; N worker threads, each with its own listener socket (SO_REUSEPORT),
;;; epoll fd, and connection table.  The kernel distributes incoming
;;; connections across workers.  Zero shared state in the hot path.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; TCP listener
;;; ---------------------------------------------------------------------------

(defparameter *listen-backlog* 128
  "Each worker's accept queue: the connections the kernel holds, completed
   and not yet accepted, before it starts dropping SYNs. Per worker, since
   each binds its own SO_REUSEPORT listener, so the server's is this times
   the worker count. Linux clamps it to net.core.somaxconn, so raising it
   past that needs the sysctl raised too.")

(defun make-tcp-listener (host port)
  "Create a TCP socket, bind to HOST:PORT, listen with a backlog of
   *LISTEN-BACKLOG*. HOST is a 4-byte IPv4 vector or a 16-byte IPv6
   vector; the family is dispatched accordingly. Sets SO_REUSEADDR,
   SO_REUSEPORT, and non-blocking. Returns the socket.

   IPV6_V6ONLY is left as the host sets it. With Linux's default,
   net.ipv6.bindv6only = 0, a listener on :: takes IPv4 peers too, as
   v4-mapped addresses (::ffff:a.b.c.d), so one listener serves both
   families; a host with bindv6only = 1 serves only IPv6 on ::."
  (let* ((family (case (length host)
                   (4  :inet)
                   (16 :inet6)
                   (t (error "make-tcp-listener: :host must be a 4-byte v4 ~
                              or 16-byte v6 vector, got ~a" host))))
         (socket (make-instance (if (eq family :inet)
                                    'sb-bsd-sockets:inet-socket
                                    'sb-bsd-sockets:inet6-socket)
                                :type :stream
                                :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (set-socket-option-int (socket-fd socket)
                           +sol-socket+ +so-reuseport+ 1)
    (sb-bsd-sockets:socket-bind socket host port)
    (sb-bsd-sockets:socket-listen socket *listen-backlog*)
    (set-nonblocking (socket-fd socket))
    socket))

;;; ---------------------------------------------------------------------------
;;; Per-worker connection table
;;;
;;; Each worker thread binds *connections* in its own dynamic scope.
;;; All connection functions use whatever binding is current —
;;; no locks, no shared state.
;;; ---------------------------------------------------------------------------

(defvar *connections* nil
  "Maps file descriptor → connection object. Bound per-worker.")

(defun register-connection (conn)
  (setf (gethash (connection-fd conn) *connections*) conn))

(defun unregister-connection (conn)
  (remhash (connection-fd conn) *connections*))

(defun lookup-connection (fd)
  (gethash fd *connections*))

(defun map-worker-websockets (function)
  "Call FUNCTION on each open WebSocket this worker owns. Returns the count.

   The companion to :ON-TICK, and the supported way to reach a set of
   connections at once. Fan-out is per worker because the write queue has no
   lock: a connection belongs to exactly one event loop and WS-SEND refuses
   one that belongs to another. An application broadcasting to everyone
   broadcasts once per worker, to that worker's share, from inside that
   worker — which is what this walks.

   Named for the worker and not for the set, because the set is the point.
   This is never every connection on the server, only the ones on this loop.
   Whether that is all of them or a sixteenth is a fact about the deployment.

   Only the :WEBSOCKET state is visited. A connection still upgrading,
   already closing, or driving an outbound fetch is not something an
   application can hand a frame to.

   Two properties make it safe to call without ceremony. The table is
   collected before anything runs, so FUNCTION may close what it was handed —
   or send on it and have the send close it — without the walk stepping on a
   table that moved underneath it; this is the discipline the idle sweep and
   worker teardown already use for the same reason. And liveness is
   re-checked immediately before each call, so a connection an earlier call
   closed is skipped rather than handed over dead.

   It does not catch. If FUNCTION raises, the walk stops and the condition
   propagates — into :ON-TICK's handler, when called from there. The
   framework has no basis for ruling that a partial fan-out is fine, and
   WS-SEND raising at *MAX-WRITE-BACKLOG* is the ordinary fate of one slow
   peer in a broadcast, so it is worth an application deciding rather than
   hoping about. Continuing past a refusing connection is three lines in the
   callback.

   Signals off a worker rather than returning zero, which would be
   indistinguishable from a server with no clients."
  (unless *connections*
    (error "map-worker-websockets: no connection table on this thread. It ~
            walks the connections one worker owns, so it has to run on that ~
            worker — from :on-tick, from a handler, or from a fetch ~
            callback. A background thread owns none."))
  (let ((live nil))
    (maphash (lambda (fd conn)
               (declare (ignore fd))
               (when (eq (connection-state conn) :websocket)
                 (push conn live)))
             *connections*)
    (let ((visited 0))
      (dolist (conn live visited)
        (when (eq (lookup-connection (connection-fd conn)) conn)
          (incf visited)
          (funcall function conn))))))

;;; ---------------------------------------------------------------------------
;;; Connection census
;;;
;;; *CONNECTIONS* is bound inside RUN-WORKER's LET, so it exists only on the
;;; thread that owns it and no other thread can read it — which is the whole
;;; point of the share-nothing design and also why a test can learn nothing
;;; about connection lifetime without help.
;;;
;;; Each worker publishes a counts plist into its own slot of a global vector
;;; on the maintenance tick. A worker owns its slot outright, so there is no
;;; lock, and the reader below sums the slots.
;;;
;;; What it answers that a request log cannot: every lifecycle defect on this
;;; branch — an outbound never swept, a teardown that misses its pair, a paused
;;; connection nothing will wake — leaks a connection with no request attached
;;; to it. A log records requests. This counts what is still here.
;;; ---------------------------------------------------------------------------

(sb-ext:defglobal *connection-census* nil
  "Vector of per-worker counts plists, indexed by worker id, or NIL before
   START-SERVER has sized it. DEFGLOBAL rather than DEFVAR for the reason
   *SHUTDOWN* is: one shared value cell, never a per-thread binding.")

(defvar *worker-census* nil
  "The census vector of this worker's run, bound by the thread START-SERVER
   spawns, beside *WORKER-GENERATION*. A worker publishes into its own run's
   vector rather than whatever *CONNECTION-CENSUS* holds now, so a worker an
   earlier run left behind cannot overwrite — or on its way out clear — the
   slot a later run's worker of the same id owns. NIL off a worker.")

(defvar *worker-id* nil
  "This worker's index into *CONNECTION-CENSUS*. Bound per-worker by
   RUN-WORKER beside the other share-nothing slots; NIL off a worker.

   Exported to be read, never set. An application that wants to attribute
   work to the worker that did it — a log line, a per-worker accumulator,
   telling a socket which of the four it is talking to — has no other way to
   ask, and the census already publishes per-worker data without offering a
   way to know which entry is yours. NIL off a worker is the answer to that
   question rather than an absence: a REPL or a test thread is on no worker
   and a caller that prints it should say so.

   Constant for the life of a thread. It is not a handle to anything: it
   indexes the census and it names a thread, and nothing keyed on it may be
   touched by another worker.")

(defun census-counts ()
  "Count the current worker's connection table: total, the inbound/outbound
   split, and a plist of state → count.

   Built fresh on every call and never patched in place — see
   PUBLISH-CONNECTION-CENSUS."
  (let ((total 0) (outbound 0) (states nil))
    (maphash (lambda (fd conn)
               (declare (ignore fd))
               (incf total)
               (when (connection-outbound-p conn) (incf outbound))
               (incf (getf states (connection-state conn) 0)))
             *connections*)
    (list :total total
          :outbound outbound
          :inbound (- total outbound)
          :states states
          :counters (counters-snapshot))))

(defun publish-connection-census ()
  "Store this worker's counts into its own census slot. No-op off a worker.

   One SETF of a freshly built plist, never a mutation of the plist already
   there. A reader on another thread sees either the previous plist or this
   one, never a half-updated one — the same discipline *HTTP-DATE-LINE-CACHE*
   documents for the same reason: patching in place would be cheaper and
   wrong."
  (let ((census *worker-census*))
    (when (and census *worker-id* (< *worker-id* (length census)))
      (setf (aref census *worker-id*) (census-counts)))))

(defun clear-connection-census ()
  "Drop this worker's published counts. Called as a worker exits, so a torn
   down server does not leave counts a later reader would sum."
  (let ((census *worker-census*))
    (when (and census *worker-id* (< *worker-id* (length census)))
      (setf (aref census *worker-id*) nil))))

(defun connection-census ()
  "A snapshot of every worker's connections. NIL before any has published.

   Readable from any thread, including one that is no worker — it reads the
   published slots, never a connection table. What it reports is up to one
   maintenance tick old, a second by default, so a caller asserting that
   something has gone away polls until it does rather than reading once.

   The contract is split, and the split is the part to read.

   Stable — :WORKERS, :TOTAL, :INBOUND, :OUTBOUND, and :COUNTERS. Counts of
   things an application already has names for. Safe to render, alert on, and
   compare across versions.

   :COUNTERS is cumulative for the life of each worker, never windowed: a
   caller wanting a rate samples twice and subtracts, and a negative
   difference means a worker restarted in between. Exactly where each count is
   taken — and the two responses no count sees — is NOTE-RESPONSE's docstring,
   and it is worth reading before alerting on these. It counts what only the
   framework sees — responses by class including every refusal no handler ever
   ran for, accepts taken and refused, and the WebSocket frames an application
   handed to a connection, by WS-SEND or as a ws-handler's return value. Not
   the pings, pongs and closes the framework sends on its own behalf. A worker
   that crashes and restarts begins a fresh set, so these can go down.

   Diagnostic — :STATES and :PER-WORKER. :STATES is keyed by the connection
   state machine's own keywords, which are internals and will change when it
   does. :PER-WORKER is one entry per worker, indexed by worker id, each the
   same shape as the sums above or NIL for a worker that has not published
   yet. Both are for looking at. Neither is worth depending on.

   A consumer must render unknown keys generically rather than matching an
   exhaustive set. Keys will be added — :COUNTERS was — and a panel that
   switches on a closed list silently stops showing whatever arrives next.

   The sums are consistent with :PER-WORKER because they are computed from
   it. They are not simultaneous: worker 3 may have published a tick after
   worker 0, so this is a picture of a server rather than an instant. The
   distinction matters exactly once — when the totals have to add up against
   something counted elsewhere, and do not."
  (when *connection-census*
    (let ((total 0) (outbound 0) (inbound 0) (states nil) (seen nil)
          (counters nil) (per-worker nil))
      (loop for slot across *connection-census*
            do (push slot per-worker)
               (when slot
                 (setf seen t)
                 (incf total    (getf slot :total 0))
                 (incf outbound (getf slot :outbound 0))
                 (incf inbound  (getf slot :inbound 0))
                 (loop for (state n) on (getf slot :states) by #'cddr
                       do (incf (getf states state 0) n))
                 (loop for (name n) on (getf slot :counters) by #'cddr
                       do (incf (getf counters name 0) n))))
      (when seen
        (list :workers (length *connection-census*)
              :total total :outbound outbound :inbound inbound
              :states states
              :counters counters
              :per-worker (nreverse per-worker))))))

;;; ---------------------------------------------------------------------------
;;; Connection lifecycle — idle timeout and WebSocket ping/pong
;;;
;;; Three mechanisms:
;;;   1. HTTP idle timeout — close connections that never finish a request
;;;   2. WebSocket ping/pong — detect dead connections (client vanished)
;;;   3. WebSocket idle timeout — close inactive but alive connections
;;; ---------------------------------------------------------------------------

(defparameter *max-connections* 10000
  "Maximum connections per worker. New accepts are refused when full, and
   when the process is close to its descriptor limit: see
   DESCRIPTORS-NEARLY-OUT-P.")

(defvar *descriptor-limit* nil
  "The process's soft descriptor limit, as START-SERVER left it after
   raising it to the hard limit. NIL before a server has started, when
   ACCEPT-CONNECTION refuses on *MAX-CONNECTIONS* alone. Process-wide,
   like the limit: every worker's descriptors count against it.")

(defun descriptors-nearly-out-p (fd)
  "Whether FD, just accepted, is in the last eighth of *DESCRIPTOR-LIMIT*.

   The kernel hands out the lowest free descriptor, so a new one's number
   is a count of the process's open descriptors, every worker's sockets,
   pipes and files alike, and the check costs nothing to keep. An eighth
   is kept back for the connections already held, whose fetches, DNS
   lookups and files need descriptors of their own. Without it a server
   under a soft limit of 1024 runs out at about a thousand connections:
   accept(2) fails with EMFILE, and clients the kernel has already
   connected wait unanswered. With it they are answered 503, as at
   *MAX-CONNECTIONS*."
  (let ((limit *descriptor-limit*))
    (and limit (>= fd (- limit (floor limit 8))))))

(defparameter *connection-limit-response*
  (let* ((body (sb-ext:string-to-octets
                (format nil "Service Unavailable: connection limit reached~%")
                :external-format :ascii))
         (crlf (coerce '(#\Return #\Linefeed) 'string)))
    (concatenate
     '(simple-array (unsigned-byte 8) (*))
     (sb-ext:string-to-octets
      (concatenate 'string
                   "HTTP/1.1 503 Service Unavailable" crlf
                   ;; Two seconds: long enough that the retry is not part
                   ;; of the same burst that caused the refusal, short
                   ;; enough that a momentary spike does not turn into a
                   ;; visible stall for a client that did nothing wrong.
                   "Retry-After: 2" crlf
                   "Content-Type: text/plain; charset=utf-8" crlf
                   (format nil "Content-Length: ~d" (length body)) crlf
                   "Connection: close" crlf
                   crlf)
      :external-format :ascii)
     body))
  "The refusal sent when a worker is at *MAX-CONNECTIONS*, built once.

   No Date header, and not for the reason a static response omits one
   from its frozen bytes — that one is served as pieces with a date line
   put between them per request. This is sent whole, and building it per
   refusal would put header construction on the one path that exists
   because the worker is already out of room. A stale Date would be worse
   than none, so it carries none.")

(defconstant +refusal-drain-size+ 2048
  "Bytes REFUSE-CONNECTION clears per read while draining a refused peer.
   Sized to hold an ordinary request in one or two passes; the drain is
   capped at four of them regardless.")

(defvar *stream-drain-buf* nil
  "Scratch buffer a :STREAMING connection's reads are discarded into,
   bound per worker by RUN-WORKER.

   A stream watches EPOLLIN only to notice the peer leaving; whatever
   the peer sends while a response streams is not a request this
   connection can answer. Kept out of the connection's own read buffer
   so a long stream cannot grow it, and so the offsets the keep-alive
   reset works from are the ones the original request left.")

(defvar *refusal-drain-buf* nil
  "Scratch buffer REFUSE-CONNECTION drains into, bound per worker by
   RUN-WORKER alongside *EPOLL-CTL-BUF* and *POLL-BUF*.

   The bytes read into it are discarded — the drain exists so that
   close(2) sends FIN rather than RST, not to look at what the peer
   said — so one buffer per worker is enough and its contents never
   need clearing between refusals.

   Per-worker rather than global for the reason every other scratch
   buffer here is: workers share nothing in the hot path, and a shared
   sink would be two threads writing one array. Nothing reads it, so
   that would in fact be harmless, which is exactly the kind of thing
   that stops being true after an edit nobody connected to it.

   NIL outside a worker — the REPL, the test suite driving
   REFUSE-CONNECTION directly — where the per-call allocation this
   replaces is still what happens.")

(defparameter *idle-timeout* 10
  "Seconds before an idle HTTP connection is closed. 0 to disable.

   Idle means no request completed and no response bytes written, so this
   covers the gap between requests on a keep-alive connection and also
   the whole arrival of one request, head and body together. A partial
   read does not refresh it, deliberately: a client that drips a request
   a byte at a time would otherwise hold its slot for as long as it kept
   dripping. The same rule cuts off an honest upload that is merely
   slow — at 20 KiB/s, ten seconds is about 200 KiB — with a close and no
   response. Behind a proxy that buffers request bodies the body arrives
   at once and this never matters. Behind one that streams them, accept
   the bodies you expect from a body handler (BUFFER-BODY, RECEIVE-BODY):
   an accepted body is timed on its progress, by *BODY-STALL-TIMEOUT*, as
   DEPLOYMENT.md's reverse-proxy section says. Raising this instead would
   lengthen every request's budget, a client dripping one included. A
   slow download is not cut off: the write path counts a writable socket
   as activity.")

(defparameter *linger-timeout* 2
  "Seconds a connection closed after an early error response may go on
   reading — and discarding — what its client is still sending, before it
   is closed. 0 closes at once.

   An answer given before the request has been read in full — a 413 for a
   body over the cap, a 431, any refusal of the request line or head —
   leaves the client mid-send. Closing then, with its bytes unread, makes
   Linux answer RST rather than FIN, which discards response bytes not yet
   sent and tells the peer the connection failed. See
   BEGIN-LINGERING-CLOSE. Bounded by time
   alone: the bytes are the client's to send, so draining them costs what
   receiving them would have.")

(defparameter *ws-idle-timeout* 86400
  "Seconds before an inactive WebSocket connection is closed. 0 to disable.
   Inactivity = no text or binary frames from the client (pongs don't count).")

(defparameter *ws-message-timeout* 30
  "Seconds a WebSocket message may take to arrive, from its first byte to
   its last. A message still unfinished then — a fragmented message short of
   its final fragment, or a frame not all here — closes the connection with
   1008. 0 to disable.

   *WS-IDLE-TIMEOUT* cannot bound this. Each message that completes resets
   its clock, and a peer that answers pings keeps the connection alive, so
   a message begun and never finished would hold its reassembly buffer, up
   to *MAX-WS-MESSAGE-SIZE*, for as long as that allows: a day by default.
   Measured from the message's start and not moved by its fragments, so a
   peer adding a byte at a time is held to it too. The WebSocket
   counterpart of *IDLE-TIMEOUT*, which bounds a request's arrival the
   same way.")

(defparameter *ws-ping-interval* 30
  "Seconds between server-initiated WebSocket pings.")

(defparameter *ws-max-missed-pongs* 3
  "Close a WebSocket connection after this many consecutive unanswered pings.")

(defun deliver-awaiting-timeout (conn epoll-fd)
  "Answer 504 on an inbound parked in :AWAITING past *FETCH-TIMEOUT*, then
   hand it to the write path to close.

   504 rather than 502 because the condition is specifically that the
   upstream did not answer in time. DELIVER-FETCH-ERROR's 502s are right
   for their cases — connect refused, short body, unparseable status, all
   genuinely a bad response from the gateway — and this is the other one.
   Answering everything with a single code is what *STATUS-REASONS*
   already declines to do.

   The paired outbound goes first, through CLOSE-OUTBOUND, because that
   is what fires the fetch callback's (NIL NIL NIL) sentinel. Skipping it
   would leak the app's cleanup on exactly the path where the fetch never
   returned.

   The write buffer is drained by construction: a connection reaches
   :AWAITING only from dispatch, after its request was fully read and
   before any response was queued, and both the keep-alive reset and the
   100-continue flush zero the buffer on the way. CONNECTION-QUEUE-WRITE
   enforces that rather than trusting it, which is why the caller catches
   — a signal here would take down a worker mid-sweep."
  (let ((out-fd (connection-awaiting-fd conn)))
    (when (>= out-fd 0)
      (let ((out-conn (lookup-connection out-fd)))
        (when out-conn
          (close-outbound out-conn epoll-fd)))))
  (setf (connection-close-after-p conn) t
        (connection-awaiting-fd conn) -1)
  (let ((bytes (strip-body-for-head
                (format-response (make-error-response 504)
                                 :connection-hint (connection-hint-for conn))
                conn)))
    (connection-queue-write conn bytes)
    (setf (connection-state conn) :write-response
          (connection-last-active conn) (get-universal-time))
    (epoll-modify epoll-fd (connection-fd conn)
                  (logior +epollout+ +epollet+))))

(defun connect-late-p (conn now)
  "True when CONN, still on the table, is an outbound connect given its
   *CONNECT-FALLBACK-DELAY* by NOW and not answered, with the name's other
   addresses still to try."
  (and (connection-outbound-p conn)
       (eq (connection-state conn) :out-connecting)
       (connection-fallback-addresses conn)
       (plusp (connection-connect-deadline conn))
       (> now (connection-connect-deadline conn))
       (eq (lookup-connection (connection-fd conn)) conn)))

(defun fetch-expired-p (conn now)
  "True when CONN, still on the table, is an outbound past its fetch
   deadline at NOW that nothing but the sweep will end.

   A detached outbound has no parked inbound, so the :AWAITING reap that
   bounds every other fetch cannot see it. Nothing else would: it is not
   idle — it is waiting on an upstream — and a paused one is subscribed to
   no epoll events at all, so no wake-up is coming either. Left alone, a
   hung upstream strands the outbound socket and the app's callback never
   fires. A pause whose target is still draining is the target's stall
   timeout's to bound, not this one's — see PAUSED-BEHIND-A-BACKLOG-P.

   An outbound still sending a streamed request body is bound the same way
   whoever it answers to: its deadline moves out whenever the body does
   (NOTE-REQUEST-PROGRESS), so what it bounds is the body standing still."
  (and (connection-outbound-p conn)
       (or (eq (connection-fetch-sink conn) :detached)
           (eq (connection-state conn) :out-send))
       (plusp (connection-fetch-deadline conn))
       (> now (connection-fetch-deadline conn))
       (not (paused-behind-a-backlog-p conn))
       (eq (lookup-connection (connection-fd conn)) conn)))

(defun inbound-overdue (conn now)
  "What CONN, an inbound still on the table, is due at NOW, as the list
   SWEEP-IDLE-CONNECTIONS ends it from: :STALLED, :WS-LATE, :BODY-STALLED
   or :IDLE, or NIL when nothing is."
  (let* ((state (connection-state conn))
         ;; A body a body handler accepted, still arriving: timed on its
         ;; progress rather than from the start of the request. See
         ;; *BODY-STALL-TIMEOUT*.
         (body-clock
           (and (member state '(:read-body :sending-100-continue))
                (plusp (connection-body-progress-at conn))))
         ;; A received body paused on a fetch it started into this
         ;; connection: the application is waiting on that fetch, whose own
         ;; deadline bounds it and whose failure disposition ends the body.
         ;; The stall clock resumes when the fetch is over, from the pause.
         (waiting-on-fetch
           (and body-clock
                (connection-body-paused conn)
                (connection-fetch-outstanding conn)))
         ;; Parked on a fetch whose streamed request is still going out: the
         ;; outbound's own bound is on that, and this one's wait for the
         ;; answer starts when the request has gone (BEGIN-RESPONSE-READ).
         (awaiting-a-send
           (and (eq state :awaiting)
                (let ((out (lookup-connection (connection-awaiting-fd conn))))
                  (and out (eq (connection-state out) :out-send)))))
         (timeout (cond
                    (body-clock *body-stall-timeout*)
                    ((eq state :websocket) *ws-idle-timeout*)
                    ;; A stream and a parked inbound are different
                    ;; judgments. *FETCH-TIMEOUT* asks how long an upstream
                    ;; may take to answer; this asks how long an app may
                    ;; produce nothing before its stream is presumed dead.
                    ;; Reusing either *IDLE-TIMEOUT* or *WS-IDLE-TIMEOUT*
                    ;; would be wrong in opposite directions — ten seconds
                    ;; reaps healthy streams, a day holds dead ones.
                    ((eq state :streaming) *stream-idle-timeout*)
                    ((eq state :awaiting)  *fetch-timeout*)
                    ;; Measured from the start of the linger; discarded
                    ;; bytes do not refresh it.
                    ((eq state :lingering) *linger-timeout*)
                    (t                     *idle-timeout*)))
         ;; A stream is judged on when its app last produced, not on when
         ;; the connection last saw an event. HANDLE-CLIENT-WRITE bumps
         ;; LAST-ACTIVE on EPOLLOUT entry, so a draining backlog would keep
         ;; refreshing the deadline of a stream whose producer stopped — the
         ;; conflation WRITE-PROGRESS-AT exists to break, one level up and
         ;; in another knob.
         ;;
         ;; A queue still moving counts as well. A producer that sent a
         ;; burst and waits on it — a paused relay among them — has not gone
         ;; quiet, its peer is reading. A queue that stops moving is the
         ;; stall arm's, which is checked first.
         (since (cond
                  ((eq state :streaming)
                   (if (plusp (connection-write-pending conn))
                       (max (connection-stream-produced-at conn)
                            (connection-write-progress-at conn))
                       (connection-stream-produced-at conn)))
                  (body-clock (connection-body-progress-at conn))
                  (t (connection-last-active conn))))
         (verdict
           (cond
             ;; Stall is checked first: a connection can be both, and
             ;; closing it for the reason that is actually wrong with it
             ;; makes the log line worth reading. Measured from the last
             ;; forward progress on the backlog, never from LAST-ACTIVE —
             ;; HANDLE-CLIENT-WRITE bumps that on EPOLLOUT entry, which
             ;; would refresh the deadline of precisely the stuck
             ;; connection. Any connection sitting on a backlog, not just a
             ;; WebSocket one. A test naming :WEBSOCKET would leave the
             ;; state most likely to build a backlog — a long-lived stream
             ;; — the one state with no stall bound, falling through to an
             ;; idle test measured on LAST-ACTIVE, the clock this exists to
             ;; avoid. Asking "is there a backlog and has it moved" needs
             ;; no list of states to keep up to date.
             ;; :AWAITING is the one exclusion, and not because of its
             ;; state so much as because it already has a more specific
             ;; answer: DELIVER-AWAITING-TIMEOUT's 504 is the documented
             ;; floor under every way a fetch can strand, and a stall close
             ;; would take that away and hand the client a bare disconnect
             ;; instead. The state is also supposed to have nothing
             ;; pending, so excluding it costs nothing that should ever
             ;; exist.
             ((and (not (eq state :awaiting))
                   (plusp *write-stall-timeout*)
                   (plusp (connection-write-pending conn))
                   (> (- now (connection-write-progress-at conn))
                      *write-stall-timeout*))
              :stalled)
             ;; A WebSocket message begun and not finished in time. Its own
             ;; clock, since a message's fragments do not move LAST-ACTIVE
             ;; and a ping answered keeps the connection alive. See
             ;; *WS-MESSAGE-TIMEOUT*.
             ((and (eq state :websocket)
                   (plusp *ws-message-timeout*)
                   (plusp (connection-ws-message-at conn))
                   (> (- now (connection-ws-message-at conn))
                      *ws-message-timeout*))
              :ws-late)
             (waiting-on-fetch nil)
             (awaiting-a-send nil)
             ((and (> timeout 0)
                   (> (- now since) timeout))
              (if body-clock :body-stalled :idle)))))
    (and verdict
         (eq (lookup-connection (connection-fd conn)) conn)
         verdict)))

(defun sweep-idle-connections (epoll-fd now)
  "Close connections that have been idle too long.
   HTTP uses *idle-timeout*. WebSocket uses *ws-idle-timeout*. A request
   body a body handler accepted uses *BODY-STALL-TIMEOUT*, measured from
   the last of its bytes to arrive rather than from the request's start.
   An :AWAITING connection is answered 504 first — see
   DELIVER-AWAITING-TIMEOUT.

   Also closes any connection whose write backlog has not moved for
   *WRITE-STALL-TIMEOUT*, whatever state it is in. That is a different
   question from idleness and the idle timeouts cannot answer it: they
   are measured on LAST-ACTIVE, which a peer that has stopped reading
   keeps fresh merely by continuing to send, and they are long by design
   on exactly the long-lived states most likely to build a backlog.

   What is due is collected in one walk of the table and acted on after
   it, and each connection is judged again when its turn comes. Acting on
   one connection can reach others: an expired fetch's :THEN is
   application code, and FORWARD-BODY's refuses the body the fetch was
   started for. By its turn a connection may have been answered, closed or
   moved on, and acting on what it was collected as would undo that — a
   stall close taking down the answer an expired fetch had just queued for
   the body, when the two come due in the same sweep. One no longer due is
   left for the next sweep to judge."
  (let ((idle nil)
        (stalled nil)
        (body-stalled nil)
        (ws-late nil)
        (connect-late nil)
        (detached-expired nil))
    (maphash (lambda (fd conn)
               (declare (ignore fd))
               (when (connect-late-p conn now)
                 (push conn connect-late))
               (when (fetch-expired-p conn now)
                 (push conn detached-expired))
               ;; Every other outbound is cleaned up via its paired
               ;; inbound's :awaiting timeout — see close-connection
               (unless (connection-outbound-p conn)
                 (case (inbound-overdue conn now)
                   (:stalled      (push conn stalled))
                   (:ws-late      (push conn ws-late))
                   (:body-stalled (push conn body-stalled))
                   (:idle         (push conn idle)))))
             *connections*)
    (dolist (conn detached-expired)
      (when (fetch-expired-p conn now)
        (if (eq (connection-state conn) :out-send)
            (log-warn "fetch request body made no progress for ~ds on outbound ~
                       fd ~d — aborting"
                      *fetch-timeout* (connection-fd conn))
            (log-warn "detached fetch exceeded ~ds on outbound fd ~d — aborting"
                      *fetch-timeout* (connection-fd conn)))
        ;; Through DELIVER-FETCH-ERROR so the app's callback fires its abort
        ;; exactly once and the target gets DELIVER-DETACHED's disposition,
        ;; rather than a bare close that leaves the stream open and the
        ;; callback never run.
        (handler-case
            (deliver-fetch-error conn epoll-fd
                                 (if (eq (connection-state conn) :out-send)
                                     "request body stalled"
                                     "detached fetch timed out"))
          (error (e)
            (log-warn "detached fetch timeout: could not abort fd ~d: ~a"
                      (connection-fd conn) e)
            (close-outbound conn epoll-fd)))))
    (dolist (conn connect-late)
      ;; After the expired fetches, any of which may have been this one.
      (when (connect-late-p conn now)
        (let ((why (format nil "no connect in ~ds" *connect-fallback-delay*)))
          (handler-case
              (unless (fall-back-to-next-address conn epoll-fd why)
                (deliver-fetch-error conn epoll-fd why))
            (error (e)
              (log-warn "fetch: could not move fd ~d to its next address: ~a"
                        (connection-fd conn) e)
              (close-outbound conn epoll-fd))))))
    (dolist (conn stalled)
      (when (eq (inbound-overdue conn now) :stalled)
        (log-info "write stalled ~ds fd ~d (~a, ~d bytes pending) — closing"
                  *write-stall-timeout* (connection-fd conn)
                  (connection-state conn)
                  (connection-write-pending conn))
        (close-connection conn epoll-fd :stalled)))
    (dolist (conn body-stalled)
      (when (eq (inbound-overdue conn now) :body-stalled)
        (log-info "request body stalled ~ds fd ~d (~d bytes buffered) — closing"
                  *body-stall-timeout* (connection-fd conn)
                  (connection-read-pos conn))
        (close-connection conn epoll-fd :stalled)))
    (dolist (conn ws-late)
      (when (eq (inbound-overdue conn now) :ws-late)
        (log-info "ws message unfinished after ~ds fd ~d (~d bytes held) — closing 1008"
                  *ws-message-timeout* (connection-fd conn)
                  (+ (connection-ws-frag-total conn) (connection-read-pos conn)))
        ;; What the message holds is let go now, not once the close frame
        ;; has gone out. A peer slow to read that holds the frame back is
        ;; then holding nothing more than its backlog.
        (setf (connection-ws-frag-buf conn) nil
              (connection-ws-frag-total conn) 0)
        (handler-case (let ((*epoll-fd* epoll-fd))
                        (ws-close conn 1008))
          (error (e)
            (log-warn "ws message timeout: could not close fd ~d with 1008: ~a"
                      (connection-fd conn) e)
            (close-connection conn epoll-fd :idle)))))
    (dolist (conn idle)
      (when (eq (inbound-overdue conn now) :idle)
        (log-debug "idle timeout fd ~d (~a)"
                   (connection-fd conn) (connection-state conn))
        ;; Collect-then-act: nothing here queues a write inside the MAPHASH
        ;; above, because DELIVER-AWAITING-TIMEOUT tears down the paired
        ;; outbound and that mutates the table being walked.
        (if (eq (connection-state conn) :awaiting)
            ;; A failure to answer must not be worse than not trying. If
            ;; anything in the 504 path signals — a write buffer that was
            ;; not drained after all, a serializer refusing a header — fall
            ;; back to a bare close, rather than letting it escape into
            ;; RUN-EVENT-LOOP, which has
            ;; no handler and would restart the worker mid-sweep.
            (handler-case (deliver-awaiting-timeout conn epoll-fd)
              (error (e)
                (log-warn "awaiting timeout: could not answer 504 on fd ~d: ~a"
                          (connection-fd conn) e)
                (close-connection conn epoll-fd :idle)))
            (close-connection conn epoll-fd :idle))))))

(defun keepalive-streams (epoll-fd now)
  "Send the keepalive bytes of any :STREAMING connection that has gone
   quiet for *STREAM-KEEPALIVE-INTERVAL*.

   Only connections whose STREAM-RESPONSE supplied keepalive bytes are
   touched, because there is nothing generic to send: a chunked stream's
   only zero-content emission is the empty chunk, and that is the
   terminator. Keeping a stream warm is necessarily the business of
   whatever protocol is riding on top of it.

   Skipped when anything is already queued — a connection with bytes
   waiting is not quiet, and adding to a backlog that is not draining
   would help nothing. The write goes out inline for the same reason the
   ping does: a keepalive is small, an idle socket takes it whole, and
   arming EPOLLOUT to deliver bytes that already fit is a wake-up for
   nothing. Failures close the connection rather than escaping into
   RUN-EVENT-LOOP, which has no handler."
  (when (plusp *stream-keepalive-interval*)
    (let ((broken nil))
      (maphash
       (lambda (fd conn)
         (declare (ignore fd))
         (when (and (eq (connection-state conn) :streaming)
                    (connection-stream-keepalive conn)
                    (zerop (connection-write-pending conn))
                    (>= (- now (connection-stream-produced-at conn))
                        *stream-keepalive-interval*))
           (handler-case
               (progn
                 ;; Counts as production, which is the point: a stream
                 ;; emitting keepalives is never reaped by
                 ;; *STREAM-IDLE-TIMEOUT*. Measured on the same clock the
                 ;; idle sweep reads, so the two cannot drift apart.
                 (setf (connection-stream-produced-at conn) now)
                 ;; Framed, not appended raw. These bytes land in the same
                 ;; body the app's sends land in, so on a chunked stream a
                 ;; raw comment line sits exactly where the peer's decoder
                 ;; expects a chunk-size — and the keepalive whose whole
                 ;; job is to stop a quiet stream being dropped becomes
                 ;; what drops it. FRAME-STREAM-BYTES is the one place
                 ;; that decision is made, so this writer cannot drift
                 ;; from STREAM-SEND again.
                 (let ((framed (frame-stream-bytes
                                conn (connection-stream-keepalive conn))))
                   (when (and framed (connection-append-write conn framed))
                     (unless (eq (connection-on-write conn) :done)
                       (epoll-modify epoll-fd (connection-fd conn)
                                     (logior +epollin+ +epollout+
                                             +epollet+))))))
             (error (e)
               (log-debug "stream keepalive failed fd ~d: ~a"
                          (connection-fd conn) e)
               (push conn broken)))))
       *connections*)
      (dolist (conn broken)
        (close-connection conn epoll-fd :disconnected)))))

(defun ping-ws-connections (epoll-fd)
  "Send pings to WebSocket connections and close dead ones.
   Dead = exceeded *ws-max-missed-pongs* consecutive unanswered pings.
   Skips connections with a write in progress (they're clearly not dead)."
  (let ((dead nil)
        (broken nil)
        (ping-frame (build-ws-ping)))
    (maphash (lambda (fd conn)
               (declare (ignore fd))
               (when (eq (connection-state conn) :websocket)
                 (cond
                   ;; Dead — too many missed pongs
                   ((>= (connection-missed-pongs conn) *ws-max-missed-pongs*)
                    (push conn dead))
                   ;; Nothing outstanding — send ping. Counts the append
                   ;; queue, not just the head: a connection with frames
                   ;; queued behind the one in flight is as clearly alive
                   ;; as one mid-write.
                   ((zerop (connection-write-pending conn))
                    (incf (connection-missed-pongs conn))
                    ;; Return ignored, and provably so: the guard above is
                    ;; that nothing is pending, and two bytes cannot
                    ;; overrun a bound the docs require to clear 1 MiB.
                    (connection-append-write conn ping-frame)
                    ;; Write it here rather than arming EPOLLOUT and coming
                    ;; back. A two-byte ping onto an empty queue is taken
                    ;; whole by any socket with room, so the arm-and-wake
                    ;; round trip was one epoll_ctl per connection per
                    ;; interval, issued as a single burst with the event
                    ;; loop serving nobody, to deliver two bytes that had
                    ;; already fit. Only a socket already backed up needs
                    ;; the arm.
                    ;;
                    ;; A peer that has gone away makes this write signal
                    ;; (EPIPE), and PING-WS-CONNECTIONS runs from the
                    ;; maintenance tick inside RUN-EVENT-LOOP, which has no
                    ;; handler and would restart the worker. Collected and
                    ;; closed alongside the unresponsive ones instead.
                    (handler-case
                        (unless (eq (connection-on-write conn) :done)
                          (epoll-modify epoll-fd (connection-fd conn)
                                        (logior +epollout+ +epollet+)))
                      (error (e)
                        (log-debug "ws ping write failed fd ~d: ~a"
                                   (connection-fd conn) e)
                        (push conn broken)))))))
             *connections*)
    (dolist (conn dead)
      (log-info "ws dead (missed ~d pongs) fd ~d"
                (connection-missed-pongs conn) (connection-fd conn))
      (close-connection conn epoll-fd))
    (dolist (conn broken)
      (close-connection conn epoll-fd))))

;;; ---------------------------------------------------------------------------
;;; Shutdown
;;; ---------------------------------------------------------------------------

(sb-ext:defglobal *shutdown* nil
  "Set to T to signal all workers to exit.
   defglobal (not defvar) to avoid per-thread bindings and ensure
   a single shared value cell across all worker threads.")

(sb-ext:defglobal *run-generation* 0
  "Which START-SERVER run is the current one, counted up as each begins.

   *SHUTDOWN* cannot tell runs apart. START-SERVER clears it on entry, and
   a worker an earlier run left behind — still inside a handler when that
   run's join gave up waiting — would see it clear when the handler
   returned and go back to serving: its old port, its old handler, beside
   the new run's workers. Every listener sets SO_REUSEPORT, so both hold
   the port and the kernel splits connections between them with nothing in
   either log. A worker belongs to the run that spawned it,
   *WORKER-GENERATION*, and WORKER-STOPPING-P stops it once a later run has
   begun.")

(defvar *worker-generation* nil
  "The run this worker thread belongs to: the *RUN-GENERATION* START-SERVER
   gave it, bound by the thread START-SERVER spawns, outside RUN-WORKER's
   restart loop. NIL off a worker.")

(defun worker-stopping-p ()
  "True when this worker should stop: shutdown was requested, or a later
   START-SERVER has begun since this worker's run did."
  (or *shutdown*
      (and *worker-generation*
           (/= *worker-generation* *run-generation*))))

(defparameter *drain-timeout* 5
  "Seconds to wait for connections to drain during graceful shutdown.
   Whole seconds: added to the clock, a fraction would turn the deadline
   into a float that resolves only to minutes, so START-SERVER refuses
   one.")

(defparameter *worker-wake-interval* 1
  "Seconds a worker's event loop may sleep in epoll_wait before waking to
   do periodic work, and the interval between shutdown-signal checks in
   the main thread's wait loop.

   This is the floor under everything periodic. A worker with no I/O is
   asleep in the kernel, so nothing that rides the event loop can happen
   more often than this — lowering it is the only way to make periodic
   work prompt, and the cost is one syscall return per worker per
   interval.

   Default 1 second balances wake-up overhead against shutdown
   responsiveness. Test harnesses bind this to a small value (e.g. 0.05)
   so teardown doesn't wait a full second per call. Float accepted —
   the worker converts to ms for epoll_wait.

   It does not set the periodic-maintenance cadence, but it bounds it.
   RUN-EVENT-LOOP gates the idle sweep and the census publish on a
   hardcoded one second and the WebSocket ping on *WS-PING-INTERVAL*, and
   looks at those gates only when the loop wakes. Lowering this makes
   shutdown prompt without making either scan run more often — the harness
   sets it to 0.05 and the sweep still runs at 1 Hz. Raising it past a
   second does the opposite for a worker with no traffic to wake it: the
   sweep, the publish and the ping all wait out the full interval.")

(defconstant +max-events+ 64
  "Maximum events to process per epoll_wait call. Internal — not a
   tunable. A larger batch increases worst-case latency for the
   tail of the batch without meaningfully improving throughput; a
   smaller batch adds syscall overhead. 64 is the size epoll-oriented
   servers commonly converge on.")

;;; ---------------------------------------------------------------------------
;;; User-extensible cleanup hooks
;;;
;;; Apps with background work (session reapers, cache flushers, metrics
;;; exporters) register zero-argument cleanup functions via REGISTER-CLEANUP.
;;; Hooks run inside start-server's unwind-protect after worker drain, each
;;; wrapped in HANDLER-CASE so one raising hook cannot prevent the rest from
;;; firing. This is the integration seam for primitives that own a live
;;; thread — e.g. a store with an expiry reaper.
;;; ---------------------------------------------------------------------------

(defvar *shutdown-hooks* nil
  "List of zero-argument cleanup functions, head = most recently registered.
   DEFVAR rather than DEFGLOBAL so tests can rebind it locally.
   Workers spawned via SB-THREAD:MAKE-THREAD see the top-level binding —
   dynamic bindings are not carried across thread creation.")

(defvar *shutdown-hooks-lock* (sb-thread:make-mutex :name "shutdown-hooks")
  "Serializes REGISTER-CLEANUP across concurrent threads.")

(defun register-cleanup (fn)
  "Register FN (a zero-argument function) to run during graceful shutdown.
   Hooks run after workers have drained, before START-SERVER returns.
   Thread-safe — callable from any thread, before or during server run.
   Each invocation is wrapped in HANDLER-CASE, so raising from a hook does
   not abort the rest. Returns FN.

   A hook runs once, at the end of the run it was registered before or
   during: the run that fires the hooks consumes them, so a second
   START-SERVER in the same image runs only the hooks registered since."
  (sb-thread:with-mutex (*shutdown-hooks-lock*)
    (push fn *shutdown-hooks*))
  fn)

(defun unregister-cleanup (fn)
  "Take FN, as REGISTER-CLEANUP returned it, back out of the hooks, for a
   resource stopped before the run ends: STOP-STORE-REAPER on a store an
   application retires. A FN no longer there (the run that fired it has
   taken the list) is no error."
  (sb-thread:with-mutex (*shutdown-hooks-lock*)
    (setf *shutdown-hooks* (delete fn *shutdown-hooks* :count 1)))
  nil)

(defun run-shutdown-hooks ()
  "Invoke each registered cleanup hook in LIFO order, catching errors.
   Called from START-SERVER's unwind-protect — do not call directly.
   Takes the list and empties it under the mutex before iterating, so the
   hooks are this run's and no later one's, and a hook that registers
   another leaves it for the next run rather than in the list being
   walked."
  (let ((hooks (sb-thread:with-mutex (*shutdown-hooks-lock*)
                 (shiftf *shutdown-hooks* nil))))
    (dolist (fn hooks)
      (handler-case (funcall fn)
        ;; SERIOUS-CONDITION, not ERROR: see HANDLE-CLIENT-READ's last catch.
        (serious-condition (e)
          (log-error "shutdown hook error: ~a" e))))))

;;; ---------------------------------------------------------------------------
;;; Request dispatch
;;; ---------------------------------------------------------------------------

(defun dispatch-request (request handler)
  "Route an HTTP request via HANDLER. Returns (values response upgrade-p).
   If the handler returns :UPGRADE, validates the WebSocket handshake."
  (classify-response request (if handler
                                 (funcall handler request)
                                 (make-error-response 501))))

(defun classify-response (request response)
  "Check RESPONSE, an answer to REQUEST from a handler or a received body's
   ON-END, against the shapes an answer may take, and return
   (values RESPONSE UPGRADE-P) — the response to carry out, :UPGRADE having
   become the 101 or the 400 its handshake earns. Anything else signals,
   naming the shapes."
  (cond
    ;; Handler signals WebSocket upgrade, as :UPGRADE or as a WS-UPGRADE
    ;; that shapes the 101
    ((or (eq response :upgrade) (ws-upgrade-p response))
     (if (websocket-upgrade-p request)
         (let ((upgrade (and (ws-upgrade-p response) response)))
           ;; A 101 naming a subprotocol the client did not offer fails the
           ;; connection at the client (RFC 6455 §4.1), with nothing said
           ;; to the server. Refused here instead, where the 500 can name
           ;; the handler's choice.
           (when (and upgrade
                      (ws-upgrade-protocol upgrade)
                      (not (member (ws-upgrade-protocol upgrade)
                                   (ws-offered-protocols request)
                                   :test #'string=)))
             (error "ws-upgrade: :protocol ~s is not one the client offered ~
                     (~{~s~^, ~}); WS-CHOOSE-PROTOCOL picks from what it offered"
                    (ws-upgrade-protocol upgrade) (ws-offered-protocols request)))
           (log-debug "~a ~a -> 101 upgrade"
                     (http-request-method request)
                     (http-request-path request))
           (values (make-websocket-handshake-response request upgrade) t))
         (progn
           ;; Throttled: a GET to the route with no upgrade headers is all
           ;; it takes, from curl, a crawler or a probe.
           (log-throttled :warn :bad-upgrade "~a ~a -> 400 bad upgrade"
                          (http-request-method request)
                          (http-request-path request))
           ;; RFC 6455 §4.4: MUST include supported version on rejection
           (let ((resp (make-error-response 400)))
             (set-response-header resp "sec-websocket-version" "13")
             (values resp nil)))))
    ;; Outbound fetch request — handler needs an external call
    ((typep response 'http-fetch-continuation)
     (log-debug "~a ~a -> fetch ~a"
                (http-request-method request)
                (http-request-path request)
                (http-fetch-continuation-url response))
     (values response nil))
    ;; Streaming response — the handler has headers now and a body it
    ;; will produce over time. Passed through for START-STREAM, which
    ;; is the only thing that knows how to frame one.
    ((typep response 'stream-response)
     (log-debug "~a ~a -> stream"
                (http-request-method request)
                (http-request-path request))
     (values response nil))
    ;; A response already in pieces: a list of byte vectors to write in
    ;; order. SERVE-STATIC's shape, so a pre-built file can carry a
    ;; current Date without the vector it was built from being rebuilt
    ;; or rewritten. The producer owns HEAD here — SERVE-STATIC decides
    ;; which segments a HEAD gets, because only it knows where its own
    ;; body begins, and STRIP-BODY-FOR-HEAD cannot find a boundary it
    ;; was not given.
    ((and (consp response)
          (typep (car response) '(simple-array (unsigned-byte 8) (*))))
     (log-debug "~a ~a -> static"
                (http-request-method request)
                (http-request-path request))
     (values response nil))
    ;; Pre-formatted response (e.g., static file — already bytes).
    ;; HEAD truncation happens centrally in HANDLE-CLIENT-READ via
    ;; STRIP-BODY-FOR-HEAD on the way to CONNECTION-QUEUE-WRITE, so
    ;; both byte-vector and HTTP-RESPONSE paths funnel through one
    ;; strip site instead of two.
    ((typep response '(simple-array (unsigned-byte 8) (*)))
     (log-debug "~a ~a -> static"
                (http-request-method request)
                (http-request-path request))
     (values response nil))
    ;; Normal HTTP response — must be an HTTP-RESPONSE struct. Anything
    ;; else is refused here with a pointed error. Let through, a handler
    ;; that forgot MAKE-TEXT-RESPONSE (returning a raw string or alist)
    ;; would trip a deep SIMPLE-TYPE-ERROR from a struct accessor inside
    ;; FORMAT-RESPONSE: the same 500, with a message that sends the
    ;; reader into HTTP-RESPONSE-STATUS rather than to their handler.
    ((typep response 'http-response)
     (log-debug "~a ~a -> ~d"
                (http-request-method request)
                (http-request-path request)
                (http-response-status response))
     (values response nil))
    (t
     (error "handler returned ~a; expected an HTTP-RESPONSE, a ~
             STREAM-RESPONSE, an HTTP-FETCH-CONTINUATION, a byte vector, ~
             a list of byte vectors (what SERVE-STATIC returns), or ~
             :UPGRADE"
            (type-of response)))))

;;; ---------------------------------------------------------------------------
;;; Accept a new connection
;;; ---------------------------------------------------------------------------

(defun refuse-connection (client-socket)
  "Answer 503 on a connection the worker has no room for, then close.

   A bare close leaves the client unable to tell \"server full\" from
   \"server broken\" — both are a socket that opens and immediately shuts —
   and gives it nothing to back off against. One pre-built write makes
   the condition name itself.

   Delivery is best effort, and both limits are structural.

   The write is a single non-blocking attempt. Blocking here would stall
   the accept loop, which is the thing the connection limit exists to
   protect. The response is under 200 bytes and an empty send buffer
   takes it whole, so a short write means the peer is already gone.

   The drain is bounded, and what it buys is a graceful close rather
   than delivery. close(2) on a socket still holding unread data makes
   Linux send RST instead of FIN. The response has already gone out by
   then, so the bytes do arrive; what the peer loses is the ordinary
   end-of-stream, getting a reset on the read that should have returned
   it — and a client that reports a connection error in place of the 503
   has learned nothing, which is the state this function exists to get
   out of.

   It shortens the odds rather than settling the matter, and cannot do
   better. The refusal happens at accept time, which may precede the
   peer's request arriving at all, so the drain clears only what is
   already queued. Waiting for the rest would mean blocking in the
   accept loop, and draining to EOF would mean parking a peer there is
   by definition no room for. Four buffers clears a request already in
   hand, which is the ordinary case; bytes still in flight, or a client
   midway through a large upload, still earn a reset — and are still no
   worse off than under a bare close."
  (let ((fd (ignore-errors (socket-fd client-socket))))
    (when (and fd (>= fd 0))
      (ignore-errors (set-nonblocking fd))
      (ignore-errors
       (nb-write fd *connection-limit-response* 0
                 (length *connection-limit-response*)))
      (let ((sink (or *refusal-drain-buf*
                      (make-array +refusal-drain-size+
                                  :element-type '(unsigned-byte 8)))))
        (dotimes (i 4)
          (declare (ignorable i))
          ;; NB-READ returns :AGAIN once the queue is empty and :EOF once
          ;; the peer is done; either way there is nothing left to clear.
          (unless (integerp (ignore-errors (nb-read fd sink 0 (length sink))))
            (return))))))
  (ignore-errors (sb-bsd-sockets:socket-close client-socket)))

(defun report-refusal (limit)
  "Log a refusal at LIMIT, :CONNECTIONS for *MAX-CONNECTIONS* or
   :DESCRIPTORS for *DESCRIPTOR-LIMIT*: the first at once, then at most
   once a minute per worker with a count of the ones in between, through
   LOG-THROTTLED.

   A refusal happens only when a worker is out of room, which is when a
   flood of connections is likeliest, and a line per refusal would have
   every worker queueing on the log lock in it. The line says refusals are
   happening, and which limit is the one met; CONNECTION-CENSUS counts
   every one of them under :REFUSED."
  (if (eq limit :descriptors)
      (log-throttled :warn :descriptor-limit
                     "descriptors nearly out (limit ~d), refusing new ~
                      accepts; counted in full under :refused in the census"
                     *descriptor-limit*)
      (log-throttled :warn :connection-limit
                     "connection limit reached (~d), refusing new accepts; ~
                      counted in full under :refused in the census"
                     *max-connections*)))

(defun accept-pending (listener-socket epoll-fd)
  "Accept every connection waiting on the listener. Returns NIL once the
   backlog is drained, and T when an accept failed first, so the caller
   knows to try again.

   The listener is edge-triggered, so its event fires when a connection
   arrives and not while connections wait. An accept that fails partway
   through the backlog — out of descriptors, typically — leaves the rest
   queued with nothing to report them again: freeing descriptors raises no
   edge, and the next one comes only when another client happens to
   connect. On a quiet server those clients would wait out their own
   timeouts after the server had recovered. RUN-EVENT-LOOP retries on its
   maintenance tick for as long as this answers T."
  (loop
    (let ((result (accept-connection listener-socket epoll-fd)))
      (cond ((null result) (return nil))
            ((eq result :failed) (return t))))))

(defun accept-connection (listener-socket epoll-fd)
  "Accept a pending connection and register it with epoll.
   Returns T if a connection was accepted, NIL if none pending (EAGAIN),
   and :FAILED if the accept itself failed — EMFILE, ENFILE, ENOMEM —
   with connections possibly still queued behind it; see ACCEPT-PENDING.
   Refuses the connection with a 503 if the per-worker limit is reached,
   or the process's descriptors nearly are."
  (let ((client-socket (handler-case
                          (sb-bsd-sockets:socket-accept listener-socket)
                        (error (e)
                          ;; Throttled, as a line a client can cause at
                          ;; will. Nothing waits here: ACCEPT-PENDING stops
                          ;; at the failure, and it is tried again on the
                          ;; tick or the next connection's arrival, so it
                          ;; cannot spin.
                          (log-throttled :error :accept-failed
                                         "socket-accept failed: ~a" e)
                          (return-from accept-connection :failed)))))
    (unless client-socket
      (return-from accept-connection nil))
    (let ((limit (cond ((and (> *max-connections* 0)
                             (>= (hash-table-count *connections*) *max-connections*))
                        :connections)
                       ((descriptors-nearly-out-p (socket-fd client-socket))
                        :descriptors))))
      (when limit
        (report-refusal limit)
        (note-refused)
        ;; And a response: REFUSE-CONNECTION writes a pre-built 503 straight
        ;; to the socket, so nothing downstream of here would count it. It is
        ;; the plainest case of a refusal no handler ran for, which is what
        ;; the census promises to count.
        (note-response 503)
        (refuse-connection client-socket)
        (return-from accept-connection t)))
    (handler-case
        (let ((conn (make-client-connection client-socket))
              (registered nil)
              (done nil))
          (unwind-protect
               (progn
                 (register-connection conn)
                 (note-accepted)
                 (setf registered t)
                 (epoll-add epoll-fd (connection-fd conn)
                            (logior +epollin+ +epollet+))
                 (log-debug "accepted fd ~d ~a" (connection-fd conn)
                            (or (connection-remote-addr conn) ""))
                 (setf done t))
            ;; If EPOLL-ADD raised after REGISTER-CONNECTION succeeded,
            ;; the connection is in *CONNECTIONS* but the kernel never
            ;; saw the fd — a stale entry that the idle sweeper would
            ;; later trip over. Drop it so the table stays honest.
            (when (and registered (not done))
              (unregister-connection conn)))
          t)
      (error (e)
        (log-error "accept failed: ~a" e)
        (ignore-errors (sb-bsd-sockets:socket-close client-socket))
        nil))))

;;; ---------------------------------------------------------------------------
;;; Close and clean up a connection
;;; ---------------------------------------------------------------------------

(defun begin-lingering-close (conn epoll-fd)
  "Close CONN after an early error response the way RFC 9112 §9.6 asks of
   a server that answers before it has read the request: shut down the
   write side, so the response is followed by a FIN, then read and discard
   whatever the client is still sending until it stops or *LINGER-TIMEOUT*
   runs out, and only then close.

   Closing at once, with the client's bytes unread in the kernel, makes
   Linux abort the connection with RST rather than end it with FIN: any
   response bytes still unsent are discarded, and the peer is told the
   connection failed. A reader on Linux keeps what had already arrived, so
   a small response read promptly survives. What survives elsewhere
   depends on the peer — a proxy or client stack that treats a reset as a
   failure may not pass the answer on — and the FIN removes the question.

   What the linger can promise has two limits. A client still sending
   when *LINGER-TIMEOUT* runs out is closed with bytes unread, so it gets
   the reset after all. And an intermediary that passes nothing back
   while it is still relaying an upload can lose the answer however the
   connection ends.

   :LINGERING is a state of its own so no worker waits: HANDLE-CLIENT-READ
   discards on each readable event and closes at the client's FIN, and
   the idle sweep closes what outlives *LINGER-TIMEOUT*, measured from
   here — the bytes discarded do not extend it."
  (unless (zerop (%shutdown (connection-fd conn) +shut-wr+))
    ;; The peer is already gone, or the socket is: nothing to linger for.
    (close-connection conn epoll-fd)
    (return-from begin-lingering-close))
  (setf (connection-state conn) :lingering
        (connection-last-active conn) (get-universal-time))
  (epoll-modify epoll-fd (connection-fd conn) (logior +epollin+ +epollet+))
  ;; Edge-triggered: what the client sent before this moment raises no new
  ;; edge, so the first drain is now.
  (linger-discard conn epoll-fd))

(defun linger-discard (conn epoll-fd)
  "Discard what a :LINGERING connection's client has sent, and close it once
   the client has finished."
  (case (connection-discard-available
         conn (or *stream-drain-buf*
                  (make-array 4096 :element-type '(unsigned-byte 8))))
    ((:eof :ok-eof) (close-connection conn epoll-fd))
    (t nil)))

(defun end-detached-fetches (conn epoll-fd)
  "Close the detached fetch still aimed at CONN, if there is one, its
   :THEN hearing the abort sentinel, and clear what such a fetch leaves on
   CONN: the outstanding marker, the failure disposition, a wait on its
   request draining, and the link to it from a relay's pause.

   For when what the fetch was started for has ended: the connection
   closing, a body ON-END has answered, a request whose response has gone
   with the connection kept for the next. A fetch outliving that would
   deliver to what followed: end the next request's body for a fetch it
   never started, close the stream a body was answered with, or have the
   next request's own FETCH-INTO refused as one already outstanding.

   The outbound is found by walking the table, since nothing parked on it
   to reach it through. Missed, it would outlive what it was fetching for,
   and a paused one, subscribed to no events, would never be woken: a leak
   with no end. The walk is made only when the marker says there is one to
   find, so a connection that started none pays nothing for it.

   The marker is cleared once the outbound is closed, not before: a :THEN
   that answers the sentinel by starting another fetch into CONN is
   refused as one already outstanding, since there is nothing left for a
   fetch to be for."
  (when (connection-fetch-outstanding conn)
    (let ((target-fd (connection-fd conn))
          (orphans nil))
      (maphash (lambda (k out)
                 (declare (ignore k))
                 (when (and (connection-outbound-p out)
                            (eq (connection-fetch-sink out) :detached)
                            (= (connection-inbound-fd out) target-fd))
                   (push out orphans)))
               *connections*)
      (dolist (out orphans)
        (close-outbound out epoll-fd)))
    (setf (connection-fetch-outstanding conn) nil))
  (setf (connection-fetch-failure-disposition conn) :close
        (connection-send-waiting conn) nil
        (connection-paused-outbound-fd conn) -1))

(defun close-connection (conn epoll-fd &optional (reason :closed))
  "Remove from epoll, unregister, close fd.

   REASON is passed to a streaming connection's ON-CLOSE callback, which
   fires here rather than at each call site so that no way of tearing a
   stream down can forget it — the same argument as the fetch cleanup
   sentinel below. Callers that know why they are closing say so;
   :CLOSED is the honest answer for the ones that do not.

   If CONN is :awaiting, also closes its outbound connection via
   CLOSE-OUTBOUND — which fires the app's fetch cleanup callback if
   the outbound was still carrying one, so DB handles / metrics /
   rate-limit counters get a defined moment to run their teardown
   even on inbound-driven aborts (drain, idle timeout, I/O error).
   MAYBE-REAP-DNS-PROCESS runs on every cleaned-up connection so a
   half-finished DNS lookup never leaks."
  (let ((fd (connection-fd conn)))
    (when (>= fd 0)
      ;; Before anything else is torn down, so the app's callback still
      ;; sees a live connection to read state off. NOTIFY-STREAM-CLOSED
      ;; is a no-op once fired, so a stream ended normally through
      ;; STREAM-CLOSE does not hear about it twice.
      (notify-stream-closed conn reason)
      ;; The same for a body being received: whatever is closing the
      ;; connection ends the body, and the plan's ON-ABORT hears why. A
      ;; body that ended already has no plan left to tell.
      (notify-body-aborted conn reason)
      ;; If parked waiting for a fetch, close the orphaned outbound too.
      (when (eq (connection-state conn) :awaiting)
        (let ((out-fd (connection-awaiting-fd conn)))
          (when (>= out-fd 0)
            (let ((out-conn (lookup-connection out-fd)))
              (when out-conn
                (close-outbound out-conn epoll-fd))))))
      ;; And a detached fetch aimed at it, which nothing parked to reach.
      (end-detached-fetches conn epoll-fd)
      (ignore-errors (epoll-remove epoll-fd fd))
      (unregister-connection conn)
      (maybe-reap-dns-process conn)
      (connection-close conn)
      (log-debug "closed fd ~d (~a)" fd reason))))

;;; ---------------------------------------------------------------------------
;;; Graceful drain — flush in-progress writes, close cleanly
;;; ---------------------------------------------------------------------------

(defun drain-connections (listener-socket epoll-fd event-buf)
  "Gracefully drain all active connections during shutdown.
   Stops accepting, sends WebSocket close frames, lets writes flush,
   force-closes anything remaining after *drain-timeout*."
  ;; Stop accepting new connections
  (ignore-errors (epoll-remove epoll-fd (socket-fd listener-socket)))
  (let ((count (hash-table-count *connections*)))
    (when (zerop count)
      (return-from drain-connections))
    (log-info "draining ~d connection~:p" count))
  ;; Phase 1: put every connection on its way out. Outbounds are closed
  ;; first, through CLOSE-OUTBOUND so their fetch callbacks fire —
  ;; CLOSE-CONNECTION on an outbound would not, because its :AWAITING
  ;; branch handles a paired outbound's teardown and not the outbound
  ;; itself. Inbounds afterward, by WIND-DOWN: any parked in :AWAITING find
  ;; their outbound already gone and short-circuit cleanly. Both lists are
  ;; collected first, because a fetch callback is application code and may
  ;; close a connection of its own.
  (flet ((wind-down (conn)
           ;; What a connection gets at shutdown, decided by its state.
           (case (connection-state conn)
             ;; Nothing owed to the peer that a drain could finish: a
             ;; request still arriving, or one whose 100 Continue has
             ;; invited a body nothing will read. A stream will not end on
             ;; its own, so waiting for it would spend the whole drain
             ;; timeout. Closed without a terminator on purpose: the client
             ;; reads a truncated chunked body, which is exactly what
             ;; happened, and a terminator would claim a complete response
             ;; the app never finished sending. The app hears :SHUTDOWN.
             ((:read-http :read-body :sending-100-continue :awaiting :streaming
               :lingering)
              (close-connection conn epoll-fd :shutdown))
             (:websocket
              ;; Must not clobber an in-flight write. The race:
              ;; ping-ws-connections queued a 2-byte ping whose
              ;; first byte was flushed, write-pos=1 write-end=2
              ;; (EAGAIN on byte 2). SIGTERM arrives, drain runs,
              ;; connection-queue-write overwrites write-buf with
              ;; the close frame bytes and resets write-pos to 0.
              ;; What hits the wire: partial ping + close frame
              ;; bytes as one contiguous buffer, and the client
              ;; interprets the close frame's FIN+opcode byte as
              ;; the ping's length, then waits forever. Same
              ;; discipline ping-ws-connections itself uses
              ;; (queue only when nothing is in flight). If a
              ;; write IS in flight, transition to :closing and
              ;; let handle-client-write tear the connection
              ;; down after the partial write finishes — the
              ;; peer sees a truncated frame rather than a
              ;; corrupt one.
              ;; Pending counts the append queue as well as the head,
              ;; so a connection with frames stacked behind the one in
              ;; flight takes the same branch rather than having the
              ;; close frame overwrite the head out from under them.
              (cond
                ((plusp (connection-write-pending conn))
                 (setf (connection-state conn) :closing))
                (t
                 (connection-queue-write conn (build-ws-close 1001))
                 (setf (connection-state conn) :closing)
                 (epoll-modify epoll-fd (connection-fd conn)
                               (logior +epollout+ +epollet+)))))
             ;; :write-response, :ws-upgrade, :closing — a response or a
             ;; frame is in flight, and is let finish. Marked to close after
             ;; it, or a keep-alive response would reset the connection to
             ;; :READ-HTTP on its last byte: the drain never closes a
             ;; connection that is reading, so shutdown would wait out
             ;; *DRAIN-TIMEOUT* for a response delivered in milliseconds,
             ;; while the client, told nothing, sends its next request onto
             ;; a socket about to be force-closed. A response already
             ;; serialized cannot be given Connection: close any more; its
             ;; connection can still be closed after it.
             (t
              (setf (connection-close-after-p conn) t)))))
    (let ((outbounds nil)
          (inbounds nil))
      (maphash (lambda (fd conn)
                 (declare (ignore fd))
                 (if (connection-outbound-p conn)
                     (push conn outbounds)
                     (push conn inbounds)))
               *connections*)
      (dolist (conn outbounds)
        (close-outbound conn epoll-fd))
      (dolist (conn inbounds)
        (when (eq (lookup-connection (connection-fd conn)) conn)
          (wind-down conn))))
    ;; Phase 2: flush remaining writes until drained or timeout
    (drain-pending-writes epoll-fd event-buf #'wind-down)))

(defun drain-pending-writes (epoll-fd event-buf wind-down)
  "Phase 2 of DRAIN-CONNECTIONS: flush what phase 1 let finish, until the
   table is empty or *DRAIN-TIMEOUT* runs out.

   A write that completes can move a connection on rather than close it:
   a 101 becomes :WEBSOCKET, which clears the close-after mark the drain
   set on it. A connection that lands in a state phase 1 would have
   handled is handed to WIND-DOWN as it lands, so the WebSocket gets its
   1001 like every other, rather than sitting until the deadline."
  (let ((deadline (+ (get-universal-time) *drain-timeout*)))
    (loop
      (when (zerop (hash-table-count *connections*))
        (log-info "all connections drained")
        (return))
      (when (> (get-universal-time) deadline)
        (log-warn "drain timeout — force-closing ~d connection~:p"
                  (hash-table-count *connections*))
        (return))
      (let ((n (epoll-wait epoll-fd event-buf +max-events+ 200)))
        (loop for i from 0 below n
              do (let* ((fd (epoll-event-fd event-buf i))
                        (flags (epoll-event-flags event-buf i))
                        (conn (lookup-connection fd)))
                   (when conn
                     (cond
                       ((or (logtest flags +epollerr+)
                            (logtest flags +epollhup+))
                        (close-connection conn epoll-fd))
                       ((logtest flags +epollout+)
                        (handle-client-write conn epoll-fd)
                        (when (and (eq (lookup-connection fd) conn)
                                   (member (connection-state conn)
                                           '(:read-http :read-body
                                             :sending-100-continue
                                             :awaiting :streaming
                                             :websocket :lingering)))
                          (funcall wind-down conn)))))))))))

;;; ---------------------------------------------------------------------------
;;; Handle readable event on a client fd
;;; ---------------------------------------------------------------------------

(defun start-stream (conn epoll-fd request sresp)
  "Send the head of a streaming response and hand CONN to the app.

   The head goes out through CONNECTION-QUEUE-WRITE, whose guard is a
   real check here: nothing should be pending on a connection that has
   just finished parsing a request, and a stream is the last thing that
   should be appended behind someone else's half-written response.

   Arming happens once, after ON-OPEN returns, from what is actually
   pending — the same reason HANDLE-CLIENT-READ's :websocket branch
   centralized it. ON-OPEN may send nothing, may send and flush
   completely, may leave a remainder, or may close the stream outright,
   and each of those wants a different interest."
  (let* ((framing (stream-framing-for request))
         (head (format-streaming-head (stream-response-response sresp) framing
                                      :connection-hint (connection-hint-for conn))))
    (connection-queue-write conn head)
    (cond
      ;; HEAD: RFC 7231 §4.3.2 wants the headers a GET would have
      ;; carried, which is what the head already is. No body follows, so
      ;; no stream starts, no terminator is owed, and the connection is
      ;; reusable exactly as it would be for any other response.
      ((eq (http-request-method request) :HEAD)
       (setf (connection-state conn) :write-response)
       (epoll-modify epoll-fd (connection-fd conn)
                     (logior +epollout+ +epollet+)))
      (t
       (when (eq framing :close)
         ;; Close-delimited framing *is* the end of the connection.
         (setf (connection-close-after-p conn) t))
       (setf (connection-stream-framing conn) framing
             (connection-stream-on-close conn) (stream-response-on-close sresp)
             (connection-stream-keepalive conn) (stream-response-keepalive sresp)
             (connection-state conn) :streaming
             (connection-last-active conn) (get-universal-time)
             (connection-stream-produced-at conn) (get-universal-time))
       (let ((on-open (stream-response-on-open sresp)))
         (when on-open
           (funcall on-open conn)))
       (epoll-modify
        epoll-fd (connection-fd conn)
        (if (eq (connection-state conn) :streaming)
            (logior +epollin+ +epollet+
                    (if (plusp (connection-write-pending conn)) +epollout+ 0))
            ;; ON-OPEN closed the stream already — a one-shot producer
            ;; that had everything to say up front. The terminator is
            ;; queued and the ordinary write path takes it from here.
            (logior +epollout+ +epollet+)))))))

(defun answer-error-or-close (conn epoll-fd status)
  "Answer CONN's request with an error STATUS and close after it — or, when
   the connection is past the point where an answer can go, close it now.

   An answer is owed only while the request is still being read or
   dispatched: :READ-HTTP or :READ-BODY. Past that, the connection has
   committed to something — a stream's head is on the wire, a 101 has
   switched protocols, a response is queued, a fetch is parked — and a
   second response would land inside or behind the first: a status line
   where a reader expects a chunk-size, or HTTP among WebSocket frames, read
   downstream as more of the first. Closing is the one answer nothing can
   misread. The peer sees the response end short, a stream's ON-CLOSE hears
   :ERROR, and a parked fetch's outbound is closed with the connection.

   The answer goes through STRIP-BODY-FOR-HEAD, so a HEAD request gets its
   error without a body. That needs CONNECTION-REQUEST, which is set only
   once PARSE-REQUEST-BYTES succeeds, so for a parse error it is usually a
   no-op. The answer can itself fail — CONNECTION-QUEUE-WRITE refuses to
   put it behind anything already queued — and then the connection is
   closed without one."
  (if (member (connection-state conn) '(:read-http :read-body))
      (handler-case
          (let ((resp (make-error-response status)))
            (set-response-header resp "connection" "close")
            (connection-queue-write conn (strip-body-for-head
                                          (format-response resp) conn))
            ;; Lingering: an answer given while the request is still being
            ;; read may leave the client sending. See BEGIN-LINGERING-CLOSE.
            (setf (connection-state conn) :write-response
                  (connection-close-after-p conn) t
                  (connection-linger conn) t)
            (epoll-modify epoll-fd (connection-fd conn)
                          (logior +epollout+ +epollet+)))
        (error ()
          (close-connection conn epoll-fd)))
      (progn
        (log-debug "fd ~d: closing in state ~a rather than answering ~d — ~
                    its response has begun" (connection-fd conn)
                    (connection-state conn) status)
        (close-connection conn epoll-fd :error))))

(defun decide-keep-alive (conn request)
  "Set CONN's CLOSE-AFTER-P from REQUEST: HTTP/1.1's default is
   keep-alive, HTTP/1.0's is close, and a Connection header overrides
   either.

   RFC 7230 §6.1 + §3.2.2: Connection is a list-valued header, and
   multiple occurrences are semantically equivalent to one comma-joined
   value. GET-HEADERS rather than GET-HEADER, so a split Connection:
   keep-alive / Connection: close pair from a cooperating proxy is read
   as close instead of the second value being silently dropped."
  (let ((conn-values (get-headers request "connection")))
    (setf (connection-close-after-p conn)
          (cond
            ((some (lambda (v)
                     (header-has-token-p v "close"))
                   conn-values)
             t)
            ((some (lambda (v)
                     (header-has-token-p v "keep-alive"))
                   conn-values)
             nil)
            ((string= (http-request-version request) "1.0") t)
            (t nil)))))

(defun carry-out-response (conn epoll-fd request response upgrade-p &key inline)
  "Set out RESPONSE, CLASSIFY-RESPONSE's answer to CONN's REQUEST: a fetch
   to park on, a stream to start, or a response to queue.

   INLINE, from the read that produced the request, lets a response be
   written here rather than on a writability wake-up: see
   WRITE-RESPONSE-INLINE-P."
  ;; Sync close-after-p from a handler-set Connection: close
  ;; header. Without this step an HTTP/1.1 handler that
  ;; explicitly asks to close the socket would advertise
  ;; 'Connection: close' on the wire while the server
  ;; happily held the socket open — framing mismatch vs
  ;; the client's expectation. The helper is the one
  ;; COMPLETE-FETCH calls for a fetch's :THEN, over either
  ;; scheme, and it walks every 'connection' header,
  ;; symmetric with DECIDE-KEEP-ALIVE's walk of the request's.
  ;; A stream's Connection header lives on the response it
  ;; carries, not on the wrapper — an app that asks to
  ;; close after a stream has to be heard the same way as
  ;; one asking after an ordinary response.
  (sync-close-after-p-from-response
   conn (if (typep response 'stream-response)
            (stream-response-response response)
            response))
  (cond
    ;; A streamed fetch already started for this request — a received
    ;; body's ON-END answering with the fetch the body went into — answers
    ;; it once the upstream has, or with what it answered already.
    ((and (typep response 'http-fetch-continuation)
          (fetch-started-p response))
     (multiple-value-bind (verdict answer) (adopt-fetch conn response)
       (when (eq verdict :answer)
         (multiple-value-bind (response upgrade-p) (classify-response request answer)
           (carry-out-response conn epoll-fd request response upgrade-p)))))
    ;; Outbound fetch — park and initiate
    ((typep response 'http-fetch-continuation)
     (initiate-fetch conn epoll-fd response))
    ;; Streaming response — send the head, hand over
    ((typep response 'stream-response)
     (start-stream conn epoll-fd request response))
    ;; A complete response, in any of its shapes, through
    ;; the function COMPLETE-FETCH shares, so a fetch's
    ;; :THEN can answer with whatever a handler can.
    (t
     (queue-final-response conn response)
     (setf (connection-state conn)
           (if upgrade-p :ws-upgrade :write-response))
     (if (and inline (write-response-inline-p conn))
         (write-response-inline conn epoll-fd)
         (epoll-modify epoll-fd (connection-fd conn)
                       (logior +epollout+ +epollet+))))))

(defun write-response-inline-p (conn)
  "T when CONN's queued response may be written at once, from the read
   that produced its request: an ordinary response on a connection kept
   alive, with nothing buffered after the request.

   Written then, a response the socket takes whole costs no change of
   epoll interest to writability and no wake-up to write it, where arming
   writability and coming back costs two of the first and one of the
   second. The other cases keep that path, for what each finishes with:
   - an upgrade, whose 101 leaves frames behind it to read;
   - a close, which ends in a lingering drain or a teardown;
   - pipelined bytes, which the keep-alive reset leaves for a read the
     wake-up's return value asks for (DISPATCH-CONNECTION-EVENT)."
  (and (eq (connection-state conn) :write-response)
       (not (connection-close-after-p conn))
       (<= (connection-read-pos conn) (connection-request-end conn))))

(defun write-response-inline (conn epoll-fd)
  "Write CONN's response now, through HANDLE-CLIENT-WRITE, which resets a
   connection whose response has all gone for its next request, as it does
   on a writability wake-up. A remainder the socket did not take is left
   to that wake-up, armed here because it is needed."
  (handle-client-write conn epoll-fd)
  (when (and (eq (lookup-connection (connection-fd conn)) conn)
             (eq (connection-state conn) :write-response))
    (epoll-modify epoll-fd (connection-fd conn)
                  (logior +epollout+ +epollet+))))

(defun answer-request (conn epoll-fd handler)
  "CONN's request is complete: parse it, hand it to HANDLER, and set out
   whatever HANDLER answers."
  (setf (connection-last-active conn) (get-universal-time))
  (let ((request (connection-parse-request conn)))
    (log-debug "~a ~a~@[?~a~] HTTP/~a ~a"
               (http-request-method request)
               (http-request-path request)
               (http-request-query request)
               (http-request-version request)
               (or (connection-remote-addr conn) ""))
    (decide-keep-alive conn request)
    (multiple-value-bind (response upgrade-p)
        (dispatch-request request handler)
      (carry-out-response conn epoll-fd request response upgrade-p :inline t))))

(defun answer-received-body (conn epoll-fd)
  "CONN's received body is complete: claim its plan, let ON-END answer the
   request, and set the answer out as a handler's would be.

   The plan is claimed before ON-END runs, so nothing that happens to the
   connection afterwards can reach ON-ABORT — ON-END returning is the
   body's end. ON-END raising is not: the body's ON-ABORT hears :ERROR,
   and the raise goes on to HANDLE-CLIENT-READ's catch, which answers 500.
   :UPGRADE is refused here by name, since a request with a body has had
   it read and is no handshake."
  (setf (connection-last-active conn) (get-universal-time))
  (let ((plan (claim-body-receiver conn))
        (request (connection-request conn)))
    (log-debug "~a ~a: body received (~d bytes)"
               (http-request-method request)
               (http-request-path request)
               (connection-body-received conn))
    (decide-keep-alive conn request)
    (let ((answer (handler-case (funcall (body-plan-on-end plan) conn)
                    ;; SERIOUS-CONDITION, not ERROR: see HANDLE-CLIENT-READ's
                    ;; last catch, which is where this goes on to.
                    (serious-condition (e)
                      (abort-received-body plan conn :error)
                      (error e)))))
      (when (or (eq answer :upgrade) (ws-upgrade-p answer))
        (error "receive-body: :on-end returned ~:[:UPGRADE~;a WS-UPGRADE~]; a ~
                request whose body was received cannot switch protocols"
               (ws-upgrade-p answer)))
      ;; A fetch started for the body, and still running now the body has
      ;; ended, is ended with it — unless the answer is that fetch, which
      ;; the request is answered with (ADOPT-FETCH): started for CONN and
      ;; not yet over. One that is over has answered already, and a fetch
      ;; started into the body after it is another, ended here as any is.
      ;; Before the answer is set out, so a stream it starts is the
      ;; stream's own to fetch into.
      (unless (and (typep answer 'http-fetch-continuation)
                   (fetch-started-p answer)
                   (not (http-fetch-continuation-ended answer))
                   (= (http-fetch-continuation-target-fd answer)
                      (connection-fd conn)))
        (end-detached-fetches conn epoll-fd))
      (multiple-value-bind (response upgrade-p)
          (classify-response request answer)
        (carry-out-response conn epoll-fd request response upgrade-p)))))

(defun answer-before-body (conn epoll-fd request response)
  "Send RESPONSE, a body handler's or a received body's ON-DATA's answer,
   as CONN's whole answer to REQUEST, whose body has not all been read, and
   close after it.

   Closed rather than kept, whatever RESPONSE's own Connection header
   says. The body has not all been read and may still be arriving — or,
   from a client that sent Expect: 100-continue, may never be sent — so
   there is no telling where a next request would begin. The close
   lingers, as every early refusal's does, so the answer is followed by a
   FIN rather than torn off by a reset (see BEGIN-LINGERING-CLOSE)."
  (log-debug "~a ~a -> ~d before the end of its body"
             (http-request-method request)
             (http-request-path request)
             (http-response-status response))
  (setf (connection-close-after-p conn) t
        (connection-linger conn) t)
  (if (plusp (connection-write-pending conn))
      ;; A 100 Continue is still going out — BODY-REFUSE can be called
      ;; between its queueing and its flush — and the answer follows it, an
      ;; interim and then a final response being an ordinary exchange.
      (connection-append-write
       conn (format-response response
                             :connection-hint :close
                             :head-only-p (eq (http-request-method request) :HEAD)))
      (queue-final-response conn response))
  (setf (connection-state conn) :write-response)
  (epoll-modify epoll-fd (connection-fd conn) (logior +epollout+ +epollet+)))

(defun body-refuse (conn response)
  "Refuse the rest of a received body with RESPONSE, from wherever on the
   worker that owns CONN the decision is made — a fetch's :THEN, an
   :ON-TICK — as ON-DATA refuses it by returning a response. RESPONSE is
   sent, the connection closes after it, and the plan's ON-ABORT hears
   :REFUSED.

   Returns T, or NIL when CONN is not receiving a body: the body has ended
   or the connection has closed, as either can have by the time a callback
   that was waiting on something else runs. A paused body may be refused;
   nothing of it is read again.

   Signals, before anything is touched, if RESPONSE is not an
   HTTP-RESPONSE, and off a worker or on a connection another worker owns,
   as BODY-RESUME does."
  (unless (typep response 'http-response)
    (error "body-refuse: ~s is not an HTTP-RESPONSE" response))
  (when (and (>= (connection-fd conn) 0) (connection-body-receiver conn))
    (unless *epoll-fd*
      (error "body-refuse: no event loop on this thread. A received body is ~
              refused from the worker that owns its connection."))
    (unless (eq (lookup-connection (connection-fd conn)) conn)
      (error "body-refuse: fd ~d is not on this worker's connection table."
             (connection-fd conn)))
    (answer-before-body conn *epoll-fd* (connection-request conn) response)
    (notify-body-aborted conn :refused)
    t))

(defun decide-request-body (conn epoll-fd peer-finished)
  "Ask *BODY-HANDLER* what becomes of the body of CONN's request, whose
   head has arrived, and begin it. Returns what CONNECTION-BEGIN-BODY
   returns, for HANDLE-CLIENT-READ to act on as it would on
   CONNECTION-ON-READ's own verdict, or :ANSWERED when the body handler
   answered before the body. PEER-FINISHED is CONNECTION-ON-READ's second
   value.

   The head is parsed here, so the body handler is handed a request with
   everything but its body, and the head is not parsed again at dispatch.
   Whatever the body handler raises is the request's failure, as a
   handler's is: HANDLE-CLIENT-READ's catch answers it, and the state is
   :READ-BODY, so the answer is the lingering error response a request
   still being read gets."
  (let* ((request (connection-parse-head conn))
         (decision (funcall *body-handler* request)))
    (cond
      ((null decision))
      ((body-plan-p decision)
       (apply-body-plan conn decision))
      ((typep decision 'http-response)
       (answer-before-body conn epoll-fd request decision)
       (return-from decide-request-body :answered))
      (t
       (error "body handler returned ~a; expected NIL, a plan from ~
               BUFFER-BODY, RECEIVE-BODY or RECEIVE-MULTIPART, or an ~
               HTTP-RESPONSE to answer ~
               before the body"
              (type-of decision))))
    (connection-begin-body conn peer-finished)))

(defun handle-client-read (conn epoll-fd handler ws-handler)
  "Handle EPOLLIN on a client connection.
   State dispatch is permissive: a stale EPOLLIN for a connection
   whose state moved to :write-response / :ws-upgrade / :closing /
   :sending-100-continue earlier in the same epoll batch silently
   no-ops instead of raising into the outer handler-case, which
   would close the connection with the legitimate response still
   in the write buffer. The race is reachable when one event in a
   batch completes a fetch (setting an inbound's state to
   :write-response) and a later event in the same batch is a
   stale EPOLLIN for that inbound."
  (handler-case
      (case (connection-state conn)
        ;; HTTP request accumulation
        ((:read-http :read-body)
         (multiple-value-bind (verdict extra) (connection-on-read conn)
           (let ((decided (eq verdict :decide-body)))
             ;; The head of a request with a body, held for *BODY-HANDLER*
             ;; before any of the body is read. What the decision begins is
             ;; the verdict, acted on below like any other.
             (when decided
               (multiple-value-setq (verdict extra)
                 (decide-request-body conn epoll-fd extra)))
             (case verdict
               (:dispatch
                (answer-request conn epoll-fd handler))
               ;; A received body is complete: its plan answers it.
               (:received
                (answer-received-body conn epoll-fd))
               ;; A received body's ON-DATA answered with EXTRA. The answer
               ;; is queued before ON-ABORT hears :REFUSED, so a callback
               ;; that looks at the connection finds it answered.
               (:refused
                (answer-before-body conn epoll-fd (connection-request conn)
                                    extra)
                (notify-body-aborted conn :refused))
               ;; A received body has more in the kernel than one pass hands
               ;; over. Writability is true at once, so arming it brings
               ;; this connection back on the next pass, after the events
               ;; already waiting; HANDLE-CLIENT-WRITE's :READ-BODY arm
               ;; reads again.
               (:yield
                (epoll-modify epoll-fd (connection-fd conn)
                              (logior +epollin+ +epollout+ +epollet+)))
               ;; A received body's ON-DATA paused it: subscribed to nothing,
               ;; so the client's bytes stay in the kernel and its window
               ;; fills, until BODY-RESUME. Errors and hang-ups are reported
               ;; whatever the interest, and close it as the peer gone.
               (:paused
                (log-debug "fd ~d: received body paused at ~d bytes"
                           (connection-fd conn) (connection-body-received conn))
                (epoll-modify epoll-fd (connection-fd conn) +epollet+))
               ;; A received body ended from inside its plan's feed, by a
               ;; BODY-REFUSE called there, which answered it.
               (:ended nil)
               (:flush-queued
                ;; connection-on-read queued response bytes that must flush
                ;; before further reads happen. Covers two cases:
                ;;   :sending-100-continue — interim 100, body read resumes
                ;;                           once the flush completes.
                ;;   :write-response       — terminal 417 on unknown Expect,
                ;;                           close-after-p=T closes after.
                ;; Either way the next step is EPOLLOUT; handle-client-write
                ;; flushes and state-based dispatch takes it from there.
                (epoll-modify epoll-fd (connection-fd conn)
                              (logior +epollout+ +epollet+)))
               ;; The read path answers :CLOSE only when the peer has gone —
               ;; nothing buffered at its FIN, or a request its FIN left
               ;; unfinishable — so a body being received hears that.
               (:close
                (close-connection conn epoll-fd :disconnected))
               ;; :CONTINUE after a decision means read again now, in this
               ;; wake-up. The decision can raise the read cap above the one
               ;; the last read stopped at, and bytes left in the kernel
               ;; then raise no new edge: a client that had sent its whole
               ;; body would wait for an event that is not coming. One more
               ;; read finds them, or finds :AGAIN and costs a syscall. The
               ;; second pass cannot come back here — the state is
               ;; :READ-BODY now. Without a decision, :CONTINUE waits for
               ;; more data, and :ANSWERED has nothing left to do.
               (:continue
                (when decided
                  (handle-client-read conn epoll-fd handler ws-handler)))))))
        ;; WebSocket frame processing
        (:websocket
         (multiple-value-bind (result peer-finished) (connection-on-read conn)
           (case result
             (:websocket
              (multiple-value-bind (response close-frame)
                  (handler-case
                      (websocket-on-read conn ws-handler)
                    ;; SERIOUS-CONDITION, not ERROR: see HANDLE-CLIENT-READ's
                    ;; own catch at the bottom of this function.
                    (serious-condition (e)
                      ;; Handler error — close directly without a close frame.
                      ;; A ws-send may have put part of a frame on the wire
                      ;; and left the rest queued, and this teardown drops
                      ;; the queue; appending a close frame instead would
                      ;; land it where the peer is still reading payload
                      ;; and be taken as continuation of the partial one.
                      (log-warn "ws handler error fd ~d: ~a"
                                (connection-fd conn) e)
                      (close-connection conn epoll-fd)
                      (return-from handle-client-read)))
                ;; The replies are queued already, by WEBSOCKET-ON-READ, in
                ;; the order they were produced among the handler's own
                ;; WS-SEND frames. What is left is the close frame, which
                ;; appends behind them rather than replacing anything:
                ;; CONNECTION-QUEUE-WRITE would clobber the queue or, since
                ;; its guard counts the queue, signal.
                (cond
                  ;; Close requested — send close frame back, then shut down
                  ((eq response :close)
                   ;; Best-effort: a full queue means the peer already is
                   ;; not draining, and the teardown is what matters.
                   (when close-frame
                     (connection-append-write conn close-frame))
                   ;; Mark as closing so on-write knows to disconnect
                   (setf (connection-state conn) :closing))
                  ;; A reply met *MAX-WRITE-BACKLOG*; WEBSOCKET-ON-READ's
                  ;; QUEUE-REPLY has why the connection goes.
                  ((eq response :backlog-full)
                   (log-warn "ws backlog full fd ~d (~d bytes pending) — closing"
                             (connection-fd conn)
                             (connection-write-pending conn))
                   (close-connection conn epoll-fd)
                   (return-from handle-client-read))
                  ;; The ws-handler closed the connection outright, as
                  ;; WS-CLOSE does a peer with no room for its close frame:
                  ;; there is nothing left to arm.
                  ((eq response :closed)
                   (return-from handle-client-read))
                  ;; If the buffer is at capacity, no frame was consumed —
                  ;; consuming one shifts the rest down — so a partial
                  ;; frame exceeds our limit: close with 1009 to prevent a
                  ;; spin loop.
                  (t
                   (when (>= (connection-read-pos conn)
                             (length (connection-read-buf conn)))
                     (log-warn "ws buffer full, no parseable frames fd ~d"
                               (connection-fd conn))
                     (connection-append-write conn (build-ws-close 1009))
                     (setf (connection-state conn) :closing))))
                ;; The peer has sent its FIN, so every frame it will ever
                ;; send has been read, and whatever is left in the buffer is
                ;; a frame that cannot be finished. The replies to the
                ;; frames that did arrive go out first, through :CLOSING's
                ;; flush-then-close, and with none queued it closes now.
                ;; Waiting instead would spin the worker: the socket stays
                ;; readable at end of stream, so each wake-up would find the
                ;; same partial frame, re-arm and wake again at once, until
                ;; the ping sweep gave up on a peer that cannot answer.
                (when (and peer-finished
                           (eq (connection-state conn) :websocket))
                  (if (plusp (connection-write-pending conn))
                      (setf (connection-state conn) :closing)
                      (progn
                        (close-connection conn epoll-fd)
                        (return-from handle-client-read))))
                ;; One arming decision for all of them. What is pending
                ;; now decides the interest, not which branch ran: the
                ;; handler's own WS-SEND calls may have flushed everything
                ;; already, or left a remainder the event loop has to
                ;; finish. Arming EPOLLIN when nothing is pending also
                ;; re-arms the edge trigger, in case the kernel still holds
                ;; data that did not fit the read buffer earlier.
                (epoll-modify epoll-fd (connection-fd conn)
                              (logior (if (plusp (connection-write-pending conn))
                                          +epollout+
                                          +epollin+)
                                      +epollet+))))
             (:close
              (close-connection conn epoll-fd))
             ;; :continue — wait for more data
             )))
        ;; A stream watches EPOLLIN for one thing: the peer leaving.
        (:streaming
         (let ((result (connection-discard-available
                        conn (or *stream-drain-buf*
                                 (make-array 4096
                                             :element-type '(unsigned-byte 8))))))
           (case result
             ;; End of stream from the client's side. Producing into a
             ;; socket with nobody on the far end is the thing this
             ;; branch exists to stop, and noticing it needs :OK-EOF —
             ;; a peer whose last bytes and FIN land in one wake-up
             ;; would otherwise report :OK and be missed entirely.
             ((:eof :ok-eof)
              (log-debug "stream peer disconnected fd ~d" (connection-fd conn))
              (close-connection conn epoll-fd :disconnected))
             ;; The peer sent something. It cannot be answered on this
             ;; connection — a pipelined request behind a stream would
             ;; have to wait for the stream to end, and the stream may
             ;; not end — so the bytes are discarded and the connection
             ;; is marked not to be reused. A client that gets a clean
             ;; close retries; one whose request vanished silently
             ;; waits forever.
             ;;
             ;; Both verdicts mean the same thing here: the peer sent
             ;; something. :OK-WANT-WRITE adds only that the transport wants
             ;; writability before it will read again, which is the write
             ;; path's business and not this one's — the bytes have already
             ;; been discarded either way.
             ((:ok :ok-want-write)
              (log-debug "stream peer sent data mid-stream fd ~d — ~
                          will not reuse" (connection-fd conn))
              (setf (connection-close-after-p conn) t))
             ;; :AGAIN and :WANT-WRITE, neither of which read anything: a
             ;; spurious wake-up, or a transport that wants to write before
             ;; it can read. Nothing to do for either.
             (t nil))))
        ;; Closing after an early error answer: discard what the client is
        ;; still sending, and close at its FIN. See BEGIN-LINGERING-CLOSE.
        (:lingering (linger-discard conn epoll-fd))
        ;; Parked for outbound fetch — ignore reads, data stays in kernel buffer
        (:awaiting nil)
        ;; Stale EPOLLIN for a state that isn't currently reading.
        ;; Silently ignored so a late notification can't escalate
        ;; into a close via the handler-case fallback below.
        (otherwise
         (log-debug "stale EPOLLIN fd ~d in state ~a — ignoring"
                    (connection-fd conn) (connection-state conn))))
    (http-parse-error (e)
      (log-throttled :warn :parse-error "parse error fd ~d: ~a" (connection-fd conn)
                (http-parse-error-message e))
      ;; Answer before closing so the client gets a proper HTTP response.
      ;; The status comes from the condition: 400 unless the raise site
      ;; knew better (413 for a body over the cap, 414 for a request line
      ;; over its cap, 431 for headers, 501 for a method or transfer
      ;; coding we do not implement, 505 for a version we do not speak).
      ;; An application can raise one too — URL-DECODE does, on bytes
      ;; that are not UTF-8 — including after its response has begun.
      ;;
      ;; A body still being received hears :ERROR. The framework's own
      ;; refusals of one — its cap, its framing — have told it why where
      ;; they were raised, so what reaches it here is the application's.
      (notify-body-aborted conn :error)
      (answer-error-or-close conn epoll-fd (http-parse-error-status e)))
    ;; SERIOUS-CONDITION, not ERROR. CL has serious conditions that are not
    ;; errors, and SBCL signals two kinds of them into ordinary code:
    ;;
    ;;   STORAGE-CONDITION  recursion past the control stack, or one
    ;;                      allocation larger than the heap has left
    ;;   SB-EXT:TIMEOUT     the end of an SB-EXT:WITH-TIMEOUT, and
    ;;                      SB-SYS:DEADLINE-TIMEOUT, its subtype, the end of
    ;;                      an SB-SYS:WITH-DEADLINE around SBCL's own waits
    ;;                      on sockets and descriptors
    ;;
    ;; Putting a time limit on a slow call is an ordinary thing for a
    ;; handler to do, and when the slowness is an upstream's, the far end of
    ;; that call decides when the timeout fires. Each of these is a request
    ;; failing rather than the server — the frames, the allocation, the wait
    ;; are gone once the catch unwinds — and a condition nothing handles
    ;; ends the process when the debugger is disabled, every worker with it.
    ;; So each costs what an error costs: the request, and the connection.
    ;; A heap worn out by many small allocations can be out of reach of any
    ;; catch — the collector can run out of room itself, and that ends the
    ;; process before anything is signalled — so for the heap this is
    ;; containment for the request that asked too much, not a defence
    ;; against running out. SB-SYS:INTERACTIVE-INTERRUPT, the remaining one,
    ;; is delivered to the main thread and never reaches a worker.
    ;;
    ;; Every catch around application code in this framework takes
    ;; SERIOUS-CONDITION, for this reason. What it gives up is the reason
    ;; SBCL keeps the timeouts out of ERROR: a timeout armed outside one of
    ;; these catches, firing while the code inside it runs, is taken as that
    ;; code's failure. A worker has no outside — nothing in the framework
    ;; arms a timeout — and the two catches that can run on an
    ;; application's own thread are the fetch address filter and the
    ;; shutdown hooks, whose contracts (a raise refuses the address; one
    ;; hook cannot stop the rest) are the ones a condition passing through
    ;; would break.
    (serious-condition (e)
      ;; This connection's own descriptor failed under a read or a write: the
      ;; peer reset, or the network between us did. Answered the way an end of
      ;; stream is, by closing and nothing else. A 500 would be addressed to
      ;; nobody — the write carrying it fails the same way — and counting it
      ;; would put a client's disconnect into :SERVER-ERROR, a count the
      ;; census calls safe to alert on. Only this connection's descriptor: a
      ;; handler whose own I/O failed on another one has failed a request,
      ;; and the 500 below is owed to it.
      (when (and (typep e 'fd-io-error)
                 (eql (fd-io-error-fd e) (connection-fd conn)))
        (log-debug "fd ~d: ~a — the peer is gone, closing"
                   (connection-fd conn) e)
        (close-connection conn epoll-fd :disconnected)
        (return-from handle-client-read nil))
      (log-warn "error fd ~d: ~a" (connection-fd conn) e)
      ;; A body still being received hears :ERROR: its ON-DATA, or the
      ;; body handler, is what raised.
      (notify-body-aborted conn :error)
      ;; A 500 before closing, so the client gets a proper HTTP response —
      ;; unless one has already begun, which is where a stream's ON-OPEN
      ;; raising after its first send lands.
      (answer-error-or-close conn epoll-fd 500))))

;;; ---------------------------------------------------------------------------
;;; Handle writable event on a client fd
;;; ---------------------------------------------------------------------------

(defun handle-client-write (conn epoll-fd)
  "Handle EPOLLOUT on a client connection.
   Inner state dispatch is permissive for the same reason as
   HANDLE-CLIENT-READ: a stale EPOLLOUT can arrive for a state
   that isn't currently writing, and raising into the handler-case
   below would overwrite a legitimate in-flight response with a 500.

   Bumps LAST-ACTIVE on entry so a legitimate slow client (mobile
   3G pulling a 5 MiB response at 200 KB/s) is not reaped by the
   idle sweeper midway through the download. EPOLLOUT firing means
   the kernel has room in the socket buffer; treating that as
   activity is the correct semantics for a slow-pipe client, and
   dispatch-time + keep-alive-reset bumps alone would not survive
   a response that takes longer than *IDLE-TIMEOUT* to flush."
  (setf (connection-last-active conn) (get-universal-time))
  (handler-case
      (let* ((backlog (plusp (connection-write-pending conn)))
             (result (connection-on-write conn)))
        (case result
          (:done
           ;; A backlog that has emptied is the event a relay's paused
           ;; upstream is waiting for. Here rather than in the :streaming
           ;; arm below because every state that can be relayed into
           ;; reaches this point, and a resume that only worked for one of
           ;; them would be the kind of gap nobody finds until a different
           ;; response shape turns up.
           ;;
           ;; Only one that was there to empty. A wake-up that found nothing
           ;; queued drained nothing: BODY-RESUME wakes a received body
           ;; through writability, and a fetch :ON-BODY paused on that
           ;; connection is not waiting for that.
           (when backlog
             (resume-paused-outbound conn epoll-fd))
           ;; All bytes sent — next action depends on state
           (case (connection-state conn)
             (:write-response
              (if (connection-close-after-p conn)
                  ;; An early error answer lingers, so the response is
                  ;; followed by a FIN and not a reset; see
                  ;; BEGIN-LINGERING-CLOSE. Every other close is at once.
                  (if (and (connection-linger conn) (plusp *linger-timeout*))
                      (begin-lingering-close conn epoll-fd)
                      (close-connection conn epoll-fd))
                  ;; Keep-alive — reset for next request,
                  ;; preserving any pipelined bytes already buffered
                  (let* ((consumed (connection-request-end conn))
                         (buffered (connection-read-pos conn))
                         (extra (- buffered consumed)))
                    ;; First, while the connection is still this request's:
                    ;; a fetch started for it, into a stream that has
                    ;; closed, is ended with it.
                    (end-detached-fetches conn epoll-fd)
                    (when (> extra 0)
                      (replace (connection-read-buf conn)
                               (connection-read-buf conn)
                               :start1 0 :start2 consumed :end2 buffered))
                    ;; Back to the default 4 KiB when a prior large body
                    ;; grew the buffer and the shifted pipelined bytes fit
                    ;; in the default; see CONNECTION-SHRINK-READ-BUF.
                    ;; READ-POS first: the shrink reads it to know what to
                    ;; carry into the fresh buffer.
                    (setf (connection-read-pos conn) (max extra 0))
                    (connection-shrink-read-buf conn)
                    (connection-reset-write conn)
                    (setf (connection-request conn) nil
                          (connection-body-expected conn) 0
                          (connection-body-decoded conn) 0
                          ;; The pipelined bytes have moved to offset 0
                          ;; and have not been searched; see the slot.
                          (connection-header-scan-pos conn) 0
                          (connection-header-end conn) 0
                          (connection-request-end conn) 0
                          (connection-body-framing conn) :length
                          ;; What a body handler decided was this request's
                          ;; and no later one's; see the slots.
                          (connection-body-limit conn) nil
                          (connection-body-progress-at conn) 0
                          (connection-close-after-p conn) nil
                          ;; Set only beside CLOSE-AFTER-P, so never set on a
                          ;; connection kept alive, and cleared with it so
                          ;; that holds whoever sets it next.
                          (connection-linger conn) nil
                          (connection-state conn) :read-http
                          (connection-last-active conn) (get-universal-time))
                    (epoll-modify epoll-fd (connection-fd conn)
                                 (logior +epollin+ +epollet+))
                    ;; Edge-triggered: data may already be waiting from a
                    ;; pipelined request. Signal the event loop to try reading.
                    (return-from handle-client-write :keep-alive))))
             (:ws-upgrade
              ;; WebSocket handshake sent — preserve any data past the HTTP request
              (let* ((http-end (connection-request-end conn))
                     (buffered (connection-read-pos conn))
                     (extra (- buffered http-end)))
                ;; As the keep-alive reset two arms up does, so the two
                ;; agree on every field.
                (end-detached-fetches conn epoll-fd)
                (when (> extra 0)
                  (replace (connection-read-buf conn) (connection-read-buf conn)
                           :start1 0 :start2 http-end :end2 buffered))
                (connection-reset-write conn)
                (setf (connection-read-pos conn) (max extra 0)
                      (connection-request conn) nil
                      (connection-body-expected conn) 0
                      (connection-body-decoded conn) 0
                      (connection-header-scan-pos conn) 0
                      (connection-header-end conn) 0
                      (connection-request-end conn) 0
                      (connection-body-framing conn) :length
                      (connection-body-limit conn) nil
                      (connection-body-progress-at conn) 0
                      ;; Cleared for the same reason the keep-alive reset
                      ;; two arms up clears it, so the two otherwise-
                      ;; identical resets agree on every field.
                      ;; WEBSOCKET-UPGRADE-P only looks for the upgrade
                      ;; token, so `Connection: close, Upgrade` is a legal
                      ;; upgrade that would leave the flag set for the life
                      ;; of the socket. Dead state — nothing in :WEBSOCKET
                      ;; reads it — which is precisely why it would be read
                      ;; wrong the first time something does.
                      (connection-close-after-p conn) nil
                      (connection-state conn) :websocket)
                ;; A WebSocket outlives any request, so a buffer a large
                ;; upgrade request grew goes back to 4 KiB here rather
                ;; than staying grown for the socket's life. See
                ;; CONNECTION-SHRINK-READ-BUF.
                (connection-shrink-read-buf conn))
              (epoll-modify epoll-fd (connection-fd conn)
                           (logior +epollin+ +epollet+))
              ;; Edge-triggered: buffered frame data from the upgrade
              ;; request is in user-space, not the kernel. MOD won't
              ;; re-fire. Signal the event loop to read immediately.
              (return-from handle-client-write :keep-alive))
             (:websocket
              ;; WebSocket frame response sent — back to reading
              (epoll-modify epoll-fd (connection-fd conn)
                           (logior +epollin+ +epollet+)))
             (:streaming
              ;; Backlog drained; the stream stays open. Drop EPOLLOUT
              ;; and keep EPOLLIN, which is the only event a stream with
              ;; nothing queued has any use for. STREAM-FLUSH arms it
              ;; again the moment a send fails to complete.
              (epoll-modify epoll-fd (connection-fd conn)
                            (logior +epollin+ +epollet+)))
             (:closing
              ;; Close frame sent — disconnect
              (close-connection conn epoll-fd))
             (:sending-100-continue
              ;; 100 Continue flushed — switch to reading the body.
              ;; Returning :keep-alive tells the main loop to re-enter
              ;; handle-client-read immediately in case the body is
              ;; already buffered (edge-triggered epoll won't re-fire
              ;; on user-space bytes).
              (connection-reset-write conn)
              (setf (connection-state conn) :read-body)
              (epoll-modify epoll-fd (connection-fd conn)
                            (logior +epollin+ +epollet+))
              (return-from handle-client-write :keep-alive))
             ;; A body being received yielded with more in the kernel: the
             ;; writability it armed is the wake-up. Back to reading only,
             ;; and :KEEP-ALIVE reads at once, as it does after the 100
             ;; Continue above. A buffered :READ-BODY has nothing to read on
             ;; an EPOLLOUT and is let fall to the stale arm instead.
             (:read-body
              (if (and (connection-body-receiver conn)
                       ;; A pause taken after the yield, in the same batch,
                       ;; is not ended by the yield's wake-up.
                       (not (connection-body-paused conn)))
                  (progn
                    (epoll-modify epoll-fd (connection-fd conn)
                                  (logior +epollin+ +epollet+))
                    (return-from handle-client-write :keep-alive))
                  (log-debug "stale EPOLLOUT :done fd ~d in state ~a — ignoring"
                             (connection-fd conn) (connection-state conn))))
             ;; Stale EPOLLOUT for a state that isn't currently
             ;; writing (e.g. a race where read-side completed
             ;; earlier in the same batch and flipped the state
             ;; before this event was dispatched). Silently ignore.
             (otherwise
              (log-debug "stale EPOLLOUT :done fd ~d in state ~a — ignoring"
                         (connection-fd conn) (connection-state conn)))))
          ;; :continue — more bytes to write
          (:continue nil)))
    (error (e)
      ;; A failure on this connection's own descriptor is its peer going
      ;; away, a reset mid-download that any client can cause at will.
      ;; HANDLE-CLIENT-READ reads the same fact the same way: DEBUG, and a
      ;; stream hears :DISCONNECTED. Logged at ERROR, every client that
      ;; closed early would be one line in the log nobody can act on. Any
      ;; other failure here is the server's own, and stays an ERROR.
      (if (and (typep e 'fd-io-error)
               (eql (fd-io-error-fd e) (connection-fd conn)))
          (progn
            (log-debug "fd ~d: ~a — the peer is gone, closing"
                       (connection-fd conn) e)
            (close-connection conn epoll-fd :disconnected))
          (progn
            (log-error "write error fd ~d: ~a" (connection-fd conn) e)
            (close-connection conn epoll-fd))))))

;;; ---------------------------------------------------------------------------
;;; Event loop
;;; ---------------------------------------------------------------------------

(defun dispatch-connection-event (fd flags owner epoll-fd handler ws-handler)
  "Apply one epoll event to the connection on FD, if that is still OWNER:
   the connection FD belonged to when the batch was taken.

   A batch is read from epoll before any of it is handled, and handling
   one event can close a connection that has an event later in the same
   batch. CLOSE-CONNECTION takes an :AWAITING inbound's outbound down with
   it, and a detached fetch's orphans. The closed fd is then the lowest
   free number, so an accept later in the batch, or a handler's fetch, is
   handed it, and the old connection's event would land on the new one:
   an EPOLLHUP closing a client that has sent nothing, or stale flags run
   against a fresh outbound. Comparing against the owner drops such an
   event. A connection opened during the batch has no event in it, so an
   fd that had no owner has nothing to apply either.

   Within one event the connection is asked for again before each later
   step, for the same reason: reading can close it, and the write and
   hang-up steps are its own only while it is still on the table."
  (let ((conn (lookup-connection fd)))
    (when (and conn (eq conn owner))
      (if (connection-outbound-p conn)
          (handle-outbound-event conn epoll-fd flags)
          ;; Inbound. EPOLLIN before EPOLLHUP, so a fire-and-close HTTP/1.0
          ;; client — it sends its request and FINs at once — has the
          ;; request processed instead of losing it to the hang-up arm.
          (flet ((still-here-p () (eq (lookup-connection fd) conn)))
            (when (logtest flags +epollin+)
              (handle-client-read conn epoll-fd handler ws-handler))
            (when (and (logtest flags +epollout+) (still-here-p))
              (when (and (eq (handle-client-write conn epoll-fd) :keep-alive)
                         (still-here-p))
                ;; Keep-alive reset: read at once, in case the next request
                ;; is already buffered, since edge-triggered epoll will not
                ;; report it again.
                (handle-client-read conn epoll-fd handler ws-handler)))
            ;; Error or hang-up, unless the read or write path already tore
            ;; the connection down. Either is the peer gone, which is what a
            ;; stream's ON-CLOSE and a received body's ON-ABORT are told. It
            ;; is also the one event a paused body still gets, subscribed as
            ;; it is to nothing, so a client that resets mid-pause is heard.
            (when (and (or (logtest flags +epollerr+)
                           (logtest flags +epollhup+))
                       (still-here-p))
              (close-connection conn epoll-fd :disconnected)))))))

(defun run-event-loop (listener-socket epoll-fd handler ws-handler on-tick)
  "Main event loop. Runs until WORKER-STOPPING-P: *SHUTDOWN* is set, or a
   later START-SERVER has begun. Either way it drains first, so a response
   a handler finished after its run was told to stop still goes out."
  (let ((listener-fd    (socket-fd listener-socket))
        (last-ping-time  (get-universal-time))
        (last-sweep-time (get-universal-time))
        ;; T while an accept has failed with the backlog possibly not
        ;; drained; the maintenance tick retries. See ACCEPT-PENDING.
        (accept-retry    nil)
        (event-buf (make-epoll-event-buf +max-events+))
        ;; The connection each event in a batch was reported for, taken
        ;; before any of them is handled. See DISPATCH-CONNECTION-EVENT.
        (owners (make-array +max-events+ :initial-element nil)))
    (loop
      (when (worker-stopping-p)
        (drain-connections listener-socket epoll-fd event-buf)
        (return))
      (let ((n (epoll-wait epoll-fd event-buf +max-events+
                           (max 10 (round (* *worker-wake-interval* 1000))))))
        (dotimes (i n)
          (setf (svref owners i)
                (lookup-connection (epoll-event-fd event-buf i))))
        (dotimes (i n)
          (let ((fd    (epoll-event-fd event-buf i))
                (flags (epoll-event-flags event-buf i)))
            (if (= fd listener-fd)
                ;; Edge-triggered: accept until none pending, or until an
                ;; accept fails and the tick has to retry.
                (setf accept-retry (accept-pending listener-socket epoll-fd))
                (dispatch-connection-event fd flags (svref owners i)
                                           epoll-fd handler ws-handler))))
        ;; Held past the batch, a closed connection would wait here for the
        ;; next batch as large before it could be collected.
        (fill owners nil :end n))
      ;; Periodic maintenance — both scans gated on elapsed wall
      ;; clock so a busy epoll loop doesn't walk the connection
      ;; table multiple times per second. 1 s is fine: idle
      ;; timeouts are measured in 10 s+ units, so sub-second sweep
      ;; granularity is pure waste.
      (let ((now (get-universal-time)))
        (when (>= (- now last-sweep-time) 1)
          (sweep-idle-connections epoll-fd now)
          ;; Rides the same 1 s gate rather than taking a third timer.
          ;; The interval is per connection and measured from that
          ;; connection's own last activity, so this needs to be asked
          ;; often enough, not on a schedule of its own — and a second
          ;; walk of the table per second would be the waste the gate
          ;; above exists to prevent.
          (keepalive-streams epoll-fd now)
          ;; Rides the same 1 s gate for the same reason KEEPALIVE-STREAMS
          ;; does — a third timer to walk the table a third time per second
          ;; is the waste the gate exists to prevent.
          (publish-connection-census)
          ;; And so does closing the getent children that have exited
          ;; since their lookups ended. A second is soon enough for a
          ;; process that is only waiting to be collected.
          (reap-dns-children)
          ;; And so does retrying a backlog an accept failure left
          ;; queued: no edge will report it, and a second is soon
          ;; enough for a server that has run out of descriptors.
          (when accept-retry
            (setf accept-retry (accept-pending listener-socket epoll-fd)))
          (setf last-sweep-time now))
        (when (>= (- now last-ping-time) *ws-ping-interval*)
          (ping-ws-connections epoll-fd)
          (setf last-ping-time now)))
      ;; ON-TICK runs last: after this pass's I/O, and after the sweep
      ;; above, so a hook sees a table the sweeper has already walked and
      ;; a census it has already published. It never runs on connections
      ;; that are draining — the *SHUTDOWN* check at the top of the loop
      ;; returns through DRAIN-CONNECTIONS before reaching here — so an
      ;; application cannot queue new work onto them. A pass that was
      ;; already under way when shutdown was requested still gets here
      ;; once, before that check sees the flag; the drain comes after it.
      ;;
      ;; Neither ordering is asserted by a test. Nothing a hook can observe
      ;; says when the loop noticed *SHUTDOWN*, and the flag is set from
      ;; another thread, so a test of "not after shutdown" would fail on the
      ;; in-flight pass above whenever the timing landed there. They hold by
      ;; the order of the forms in this loop, and this comment is where that
      ;; order is written down.
      (when on-tick
        (handler-case (funcall on-tick *worker-id*)
          ;; SERIOUS-CONDITION, not ERROR: see HANDLE-CLIENT-READ's last catch.
          (serious-condition (e)
            ;; A hook that raises every pass would log at the wake rate —
            ;; twenty lines a second per worker at the cadence a fan-out
            ;; demo wants — and every one of them takes *LOG-LOCK*, which
            ;; log.lisp documents as the one lock every worker contends
            ;; for. Silence would be worse: a hook that never runs looks
            ;; exactly like a hook that was never installed. So the first
            ;; is reported at once, and then at most one a minute with a
            ;; count of the rest.
            (log-throttled :error :on-tick ":on-tick raised on worker ~d: ~a"
                           *worker-id* e)))))))

;;; ---------------------------------------------------------------------------
;;; Worker
;;; ---------------------------------------------------------------------------

(defun run-worker (host port worker-id handler ws-handler on-tick
                   body-handler &optional listener)
  "Run a single worker: own listener, own epoll fd, own connections.
   Automatically restarts on unhandled errors (with backoff).

   BODY-HANDLER is bound as *BODY-HANDLER* for the worker's life rather
   than passed down the event loop beside HANDLER; that variable says
   why.

   LISTENER, when supplied, is an already-bound socket this worker
   adopts instead of binding one of its own. START-SERVER hands one to
   worker 0 when :PORT was 0, so that the kernel-assigned port is held
   by a real listener from the first instant it is knowable — see
   START-SERVER for why closing it and rebinding is not survivable.

   Adopted for the first pass only. A restart after a crash binds
   normally, and lands on the right port because by then PORT is the
   concrete number START-SERVER resolved — so the restarted worker
   rejoins its siblings rather than appearing somewhere new.

   The epoll fd is logged at startup because, should a worker fail with
   EBADF on its own, the number is what tells a descriptor closed
   underneath it from a wrong one arriving. An epoll fd is a bare number
   with nothing owning it, which makes a worker the ideal victim of a
   stray close: a socket closed with %CLOSE leaves SB-BSD-SOCKETS'
   finalizer armed on a number the kernel reissues, and a later GC closes
   it under its new owner. CONNECTION-CLOSE says how it avoids that."
  (loop
    (handler-case
        (with-worker-urandom
        (let ((*connections* (make-hash-table :test #'eql))
              ;; This worker's census slot. Bound here beside the other
              ;; share-nothing slots, because owning the slot outright is
              ;; what lets the publish be lock-free.
              (*worker-id* worker-id)
              (*body-handler* body-handler)
              ;; *CONNECTION-SERIAL* is not bound here. Everything in this
              ;; LET a crash may throw away and begin again; a serial may
              ;; not, so it is bound by the thread START-SERVER spawns,
              ;; outside this loop. See the comment there.
              ;; Per-worker DNS cache. Workers share nothing in the hot
              ;; path, so each keeps its own table and no lock is needed.
              ;; Inert unless the app opts in via *DNS-CACHE-TTL*; a
              ;; worker restart drops its cache, which is harmless — the
              ;; next fetch to each host pays one getent again.
              (*dns-cache* (make-hash-table :test #'equal))
              ;; getent children whose lookups are over and whose exits
              ;; have not been seen yet. Per worker for the same reason as
              ;; the cache. A restart drops only this record of them: a
              ;; child that was never closed is still on SBCL's list of
              ;; active processes, which is the list SBCL collects from.
              (*dns-children* (make-hash-table :test #'eq))
              (*epoll-ctl-buf* (make-array +epoll-event-size+
                                           :element-type '(unsigned-byte 8)))
              (*poll-buf* (make-array 8 :element-type '(unsigned-byte 8)))
              (*refusal-drain-buf*
                (make-array +refusal-drain-size+
                            :element-type '(unsigned-byte 8)))
              (*log-throttle* (make-hash-table :test #'eq))
              (*stream-drain-buf*
                (make-array 4096 :element-type '(unsigned-byte 8)))
              ;; CAR is the second the string was built for. 0 can never
              ;; be the current universal time, so the first response of
              ;; the worker's life formats and the rest of that second
              ;; read.
              (*http-date-cache* (cons 0 ""))
              ;; Same shape, different consumer: the pre-built static
              ;; responses want the date as a finished header line rather
              ;; than a string to hand a serializer.
              (*http-date-line-cache*
                (cons 0 (make-array 0 :element-type '(unsigned-byte 8)))))
          ;; Split the listener and epoll-fd bindings so a failure of
          ;; EPOLL-CREATE (EMFILE, ENOMEM) still tears down the bound
          ;; listener socket — a shared let* would leak it because the
          ;; cleanup form references EPOLL-FD which is never bound on
          ;; that path.
          ;; SHIFTF rather than a plain read: an adopted listener belongs
          ;; to this worker's first pass only. Reusing it after a restart
          ;; would hand the new event loop a socket the crashed pass
          ;; already closed in its own cleanup below.
          (let ((listener (or (shiftf listener nil)
                              (make-tcp-listener host port))))
            (unwind-protect
                 (let* ((epoll-fd (epoll-create))
                        ;; Bound per worker alongside the other share-nothing
                        ;; slots, so app-facing writers can arm EPOLLOUT
                        ;; without an epoll fd being threaded through an
                        ;; exported signature. STREAM-SEND may be called from
                        ;; a fetch callback or a producer the read branch
                        ;; never returns through, and a remainder left queued
                        ;; with nothing armed waits for an event that is not
                        ;; coming.
                        (*epoll-fd* epoll-fd)
                        ;; Same reasoning, one slot over: counted on this
                        ;; thread only, so an increment needs no lock and
                        ;; cannot contend. A restart after a crash starts a
                        ;; fresh set rather than resuming the dead worker's,
                        ;; which is honest — the counts describe a worker, and
                        ;; this is a new one.
                        (*counters* (make-counters))
                        ;; Set once the event loop returns, which it does
                        ;; only after a drain. Still NIL in the cleanup below
                        ;; means a condition is unwinding through it.
                        (loop-returned nil))
                   (log-info "worker ~d started (epoll fd ~d)"
                             worker-id epoll-fd)
                   (epoll-add epoll-fd (socket-fd listener)
                              (logior +epollin+ +epollet+))
                   (unwind-protect
                       (progn
                         (run-event-loop listener epoll-fd handler ws-handler
                                         on-tick)
                         (setf loop-returned t))
                     ;; Cleanup on worker crash or normal exit. Split the
                     ;; table into outbounds and everything else — outbounds
                     ;; go through CLOSE-OUTBOUND so their fetch callbacks
                     ;; fire even when the worker dies with requests in
                     ;; flight, matching the contract that every fetch's
                     ;; :then closure runs exactly once. Non-outbounds get
                     ;; the raw CONNECTION-CLOSE; we can't deliver anything
                     ;; to their clients at this point and the epoll fd is
                     ;; about to be torn down anyway.
                     ;;
                     ;; A stream's ON-CLOSE carries the same contract —
                     ;; exactly once, however the stream ends — and fires
                     ;; here first: :ERROR when the loop was unwound by a
                     ;; crash, :SHUTDOWN when it returned after a drain that
                     ;; ran out of time. Skipping it would leave every
                     ;; stream on a crashed worker registered wherever the
                     ;; application put it, for good. NOTIFY-STREAM-CLOSED
                     ;; absorbs a raising callback, so one cannot stop the
                     ;; closes after it.
                     ;;
                     ;; Both kinds are collected before anything is closed
                     ;; or any callback runs: nothing may mutate the table
                     ;; MAPHASH is walking, and a callback is application
                     ;; code that may close a connection of its own.
                     (let ((outbounds nil)
                           (inbounds nil)
                           (reason (if loop-returned :shutdown :error)))
                       (maphash (lambda (fd conn)
                                  (declare (ignore fd))
                                  (if (connection-outbound-p conn)
                                      (push conn outbounds)
                                      (push conn inbounds)))
                                *connections*)
                       (dolist (conn inbounds)
                         (notify-stream-closed conn reason)
                         (notify-body-aborted conn reason)
                         (maybe-reap-dns-process conn)
                         (connection-close conn))
                       (dolist (conn outbounds)
                         (close-outbound conn epoll-fd)))
                     ;; One last pass over the children those closes and the
                     ;; worker's life parked. Any still running is left on
                     ;; SBCL's own list rather than waited for here, where a
                     ;; child that would not die would hold the worker's
                     ;; exit.
                     (reap-dns-children)
                     ;; Drop this worker's published counts on the way out,
                     ;; or a torn down server leaves totals a later reader
                     ;; would sum into a fresh one's.
                     (clear-connection-census)
                     (%close epoll-fd)))
              ;; Runs whether EPOLL-CREATE succeeded or raised.
              (sb-bsd-sockets:socket-close listener)))
          ;; Normal exit (shutdown requested)
          (log-info "worker ~d stopped" worker-id)
          (return)))
      ;; SERIOUS-CONDITION, not ERROR: see HANDLE-CLIENT-READ's last catch.
      ;; Here it is the backstop for one raised outside every narrower
      ;; catch, where a restart costs this worker's connections and an
      ;; uncaught condition costs the process.
      (serious-condition (e)
        (log-error "worker ~d crashed: ~a — restarting" worker-id e)
        ;; 1-second backoff, sliced into *worker-wake-interval*
        ;; chunks so a SIGTERM arriving during the backoff is noticed
        ;; within one slice rather than after the full second.
        (let ((until (+ (get-internal-real-time)
                        internal-time-units-per-second)))
          (loop until (or (worker-stopping-p)
                          (>= (get-internal-real-time) until))
                do (sleep *worker-wake-interval*)))
        (when (worker-stopping-p) (return))))))

;;; ---------------------------------------------------------------------------
;;; CPU count
;;;
;;; How many workers the machine can actually run, which inside a container
;;; is not how many CPUs the machine has. /sys/devices/system/cpu/online is
;;; the host's topology and a container sees all of it, so read alone it
;;; would give a process limited to half a core on a 16-core host sixteen
;;; event loops, sixteen epoll instances and sixteen listener sockets to
;;; time-slice half a core between them — each running its own maintenance
;;; sweep every second.
;;;
;;; The answer nearest the truth is asked for first:
;;;
;;;   cgroup v2   /sys/fs/cgroup/cpu.max          "QUOTA PERIOD" or "max PERIOD"
;;;   cgroup v1   cpu.cfs_quota_us / cfs_period_us   quota -1 means unlimited
;;;   affinity    sched_getaffinity(2)               cpuset, and nothing else
;;;   topology    /sys/devices/system/cpu/online     the host's cores
;;;
;;; Each source answers NIL when it has nothing to say — no file, an unlimited
;;; quota, a shape it cannot parse — and the smallest of what remains wins.
;;;
;;; The smallest, not the first, because the sources answer different
;;; questions. A quota says how much CPU time may be consumed; an affinity
;;; mask says how many CPUs it may be consumed on. Usable parallelism is the
;;; lesser of the two, and `--cpus=8 --cpuset-cpus=0,1` is reachable: reading
;;; the quota first spawns eight event loops onto two CPUs, which is a smaller
;;; instance of the defect this function exists to fix. Taking the first
;;; answer is correct only when every other source is silent, which makes it
;;; a special case of taking the least.
;;;
;;; One cost, stated rather than discovered: a defective parser can cap a
;;; large host where first-answer-wins would skip it. That error lands
;;; on the wasteful side rather than the over-subscribed one, which is the
;;; direction to fail in, and each parser carries its own assertions.
;;;
;;; The parsing is
;;; split from the reading because a machine that exposes cgroup files cannot
;;; be relied on to exist: the tests drive the parsers with synthetic contents
;;; so every branch is exercised deliberately, including the degradation path
;;; that is all a cgroup-less host would ever reach.
;;; ---------------------------------------------------------------------------

(defun %read-first-line (path)
  "First line of PATH, or NIL if it cannot be read."
  (handler-case
      (with-open-file (s path :if-does-not-exist nil)
        (when s (read-line s nil nil)))
    (error () nil)))

(defun quota-to-workers (quota period)
  "Workers implied by a CPU quota of QUOTA per PERIOD, or NIL if either is
   unusable.

   CEILING, never FLOOR or ROUND. A 0.5-CPU container — `cpus: '0.5'`, the
   shape this whole function exists for — floors to 0 workers, and
   START-SERVER refuses a non-positive :WORKERS, so flooring would give a
   server that will not boot in exactly the deployment this exists to
   serve. A fractional share still needs one worker to run on."
  (when (and (integerp quota) (integerp period)
             (plusp quota) (plusp period))
    (max 1 (ceiling quota period))))

(defun parse-cpu-max (line)
  "Workers implied by a cgroup v2 cpu.max line, or NIL.

   The format is \"QUOTA PERIOD\". QUOTA is the literal string `max` when no
   limit is set, which is not a number and must fall through to the next step
   rather than being parsed — PARSE-INTEGER on it raises, and a parser that
   guessed would answer with whatever garbage it made of the word."
  (when line
    (let* ((line (string-trim '(#\Space #\Tab #\Return) line))
           (sp (position #\Space line)))
      (when sp
        (let ((quota (ignore-errors (parse-integer line :end sp)))
              (period (ignore-errors (parse-integer line :start (1+ sp)))))
          (quota-to-workers quota period))))))

(defun parse-cfs-quota (quota-line period-line)
  "Workers implied by a cgroup v1 cpu.cfs_quota_us / cpu.cfs_period_us pair,
   or NIL. A quota of -1 means unlimited and falls through; QUOTA-TO-WORKERS
   refuses it along with every other non-positive value."
  (when (and quota-line period-line)
    (quota-to-workers
     (ignore-errors (parse-integer (string-trim '(#\Space #\Tab #\Return)
                                                quota-line)))
     (ignore-errors (parse-integer (string-trim '(#\Space #\Tab #\Return)
                                                period-line))))))

(defun parse-cpu-list (line)
  "Count the CPUs named by a kernel CPU-list string, or NIL.

   Handles the simple '0-N' shape and the multi-range 'A-B,C,D-E' shape that
   hotplugged or heterogeneous topologies produce — Intel E-cores offline, a
   VM with a non-contiguous mask. A parser that reads one dash and one
   integer falls back to 1 on any comma, wasting cores on exactly the
   machines where parallelism matters most."
  (when line
    (handler-case
        (loop with total = 0
              with start = 0
              with len = (length line)
              while (< start len)
              do (let* ((comma (or (position #\, line :start start) len))
                        (dash  (position #\- line :start start :end comma)))
                   (if dash
                       (let ((lo (parse-integer line :start start :end dash))
                             (hi (parse-integer line :start (1+ dash)
                                                     :end comma)))
                         (incf total (1+ (- hi lo))))
                       (progn
                         ;; Single-CPU token — still parse to validate.
                         (parse-integer line :start start :end comma)
                         (incf total)))
                   (setf start (1+ comma)))
              finally (return (when (plusp total) total)))
      (error () nil))))

(sb-alien:define-alien-routine ("sched_getaffinity" %sched-getaffinity)
    sb-alien:int
  (pid sb-alien:int)
  (cpusetsize sb-alien:unsigned-long)
  (mask (sb-alien:* t)))

(defconstant +cpu-set-bytes+ 128
  "sizeof(cpu_set_t) — 1024 bits, the kernel's fixed default.")

(defun count-set-bits (buf)
  "Number of 1 bits across BUF. The CPUs this process may run on, once
   sched_getaffinity has filled a cpu_set_t."
  (loop for b across buf sum (logcount b)))

(defun affinity-cpu-count ()
  "CPUs this process is permitted to run on, or NIL if the call fails.

   Answers what a cpuset restricts and a quota does not. A container given
   --cpuset-cpus rather than --cpus has no quota to read, so without this the
   chain falls all the way through to the host's topology."
  (handler-case
      (let ((buf (make-array +cpu-set-bytes+ :element-type '(unsigned-byte 8)
                                             :initial-element 0)))
        (sb-sys:with-pinned-objects (buf)
          (when (zerop (%sched-getaffinity 0 +cpu-set-bytes+
                                           (sb-sys:vector-sap buf)))
            (let ((n (count-set-bits buf)))
              (when (plusp n) n)))))
    (error () nil)))

(defun fewest-cpus (&rest counts)
  "The smallest positive count among COUNTS, ignoring NILs, or NIL when
   every source declined.

   Split out from CPU-COUNT because it is the only part of the decision that
   can be tested without a container: the sources are files and a syscall,
   and no host either side of this code can be made to disagree with itself
   on demand."
  (let ((usable (remove-if-not (lambda (n) (and (integerp n) (plusp n)))
                               counts)))
    (when usable (reduce #'min usable))))

(defun cpu-count ()
  "Workers this process can usefully run. See the section banner for the
   sources and why the least of them wins."
  (or (fewest-cpus
       (parse-cpu-max (%read-first-line "/sys/fs/cgroup/cpu.max"))
       (parse-cfs-quota
        (%read-first-line "/sys/fs/cgroup/cpu/cpu.cfs_quota_us")
        (%read-first-line "/sys/fs/cgroup/cpu/cpu.cfs_period_us"))
       (affinity-cpu-count)
       (parse-cpu-list (%read-first-line "/sys/devices/system/cpu/online")))
      (progn
        (log-warn "cpu-count: no CPU information available, ~
                   defaulting to 1 worker")
        1)))

;;; ---------------------------------------------------------------------------
;;; Server entry point
;;; ---------------------------------------------------------------------------

(defun start-server (&key (host #(127 0 0 1)) (port 8081) (workers (cpu-count))
                          handler ws-handler body-handler on-listen on-tick)
  "Start the server with WORKERS event loops on HOST:PORT.
   HOST is a 4-byte IPv4 vector or a 16-byte IPv6 vector, and
   MAKE-TCP-LISTENER dispatches the socket family on its length. Default
   #(127 0 0 1) is IPv4 loopback only; #(0 0 0 0) is all IPv4 interfaces,
   and the sixteen-byte forms are the v6 counterparts — README's
   Configuration section spells all four out. Said here because a reader who
   stops at this docstring would otherwise conclude the framework is
   IPv4-only.
   HANDLER: function (request) -> response or :UPGRADE.
   WS-HANDLER: function (connection frame) -> bytes or NIL.
   BODY-HANDLER: function (request) -> NIL, a body plan, or an
     HTTP-RESPONSE; a symbol naming one; or NIL. Called once the head of
     a request with a body has arrived, before any of the body is read —
     see src/server/body.lisp, and src/server/multipart.lisp for a form
     received as it arrives.
   ON-TICK: function (worker-id), a symbol naming one, or NIL.
   Each worker gets its own listener socket (SO_REUSEPORT), epoll fd,
   and connection table. Ctrl-C shuts down all workers.

   ON-TICK runs on every pass of every worker's event loop, on that
   worker's own thread, with that worker's connection table and epoll fd
   bound. The bindings are the point: the hook runs on the thread that
   owns this worker's connections, so writing to those from it is legal.
   It does not make another worker's connections writable. WS-SEND,
   STREAM-CLOSE and FETCH-INTO check, and refuse a connection this worker
   does not own; STREAM-SEND does not check (README, under Limitations), so
   a hook that walks a registry shared across workers and calls it on each
   entry is exactly where a cross-worker write goes through without a word.
   It is how an application does periodic per-worker work — rotating
   counters it keeps itself, or writing to the connections this worker
   owns, which is the only way to reach them, since no worker may touch
   another's table.

   Three things a hook has to be.

   Cheap. It runs inside the loop, so one that blocks stops that worker
   for every connection on it. There is no timeout and no watchdog: the
   loop cannot preempt it, and a hook that hangs is indistinguishable
   from a worker that died.

   Ready to run often. The interval is bounded below by
   *WORKER-WAKE-INTERVAL* only while the worker is idle, and by nothing
   at all while it is busy — a loaded worker may tick thousands of times
   a second. Work that should happen on a schedule needs its own gate,
   the way the idle sweep gates itself at one second.

   Allowed to raise. A raise is caught, the worker continues, and the
   error is logged once and then at most once a minute with a count —
   enough that a broken hook is never silent, bounded so that one
   raising every pass cannot flood *LOG-LOCK*.

   It does not run while a worker drains for shutdown. That is the precise
   claim, and it is narrower than never running after shutdown: shutdown is requested
   from another thread and a worker notices at the top of its next pass, so a
   pass already under way — waiting in epoll_wait, say — finishes, hook
   included, before the drain begins. The hook can run once after shutdown is
   requested; it never runs on connections that are draining.

   PORT may be 0, in which case the kernel assigns one and ON-LISTEN —
   a function of one argument, called once, on the calling thread, after
   every worker thread has been spawned — receives the port actually
   bound. It is called for a fixed port too, so a caller need not know
   which kind it asked for. This function does not return until
   shutdown, so a callback is the only way to answer the question.

   A caller that binds port 0 itself, closes, and passes the number here
   has a race. Every listener sets SO_REUSEPORT, so a second process
   handed that number in the gap does not fail its bind: both hold the
   port and the kernel splits traffic between them, silently, with
   nothing in either log. Resolving here means the port is never free
   between being chosen and being served."
  (unless (and (integerp workers) (plusp workers))
    (error "start-server: :workers must be a positive integer, got ~a"
           workers))
  ;; WS-SEND checks this too, but only WS-SEND does, and a handler that
  ;; returns a frame instead of pushing one never goes through it — that
  ;; path appends directly. A zero here would therefore leave the primary
  ;; documented shape with no time bound on an undrained queue at all,
  ;; while three docs promise there is no setting that disables it.
  (unless (and (realp *write-stall-timeout*) (plusp *write-stall-timeout*))
    (error "start-server: *write-stall-timeout* is ~s; it must be positive. ~
            It is the only deadline on a write queue the peer may never ~
            drain — the idle timeouts are measured on last activity, which ~
            a peer that stops reading keeps fresh by continuing to send."
           *write-stall-timeout*))
  ;; The docstring and two documents state this as a requirement and
  ;; nothing enforced it. Both limits are exported and tunable apart —
  ;; which is what makes it a requirement rather than an identity to lean
  ;; on — and the shape it protects is the obvious one: a deployment
  ;; trimming memory lowers the backlog, the default MiB of headroom
  ;; disappears, and an echo handler is handed a message it is then
  ;; refused permission to return. Checked at startup rather than at send
  ;; time because a misconfiguration should not wait for the first
  ;; maximal message to reveal itself. The message size is checked by
  ;; name first, since the comparison reads it.
  (unless (and (integerp *max-ws-message-size*) (plusp *max-ws-message-size*))
    (error "start-server: *max-ws-message-size* is ~s; it must be a positive ~
            integer."
           *max-ws-message-size*))
  (unless (and (integerp *max-write-backlog*)
               (>= *max-write-backlog* (+ *max-ws-message-size* 10)))
    (error "start-server: *max-write-backlog* is ~s and ~
            *max-ws-message-size* is ~s. The backlog must clear the ~
            message size by at least 10 bytes — the largest header ~
            BUILD-WS-FRAME emits — or a maximal legal WebSocket message ~
            cannot be sent even onto an empty queue. Raise the backlog to ~
            at least ~s, or lower *max-ws-message-size*."
           *max-write-backlog* *max-ws-message-size*
           (+ *max-ws-message-size* 10)))
  ;; Third invariant, same place and the same argument. *FETCH-TIMEOUT* is
  ;; the documented floor under every way a fetch can fail to return — the
  ;; DNS phase, the connect, the read, and the :AWAITING sweep that answers
  ;; a parked caller 504 when none of them finish. At zero the sweep never
  ;; fires, so a parked inbound waits on an upstream that may never speak
  ;; and the only remaining bound is the idle timeout, which the fetch
  ;; keeps fresh. Checked at boot rather than at the first parked request,
  ;; for the reason the two above give: a misconfiguration should not wait
  ;; for the shape that reveals it. Whole seconds, since the fetch path adds
  ;; it to GET-UNIVERSAL-TIME; the eleventh says why that rules out a
  ;; fraction.
  (unless (and (integerp *fetch-timeout*) (plusp *fetch-timeout*))
    (error "start-server: *fetch-timeout* is ~s; it must be a positive whole ~
            number of seconds. It is the deadline on every phase of an ~
            outbound fetch and the only thing that answers a parked caller ~
            when an upstream goes quiet — there is no setting that disables ~
            it."
           *fetch-timeout*))
  ;; Fourth invariant, same place and the same argument as the three
  ;; above. A non-function :ON-TICK raises on the first pass of every
  ;; worker's loop and on every pass after it — which is precisely the
  ;; shape the rate limit in RUN-EVENT-LOOP exists to survive, arrived at
  ;; through a typo rather than a bug. Refusing at boot costs one check
  ;; and turns a log full of suppressed occurrences into one clear error
  ;; before a socket is ever bound.
  ;;
  ;; A symbol naming a function is accepted, as :HANDLER accepts one: FUNCALL
  ;; looks it up on every call, which is what lets a running server pick up a
  ;; hook redefined at the REPL. A symbol naming nothing is still refused, and
  ;; so is one naming a macro or a special operator, since neither can be
  ;; funcalled and each would fail every pass exactly as a typo would.
  (unless (or (null on-tick)
              (functionp on-tick)
              (and (symbolp on-tick)
                   (fboundp on-tick)
                   (not (macro-function on-tick))
                   (not (special-operator-p on-tick))))
    (error "start-server: :on-tick is ~s; it must be a function of one ~
            argument (the worker id), a symbol naming one, or NIL. It runs ~
            on every pass of every worker's event loop, so a value that ~
            cannot be funcalled fails on every pass rather than once."
           on-tick))
  ;; Fifth, for the same reason. MATCH-PATH reads *MATCH-PATH-LITERALS* on
  ;; every call, so a typo there would fail every routed request with a
  ;; 500 instead of failing once, here.
  (unless (member *match-path-literals* '(:exact :decoded))
    (error "start-server: *match-path-literals* is ~s; it must be :EXACT ~
            (a route's literals match a path as it arrived) or :DECODED ~
            (they match it percent-decoded)."
           *match-path-literals*))
  ;; Sixth, :ON-TICK's rule and its reason: a body handler that cannot be
  ;; funcalled would fail every request with a body, each with a 500.
  (unless (or (null body-handler)
              (functionp body-handler)
              (and (symbolp body-handler)
                   (fboundp body-handler)
                   (not (macro-function body-handler))
                   (not (special-operator-p body-handler))))
    (error "start-server: :body-handler is ~s; it must be a function of one ~
            argument (the request, without its body), a symbol naming one, ~
            or NIL."
           body-handler))
  ;; Seventh, *WRITE-STALL-TIMEOUT*'s argument: once a body handler has
  ;; accepted a body, this is the only time bound on it, and at zero a
  ;; client that stops sending would hold its connection for good.
  (unless (and (realp *body-stall-timeout*) (plusp *body-stall-timeout*))
    (error "start-server: *body-stall-timeout* is ~s; it must be positive. ~
            It is the only deadline on a request body a body handler has ~
            accepted — such a body is timed on its progress, not by ~
            *idle-timeout*."
           *body-stall-timeout*))
  ;; Eighth. A level LOG-LEVEL-VALUE does not know reads as the lowest, so
  ;; a typo such as :WARNING would turn on every DEBUG line, unannounced.
  (unless (member *log-level* *log-levels*)
    (error "start-server: *log-level* is ~s; it must be one of ~{~s~^, ~}."
           *log-level* *log-levels*))
  ;; Ninth. Read by each worker as it binds, so a bad value would fail
  ;; every worker's listener rather than the start.
  (unless (and (integerp *listen-backlog*) (plusp *listen-backlog*))
    (error "start-server: *listen-backlog* is ~s; it must be a positive ~
            integer (the kernel clamps it to net.core.somaxconn)."
           *listen-backlog*))
  ;; Tenth. Read when an upstream's head arrives, so a bad value would fail
  ;; every fetch with a 502 that says nothing about the setting.
  (dolist (name '(*max-upstream-header-count*
                  *max-upstream-header-line-length*
                  *max-upstream-total-header-bytes*))
    (let ((value (symbol-value name)))
      (unless (or (null value) (and (integerp value) (plusp value)))
        (error "start-server: ~(~a~) is ~s; it must be a positive integer, ~
                or NIL for the inbound limit's value."
               name value))))
  ;; Eleventh. The timeouts and intervals the idle sweep, the ping pass,
  ;; the keepalive tick, the fetch path, the drain and JWT-VERIFY read, and
  ;; the sizes and counts the read path, the WebSocket parser, the fetch
  ;; path and JSON-PARSE hold their input to. Each is read on a worker. A
  ;; value that is not a number raises there: in the sweep, a tick or an
  ;; accept it reaches RUN-WORKER's backstop, so the worker restarts,
  ;; dropping every connection it held, and does it again on the next pass;
  ;; anywhere else it fails every request, fetch or token it touches, with
  ;; nothing that names the setting. Where a setting's docstring names an
  ;; off, 0 is it, and 0 is allowed where it means something: no request
  ;; body, no limit on connections, no interim response.
  ;;
  ;; A fraction is fine for a setting held against a difference of two
  ;; clock readings. One added to GET-UNIVERSAL-TIME takes whole seconds:
  ;; the deadline lands in an integer slot, and a float at the clock's
  ;; magnitude would resolve only to 256 seconds besides.
  (loop for (name kind) in '((*idle-timeout* :seconds)
                             (*ws-idle-timeout* :seconds)
                             (*ws-message-timeout* :seconds)
                             (*stream-idle-timeout* :seconds)
                             (*linger-timeout* :seconds)
                             (*stream-keepalive-interval* :seconds)
                             (*ws-ping-interval* :positive-seconds)
                             (*worker-wake-interval* :positive-seconds)
                             (*dns-cache-ttl* :whole-seconds)
                             (*drain-timeout* :whole-seconds)
                             (*jwt-clock-skew* :whole-seconds)
                             (*connect-fallback-delay* :positive-whole-seconds)
                             (*max-request-line-length* :positive-count)
                             (*max-header-count* :positive-count)
                             (*max-header-line-length* :positive-count)
                             (*max-total-header-bytes* :positive-count)
                             (*max-ws-payload-size* :positive-count)
                             (*max-outbound-response-size* :positive-count)
                             (*max-streaming-line-size* :positive-count)
                             (*dns-cache-max-entries* :positive-count)
                             (*json-max-depth* :positive-count)
                             (*json-max-string-length* :positive-count)
                             (*ws-max-missed-pongs* :positive-count)
                             (*max-body-size* :count)
                             (*max-connections* :count)
                             (*max-interim-responses* :count))
        for value = (symbol-value name)
        unless (ecase kind
                 (:seconds (and (realp value) (>= value 0)))
                 (:positive-seconds (and (realp value) (plusp value)))
                 ((:whole-seconds :count) (and (integerp value) (>= value 0)))
                 ((:positive-whole-seconds :positive-count)
                  (and (integerp value) (plusp value))))
          do (error "start-server: ~(~a~) is ~s; it must be ~a."
                    name value
                    (ecase kind
                      (:seconds "a number of seconds, 0 or more")
                      (:positive-seconds "a positive number of seconds")
                      (:whole-seconds "a whole number of seconds, 0 or more")
                      (:positive-whole-seconds "a positive whole number of seconds")
                      (:count "an integer, 0 or more")
                      (:positive-count "a positive integer"))))
  ;; A new run before *SHUTDOWN* is cleared, so no worker of an earlier run
  ;; ever sees the flag clear while its run is still the current one: see
  ;; *RUN-GENERATION*.
  (setf *run-generation* (1+ *run-generation*))
  (setf *shutdown* nil)
  ;; Sized here, before any worker exists, because a worker's slot index is
  ;; its id and the vector has to be there when the first tick publishes.
  ;; Replaced rather than cleared, so a second START-SERVER in one image
  ;; cannot inherit a previous run's counts.
  (setf *connection-census* (make-array workers :initial-element nil))
  ;; Save the previous SIGPIPE and SIGTERM handlers so start-server can
  ;; be called from inside a host SBCL image (a REPL, a test runner, an
  ;; orchestrator) without permanently stealing the signals.
  ;;   SIGPIPE -> :ignore so writes to a broken peer return EPIPE
  ;;   SIGTERM -> set *shutdown*, matching Ctrl-C's path
  ;;
  ;; Two nested unwind-protects, one per signal: SIGPIPE in the
  ;; outer, SIGTERM in the inner. If SIGTERM install raises, the
  ;; outer cleanup still restores SIGPIPE. If both succeed and the
  ;; body raises (MAKE-THREAD exhaustion, etc.), both cleanups run.
  ;; A single parallel let would leak SIGPIPE if the SIGTERM init
  ;; form raised before the body was entered.
  (let ((prev-sigpipe (sb-sys:enable-interrupt sb-unix:sigpipe :ignore)))
    (unwind-protect
         (let ((prev-sigterm (sb-sys:enable-interrupt sb-unix:sigterm
                               (lambda (signal info context)
                                 (declare (ignore signal info context))
                                 (setf *shutdown* t)))))
           (unwind-protect
                ;; Resolve port 0 here, once, rather than letting the
                ;; workers each pass it to bind(2). Two things break if
                ;; they do: N workers on port 0 land on N *different*
                ;; ephemeral ports, and RUN-WORKER's restart loop rebinds,
                ;; so a crashed worker would come back somewhere its
                ;; siblings are not. Neither shows up as an error — the
                ;; server keeps serving on whichever port the caller
                ;; happens to have been told about.
                ;;
                ;; The socket bound here is handed to worker 0 rather than
                ;; closed and rebound: a port that is closed and rebound is
                ;; free for an instant, and the docstring above is about
                ;; what gets into that instant.
                (let* ((listener0 (when (zerop port)
                                    (make-tcp-listener host port)))
                       (port (if listener0
                                 (nth-value 1 (sb-bsd-sockets:socket-name
                                               listener0))
                                 port)))
                  (log-info "starting ~d worker~:p on ~a"
                            workers (format-peer-addr host port))
                  ;; Before the workers, which read it at every accept.
                  (multiple-value-bind (limit before) (raise-descriptor-limit)
                    (setf *descriptor-limit* limit)
                    (if (> limit before)
                        (log-info "descriptor limit ~d, raised from ~d" limit before)
                        (log-info "descriptor limit ~d" limit)))
                  ;; Accumulate threads incrementally rather than via LOOP
                  ;; COLLECT: if MAKE-THREAD raises on iteration N, the
                  ;; partial list in THREADS still covers workers 0..N-1 so
                  ;; the drain cleanup can signal and join them.
                  (let ((threads nil)
                        (generation *run-generation*)
                        (census *connection-census*))
                    (unwind-protect
                         (progn
                           (dotimes (i workers)
                             (let ((id i)
                                   (adopted (when (zerop i) listener0)))
                               (push (sb-thread:make-thread
                                      (lambda ()
                                        ;; Bound here, around RUN-WORKER and
                                        ;; so outside its restart loop, where
                                        ;; every other per-worker slot is
                                        ;; bound fresh on each pass. A serial
                                        ;; names a connection, and a worker
                                        ;; that crashed and counted from one
                                        ;; again would hand a second peer the
                                        ;; first one's name. The run and its
                                        ;; census belong to the thread for the
                                        ;; same reason: a restart is the same
                                        ;; worker of the same run.
                                        (let ((*connection-serial* 0)
                                              (*worker-generation* generation)
                                              (*worker-census* census))
                                          (run-worker host port id
                                                      handler ws-handler
                                                      on-tick body-handler
                                                      adopted)))
                                      :name (format nil "web-skeleton-~d" i))
                                     threads)
                               ;; Ownership passes to the thread only once
                               ;; the thread exists. Clearing LISTENER0
                               ;; after a successful MAKE-THREAD — never
                               ;; before — is what keeps the cleanup below
                               ;; and worker 0 from both closing it, while
                               ;; still closing it if MAKE-THREAD raised.
                               (when adopted (setf listener0 nil))))
                           ;; After the spawn loop, so a caller that
                           ;; connects the moment it learns the port finds
                           ;; workers on their way up rather than a bound
                           ;; socket with nobody behind it. The listener is
                           ;; already accepting into its backlog either way.
                           (when on-listen (funcall on-listen port))
                           (handler-case
                               ;; Main thread waits for interrupt or SIGTERM
                               (loop (sleep *worker-wake-interval*)
                                     (when *shutdown*
                                       (log-info "shutting down")
                                       (return)))
                             (sb-sys:interactive-interrupt ()
                               (format t "~%")
                               (log-info "interrupted — shutting down"))))
                      ;; Signal workers to drain and stop. Runs on normal
                      ;; exit, interactive-interrupt, AND a MAKE-THREAD
                      ;; raise partway through the spawn loop.
                      (setf *shutdown* t)
                      ;; Non-NIL only if MAKE-THREAD raised before worker 0
                      ;; adopted it, in which case nothing else will ever
                      ;; close it.
                      (when listener0
                        (ignore-errors
                         (sb-bsd-sockets:socket-close listener0)))
                      ;; A worker still inside a handler or a callback when
                      ;; its join gives up is still running, and is said to
                      ;; be, by name, rather than this returning under a
                      ;; plain "stopped". It belongs to this run and stops
                      ;; when it comes back to its loop — WORKER-STOPPING-P
                      ;; holds even after a later START-SERVER clears
                      ;; *SHUTDOWN* — but until then it holds its listener,
                      ;; and the port with it.
                      (let ((still-running 0))
                        (dolist (thread threads)
                          (when (eq :timeout
                                    (nth-value 1 (ignore-errors
                                                  (sb-thread:join-thread
                                                   thread :default nil
                                                   :timeout (+ *drain-timeout* 3)))))
                            (incf still-running)
                            (log-warn "~a did not stop within ~d seconds and is ~
                                       still running: a handler or callback on ~
                                       it has not returned. It stops when it ~
                                       does."
                                      (sb-thread:thread-name thread)
                                      (+ *drain-timeout* 3))))
                        ;; Workers have drained; run any app-registered
                        ;; cleanup before returning. Hooks own their own error
                        ;; handling — one raising hook cannot block the rest,
                        ;; cannot block the "stopped" log, and cannot prevent
                        ;; START-SERVER from returning to its caller.
                        (run-shutdown-hooks)
                        (if (zerop still-running)
                            (log-info "stopped")
                            (log-warn "stopped, with ~d worker~:p still running"
                                      still-running))))))
             ;; Inner cleanup: restore SIGTERM
             (ignore-errors
              (sb-sys:enable-interrupt sb-unix:sigterm prev-sigterm))))
      ;; Outer cleanup: restore SIGPIPE (runs even if SIGTERM install raised)
      (ignore-errors
       (sb-sys:enable-interrupt sb-unix:sigpipe prev-sigpipe)))))
