(in-package :web-skeleton)

;;; ---------------------------------------------------------------------------
;;; Per-worker counters
;;;
;;; Cumulative counts of things only the framework sees. An application can
;;; count its own handler calls perfectly well; what it cannot count is the
;;; requests that never reached a handler — the 400 for ambiguous framing, the
;;; 413 for a buffer that filled, the 505 for a version token, the 503 for a
;;; worker at its connection limit. Those are the interesting ones, and they
;;; are invisible from above by construction.
;;;
;;; Bound per worker, like *CONNECTIONS* and *EPOLL-FD*, so an increment needs
;;; no lock and cannot contend with another worker. Published into that
;;; worker's census slot on the maintenance tick and summed by a reader on any
;;; thread, which is the same discipline the connection counts already use.
;;;
;;; Cumulative for the life of each worker, and never windowed. Five minutes
;;; and an hour are presentation, and a framework that ships a metrics format
;;; ships the wrong one. A caller that wants a rate samples two of these and
;;; subtracts; one that wants a graph keeps its own ring on :ON-TICK. Not
;;; monotonic across a restart: a worker that crashes begins a fresh set, so a
;;; difference taken across one can be negative, and means a restart rather
;;; than a rate.
;;; ---------------------------------------------------------------------------

(defstruct counters
  (accepted      0 :type unsigned-byte)
  (refused       0 :type unsigned-byte)
  (responses     0 :type unsigned-byte)
  (informational 0 :type unsigned-byte)
  (successful    0 :type unsigned-byte)
  (redirected    0 :type unsigned-byte)
  (client-error  0 :type unsigned-byte)
  (server-error  0 :type unsigned-byte)
  (ws-frames     0 :type unsigned-byte))

(defvar *counters* nil
  "This worker's counters, or NIL off a worker. Bound in RUN-WORKER beside
   the other share-nothing slots. Every NOTE- function below is a no-op when
   it is NIL, so a test or a REPL that formats a response outside a worker
   counts nothing rather than raising.")

(defun note-accepted ()
  (when *counters* (incf (counters-accepted *counters*))))

(defun note-refused ()
  (when *counters* (incf (counters-refused *counters*))))

(defun note-ws-frame ()
  "Record one WebSocket frame an application handed to a connection.

   Two ways to hand one over, and both call this: WS-SEND, and returning the
   frame from a ws-handler. The second is the documented reply path and it
   never passes through WS-SEND, so counting only there would miss the
   commonest way an application answers.

   Not the frames the framework sends on its own behalf — a pong answering a
   ping, the pings that keep a socket alive, a close. Those are the
   framework's traffic rather than the application's, and a count mixing the
   two could not be read as either.

   One per call or return, not one per frame on the wire: bytes handed over
   in one piece count once, however many frames they hold.

   The two are counted at different moments, and it is the one place they
   differ. WS-SEND counts after its append succeeds, so a frame refused at
   *MAX-WRITE-BACKLOG* raises and is not counted. A reply is counted when the
   handler returns it, before it is queued: a reply to a peer already at the
   backlog is counted, and the connection is closed rather than the reply
   sent. Counting it after the queue would mean the read path returning a
   count beside its bytes, for the one frame on a connection already being
   torn down."
  (when *counters* (incf (counters-ws-frames *counters*))))

(defun note-response (status)
  "Record one response of STATUS, by class as well as in total.

   Counted where the framework produces a response, not where a handler
   returned — most of what is worth counting never passes through a handler:
   a parse error becomes a response without one ever running. And not where
   bytes are queued. The queue carries response heads, bodies, streamed
   chunks, WebSocket frames and whatever byte vector an application hands
   over; counting there would mean reading a status back out of bytes and
   telling a head from all of that. The producer already knows the status it
   wrote.

   The producers, and a new one has to call this too, once its bytes exist:
   FORMAT-RESPONSE for anything built per request, STATIC-SEGMENTS and the two
   range builders for cached files, FORMAT-STREAMING-HEAD for a streamed body,
   and ACCEPT-CONNECTION for the 503 a worker at its limit sends.

   Two of those an application can call itself, FORMAT-RESPONSE and
   SERVE-STATIC, and both count once the bytes exist — on whatever worker
   calls them, whether or not those bytes are then sent. An application that
   calls SERVE-STATIC, discards the result and answers 403 instead has counted
   two responses. A response the serializer refuses never has bytes, so it is
   not counted: FORMAT-RESPONSE raises for a CTL in a header before counting,
   and the 500 that replaces it is the only response counted. Off a worker
   neither counts anything, which is how a response can be built only to be
   inspected.

   Two responses are not counted at all. One a handler returns as a byte
   vector it built itself: the framework queues it as given and never learns
   its status — which is also how an application keeps a response out of the
   count on purpose. And the interim 100 Continue, which precedes the final
   response to the same request rather than standing in for one.

   That leaves one 1xx the framework counts of its own: the 101 that
   completes a WebSocket upgrade. So :INFORMATIONAL is the upgrades, plus any
   1xx an application's handler builds itself."
  (when *counters*
    (incf (counters-responses *counters*))
    (case (floor status 100)
      (1 (incf (counters-informational *counters*)))
      (2 (incf (counters-successful *counters*)))
      (3 (incf (counters-redirected *counters*)))
      (4 (incf (counters-client-error *counters*)))
      (5 (incf (counters-server-error *counters*))))))

(defun counters-snapshot (&optional (c *counters*))
  "A plist of C, or NIL when there is none.

   Freshly built, never the struct itself. The census publishes by replacing
   a slot outright, and handing a reader on another thread the live struct
   would let it see fields a worker is still incrementing — the same reason
   PUBLISH-CONNECTION-CENSUS builds a new plist rather than patching one."
  (when c
    (list :accepted      (counters-accepted c)
          :refused       (counters-refused c)
          :responses     (counters-responses c)
          :informational (counters-informational c)
          :successful    (counters-successful c)
          :redirected    (counters-redirected c)
          :client-error  (counters-client-error c)
          :server-error  (counters-server-error c)
          :ws-frames     (counters-ws-frames c))))
