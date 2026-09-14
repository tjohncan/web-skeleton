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
;;; Monotonic, and never windowed. Five minutes and an hour are presentation,
;;; and a framework that ships a metrics format ships the wrong one. A caller
;;; that wants a rate samples two of these and subtracts; one that wants a
;;; graph keeps its own ring on :ON-TICK.
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
   in one piece count once, however many frames they hold."
  (when *counters* (incf (counters-ws-frames *counters*))))

(defun note-response (status)
  "Record one response of STATUS, by class as well as in total.

   Counted where a response is handed to a connection rather than where a
   handler returned, because most of what is worth counting never passes
   through a handler: a parse error becomes a response without one ever
   running.

   Not where the bytes are serialized in general. A cached file's bytes are
   serialized once at startup and sent thousands of times, and a refusal at
   the connection limit is pre-built bytes written straight to a socket; in
   both, serialization and emission are different moments.

   So every path that emits a response calls this with its own status, and a
   new one has to remember: FORMAT-RESPONSE for anything built per request,
   STATIC-SEGMENTS and the two range builders for cached files,
   FORMAT-STREAMING-HEAD for a streamed body, and ACCEPT-CONNECTION for the
   503 a worker at its limit sends.

   FORMAT-RESPONSE is the one of those an application can call, and it counts
   on whatever worker calls it — whether or not the bytes are then sent. Off a
   worker it counts nothing, which is how an application builds responses it
   means only to inspect."
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
