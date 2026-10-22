;;;; body.lisp — a request's body, decided at its head
;;;;
;;;; A handler is called with a whole request, and for most requests that
;;;; is the right shape: the body is small, the cap is *MAX-BODY-SIZE*,
;;;; and nothing about it needs deciding before it is read. Some requests
;;;; are not like that. An upload route wants a larger cap than the JSON
;;;; API beside it; an unauthenticated upload should be refused before its
;;;; first byte is read, not after its last.
;;;;
;;;; START-SERVER's :BODY-HANDLER is the place for that decision. It is
;;;; called on the worker once the head of a request with a body has
;;;; arrived, with the request parsed and its body NIL, before any of the
;;;; body is read and before a 100 Continue invites it. It answers:
;;;;
;;;;   NIL                  the body is read as it would be without a body
;;;;                        handler: buffered under *MAX-BODY-SIZE*, timed
;;;;                        by *IDLE-TIMEOUT*, then :HANDLER
;;;;   (BUFFER-BODY ...)    buffered under a cap of this request's own,
;;;;                        timed on progress, then :HANDLER
;;;;   (RECEIVE-BODY ...)   handed to the application as it arrives, a read
;;;;                        at a time, and answered by its :ON-END; the
;;;;                        body is never held whole, so its size is the
;;;;                        cap's business and not memory's
;;;;   (RECEIVE-MULTIPART ...)
;;;;                        the same for a multipart/form-data body, parsed
;;;;                        as it arrives into the parts of a form; see
;;;;                        src/server/multipart.lisp
;;;;   an HTTP-RESPONSE     the answer, sent before the body is read
;;;;
;;;; The handler contract does not change. :HANDLER is still called with a
;;;; whole request, and an application with no body handler reads bodies
;;;; exactly as it would have without this file. A received body never
;;;; reaches :HANDLER at all: the plan that received it answers it.
;;;;
;;;; The decision belongs to the head and to nothing earlier: the framing
;;;; checks (Transfer-Encoding against Content-Length, an unknown Expect)
;;;; come first, because they are about whether the request can be read at
;;;; all, and a body handler is never shown a request the framework is
;;;; about to refuse.

(in-package :web-skeleton)

(defparameter *body-stall-timeout* 30
  "Seconds the body of a request a body handler accepted may go without a
   byte arriving before the connection is closed. Default 30. Must be
   positive; validated when the server starts.

   Separate from *IDLE-TIMEOUT* because the two bound different things.
   *IDLE-TIMEOUT* is the whole budget for a request to arrive, and the
   bytes arriving do not extend it — which is what stops a client
   dripping a request, and also what cuts off an honest upload that is
   merely slow. A body the application has accepted is one it is waiting
   for, so it is judged on whether it is still arriving: every read that
   brings some of it restarts the clock. The head is still the idle
   timeout's, measured from the connection's last request, and so is any
   body no body handler accepted.

   The trade is the one *WRITE-STALL-TIMEOUT* makes in the other
   direction: a client that sends a byte per interval is never closed by
   this. What it holds is a connection slot and a read buffer no larger
   than the body's cap allows. A client that slow belongs to the proxy in
   front (DEPLOYMENT.md, Reverse proxy).

   Positive, because once a body is accepted this is its only time bound.
   At zero an accepted body could stall forever with its slot held.")

(defun buffer-body (&key (max-size *max-body-size*))
  "A body plan, for a body handler to return: read the request's body
   whole, under a cap of MAX-SIZE bytes, then call :HANDLER with the whole
   request as usual.

   The cap is this request's, above or below *MAX-BODY-SIZE*, and it is
   held the way that one is: a Content-Length over it is refused with 413
   before any of the body is read, and a chunked body is refused the
   moment its decoded total, or a chunk header, would pass it. The read
   buffer may grow to the cap, so a large cap is memory the request may
   really use — that much per connection buffering a body this way at
   once.

   A body accepted this way is timed on its progress, by
   *BODY-STALL-TIMEOUT*, rather than as a whole by *IDLE-TIMEOUT*: a
   large upload takes as long as it takes, so long as it keeps arriving."
  (unless (and (integerp max-size)
               (<= 0 max-size)
               (< (+ max-size (request-head-budget)) array-total-size-limit))
    (error "buffer-body: :max-size is ~s; it must be a non-negative integer ~
            of bytes the read buffer can hold."
           max-size))
  (%make-body-plan :kind :buffer :max-size max-size))

(defun %callable-p (f)
  "True for something FUNCALL can call: a function, or a symbol naming one
   that is neither a macro nor a special operator."
  (or (functionp f)
      (and (symbolp f) f (fboundp f)
           (not (macro-function f))
           (not (special-operator-p f)))))

(defun receive-body (&key on-data on-end on-abort (max-size *max-body-size*))
  "A body plan, for a body handler to return: hand the request's body to
   the application as it arrives, and let ON-END answer the request.

   ON-DATA  (CONN BYTES) — the next run of the body, decoded, as a fresh
            vector the application may keep. Called once per read that
            brought some of it, so the pieces are the size reads happen to
            be and not the sender's chunks. Its value is ignored unless it
            is :PAUSE or an HTTP-RESPONSE.
              :PAUSE stops the body being handed over, from this run on,
              until BODY-RESUME: the connection is subscribed to nothing,
              the client's bytes wait in the kernel, and its send window
              fills. For an application whose sink is asynchronous — each
              part forwarded by a fetch it starts with FETCH-INTO, whose
              :THEN resumes. A pause taken on the body's last bytes holds
              ON-END back until the resume.
              An HTTP-RESPONSE refuses the rest of the body: the response
              is sent, and the connection closes after it, its close
              lingering as every refusal of a request still arriving does.
              BODY-REFUSE does the same from outside ON-DATA.
   ON-END   (CONN) — the body is complete. Returns the request's response,
            in any shape :HANDLER may answer with but :UPGRADE: an
            HTTP-RESPONSE, a STREAM-RESPONSE, a fetch continuation, a byte
            vector, or SERVE-STATIC's segments.
   ON-ABORT (CONN REASON) — the body ended any other way. Called at most
            once, and never once ON-END has returned:
              :DISCONNECTED  the client left before the body was complete
              :STALLED       *BODY-STALL-TIMEOUT* passed without a byte
              :TOO-LARGE     the body passed MAX-SIZE; answered 413
              :MALFORMED     its chunked framing was wrong; answered 400
              :REFUSED       ON-DATA answered with a response, or
                             BODY-REFUSE was called
              :ERROR         ON-DATA or ON-END raised; answered 500
              :UPSTREAM-FAILED
                             a fetch started into the connection failed,
                             under its default :FAILURE-DISPOSITION
              :SHUTDOWN      the server drained
              :CLOSED        the connection closed for any other reason
            The one place to release what ON-DATA has been building — a
            partial file, an upload begun elsewhere.
   MAX-SIZE the cap on the body's decoded bytes, *MAX-BODY-SIZE* by
            default, or NIL for none. A Content-Length over it is refused
            before any of the body is read, a chunked body the moment its
            total or a chunk header would pass it.

   The body is never held whole. The read buffer holds the head and at
   most one read of the body — +RECEIVE-WINDOW+ bytes, or whatever arrived
   with the head before the body handler was asked — so a body's size is
   the cap's business and not memory's, and NIL is a cap a server can
   afford.

   Everything ON-DATA has been handed is provisional until ON-END. A body
   cut short, or found to be malformed partway, has had some of its bytes
   delivered by the time ON-ABORT says so; a chunked body's bytes are
   handed over as they arrive, before the CRLF that ends their chunk has
   been seen.

   The callbacks run on the worker and hold it while they run. That is
   backpressure for free when the application writes what it is handed
   somewhere synchronous, a file say: nothing more is read until ON-DATA
   returns, the kernel's receive window fills, and the client slows. It is
   also every other connection on the worker waiting, so ON-DATA should
   do what a request handler should and nothing slower.

   A body received this way is timed on its progress by
   *BODY-STALL-TIMEOUT*, as BUFFER-BODY's is."
  (unless (%callable-p on-data)
    (error "receive-body: :on-data is ~s; it must be a function of two ~
            arguments (the connection and the bytes)."
           on-data))
  (unless (%callable-p on-end)
    (error "receive-body: :on-end is ~s; it must be a function of one ~
            argument (the connection) that returns the response."
           on-end))
  (unless (or (null on-abort) (%callable-p on-abort))
    (error "receive-body: :on-abort is ~s; it must be a function of two ~
            arguments (the connection and the reason), or NIL."
           on-abort))
  (unless (or (null max-size) (and (integerp max-size) (<= 0 max-size)))
    (error "receive-body: :max-size is ~s; it must be a non-negative ~
            integer of bytes, or NIL for no cap."
           max-size))
  (%make-body-plan :kind :receive
                   :max-size (or max-size most-positive-fixnum)
                   ;; ON-DATA takes every run whole, so a pause is taken at
                   ;; the run's end.
                   :feed (lambda (conn buf from to)
                           (values (funcall on-data conn (subseq buf from to)) to))
                   :on-end on-end
                   :on-abort on-abort))

(defun apply-body-plan (conn plan)
  "Record PLAN on CONN: the cap its body is held to, the progress clock a
   body accepted by a body handler is timed on, and for a :RECEIVE plan,
   the receiver itself."
  (setf (connection-body-limit conn) (body-plan-max-size plan)
        (connection-body-progress-at conn) (get-universal-time))
  (when (eq (body-plan-kind plan) :receive)
    (setf (connection-body-receiver conn) plan
          (connection-body-received conn) 0
          (connection-body-owed conn) 0
          (connection-body-held conn) 0
          (connection-body-paused conn) nil)))

(defun body-resume (conn)
  "Resume a received body that its ON-DATA paused.

   What the pause left in the read buffer is handed over first, then
   reading goes on — or, when the pause came with the body's last bytes,
   ON-END runs. The connection is woken through writability, which its
   socket has at once, so the resume does not wait on more bytes
   arriving: the ones already buffered raise no readability edge of their
   own.

   CONN is what ON-DATA was called with. Returns T, or NIL when there is
   nothing to resume — the body is not paused, it has ended, or the
   connection has closed — so a caller that resumes from a callback that
   may run after the client left is not punished for it.

   Signals, before anything is touched, off a worker or on a connection
   another worker owns, as WS-SEND and STREAM-CLOSE do: the resume arms
   the owning worker's epoll interest, and only that worker may.

   A resume restarts the stall clock. A pause is the application not
   reading, so the body was not stalled while it lasted — though a pause
   longer than *BODY-STALL-TIMEOUT* is ended as a stall, unless what the
   application is waiting on is a fetch started into this connection,
   which its own deadline bounds."
  (when (>= (connection-fd conn) 0)
    (unless *epoll-fd*
      (error "body-resume: no event loop on this thread. A received body is ~
              resumed from the worker that owns its connection — a fetch's ~
              :THEN, an :ON-TICK, another callback on that worker."))
    (unless (eq (lookup-connection (connection-fd conn)) conn)
      (error "body-resume: fd ~d is not on this worker's connection table. ~
              Only the worker that owns a connection may resume its body."
             (connection-fd conn)))
    (when (and (connection-body-receiver conn) (connection-body-paused conn))
      (setf (connection-body-paused conn) nil
            (connection-body-progress-at conn) (get-universal-time))
      (epoll-modify *epoll-fd* (connection-fd conn)
                    (logior +epollin+ +epollout+ +epollet+))
      t)))

