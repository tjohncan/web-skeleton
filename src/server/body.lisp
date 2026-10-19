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
;;;;   an HTTP-RESPONSE     the answer, sent before the body is read
;;;;
;;;; The handler contract does not change. :HANDLER is still called with a
;;;; whole request, and an application with no body handler reads bodies
;;;; exactly as it would have without this file.
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

(defstruct (body-plan (:constructor %make-body-plan))
  "What a body handler decided for a request's body. Built by BUFFER-BODY;
   never by hand, so every plan has been through the constructor's checks."
  (kind :buffer :type keyword)
  (max-size 0 :type (integer 0)))

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

(defun apply-body-plan (conn plan)
  "Record PLAN on CONN: the cap its body is held to, and the progress
   clock a body accepted by a body handler is timed on."
  (setf (connection-body-limit conn) (body-plan-max-size plan)
        (connection-body-progress-at conn) (get-universal-time)))
