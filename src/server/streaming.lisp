;;;; streaming.lisp — server-generated streaming responses
;;;;
;;;; The framework has three chunked *decoders* and no encoder: everything
;;;; chunked it has ever seen arrived from an upstream. Producing a stream
;;;; is the other direction, and it starts here.
;;;;
;;;; This file is deliberately not fetch.lisp. The decoders live there
;;;; because they belong to reading an upstream response; encoding belongs
;;;; to writing our own.

(in-package :web-skeleton)

;;; ---------------------------------------------------------------------------
;;; Chunked transfer encoding — the producing side
;;;
;;; Encode to the intersection of what the three readers require, not to
;;; what any one of them forgives. DECODE-CHUNKED-BODY demands strict CRLF
;;; after both the size line and the data ("Lax trailing CRLF is a smuggling
;;; primitive"); PARSE-CHUNKED-SIZE-LINE tolerates BWS around the hex and
;;; strips chunk-extensions; CHUNKED-BODY-COMPLETE-P is lax on purpose and
;;; validates nothing. Their tolerances exist to survive other people's
;;; output. They are not a licence for ours, so what goes on the wire is
;;; bare lowercase hex, CRLF, data, CRLF — no extensions, no whitespace,
;;; nothing any strict downstream re-parser could read differently.
;;; ---------------------------------------------------------------------------

(defun chunked-terminator ()
  "The last-chunk and its empty trailer section: 0 CRLF CRLF.

   Its own entry point, and that is the whole point of it. If the
   terminator could also be produced by encoding a zero-length payload,
   then any producer that happened to emit nothing — a filter that
   matched no rows this tick, a serializer handed an empty batch — would
   end the message early while emitting bytes every decoder accepts.
   The stream would be truncated, not corrupted, so nothing would raise
   anywhere and the reader would simply believe it had the whole thing.
   A generated property can be made to catch that; a function that cannot
   emit the byte sequence does not have the failure mode. Same shape as
   CONNECTION-QUEUE-WRITE beside CONNECTION-APPEND-WRITE: two doors named
   for intent, so the dangerous one is never reached by accident.

   Shared and never mutated, like the pre-built ping frame — the write
   queue holds vectors by reference and advances a per-connection offset
   through them."
  (load-time-value
   (sb-ext:string-to-octets
    (coerce (list #\0 #\Return #\Newline #\Return #\Newline) 'string)
    :external-format :ascii)
   t))

(defun encode-chunk (bytes &key (start 0) (end (length bytes)))
  "Frame BYTES[START..END) as one chunk. Returns the framed vector, or
   NIL when the range is empty.

   An empty write is a no-op rather than an error. A streaming producer
   naturally has nothing to say on some passes, so refusing would put a
   guard at every call site, and the cost of one missing guard is a
   truncated stream — the exact failure CHUNKED-TERMINATOR is split out
   to prevent. Nothing is lost by dropping it, because an empty chunk
   carries nothing.

   That reasoning is about chunked framing and stops here. It must not
   climb to the streaming surface above: an empty *SSE* write is not
   nothing — a bare comment line is the keepalive that stops an
   intermediary reaping an idle stream, and swallowing it would show up
   only as connections dying behind a proxy, in the deployment the docs
   assume and in none of the tests."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  ;; A reversed or out-of-bounds range is a caller bug, and without this
  ;; it would borrow the quiet return that an empty range is entitled to:
  ;; (:start 2 :end 1) and (:start 2 :end 2) would answer NIL alike, one
  ;; of them correctly. Bytes vanishing from a stream is the failure this
  ;; whole file is arranged against, so the two do not get to look the
  ;; same — the more so once app code is passing the offsets.
  (unless (<= 0 start end (length bytes))
    (error "encode-chunk: bad range (start ~d, end ~d, length ~d)"
           start end (length bytes)))
  (let ((len (- end start)))
    (when (plusp len)
      (let* ((header (sb-ext:string-to-octets
                      (format nil "~(~x~)~c~c" len #\Return #\Newline)
                      :external-format :ascii))
             (hlen (length header))
             (out (make-array (+ hlen len 2)
                              :element-type '(unsigned-byte 8))))
        (replace out header)
        (replace out bytes :start1 hlen :start2 start :end2 end)
        (setf (aref out (+ hlen len)) 13
              (aref out (+ hlen len 1)) 10)
        out))))

;;; ---------------------------------------------------------------------------
;;; Streaming response head
;;;
;;; The terminator's lifecycle, decided here because this is the function
;;; that decides there is no Content-Length:
;;;
;;;   The framework emits CHUNKED-TERMINATOR on stream teardown, on every
;;;   path out — normal end, handler error, connection reaped. The app
;;;   never writes it and is given no way to.
;;;
;;;   A stream whose terminator was not written may not return its
;;;   connection to :READ-HTTP. If the terminator cannot be emitted at all
;;;   — the backlog is full, the socket is gone — the connection is closed
;;;   rather than reused.
;;;
;;; Both halves are needed and they cover different failures. Splitting
;;; the terminator behind its own door made truncation impossible *inside*
;;; the encoder; it did not make anything emit it. A handler that returns
;;; early, raises after its first chunk, or takes a path nobody thought
;;; about leaves an unterminated body on the socket, and a keep-alive
;;; reuse then puts the next response's status line exactly where a
;;; downstream reader expects a chunk-size. That is the parser
;;; disagreement PARSE-CHUNKED-SIZE-LINE's docstring is written to defend
;;; against, manufactured by us instead of by an upstream.
;;;
;;; Enforcement lands with the :STREAMING state; the contract is stated
;;; here so it is not invented there.
;;; ---------------------------------------------------------------------------

(defun stream-framing-for (request)
  "The body framing a streaming response to REQUEST must use.

     :CHUNKED — HTTP/1.1.
     :CLOSE   — anything older. Chunked transfer coding is an HTTP/1.1
                feature (RFC 7230 §4.1); a 1.0 client cannot read it, so
                the only framing left is the one the end of the
                connection provides.

   Close-delimited framing is the shape whose *reading* side was broken
   until :OK-EOF landed — a peer's FIN arriving in the same read as the
   last data was swallowed, and the response hung. Named here because
   this is the function that decides to start producing them."
  (if (string= (http-request-version request) "1.1")
      :chunked
      :close))

(defun format-streaming-head (response framing &key connection-hint)
  "Serialize the status line and headers of a streaming response.
   No body, and deliberately no Content-Length — a response whose length
   is unknown when the headers go out is the entire point.

   FRAMING comes from STREAM-FRAMING-FOR. :CHUNKED stamps
   Transfer-Encoding; :CLOSE stamps Connection: close, because the end of
   the connection is the framing and the caller must actually close.

   HEAD is the caller's business, not this function's. RFC 7231 §4.3.2
   wants a HEAD response carrying the headers the GET would have carried,
   which is exactly what this already produces — so a HEAD dispatch emits
   this head, starts no stream, and owes no terminator.

   Shares SERIALIZE-HTTP-MESSAGE with FORMAT-RESPONSE rather than taking
   a third behavioral flag on it. Almost everything FORMAT-RESPONSE does
   with a body is Content-Length work — computing it, defaulting it to
   zero for bodiless statuses, preserving it across HEAD — and a stream
   wants none of it. The flag would have switched off most of the
   function it was added to.

   Refuses rather than reconciling. A caller-supplied Content-Length or
   Transfer-Encoding means the caller thinks it owns framing, and two
   opinions about framing on one response is the disagreement this
   codebase treats as the threat model. A status that cannot carry a body
   is refused for the same reason: there is nothing to stream."
  (let ((status (http-response-status response))
        (headers (http-response-headers response)))
    (unless (<= 100 status 599)
      (error "HTTP status ~d out of range (must be 100-599)" status))
    (when (or (<= 100 status 199) (= status 204) (= status 304))
      (error "streaming response: status ~d cannot carry a body" status))
    (when (assoc "content-length" headers :test #'string-equal)
      (error "streaming response: caller set Content-Length; a stream ~
              has no length to declare"))
    (when (assoc "transfer-encoding" headers :test #'string-equal)
      (error "streaming response: caller set Transfer-Encoding; the ~
              framework owns the framing of a stream"))
    (let* ((headers
             (ecase framing
               (:chunked (cons (cons "transfer-encoding" "chunked") headers))
               ;; On this path Connection is not a hint, it is the framing:
               ;; with no Content-Length and no Transfer-Encoding, the end
               ;; of the connection is the only thing that says where the
               ;; body stops. FORMAT-RESPONSE lets a caller-set Connection
               ;; win, correctly, because there Content-Length does the
               ;; framing and Connection only advises — the roles swap
               ;; here, and inheriting the convention with the shape would
               ;; put "connection: keep-alive" on a response nothing can
               ;; find the end of. Agreement is accepted; contradiction is
               ;; refused, same as Content-Length and Transfer-Encoding.
               (:close
                (let ((existing (assoc "connection" headers
                                       :test #'string-equal)))
                  (cond
                    ((null existing)
                     (cons (cons "connection" "close") headers))
                    ((string-equal (string-trim '(#\Space #\Tab)
                                                (cdr existing))
                                   "close")
                     headers)
                    (t
                     (error "streaming response: framing is close-delimited ~
                             but the caller set Connection: ~a — on this ~
                             path that header is the framing, and nothing ~
                             else says where the body ends"
                            (cdr existing))))))))
           ;; The hint still applies on the chunked path — a server that
           ;; has decided to close SHOULD say so (RFC 7230 §6.1) even
           ;; when the body is self-framing. On the :CLOSE path the
           ;; header is already there.
           (headers (if (and connection-hint
                             (not (assoc "connection" headers
                                         :test #'string-equal)))
                        (cons (cons "connection"
                                    (ecase connection-hint
                                      (:close "close")
                                      (:keep-alive "keep-alive")))
                              headers)
                        headers))
           ;; RFC 7231 §7.1.1.2: origin server MUST send Date.
           (headers (if (assoc "date" headers :test #'string-equal)
                        headers
                        (cons (cons "date" (http-date)) headers))))
      (serialize-http-message
       (format nil "HTTP/1.1 ~d ~a" status (status-reason status))
       headers
       nil))))

;;; ---------------------------------------------------------------------------
;;; The app-facing stream
;;;
;;; A handler returns a STREAM-RESPONSE instead of an HTTP-RESPONSE. The
;;; framework sends the head, moves the connection to :STREAMING, and
;;; hands the connection to ON-OPEN. From there the app calls STREAM-SEND
;;; as it has something to say and STREAM-CLOSE when it does not.
;;;
;;; The connection is the handle, which is the shape WS-SEND already
;;; established — one fewer object for an app to hold, and no way for a
;;; handle to outlive the thing it refers to.
;;;
;;; Producing from another thread is out of scope, deliberately: nothing
;;; here is synchronized, and the event loop owning the connection is
;;; what makes the write queue safe without a lock. An app that wants
;;; fan-out holds its own registry and pushes from the worker that owns
;;; each connection.
;;; ---------------------------------------------------------------------------

(defparameter *stream-idle-timeout* 300
  "Seconds a :STREAMING connection may go without the app producing
   anything before it is closed. Default 5 minutes. Zero disables it.

   Separate from *IDLE-TIMEOUT* because they are different judgments
   about different things. An HTTP connection idle for ten seconds has
   most likely gone away; a stream idle for ten seconds is usually a
   stream, and reaping it would make the feature useless. Separate from
   *WS-IDLE-TIMEOUT* for the same reason in the other direction — a day
   is far too long to hold a connection whose producer has quietly
   stopped.

   Distinct again from *WRITE-STALL-TIMEOUT*, which asks whether bytes
   are leaving. This one asks whether any are arriving to send. A stream
   can be perfectly healthy at the socket and dead at the source.

   Measured on CONNECTION-STREAM-PRODUCED-AT and deliberately not on
   LAST-ACTIVE, or that distinction would be false exactly when it
   matters: HANDLE-CLIENT-WRITE bumps LAST-ACTIVE on EPOLLOUT entry, so a
   draining backlog would refresh the deadline of a stream whose producer
   had stopped, and the knob would quietly be measuring the other one's
   question.

   A keepalive refreshes it, so a stream that emits keepalives is never
   reaped by this — which is the point of having one.")

(defparameter *stream-keepalive-interval* 30
  "Seconds of quiet before a :STREAMING connection is sent its keepalive
   bytes, if it has any. Zero disables keepalives entirely.

   The bytes come from the STREAM-RESPONSE, because there is no generic
   keepalive to send. A chunked stream has no idle form of its own: the
   only zero-content thing it can emit is the empty chunk, and that is
   the terminator. Anything that keeps a stream warm has to be content
   at the layer above — an SSE comment line is the standard one.")

(defstruct (stream-response (:constructor %make-stream-response))
  (response  nil)                      ; HTTP-RESPONSE — status and headers
  (on-open   nil :type (or null function))
  (on-close  nil :type (or null function))
  (keepalive nil :type (or null (simple-array (unsigned-byte 8) (*)))))

(defun make-stream-response (&key (status 200) headers on-open on-close keepalive)
  "A response whose body the app produces over time.

   Return one from a handler the way you would return an HTTP-RESPONSE.
   The framework serializes the head — no Content-Length, framing chosen
   from the client's HTTP version — and then calls ON-OPEN with the
   connection.

   ON-OPEN   (CONN)          — the stream is live; send if you have
                               something now, or hand CONN to whatever
                               will produce later.
   ON-CLOSE  (CONN REASON)   — fires exactly once, however the stream
                               ends: :DONE when the app closed it,
                               :DISCONNECTED when the peer went away,
                               :IDLE or :STALLED when a deadline took it,
                               :SHUTDOWN on server drain. Release
                               whatever the app registered here.
   KEEPALIVE (bytes or NIL)  — sent when the stream goes quiet. See
                               *STREAM-KEEPALIVE-INTERVAL* for why the
                               framework cannot invent these.

   HEADERS is an alist. Content-Length and Transfer-Encoding are refused
   — the framework owns framing for a stream."
  ;; An empty keepalive fires on schedule and sends nothing, forever:
  ;; FRAME-STREAM-BYTES returns NIL for an empty payload on both paths,
  ;; correctly, and the sweep skips it. The only symptom would be streams
  ;; dying behind a proxy — which is what a keepalive is for, so it looks
  ;; like the keepalive is not working rather than not existing. NIL is
  ;; how you decline one.
  (when (and keepalive (zerop (length keepalive)))
    (error "make-stream-response: :keepalive is empty — that sends nothing ~
            on every interval forever. Pass NIL to decline a keepalive."))
  (let ((resp (make-http-response :status status)))
    (loop for (name . value) in headers
          do (set-response-header resp name value))
    (%make-stream-response :response resp
                           :on-open on-open
                           :on-close on-close
                           :keepalive keepalive)))

(defun stream-send (conn bytes)
  "Send BYTES on CONN's stream. Returns T if everything reached the
   kernel, NIL if a remainder is queued for the event loop.

   Does not block, and a NIL return is what a slow peer looks like
   rather than an error — the same contract as WS-SEND, for the same
   reason. Framing is applied here: a :CHUNKED stream gets each call
   framed as one chunk, a :CLOSE stream sends the bytes as they are.

   An empty BYTES is a no-op on both paths and returns T. On the chunked
   path that is ENCODE-CHUNK's rule; on the close-delimited path there
   is simply nothing to write.

   Signals if the connection is not streaming, or if it is already at
   *MAX-WRITE-BACKLOG* — the frame is not queued, not truncated, and the
   caller learns the peer is too far behind rather than discovering it
   as a gap the peer can never detect."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (unless (eq (connection-state conn) :streaming)
    (error "stream-send: fd ~d is in state ~a, not :streaming"
           (connection-fd conn) (connection-state conn)))
  (let ((framed (frame-stream-bytes conn bytes)))
    (cond
      ((null framed) t)
      ((connection-append-write conn framed)
       ;; The production clock, not LAST-ACTIVE — see
       ;; CONNECTION-STREAM-PRODUCED-AT.
       (setf (connection-stream-produced-at conn) (get-universal-time))
       (stream-flush conn))
      (t
       (error "stream-send: fd ~d is at *max-write-backlog* (~d bytes ~
               pending, this send is ~d); the peer is not draining."
              (connection-fd conn)
              (connection-write-pending conn)
              (length framed))))))

(defun frame-stream-bytes (conn bytes)
  "Frame BYTES for CONN's stream, or NIL when there is nothing to send.

   The one place a stream's framing is applied, because there is more
   than one writer and they land in the same body. The keepalive sweep
   is the other, and it appended raw bytes until it was found doing so —
   on a chunked stream that put a comment line where the peer's decoder
   expects a chunk-size, which kills the stream the keepalive exists to
   keep alive. Two writers deciding framing separately is the same
   hazard as two readers deciding acceptance separately, and this
   codebase already names that one."
  (ecase (connection-stream-framing conn)
    (:chunked (encode-chunk bytes))
    ;; Copied, not passed through. The queue holds vectors by reference
    ;; and advances an offset through them, so handing it the app's own
    ;; buffer means anything written into that buffer before it drains
    ;; goes out instead of what was sent. Reusing one buffer is the
    ;; obvious way to write a producer, and without this the rule for
    ;; whether that is allowed would depend on the client's HTTP version
    ;; — chunked copies here because ENCODE-CHUNK builds a new vector,
    ;; close framing did not. An app cannot see which framing it got, so
    ;; it would be correct against every browser and corrupt against one
    ;; old client.
    (:close (when (plusp (length bytes)) (copy-seq bytes)))))

(defun stream-flush (conn)
  "Write what the socket will take now. Returns T if the queue emptied,
   NIL if a remainder is left for the event loop — in which case
   EPOLLOUT is armed so there is an event to finish it on.

   The arming is conditional and self-limiting: it happens only when a
   send does not fully flush, which means the socket is genuinely backed
   up. A peer keeping up costs no epoll_ctl at all, the same property
   the ping sweep gained when its write moved inline.

   EPOLLIN stays in the mask because a stream still has to notice its
   peer going away — that is the only read event a :STREAMING connection
   cares about, and missing it means producing into a socket nobody is
   on the other end of."
  (let ((done (eq (connection-on-write conn) :done)))
    (unless (or done (null *epoll-fd*))
      (epoll-modify *epoll-fd* (connection-fd conn)
                    (logior +epollin+ +epollout+ +epollet+)))
    done))

(defun notify-stream-closed (conn reason)
  "Fire CONN's stream ON-CLOSE exactly once, with REASON.

   Nulls the slot before calling, not after: a callback that signals
   must not leave the slot armed for a second delivery from whatever
   handles the signal. The fetch callback contract is kept the same way
   and for the same reason — an app that releases a resource twice is
   worse off than one that never hears."
  (let ((fn (connection-stream-on-close conn)))
    (when fn
      (setf (connection-stream-on-close conn) nil)
      (handler-case (funcall fn conn reason)
        (error (e)
          (log-warn "stream on-close signalled on fd ~d (~a): ~a"
                    (connection-fd conn) reason e))))))

(defun stream-close (conn)
  "End the stream on CONN normally.

   Emits the terminator when the framing needs one, fires ON-CLOSE with
   :DONE, and hands the connection back to the ordinary write path —
   which flushes what is queued and then either resets for keep-alive or
   closes, exactly as it does for a non-streaming response.

   This is the only route from :STREAMING back to :READ-HTTP, which is
   what enforces the rule that a chunked body cannot be reused as a
   connection without its terminator having been written. Every other
   way a stream can end goes through CLOSE-CONNECTION and takes the
   socket with it."
  (unless (eq (connection-state conn) :streaming)
    (error "stream-close: fd ~d is in state ~a, not :streaming"
           (connection-fd conn) (connection-state conn)))
  (when (eq (connection-stream-framing conn) :chunked)
    (unless (connection-append-write conn (chunked-terminator))
      ;; No room for five bytes means the peer is hopelessly behind. The
      ;; body cannot be terminated, so the connection must not survive to
      ;; be reused — an unterminated chunked body followed by a fresh
      ;; response is the smuggling shape this all exists to avoid.
      (log-warn "stream-close: no room for the terminator on fd ~d — ~
                 closing rather than reusing" (connection-fd conn))
      (setf (connection-close-after-p conn) t)))
  ;; State first, notification second. STREAM-SEND's guard is what stops
  ;; a callback appending after the terminator, and it can only fire if
  ;; the state has already moved — otherwise a goodbye event sent from
  ;; ON-CLOSE lands behind the terminator, and on a keep-alive connection
  ;; those bytes prefix the next response. That is the smuggling shape
  ;; this file's header comment exists to prevent, manufactured from
  ;; inside the callback announcing the stream is over.
  ;;
  ;; CLOSE-CONNECTION deliberately does the opposite, and its comment is
  ;; right there: the fd is about to close, so nothing can be appended
  ;; and the callback may as well see a live connection. Here the
  ;; connection survives, which is exactly what makes that ordering
  ;; unsafe. The reasoning does not transplant.
  (setf (connection-stream-framing conn) nil
        (connection-stream-keepalive conn) nil
        (connection-state conn) :write-response)
  (notify-stream-closed conn :done)
  (stream-flush conn)
  (values))

;;; ---------------------------------------------------------------------------
;;; Server-Sent Events
;;;
;;; Field validation follows VALIDATE-COOKIE-FIELD's register — refuse
;;; before building anything, one named validator, a docstring naming the
;;; grammar and the consequence. The register transplants; the character
;;; set does not, and the two differ in every particular that matters:
;;;
;;;   BUILD-COOKIE guards a value going into a header the framework
;;;   serializes. Its delimiters are ';' and CR/LF, and a bad value is
;;;   header injection.
;;;
;;;   SSE guards a value going into a body whose framing is already
;;;   committed. Its delimiter is a line break — LF, CR, or CRLF, all
;;;   three per the EventSource spec — and a bad value is *event*
;;;   injection: a client dispatching an event the app never sent, with
;;;   fields it never wrote.
;;;
;;; NUL is refused here for a third reason again. It is not a delimiter;
;;; the spec has the client silently ignore an id containing one, so the
;;; last-event-ID never updates and a reconnect replays from the wrong
;;; point — a failure with no symptom until the reconnect happens.
;;; ---------------------------------------------------------------------------

(defun validate-sse-field (kind value &key allow-lf)
  "Reject characters that would let VALUE restructure the event stream.

   CR and LF both end a line for EventSource, so either one inside a
   field value ends the field early and hands whatever follows to the
   client as a new field — or, on a blank line, dispatches an event the
   app never wrote. NUL is rejected because the spec has the client drop
   an id containing one on the floor, which turns a reconnect into a
   replay from the wrong position with nothing raised anywhere.

   DATA is the one field where a line break is legitimate and is handled
   rather than refused — see SSE-EVENT-BYTES — but only LF. A lone CR
   cannot survive the round trip in any case: the client would rejoin
   consecutive data lines with LF, so passing one through would silently
   rewrite the app's bytes."
  ;; Named rather than left to FIND's type error. An integer id is the
  ;; most natural thing an app passes — sequence numbers are what
  ;; Last-Event-ID replay is for — and "The value 42 is not of type
  ;; SEQUENCE" names neither the argument nor the fix. RETRY already had
  ;; a check of its own; three of the four fields did not.
  (unless (stringp value)
    (error "sse: ~a must be a string, got ~s. Field values go on the wire ~
            as text; convert first (WRITE-TO-STRING for a number)."
           kind value))
  (when (find #\Return value)
    (error "sse: ~a contains a CR — EventSource treats it as a line ~
            terminator, so it would end the field early" kind))
  (unless allow-lf
    (when (find #\Newline value)
      (error "sse: ~a contains an LF — only data may span lines" kind)))
  (when (find (code-char 0) value)
    (error "sse: ~a contains a NUL — the client would discard the field ~
            silently" kind)))

(defun sse-comment-bytes (&optional (text ""))
  "A comment line: ':' TEXT LF. Ignored by every client, which is what
   makes it the keepalive.

   The one SSE emission carrying no data, and deliberately its own
   function rather than an empty event. An event with no data is not
   dispatched at all — the spec returns early on an empty data buffer —
   so routing a keepalive through SSE-SEND would produce bytes no client
   acts on. It also must never encode to nothing: ENCODE-CHUNK treats an
   empty payload as nothing to send, correctly, and an empty keepalive
   would silently stop keeping anything alive. The bare form is still two
   bytes."
  (when (plusp (length text))
    (validate-sse-field "comment" text))
  (sb-ext:string-to-octets (format nil ":~a~c" text #\Newline)
                           :external-format :utf-8))

(defun sse-event-bytes (&key data event id retry)
  "Serialize one Server-Sent Event.

   DATA is required: EventSource does not dispatch an event whose data
   buffer is empty, so an event without it would be delivered to nobody
   while looking sent from here. A line break in DATA is legitimate and
   becomes one `data:` line per segment, which is how the protocol
   carries multi-line payloads; the client rejoins them with LF.

   Everything is validated before anything is built, so a refused event
   queues nothing and leaves the stream exactly as it was. Half an event
   on the wire would be worse than no event: the client would splice it
   onto whatever came next."
  ;; Type before emptiness: (LENGTH 7) raises on the way to the length
  ;; check, and the caller learns about SEQUENCE rather than about data.
  (when (and data (not (stringp data)))
    (error "sse: data must be a string, got ~s. Field values go on the ~
            wire as text; convert first (WRITE-TO-STRING for a number)."
           data))
  (unless (and data (plusp (length data)))
    (error "sse: an event needs data — EventSource does not dispatch one ~
            with an empty data buffer. Use SSE-COMMENT for a keepalive."))
  (when event (validate-sse-field "event" event))
  (when id (validate-sse-field "id" id))
  (validate-sse-field "data" data :allow-lf t)
  (when retry
    (unless (and (integerp retry) (not (minusp retry)))
      (error "sse: retry must be a non-negative integer of milliseconds, ~
              got ~s" retry)))
  (sb-ext:string-to-octets
   (with-output-to-string (out)
     (when event (format out "event: ~a~c" event #\Newline))
     (when id (format out "id: ~a~c" id #\Newline))
     (when retry (format out "retry: ~d~c" retry #\Newline))
     (loop with start = 0
           for nl = (position #\Newline data :start start)
           do (format out "data: ~a~c"
                      (subseq data start (or nl (length data))) #\Newline)
              (if nl (setf start (1+ nl)) (return)))
     (write-char #\Newline out))
   :external-format :utf-8))

(defun make-sse-response (&key headers on-open on-close (keepalive t))
  "A streaming response carrying Server-Sent Events.

   Sets the three headers this needs and lets them win over HEADERS: the
   content type defines the protocol, `cache-control: no-cache` keeps an
   intermediary from serving a stale prefix of an infinite response, and
   `x-accel-buffering: no` turns off nginx's buffering, which is on by
   default for this content type and would otherwise hold events until a
   buffer filled — a stream delivered in batches is not a stream. The
   deployment story here assumes a proxy in front, so the header is not
   optional decoration. An app that needs different proxy hints can build
   a MAKE-STREAM-RESPONSE directly.

   KEEPALIVE T installs a bare comment line, sent whenever the stream
   goes quiet for *STREAM-KEEPALIVE-INTERVAL*. That is what stops an
   intermediary reaping an idle stream, and it is why the framework could
   not supply a generic one: at the chunked layer the only zero-content
   emission is the terminator."
  (make-stream-response
   :status 200
   :headers (append headers
                    (list (cons "content-type" "text/event-stream")
                          (cons "cache-control" "no-cache")
                          (cons "x-accel-buffering" "no")))
   :on-open on-open
   :on-close on-close
   :keepalive (when keepalive (sse-comment-bytes))))

(defun sse-send (conn &key data event id retry)
  "Send one Server-Sent Event on CONN. Returns what STREAM-SEND returns:
   T if everything reached the kernel, NIL if a remainder is queued.

   Refuses whole. The event is serialized and validated before a byte is
   queued, so a rejected field leaves the stream well-formed and short
   rather than half-written — the same discipline CONNECTION-APPEND-WRITE
   applies to a frame that will not fit, one layer up."
  (stream-send conn (sse-event-bytes :data data :event event
                                     :id id :retry retry)))

(defun sse-comment (conn &optional (text ""))
  "Send a comment line on CONN. Clients ignore it; intermediaries do not,
   which is the point of sending one."
  (stream-send conn (sse-comment-bytes text)))

(defun stream-full-p (conn)
  "True when CONN's queue is at *MAX-WRITE-BACKLOG* and STREAM-SEND
   would signal. A producer that can pause should ask before generating
   an expensive payload — but this is a hint about bytes already queued,
   not a promise about the next send, which only STREAM-SEND's own
   return can give. See CONNECTION-WRITE-FULL-P."
  (connection-write-full-p conn))
