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
