(in-package :web-skeleton)

;;; ===========================================================================
;;; Chunked transfer coding — the decoders, shared by both directions
;;;
;;; These two walk the same wire format the response side writes and the
;;; request side reads, and they live here rather than in FETCH because of
;;; the compile order: FETCH depends on CONNECTION, so a decoder defined
;;; there is invisible to the inbound state machine that needs it. The
;;; alternative was a second walk inbound, which is the disagreement this
;;; codebase is organised against — two readers of one framing, differing
;;; about where a body ends.
;;;
;;; Both are pure byte walks over a buffer. Nothing here knows about
;;; connections, sockets, or which direction the bytes are travelling; the
;;; dependencies are HEX-DIGIT-VALUE and *MAX-HEADER-LINE-LENGTH*, the cap
;;; on a size line. The policy — what a caller does with "not yet", and
;;; what it does with a trailer section — belongs to the caller, which is
;;; why neither of these has an opinion about it.
;;;
;;; They accept the same framing, not merely the same well-formed framing.
;;; Both read a size line through PARSE-CHUNK-SIZE-BYTES and both require
;;; the CRLF after each chunk's data, so a body one of them refuses, the
;;; other refuses too. That matters most where the decoder never runs: a
;;; consumer of the walk's ON-DATA has been handed each chunk already, and
;;; the walk is then the only reader the bytes ever meet.
;;;
;;; What they deliberately do NOT do: consume the trailer section. Both
;;; stop at the zero-size chunk header. That is correct for an outbound
;;; response, where nothing follows the body; inbound, on a connection
;;; that will be reused, the bytes past that point are the next request's
;;; and the caller has to say where the request really ended.
;;; ===========================================================================

(define-condition chunked-framing-error (error)
  ((message :initarg :message :reader chunked-framing-error-message))
  (:report (lambda (c s) (write-string (chunked-framing-error-message c) s)))
  (:documentation
   "Chunked framing this file has decided is wrong.

    Both readers here signal it and nothing else in this file signals at
    all: HEX-DIGIT-VALUE is total, and every AREF is guarded by a bounds
    test, so any *other* condition escaping these functions is a bug in
    them rather than a fault in the bytes.

    That distinction is the whole reason the condition exists. Callers
    convert framing faults into the client's answer — 400 inbound, 502 for
    an upstream — and a caller catching plain ERROR would hand a client a
    400 for a defect in this code, which is the one thing that must not
    happen in the middle of a file whose job is deciding whose fault a
    malformed message is."))

(defun chunked-error (format-string &rest args)
  "Signal a CHUNKED-FRAMING-ERROR with a formatted message."
  (error 'chunked-framing-error
         :message (apply #'format nil format-string args)))

(defun parse-chunk-size-bytes (buf pos end)
  "Read the chunk-size line that begins at POS in BUF[POS..END).
   Returns (values SIZE NEXT), NEXT being the offset just past the line's
   CRLF, or (values NIL NIL) while the line has not all arrived. Signals
   CHUNKED-FRAMING-ERROR for a line that no further byte could make
   valid, as soon as the bytes that show it are here.

   The one reading of a size line for CHUNKED-BODY-COMPLETE-P and
   DECODE-CHUNKED-BODY, so the walk that decides where a body ends and
   the decoder that delivers it cannot accept different lines. The line
   is

     1*16HEXDIG *( SP / HTAB ) [ \";\" chunk-ext ] CRLF

   where the chunk-ext is any bytes but CR and LF and is skipped, not
   interpreted, and the line before its CRLF is at most
   *MAX-HEADER-LINE-LENGTH* bytes. That is also the line
   STREAM-CHUNKED-LINES accepts, through READER-READ-CRLF-LINE and
   PARSE-CHUNKED-SIZE-LINE, so every chunked reader in the framework
   reads a size line the same way. Whitespace may follow the digits and
   never precede them: RFC 9112 §7.1.1 allows it before an extension's
   ';', and the readers also tolerate it before the CRLF.

   A CR or LF anywhere but the line's own CRLF is refused rather than
   read as its end. A reader that ends the line at a bare CR finds the
   chunk data a byte or more early, and one that ends it at the CRLF
   finds it where the CRLF says. Two hops reading the line those two ways
   disagree about every byte of the body after it, which is the shape of
   a smuggled request, and refusing is the only answer both can agree
   with.

   The length cap is what makes a line that never ends a decided fault
   and not an incomplete one: past it, no CRLF still to come would make
   the line acceptable, and a walk polling a growing buffer would
   otherwise wait for one until some larger bound fired and described
   itself instead."
  (let* ((start pos)
         ;; One past the longest line the cap allows. Nothing here reads
         ;; beyond it, so a line of any length costs at most the cap to
         ;; reject, however often a growing buffer asks.
         (limit (min end (+ start *max-header-line-length* 1)))
         (size 0)
         (digits 0))
    ;; chunk-size: one to sixteen hex digits. Sixteen already names more
    ;; bytes than any cap admits, and past it a hostile run of digits
    ;; would only grow an ever larger integer, each step costlier.
    (loop
      (when (>= pos limit) (return))
      (let ((digit (hex-digit-value (aref buf pos))))
        (unless digit (return))
        (incf digits)
        (when (> digits 16)
          (chunked-error "chunked: chunk-size too many hex digits"))
        (setf size (+ (ash size 4) digit))
        (incf pos)))
    (when (zerop digits)
      ;; Decided, not incomplete, once a byte is here to look at: the loop
      ;; stopped on it, so it is not a hex digit, and a chunk-size has
      ;; nothing before its digits.
      (when (< pos end)
        (chunked-error "chunked: expected a chunk-size, found byte 0x~2,'0x"
                       (aref buf pos)))
      (return-from parse-chunk-size-bytes (values nil nil)))
    ;; BWS after the digits, then an extension that runs to the CR.
    (loop while (and (< pos limit)
                     (let ((b (aref buf pos))) (or (= b 32) (= b 9))))
          do (incf pos))
    (when (and (< pos limit) (= (aref buf pos) 59))
      (loop while (and (< pos limit)
                       (let ((b (aref buf pos))) (and (/= b 13) (/= b 10))))
            do (incf pos)))
    (cond
      ((> (- pos start) *max-header-line-length*)
       (chunked-error "chunked: chunk-size line too long (max ~d)"
                      *max-header-line-length*))
      ((>= pos end) (values nil nil))
      ((= (aref buf pos) 10)
       (chunked-error "chunked: bare LF in chunk-size line"))
      ((/= (aref buf pos) 13)
       (chunked-error "chunked: invalid byte 0x~2,'0x after chunk-size"
                      (aref buf pos)))
      ((>= (1+ pos) end) (values nil nil))
      ((/= (aref buf (1+ pos)) 10)
       (chunked-error "chunked: bare CR in chunk-size line"))
      (t (values size (+ pos 2))))))

(defun chunked-body-complete-p (buf start end &optional (resume start) on-data)
  "Return (values COMPLETE-P NEXT-RESUME) for the chunked body in
   BUF[START..END). COMPLETE-P is T once the zero-size chunk header has
   arrived. Walks the chunk framing, skipping *over* chunk data rather
   than scanning it, so the cost is proportional to the number of chunks
   and not to the bytes.

   RESUME is a NEXT-RESUME returned by an earlier call — an offset that
   is known to sit on a chunk-header boundary with everything before it
   already validated. A caller polling a growing buffer threads it back
   in, so each chunk is walked exactly once over the life of a transfer
   instead of the whole body being rescanned on every read. Without it,
   an upstream that dribbles N chunks costs O(N^2) header steps, which an
   adversarial (if reachable) upstream could turn into real CPU burn
   inside the response-size cap. Default RESUME = START scans from
   scratch, which is what a one-shot caller wants.

   It accepts exactly the framing DECODE-CHUNKED-BODY accepts: each size
   line through PARSE-CHUNK-SIZE-BYTES, and each chunk's data followed by
   CRLF. ON-DATA is why it has to. A consumer of ON-DATA has each chunk
   once the walk has proved it, and a relay never runs the decoder after
   that, so a fault the walk let through would reach the application and
   never be refused at all.

   NIL means only 'keep reading', and the walk answers it only for
   framing that more bytes could still complete. A fault raises as soon
   as the bytes that show it have arrived, and never sooner, so the walk
   neither waits on a body that can no longer become valid nor refuses
   one that has only not finished arriving. Waiting on a decided fault
   would hold the request until some outer cap fired and described itself
   instead of the fault — inbound, a client answered by nothing and closed
   at the idle timeout. Callers convert the raise: 400 inbound, and the
   outbound handler-case's 502.

   It stops at the zero-size chunk header rather than at the trailing
   CRLF, which is exactly where DECODE-CHUNKED-BODY stops too (trailers
   are not consumed), so the two agree on the completion point.

   Beyond COMPLETE-P and NEXT-RESUME it returns three more values, in
   order:

     AFTER-SIZE-LINE  the offset just past the zero-size chunk header's
                   own CRLF, which is where the trailer section begins. It
                   exists so an inbound caller does not have to re-walk
                   that line to find it — two walks of one header line is
                   the second reader this file exists to avoid, even when
                   both would agree. Meaningless when COMPLETE-P is NIL.
     DATA-BYTES    chunk data proved whole by *this* call. RESUME means
                   the walk never revisits a chunk, so a caller polling a
                   growing buffer accumulates these into the decoded total
                   without ever counting a chunk twice.
     PENDING-SIZE  the declared size of a chunk whose data has not all
                   arrived, or 0. Transient, not cumulative: that chunk is
                   counted in DATA-BYTES by the later call that proves it
                   whole. It exists so a caller can refuse a header
                   claiming more than it will ever accept at the moment
                   the header arrives — waiting for the bytes instead only
                   ever reaches whatever answer the read buffer gives.

   Existing callers that take two values are unaffected. None of the three
   is a policy: this file is direction-neutral and the two directions have
   different caps — *MAX-BODY-SIZE* inbound, *MAX-OUTBOUND-RESPONSE-SIZE*
   outbound — so the walk reports sizes and the caller decides what is too
   big.

   RESUME is clamped up to START, never down. That guards a cursor that is
   too low; a cursor that is too *high* — one left over from a previous
   request on a reused connection — passes straight through and starts the
   walk past framing it never validated. Nothing here can detect that, so
   a caller that reuses a connection has to clear its cursor.

   ON-DATA, when supplied, is called (BUF START END) once per chunk whose
   framing this walk has just proved whole — the same visit, handing the
   bytes back instead of only stepping over them. Never called twice for
   a chunk, because RESUME means the walk never revisits one.

   ON-DATA's value is ignored unless it is :HALT, which ends the walk at
   that chunk: COMPLETE-P is NIL and NEXT-RESUME is the following chunk's
   boundary, so a later call resumes there and nothing past the halting
   chunk has been handed over. That is how a consumer applies backpressure
   at the chunk it decided on, rather than after every chunk the buffer
   happened to hold."
  (let ((pos (max start resume))
        (data-bytes 0))
    (loop
      ;; BOUNDARY is the start of the chunk header about to be parsed:
      ;; everything before it is validated framing, so it is the offset
      ;; handed back for the next call to resume from.
      (let ((boundary pos))
        (multiple-value-bind (size next) (parse-chunk-size-bytes buf pos end)
          (unless size
            (return (values nil boundary 0 data-bytes 0)))
          (setf pos next)
          ;; Zero-size chunk header = end of body.
          (when (zerop size)
            ;; POS is past that line's CRLF, which is where the trailer
            ;; section begins — handed back so an inbound caller need not
            ;; re-walk this line to find it.
            (return (values t boundary pos data-bytes 0)))
          ;; Skip the chunk data rather than scanning it, which is what
          ;; keeps a body whose *contents* happen to contain
          ;; "0\\r\\n\\r\\n" from being mistaken for a terminator — a
          ;; naive suffix check would truncate there.
          (let* ((data-start pos)
                 (data-end (+ pos size)))
            ;; The CRLF after the data, each byte judged the moment it
            ;; lands. A chunk whose data is followed by anything else is
            ;; refused before ON-DATA sees it, so no consumer is handed a
            ;; chunk the decoder would have refused.
            (when (and (< data-end end) (/= (aref buf data-end) 13))
              (chunked-error "chunked: expected CRLF after chunk-data"))
            (when (and (< (1+ data-end) end) (/= (aref buf (1+ data-end)) 10))
              (chunked-error "chunked: expected CRLF after chunk-data"))
            (setf pos (+ data-end 2))
            (when (> pos end)
              ;; This chunk's data has not all landed. Its declared SIZE is
              ;; the pending amount, and handing it back is the difference
              ;; between an honest refusal and a stall: a header claiming
              ;; ffffffff bytes is answerable *now*, while waiting for the
              ;; bytes only ever produces the read buffer's answer.
              (return (values nil boundary 0 data-bytes size)))
            ;; Counted only once the framing proves the chunk whole, which
            ;; is also why RESUME makes the sum safe to accumulate across
            ;; calls: the walk never revisits a chunk it has counted.
            (incf data-bytes size)
            ;; The walk has already proved this chunk whole; ON-DATA is how
            ;; the bytes leave without a second pass over them.
            (when (and on-data
                       (eq (funcall on-data buf data-start data-end) :halt))
              ;; POS is the next chunk's boundary, everything before it
              ;; validated, so it is where a later call resumes.
              (return (values nil pos 0 data-bytes 0)))))))))

(defun chunked-body-feed (buf start end owed on-data)
  "Walk the chunked body in BUF[START..END) for a consumer that takes its
   data as it arrives — a chunk's bytes before the whole chunk has landed
   — and hand them to ON-DATA, called (BUF FROM TO REST) once per run of
   data. REST is the data of the same chunk still to come after the run,
   as its size line declared: what a cap on the body needs in order to
   refuse a chunk before any of it is handed on.

   CHUNKED-BODY-COMPLETE-P hands over a chunk only once all of it is
   here, which needs a buffer as large as the largest chunk; a body
   received into an application cannot ask that of the client, since one
   chunk can be the whole upload. This hands over what is here and
   remembers what is owed, so a buffer the size of one read will do
   whatever the chunk sizes.

   OWED is what the previous call returned: the bytes of the current chunk
   still to come, its data and then the CRLF after it, or 0 at a chunk
   boundary. START is where those bytes resume — everything before it was
   walked by an earlier call, and the caller may have dropped it. A first
   call passes the body's start and 0.

   Returns (values COMPLETE-P NEXT OWED DATA-BYTES PENDING AFTER-SIZE-LINE):

     COMPLETE-P       T once the zero-size chunk's line has arrived.
     NEXT             where the next call starts. BUF[START, NEXT) has been
                      walked and handed over; the caller may drop it. When
                      COMPLETE-P, the start of the zero-size line, so that a
                      call made again while the trailer section is still
                      arriving reads that line again rather than past it.
     OWED             the next call's OWED.
     DATA-BYTES       data handed to ON-DATA by this call.
     PENDING          data of the current chunk still to come, which the
                      size line has already declared: what a cap on the
                      body can refuse before the bytes turn up.
     AFTER-SIZE-LINE  when COMPLETE-P, where the trailer section begins,
                      for CHUNKED-TRAILER-STATUS; otherwise 0.

   The framing is the decoder's, as CHUNKED-BODY-COMPLETE-P's is: size
   lines through PARSE-CHUNK-SIZE-BYTES, and the CRLF after each chunk's
   data checked a byte at a time as it lands. A fault raises
   CHUNKED-FRAMING-ERROR once the bytes that show it are here and never
   before, so this neither waits on a body that can no longer become valid
   nor refuses one still arriving.

   What it cannot promise is the walk's: a chunk is handed over before the
   CRLF after it has been seen, so a body that turns out malformed has had
   some of its bytes delivered already. A consumer holds what it is given
   as provisional until the body completes, the same as for a
   Content-Length body cut short by its client.

   ON-DATA's value is ignored unless it is :HALT, which ends the walk
   there: NEXT is just past the bytes handed over, and a later call
   resumes from it with nothing handed over twice.

   A run is a chunk's bytes within one call, so a body of small chunks is
   many runs. CONNECTION-DELIVER-BODY gathers a read's runs into one
   before its feed sees them; a consumer handed the runs one by one is
   handed the sender's chunking with them."
  (let ((pos start)
        (data-bytes 0))
    (flet ((pending () (max 0 (- owed 2))))
      (loop
        (if (plusp owed)
            ;; Inside a chunk: as much of its data as is here, then its CRLF.
            (let ((take (min (pending) (- end pos))))
              (when (plusp take)
                (let ((from pos))
                  (incf pos take)
                  (decf owed take)
                  (incf data-bytes take)
                  (when (and on-data
                             (eq (funcall on-data buf from pos (pending)) :halt))
                    (return (values nil pos owed data-bytes (pending) 0)))))
              ;; The CRLF, each byte judged as it lands.
              (loop while (and (<= 1 owed 2) (< pos end))
                    do (unless (= (aref buf pos) (if (= owed 2) 13 10))
                         (chunked-error "chunked: expected CRLF after chunk-data"))
                       (incf pos)
                       (decf owed))
              (when (plusp owed)
                ;; The bytes ran out inside this chunk.
                (return (values nil pos owed data-bytes (pending) 0))))
            ;; At a chunk boundary: its size line.
            (let ((boundary pos))
              (multiple-value-bind (size next) (parse-chunk-size-bytes buf pos end)
                (unless size
                  (return (values nil boundary 0 data-bytes 0 0)))
                (when (zerop size)
                  (return (values t boundary 0 data-bytes 0 next)))
                (setf pos next
                      owed (+ size 2)))))))))

(defun decode-chunked-body (buf start end)
  "Decode chunked transfer encoding from BUF[START..END).
   Returns a byte vector with the chunk framing stripped.

   Raises on truncation: the zero-size chunk header (0 CRLF) is the
   only permitted exit. A response that runs out of bytes before
   the terminator (MITM RST, short read, upstream crash mid-body)
   is reported as an error so the caller's outer handler-case can
   convert it into a 502 or fire the fetch cleanup callback — the
   same discipline as the Content-Length truncation guard in
   COMPLETE-FETCH and STREAM-RESPONSE-LINES, applied to the chunked
   path which has no Content-Length to compare against.

   Also requires strict CRLF after both chunk-size and chunk-data
   per RFC 7230 §4.1. Lax trailing CRLF is a smuggling primitive
   against a strict downstream that re-parses the body bytes. The size
   line is read by PARSE-CHUNK-SIZE-BYTES, the reading
   CHUNKED-BODY-COMPLETE-P uses, so a body this refuses never reaches a
   consumer of that walk either."
  (let ((out (make-array (- end start) :element-type '(unsigned-byte 8)
                                        :fill-pointer 0))
        (pos start)
        (terminated nil))
    (loop
      (multiple-value-bind (size next) (parse-chunk-size-bytes buf pos end)
        (unless size
          ;; The bytes ran out before a whole size line: the upstream
          ;; closed mid-stream, or the body never included its zero-size
          ;; terminator. Both are truncation, refused below.
          (return))
        (setf pos next)
        ;; Zero-size chunk = end (the only clean exit from this loop).
        (when (zerop size)
          (setf terminated t)
          (return))
        ;; Copy chunk data. Short-read here is truncation: we declared
        ;; SIZE bytes and need exactly that many.
        (when (> (+ pos size) end)
          (chunked-error "chunked: short chunk-data (~d of ~d bytes)"
                 (- end pos) size))
        ;; REPLACE rather than a byte at a time. OUT is pre-sized to the
        ;; whole input span above and a decoded body is never larger than
        ;; its encoding, so it can never need to extend — which made
        ;; VECTOR-PUSH-EXTEND a per-byte function call buying nothing, and
        ;; roughly a million of them for a 1 MiB body on the inbound
        ;; request path.
        ;;
        ;; It also retires a latent portability question: OUT is created
        ;; with a fill pointer and no :ADJUSTABLE T, and VECTOR-PUSH-EXTEND
        ;; on a non-adjustable vector is undefined per CLHS. It happens to
        ;; work in SBCL, and every other accumulator in this tree passes
        ;; :ADJUSTABLE T. REPLACE makes the question moot instead of
        ;; answering it.
        ;;
        ;; Fill pointer first, then copy. REPLACE treats the fill pointer
        ;; as the vector's active end, so copying to :START1 (FILL-POINTER
        ;; OUT) before moving it writes into no active region at all and
        ;; silently does nothing.
        (let ((dest (fill-pointer out)))
          (setf (fill-pointer out) (+ dest size))
          (replace out buf :start1 dest :start2 pos :end2 (+ pos size)))
        (incf pos size)
        ;; Require strict CRLF after chunk-data (RFC 7230 §4.1). Taking
        ;; a CR, an LF or nothing at all as the end of the data would
        ;; read 5\r\nhellonext... (no CRLF between the data and the next
        ;; chunk-size) differently from a strict downstream that
        ;; re-parses the body bytes, which is a smuggling primitive.
        (unless (and (<= (+ pos 2) end)
                     (= (aref buf pos) 13)
                     (= (aref buf (1+ pos)) 10))
          (chunked-error "chunked: expected CRLF after chunk-data"))
        (incf pos 2)))
    (unless terminated
      (chunked-error "chunked: incomplete response (no zero-size terminator)"))
    (subseq out 0 (fill-pointer out))))

(defun chunked-trailer-status (buf pos end)
  "Classify the trailer section beginning at POS in BUF[POS..END).
   POS is CHUNKED-BODY-COMPLETE-P's third value.

   Returns (values STATUS OFFSET):

     :EMPTY       there was no trailer section; OFFSET is one past its
                  terminating CRLF, and is where the request ends.
     :PRESENT     a trailer field begins at OFFSET.
     :INCOMPLETE  not enough bytes have arrived to tell; OFFSET is POS.
     :MALFORMED   a bare CR sits where the terminator should; OFFSET is POS.

   Framing only. Whether a trailer section is acceptable is the caller's
   decision — outbound, nothing follows the body and the question does not
   arise; inbound, on a connection that will be reused, refusing or
   consuming it changes where the *next* request starts, which is why this
   answers the fact and not the policy.

   The two-byte wait is the whole point of the function. A body whose last
   bytes are `0 CRLF` and nothing else is NOT complete inbound: the CRLF
   that terminates an empty trailer section has not arrived. Answering
   :EMPTY there would put the request's end two bytes early, and those two
   bytes would be shifted to offset 0 and read as the start of the next
   request — a smuggled request, manufactured by us."
  (cond
    ;; Nothing past the size line yet.
    ((>= pos end) (values :incomplete pos))
    ;; Anything that is not a CR starts a trailer field. No need to wait
    ;; for more bytes to know that much.
    ((/= (aref buf pos) 13) (values :present pos))
    ;; A CR whose LF has not landed — the trailer terminator's own CR,
    ;; arriving split. Not the `...0 CRLF` case: that one has nothing past
    ;; the size line at all and is the first branch.
    ((>= (1+ pos) end) (values :incomplete pos))
    ;; CRLF: the empty trailer section, and the request ends past it.
    ((= (aref buf (1+ pos)) 10) (values :empty (+ pos 2)))
    ;; CR followed by something else.
    (t (values :malformed pos))))
