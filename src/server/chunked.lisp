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
;;; only dependency is HEX-DIGIT-VALUE. The policy — what a caller does
;;; with "not yet", and what it does with a trailer section — belongs to
;;; the caller, which is why neither of these has an opinion about it.
;;;
;;; What they deliberately do NOT do: consume the trailer section. Both
;;; stop at the zero-size chunk header. That is correct for an outbound
;;; response, where nothing follows the body; inbound, on a connection
;;; that will be reused, the bytes past that point are the next request's
;;; and the caller has to say where the request really ended.
;;; ===========================================================================

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

   This is a 'do we have it all yet?' predicate, not a validator, and it
   is deliberately permissive about malformed framing: DECODE-CHUNKED-BODY
   is the authority and rejects bad framing loudly when the response is
   delivered (which becomes a 502). NIL here only ever means 'keep
   reading' — so a too-strict predicate would hang, while a too-lax one
   merely reaches a loud decode error. Lean lax.

   It stops at the zero-size chunk header rather than at the trailing
   CRLF, which is exactly where DECODE-CHUNKED-BODY stops too (trailers
   are not consumed), so the two agree on the completion point.

   A third value, AFTER-SIZE-LINE, is returned with COMPLETE-P: the offset
   just past the zero-size chunk header's own CRLF, which is where the
   trailer section begins. It exists so an inbound caller does not have to
   re-walk that line to find it — two walks of one header line is the
   second reader this file exists to avoid, even when both would agree.
   Meaningless when COMPLETE-P is NIL, and existing callers that take two
   values are unaffected.

   RESUME is clamped up to START, never down. That guards a cursor that is
   too low; a cursor that is too *high* — one left over from a previous
   request on a reused connection — passes straight through and starts the
   walk past framing it never validated. Nothing here can detect that, so
   a caller that reuses a connection has to clear its cursor.

   ON-DATA, when supplied, is called (BUF START END) once per chunk whose
   framing this walk has just proved whole — the same visit, handing the
   bytes back instead of only stepping over them. Never called twice for
   a chunk, because RESUME means the walk never revisits one."
  (let ((pos (max start resume)))
    (loop
      ;; BOUNDARY is the start of the chunk header about to be parsed:
      ;; everything before it is validated framing, so it is the offset
      ;; handed back for the next call to resume from.
      (let ((boundary pos)
            (size 0)
            (digits 0)
            (found nil))
        ;; chunk-size — at least one hex digit, capped like the decoder's.
        (loop
          (when (>= pos end) (return))
          (let ((digit (hex-digit-value (aref buf pos))))
            (unless digit (return))
            (incf digits)
            (when (> digits 16)
              (return-from chunked-body-complete-p (values nil boundary)))
            (setf size (+ (ash size 4) digit)
                  found t)
            (incf pos)))
        (unless found
          (return (values nil boundary)))
        ;; Skip any chunk-extensions; the size line's LF must have landed.
        (let ((lf (position 10 buf :start pos :end end)))
          (unless lf (return (values nil boundary)))
          (setf pos (1+ lf)))
        ;; Zero-size chunk header = end of body.
        (when (zerop size)
          ;; POS is past that line's LF, which is where the trailer
          ;; section begins — handed back so an inbound caller need not
          ;; re-walk this line to find it.
          (return (values t boundary pos)))
        ;; Skip the chunk data and its trailing CRLF. Note this jumps the
        ;; data rather than scanning it, which is what keeps a body whose
        ;; *contents* happen to contain "0\\r\\n\\r\\n" from being mistaken
        ;; for a terminator — a naive suffix check would truncate there.
        (let ((data-start pos))
          (incf pos size)
          (incf pos 2)
          (when (> pos end)
            (return (values nil boundary)))
          ;; The walk has already proved this chunk whole; ON-DATA is how
          ;; the bytes leave without a second pass over them.
          (when on-data
            (funcall on-data buf data-start (+ data-start size))))))))

(defun decode-chunked-body (buf start end)
  "Decode chunked transfer encoding from BUF[START..END).
   Returns a byte vector with the chunk framing stripped.

   Raises on truncation: the zero-size chunk header (0 CRLF) is the
   only permitted exit. A response that runs out of bytes before
   the terminator (MITM RST, short read, upstream crash mid-body)
   is reported as an error so the caller's outer handler-case can
   convert it into a 502 or fire the fetch cleanup callback — the
   same discipline as the Content-Length truncation guard in
   COMPLETE-FETCH and HTTPS-FETCH, applied to the chunked path
   which has no Content-Length to compare against.

   Also requires strict CRLF after both chunk-size and chunk-data
   per RFC 7230 §4.1. Lax trailing CRLF is a smuggling primitive
   against a strict downstream that re-parses the body bytes."
  (let ((out (make-array (- end start) :element-type '(unsigned-byte 8)
                                        :fill-pointer 0))
        (pos start)
        (terminated nil))
    (loop
      ;; Parse chunk size (hex digits)
      (let ((size 0)
            (found nil)
            (digits 0))
        (loop while (< pos end)
              do (let ((digit (hex-digit-value (aref buf pos))))
                   (if digit
                       (progn (incf digits)
                              (when (> digits 16)
                                (error "chunked: chunk-size too many hex digits"))
                              (setf size (+ (ash size 4) digit)
                                    found t)
                              (incf pos))
                       (return))))
        (unless found
          ;; Ran out of bytes before seeing a chunk-size header.
          ;; Either the upstream closed mid-stream (truncation) or
          ;; the response never included the 0-chunk terminator.
          ;; Both are incomplete.
          (return))
        ;; RFC 7230 §4.1.1 lists only ';' (chunk-ext start), SP /
        ;; HTAB (BWS tolerance), and CR (start of CRLF) as legal
        ;; bytes after the hex digits of a chunk-size. Anything
        ;; else is rejected symmetrically with PARSE-CHUNKED-SIZE-LINE
        ;; on the streaming path — without this check '5g\r\n'
        ;; would silently parse as chunk-size 5 with 'g' as an
        ;; implicit extension and become a smuggling primitive
        ;; against a stricter downstream.
        (when (< pos end)
          (let ((b (aref buf pos)))
            (unless (or (= b 59)  ; ';'
                        (= b 32)  ; SP
                        (= b 9)   ; HTAB
                        (= b 13)) ; CR
              (error "chunked: invalid byte 0x~2,'0x after chunk-size" b))))
        ;; Require strict CRLF after the chunk-size (RFC 7230 §4.1).
        ;; Any chunk extensions between the hex digits and CRLF are
        ;; passed through untouched — we scan for the LF and verify
        ;; the preceding byte is CR so a bare-LF or truncated line
        ;; cannot slip through as "5junk<LF>data".
        (let ((eol pos))
          (loop while (and (< eol end) (/= (aref buf eol) 10))
                do (incf eol))
          (unless (and (< eol end)
                       (> eol pos)
                       (= (aref buf (1- eol)) 13))
            (error "chunked: expected CRLF after chunk-size"))
          (setf pos (1+ eol)))
        ;; Zero-size chunk = end (the only clean exit from this loop).
        (when (zerop size)
          (setf terminated t)
          (return))
        ;; Copy chunk data. Short-read here is truncation: we declared
        ;; SIZE bytes and need exactly that many.
        (when (> (+ pos size) end)
          (error "chunked: short chunk-data (~d of ~d bytes)"
                 (- end pos) size))
        (loop for i from pos below (+ pos size)
              do (vector-push-extend (aref buf i) out))
        (incf pos size)
        ;; Require strict CRLF after chunk-data (RFC 7230 §4.1).
        ;; The lax \r-or-\n-or-nothing accept-anything behaviour was
        ;; a smuggling primitive: a response shaped 5\r\nhellonext...
        ;; (no CRLF between the data and the next chunk-size) would
        ;; be decoded differently by web-skeleton and a strict
        ;; downstream that re-parsed the body bytes.
        (unless (and (<= (+ pos 2) end)
                     (= (aref buf pos) 13)
                     (= (aref buf (1+ pos)) 10))
          (error "chunked: expected CRLF after chunk-data"))
        (incf pos 2)))
    (unless terminated
      (error "chunked: incomplete response (no zero-size terminator)"))
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
    ;; A CR whose LF has not landed. This is the `...0 CRLF` case.
    ((>= (1+ pos) end) (values :incomplete pos))
    ;; CRLF: the empty trailer section, and the request ends past it.
    ((= (aref buf (1+ pos)) 10) (values :empty (+ pos 2)))
    ;; CR followed by something else.
    (t (values :malformed pos))))
