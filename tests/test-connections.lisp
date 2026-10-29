(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; Server tests, continued — request bodies, a connection's life, and the
;;; runtime around it
;;;
;;; A file of its own because one file of every server test grew past what
;;; SBCL 2.2.9 compiles in its default 1 GiB heap, though no one form in it
;;; was large.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Transfer-Encoding: the rules, and the codes they answer with
;;; ---------------------------------------------------------------------------

(defun test-transfer-encoding-rules ()
  (format t "~%Transfer-Encoding rules~%")
  (labels ((raw (version headers)
             (with-output-to-string (s)
               (format s "POST / HTTP/~a~a" version *crlf*)
               (dolist (h headers) (format s "~a~a" h *crlf*))
               (format s "~a" *crlf*)))
           (bytes (version headers)
             (sb-ext:string-to-octets (raw version headers)
                                      :external-format :ascii))
           (te (value)
             ;; A Transfer-Encoding header line carrying VALUE verbatim,
             ;; so a case can put a bare CR inside it.
             (concatenate 'string "Transfer-Encoding: " value))
           (cr (before after)
             ;; BEFORE and AFTER joined by a bare CR — a byte no header
             ;; value may contain, and one this scan has to notice before
             ;; PARSE-HEADERS-BYTES gets to at dispatch.
             (concatenate 'string before (string #\Return) after))
           (lf (before after)
             ;; The same, joined by a bare LF.
             (concatenate 'string before (string #\Newline) after))
           (coding-of (headers &optional (version "1.1"))
             ;; Called the way CONNECTION-ON-READ calls it: bounded by the
             ;; CRLFCRLF and started past the request line.
             (let* ((b (bytes version headers))
                    (header-end (web-skeleton::scan-crlf-crlf b 0 (length b)))
                    (hdr-start (+ (web-skeleton::scan-crlf b 0 header-end) 2)))
               (attempt (web-skeleton::scan-transfer-encoding
                         b header-end hdr-start))))
           (on-read (headers &optional (version "1.1"))
             ;; The status the rules answer with, taken where the client
             ;; would get it: the condition CONNECTION-ON-READ raises, which
             ;; is what HANDLE-CLIENT-READ hands to MAKE-ERROR-RESPONSE. A
             ;; read-fn answering :AGAIN keeps the whole request in the
             ;; buffer and off any socket.
             (let ((b (bytes version headers))
                   (conn (web-skeleton::make-connection
                          :fd -1
                          :read-fn (lambda (buffer start max-bytes)
                                     (declare (ignore buffer start max-bytes))
                                     :again))))
               (setf (web-skeleton::connection-read-buf conn) b
                     (web-skeleton::connection-read-pos conn) (length b))
               (handler-case (web-skeleton::connection-on-read conn)
                 (web-skeleton:http-parse-error (e)
                   (web-skeleton::http-parse-error-status e))
                 ;; Any other raise as its text, so it fails a check rather
                 ;; than ending the run: DRIVE's reason, below.
                 (error (e) (princ-to-string e))))))

    ;; ---- classification ----

    ;; The zero-count arm. A scanner that matched a header name as a
    ;; substring — "x-transfer-encoding", or the word inside a value —
    ;; would answer a coding here.
    (check "TE: absent is NIL"
           (coding-of '("Host: x" "X-Transfer-Encoding: chunked")) nil)

    ;; END is the CRLFCRLF position, so the last header line's CR sits *at*
    ;; END and a value scan bounded by END cannot see its terminator. These
    ;; two differ only in whether Transfer-Encoding is the final header; a
    ;; scan that got the bound wrong passes the second and fails the first.
    (check "TE: last header still reads its value"
           (coding-of '("Host: x" "Transfer-Encoding: chunked")) :chunked)
    (check "TE: non-final header reads the same"
           (coding-of '("Transfer-Encoding: chunked" "Host: x")) :chunked)

    (check "TE: field name and value both case fold"
           (coding-of '("Host: x" "TRANSFER-ENCODING: CHUNKED")) :chunked)

    (check "TE: surrounding OWS trimmed"
           (coding-of '("Host: x" "Transfer-Encoding:   chunked  ")) :chunked)

    ;; RFC 7230 §7's list rule permits empty elements. Counting them would
    ;; push chunked out of final position and refuse a legal message for a
    ;; reason that is not true of it.
    (check "TE: empty list elements are legal"
           (coding-of '("Host: x" "Transfer-Encoding: chunked,,")) :chunked)

    (check "TE: gzip alone is unsupported"
           (coding-of '("Host: x" "Transfer-Encoding: gzip")) :unsupported)

    ;; Legal, and chunked *is* final — the refusal is about the inner
    ;; coding, not the framing, which is why it is not :INVALID.
    (check "TE: gzip, chunked is unsupported"
           (coding-of '("Host: x" "Transfer-Encoding: gzip, chunked"))
           :unsupported)

    ;; RFC 7230 §3.3.1: chunked is applied once, and applied last.
    (check "TE: chunked in a non-final position is invalid"
           (coding-of '("Host: x" "Transfer-Encoding: chunked, gzip")) :invalid)
    (check "TE: chunked repeated in one value is invalid"
           (coding-of '("Host: x" "Transfer-Encoding: chunked, chunked"))
           :invalid)

    ;; The combination rule for repeated headers is defined, and computing
    ;; it is the reconciliation step the first rule refuses.
    (check "TE: two Transfer-Encoding headers are invalid"
           (coding-of '("Host: x" "Transfer-Encoding: gzip"
                        "Transfer-Encoding: chunked"))
           :invalid)

    (check "TE: a value naming no coding is invalid"
           (coding-of '("Host: x" "Transfer-Encoding:")) :invalid)

    ;; A bare CR ends the value scan early, so this reader sees a different
    ;; value than PARSE-HEADERS-BYTES will. Each of these four moves a
    ;; different way against its CR-free twin — less refusing, more
    ;; refusing, and twice not at all — which is why the arm rests on the
    ;; disagreement and not on a direction, as SCAN-TRANSFER-ENCODING's
    ;; bare-CR arm says.
    ;;
    ;; The no-space case is the one that distinguishes this arm. The spaced
    ;; case is here because it is what a natural probe writes, and the fold
    ;; arm catches it for an unrelated reason — tested alone, it makes a
    ;; missing bare-CR arm look present.
    (check "TE: bare CR before a comma is invalid"
           (coding-of (list "Host: x" (te (cr "chunked" ",gzip")))) :invalid)
    (check "TE: bare CR with the natural spacing is invalid"
           (coding-of (list "Host: x" (te (cr "chunked" ", gzip")))) :invalid)
    (check "TE: bare CR inside a token is invalid"
           (coding-of (list "Host: x" (te (cr "chu" "nked")))) :invalid)
    (check "TE: bare CR at the end of the value is invalid"
           (coding-of (list "Host: x" (te (cr "gzip" "")))) :invalid)

    ;; A bare LF is the same disagreement from the other side: this scan
    ;; would take `chunked<LF>X` for one value and answer it 501, a coding
    ;; it does not implement, where PARSE-HEADERS-BYTES refuses the LF 400.
    (check "TE: bare LF inside the value is invalid"
           (coding-of (list "Host: x" (te (lf "chunked" "X")))) :invalid)
    (check "TE: bare LF at the end of the value is invalid"
           (coding-of (list "Host: x" (te (lf "gzip" "")))) :invalid)

    ;; PARSE-HEADERS-BYTES rejects obsolete line folding too, but at
    ;; dispatch — after this scan has already decided how the body is
    ;; framed. Broken state: the fold is not noticed here, this reader
    ;; answers :CHUNKED for a value the parser reads as `chunked , gzip`,
    ;; and the two disagree about the framing of a body.
    (check "TE: obs-folded value is invalid"
           (coding-of (list "Host: x" "Transfer-Encoding: chunked"
                            (concatenate 'string (string #\Tab) ", gzip")))
           :invalid)

    ;; ---- the codes ----

    ;; RFC 7230 §3.3.3 says TE overrides CL. Answering 400 is the refusal
    ;; to apply that rule at all.
    (check "TE + Content-Length is 400"
           (on-read '("Host: x" "Transfer-Encoding: chunked"
                      "Content-Length: 5"))
           400)

    ;; Only *presence* is the violation, so the value is never consulted.
    ;; Broken state: SCAN-CONTENT-LENGTH's own rejections reach the client,
    ;; and the rule above holds only for Content-Lengths that happen to
    ;; parse — 400 for the value here, 413 below, both naming a condition
    ;; that is not why the request is refused.
    (check "TE + unparseable Content-Length is still the pair, 400"
           (on-read '("Host: x" "Transfer-Encoding: chunked"
                      "Content-Length: abc"))
           400)
    (check "TE + oversized Content-Length is 400, not 413"
           (on-read '("Host: x" "Transfer-Encoding: chunked"
                      "Content-Length: 9999999999999999"))
           400)

    (check "TE on HTTP/1.0 is 400"
           (on-read '("Host: x" "Transfer-Encoding: chunked") "1.0") 400)

    ;; The gate is stated as "for 1.1", not "against 1.0". The two
    ;; implementations agree on every 1.0 request and part here: under
    ;; "against 1.0" this byte is not 48, the gate does not fire, and the
    ;; coding answers 501. PARSE-REQUEST-BYTES would answer 505 for the
    ;; version — but at dispatch, and the framing is decided before that.
    (check "TE on an unvalidated version is 400"
           (on-read '("Host: x" "Transfer-Encoding: chunked") "1.9") 400)

    ;; The other half of that answer, and the premise the gate's comment
    ;; rests on: the same request without the header is the parser's 505,
    ;; raised at dispatch. The gate answers 400 rather than 505 because it
    ;; declines to hold a second copy of the supported-version set — this
    ;; says the set really does live somewhere else, so the asymmetry is a
    ;; division of labour and not an oversight.
    ;;
    ;; A pinned premise, not a detector, and the only one of these that no
    ;; revert can reach — `typo'd version is 505` above exercises the same
    ;; branch of the same function for the same reason. It earns its place
    ;; by sitting beside the 400 it explains, and nowhere else. Anything
    ;; that breaks it fails there first.
    (check "no TE: an unsupported version is the parser's 505"
           (let ((b (bytes "1.9" '("Host: x"))))
             (handler-case (progn (web-skeleton::parse-request-bytes
                                   b 0 (length b))
                                  :no-error)
               (web-skeleton:http-parse-error (e)
                 (web-skeleton::http-parse-error-status e))))
           505)

    ;; Transfer-Encoding is a 1.1 field, so the gate covers every coding.
    ;; Broken state: the gate sits inside the chunked arm, and a 1.0
    ;; request naming gzip answers 501 — the coding, when the version is
    ;; the reason it is refused.
    (check "TE: gzip on HTTP/1.0 is 400, not 501"
           (on-read '("Host: x" "Transfer-Encoding: gzip") "1.0") 400)

    (check "TE with chunked non-final is 400"
           (on-read '("Host: x" "Transfer-Encoding: chunked, gzip")) 400)

    (check "TE repeated as two headers is 400"
           (on-read '("Host: x" "Transfer-Encoding: gzip"
                      "Transfer-Encoding: chunked"))
           400)

    (check "TE bare CR is 400"
           (on-read (list "Host: x" (te (cr "chunked" ",gzip")))) 400)

    (check "TE bare LF is 400"
           (on-read (list "Host: x" (te (lf "chunked" "X")))) 400)

    (check "TE obs-folded is 400"
           (on-read (list "Host: x" "Transfer-Encoding: chunked"
                          (concatenate 'string (string #\Tab) ", gzip")))
           400)

    ;; §3.3.1's own code for a coding the server does not understand.
    (check "TE: gzip is 501"
           (on-read '("Host: x" "Transfer-Encoding: gzip")) 501)
    (check "TE: gzip, chunked is 501"
           (on-read '("Host: x" "Transfer-Encoding: gzip, chunked")) 501)
    ;; Chunked is accepted now, so what this pins is the rules running
    ;; ahead of the body arm: the request is well formed, no body byte has
    ;; arrived, and the answer is to keep reading rather than to refuse or
    ;; to dispatch a POST with its body still on the wire.
    ;; TEST-CHUNKED-REQUEST-BODY covers where it ends.
    (check "TE: chunked alone is accepted and waits for its body"
           (on-read '("Host: x" "Transfer-Encoding: chunked")) :continue)

    ;; The zero-behavior-change claim, asserted rather than assumed.
    (check "no TE: request still dispatches"
           (on-read '("Host: x")) :dispatch)
    (check "no TE: a Content-Length body still reads"
           (on-read '("Host: x" "Content-Length: 5")) :continue)))

;;; ---------------------------------------------------------------------------
;;; Chunked request bodies: where the request ends, and what ends it
;;; ---------------------------------------------------------------------------

(defun test-chunked-request-body ()
  (format t "~%Chunked request bodies~%")
  (labels ((req (body &key (headers '("Host: x" "Transfer-Encoding: chunked"))
                           (version "1.1"))
             (sb-ext:string-to-octets
              (with-output-to-string (s)
                (format s "POST /u HTTP/~a~a" version *crlf*)
                (dolist (h headers) (format s "~a~a" h *crlf*))
                (format s "~a~a" *crlf* body))
              :external-format :ascii))
           (drive (bytes)
             ;; Returns (values VERDICT CONN). A read-fn answering :AGAIN
             ;; keeps everything in the buffer and off any socket, so the
             ;; verdict is the state machine's and nothing else.
             (let ((conn (web-skeleton::make-connection
                          :fd -1
                          :read-fn (lambda (buffer start max-bytes)
                                     (declare (ignore buffer start max-bytes))
                                     :again))))
               (setf (web-skeleton::connection-read-buf conn) bytes
                     (web-skeleton::connection-read-pos conn) (length bytes))
               (values (handler-case (web-skeleton::connection-on-read conn)
                         (web-skeleton:http-parse-error (e)
                           (web-skeleton::http-parse-error-status e))
                         ;; Anything else becomes its own text rather than
                         ;; ending the run. CONNECTION-ON-READ can raise a
                         ;; plain error — the walk does, on a size line it
                         ;; can already tell is invalid — and a raise that
                         ;; escapes here takes every later assertion in the
                         ;; file with it. A failed CHECK carrying the
                         ;; condition says the same thing and stays
                         ;; countable.
                         (error (e) (princ-to-string e)))
                       conn)))
           (verdict (body &rest args)
             (values (apply #'drive (list (apply #'req body args)))))
           (body-of (body)
             ;; Drive to :DISPATCH, then parse — the decoded body is what
             ;; a handler would receive.
             (multiple-value-bind (v conn) (drive (req body))
               (declare (ignore v))
               (handler-case
                   (let ((r (web-skeleton::connection-parse-request conn)))
                     (sb-ext:octets-to-string (web-skeleton:http-request-body r)
                                              :external-format :ascii))
                 (web-skeleton:http-parse-error (e)
                   (web-skeleton::http-parse-error-status e))))))

    ;; ---- where a chunked request ends ----

    ;; Headers only: the framing says a body is coming and none of it has
    ;; arrived. Broken state: the Content-Length arm answers "no body" for
    ;; this request and dispatches a POST with its body still on the wire.
    (check "chunked: no body bytes yet keeps reading"
           (verdict "") :continue)

    ;; The whole thing in one read.
    (check "chunked: a complete body dispatches"
           (verdict (format nil "3~a123~a0~a~a" *crlf* *crlf* *crlf* *crlf*))
           :dispatch)

    ;; THE two-byte rule. `...0 CRLF` is the zero-size chunk header and
    ;; nothing else; the CRLF that terminates an empty trailer section has
    ;; not arrived. Broken state: complete is answered here, REQUEST-END
    ;; lands two bytes early, and those two bytes are shifted to offset 0
    ;; and read as the beginning of the next request.
    (check "chunked: the terminator alone is not the end of the request"
           (verdict (format nil "3~a123~a0~a" *crlf* *crlf* *crlf*))
           :continue)
    (check "chunked: the empty trailer's CRLF is what ends it"
           (verdict (format nil "3~a123~a0~a~a" *crlf* *crlf* *crlf* *crlf*))
           :dispatch)

    ;; An empty body is still a body, and still needs its terminator.
    (check "chunked: zero chunks, terminated, dispatches"
           (verdict (format nil "0~a~a" *crlf* *crlf*)) :dispatch)

    ;; The boundary itself, not a delivery-shaped proxy for it: with a
    ;; second request pipelined behind, REQUEST-END has to land exactly on
    ;; its first byte. Computed from the pieces rather than written as a
    ;; number — a hand-counted offset is a second implementation of the
    ;; arithmetic this slot exists to delete, and an easy one to get wrong.
    (let* ((first-req (req (format nil "3~a123~a0~a~a"
                                   *crlf* *crlf* *crlf* *crlf*)))
           (both (concatenate '(vector (unsigned-byte 8))
                              first-req
                              (sb-ext:string-to-octets
                               "GET /b HTTP/1.1" :external-format :ascii))))
      (multiple-value-bind (v conn) (drive both)
        (declare (ignore v))
        (check "chunked: the boundary lands on the next request's first byte"
               (web-skeleton::connection-request-end conn)
               (length first-req))))

    ;; ---- trailers ----

    ;; The decision: refuse. Nothing surfaces trailers to an app, so
    ;; accepting silently discards data the client believed it sent, and
    ;; consuming needs a second header parser whose disagreement with the
    ;; first is the shape these rules exist to prevent.
    (check "chunked: a trailer field is refused"
           (verdict (format nil "0~aX-T: 1~a~a" *crlf* *crlf* *crlf*)) 400)

    ;; The headline acceptance criterion, asserted at the boundary rather
    ;; than at delivery: a trailer section carrying a complete HTTP request
    ;; must not become a second request. Refusing means the boundary for
    ;; this request is never computed at all.
    (check "chunked: a trailer holding a whole request is refused"
           (verdict (format nil "0~aGET /admin HTTP/1.1~aHost: x~a~a"
                            *crlf* *crlf* *crlf* *crlf*))
           400)

    ;; A bare CR where the terminator belongs.
    (check "chunked: a bare CR after the terminator is refused"
           (verdict (format nil "0~a~a!" *crlf* (string #\Return))) 400)

    ;; ---- the body a handler receives ----

    (check "chunked: the decoded body reaches the request"
           (body-of (format nil "3~aabc~a2~ade~a0~a~a"
                            *crlf* *crlf* *crlf* *crlf* *crlf* *crlf*))
           "abcde")

    (check "chunked: an empty chunked body decodes to nothing"
           (body-of (format nil "0~a~a" *crlf* *crlf*)) "")
    ;; `5g`, a size with junk after its digit. The walk refuses it on
    ;; arrival, as the decoder would: no later byte makes `g` a size, an
    ;; extension or a line end, so waiting would only hold the request.
    (check "chunked: a junk byte after the chunk-size is 400"
           (verdict (format nil "5g~ahello~a0~a~a"
                            *crlf* *crlf* *crlf* *crlf*))
           400)

    ;; Data followed by something other than its CRLF. Refused by the walk
    ;; before the request completes, so a body the decoder would refuse is
    ;; never dispatched — and never handed to a consumer of the walk, who
    ;; would have nothing behind it to refuse it later.
    (check "chunked: chunk data not followed by CRLF is 400"
           (verdict (format nil "3~aabcXX0~a~a" *crlf* *crlf* *crlf*)) 400)

    ;; The decoder's own refusal still becomes the client's 400. The walk
    ;; and the decoder accept the same bodies, so no request reaches this
    ;; through the state machine; the boundary is set by hand, over bytes
    ;; the walk never saw. Broken state: the decode error escapes
    ;; CONNECTION-PARSE-REQUEST and HANDLE-CLIENT-READ's generic arm
    ;; answers 500 — the server blamed for a malformed request.
    (let* ((bytes (req (format nil "3~aabcXX0~a~a" *crlf* *crlf* *crlf*)))
           (conn (web-skeleton::make-connection :fd -1))
           (header-end (web-skeleton::scan-crlf-crlf bytes 0 (length bytes))))
      (setf (web-skeleton::connection-read-buf conn) bytes
            (web-skeleton::connection-read-pos conn) (length bytes)
            (web-skeleton::connection-header-end conn) header-end
            (web-skeleton::connection-body-framing conn) :chunked
            (web-skeleton::connection-request-end conn) (length bytes))
      (check "chunked: a body the decoder rejects is 400, not 500"
             (handler-case
                 (progn (web-skeleton::connection-parse-request conn) :parsed)
               (web-skeleton:http-parse-error (e)
                 (web-skeleton::http-parse-error-status e))
               (error (e) (princ-to-string e)))
             400))



    ;; ---- the size cap, which has no declared length to read ----

    ;; *MAX-BODY-SIZE* is enforced before allocating on the Content-Length
    ;; path, against a number the client supplied. A chunked request
    ;; supplies none, so the check has to become incremental, and this is
    ;; the only place a *body*-size refusal can come from at all.
    (let ((web-skeleton::*max-body-size* 4))
      (check "chunked: a body over the cap is 413"
             (verdict (format nil "5~ahello~a0~a~a" *crlf* *crlf* *crlf* *crlf*))
             413)
      ;; The boundary, so the comparison is > and not >=.
      (check "chunked: a body exactly at the cap is served"
             (verdict (format nil "4~ahell~a0~a~a" *crlf* *crlf* *crlf* *crlf*))
             :dispatch)
      ;; Spread across chunks: no single chunk crosses the cap, the
      ;; running total does. Broken state: the check reads one chunk's
      ;; size instead of accumulating, and three 2-byte chunks pass a
      ;; 4-byte cap.
      (check "chunked: the cap is a running total, not a per-chunk test"
             (verdict (format nil "2~aab~a2~acd~a2~aef~a0~a~a"
                              *crlf* *crlf* *crlf* *crlf*
                              *crlf* *crlf* *crlf* *crlf*))
             413))

    ;; Refused on the *declared* size, the moment the header arrives.
    ;; Broken state: the walk skips past the promised bytes, finds itself
    ;; beyond the buffer, answers "keep reading", and the client is
    ;; eventually told the read buffer filled — when what it actually did
    ;; was declare a chunk larger than any body this server accepts. The
    ;; buffer's answer is true and useless; a client cannot act on it.
    (check "chunked: an oversized chunk header is refused on arrival"
           (verdict (format nil "ffffffff~a" *crlf*)) 413)
    ;; Sixteen digits is the walk's own limit, and the product is past
    ;; MOST-POSITIVE-FIXNUM — the projected total is a local, so it may be
    ;; a bignum without the accumulator slot ever leaving FIXNUM.
    (check "chunked: a sixteen-digit chunk size is refused too"
           (verdict (format nil "ffffffffffffffff~a" *crlf*)) 413)
    ;; One digit further and the size is not large, it is unrepresentable
    ;; — the walk's own digit cap trips before any value exists to
    ;; project. Broken state: that arm answers "keep reading", the
    ;; projection sees nothing, and the request waits for bytes that
    ;; cannot come until the read buffer fills and answers about itself.
    ;; 400 rather than 413 because the fault is the framing, not a size.
    (check "chunked: a size line past the digit limit is 400, not a stall"
           (verdict (format nil "fffffffffffffffff~a" *crlf*)) 400)
    ;; ---- the buffer-full arm ----

    ;; A chunked body larger than the read cap has to answer 413, and this
    ;; is the assertion for the third reader of "is the body complete".
    ;; Content-Length arithmetic in the :FULL arm, which a chunked request
    ;; satisfies unconditionally — BODY-EXPECTED is 0 — would call a
    ;; half-arrived body complete, fall through, and then :READ-BODY
    ;; would answer :CONTINUE while the buffer sat at its cap with nothing
    ;; able to read further. The connection would be held until the idle
    ;; sweeper took it. Broken state: :CONTINUE here instead of 413.
    ;;
    ;; Two calls because the arm is only reachable from :READ-BODY: the
    ;; first establishes the framing, the second arrives with the buffer
    ;; already at its cap.
    (let* ((web-skeleton::*max-body-size* 256)
           (conn (web-skeleton::make-connection
                  :fd -1
                  :read-fn (lambda (buffer start max-bytes)
                             (declare (ignore buffer start max-bytes))
                             :again)))
           (head (req ""))
           (cap (web-skeleton::connection-read-cap conn)))
      ;; First pass: headers only, framing established, waiting on a body.
      (setf (web-skeleton::connection-read-buf conn) head
            (web-skeleton::connection-read-pos conn) (length head))
      (check "chunked: buffer-full setup reaches :read-body"
             (attempt (web-skeleton::connection-on-read conn)) :continue)
      ;; Second pass: a buffer at its cap holding framing that is
      ;; *incomplete* rather than invalid, so this reaches the :FULL arm
      ;; and not one of the refusals above it. The filler is legal chunks
      ;; of one byte each, every size line carrying a long extension, and
      ;; the last one cut off wherever the cap falls. The buffer fills
      ;; while the decoded total stays far below the body cap and no byte
      ;; is a fault, so the walk answers keep reading with nothing that
      ;; projects past the cap, and the buffer's 413 is the only answer
      ;; left.
      ;;
      ;; The obvious fillers each reach a different refusal first, which
      ;; is a correct answer to a different question. A size line that
      ;; never ends is refused at *MAX-HEADER-LINE-LENGTH*, and a run of
      ;; hex digits at sixteen, both with a 400; one-byte chunks without
      ;; extensions reach the body cap's 413, one decoded byte for every
      ;; six on the wire, long before the buffer is full.
      (let ((unit (sb-ext:string-to-octets
                   (concatenate 'string "1;" (make-string 4000 :initial-element #\a)
                                *crlf* "x" *crlf*)
                   :external-format :ascii))
            (big (make-array cap :element-type '(unsigned-byte 8))))
        (replace big head)
        (loop for at from (length head) below cap by (length unit)
              do (replace big unit :start1 at))
        (setf (web-skeleton::connection-read-buf conn) big
              (web-skeleton::connection-read-pos conn) cap)
        (check "chunked: a body past the read cap is 413, not a stall"
               (handler-case (web-skeleton::connection-on-read conn)
                 (web-skeleton:http-parse-error (e)
                   (web-skeleton::http-parse-error-status e)))
               413)))

    ;; ---- Expect: 100-continue ----

    ;; Routed through :SENDING-100-CONTINUE rather than around it, so the
    ;; interim leaves by the same path a Content-Length body's does.
    ;; Broken state: the chunked arm answers :CONTINUE, the client waits
    ;; its timeout and sends anyway, and every chunked upload from a
    ;; well-behaved client pays a second or more it should not.
    (multiple-value-bind (v conn)
        (drive (req "" :headers '("Host: x" "Transfer-Encoding: chunked"
                                  "Expect: 100-continue")))
      (check "chunked + 100-continue: the interim is flushed"
             v :flush-queued)
      (check "chunked + 100-continue: it is the interim that was queued"
             (let ((b (web-skeleton::connection-write-buf conn)))
               (and b (search "100 Continue"
                              (sb-ext:octets-to-string
                               b :external-format :ascii))
                    t))
             t)
      (check "chunked + 100-continue: and the state is the interim's"
             (web-skeleton::connection-state conn) :sending-100-continue))

    ;; The client did not wait. Sending an interim it has stopped
    ;; listening for is latency on what is, for chunked, the common
    ;; shape — a sender that ignored the expectation has usually sent the
    ;; whole body. Broken state: the interim arm is tested before the
    ;; already-complete one, and every such upload takes an extra
    ;; round trip.
    (multiple-value-bind (v conn)
        (drive (req (format nil "3~aabc~a0~a~a" *crlf* *crlf* *crlf* *crlf*)
                    :headers '("Host: x" "Transfer-Encoding: chunked"
                               "Expect: 100-continue")))
      (check "chunked + 100-continue: a body already here dispatches"
             v :dispatch)
      (check "chunked + 100-continue: and no interim was queued"
             (null (web-skeleton::connection-write-buf conn)) t))
    ;; ---- interaction with the rules already in place ----

    ;; The chunked arm falls through to the body cond rather than
    ;; returning early, so the Expect gate still runs ahead of it.
    ;; Broken state: chunked returns from the classification block and a
    ;; chunked POST with an unknown expectation is accepted where a
    ;; Content-Length one is refused.
    (check "chunked: an unknown Expect still 417s"
           (verdict "" :headers '("Host: x" "Transfer-Encoding: chunked"
                                  "Expect: x-foo"))
           :flush-queued)))

;;; ---------------------------------------------------------------------------
;;; A body handler is asked at the head
;;; ---------------------------------------------------------------------------

(defun test-body-handler-at-the-head ()
  "With a body handler installed, the head of a request with a body stops
   CONNECTION-ON-READ before anything that needs the body's cap: no size
   check, no walk over a chunked body, and no 100 Continue, which would
   invite a body the handler may refuse. CONNECTION-BEGIN-BODY does all
   three once the decision is made, under the cap it made.

   State-machine level, over a read-fn that answers :AGAIN, so every
   verdict is the state machine's and none is a socket's. The body handler
   itself is never called here — CONNECTION-ON-READ only asks whether
   there is one — which is why a bare function stands in for it."
  (format t "~%Body handler: the head is held for it~%")
  (labels ((req (lines body)
             (sb-ext:string-to-octets
              (with-output-to-string (s)
                (format s "POST /u HTTP/1.1~a" *crlf*)
                (dolist (h lines) (format s "~a~a" h *crlf*))
                (format s "~a~a" *crlf* body))
              :external-format :ascii))
           (conn-for (bytes read-result)
             (let ((conn (web-skeleton::make-connection
                          :fd -1
                          :read-fn (lambda (buffer start max-bytes)
                                     (declare (ignore buffer start max-bytes))
                                     read-result))))
               (setf (web-skeleton::connection-read-buf conn) bytes
                     (web-skeleton::connection-read-pos conn) (length bytes))
               conn))
           (drive (bytes &key (body-handler #'identity) (read-result :again))
             ;; (values VERDICTS CONN): every value the read returned, or the
             ;; status of what it raised.
             (let ((conn (conn-for bytes read-result))
                   (web-skeleton::*body-handler* body-handler))
               (values (handler-case
                           (multiple-value-list
                            (web-skeleton::connection-on-read conn))
                         (web-skeleton:http-parse-error (e)
                           (web-skeleton::http-parse-error-status e)))
                       conn)))
           (begin (conn &key plan)
             (when plan (web-skeleton::apply-body-plan conn plan))
             (handler-case (web-skeleton::connection-begin-body conn nil)
               (web-skeleton:http-parse-error (e)
                 (list (web-skeleton::http-parse-error-status e)
                       (web-skeleton::http-parse-error-message e))))))
    (let ((web-skeleton::*max-body-size* 4))
      ;; ---- held: nothing the cap governs happens before the decision ----
      (multiple-value-bind (v conn)
          (drive (req '("Host: x" "Content-Length: 10") "0123456789"))
        (check "held: a Content-Length head goes to the body handler"
               v '(:decide-body nil))
        (check "held: before its size check — ten bytes over a cap of four"
               (web-skeleton::connection-state conn) :read-body)
        ;; Decided under a cap above the default, the same body goes through.
        (check "begun under a plan's cap above *max-body-size*: dispatched"
               (begin conn :plan (buffer-body :max-size 100)) :dispatch)
        (check "and the read buffer's ceiling follows the plan"
               (web-skeleton::connection-read-cap conn)
               (+ 100 (web-skeleton::request-head-budget)))
        (check "and the body is timed on its progress from the decision"
               (plusp (web-skeleton::connection-body-progress-at conn)) t))
      ;; The plan's cap binds below the default as well as above it.
      (multiple-value-bind (v conn)
          (drive (req '("Host: x" "Content-Length: 3") "abc"))
        (declare (ignore v))
        (check "begun under a plan's cap below the body: 413, naming the cap"
               (begin conn :plan (buffer-body :max-size 2))
               '(413 "body too large (3 bytes, max 2)")))
      ;; Expect: 100-continue — the interim waits for the decision.
      (multiple-value-bind (v conn)
          (drive (req '("Host: x" "Content-Length: 3" "Expect: 100-continue") ""))
        (check "held: before the 100 Continue"
               (list v (web-skeleton::connection-write-pending conn))
               '((:decide-body nil) 0))
        (check "begun: the 100 Continue is queued once the body is accepted"
               (list (begin conn)
                     (web-skeleton::connection-state conn)
                     (plusp (web-skeleton::connection-write-pending conn)))
               '(:flush-queued :sending-100-continue t)))
      ;; Chunked: the walk is what enforces the cap, so it waits too.
      (multiple-value-bind (v conn)
          (drive (req '("Host: x" "Transfer-Encoding: chunked")
                      (format nil "a~a0123456789~a0~a~a"
                              *crlf* *crlf* *crlf* *crlf*)))
        (check "held: a chunked head goes to the body handler before its walk"
               (list v
                     (web-skeleton::connection-body-decoded conn)
                     (web-skeleton::connection-chunk-scan-pos conn))
               '((:decide-body nil) 0 0))
        (check "begun under a plan's cap: the walk completes the body"
               (begin conn :plan (buffer-body :max-size 10)) :dispatch))
      (multiple-value-bind (v conn)
          (drive (req '("Host: x" "Transfer-Encoding: chunked")
                      (format nil "a~a0123456789~a0~a~a"
                              *crlf* *crlf* *crlf* *crlf*)))
        (declare (ignore v))
        (check "begun under a smaller cap: the walk refuses it, naming the cap"
               (begin conn :plan (buffer-body :max-size 9))
               '(413 "chunked body too large (10 bytes, max 9)")))
      ;; The peer's FIN rides along, for the verdict the decision leads to.
      (check "held: a FIN that came with the head is the second value"
             (values (drive (req '("Host: x" "Content-Length: 3") "a")
                            :read-result :eof))
             '(:decide-body t))
      ;; ---- not held ----
      (check "not held: a request without a body dispatches"
             (values (drive (sb-ext:string-to-octets
                             (format nil "GET / HTTP/1.1~aHost: x~a~a"
                                     *crlf* *crlf* *crlf*)
                             :external-format :ascii)))
             '(:dispatch))
      ;; The framing is the framework's to settle, and comes first: a body
      ;; handler is never shown a request about to be refused.
      (check "framing first: Transfer-Encoding with Content-Length is 400"
             (values (drive (req '("Host: x" "Transfer-Encoding: chunked"
                                   "Content-Length: 3")
                                 "")))
             400)
      (multiple-value-bind (v conn)
          (drive (req '("Host: x" "Content-Length: 3" "Expect: x-other") "abc"))
        (check "framing first: an unknown Expect is answered 417"
               (list v (web-skeleton::connection-state conn))
               '((:flush-queued) :write-response)))
      ;; And without a body handler, nothing about the head changes.
      (multiple-value-bind (v conn)
          (drive (req '("Host: x" "Content-Length: 3" "Expect: 100-continue") "")
                 :body-handler nil)
        (check "no body handler: the interim goes out at once, as before"
               (list v (web-skeleton::connection-state conn))
               '((:flush-queued) :sending-100-continue)))
      (check "no body handler: the default cap refuses at the head, as before"
             (values (drive (req '("Host: x" "Content-Length: 10") "")
                            :body-handler nil))
             413))
    ;; ---- the plan's own checks ----
    (check "buffer-body: a negative cap is refused"
           (and (stringp (attempt (buffer-body :max-size -1))) t) t)
    (check "buffer-body: a cap that is not a number of bytes is refused"
           (and (stringp (attempt (buffer-body :max-size "1MB"))) t) t)
    (check "buffer-body: the default cap is *max-body-size* at the call"
           (let ((web-skeleton::*max-body-size* 7))
             (web-skeleton::body-plan-max-size (buffer-body)))
           7)))

(defun test-body-decision-rereads-under-its-cap ()
  "A decision that raises the read cap is followed by a read in the same
   wake-up.

   The read that brought the head stopped at the cap the head was read
   under, with the rest of the body already in the kernel. An
   edge-triggered descriptor raises no new edge for bytes already there,
   so a client that had sent everything would wait on an event that is
   not coming, until the stall clock closed it. Over a real loopback pair,
   because the bytes have to be in the kernel and not in a read-fn for
   that to be the situation; the whole request is written, and has
   landed, before the server reads any of it.

   The head budgets are shrunk so the default read cap is a couple of KiB
   and below the 4 KiB the read buffer starts at: the first read stops
   with the buffer full and part of the body unread, which is the shape
   that needs the second read."
  (format t "~%Body handler: the read after a raised cap~%")
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create))
          (handed nil))
      (unwind-protect
           (let* ((fd (web-skeleton::socket-fd server))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (web-skeleton::*max-total-header-bytes* 1024)
                  (web-skeleton::*max-request-line-length* 1024)
                  (web-skeleton::*max-header-count* 10)
                  (web-skeleton::*max-body-size* 16)
                  (web-skeleton::*body-handler*
                    (lambda (req)
                      (declare (ignore req))
                      (buffer-body :max-size 8192)))
                  (head (sb-ext:string-to-octets
                         (format nil "POST /u HTTP/1.1~aHost: x~a~
                                      Content-Length: 4096~a~a"
                                 *crlf* *crlf* *crlf* *crlf*)
                         :external-format :ascii))
                  (conn (web-skeleton::make-connection
                         :fd fd :socket server
                         :last-active (get-universal-time))))
             (web-skeleton::set-nonblocking fd)
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd fd (logior web-skeleton::+epollin+
                                                      web-skeleton::+epollet+))
             (let ((out (sb-bsd-sockets:socket-make-stream
                         client :output t :element-type '(unsigned-byte 8))))
               (write-sequence head out)
               (write-sequence (make-array 4096 :element-type '(unsigned-byte 8)
                                                :initial-element 66)
                               out)
               (finish-output out))
             (%await-arrival fd :bytes (+ (length head) 4096))
             (check "precondition: the default read cap is below the request"
                    (< (web-skeleton::connection-read-cap conn) (+ (length head) 4096))
                    t)
             (web-skeleton::handle-client-read
              conn epfd
              (lambda (req)
                (setf handed (length (http-request-body req)))
                (make-text-response 200 "ok"))
              nil)
             (check "decided: the whole body is read in that wake-up, under the raised cap"
                    handed 4096))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client))))))

(defun test-received-body-window-and-yield ()
  "A received body costs one window of buffer, and a client with more in
   the kernel than one wake-up hands over is yielded to and read again.

   Driven through HANDLE-CLIENT-READ and HANDLE-CLIENT-WRITE over a read-fn
   that serves a queue of bytes standing in for the kernel, with
   EPOLL-MODIFY recorded rather than called. A socket cannot be told what
   to hold — how much a peer's writes leave queued is the kernel's choice
   — and both properties turn on exactly that.

     the window   The head arrives with half a mebibyte behind it, which
                  the read that finds the head pulls in under the default
                  cap. The body handler then receives it; once that first
                  piece is handed over, the buffer goes back to the head
                  and one window, and the pieces after it are read into
                  that.
     the yield    A mebibyte and more then waits in the kernel. One
                  wake-up hands over +RECEIVE-ROUNDS+ windows and yields,
                  arming writability; the write side's :READ-BODY arm,
                  woken by it, answers :KEEP-ALIVE, which reads again — and
                  nothing else would, since no new bytes arrive to raise an
                  edge."
  (format t "~%Received body: the window and the yield~%")
  (let* ((kernel (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0))
         (taken 0)
         (pieces nil)
         (widths nil)
         (masks nil)
         (answered nil)
         (real (symbol-function 'web-skeleton::epoll-modify))
         (window web-skeleton::+receive-window+)
         (rounds web-skeleton::+receive-rounds+))
    (flet ((supply (bytes) (loop for b across bytes do (vector-push-extend b kernel)))
           (body-bytes (n) (make-array n :element-type '(unsigned-byte 8)
                                         :initial-element 98)))
      (unwind-protect
           (let* ((web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*body-handler*
                    (lambda (req)
                      (declare (ignore req))
                      (receive-body
                       :max-size nil
                       :on-data (lambda (conn bytes)
                                  (push (length bytes) pieces)
                                  (push (length (web-skeleton::connection-read-buf conn))
                                        widths)
                                  nil)
                       :on-end (lambda (conn) (declare (ignore conn))
                                 (setf answered t)
                                 (make-text-response 200 "ok")))))
                  (total (+ (* 512 1024) (* (1+ rounds) window) 100))
                  (conn (web-skeleton::make-connection
                         :fd -1 :last-active (get-universal-time)
                         :read-fn (lambda (buffer start max-bytes)
                                    (let ((n (min max-bytes (- (fill-pointer kernel) taken))))
                                      (if (zerop n)
                                          :again
                                          (progn
                                            (replace buffer kernel :start1 start
                                                     :start2 taken :end2 (+ taken n))
                                            (incf taken n)
                                            n)))))))
             (setf (symbol-function 'web-skeleton::epoll-modify)
                   (lambda (efd fd mask) (declare (ignore efd fd)) (push mask masks)))
             ;; ---- the window ----
             (supply (sb-ext:string-to-octets
                      (format nil "POST /u HTTP/1.1~aHost: x~aContent-Length: ~d~a~a"
                              *crlf* *crlf* total *crlf* *crlf*)
                      :external-format :ascii))
             (supply (body-bytes (* 512 1024)))
             (web-skeleton::handle-client-read conn -1 nil nil)
             (check "window: the first piece is what came with the head"
                    (and pieces (> (first pieces) window) t) t)
             (supply (body-bytes window))
             (web-skeleton::handle-client-read conn -1 nil nil)
             (check "window: after it, the buffer is the head and one window"
                    (<= (first widths) (+ window 100)) t)
             ;; ---- the yield ----
             (setf masks nil)
             (let ((before (length pieces)))
               (supply (body-bytes (* rounds window)))
               (supply (body-bytes (- total (* 512 1024) window (* rounds window))))
               (web-skeleton::handle-client-read conn -1 nil nil)
               (check "yield: one wake-up hands over its rounds and no more"
                      (- (length pieces) before) rounds)
               (check "yield: and arms writability to come back"
                      (and (some (lambda (m) (logtest m web-skeleton::+epollout+)) masks)
                           t)
                      t)
               (check "yield: the writable wake-up asks for a read"
                      (web-skeleton::handle-client-write conn -1) :keep-alive)
               (web-skeleton::handle-client-read conn -1 nil nil)
               (check "yield: and the read finishes the body"
                      (list answered (reduce #'+ pieces)) (list t total))))
        (setf (symbol-function 'web-skeleton::epoll-modify) real)))))

(defun test-body-resume-and-refuse-contract ()
  "BODY-RESUME and BODY-REFUSE do nothing to a body that has ended or a
   connection that has closed, answering NIL, and refuse to touch a live
   one from anywhere but the worker that owns it.

   The NILs are what let a callback that was waiting on something else —
   a fetch's :THEN, most often — resume or refuse without first asking
   whether the client is still there. The refusals come before anything
   is touched, as WS-SEND's and STREAM-CLOSE's do."
  (format t "~%body-resume and body-refuse: the contract~%")
  (let ((plan (receive-body :on-data (lambda (c b) (declare (ignore c b)) nil)
                            :on-end (lambda (c) (declare (ignore c))
                                      (make-text-response 200 "ok"))))
        (ok (make-text-response 200 "no")))
    ;; ---- closed, or not receiving: NIL, whatever thread ----
    (let ((closed (web-skeleton::make-connection :fd -1 :state :read-body)))
      (setf (web-skeleton::connection-body-receiver closed) plan
            (web-skeleton::connection-body-paused closed) t)
      (check "resume: a closed connection is a no-op" (body-resume closed) nil)
      (check "refuse: a closed connection is a no-op" (body-refuse closed ok) nil))
    (let* ((web-skeleton::*connections* (make-hash-table :test #'eql))
           (web-skeleton::*epoll-fd* 999)
           (idle (web-skeleton::make-connection :fd 5 :state :read-body)))
      (web-skeleton::register-connection idle)
      (check "resume: a connection receiving no body is a no-op"
             (body-resume idle) nil)
      (check "refuse: a connection receiving no body is a no-op"
             (body-refuse idle ok) nil)
      ;; Receiving, but not paused: nothing to resume, and no epoll call.
      (setf (web-skeleton::connection-body-receiver idle) plan)
      (check "resume: a body that is not paused is a no-op"
             (body-resume idle) nil))
    ;; ---- live, but not this worker's: refused before anything moves ----
    (let ((live (web-skeleton::make-connection :fd 5 :state :read-body)))
      (setf (web-skeleton::connection-body-receiver live) plan
            (web-skeleton::connection-body-paused live) t)
      (let ((web-skeleton::*epoll-fd* nil))
        (check "resume: refused off a worker"
               (and (stringp (attempt (body-resume live))) t) t)
        (check "refuse: refused off a worker"
               (and (stringp (attempt (body-refuse live ok))) t) t))
      (let ((web-skeleton::*connections* (make-hash-table :test #'eql))
            (web-skeleton::*epoll-fd* 999))
        (check "resume: refused on a connection another worker owns"
               (and (stringp (attempt (body-resume live))) t) t)
        (check "refuse: refused on a connection another worker owns"
               (and (stringp (attempt (body-refuse live ok))) t) t))
      (check "and the refusals touched nothing: still paused, still receiving"
             (list (web-skeleton::connection-body-paused live)
                   (not (null (web-skeleton::connection-body-receiver live))))
             '(t t)))
    (check "refuse: a response that is not an HTTP-RESPONSE is refused"
           (and (stringp (attempt (body-refuse
                                   (web-skeleton::make-connection :fd -1)
                                   "nope")))
                t)
           t)))

;;; ---------------------------------------------------------------------------
;;; multipart/form-data
;;; ---------------------------------------------------------------------------

(defun %form (&rest lines)
  "LINES joined by CRLF, as a multipart body's lines are."
  (format nil (concatenate 'string "~{~a~^" *crlf* "~}") lines))

(defun %multipart-outcome (body &key (boundary "XyZ") content-type
                                     (max-parts 1000) (max-part-head-size 16384))
  "PARSE-MULTIPART's reading of BODY, in terms a CHECK can compare: each
   part as (NAME FILENAME CONTENT-TYPE CONTENT), the content read as
   Latin-1 so any byte survives, or (:REFUSED status message)."
  (handler-case
      (mapcar (lambda (p)
                (list (form-part-name p) (form-part-filename p)
                      (form-part-content-type p)
                      (sb-ext:octets-to-string (form-part-body p)
                                               :external-format :latin-1)))
              (parse-multipart
               (make-test-request
                :method :post :path "/form"
                :headers (list (cons "content-type"
                                     (or content-type
                                         (format nil "multipart/form-data; boundary=~a"
                                                 boundary))))
                :body body)
               :max-parts max-parts :max-part-head-size max-part-head-size))
    (http-parse-error (e)
      (list :refused (http-parse-error-status e) (http-parse-error-message e)))))

(defun %refused-with (outcome)
  "The status of a refused %MULTIPART-OUTCOME, or the outcome itself."
  (if (and (consp outcome) (eq (first outcome) :refused))
      (second outcome)
      outcome))

(defun test-multipart-content-type ()
  "The boundary comes from the request's Content-Type: one boundary
   parameter, of RFC 2046's characters, one to seventy of them and not
   ending in a space. A Content-Type that is not multipart/form-data is
   415 — the request is in a format this does not read — and one that is,
   without a usable boundary, is 400."
  (format t "~%multipart: the Content-Type and its boundary~%")
  (let ((body (%form "--b" "Content-Disposition: form-data; name=x" "" "1" "--b--")))
    (flet ((with-type (type)
             (%refused-with (%multipart-outcome body :content-type type))))
      (check "content-type: a bare boundary"
             (with-type "multipart/form-data; boundary=b") '(("x" nil nil "1")))
      (check "content-type: a quoted boundary"
             (with-type "multipart/form-data; boundary=\"b\"") '(("x" nil nil "1")))
      (check "content-type: type and parameter names in any case, other parameters ignored"
             (with-type "Multipart/Form-Data; charset=utf-8; BOUNDARY=b")
             '(("x" nil nil "1")))
      (check "content-type: no Content-Type is 415"
             (%refused-with
              (handler-case (parse-multipart (make-test-request :method :post :body body))
                (http-parse-error (e) (list :refused (http-parse-error-status e)))))
             415)
      (check "content-type: another media type is 415"
             (with-type "application/x-www-form-urlencoded") 415)
      (check "content-type: multipart/mixed is 415" (with-type "multipart/mixed; boundary=b") 415)
      (check "content-type: no boundary is 400" (with-type "multipart/form-data") 400)
      (check "content-type: an empty boundary is 400"
             (with-type "multipart/form-data; boundary=\"\"") 400)
      (check "content-type: a boundary given twice is 400"
             (with-type "multipart/form-data; boundary=b; boundary=c") 400)
      (check "content-type: a boundary with a character RFC 2046 does not allow is 400"
             (with-type "multipart/form-data; boundary=\"b@\"") 400)
      (check "content-type: a boundary ending in a space is 400"
             (with-type "multipart/form-data; boundary=\"b \"") 400)
      (check "content-type: a boundary of 71 characters is 400"
             (with-type (format nil "multipart/form-data; boundary=~a"
                                (make-string 71 :initial-element #\b)))
             400)
      (check "content-type: an unterminated quoted value is 400"
             (with-type "multipart/form-data; boundary=\"b") 400)
      (check "content-type: two Content-Type fields are 400"
             (%refused-with
              (handler-case
                  (parse-multipart
                   (make-test-request :method :post
                                      :headers '(("content-type" . "multipart/form-data; boundary=b")
                                                 ("content-type" . "multipart/form-data; boundary=c"))
                                      :body body))
                (http-parse-error (e) (list :refused (http-parse-error-status e)))))
             400))
    ;; Seventy characters, every punctuation mark RFC 2046 allows and the
    ;; space it allows inside, is a boundary.
    (let ((long (concatenate 'string "'()+_,-./:=? " (make-string 57 :initial-element #\Z))))
      (check "content-type: seventy of the allowed characters are a boundary"
             (%multipart-outcome (%form (format nil "--~a" long)
                                        "Content-Disposition: form-data; name=x" "" "1"
                                        (format nil "--~a--" long))
                                 :content-type (format nil "multipart/form-data; boundary=\"~a\""
                                                       long))
             '(("x" nil nil "1"))))))

(defun test-multipart-parse ()
  "PARSE-MULTIPART reads what a browser sends, every way RFC 2046 lets the
   same parts be written, and refuses a body that leaves room for two
   readings."
  (format t "~%multipart: parsing a body~%")
  ;; ---- what a browser sends ----
  (check "parse: a text field and a file, as a browser sends them"
         (%multipart-outcome
          (%form "--XyZ"
                 "Content-Disposition: form-data; name=\"title\""
                 ""
                 "hello"
                 "--XyZ"
                 "Content-Disposition: form-data; name=\"file\"; filename=\"notes.txt\""
                 "Content-Type: text/plain"
                 ""
                 (format nil "line one~aline two" *crlf*)
                 "--XyZ--"
                 ""))
         `(("title" nil nil "hello")
           ("file" "notes.txt" "text/plain" ,(format nil "line one~aline two" *crlf*))))
  (check "parse: a file input with no file chosen has filename \"\" and no content"
         (%multipart-outcome
          (%form "--XyZ"
                 "Content-Disposition: form-data; name=\"f\"; filename=\"\""
                 "Content-Type: application/octet-stream"
                 ""
                 ""
                 "--XyZ--"))
         '(("f" "" "application/octet-stream" "")))
  (check "parse: a form with no fields is the close delimiter alone"
         (%multipart-outcome (%form "--XyZ--" "")) nil)
  (check "parse: and without the CRLF after it"
         (%multipart-outcome "--XyZ--") nil)
  (check "parse: a name sent twice is two parts, in order"
         (mapcar #'first (%multipart-outcome
                          (%form "--XyZ" "Content-Disposition: form-data; name=p" "" "1"
                                 "--XyZ" "Content-Disposition: form-data; name=p" "" "2"
                                 "--XyZ--")))
         '("p" "p"))
  ;; ---- values as browsers write them ----
  (check "parse: a backslash in a quoted value is an ordinary character"
         (second (first (%multipart-outcome
                         (%form "--XyZ"
                                "Content-Disposition: form-data; name=f; filename=\"C:\\dir\\a.txt\""
                                "" "x" "--XyZ--"))))
         "C:\\dir\\a.txt")
  (check "parse: a folder upload's relative path is passed as sent"
         (second (first (%multipart-outcome
                         (%form "--XyZ"
                                "Content-Disposition: form-data; name=f; filename=\"photos/2026/a.jpg\""
                                "" "x" "--XyZ--"))))
         "photos/2026/a.jpg")
  (check "parse: a quoted value keeps the ; and spaces inside it"
         (first (first (%multipart-outcome
                        (%form "--XyZ" "Content-Disposition: form-data; name=\"a; b\"" "" "x"
                               "--XyZ--"))))
         "a; b")
  (check "parse: UTF-8 in a filename"
         (second (first (%multipart-outcome
                         (%form "--XyZ" "Content-Disposition: form-data; name=f; filename=\"café.txt\""
                                "" "x" "--XyZ--"))))
         "café.txt")
  (check "parse: header names in any case, whitespace around =, bare values"
         (%multipart-outcome
          (%form "--XyZ"
                 "CONTENT-DISPOSITION: Form-Data;name = a ;filename= b.bin"
                 "content-type: image/png"
                 "" "x" "--XyZ--"))
         '(("a" "b.bin" "image/png" "x")))
  ;; ---- what the format says to ignore ----
  (check "parse: a preamble, transport padding and an epilogue are ignored"
         (%multipart-outcome
          (%form "a preamble"
                 (format nil "--XyZ ~c " #\Tab)
                 "Content-Disposition: form-data; name=a"
                 "" "A"
                 "--XyZ--  "
                 "an epilogue, --XyZ and all"))
         '(("a" nil nil "A")))
  (check "parse: a header field other than the three is kept and ignored"
         (let ((parts (handler-case
                          (parse-multipart
                           (make-test-request
                            :method :post
                            :headers '(("content-type" . "multipart/form-data; boundary=XyZ"))
                            :body (%form "--XyZ" "Content-Disposition: form-data; name=a"
                                         "X-Note: kept" "" "A" "--XyZ--")))
                        (http-parse-error () nil))))
           (form-part-headers (first parts)))
         '(("content-disposition" . "form-data; name=a") ("x-note" . "kept")))
  (check "parse: Content-Transfer-Encoding binary, 8bit and 7bit leave the content as sent"
         (loop for cte in '("binary" "8bit" "7BIT")
               collect (fourth (first (%multipart-outcome
                                       (%form "--XyZ" "Content-Disposition: form-data; name=a"
                                              (format nil "Content-Transfer-Encoding: ~a" cte)
                                              "" "QQ==" "--XyZ--")))))
         '("QQ==" "QQ==" "QQ=="))
  ;; ---- content near the boundary ----
  (check "parse: content holding what a delimiter begins with is content"
         (fourth (first (%multipart-outcome
                         (%form "--XyZ" "Content-Disposition: form-data; name=a" ""
                                "x" "--Xy" "-XyZ" "XyZ" "--XyZ--"))))
         (%form "x" "--Xy" "-XyZ" "XyZ"))
  (check "parse: any byte in content, the ones a delimiter is made of included"
         (fourth (first (%multipart-outcome
                         (concatenate '(simple-array (unsigned-byte 8) (*))
                                      (sb-ext:string-to-octets
                                       (%form "--XyZ" "Content-Disposition: form-data; name=a" "" "")
                                       :external-format :ascii)
                                      (coerce (loop for i below 256 collect i)
                                              '(simple-array (unsigned-byte 8) (*)))
                                      (sb-ext:string-to-octets (format nil "~a--XyZ--" *crlf*)
                                                               :external-format :ascii)))))
         (coerce (loop for i below 256 collect (code-char i)) 'string))
  ;; RFC 2046's grammar lets a part with no content have no line of it:
  ;; its head's blank line is then the delimiter's CRLF.
  (check "parse: an empty part written with no line of content"
         (%multipart-outcome
          (%form "--XyZ" "Content-Disposition: form-data; name=a" ""
                 "--XyZ" "Content-Disposition: form-data; name=b" "" "B"
                 "--XyZ--"))
         '(("a" nil nil "") ("b" nil nil "B")))
  (check "parse: and one written with an empty line of content"
         (%multipart-outcome
          (%form "--XyZ" "Content-Disposition: form-data; name=a" "" ""
                 "--XyZ--"))
         '(("a" nil nil "")))
  ;; ---- refused: two readings, or none ----
  (check "refused: a line beginning with the boundary that is not a delimiter"
         (%refused-with (%multipart-outcome
                         (%form "--XyZ" "Content-Disposition: form-data; name=a" "" "A"
                                "--XyZnot-a-delimiter" "--XyZ--")))
         400)
  ;; One stray byte between the boundary and its CRLF, with a well-formed
  ;; part after it: read as transport padding, this would be two parts.
  (check "refused: a boundary with one stray byte before its CRLF, a part after it"
         (%refused-with (%multipart-outcome
                         (%form "--XyZ" "Content-Disposition: form-data; name=a" "" "A"
                                "--XyZ!" "Content-Disposition: form-data; name=b" "" "B"
                                "--XyZ--")))
         400)
  (check "refused: a part's content beginning with the boundary"
         (%refused-with (%multipart-outcome
                         (%form "--XyZ" "Content-Disposition: form-data; name=a" ""
                                "--XyZX" "--XyZ--")))
         400)
  (check "refused: bare LF line ends"
         (%refused-with (%multipart-outcome
                         (format nil "--XyZ~aContent-Disposition: form-data; name=a~a~aA~a--XyZ--"
                                 #\Newline #\Newline #\Newline #\Newline)))
         400)
  (check "refused: a body ended before its close delimiter"
         (%refused-with (%multipart-outcome
                         (%form "--XyZ" "Content-Disposition: form-data; name=a" "" "A")))
         400)
  (check "refused: a body with no boundary at all"
         (%refused-with (%multipart-outcome "name=a&b=c")) 400)
  (check "refused: an empty body" (%refused-with (%multipart-outcome "")) 400)
  (check "refused: bytes after the close delimiter that do not end its line"
         (%refused-with (%multipart-outcome (%form "--XyZ--x"))) 400)
  (check "refused: a CR after the close delimiter with no LF"
         (%refused-with (%multipart-outcome
                         (concatenate 'string "--XyZ--" (string #\Return))))
         400)
  ;; ---- refused: a part's head ----
  (flet ((head (&rest fields)
           (%refused-with (%multipart-outcome
                           (apply #'%form "--XyZ" (append fields (list "" "A" "--XyZ--")))))))
    (check "head: no header fields" (head) 400)
    (check "head: no Content-Disposition" (head "Content-Type: text/plain") 400)
    (check "head: two Content-Disposition fields"
           (head "Content-Disposition: form-data; name=a" "Content-Disposition: form-data; name=b")
           400)
    (check "head: a disposition other than form-data"
           (head "Content-Disposition: attachment; name=a") 400)
    (check "head: no name" (head "Content-Disposition: form-data; filename=a") 400)
    (check "head: name given twice" (head "Content-Disposition: form-data; name=a; name=b") 400)
    (check "head: filename given twice"
           (head "Content-Disposition: form-data; name=a; filename=b; filename=c") 400)
    (check "head: filename* (RFC 7578 4.2 forbids it)"
           (head "Content-Disposition: form-data; name=a; filename*=utf-8''b.txt") 400)
    (check "head: name* too" (head "Content-Disposition: form-data; name*=utf-8''a") 400)
    (check "head: two Content-Type fields"
           (head "Content-Disposition: form-data; name=a" "Content-Type: a/b" "Content-Type: c/d")
           400)
    (check "head: Content-Transfer-Encoding base64"
           (head "Content-Disposition: form-data; name=a" "Content-Transfer-Encoding: base64")
           400)
    (check "head: Content-Transfer-Encoding quoted-printable"
           (head "Content-Disposition: form-data; name=a"
                 "Content-Transfer-Encoding: quoted-printable")
           400)
    (check "head: a field with no colon" (head "Content-Disposition: form-data; name=a" "junk") 400)
    (check "head: a parameter with no value" (head "Content-Disposition: form-data; name") 400)
    (check "head: a value that is not UTF-8"
           (%refused-with
            (%multipart-outcome
             (concatenate '(simple-array (unsigned-byte 8) (*))
                          (sb-ext:string-to-octets
                           (format nil "--XyZ~aContent-Disposition: form-data; name=\"" *crlf*)
                           :external-format :ascii)
                          #(#xE9)
                          (sb-ext:string-to-octets (format nil "\"~a~aA~a--XyZ--" *crlf* *crlf* *crlf*)
                                                   :external-format :ascii))))
           400))
  ;; ---- limits, refused 413 ----
  (let ((two (%form "--XyZ" "Content-Disposition: form-data; name=a" "" "A"
                    "--XyZ" "Content-Disposition: form-data; name=b" "" "B" "--XyZ--")))
    (check "limit: more parts than :max-parts" (%refused-with (%multipart-outcome two :max-parts 1))
           413)
    (check "limit: as many as :max-parts is within it"
           (length (%multipart-outcome two :max-parts 2)) 2)
    (check "limit: :max-parts NIL is no limit" (length (%multipart-outcome two :max-parts nil)) 2))
  (let ((body (%form "--XyZ" "Content-Disposition: form-data; name=a" "" "A" "--XyZ--")))
    ;; The head here is the field's 38 bytes, its CRLF and the blank line: 42.
    (check "limit: a part's head over :max-part-head-size"
           (%refused-with (%multipart-outcome body :max-part-head-size 41)) 413)
    (check "limit: a head exactly :max-part-head-size is within it"
           (length (%multipart-outcome body :max-part-head-size 42)) 1))
  (check "limit: more than sixteen header fields in a part"
         (%refused-with
          (%multipart-outcome
           (apply #'%form "--XyZ" "Content-Disposition: form-data; name=a"
                  (append (loop for i below 16 collect (format nil "X-~d: v" i))
                          (list "" "A" "--XyZ--")))))
         413)
  (check "limit: the arguments are checked"
         (list (and (stringp (attempt (parse-multipart (make-test-request) :max-parts -1))) t)
               (and (stringp (attempt (parse-multipart (make-test-request) :max-part-head-size 0))) t))
         '(t t))
  ;; ---- the helpers ----
  (let ((parts (handler-case
                   (parse-multipart
                    (make-test-request
                     :method :post
                     :headers '(("content-type" . "multipart/form-data; boundary=XyZ"))
                     :body (concatenate '(simple-array (unsigned-byte 8) (*))
                                        (sb-ext:string-to-octets
                                         (%form "--XyZ" "Content-Disposition: form-data; name=n"
                                                "" "Zoë"
                                                "--XyZ" "Content-Disposition: form-data; name=m"
                                                "" "1"
                                                "--XyZ" "Content-Disposition: form-data; name=m"
                                                "" "2"
                                                "--XyZ" "Content-Disposition: form-data; name=bad"
                                                "" "")
                                         :external-format :utf-8)
                                        #(#xFF)
                                        (sb-ext:string-to-octets (format nil "~a--XyZ--" *crlf*)
                                                                 :external-format :ascii))))
                 (http-parse-error () nil))))
    (check "form-part-text: a field's content as UTF-8"
           (form-part-text (find-form-part parts "n")) "Zoë")
    (check "find-form-part: the first of a name"
           (form-part-text (find-form-part parts "m")) "1")
    (check "find-form-parts: all of a name, in order"
           (mapcar #'form-part-text (find-form-parts parts "m")) '("1" "2"))
    (check "find-form-part: a name not sent" (find-form-part parts "nope") nil)
    (check "form-part-text: content that is not UTF-8 is 400"
           (handler-case (form-part-text (find-form-part parts "bad"))
             (http-parse-error (e) (http-parse-error-status e)))
           400)
    (check "form-part-text: a part with no body, as a streamed one has, is NIL"
           (form-part-text (web-skeleton::%make-form-part :name "s")) nil)))

(defun test-receive-multipart-contract ()
  "RECEIVE-MULTIPART checks its arguments when it is called, and reads the
   request's Content-Type there too: a request that is not
   multipart/form-data is refused at the head, from the body handler,
   before any of its body is read."
  (format t "~%receive-multipart: the contract~%")
  (let ((form (make-test-request :method :post
                                 :headers '(("content-type" . "multipart/form-data; boundary=b"))))
        (end (lambda (conn parts) (declare (ignore conn parts)) (make-text-response 200 "ok"))))
    (check "contract: a plan, for a body handler to return"
           (web-skeleton::body-plan-p (receive-multipart form :on-end end)) t)
    (check "contract: its cap is :max-size, and NIL is none"
           (list (web-skeleton::body-plan-max-size (receive-multipart form :on-end end :max-size 9))
                 (web-skeleton::body-plan-max-size (receive-multipart form :on-end end :max-size nil)))
           (list 9 most-positive-fixnum))
    (flet ((refused (&rest args)
             (and (stringp (attempt (apply #'receive-multipart form args))) t)))
      (check "contract: :on-end is required" (refused) t)
      (check "contract: a callback that is not a function"
             (refused :on-end end :on-part 42) t)
      (check "contract: a symbol naming a macro is not a function"
             (refused :on-end end :on-part-data 'when) t)
      (check "contract: :max-size negative" (refused :on-end end :max-size -1) t)
      (check "contract: :max-parts not a count" (refused :on-end end :max-parts "10") t)
      (check "contract: :max-part-head-size zero" (refused :on-end end :max-part-head-size 0) t)
      (check "contract: :max-collected-size negative" (refused :on-end end :max-collected-size -1) t))
    (check "contract: a request that is not multipart/form-data is 415, at the head"
           (handler-case (receive-multipart (make-test-request :method :post
                                                               :headers '(("content-type" . "text/plain")))
                                            :on-end end)
             (http-parse-error (e) (http-parse-error-status e)))
           415)
    (check "contract: multipart/form-data without a boundary is 400, at the head"
           (handler-case (receive-multipart (make-test-request :method :post
                                                               :headers '(("content-type" . "multipart/form-data")))
                                            :on-end end)
             (http-parse-error (e) (http-parse-error-status e)))
           400)))

(defmacro with-read-fn-connection ((conn kernel &key (fd 2000000000)) &body body)
  "Run BODY with CONN, a connection whose reads come from KERNEL, an
   adjustable byte vector standing in for the socket's receive queue, on a
   connection table and epoll fd of its own, with EPOLL-MODIFY recorded
   rather than called.

   FD is one no process has open, so the table can hold the connection —
   BODY-RESUME refuses one that is not this worker's — while a close, if a
   failure leads to one, closes nothing. EPOLL-MASKS is bound in BODY to the
   masks recorded, newest first."
  (let ((taken (gensym "TAKEN")) (real (gensym "REAL")))
    `(let* ((,taken 0)
            (epoll-masks nil)
            (,real (symbol-function 'web-skeleton::epoll-modify))
            (web-skeleton::*connections* (make-hash-table :test #'eql))
            (web-skeleton::*epoll-fd* 999)
            (,conn (web-skeleton::make-connection
                    :fd ,fd :last-active (get-universal-time)
                    :read-fn (lambda (buffer start max-bytes)
                               (let ((n (min max-bytes (- (fill-pointer ,kernel) ,taken))))
                                 (if (zerop n)
                                     :again
                                     (progn
                                       (replace buffer ,kernel :start1 start
                                                :start2 ,taken :end2 (+ ,taken n))
                                       (incf ,taken n)
                                       n)))))))
       (declare (ignorable epoll-masks))
       (web-skeleton::register-connection ,conn)
       (unwind-protect
            (progn
              (setf (symbol-function 'web-skeleton::epoll-modify)
                    (lambda (efd fd mask) (declare (ignore efd fd)) (push mask epoll-masks)))
              ,@body)
         (setf (symbol-function 'web-skeleton::epoll-modify) ,real)))))

(defun %supply (kernel bytes)
  "Append BYTES, a string or bytes, to KERNEL."
  (loop for b across (if (stringp bytes)
                         (sb-ext:string-to-octets bytes :external-format :latin-1)
                         bytes)
        do (vector-push-extend b kernel)))

(defun %kernel ()
  (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun %resume-and-read (conn)
  "What the loop does after BODY-RESUME: the writable wake-up it armed,
   whose :READ-BODY arm asks for a read, and the read."
  (when (eq (web-skeleton::handle-client-write conn 999) :keep-alive)
    (web-skeleton::handle-client-read conn 999 nil nil)))

(defun test-received-body-pause-within-a-run ()
  "A plan's feed that takes a run only partway and pauses there leaves the
   rest of the run buffered, and the read after BODY-RESUME hands it over
   first: every byte reaches the feed once and in order, a Content-Length
   body and a chunked one alike, and while the pause holds nothing more is
   handed over. A pause short of the body's last byte holds ON-END back,
   since the body has not all been taken.

   The feed is built by hand. RECEIVE-BODY's own takes every run whole;
   RECEIVE-MULTIPART's is where a pause inside a run comes from in
   practice, but the mechanism is the read path's, and is tested as that."
  (format t "~%Received body: a pause inside a run~%")
  (dolist (framing '(:length :chunked))
    (let* ((body (let ((v (make-array 3000 :element-type '(unsigned-byte 8))))
                   (dotimes (i 3000 v) (setf (aref v i) (mod (* i 7) 251)))))
           (taken (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
           (calls 0)
           (while-paused 0)
           (paused nil)
           (ended nil)
           (kernel (%kernel))
           (name (string-downcase (symbol-name framing))))
      (with-read-fn-connection (conn kernel)
        (let ((web-skeleton::*body-handler*
                (lambda (req)
                  (declare (ignore req))
                  (web-skeleton::%make-body-plan
                   :kind :receive :max-size most-positive-fixnum
                   :feed (lambda (c buf from to)
                           (declare (ignore c))
                           (when paused (incf while-paused))
                           (incf calls)
                           ;; Half of every run of two bytes or more, and a
                           ;; pause there; a run of one is taken whole.
                           (let ((upto (if (>= (- to from) 2)
                                           (+ from (floor (- to from) 2))
                                           to)))
                             (loop for i from from below upto
                                   do (vector-push-extend (aref buf i) taken))
                             (if (< upto to)
                                 (progn (setf paused t) (values :pause upto))
                                 (values nil to))))
                   :on-end (lambda (c) (declare (ignore c))
                             (setf ended t)
                             (make-text-response 200 "ok"))))))
          (%supply kernel (format nil "POST /u HTTP/1.1~aHost: x~a~a~a"
                                  *crlf* *crlf*
                                  (if (eq framing :length)
                                      (format nil "Content-Length: ~d~a" (length body) *crlf*)
                                      (format nil "Transfer-Encoding: chunked~a" *crlf*))
                                  *crlf*))
          (%supply kernel (if (eq framing :length)
                              body
                              (concatenate '(vector (unsigned-byte 8))
                                           (sb-ext:string-to-octets
                                            (format nil "~x~a" (length body) *crlf*)
                                            :external-format :ascii)
                                           body
                                           (sb-ext:string-to-octets
                                            (format nil "~a0~a~a" *crlf* *crlf* *crlf*)
                                            :external-format :ascii))))
          (web-skeleton::handle-client-read conn 999 nil nil)
          (check (format nil "~a: the first run, halved, and then a pause" name)
                 (list calls (fill-pointer taken) (web-skeleton::connection-body-paused conn))
                 (list 1 (floor (length body) 2) t))
          (check (format nil "~a: paused, it is subscribed to nothing" name)
                 (first epoll-masks) web-skeleton::+epollet+)
          ;; A stale event changes nothing while the pause holds.
          (web-skeleton::handle-client-read conn 999 nil nil)
          (check (format nil "~a: nothing is handed over while paused" name)
                 (list calls while-paused) '(1 0))
          ;; Resume until done: each resume hands over the rest of the run the
          ;; pause fell in, first.
          (loop repeat 40
                until ended
                do (setf paused nil)
                   (body-resume conn)
                   (%resume-and-read conn))
          (check (format nil "~a: every byte, once, in order" name)
                 (equalp (coerce taken '(simple-array (unsigned-byte 8) (*))) body) t)
          (check (format nil "~a: ON-END only once all of it was taken" name)
                 (list ended (web-skeleton::connection-body-received conn))
                 (list t (length body)))
          (check (format nil "~a: never called while a pause held" name)
                 while-paused 0))))))

(defun %one-byte-chunks (bytes)
  "BYTES framed as chunks of one byte each, with no terminator."
  (let ((out (%kernel)))
    (loop for b across bytes
          do (%supply out (format nil "1~a" *crlf*))
             (vector-push-extend b out)
             (%supply out *crlf*))
    out))

(defun test-received-chunked-body-one-run-per-read ()
  "A received chunked body reaches its plan one run a read, however small
   its chunks: RECEIVE-BODY's ON-DATA, and a streamed part's ON-PART-DATA
   under RECEIVE-MULTIPART, are called once for a read that brought a
   thousand one-byte chunks. Handed over a chunk at a time, a client
   choosing one-byte chunks would cost a call into the application, and
   through FORWARD-BODY a write upstream, for every byte."
  (format t "~%Received body: one run a read, however small the chunks~%")
  ;; ---- RECEIVE-BODY ----
  (let ((body (let ((v (make-array 2000 :element-type '(unsigned-byte 8))))
                (dotimes (i 2000 v) (setf (aref v i) (mod (* i 7) 251)))))
        (pieces '())
        (ended nil)
        (kernel (%kernel)))
    (with-read-fn-connection (conn kernel)
      (let ((web-skeleton::*body-handler*
              (lambda (req)
                (declare (ignore req))
                (receive-body :on-data (lambda (c bytes)
                                         (declare (ignore c))
                                         (push bytes pieces)
                                         nil)
                              :on-end (lambda (c)
                                        (declare (ignore c))
                                        (setf ended t)
                                        (make-text-response 200 "ok"))))))
        (%supply kernel (format nil "POST /u HTTP/1.1~aHost: x~a~
                                     Transfer-Encoding: chunked~a~a"
                                *crlf* *crlf* *crlf* *crlf*))
        (%supply kernel (%one-byte-chunks (subseq body 0 1000)))
        (web-skeleton::handle-client-read conn 999 nil nil)
        (check "receive-body: a read of a thousand one-byte chunks is one call"
               (mapcar #'length pieces) '(1000))
        (%supply kernel (%one-byte-chunks (subseq body 1000)))
        (%supply kernel (format nil "0~a~a" *crlf* *crlf*))
        (web-skeleton::handle-client-read conn 999 nil nil)
        (check "receive-body: and the next read, the rest and the end, one more"
               (list (mapcar #'length pieces) ended) '((1000 1000) t))
        (check "receive-body: the body, whole and in order"
               (equalp (apply #'concatenate '(vector (unsigned-byte 8)) (reverse pieces))
                       body)
               t))))
  ;; ---- RECEIVE-MULTIPART, a streamed part ----
  (let* ((file (make-array 1000 :element-type '(unsigned-byte 8) :initial-element 65))
         (form (concatenate '(vector (unsigned-byte 8))
                            (sb-ext:string-to-octets
                             (%form "--B0und"
                                    "Content-Disposition: form-data; name=\"up\"; filename=\"x.bin\""
                                    "" "")
                             :external-format :latin-1)
                            file
                            (sb-ext:string-to-octets (%form "" "--B0und--" "")
                                                     :external-format :latin-1)))
         (runs '())
         (answered nil)
         (kernel (%kernel)))
    (with-read-fn-connection (conn kernel)
      (let ((web-skeleton::*body-handler*
              (lambda (req)
                (receive-multipart
                 req
                 :on-part (lambda (c part) (declare (ignore c part)) :stream)
                 :on-part-data (lambda (c part bytes)
                                 (declare (ignore c part))
                                 (push bytes runs)
                                 nil)
                 :on-end (lambda (c parts)
                           (declare (ignore c parts))
                           (setf answered t)
                           (make-text-response 200 "stored"))))))
        (%supply kernel (format nil "POST /f HTTP/1.1~aHost: x~a~
                                     Content-Type: multipart/form-data; boundary=B0und~a~
                                     Transfer-Encoding: chunked~a~a"
                                *crlf* *crlf* *crlf* *crlf* *crlf*))
        (%supply kernel (%one-byte-chunks form))
        (%supply kernel (format nil "0~a~a" *crlf* *crlf*))
        (web-skeleton::handle-client-read conn 999 nil nil)
        (check "receive-multipart: a part in one-byte chunks, read at once, is one run"
               (list (length runs) answered) '(1 t))
        (check "receive-multipart: the part, whole"
               (equalp (apply #'concatenate '(vector (unsigned-byte 8)) (reverse runs))
                       file)
               t))))
  ;; ---- the cap, across the read's chunks ----
  ;; Twelve chunks of ten against a cap of 100, and framing no reader
  ;; accepts after them, all in one read. The eleventh chunk is the one
  ;; that passes the cap, and the refusal is its 413, before the walk
  ;; reaches the bad framing: the run gathered so far counts, not only the
  ;; chunk in hand.
  (let ((handed 0)
        (aborted nil)
        (kernel (%kernel)))
    (with-read-fn-connection (conn kernel)
      (let ((web-skeleton::*body-handler*
              (lambda (req)
                (declare (ignore req))
                (receive-body :max-size 100
                              :on-data (lambda (c bytes)
                                         (declare (ignore c))
                                         (incf handed (length bytes))
                                         nil)
                              :on-end (lambda (c)
                                        (declare (ignore c))
                                        (make-text-response 200 "ok"))
                              :on-abort (lambda (c reason)
                                          (declare (ignore c))
                                          (push reason aborted)))))
            (web-skeleton:*log-level* :error))
        (%supply kernel (format nil "POST /u HTTP/1.1~aHost: x~a~
                                     Transfer-Encoding: chunked~a~a"
                                *crlf* *crlf* *crlf* *crlf*))
        (dotimes (i 12)
          (%supply kernel (format nil "a~a0123456789~a" *crlf* *crlf*)))
        (%supply kernel (format nil "zz~a" *crlf*))
        (web-skeleton::handle-client-read conn 999 nil nil)
        (check "cap: chunks that pass it together in one read are refused at the one that does"
               (list aborted handed) '((:too-large) 0))))))

(defun test-receive-multipart-pauses-where-it-falls ()
  "A received form's pause holds the body at the event that asked for it,
   inside a read as well as at its end: nothing more of the body is parsed
   and no callback is called until BODY-RESUME, and then the parse goes on
   from that event.

   The whole form arrives in one read, so every pause falls inside it. The
   first part's :ON-PART-END pauses with the second part in the same
   buffer; the streamed part's first run pauses; and the last part's
   :ON-PART-END pauses with the close delimiter's CRLF still unread, which
   holds :ON-END back."
  (format t "~%receive-multipart: a pause holds where it falls~%")
  (let* ((events '())
         (paused nil)
         (while-paused 0)
         (answered nil)
         (aborted '())
         (kernel (%kernel))
         (file (let ((v (make-array 2000 :element-type '(unsigned-byte 8))))
                 (dotimes (i 2000 v) (setf (aref v i) (mod (* i 13) 256)))))
         (got-file (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (form (concatenate '(vector (unsigned-byte 8))
                            (sb-ext:string-to-octets
                             (%form "--B0und"
                                    "Content-Disposition: form-data; name=\"token\""
                                    "" "t0k"
                                    "--B0und"
                                    "Content-Disposition: form-data; name=\"up\"; filename=\"x.bin\""
                                    "" "")
                             :external-format :latin-1)
                            file
                            (sb-ext:string-to-octets
                             (%form "" "--B0und"
                                    "Content-Disposition: form-data; name=\"note\""
                                    "" "last"
                                    "--B0und--"
                                    "")
                             :external-format :latin-1))))
    (flet ((note (&rest event)
             (when paused (incf while-paused))
             (push event events)))
      (with-read-fn-connection (conn kernel)
        (let ((web-skeleton::*body-handler*
                (lambda (req)
                  (receive-multipart
                   req
                   :on-part (lambda (c part)
                              (declare (ignore c))
                              (note :part (form-part-name part))
                              (if (form-part-filename part) :stream nil))
                   :on-part-data (lambda (c part bytes)
                                   (declare (ignore c part))
                                   (note :data (length bytes))
                                   (loop for b across bytes do (vector-push-extend b got-file))
                                   ;; The first run of the file pauses.
                                   (when (= (fill-pointer got-file) (length bytes))
                                     (setf paused t)
                                     :pause))
                   :on-part-end (lambda (c part)
                                  (declare (ignore c))
                                  (note :part-end (form-part-name part))
                                  (when (member (form-part-name part) '("token" "note")
                                                :test #'string=)
                                    (setf paused t)
                                    :pause))
                   :on-end (lambda (c parts)
                             (declare (ignore c))
                             (note :end (mapcar #'form-part-text parts))
                             (setf answered t)
                             (make-text-response 200 "stored"))
                   :on-abort (lambda (c reason) (declare (ignore c)) (push reason aborted))))))
          (%supply kernel (format nil "POST /f HTTP/1.1~aHost: x~a~
                                       Content-Type: multipart/form-data; boundary=B0und~a~
                                       Content-Length: ~d~a~a"
                                  *crlf* *crlf* *crlf* (length form) *crlf* *crlf*))
          (%supply kernel form)
          (web-skeleton::handle-client-read conn 999 nil nil)
          (check "paused: the first part's end pauses, the second part still unread"
                 (reverse events) '((:part "token") (:part-end "token")))
          (web-skeleton::handle-client-read conn 999 nil nil)
          (check "paused: a stale read changes nothing" (length events) 2)
          (setf paused nil)
          (body-resume conn)
          (%resume-and-read conn)
          (check "paused: resumed, the file's head and its first run, and a pause"
                 (mapcar #'first (subseq (reverse events) 2)) '(:part :data))
          (setf paused nil)
          (body-resume conn)
          (%resume-and-read conn)
          (check "paused: resumed, the rest to the last part's end, which pauses"
                 (list (car events) answered)
                 '((:part-end "note") nil))
          (check "paused: with the close delimiter's line still unread, ON-END waits"
                 (< (web-skeleton::connection-body-received conn) (length form)) t)
          (setf paused nil)
          (body-resume conn)
          (%resume-and-read conn)
          (check "paused: resumed once more, ON-END answers"
                 (list answered (first (car events))) '(t :end))
          (check "paused: the file arrived whole and in order"
                 (equalp (coerce got-file '(simple-array (unsigned-byte 8) (*))) file) t)
          (check "paused: ON-END had the collected fields, and not the streamed file"
                 (second (find :end events :key #'first)) '("t0k" "last"))
          (check "paused: no callback ran while a pause held" while-paused 0)
          (check "paused: and nothing was aborted" aborted nil))))))

(defun test-received-form-holds-its-bytes ()
  "A collected part that arrives a byte a read holds about what it counts.

   The limit on what a received form collects counts bytes, while what a
   part holds was kept a read at a time: a client sending a byte a packet
   had each byte cost a cell and a vector besides, dozens of times what was
   counted. Fifty thousand bytes of a part are read here one at a time, and
   the heap measured across them, after a full collection either side, has
   grown by less than eight bytes for each; kept a read at a time it grows
   by about forty-eight. The part then arrives whole, in order.

   The kernel stands in for the socket and is sized for the whole request
   up front, so its own growth is not in the measure."
  (format t "~%receive-multipart: a part a byte a read holds its bytes~%")
  (let* ((n 60000)
         (data (let ((v (make-array n :element-type '(unsigned-byte 8))))
                 (dotimes (i n v) (setf (aref v i) (mod (* i 31) 256)))))
         (head-of-part (sb-ext:string-to-octets
                        (format nil "--B0und~aContent-Disposition: form-data; name=\"big\"~a~a"
                                *crlf* *crlf* *crlf*)
                        :external-format :latin-1))
         (tail (sb-ext:string-to-octets (format nil "~a--B0und--~a" *crlf* *crlf*)
                                        :external-format :latin-1))
         (form (concatenate '(simple-array (unsigned-byte 8) (*)) head-of-part data tail))
         (request (sb-ext:string-to-octets
                   (format nil "POST /f HTTP/1.1~aHost: x~a~
                                Content-Type: multipart/form-data; boundary=B0und~a~
                                Content-Length: ~d~a~a"
                           *crlf* *crlf* *crlf* (length form) *crlf* *crlf*)
                   :external-format :latin-1))
         (kernel (make-array (+ (length request) (length form))
                             :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (got nil)
         (grown nil))
    (with-read-fn-connection (conn kernel)
      (let ((web-skeleton::*body-handler*
              (lambda (req)
                (receive-multipart
                 req
                 :on-end (lambda (c parts)
                           (declare (ignore c))
                           (setf got (form-part-body (first parts)))
                           (make-text-response 200 "stored")))))
            (web-skeleton:*log-stream* (make-broadcast-stream)))
        (flet ((read-bytes (from to)
                 (loop for i from from below to
                       do (vector-push (aref form i) kernel)
                          (web-skeleton::handle-client-read conn 999 nil nil))))
          (%supply kernel request)
          (web-skeleton::handle-client-read conn 999 nil nil)
          ;; The part's head, and the first thousand bytes of its body, so
          ;; what is measured is the part growing.
          (read-bytes 0 (+ (length head-of-part) 1000))
          (sb-ext:gc :full t)
          (let ((before (sb-kernel:dynamic-usage))
                (measured 50000))
            (read-bytes (+ (length head-of-part) 1000)
                        (+ (length head-of-part) 1000 measured))
            (sb-ext:gc :full t)
            (setf grown (/ (- (sb-kernel:dynamic-usage) before) measured)))
          (loop for i from (+ (length head-of-part) 1000 50000) below (length form)
                do (vector-push (aref form i) kernel))
          (web-skeleton::handle-client-read conn 999 nil nil))))
    (check "a byte a read: the part holds under eight bytes for each it counts"
           (< grown 8) t)
    (check "a byte a read: and arrives whole, in order" (equalp got data) t)))

(defun test-multipart-parameters-in-linear-time ()
  "A header value's parameters are read in time that grows with their
   count, not its square.

   Each parameter's name is checked against those before it, since one
   given twice is refused, and a part head of the default 16 KiB holds
   thousands of them. Forty thousand are read here in well under a second;
   searched for one by one, they take seconds. One given twice is still
   refused."
  (format t "~%multipart: parameters read in linear time~%")
  (let* ((n 40000)
         (value (with-output-to-string (s)
                  (write-string "form-data" s)
                  (dotimes (i n) (format s "; p~d=v" i))))
         (start (get-internal-real-time))
         (params (web-skeleton::%parse-parameters value "test"))
         (seconds (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))
    (check "many parameters: every one read" (length params) n)
    (check "many parameters: in well under a second" (< seconds 1) t)
    (check "many parameters: one given twice is still refused"
           (handler-case (progn (web-skeleton::%parse-parameters
                                 (concatenate 'string value "; P17=w") "test")
                                nil)
             (http-parse-error (e) (http-parse-error-status e)))
           400)))

(defun test-streamed-fetch-contract ()
  "A fetch whose body the application streams: what HTTP-FETCH accepts,
   the head BUILD-OUTBOUND-REQUEST writes for one, and what FETCH-SEND and
   FETCH-FINISH answer and refuse before any connection is involved."
  (format t "~%Streamed request bodies: the contract~%")
  (flet ((streamed (&rest args)
           (apply #'http-fetch :put "http://127.0.0.1:9/u" :body :stream
                  :then (lambda (s h b) (declare (ignore s h b)) nil)
                  args))
         (bytes (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 97))
         (head (&rest args)
           (sb-ext:octets-to-string
            (apply #'web-skeleton::build-outbound-request :put "h" "/u" args)
            :external-format :latin-1)))
    ;; ---- making one ----
    (check "make: :content-length without :body :stream is refused"
           (and (stringp (attempt (http-fetch :put "http://h/" :body "x" :content-length 1
                                              :then #'identity)))
                t)
           t)
    (check "make: a negative :content-length is refused"
           (and (stringp (attempt (streamed :content-length -1))) t) t)
    (check "make: a streamed fetch has no whole body, and is not started"
           (let ((f (streamed)))
             (list (web-skeleton::http-fetch-continuation-body f) (fetch-started-p f)))
           '(nil nil))
    ;; ---- its head ----
    (check "head: a declared length is the Content-Length"
           (let ((h (head :streamed t :declared-length 5)))
             (list (and (search "content-length: 5" h) t)
                   (and (search "transfer-encoding" h) t)
                   (subseq h (- (length h) 4))))
           (list t nil (format nil "~a~a" *crlf* *crlf*)))
    (check "head: no declared length is chunked"
           (let ((h (head :streamed t)))
             (list (and (search "transfer-encoding: chunked" h) t)
                   (and (search "content-length" h) t)))
           '(t nil))
    (check "head: a caller's Transfer-Encoding is still refused"
           (and (stringp (attempt (head :streamed t
                                        :headers '(("transfer-encoding" . "chunked")))))
                t)
           t)
    (check "head: and a caller's Content-Length"
           (and (stringp (attempt (head :streamed t :declared-length 3
                                        :headers '(("content-length" . "3")))))
                t)
           t)
    ;; ---- sending before it has started: held for it ----
    (let ((f (streamed :content-length 6)))
      (check "send: before the start, the bytes are held and it answers NIL"
             (list (fetch-send f (bytes 4))
                   (web-skeleton::http-fetch-continuation-pending-bytes f)
                   (web-skeleton::http-fetch-continuation-sent f))
             '(nil 4 4))
      (check "send: an empty piece sends nothing, and answers as a send would"
             (list (fetch-send f (bytes 0))
                   (web-skeleton::http-fetch-continuation-pending-bytes f)
                   (web-skeleton::http-fetch-continuation-sent f))
             '(nil 4 4))
      (check "send: past the declared length is refused, and nothing is held"
             (list (and (stringp (attempt (fetch-send f (bytes 3)))) t)
                   (web-skeleton::http-fetch-continuation-sent f))
             '(t 4))
      (check "finish: short of the declared length is refused"
             (and (stringp (attempt (fetch-finish f))) t) t)
      (fetch-send f (bytes 2))
      (check "finish: at the declared length, T"
             (fetch-finish f) t)
      (check "finish: twice is once" (fetch-finish f) t)
      (check "send: after the finish is refused"
             (and (stringp (attempt (fetch-send f (bytes 1)))) t) t))
    (let ((f (streamed))
          (web-skeleton:*max-write-backlog* 16))
      (fetch-send f (bytes 6))
      (check "send: sending on through NIL is refused at *max-write-backlog*"
             (list (and (stringp (attempt (fetch-send f (bytes 20)))) t)
                   (web-skeleton::http-fetch-continuation-sent f))
             '(t 6)))
    (let ((f (streamed)))
      (fetch-send f (bytes 3))
      (fetch-finish f)
      (check "chunked: each send a chunk, and the finish its terminator"
             (sb-ext:octets-to-string
              (apply #'concatenate '(vector (unsigned-byte 8))
                     (reverse (web-skeleton::http-fetch-continuation-pending f)))
              :external-format :latin-1)
             (format nil "3~aaaa~a0~a~a" *crlf* *crlf* *crlf* *crlf*)))
    ;; Each piece held is added without walking those before it: fifty
    ;; thousand sends, each through a NIL as a producer not pausing would,
    ;; take well under a second, where a walk of the list each time takes
    ;; seconds. Sent past NIL only to have many pieces held; the backlog is
    ;; raised to hold them.
    (let ((f (streamed :content-length 50000))
          (web-skeleton:*max-write-backlog* most-positive-fixnum)
          (start (get-internal-real-time)))
      (dotimes (i 50000) (fetch-send f (bytes 1)))
      (check "send: many pieces held, each added in constant time"
             (list (length (web-skeleton::http-fetch-continuation-pending f))
                   (< (- (get-internal-real-time) start)
                      internal-time-units-per-second))
             '(50000 t)))
    ;; ---- an ended fetch ----
    (let ((f (streamed)))
      (setf (web-skeleton::http-fetch-continuation-ended f) t)
      (check "ended: a send is dropped and answers :ENDED" (fetch-send f (bytes 3)) :ended)
      (check "ended: and a finish" (fetch-finish f) :ended))
    ;; ---- a fetch given its body whole ----
    (let ((f (http-fetch :put "http://h/" :body "whole" :then #'identity)))
      (check "whole: FETCH-SEND refuses it"
             (and (stringp (attempt (fetch-send f (bytes 1)))) t) t)
      (check "whole: FETCH-FINISH refuses it"
             (and (stringp (attempt (fetch-finish f))) t) t)
      (setf (web-skeleton::http-fetch-continuation-target-fd f) 5)
      (check "whole: never counts as started, however often it is returned"
             (fetch-started-p f) nil)
      (check "forward-body: refuses a fetch given its body whole"
             (and (stringp (attempt (forward-body f))) t) t))
    ;; ---- carried by a worker: refused from anywhere else ----
    (let ((f (streamed))
          (carrier (web-skeleton::make-connection :fd 7 :state :out-send :outbound-p t)))
      (setf (web-skeleton::http-fetch-continuation-carrier f) carrier
            (web-skeleton::http-fetch-continuation-target-fd f) 6)
      (check "started: counts as started" (fetch-started-p f) t)
      (check "forward-body: refuses a fetch already started"
             (and (stringp (attempt (forward-body f))) t) t)
      (let ((web-skeleton::*epoll-fd* nil))
        (check "worker: a send off a worker is refused"
               (and (stringp (attempt (fetch-send f (bytes 1)))) t) t))
      (let ((web-skeleton::*epoll-fd* 999)
            (web-skeleton::*connections* (make-hash-table :test #'eql)))
        (check "worker: a send on a fetch another worker carries is refused"
               (and (stringp (attempt (fetch-send f (bytes 1)))) t) t)
        (check "worker: and nothing was counted as sent"
               (web-skeleton::http-fetch-continuation-sent f) 0)))))

(defun test-streamed-fetch-sweep ()
  "The sweep's rules for a fetch still sending a streamed request body.

   A request answered with such a fetch is parked on its outbound, and is
   not answered 504 by the :AWAITING clock while its request is still going
   out: its wait for the answer starts when the request has gone
   (BEGIN-RESPONSE-READ). The outbound is bound meanwhile by a deadline of
   its own, which the body's progress moves and which ends the fetch once
   it passes, the parked request answered 502 as for any failed fetch.

   Control: the same parked request on an outbound reading its answer is
   answered 504, so the first is spared for what its outbound is doing."
  (format t "~%Streamed request bodies: the sweep~%")
  (flet ((sweep (&key (out-state :out-send) (deadline-in 10))
           ;; A request parked longer than *FETCH-TIMEOUT* on an outbound in
           ;; OUT-STATE whose deadline is DEADLINE-IN seconds away. Answers
           ;; the request's state, the status line it was answered with if
           ;; any, and whether the outbound is still registered.
           (let ((epfd (web-skeleton::epoll-create))
                 (in-fd (web-skeleton::epoll-create))
                 (out-fd (web-skeleton::epoll-create))
                 (now (get-universal-time)))
             (unwind-protect
                  (let* ((web-skeleton::*connections* (make-hash-table :test #'eql))
                         (web-skeleton:*log-stream* (make-broadcast-stream))
                         (in (web-skeleton::make-connection
                              :fd in-fd :state :awaiting
                              :last-active (- now web-skeleton:*fetch-timeout* 5)))
                         (out (web-skeleton::make-connection
                               :fd out-fd :state out-state :outbound-p t
                               :inbound-fd in-fd :fetch-sink :inbound
                               :fetch-deadline (+ now deadline-in)
                               :last-active now)))
                    (setf (web-skeleton::connection-awaiting-fd in) out-fd)
                    (dolist (fd (list in-fd out-fd))
                      (web-skeleton::epoll-add epfd fd (logior web-skeleton::+epollin+
                                                               web-skeleton::+epollet+)))
                    (web-skeleton::register-connection in)
                    (web-skeleton::register-connection out)
                    (web-skeleton::sweep-idle-connections epfd now)
                    (let ((buf (web-skeleton::connection-write-buf in)))
                      (list (web-skeleton::connection-state in)
                            (and buf
                                 (let ((text (sb-ext:octets-to-string
                                              buf :external-format :latin-1)))
                                   (subseq text 0 (search *crlf* text))))
                            (and (web-skeleton::lookup-connection out-fd) t))))
               (ignore-errors (web-skeleton::%close in-fd))
               (ignore-errors (web-skeleton::%close out-fd))
               (ignore-errors (web-skeleton::%close epfd))))))
    (check "sweep: a request parked on a request still going out is left waiting"
           (sweep) '(:awaiting nil t))
    (check "control: parked on an outbound reading its answer, it is answered 504"
           (sweep :out-state :out-read) '(:write-response "HTTP/1.1 504 Gateway Timeout" nil))
    (check "sweep: a request body standing still past its deadline ends the fetch, and 502"
           (sweep :deadline-in -5) '(:write-response "HTTP/1.1 502 Bad Gateway" nil))))

(defun test-sweep-acts-only-on-what-is-still-due ()
  "The sweep collects what is due and then acts on it, and each connection
   is judged again when its turn comes.

   A received body standing still past *BODY-STALL-TIMEOUT*, and a detached
   fetch into its connection past its own deadline in the same sweep, as
   FORWARD-BODY's upload and the body it forwards are at equal settings.
   The fetch's end is acted on first, and its :THEN refuses the body with a
   502, as FORWARD-BODY's does. The body was collected as stalled, but by
   its turn it has been answered, so it is not closed under its answer, and
   ON-ABORT's :REFUSED is the truth. Control: the same body with no fetch
   into it is closed as stalled.

   And two detached fetches past their deadlines, each into a body of its
   own and each :THEN stopping the other, so whichever ends first stops
   the second. The second was collected as expired, but by its turn the
   application has stopped it, and a stopped fetch leaves its connection
   as it was: only the first fetch's body is ended, by that fetch's own
   failure."
  (format t "~%The sweep: what is due is judged again at its turn~%")
  (labels ((receiving (now &rest slots)
             ;; A connection receiving a body of 100000 bytes, on a
             ;; descriptor of its own, with SLOTS over the defaults.
             (let ((head (sb-ext:string-to-octets
                          (format nil "POST /up HTTP/1.1~aHost: x~a~
                                       Content-Length: 100000~a~a"
                                  *crlf* *crlf* *crlf* *crlf*)
                          :external-format :latin-1)))
               (apply #'web-skeleton::make-connection
                      (append slots
                              (list :fd (web-skeleton::epoll-create)
                                    :state :read-body
                                    :request (web-skeleton::parse-request-bytes
                                              head 0 (length head))
                                    :last-active now
                                    :body-progress-at now)))))
           (detached-into (target now &rest slots)
             ;; A detached fetch into TARGET past its deadline, on a
             ;; descriptor of its own, with SLOTS over the defaults.
             (apply #'web-skeleton::make-connection
                    (append slots
                            (list :fd (web-skeleton::epoll-create)
                                  :state :out-read :outbound-p t
                                  :inbound-fd (web-skeleton::connection-fd target)
                                  :fetch-sink :detached
                                  :fetch-deadline (- now 5)
                                  :last-active now))))
           (sweep-once (epfd now registered)
             ;; Put REGISTERED on the table, and in EPFD's interest as the
             ;; event loop would, and sweep once.
             (dolist (conn registered)
               (web-skeleton::epoll-add epfd (web-skeleton::connection-fd conn)
                                        (logior web-skeleton::+epollin+
                                                web-skeleton::+epollet+))
               (web-skeleton::register-connection conn))
             (web-skeleton::sweep-idle-connections epfd now))
           (on-a-table (fn)
             ;; FN called with an epoll fd and the time, inside a
             ;; connection table of its own. FN answers its result and the
             ;; connections it made, which are closed afterwards: closing
             ;; one the sweep closed already is a no-op.
             (let ((epfd (web-skeleton::epoll-create))
                   (conns nil))
               (unwind-protect
                    (let ((web-skeleton::*connections* (make-hash-table :test #'eql))
                          (web-skeleton::*epoll-fd* epfd)
                          (web-skeleton:*log-stream* (make-broadcast-stream)))
                      (multiple-value-bind (result made)
                          (funcall fn epfd (get-universal-time))
                        (setf conns made)
                        result))
                 (dolist (conn conns)
                   (ignore-errors (web-skeleton::connection-close conn)))
                 (ignore-errors (web-skeleton::%close epfd)))))
           (stalled-body (&key with-fetch)
             ;; A received body stalled past its clock and, WITH-FETCH, a
             ;; detached fetch into it past its deadline whose :THEN refuses
             ;; the body. Answers the body's state, the status line queued on
             ;; it, and what its ON-ABORT heard.
             (on-a-table
              (lambda (epfd now)
                (let* ((heard nil)
                       (in (receiving now
                                      :last-active (- now 60)
                                      :body-progress-at (- now web-skeleton:*body-stall-timeout* 5)
                                      :fetch-outstanding with-fetch
                                      :fetch-failure-disposition :keep
                                      :body-receiver
                                      (receive-body
                                       :on-data (lambda (conn bytes)
                                                  (declare (ignore conn bytes))
                                                  nil)
                                       :on-end (lambda (conn) (declare (ignore conn)) nil)
                                       :on-abort (lambda (conn reason)
                                                   (declare (ignore conn))
                                                   (setf heard reason)))))
                       (out (detached-into in now
                                           :state :out-send
                                           :fetch-callback
                                           (lambda (status headers body)
                                             (declare (ignore status headers body))
                                             (body-refuse in (make-error-response 502))
                                             nil))))
                  (sweep-once epfd now (if with-fetch (list in out) (list in)))
                  (values (list (web-skeleton::connection-state in)
                                (and (plusp (web-skeleton::connection-write-pending in))
                                     (let ((text (sb-ext:octets-to-string
                                                  (web-skeleton::connection-write-buf in)
                                                  :external-format :latin-1)))
                                       (subseq text 0 (search *crlf* text))))
                                heard)
                          (list in out)))))))
    (check "sweep: a stalled body answered by its fetch's end keeps its answer"
           (stalled-body :with-fetch t)
           '(:write-response "HTTP/1.1 502 Bad Gateway" :refused))
    (check "control: with no fetch into it, the stalled body is closed as stalled"
           (stalled-body) '(:closing nil :stalled))
    (check "sweep: a fetch stopped before its turn leaves its body as it was"
           (on-a-table
            (lambda (epfd now)
              (let* ((bodies (loop repeat 2
                                   collect (receiving
                                            now
                                            :fetch-outstanding t
                                            :body-receiver
                                            (receive-body
                                             :on-data (lambda (conn bytes)
                                                        (declare (ignore conn bytes))
                                                        nil)
                                             :on-end (lambda (conn)
                                                       (declare (ignore conn))
                                                       nil)))))
                     (fetches (mapcar (lambda (body) (detached-into body now)) bodies)))
                (flet ((stopping (other)
                         (lambda (status headers body)
                           (declare (ignore status headers body))
                           (web-skeleton::stop-fetch other epfd)
                           nil)))
                  (setf (web-skeleton::connection-fetch-callback (first fetches))
                        (stopping (second fetches))
                        (web-skeleton::connection-fetch-callback (second fetches))
                        (stopping (first fetches))))
                (sweep-once epfd now (append bodies fetches))
                (values (sort (mapcar #'web-skeleton::connection-state bodies) #'string<)
                        (append bodies fetches)))))
           '(:closing :read-body))))

(defun test-body-handler-validated-at-boot ()
  "START-SERVER refuses a :BODY-HANDLER it could not call, and a
   *BODY-STALL-TIMEOUT* that would leave an accepted body with no time
   bound, before it binds.

   :HOST is deliberately invalid, as in TEST-ON-TICK-VALIDATED-AT-BOOT, so
   a revert fails these at MAKE-TCP-LISTENER instead of starting a server."
  (format t "~%start-server: the body handler's boot invariants~%")
  (flet ((boot (&rest args)
           (attempt (apply #'web-skeleton:start-server
                           :host #(1 2 3) :port 0 :workers 1 args))))
    (check "boot: a non-function :body-handler is refused by name"
           (let ((msg (boot :body-handler 42)))
             (and (stringp msg) (search ":body-handler" msg) t))
           t)
    (check "boot: a symbol naming a macro is refused"
           (let ((msg (boot :body-handler 'when)))
             (and (stringp msg) (search ":body-handler" msg) t))
           t)
    (check "boot: a symbol naming a function is accepted"
           (let ((msg (boot :body-handler 'identity)))
             (and (stringp msg) (not (search ":body-handler" msg)) t))
           t)
    (check "boot: *body-stall-timeout* 0 is refused by name"
           (let ((msg (let ((web-skeleton:*body-stall-timeout* 0)) (boot))))
             (and (stringp msg) (search "*body-stall-timeout*" msg) t))
           t)
    (check "control: the default *body-stall-timeout* passes the check"
           (let ((msg (boot)))
             (and (stringp msg) (not (search "*body-stall-timeout*" msg)) t))
           t)))

;;; ---------------------------------------------------------------------------
;;; A request that can never be finished does not keep its slot
;;; ---------------------------------------------------------------------------

(defun test-unfinishable-request-closes ()
  (format t "~%Unfinishable request + peer FIN~%")
  (labels ((drive (raw eof-verdict)
             ;; RAW arrives in one read, then the peer's end-of-stream.
             ;; CONNECTION-READ-AVAILABLE reports :OK-EOF for bytes and
             ;; FIN together and :EOF for FIN alone, and both mean the
             ;; same thing to a request still waiting on bytes.
             (let* ((bytes (sb-ext:string-to-octets raw
                                                    :external-format :ascii))
                    (sent (list nil))
                    (conn (web-skeleton::make-connection
                           :fd -1
                           :read-fn
                           (lambda (buffer start max-bytes)
                             (cond
                               ((car sent) eof-verdict)
                               (t (setf (car sent) t)
                                  (let ((n (min (length bytes) max-bytes)))
                                    (replace buffer bytes :start1 start
                                                          :end2 n)
                                    n)))))))
               (handler-case (web-skeleton::connection-on-read conn)
                 (web-skeleton:http-parse-error (e)
                   (web-skeleton::http-parse-error-status e))
                 (error (e) (princ-to-string e))))))

    ;; Headers that never terminate. Nothing more is coming, so waiting
    ;; for a CRLFCRLF that cannot arrive holds a connection slot for
    ;; *IDLE-TIMEOUT* with no peer on the other end — free for whoever
    ;; sent it and walked away.
    (check "unfinishable: half a request line with FIN closes"
           (drive "GET /par" :eof) :close)

    ;; A Content-Length body short of its declared length.
    (check "unfinishable: a body short of its Content-Length closes"
           (drive (format nil "POST /u HTTP/1.1~aHost: x~aContent-Length: 10~a~aabc"
                          *crlf* *crlf* *crlf* *crlf*)
                  :eof)
           :close)

    ;; And chunked, which is the framing that has no declared length to
    ;; be short of — the terminator is missing and no byte can supply it.
    ;; Broken state: the check reads BODY-EXPECTED, which is 0 here, and
    ;; a half-arrived chunked upload is held rather than closed.
    (check "unfinishable: a chunked body without its terminator closes"
           (drive (format nil "POST /u HTTP/1.1~aHost: x~aTransfer-Encoding: chunked~a~a3~aab"
                          *crlf* *crlf* *crlf* *crlf* *crlf*)
                  :eof)
           :close)

    ;; The other half of the rule, and the one that must not move: a
    ;; *complete* request arriving with its own FIN is answered, not
    ;; closed. This is what TEST-HARNESS-PIPELINED-WITH-FIN-E2E covers
    ;; end to end; asserted here too because it is the failure this
    ;; change could plausibly cause and a unit check names it directly.
    (check "unfinishable: a complete request with FIN still dispatches"
           (drive (format nil "GET / HTTP/1.1~aHost: x~a~a" *crlf* *crlf* *crlf*)
                  :eof)
           :dispatch)
    (check "unfinishable: a complete chunked request with FIN dispatches"
           (drive (format nil "POST /u HTTP/1.1~aHost: x~aTransfer-Encoding: chunked~a~a3~aabc~a0~a~a"
                          *crlf* *crlf* *crlf* *crlf* *crlf* *crlf* *crlf* *crlf*)
                  :eof)
           :dispatch)))

;;; ---------------------------------------------------------------------------
;;; An unterminated head is refused at its budget
;;; ---------------------------------------------------------------------------

(defun test-unterminated-head-refused-at-its-budget ()
  "A request head still unterminated at REQUEST-HEAD-BUDGET is refused
   then, with the status its first fault earns, not buffered toward the
   read cap, which also makes room for a body.

   Small budgets so the arithmetic is exact: the head budget is 64 + 64 +
   2*4 + 4 = 140 bytes, the read cap 64 more. Broken state: every
   unterminated head below the read cap waits, and on a socket is
   answered by the idle sweep's close."
  (format t "~%An unterminated head is refused at its budget~%")
  (labels ((drive (raw)
             ;; RAW arrives as fast as the buffer takes it, then the source
             ;; would block: the verdict CONNECTION-ON-READ reaches, or the
             ;; status it raised.
             (let* ((bytes (sb-ext:string-to-octets raw :external-format :ascii))
                    (sent 0)
                    (conn (web-skeleton::make-connection
                           :fd -1
                           :read-fn
                           (lambda (buffer start max-bytes)
                             (if (>= sent (length bytes))
                                 :again
                                 (let ((n (min max-bytes (- (length bytes) sent))))
                                   (replace buffer bytes :start1 start
                                                         :start2 sent
                                                         :end2 (+ sent n))
                                   (incf sent n)
                                   n))))))
               (handler-case (web-skeleton::connection-on-read conn)
                 (web-skeleton:http-parse-error (e)
                   (web-skeleton::http-parse-error-status e))
                 (error (e) (princ-to-string e)))))
           (unterminated (total)
             ;; A request line and one header line that has not ended, TOTAL
             ;; bytes in all.
             (let ((line (format nil "GET / HTTP/1.1~a" *crlf*)))
               (concatenate 'string line "X-P: "
                            (make-string (- total (length line) 5)
                                         :initial-element #\p)))))
    (let ((web-skeleton:*max-body-size* 64)
          (web-skeleton:*max-total-header-bytes* 64)
          (web-skeleton:*max-request-line-length* 64)
          (web-skeleton:*max-header-count* 4))
      (check "the head budget these give is 140" (web-skeleton::request-head-budget) 140)
      (check "one byte short of the budget, an unterminated head still waits"
             (drive (unterminated 139)) :continue)
      (check "at the budget it is refused 431, its terminator still to come"
             (drive (unterminated 140)) 431)
      (check "a request line that never ends is refused 414"
             (drive (concatenate 'string "GET /"
                                 (make-string 150 :initial-element #\a)))
             414)
      ;; The lines that did end are parsed, and their first fault wins:
      ;; here the version, which PARSE-REQUEST-BYTES judges before headers.
      (check "a fault in the lines that ended is the one answered"
             (drive (concatenate 'string "GET / HTTP/2.0" *crlf* "X-P: "
                                 (make-string 130 :initial-element #\p)))
             505)
      (check "control: a head that ends is left to dispatch, as before"
             (drive (format nil "GET / HTTP/1.1~aHost: x~a~a" *crlf* *crlf* *crlf*))
             :dispatch))))

;;; ---------------------------------------------------------------------------
;;; A declared body costs what arrives, not what was declared
;;; ---------------------------------------------------------------------------

(defun test-declared-body-is-not-preallocated ()
  (format t "~%Content-Length: the read buffer follows the bytes~%")
  (labels ((head (path headers)
             (with-output-to-string (s)
               (format s "POST ~a HTTP/1.1~a" path *crlf*)
               (dolist (h headers) (format s "~a~a" h *crlf*))
               (format s "~a" *crlf*)))
           (drive (raw)
             ;; RAW arrives as fast as the buffer takes it, then the source
             ;; would block. Returns (values VERDICT CONN). :AGAIN rather
             ;; than :EOF at the end, so a request still short of its body
             ;; is waiting rather than unfinishable.
             (let* ((bytes (sb-ext:string-to-octets raw :external-format :ascii))
                    (sent 0)
                    (conn (web-skeleton::make-connection
                           :fd -1
                           :read-fn
                           (lambda (buffer start max-bytes)
                             (if (>= sent (length bytes))
                                 :again
                                 (let ((n (min max-bytes (- (length bytes) sent))))
                                   (replace buffer bytes :start1 start
                                                         :start2 sent
                                                         :end2 (+ sent n))
                                   (incf sent n)
                                   n))))))
               (values (handler-case (web-skeleton::connection-on-read conn)
                         (web-skeleton:http-parse-error (e)
                           (web-skeleton::http-parse-error-status e))
                         (error (e) (princ-to-string e)))
                       conn))))

    ;; The headline. A head declaring a megabyte, and no byte of the
    ;; megabyte. Broken state: the buffer is grown to the declared length
    ;; on the spot, and every connection that sends only this much holds
    ;; *MAX-BODY-SIZE* of memory for as long as the idle timeout allows.
    (let ((web-skeleton:*max-body-size* (* 1024 1024)))
      (multiple-value-bind (v conn)
          (drive (head "/u" (list "Host: x"
                                  (format nil "Content-Length: ~d"
                                          web-skeleton:*max-body-size*))))
        (check "content-length: a declared body waits for its bytes" v :continue)
        (check "content-length: and nothing is allocated ahead of them"
               (< (length (web-skeleton::connection-read-buf conn))
                  web-skeleton:*max-body-size*)
               t)))

    ;; Small budgets so the arithmetic can be stated exactly. The read cap
    ;; is their sum plus every CRLF they leave out: 64 + 64 + 64 + 2*4 + 4.
    (let ((web-skeleton:*max-body-size* 64)
          (web-skeleton:*max-total-header-bytes* 64)
          (web-skeleton:*max-request-line-length* 64)
          (web-skeleton:*max-header-count* 4))
      (check "content-length: the cap these budgets give is 204"
             (web-skeleton::connection-read-cap
              (web-skeleton::make-connection :fd -1))
             204)

      ;; The premise the refusal below rests on: the largest request the
      ;; budgets admit fits the cap exactly. A 64-byte request line, four
      ;; headers totalling 64 bytes, a 64-byte body — 204 on the wire.
      ;; Broken state: the cap loses a byte to an edit, and a request the
      ;; parser would have accepted is refused for its size.
      (let* ((line-path (concatenate 'string "/"
                                     (make-string 49 :initial-element #\a)))
             (headers (list "Host: x"
                            "Content-Length: 64"
                            (concatenate 'string "X-A: "
                                         (make-string 14 :initial-element #\b))
                            (concatenate 'string "X-B: "
                                         (make-string 15 :initial-element #\c))))
             (raw (head line-path headers))
             (body (make-string 64 :initial-element #\d)))
        (check "content-length: the largest admissible request is 204 bytes"
               (+ (length raw) (length body))
               204)
        (check "content-length: and it is not refused for its size"
               (values (drive raw))
               :continue)
        (multiple-value-bind (v conn) (drive (concatenate 'string raw body))
          (check "content-length: and it dispatches once its body is here"
                 v :dispatch)
          (check "content-length: and the parser accepts it"
                 (attempt (length (web-skeleton:http-request-body
                                   (web-skeleton::connection-parse-request
                                    conn))))
                 64)))

      ;; Over the header budget, and with a body that takes the whole past
      ;; the cap. The head is refused the moment it is complete, with the
      ;; code that names the budget. Broken state: it waits for a body that
      ;; cannot fit — on a socket, until the idle sweep closes it without an
      ;; answer, because the edge for bytes left in the kernel already fired.
      (let ((raw (head "/u" (list "Host: x"
                                  "Content-Length: 64"
                                  (concatenate 'string "X-Pad: "
                                               (make-string 100
                                                            :initial-element #\p))))))
        (check "content-length: an over-budget head that cannot fit is 431"
               (values (drive raw))
               431))

      ;; The same over-budget head with a body small enough to fit still
      ;; waits for it, and is refused at dispatch as it always was. The
      ;; early refusal is for the request that cannot fit, not a second
      ;; header check.
      (let ((raw (head "/u" (list "Host: x"
                                  "Content-Length: 1"
                                  (concatenate 'string "X-Pad: "
                                               (make-string 100
                                                            :initial-element #\p))))))
        (check "content-length: an over-budget head that fits still waits"
               (values (drive raw))
               :continue)))))

;;; ---------------------------------------------------------------------------
;;; A request head arriving in pieces is searched once, not once per piece
;;; ---------------------------------------------------------------------------

(defun test-head-search-resumes ()
  "The search for the CRLFCRLF that ends a request head resumes where the
   last one stopped, so a head arriving in small pieces costs linear work.

   Measured by encapsulating SCAN-CRLF-CRLF and summing the bytes each call
   is asked to search. Searching from 0 on every read makes that the sum
   of every prefix of the head — for 8 KiB in 16-byte pieces, two million
   bytes for eight thousand delivered. Broken state: the sum is quadratic.

   The terminator itself can arrive split across reads, three bytes of it
   on one side at most, and each of the three splits is asserted. A resume
   point off by one passes the work bound and fails one of these."
  (format t "~%Request head: searched once, however it arrives~%")
  (labels ((ascii (s) (sb-ext:string-to-octets s :external-format :ascii))
           (drive-pieces (pieces)
             ;; One piece per wake-up: each CONNECTION-ON-READ reads one piece
             ;; and then finds the source empty, as a socket would between
             ;; segments. Returns the last verdict and the connection.
             (let* ((queue (copy-list pieces))
                    (ready nil)
                    (conn (web-skeleton::make-connection
                           :fd -1
                           :read-fn
                           (lambda (buffer start max-bytes)
                             (if (not ready)
                                 :again
                                 (let* ((piece (first queue))
                                        (n (min max-bytes (length piece))))
                                   (replace buffer piece :start1 start :end2 n)
                                   (if (= n (length piece))
                                       (progn (pop queue) (setf ready nil))
                                       (setf (first queue) (subseq piece n)))
                                   n)))))
                    (verdict nil))
               (loop while queue
                     do (setf ready t
                              verdict (handler-case
                                          (web-skeleton::connection-on-read conn)
                                        (web-skeleton:http-parse-error (e)
                                          (web-skeleton::http-parse-error-status e)))))
               (values verdict conn)))
           (split (bytes size)
             (loop for i from 0 below (length bytes) by size
                   collect (subseq bytes i (min (length bytes) (+ i size))))))
    (let* ((head (ascii (concatenate 'string
                                     "GET / HTTP/1.1" *crlf*
                                     "Host: x" *crlf*
                                     "X-Pad: " (make-string 8000 :initial-element #\a)
                                     *crlf* *crlf*)))
           (searched 0))
      (unwind-protect
           (progn
             (sb-int:encapsulate 'web-skeleton::scan-crlf-crlf 'counted
                                 (lambda (f buf start end)
                                   (incf searched (max 0 (- end start)))
                                   (funcall f buf start end)))
             (multiple-value-bind (v conn) (drive-pieces (split head 16))
               (check "head search: a head in 16-byte pieces dispatches" v :dispatch)
               (check "head search: at the right boundary"
                      (web-skeleton::connection-request-end conn) (length head))
               (check "head search: the work is linear in the head, not quadratic"
                      (< searched (* 4 (length head)))
                      t)
               ;; The control: every byte was searched at least once. Were
               ;; SCAN-CRLF-CRLF ever inlined, the wrapper above would count
               ;; nothing and the bound would hold for nothing.
               (check "head search: and every byte of it was searched"
                      (>= searched (length head))
                      t)))
        (sb-int:unencapsulate 'web-skeleton::scan-crlf-crlf 'counted))
      ;; Each place the terminator can be cut: CR | LF CR LF, CR LF | CR LF,
      ;; CR LF CR | LF.
      (dolist (k '(3 2 1))
        (multiple-value-bind (v conn)
            (drive-pieces (list (subseq head 0 (- (length head) k))
                                (subseq head (- (length head) k))))
          (check (format nil "head search: a terminator split ~d byte~:p from its end is found"
                         k)
                 (list v (web-skeleton::connection-request-end conn))
                 (list :dispatch (length head))))))))

;;; ---------------------------------------------------------------------------
;;; A request head with no header fields
;;; ---------------------------------------------------------------------------

(defun test-a-head-with-no-fields ()
  "A request line followed by CRLFCRLF is a whole request head: the line's
   own CRLF is the first half of the terminator. CONNECTION-ON-READ finds
   the request line's end there and dispatches; PARSE-REQUEST-BYTES then
   accepts it at HTTP/1.0, which requires no field, and refuses it at 1.1
   for its missing Host. OPTIONS / HTTP/1.0 alone is HAProxy's default
   health probe.

   Searching for the request line's end short of the terminator misses
   that CRLF, and the head is refused 400 as having no request line."
  (format t "~%A request head with no header fields~%")
  (flet ((on-read (line)
           (let ((b (sb-ext:string-to-octets
                     (format nil "~a~a~a" line *crlf* *crlf*)
                     :external-format :ascii))
                 (conn (web-skeleton::make-connection
                        :fd -1
                        :read-fn (lambda (buffer start max-bytes)
                                   (declare (ignore buffer start max-bytes))
                                   :again))))
             (setf (web-skeleton::connection-read-buf conn) b
                   (web-skeleton::connection-read-pos conn) (length b))
             (handler-case
                 (list (web-skeleton::connection-on-read conn)
                       (web-skeleton::connection-request-end conn))
               (web-skeleton:http-parse-error (e)
                 (list (web-skeleton::http-parse-error-status e)))
               (error (e) (list :raised (princ-to-string e)))))))
    (check "no fields: GET / HTTP/1.0 dispatches, ending at its terminator"
           (on-read "GET / HTTP/1.0") (list :dispatch 18))
    (check "no fields: OPTIONS / HTTP/1.0 dispatches"
           (on-read "OPTIONS / HTTP/1.0") (list :dispatch 22))
    (check "no fields: HTTP/1.1 dispatches too, for the parser to judge"
           (on-read "GET / HTTP/1.1") (list :dispatch 18))))

;;; ---------------------------------------------------------------------------
;;; A buffer that fills on a read is answered on that read
;;; ---------------------------------------------------------------------------

(defun test-full-buffer-is-answered-when-it-fills ()
  "A read that fills the buffer to its cap, with the request still
   unfinished, is answered 413 by that read.

   The :FULL arm answers a buffer that was already full when a read began.
   The read that fills it reports :OK instead, because it read something,
   and leaves the rest of the request in the kernel — where an
   edge-triggered descriptor reports nothing new, so a client that had
   sent everything waited out the idle timeout and got no answer.

   The source here never runs dry: it has more than the cap to give and
   would give it, which is the kernel holding the rest. Budgets are set so
   the cap is above the 4 KiB first buffer and the buffer grows to meet it,
   as a real one does."
  (format t "~%Read buffer: full on the read that fills it~%")
  (let ((web-skeleton:*max-body-size* 8192)
        (web-skeleton:*max-total-header-bytes* 1024)
        (web-skeleton:*max-request-line-length* 1024)
        (web-skeleton:*max-header-count* 10))
    (labels ((ascii (s) (sb-ext:string-to-octets s :external-format :ascii))
             (drive (bytes)
               (let* ((sent 0)
                      (conn (web-skeleton::make-connection
                             :fd -1
                             :read-fn
                             (lambda (buffer start max-bytes)
                               (if (>= sent (length bytes))
                                   :again
                                   (let ((n (min max-bytes (- (length bytes) sent))))
                                     (replace buffer bytes :start1 start
                                                           :start2 sent
                                                           :end2 (+ sent n))
                                     (incf sent n)
                                     n))))))
                 (values (handler-case (web-skeleton::connection-on-read conn)
                           (web-skeleton:http-parse-error (e)
                             (web-skeleton::http-parse-error-status e))
                           (error (e) (princ-to-string e)))
                         conn))))
      (let ((cap (web-skeleton::connection-read-cap
                  (web-skeleton::make-connection :fd -1))))
        (check "full buffer: the cap is past the first buffer, so it grows"
               (> cap 4096) t)
        ;; Small chunks: the wire carries six bytes for each one decoded, so
        ;; the buffer fills long before the body reaches *MAX-BODY-SIZE*.
        ;; Broken state: :CONTINUE, and a wait for an edge that has fired.
        (let ((body (with-output-to-string (s)
                      (dotimes (i 2000) (format s "1~aX~a" *crlf* *crlf*)))))
          (check "full buffer: a chunked body of small chunks is 413 at once"
                 (values (drive (ascii (concatenate
                                        'string
                                        "POST /u HTTP/1.1" *crlf*
                                        "Host: x" *crlf*
                                        "Transfer-Encoding: chunked" *crlf*
                                        *crlf* body))))
                 413))
        ;; A header block that never ends is answered at once as well, but
        ;; not by the full buffer: it reaches REQUEST-HEAD-BUDGET first and
        ;; is refused there with the 431 its headers earn, before the
        ;; buffer can fill. TEST-UNTERMINATED-HEAD-REFUSED-AT-ITS-BUDGET has
        ;; that boundary exactly.
        (check "full buffer: headers that never end are refused at once, 431"
               (values (drive (ascii (concatenate
                                      'string
                                      "GET / HTTP/1.1" *crlf*
                                      "X-Pad: "
                                      (make-string (* 2 cap)
                                                   :initial-element #\p)))))
               431)
        ;; The other half: a full buffer is not a refusal by itself. A
        ;; complete request with pipelined bytes behind it can fill the
        ;; buffer just as well, and is dispatched. Broken state: the
        ;; refusal is decided on the buffer alone, ahead of the state
        ;; machine, and a well-formed request is refused for what follows
        ;; it on the wire.
        (multiple-value-bind (v conn)
            (drive (ascii (concatenate
                           'string
                           "POST /u HTTP/1.1" *crlf*
                           "Host: x" *crlf*
                           "Transfer-Encoding: chunked" *crlf*
                           *crlf*
                           "3" *crlf* "abc" *crlf* "0" *crlf* *crlf*
                           "GET /next HTTP/1.1" *crlf*
                           "X-Pad: " (make-string (* 2 cap)
                                                  :initial-element #\q))))
          (check "full buffer: a complete request in a full buffer dispatches"
                 v :dispatch)
          (check "full buffer: and the buffer really was full"
                 (= (web-skeleton::connection-read-pos conn) cap)
                 t))))))

;;; ---------------------------------------------------------------------------
;;; A paused detached fetch resumes when its target drains
;;; ---------------------------------------------------------------------------

(defun %detached-pause-pass (label target-state)
  "Drive RESUME-PAUSED-OUTBOUND's edge against a target in TARGET-STATE.

   LABEL prefixes every assertion, so a failure names which target state
   broke rather than only that one did."
  (multiple-value-bind (out-server out-client) (%loopback-pair)
    (multiple-value-bind (tgt-server tgt-client) (%loopback-pair)
      (let ((epfd (web-skeleton::epoll-create)))
        (unwind-protect
             (let* ((out-fd (web-skeleton::socket-fd out-server))
                    (tgt-fd (web-skeleton::socket-fd tgt-server))
                    (web-skeleton::*connections* (make-hash-table :test #'eql))
                    (web-skeleton::*epoll-fd* epfd)
                    ;; The outbound, paused exactly as HANDLE-OUTBOUND-READ
                    ;; leaves one: subscribed to no events at all, so
                    ;; nothing but a resume can ever wake it.
                    ;; Started ten seconds ago and paused for the last
                    ;; five, so the resume has a known interval to add.
                    (out (web-skeleton::make-connection
                          :fd out-fd :socket out-server :state :out-read
                          :outbound-p t
                          :fetch-sink :detached
                          :fetch-paused t
                          :fetch-paused-at (- (get-universal-time) 5)
                          :fetch-started-at (- (get-universal-time) 10)
                          :fetch-deadline (+ (get-universal-time) 20)
                          :inbound-fd tgt-fd
                          :last-active (get-universal-time)))
                    ;; The target: a streaming connection with the
                    ;; back-link the pause left on it.
                    (tgt (web-skeleton::make-connection
                          :fd tgt-fd :socket tgt-server :state target-state
                          :stream-framing (when (eq target-state :streaming)
                                            :chunked)
                          :paused-outbound-fd out-fd
                          :last-active (get-universal-time))))
               (web-skeleton::set-nonblocking out-fd)
               (web-skeleton::set-nonblocking tgt-fd)
               (web-skeleton::register-connection out)
               (web-skeleton::register-connection tgt)
               (web-skeleton::epoll-add epfd out-fd web-skeleton::+epollet+)
               (web-skeleton::epoll-add epfd tgt-fd
                                        (logior web-skeleton::+epollout+
                                                web-skeleton::+epollet+))
               (check (format nil "~a: the outbound starts paused" label)
                      (web-skeleton::connection-fetch-paused out) t)
               ;; A backlog on the target, and then the drain that ends it.
               (web-skeleton::connection-append-write
                tgt (sb-ext:string-to-octets "queued" :external-format :ascii))
               (check (format nil "~a: the target has a backlog to drain" label)
                      (plusp (web-skeleton::connection-write-pending tgt)) t)
               (web-skeleton::handle-client-write tgt epfd)
               (check (format nil "~a: the target drained" label)
                      (web-skeleton::connection-write-pending tgt) 0)
               ;; The property.
               (check (format nil "~a: draining the target resumed the fetch" label)
                      (web-skeleton::connection-fetch-paused out) nil)
               (check (format nil "~a: and the back-link was cleared with it" label)
                      (web-skeleton::connection-paused-outbound-fd tgt) -1)
               ;; The deadline moved by the time spent paused, so a relay
               ;; is not killed for applying the backpressure it was told
               ;; to apply. Compared with the deadline the fixture set:
               ;; start plus thirty, then at least the five paused seconds.
               (check (format nil "~a: the deadline moved by the paused time" label)
                      (>= (web-skeleton::connection-fetch-deadline out)
                          (+ (web-skeleton::connection-fetch-started-at out)
                             30 5))
                      t))
          (ignore-errors (web-skeleton::%close epfd))
          (ignore-errors (sb-bsd-sockets:socket-close out-server))
          (ignore-errors (sb-bsd-sockets:socket-close out-client))
          (ignore-errors (sb-bsd-sockets:socket-close tgt-server))
          (ignore-errors (sb-bsd-sockets:socket-close tgt-client)))))))

(defun test-detached-pause-auto-resumes ()
  "A paused detached fetch is resumed by its target's backlog draining.

   Sibling to TEST-AUTOMATIC-RESUME-EDGE rather than a replacement for it.
   That one covers the mechanism, with a :STREAMING target; what it cannot
   cover is that anything reaches it. RESUME-PAUSED-OUTBOUND runs from
   HANDLE-CLIENT-WRITE's :DONE arm when a connection's backlog empties, and
   an :AWAITING inbound — the one connection a fetch can pause against
   without the detached sink — has nothing queued by construction and so
   never receives EPOLLOUT. Tested alone, the mechanism would have no
   reachable caller.

   So what this adds is the sink, not the edge: a paused outbound whose
   FETCH-SINK is :DETACHED, resumed by its target draining, with the
   deadline advanced by the interval spent paused.

   Driven here rather than end to end, and that is a deliberate retreat.
   The e2e version has to make a real client stop reading until the
   target's socket refuses a write, and loopback will not cooperate: its
   buffers absorb a large write with the target's pending bytes never
   leaving 0, and writing more makes the test slow without making it
   reliable. A test that cannot provoke the state it names is worse than
   one that drives the edge directly — it passes for the wrong reason.

   So the edge is driven: a target holding a backlog and a paused outbound
   behind it, flushed until :DONE, with the outbound's interest checked
   before and after.

   The precondition this pins down: auto-resume needs the target to have
   *actually backed up*. STREAM-SEND
   flushes inline, and a flush that completes arms nothing and so never
   reaches HANDLE-CLIENT-WRITE — so a pause taken while the target's queue
   was empty has no wake-up coming and still needs an explicit
   FETCH-RESUME.

   Run against both target states. RESUME-PAUSED-OUTBOUND sits in
   HANDLE-CLIENT-WRITE's :DONE arm ahead of the state dispatch, and its own
   comment says why: every state that can be relayed into reaches that
   point, and a resume working for only one of them would be the kind of
   gap nobody finds until a different response shape turns up.

   The :WEBSOCKET pass is a regression guard rather than a detector: the
   edge itself is state-agnostic and passes either way. What decides
   whether it is reachable is the arming — arming the remainder WS-SEND
   leaves behind is what lets HANDLE-CLIENT-WRITE run for a websocket
   target at all. This asserts the half of that chain the arming detector
   cannot see."
  (format t "~%Detached fetch: pause resumes when the target drains~%")
  (%detached-pause-pass "detached pause" :streaming)
  (%detached-pause-pass "detached pause (ws)" :websocket))

;;; ---------------------------------------------------------------------------
;;; The write path a closed stream is handed to, and the event that runs it
;;;
;;; STREAM-CLOSE moves the connection to :WRITE-RESPONSE, which
;;; HANDLE-CLIENT-WRITE and nothing else transitions out of. Handing off
;;; through STREAM-FLUSH, which arms only when the flush did not complete,
;;; would arm nothing for the ordinary close — a five-byte terminator onto
;;; a socket with room — and leave the connection parked.
;;;
;;; TEST-STREAM-LIFECYCLE builds this exact state and asserts both halves
;;; of its precondition — :WRITE-RESPONSE, and nothing left queued — and
;;; then never asks whether anything would move it. That is the shape of
;;; the gap, and the reason it survived a green suite.
;;; ---------------------------------------------------------------------------

(defun %stream-close-pass (back-up)
  "Close a stream and report the flush state, the interest armed, and where
   the connection was left.

   Returns (values PENDING MASKS CLOSED-STATE POST-STATE) — bytes still
   queued afterwards, every mask armed while STREAM-CLOSE ran in order, the
   state it left, and the state after one turn of the event loop.

   That last turn is driven through epoll rather than by calling
   HANDLE-CLIENT-WRITE, which is the difference between asserting the
   consequence and assuming it: the direct call runs whether or not
   anything armed the fd.

   BACK-UP shrinks SO_SNDBUF and pre-queues more than the socket will take,
   which is the branch STREAM-FLUSH's own arming covers. Without it the
   terminator goes out whole, the flush completes, and that is both the
   ordinary case and the one STREAM-FLUSH alone would leave unarmed."
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create))
          (masks nil)
          (real (symbol-function 'web-skeleton::epoll-modify)))
      (unwind-protect
           (let* ((fd (web-skeleton::socket-fd server))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (conn (web-skeleton::make-connection
                         :fd fd :socket server :state :streaming
                         :stream-framing :chunked
                         :last-active (get-universal-time))))
             (web-skeleton::set-nonblocking fd)
             (when back-up
               ;; A send buffer as small as the kernel will give.
               (web-skeleton::set-socket-option-int
                fd web-skeleton::+sol-socket+ +so-sndbuf+ 2048)
               (web-skeleton::connection-append-write
                conn (make-array (* 512 1024)
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 80)))
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
             ;; Recording starts here, so what is counted is STREAM-CLOSE's
             ;; own arming and not the setup's.
             (setf (symbol-function 'web-skeleton::epoll-modify)
                   (lambda (efd f mask)
                     (push mask masks)
                     (funcall real efd f mask)))
             (web-skeleton:stream-close conn)
             (setf (symbol-function 'web-skeleton::epoll-modify) real)
             (let ((pending (web-skeleton::connection-write-pending conn))
                   (closed-state (web-skeleton::connection-state conn))
                   (evbuf (make-array (* 4 web-skeleton::+epoll-event-size+)
                                      :element-type '(unsigned-byte 8))))
               ;; One turn of the event loop, driven the way the loop drives
               ;; it: ask epoll what is ready, and dispatch only that.
               ;; Calling HANDLE-CLIENT-WRITE directly would prove nothing —
               ;; it would run whether or not anything armed it, which is
               ;; the whole defect. Fifty milliseconds, not a deadline: a
               ;; writable socket with an armed interest is reported on the
               ;; first call, and an unarmed one is never reported at all,
               ;; so neither answer is waited for.
               (let ((answered :not-run))
                 (when (plusp (web-skeleton::epoll-wait epfd evbuf 4 50))
                   (setf answered
                         (web-skeleton::handle-client-write conn epfd)))
                 (values pending
                         (reverse masks)
                         closed-state
                         (web-skeleton::connection-state conn)
                         answered))))
        (setf (symbol-function 'web-skeleton::epoll-modify) real)
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client))))))

(defun test-stream-close-arms-the-write-path ()
  "A closed stream gets an event to finish on, whether or not it needed one
   to flush.

   The defect case is the ordinary one. With the terminator away and the
   queue empty there is nothing left to write, which is precisely why
   STREAM-FLUSH declined to arm — and precisely when the connection still
   has a transition owed to it: :WRITE-RESPONSE is where a keep-alive
   connection resets to :READ-HTTP and where a close-delimited one closes,
   and only HANDLE-CLIENT-WRITE performs it. Bounded by the idle sweep, so
   the symptom is liveness rather than loss: a FIN that waits
   *IDLE-TIMEOUT*, and a keep-alive socket that never answers again.

   The backed-up case is the control, and it is what makes the first
   assertion about STREAM-CLOSE rather than about streams in general. It
   is armed either way — by STREAM-FLUSH's own arming as much as by
   STREAM-CLOSE's — so it passes on both sides. Only the case where the
   flush succeeded moves.

   The defect case also asserts what the arming buys: one turn of the loop
   and the connection is back at :READ-HTTP, reusable. That turn is driven
   through EPOLL-WAIT and dispatched only if the fd is reported, which is
   the difference between asserting the consequence and assuming it —
   calling HANDLE-CLIENT-WRITE directly would run whether or not anything
   armed the fd. Under the defect the loop is asked, told nothing is ready,
   and the connection stays :WRITE-RESPONSE. Fifty milliseconds bounds it
   and neither answer waits for the bound: an armed writable socket is
   reported on the first call, an unarmed one never.

   The control does not assert that half. Its socket is full, so no
   EPOLLOUT arrives whether or not the interest is set, and the loop turn
   is uninformative there rather than wrong.

   The first case asserts the whole mask list rather than membership,
   which pins three things at once: that arming happened, that it happened
   exactly once, and that it is EPOLLOUT alone. The last is the decision
   worth pinning — STREAM-FLUSH's mask keeps EPOLLIN for a reason its own
   docstring gives, that a stream has to notice its peer going away, and
   this connection is no longer a stream. The control asserts membership
   only, because its mask is the thing that changed."
  (format t "~%Stream close: the write path gets an event to run on~%")
  (multiple-value-bind (pending masks state post answered)
      (%stream-close-pass nil)
    (check "stream close: the terminator flushed completely" pending 0)
    (check "stream close: the ordinary write path is in charge"
           state :write-response)
    (check "stream close: EPOLLOUT was armed even so"
           masks
           (list (logior web-skeleton::+epollout+ web-skeleton::+epollet+)))
    (check "stream close: and one turn of the loop reuses the connection"
           post :read-http)
    ;; The loop is told to answer, not merely reset. HANDLE-CLIENT-WRITE's
    ;; :KEEP-ALIVE is what sends the event loop back into the read path
    ;; without waiting for another EPOLLIN, and it is the step between a
    ;; connection that is reusable and a client that is actually answered.
    ;; Discarding it left that half of the claim untested.
    (check "stream close: and the loop is told to read the next request"
           answered :keep-alive))
  (multiple-value-bind (pending masks state post answered)
      (%stream-close-pass t)
    (declare (ignore state post answered))
    (check "stream close: the backed-up control did not flush"
           (plusp pending) t)
    (check "stream close: and the control is armed on both sides"
           (and (find-if (lambda (m)
                           (plusp (logand m web-skeleton::+epollout+)))
                         masks)
                t)
           t)))

(defun test-stream-close-refuses-a-foreign-connection ()
  "STREAM-CLOSE refuses a connection this worker does not own.

   The witness is the ON-CLOSE counter, not the write queue, and that is
   the whole design of this test. Copying WS-SEND's fixture would be the
   natural instinct and would pin nothing: a five-byte terminator flushes
   completely on any socket with room, so WRITE-PENDING is 0 wherever the
   guard sits.

   What cannot be undone is the notification. NOTIFY-STREAM-CLOSED nulls
   the slot before calling so the callback fires exactly once — so a
   cross-worker close tells the application :DONE, and the owning
   worker's own teardown notification afterwards is a no-op. The app's
   first and last word about that stream is a false one. A counter that
   stays at 0 is the assertion that no such word was said.

   CONNECTION-STATE is the second witness and it pins the placement
   directly. Moved to just before the arm — the natural wrong spot, since
   that is where the arming's incidental check sits — the raise still
   carries the right message and PENDING is still 0, but the counter is 1
   and the state has moved to :WRITE-RESPONSE. Both go red there and
   neither depends on socket-buffer behaviour.

   Real sockets on both sides, not epoll descriptors. The control has to
   reach CONNECTION-ON-WRITE and put the terminator on a wire, and an
   epoll fd raises from send(2) — which would fail the control for a
   fixture reason and, worse, would give the *foreign* connection a
   second way to raise once the guard is reverted, confounding the
   detector with an ENOTSOCK it was never about.

   Exactly one of the last two assertions is a control. \"a connection
   this worker owns still closes\" is: registering is the only change,
   and it passes with the guard or without it. \"and that one did tell
   the app\" is not, and should not be read as one: without the guard the
   *foreign* close is accepted and fires the callback too, so the counter
   reaches 2, and this fails as a cascade from the one defect rather than
   on its own. The sibling FETCH-INTO ownership test reads the same way."
  (format t "~%stream-close: a connection this worker does not own~%")
  (multiple-value-bind (fserver fclient) (%loopback-pair)
    (multiple-value-bind (oserver oclient) (%loopback-pair)
      (let ((epfd (web-skeleton::epoll-create)))
        (unwind-protect
             (let* ((fires 0)
                    (ffd (web-skeleton::socket-fd fserver))
                    (ofd (web-skeleton::socket-fd oserver))
                    (web-skeleton::*connections* (make-hash-table :test #'eql))
                    (web-skeleton::*epoll-fd* epfd)
                    (on-close (lambda (c reason)
                                (declare (ignore c reason))
                                (incf fires)))
                    (foreign (web-skeleton::make-connection
                              :fd ffd :socket fserver :state :streaming
                              :stream-framing :chunked
                              :stream-on-close on-close
                              :last-active (get-universal-time)))
                    (owned (web-skeleton::make-connection
                            :fd ofd :socket oserver :state :streaming
                            :stream-framing :chunked
                            :stream-on-close on-close
                            :last-active (get-universal-time))))
               (web-skeleton::set-nonblocking ffd)
               (web-skeleton::set-nonblocking ofd)
               ;; On neither the table nor the epoll set.
               (check "stream close: a connection this worker does not own is refused"
                      (handler-case (progn (web-skeleton:stream-close foreign) nil)
                        (error (e)
                          (not (null (search "not on this worker's connection table"
                                             (princ-to-string e))))))
                      t)
               (check "stream close: the refusal told the app nothing"
                      fires 0)
               (check "stream close: nor moved the connection out of :streaming"
                      (web-skeleton::connection-state foreign) :streaming)
               ;; The control. Registered and armed, so the terminator has a
               ;; write path to be handed to.
               (web-skeleton::register-connection owned)
               (web-skeleton::epoll-add epfd ofd
                                        (logior web-skeleton::+epollin+
                                                web-skeleton::+epollet+))
               (check "stream close: a connection this worker owns still closes"
                      (handler-case (progn (web-skeleton:stream-close owned) :closed)
                        (error (e) (format nil "signalled: ~a" e)))
                      :closed)
               (check "stream close: and that one did tell the app"
                      fires 1))
          (ignore-errors (web-skeleton::%close epfd))
          (ignore-errors (sb-bsd-sockets:socket-close fserver))
          (ignore-errors (sb-bsd-sockets:socket-close fclient))
          (ignore-errors (sb-bsd-sockets:socket-close oserver))
          (ignore-errors (sb-bsd-sockets:socket-close oclient)))))))

;;; ---------------------------------------------------------------------------
;;; The remainder a fetch callback leaves behind
;;;
;;; WS-SEND's second caller is a fetch callback on a :WEBSOCKET target, and
;;; it runs on the outbound connection's read path. HANDLE-CLIENT-READ,
;;; which arms a WebSocket after its handler returns, does not run for the
;;; target there at all, so without WS-SEND's own arming a remainder would
;;; sit with nothing subscribed to writability, waiting for an event that
;;; is not coming.
;;;
;;; Driven at the seam: DELIVER-DETACHED with a callback that sends, which
;;; is what FETCH-INTO's :THEN reduces to. End to end it would reach the
;;; same two lines through an upgrade handshake and a real upstream
;;; response, either of which could be what broke instead.
;;; ---------------------------------------------------------------------------

(defun test-ws-send-arms-from-a-fetch-callback ()
  "A frame sent from a fetch callback leaves its remainder armed.

   The target is a real socket with a shrunk send buffer and a peer that
   never reads — TEST-WS-HANDLER-PUSH-AND-RETURN's arrangement, and the
   only one that produces a remainder at all. With room to spare WS-SEND
   flushes completely and there is nothing left to arm for, so the two
   paths are indistinguishable.

   EPOLL-MODIFY is recorded rather than epoll being polled. A socket whose
   send buffer is full is not writable, so EPOLL-WAIT reports nothing
   whether or not EPOLLOUT was armed: the observation that looks the most
   direct is the one that cannot tell the two cases apart.

   Three checks, and the first two are the precondition. Without a
   remainder there is no claim to make, and a frame that fits would leave
   the third vacuous rather than failing — so the flush is asserted to have
   been incomplete before anything is asserted about arming."
  (format t "~%ws-send: arming from a fetch callback~%")
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create))
          (calls nil)
          (real (symbol-function 'web-skeleton::epoll-modify)))
      (unwind-protect
           (let* ((tgt-fd (web-skeleton::socket-fd server))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (target (web-skeleton::make-connection
                           :fd tgt-fd :socket server :state :websocket
                           :fetch-outstanding t
                           :last-active (get-universal-time)))
                  (frame (web-skeleton::build-ws-frame
                          web-skeleton::+ws-op-binary+
                          (make-array (* 512 1024)
                                      :element-type '(unsigned-byte 8)
                                      :initial-element 80)))
                  (flushed :unset))
             (web-skeleton::set-nonblocking tgt-fd)
             ;; A send buffer as small as the kernel will give.
             (web-skeleton::set-socket-option-int
              tgt-fd web-skeleton::+sol-socket+ +so-sndbuf+ 2048)
             (web-skeleton::register-connection target)
             (web-skeleton::epoll-add epfd tgt-fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
             (setf (symbol-function 'web-skeleton::epoll-modify)
                   (lambda (efd fd mask)
                     (push (cons fd mask) calls)
                     (funcall real efd fd mask)))
             ;; What FETCH-INTO's :THEN reduces to, on the path where the
             ;; outbound is what epoll woke and the target is not.
             (web-skeleton::deliver-detached
              tgt-fd epfd
              (lambda (s h b)
                (declare (ignore s h b))
                (setf flushed (web-skeleton::ws-send target frame))
                nil)
              :delivered)
             (setf (symbol-function 'web-skeleton::epoll-modify) real)
             (check "callback ws-send: the frame did not all fit" flushed nil)
             (check "callback ws-send: a remainder is queued"
                    (plusp (web-skeleton::connection-write-pending target)) t)
             (check "callback ws-send: EPOLLOUT is armed on the target"
                    (and (find-if
                          (lambda (c)
                            (and (= (car c) tgt-fd)
                                 (plusp (logand (cdr c)
                                                web-skeleton::+epollout+))))
                          calls)
                         t)
                    t))
        (setf (symbol-function 'web-skeleton::epoll-modify) real)
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client))))))

;;; ---------------------------------------------------------------------------
;;; The third caller: a handler that sends to somebody else
;;;
;;; HANDLE-CLIENT-READ arms (CONNECTION-FD CONN) — the connection it was
;;; woken for. A ws-handler that sends to any *other* connection is
;;; therefore in exactly the position the fetch callback was in, and that
;;; is not a hypothetical shape: it is the one DEPLOYMENT.md documents
;;; under fan-out, "calling ws-send in a loop" over a subscriber list.
;;;
;;; WS-SEND's own arming covers it by construction, and covered by
;;; construction with nothing checking it is, by this suite's own
;;; criterion, a claim without a detector.
;;; ---------------------------------------------------------------------------

(defun %ws-half-close-pass (client-bytes &key reply)
  "Send CLIENT-BYTES on a :WEBSOCKET connection, half-close the client, and
   wake the connection as the event loop would — HANDLE-CLIENT-WRITE while
   something is queued, HANDLE-CLIENT-READ otherwise — for at most fifty
   wake-ups or until it is closed. REPLY, when given, is what the
   ws-handler answers each complete frame with.

   Returns (values WAKE-UPS CLOSED RECEIVED): how many wake-ups ran, whether
   the connection was closed, and what the client read before end of
   stream, which is read only once the server has closed and so cannot
   block."
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let* ((fd (web-skeleton::socket-fd server))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (conn (web-skeleton::make-connection
                         :fd fd :socket server :state :websocket
                         :last-active (get-universal-time)))
                  (stream (sb-bsd-sockets:socket-make-stream
                           client :input t :output t
                           :element-type '(unsigned-byte 8)))
                  (wake-ups 0))
             (web-skeleton::set-nonblocking fd)
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd fd
                                      (logior web-skeleton::+epollin+
                                              web-skeleton::+epollet+))
             (write-sequence client-bytes stream)
             (force-output stream)
             (sb-bsd-sockets:socket-shutdown client :direction :output)
             (%await-arrival fd :bytes (length client-bytes) :fin t)
             (loop while (and (< wake-ups 50)
                              (>= (web-skeleton::connection-fd conn) 0))
                   do (incf wake-ups)
                      (if (plusp (web-skeleton::connection-write-pending conn))
                          (web-skeleton::handle-client-write conn epfd)
                          (web-skeleton::handle-client-read
                           conn epfd nil
                           (lambda (c f) (declare (ignore c f)) reply))))
             (let ((closed (< (web-skeleton::connection-fd conn) 0)))
               (values wake-ups
                       closed
                       (when closed
                         (let ((got (make-array 0 :element-type '(unsigned-byte 8)
                                                  :adjustable t :fill-pointer 0)))
                           (loop for b = (read-byte stream nil nil)
                                 while b do (vector-push-extend b got))
                           (coerce got '(simple-array (unsigned-byte 8) (*))))))))
        (ignore-errors (sb-bsd-sockets:socket-close client))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (web-skeleton::%close epfd))))))

(defun test-websocket-peer-that-leaves-mid-frame ()
  "A WebSocket whose peer half-closes with a frame unfinished is closed,
   not woken forever.

   Nothing can follow the peer's FIN, so a partial frame left in the buffer
   can never complete. The socket stays readable at end of stream, and a
   read path that only waits for more re-arms EPOLLIN and is woken again at
   once: a busy loop on the worker, which held until the ping sweep gave up
   on a peer that cannot answer — a minute and a half with the defaults.

   Two partial shapes, and a control: a complete frame followed by the FIN,
   whose reply still has to reach the half-closed peer before the close."
  (format t "~%WebSocket: a peer that leaves mid-frame is closed~%")
  (let ((whole (make-test-ws-frame "hello")))
    (multiple-value-bind (wake-ups closed)
        (%ws-half-close-pass (make-array 1 :element-type '(unsigned-byte 8)
                                           :initial-element #x81))
      (check "ws half-close: one header byte, then FIN, is closed"
             closed t)
      (check "ws half-close: one header byte, in at most two wake-ups"
             (<= wake-ups 2) t))
    (multiple-value-bind (wake-ups closed)
        (%ws-half-close-pass (subseq whole 0 (1- (length whole))))
      (check "ws half-close: a frame one byte short, then FIN, is closed"
             closed t)
      (check "ws half-close: a frame one byte short, in at most two wake-ups"
             (<= wake-ups 2) t))
    ;; Control: the reply to a frame that did arrive is delivered first.
    (let ((reply (web-skeleton::build-ws-frame
                  web-skeleton::+ws-op-text+
                  (sb-ext:string-to-octets "bye" :external-format :utf-8))))
      (multiple-value-bind (wake-ups closed received)
          (%ws-half-close-pass whole :reply reply)
        (declare (ignore wake-ups))
        (check "ws half-close: control: a whole frame, then FIN, is closed"
               closed t)
        (check "ws half-close: control: and its reply reached the peer first"
               (and received (equalp received reply))
               t)))))

(defun %ws-fanout-pass ()
  "Run the documented fan-out shape and report what got armed.

   A real masked frame arrives on A; A's handler sends a 512 KiB frame to
   B, whose send buffer is shrunk so a remainder is guaranteed. Returns
   (values SENT PENDING-B TOUCHED-A ARMED-B) — WS-SEND's answer, the bytes
   left on B, whether epoll was touched for A at all, and whether EPOLLOUT
   was armed for B.

   Asymmetric on purpose. A's handler returns NIL, so A has nothing pending
   and HANDLE-CLIENT-READ arms it EPOLLIN — the question for A is only
   whether the read path ran and reached its arming at all, which is what
   separates a B that was missed from a run where nothing armed anything at
   all.

   Driven through the real HANDLE-CLIENT-READ rather than by calling the
   handler, because the arming under test is the one HANDLE-CLIENT-READ
   performs after a handler returns. Calling the handler directly would
   remove the very code whose scope is the question."
  (multiple-value-bind (a-server a-client) (%loopback-pair)
    (multiple-value-bind (b-server b-client) (%loopback-pair)
      (let ((epfd (web-skeleton::epoll-create))
            (calls nil)
            (real (symbol-function 'web-skeleton::epoll-modify)))
        (unwind-protect
             (let* ((a-fd (web-skeleton::socket-fd a-server))
                    (b-fd (web-skeleton::socket-fd b-server))
                    (web-skeleton::*connections* (make-hash-table :test #'eql))
                    (web-skeleton::*epoll-fd* epfd)
                    (conn-a (web-skeleton::make-connection
                             :fd a-fd :socket a-server :state :websocket
                             :last-active (get-universal-time)))
                    (conn-b (web-skeleton::make-connection
                             :fd b-fd :socket b-server :state :websocket
                             :last-active (get-universal-time)))
                    (big (web-skeleton::build-ws-frame
                          web-skeleton::+ws-op-binary+
                          (make-array (* 512 1024)
                                      :element-type '(unsigned-byte 8)
                                      :initial-element 80)))
                    (sent :unset))
               (web-skeleton::set-nonblocking a-fd)
               (web-skeleton::set-nonblocking b-fd)
               ;; Only B is shrunk. A has to stay able to take its own
               ;; handler's return value, or the two fds would both hold
               ;; remainders and the assertion could not tell them apart.
               (web-skeleton::set-socket-option-int
                b-fd web-skeleton::+sol-socket+ +so-sndbuf+ 2048)
               (web-skeleton::register-connection conn-a)
               (web-skeleton::register-connection conn-b)
               (web-skeleton::epoll-add epfd a-fd
                                        (logior web-skeleton::+epollin+
                                                web-skeleton::+epollet+))
               (web-skeleton::epoll-add epfd b-fd
                                        (logior web-skeleton::+epollin+
                                                web-skeleton::+epollet+))
               (let ((stream (sb-bsd-sockets:socket-make-stream
                              a-client :input t :output t
                              :element-type '(unsigned-byte 8)))
                     (frame (make-test-ws-frame "ping-a")))
                 (write-sequence frame stream)
                 (force-output stream)
                 (%await-arrival a-fd :bytes (length frame)))
               ;; Recording starts after the setup so what is counted is the
               ;; read path's own arming and not EPOLL-ADD's.
               (setf (symbol-function 'web-skeleton::epoll-modify)
                     (lambda (efd fd mask)
                       (push (cons fd mask) calls)
                       (funcall real efd fd mask)))
               (web-skeleton::handle-client-read
                conn-a epfd nil
                (lambda (c f)
                  (declare (ignore c f))
                  (setf sent (web-skeleton:ws-send conn-b big))
                  nil))
               (setf (symbol-function 'web-skeleton::epoll-modify) real)
               (values sent
                       (web-skeleton::connection-write-pending conn-b)
                       (and (find a-fd calls :key #'car) t)
                       (and (find-if (lambda (c)
                                       (and (= (car c) b-fd)
                                            (plusp (logand
                                                    (cdr c)
                                                    web-skeleton::+epollout+))))
                                     calls)
                            t)))
          (setf (symbol-function 'web-skeleton::epoll-modify) real)
          (ignore-errors (web-skeleton::%close epfd))
          (ignore-errors (sb-bsd-sockets:socket-close a-server))
          (ignore-errors (sb-bsd-sockets:socket-close a-client))
          (ignore-errors (sb-bsd-sockets:socket-close b-server))
          (ignore-errors (sb-bsd-sockets:socket-close b-client)))))))


(defun test-ws-send-arms-a-fan-out-target ()
  "A handler that sends to somebody else arms that somebody else.

   HANDLE-CLIENT-READ arms the connection it was woken for, and only that
   one. So a ws-handler pushing to a subscriber list is in exactly the
   position the fetch callback was in — nothing downstream arms the target
   — and DEPLOYMENT.md documents that shape under fan-out rather than
   treating it as exotic. The same one-line fix covers both; only one of
   them was claimed.

   Four assertions, and the first two are the precondition. A frame that
   fits leaves nothing to strand, so WS-SEND returning NIL and B holding a
   backlog is what makes the fourth check about the claim rather than about
   the fixture.

   The third is the control, and what it guards against is the fourth going
   vacuous later rather than anything failing now. Under the defect A is
   still armed — HANDLE-CLIENT-READ names A's own fd — so a
   failure list reading A touched, B not states the defect precisely.

   The case it really exists for is fixture drift. Simplify this test by
   calling the handler directly instead of driving HANDLE-CLIENT-READ — the
   obvious tidy-up, and someone will try it — and the fourth check still
   passes, because WS-SEND arms B itself. The seam stops being crossed
   and nothing says so. That is the *pre-arranged* mode, reachable only
   because WS-SEND arms, and this check is the thing that catches it: that
   drift fails check three alone and leaves the other three green.

   EPOLLIN is what A gets, not EPOLLOUT: its handler returned NIL, so A has
   nothing pending. Hence the asymmetry between the third check and the
   fourth — for A the question is whether epoll was touched at all, for B
   it is whether the right interest was set."
  (format t "~%ws-send: arming a fan-out target~%")
  (multiple-value-bind (sent pending-b touched-a armed-b) (%ws-fanout-pass)
    (check "fan-out ws-send: the frame did not all fit" sent nil)
    (check "fan-out ws-send: a remainder is queued on the target"
           (plusp pending-b) t)
    (check "fan-out ws-send: the read path armed its own connection"
           touched-a t)
    (check "fan-out ws-send: and EPOLLOUT is armed on the fan-out target"
           armed-b t)))

;;; ---------------------------------------------------------------------------
;;; The close code for a frame that is too large
;;; ---------------------------------------------------------------------------

(defun test-ws-oversized-frame-close-code ()
  "An oversized data frame closes 1009; an oversized control frame closes 1002.

   RFC 6455 7.4.1 has 1009 (Message Too Big) for the data case, and the
   oversized fragmented *message* — the same complaint one layer up —
   closes 1009 in two places. A single frame raised out of
   TRY-PARSE-WS-FRAME as a generic error would take WEBSOCKET-ON-READ's
   catch-all and close 1002: one file, two answers to one question.

   The control frame is the other half, and it wants the other code.
   *MAX-WS-PAYLOAD-SIZE* is a limit this endpoint chose, so exceeding it
   is 1009 — 'too big for me'. §5.5 caps every control frame at 125 bytes
   for everyone, so exceeding *that* is malformed rather than inconvenient
   and earns 1002. With the size check ahead of the §5.5 one, a ping
   declaring more than 64 KiB would take the size check first and be told
   its message was too big for us, when what is wrong with it is that no
   endpoint may send it.

   Driven through WEBSOCKET-ON-READ rather than by asserting the condition
   type, because the code on the wire is the claim and the condition is
   only how it gets there. The neighbouring 1002 test is a control that
   still passes: an unknown opcode is a protocol error and keeps its code.

   *MAX-WS-PAYLOAD-SIZE* is lowered rather than a real 1 MiB frame built.
   What the parser compares is the declared length against the bound, and a
   small bound reaches that comparison with eight bytes instead of a
   megabyte. The ping needs 200 — over §5.5's 125 *and* over the lowered
   bound, since a frame that trips only one of them cannot tell which
   check ran first."
  (format t "~%ws: the close code for a frame that is too large~%")
  (flet ((close-code-for (frame)
           (let ((conn (web-skeleton::make-connection
                        :fd -1 :state :websocket :last-active 0)))
             (setf (web-skeleton::connection-read-buf conn) frame
                   (web-skeleton::connection-read-pos conn) (length frame))
             (multiple-value-bind (action response)
                 (web-skeleton::websocket-on-read
                  conn (lambda (c f) (declare (ignore c f)) nil))
               (values action
                       (and response
                            (>= (length response) 4)
                            (logior (ash (aref response 2) 8)
                                    (aref response 3))))))))
    (let ((web-skeleton::*max-ws-payload-size* 4))
      (multiple-value-bind (action code)
          (close-code-for (make-masked-frame t 1 #(104 101 108 108 111 32 119 111)))
        (check "oversized frame: the connection closes" action :close)
        (check "oversized frame: with 1009, not 1002" code 1009))
      ;; Opcode 9 is ping. 200 bytes is over both bounds, so the code that
      ;; comes back names which check the parser reached first.
      (multiple-value-bind (action code)
          (close-code-for
           (make-masked-frame t 9 (make-array 200 :element-type '(unsigned-byte 8)
                                                  :initial-element 65)))
        (check "oversized control frame: the connection closes" action :close)
        (check "oversized control frame: with 1002, not 1009" code 1002)))))

(defun test-ws-fragments-are-held-as-their-bytes ()
  "A fragmented message holds what its fragments carry, and no more than
   *MAX-WS-MESSAGE-SIZE* of it, however many fragments there are.

   An empty continuation adds nothing to the message and still costs its
   peer six bytes, so a message kept fragment by fragment would grow with
   every one while its byte count, and the cap on it, stood still. Twenty
   thousand of them are fed here behind a one-byte first fragment, and the
   message is still one byte held.

   Then the bytes themselves: three hundred one-byte fragments arrive as
   the message they spell, and a message exactly at the cap is delivered
   while one byte over it closes 1009."
  (format t "~%ws: fragments are held as their bytes~%")
  (flet ((feed (frames &optional handler)
           (let ((conn (web-skeleton::make-connection
                        :fd -1 :state :websocket :last-active 0))
                 (buf (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                             frames)))
             (setf (web-skeleton::connection-read-buf conn) buf
                   (web-skeleton::connection-read-pos conn) (length buf))
             (multiple-value-bind (action response)
                 (web-skeleton::websocket-on-read
                  conn (or handler (lambda (c f) (declare (ignore c f)) nil)))
               (values conn action
                       (and response
                            (>= (length response) 4)
                            (logior (ash (aref response 2) 8)
                                    (aref response 3)))))))
         (octets (&rest bytes)
           (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
    (let ((conn (feed (cons (make-masked-frame nil 2 (octets 7))
                            (loop repeat 20000
                                  collect (make-masked-frame nil 0 (octets)))))))
      (check "empty fragments: the message is still arriving"
             (web-skeleton::connection-ws-frag-total conn) 1)
      (check "empty fragments: and one byte is all it holds"
             (length (web-skeleton::connection-ws-frag-buf conn)) 1))
    (let ((got nil))
      (feed (append (list (make-masked-frame nil 2 (octets 0)))
                    (loop for i from 1 below 299
                          collect (make-masked-frame nil 0 (octets (mod i 256))))
                    (list (make-masked-frame t 0 (octets (mod 299 256)))))
            (lambda (c f) (declare (ignore c)) (setf got (ws-frame-payload f)) nil))
      (check "one-byte fragments: the message they spell"
             (equalp got (coerce (loop for i below 300 collect (mod i 256))
                                 '(simple-array (unsigned-byte 8) (*))))
             t))
    (let ((web-skeleton:*max-ws-message-size* 8)
          (got nil))
      (multiple-value-bind (conn action)
          (feed (list (make-masked-frame nil 2 (octets 1 2 3))
                      (make-masked-frame nil 0 (octets 4 5 6 7))
                      (make-masked-frame t 0 (octets 8)))
                (lambda (c f) (declare (ignore c)) (setf got (ws-frame-payload f)) nil))
        (declare (ignore conn))
        (check "at the cap: delivered" action nil)
        (check "at the cap: whole" (equalp got (octets 1 2 3 4 5 6 7 8)) t))
      (multiple-value-bind (conn action code)
          (feed (list (make-masked-frame nil 2 (octets 1 2 3 4))
                      (make-masked-frame nil 0 (octets 5 6 7 8))
                      (make-masked-frame t 0 (octets 9))))
        (check "over the cap: closed" action :close)
        (check "over the cap: with 1009" code 1009)
        (check "over the cap: and nothing held"
               (web-skeleton::connection-ws-frag-buf conn) nil)))))

(defun test-small-writes-are-queued-joined ()
  "Pongs to a stream of empty pings are queued in vectors of a few hundred
   bytes, not one apiece.

   *MAX-WRITE-BACKLOG* counts bytes, and an empty ping's pong is two of
   them, while a queued segment of its own costs a cell and a vector
   header besides: a peer that pings and never reads would have a backlog
   that holds many times what it counts. Twenty thousand empty pings are
   fed here. The first pong is the head and the rest join behind it into
   +QUEUE-MERGE-LIMIT+-byte vectors, and the bytes are every pong in
   order.

   What is joined is copied, never written into: a vector handed to the
   queue is the same after another has joined it, as a shared one, the
   pre-built ping frame or a static response, must be. A WRITE-SPAN is
   never joined."
  (format t "~%Write queue: small writes are joined~%")
  (let* ((n 20000)
         (conn (web-skeleton::make-connection
                :fd -1 :state :websocket :last-active 0))
         (buf (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                     (loop repeat n
                           collect (make-masked-frame t 9 #())))))
    (setf (web-skeleton::connection-read-buf conn) buf
          (web-skeleton::connection-read-pos conn) (length buf))
    (check "empty pings: read without a close"
           (web-skeleton::websocket-on-read
            conn (lambda (c f) (declare (ignore c f)) nil))
           nil)
    (check "empty pings: every pong counted"
           (web-skeleton::connection-write-pending conn) (* 2 n))
    (check "empty pings: the pongs behind the head in vectors of the limit"
           (length (web-skeleton::connection-write-queue conn))
           (ceiling (* 2 (1- n)) web-skeleton::+queue-merge-limit+))
    (check "empty pings: and the bytes are every pong, in order"
           (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                                    :adjustable t :fill-pointer 0)))
             (flet ((take (bytes start end)
                      (loop for i from start below end
                            do (vector-push-extend (aref bytes i) out))))
               (take (web-skeleton::connection-write-buf conn)
                     (web-skeleton::connection-write-pos conn)
                     (web-skeleton::connection-write-end conn))
               (dolist (segment (web-skeleton::connection-write-queue conn))
                 (multiple-value-call #'take
                   (web-skeleton::segment-bounds segment))))
             (and (= (length out) (* 2 n))
                  (loop for i from 0 below (length out) by 2
                        always (and (= (aref out i) #x8A)
                                    (zerop (aref out (1+ i)))))))
           t))
  (flet ((octets (&rest bytes)
           (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
    (let ((conn (web-skeleton::make-connection :fd -1 :last-active 0))
          (shared (octets 2 3))
          (file (make-array 8 :element-type '(unsigned-byte 8) :initial-element 9)))
      (web-skeleton::connection-append-write conn (octets 1))
      (web-skeleton::connection-append-write conn shared)
      (web-skeleton::connection-append-write conn (octets 4))
      (web-skeleton::%queue-tail conn (web-skeleton::make-write-span file 2 5))
      (web-skeleton::connection-append-write conn (octets 5))
      (check "joined: a vector handed over is not written into"
             (equalp shared (octets 2 3)) t)
      (check "joined: it is copied into the tail with the next"
             (equalp (first (web-skeleton::connection-write-queue conn))
                     (octets 2 3 4))
             t)
      (check "joined: a span is not joined, before or after"
             (mapcar #'web-skeleton::segment-length
                     (web-skeleton::connection-write-queue conn))
             '(3 3 1))
      (check "joined: and the file under it is untouched"
             (every (lambda (b) (= b 9)) file) t))))

;;; ---------------------------------------------------------------------------
;;; A verdict the discard loop must not spin on
;;; ---------------------------------------------------------------------------

(defun test-discard-available-want-write ()
  "CONNECTION-DISCARD-AVAILABLE hands back :WANT-WRITE instead of looping.

   CONNECTION-READ-INTO has four non-integer verdicts. A loop handling two
   would let :WANT-WRITE fall into the integer default, set a flag and go
   round again — a hot spin inside the event loop with no exit. Its own
   docstring says the cond is deliberately the same shape as
   CONNECTION-READ-AVAILABLE's so the two can be read side by side, and
   this holds it to that.

   Unreachable in practice: the only caller is the :STREAMING inbound path
   and there is no inbound TLS. That is how long a spin like this would
   stay invisible, and it is the argument for the assertion rather than
   against it.

   The stub is bounded on purpose. An unbounded one would hang the run
   under the defect rather than fail it, which is the *too violent* mode —
   a detector whose failure is indistinguishable from a machine problem.
   Bounded, the defect exhausts the budget, falls through to :EOF and
   answers :OK-EOF, so both assertions fail and the run continues.

   The call count is the second assertion for the same reason: a fix that
   answered :WANT-WRITE after looping ten times would satisfy the first
   check and still be the bug."
  (format t "~%Discard loop: :WANT-WRITE is handed back, not spun on~%")
  (let ((calls 0)
        (real (symbol-function 'web-skeleton::connection-read-into)))
    (unwind-protect
         (progn
           (setf (symbol-function 'web-skeleton::connection-read-into)
                 (lambda (conn buffer start max-bytes)
                   (declare (ignore conn buffer start max-bytes))
                   (incf calls)
                   (if (< calls 20) :want-write :eof)))
           (let ((conn (web-skeleton::make-connection
                        :fd -1 :state :streaming :last-active 0))
                 (sink (make-array 64 :element-type '(unsigned-byte 8))))
             (check "discard: :want-write is answered, not swallowed"
                    (web-skeleton::connection-discard-available conn sink)
                    :want-write)
             (check "discard: and it answered on the first read"
                    calls 1)))
      (setf (symbol-function 'web-skeleton::connection-read-into) real))))

;;; ---------------------------------------------------------------------------
;;; The third boot invariant
;;; ---------------------------------------------------------------------------

(defun test-fetch-timeout-validated-at-boot ()
  "START-SERVER refuses a non-positive *FETCH-TIMEOUT* before it binds.

   It already refuses a non-positive *WRITE-STALL-TIMEOUT* and an
   under-sized *MAX-WRITE-BACKLOG*, with the same reasoning: a
   misconfiguration should not wait for the shape that reveals it.
   *FETCH-TIMEOUT* is the floor under every way a fetch can fail to return
   — the DNS phase, the connect, the read, and the :AWAITING sweep that
   answers a parked caller 504 — and at zero the sweep never fires.

   The :HOST is deliberately invalid. If the validation is reverted,
   START-SERVER continues to MAKE-TCP-LISTENER, which refuses a three-byte
   vector by name — so the revert fails this assertion instead of starting
   a real server inside the suite. A detector that leaves a listener
   running when it fails is worse than none.

   Asserted on the message rather than on the fact of a raise, because both
   paths raise and only one of them is this invariant."
  (format t "~%start-server: the third boot invariant~%")
  (let ((web-skeleton:*fetch-timeout* 0))
    (check "boot: a zero *fetch-timeout* is refused by name"
           (let ((msg (attempt (web-skeleton:start-server
                                :host #(1 2 3) :port 0 :workers 1))))
             (and (stringp msg) (search "*fetch-timeout*" msg) t))
           t)))

(defun test-log-level-validated-at-boot ()
  "START-SERVER refuses a *LOG-LEVEL* that is not one of *LOG-LEVELS*, and
   a line logged at such a level is shown rather than dropped.

   Counted as the lowest severity, an unknown level would have
   (setf *log-level* :warning) turn on every DEBUG line with nothing said,
   and drop a line logged at :warning unless DEBUG were on.
   The invalid :HOST is TEST-FETCH-TIMEOUT-VALIDATED-AT-BOOT's device: with
   the check reverted, MAKE-TCP-LISTENER refuses it by another name, and
   no server starts."
  (format t "~%start-server: *log-level* is a known level~%")
  (let ((*log-level* :warning))
    (check "boot: an unknown *log-level* is refused by name"
           (let ((msg (attempt (web-skeleton:start-server
                                :host #(1 2 3) :port 0 :workers 1))))
             (and (stringp msg) (search "*log-level*" msg) t))
           t))
  (let ((out (make-string-output-stream)))
    (let ((*log-stream* out)
          (*log-level* :info))
      (log-msg :warning "a line at a misspelt level")
      (log-msg :debug "a line at debug"))
    (let ((text (get-output-stream-string out)))
      (check "log: a line at an unknown level is shown"
             (not (null (search "a line at a misspelt level" text))) t)
      (check "log: while DEBUG stays below INFO"
             (search "a line at debug" text) nil))))

(defun test-numeric-settings-validated-at-boot ()
  "START-SERVER refuses a numeric setting outside its range, by name: the
   timeouts and intervals the idle sweep, the ping pass, the keepalive
   tick, the fetch path, the drain and JWT-VERIFY read, and the sizes and
   counts the read path, the WebSocket parser, the fetch path and
   JSON-PARSE hold their input to. Read on a worker, a value that is not a
   number raises there: in the sweep it crashes the worker a second after
   the first WebSocket opens, and *MAX-CONNECTIONS* does at every accept.

   A fraction passes where the setting is held against a difference of
   clock readings, and is refused where it is added to the clock: those
   deadlines land in integer slots, and a fractional *CONNECT-FALLBACK-
   DELAY*, RFC 8305's 250 ms say, would fail every fetch to a name with
   two addresses.

   Each setting is tried with NIL and the nearest values outside its range,
   which are refused, and with the edges of its range, which pass its
   check: the refusal that comes then is the invalid :HOST's,
   TEST-FETCH-TIMEOUT-VALIDATED-AT-BOOT's device, so no server starts
   either way."
  (format t "~%start-server: the numeric settings are numbers in range~%")
  (flet ((refusal (name value)
           (progv (list name) (list value)
             (attempt (web-skeleton:start-server :host #(1 2 3) :port 0 :workers 1)))))
    (loop for (name bad edges)
            in '((web-skeleton:*idle-timeout* (nil -1) (0 5/2))
                 (web-skeleton:*ws-idle-timeout* (nil -1) (0 5/2))
                 (web-skeleton:*ws-message-timeout* (nil -1) (0 5/2))
                 (web-skeleton:*stream-idle-timeout* (nil -1) (0 5/2))
                 (web-skeleton:*linger-timeout* (nil -1) (0 0.5))
                 (web-skeleton:*stream-keepalive-interval* (nil -1) (0 5/2))
                 (web-skeleton:*ws-ping-interval* (nil 0) (1/2))
                 (web-skeleton:*worker-wake-interval* (nil 0) (0.05))
                 (web-skeleton:*fetch-timeout* (nil 0 2.5) (1))
                 (web-skeleton:*dns-cache-ttl* (nil -1 30.5) (0))
                 (web-skeleton:*drain-timeout* (nil -1 0.5) (0))
                 (web-skeleton:*jwt-clock-skew* (nil -1 0.5) (0))
                 (web-skeleton:*connect-fallback-delay* (nil 0 0.25 1/2) (1))
                 (web-skeleton:*max-ws-message-size* (nil 0) (1))
                 (web-skeleton:*max-request-line-length* (nil 0 1.5) (1))
                 (web-skeleton:*max-header-count* (nil 0) (1))
                 (web-skeleton:*max-header-line-length* (nil 0) (1))
                 (web-skeleton:*max-total-header-bytes* (nil 0) (1))
                 (web-skeleton:*max-ws-payload-size* (nil 0) (1))
                 (web-skeleton:*max-outbound-response-size* (nil 0) (1))
                 (web-skeleton:*max-streaming-line-size* (nil 0) (1))
                 (web-skeleton:*dns-cache-max-entries* (nil 0) (1))
                 (web-skeleton:*json-max-depth* (nil 0) (1))
                 (web-skeleton:*json-max-string-length* (nil 0) (1))
                 (web-skeleton:*ws-max-missed-pongs* (nil 0 1.5) (1))
                 (web-skeleton:*max-body-size* (nil -1 1.5) (0))
                 (web-skeleton:*max-connections* (nil -1) (0))
                 (web-skeleton:*max-interim-responses* (nil -1) (0)))
          do (let ((label (string-downcase (symbol-name name))))
               (dolist (value bad)
                 (check (format nil "boot: ~a ~s is refused by name" label value)
                        (let ((msg (refusal name value)))
                          (and (stringp msg) (search label msg) t))
                        t))
               (dolist (value edges)
                 (check (format nil "boot: ~a ~s passes its check" label value)
                        (let ((msg (refusal name value)))
                          (and (stringp msg) (not (search label msg)) t))
                        t))))))

(defun test-listen-backlog-reaches-the-listener ()
  "MAKE-TCP-LISTENER listens with *LISTEN-BACKLOG*, and START-SERVER
   refuses one that is not a positive integer.

   Nothing accepts on these listeners, so the accept queue is all there
   is. At a backlog of 1 Linux holds two completed connections, and a
   third connect waits, its SYN dropped; at 8 the third completes at
   once. A listener that ignored the setting would answer both the same."
  (format t "~%start-server: *listen-backlog* reaches listen(2)~%")
  (flet ((third-connect-completes-p (backlog)
           (let* ((web-skeleton:*listen-backlog* backlog)
                  (listener (web-skeleton::make-tcp-listener #(127 0 0 1) 0))
                  (port (nth-value 1 (sb-bsd-sockets:socket-name listener)))
                  (clients (loop repeat 3
                                 collect (make-instance 'sb-bsd-sockets:inet-socket
                                                        :type :stream :protocol :tcp))))
             (unwind-protect
                  (progn
                    (sb-bsd-sockets:socket-connect (first clients) #(127 0 0 1) port)
                    (sb-bsd-sockets:socket-connect (second clients) #(127 0 0 1) port)
                    (handler-case
                        (progn (web-skeleton::blocking-connect
                                (third clients) #(127 0 0 1) port 0.5)
                               t)
                      (error () nil)))
               (dolist (c clients) (ignore-errors (sb-bsd-sockets:socket-close c)))
               (ignore-errors (sb-bsd-sockets:socket-close listener))))))
    (check "backlog: at 1, a third connect waits"
           (third-connect-completes-p 1) nil)
    (check "backlog: at 8, it completes"
           (third-connect-completes-p 8) t))
  (let ((web-skeleton:*listen-backlog* 0))
    (check "boot: a zero *listen-backlog* is refused by name"
           (let ((msg (attempt (web-skeleton:start-server
                                :host #(1 2 3) :port 0 :workers 1))))
             (and (stringp msg) (search "*listen-backlog*" msg) t))
           t)))

(defun test-accept-goes-on-past-a-connection-it-could-not-set-up ()
  "A connection accepted and then closed, because it could not be set up,
   does not stop ACCEPT-PENDING: the connections queued behind it are
   accepted too.

   The listener is edge-triggered, and its event does not come again for
   connections already waiting, so one left in the backlog waits for the
   next client to arrive. The set-up is made to fail here by an epoll fd
   that is not one, which EPOLL-ADD refuses; two clients are queued, and
   the backlog is empty afterwards, with nothing left in the table. The
   failure is logged once for the two, throttled as a failed accept is,
   since one that fails for a connection is likely to fail for each."
  (format t "~%accept: on past a connection it could not set up~%")
  (let* ((listener (web-skeleton::make-tcp-listener #(127 0 0 1) 0))
         (port (nth-value 1 (sb-bsd-sockets:socket-name listener)))
         (clients (loop repeat 2
                        collect (make-instance 'sb-bsd-sockets:inet-socket
                                               :type :stream :protocol :tcp)))
         (log (make-string-output-stream))
         (web-skeleton::*connections* (make-hash-table :test #'eql))
         (web-skeleton:*log-stream* log)
         (web-skeleton::*log-throttle* (make-hash-table :test #'eq)))
    (unwind-protect
         (progn
           (dolist (c clients)
             (sb-bsd-sockets:socket-connect c #(127 0 0 1) port))
           (check "set-up failing: ACCEPT-PENDING drains, nothing to retry"
                  (web-skeleton::accept-pending listener -1) nil)
           (check "set-up failing: no connection is left in the backlog"
                  (let ((left (sb-bsd-sockets:socket-accept listener)))
                    (when left (sb-bsd-sockets:socket-close left))
                    (and left t))
                  nil)
           (check "set-up failing: and none in the table"
                  (hash-table-count web-skeleton::*connections*) 0)
           (check "set-up failing: logged once for the two"
                  (let ((text (get-output-stream-string log)))
                    (loop with start = 0
                          for at = (search "accept failed" text :start2 start)
                          while at count t do (setf start (1+ at))))
                  1))
      (dolist (c clients) (ignore-errors (sb-bsd-sockets:socket-close c)))
      (ignore-errors (sb-bsd-sockets:socket-close listener)))))

(defun test-on-tick-runs-with-the-workers-bindings ()
  "ON-TICK runs on each worker's own thread with that worker's epoll fd
   and connection table bound.

   The bindings are the entire primitive. Fanning a message out to the
   connections a worker owns has to happen from that worker, and WS-SEND's
   ownership guard is (WHEN *EPOLL-FD* ...) — so a hook running anywhere
   else would not merely fail, it would skip the guard and append to an
   unlocked queue while reporting success. Asserting the hook was called
   proves almost nothing; asserting the bindings are live inside it is what
   makes the call worth making.

   Two workers, so `every worker ticks` is measured rather than inferred
   from where the call sits in RUN-EVENT-LOOP."
  (format t "~%on-tick: runs on every worker with its bindings live~%")
  ;; Counted, not flagged. A boolean initialised to T and only cleared on
  ;; failure passes when the hook never runs at all, which is the one
  ;; outcome this test exists to catch.
  (let ((ticks 0) (ids nil) (epoll-live 0) (table-live 0)
        (lock (sb-thread:make-mutex :name "on-tick-bindings")))
    (with-test-server (:workers 2
                       :on-tick (lambda (id)
                                  (sb-thread:with-mutex (lock)
                                    (incf ticks)
                                    (pushnew id ids)
                                    (when web-skeleton::*epoll-fd*
                                      (incf epoll-live))
                                    (when web-skeleton::*connections*
                                      (incf table-live)))))
      ;; Control. Passes whether or not ON-TICK exists, so a failure here
      ;; says the fixture broke rather than the feature.
      (check "control: the server is up and answering"
             (multiple-value-bind (status) (test-http-request :get "/__nonce")
               (eql status 200))
             t)
      (sleep 0.6)
      (sb-thread:with-mutex (lock)
        (check "on-tick ran" (> ticks 0) t)
        (check "*epoll-fd* was bound on every tick"
               (and (> ticks 0) (= epoll-live ticks)) t)
        (check "*connections* was bound on every tick"
               (and (> ticks 0) (= table-live ticks)) t)
        (check "both workers ticked"
               (and (member 0 ids) (member 1 ids) t)
               t)))))

(defun test-on-tick-survives-a-raising-hook ()
  "A raising hook is caught and the loop keeps turning.

   A per-pass hook runs thousands of times a second under load, so any
   raise it can produce it will produce. Letting one out would take down a
   worker and every connection on it.

   Asserted on the tick COUNT rather than on the server still answering,
   because RUN-WORKER restarts a crashed worker with backoff — so a server
   that answers proves nothing here. Eight-odd ticks in 0.4s at the
   harness's 0.05s wake interval means the same loop kept running; a crash
   and restart cycle would show one or two. The counter also makes a revert
   visible: with no ON-TICK support it stays 0 rather than passing because
   nothing ever ran."
  (format t "~%on-tick: a raising hook does not take the worker down~%")
  (let ((entered 0)
        (lock (sb-thread:make-mutex :name "on-tick-raise"))
        ;; Global SETF, not a binding: the worker is a thread START-SERVER
        ;; spawns and reads the global value, so a LET here would capture
        ;; nothing and the count below would pass on an empty string.
        (log (make-string-output-stream))
        (saved-stream web-skeleton::*log-stream*))
    (setf web-skeleton::*log-stream* log)
    (unwind-protect
         (with-test-server (:on-tick (lambda (id)
                                       (declare (ignore id))
                                       (sb-thread:with-mutex (lock) (incf entered))
                                       (error "deliberate: on-tick raised")))
           (sleep 0.4)
           (let ((n (sb-thread:with-mutex (lock) entered)))
             (check "the hook ran and raised" (> n 0) t)
             (check "the loop kept turning rather than crashing and backing off"
                    (> n 3) t))
           (check "the server still answers after all that"
                  (multiple-value-bind (status) (test-http-request :get "/__nonce")
                    (eql status 200))
                  t))
      (setf web-skeleton::*log-stream* saved-stream))
    ;; Several raises and one report. A hook that raises on every pass would
    ;; otherwise write a line on every pass, holding the one lock every worker
    ;; takes to log — and the suite passed with that limit removed, because
    ;; nothing counted the lines.
    (let* ((text (get-output-stream-string log))
           (reports (loop with start = 0
                          for i = (search ":on-tick raised" text :start2 start)
                          while i count t do (setf start (1+ i)))))
      (check "a hook raising on every pass is reported once, not every pass"
             reports 1))))

(defun %what-escapes (thunk)
  "What THUNK signals, as (STORAGE-CONDITION-P TIMEOUT-P ERROR-P), or
   :RETURNED."
  (handler-case (progn (funcall thunk) :returned)
    (serious-condition (c)
      (list (typep c 'storage-condition)
            (typep c 'sb-ext:timeout)
            (typep c 'error)))))

(defun test-serious-conditions-cost-the-request ()
  "Application code that runs out of stack, out of heap, or out of the time
   it gave itself costs its own request, and nothing more.

   The first two signal a STORAGE-CONDITION and the third an SB-EXT:TIMEOUT,
   and none of them is an ERROR, so a catch written as (ERROR (E) ...) lets
   it past — RUN-WORKER's included — and SBCL ends the process: one
   request's recursion, or one slow upstream behind a handler's own
   WITH-TIMEOUT, takes the whole server down.

   One server for every site an HTTP request can reach, asserted in an
   order where each answer is read off the request that caused it. Under
   a revert the first escaping condition ends the worker — through
   CALL-WITH-ESCAPES-ENDING-THE-THREAD, rather than the process — and
   every check after it fails on a refused connection, which is the cost
   being asserted against. The checks between the failures are the
   server still answering, and they are what separate a 500 from a worker
   that happened to restart in time."
  (format t "~%serious conditions: stack, heap or a handler's own timeout cost one request~%")
  ;; The premise. If any of these were an ERROR, every check below would
  ;; pass against code that never looks past ERROR.
  (check "premise: unbounded recursion is a storage-condition, not an error"
         (%what-escapes (lambda () (%exhaust-the-stack 0)))
         '(t nil nil))
  (check "premise: an allocation past the heap is one too"
         (%what-escapes #'%exhaust-the-heap)
         '(t nil nil))
  (check "premise: the end of a with-timeout is a timeout, not an error"
         (%what-escapes #'%run-out-of-time)
         '(nil t nil))
  (let ((upstream-port nil))
    (call-with-escapes-ending-the-thread
     (lambda ()
       (with-test-server
           (:handler
            (lambda (req)
              (let ((path (http-request-path req)))
                (flet ((fetch-then (fail)
                         (defer-to-fetch :get (format nil "http://127.0.0.1:~d/upstream"
                                                      upstream-port)
                           :then (lambda (s h b)
                                   (declare (ignore s h b))
                                   (funcall fail))))
                       (fetch-on-body (fail)
                         (http-fetch :get (format nil "http://127.0.0.1:~d/upstream-chunked"
                                                  upstream-port)
                                     :on-body (lambda (out chunk)
                                                (declare (ignore out chunk))
                                                (funcall fail))
                                     :then (lambda (s h b)
                                             (declare (ignore s h b))
                                             (make-text-response 200 "unreached")))))
                  (cond
                    ((string= path "/stack") (%exhaust-the-stack 0))
                    ((string= path "/heap") (%exhaust-the-heap))
                    ((string= path "/late") (%run-out-of-time))
                    ((string= path "/ws") :upgrade)
                    ((string= path "/upstream") (make-text-response 200 "up"))
                    ((string= path "/upstream-chunked")
                     (make-stream-response
                      :on-open (lambda (c)
                                 (stream-send c (sb-ext:string-to-octets
                                                 "chunk" :external-format :ascii))
                                 (stream-close c))))
                    ((string= path "/then")
                     (fetch-then (lambda () (%exhaust-the-stack 0))))
                    ((string= path "/then-late") (fetch-then #'%run-out-of-time))
                    ((string= path "/on-body")
                     (fetch-on-body (lambda () (%exhaust-the-stack 0))))
                    ((string= path "/on-body-late")
                     (fetch-on-body #'%run-out-of-time))
                    (t (make-text-response 200 "alive"))))))
            :ws-handler (lambda (conn frame)
                          (declare (ignore conn))
                          (if (equalp (ws-frame-payload frame)
                                      (sb-ext:string-to-octets
                                       "late" :external-format :ascii))
                              (%run-out-of-time)
                              (%exhaust-the-stack 0))))
         (setf upstream-port *test-port*)
         (flet ((status (path)
                  (attempt (values (test-http-request :get path))))
                (ws-answer (text)
                  ;; What a WebSocket gets back for one frame of TEXT.
                  (let* ((sock (attempt (connect-to-test-server)))
                         (stream (and (typep sock 'sb-bsd-sockets:socket)
                                      (sb-bsd-sockets:socket-make-stream
                                       sock :input t :output t
                                       :element-type '(unsigned-byte 8)))))
                    (unwind-protect
                         (and stream
                              (search "101" (%ws-client-upgrade stream))
                              (progn
                                (write-sequence (%ws-client-frame 1 text) stream)
                                (force-output stream)
                                (multiple-value-bind (buf reason)
                                    (read-until-bounded stream :seconds 3)
                                  (list reason (fill-pointer buf)))))
                      (when stream (ignore-errors (close stream)))
                      (when (typep sock 'sb-bsd-sockets:socket)
                        (ignore-errors (sb-bsd-sockets:socket-close sock)))))))
           (check "a handler out of stack is answered 500" (status "/stack") 500)
           (check "and the server answers the next request" (status "/") 200)
           (check "a handler out of heap is answered 500" (status "/heap") 500)
           (check "and the next one after that" (status "/") 200)
           (check "a handler whose own with-timeout ran out is answered 500"
                  (status "/late") 500)
           (check "and the next one after that, too" (status "/") 200)
           (check "a fetch :then out of stack is answered 500" (status "/then") 500)
           (check "a fetch :then whose with-timeout ran out is answered 500"
                  (status "/then-late") 500)
           (check "an :on-body out of stack fails its fetch, 502"
                  (status "/on-body") 502)
           (check "an :on-body whose with-timeout ran out fails its fetch, 502"
                  (status "/on-body-late") 502)
           ;; The WebSocket arm closes without a frame, exactly as it does for
           ;; an ERROR: a WS-SEND may have left part of a frame on the wire.
           (check "a ws-handler out of stack closes its connection, unanswered"
                  (ws-answer "deep") '(:eof 0))
           (check "a ws-handler whose with-timeout ran out closes it the same way"
                  (ws-answer "late") '(:eof 0))
           (check "and the server answers after that too" (status "/") 200)))))))

(defun test-serious-conditions-spare-the-hook ()
  "An :ON-TICK hook that runs out of stack, or out of the time it gave
   itself, is reported like one that raises, and the loop keeps turning.

   A server of its own, because under a revert the escape ends the worker
   on its first pass, before any request could tell one site from another.
   Asserted on the pass count, as TEST-ON-TICK-SURVIVES-A-RAISING-HOOK is
   and for its reason: RUN-WORKER restarts a crashed worker, so a server
   that still answers proves nothing about the loop that was running. The
   first pass runs out of stack and the second out of time, so a loop that
   survives only one of them stops after it."
  (format t "~%serious conditions: an :on-tick hook out of stack, then out of time~%")
  (let ((ticks 0)
        (lock (sb-thread:make-mutex :name "serious-tick"))
        (log (make-string-output-stream))
        (saved-stream web-skeleton::*log-stream*))
    (setf web-skeleton::*log-stream* log)
    (unwind-protect
         (call-with-escapes-ending-the-thread
          (lambda ()
            (with-test-server (:on-tick (lambda (id)
                                          (declare (ignore id))
                                          (case (sb-thread:with-mutex (lock)
                                                  (incf ticks))
                                            (1 (%exhaust-the-stack 0))
                                            (2 (%run-out-of-time)))))
              (sleep 0.4)
              (check "the hook ran out of stack, then time, and the loop kept turning"
                     (> (sb-thread:with-mutex (lock) ticks) 3)
                     t))))
      (setf web-skeleton::*log-stream* saved-stream))
    (let ((text (get-output-stream-string log)))
      (check "the hook's first condition was reported"
             (not (null (search ":on-tick raised" text))) t)
      (check "and no worker crashed and restarted for either"
             (search "crashed" text) nil))))

(defun test-serious-conditions-at-the-backstop ()
  "A timeout that reaches a worker outside every narrower catch restarts
   that worker; it does not end the process.

   SB-EXT:WITH-TIMEOUT delivers its TIMEOUT by interrupting the thread
   from a timer, and a timer that expires as the timed call returns can
   deliver it after the call has left: into the event loop, outside every
   catch around application code. RUN-WORKER's backstop is what remains
   there, and a backstop on ERROR alone lets a TIMEOUT through.

   Delivered here the way the timer delivers it, by INTERRUPT-THREAD, to a
   worker with nothing to do, so it lands in the loop rather than inside a
   request's catch. Under a revert it ends the worker — through
   CALL-WITH-ESCAPES-ENDING-THE-THREAD, rather than the process — and the
   server never answers again."
  (format t "~%serious conditions: a stray timeout at RUN-WORKER's backstop~%")
  (let ((log (make-string-output-stream))
        (saved-stream web-skeleton::*log-stream*)
        ;; The worker that answers, recorded by the handler on its own
        ;; thread. A lookup by thread name could find a worker an earlier
        ;; server left behind, and interrupt that one instead.
        (answered-on nil)
        (lock (sb-thread:make-mutex :name "backstop-worker")))
    (setf web-skeleton::*log-stream* log)
    (unwind-protect
         (call-with-escapes-ending-the-thread
          (lambda ()
            (with-test-server (:handler (lambda (req)
                                          (declare (ignore req))
                                          (sb-thread:with-mutex (lock)
                                            (setf answered-on sb-thread:*current-thread*))
                                          (make-text-response 200 "ok")))
              (flet ((answers-p (seconds)
                       ;; Retried: across a restart there is a second with
                       ;; no listener, and a refused connect is expected.
                       (let ((deadline (+ (get-internal-real-time)
                                          (* seconds
                                             internal-time-units-per-second))))
                         (loop
                           (when (eql (attempt (values (test-http-request
                                                        :get "/")))
                                      200)
                             (return t))
                           (when (> (get-internal-real-time) deadline)
                             (return nil))
                           (sleep 0.1)))))
                (check "control: the server answers before the timeout"
                       (answers-p 2) t)
                (let ((worker (sb-thread:with-mutex (lock) answered-on)))
                  (check "control: the worker thread was found" (not (null worker)) t)
                  (when worker
                    (sb-thread:interrupt-thread
                     worker (lambda () (error 'sb-ext:timeout)))))
                ;; The restart backs off a second before it binds again.
                (check "the worker took the timeout, restarted, and answers"
                       (answers-p 6) t)))))
      (setf web-skeleton::*log-stream* saved-stream))
    (check "the log names the restart"
           (not (null (search "crashed" (get-output-stream-string log))))
           t)))

(defun test-serious-conditions-at-the-callback-sites ()
  "Each remaining place application code is called, given a callback that
   runs out of stack or out of the time it gave itself, returns the way it
   returns for one that raises.

   Driven directly rather than through a server: these are teardown paths
   and policy hooks, where the answer is that the call came back and what
   it came back with. Under a revert the condition leaves the call, and
   %WHAT-ESCAPES reports it in place of the answer."
  (format t "~%serious conditions: the callback sites~%")
  (let ((web-skeleton:*log-stream* (make-broadcast-stream)))
    (loop
      for (how . fail) in (list (cons "out of stack" (lambda () (%exhaust-the-stack 0)))
                                (cons "out of time" #'%run-out-of-time))
      do (let ((fail fail))
           ;; Fails closed, the same as a raising filter: no address is
           ;; dialed on the word of a policy that could not answer.
           (let ((web-skeleton:*fetch-address-filter*
                   (lambda (ip family host)
                     (declare (ignore ip family host))
                     (funcall fail))))
             (check (format nil "an address filter ~a refuses the address" how)
                    (%what-escapes
                     (lambda ()
                       (web-skeleton::fetch-address-allowed-p
                        #(93 184 216 34) :inet "x")))
                    :returned)
             (check (format nil "and the answer to a filter ~a is a refusal" how)
                    (handler-case
                        (web-skeleton::fetch-address-allowed-p
                         #(93 184 216 34) :inet "x")
                      (serious-condition () :escaped))
                    nil))
           (check (format nil "a stream's :on-close ~a is absorbed like a raise" how)
                  (%what-escapes
                   (lambda ()
                     (web-skeleton::notify-stream-closed
                      (web-skeleton::make-connection
                       :fd -1
                       :stream-on-close (lambda (conn reason)
                                          (declare (ignore conn reason))
                                          (funcall fail)))
                      :closed)))
                  :returned)
           (check (format nil "a fetch cleanup callback ~a is absorbed" how)
                  (%what-escapes
                   (lambda ()
                     (web-skeleton::finish-fetch (lambda (s h b)
                                                   (declare (ignore s h b))
                                                   (funcall fail))
                                                 :aborted)))
                  :returned)
           (check (format nil "a detached fetch's :then ~a is absorbed" how)
                  (%what-escapes
                   (lambda ()
                     (let ((web-skeleton::*connections* (make-hash-table)))
                       (web-skeleton::deliver-detached
                        4242 -1 (lambda (s h b)
                                  (declare (ignore s h b))
                                  (funcall fail))
                        :delivered :status 200))))
                  :returned)
           ;; LIFO: the hook registered last runs first, so the failing one
           ;; is registered second and the one that records is registered
           ;; first. Reaching the recorder is the claim — a raise must not
           ;; stop the rest.
           (let* ((reached nil)
                  (web-skeleton::*shutdown-hooks* nil))
             (web-skeleton:register-cleanup (lambda () (setf reached t)))
             (web-skeleton:register-cleanup (lambda () (funcall fail)))
             (check (format nil "a shutdown hook ~a does not stop the next one" how)
                    (list (%what-escapes #'web-skeleton::run-shutdown-hooks)
                          reached)
                    '(:returned t)))))))

(defun test-a-raise-after-the-response-began ()
  "Application code that raises after its response has begun gets the
   connection closed, not a second response.

   A stream's ON-OPEN runs after the head is queued, and on an idle socket
   the head — and whatever ON-OPEN sent — is on the wire by the time it
   raises. Answering that with a 500 put a whole second response inside
   the first one's chunked body: a status line where a reader expects a
   chunk-size, which a proxy reports as an invalid chunked response, and
   two responses counted for one request.

   The WebSocket half is the same rule from the other side: a raise outside
   the ws-handler's own catch put an HTTP 500 among the frames. Provoked by
   making the read path raise once on the established WebSocket, since a
   ws-handler's own raise is caught before it gets that far.

   Controls: a handler that raises before answering still gets its 500,
   and a stream that ends normally still gets its terminator."
  (format t "~%a raise after the response began: closed, not answered twice~%")
  (let ((reasons nil)
        (armed nil)
        (lock (sb-thread:make-mutex :name "begun-response")))
    (flet ((ascii (s) (sb-ext:string-to-octets s :external-format :ascii)))
      (with-test-server
          (:handler
           (lambda (req)
             (let ((path (http-request-path req)))
               (cond
                 ((string= path "/raises-midway")
                  (make-stream-response
                   :on-open (lambda (c)
                              (stream-send c (ascii "first part"))
                              (error "deliberate: the producer failed after ~
                                      its first send"))
                   :on-close (lambda (c reason)
                               (declare (ignore c))
                               (sb-thread:with-mutex (lock)
                                 (push reason reasons)))))
                 ((string= path "/ends-normally")
                  (make-stream-response
                   :on-open (lambda (c)
                              (stream-send c (ascii "whole"))
                              (stream-close c))))
                 ((string= path "/raises-first")
                  (error "deliberate: the handler failed before answering"))
                 ((string= path "/ws") :upgrade)
                 (t (make-text-response 200 "ok")))))
           :ws-handler (lambda (conn frame)
                         (declare (ignore conn frame))
                         nil))
        (flet ((raw-get (path)
                 ;; Everything the server sends for PATH, read to the close.
                 (let* ((sock (connect-to-test-server))
                        (stream (sb-bsd-sockets:socket-make-stream
                                 sock :input t :output t
                                 :element-type '(unsigned-byte 8))))
                   (unwind-protect
                        (progn
                          (write-sequence
                           (ascii (crlf (format nil "GET ~a HTTP/1.1" path)
                                        "Host: localhost" "Connection: close"))
                           stream)
                          (force-output stream)
                          (multiple-value-bind (buf reason)
                              (read-until-bounded stream :seconds 3)
                            (values (sb-ext:octets-to-string
                                     buf :external-format :latin-1)
                                    reason)))
                     (ignore-errors (close stream))
                     (ignore-errors (sb-bsd-sockets:socket-close sock)))))
               (status-lines (text)
                 (loop with start = 0
                       for i = (search "HTTP/1.1 " text :start2 start)
                       while i count t do (setf start (1+ i))))
               (ends-with-terminator-p (text)
                 (let ((n (length text)))
                   (and (>= n 5)
                        (string= (subseq text (- n 5))
                                 (coerce '(#\0 #\Return #\Newline
                                           #\Return #\Newline)
                                         'string))))))
          (multiple-value-bind (text reason) (raw-get "/raises-midway")
            (check "midway: the head and the first chunk went out"
                   (and (search "HTTP/1.1 200" text) (search "first part" text) t)
                   t)
            (check "midway: no second status line follows them"
                   (status-lines text) 1)
            (check "midway: and no terminator claims the body complete"
                   (ends-with-terminator-p text) nil)
            (check "midway: the peer saw the connection close" reason :eof))
          (check "midway: on-close heard :error, once"
                 (sb-thread:with-mutex (lock) reasons) '(:error))
          (check "control: a handler raising before answering gets its 500"
                 (attempt (values (test-http-request :get "/raises-first")))
                 500)
          (check "control: a stream that ends normally gets its terminator"
                 (ends-with-terminator-p (values (raw-get "/ends-normally")))
                 t)
          ;; The WebSocket half.
          (let* ((sock (connect-to-test-server))
                 (stream (sb-bsd-sockets:socket-make-stream
                          sock :input t :output t
                          :element-type '(unsigned-byte 8))))
            (unwind-protect
                 (progn
                   ;; Raises after a real read that found the frame, not
                   ;; instead of it: bytes left unread in the kernel make
                   ;; close(2) send RST, which can destroy what was sent
                   ;; before it, and the check below has to see what was
                   ;; sent. The read the worker makes as the upgrade
                   ;; completes finds nothing and answers :CONTINUE.
                   (sb-int:encapsulate
                    'web-skeleton::connection-on-read 'raise-once
                    (lambda (f conn)
                      (let ((verdict (funcall f conn)))
                        (when (and armed (eq verdict :websocket))
                          (setf armed nil)
                          (error "deliberate: the read path raised on a ~
                                  WebSocket"))
                        verdict)))
                   (check "ws: the upgrade completes"
                          (not (null (search "101" (%ws-client-upgrade stream))))
                          t)
                   (setf armed t)
                   (write-sequence (%ws-client-frame 1 "hello") stream)
                   (force-output stream)
                   (check "ws: the raise closes the WebSocket with nothing sent"
                          (multiple-value-bind (buf reason)
                              (read-until-bounded stream :seconds 3)
                            (list reason (fill-pointer buf)))
                          '(:eof 0)))
              (sb-int:unencapsulate 'web-skeleton::connection-on-read 'raise-once)
              (ignore-errors (close stream))
              (ignore-errors (sb-bsd-sockets:socket-close sock)))))))))

(defun test-a-worker-crash-closes-its-streams-with-on-close ()
  "A stream on a worker that crashes hears ON-CLOSE, once, with :ERROR.

   ON-CLOSE fires exactly once however the stream ends, and applications
   release whatever they registered for a stream there — a subscriber set,
   a fan-out list. A crash cleanup that closed every client connection
   with the bare CONNECTION-CLOSE, which does not notify, would leave every
   stream on that worker registered for good.

   The crash is real: the worker is interrupted with an error while idle,
   which reaches RUN-WORKER's backstop the way any escaping raise does.
   The worker is the one the handler ran on, recorded there: a lookup by
   thread name could find a worker an earlier server left behind."
  (format t "~%a worker crash tells its streams, through ON-CLOSE~%")
  (let ((reasons nil)
        (answered-on nil)
        (lock (sb-thread:make-mutex :name "crash-on-close"))
        (log (make-string-output-stream))
        (saved-stream web-skeleton::*log-stream*))
    (setf web-skeleton::*log-stream* log)
    (unwind-protect
         (with-test-server
             (:handler
              (lambda (req)
                (declare (ignore req))
                (sb-thread:with-mutex (lock)
                  (setf answered-on sb-thread:*current-thread*))
                (make-stream-response
                 :on-open (lambda (c)
                            (stream-send c (sb-ext:string-to-octets
                                            "open" :external-format :ascii)))
                 :on-close (lambda (c reason)
                             (declare (ignore c))
                             (sb-thread:with-mutex (lock)
                               (push reason reasons))))))
           (let* ((sock (connect-to-test-server))
                  (stream (sb-bsd-sockets:socket-make-stream
                           sock :input t :output t
                           :element-type '(unsigned-byte 8))))
             (unwind-protect
                  (progn
                    (write-sequence (sb-ext:string-to-octets
                                     (crlf "GET /events HTTP/1.1" "Host: x")
                                     :external-format :ascii)
                                    stream)
                    (force-output stream)
                    (check "control: the stream is open and has sent its first chunk"
                           (nth-value 1 (read-until-bounded
                                         stream :seconds 3
                                         :until (lambda (buf fill)
                                                  (search (sb-ext:string-to-octets
                                                           "open" :external-format :ascii)
                                                          buf :end2 fill))))
                           :satisfied)
                    (let ((worker (sb-thread:with-mutex (lock) answered-on)))
                      (check "control: the worker the stream is on was found"
                             (not (null worker)) t)
                      (when worker
                        (sb-thread:interrupt-thread
                         worker (lambda () (error "deliberate: the worker crashed")))))
                    (check "the peer sees its stream end"
                           (nth-value 1 (read-until-bounded stream :seconds 3))
                           :eof)
                    (let ((deadline (+ (get-internal-real-time)
                                       (* 3 internal-time-units-per-second))))
                      (loop until (or (sb-thread:with-mutex (lock) reasons)
                                      (> (get-internal-real-time) deadline))
                            do (sleep 0.05)))
                    (check "ON-CLOSE heard the crash, once, as :error"
                           (sb-thread:with-mutex (lock) reasons)
                           '(:error)))
               (ignore-errors (close stream))
               (ignore-errors (sb-bsd-sockets:socket-close sock)))))
      (setf web-skeleton::*log-stream* saved-stream))
    (check "control: the worker did crash"
           (not (null (search "crashed" (get-output-stream-string log))))
           t)))

(defun test-on-tick-validated-at-boot ()
  "START-SERVER refuses a non-function :ON-TICK before it binds.

   Same argument as the three boot invariants above it: a misconfiguration
   should not wait for the shape that reveals it. This one is worse than
   most if it waits — a value that cannot be funcalled raises on the first
   pass of every worker's loop and on every pass after, which is exactly
   the flood the rate limit in RUN-EVENT-LOOP exists to survive, reached
   through a typo instead of a bug.

   :HOST is deliberately invalid, so a revert fails this assertion at
   MAKE-TCP-LISTENER instead of starting a real server inside the suite."
  (format t "~%start-server: the fourth boot invariant~%")
  (check "boot: a non-function :on-tick is refused by name"
         (let ((msg (attempt (web-skeleton:start-server
                              :host #(1 2 3) :port 0 :workers 1
                              :on-tick 42))))
           (and (stringp msg) (search ":on-tick" msg) t))
         t)
  (check "control: the same call without :on-tick fails elsewhere"
         (let ((msg (attempt (web-skeleton:start-server
                              :host #(1 2 3) :port 0 :workers 1))))
           (and (stringp msg) (not (search ":on-tick" msg)) t))
         t)
  ;; A symbol naming a function is accepted, as :HANDLER accepts one, so a
  ;; hook redefined at the REPL reaches a running server. Accepted here means
  ;; the call gets past the check and fails on the bad host instead.
  (check "boot: a symbol naming a function is accepted"
         (let ((msg (attempt (web-skeleton:start-server
                              :host #(1 2 3) :port 0 :workers 1
                              :on-tick 'identity))))
           (and (stringp msg) (not (search ":on-tick" msg)) t))
         t)
  ;; And the two a symbol could still get wrong. Without these, accepting
  ;; every symbol would pass the assertion above.
  (check "boot: a symbol naming no function is still refused"
         (let ((msg (attempt (web-skeleton:start-server
                              :host #(1 2 3) :port 0 :workers 1
                              :on-tick 'no-function-is-named-this))))
           (and (stringp msg) (search ":on-tick" msg) t))
         t)
  (check "boot: a symbol naming a macro is refused, since it cannot be funcalled"
         (let ((msg (attempt (web-skeleton:start-server
                              :host #(1 2 3) :port 0 :workers 1
                              :on-tick 'when))))
           (and (stringp msg) (search ":on-tick" msg) t))
         t))

(defun %ws-conn (fd state)
  "Register a bare connection in the current *CONNECTIONS* with STATE."
  (let ((c (web-skeleton::make-connection :fd fd :last-active 0)))
    (setf (web-skeleton::connection-state c) state)
    (web-skeleton::register-connection c)
    c))

(defun test-map-worker-websockets-visits-only-websockets ()
  "The walk visits :WEBSOCKET and nothing else.

   The states it must skip are not hypothetical: a connection mid-upgrade,
   one already closing, and one writing a plain HTTP response all live in the
   same table, and handing any of them to an application that is about to
   call WS-SEND would produce a frame on a socket that is not speaking that
   protocol yet, or at all."
  (format t "~%map-worker-websockets: only websockets, only this worker~%")
  (let ((web-skeleton::*connections* (make-hash-table :test #'eql)))
    (%ws-conn 101 :websocket)
    (%ws-conn 102 :websocket)
    (%ws-conn 103 :read-http)
    (%ws-conn 104 :closing)
    (%ws-conn 105 :write-response)
    ;; Control: every state really is in the table, so a walk finding two is
    ;; selecting rather than failing to see the other three.
    (check "control: all five states are registered"
           (hash-table-count web-skeleton::*connections*) 5)
    (let ((seen nil))
      (let ((n (web-skeleton:map-worker-websockets
                (lambda (c) (push (web-skeleton::connection-fd c) seen)))))
        (check "visits exactly the websockets" (sort seen #'<) (list 101 102))
        (check "and returns how many it called" n 2)))))

(defun test-map-worker-websockets-skips-what-a-callback-closed ()
  "A connection an earlier callback closed is skipped, not handed over dead.

   This is what lets a broadcast call WS-SEND without asking first. One slow
   peer's send can close its connection, and on a table collected up front
   the entry behind it would otherwise still be walked — WS-SEND would then
   refuse it for not being in the table, turning one dead peer into a raise
   that stops the fan-out to everyone after it."
  (format t "~%map-worker-websockets: the liveness re-check~%")
  (let ((web-skeleton::*connections* (make-hash-table :test #'eql)))
    (let* ((a (%ws-conn 201 :websocket))
           (b (%ws-conn 202 :websocket))
           (calls 0))
      (let ((n (web-skeleton:map-worker-websockets
                (lambda (c)
                  (incf calls)
                  ;; Whichever one we were handed, close the other.
                  (web-skeleton::unregister-connection (if (eq c a) b a))))))
        (check "the closed one is not visited" calls 1)
        (check "the count reflects only what was called" n 1)))))

(defun test-map-worker-websockets-refuses-off-a-worker ()
  "Off a worker there is no table, and zero would be a lie.

   Returning zero would be indistinguishable from a server with no clients,
   which is the shape a background thread would see every time — the same
   silence that makes writing from off a worker dangerous in the first
   place, since the ownership guards are (WHEN *EPOLL-FD* ...) and skip."
  (format t "~%map-worker-websockets: refuses off a worker~%")
  (let ((web-skeleton::*connections* nil))
    (check "refuses by name rather than answering zero"
           (let ((msg (attempt (web-skeleton:map-worker-websockets #'identity))))
             (and (stringp msg) (search "connection table" msg) t))
           t)))

(defun test-map-worker-websockets-delivers-from-on-tick ()
  "A frame sent to every WebSocket from inside :ON-TICK reaches the client.

   The fan-out the demo is built on, end to end on a live server: a real
   handshake, a hook that walks the worker's WebSockets and sends each a
   frame, and that frame read back off the client's own socket. A walk run
   inside the hook over an empty table would show only that the hook can
   call it, and leave delivery resting on two halves proven apart.

   The hook sends once, when the test arms it, after the client has read its
   101. An upgrade the client has seen is one the worker has recorded: the
   state changes in the pass that writes the response, before any hook runs.
   The walk counts what it visits, and that count is the control — a frame
   from a hook whose walk found nothing did not come from the walk."
  (format t "~%map-worker-websockets: a frame from on-tick reaches the client~%")
  (let ((armed nil) (visited 0) (errs 0)
        (lock (sb-thread:make-mutex :name "fan-out")))
    (with-test-server
        (:handler (lambda (req) (declare (ignore req)) :upgrade)
         :ws-handler (lambda (conn frame) (declare (ignore conn frame)) nil)
         :on-tick (lambda (id)
                    (declare (ignore id))
                    (when (sb-thread:with-mutex (lock) (shiftf armed nil))
                      (handler-case
                          (web-skeleton:map-worker-websockets
                           (lambda (c)
                             (sb-thread:with-mutex (lock) (incf visited))
                             (web-skeleton:ws-send
                              c (web-skeleton:build-ws-text "from the tick"))))
                        (error () (sb-thread:with-mutex (lock) (incf errs)))))))
      (let* ((sock (connect-to-test-server))
             (stream (sb-bsd-sockets:socket-make-stream
                      sock :input t :output t :element-type '(unsigned-byte 8))))
        (unwind-protect
             (progn
               (check "control: the upgrade was accepted"
                      (not (null (search "101" (%ws-client-upgrade stream))))
                      t)
               (sb-thread:with-mutex (lock) (setf armed t))
               ;; A server frame is unmasked: a first byte, a length under 126
               ;; for a payload this short, and the payload.
               (let ((frame (read-until-bounded
                             stream :seconds 3
                             :until (lambda (buf fill)
                                      (and (>= fill 2)
                                           (>= fill (+ 2 (logand (aref buf 1)
                                                                 #x7f))))))))
                 (sb-thread:with-mutex (lock)
                   (check "control: the walk visited the one WebSocket" visited 1)
                   (check "and nothing in it signalled" errs 0))
                 (check "the frame the hook sent is the frame the client read"
                        (and (>= (length frame) 2)
                             (= (aref frame 0) #x81)
                             (string= (sb-ext:octets-to-string
                                       (subseq frame 2) :external-format :utf-8)
                                      "from the tick"))
                        t)))
          (ignore-errors (sb-bsd-sockets:socket-close sock)))))))

(defun test-connection-census-is-exported-with-a-split-contract ()
  "CONNECTION-CENSUS is reachable without :: and reports per worker.

   Two workers, and a connection held open for the duration, so the sums are
   over something rather than over nothing — a census of an empty server
   agrees with almost any mistake.

   Polled rather than slept: the census publishes on the maintenance gate at
   1 Hz, and each worker publishes its own slot on its own tick, so there is
   no single interval that is both long enough and not wasteful.

   :STATES and :PER-WORKER are asserted to exist and to be consistent with
   the sums, never on their exact contents, which is what the docstring
   calls diagnostic. A test pinning the state keywords would be a test of
   the state machine wearing a census costume."
  (format t "~%connection-census: exported, per worker, split contract~%")
  (with-test-server (:workers 2)
    (let ((held (connect-to-test-server)))
      (unwind-protect
           (let ((census nil)
                 (deadline (+ (get-universal-time) 6)))
             (loop until (or (let ((pw (getf census :per-worker)))
                               (and pw (= (length pw) 2) (every #'identity pw)))
                             (> (get-universal-time) deadline))
                   do (setf census (web-skeleton:connection-census))
                      (sleep 0.05))
             (check "every worker published a slot"
                    (let ((pw (getf census :per-worker)))
                      (and (= (length pw) 2) (every #'identity pw) t))
                    t)
             (check ":workers is the configured count"
                    (getf census :workers) 2)
             (check "the held connection is counted"
                    (>= (getf census :total 0) 1) t)
             (check "the sums are computed from the per-worker entries"
                    (= (getf census :total)
                       (reduce #'+ (getf census :per-worker)
                               :key (lambda (slot) (getf slot :total 0))))
                    t)
             (check ":states accounts for as many connections as :total"
                    (= (getf census :total)
                       (loop for (state n) on (getf census :states) by #'cddr
                             do (progn state)
                             sum n))
                    t))
        (ignore-errors (close held))))))

(defun test-a-log-line-is-one-line ()
  "One LOG-MSG call emits exactly one line, whatever it was handed.

   ~S on a nested structure pretty-prints by default, wrapping at
   *PRINT-RIGHT-MARGIN* and turning one call into a dozen lines. That is
   not cosmetic on this logger: it holds *LOG-LOCK* — the one lock every
   worker contends for — across the format and the FORCE-OUTPUT, and it
   makes a line unfindable, because a grep matching the message returns the
   first fragment and silently hides the rest.

   Found by tripping on it: a census logged at :DEBUG came out eleven lines
   wide, and reading one back needed grep -A6 and a guess at the number.

   Also asserts the bound. A value large enough to fill a disk from inside
   the mutex should truncate instead."
  (format t "~%log: one call, one line~%")
  (let ((deep (loop for i from 0 below 40
                    collect (list :k i :v (list :a i :b (list :c i))))))
    (let ((out (with-output-to-string (capture)
                 (let ((web-skeleton:*log-stream* capture)
                       (web-skeleton:*log-level* :debug))
                   (web-skeleton::log-debug "structure ~s" deep)))))
      ;; Control: it logged at all, and logged the thing it was given.
      (check "control: the message reached the stream"
             (and (search "structure" out) (search ":K" out) t) t)
      (check "one call is one line" (count #\Newline out) 1))
    (let ((out (with-output-to-string (capture)
                 (let ((web-skeleton:*log-stream* capture)
                       (web-skeleton:*log-level* :debug))
                   (web-skeleton::log-debug "wide ~s"
                                            (loop for i from 0 below 5000
                                                  collect i))))))
      (check "a runaway structure is truncated, not printed whole"
             (< (length out) 2000) t))))

(defun test-counters-count-responses-by-class ()
  "Every response is counted once in total and once in its class.

   The class split is the point. A total alone cannot tell a server answering
   a thousand requests from one refusing a thousand, and the refusals are the
   half an application cannot see for itself."
  (format t "~%counters: responses by class~%")
  (let ((web-skeleton::*counters* (web-skeleton::make-counters)))
    (mapc #'web-skeleton::note-response (list 100 200 201 301 400 404 500))
    (web-skeleton::note-accepted)
    (web-skeleton::note-refused)
    (web-skeleton::note-ws-frame)
    (let ((snap (web-skeleton::counters-snapshot)))
      (check "counted in total"  (getf snap :responses) 7)
      (check "1xx informational" (getf snap :informational) 1)
      (check "2xx successful"    (getf snap :successful) 2)
      (check "3xx redirected"    (getf snap :redirected) 1)
      (check "4xx client-error"  (getf snap :client-error) 2)
      (check "5xx server-error"  (getf snap :server-error) 1)
      (check "accepts taken"     (getf snap :accepted) 1)
      (check "accepts refused"   (getf snap :refused) 1)
      (check "ws frames queued"  (getf snap :ws-frames) 1)))
  (check "off a worker: nothing to snapshot, and no raise getting there"
         (let ((web-skeleton::*counters* nil))
           (web-skeleton::note-response 200)
           (web-skeleton::note-accepted)
           (web-skeleton::counters-snapshot))
         nil))

(defun test-counters-see-a-response-no-handler-produced ()
  "A request refused by the parser is counted, and the handler never runs.

   This is the argument for counting in the framework rather than in the
   application. Transfer-Encoding alongside Content-Length is refused before
   dispatch, so the caller gets a 400 no handler produced — and an
   application counting its own calls would report that request as never
   having happened at all.

   Polled rather than slept: the census publishes on the maintenance gate at
   1 Hz, which is slower than this test would care to guess at."
  (format t "~%counters: the refusals an application cannot see~%")
  ;; Counted on the worker and read here, so under a lock, as every other
  ;; cross-thread count in this file is.
  (let ((calls 0)
        (lock (sb-thread:make-mutex :name "counters-calls")))
    (with-test-server (:workers 1
                       :handler (lambda (req)
                                  (declare (ignore req))
                                  (sb-thread:with-mutex (lock) (incf calls))
                                  (web-skeleton:make-text-response 200 "ok")))
      (test-http-request :get "/")
      (let ((calls-before (sb-thread:with-mutex (lock) calls)))
        (let* ((sock (connect-to-test-server))
               (stream (sb-bsd-sockets:socket-make-stream
                        sock :input t :output t
                        :element-type '(unsigned-byte 8))))
          (unwind-protect
               (progn
                 (write-sequence
                  (sb-ext:string-to-octets
                   (crlf "POST /x HTTP/1.1" "Host: h"
                         "Content-Length: 5" "Transfer-Encoding: chunked")
                   :external-format :utf-8)
                  stream)
                 (force-output stream)
                 ;; The refusal's head read back, so the server is done
                 ;; with the request before the count is read.
                 (read-until-bounded
                  stream :seconds 5
                  :until (lambda (buf fill)
                           (web-skeleton::scan-crlf-crlf buf 0 fill))))
            (ignore-errors (close stream))
            (ignore-errors (sb-bsd-sockets:socket-close sock))))
        (check "the handler never ran for the refused request"
               (sb-thread:with-mutex (lock) calls) calls-before))
      ;; Waits for both responses rather than the first. A census published
      ;; between the two requests would otherwise end the poll on a count of
      ;; one and fail the total for a reason that has nothing to do with it.
      (let ((k nil) (deadline (+ (get-universal-time) 6)))
        (loop until (or (and k (>= (getf k :responses 0) 2))
                        (> (get-universal-time) deadline))
              do (setf k (getf (web-skeleton:connection-census) :counters))
                 (sleep 0.05))
        (check "control: the handled request was counted successful"
               (>= (getf k :successful 0) 1) t)
        (check "the refusal was counted as a client error"
               (>= (getf k :client-error 0) 1) t)
        (check "and both are in the total"
               (>= (getf k :responses 0) 2) t)
        ;; Two connections were taken, the refused one included: the parser
        ;; refused its request, not the accept. The class counts above are
        ;; asserted on responses and would all pass with the accept count
        ;; never incremented, which is the gap this closes.
        (check "every connection taken is counted as an accept"
               (>= (getf k :accepted 0) 2) t)))))

(defun %ws-client-upgrade (stream)
  "Send an RFC 6455 handshake for /ws on STREAM and read the response head,
   to its blank line and no further, so a frame the server sends next is left
   on the stream for the caller. Returns the head as a string, for the caller
   to find its 101 in."
  (write-sequence (sb-ext:string-to-octets
                   (crlf "GET /ws HTTP/1.1" "Host: localhost"
                         "Upgrade: websocket" "Connection: Upgrade"
                         "Sec-WebSocket-Version: 13"
                         "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==")
                   :external-format :ascii)
                  stream)
  (force-output stream)
  (sb-ext:octets-to-string
   (read-until-bounded stream
                       :seconds 3
                       :until (lambda (buf fill)
                                (and (>= fill 4)
                                     (= (aref buf (- fill 4)) 13)
                                     (= (aref buf (- fill 3)) 10)
                                     (= (aref buf (- fill 2)) 13)
                                     (= (aref buf (- fill 1)) 10))))
   :external-format :latin-1))

(defun %reset-socket (socket)
  "Close SOCKET with a reset rather than a FIN.

   SO_LINGER on with a zero timeout makes close(2) drop anything unsent and
   send RST, which is how a client looks when it crashes or its network drops
   it. Signals if the option does not take: a close that quietly sent a FIN
   instead would pass any test built on this, whether the server handled a
   reset or not."
  (let ((fd (sb-bsd-sockets:socket-file-descriptor socket)))
    (sb-alien:with-alien ((linger (array sb-alien:int 2)))
      (setf (sb-alien:deref linger 0) 1
            (sb-alien:deref linger 1) 0)
      (unless (zerop (sb-alien:alien-funcall
                      (sb-alien:extern-alien
                       "setsockopt"
                       (function sb-alien:int sb-alien:int sb-alien:int
                                 sb-alien:int sb-alien:system-area-pointer
                                 sb-alien:unsigned))
                      fd web-skeleton::+sol-socket+ +so-linger+
                      (sb-alien:alien-sap linger) 8))
        (error "setsockopt SO_LINGER failed on fd ~d" fd)))
    (sb-bsd-sockets:socket-close socket)))

(defun %ws-client-frame (opcode payload &key (fin t))
  "A client-to-server frame: masked, as RFC 6455 requires of a client.
   PAYLOAD is under 126 bytes, which is all these tests send."
  (let* ((bytes (if (stringp payload)
                    (sb-ext:string-to-octets payload :external-format :utf-8)
                    payload))
         (len (length bytes))
         (mask #(55 11 202 99))
         (out (make-array (+ 6 len) :element-type '(unsigned-byte 8))))
    (assert (< len 126))
    (setf (aref out 0) (logior (if fin #x80 0) opcode)
          (aref out 1) (logior #x80 len))
    (dotimes (i 4) (setf (aref out (+ 2 i)) (aref mask i)))
    (dotimes (i len)
      (setf (aref out (+ 6 i)) (logxor (aref bytes i) (aref mask (mod i 4)))))
    out))

(defun test-ws-send-refuses-a-closing-websocket ()
  "WS-SEND refuses a WebSocket whose close frame is queued, so no data
   frame follows a close frame onto the wire (RFC 6455 §5.5.1).

   The live shape is the one README's fan-out notes describe: an
   application keeps its own registry and prunes it on the refusal.
   :ON-TICK runs at the end of every pass, after the pass that queued the
   close echo and before the write that flushes it, so a broadcast from
   the hook meets the closing connection while it is still on the table.
   The client reads to the end of the connection, and any frame after
   the close frame is the failure. The ticks that arrive before the close
   are the control that the broadcast reaches an open connection."
  (format t "~%ws-send: a closing WebSocket takes no frame~%")
  (let ((conn (web-skeleton::make-connection :fd -1 :state :closing
                                             :last-active 0)))
    ;; On the message: fd -1 fails any write, so a raise alone could be
    ;; the flush failing after the frame was queued.
    (check "closing ws: ws-send refuses a :closing connection"
           (handler-case (progn (web-skeleton::ws-send
                                 conn (web-skeleton:build-ws-text "late"))
                                nil)
             (error (e) (and (search "not an open WebSocket" (princ-to-string e))
                             t)))
           t)
    (check "closing ws: and queues nothing"
           (web-skeleton::connection-write-pending conn) 0))
  (let ((registry nil))
    (with-test-server
        (:workers 1
         :handler (lambda (req) (declare (ignore req)) :upgrade)
         :ws-handler (lambda (conn frame)
                       (declare (ignore frame))
                       (pushnew conn registry)
                       nil)
         :on-tick (lambda (worker-id)
                    (declare (ignore worker-id))
                    (dolist (conn registry)
                      (handler-case
                          (web-skeleton::ws-send
                           conn (web-skeleton:build-ws-text "tick"))
                        (error ()
                          (setf registry (remove conn registry)))))))
      (flet ((frames (buf)
               ;; The opcodes of the whole frames in BUF, in order, and where
               ;; the last of them ends. Server frames are unmasked and these
               ;; are short, so a frame is two header bytes and its payload.
               (let ((pos 0) (fill (fill-pointer buf)) (opcodes nil))
                 (loop while (< (1+ pos) fill)
                       do (let ((len (logand (aref buf (1+ pos)) #x7F)))
                            (when (> (+ pos 2 len) fill) (return))
                            (push (logand (aref buf pos) #x0F) opcodes)
                            (incf pos (+ 2 len))))
                 (values (nreverse opcodes) pos))))
        (let* ((sock (connect-to-test-server))
               (stream (sb-bsd-sockets:socket-make-stream
                        sock :input t :output t :element-type '(unsigned-byte 8))))
          (unwind-protect
               (let ((buf (make-array 256 :element-type '(unsigned-byte 8)
                                          :fill-pointer 0 :adjustable t)))
                 (%ws-client-upgrade stream)
                 (write-sequence (%ws-client-frame 1 "join") stream)
                 (force-output stream)
                 (read-until-bounded stream :seconds 3 :into buf
                                            :until (lambda (b fill)
                                                     (declare (ignore b))
                                                     (>= fill 6)))
                 (check "closing ws: the broadcast reached the open connection"
                        (first (frames buf)) 1)
                 (write-sequence (%ws-client-frame 8 #(3 232)) stream)
                 (force-output stream)
                 (multiple-value-bind (all reason)
                     (read-until-bounded stream :seconds 3 :into buf)
                   (multiple-value-bind (opcodes end) (frames all)
                     (check "closing ws: the server's close frame arrived"
                            (and (member 8 opcodes) t) t)
                     (check "closing ws: no frame follows the close frame"
                            (list (rest (member 8 opcodes))
                                  (- (fill-pointer all) end))
                            (list nil 0))
                     (check "closing ws: and the server closed the connection"
                            reason :eof))))
            (ignore-errors (close stream))
            (ignore-errors (sb-bsd-sockets:socket-close sock))))))))

(defun test-ws-close ()
  "WS-CLOSE ends a WebSocket from the server's side: a close frame with the
   code asked for, then the connection goes. A peer at *MAX-WRITE-BACKLOG*
   has no room for the frame and is closed at once. An application whose
   own events outran a peer has this as its answer, the one DEPLOYMENT.md
   gives, where before it could only drop frames and keep the socket.

   Driven over a loopback pair, so the client reads the frame off a real
   socket and the close is the event loop's own write path finishing."
  (format t "~%ws-close: closing a WebSocket from the server's side~%")
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let* ((fd (web-skeleton::socket-fd server))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (conn (web-skeleton::make-connection
                         :fd fd :socket server :state :websocket
                         :last-active (get-universal-time))))
             (web-skeleton::set-nonblocking fd)
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd fd (logior web-skeleton::+epollin+
                                                      web-skeleton::+epollet+))
             (check "ws-close: answers T" (web-skeleton:ws-close conn 1013) t)
             (check "ws-close: the connection is closing"
                    (web-skeleton::connection-state conn) :closing)
             (check "ws-close: and takes no further frame"
                    (handler-case
                        (progn (web-skeleton:ws-send
                                conn (web-skeleton:build-ws-text "late"))
                               :sent)
                      (error () :refused))
                    :refused)
             (check "ws-close: a second close is refused, not doubled"
                    (handler-case (progn (web-skeleton:ws-close conn) :closed)
                      (error () :refused))
                    :refused)
             ;; The write path the arming hands it to: the frame goes out
             ;; and the connection with it.
             (web-skeleton::handle-client-write conn epfd)
             (check "ws-close: once written, the connection is gone"
                    (web-skeleton::lookup-connection fd) nil)
             (let ((buf (make-array 16 :element-type '(unsigned-byte 8))))
               (multiple-value-bind (b n) (sb-bsd-sockets:socket-receive client buf nil)
                 (declare (ignore b))
                 (check "ws-close: the client read a close frame with the code"
                        (list n (aref buf 0)
                              (logior (ash (aref buf 2) 8) (aref buf 3)))
                        (list 4 #x88 1013)))))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client)))))
  ;; At the bound: no room for the frame, so the connection goes at once.
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create)))
      (unwind-protect
           (let* ((fd (web-skeleton::socket-fd server))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (web-skeleton:*max-write-backlog* 64)
                  (conn (web-skeleton::make-connection
                         :fd fd :socket server :state :websocket
                         :last-active (get-universal-time))))
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd fd web-skeleton::+epollet+)
             (web-skeleton::connection-append-write
              conn (make-array 64 :element-type '(unsigned-byte 8)
                                  :initial-element 65))
             (web-skeleton:ws-close conn)
             (check "ws-close at the bound: closed at once"
                    (list (web-skeleton::lookup-connection fd)
                          (web-skeleton::connection-fd conn))
                    (list nil -1)))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client)))))
  ;; The same from a ws-handler, with a frame behind the one it closes on:
  ;; the read ends at the close, and nothing is armed on a descriptor that
  ;; is gone, which would fail and be logged as the server's own error.
  (multiple-value-bind (server client) (%loopback-pair)
    (let ((epfd (web-skeleton::epoll-create))
          (calls 0)
          (log (make-string-output-stream)))
      (unwind-protect
           (let* ((fd (web-skeleton::socket-fd server))
                  (web-skeleton::*connections* (make-hash-table :test #'eql))
                  (web-skeleton::*epoll-fd* epfd)
                  (web-skeleton:*max-write-backlog* 64)
                  (web-skeleton:*log-stream* log)
                  (conn (web-skeleton::make-connection
                         :fd fd :socket server :state :websocket
                         :last-active (get-universal-time)))
                  (frames (concatenate '(vector (unsigned-byte 8))
                                       (%ws-client-frame 1 "bye")
                                       (%ws-client-frame 1 "after"))))
             (web-skeleton::set-nonblocking fd)
             (web-skeleton::register-connection conn)
             (web-skeleton::epoll-add epfd fd (logior web-skeleton::+epollin+
                                                      web-skeleton::+epollet+))
             (web-skeleton::connection-append-write
              conn (make-array 64 :element-type '(unsigned-byte 8)
                                  :initial-element 65))
             (let ((out (sb-bsd-sockets:socket-make-stream
                         client :output t :element-type '(unsigned-byte 8))))
               (write-sequence frames out)
               (finish-output out))
             (%await-arrival fd :bytes (length frames))
             (web-skeleton::handle-client-read
              conn epfd nil
              (lambda (c frame)
                (declare (ignore frame))
                (incf calls)
                (web-skeleton:ws-close c)
                nil))
             (check "ws-close from a handler at the bound: closed, the handler called once"
                    (list (web-skeleton::lookup-connection fd)
                          (web-skeleton::connection-fd conn)
                          calls)
                    (list nil -1 1))
             (check "ws-close from a handler at the bound: and nothing logged as an error"
                    (search "error fd" (get-output-stream-string log))
                    nil))
        (ignore-errors (web-skeleton::%close epfd))
        (ignore-errors (sb-bsd-sockets:socket-close server))
        (ignore-errors (sb-bsd-sockets:socket-close client)))))
  ;; Off a worker there is no loop to carry the close out.
  (check "ws-close: refused off a worker"
         (let ((web-skeleton::*epoll-fd* nil))
           (handler-case
               (progn (web-skeleton:ws-close
                       (web-skeleton::make-connection :fd -1 :state :websocket))
                      :closed)
             (error () :refused)))
         :refused))

(defun test-ws-close-from-a-handler-e2e ()
  "A ws-handler that closes its own connection with WS-CLOSE ends the read
   it was called in. The client gets the close frame with the code asked
   for and then the end of the connection, and nothing else: not a reply
   to a frame that followed in the same read, not the pong to a ping
   there, not the reply the closing handler returned.

   Three connections, each sending its frames in one write. On the first,
   a later frame's handler would return its echo. On the second it would
   send the echo with WS-SEND, which refuses a closing connection, so the
   handler would raise, and the raise would close the connection without
   the close frame. On the third, the closing handler returns a reply of
   its own."
  (format t "~%ws-close from a ws-handler: nothing follows its close frame~%")
  (let ((lock (sb-thread:make-mutex :name "ws-close-handled"))
        (handled nil))
    (with-test-server
        (:workers 1
         :handler (lambda (req) (declare (ignore req)) :upgrade)
         :ws-handler (lambda (conn frame)
                       (let ((text (sb-ext:octets-to-string
                                    (web-skeleton::ws-frame-payload frame)
                                    :external-format :utf-8)))
                         (sb-thread:with-mutex (lock) (push text handled))
                         (cond
                           ((string= text "bye") (ws-close conn 1000) nil)
                           ((string= text "bye, with a reply")
                            (ws-close conn 1000)
                            (build-ws-text "late"))
                           ((string= text "send")
                            (ws-send conn (build-ws-text "echo"))
                            nil)
                           (t (build-ws-text "echo"))))))
      (flet ((exchange (&rest frames)
               ;; FRAMES in one write on a fresh WebSocket. Returns (values
               ;; OPCODES CODE LEFTOVER REASON HANDLED): the opcodes of the
               ;; frames read to the end of the connection, the close
               ;; frame's code, the bytes after the last whole frame, why the
               ;; read ended, and the messages the handler was called for.
               (sb-thread:with-mutex (lock) (setf handled nil))
               (let* ((sock (connect-to-test-server))
                      (stream (sb-bsd-sockets:socket-make-stream
                               sock :input t :output t
                                    :element-type '(unsigned-byte 8))))
                 (unwind-protect
                      (progn
                        (%ws-client-upgrade stream)
                        (write-sequence (apply #'concatenate
                                               '(vector (unsigned-byte 8)) frames)
                                        stream)
                        (force-output stream)
                        (multiple-value-bind (all reason)
                            (read-until-bounded stream :seconds 3)
                          (let ((pos 0) (fill (fill-pointer all))
                                (opcodes nil) (code nil))
                            ;; Server frames are unmasked, and these are short.
                            (loop while (< (1+ pos) fill)
                                  do (let ((op (logand (aref all pos) #x0F))
                                           (len (logand (aref all (1+ pos)) #x7F)))
                                       (when (> (+ pos 2 len) fill) (return))
                                       (push op opcodes)
                                       (when (and (= op 8) (>= len 2))
                                         (setf code (logior (ash (aref all (+ pos 2)) 8)
                                                            (aref all (+ pos 3)))))
                                       (incf pos (+ 2 len))))
                            (values (nreverse opcodes) code (- fill pos) reason
                                    (sb-thread:with-mutex (lock) (reverse handled))))))
                   (ignore-errors (close stream))
                   (ignore-errors (sb-bsd-sockets:socket-close sock))))))
        (check "a later frame's returned echo: the close frame, then the end, and nothing else"
               (multiple-value-list
                (exchange (%ws-client-frame 1 "bye") (%ws-client-frame 1 "after")
                          (%ws-client-frame 9 "p")))
               '((8) 1000 0 :eof ("bye")))
        (check "a later frame's echo by ws-send: the same, the close frame not lost"
               (multiple-value-list
                (exchange (%ws-client-frame 1 "bye") (%ws-client-frame 1 "send")
                          (%ws-client-frame 9 "p")))
               '((8) 1000 0 :eof ("bye")))
        (check "the closing handler's own reply is not sent either"
               (multiple-value-list
                (exchange (%ws-client-frame 1 "bye, with a reply")))
               '((8) 1000 0 :eof ("bye, with a reply")))))))

(defun test-ws-message-deadline-e2e ()
  "A peer that begins a fragmented message and never finishes it is closed
   with 1008 once *WS-MESSAGE-TIMEOUT* has passed, though the connection is
   otherwise healthy; a peer that finishes its message in time is answered
   and kept.

   *WS-MESSAGE-TIMEOUT* is 1 for the run. SETF rather than LET: the worker
   reads it, and dynamic bindings do not cross MAKE-THREAD."
  (format t "~%WebSocket: an unfinished message is closed in time~%")
  (let ((saved web-skeleton:*ws-message-timeout*)
        (lock (sb-thread:make-mutex :name "ws-deadline"))
        (received nil))
    (setf web-skeleton:*ws-message-timeout* 1)
    (unwind-protect
         (with-test-server
             (:handler (lambda (req) (declare (ignore req)) :upgrade)
              :ws-handler (lambda (conn frame)
                            (declare (ignore conn))
                            (sb-thread:with-mutex (lock)
                              (push (sb-ext:octets-to-string (ws-frame-payload frame)
                                                             :external-format :utf-8)
                                    received))
                            nil))
           (flet ((session (frames &key (pause 0) (seconds 4))
                    ;; Upgrade, send FRAMES with PAUSE seconds between them,
                    ;; and read what comes back for SECONDS. Answers
                    ;; (values BYTES REASON SECONDS-TAKEN).
                    (let* ((sock (connect-to-test-server))
                           (stream (sb-bsd-sockets:socket-make-stream
                                    sock :input t :output t
                                    :element-type '(unsigned-byte 8)))
                           (start (get-internal-real-time)))
                      (unwind-protect
                           (progn
                             (%ws-client-upgrade stream)
                             (dolist (frame frames)
                               (write-sequence frame stream)
                               (force-output stream)
                               (sleep pause))
                             (multiple-value-bind (buf reason)
                                 (read-until-bounded stream :seconds seconds)
                               (values (coerce buf 'list) reason
                                       (/ (float (- (get-internal-real-time) start))
                                          internal-time-units-per-second))))
                        (ignore-errors (close stream))
                        (ignore-errors (sb-bsd-sockets:socket-close sock))))))
             (multiple-value-bind (bytes reason secs)
                 (session (list (%ws-client-frame 1 "hel" :fin nil)))
               (check "unfinished: closed with 1008"
                      (list (first bytes) (and (cdddr bytes)
                                               (logior (ash (third bytes) 8) (fourth bytes)))
                            reason)
                      (list #x88 1008 :eof))
               (check "unfinished: once its time had passed" (< 0.9 secs 3.9) t))
             (multiple-value-bind (bytes reason)
                 (session (list (%ws-client-frame 1 "hel" :fin nil)
                                (%ws-client-frame 0 "lo"))
                          :pause 0.3 :seconds 2.5)
               (check "finished in time: answered, and kept open"
                      (list bytes reason (sb-thread:with-mutex (lock) received))
                      (list nil :deadline '("hello"))))))
      (setf web-skeleton:*ws-message-timeout* saved))))

(defun test-ws-upgrade-shapes-the-101 ()
  "WS-UPGRADE, WS-CHOOSE-PROTOCOL and WS-OFFERED-PROTOCOLS, and the 101
   CLASSIFY-RESPONSE makes of a WS-UPGRADE: the chosen subprotocol and the
   application's headers on it, a subprotocol the client did not offer
   refused, the handshake's own fields refused at WS-UPGRADE, and a request
   that is not an upgrade answered 400 whatever the handler returned."
  (format t "~%WebSocket: a handler shapes the 101~%")
  (flet ((handshake (&rest extra)
           (make-test-request
            :path "/ws"
            :headers (append '(("Upgrade" . "websocket") ("Connection" . "Upgrade")
                               ("Sec-WebSocket-Version" . "13")
                               ("Sec-WebSocket-Key" . "dGhlIHNhbXBsZSBub25jZQ=="))
                             extra)))
         (refused (&rest args)
           (handler-case (progn (apply #'ws-upgrade args) :accepted)
             (error (e) (princ-to-string e))))
         (header (response name)
           (cdr (assoc name (http-response-headers response) :test #'string-equal))))
    (let ((offered (handshake '("Sec-WebSocket-Protocol" . "chat, superchat")
                              '("Sec-WebSocket-Protocol" . " v2.json ,"))))
      (check "offered: every field, split on commas and trimmed, in order"
             (ws-offered-protocols offered) '("chat" "superchat" "v2.json"))
      (check "choose: the first of ours the client offered, in our order"
             (ws-choose-protocol offered '("superchat" "chat")) "superchat")
      (check "choose: names compare exactly"
             (ws-choose-protocol offered '("Chat")) nil)
      (check "choose: none in common is NIL"
             (ws-choose-protocol (handshake) '("chat")) nil)
      (multiple-value-bind (response upgrade-p)
          (web-skeleton::classify-response
           offered (ws-upgrade :protocol "superchat"
                               :headers '(("Set-Cookie" . "a=1") ("Set-Cookie" . "b=2")
                                          ("X-Trace" . "t-1"))))
        (check "101: an upgrade, with the subprotocol chosen"
               (list upgrade-p (http-response-status response)
                     (header response "sec-websocket-protocol")
                     (and (header response "sec-websocket-accept") t))
               '(t 101 "superchat" t))
        (check "101: and the application's headers, a repeated name kept twice"
               (list (count "set-cookie" (http-response-headers response)
                            :key #'car :test #'string-equal)
                     (header response "x-trace"))
               '(2 "t-1")))
      (check "101: a plain :upgrade names no subprotocol"
             (header (web-skeleton::classify-response offered :upgrade)
                     "sec-websocket-protocol")
             nil)
      (check "a subprotocol the client did not offer is refused, by name"
             (handler-case (progn (web-skeleton::classify-response
                                   offered (ws-upgrade :protocol "mqtt"))
                                  :accepted)
               (error (e) (and (search "\"mqtt\"" (princ-to-string e)) :refused)))
             :refused)
      (check "and so is any subprotocol when the client offered none"
             (handler-case (progn (web-skeleton::classify-response
                                   (handshake) (ws-upgrade :protocol "chat"))
                                  :accepted)
               (error () :refused))
             :refused))
    (check "a request that is not an upgrade is answered 400 all the same"
           (multiple-value-list
            (let ((r (web-skeleton::classify-response
                      (make-test-request :path "/ws") (ws-upgrade :protocol "chat"))))
              (http-response-status r)))
           '(400))
    (dolist (name '("Upgrade" "connection" "Sec-WebSocket-Accept" "sec-websocket-protocol"
                    "Sec-WebSocket-Extensions" "Content-Length" "transfer-encoding"))
      (check (format nil "ws-upgrade: ~a is the handshake's, refused" name)
             (let ((why (refused :headers (list (cons name "x")))))
               (and (stringp why) (search (string-downcase name) (string-downcase why)) t))
             t))
    (check "ws-upgrade: a header that is not a pair of strings is refused"
           (list (stringp (refused :headers '(("x-a" . 1))))
                 (stringp (refused :headers '("x-a")))
                 (stringp (refused :protocol 'chat)))
           '(t t t))
    (check "ws-upgrade: with nothing given, it is a plain upgrade"
           (multiple-value-bind (response upgrade-p)
               (web-skeleton::classify-response (handshake) (ws-upgrade))
             (list upgrade-p (http-response-status response)
                   (header response "sec-websocket-protocol")))
           '(t 101 nil))))

(defun test-ws-upgrade-subprotocol-e2e ()
  "A browser-shaped handshake offering subprotocols, answered live: the
   101 names the one chosen and carries the handler's header, and the
   WebSocket then works as any other."
  (format t "~%WebSocket: a subprotocol chosen, end to end~%")
  (with-test-server
      (:handler (lambda (req)
                  (let ((p (ws-choose-protocol req '("superchat" "chat"))))
                    (if p
                        (ws-upgrade :protocol p :headers '(("x-trace" . "t-9")))
                        (make-error-response 400))))
       :ws-handler (lambda (conn frame)
                     (declare (ignore conn))
                     (build-ws-text (format nil "echo:~a"
                                            (sb-ext:octets-to-string
                                             (ws-frame-payload frame)
                                             :external-format :utf-8)))))
    (let* ((sock (connect-to-test-server))
           (stream (sb-bsd-sockets:socket-make-stream
                    sock :input t :output t :element-type '(unsigned-byte 8))))
      (unwind-protect
           (progn
             (write-sequence (sb-ext:string-to-octets
                              (crlf "GET /ws HTTP/1.1" "Host: localhost"
                                    "Upgrade: websocket" "Connection: Upgrade"
                                    "Sec-WebSocket-Version: 13"
                                    "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ=="
                                    "Sec-WebSocket-Protocol: chat, superchat")
                              :external-format :ascii)
                             stream)
             (force-output stream)
             (let ((head (sb-ext:octets-to-string
                          (read-until-bounded
                           stream :seconds 3
                           :until (lambda (buf fill)
                                    (and (>= fill 4)
                                         (= (aref buf (- fill 4)) 13) (= (aref buf (- fill 3)) 10)
                                         (= (aref buf (- fill 2)) 13) (= (aref buf (- fill 1)) 10))))
                          :external-format :latin-1)))
               (check "e2e: 101, naming the subprotocol the server preferred"
                      (list (and (search "HTTP/1.1 101" head) t)
                            (and (search "sec-websocket-protocol: superchat" head
                                         :test #'char-equal)
                                 t)
                            (and (search "x-trace: t-9" head :test #'char-equal) t))
                      '(t t t)))
             (write-sequence (%ws-client-frame 1 "hi") stream)
             (force-output stream)
             (let ((reply (read-until-bounded stream :seconds 3
                                              :until (lambda (buf fill)
                                                       (declare (ignore buf))
                                                       (>= fill 9)))))
               (check "e2e: and the WebSocket answers as any other"
                      (sb-ext:octets-to-string (subseq reply 2 (fill-pointer reply))
                                               :external-format :latin-1)
                      "echo:hi")))
        (ignore-errors (close stream))
        (ignore-errors (sb-bsd-sockets:socket-close sock))))))

(defun test-ws-handler-reply-of-the-wrong-type ()
  "A ws-handler that returns something other than a frame's bytes — a
   string, the commonest slip — has its connection closed, however many
   frames shared the read, and nothing is written onto the WebSocket: no
   HTTP 500, which a client would read as frame bytes.

   CHECK-WS-REPLY refuses the reply where it is received, inside the catch
   that closes the WebSocket, with a message naming the fix. The two-reply
   session asserts the batch shape ends the same way, and the
   frame-returning session is the control that the check refuses only the
   wrong type."
  (format t "~%ws-handler: a reply that is not a frame~%")
  (with-test-server
      (:handler (lambda (req) (declare (ignore req)) :upgrade)
       :ws-handler (lambda (conn frame)
                     (declare (ignore conn))
                     (if (string= (sb-ext:octets-to-string
                                   (web-skeleton:ws-frame-payload frame)
                                   :external-format :utf-8)
                                  "frame")
                         (web-skeleton:build-ws-text "a frame")
                         "a string, not a frame")))
    (flet ((session (texts &key until)
             ;; Upgrade, send every text frame in one write so they share a
             ;; read, and return (REASON . TEXT) of what came back.
             (let* ((sock (connect-to-test-server))
                    (stream (sb-bsd-sockets:socket-make-stream
                             sock :input t :output t
                             :element-type '(unsigned-byte 8))))
               (unwind-protect
                    (progn
                      (%ws-client-upgrade stream)
                      (write-sequence
                       (apply #'concatenate '(vector (unsigned-byte 8))
                              (mapcar (lambda (s) (%ws-client-frame 1 s)) texts))
                       stream)
                      (force-output stream)
                      (multiple-value-bind (buf reason)
                          (read-until-bounded stream :seconds 3 :until until)
                        (cons reason (sb-ext:octets-to-string
                                      buf :external-format :latin-1))))
                 (ignore-errors (close stream))
                 (ignore-errors (sb-bsd-sockets:socket-close sock))))))
      (let ((one (session '("string"))))
        (check "ws reply: a string reply closes the connection, unanswered"
               one '(:eof . ""))
        (check "ws reply: and no HTTP response is written onto the WebSocket"
               (search "HTTP/1.1" (cdr one)) nil))
      (check "ws reply: two string replies in one read end the same way"
             (session '("string" "string"))
             '(:eof . ""))
      (check "ws reply: a frame reply still arrives"
             (car (session '("frame")
                           :until (lambda (buf fill)
                                    (declare (ignore buf))
                                    (>= fill 9))))
             :satisfied))))

(defun test-ws-replies-keep-the-order-they-were-made-in ()
  "A reply a ws-handler returns and a frame it pushes with WS-SEND reach
   the peer in the order they were produced, however the messages that
   prompted them were segmented.

   The handler returns its reply to \"first\" and pushes its reply to
   \"second\". Sent in separate writes the two come back in order however
   replies are queued. Sent in one write, a returned reply held to the end
   of the batch while the pushed one goes out at once would reach the peer
   reversed. Both shapes are asserted, the separate writes as the control."
  (format t "~%ws: replies keep the order they were made in~%")
  (with-test-server
      (:handler (lambda (req) (declare (ignore req)) :upgrade)
       :ws-handler (lambda (conn frame)
                     (let ((text (sb-ext:octets-to-string
                                  (ws-frame-payload frame) :external-format :utf-8)))
                       (if (string= text "first")
                           (build-ws-text "reply-to-first")
                           (progn (ws-send conn (build-ws-text "reply-to-second"))
                                  nil)))))
    (flet ((replies (writes)
             ;; Send WRITES, each a list of texts going out in one write, and
             ;; return the texts of the first two frames that come back.
             (let* ((sock (connect-to-test-server))
                    (stream (sb-bsd-sockets:socket-make-stream
                             sock :input t :output t
                             :element-type '(unsigned-byte 8))))
               (unwind-protect
                    (progn
                      (%ws-client-upgrade stream)
                      (dolist (texts writes)
                        (write-sequence
                         (apply #'concatenate '(vector (unsigned-byte 8))
                                (mapcar (lambda (s) (%ws-client-frame 1 s)) texts))
                         stream)
                        (force-output stream)
                        (sleep 0.2))
                      ;; Server frames are unmasked; these replies are short,
                      ;; so a frame is two header bytes and its payload.
                      (let ((buf (read-until-bounded
                                  stream :seconds 3
                                  :until (lambda (buf fill)
                                           (and (>= fill 2)
                                                (let ((second (+ 2 (aref buf 1))))
                                                  (and (>= fill (+ second 2))
                                                       (>= fill (+ second 2
                                                                   (aref buf (1+ second)))))))))))
                        (loop with pos = 0
                              repeat 2
                              while (< (1+ pos) (fill-pointer buf))
                              collect (let ((len (aref buf (1+ pos))))
                                        (prog1 (sb-ext:octets-to-string
                                                (subseq buf (+ pos 2) (+ pos 2 len))
                                                :external-format :latin-1)
                                          (incf pos (+ 2 len)))))))
                 (ignore-errors (close stream))
                 (ignore-errors (sb-bsd-sockets:socket-close sock))))))
      (check "ws order: in separate writes, as produced"
             (replies '(("first") ("second")))
             '("reply-to-first" "reply-to-second"))
      (check "ws order: in one write, as produced too"
             (replies '(("first" "second")))
             '("reply-to-first" "reply-to-second")))))

(defun test-counters-count-every-way-a-frame-is-handed-over ()
  "A frame is counted however the application hands it over, and a frame the
   framework sends on its own is not.

   Three call sites and one exclusion, each asserted as a delta so a revert of
   any one site reddens its own line: WS-SEND, a ws-handler's reply to a
   single frame, and its reply to a fragmented message, which completes on a
   different path. A test that called NOTE-WS-FRAME directly would pass with
   every call site deleted.

   The last delta sends a ping beside a reply. The framework answers the ping
   with a pong of its own, which is not the application's traffic and must not
   move the count — but a check that the count did not move passes on a census
   that never published. So the reply rides along as the control: the delta is
   exactly one, which is the reply counted and the pong not."
  (format t "~%counters: every way a frame is handed over~%")
  (with-test-server
      (:workers 1
       :handler (lambda (req)
                  (declare (ignore req))
                  :upgrade)
       :ws-handler (lambda (conn frame)
                     (let ((text (sb-ext:octets-to-string
                                  (web-skeleton:ws-frame-payload frame)
                                  :external-format :utf-8)))
                       (cond ((string= text "send")
                              (web-skeleton::ws-send
                               conn (web-skeleton:build-ws-text "sent"))
                              nil)
                             ((string= text "reply")
                              (web-skeleton:build-ws-text "replied"))
                             (t nil)))))
    (let* ((sock (connect-to-test-server))
           (stream (sb-bsd-sockets:socket-make-stream
                    sock :input t :output t :element-type '(unsigned-byte 8))))
      (unwind-protect
           (flet ((put (bytes) (write-sequence bytes stream) (force-output stream))
                  (frames ()
                    (getf (getf (web-skeleton:connection-census) :counters)
                          :ws-frames 0)))
             (flet ((frames-after (previous)
                      ;; Polled: the census publishes at 1 Hz, and a site
                      ;; that was reverted never moves, so the wait ends on
                      ;; the deadline and the delta reads zero.
                      (let ((deadline (+ (get-universal-time) 4))
                            (n (frames)))
                        (loop until (or (> n previous)
                                        (> (get-universal-time) deadline))
                              do (sleep 0.05) (setf n (frames)))
                        n)))
               ;; The control for all of it: without an upgrade there are
               ;; no frames, and every delta below would read as a revert.
               (check "control: the upgrade was accepted"
                      (not (null (search "101" (%ws-client-upgrade stream))))
                      t)
               (let ((deadline (+ (get-universal-time) 4)))
                 (loop until (or (web-skeleton:connection-census)
                                 (> (get-universal-time) deadline))
                       do (sleep 0.05)))
               (let* ((b0 (frames))
                      (n1 (progn (put (%ws-client-frame 1 "send"))
                                 (frames-after b0)))
                      (n2 (progn (put (%ws-client-frame 1 "reply"))
                                 (frames-after n1)))
                      (n3 (progn (put (%ws-client-frame 1 "rep" :fin nil))
                                 (put (%ws-client-frame 0 "ly"))
                                 (frames-after n2)))
                      (n4 (progn (put (%ws-client-frame 9 "are you there"))
                                 (put (%ws-client-frame 1 "reply"))
                                 (frames-after n3))))
                 (check "a frame handed over with ws-send is counted"
                        (- n1 b0) 1)
                 (check "a ws-handler's reply to a frame is counted"
                        (- n2 n1) 1)
                 (check "and its reply to a fragmented message"
                        (- n3 n2) 1)
                 (check "the pong the framework sends is not, the reply is"
                        (- n4 n3) 1))))
        (ignore-errors (close stream))
        (ignore-errors (sb-bsd-sockets:socket-close sock))))))

(defun test-counters-a-client-reset-is-not-a-server-error ()
  "A client that resets its connection is closed, not answered with a 500 that
   is then counted against the server.

   A read of a reset socket fails with ECONNRESET, and HANDLE-CLIENT-READ
   answered anything that escaped its dispatch with a 500 built through
   FORMAT-RESPONSE. The 500 was always addressed to nobody. Counting it made a
   client's disconnect a :SERVER-ERROR, in the block the census calls safe to
   alert on, and clients reset all the time.

   Three resets, from the three places a client can be when it goes: idle
   before sending a byte, part way through a request, and upgraded to a
   WebSocket.

   Two controls. The log shows the server read each reset as ECONNRESET, not
   as an end of stream; without that, a close that sent a FIN would pass here
   on code that counts resets. And a handler that raises still gets its
   500, counted, so what goes uncounted is the client's own descriptor
   failing and not every error."
  (format t "~%counters: a client's reset is not a server error~%")
  (let ((log (make-string-output-stream))
        (so-far "")
        (saved-stream web-skeleton:*log-stream*)
        (saved-level web-skeleton:*log-level*))
    ;; Global SETF, as the restart test does: the worker thread reads the
    ;; global values, so a binding here would capture nothing.
    (setf web-skeleton:*log-stream* log
          web-skeleton:*log-level* :debug)
    (unwind-protect
         (with-test-server
             (:handler (lambda (req)
                         (let ((path (web-skeleton:http-request-path req)))
                           (cond ((string= path "/ws") :upgrade)
                                 ((string= path "/boom")
                                  (error "the handler failed on purpose"))
                                 (t (make-text-response 200 "ok")))))
              :ws-handler (lambda (conn frame) (declare (ignore conn frame)) nil))
           (flet ((resets-read ()
                    ;; Drained under the lock the worker holds while writing,
                    ;; and kept, because draining a string stream empties it.
                    (setf so-far
                          (concatenate 'string so-far
                                       (sb-thread:with-mutex
                                           (web-skeleton::*log-lock*)
                                         (get-output-stream-string log))))
                    (loop with start = 0
                          for at = (search "ECONNRESET" so-far :start2 start)
                          while at
                          count t
                          do (setf start (1+ at)))))
             (let* ((idle (connect-to-test-server))
                    (partial (connect-to-test-server))
                    (ws (connect-to-test-server))
                    (partial-stream (sb-bsd-sockets:socket-make-stream
                                     partial :output t
                                     :element-type '(unsigned-byte 8)))
                    (ws-stream (sb-bsd-sockets:socket-make-stream
                                ws :input t :output t
                                :element-type '(unsigned-byte 8))))
               ;; A request line and one header, and no blank line to end them.
               (write-sequence (sb-ext:string-to-octets
                                (format nil "GET / HTTP/1.1~c~cHost: localhost~c~c"
                                        #\Return #\Linefeed #\Return #\Linefeed)
                                :external-format :ascii)
                               partial-stream)
               (force-output partial-stream)
               (check "control: the WebSocket was upgraded before it reset"
                      (not (null (search "101" (%ws-client-upgrade ws-stream))))
                      t)
               ;; Long enough for the worker to accept the other two and take
               ;; the partial request into its buffer before anything resets.
               (sleep 0.3)
               (mapc #'%reset-socket (list idle partial ws)))
             (let ((deadline (+ (get-universal-time) 6)))
               (loop until (or (>= (resets-read) 3)
                               (> (get-universal-time) deadline))
                     do (sleep 0.05)))
             (check "control: the server read all three resets as ECONNRESET"
                    (>= (resets-read) 3) t)
             (check "control: a handler that raises still answers 500"
                    (test-http-request :get "/boom") 500)
             ;; Polled until that 500 is published and every connection has
             ;; gone, so the count read is one taken after all four were seen.
             (let ((census nil) (deadline (+ (get-universal-time) 6)))
               (loop until (or (and census
                                    (>= (getf (getf census :counters)
                                              :server-error 0)
                                        1)
                                    (zerop (getf census :total 1)))
                               (> (get-universal-time) deadline))
                     do (sleep 0.05)
                        (setf census (web-skeleton:connection-census)))
               (let ((errors (getf (getf census :counters) :server-error 0)))
                 (check "control: that 500 is counted" (>= errors 1) t)
                 (check "and the three resets added nothing to it" errors 1)))))
      (setf web-skeleton:*log-stream* saved-stream
            web-skeleton:*log-level* saved-level))))

(defun test-counters-count-a-response-the-serializer-never-built ()
  "A cached file is a response, and it was not being counted.

   FORMAT-RESPONSE is where a per-request response is built, so counting
   there caught everything that passes through it. Static files do not pass
   through it: their bytes are serialized once at startup and sent from the
   cache thereafter. Static files are also most of what a page load asks
   for, so the census reported one response for a page that made a dozen —
   wrong by an order of magnitude, in the one number a reader would check.

   The first assertion is a control. A per-request response counted before
   this fix and counts after it, so a run where the cached paths also count
   is telling us about the cache rather than about the harness."
  (format t "~%counters: a cached file is a response~%")
  (let* ((content (sb-ext:string-to-octets
                   "0123456789abcdefghijklmnopqrstuvwxyz"
                   :external-format :ascii))   ; 36 bytes
         (entry (web-skeleton::build-static-response
                 "text/plain; charset=utf-8" content 0))
         (saved web-skeleton::*static-cache*))
    (unwind-protect
         (progn
           (setf web-skeleton::*static-cache* (make-hash-table :test #'equal))
           (setf (gethash "/data.txt" web-skeleton::*static-cache*) entry)
           (flet ((counted (thunk)
                    ;; A fresh set per case, so each number below is that
                    ;; case's own and not a running total someone has to
                    ;; subtract in their head to read.
                    (let ((web-skeleton::*counters* (web-skeleton::make-counters)))
                      (funcall thunk)
                      (web-skeleton::counters-snapshot)))
                  (get* (&rest headers)
                    (serve-static (make-test-request :method :GET
                                                     :path "/data.txt"
                                                     :headers headers))))
             (let ((snap (counted
                          (lambda ()
                            (web-skeleton::format-response
                             (web-skeleton::make-http-response :status 200))))))
               (check "control: a per-request response counts"
                      (list (getf snap :responses) (getf snap :successful))
                      '(1 1)))
             (let ((snap (counted (lambda () (get*)))))
               (check "a cached 200 is one response"
                      (list (getf snap :responses) (getf snap :successful))
                      '(1 1)))
             (let ((snap (counted
                          (lambda ()
                            (serve-static (make-test-request :method :HEAD
                                                             :path "/data.txt"))))))
               (check "a cached HEAD is one response"
                      (list (getf snap :responses) (getf snap :successful))
                      '(1 1)))
             ;; 304 and 416 are the two a reader is most likely to want and
             ;; least likely to generate on purpose: a browser revalidating,
             ;; and a client asking for bytes that are not there.
             (let ((snap (counted
                          (lambda ()
                            (get* (cons "if-none-match"
                                        (web-skeleton::static-entry-etag entry)))))))
               (check "a cached 304 is one response, counted 3xx"
                      (list (getf snap :responses) (getf snap :redirected))
                      '(1 1)))
             (let ((snap (counted (lambda () (get* (cons "range" "bytes=10-19"))))))
               (check "a 206 is one response, counted 2xx"
                      (list (getf snap :responses) (getf snap :successful))
                      '(1 1)))
             (let ((snap (counted (lambda () (get* (cons "range" "bytes=900-999"))))))
               (check "a 416 is one response, counted 4xx"
                      (list (getf snap :responses) (getf snap :client-error))
                      '(1 1)))))
      (setf web-skeleton::*static-cache* saved)))
  ;; A response the serializer refuses is not counted — and neither the
  ;; original nor the 500 that replaces it is counted twice. A count taken
  ;; before the serialize that can reject the headers would count a phantom
  ;; redirect for a 302 whose Location carried a CRLF, one the client never
  ;; saw, on top of the 500 it did. The three assertions pin it: the
  ;; serializer refuses, the refused response counts nothing, and the same
  ;; 302 without the CRLF still counts once — so the count can move neither
  ;; before the serialize (the refused one would count) nor out of reach
  ;; of a good response (the good one would not).
  (let ((web-skeleton::*counters* (web-skeleton::make-counters)))
    (flet ((crlf-302 ()
             (let ((r (web-skeleton::make-http-response :status 302)))
               (web-skeleton::set-response-header
                r "location" (format nil "/a~c~c/b" #\Return #\Linefeed))
               r)))
      (check "the serializer refuses a CRLF smuggled into a header value"
             (handler-case (progn (web-skeleton::format-response (crlf-302))
                                  :built)
               (error () :refused))
             :refused)
      (let ((snap (web-skeleton::counters-snapshot)))
        (check "a response the serializer refused is counted nowhere"
               (list (getf snap :responses)
                     (getf snap :redirected)
                     (getf snap :server-error))
               '(0 0 0)))))
  (let ((web-skeleton::*counters* (web-skeleton::make-counters)))
    (web-skeleton::format-response
     (web-skeleton::make-http-response :status 302))
    (let ((snap (web-skeleton::counters-snapshot)))
      (check "control: the same 302, serialized, counts once as a redirect"
             (list (getf snap :responses) (getf snap :redirected))
             '(1 1))))
  (let ((web-skeleton::*counters* (web-skeleton::make-counters)))
    (flet ((crlf-head ()
             (let ((r (web-skeleton::make-http-response :status 200)))
               (web-skeleton::set-response-header
                r "x-note" (format nil "a~cb" #\Return))
               r)))
      (ignore-errors
       (web-skeleton::format-streaming-head (crlf-head) :chunked))
      (check "a streamed head the serializer refused is not counted"
             (getf (web-skeleton::counters-snapshot) :responses)
             0)))
  ;; The streamed head is the fourth path that never reaches FORMAT-RESPONSE.
  ;; One response, counted once, at the head — the chunks after it are not
  ;; responses and STREAM-SEND does not count them.
  (let ((web-skeleton::*counters* (web-skeleton::make-counters)))
    (web-skeleton::format-streaming-head
     (web-skeleton::make-http-response :status 200) :chunked)
    (let ((snap (web-skeleton::counters-snapshot)))
      (check "a streamed response is counted once, at its head"
             (list (getf snap :responses) (getf snap :successful))
             '(1 1)))))

(defun test-getent-parse-separates-policy-from-resolution ()
  "A name whose addresses policy refused is distinguishable from one that
   did not resolve.

   Both still return NIL, and both still answer the client 502: which of the
   two happened is not the client's business. An operator is a different
   audience. Told `DNS lookup failed` when getent answered perfectly well and
   their own filter said no, they go and debug a resolver.

   The two controls are the point of the test. Without them a second value
   that always returned zero, or always returned one, would pass whichever
   single case was asserted."
  (format t "~%dns: a refusal is not a resolution failure~%")
  (let ((line (sb-ext:string-to-octets
               (format nil "127.0.0.1 STREAM localhost~%")
               :external-format :ascii))
        (web-skeleton:*log-stream* (make-broadcast-stream)))
    (let ((web-skeleton:*fetch-address-filter* nil))
      (multiple-value-bind (parsed refused)
          (web-skeleton::parse-getent-output line (length line) "localhost")
        (check "control: with no filter the address is chosen"
               (and (consp parsed) (equalp (car parsed) #(127 0 0 1)) t) t)
        (check "control: and nothing was refused" refused 0)))
    (let ((web-skeleton:*fetch-address-filter*
            (lambda (ip family host)
              (declare (ignore ip family host))
              nil)))
      (multiple-value-bind (parsed refused)
          (web-skeleton::parse-getent-output line (length line) "localhost")
        (check "a refused address still yields no answer" parsed nil)
        (check "but the caller can see policy refused one" refused 1)))
    (let ((nothing (sb-ext:string-to-octets (format nil "~%")
                                            :external-format :ascii)))
      (multiple-value-bind (parsed refused)
          (web-skeleton::parse-getent-output nothing (length nothing) "nowhere")
        (check "control: nothing to refuse when nothing resolved"
               (list parsed refused) (list nil 0))))))

(defun test-parse-error-carries-its-status-to-a-caller ()
  "An application that catches HTTP-PARSE-ERROR can ask what it carries.

   The condition was exported and its readers were not, which gives a caller
   something it can catch and cannot interrogate. A handler wanting to log
   that a request was refused with 505 rather than 400 had to re-derive the
   status by reading the printed message, which is a parser of a parser.

   Reached through the WEB-SKELETON package rather than WEB-SKELETON:: on
   purpose: the whole assertion is that these are public."
  (format t "~%http-parse-error: status and message reach a caller~%")
  (flet ((bad (version)
           (format nil "GET /x HTTP/~a~c~cHost: h~c~c~c~c"
                   version #\Return #\Newline #\Return #\Newline
                   #\Return #\Newline)))
    (check "the status is readable"
           (handler-case (web-skeleton:parse-request (bad "9.9"))
             (web-skeleton:http-parse-error (e)
               (web-skeleton:http-parse-error-status e)))
           505)
    (check "the message is readable without parsing the printed form"
           (handler-case (web-skeleton:parse-request (bad "9.9"))
             (web-skeleton:http-parse-error (e)
               (and (stringp (web-skeleton:http-parse-error-message e))
                    (search "version" (web-skeleton:http-parse-error-message e))
                    t)))
           t)
    ;; Control: a well-formed request does not signal, so the two assertions
    ;; above are reading a condition that was raised rather than one the test
    ;; manufactured.
    (check "control: a well-formed request parses"
           (handler-case (progn (web-skeleton:parse-request (bad "1.1")) :parsed)
             (web-skeleton:http-parse-error () :refused))
           :parsed)))

;;; ---------------------------------------------------------------------------
;;; cpu-count
;;;
;;; Driven with synthetic file contents rather than the live filesystem,
;;; because a host that exposes cgroup CPU files cannot be relied on. CI does
;;; not, so without this every branch except the final fall-through, the one
;;; an ordinary host takes, would go unexercised.
;;; ---------------------------------------------------------------------------

(defun test-cpu-count-parsers ()
  (format t "~%cpu-count: quota and topology parsing~%")

  ;; cgroup v2. The unlimited form's first field is the literal string
  ;; `max`, not a number: parsed as an integer it raises, and a parser that
  ;; guessed would answer with whatever it made of the word. It must decline
  ;; so the chain moves on.
  (check "cpu.max: unlimited declines"
         (web-skeleton::parse-cpu-max "max 100000") nil)
  (check "cpu.max: one full CPU"
         (web-skeleton::parse-cpu-max "100000 100000") 1)
  (check "cpu.max: four CPUs"
         (web-skeleton::parse-cpu-max "400000 100000") 4)
  ;; The shape the quota reading exists for. FLOOR gives 0 here, and
  ;; START-SERVER refuses a non-positive :WORKERS — so rounding down would
  ;; give a server that will not boot in the deployment it serves.
  (check "cpu.max: half a CPU still gets one worker"
         (web-skeleton::parse-cpu-max "50000 100000") 1)
  (check "cpu.max: 2.5 CPUs rounds up"
         (web-skeleton::parse-cpu-max "250000 100000") 3)
  (check "cpu.max: trailing newline tolerated"
         (web-skeleton::parse-cpu-max (format nil "200000 100000~c" #\Newline)) 2)
  (check "cpu.max: garbage declines"
         (web-skeleton::parse-cpu-max "not a quota") nil)
  (check "cpu.max: single field declines"
         (web-skeleton::parse-cpu-max "100000") nil)
  (check "cpu.max: zero period declines"
         (web-skeleton::parse-cpu-max "100000 0") nil)
  (check "cpu.max: NIL line declines"
         (web-skeleton::parse-cpu-max nil) nil)

  ;; cgroup v1. Unlimited is a quota of -1 rather than a word.
  (check "cfs: unlimited declines"
         (web-skeleton::parse-cfs-quota "-1" "100000") nil)
  (check "cfs: two CPUs"
         (web-skeleton::parse-cfs-quota "200000" "100000") 2)
  (check "cfs: half a CPU still gets one worker"
         (web-skeleton::parse-cfs-quota "50000" "100000") 1)
  (check "cfs: a missing file declines"
         (web-skeleton::parse-cfs-quota nil "100000") nil)

  ;; Topology, the last answer in the chain and the only one CI reaches.
  (check "cpu list: simple range"
         (web-skeleton::parse-cpu-list "0-15") 16)
  (check "cpu list: single cpu"
         (web-skeleton::parse-cpu-list "0") 1)
  (check "cpu list: multi-range with a bare cpu"
         (web-skeleton::parse-cpu-list "0-3,8,12-13") 7)
  (check "cpu list: garbage declines"
         (web-skeleton::parse-cpu-list "nonsense") nil)
  (check "cpu list: empty declines"
         (web-skeleton::parse-cpu-list "") nil)

  ;; Affinity is a syscall and cannot be synthesised, so its arithmetic is
  ;; tested where it lives: the popcount over a filled cpu_set_t.
  (check "affinity: bits counted across bytes"
         (web-skeleton::count-set-bits #(#xFF #x0F #x00)) 12)
  (check "affinity: an empty mask counts nothing"
         (web-skeleton::count-set-bits #(0 0 0 0)) 0)

  ;; And the live call, which on any Linux host must answer something
  ;; positive. This is the one arm that runs against the real machine.
  (let ((n (web-skeleton::affinity-cpu-count)))
    (check "affinity: the live call answers a positive count"
           (and (integerp n) (plusp n)) t))

  ;; The least of the sources, not the first. These are the assertions that
  ;; distinguish the two: no host either of us can test on will disagree
  ;; with itself, so on every real machine MIN and first-answer-wins return
  ;; the same number and are indistinguishable.
  (check "min: a quota of eight against an affinity of two answers two"
         (web-skeleton::fewest-cpus
          (web-skeleton::parse-cpu-max "800000 100000") 2)
         2)
  (check "min: an affinity of sixteen against a quota of two answers two"
         (web-skeleton::fewest-cpus
          (web-skeleton::parse-cpu-max "200000 100000") 16)
         2)
  (check "min: a silent quota leaves affinity to answer"
         (web-skeleton::fewest-cpus
          (web-skeleton::parse-cpu-max "max 100000") 16)
         16)
  (check "min: every source silent declines"
         (web-skeleton::fewest-cpus nil nil nil) nil)
  (check "min: a non-positive source is ignored rather than winning"
         (web-skeleton::fewest-cpus 0 -3 4) 4)

  ;; The whole thing, on whatever this host is. Cannot assert a number,
  ;; which is the host's, but it must always be usable, since START-SERVER
  ;; refuses anything else.
  (let ((n (web-skeleton::cpu-count)))
    (check "cpu-count: answers a positive integer"
           (and (integerp n) (plusp n)) t)))

;;; ---------------------------------------------------------------------------
;;; Runner
;;; ---------------------------------------------------------------------------

(defun test-connections ()
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
  (format t "~%=== Server Tests, continued ===~%")
  (test-transfer-encoding-rules)
  (test-chunked-request-body)
  (test-body-handler-at-the-head)
  (test-body-decision-rereads-under-its-cap)
  (test-received-body-window-and-yield)
  (test-body-resume-and-refuse-contract)
  (test-received-body-pause-within-a-run)
  (test-received-chunked-body-one-run-per-read)
  (test-multipart-content-type)
  (test-multipart-parse)
  (test-receive-multipart-contract)
  (test-receive-multipart-pauses-where-it-falls)
  (test-received-form-holds-its-bytes)
  (test-multipart-parameters-in-linear-time)
  (test-streamed-fetch-contract)
  (test-streamed-fetch-sweep)
  (test-sweep-acts-only-on-what-is-still-due)
  (test-unfinishable-request-closes)
  (test-declared-body-is-not-preallocated)
  (test-unterminated-head-refused-at-its-budget)
  (test-head-search-resumes)
  (test-a-head-with-no-fields)
  (test-full-buffer-is-answered-when-it-fills)
  (test-ws-send-refuses-a-closing-websocket)
  (test-ws-close)
  (test-ws-close-from-a-handler-e2e)
  (test-ws-message-deadline-e2e)
  (test-ws-upgrade-shapes-the-101)
  (test-ws-upgrade-subprotocol-e2e)
  (test-ws-handler-reply-of-the-wrong-type)
  (test-ws-replies-keep-the-order-they-were-made-in)
  (test-websocket-peer-that-leaves-mid-frame)
  (test-detached-pause-auto-resumes)
  (test-ws-send-arms-from-a-fetch-callback)
  (test-ws-send-arms-a-fan-out-target)
  (test-stream-close-arms-the-write-path)
  (test-stream-close-refuses-a-foreign-connection)
  (test-cpu-count-parsers)
  (test-ws-oversized-frame-close-code)
  (test-ws-fragments-are-held-as-their-bytes)
  (test-small-writes-are-queued-joined)
  (test-discard-available-want-write)
  (test-fetch-timeout-validated-at-boot)
  (test-log-level-validated-at-boot)
  (test-numeric-settings-validated-at-boot)
  (test-listen-backlog-reaches-the-listener)
  (test-accept-goes-on-past-a-connection-it-could-not-set-up)
  (test-on-tick-runs-with-the-workers-bindings)
  (test-on-tick-survives-a-raising-hook)
  (test-serious-conditions-cost-the-request)
  (test-serious-conditions-spare-the-hook)
  (test-serious-conditions-at-the-backstop)
  (test-serious-conditions-at-the-callback-sites)
  (test-a-raise-after-the-response-began)
  (test-a-worker-crash-closes-its-streams-with-on-close)
  (test-on-tick-validated-at-boot)
  (test-body-handler-validated-at-boot)
  (test-map-worker-websockets-visits-only-websockets)
  (test-map-worker-websockets-skips-what-a-callback-closed)
  (test-map-worker-websockets-refuses-off-a-worker)
  (test-map-worker-websockets-delivers-from-on-tick)
  (test-connection-census-is-exported-with-a-split-contract)
  (test-a-log-line-is-one-line)
  (test-counters-count-responses-by-class)
  (test-counters-see-a-response-no-handler-produced)
  (test-counters-count-every-way-a-frame-is-handed-over)
  (test-counters-count-a-response-the-serializer-never-built)
  (test-counters-a-client-reset-is-not-a-server-error)
  (test-getent-parse-separates-policy-from-resolution)
  (test-parse-error-carries-its-status-to-a-caller)
  (report-suite "Server, continued")
  (zerop *tests-failed*))
