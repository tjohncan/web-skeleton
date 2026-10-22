(in-package :web-skeleton)

;;; ===========================================================================
;;; WebSocket Protocol (RFC 6455)
;;;
;;; Handshake, frame parser, frame writer, and connection handler.
;;; All I/O uses non-blocking fd operations via the connection's buffers.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defparameter *websocket-guid* "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "Magic GUID from RFC 6455 §4.2.2, used in the opening handshake.")

(defconstant +ws-op-continuation+ #x0)
(defconstant +ws-op-text+  #x1)
(defconstant +ws-op-binary+ #x2)
(defconstant +ws-op-close+ #x8)
(defconstant +ws-op-ping+  #x9)
(defconstant +ws-op-pong+  #xA)

;;; *max-ws-payload-size* is defined in http.lisp alongside the other limits.

(define-condition ws-frame-too-large (error)
  ((message :initarg :message :reader ws-frame-too-large-message))
  (:report (lambda (c s) (write-string (ws-frame-too-large-message c) s)))
  (:documentation
   "A single frame declaring more than *MAX-WS-PAYLOAD-SIZE* bytes.

    Its own condition because RFC 6455 7.4.1 has 1009 (Message Too Big) for
    it, and every other parse failure in TRY-PARSE-WS-FRAME is a protocol
    error answered 1002. Without the distinction the oversized *frame*
    would take WEBSOCKET-ON-READ's catch-all and close 1002, while the
    oversized fragmented *message* — the same complaint one layer up —
    closes 1009 in two places. One file, two answers to one question,
    which is the disagreement this codebase is organised against."))

;;; ---------------------------------------------------------------------------
;;; Handshake
;;; ---------------------------------------------------------------------------

(defun websocket-accept-key (client-key)
  "Compute the Sec-WebSocket-Accept value for a given Sec-WebSocket-Key.
   This is Base64(SHA-1(key + GUID))."
  (let* ((combined (concatenate 'string client-key *websocket-guid*))
         (digest (sha1 (sb-ext:string-to-octets combined :external-format :ascii))))
    (base64-encode digest)))

(defun ws-close-code-valid-p (code)
  "T for a status code a close frame may carry (RFC 6455 §7.4): 1000-1003,
   1007-1014 and 3000-4999.

   The rest are not codes either end may put in a frame: below 1000 is
   unused, 1004 is reserved, 1005, 1006 and 1015 stand for a missing code,
   an abnormal close and a TLS failure and are never sent, 1016-2999 are
   unassigned, and 5000 and up are outside the range. One definition for
   both directions. BUILD-WS-CLOSE refuses to send any other code, and
   WEBSOCKET-ON-READ answers a peer's close frame carrying one with 1002,
   as it answers every other malformed frame."
  (or (<= 1000 code 1003)
      (<= 1007 code 1014)
      (<= 3000 code 4999)))

(defun websocket-upgrade-p (request)
  "Check if REQUEST is a valid WebSocket upgrade request.
   Sec-WebSocket-Key validation includes a base64 charset check —
   the length test alone accepts any 24-character string, including
   strings containing non-ASCII bytes that would later trip
   WEBSOCKET-ACCEPT-KEY's :external-format :ascii conversion and
   surface as 500 instead of the 400 a malformed key deserves.

   Connection and Upgrade are read across every field of that name, as
   the keep-alive decision reads Connection: RFC 7230 §3.2.2 makes
   repeated fields one comma list, and a proxy that writes its own
   Connection: keep-alive beside the client's Connection: Upgrade would
   otherwise fail the upgrade on whichever came first. Sec-WebSocket-Key
   and Sec-WebSocket-Version must each appear exactly once (RFC 6455
   §11.3.1, §11.3.5). With two keys, the accept value would be computed
   from whichever this reader picked, and any reader that picked the
   other would disagree."
  (and (eq (http-request-method request) :GET)
       (string= (http-request-version request) "1.1")
       (let ((upgrades    (get-headers request "upgrade"))
             (connections (get-headers request "connection"))
             (keys        (get-headers request "sec-websocket-key"))
             (versions    (get-headers request "sec-websocket-version")))
         (and (some (lambda (v) (header-has-token-p v "websocket")) upgrades)
              (some (lambda (v) (header-has-token-p v "upgrade")) connections)
              (= (length keys) 1)
              (= (length versions) 1)
              (let ((key (first keys))
                    (version (first versions)))
                (and
                 (= (length key) 24) ; base64(16 bytes) per RFC 6455 §4.2.2
                 ;; RFC 4648 standard base64 alphabet: A-Z / a-z / 0-9 / '+'
                 ;; / '/' with '=' padding. Sec-WebSocket-Key is a
                 ;; fixed-size base64 over 16 random bytes, so the shape is
                 ;; fully determined — 22 data characters and then exactly
                 ;; "==". Checked positionally: a flat alphabet sweep across
                 ;; all 24 admits '=' anywhere, so it would accept a key of
                 ;; 24 '=' characters.
                 (string= key "==" :start1 22)
                 (loop for i from 0 below 22
                       for c = (char key i)
                       always (or (char<= #\A c #\Z)
                                  (char<= #\a c #\z)
                                  (char<= #\0 c #\9)
                                  (char= c #\+) (char= c #\/)))
                 (string= version "13")))))))

;;; ---------------------------------------------------------------------------
;;; Shaping the 101: a subprotocol, and headers of the application's
;;; ---------------------------------------------------------------------------

(defstruct (ws-upgrade (:constructor %make-ws-upgrade (protocol headers)))
  "A handler's answer to a WebSocket handshake that shapes the 101, made by
   WS-UPGRADE and returned in place of :UPGRADE."
  (protocol nil :type (or null string))
  (headers  nil :type list))

(defparameter *ws-handshake-owned-headers*
  '("upgrade" "connection" "sec-websocket-accept" "sec-websocket-protocol"
    "sec-websocket-extensions" "content-length" "transfer-encoding")
  "Fields of the 101 the handshake writes or rules out, which WS-UPGRADE's
   HEADERS may not carry. Sec-WebSocket-Protocol is WS-UPGRADE's PROTOCOL,
   checked against what the client offered. Sec-WebSocket-Extensions would
   agree to an extension, such as permessage-deflate, that the frame
   reader does not implement. A 101 has no body to frame.")

(defun ws-upgrade (&key protocol headers)
  "An answer to a WebSocket handshake that shapes the 101, for a handler to
   return in place of :UPGRADE.

   PROTOCOL is the subprotocol chosen, sent as the 101's
   Sec-WebSocket-Protocol. It must be one the client offered (RFC 6455
   §4.2.2), and a client that offered any fails the connection unless the
   101 names one of them; WS-CHOOSE-PROTOCOL picks it. HEADERS is an alist
   of further fields for the 101, (NAME . VALUE) strings: a cookie, a
   correlation id. Each goes out as written, so a name may repeat.

   Signals for a field the handshake owns (*WS-HANDSHAKE-OWNED-HEADERS*),
   and for a name or value that is not a string. The handshake's own
   checks still decide the answer: a request that is not a valid upgrade
   is answered 400, and a PROTOCOL the client did not offer is a 500 that
   names it."
  (unless (or (null protocol) (stringp protocol))
    (error "ws-upgrade: :protocol is ~s; it must be a string, or NIL" protocol))
  (unless (listp headers)
    (error "ws-upgrade: :headers is ~s; it must be an alist of (name . value) ~
            strings" headers))
  (loop for entry in headers
        do (unless (and (consp entry) (stringp (car entry)) (stringp (cdr entry)))
             (error "ws-upgrade: header ~s is not a (name . value) pair of strings"
                    entry))
           (when (member (car entry) *ws-handshake-owned-headers*
                         :test #'string-equal)
             (error "ws-upgrade: ~a is the handshake's to write~a"
                    (car entry)
                    (if (string-equal (car entry) "sec-websocket-protocol")
                        "; choose the subprotocol with :protocol"
                        ""))))
  (%make-ws-upgrade protocol (copy-alist headers)))

(defun ws-offered-protocols (request)
  "The subprotocols REQUEST's client offered, in the order offered: every
   Sec-WebSocket-Protocol field, split on commas, each name trimmed."
  (loop for value in (get-headers request "sec-websocket-protocol")
        nconc (loop for start = 0 then (1+ comma)
                    for comma = (position #\, value :start start)
                    for name = (string-trim '(#\Space #\Tab)
                                            (subseq value start comma))
                    unless (zerop (length name)) collect name
                    while comma)))

(defun ws-choose-protocol (request supported)
  "The subprotocol to answer REQUEST's handshake with: the first of
   SUPPORTED, a list of names in the application's order of preference,
   that the client offered, or NIL when it offered none of them. Names
   are compared exactly, as browsers compare the one a 101 returns."
  (let ((offered (ws-offered-protocols request)))
    (find-if (lambda (name) (member name offered :test #'string=)) supported)))

(defun make-websocket-handshake-response (request &optional upgrade)
  "Build the 101 Switching Protocols response for a WebSocket upgrade.
   Returns an HTTP-RESPONSE. UPGRADE, a WS-UPGRADE, adds its subprotocol
   and its headers; it has been checked against REQUEST already."
  (let* ((key (get-header request "sec-websocket-key"))
         (accept (websocket-accept-key key))
         (response (make-http-response :status 101)))
    (set-response-header response "upgrade" "websocket")
    (set-response-header response "connection" "Upgrade")
    (set-response-header response "sec-websocket-accept" accept)
    (when upgrade
      (when (ws-upgrade-protocol upgrade)
        (set-response-header response "sec-websocket-protocol"
                             (ws-upgrade-protocol upgrade)))
      (loop for (name . value) in (ws-upgrade-headers upgrade)
            do (add-response-header response name value)))
    response))

;;; ---------------------------------------------------------------------------
;;; Frame structure
;;; ---------------------------------------------------------------------------

(defstruct ws-frame
  (fin    t   :type boolean)
  (opcode 0   :type (unsigned-byte 4))
  (payload (make-array 0 :element-type '(unsigned-byte 8))
          :type (simple-array (unsigned-byte 8) (*))))

;;; ---------------------------------------------------------------------------
;;; Incremental frame reader
;;;
;;; Tries to parse a complete frame from the connection's read buffer.
;;; Returns a WS-FRAME if one is complete, or NIL if more data needed.
;;;
;;; Wire format (RFC 6455 §5.2):
;;;   Byte 0:  [FIN:1][RSV:3][OPCODE:4]
;;;   Byte 1:  [MASK:1][PAYLOAD-LEN:7]
;;;   Extended payload length (0, 2, or 8 bytes depending on len)
;;;   Masking key (4 bytes, present if MASK=1)
;;;   Payload data
;;; ---------------------------------------------------------------------------

(defun utf-8-well-formed-p (bytes &optional (start 0) (end (length bytes)))
  "T when BYTES[START..END) is well-formed UTF-8 (RFC 3629, and the
   Unicode Standard's table of well-formed byte sequences): no overlong
   form, no surrogate code point, nothing past U+10FFFF, and no sequence
   cut short. What RFC 6455 §8.1 asks of a text message and a close
   reason.

   Read in place. Decoding to check would build a string of up to four
   bytes a character, a 4 MiB one for a 1 MiB message, only to drop it.
   TEST-UTF-8-WELL-FORMED-P holds this to SBCL's decoder on the edge
   cases and on random bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum start end))
  (let ((i start))
    (declare (type fixnum i))
    (flet ((tail-p (j lo hi)
             ;; Byte J is there, and in LO..HI.
             (and (< j end) (<= lo (aref bytes j) hi))))
      (declare (inline tail-p))
      (loop
        (when (>= i end) (return t))
        (let ((b (aref bytes i)))
          (cond
            ((< b #x80) (incf i))
            ;; C0 and C1 could only begin an overlong form of ASCII.
            ((<= #xC2 b #xDF)
             (unless (tail-p (+ i 1) #x80 #xBF) (return nil))
             (incf i 2))
            ;; E0's second byte from A0, or the form is overlong; ED's up
            ;; to 9F, or it is a surrogate.
            ((<= #xE0 b #xEF)
             (unless (and (tail-p (+ i 1)
                                  (if (= b #xE0) #xA0 #x80)
                                  (if (= b #xED) #x9F #xBF))
                          (tail-p (+ i 2) #x80 #xBF))
               (return nil))
             (incf i 3))
            ;; F0's second byte from 90, or the form is overlong; F4's up to
            ;; 8F, or it is past U+10FFFF. F5 and above begin nothing.
            ((<= #xF0 b #xF4)
             (unless (and (tail-p (+ i 1)
                                  (if (= b #xF0) #x90 #x80)
                                  (if (= b #xF4) #x8F #xBF))
                          (tail-p (+ i 2) #x80 #xBF)
                          (tail-p (+ i 3) #x80 #xBF))
               (return nil))
             (incf i 4))
            (t (return nil))))))))

(defun try-parse-ws-frame (buf start end)
  "Try to parse a WebSocket frame from BUF[START..END).
   Returns (values frame bytes-consumed) if complete, or (values NIL 0)."
  (let ((available (- end start)))
    ;; Need at least 2 bytes for the header
    (when (< available 2)
      (return-from try-parse-ws-frame (values nil 0)))
    (let* ((b0 (aref buf start))
           (b1 (aref buf (+ start 1)))
           (fin (logbitp 7 b0))
           (opcode (logand b0 #x0F))
           (masked (logbitp 7 b1))
           (len7 (logand b1 #x7F))
           (header-size 2)
           payload-length)
      ;; RSV1/2/3 must be zero unless an extension negotiated them (RFC 6455 §5.2)
      (when (logtest b0 #x70)
        (error "WebSocket: non-zero RSV bits"))
      ;; RFC 6455 §5.1: every client-to-server frame is masked. The mask
      ;; bit is byte 1, so this is decidable from the two bytes already
      ;; in hand — no reason to wait. Checked after the availability test,
      ;; an unmasked frame would be buffered to completion before being
      ;; refused, which is a peer choosing how much of our memory to
      ;; occupy with something we had already decided to reject.
      (unless masked
        (error "WebSocket: received unmasked client frame"))
      ;; RFC 6455 §5.2: opcodes 3-7 and 11-15 are reserved, and a frame
      ;; carrying one fails the connection. Decidable from byte 0, so for
      ;; the reason above it is refused here, before its payload is read:
      ;; 3-7 count as data frames, and would otherwise be buffered and
      ;; unmasked up to *MAX-WS-PAYLOAD-SIZE* only to be refused.
      ;; WEBSOCKET-ON-READ's unknown-opcode arm stays as its own backstop.
      (when (or (<= 3 opcode 7) (<= 11 opcode 15))
        (error "WebSocket: reserved opcode ~d" opcode))
      ;; Determine payload length and header size
      (cond
        ((<= len7 125)
         (setf payload-length len7))
        ((= len7 126)
         (when (< available 4)
           (return-from try-parse-ws-frame (values nil 0)))
         (setf payload-length (logior (ash (aref buf (+ start 2)) 8)
                                      (aref buf (+ start 3)))
               header-size 4)
         ;; RFC 6455 §5.2: "the minimal number of bytes MUST be used
         ;; to encode the length." A 16-bit extended length < 126 is
         ;; non-canonical — the peer should have used the 7-bit form.
         ;; Accepting it opens a length-parser disagreement vector
         ;; against strict downstreams.
         (when (< payload-length 126)
           (error "WebSocket: non-canonical 2-byte length")))
        ((= len7 127)
         (when (< available 10)
           (return-from try-parse-ws-frame (values nil 0)))
         (setf payload-length
               (loop for i from 0 below 8
                     sum (ash (aref buf (+ start 2 i)) (* 8 (- 7 i))))
               header-size 10)
         ;; RFC 6455 §5.2 minimal-encoding rule: a 64-bit extended
         ;; length < 65536 is non-canonical.
         (when (< payload-length 65536)
           (error "WebSocket: non-canonical 8-byte length"))))
      ;; RFC 6455 §5.2: 64-bit length MSB must be 0
      (when (logbitp 63 payload-length)
        (error "WebSocket: invalid payload length (MSB set)"))
      ;; Control frames (opcode >= 8): must have payload <= 125 and FIN=1
      ;; (RFC 6455 §5.5)
      ;;
      ;; Ahead of the size check below, and the order is the close code.
      ;; WS-FRAME-TOO-LARGE closes 1009, which is the right answer for a
      ;; data frame this endpoint will not buffer — a limit we chose. A
      ;; control frame over 125 bytes is not a limit we chose: §5.5 makes
      ;; it malformed for anyone, so 1002 is what it earns. Checked after
      ;; the size check, a ping declaring a megabyte would get 1009 and
      ;; tell the peer its message was too big for us rather than that its
      ;; frame was illegal. Both checks read the declared length out of
      ;; the header, so nothing is buffered either way, and either order
      ;; rejects early.
      (when (>= opcode 8)
        (when (> payload-length 125)
          (error "WebSocket: control frame payload too large (~d bytes, max 125)"
                 payload-length))
        (unless fin
          (error "WebSocket: fragmented control frame")))
      ;; Reject oversized frames early
      (when (> payload-length *max-ws-payload-size*)
        (error 'ws-frame-too-large
               :message (format nil "WebSocket: frame too large (~d bytes, max ~d)"
                                payload-length *max-ws-payload-size*)))
      ;; Account for mask key
      (when masked (incf header-size 4))
      ;; Check if we have the full frame
      (let ((frame-size (+ header-size payload-length)))
        (when (< available frame-size)
          (return-from try-parse-ws-frame (values nil 0)))
        ;; Unmask payload — read mask key directly from buffer, no allocation
        (let* ((mask-start (+ start (- header-size 4)))
               (payload-start (+ start header-size))
               (payload (make-array payload-length
                                    :element-type '(unsigned-byte 8))))
          (loop for i from 0 below payload-length
                do (setf (aref payload i)
                         (logxor (aref buf (+ payload-start i))
                                 (aref buf (+ mask-start (logand i 3))))))
          (values (make-ws-frame :fin fin :opcode opcode :payload payload)
                  frame-size))))))

;;; ---------------------------------------------------------------------------
;;; Frame writer
;;;
;;; Server-to-client frames are NOT masked (per spec).
;;; Returns a byte vector ready to write.
;;; ---------------------------------------------------------------------------

(defun build-ws-frame (opcode payload)
  "Build a WebSocket frame as a byte vector. PAYLOAD is a byte vector."
  (let* ((len (length payload))
         (header-size (cond ((<= len 125) 2)
                            ((<= len 65535) 4)
                            (t 10)))
         (frame (make-array (+ header-size len)
                            :element-type '(unsigned-byte 8))))
    ;; Byte 0: FIN=1, opcode
    (setf (aref frame 0) (logior #x80 opcode))
    ;; Length encoding (no mask bit — server frames are unmasked)
    (cond
      ((<= len 125)
       (setf (aref frame 1) len))
      ((<= len 65535)
       (setf (aref frame 1) 126
             (aref frame 2) (logand #xFF (ash len -8))
             (aref frame 3) (logand #xFF len)))
      (t
       (setf (aref frame 1) 127)
       (loop for i from 0 below 8
             do (setf (aref frame (+ 2 i))
                      (logand #xFF (ash len (* -8 (- 7 i))))))))
    ;; Payload
    (replace frame payload :start1 header-size)
    frame))

(defun build-ws-text (text)
  "Build a text frame for TEXT."
  (build-ws-frame +ws-op-text+
                  (sb-ext:string-to-octets text :external-format :utf-8)))

(defun build-ws-close (&optional (code 1000))
  "Build a close frame with a status code. CODE must be one
   WS-CLOSE-CODE-VALID-P accepts: 1000-1003, 1007-1014, or 3000-4999.
   Codes < 1000, the reserved 1004/1005/1006/1015, the unassigned
   1016-2999, and 5000+ are rejected here, as the receive path refuses
   them from a peer. A silent u16 truncation on out-of-range input
   (values > 65535) would put bytes on the wire the application never
   asked for."
  (unless (ws-close-code-valid-p code)
    (error "build-ws-close: code ~a not allowed to be sent per RFC 6455 §7.4.1 ~
            (use 1000-1003, 1007-1014, or 3000-4999)" code))
  (let ((payload (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref payload 0) (logand #xFF (ash code -8))
          (aref payload 1) (logand #xFF code))
    (build-ws-frame +ws-op-close+ payload)))

(defun build-ws-pong (payload)
  "Build a pong frame echoing PAYLOAD."
  (build-ws-frame +ws-op-pong+ payload))

(defun build-ws-ping ()
  "Return the pre-built ping frame (constant — safe because frames are only read)."
  (load-time-value
   (build-ws-frame +ws-op-ping+
                   (make-array 0 :element-type '(unsigned-byte 8)))))

;;; ---------------------------------------------------------------------------
;;; Frame send
;;;
;;; Queues a frame and flushes what the socket will accept right now.
;;; Three callers, and only the first is served by HANDLE-CLIENT-READ's
;;; arming:
;;;
;;;   1. A ws-handler sending on the connection it was handed. The event
;;;      loop is paused while it runs, so there is no contention with pings
;;;      or other writes, and HANDLE-CLIENT-READ arms that connection once
;;;      the handler returns.
;;;   2. A ws-handler sending to a *different* connection — fan-out, which
;;;      DEPLOYMENT.md documents as the shape to prefer over your own queue.
;;;      HANDLE-CLIENT-READ arms (CONNECTION-FD CONN), the connection it was
;;;      woken for. Nobody arms the subscriber.
;;;   3. A fetch callback on a :WEBSOCKET target, which runs on the
;;;      *outbound* connection's read path. HANDLE-OUTBOUND-READ arms the
;;;      outbound. Nobody arms the target.
;;;
;;; Two and three are why the arming below exists, and they are the same
;;; defect: an arming site that names one connection, reached from a context
;;; that wrote to another.
;;;
;;; The flush is opportunistic: one non-blocking pass, no spin and no
;;; deadline. A pure append would have been simpler, and wrong — the
;;; event loop does not turn while a handler runs, so nothing would reach
;;; the peer until the handler returned, and a handler that streams for
;;; thirty seconds is the documented use. The pass keeps incremental
;;; delivery for a peer that is keeping up, and a peer that is not gets
;;; its bytes queued instead of freezing the worker.
;;;
;;; EPOLLOUT is armed here, and only when the flush did not finish. That
;;; is STREAM-FLUSH's conditional, and it costs nothing for a peer that is
;;; keeping up: the flush finishes, so a handler sending in a loop to a
;;; draining peer pays no epoll_ctl at all. Against a peer that is *not*
;;; draining the same loop pays one MOD per frame with an identical mask,
;;; which is the case the objection to arming per frame was actually
;;; about. It is the cheaper half of the trade — the alternative is the
;;; defect below — but the conditional narrows that cost rather than
;;; removing it. The fd comes from *EPOLL-FD*, the worker's own, bound for
;;; exactly this and NIL outside a worker — so the check is a check and
;;; not an assumption.
;;;
;;; Leaving it to HANDLE-CLIENT-READ would be right for one caller and
;;; wrong for the other. That site arms after a handler returns, and
;;; WS-SEND is also reachable from a fetch callback on a :WEBSOCKET
;;; target, which runs on the *outbound* connection's read path — where
;;; HANDLE-OUTBOUND-READ arms the outbound and nothing arms the target.
;;; HANDLE-CLIENT-READ does not run for it unless its peer happens to
;;; send something. A frame larger than the socket takes would leave its
;;; tail queued with no epoll_ctl against the target, waiting for an event
;;; that is not coming, until *WRITE-STALL-TIMEOUT* closed the connection
;;; rather than flushing it. Every send would have reported success and
;;; the peer would get a truncated message, which is the failure this
;;; codebase refuses everywhere else.
;;;
;;; EPOLLOUT alone, not EPOLLIN with it. That is what HANDLE-CLIENT-READ
;;; arms for a :WEBSOCKET connection carrying a backlog, and
;;; HANDLE-CLIENT-WRITE restores EPOLLIN once the queue drains, so this
;;; enters a loop that already exists rather than adding a third mask
;;; convention to one state.
;;;
;;; A peer that is *gone* is still noticed, and the reason is stronger than
;;; writability: EPOLLERR and EPOLLHUP are reported whether or not they were
;;; requested — epoll_ctl(2) says so — so a full close arrives as
;;; OUT|ERR|HUP under this mask and the next write raises ECONNRESET.
;;;
;;; A peer that *half-closes* while we are behind is not noticed: the mask
;;; reports no event for it, and it waits for *WRITE-STALL-TIMEOUT*. That
;;; is a real exception and it is stated rather than rounded off — but it
;;; is not one this widens, because HANDLE-CLIENT-READ already arms
;;; EPOLLOUT alone for a backlogged :WEBSOCKET connection, and a WebSocket
;;; peer that half-closes without a close frame is violating RFC 6455
;;; 5.5.1 already.
;;;
;;; Adding EPOLLIN would instead let a handler be re-entered against a full
;;; queue, which is where WS-SEND signals — turning a slow peer into a dead
;;; connection.
;;;
;;; And it bounds per-connection memory, which is what makes the exclusive
;;; convention a design rather than an accident. While EPOLLIN is dropped
;;; HANDLE-CLIENT-READ does not run, so WS-FRAG-BUF cannot grow: a peer
;;; cannot keep pushing fragments into reassembly while its write side is
;;; stuck. Under the combined mask the worst case per connection becomes
;;; *MAX-WRITE-BACKLOG* and *MAX-WS-MESSAGE-SIZE* and the read buffer all at
;;; once, rather than in alternation. The kernel receive buffer filling and
;;; the peer's send window closing is the correct backpressure for a peer
;;; that will not drain, and this mask is what produces it.
;;; ---------------------------------------------------------------------------

(defun ws-send (conn frame-bytes)
  "Queue FRAME-BYTES for CONN and flush as much as the socket takes now.
   FRAME-BYTES should be a byte vector from BUILD-WS-TEXT, BUILD-WS-FRAME, etc.
   Returns T if everything reached the kernel, NIL if a remainder is queued.

   Does not block. Bytes may still be in the queue when this returns, and
   the event loop flushes the rest — so a NIL return is the normal way a
   slow peer looks, not an error. Failures of the flush itself do surface
   here; failures of the deferred remainder surface on the event loop.

   Call it from within ws-handler, from :ON-TICK on a WebSocket this worker
   owns — MAP-WORKER-WEBSOCKETS walks them — or from a fetch callback on a
   :WEBSOCKET target. Inside a handler or the hook the event loop is paused,
   so there is no write contention; from a fetch callback the target is a
   connection nothing else is writing to for the life of the fetch. A
   remainder is handed to the event loop the same way in all three — see the
   header comment for why it has to be handed over here.

   Signals if the connection is already at *MAX-WRITE-BACKLOG*: the frame
   is not queued, not truncated, and the peer is far enough behind that
   dropping it silently would leave the app's view and the peer's view of
   the stream permanently different.

   Signals if CONN is not on this worker's connection table — an app
   reaching across workers, whose frame would otherwise be appended to an
   unsynchronised queue. FETCH-INTO refuses the same misuse for the same
   reason, and the two agree: both refuse every cross-worker call, not
   only the ones that happen to leave a remainder.

   Signals if CONN is not an open WebSocket, :CLOSING included. A close
   frame is queued or sent by then, the echo of the peer's or the drain's
   own, and RFC 6455 §5.5.1 allows no data frame after it. The connection
   stays on the table until the write path flushes that frame and closes,
   so the ownership check passes, and an application keeping its own
   registry meets one on the pass its client closed. This refusal is what
   README tells such a registry to prune on. MAP-WORKER-WEBSOCKETS visits
   open WebSockets only, so a walk through it never sees one.

   The wrong worker, not the wrong thread. Asking whether an fd is on this
   worker's table takes a worker to ask from, so a call off the event loop
   entirely — an application's own timer or queue consumer — finds no table
   and is not refused. FETCH-INTO refuses that one as well, but for
   an unrelated reason: it needs a loop to drive the outbound it opens — a
   functional precondition this function does not share, since a send off a
   worker to a connection nothing else is touching works correctly and the
   harness relies on it.

   Closing it here would mean refusing every call from outside an event
   loop. That is reachable and it is not this function's decision:
   STREAM-SEND and STREAM-CLOSE have the identical hole, and narrowing one
   of the three leaves a boundary that reads as an oversight instead of a
   rule. Three together or none.

   Signals also if the arming fails, and that one is not symmetric with the
   others: the frame has been queued and flushed by then, so the raise
   reports that the *remainder* has no event coming, not that the send did
   not happen. Arming catches a cross-worker call only incidentally — an
   fd absent from this worker's epoll gives ENOENT — and only when the
   flush leaves something behind. The check above is what makes refusing
   one a guarantee; this one stays as the report that a queued remainder
   has nothing coming."
  (unless (plusp *write-stall-timeout*)
    (error "ws-send: *write-stall-timeout* is ~s; it must be positive. ~
            There is no unbounded setting, because it is the only ~
            deadline on a queue this connection may never drain."
           *write-stall-timeout*))
  ;; Ownership before the queue is touched, because the queue is what a
  ;; cross-worker call corrupts: CONNECTION-APPEND-WRITE mutates a vector
  ;; and CONNECTION-ON-WRITE calls send(2), both from a thread that owns
  ;; neither. Refusing after the append would report the misuse and have
  ;; committed it anyway.
  ;;
  ;; Conditional on *EPOLL-FD*, where FETCH-INTO requires it. The two want
  ;; different things from the same fact: FETCH-INTO opens an outbound that
  ;; only an event loop can drive, so no loop is itself the error, while
  ;; WS-SEND is a queue-and-flush that works fine off a worker and the
  ;; harness calls it that way on bare connections. Here NIL means "not on
  ;; a worker at all", which is not the misuse being caught. *CONNECTIONS*
  ;; is bound outside *EPOLL-FD* in RUN-WORKER, so non-NIL guarantees a
  ;; live table to ask.
  ;;
  ;; EQ against the table, not a comparison on the fd number: an fd
  ;; reissued to a different connection on this worker would satisfy the
  ;; number while being the wrong object.
  (when *epoll-fd*
    (unless (eq (lookup-connection (connection-fd conn)) conn)
      (error "ws-send: fd ~d is not on this worker's connection table. ~
              A WebSocket can only be written from the worker that owns ~
              it — the write queue has no lock precisely because nothing ~
              else touches it, so appending here would corrupt it and the ~
              flush would call send(2) from the wrong thread."
             (connection-fd conn))))
  (unless (eq (connection-state conn) :websocket)
    (error "ws-send: fd ~d is ~s, not an open WebSocket. A frame after a ~
            close frame is refused (RFC 6455 §5.5.1), and a connection that ~
            is not a WebSocket takes no frames."
           (connection-fd conn) (connection-state conn)))
  (unless (connection-append-write conn frame-bytes)
    (error "ws-send: fd ~d is at *max-write-backlog* (~d bytes pending, ~
            frame is ~d); the peer is not draining."
           (connection-fd conn)
           (connection-write-pending conn)
           (length frame-bytes)))
  ;; After the append succeeded, so a frame refused at the backlog is not
  ;; counted as one sent. This counts frames handed to the queue, not bytes
  ;; that reached a peer — the queue is where this function's responsibility
  ;; ends, and a count that claimed delivery would be claiming something no
  ;; write path here can know.
  (note-ws-frame)
  (let ((done (eq (connection-on-write conn) :done)))
    (unless (or done (null *epoll-fd*))
      (epoll-modify *epoll-fd* (connection-fd conn)
                    (logior +epollout+ +epollet+)))
    done))

(defun ws-close (conn &optional (code 1000))
  "Close the WebSocket CONN from the application's side. Queues a close
   frame carrying CODE and closes the connection once it has gone out.
   Returns T.

   The answer DEPLOYMENT.md gives an application whose own events have
   outrun a peer: a dropped event is invisible to the client, so its view
   and the server's would part for good, and the connection should go. A
   peer at *MAX-WRITE-BACKLOG* has no room for the close frame either, and
   is not reading, so it is closed at once rather than waited on.

   Called from a ws-handler, it ends the handling of that read: the reply
   the handler returns is not sent, and no frame after the one it was
   called for is handled, since nothing may follow the close frame (RFC
   6455 §5.5.1).

   Refused, as WS-SEND refuses, for a connection this worker does not own
   and for one that is not an open WebSocket, :CLOSING included: a close
   frame is already on its way there. Refused too off a worker, since the
   close is the event loop's to carry out. CODE must be one a close frame
   may carry; BUILD-WS-CLOSE says which."
  (unless *epoll-fd*
    (error "ws-close: not on a worker; the close is carried out by the event ~
            loop that owns the connection"))
  (unless (eq (lookup-connection (connection-fd conn)) conn)
    (error "ws-close: fd ~d is not on this worker's connection table"
           (connection-fd conn)))
  (unless (eq (connection-state conn) :websocket)
    (error "ws-close: fd ~d is ~s, not an open WebSocket"
           (connection-fd conn) (connection-state conn)))
  (let ((frame (build-ws-close code)))
    (if (connection-append-write conn frame)
        (progn
          ;; :CLOSING is what HANDLE-CLIENT-WRITE closes on once the queue
          ;; drains, and what WS-SEND refuses meanwhile. Armed whatever the
          ;; queue holds, so the close happens even when this frame is the
          ;; only thing in it.
          (setf (connection-state conn) :closing)
          (epoll-modify *epoll-fd* (connection-fd conn)
                        (logior +epollout+ +epollet+)))
        (close-connection conn *epoll-fd*)))
  t)

(defun check-ws-reply (reply)
  "REPLY, a ws-handler's return value, if it is one the write queue can
   carry: NIL for no reply, or a frame's bytes. Signals otherwise.

   Checked where the reply is received, before WEBSOCKET-ON-READ queues
   it. CONNECTION-APPEND-WRITE declares its argument a byte vector, so a
   string would otherwise raise inside the queue, with a message about a
   type declaration rather than about the handler. Raised here, it raises
   inside the catch that closes the WebSocket, and the message names the
   fix the way DISPATCH-REQUEST's does for an HTTP handler."
  (unless (or (null reply)
              (typep reply '(simple-array (unsigned-byte 8) (*))))
    (error "ws-handler returned ~a; expected NIL, or a frame's bytes from ~
            BUILD-WS-TEXT or BUILD-WS-FRAME"
           (type-of reply)))
  reply)

(defun ws-shift-buffer (conn buf pos end)
  "Shift unconsumed bytes to the start of the read buffer."
  (let ((remaining (- end pos)))
    (when (> remaining 0)
      (replace buf buf :start1 0 :start2 pos :end2 end))
    (setf (connection-read-pos conn) remaining)))

;;; ---------------------------------------------------------------------------
;;; WebSocket event handler
;;;
;;; Called by the event loop when a WebSocket connection is readable.
;;; Parses frames from the read buffer, queues each reply on the connection
;;; as it is produced, and says whether the connection should close.
;;; ---------------------------------------------------------------------------

(defun websocket-on-read (conn ws-handler)
  "Process WebSocket frames from CONN's read buffer.

   Every reply goes onto CONN's write queue the moment it exists, the
   ws-handler's returned frames and the pongs alike, so it is ordered
   among the frames WS-SEND appends exactly as it was produced. Held back
   to the end of the batch, a reply would go out behind a WS-SEND made
   while a later message in the same read was handled, and the order a
   peer saw would turn on how TCP had segmented its writes.

   A ws-handler that closes its own connection, with WS-CLOSE, ends the
   read there. Nothing may follow a close frame (RFC 6455 §5.5.1), so the
   reply that handler returned is not sent, and no later frame in the
   same read is handled, a ping's pong included.

   Returns (VALUES :CLOSE CLOSE-FRAME) when the connection should close —
   the caller queues the frame behind the replies already there —
   :BACKLOG-FULL when a reply was refused at *MAX-WRITE-BACKLOG*, :CLOSED
   when a ws-handler closed the connection outright, as WS-CLOSE does a
   peer with no room for its frame, and NIL otherwise, a close a
   ws-handler began included: its frame is queued and armed."
  (let ((buf (connection-read-buf conn))
        (pos 0)
        (end (connection-read-pos conn)))
    (labels ((close-with (code)
               ;; Tear-down helper for every close site, client-initiated
               ;; or an error's. Replies from earlier frames in the batch
               ;; are already queued, so the close frame the caller adds
               ;; lands behind them.
               (ws-shift-buffer conn buf pos end)
               (values :close (build-ws-close code)))
             (handled (frame)
               ;; FRAME to the ws-handler, and its reply onto the queue —
               ;; unless the handler closed the connection, which ends the
               ;; read: what is left in the buffer is moot.
               (let ((reply (funcall ws-handler conn frame)))
                 (unless (eq (connection-state conn) :websocket)
                   (ws-shift-buffer conn buf pos end)
                   (return-from websocket-on-read
                     (if (minusp (connection-fd conn)) :closed nil)))
                 (let ((response (check-ws-reply reply)))
                   ;; A reply is a frame handed to this connection as
                   ;; surely as one from WS-SEND, and it does not pass
                   ;; through WS-SEND, so it is counted here.
                   (when response
                     (note-ws-frame)
                     (queue-reply response)))))
             (queue-reply (bytes)
               ;; A peer already at *MAX-WRITE-BACKLOG* is not draining.
               ;; Dropping the reply would leave the application's view of
               ;; the conversation and the peer's permanently different with
               ;; nothing raised anywhere, so the caller closes instead.
               (unless (connection-append-write conn bytes)
                 (return-from websocket-on-read :backlog-full))))
    (loop
      (multiple-value-bind (frame consumed)
          (handler-case
              (try-parse-ws-frame buf pos end)
            ;; Ahead of the catch-all, and the only reason it is a separate
            ;; clause: RFC 6455 7.4.1 answers an oversized frame 1009, the
            ;; same code the oversized fragmented message gets two branches
            ;; below, so a client keying a retry policy on 1009 sees it for
            ;; both shapes.
            (ws-frame-too-large (e)
              (log-throttled :warn :ws-frame-error "ws frame error fd ~d: ~a" (connection-fd conn) e)
              (return-from websocket-on-read (close-with 1009)))
            (error (e)
              ;; Protocol errors (RSV bits, unmasked, bad opcode, etc.)
              ;; → close with 1002 per RFC 6455 7.1.7
              (log-throttled :warn :ws-frame-error "ws frame error fd ~d: ~a" (connection-fd conn) e)
              (return-from websocket-on-read (close-with 1002))))
        (unless frame
          ;; No complete frame — shift unconsumed bytes to start of buffer
          (when (> pos 0)
            (ws-shift-buffer conn buf pos end))
          ;; and give back a buffer one large frame grew, once what is
          ;; left fits the default: a WebSocket lives for hours, and
          ;; nothing else shrinks it. See CONNECTION-SHRINK-READ-BUF.
          (connection-shrink-read-buf conn)
          ;; The clock runs while something is unfinished — a fragmented
          ;; message short of its last fragment, or a frame not all here —
          ;; and *WS-MESSAGE-TIMEOUT* is measured from when it started.
          ;; Started here unless it is already running, and stopped here
          ;; once nothing is left. A completed data message also stops it
          ;; below; a control frame split across two reads starts it and
          ;; completes as no data message, so this is what stops it then.
          (if (or (connection-ws-frag-buf conn)
                  (plusp (connection-read-pos conn)))
              (when (zerop (connection-ws-message-at conn))
                (setf (connection-ws-message-at conn) (get-universal-time)))
              (setf (connection-ws-message-at conn) 0))
          (return))
        ;; Advance past this frame
        (incf pos consumed)
        ;; Handle the frame
        (let ((opcode (ws-frame-opcode frame)))
          (cond
            ;; Application frame (text or binary)
            ((or (= opcode +ws-op-text+) (= opcode +ws-op-binary+))
             ;; RFC 6455 §5.4: new data frame while fragmentation in progress
             ;; is a protocol error
             (when (connection-ws-frag-buf conn)
               (log-throttled :warn :ws-fragment "ws new data frame mid-fragment fd ~d"
                         (connection-fd conn))
               (setf (connection-ws-frag-buf conn) nil
                     (connection-ws-frag-total conn) 0)
               (return-from websocket-on-read (close-with 1002)))
             (if (ws-frame-fin frame)
                 ;; Complete single-frame message
                 (progn
                   ;; RFC 6455 §5.6: text frames must contain valid UTF-8
                   (when (and (= opcode +ws-op-text+)
                              (not (utf-8-well-formed-p (ws-frame-payload frame))))
                     (log-throttled :warn :ws-utf-8 "ws invalid UTF-8 in text frame fd ~d"
                                    (connection-fd conn))
                     (return-from websocket-on-read (close-with 1007)))
                   (log-debug "ws recv opcode ~d (~d bytes) fd ~d"
                              opcode (length (ws-frame-payload frame))
                              (connection-fd conn))
                   (setf (connection-last-active conn) (get-universal-time)
                         (connection-ws-message-at conn) 0)
                   (when ws-handler
                     (handled frame)))
                 ;; First fragment — start accumulating. Enforce the
                 ;; message-size cap on the starting size too, not
                 ;; only on continuation accumulation, so apps that
                 ;; configure *max-ws-payload-size* higher than
                 ;; *max-ws-message-size* cannot slip a giant first
                 ;; fragment past the running-total check.
                 (let ((len (length (ws-frame-payload frame))))
                   (if (> len *max-ws-message-size*)
                       (progn
                         (log-throttled :warn :ws-fragment "ws fragmented message too large ~
                                    on first fragment (~d bytes) fd ~d"
                                   len (connection-fd conn))
                         (return-from websocket-on-read (close-with 1009)))
                       (progn
                         (setf (connection-ws-frag-opcode conn) opcode
                               (connection-ws-frag-buf conn)
                               (list (ws-frame-payload frame))
                               (connection-ws-frag-total conn) len)
                         (log-debug "ws frag start opcode ~d fd ~d"
                                    opcode (connection-fd conn)))))))
            ;; Continuation frame
            ((= opcode +ws-op-continuation+)
             (unless (connection-ws-frag-buf conn)
               (log-throttled :warn :ws-fragment "ws continuation without start fd ~d"
                         (connection-fd conn))
               (return-from websocket-on-read (close-with 1002)))
             ;; Accumulate fragment — O(1) running total instead of re-scanning
             (push (ws-frame-payload frame) (connection-ws-frag-buf conn))
             (incf (connection-ws-frag-total conn) (length (ws-frame-payload frame)))
             (when (> (connection-ws-frag-total conn) *max-ws-message-size*)
               (log-throttled :warn :ws-fragment "ws fragmented message too large (~d bytes) fd ~d"
                         (connection-ws-frag-total conn) (connection-fd conn))
               (setf (connection-ws-frag-buf conn) nil
                     (connection-ws-frag-total conn) 0)
               (return-from websocket-on-read (close-with 1009)))
             (when (ws-frame-fin frame)
               ;; Final fragment — reassemble and deliver
               (let* ((chunks (nreverse (connection-ws-frag-buf conn)))
                      (total (connection-ws-frag-total conn))
                      (payload (make-array total :element-type '(unsigned-byte 8)))
                      (offset 0))
                 (dolist (chunk chunks)
                   (replace payload chunk :start1 offset)
                   (incf offset (length chunk)))
                 (setf (connection-ws-frag-buf conn) nil
                       (connection-ws-frag-total conn) 0)
                 (log-debug "ws frag complete opcode ~d (~d bytes) fd ~d"
                            (connection-ws-frag-opcode conn) total
                            (connection-fd conn))
                 ;; RFC 6455 §5.6: validate UTF-8 for reassembled text
                 (when (and (= (connection-ws-frag-opcode conn) +ws-op-text+)
                            (not (utf-8-well-formed-p payload)))
                   (log-throttled :warn :ws-utf-8 "ws invalid UTF-8 in reassembled text fd ~d"
                                  (connection-fd conn))
                   (return-from websocket-on-read (close-with 1007)))
                 (setf (connection-last-active conn) (get-universal-time)
                       (connection-ws-message-at conn) 0)
                 (when ws-handler
                   (handled (make-ws-frame
                             :fin t
                             :opcode (connection-ws-frag-opcode conn)
                             :payload payload))))))
            ;; Control frames (can arrive between fragments per RFC 6455 §5.4)
            ((= opcode +ws-op-ping+)
             (log-debug "ws ping from client fd ~d" (connection-fd conn))
             (queue-reply (build-ws-pong (ws-frame-payload frame))))
            ((= opcode +ws-op-pong+)
             (log-debug "ws pong fd ~d" (connection-fd conn))
             (setf (connection-missed-pongs conn) 0))
            ((= opcode +ws-op-close+)
             (log-debug "ws close requested on fd ~d" (connection-fd conn))
             (setf (connection-ws-frag-buf conn) nil
                   (connection-ws-frag-total conn) 0)
             (let* ((payload (ws-frame-payload frame))
                    ;; RFC 6455 §5.5.1: close body must be 0 or >= 2 bytes
                    (raw-code (cond
                                ((zerop (length payload)) 1000)
                                ((= (length payload) 1)
                                 (log-throttled :warn :ws-close "ws close with 1-byte body fd ~d"
                                           (connection-fd conn))
                                 (return-from websocket-on-read
                                   (close-with 1002)))
                                (t (logior (ash (aref payload 0) 8)
                                           (aref payload 1)))))
                    (code raw-code))
               ;; A status code no close frame may carry is a malformed
               ;; frame, like the 1-byte body above, and gets the same 1002.
               (unless (ws-close-code-valid-p code)
                 (log-throttled :warn :ws-close "ws close with invalid status code ~d fd ~d"
                                code (connection-fd conn))
                 (return-from websocket-on-read (close-with 1002)))
               ;; RFC 6455 §5.5.1: close reason text must be valid UTF-8
               (unless (utf-8-well-formed-p payload (min 2 (length payload)))
                 (log-throttled :warn :ws-utf-8 "ws invalid UTF-8 in close reason fd ~d"
                                (connection-fd conn))
                 (return-from websocket-on-read (close-with 1007)))
               ;; Normal client-initiated close: the replies to earlier
               ;; frames in this batch are queued already, and CLOSE-WITH
               ;; shifts the buffer.
               (return-from websocket-on-read (close-with code))))
            (t
             ;; RFC 6455 §5.2: unknown opcodes MUST fail the connection
             (log-throttled :warn :ws-opcode "ws unknown opcode ~d fd ~d" opcode (connection-fd conn))
             (return-from websocket-on-read (close-with 1002)))))))
      nil)))
