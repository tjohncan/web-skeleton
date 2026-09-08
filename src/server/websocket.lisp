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

;;; ---------------------------------------------------------------------------
;;; Handshake
;;; ---------------------------------------------------------------------------

(defun websocket-accept-key (client-key)
  "Compute the Sec-WebSocket-Accept value for a given Sec-WebSocket-Key.
   This is Base64(SHA-1(key + GUID))."
  (let* ((combined (concatenate 'string client-key *websocket-guid*))
         (digest (sha1 (sb-ext:string-to-octets combined :external-format :ascii))))
    (base64-encode digest)))

(defun clamp-close-code (raw-code)
  "Map a client-supplied WebSocket close code to the code we echo
   back (RFC 6455 §7.4.1 / §7.4.2). Allowed ranges: 1000-1003,
   1007-1014, 3000-4999. Everything else (< 1000, reserved
   1004/1005/1006/1015, unassigned 1016-2999, undefined >= 5000)
   is clamped to 1000. Lifted out of WEBSOCKET-ON-READ so the
   reserved-code logic has a single definition."
  (if (or (< raw-code 1000)
          (member raw-code '(1004 1005 1006 1015))
          (and (>= raw-code 1016) (<= raw-code 2999))
          (> raw-code 4999))
      1000
      raw-code))

(defun websocket-upgrade-p (request)
  "Check if REQUEST is a valid WebSocket upgrade request.
   Sec-WebSocket-Key validation includes a base64 charset check —
   the length test alone accepts any 24-character string, including
   strings containing non-ASCII bytes that would later trip
   WEBSOCKET-ACCEPT-KEY's :external-format :ascii conversion and
   surface as 500 instead of the 400 a malformed key deserves."
  (and (eq (http-request-method request) :GET)
       (string= (http-request-version request) "1.1")
       (let ((upgrade    (get-header request "upgrade"))
             (connection (get-header request "connection"))
             (key        (get-header request "sec-websocket-key"))
             (version    (get-header request "sec-websocket-version")))
         (and upgrade
              (header-has-token-p upgrade "websocket")
              connection
              (header-has-token-p connection "upgrade")
              key
              (= (length key) 24)  ; base64(16 bytes) per RFC 6455 §4.2.2
              ;; RFC 4648 standard base64 alphabet: A-Z / a-z / 0-9 / '+' / '/'
              ;; with '=' padding. Sec-WebSocket-Key is a fixed-size base64
              ;; over 16 random bytes, so the shape is fully determined —
              ;; 22 data characters and then exactly "==". Checked
              ;; positionally: a flat alphabet sweep across all 24 admits
              ;; '=' anywhere, which accepted a key of 24 '=' characters
              ;; while this comment claimed the positional strictness the
              ;; loop did not have.
              (string= key "==" :start1 22)
              (loop for i from 0 below 22
                    for c = (char key i)
                    always (or (char<= #\A c #\Z)
                               (char<= #\a c #\z)
                               (char<= #\0 c #\9)
                               (char= c #\+) (char= c #\/)))
              version
              (string= version "13")))))

(defun make-websocket-handshake-response (request)
  "Build the 101 Switching Protocols response for a WebSocket upgrade.
   Returns an HTTP-RESPONSE."
  (let* ((key (get-header request "sec-websocket-key"))
         (accept (websocket-accept-key key))
         (response (make-http-response :status 101)))
    (set-response-header response "upgrade" "websocket")
    (set-response-header response "connection" "Upgrade")
    (set-response-header response "sec-websocket-accept" accept)
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
      ;; in hand — no reason to wait. Checked below the availability test
      ;; it meant an unmasked frame was buffered to completion before
      ;; being refused, which is a peer choosing how much of our memory
      ;; to occupy with something we had already decided to reject.
      (unless masked
        (error "WebSocket: received unmasked client frame"))
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
      ;; Reject oversized frames early
      (when (> payload-length *max-ws-payload-size*)
        (error "WebSocket: frame too large (~d bytes, max ~d)"
               payload-length *max-ws-payload-size*))
      ;; Control frames (opcode >= 8): must have payload <= 125 and FIN=1
      ;; (RFC 6455 §5.5)
      (when (>= opcode 8)
        (when (> payload-length 125)
          (error "WebSocket: control frame payload too large (~d bytes, max 125)"
                 payload-length))
        (unless fin
          (error "WebSocket: fragmented control frame")))
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
  "Build a close frame with a status code. CODE must be in the
   server-sendable set per RFC 6455 §7.4.1: 1000-1003, 1007-1014,
   or 3000-4999. Codes < 1000, the reserved 1004/1005/1006/1015,
   the unassigned 1016-2999, and 5000+ are rejected here — the
   stricter counterpart to CLAMP-CLOSE-CODE on the receive path,
   which accepts any out-of-range peer code by clamping it to
   1000. A silent u16 truncation on out-of-range input (the old
   behavior for values > 65535) would put bytes on the wire the
   application never asked for."
  (unless (or (<= 1000 code 1003)
              (<= 1007 code 1014)
              (<= 3000 code 4999))
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
;;; Two callers: ws-handler, where the event loop is paused while the
;;; handler runs and there is no contention with pings or other writes; and
;;; a fetch callback on a :WEBSOCKET target, which runs on the outbound
;;; connection's read path. The second one is why the arming below exists.
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
;;; is STREAM-FLUSH's conditional and it is self-limiting: a peer keeping
;;; up costs no epoll_ctl at all, so a handler sending in a loop pays
;;; nothing, which was the whole objection to arming per frame. The fd
;;; comes from *EPOLL-FD*, the worker's own, bound for exactly this and
;;; NIL outside a worker — so the check is a check and not an assumption.
;;;
;;; Leaving it to HANDLE-CLIENT-READ was correct for one caller and wrong
;;; for the other. That site arms after a handler returns, and WS-SEND is
;;; also reachable from a fetch callback on a :WEBSOCKET target, which
;;; runs on the *outbound* connection's read path — where
;;; HANDLE-OUTBOUND-READ arms the outbound and nothing arms the target.
;;; HANDLE-CLIENT-READ does not run for it unless its peer happens to
;;; send something. Measured on that path: a 512 KiB frame, 444 KiB still
;;; queued, and not one epoll_ctl against the target. The tail of the last
;;; frame then waits for an event that is not coming, and
;;; *WRITE-STALL-TIMEOUT* closes the connection rather than flushing it.
;;; Every send had reported success and the peer got a truncated message,
;;; which is the failure this codebase refuses everywhere else.
;;;
;;; EPOLLOUT alone, not EPOLLIN with it. That is what HANDLE-CLIENT-READ
;;; arms for a :WEBSOCKET connection carrying a backlog, and
;;; HANDLE-CLIENT-WRITE restores EPOLLIN once the queue drains, so this
;;; enters a loop that already exists rather than adding a third mask
;;; convention to one state. A peer leaving while the connection is behind
;;; is still noticed: a closed socket reports writable, the write fails,
;;; and the connection goes. Adding EPOLLIN here would instead let a
;;; handler be re-entered against a full queue, which is where WS-SEND
;;; signals.
;;; ---------------------------------------------------------------------------

(defun ws-send (conn frame-bytes)
  "Queue FRAME-BYTES for CONN and flush as much as the socket takes now.
   FRAME-BYTES should be a byte vector from BUILD-WS-TEXT, BUILD-WS-FRAME, etc.
   Returns T if everything reached the kernel, NIL if a remainder is queued.

   Does not block. Bytes may still be in the queue when this returns, and
   the event loop flushes the rest — so a NIL return is the normal way a
   slow peer looks, not an error. Failures of the flush itself do surface
   here; failures of the deferred remainder surface on the event loop.

   Call it from within ws-handler, or from a fetch callback on a
   :WEBSOCKET target. Inside a handler the event loop is paused, so there
   is no write contention; from a fetch callback the target is a
   connection nothing else is writing to for the life of the fetch. A
   remainder is handed to the event loop the same way in both — see the
   header comment for why it has to be handed over here.

   Signals if the connection is already at *MAX-WRITE-BACKLOG*: the frame
   is not queued, not truncated, and the peer is far enough behind that
   dropping it silently would leave the app's view and the peer's view of
   the stream permanently different.

   Signals also if the arming fails, and that one is not symmetric with the
   first: the frame has been queued and flushed by then, so the raise
   reports that the *remainder* has no event coming, not that the send did
   not happen. On a share-nothing worker the way to provoke it is to call
   this for a connection that is not on this thread's epoll — an app
   reaching across workers, which was previously silent and appended to an
   unsynchronised queue. STREAM-SEND has carried the same behaviour through
   STREAM-FLUSH all along, so this is a new raise on this function rather
   than a new one in the API."
  (unless (plusp *write-stall-timeout*)
    (error "ws-send: *write-stall-timeout* is ~s; it must be positive. ~
            There is no unbounded setting, because it is the only ~
            deadline on a queue this connection may never drain."
           *write-stall-timeout*))
  (unless (connection-append-write conn frame-bytes)
    (error "ws-send: fd ~d is at *max-write-backlog* (~d bytes pending, ~
            frame is ~d); the peer is not draining."
           (connection-fd conn)
           (connection-write-pending conn)
           (length frame-bytes)))
  (let ((done (eq (connection-on-write conn) :done)))
    (unless (or done (null *epoll-fd*))
      (epoll-modify *epoll-fd* (connection-fd conn)
                    (logior +epollout+ +epollet+)))
    done))

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
;;; Parses frames from the read buffer and returns response bytes to write,
;;; or :CLOSE if the connection should be shut down.
;;; ---------------------------------------------------------------------------

(defun websocket-on-read (conn ws-handler)
  "Process WebSocket frames from CONN's read buffer.
   Returns a byte vector to write back, :CLOSE, or NIL (no response needed)."
  (let ((buf (connection-read-buf conn))
        (pos 0)
        (end (connection-read-pos conn))
        (responses nil))
    (labels ((close-with (code)
               ;; Tear-down helper for every error-close site. A
               ;; bare (values :close (build-ws-close NNNN)) would
               ;; drop the RESPONSES list populated by earlier frames
               ;; in the same batch — a reply from frame N-1 would be
               ;; lost if frame N had an RSV bit set. Funneling every
               ;; site through one helper keeps the concat-then-close
               ;; shape consistent with the normal close branch.
               (ws-shift-buffer conn buf pos end)
               (let ((close-frame (build-ws-close code)))
                 (if responses
                     (let* ((all (nreverse (cons close-frame responses)))
                            (total (reduce #'+ all :key #'length))
                            (out (make-array total
                                              :element-type '(unsigned-byte 8)))
                            (offset 0))
                       (dolist (r all)
                         (replace out r :start1 offset)
                         (incf offset (length r)))
                       (values :close out))
                     (values :close close-frame)))))
    (loop
      (multiple-value-bind (frame consumed)
          (handler-case
              (try-parse-ws-frame buf pos end)
            (error (e)
              ;; Protocol errors (RSV bits, oversized, unmasked, etc.)
              ;; → close with 1002 per RFC 6455 §7.1.7
              (log-warn "ws frame error fd ~d: ~a" (connection-fd conn) e)
              (return-from websocket-on-read (close-with 1002))))
        (unless frame
          ;; No complete frame — shift unconsumed bytes to start of buffer
          (when (> pos 0)
            (ws-shift-buffer conn buf pos end))
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
               (log-warn "ws new data frame mid-fragment fd ~d"
                         (connection-fd conn))
               (setf (connection-ws-frag-buf conn) nil
                     (connection-ws-frag-total conn) 0)
               (return-from websocket-on-read (close-with 1002)))
             (if (ws-frame-fin frame)
                 ;; Complete single-frame message
                 (progn
                   ;; RFC 6455 §5.6: text frames must contain valid UTF-8
                   (when (= opcode +ws-op-text+)
                     (handler-case
                         (sb-ext:octets-to-string (ws-frame-payload frame)
                                                   :external-format :utf-8)
                       (error ()
                         (log-warn "ws invalid UTF-8 in text frame fd ~d"
                                   (connection-fd conn))
                         (return-from websocket-on-read (close-with 1007)))))
                   (log-debug "ws recv opcode ~d (~d bytes) fd ~d"
                              opcode (length (ws-frame-payload frame))
                              (connection-fd conn))
                   (setf (connection-last-active conn) (get-universal-time))
                   (when ws-handler
                     (let ((response (funcall ws-handler conn frame)))
                       (when response (push response responses)))))
                 ;; First fragment — start accumulating. Enforce the
                 ;; message-size cap on the starting size too, not
                 ;; only on continuation accumulation, so apps that
                 ;; configure *max-ws-payload-size* higher than
                 ;; *max-ws-message-size* cannot slip a giant first
                 ;; fragment past the running-total check.
                 (let ((len (length (ws-frame-payload frame))))
                   (if (> len *max-ws-message-size*)
                       (progn
                         (log-warn "ws fragmented message too large ~
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
               (log-warn "ws continuation without start fd ~d"
                         (connection-fd conn))
               (return-from websocket-on-read (close-with 1002)))
             ;; Accumulate fragment — O(1) running total instead of re-scanning
             (push (ws-frame-payload frame) (connection-ws-frag-buf conn))
             (incf (connection-ws-frag-total conn) (length (ws-frame-payload frame)))
             (when (> (connection-ws-frag-total conn) *max-ws-message-size*)
               (log-warn "ws fragmented message too large (~d bytes) fd ~d"
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
                 (when (= (connection-ws-frag-opcode conn) +ws-op-text+)
                   (handler-case
                       (sb-ext:octets-to-string payload :external-format :utf-8)
                     (error ()
                       (log-warn "ws invalid UTF-8 in reassembled text fd ~d"
                                 (connection-fd conn))
                       (return-from websocket-on-read (close-with 1007)))))
                 (setf (connection-last-active conn) (get-universal-time))
                 (let ((complete (make-ws-frame
                                  :fin t
                                  :opcode (connection-ws-frag-opcode conn)
                                  :payload payload)))
                   (when ws-handler
                     (let ((response (funcall ws-handler conn complete)))
                       (when response (push response responses))))))))
            ;; Control frames (can arrive between fragments per RFC 6455 §5.4)
            ((= opcode +ws-op-ping+)
             (log-debug "ws ping from client fd ~d" (connection-fd conn))
             (push (build-ws-pong (ws-frame-payload frame)) responses))
            ((= opcode +ws-op-pong+)
             (log-debug "ws pong fd ~d" (connection-fd conn))
             (setf (connection-missed-pongs conn) 0))
            ((= opcode +ws-op-close+)
             (log-info "ws close requested on fd ~d" (connection-fd conn))
             (setf (connection-ws-frag-buf conn) nil
                   (connection-ws-frag-total conn) 0)
             (let* ((payload (ws-frame-payload frame))
                    ;; RFC 6455 §5.5.1: close body must be 0 or >= 2 bytes
                    (raw-code (cond
                                ((zerop (length payload)) 1000)
                                ((= (length payload) 1)
                                 (log-warn "ws close with 1-byte body fd ~d"
                                           (connection-fd conn))
                                 (return-from websocket-on-read
                                   (close-with 1002)))
                                (t (logior (ash (aref payload 0) 8)
                                           (aref payload 1)))))
                    (code (clamp-close-code raw-code)))
               ;; RFC 6455 §5.5.1: close reason text must be valid UTF-8
               (when (> (length payload) 2)
                 (handler-case
                     (sb-ext:octets-to-string payload :start 2
                                               :external-format :utf-8)
                   (error ()
                     (log-warn "ws invalid UTF-8 in close reason fd ~d"
                               (connection-fd conn))
                     (return-from websocket-on-read (close-with 1007)))))
               ;; Normal client-initiated close: CLOSE-WITH concatenates
               ;; any pending responses from earlier frames in this batch
               ;; with the close frame and shifts the buffer.
               (return-from websocket-on-read (close-with code))))
            (t
             ;; RFC 6455 §5.2: unknown opcodes MUST fail the connection
             (log-warn "ws unknown opcode ~d fd ~d" opcode (connection-fd conn))
             (return-from websocket-on-read (close-with 1002)))))))
      ;; Concatenate response frames into a single write buffer
      (when responses
        (setf responses (nreverse responses))
        (if (= (length responses) 1)
            (first responses)
            (let* ((total (reduce #'+ responses :key #'length))
                   (out (make-array total :element-type '(unsigned-byte 8)))
                   (offset 0))
              (dolist (r responses)
                (replace out r :start1 offset)
                (incf offset (length r)))
              out))))))
