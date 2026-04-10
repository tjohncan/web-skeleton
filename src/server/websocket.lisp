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

(defun connection-header-has-token-p (header-value token)
  "Check if TOKEN appears in a comma-separated header value (RFC 7230 §3.2.6).
   Comparison is case-insensitive, tokens are trimmed of whitespace.
   Zero intermediate string allocation."
  (let ((len (length header-value))
        (token-len (length token)))
    (loop with pos = 0
          while (< pos len)
          do ;; Skip leading whitespace
             (loop while (and (< pos len)
                              (let ((ch (char header-value pos)))
                                (or (char= ch #\Space) (char= ch #\Tab))))
                   do (incf pos))
             ;; Find end of token (comma or end of string)
             (let ((end (or (position #\, header-value :start pos) len)))
               ;; Trim trailing whitespace
               (let ((trimmed-end end))
                 (loop while (and (> trimmed-end pos)
                                  (let ((ch (char header-value (1- trimmed-end))))
                                    (or (char= ch #\Space) (char= ch #\Tab))))
                       do (decf trimmed-end))
                 ;; Compare bounded region case-insensitively
                 (when (and (= (- trimmed-end pos) token-len)
                            (string-equal header-value token
                                         :start1 pos :end1 trimmed-end))
                   (return t))
                 ;; Move past comma
                 (setf pos (1+ end)))))))

(defun websocket-upgrade-p (request)
  "Check if REQUEST is a valid WebSocket upgrade request."
  (and (eq (http-request-method request) :GET)
       (string= (http-request-version request) "1.1")
       (let ((upgrade    (get-header request "upgrade"))
             (connection (get-header request "connection"))
             (key        (get-header request "sec-websocket-key"))
             (version    (get-header request "sec-websocket-version")))
         (and upgrade
              (string-equal upgrade "websocket")
              connection
              (connection-header-has-token-p connection "upgrade")
              key
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
      ;; Determine payload length and header size
      (cond
        ((<= len7 125)
         (setf payload-length len7))
        ((= len7 126)
         (when (< available 4)
           (return-from try-parse-ws-frame (values nil 0)))
         (setf payload-length (logior (ash (aref buf (+ start 2)) 8)
                                      (aref buf (+ start 3)))
               header-size 4))
        ((= len7 127)
         (when (< available 10)
           (return-from try-parse-ws-frame (values nil 0)))
         (setf payload-length
               (loop for i from 0 below 8
                     sum (ash (aref buf (+ start 2 i)) (* 8 (- 7 i))))
               header-size 10)))
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
        ;; Client frames must be masked
        (unless masked
          (error "WebSocket: received unmasked client frame"))
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
  "Build a close frame with a status code."
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

(defparameter *ws-send-timeout* 10
  "Seconds before ws-send gives up writing a frame. 0 to disable.")

;;; ---------------------------------------------------------------------------
;;; Synchronous frame send
;;;
;;; Writes a complete WebSocket frame to a connection, blocking until
;;; all bytes are flushed.  Intended for use inside ws-handler — the
;;; event loop is paused while the handler runs, so there is no
;;; contention with pings or other writes.
;;; ---------------------------------------------------------------------------

(defun ws-send (conn frame-bytes)
  "Send FRAME-BYTES to CONN synchronously, blocking until fully written.
   FRAME-BYTES should be a byte vector from BUILD-WS-TEXT, BUILD-WS-FRAME, etc.
   Safe to call from within ws-handler — the event loop is paused while the
   handler runs, so there is no write contention.
   Signals an error if the write exceeds *ws-send-timeout*."
  (let ((fd (connection-fd conn))
        (pos 0)
        (end (length frame-bytes))
        (deadline (when (> *ws-send-timeout* 0)
                    (+ (get-universal-time) *ws-send-timeout*))))
    (loop while (< pos end)
          do (when (and deadline (> (get-universal-time) deadline))
               (error "ws-send: write timeout"))
             (let ((result (nb-write fd frame-bytes pos (- end pos))))
               (if (eq result :again)
                   (poll-writable fd 1000)
                   (incf pos result))))))

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
    (loop
      (multiple-value-bind (frame consumed)
          (try-parse-ws-frame buf pos end)
        (unless frame
          ;; No complete frame — shift unconsumed bytes to start of buffer
          (when (> pos 0)
            (let ((remaining (- end pos)))
              (when (> remaining 0)
                (replace buf buf :start1 0 :start2 pos :end2 end))
              (setf (connection-read-pos conn) remaining)))
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
               (setf (connection-ws-frag-buf conn) nil)
               (let ((remaining (- end pos)))
                 (when (> remaining 0)
                   (replace buf buf :start1 0 :start2 pos :end2 end))
                 (setf (connection-read-pos conn) remaining))
               (return-from websocket-on-read
                 (values :close (build-ws-close 1002))))
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
                         (let ((remaining (- end pos)))
                           (when (> remaining 0)
                             (replace buf buf :start1 0 :start2 pos :end2 end))
                           (setf (connection-read-pos conn) remaining))
                         (return-from websocket-on-read
                           (values :close (build-ws-close 1007))))))
                   (log-debug "ws recv opcode ~d (~d bytes) fd ~d"
                              opcode (length (ws-frame-payload frame))
                              (connection-fd conn))
                   (setf (connection-last-active conn) (get-universal-time))
                   (when ws-handler
                     (let ((response (funcall ws-handler conn frame)))
                       (when response (push response responses)))))
                 ;; First fragment — start accumulating
                 (progn
                   (setf (connection-ws-frag-opcode conn) opcode
                         (connection-ws-frag-buf conn)
                         (list (ws-frame-payload frame)))
                   (log-debug "ws frag start opcode ~d fd ~d"
                              opcode (connection-fd conn)))))
            ;; Continuation frame
            ((= opcode +ws-op-continuation+)
             (unless (connection-ws-frag-buf conn)
               (log-warn "ws continuation without start fd ~d"
                         (connection-fd conn))
               ;; Shift buffer, then signal close
               (let ((remaining (- end pos)))
                 (when (> remaining 0)
                   (replace buf buf :start1 0 :start2 pos :end2 end))
                 (setf (connection-read-pos conn) remaining))
               (return-from websocket-on-read
                 (values :close (build-ws-close 1002))))
             ;; Accumulate fragment
             (push (ws-frame-payload frame) (connection-ws-frag-buf conn))
             ;; Check total size
             (let ((total (reduce #'+ (connection-ws-frag-buf conn) :key #'length)))
               (when (> total *max-ws-payload-size*)
                 (log-warn "ws fragmented message too large (~d bytes) fd ~d"
                           total (connection-fd conn))
                 (setf (connection-ws-frag-buf conn) nil)
                 (let ((remaining (- end pos)))
                   (when (> remaining 0)
                     (replace buf buf :start1 0 :start2 pos :end2 end))
                   (setf (connection-read-pos conn) remaining))
                 (return-from websocket-on-read
                   (values :close (build-ws-close 1009)))))
             (when (ws-frame-fin frame)
               ;; Final fragment — reassemble and deliver
               (let* ((chunks (nreverse (connection-ws-frag-buf conn)))
                      (total (reduce #'+ chunks :key #'length))
                      (payload (make-array total :element-type '(unsigned-byte 8)))
                      (offset 0))
                 (dolist (chunk chunks)
                   (replace payload chunk :start1 offset)
                   (incf offset (length chunk)))
                 (setf (connection-ws-frag-buf conn) nil)
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
                       (let ((remaining (- end pos)))
                         (when (> remaining 0)
                           (replace buf buf :start1 0 :start2 pos :end2 end))
                         (setf (connection-read-pos conn) remaining))
                       (return-from websocket-on-read
                         (values :close (build-ws-close 1007))))))
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
             (setf (connection-ws-frag-buf conn) nil)
             (let* ((payload (ws-frame-payload frame))
                    (code (if (>= (length payload) 2)
                              (logior (ash (aref payload 0) 8) (aref payload 1))
                              1000))
                    (remaining (- end pos)))
               (when (> remaining 0)
                 (replace buf buf :start1 0 :start2 pos :end2 end))
               (setf (connection-read-pos conn) remaining)
               (return-from websocket-on-read
                 (values :close (build-ws-close code)))))
            (t
             (log-warn "ws unhandled opcode: ~d" opcode))))))
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
            out)))))
