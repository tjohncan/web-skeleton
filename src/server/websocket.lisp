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

(defun websocket-upgrade-p (request)
  "Check if REQUEST is a valid WebSocket upgrade request."
  (and (eq (http-request-method request) :GET)
       (let ((upgrade    (get-header request "upgrade"))
             (connection (get-header request "connection"))
             (key        (get-header request "sec-websocket-key"))
             (version    (get-header request "sec-websocket-version")))
         (and upgrade
              (string-equal upgrade "websocket")
              connection
              (search "upgrade" (string-downcase connection))
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
      ;; Account for mask key
      (when masked (incf header-size 4))
      ;; Check if we have the full frame
      (let ((frame-size (+ header-size payload-length)))
        (when (< available frame-size)
          (return-from try-parse-ws-frame (values nil 0)))
        ;; Client frames must be masked
        (unless masked
          (error "WebSocket: received unmasked client frame"))
        ;; Extract mask key and payload
        (let* ((mask-offset (- header-size 4))
               (mask-key (subseq buf (+ start mask-offset)
                                     (+ start mask-offset 4)))
               (payload-start (+ start header-size))
               (payload (make-array payload-length
                                    :element-type '(unsigned-byte 8))))
          ;; Copy and unmask
          (loop for i from 0 below payload-length
                do (setf (aref payload i)
                         (logxor (aref buf (+ payload-start i))
                                 (aref mask-key (mod i 4)))))
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
  "Build a ping frame with empty payload."
  (build-ws-frame +ws-op-ping+
                  (make-array 0 :element-type '(unsigned-byte 8))))

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
        (cond
          ((or (= (ws-frame-opcode frame) +ws-op-text+)
               (= (ws-frame-opcode frame) +ws-op-binary+))
           (log-debug "ws recv opcode ~d (~d bytes) fd ~d"
                      (ws-frame-opcode frame)
                      (length (ws-frame-payload frame))
                      (connection-fd conn))
           ;; Application frame — counts as activity
           (setf (connection-last-active conn) (get-universal-time))
           (when ws-handler
             (let ((response (funcall ws-handler conn frame)))
               (when response (push response responses)))))
          ((= (ws-frame-opcode frame) +ws-op-ping+)
           (log-debug "ws ping from client fd ~d" (connection-fd conn))
           (push (build-ws-pong (ws-frame-payload frame)) responses))
          ((= (ws-frame-opcode frame) +ws-op-pong+)
           ;; Pong received — client is alive, reset dead detection counter.
           ;; Does NOT count as user activity for idle timeout purposes.
           (log-debug "ws pong fd ~d" (connection-fd conn))
           (setf (connection-missed-pongs conn) 0))
          ((= (ws-frame-opcode frame) +ws-op-close+)
           (log-info "ws close requested on fd ~d" (connection-fd conn))
           ;; Shift buffer, then signal close
           (let ((remaining (- end pos)))
             (when (> remaining 0)
               (replace buf buf :start1 0 :start2 pos :end2 end))
             (setf (connection-read-pos conn) remaining))
           (return-from websocket-on-read
             (values :close (build-ws-close))))
          (t
           (log-warn "ws unhandled opcode: ~d" (ws-frame-opcode frame))))))
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
