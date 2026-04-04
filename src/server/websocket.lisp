(in-package :web-skeleton)

;;; ===========================================================================
;;; WebSocket Protocol (RFC 6455)
;;;
;;; Handshake, frame parser, frame writer, and connection loop.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defparameter *websocket-guid* "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "Magic GUID from RFC 6455 §4.2.2, used in the opening handshake.")

(defparameter *ws-op-text*  #x1)
(defparameter *ws-op-close* #x8)
(defparameter *ws-op-ping*  #x9)
(defparameter *ws-op-pong*  #xA)

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
  (payload #() :type (vector (unsigned-byte 8))))

;;; ---------------------------------------------------------------------------
;;; Frame reader
;;;
;;; Wire format (RFC 6455 §5.2):
;;;
;;;   Byte 0:  [FIN:1][RSV:3][OPCODE:4]
;;;   Byte 1:  [MASK:1][PAYLOAD-LEN:7]
;;;   Extended payload length (0, 2, or 8 bytes depending on len)
;;;   Masking key (4 bytes, present if MASK=1)
;;;   Payload data
;;;
;;; Client-to-server frames MUST be masked. We reject unmasked frames.
;;; ---------------------------------------------------------------------------

(defun read-n-bytes (stream n)
  "Read exactly N bytes from STREAM. Returns a byte vector."
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (let ((got (read-sequence buf stream)))
      (when (< got n)
        (error "WebSocket: connection closed mid-frame (got ~d of ~d bytes)" got n)))
    buf))

(defun read-websocket-frame (stream)
  "Read a single WebSocket frame from STREAM. Returns a WS-FRAME."
  ;; Byte 0: FIN + opcode
  (let* ((b0 (read-byte stream))
         (fin (logbitp 7 b0))
         (opcode (logand b0 #x0F))
         ;; Byte 1: MASK + payload length
         (b1 (read-byte stream))
         (masked (logbitp 7 b1))
         (len7 (logand b1 #x7F))
         ;; Extended length
         (payload-length
           (cond
             ((<= len7 125) len7)
             ((= len7 126)
              (let ((ext (read-n-bytes stream 2)))
                (logior (ash (aref ext 0) 8)
                        (aref ext 1))))
             ((= len7 127)
              (let ((ext (read-n-bytes stream 8)))
                (loop for i from 0 below 8
                      sum (ash (aref ext i) (* 8 (- 7 i))))))))
         ;; Masking key (4 bytes if masked)
         (mask-key (when masked (read-n-bytes stream 4)))
         ;; Payload
         (payload (read-n-bytes stream payload-length)))
    ;; Client frames must be masked
    (unless masked
      (error "WebSocket: received unmasked client frame"))
    ;; Unmask the payload: payload[i] XOR mask-key[i mod 4]
    (loop for i from 0 below payload-length
          do (setf (aref payload i)
                   (logxor (aref payload i)
                           (aref mask-key (mod i 4)))))
    (make-ws-frame :fin fin :opcode opcode :payload payload)))

;;; ---------------------------------------------------------------------------
;;; Frame writer
;;;
;;; Server-to-client frames are NOT masked (per spec).
;;; ---------------------------------------------------------------------------

(defun write-websocket-frame (stream opcode payload)
  "Write a WebSocket frame to STREAM. PAYLOAD is a byte vector."
  (let ((len (length payload)))
    ;; Byte 0: FIN=1, opcode
    (write-byte (logior #x80 opcode) stream)
    ;; Byte 1+: payload length (no mask bit — server frames are unmasked)
    (cond
      ((<= len 125)
       (write-byte len stream))
      ((<= len 65535)
       (write-byte 126 stream)
       (write-byte (logand #xFF (ash len -8)) stream)
       (write-byte (logand #xFF len) stream))
      (t
       (write-byte 127 stream)
       (loop for i from 7 downto 0
             do (write-byte (logand #xFF (ash len (* -8 i))) stream))))
    ;; Payload
    (write-sequence payload stream)
    (force-output stream)))

(defun send-ws-text (stream text)
  "Send a text message over a WebSocket connection."
  (write-websocket-frame stream *ws-op-text*
                         (sb-ext:string-to-octets text :external-format :utf-8)))

(defun send-ws-close (stream &optional (code 1000))
  "Send a close frame with a status code."
  (let ((payload (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref payload 0) (logand #xFF (ash code -8)))
    (setf (aref payload 1) (logand #xFF code))
    (write-websocket-frame stream *ws-op-close* payload)))

;;; ---------------------------------------------------------------------------
;;; Connection loop
;;; ---------------------------------------------------------------------------

(defun websocket-loop (stream)
  "Main WebSocket loop. Reads frames, handles control messages, echoes text."
  (log-info "ws connection established")
  (handler-case
      (loop
        (let ((frame (read-websocket-frame stream)))
          (cond
            ;; Text frame — echo it back
            ((= (ws-frame-opcode frame) *ws-op-text*)
             (let ((text (sb-ext:octets-to-string
                          (ws-frame-payload frame)
                          :external-format :utf-8)))
               (log-debug "ws recv: ~a" text)
               (send-ws-text stream text)
               (log-debug "ws sent: ~a" text)))
            ;; Ping — respond with pong (same payload)
            ((= (ws-frame-opcode frame) *ws-op-ping*)
             (log-debug "ws ping received")
             (write-websocket-frame stream *ws-op-pong*
                                    (ws-frame-payload frame)))
            ;; Close — send close back, exit loop
            ((= (ws-frame-opcode frame) *ws-op-close*)
             (log-info "ws close requested")
             (send-ws-close stream)
             (return))
            ;; Anything else — log and ignore
            (t
             (log-warn "ws unhandled opcode: ~d" (ws-frame-opcode frame))))))
    (end-of-file ()
      (log-info "ws client disconnected"))
    (error (e)
      (log-error "ws error: ~a" e))))
