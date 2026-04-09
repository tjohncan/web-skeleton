(in-package :web-skeleton)

;;; ===========================================================================
;;; Connection object + state machine
;;;
;;; Each connection tracks its file descriptor, read/write buffers, and
;;; protocol state.  The event loop calls CONNECTION-ON-READ when epoll
;;; reports the fd is readable, and CONNECTION-ON-WRITE when writable.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Connection struct
;;; ---------------------------------------------------------------------------

(defstruct connection
  ;; Identity
  (fd        -1  :type fixnum)               ; raw file descriptor
  (socket    nil)                             ; sb-bsd-sockets object (for accept)
  ;; Protocol state
  ;;   :read-http       — accumulating HTTP request bytes
  ;;   :read-body       — have headers, reading Content-Length body
  ;;   :write-response  — sending HTTP response (keep-alive or close when done)
  ;;   :ws-upgrade      — sending WebSocket handshake (switch to :websocket when done)
  ;;   :websocket       — reading/writing WebSocket frames
  ;;   :closing         — sending close frame (disconnect when done)
  ;;   :awaiting        — parked, waiting for outbound fetch to complete
  ;;   :out-connecting   — outbound: TCP connect in progress
  ;;   :out-write        — outbound: sending HTTP request
  ;;   :out-read         — outbound: reading HTTP response
  (state     :read-http :type keyword)
  ;; Read buffer — accumulates incoming bytes, grows as needed
  (read-buf  (make-array 4096 :element-type '(unsigned-byte 8))
             :type (simple-array (unsigned-byte 8) (*)))
  (read-pos  0   :type fixnum)               ; bytes in read-buf so far
  ;; Write buffer — outgoing bytes to flush
  (write-buf nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (write-pos 0   :type fixnum)               ; bytes sent so far
  (write-end 0   :type fixnum)               ; total bytes to send
  ;; Parsed request (set once headers + body are complete)
  (request   nil :type (or null http-request))
  ;; Content-Length tracking (during :read-body state)
  (body-expected 0 :type fixnum)             ; Content-Length value
  (header-end    0 :type fixnum)             ; byte offset where body starts
  ;; Activity tracking (for idle timeout and ping/pong)
  (last-active   0 :type integer)             ; updated on real activity only
  (missed-pongs  0 :type fixnum)
  ;; Outbound fetch (when this connection IS an outbound call)
  (outbound-p     nil :type boolean)          ; T for outbound connections
  (inbound-fd     -1  :type fixnum)           ; fd of the parked inbound connection
  (fetch-callback nil)                        ; (status headers body) -> response
  ;; Awaiting (when this inbound connection is waiting for a fetch)
  (awaiting-fd    -1  :type fixnum)           ; fd of the outbound connection
  ;; Keep-alive
  (close-after-p  nil :type boolean)          ; T = close after response sent
  ;; WebSocket fragment reassembly
  (ws-frag-opcode  0  :type fixnum)           ; opcode from the first fragment
  (ws-frag-buf   nil :type list))              ; accumulated payload chunks, or NIL

;;; ---------------------------------------------------------------------------
;;; Constructor
;;; ---------------------------------------------------------------------------

(defun make-client-connection (client-socket)
  "Wrap a newly accepted socket into a connection object.
   Sets the fd to non-blocking."
  (let ((fd (socket-fd client-socket)))
    (set-nonblocking fd)
    (make-connection :fd fd
                     :socket client-socket
                     :last-active (get-universal-time))))

;;; ---------------------------------------------------------------------------
;;; Cleanup
;;; ---------------------------------------------------------------------------

(defun connection-close (conn)
  "Close a connection's file descriptor. Safe to call multiple times."
  (let ((fd (connection-fd conn)))
    (when (>= fd 0)
      (ignore-errors (%close fd))
      (setf (connection-fd conn) -1
            (connection-socket conn) nil
            (connection-state conn) :closing))))

;;; ---------------------------------------------------------------------------
;;; Read buffer helpers
;;; ---------------------------------------------------------------------------

(defun connection-read-available (conn)
  "Drain all available bytes from fd into read buffer (edge-triggered).
   Grows the buffer as needed, up to the state-appropriate limit.
   Returns :OK if any data was read, :EOF, :FULL, or :AGAIN."
  (let ((any-read nil)
        (max-size (if (eq (connection-state conn) :websocket)
                      (+ *max-ws-payload-size* 14)  ; max masked frame header
                      *max-body-size*)))
    (loop
      (let* ((buf (connection-read-buf conn))
             (pos (connection-read-pos conn))
             (space (- (length buf) pos)))
        (when (<= space 0)
          ;; Buffer full — grow or give up
          (if (>= (length buf) max-size)
              (return (if any-read :ok :full))
              (let* ((new-size (min (* (length buf) 2) max-size))
                     (new-buf (make-array new-size
                                          :element-type '(unsigned-byte 8))))
                (replace new-buf buf :end2 pos)
                (setf (connection-read-buf conn) new-buf
                      buf new-buf
                      space (- new-size pos)))))
        (let ((result (nb-read (connection-fd conn) buf pos space)))
          (cond
            ((eq result :eof)   (return :eof))
            ((eq result :again) (return (if any-read :ok :again)))
            (t (incf (connection-read-pos conn) result)
               (setf any-read t))))))))

;;; ---------------------------------------------------------------------------
;;; Extract Content-Length from raw header bytes
;;; ---------------------------------------------------------------------------

(defun scan-content-length (buf end)
  "Scan BUF[0..END) for a Content-Length header value.
   Returns the integer value, or 0 if not found.
   Signals http-parse-error on duplicate conflicting values
   (RFC 7230 §3.3.2 — prevents request smuggling).
   Operates on bytes directly — no string allocation."
  (let ((name (load-time-value
               (sb-ext:string-to-octets "content-length:"
                                         :external-format :ascii)))
        (result nil))
    (loop for i from 0 below end
          do (when (and ;; Only match at start of a header line (after CRLF)
                        (and (>= i 2)
                             (= (aref buf (- i 2)) 13)
                             (= (aref buf (- i 1)) 10))
                        ;; Case-insensitive match of "content-length:"
                        (<= (+ i (length name)) end)
                        (loop for j below (length name)
                              for b = (aref buf (+ i j))
                              for n = (aref name j)
                              always (or (= b n)
                                         (and (<= 97 n 122)
                                              (= b (- n 32))))))
               ;; Found — parse digits after optional whitespace
               (let ((pos (+ i (length name))))
                 ;; Skip OWS (spaces and tabs)
                 (loop while (and (< pos end)
                                  (or (= (aref buf pos) 32)
                                      (= (aref buf pos) 9)))
                       do (incf pos))
                 ;; Parse decimal digits
                 (let ((value 0) (found nil))
                   (loop while (and (< pos end) (<= 48 (aref buf pos) 57))
                         do (setf value (+ (* value 10) (- (aref buf pos) 48))
                                  found t)
                            (incf pos))
                   (when found
                     ;; Reject trailing non-whitespace (e.g. "100foo")
                     (when (and (< pos end)
                                (let ((b (aref buf pos)))
                                  (not (or (= b 13) (= b 10)
                                           (= b 32) (= b 9)))))
                       (http-parse-error "invalid Content-Length"))
                     (if result
                         (unless (= value result)
                           (http-parse-error "duplicate Content-Length"))
                         (setf result value)))))))
    (or result 0)))

;;; ---------------------------------------------------------------------------
;;; Reject Transfer-Encoding (inbound chunked not implemented)
;;; ---------------------------------------------------------------------------

(defun scan-transfer-encoding (buf end)
  "Return T if BUF[0..END) contains a Transfer-Encoding header.
   We do not implement inbound chunked decoding — requests carrying
   Transfer-Encoding are rejected to prevent CL-TE smuggling."
  (let ((name (load-time-value
               (sb-ext:string-to-octets "transfer-encoding:"
                                         :external-format :ascii))))
    (loop for i from 0 below end
          thereis (and (and (>= i 2)
                            (= (aref buf (- i 2)) 13)
                            (= (aref buf (- i 1)) 10))
                       (<= (+ i (length name)) end)
                       (loop for j below (length name)
                             for b = (aref buf (+ i j))
                             for n = (aref name j)
                             always (or (= b n)
                                        (and (<= 97 n 122)
                                             (= b (- n 32)))))))))

;;; ---------------------------------------------------------------------------
;;; State machine: on-read
;;;
;;; Called by the event loop when epoll reports EPOLLIN.
;;; Returns:
;;;   :CONTINUE  — stay in current state, wait for more data
;;;   :DISPATCH  — full HTTP request ready, route it
;;;   :WEBSOCKET — WebSocket frame(s) available to process
;;;   :CLOSE     — connection should be closed
;;; ---------------------------------------------------------------------------

(defun connection-on-read (conn)
  "Handle readable event. Reads available data and advances protocol state."
  (let ((read-result (connection-read-available conn)))
    (case read-result
      (:eof   (return-from connection-on-read :close))
      (:full
       ;; For WebSocket, let frames be parsed and shifted before giving up
       (if (eq (connection-state conn) :websocket)
           (return-from connection-on-read :websocket)
           (progn
             (log-warn "read buffer full on fd ~d" (connection-fd conn))
             (return-from connection-on-read :close))))
      (:again (when (zerop (connection-read-pos conn))
                (return-from connection-on-read :continue))))
    ;; Update activity timestamp for HTTP states only.
    ;; WebSocket updates selectively in websocket-on-read
    ;; (application frames count, protocol pongs don't).
    (unless (eq (connection-state conn) :websocket)
      (setf (connection-last-active conn) (get-universal-time)))
    ;; We have new data — check state.
    ;; Only :read-http, :read-body, and :websocket watch EPOLLIN.
    ;; Other states (:write-response, :ws-upgrade, :closing) watch EPOLLOUT only.
    (ecase (connection-state conn)
      (:read-http
       (let ((header-end (scan-crlf-crlf (connection-read-buf conn)
                                          0 (connection-read-pos conn))))
         (if header-end
             (progn
               ;; Found CRLFCRLF — reject Transfer-Encoding (not implemented)
               (when (scan-transfer-encoding (connection-read-buf conn) header-end)
                 (http-parse-error "Transfer-Encoding not supported"))
               ;; Check if there's a body to read
               (let* ((body-start (+ header-end 4))
                      (content-length (scan-content-length
                                       (connection-read-buf conn) header-end)))
               (if (and content-length (> content-length 0))
                   ;; Need to read a body
                   (progn
                     ;; Reject oversized bodies before allocating
                     (when (> content-length *max-body-size*)
                       (http-parse-error "body too large (~d bytes, max ~d)"
                                         content-length *max-body-size*))
                     ;; Grow read buffer if needed
                     (let ((total-needed (+ body-start content-length)))
                       (when (> total-needed (length (connection-read-buf conn)))
                         (let ((new-buf (make-array total-needed
                                                    :element-type '(unsigned-byte 8)
                                                    :initial-element 0)))
                           (replace new-buf (connection-read-buf conn)
                                    :end2 (connection-read-pos conn))
                           (setf (connection-read-buf conn) new-buf))))
                     (let ((body-available (- (connection-read-pos conn) body-start)))
                       (setf (connection-state conn) :read-body
                             (connection-body-expected conn) content-length
                             (connection-header-end conn) header-end)
                       (if (>= body-available content-length)
                           :dispatch     ; already have the full body
                           :continue)))  ; need more bytes
                   ;; No body — request is complete
                   (progn
                     (setf (connection-header-end conn) header-end)
                     :dispatch))))
             ;; No CRLFCRLF yet — keep reading
             :continue)))
      (:read-body
       (let* ((body-start (+ (connection-header-end conn) 4))
              (body-available (- (connection-read-pos conn) body-start)))
         (if (>= body-available (connection-body-expected conn))
             :dispatch
             :continue)))
      (:websocket
       :websocket))))

;;; ---------------------------------------------------------------------------
;;; Parse the buffered request
;;; ---------------------------------------------------------------------------

(defun connection-parse-request (conn)
  "Parse the complete HTTP request from the read buffer.
   Sets the connection's request field. Returns the request or signals error."
  (let* ((header-end (connection-header-end conn))
         (body-start (+ header-end 4))
         (content-length (connection-body-expected conn))
         (request (parse-request-bytes (connection-read-buf conn)
                                       0 body-start)))
    (when (> content-length 0)
      (setf (http-request-body request)
            (subseq (connection-read-buf conn) body-start
                    (+ body-start content-length))))
    ;; RFC 7230 §5.4: HTTP/1.1 requests MUST include a Host header
    (when (and (string= (http-request-version request) "1.1")
               (not (get-header request "host")))
      (http-parse-error "missing Host header in HTTP/1.1 request"))
    (setf (connection-request conn) request)
    request))

;;; ---------------------------------------------------------------------------
;;; State machine: queue write
;;; ---------------------------------------------------------------------------

(defun connection-queue-write (conn bytes)
  "Queue BYTES for writing. Caller is responsible for setting state."
  (setf (connection-write-buf conn) bytes
        (connection-write-pos conn) 0
        (connection-write-end conn) (length bytes)))

;;; ---------------------------------------------------------------------------
;;; State machine: on-write
;;;
;;; Returns:
;;;   :CONTINUE — more bytes to write, keep watching EPOLLOUT
;;;   :DONE     — all bytes sent; caller checks state to decide next action
;;; ---------------------------------------------------------------------------

(defun connection-on-write (conn)
  "Handle writable event. Flushes the write buffer."
  (let* ((buf (connection-write-buf conn))
         (pos (connection-write-pos conn))
         (remaining (- (connection-write-end conn) pos)))
    (when (zerop remaining)
      (return-from connection-on-write :done))
    (let ((result (nb-write (connection-fd conn) buf pos remaining)))
      (cond
        ((eq result :again) :continue)
        (t
         (incf (connection-write-pos conn) result)
         (if (>= (connection-write-pos conn) (connection-write-end conn))
             :done
             :continue))))))
