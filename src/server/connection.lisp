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
  ;;   :write-response  — sending HTTP response (close when done)
  ;;   :ws-upgrade      — sending WebSocket handshake (switch to :websocket when done)
  ;;   :websocket       — reading/writing WebSocket frames
  ;;   :closing         — sending close frame (disconnect when done)
  (state     :read-http :type keyword)
  ;; Read buffer — accumulates incoming bytes
  ;; Sized for the largest legal WebSocket frame: max payload + 14 bytes
  ;; frame overhead (2 header + 8 extended length + 4 mask key).
  (read-buf  (make-array (+ *max-ws-payload-size* 14)
                         :element-type '(unsigned-byte 8))
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
  (missed-pongs  0 :type fixnum))

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
   Returns :OK if any data was read, :EOF, :FULL, or :AGAIN."
  (let ((any-read nil))
    (loop
      (let* ((buf (connection-read-buf conn))
             (pos (connection-read-pos conn))
             (space (- (length buf) pos)))
        (when (<= space 0)
          (return (if any-read :ok :full)))
        (let ((result (nb-read (connection-fd conn) buf pos space)))
          (cond
            ((eq result :eof)   (return :eof))
            ((eq result :again) (return (if any-read :ok :again)))
            (t (incf (connection-read-pos conn) result)
               (setf any-read t))))))))

;;; ---------------------------------------------------------------------------
;;; CRLFCRLF scanner
;;; ---------------------------------------------------------------------------

(defun scan-header-end (buf end)
  "Scan BUF[0..END) for the CRLFCRLF sequence (13 10 13 10).
   Returns the index of the first CR, or NIL."
  (loop for i from 0 to (- end 4)
        when (and (= (aref buf i)       13)
                  (= (aref buf (+ i 1)) 10)
                  (= (aref buf (+ i 2)) 13)
                  (= (aref buf (+ i 3)) 10))
          return i))

;;; ---------------------------------------------------------------------------
;;; Extract Content-Length from raw header bytes
;;; ---------------------------------------------------------------------------

(defun scan-content-length (buf header-end-pos)
  "Scan the header bytes in BUF for a Content-Length value.
   Returns the integer value, or 0 if not found."
  (let* ((header-str (sb-ext:octets-to-string buf :end header-end-pos
                                               :external-format :utf-8))
         (lower (string-downcase header-str))
         (cl-pos (search "content-length:" lower)))
    (if cl-pos
        (let* ((val-start (+ cl-pos (length "content-length:")))
               (val-end (or (position #\Return lower :start val-start)
                            (length lower)))
               (val (string-trim '(#\Space #\Tab)
                                 (subseq header-str val-start val-end))))
          (or (parse-integer val :junk-allowed t) 0))
        0)))

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
      (:full  (log-warn "read buffer full on fd ~d" (connection-fd conn))
              (return-from connection-on-read :close))
      (:again (return-from connection-on-read :continue)))
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
       (let ((header-end (scan-header-end (connection-read-buf conn)
                                          (connection-read-pos conn))))
         (if header-end
             ;; Found CRLFCRLF — check if there's a body to read
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
                     :dispatch)))
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
         (total-bytes (if (> content-length 0)
                          (+ body-start content-length)
                          (+ header-end 4)))  ; just headers + CRLFCRLF
         (raw-str (sb-ext:octets-to-string
                   (connection-read-buf conn)
                   :end total-bytes
                   :external-format :utf-8)))
    (let ((request (parse-request raw-str)))
      (setf (connection-request conn) request)
      request)))

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
