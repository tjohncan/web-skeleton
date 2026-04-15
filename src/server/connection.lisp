(in-package :web-skeleton)

;;; ===========================================================================
;;; Connection object + state machine
;;;
;;; Each connection tracks its file descriptor, read/write buffers, and
;;; protocol state.  The event loop calls CONNECTION-ON-READ when epoll
;;; reports the fd is readable, and CONNECTION-ON-WRITE when writable.
;;; ===========================================================================

;;; CONNECTION-READ-AVAILABLE needs to see *MAX-OUTBOUND-RESPONSE-SIZE*
;;; (defined in src/server/fetch.lisp) to cap outbound reads separately
;;; from inbound request bodies. fetch.lisp loads after connection.lisp,
;;; so we proclaim the symbol special up front to silence the compile-
;;; time "undefined variable" warning — the actual defparameter lands
;;; before any call to CONNECTION-READ-AVAILABLE happens.
(declaim (special *max-outbound-response-size*))

;;; ---------------------------------------------------------------------------
;;; Connection struct
;;; ---------------------------------------------------------------------------

(defstruct connection
  ;; Identity
  (fd        -1  :type fixnum)               ; raw file descriptor
  (socket    nil)                             ; sb-bsd-sockets object (for accept)
  (remote-addr nil)                           ; peer IP as string, or NIL for outbound
  ;; Protocol state
  ;;   :read-http             — accumulating HTTP request bytes
  ;;   :read-body             — have headers, reading Content-Length body
  ;;   :sending-100-continue  — flushing interim status before body read
  ;;   :write-response        — sending HTTP response (keep-alive or close when done)
  ;;   :ws-upgrade            — sending WebSocket handshake
  ;;   :websocket             — reading/writing WebSocket frames
  ;;   :closing               — sending close frame (disconnect when done)
  ;;   :awaiting              — parked, waiting for outbound fetch to complete
  ;;   :out-dns               — outbound: getent subprocess resolving hostname
  ;;   :out-connecting        — outbound: TCP connect in progress
  ;;   :out-write             — outbound: sending HTTP request
  ;;   :out-read              — outbound: reading HTTP response
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
  (ws-frag-buf   nil :type list)              ; accumulated payload chunks, or NIL
  (ws-frag-total  0  :type fixnum)            ; running total bytes in frag-buf
  ;; DNS lookup (set during :out-dns phase on an outbound connection)
  (dns-process nil)                           ; sb-ext:process running getent
  (dns-then    nil :type (or null function))) ; (IP FAMILY) -> kick off TCP phase

;;; ---------------------------------------------------------------------------
;;; Constructor
;;; ---------------------------------------------------------------------------

(defun format-peer-addr (host port)
  "Format a (HOST PORT) pair as a peer-address string for log output.
   HOST is a 4-byte IPv4 vector or a 16-byte IPv6 vector; the v6 form
   uses the bracketed 8-group lowercase hex representation (RFC 5952).
   No :: compression — a log line doesn't need canonical form, just
   unambiguous identity."
  (case (length host)
    (4  (format nil "~{~d~^.~}:~d" (coerce host 'list) port))
    (16 (format nil "[~(~{~x~^:~}~)]:~d"
                (loop for i from 0 below 16 by 2
                      collect (logior (ash (aref host i) 8)
                                      (aref host (1+ i))))
                port))
    (t  (format nil "<addr>:~d" port))))

(defun make-client-connection (client-socket)
  "Wrap a newly accepted socket into a connection object.
   Sets the fd to non-blocking. Captures the peer address."
  (let ((fd (socket-fd client-socket))
        (addr (ignore-errors
                (multiple-value-bind (host port)
                    (sb-bsd-sockets:socket-peername client-socket)
                  (format-peer-addr host port)))))
    (set-nonblocking fd)
    (make-connection :fd fd
                     :socket client-socket
                     :remote-addr addr
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

(defun maybe-reap-dns-process (conn)
  "If CONN has an attached getent process (a :out-dns outbound during
   the DNS phase), kill it (if still running) and reap the zombie.
   Safe to call on any connection — no-op when there is no process.
   Idempotent: nulls the process slot so repeat calls are harmless.
   Called from every teardown path (CLOSE-OUTBOUND in fetch.lisp,
   CLOSE-CONNECTION in main.lisp) so a half-finished DNS lookup never
   leaks a zombie process. Lives here alongside CONNECTION-CLOSE for
   load-order symmetry with both callers."
  (let ((process (connection-dns-process conn)))
    (when process
      (ignore-errors
       (when (sb-ext:process-alive-p process)
         (sb-ext:process-kill process 9)))
      (ignore-errors (sb-ext:process-close process))
      (setf (connection-dns-process conn) nil))))

;;; ---------------------------------------------------------------------------
;;; Read buffer helpers
;;; ---------------------------------------------------------------------------

(defun connection-read-available (conn)
  "Drain all available bytes from fd into read buffer (edge-triggered).
   Grows the buffer as needed, up to the state-appropriate limit.
   Returns :OK if any data was read, :EOF, :FULL, or :AGAIN.

   The size cap is state- and direction-dependent:
     :websocket         → *max-ws-payload-size* + 14 (masked header)
     outbound response  → *max-outbound-response-size* (8 MiB default)
     inbound request    → *max-body-size*             (1 MiB default)
   Keeping the inbound and outbound caps separate means a 1 MiB+ HTTPS
   response (which a real upstream will routinely send) doesn't get
   truncated by the inbound request-body budget."
  (let ((any-read nil)
        (max-size (cond
                    ((eq (connection-state conn) :websocket)
                     (+ *max-ws-payload-size* 14))
                    ((connection-outbound-p conn)
                     *max-outbound-response-size*)
                    (t
                     *max-body-size*))))
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
            ((eq result :eof)   (return (if any-read :ok :eof)))
            ((eq result :again) (return (if any-read :ok :again)))
            (t (incf (connection-read-pos conn) result)
               (setf any-read t))))))))

;;; ---------------------------------------------------------------------------
;;; Extract Content-Length from raw header bytes
;;; ---------------------------------------------------------------------------

(defun scan-content-length (buf end)
  "Scan BUF[0..END) for a Content-Length header value.
   Returns the integer value, or NIL if not found.
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
                 (let ((value 0) (found nil) (digits 0))
                   (loop while (and (< pos end) (<= 48 (aref buf pos) 57))
                         do (incf digits)
                            (when (> digits 15)
                              (http-parse-error "Content-Length too large"))
                            (setf value (+ (* value 10) (- (aref buf pos) 48))
                                  found t)
                            (incf pos))
                   ;; Header name matched but no digits — reject
                   (unless found
                     (http-parse-error "invalid Content-Length value"))
                   ;; Skip optional trailing whitespace
                   (loop while (and (< pos end)
                                    (or (= (aref buf pos) 32)
                                        (= (aref buf pos) 9)))
                         do (incf pos))
                   ;; Next byte must be CR or end of scanned region
                   (when (and (< pos end)
                              (not (= (aref buf pos) 13)))
                     (http-parse-error "invalid Content-Length"))
                   (if result
                       (unless (= value result)
                         (http-parse-error "duplicate Content-Length"))
                       (setf result value))))))
    result))

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
;;; Expect: 100-continue
;;;
;;; When a request carries "Expect: 100-continue" and we intend to read the
;;; body, RFC 7231 §5.1.1 wants the server to answer with an interim
;;; "HTTP/1.1 100 Continue" status before the body arrives. Without it,
;;; curl / Go / Python / Java HTTP clients all pause 1-3s after sending
;;; headers on large POSTs before giving up and sending the body.
;;; ---------------------------------------------------------------------------

(defparameter *http-100-continue-bytes*
  (sb-ext:string-to-octets
   (format nil "HTTP/1.1 100 Continue~c~c~c~c"
           #\Return #\Newline #\Return #\Newline)
   :external-format :ascii)
  "Pre-built bytes for the HTTP/1.1 100 Continue interim response.
   Queued before reading the body when the client asks for it.")

(defun scan-expect-100-continue (buf end)
  "Return T if BUF[0..END) contains an Expect header whose value is
   100-continue (case-insensitive). Start-of-line match only — suffixed
   names like X-Expect do not match. The token terminator check blocks
   near-miss matches like 100-continued."
  (let ((name (load-time-value
               (sb-ext:string-to-octets "expect:"
                                         :external-format :ascii)))
        (token (load-time-value
                (sb-ext:string-to-octets "100-continue"
                                          :external-format :ascii))))
    (loop for i from 0 below end
          do (when (and (>= i 2)
                        (= (aref buf (- i 2)) 13)
                        (= (aref buf (- i 1)) 10)
                        (<= (+ i (length name)) end)
                        (loop for j below (length name)
                              for b = (aref buf (+ i j))
                              for n = (aref name j)
                              always (or (= b n)
                                         (and (<= 97 n 122)
                                              (= b (- n 32))))))
               (let ((pos (+ i (length name))))
                 ;; Skip OWS after the colon.
                 (loop while (and (< pos end)
                                  (or (= (aref buf pos) 32)
                                      (= (aref buf pos) 9)))
                       do (incf pos))
                 ;; Case-insensitive match against "100-continue".
                 (when (and (<= (+ pos (length token)) end)
                            (loop for k below (length token)
                                  for b = (aref buf (+ pos k))
                                  for n = (aref token k)
                                  always (or (= b n)
                                             (and (<= 97 n 122)
                                                  (= b (- n 32))))))
                   ;; Require a token terminator so 100-continued (etc.)
                   ;; does not match. Accepted terminators: CR (end of
                   ;; header line), SP/TAB (continuation), and ';'
                   ;; (header parameter start — 'Expect: 100-continue;q=1.0'
                   ;; is rare but permitted by RFC 7231).
                   (let ((after (+ pos (length token))))
                     (when (or (>= after end)
                               (let ((b (aref buf after)))
                                 (or (= b 13) (= b 32) (= b 9) (= b 59))))
                       (return t)))))))))

;;; ---------------------------------------------------------------------------
;;; State machine: on-read
;;;
;;; Called by the event loop when epoll reports EPOLLIN.
;;; Returns:
;;;   :CONTINUE      — stay in current state, wait for more data
;;;   :DISPATCH      — full HTTP request ready, route it
;;;   :SEND-CONTINUE — 100 Continue queued for an Expect: 100-continue
;;;                    request; caller must flip the fd to EPOLLOUT
;;;   :WEBSOCKET     — WebSocket frame(s) available to process
;;;   :CLOSE         — connection should be closed
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
           (http-parse-error "request too large (buffer full)")))
      (:again (when (zerop (connection-read-pos conn))
                (return-from connection-on-read :continue))))
    ;; Activity timestamp is NOT updated here on partial reads.
    ;; It resets only on complete request dispatch (in handle-client-read)
    ;; to prevent slowloris attacks from resetting the idle timer with
    ;; drip-fed bytes. WebSocket updates selectively in websocket-on-read.
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
                           (setf (connection-read-buf conn) new-buf))
                         ;; Re-drain: buffer grew past connection-read-available's
                         ;; original cap, kernel may still have data (edge-triggered)
                         (when (eq (connection-read-available conn) :eof)
                           (return-from connection-on-read :close))))
                     (let ((body-available (- (connection-read-pos conn) body-start)))
                       (setf (connection-body-expected conn) content-length
                             (connection-header-end conn) header-end)
                       (cond
                         ;; Already have the full body — dispatch even if
                         ;; Expect: 100-continue is set. The client chose
                         ;; not to wait, and sending 100 now is pointless.
                         ((>= body-available content-length)
                          (setf (connection-state conn) :read-body)
                          :dispatch)
                         ;; Body still incoming and the client asked us to
                         ;; confirm before sending it. Queue the interim
                         ;; response; the worker flushes it and the body
                         ;; arrives next.
                         ((scan-expect-100-continue (connection-read-buf conn)
                                                    header-end)
                          (connection-queue-write conn *http-100-continue-bytes*)
                          (setf (connection-state conn) :sending-100-continue)
                          :send-continue)
                         ;; Plain body wait.
                         (t
                          (setf (connection-state conn) :read-body)
                          :continue))))
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
    ;; RFC 7230 §5.4: HTTP/1.1 requests MUST have exactly one Host header
    (when (string= (http-request-version request) "1.1")
      (let ((host-count (length (get-headers request "host"))))
        (when (zerop host-count)
          (http-parse-error "missing Host header in HTTP/1.1 request"))
        (when (> host-count 1)
          (http-parse-error "duplicate Host header"))))
    (setf (connection-request conn) request)
    request))

;;; ---------------------------------------------------------------------------
;;; Request boundary
;;; ---------------------------------------------------------------------------

(defun connection-request-end (conn)
  "Byte offset past the complete HTTP request (headers + body).
   Used by keep-alive and ws-upgrade to find pipelined/extra data."
  (+ (connection-header-end conn) 4 (connection-body-expected conn)))

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
  "Handle writable event. Loops until EAGAIN or all bytes sent.
   Edge-triggered epoll requires draining writability in one pass."
  (let ((buf (connection-write-buf conn))
        (end (connection-write-end conn)))
    (loop
      (let* ((pos (connection-write-pos conn))
             (remaining (- end pos)))
        (when (zerop remaining)
          (return :done))
        (let ((result (nb-write (connection-fd conn) buf pos remaining)))
          (cond
            ((eq result :again) (return :continue))
            (t (incf (connection-write-pos conn) result))))))))
