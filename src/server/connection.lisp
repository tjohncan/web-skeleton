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
  (fetch-method   :GET :type keyword)         ; method of the outbound request
                                              ; — needed so COMPLETE-FETCH can
                                              ; exempt HEAD from its CL
                                              ; truncation guard (RFC 7230 §3.3.2:
                                              ; HEAD responses carry CL but no body)
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
      ;; process-close already closed the pipe fd — mark it so
      ;; connection-close's (when (>= fd 0) (%close fd)) skips
      ;; the double-close.
      (setf (connection-fd conn) -1)
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
  ;; Known coupling: the :read-http phase cap is *max-body-size*
  ;; which must also accommodate headers. At default 1 MiB this
  ;; is generous; an operator setting *max-body-size* very low
  ;; (e.g. 64 KB) while allowing large headers will hit the cap
  ;; before headers are fully buffered.
  (let ((any-read nil)
        (max-size (cond
                    ((eq (connection-state conn) :websocket)
                     (+ *max-ws-payload-size* 14))
                    ;; DNS pipe output is 'getent ahosts <host>' stdout —
                    ;; a handful of STREAM / DGRAM / RAW lines per
                    ;; address family, typically well under 1 KiB. Cap
                    ;; at 8 KiB to match RESOLVE-HOST-BLOCKING's
                    ;; explicit 8192-byte cap on the synchronous path;
                    ;; reusing *MAX-OUTBOUND-RESPONSE-SIZE* here would
                    ;; let a pathological NSS module produce an 8 MiB
                    ;; buffer for what is definitionally a few lines.
                    ((eq (connection-state conn) :out-dns)
                     8192)
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

(defun scan-content-length (buf end &optional (start 0))
  "Scan BUF[START..END) for a Content-Length header value.
   Returns the integer value, or NIL if not found.
   Signals http-parse-error on duplicate conflicting values
   (RFC 7230 §3.3.3 — 'if a message is received without
   Transfer-Encoding and with multiple Content-Length header
   fields having differing field-values', the smuggling vector).
   Operates on bytes directly — no string allocation."
  (let ((name (load-time-value
               (sb-ext:string-to-octets "content-length:"
                                         :external-format :ascii)))
        (result nil))
    (loop for i from start below end
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
                         ;; 10 decimal digits caps at 9_999_999_999 ≈ 9.3 GB
                         ;; which is still four orders of magnitude past
                         ;; any sensible *MAX-BODY-SIZE* default. The prior
                         ;; 15-digit cap permitted values up to 10^15 ~ 900 TB,
                         ;; which the real *MAX-BODY-SIZE* check always
                         ;; rejected anyway — so the per-digit loop was
                         ;; cosmetic, and an attacker sending
                         ;; 'Content-Length: 999999999999999' forced a
                         ;; bignum accumulator for free. Tightening to 10
                         ;; makes the per-digit cap do real work.
                         do (incf digits)
                            (when (> digits 10)
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

(defun scan-transfer-encoding (buf end &optional (start 0))
  "Return T if BUF[START..END) contains a Transfer-Encoding header.

   Used on two paths with different consequences:
     Inbound: any Transfer-Encoding is rejected as an unimplemented
       framing mode. We do not decode chunked request bodies —
       accepting one would expose the CL-TE smuggling gap that
       motivates the rejection.
     Outbound response: a present Transfer-Encoding means 'ignore
       any Content-Length' per RFC 7230 §3.3.3 (TE wins over CL).
       HANDLE-OUTBOUND-READ and COMPLETE-FETCH use this to select
       between the chunked decoder and the CL-bounded body slice.

   The function just answers 'is TE present?' — the policy
   decision about what to do with the answer lives at the call
   site. The older docstring only described the inbound path and
   misled a reader grepping scan-transfer-encoding to wonder why
   outbound chunked decoding worked at all."
  (let ((name (load-time-value
               (sb-ext:string-to-octets "transfer-encoding:"
                                         :external-format :ascii))))
    (loop for i from start below end
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
;;; Expect: disposition scanning (RFC 7231 §5.1.1)
;;;
;;; The Expect header carries one or more expectations. The only one
;;; defined by RFC 7231 is "100-continue" — the server MUST answer with
;;; an interim "HTTP/1.1 100 Continue" before the body arrives, or
;;; curl / Go / Python / Java clients pause 1-3s on large POSTs before
;;; giving up and sending the body anyway. Any other token is an
;;; "unknown expectation"; §5.1.1 says the server MAY respond with 417
;;; Expectation Failed. The framework always 417s — silently accepting
;;; unknown expectations is an interoperability hazard against strict
;;; upstreams. The 417 fires regardless of body presence so a GET with
;;; Expect: x-foo can't probe the handler while a POST with the same
;;; header gets rejected.
;;; ---------------------------------------------------------------------------

(defparameter *http-100-continue-bytes*
  (sb-ext:string-to-octets
   (format nil "HTTP/1.1 100 Continue~c~c~c~c"
           #\Return #\Newline #\Return #\Newline)
   :external-format :ascii)
  "Pre-built bytes for the HTTP/1.1 100 Continue interim response.
   Queued before reading the body when the client asks for it.")

(defun scan-expect-disposition (buf end &optional (start 0))
  "Classify the Expect header in BUF[START..END). Returns
     :100-CONTINUE — an Expect: 100-continue header (case-insensitive)
                     with a valid token terminator. Near-miss values
                     like 100-continued are classified as :UNKNOWN.
     :UNKNOWN      — an Expect header whose value is not 100-continue.
                     RFC 7231 §5.1.1 lets the server respond with 417
                     (MAY); the state machine uses this classification
                     to do so unconditionally.
     :NONE         — no Expect header present.
   Start-of-line match only — suffixed names like X-Expect do not
   match. First Expect header wins (early return)."
  (let ((name (load-time-value
               (sb-ext:string-to-octets "expect:"
                                         :external-format :ascii)))
        (token (load-time-value
                (sb-ext:string-to-octets "100-continue"
                                          :external-format :ascii))))
    (loop for i from start below end
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
               ;; Found an Expect header. Classify it.
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
                   ;; does not match. Accepted terminators:
                   ;;   CR (13)  — end of header line
                   ;;   SP (32)  — whitespace continuation
                   ;;   TAB (9)  — whitespace continuation
                   ;;   ';' (59) — header parameter start
                   ;;              (Expect: 100-continue;q=1.0 — rare
                   ;;               but permitted by RFC 7231)
                   ;;   ',' (44) — list separator. RFC 7231 §5.1.1
                   ;;              defines Expect as 1#expectation
                   ;;              (the RFC 7230 §7 list rule), so
                   ;;              Expect: 100-continue, foo is a
                   ;;              well-formed shape and the comma
                   ;;              terminates the 100-continue token.
                   (let ((after (+ pos (length token))))
                     (when (or (>= after end)
                               (let ((b (aref buf after)))
                                 (or (= b 13) (= b 32) (= b 9)
                                     (= b 59) (= b 44))))
                       (return-from scan-expect-disposition :100-continue))))
                 ;; Header present but value not 100-continue.
                 (return-from scan-expect-disposition :unknown))))
    :none))

;;; ---------------------------------------------------------------------------
;;; State machine: on-read
;;;
;;; Called by the event loop when epoll reports EPOLLIN.
;;; Returns:
;;;   :CONTINUE      — stay in current state, wait for more data
;;;   :DISPATCH      — full HTTP request ready, route it
;;;   :FLUSH-QUEUED  — bytes queued on the write buffer that must be
;;;                    flushed before further reads. Covers both the
;;;                    interim 100 Continue (Expect: 100-continue →
;;;                    body read resumes after flush) and the terminal
;;;                    417 (unknown Expect → close after flush).
;;;                    Caller must flip the fd to EPOLLOUT.
;;;   :WEBSOCKET     — WebSocket frame(s) available to process
;;;   :CLOSE         — connection should be closed
;;; ---------------------------------------------------------------------------

(defun connection-on-read (conn)
  "Handle readable event. Reads available data and advances protocol state."
  (let ((read-result (connection-read-available conn)))
    (case read-result
      (:eof
       ;; Peer closed. If user-space still has buffered bytes (a pipelined
       ;; second request shifted to offset 0 after keep-alive reset, or a
       ;; fire-and-close HTTP/1.0 client) let the state machine process
       ;; them first — closing unconditionally drops valid requests
       ;; whose bytes arrived before the FIN.
       (when (zerop (connection-read-pos conn))
         (return-from connection-on-read :close)))
      (:full
       (cond
         ;; WebSocket: let frames be parsed and shifted before giving up.
         ((eq (connection-state conn) :websocket)
          (return-from connection-on-read :websocket))
         ;; :read-body with a complete body means the buffer cap fired
         ;; after we already had everything we need (content-length
         ;; near *MAX-BODY-SIZE*, or pipelined bytes queued past the
         ;; body). Fall through to the state-machine dispatch check.
         ((and (eq (connection-state conn) :read-body)
               (>= (- (connection-read-pos conn)
                      (+ (connection-header-end conn) 4))
                   (connection-body-expected conn)))
          nil)
         (t
          (http-parse-error "request too large (buffer full)"))))
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
             (let* ((buf (connection-read-buf conn))
                    ;; Scanners match on CRLF-anchored header patterns,
                    ;; so starting at byte 0 lets a \r\n injected in the
                    ;; request-target create a fake header boundary before
                    ;; parse-request-bytes validates CTL bytes.
                    (req-line-end (scan-crlf buf 0 header-end)))
               (unless req-line-end
                 (http-parse-error "no CRLF in request line"))
               ;; Verify the request line ends with HTTP/1.x before
               ;; the scanners run. A \r\n injected in the request-
               ;; target makes scan-crlf anchor on the injection point,
               ;; producing a truncated "request line" with no version.
               ;; The scanners would then see the injected bytes as
               ;; real headers and act on a fake Content-Length. This
               ;; version check catches every injection shape: the
               ;; attacker cannot fit a valid HTTP/1.x suffix before
               ;; the injected CRLF without making the line valid.
               (let ((sp (position 32 buf :start 0 :end req-line-end
                                        :from-end t)))
                 (unless (and sp
                              (= (- req-line-end sp) 9)
                              (= (aref buf (+ sp 1)) 72)   ; H
                              (= (aref buf (+ sp 2)) 84)   ; T
                              (= (aref buf (+ sp 3)) 84)   ; T
                              (= (aref buf (+ sp 4)) 80)   ; P
                              (= (aref buf (+ sp 5)) 47)   ; /
                              (= (aref buf (+ sp 6)) 49)   ; 1
                              (= (aref buf (+ sp 7)) 46))  ; .
                   (http-parse-error "malformed request line"))
                 ;; Capture the minor-version byte (48 = HTTP/1.0,
                 ;; 49 = HTTP/1.1) so SCAN-EXPECT-100-CONTINUE can be
                 ;; gated on 1.1 below — RFC 7231 §5.1.1 scopes the
                 ;; interim 100 Continue response to HTTP/1.1.
                 (let ((minor-version-byte (aref buf (+ sp 8)))
                       (hdr-start (+ req-line-end 2)))
                 ;; Found CRLFCRLF — reject Transfer-Encoding (not implemented)
                 (when (scan-transfer-encoding buf header-end hdr-start)
                   (http-parse-error "Transfer-Encoding not supported"))
                 ;; Classify Expect once — disposition gates dispatch
                 ;; before the body-presence split so a no-body GET with
                 ;; Expect: x-foo 417s the same as a bodied POST does.
                 (let* ((body-start (+ header-end 4))
                        (content-length (scan-content-length
                                         buf header-end hdr-start))
                        (expect (scan-expect-disposition
                                 buf header-end hdr-start)))
                 (cond
                   ;; Unknown Expect → 417 regardless of body. RFC 7231
                   ;; §5.1.1 MAY; framework always 417s so a GET probe
                   ;; can't bypass the check a matching POST receives.
                   ;; Silently accepting unknown expectations is an
                   ;; interop hazard against strict upstreams.
                   ((eq expect :unknown)
                    ;; Build the 417 response dynamically via format-
                    ;; response so Date (RFC 7231 §7.1.1.2 MUST on
                    ;; origin-server status responses) is current on
                    ;; each reject. Pre-building at load time bakes a
                    ;; stale Date; the 417 path is rare enough that
                    ;; the per-request allocation is free. :head-only-p
                    ;; suppresses the body on HEAD + Expect shapes per
                    ;; RFC 7231 §4.3.2 ("MUST NOT send a message body")
                    ;; — the method bytes are at buf[0..3], cheaper to
                    ;; match inline than to wait for parse-request-bytes.
                    (let ((head-p (and (>= req-line-end 5)
                                       (= (aref buf 0) 72)   ; H
                                       (= (aref buf 1) 69)   ; E
                                       (= (aref buf 2) 65)   ; A
                                       (= (aref buf 3) 68)   ; D
                                       (= (aref buf 4) 32)))); SP
                      (connection-queue-write
                       conn
                       (format-response (make-error-response 417)
                                        :connection-hint :close
                                        :head-only-p head-p)))
                    (setf (connection-state conn) :write-response
                          (connection-close-after-p conn) t)
                    :flush-queued)
                   ;; Body present — read it, dispatching when complete.
                   ((and content-length (> content-length 0))
                    ;; Reject oversized bodies before allocating.
                    (when (> content-length *max-body-size*)
                      (http-parse-error "body too large (~d bytes, max ~d)"
                                        content-length *max-body-size*))
                    ;; Grow read buffer if needed.
                    (let ((total-needed (+ body-start content-length)))
                      (when (> total-needed (length (connection-read-buf conn)))
                        (let ((new-buf (make-array total-needed
                                                   :element-type '(unsigned-byte 8)
                                                   :initial-element 0)))
                          (replace new-buf (connection-read-buf conn)
                                   :end2 (connection-read-pos conn))
                          (setf (connection-read-buf conn) new-buf))
                        ;; Re-drain: buffer grew past connection-read-available's
                        ;; original cap, kernel may still have data (edge-triggered).
                        ;; :EOF (peer FIN'd) and :FULL (buffer at cap) are
                        ;; terminal only if the body is still incomplete.
                        ;; When CONTENT-LENGTH is at or near *MAX-BODY-SIZE*,
                        ;; the pre-grown buffer exceeds the drain cap and
                        ;; :FULL fires at pos = body-start+CL — which IS
                        ;; the complete body. Close only on truly incomplete
                        ;; bodies so that CL-at-cap requests still dispatch.
                        (case (connection-read-available conn)
                          ((:eof :full)
                           (when (< (- (connection-read-pos conn) body-start)
                                    content-length)
                             (return-from connection-on-read :close))))))
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
                        ;; Body still incoming and client asked for
                        ;; 100 Continue. RFC 7231 §5.1.1 scopes 1xx to
                        ;; HTTP/1.1; a 1.0 client with Expect gets its
                        ;; body read without the interim.
                        ((and (= minor-version-byte 49)
                              (eq expect :100-continue))
                         (connection-queue-write
                          conn *http-100-continue-bytes*)
                         (setf (connection-state conn) :sending-100-continue)
                         :flush-queued)
                        ;; Plain body wait.
                        (t
                         (setf (connection-state conn) :read-body)
                         :continue))))
                   ;; No body — request is complete.
                   (t
                    (setf (connection-header-end conn) header-end)
                    :dispatch))))))
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
