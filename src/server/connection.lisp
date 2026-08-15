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
  ;; Vectors queued behind write-buf, oldest first. write-buf is the head
  ;; of the queue and these follow it; CONNECTION-ON-WRITE promotes the
  ;; next one each time the head drains. Held by reference and never
  ;; written into — see CONNECTION-APPEND-WRITE for why that matters.
  (write-queue      nil :type list)
  (write-queue-tail nil :type list)          ; last cons of write-queue, for O(1) append
  (write-queued     0   :type fixnum)        ; unsent bytes in write-queue, head excluded
  ;; When this connection last made forward progress on its write backlog:
  ;; the moment the current episode began, or the last time bytes actually
  ;; left for the peer. Deliberately not LAST-ACTIVE, which HANDLE-CLIENT-WRITE
  ;; bumps on EPOLLOUT *entry* — that would refresh the deadline of exactly
  ;; the connection that is stuck. See the stall sweep in main.lisp.
  (write-progress-at 0 :type integer)
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
  ;; Resume offset for the chunked-completion walk on an outbound read
  ;; (see CHUNKED-BODY-COMPLETE-P). Everything before it is validated
  ;; chunk framing, so each chunk is walked once across the whole
  ;; transfer instead of the body being rescanned on every read. Fresh
  ;; per outbound connection — outbound connections are never reused —
  ;; so it needs no reset.
  (chunk-scan-pos  0  :type fixnum)
  ;; Keep-alive
  (close-after-p  nil :type boolean)          ; T = close after response sent
  ;; WebSocket fragment reassembly
  (ws-frag-opcode  0  :type fixnum)           ; opcode from the first fragment
  (ws-frag-buf   nil :type list)              ; accumulated payload chunks, or NIL
  (ws-frag-total  0  :type fixnum)            ; running total bytes in frag-buf
  ;; DNS lookup (set during :out-dns phase on an outbound connection)
  (dns-process nil)                           ; sb-ext:process running getent
  (dns-then    nil :type (or null function))  ; (IP FAMILY) -> kick off TCP phase
  (dns-host    nil :type (or null string)))   ; hostname being resolved — carried
                                              ; so the address filter and the log
                                              ; lines can name it when the getent
                                              ; output lands

;;; ---------------------------------------------------------------------------
;;; Constructor
;;; ---------------------------------------------------------------------------

(defun format-peer-addr (host port)
  "Format a (HOST PORT) pair as a peer-address string for log output.
   HOST is a 4-byte IPv4 vector or a 16-byte IPv6 vector. The v6 form is
   bracketed per RFC 3986 §3.2.2 so the port cannot be read as another
   hex group; the v4 form needs no brackets.

   The address itself is FORMAT-IP's job. This used to walk the sixteen
   bytes itself, which meant two implementations of one hex format — and
   a log line that disagreed with a filter decision about what an address
   even looked like would be a miserable thing to debug. Brackets and a
   port are the whole difference, so that is all this adds."
  (let ((addr (format-ip host)))
    (if (= (length host) 16)
        (format nil "[~a]:~d" addr port)
        (format nil "~a:~d" addr port))))

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

(defun connection-read-cap (conn)
  "Ceiling on CONN's read buffer, by state and direction:
     :websocket         → *max-ws-payload-size* + 14 (masked header)
     :out-dns           → 8 KiB
     outbound response  → *max-outbound-response-size* (8 MiB default)
     inbound request    → every inbound budget summed (~1.07 MiB default)

   Keeping the inbound and outbound caps separate means a 1 MiB+ HTTPS
   response (which a real upstream will routinely send) doesn't get
   truncated by the inbound request-body budget.

   Split out of CONNECTION-READ-AVAILABLE so the arithmetic can be
   asserted without an fd to read from."
  (cond
    ((eq (connection-state conn) :websocket)
     (+ *max-ws-payload-size* 14))
    ;; DNS pipe output is 'getent ahosts <host>' stdout — a handful of
    ;; STREAM / DGRAM / RAW lines per address family, typically well
    ;; under 1 KiB. Cap at 8 KiB to match RESOLVE-HOST-BLOCKING's
    ;; explicit 8192-byte cap on the synchronous path; reusing
    ;; *MAX-OUTBOUND-RESPONSE-SIZE* here would let a pathological NSS
    ;; module produce an 8 MiB buffer for what is definitionally a few
    ;; lines.
    ((eq (connection-state conn) :out-dns)
     8192)
    ((connection-outbound-p conn)
     *max-outbound-response-size*)
    ;; The inbound cap is the sum of the budgets that actually apply, not
    ;; *max-body-size* alone. Aliasing them made one knob quietly move
    ;; two: a JSON API tightening the body cap to 32 KiB also capped
    ;; total request bytes at 32 KiB, so a request with large-but-legal
    ;; headers — a fat cookie jar, a long Authorization, a proxy's
    ;; X-Forwarded-* chain — died on the buffer with a 400 that blamed
    ;; the body.
    ;;
    ;; Summed exactly rather than padded with slack, the same way the
    ;; :websocket arm spells out its 14-byte masked header. The last two
    ;; terms are why this is not simply the sum of two variables:
    ;; *max-total-header-bytes* counts header line bytes only, so the
    ;; request line and every CRLF fall outside it.
    (t
     (+ *max-body-size*
        *max-total-header-bytes*
        *max-request-line-length*
        (* 2 *max-header-count*)  ; CRLF ending each header
        4))))                     ; request-line and blank-line CRLFs

(defun connection-read-available (conn)
  "Drain all available bytes from fd into read buffer (edge-triggered).
   Grows the buffer as needed, up to CONNECTION-READ-CAP.

   Returns:
     :OK      — read some bytes, and the fd would block on the next read
     :OK-EOF  — read some bytes, and then hit end of stream
     :EOF     — read nothing, already at end of stream
     :AGAIN   — read nothing, would block
     :FULL    — buffer is at CONNECTION-READ-CAP with no room to grow

   :OK-EOF is separate from :OK because the difference is the whole
   framing on two paths, and collapsing them cost this framework two
   bugs.

   Both arise when a peer's last bytes and its end-of-stream arrive in the
   same wake-up, which is the ordinary shape on loopback and a coin-toss
   across a network. Reported as :OK, the EOF is simply discarded here,
   and every caller that treats end-of-stream as terminal loses it:

     A close-delimited outbound response — no Content-Length, no
     Transfer-Encoding — is framed by the close and nothing else.
     OUTBOUND-RESPONSE-COMPLETE-P says so in as many words and returns NIL
     forever, deferring to the caller's EOF branch. That branch was
     unreachable, so the fetch sat until *FETCH-TIMEOUT*.

     A `getent` pipe carries the same hazard, on a race rather than
     reliably. getent writes its output in one go and the EOF appears when
     it exits, so whether one drain sees both depends on whether it has
     exited by the time we read. Usually it has not: the data comes back
     :OK, the exit arrives as a later event, and HANDLE-DNS-READY's \"no
     usable address\" branch fires on a clean :EOF. When it has — a cached
     answer, a loaded box, any scheduling that lets it finish first — the
     two coalesce and that branch was skipped. The name it strands is one
     whose every address *FETCH-ADDRESS-FILTER* refused, so the failure
     lands on the SSRF-defense path and only sometimes.

   Neither is rescued by epoll. A TCP peer calling close(2) does not set
   EPOLLHUP — that flag means both directions are down, and a FIN alone
   does not qualify; measured, not assumed. The pipe does set it, and the
   :OUT-DNS branch of HANDLE-OUTBOUND-EVENT dispatches without looking at
   flags. So the one fd type that gets the signal is the one that ignores
   it.

   Subscribing to EPOLLRDHUP (0x2000) is the obvious alternative and is
   the wrong shape: it would mean re-registering every fd's event mask, it
   does nothing for the pipe, and it asks the kernel to tell us something
   we already know. The EOF is in hand at the moment this function
   returns. The defect is that the return value had nowhere to put it."
  (let ((any-read nil)
        (max-size (connection-read-cap conn)))
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
            ((eq result :eof)   (return (if any-read :ok-eof :eof)))
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
                              ;; An 11-digit length is >= 10 GB, which is
                              ;; over any *max-body-size* worth setting —
                              ;; 413 rather than 400, same as the body
                              ;; check that would have caught it if we
                              ;; had let the number finish parsing.
                              (http-reject 413 "Content-Length too large"))
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
    ;; :OK and :OK-EOF both fall through to the state machine below, and
    ;; deliberately: bytes are bytes, and a fire-and-close client whose
    ;; request arrived with its FIN still deserves an answer. The :EOF arm
    ;; matters only when there is nothing buffered to answer with.
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
          ;; The buffer is at CONNECTION-READ-CAP with no complete
          ;; request in it. Whatever the client is sending, there is
          ;; more of it than we will hold — 413, not 400.
          (http-reject 413 "request too large (buffer full)"))))
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
                 ;; RFC 7230 §3.3.1 names the code for this exactly: a
                 ;; server that receives a transfer coding it does not
                 ;; understand SHOULD answer 501. Not 411 — that is for
                 ;; refusing a request until it carries a Content-Length,
                 ;; and it would misdescribe a client whose framing is
                 ;; legal and simply unimplemented here.
                 (when (scan-transfer-encoding buf header-end hdr-start)
                   (http-reject 501 "Transfer-Encoding not supported"))
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
                      (http-reject 413 "body too large (~d bytes, max ~d)"
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
                        ;; :EOF and :OK-EOF (peer FIN'd, with or without
                        ;; bytes on the way past) and :FULL (buffer at cap)
                        ;; are terminal only if the body is still
                        ;; incomplete. :OK-EOF belongs here for the same
                        ;; reason :EOF does — a body that arrives with its
                        ;; own FIN and is still short of Content-Length is
                        ;; never going to be completed, and waiting for a
                        ;; wake-up that cannot come just holds the slot
                        ;; until the idle sweeper takes it.
                        ;; When CONTENT-LENGTH is at or near *MAX-BODY-SIZE*,
                        ;; the pre-grown buffer exceeds the drain cap and
                        ;; :FULL fires at pos = body-start+CL — which IS
                        ;; the complete body. Close only on truly incomplete
                        ;; bodies so that CL-at-cap requests still dispatch.
                        (case (connection-read-available conn)
                          ((:eof :ok-eof :full)
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
;;;
;;; WRITE-BUF is the head of a queue whose tail is WRITE-QUEUE. Two ways in:
;;;
;;;   CONNECTION-QUEUE-WRITE  — replaces the head, signals if anything is
;;;                             pending. The original, unchanged. A response
;;;                             is one vector and the caller owns the socket.
;;;   CONNECTION-APPEND-WRITE — adds behind whatever is pending. For producers
;;;                             that emit repeatedly without waiting for the
;;;                             peer: WS-SEND, and the streaming surfaces.
;;;
;;; Keeping both is deliberate. QUEUE-WRITE's guard is a real one — it caught
;;; the class of bug where a second response is built for a connection that
;;; never finished sending the first, and turning every caller into an append
;;; would convert that signal into a peer quietly receiving two responses
;;; concatenated. Callers that legitimately queue say so by name.
;;; ---------------------------------------------------------------------------

(defparameter *max-write-backlog* (* 2 1024 1024)
  "Maximum unsent bytes a single connection may hold, default 2 MiB.
   Counts the in-flight head plus everything queued behind it.

   Reached when a producer outruns the peer — an app pushing events
   faster than a phone on a train can read them. A send that would
   exceed the bound is refused whole rather than truncated, and the
   caller decides what that means (see CONNECTION-APPEND-WRITE).

   Must clear *MAX-WS-MESSAGE-SIZE* by at least 10 bytes — the largest
   header BUILD-WS-FRAME emits — or a maximal legal message cannot be
   sent even onto an empty queue. The receive path accepts a payload of
   exactly *MAX-WS-MESSAGE-SIZE*, and framing it for the trip back costs
   the extended-length header, so an echo handler in the default
   configuration would be handed a message it is then refused permission
   to return. Both limits are exported and tunable apart, which makes
   this a requirement to keep rather than an identity to lean on; the
   default leaves a full MiB of room rather than the ten bytes that
   would technically satisfy it.

   Per connection, so the worst case is this times *MAX-CONNECTIONS*
   times worker count — the same arithmetic as the read buffers, and
   it needs every connection to be simultaneously backed up to get
   there. Lower it if the deployment has many connections and a
   generous ulimit; raise it for few connections and bursty output.")

(defparameter *write-stall-timeout* 10
  "Seconds a connection may sit on a write backlog that is not moving
   before it is closed. Must be positive; START-SERVER enforces that.

   The time half of the pair whose byte half is *MAX-WRITE-BACKLOG*: too
   much queued, and queued too long. They share a prefix because they are
   two limits on one thing.

   This was *WS-SEND-TIMEOUT*, which bounded a blocking spin inside
   WS-SEND back when the thing at risk was the worker. WS-SEND queues and
   returns now, so what can go wrong is that one connection's queue never
   drains — and once SSE and chunked streams queue through the same path,
   a name saying 'ws' pointed operators at the wrong knob for every
   surface but the one it was named after. Renamed rather than widened
   quietly: an operator tuning a stalled SSE stream would never have
   looked at a WebSocket setting.

   Measured from the last forward progress on the backlog, never from the
   connection's last activity — HANDLE-CLIENT-WRITE bumps LAST-ACTIVE on
   EPOLLOUT *entry*, which would refresh the deadline of precisely the
   connection that is stuck. See CONNECTION-WRITE-PROGRESS-AT.

   An inactivity bound, not a total. It restarts on any byte the peer
   accepts, so a peer that trickles is never closed: its memory is capped
   by *MAX-WRITE-BACKLOG*, its time is not. Deliberate — the total bound
   it replaced was a total on the worker, which is the more expensive
   thing to hold.

   No setting disables it. Zero used to mean a worker pinned forever; it
   would now mean a connection holding a full backlog forever, against
   idle timeouts that are long by design on exactly the states most
   likely to build one.")

(defun connection-write-pending (conn)
  "Unsent bytes on CONN: what is left of the head, plus the queue behind it."
  (+ (- (connection-write-end conn) (connection-write-pos conn))
     (connection-write-queued conn)))

(defun connection-write-full-p (conn)
  "True when CONN is at or over *MAX-WRITE-BACKLOG*.

   A hint for a producer deciding whether to keep going at all, never a
   pre-flight check for one send. This answers about the bytes already
   queued; CONNECTION-APPEND-WRITE answers about the bytes being handed
   over, and only its return value is authoritative. With a byte of room
   left this reports NIL and the next 64 KiB append is still refused. A
   caller that reads a NIL here as permission and drops the append's
   return loses whatever it had read — the same invisible data loss that
   the close-on-full disposition below exists to avoid, arriving by the
   back door.

   The two dispositions differ and neither belongs here: an
   app-generated stream should close, because a dropped event is
   invisible to the client and its view diverges permanently; a relay
   should stop reading its upstream instead, because the client has not
   misbehaved and letting the upstream's TCP window fill turns a killed
   download into a slow one."
  (>= (connection-write-pending conn) *max-write-backlog*))

(defun connection-reset-write (conn)
  "Drop all write state. Used where a connection is recycled for its
   next request — keep-alive reset, ws-upgrade completion, 100-continue.

   The queue has to go with the head. Those sites used to zero the three
   buffer slots inline, which was the whole of the write state; leaving
   a queued vector behind now would flush it into the *next* response on
   the same socket, arriving as a prefix nobody sent."
  (setf (connection-write-buf conn) nil
        (connection-write-pos conn) 0
        (connection-write-end conn) 0
        (connection-write-queue conn) nil
        (connection-write-queue-tail conn) nil
        (connection-write-queued conn) 0))

(defun connection-append-write (conn bytes)
  "Queue BYTES behind whatever CONN has pending. Returns T if accepted,
   NIL if it would pass *MAX-WRITE-BACKLOG* — in which case nothing is
   queued and the caller must decide (CONNECTION-WRITE-FULL-P documents
   the two dispositions).

   Refusing whole rather than appending a prefix is the point: a
   truncated frame is a protocol error on the peer's side, while a
   refused one leaves the stream well-formed and short, which the
   caller can act on.

   BYTES is held by reference and never written into. Shared, reused
   vectors reach this queue — the pre-built ping frame, the 100-Continue
   bytes, the connection-limit refusal, and every static file's response,
   which is one vector served to every request for that file. Advancing
   through them is safe because only WRITE-POS moves and that lives on
   the connection. In-place compaction of the head would corrupt static
   serving for every subsequent request, permanently and invisibly, so
   this queue does not compact: it promotes.

   Does not write to the socket. The flush is a separate step so this
   can also serve the relay path, where the event loop is already
   turning and an opportunistic flush would be redundant work on a
   socket that is about to report writable anyway."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((len (length bytes)))
    (cond
      ((> (+ (connection-write-pending conn) len) *max-write-backlog*) nil)
      ;; Nothing outstanding — become the head. Skips a cons, and keeps
      ;; the common single-vector case identical in shape to QUEUE-WRITE.
      ;; Start of a backlog episode, so the stall clock starts here; an
      ;; append onto a queue that is already busy deliberately does not
      ;; restart it, or a producer that keeps pushing would hold a peer
      ;; that never reads alive indefinitely.
      ((zerop (connection-write-pending conn))
       (setf (connection-write-buf conn) bytes
             (connection-write-pos conn) 0
             (connection-write-end conn) len
             (connection-write-progress-at conn) (get-universal-time))
       t)
      (t
       (let ((cell (list bytes)))
         (if (connection-write-queue-tail conn)
             (setf (cdr (connection-write-queue-tail conn)) cell)
             (setf (connection-write-queue conn) cell))
         (setf (connection-write-queue-tail conn) cell))
       (incf (connection-write-queued conn) len)
       t))))

(defun connection-promote-write (conn)
  "Make the next queued vector the head. Returns NIL if the queue is empty."
  (let ((next (pop (connection-write-queue conn))))
    (when next
      (unless (connection-write-queue conn)
        (setf (connection-write-queue-tail conn) nil))
      (decf (connection-write-queued conn) (length next))
      (setf (connection-write-buf conn) next
            (connection-write-pos conn) 0
            (connection-write-end conn) (length next))
      t)))

(defun connection-queue-write (conn bytes)
  "Replace the write buffer with BYTES. Caller is responsible for
   setting state, and for the buffer having drained first — this
   queues nothing behind an in-flight write, it overwrites it.

   Calling with bytes still un-flushed signals rather than truncating.
   Every one of the current call sites is reached from a read state,
   a freshly-created outbound connection, or an inbound parked in
   :awaiting with its buffer already drained, so the invariant holds
   by construction — but by construction is a property of today's
   nineteen callers, not of the function. DRAIN-CONNECTIONS and
   PING-WS-CONNECTIONS both test (< write-pos write-end) before
   calling, which is the same invariant enforced two levels out; the
   next caller to skip that test would otherwise ship the peer a
   truncated frame followed by a whole one, and the corruption would
   surface as a protocol error somewhere else entirely.

   The guard counts the append queue too. A caller that means to queue
   has CONNECTION-APPEND-WRITE and says so; reaching this function with
   a queue behind the head means two producers believe they own the
   socket, which is the same mistake the guard already exists to catch."
  (let ((pending (connection-write-pending conn)))
    (when (plusp pending)
      (error "connection-queue-write would clobber ~d un-flushed byte~:p on fd ~d"
             pending (connection-fd conn))))
  (setf (connection-write-buf conn) bytes
        (connection-write-pos conn) 0
        (connection-write-end conn) (length bytes)
        ;; Guard above proves nothing was pending, so this is always the
        ;; start of an episode.
        (connection-write-progress-at conn) (get-universal-time)))

;;; ---------------------------------------------------------------------------
;;; State machine: on-write
;;;
;;; Returns:
;;;   :CONTINUE — more bytes to write, keep watching EPOLLOUT
;;;   :DONE     — all bytes sent; caller checks state to decide next action
;;; ---------------------------------------------------------------------------

(defun connection-on-write (conn)
  "Handle writable event. Loops until EAGAIN or all bytes sent.
   Edge-triggered epoll requires draining writability in one pass.

   Reads the head's slots each turn instead of hoisting them out of the
   loop. The hoist was correct while a connection could hold one vector
   and only one, but the head now moves underneath: it advances to the
   next queued vector as each drains, and an append can land while this
   loop is running. A stale END would stop the pass at the old vector's
   length and wait for an EPOLLOUT that — the socket having never
   reported unwritable — is not coming."
  (let ((wrote nil))
    (flet ((finish (result)
             ;; One timestamp per pass rather than per chunk. GET-UNIVERSAL-TIME
             ;; is a syscall and this loop runs per writable event; the stall
             ;; deadline is in seconds, so per-chunk resolution buys nothing.
             (when wrote
               (setf (connection-write-progress-at conn) (get-universal-time)))
             result))
      (loop
        (let* ((pos (connection-write-pos conn))
               (remaining (- (connection-write-end conn) pos)))
          (cond
            ((plusp remaining)
             (let ((result (nb-write (connection-fd conn) (connection-write-buf conn)
                                     pos remaining)))
               (if (eq result :again)
                   (return (finish :continue))
                   (progn (setf wrote t)
                          (incf (connection-write-pos conn) result)))))
            ;; Head drained; promote the next queued vector and keep writing.
            ;; The socket is still writable and will not say so a second time.
            ((connection-promote-write conn))
            (t (return (finish :done)))))))))
