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
  ;; Byte source and sink. NIL means the raw fd, via NB-READ / NB-WRITE.
  ;; A transport that is not the fd — TLS, whose bytes come out of
  ;; SSL_read rather than read(2) — installs a closure of the same
  ;; contract here, and the rest of the state machine does not change.
  ;;
  ;; INVARIANT: CONNECTION-READ-INTO and CONNECTION-WRITE-FROM are the
  ;; only paths to a connection's transport. Not a style preference —
  ;; it is load-bearing for TLS, and the reason is in
  ;; CONNECTION-READ-INTO's docstring. A "just this once" direct call at
  ;; some site that looks special is how it gets broken.
  (read-fn   nil :type (or null function))
  (write-fn  nil :type (or null function))
  ;; One step of a transport handshake, or NIL for a transport that has
  ;; none. Called with no arguments; answers :DONE, :WANT-READ,
  ;; :WANT-WRITE, or raises. Its presence is what tells
  ;; HANDLE-OUTBOUND-CONNECT that a completed TCP connect is not yet a
  ;; usable connection.
  (handshake-fn nil :type (or null function))
  ;; Transport teardown, or NIL. A transport that allocates anything whose
  ;; lifetime is the connection's -- a foreign staging buffer, an SSL --
  ;; puts its release here rather than trusting call sites to remember,
  ;; which is the argument NOTIFY-STREAM-CLOSED already won.
  (close-fn nil :type (or null function))
  ;; T while the epoll interest is armed on the opposite direction to the
  ;; one this connection's state implies -- a read waiting for writability,
  ;; or a write waiting for readability. Only a transport that can invert
  ;; ever sets it; it exists so the ordinary path pays no extra
  ;; EPOLL_CTL_MOD to restore an interest it never changed.
  (interest-inverted nil :type boolean)
  ;; Protocol state
  ;;   :read-http             — accumulating HTTP request bytes
  ;;   :read-body             — have headers, reading Content-Length body
  ;;   :sending-100-continue  — flushing interim status before body read
  ;;   :write-response        — sending HTTP response (keep-alive or close when done)
  ;;   :ws-upgrade            — sending WebSocket handshake
  ;;   :websocket             — reading/writing WebSocket frames
  ;;   :streaming             — response head sent, app is producing a body
  ;;   :closing               — sending close frame (disconnect when done)
  ;;   :awaiting              — parked, waiting for outbound fetch to complete
  ;;   :out-dns               — outbound: getent subprocess resolving hostname
  ;;   :out-connecting        — outbound: TCP connect in progress
  ;;   :out-handshake         — outbound: transport handshake in progress,
  ;;                            the one state where readable does not mean
  ;;                            read and writable does not mean write
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
  ;; How this request's body is framed on the wire. :LENGTH means
  ;; Content-Length and BODY-EXPECTED is its value; :CHUNKED means the
  ;; framing walk decides where the body ends and BODY-EXPECTED is 0 until
  ;; DECODE-CHUNKED-BODY runs, after which it is the decoded length.
  ;;
  ;; A keyword rather than a sentinel in BODY-EXPECTED, because "0" already
  ;; means "no body" on the Content-Length path and a chunked request whose
  ;; body has not arrived yet would be indistinguishable from one that has
  ;; none. Two states sharing one encoding is how a body gets skipped.
  (body-framing  :length :type keyword)
  ;; Content-Length tracking (during :read-body state)
  (body-expected 0 :type fixnum)             ; Content-Length value
  (header-end    0 :type fixnum)             ; byte offset where body starts
  ;; Byte offset one past the whole request on the wire — headers, the
  ;; CRLFCRLF, and every byte the body's framing occupies. Read by the
  ;; keep-alive reset and the ws-upgrade completion to find where pipelined
  ;; data begins.
  ;;
  ;; A separate quantity from BODY-EXPECTED, not a convenience. For a
  ;; Content-Length body the two agree — the wire length is the declared
  ;; length — and for a chunked one they cannot: the wire carries chunk
  ;; headers the decoded body does not, and a trailer section sits past
  ;; both. Computing this end from BODY-EXPECTED was an identity that held
  ;; only while every body was Content-Length framed. It does not hold now.
  ;;
  ;; Every path that completes a request states this before dispatch — not
  ;; "every path that sets HEADER-END", which was true until chunked
  ;; framing arrived and is now the one reading that leads somewhere
  ;; impossible: a chunked request sets HEADER-END at :READ-HTTP and cannot
  ;; know its boundary until the framing walk reaches the terminator in
  ;; :READ-BODY. Stating it at header-parse time is precisely the thing
  ;; that cannot be done.
  ;;
  ;; That is correctness, and it takes one test per completing path,
  ;; because each only exercises the keep-alive reset for the request shape
  ;; it pipelines behind. Measured, each setter self-assigned in turn:
  ;;
  ;;   the no-body path   TEST-HARNESS-PIPELINED-WITH-FIN-E2E fails on
  ;;     `server closed the connection`, and the run dies there.
  ;;     TEST-HARNESS-PIPELINED-AFTER-BODY-E2E prints nothing at all — it
  ;;     is registered one line later — and could not catch this even if
  ;;     it ran, because its second request carries Connection: close, so
  ;;     no bodiless request in it is ever followed by a keep-alive reset.
  ;;
  ;;   the Content-Length path   TEST-HARNESS-PIPELINED-AFTER-BODY-E2E
  ;;     fails while all five of PIPELINED-WITH-FIN pass. With the
  ;;     boundary merely wrong rather than absent, +3 or -3, it is a
  ;;     countable 1612 / 2 rather than a dead run.
  ;;
  ;;   the chunked path   the boundary is stated by
  ;;     CONNECTION-BODY-COMPLETE-P, where the framing walk finds it, and
  ;;     not by any arithmetic a caller could recompute. Five assertions
  ;;     across both files catch its removal — 1632 / 5 — because a
  ;;     boundary of 0 there takes the decoded body with it: the decode
  ;;     runs to REQUEST-END.
  ;;
  ;; Both resets clear this alongside BODY-EXPECTED, HEADER-END and
  ;; BODY-FRAMING. That is diagnosability rather than correctness, and no
  ;; test can distinguish it — neutering either reset changes nothing
  ;; today, because every path in states the boundary before anything
  ;; reads it, and :WEBSOCKET never returns to :READ-HTTP. It is not dead
  ;; and it is not load-bearing: it chooses what happens when a future
  ;; path forgets. Cleared to 0, a forgotten boundary shifts the next
  ;; request to offset 0 and fails unmissably; left over from the request
  ;; before, it fails at a plausible offset, quietly. Do not delete it as
  ;; dead, and do not trust it as a guarantee.
  ;;
  ;; CHUNK-SCAN-POS is deliberately *not* in that list, for a reason its
  ;; own comment gives: the resets would write the same 0 its setter
  ;; already writes, which changes no failure mode and hides the setter
  ;; from every test.
  (request-end   0 :type fixnum)
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
  ;; Resume offset for the chunked-completion walk (see
  ;; CHUNKED-BODY-COMPLETE-P). Everything before it is validated chunk
  ;; framing, so each chunk is walked once across the whole transfer
  ;; instead of the body being rescanned on every read.
  ;;
  ;; Used in both directions now. It used to say "fresh per outbound
  ;; connection — outbound connections are never reused — so it needs no
  ;; reset", which was true until a chunked *request* body used the same
  ;; walk. Inbound connections are reused, and a leftover cursor is the one
  ;; kind of wrong value nothing downstream can catch: the walk clamps a
  ;; resume that is too *low* up to START, and a cursor left from the
  ;; previous request is too *high*, so it passes straight through and the
  ;; walk begins past framing it never validated.
  ;;
  ;; Cleared where the framing is set up — the chunked arm of
  ;; CONNECTION-ON-READ — and deliberately NOT in the two connection
  ;; resets, though REQUEST-END beside it is cleared in both. The
  ;; difference is what the write would say: those resets clear REQUEST-END
  ;; to 0 where its setters write real boundaries, so they turn a forgotten
  ;; boundary from a plausible offset into an unmissable one. Here they
  ;; would write the same 0 the setter already writes, changing nothing and
  ;; masking the setter from every test — which is exactly what happened:
  ;; with both in place, neutering either one was silent, because each
  ;; covered the other.
  ;;
  ;; With one mechanism, TEST-HARNESS-CHUNKED-KEEPALIVE-E2E catches it —
  ;; 1633 / 2, measured. That test's first body is large on purpose; the
  ;; clamp hides a leftover cursor that is smaller than the next request's
  ;; body-start.
  (chunk-scan-pos  0  :type fixnum)
  ;; (CONN BYTES) per chunk as an outbound response arrives, or NIL to
  ;; buffer the whole body. Set from the continuation by INITIATE-FETCH.
  (fetch-on-body   nil :type (or null function))
  ;; T while ON-BODY has asked for backpressure: EPOLLIN is dropped and
  ;; the upstream's send window fills. FETCH-RESUME re-arms.
  (fetch-paused    nil :type boolean)
  ;; On an *inbound* connection: the fd of an outbound that paused while
  ;; relaying into it, or -1. The back-link exists because the pause is
  ;; recorded on the outbound and the event that should end it — this
  ;; connection's backlog draining — arrives here. Outbound connections
  ;; already carry INBOUND-FD; this is the other direction, and it is set
  ;; only while a pause is outstanding so nothing has to be cleaned up on
  ;; the ordinary path.
  (paused-outbound-fd -1 :type fixnum)
  ;; Streaming response — set while STATE is :streaming
  (stream-framing   nil :type (or null keyword))  ; :chunked or :close
  ;; (CONN REASON) called exactly once when the stream ends, however it
  ;; ends. Nulled as it fires, the same discipline the fetch callback
  ;; uses, because an app that releases a resource twice is worse off
  ;; than one that never hears.
  (stream-on-close  nil :type (or null function))
  ;; When the app last produced something for this stream. Not
  ;; LAST-ACTIVE, which HANDLE-CLIENT-WRITE bumps on EPOLLOUT entry: a
  ;; draining backlog would keep refreshing the deadline of a stream
  ;; whose producer has stopped, which is the same conflation
  ;; WRITE-PROGRESS-AT exists to break, one level up. The two clocks
  ;; answer different questions — are bytes leaving, and is anything
  ;; arriving to send.
  (stream-produced-at 0 :type integer)
  ;; Bytes to send when a stream goes quiet, or NIL for no keepalive.
  ;; Necessarily supplied from above: a chunked stream has no idle form
  ;; of its own, since the empty chunk is the terminator.
  (stream-keepalive nil :type (or null (simple-array (unsigned-byte 8) (*))))
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
  "Close a connection's file descriptor. Safe to call multiple times.

   Runs CLOSE-FN first, exactly once, and before the descriptor goes: a
   transport teardown that needs to talk to the peer has nothing to talk
   over afterwards. Cleared before it is called so a re-entrant close
   cannot run it twice, and a raise inside it is logged rather than
   propagated, because a failed release must not leave the descriptor
   open on the way out."
  (let ((fd (connection-fd conn)))
    (when (>= fd 0)
      (let ((release (connection-close-fn conn)))
        (when release
          (setf (connection-close-fn conn) nil)
          (handler-case (funcall release)
            (error (e)
              (log-warn "transport close failed on fd ~d: ~a" fd e)))))
      ;; Close through the socket object when there is one, rather than
      ;; %CLOSE on the raw descriptor. SB-BSD-SOCKETS arms a finalizer that
      ;; closes the fd when the socket is collected, and %CLOSE does not
      ;; disarm it: the number is freed, the kernel hands it to whoever
      ;; opens next, and a later GC closes it under them. The symptom is an
      ;; EBADF on a descriptor its owner never closed, arriving whenever a
      ;; GC happens to run -- which is nowhere near the code that caused it.
      (let ((socket (connection-socket conn)))
        (if socket
            (ignore-errors (sb-bsd-sockets:socket-close socket))
            (ignore-errors (%close fd))))
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

(defun connection-read-into (conn buffer start max-bytes)
  "Read from CONN's transport into BUFFER[START..START+MAX-BYTES).
   Returns bytes read, :AGAIN if it would block, :EOF at end of stream,
   or raises — NB-READ's contract, because the raw fd is the default and
   a second contract at this seam would be a second thing to get wrong.

   Every read of a connection goes through here, and that is an invariant
   rather than a tidiness. Edge-triggered epoll reports a transition, so
   readability has to be drained in one pass or the remainder waits for an
   event that will not come; CONNECTION-READ-AVAILABLE is that pass, and
   it terminates on :AGAIN. Under TLS a second buffer appears beneath the
   socket — SSL_read decrypts a whole record, and consuming part of it
   leaves the rest in OpenSSL's buffer with nothing left on the fd for
   epoll to notice. Mapping SSL_ERROR_WANT_READ to :AGAIN makes the
   existing drain loop enforce that too, at no cost and with no new
   discipline to remember.

   That structural closure holds only while this is the sole path in. A
   direct SSL_read anywhere else re-opens exactly the hang it removes, and
   it would do so intermittently, on records that happen to be larger than
   one buffer. It also covers WANT_READ alone: WANT_WRITE from a read is a
   direction inversion the state machine cannot express yet, and belongs
   to a later round rather than to this seam."
  (let ((fn (connection-read-fn conn)))
    (if fn
        (funcall fn buffer start max-bytes)
        (nb-read (connection-fd conn) buffer start max-bytes))))

(defun connection-write-from (conn buffer start nbytes)
  "Write BUFFER[START..START+NBYTES) to CONN's transport. Returns bytes
   written, :AGAIN if it would block, or raises — NB-WRITE's contract.

   The counterpart to CONNECTION-READ-INTO and the same invariant: every
   write of a connection goes through here. The write queue holds vectors
   by reference and never mutates them, which is what lets a partial write
   resume from an offset, and a transport that needs the bytes to sit still
   across a retry depends on that already."
  (let ((fn (connection-write-fn conn)))
    (if fn
        (funcall fn buffer start nbytes)
        (nb-write (connection-fd conn) buffer start nbytes))))

(defun connection-read-available (conn)
  "Drain all available bytes from the transport into the read buffer
   (edge-triggered).
   Grows the buffer as needed, up to CONNECTION-READ-CAP.

   Returns:
     :OK      — read some bytes, and the fd would block on the next read
     :OK-EOF  — read some bytes, and then hit end of stream
     :EOF     — read nothing, already at end of stream
     :AGAIN   — read nothing, would block
     :FULL    — buffer is at CONNECTION-READ-CAP with no room to grow
     :WANT-WRITE / :OK-WANT-WRITE
              — read nothing / read some, and the transport now needs the
                socket to become *writable* before this read can continue.
                Only a TLS-style transport produces these; a raw fd never
                does. See CONNECTION-READ-INTO.

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
        (let ((result (connection-read-into conn buf pos space)))
          (cond
            ((eq result :eof)   (return (if any-read :ok-eof :eof)))
            ((eq result :again) (return (if any-read :ok :again)))
            ;; The transport wants to send before it can read again. Told
            ;; apart from :AGAIN because the answer is different -- :AGAIN
            ;; waits for readability, this waits for writability and then
            ;; re-issues the *read*. Split into two verdicts for the same
            ;; reason :OK-EOF is split from :EOF: the caller has bytes to
            ;; process in one case and not the other, and collapsing that
            ;; distinction has cost this framework two bugs already.
            ((eq result :want-write)
             (return (if any-read :ok-want-write :want-write)))
            (t (incf (connection-read-pos conn) result)
               (setf any-read t))))))))

(defun connection-discard-available (conn sink)
  "Drain everything readable on CONN's fd into SINK and throw it away.

   Returns the same verdicts as CONNECTION-READ-AVAILABLE, and the cond
   below is deliberately the same shape so the two can be read side by
   side. The caller needs to tell 'the peer said something' from 'the
   peer is gone', and those two arrive in one wake-up often enough that
   collapsing them cost this framework two bugs already.

   Separate from CONNECTION-READ-AVAILABLE because that one accumulates
   into the connection's own read buffer — where, on a :STREAMING
   connection, the original request's bytes and any pipelined ones are
   still sitting at the offsets the keep-alive reset works from. Growing
   that buffer with data we mean to discard would both hold memory for
   the life of the stream and disturb those offsets.

   SINK is reused and never read, so one per worker is enough and it
   never needs clearing between uses."
  (declare (type (simple-array (unsigned-byte 8) (*)) sink))
  (let ((any-read nil))
    (loop
      (let ((result (connection-read-into conn sink 0 (length sink))))
        (cond
          ((eq result :eof)   (return (if any-read :ok-eof :eof)))
          ((eq result :again) (return (if any-read :ok :again)))
          (t (setf any-read t)))))))

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
;;; Transfer-Encoding: which coding, not merely whether one is present
;;;
;;; Three rules decide an inbound Transfer-Encoding. They are written as
;;; rules rather than left to fall out of the parse, because each refuses
;;; for a different reason and answers with a different code:
;;;
;;;   Transfer-Encoding and Content-Length together are refused, never
;;;     reconciled. RFC 7230 §3.3.3 says TE overrides CL. Every
;;;     request-smuggling CVE in the genre is two hops applying that rule
;;;     differently, or one of them not applying it at all. Applying it
;;;     here is what would make this server the hop that disagrees.
;;;   Only `chunked`, and only as the final coding. `gzip, chunked` is
;;;     legal and unimplemented; `chunked, gzip` is illegal.
;;;   Transfer-Encoding requires HTTP/1.1. Chunked is a 1.1 framing.
;;;
;;; The classification lives here and the status codes live at the call
;;; site, which is the split SCAN-CONTENT-LENGTH and
;;; SCAN-EXPECT-DISPOSITION already use.
;;; ---------------------------------------------------------------------------

(defun ascii-token-equal-p (buf start end token)
  "T when BUF[START..END) equals TOKEN, ASCII case folded.
   TOKEN is written lowercase; an uppercase byte matches the lowercase
   letter 32 above it."
  (and (= (- end start) (length token))
       (loop for j below (length token)
             for b = (aref buf (+ start j))
             for n = (char-code (char token j))
             always (or (= b n)
                        (and (<= 97 n 122) (= b (- n 32)))))))

(defun classify-transfer-coding (buf start end)
  "Classify the Transfer-Encoding field-value in BUF[START..END).
   Returns :CHUNKED, :UNSUPPORTED or :INVALID — SCAN-TRANSFER-ENCODING
   documents what each one means.

   The value is a comma-separated list (RFC 7230 §7 #rule), which permits
   empty elements, so `chunked,,` names one coding and not three. Skipping
   them is the grammar, not leniency: refusing them would refuse a legal
   message, and counting them would move `chunked` out of final position
   and refuse it for the wrong reason."
  (let ((codings nil)
        (pos start))
    (loop
      (let* ((comma (position 44 buf :start pos :end end))
             (bound (or comma end)))
        (multiple-value-bind (vs ve) (trim-ows-bounds buf pos bound)
          (when (> ve vs)
            (push (cons vs ve) codings)))
        (unless comma (return))
        (setf pos (1+ comma))))
    (setf codings (nreverse codings))
    (let* ((n (length codings))
           (chunked (loop for c in codings
                          for idx from 0
                          when (ascii-token-equal-p buf (car c) (cdr c) "chunked")
                          collect idx)))
      (cond
        ;; A Transfer-Encoding header naming no coding at all.
        ((zerop n) :invalid)
        ;; No chunked anywhere — `gzip`, `deflate`, `identity`. Legal
        ;; codings, none of them implemented here.
        ((null chunked) :unsupported)
        ;; RFC 7230 §3.3.1: chunked is applied once, and applied last.
        ;; Both violations are the same shape — a body whose framing
        ;; depends on which coding a reader believes is outermost.
        ((rest chunked) :invalid)
        ((/= (first chunked) (1- n)) :invalid)
        ;; Exactly `chunked`, alone: the one framing this can decode.
        ((= n 1) :chunked)
        ;; `gzip, chunked` — well formed, and chunked is final, but the
        ;; inner coding is not implemented, so the framing could be
        ;; walked and the body still could not be delivered.
        (t :unsupported)))))

(defun scan-transfer-encoding (buf end &optional (start 0))
  "Classify the Transfer-Encoding of the message headed in BUF[START..END).

     NIL           no Transfer-Encoding header.
     :CHUNKED      one header, whose value is the single token `chunked`.
     :UNSUPPORTED  a coding this framework does not implement — `gzip`, or
                   `gzip, chunked`. RFC 7230 §3.3.1 names 501 for exactly
                   this: a transfer coding the server does not understand.
     :INVALID      a framing no reader should try to reconcile: more than
                   one Transfer-Encoding header, chunked repeated or in a
                   non-final position, a value naming no coding, or a value
                   continued by obsolete line folding. 400.

   NIL is the only absent answer and every classification is true, so the
   outbound callers that test this for presence read it unchanged. There a
   present Transfer-Encoding means 'ignore any Content-Length' per RFC 7230
   §3.3.3, selecting the chunked decoder over the CL-bounded slice in
   HANDLE-OUTBOUND-READ and COMPLETE-FETCH.

   Repeated headers are :INVALID rather than combined. §3.3.1 defines the
   combination — the field-values join into one comma list — but computing
   it is the reconciliation step the first rule above exists to refuse.

   Obsolete line folding is :INVALID here even though PARSE-HEADERS-BYTES
   already rejects it, because that rejection fires at dispatch and this
   scan runs at CRLFCRLF, and this is the reader that decides how the body
   is framed. A folded

     Transfer-Encoding: chunked
      , gzip

   reads as `chunked` to a scan that stops at the first CRLF and as
   `chunked , gzip` to the parser. Refusing the fold is what keeps two
   readers of one header from disagreeing about a body's framing."
  (let ((name (load-time-value
               (sb-ext:string-to-octets "transfer-encoding:"
                                         :external-format :ascii)))
        (value-start nil)
        (count 0))
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
               (incf count)
               (unless value-start
                 (setf value-start (+ i (length name))))))
    (cond
      ((zerop count) nil)
      ((> count 1) :invalid)
      (t
       ;; The value runs to its line terminator. END is the CRLFCRLF
       ;; position, so when Transfer-Encoding is the *last* header its own
       ;; CR sits exactly at END — outside the [VALUE-START, END) every
       ;; other scanner here searches. That is why the fallback is END
       ;; rather than a defensive guess: END is precisely where the
       ;; terminator is when the search does not find one. SCAN-CRLF is
       ;; not used for the same reason — its bound stops at (1- END), so
       ;; it answers NIL for a Transfer-Encoding that happens to come last.
       (let ((line-end (or (position 13 buf :start value-start :end end)
                           end)))
         (cond
           ;; A bare CR makes this reader see a different value than
           ;; PARSE-HEADERS-BYTES will see, and this is the reader that
           ;; decides how the body is framed. Measured against each shape's
           ;; CR-free twin, on the code that had no arm here:
           ;;
           ;;   chunked<CR>,gzip   :CHUNKED     vs :INVALID       refuses less
           ;;   chu<CR>nked        :UNSUPPORTED vs :CHUNKED       refuses more
           ;;   gzip<CR>           :UNSUPPORTED vs :UNSUPPORTED   neither
           ;;
           ;; It moves in both directions and sometimes not at all, so the
           ;; direction is incidental: the disagreement is the fault. That
           ;; is the same argument the fold arm rests on, which is why the
           ;; two sit together.
           ;;
           ;; LINE-END < END is exactly "this CR is not the fallback", so
           ;; LINE-END+1 is in bounds whenever the test runs, and a real
           ;; line terminator has its LF there.
           ((and (< line-end end)
                 (/= (aref buf (1+ line-end)) 10))
            :invalid)
           ;; The value continues on a folded line this scan would not see.
           ((and (< (+ line-end 2) end)
                 (let ((b (aref buf (+ line-end 2))))
                   (or (= b 32) (= b 9))))
            :invalid)
           (t (classify-transfer-coding buf value-start line-end))))))))

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
;;; Is this request's body finished?
;;;
;;; One function, because there are two callers and they must not answer
;;; differently. CONNECTION-ON-READ asks twice per wake-up: once in the
;;; :FULL arm, deciding whether a buffer at its cap holds a complete
;;; request or is a 413, and once in the :READ-BODY arm, deciding whether
;;; to dispatch. Those two disagreeing is not hypothetical — the :FULL arm
;;; used to be Content-Length arithmetic, and a chunked request, whose
;;; BODY-EXPECTED is 0 until the decode runs, satisfies
;;;
;;;   (>= (- read-pos body-start) body-expected)
;;;
;;; unconditionally. A chunked body that filled the buffer would have
;;; fallen through the :FULL arm as *complete* while :READ-BODY answered
;;; :CONTINUE, and nothing could read further because the buffer was at
;;; cap: the connection would sit there until the idle sweeper took it,
;;; instead of answering 413.
;;; ---------------------------------------------------------------------------

(defun connection-body-complete-p (conn)
  "T when CONN's request body is entirely present, NIL to keep reading.

   Advances CHUNK-SCAN-POS and, on completion, states REQUEST-END — so
   this is the function that discharges 'every path that completes a
   request states the boundary before dispatch' for the chunked path.
   Calling it twice is cheap and answers the same thing: the cursor means
   the second walk revisits no chunk.

   Raises 400 on a trailer section, which is this framework's answer to
   one. Nothing here surfaces trailers to an app, so accepting them would
   silently discard data the client believed it sent — and consuming them
   would need its own bound and its own header-field validation, a second
   header parser whose disagreement with the first is the exact shape
   these rules exist to prevent. Refusing also means the boundary for a
   trailer-bearing request is never computed at all, which is the strongest
   possible answer to the smuggle this issue is named for."
  (let ((body-start (+ (connection-header-end conn) 4))
        (end (connection-read-pos conn)))
    (ecase (connection-body-framing conn)
      (:length
       (>= (- end body-start) (connection-body-expected conn)))
      (:chunked
       (multiple-value-bind (complete resume after-size-line)
           (chunked-body-complete-p (connection-read-buf conn)
                                    body-start end
                                    (connection-chunk-scan-pos conn))
         (setf (connection-chunk-scan-pos conn) resume)
         (when complete
           (multiple-value-bind (status offset)
               (chunked-trailer-status (connection-read-buf conn)
                                       after-size-line end)
             (ecase status
               ;; `...0 CRLF` and no more: the terminator of an empty
               ;; trailer section has not arrived. Answering complete here
               ;; would end the request two bytes early and hand those two
               ;; bytes to the next parse as a request line.
               (:incomplete nil)
               (:present
                (http-reject 400 "trailer section not accepted"))
               (:malformed
                (http-reject 400 "malformed chunked framing after terminator"))
               (:empty
                (setf (connection-request-end conn) offset)
                t)))))))))


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
         ;;
         ;; Through CONNECTION-BODY-COMPLETE-P rather than by recomputing
         ;; the Content-Length arithmetic here: that arithmetic answers
         ;; "yes" unconditionally for a chunked body, whose BODY-EXPECTED
         ;; is 0, so this arm would call a half-arrived chunked request
         ;; complete while :READ-BODY kept waiting for a terminator no
         ;; further read could deliver.
         ((and (eq (connection-state conn) :read-body)
               (connection-body-complete-p conn))
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
                 ;; Found CRLFCRLF — decide the Transfer-Encoding before
                 ;; anything else reads the body, because this is the
                 ;; header that says how the body is framed at all. Each
                 ;; refusal carries the code that describes it: 501 is
                 ;; RFC 7230 §3.3.1's answer for a transfer coding the
                 ;; server does not understand, and 400 is for a framing
                 ;; that two readers could resolve differently. Not 411 —
                 ;; that is for refusing a request until it carries a
                 ;; Content-Length, and it would misdescribe a client
                 ;; whose framing is legal and simply unimplemented here.
                 (let ((coding (scan-transfer-encoding buf header-end hdr-start)))
                   (when coding
                     ;; Transfer-Encoding is an HTTP/1.1 field, so this
                     ;; gates every coding and not only chunked: a 1.0
                     ;; request naming `gzip` answers 400 here rather than
                     ;; the 501 below, because the version is the reason it
                     ;; is refused and the coding never gets to matter.
                     ;;
                     ;; The test is for 1.1 rather than against 1.0 so that
                     ;; a version token this parser has not validated yet
                     ;; refuses too, instead of falling through on a byte
                     ;; that merely is not 48. HTTP/1.9 with a
                     ;; Transfer-Encoding is the shape that separates the
                     ;; two.
                     ;;
                     ;; And 400 rather than 505 for that shape, though
                     ;; PARSE-REQUEST-BYTES answers 505 for the same request
                     ;; without the header. The asymmetry is deliberate.
                     ;; This gate answers "is this 1.1", which is the only
                     ;; question a framing decision needs, and it declines
                     ;; to answer "is this version supported" — that set
                     ;; belongs to PARSE-REQUEST-BYTES, and a second copy of
                     ;; it here is the two-readers disagreement these rules
                     ;; exist to prevent, one release away from mattering.
                     ;; The framing has to be settled at CRLFCRLF because it
                     ;; decides how many body bytes to wait for; the version
                     ;; fault is answered at dispatch, where it is owned.
                     (unless (= minor-version-byte 49)
                       (http-reject 400 "Transfer-Encoding requires HTTP/1.1"))
                     (ecase coding
                       (:invalid
                        (http-reject 400 "malformed Transfer-Encoding"))
                       (:unsupported
                        (http-reject 501 "Transfer-Encoding not supported"))
                       (:chunked
                        ;; RFC 7230 §3.3.3: Transfer-Encoding overrides
                        ;; Content-Length. Refuse rather than apply that
                        ;; rule — a front end applying it differently, or
                        ;; not at all, is the whole smuggling genre, and
                        ;; a server that never resolves the pair cannot
                        ;; be the half that resolves it wrongly.
                        ;;
                        ;; Only *presence* is the violation here; the value
                        ;; is never consulted, so a Content-Length this
                        ;; server refuses to parse is still a Content-Length
                        ;; that is present. Letting SCAN-CONTENT-LENGTH's
                        ;; own rejections through would answer 413 or 400
                        ;; for the value on a request whose actual fault is
                        ;; the pair — naming the wrong condition, and
                        ;; making the rule above true only for
                        ;; Content-Lengths that happen to parse. Catching
                        ;; keeps one reader of the header rather than
                        ;; adding a second that could disagree with it
                        ;; about presence.
                        (when (handler-case
                                  (and (scan-content-length
                                        buf header-end hdr-start)
                                       t)
                                (http-parse-error () t))
                          (http-reject 400
                                       "Transfer-Encoding with Content-Length"))
                        ;; Accepted. The body arm is in the cond below,
                        ;; reached by falling through rather than by
                        ;; returning here, so a chunked request passes the
                        ;; same Expect gate a Content-Length one does — a
                        ;; chunked POST with Expect: x-foo has to 417 for
                        ;; the same reason a bodied POST does.
                        )))
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
                   ;; Chunked body. Ahead of the Content-Length arm rather
                   ;; than inside it, because a chunked request declares no
                   ;; length anywhere — CONTENT-LENGTH is NIL here by
                   ;; construction, the pair having already been refused
                   ;; above — so the arm that reads it would answer "no
                   ;; body" and dispatch a POST with its body still on the
                   ;; wire.
                   ((eq coding :chunked)
                    ;; REQUEST-END stays 0 until the framing walk finds the
                    ;; terminator. That is the "not known yet" state, and
                    ;; the reason both resets clear this field rather than
                    ;; leaving the previous request's value in it: 0 is a
                    ;; boundary that fails loudly, a leftover is one that
                    ;; fails quietly.
                    (setf (connection-header-end conn) header-end
                          (connection-body-framing conn) :chunked
                          (connection-body-expected conn) 0
                          (connection-chunk-scan-pos conn) 0
                          (connection-request-end conn) 0
                          (connection-state conn) :read-body)
                    ;; The whole body can already be buffered — a small
                    ;; upload arrives in one read — so ask before waiting
                    ;; for an event that would never come.
                    (if (connection-body-complete-p conn)
                        :dispatch
                        :continue))
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
                            (connection-header-end conn) header-end
                            ;; The wire end of a Content-Length request is
                            ;; its declared end. Stated rather than derived,
                            ;; because the chunked arm cannot state it here
                            ;; at all — it is not known until the framing
                            ;; walk reaches the terminator.
                            (connection-request-end conn)
                            (+ body-start content-length))
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
                    ;; BODY-EXPECTED is deliberately not set here: it is
                    ;; already 0, from the reset that ended the previous
                    ;; request on this connection. The boundary is not
                    ;; allowed the same shortcut — it is stated, because a
                    ;; field whose correctness rides on what some other
                    ;; path left behind is the staleness this slot exists
                    ;; to remove.
                    (setf (connection-header-end conn) header-end
                          (connection-request-end conn) (+ header-end 4))
                    :dispatch)))))))
             ;; No CRLFCRLF yet — keep reading
             :continue)))
      (:read-body
       (if (connection-body-complete-p conn)
           :dispatch
           :continue))
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
    (ecase (connection-body-framing conn)
      (:length
       (when (> content-length 0)
         (setf (http-request-body request)
               (subseq (connection-read-buf conn) body-start
                       (+ body-start content-length)))))
      (:chunked
       ;; The framing walk that got us here is deliberately lax — it
       ;; answers "do we have it all yet", and a too-strict predicate
       ;; would hang rather than refuse. DECODE-CHUNKED-BODY is the
       ;; validator, so a body the walk waved through can still be
       ;; rejected here, and its error has to become the client's 400
       ;; rather than the 500 that any other unhandled error becomes:
       ;; the request is malformed, not the server.
       (let ((decoded (handler-case
                          (decode-chunked-body (connection-read-buf conn)
                                               body-start
                                               (connection-request-end conn))
                        (error (e)
                          (http-reject 400 "malformed chunked body: ~a" e)))))
         (setf (http-request-body request) decoded
               ;; BODY-EXPECTED becomes the decoded length, so it means the
               ;; same thing on both paths once the body exists.
               (connection-body-expected conn) (length decoded)))))
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

(defvar *epoll-fd* nil
  "The epoll fd of the worker running on this thread, bound by RUN-WORKER.

   Exists so a writer reachable from app code can arm EPOLLOUT without
   the fd being threaded through an exported signature. NIL outside a
   worker, which is the case unit tests and REPL calls run in — writers
   check rather than assume, since a stream driven by hand has no event
   loop to hand the remainder to anyway.")

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
      (t (%queue-tail conn bytes) t))))

(defun %queue-tail (conn bytes)
  "Put BYTES on the tail of CONN's write queue. No bound check — whether
   a bound applies is the caller's question, and the two callers answer
   it differently."
  (let ((cell (list bytes)))
    (if (connection-write-queue-tail conn)
        (setf (cdr (connection-write-queue-tail conn)) cell)
        (setf (connection-write-queue conn) cell))
    (setf (connection-write-queue-tail conn) cell))
  (incf (connection-write-queued conn) (length bytes))
  t)

(defun connection-queue-segments (conn segments)
  "Queue a complete response that arrives in pieces: the first becomes
   the head, the rest follow it.

   Signals if anything is already pending, exactly as
   CONNECTION-QUEUE-WRITE does and for the same reason — this is the same
   act, a whole response handed over at once, that happens to come in
   more than one vector.

   Deliberately outside *MAX-WRITE-BACKLOG*. That bound is for a producer
   outrunning its peer, where refusing whole leaves the stream
   well-formed and lets the caller pick a disposition; its own docstring
   says as much. A response already complete in memory has no producer to
   throttle and no caller to decide, so refusing one of its pieces
   conserves nothing and truncates the message instead — headers
   promising a body the peer never receives, and on a keep-alive
   connection the next response arriving where that body should have
   been. A static file served as one finished vector has always been
   queued without a size limit; arriving in pieces does not change what
   it is."
  (unless segments
    (error "connection-queue-segments: nothing to queue on fd ~d"
           (connection-fd conn)))
  (connection-queue-write conn (first segments))
  (dolist (seg (rest segments))
    (%queue-tail conn seg))
  t)

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
             (let ((result (connection-write-from
                            conn (connection-write-buf conn) pos remaining)))
               (cond
                 ((eq result :again) (return (finish :continue)))
                 ;; The mirror image: the transport must receive before it
                 ;; can send again, and what it wants re-issued is this
                 ;; *write* once the socket is readable.
                 ((eq result :want-read) (return (finish :want-read)))
                 (t (setf wrote t)
                    (incf (connection-write-pos conn) result)))))
            ;; Head drained; promote the next queued vector and keep writing.
            ;; The socket is still writable and will not say so a second time.
            ((connection-promote-write conn))
            (t (return (finish :done)))))))))
