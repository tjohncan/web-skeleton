(in-package :web-skeleton)

;;; ===========================================================================
;;; Linux epoll FFI bindings (via sb-alien)
;;;
;;; Wraps the three epoll syscalls, fcntl for O_NONBLOCK, and raw
;;; read/write for non-blocking I/O.  Everything here is a thin shim
;;; over the kernel interface — no policy, no buffering.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Constants (from <sys/epoll.h>, <fcntl.h>, <errno.h>)
;;; ---------------------------------------------------------------------------

;;; epoll_ctl operations
(defconstant +epoll-ctl-add+ 1)
(defconstant +epoll-ctl-del+ 2)
(defconstant +epoll-ctl-mod+ 3)

;;; epoll event flags
(defconstant +epollin+  #x001)    ; fd is readable
(defconstant +epollout+ #x004)    ; fd is writable
(defconstant +epollerr+ #x008)    ; error condition
(defconstant +epollhup+ #x010)    ; hang up
(defconstant +epollet+  (ash 1 31))  ; edge-triggered

;;; struct epoll_event layout
;;;
;;; Linux UAPI declares struct epoll_event with __attribute__((packed))
;;; on x86_64 only (see include/uapi/linux/eventpoll.h). Every other
;;; arch follows natural alignment, so the u64 data union forces 4
;;; bytes of padding after the u32 events field:
;;;
;;;   x86_64:   events@0 (4) + data@4 (8)                = 12 bytes
;;;   arm64:    events@0 (4) + pad@4 (4) + data@8 (8)    = 16 bytes
;;;
;;; Hardcoding 12 everywhere would silently break the framework on
;;; aarch64: epoll_wait would overflow the caller-supplied buffer
;;; (heap corruption) and the per-event field offsets would be
;;; off-by-(4*index) (garbage fd reads). Fail loudly at load time on
;;; any target we haven't verified instead of assuming a layout.
(defconstant +epoll-event-size+
  #+x86-64 12
  #+arm64  16
  #-(or x86-64 arm64)
  (error "epoll struct layout not verified for this SBCL target. ~
          Patch src/epoll.lisp after consulting include/uapi/linux/eventpoll.h."))

(defconstant +epoll-event-data-offset+
  #+x86-64 4
  #+arm64  8
  #-(or x86-64 arm64)
  (error "epoll struct layout not verified for this SBCL target."))

;;; fcntl commands and flags
(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)
(defconstant +o-nonblock+ #o4000)  ; 2048 decimal

;;; errno values we care about
(defconstant +eagain+      11)
(defconstant +ewouldblock+ 11)    ; same as EAGAIN on Linux
(defconstant +eintr+        4)

;;; ---------------------------------------------------------------------------
;;; errno access
;;; ---------------------------------------------------------------------------

(sb-alien:define-alien-routine ("__errno_location" errno-location)
    (sb-alien:* sb-alien:int))

(defun errno-name (errno)
  "Return a human-readable name for common errno values, or nil."
  (case errno
    (4   "EINTR")
    (9   "EBADF")
    (11  "EAGAIN")
    (13  "EACCES")
    (22  "EINVAL")
    (23  "EMFILE")
    (24  "ENFILE")
    (32  "EPIPE")
    (104 "ECONNRESET")
    (107 "ENOTCONN")
    (111 "ECONNREFUSED")
    (t   nil)))

(defun errno-string (errno)
  "Format errno as 'NAME (N)' or just 'errno N' if unnamed."
  (let ((name (errno-name errno)))
    (if name
        (format nil "~a (~d)" name errno)
        (format nil "errno ~d" errno))))

(defun get-errno ()
  "Return the current errno value."
  (sb-alien:deref (errno-location)))

;;; ---------------------------------------------------------------------------
;;; epoll syscalls
;;; ---------------------------------------------------------------------------

(sb-alien:define-alien-routine ("epoll_create1" %epoll-create1) sb-alien:int
  (flags sb-alien:int))

(sb-alien:define-alien-routine ("epoll_ctl" %epoll-ctl) sb-alien:int
  (epfd sb-alien:int)
  (op sb-alien:int)
  (fd sb-alien:int)
  (event (sb-alien:* t)))

(sb-alien:define-alien-routine ("epoll_wait" %epoll-wait) sb-alien:int
  (epfd sb-alien:int)
  (events (sb-alien:* t))
  (maxevents sb-alien:int)
  (timeout sb-alien:int))

;;; ---------------------------------------------------------------------------
;;; fcntl
;;; ---------------------------------------------------------------------------

(sb-alien:define-alien-routine ("fcntl" %fcntl) sb-alien:int
  (fd sb-alien:int)
  (cmd sb-alien:int)
  (arg sb-alien:int))

;;; ---------------------------------------------------------------------------
;;; Raw read/write
;;; ---------------------------------------------------------------------------

(sb-alien:define-alien-routine ("read" %read) sb-alien:long
  (fd sb-alien:int)
  (buf (sb-alien:* t))
  (nbytes sb-alien:unsigned-long))

(sb-alien:define-alien-routine ("write" %write) sb-alien:long
  (fd sb-alien:int)
  (buf (sb-alien:* t))
  (nbytes sb-alien:unsigned-long))

(sb-alien:define-alien-routine ("close" %close) sb-alien:int
  (fd sb-alien:int))

(sb-alien:define-alien-routine ("poll" %poll) sb-alien:int
  (fds (sb-alien:* t))
  (nfds sb-alien:unsigned-long)
  (timeout sb-alien:int))

(sb-alien:define-alien-routine ("setsockopt" %setsockopt) sb-alien:int
  (sockfd sb-alien:int)
  (level sb-alien:int)
  (optname sb-alien:int)
  (optval (sb-alien:* t))
  (optlen sb-alien:unsigned-int))

(sb-alien:define-alien-routine ("getsockopt" %getsockopt) sb-alien:int
  (sockfd sb-alien:int)
  (level sb-alien:int)
  (optname sb-alien:int)
  (optval (sb-alien:* t))
  (optlen (sb-alien:* sb-alien:unsigned-int)))

;;; ===========================================================================
;;; Lisp wrappers — these are the public interface
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Byte packing (little-endian, for struct fields)
;;; ---------------------------------------------------------------------------

(declaim (inline pack-le-u32 unpack-le-u32))

(defun pack-le-u32 (buf offset value)
  "Write a 32-bit VALUE into BUF at OFFSET as little-endian."
  (setf (aref buf offset)       (logand value #xFF)
        (aref buf (+ offset 1)) (logand (ash value -8) #xFF)
        (aref buf (+ offset 2)) (logand (ash value -16) #xFF)
        (aref buf (+ offset 3)) (logand (ash value -24) #xFF)))

(defun unpack-le-u32 (buf offset)
  "Read a 32-bit little-endian value from BUF at OFFSET."
  (logior (aref buf offset)
          (ash (aref buf (+ offset 1)) 8)
          (ash (aref buf (+ offset 2)) 16)
          (ash (aref buf (+ offset 3)) 24)))

;;; ---------------------------------------------------------------------------
;;; epoll
;;; ---------------------------------------------------------------------------

(defconstant +epoll-cloexec+ #x80000)

(defun epoll-create ()
  "Create a new epoll instance. Returns the epoll file descriptor.
   Sets CLOEXEC to prevent fd leak on fork+exec."
  (let ((fd (%epoll-create1 +epoll-cloexec+)))
    (when (< fd 0)
      (error "epoll_create1 failed: ~a" (errno-string (get-errno))))
    fd))

(defvar *epoll-ctl-buf* nil
  "Pre-allocated struct epoll_event buffer for epoll_ctl calls.
   Bound per-worker. Length is +EPOLL-EVENT-SIZE+ bytes — the
   packed x86_64 layout is 12, arm64's naturally aligned layout
   is 16.")

(defun %epoll-ctl-call (epoll-fd op fd events)
  "Internal: populate the ctl buffer and call epoll_ctl."
  (let ((event (or *epoll-ctl-buf*
                   (make-array +epoll-event-size+
                               :element-type '(unsigned-byte 8)))))
    ;; struct epoll_event: u32 events + epoll_data_t data.
    ;; Stash the fd in the low 32 bits of the data u64.
    (pack-le-u32 event 0 events)
    ;; Zero the 4 bytes of alignment padding between events and data
    ;; on naturally aligned arches. x86_64 packs the struct, so this
    ;; line is elided by the reader and there is nothing to zero.
    #+arm64 (pack-le-u32 event 4 0)
    (pack-le-u32 event +epoll-event-data-offset+       fd)
    (pack-le-u32 event (+ +epoll-event-data-offset+ 4) 0)
    (sb-sys:with-pinned-objects (event)
      (let ((result (%epoll-ctl epoll-fd op fd
                                (sb-sys:vector-sap event))))
        (when (< result 0)
          (error "epoll_ctl failed: ~a" (errno-string (get-errno))))))))

(defun epoll-add (epoll-fd fd events)
  "Register FD with EPOLL-FD, watching for EVENTS (bitmask)."
  (%epoll-ctl-call epoll-fd +epoll-ctl-add+ fd events))

(defun epoll-modify (epoll-fd fd events)
  "Modify the events watched for FD on EPOLL-FD."
  (%epoll-ctl-call epoll-fd +epoll-ctl-mod+ fd events))

(defun epoll-remove (epoll-fd fd)
  "Remove FD from EPOLL-FD."
  (let ((result (%epoll-ctl epoll-fd +epoll-ctl-del+ fd
                            (sb-sys:int-sap 0))))
    (when (< result 0)
      (error "epoll_ctl DEL failed: ~a" (errno-string (get-errno))))))

(defun epoll-wait (epoll-fd event-buf max-events timeout-ms)
  "Wait for events on EPOLL-FD into pre-allocated EVENT-BUF.
   EVENT-BUF must be at least (* MAX-EVENTS +EPOLL-EVENT-SIZE+) bytes.
   Returns the number of events ready (0 on timeout or interrupt).
   Caller reads events via EPOLL-EVENT-FD and EPOLL-EVENT-FLAGS."
  (sb-sys:with-pinned-objects (event-buf)
    (let ((n (%epoll-wait epoll-fd (sb-sys:vector-sap event-buf)
                          max-events timeout-ms)))
      (cond
        ((> n 0) n)
        ((zerop n) 0)
        (t
         (let ((err (get-errno)))
           (if (= err +eintr+)
               0
               (error "epoll_wait failed: ~a" (errno-string err)))))))))

(declaim (inline epoll-event-fd epoll-event-flags))

(defun epoll-event-fd (event-buf index)
  "Read the fd from event at INDEX in EVENT-BUF. The fd lives in the
   low 32 bits of the epoll_data_t union, which sits at
   +EPOLL-EVENT-DATA-OFFSET+ within each struct epoll_event."
  (unpack-le-u32 event-buf
                 (+ (* index +epoll-event-size+)
                    +epoll-event-data-offset+)))

(defun epoll-event-flags (event-buf index)
  "Read the event flags from event at INDEX in EVENT-BUF."
  (unpack-le-u32 event-buf (* index +epoll-event-size+)))

(defun make-epoll-event-buf (max-events)
  "Allocate a reusable event buffer for MAX-EVENTS.
   Sized at MAX-EVENTS * sizeof(struct epoll_event) for the running
   arch (12 bytes per event on x86_64, 16 on arm64)."
  (make-array (* max-events +epoll-event-size+)
              :element-type '(unsigned-byte 8)))

;;; ---------------------------------------------------------------------------
;;; Socket options
;;; ---------------------------------------------------------------------------

;;; socket option constants
(defconstant +sol-socket+    1)
(defconstant +so-reuseport+ 15)
(defconstant +so-error+      4)
(defconstant +so-rcvtimeo+  20)
(defconstant +so-sndtimeo+  21)

(defun set-socket-option-int (fd level optname value)
  "Set an integer-valued socket option."
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
    (pack-le-u32 buf 0 value)
    (sb-sys:with-pinned-objects (buf)
      (let ((result (%setsockopt fd level optname (sb-sys:vector-sap buf) 4)))
        (when (< result 0)
          (error "setsockopt failed: ~a" (errno-string (get-errno))))))))

(defun set-socket-timeout (fd seconds)
  "Set SO_RCVTIMEO and SO_SNDTIMEO on FD.
   SECONDS is an integer. struct timeval on 64-bit Linux is 16
   bytes: tv_sec (8) + tv_usec (8), both little-endian. We write
   only the low 32 bits of tv_sec via PACK-LE-U32; the upper 32
   bits and tv_usec stay zero from :INITIAL-ELEMENT 0, which is
   correct for every SECONDS under 2^32 (≈136 years)."
  (let ((buf (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (pack-le-u32 buf 0 seconds)
    (sb-sys:with-pinned-objects (buf)
      (let ((result (%setsockopt fd +sol-socket+ +so-rcvtimeo+
                                 (sb-sys:vector-sap buf) 16)))
        (when (< result 0)
          (error "setsockopt SO_RCVTIMEO failed: ~a" (errno-string (get-errno)))))
      (let ((result (%setsockopt fd +sol-socket+ +so-sndtimeo+
                                 (sb-sys:vector-sap buf) 16)))
        (when (< result 0)
          (error "setsockopt SO_SNDTIMEO failed: ~a" (errno-string (get-errno))))))))

(defun get-socket-option-int (fd level optname)
  "Get an integer-valued socket option."
  (let ((val-buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0))
        (len-buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
    (pack-le-u32 len-buf 0 4)
    (sb-sys:with-pinned-objects (val-buf len-buf)
      (let ((result (%getsockopt fd level optname
                                  (sb-sys:vector-sap val-buf)
                                  (sb-sys:vector-sap len-buf))))
        (when (< result 0)
          (error "getsockopt failed: ~a" (errno-string (get-errno))))))
    (unpack-le-u32 val-buf 0)))

;;; ---------------------------------------------------------------------------
;;; Non-blocking socket setup
;;; ---------------------------------------------------------------------------

(defun set-nonblocking (fd)
  "Set the O_NONBLOCK flag on FD."
  (let ((flags (%fcntl fd +f-getfl+ 0)))
    (when (< flags 0)
      (error "fcntl F_GETFL failed: ~a" (errno-string (get-errno))))
    (let ((result (%fcntl fd +f-setfl+ (logior flags +o-nonblock+))))
      (when (< result 0)
        (error "fcntl F_SETFL failed: ~a" (errno-string (get-errno)))))))

(defun set-blocking (fd)
  "Clear the O_NONBLOCK flag on FD."
  (let ((flags (%fcntl fd +f-getfl+ 0)))
    (when (< flags 0)
      (error "fcntl F_GETFL failed: ~a" (errno-string (get-errno))))
    (let ((result (%fcntl fd +f-setfl+ (logand flags (lognot +o-nonblock+)))))
      (when (< result 0)
        (error "fcntl F_SETFL failed: ~a" (errno-string (get-errno)))))))

(defun socket-fd (socket)
  "Extract the raw file descriptor from an sb-bsd-sockets socket."
  (sb-bsd-sockets:socket-file-descriptor socket))

;;; ---------------------------------------------------------------------------
;;; poll(2) — block until an fd is ready
;;; ---------------------------------------------------------------------------

(defconstant +pollout+ #x004)

(defvar *poll-buf* nil
  "Pre-allocated 8-byte buffer for poll calls. Bound per-worker.")

(defun poll-writable (fd timeout-ms)
  "Block until FD is writable or TIMEOUT-MS elapses.
   Returns T if writable, NIL on timeout or EINTR. Other poll(2)
   errors (EBADF/EINVAL/EFAULT/ENOMEM) raise — silently masking them
   as timeouts would turn programming mistakes into mysterious hangs."
  ;; struct pollfd: int fd (4) + short events (2) + short revents (2) = 8 bytes
  (let ((buf (or *poll-buf*
                 (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))))
    (pack-le-u32 buf 0 fd)
    (setf (aref buf 4) (logand +pollout+ #xFF)
          (aref buf 5) (logand (ash +pollout+ -8) #xFF))
    (sb-sys:with-pinned-objects (buf)
      (let ((n (%poll (sb-sys:vector-sap buf) 1 timeout-ms)))
        (cond
          ((> n 0) t)
          ((zerop n) nil)                         ; timeout
          (t (let ((err (get-errno)))
               (if (= err +eintr+)
                   nil                            ; spurious wake-up
                   (error "poll failed: ~a" (errno-string err))))))))))

;;; ---------------------------------------------------------------------------
;;; Bounded blocking connect
;;;
;;; SO_RCVTIMEO and SO_SNDTIMEO do not apply to connect(2) — Linux
;;; falls back to tcp_syn_retries (~120s) on a black-holed peer. To
;;; bound a blocking fetch setup under *FETCH-TIMEOUT*, we switch the
;;; socket to non-blocking, let connect(2) return immediately with
;;; EINPROGRESS, poll-wait up to the timeout, check SO_ERROR, then
;;; put the socket back in blocking mode so subsequent read/write use
;;; the familiar blocking semantics (still bounded by SO_RCVTIMEO /
;;; SO_SNDTIMEO which the caller sets beforehand).
;;; ---------------------------------------------------------------------------

(defun blocking-connect (socket ip port timeout-seconds)
  "Connect SOCKET to IP:PORT bounded by TIMEOUT-SECONDS. Uses a
   non-blocking connect + poll(2) because connect(2) itself is not
   bounded by SO_RCVTIMEO/SO_SNDTIMEO. Leaves SOCKET in blocking mode
   on success so subsequent read/write block normally. Raises on
   timeout or any connect failure. Returns SOCKET."
  (let ((fd (socket-fd socket)))
    (set-nonblocking fd)
    ;; Mirror the async path: a non-blocking connect either succeeds
    ;; immediately (localhost) or raises with EINPROGRESS. Either way,
    ;; the SO_ERROR check below after POLL-WRITABLE is authoritative.
    (handler-case
        (sb-bsd-sockets:socket-connect socket ip port)
      (sb-bsd-sockets:socket-error () nil))
    (unless (poll-writable fd (* timeout-seconds 1000))
      (error "connect: timed out after ~a seconds" timeout-seconds))
    (let ((err (get-socket-option-int fd +sol-socket+ +so-error+)))
      (unless (zerop err)
        (error "connect: ~a" (errno-string err))))
    (set-blocking fd)
    socket))

;;; ---------------------------------------------------------------------------
;;; Non-blocking read/write
;;; ---------------------------------------------------------------------------

(defun nb-read (fd buffer start max-bytes)
  "Non-blocking read into BUFFER starting at START, up to MAX-BYTES.
   Returns bytes read, :AGAIN if EAGAIN/EWOULDBLOCK, or :EOF on close."
  (sb-sys:with-pinned-objects (buffer)
    (let ((n (%read fd
                    (sb-sys:sap+ (sb-sys:vector-sap buffer) start)
                    max-bytes)))
      (cond
        ((> n 0) n)
        ((zerop n) :eof)
        (t (let ((err (get-errno)))
             (if (or (= err +eagain+) (= err +ewouldblock+))
                 :again
                 (error "read failed: ~a" (errno-string err)))))))))

(defun nb-write (fd buffer start nbytes)
  "Non-blocking write from BUFFER starting at START, NBYTES bytes.
   Returns bytes written or :AGAIN if EAGAIN/EWOULDBLOCK.

   Linux write(2) on a non-zero count returns > 0 on success or -1
   on error; it never returns 0 for stream sockets. A zero return
   on some exotic fd type — or a kernel oddity we have yet to meet —
   would loop the caller's (incf write-pos 0) forever, so we map
   zero to :AGAIN defensively. The cost is a single extra epoll
   wake-up in the theoretical case and nothing in the real one."
  (sb-sys:with-pinned-objects (buffer)
    (let ((n (%write fd
                     (sb-sys:sap+ (sb-sys:vector-sap buffer) start)
                     nbytes)))
      (cond
        ((> n 0)    n)
        ((zerop n)  :again)
        (t (let ((err (get-errno)))
             (if (or (= err +eagain+) (= err +ewouldblock+))
                 :again
                 (error "write failed: ~a" (errno-string err)))))))))
