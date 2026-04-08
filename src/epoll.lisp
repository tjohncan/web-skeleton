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

(defun epoll-create ()
  "Create a new epoll instance. Returns the epoll file descriptor."
  (let ((fd (%epoll-create1 0)))
    (when (< fd 0)
      (error "epoll_create1 failed: errno ~d" (get-errno)))
    fd))

(defvar *epoll-ctl-buf* nil
  "Pre-allocated 12-byte buffer for epoll_ctl calls. Bound per-worker.")

(defun %epoll-ctl-call (epoll-fd op fd events)
  "Internal: populate the ctl buffer and call epoll_ctl."
  (let ((event (or *epoll-ctl-buf*
                   (make-array 12 :element-type '(unsigned-byte 8)))))
    ;; struct epoll_event: uint32_t events + epoll_data_t data
    ;; We store the fd in the data union (offset 4, as int32)
    (pack-le-u32 event 0 events)
    (pack-le-u32 event 4 fd)
    (pack-le-u32 event 8 0)
    (sb-sys:with-pinned-objects (event)
      (let ((result (%epoll-ctl epoll-fd op fd
                                (sb-sys:vector-sap event))))
        (when (< result 0)
          (error "epoll_ctl failed: errno ~d" (get-errno)))))))

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
      (error "epoll_ctl DEL failed: errno ~d" (get-errno)))))

(defun epoll-wait (epoll-fd event-buf max-events timeout-ms)
  "Wait for events on EPOLL-FD into pre-allocated EVENT-BUF.
   EVENT-BUF must be at least (* MAX-EVENTS 12) bytes.
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
               (error "epoll_wait failed: errno ~d" err))))))))

(declaim (inline epoll-event-fd epoll-event-flags))

(defun epoll-event-fd (event-buf index)
  "Read the fd from event at INDEX in EVENT-BUF."
  (unpack-le-u32 event-buf (+ (* index 12) 4)))

(defun epoll-event-flags (event-buf index)
  "Read the event flags from event at INDEX in EVENT-BUF."
  (unpack-le-u32 event-buf (* index 12)))

(defun make-epoll-event-buf (max-events)
  "Allocate a reusable event buffer for MAX-EVENTS."
  (make-array (* max-events 12) :element-type '(unsigned-byte 8)))

;;; ---------------------------------------------------------------------------
;;; Socket options
;;; ---------------------------------------------------------------------------

;;; socket option constants
(defconstant +sol-socket+    1)
(defconstant +so-reuseport+ 15)
(defconstant +so-error+      4)

(defun set-socket-option-int (fd level optname value)
  "Set an integer-valued socket option."
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
    (pack-le-u32 buf 0 value)
    (sb-sys:with-pinned-objects (buf)
      (let ((result (%setsockopt fd level optname (sb-sys:vector-sap buf) 4)))
        (when (< result 0)
          (error "setsockopt failed: errno ~d" (get-errno)))))))

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
          (error "getsockopt failed: errno ~d" (get-errno)))))
    (unpack-le-u32 val-buf 0)))

;;; ---------------------------------------------------------------------------
;;; Non-blocking socket setup
;;; ---------------------------------------------------------------------------

(defun set-nonblocking (fd)
  "Set the O_NONBLOCK flag on FD."
  (let ((flags (%fcntl fd +f-getfl+ 0)))
    (when (< flags 0)
      (error "fcntl F_GETFL failed: errno ~d" (get-errno)))
    (let ((result (%fcntl fd +f-setfl+ (logior flags +o-nonblock+))))
      (when (< result 0)
        (error "fcntl F_SETFL failed: errno ~d" (get-errno))))))

(defun socket-fd (socket)
  "Extract the raw file descriptor from an sb-bsd-sockets socket."
  (sb-bsd-sockets:socket-file-descriptor socket))

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
                 (error "read failed: errno ~d" err))))))))

(defun nb-write (fd buffer start nbytes)
  "Non-blocking write from BUFFER starting at START, NBYTES bytes.
   Returns bytes written or :AGAIN if EAGAIN/EWOULDBLOCK."
  (sb-sys:with-pinned-objects (buffer)
    (let ((n (%write fd
                     (sb-sys:sap+ (sb-sys:vector-sap buffer) start)
                     nbytes)))
      (cond
        ((>= n 0) n)
        (t (let ((err (get-errno)))
             (if (or (= err +eagain+) (= err +ewouldblock+))
                 :again
                 (error "write failed: errno ~d" err))))))))
