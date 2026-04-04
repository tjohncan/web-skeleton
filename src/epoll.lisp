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

;;; ===========================================================================
;;; Lisp wrappers — these are the public interface
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; epoll
;;; ---------------------------------------------------------------------------

(defun epoll-create ()
  "Create a new epoll instance. Returns the epoll file descriptor."
  (let ((fd (%epoll-create1 0)))
    (when (< fd 0)
      (error "epoll_create1 failed: errno ~d" (get-errno)))
    fd))

(defun epoll-add (epoll-fd fd events)
  "Register FD with EPOLL-FD, watching for EVENTS (bitmask)."
  (let ((event (make-array 12 :element-type '(unsigned-byte 8)
                              :initial-element 0)))
    ;; struct epoll_event: uint32_t events + epoll_data_t data
    ;; We store the fd in the data union (offset 4, as int32)
    (setf (aref event 0) (logand events #xFF)
          (aref event 1) (logand (ash events -8) #xFF)
          (aref event 2) (logand (ash events -16) #xFF)
          (aref event 3) (logand (ash events -24) #xFF)
          (aref event 4) (logand fd #xFF)
          (aref event 5) (logand (ash fd -8) #xFF)
          (aref event 6) (logand (ash fd -16) #xFF)
          (aref event 7) (logand (ash fd -24) #xFF))
    (sb-sys:with-pinned-objects (event)
      (let ((result (%epoll-ctl epoll-fd +epoll-ctl-add+ fd
                                (sb-sys:vector-sap event))))
        (when (< result 0)
          (error "epoll_ctl ADD failed: errno ~d" (get-errno)))))))

(defun epoll-modify (epoll-fd fd events)
  "Modify the events watched for FD on EPOLL-FD."
  (let ((event (make-array 12 :element-type '(unsigned-byte 8)
                              :initial-element 0)))
    (setf (aref event 0) (logand events #xFF)
          (aref event 1) (logand (ash events -8) #xFF)
          (aref event 2) (logand (ash events -16) #xFF)
          (aref event 3) (logand (ash events -24) #xFF)
          (aref event 4) (logand fd #xFF)
          (aref event 5) (logand (ash fd -8) #xFF)
          (aref event 6) (logand (ash fd -16) #xFF)
          (aref event 7) (logand (ash fd -24) #xFF))
    (sb-sys:with-pinned-objects (event)
      (let ((result (%epoll-ctl epoll-fd +epoll-ctl-mod+ fd
                                (sb-sys:vector-sap event))))
        (when (< result 0)
          (error "epoll_ctl MOD failed: errno ~d" (get-errno)))))))

(defun epoll-remove (epoll-fd fd)
  "Remove FD from EPOLL-FD."
  (let ((result (%epoll-ctl epoll-fd +epoll-ctl-del+ fd
                            (sb-sys:int-sap 0))))
    (when (< result 0)
      (error "epoll_ctl DEL failed: errno ~d" (get-errno)))))

(defun epoll-wait (epoll-fd max-events timeout-ms)
  "Wait for events on EPOLL-FD.  Returns a list of (fd . events) pairs.
   TIMEOUT-MS: milliseconds to wait (-1 = block indefinitely, 0 = return immediately)."
  (let* ((event-size 12)    ; sizeof(struct epoll_event) on x86-64
         (buf (make-array (* max-events event-size)
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (sb-sys:with-pinned-objects (buf)
      (let ((n (%epoll-wait epoll-fd (sb-sys:vector-sap buf)
                            max-events timeout-ms)))
        (cond
          ((> n 0)
           (loop for i from 0 below n
                 for offset = (* i event-size)
                 collect (cons
                          ;; fd from data union (offset +4)
                          (logior (aref buf (+ offset 4))
                                  (ash (aref buf (+ offset 5)) 8)
                                  (ash (aref buf (+ offset 6)) 16)
                                  (ash (aref buf (+ offset 7)) 24))
                          ;; events bitmask (offset +0)
                          (logior (aref buf offset)
                                  (ash (aref buf (+ offset 1)) 8)
                                  (ash (aref buf (+ offset 2)) 16)
                                  (ash (aref buf (+ offset 3)) 24)))))
          ((zerop n) nil)   ; timeout, no events
          (t
           (let ((err (get-errno)))
             (if (= err +eintr+)
                 nil        ; interrupted by signal, caller can retry
                 (error "epoll_wait failed: errno ~d" err)))))))))

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
