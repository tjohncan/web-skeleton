(in-package :web-skeleton)

;;; ===========================================================================
;;; Cryptographic random
;;;
;;; Thin helpers over /dev/urandom. Linux-only — the framework assumes
;;; Linux throughout (epoll, /sys/devices/system/cpu/online, etc.), so
;;; no portable fallback.
;;;
;;; /dev/urandom is the correct source: non-blocking, seeded from the
;;; kernel entropy pool, and cryptographically secure after boot-time
;;; initialization. Do NOT use CL's RANDOM for anything security-adjacent
;;; — it is a fast Mersenne Twister (or similar), entirely predictable
;;; once a few outputs are observed.
;;; ===========================================================================

(defvar *urandom-stream* nil
  "An open stream on /dev/urandom, or NIL. Bound per worker by
   WITH-WORKER-URANDOM, alongside *CONNECTIONS* and the other
   worker-local state, so RANDOM-BYTES does not pay an open + close on
   every call — which a session-per-request app makes a per-request
   cost.

   Per-worker rather than global precisely because it is a stream with a
   read position and a buffer: two threads sharing one would need a lock,
   and the point of this is to remove work from the hot path, not to
   move it into a mutex. Worker-local state needs no lock at all, which
   is the same reasoning *CONNECTIONS* and *DNS-CACHE* already run on.

   NIL is the ordinary state outside a worker thread — app startup, the
   REPL, the test suite — and RANDOM-BYTES falls back to opening one per
   call there. Note that a dynamic binding does not cross
   SB-THREAD:MAKE-THREAD, so binding this anywhere but inside the worker
   would leave every worker seeing NIL.")

(defun random-bytes (n)
  "Read N bytes from /dev/urandom. Returns a fresh byte vector of length N.
   Signals an error on short read — should not happen on a healthy Linux
   system, but the check catches filesystem oddities in misconfigured
   containers or chroots.

   Uses the worker's cached stream when there is one, otherwise opens
   and closes its own. A cached stream that fails is not fatal: the
   call falls back to opening its own, matching WITH-WORKER-URANDOM's
   open, which also degrades to per-call rather than refusing to start
   a worker. Without that, one bad read would poison the binding for
   the worker's whole life — the stream stays bound, so every later
   call fails too, and RANDOM-TOKEN feeds session IDs and CSRF tokens.
   Per-call opens were the behaviour before the cache existed, so
   falling back to them is a return to something known to work."
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (flet ((read-fresh ()
             (with-open-file (s "/dev/urandom"
                                :direction :input
                                :element-type '(unsigned-byte 8))
               (unless (= (read-sequence buf s) n)
                 (error "short read from /dev/urandom")))))
      (if *urandom-stream*
          (unless (ignore-errors
                   (= (read-sequence buf *urandom-stream*) n))
            (read-fresh))
          (read-fresh)))
    buf))

(defmacro with-worker-urandom (&body body)
  "Run BODY with *URANDOM-STREAM* bound to an open /dev/urandom.

   Wrapped here rather than opened inline in RUN-WORKER so that the
   path, the element type and the close all live next to RANDOM-BYTES,
   which is the only code that cares. If the open fails — no /dev in a
   stripped container, an exhausted fd table — the binding stays NIL and
   RANDOM-BYTES goes on opening per call, so a worker still starts and
   still produces tokens. Failing to make random cheaper is not a reason
   to fail to serve."
  (let ((s (gensym "URANDOM")))
    `(let* ((,s (ignore-errors
                 (open "/dev/urandom"
                       :direction :input
                       :element-type '(unsigned-byte 8))))
            (*urandom-stream* ,s))
       (unwind-protect (progn ,@body)
         ;; Same softness as the open, and for a sharper reason: this
         ;; runs during unwinding, so a signalling close would replace
         ;; whatever condition is already on its way out of the worker.
         (when ,s (ignore-errors (close ,s)))))))

(defun random-token (&key (bytes 32))
  "Return a cryptographically secure random token as a base64url string.
   Default BYTES=32 gives ~256 bits of entropy and encodes to 43
   unpadded base64url characters. Suitable for session IDs, CSRF tokens,
   nonces, PKCE verifiers — anything that wants an unguessable string."
  (base64url-encode (random-bytes bytes)))
