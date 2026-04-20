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

(defun random-bytes (n)
  "Read N bytes from /dev/urandom. Returns a fresh byte vector of length N.
   Signals an error on short read — should not happen on a healthy Linux
   system, but the check catches filesystem oddities in misconfigured
   containers or chroots."
  (with-open-file (s "/dev/urandom"
                     :direction :input
                     :element-type '(unsigned-byte 8))
    (let ((buf (make-array n :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence buf s) n)
        (error "short read from /dev/urandom"))
      buf)))

(defun random-token (&key (bytes 32))
  "Return a cryptographically secure random token as a base64url string.
   Default BYTES=32 gives ~256 bits of entropy and encodes to 43
   unpadded base64url characters. Suitable for session IDs, CSRF tokens,
   nonces, PKCE verifiers — anything that wants an unguessable string."
  (base64url-encode (random-bytes bytes)))
