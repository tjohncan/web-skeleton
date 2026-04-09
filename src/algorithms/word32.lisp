(in-package :web-skeleton)

;;; ===========================================================================
;;; 32-bit unsigned word operations
;;;
;;; Shared by SHA-1 and SHA-256.  Common Lisp has arbitrary-precision
;;; integers, so we mask to 32 bits after every operation to stay in range.
;;; ===========================================================================

(defun u32+ (&rest args)
  "Add any number of values, masked to 32 bits."
  (logand #xFFFFFFFF (apply #'+ args)))

(declaim (inline u32-rotate-left))

(defun u32-rotate-left (n count)
  "Rotate 32-bit integer N left by COUNT bits."
  (logand #xFFFFFFFF
          (logior (ash n count)
                  (ash n (- count 32)))))
