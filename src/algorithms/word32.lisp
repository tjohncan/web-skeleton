(in-package :web-skeleton)

;;; ===========================================================================
;;; 32-bit unsigned word operations
;;;
;;; Shared by SHA-1 and SHA-256.  Common Lisp has arbitrary-precision
;;; integers, so we mask to 32 bits after every operation to stay in range.
;;; Where the operands are declared (UNSIGNED-BYTE 32), SBCL reads a masked
;;; sum or shift as modular arithmetic and does it in machine words.
;;; ===========================================================================

(defmacro u32+ (&rest args)
  "ARGS added and masked to 32 bits.

   A macro, so a call is the masked sum itself rather than a call through
   generic + with its arguments in a list, and SBCL can add in machine
   words where the arguments are declared (UNSIGNED-BYTE 32)."
  `(logand #xFFFFFFFF (+ ,@args)))

(declaim (inline u32-rotate-left))

(defun u32-rotate-left (n count)
  "Rotate 32-bit integer N left by COUNT bits."
  (declare (type (unsigned-byte 32) n)
           (type (integer 0 32) count))
  (logand #xFFFFFFFF
          (logior (ash n count)
                  (ash n (- count 32)))))
