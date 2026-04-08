(in-package :web-skeleton)

;;; ===========================================================================
;;; Hex encoding/decoding utilities
;;; ===========================================================================

(declaim (inline hex-digit-value))

(defun hex-digit-value (byte)
  "Return 0-15 for an ASCII hex digit byte, or NIL."
  (cond ((<= 48 byte 57)  (- byte 48))       ; 0-9
        ((<= 65 byte 70)  (- byte 55))       ; A-F
        ((<= 97 byte 102) (- byte 87))       ; a-f
        (t nil)))

(defun bytes-to-hex (bytes)
  "Convert a byte vector to a lowercase hex string. Single allocation."
  (let* ((len (length bytes))
         (hex (make-string (* len 2)))
         (alphabet "0123456789abcdef"))
    (loop for i from 0 below len
          for b = (aref bytes i)
          for j = (* i 2)
          do (setf (char hex j)      (char alphabet (ash b -4))
                   (char hex (1+ j)) (char alphabet (logand b #xF))))
    hex))
