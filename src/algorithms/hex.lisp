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

(defun hex-decode (hex-string)
  "Convert a hex string to a byte vector."
  (let* ((len (length hex-string))
         (bytes (make-array (ash len -1) :element-type '(unsigned-byte 8))))
    (when (oddp len)
      (error "hex-decode: odd-length input"))
    (loop for i from 0 below len by 2
          for j from 0
          for hi = (hex-digit-value (char-code (char hex-string i)))
          for lo = (hex-digit-value (char-code (char hex-string (1+ i))))
          do (unless (and hi lo)
               (error "hex-decode: invalid hex character at ~d" i))
             (setf (aref bytes j) (logior (ash hi 4) lo)))
    bytes))
