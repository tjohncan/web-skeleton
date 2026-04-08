(in-package :web-skeleton)

;;; ===========================================================================
;;; HMAC-SHA256 (RFC 2104)
;;;
;;; Standard HMAC construction using SHA-256.
;;; Used by JWT validation, webhook signature verification, and HKDF.
;;; ===========================================================================

(defconstant +hmac-sha256-block-size+ 64
  "SHA-256 processes 512-bit (64-byte) blocks.")

(defun hmac-sha256 (key message)
  "Compute HMAC-SHA256 of MESSAGE using KEY. Both are byte vectors.
   Returns a 32-byte MAC."
  (let ((block-key (make-array +hmac-sha256-block-size+
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    ;; If key is longer than block size, hash it first
    (if (> (length key) +hmac-sha256-block-size+)
        (replace block-key (sha256 key))
        (replace block-key key))
    ;; Inner and outer padded keys
    (let ((ipad (make-array +hmac-sha256-block-size+
                             :element-type '(unsigned-byte 8)))
          (opad (make-array +hmac-sha256-block-size+
                             :element-type '(unsigned-byte 8))))
      (loop for i from 0 below +hmac-sha256-block-size+
            do (setf (aref ipad i) (logxor (aref block-key i) #x36)
                     (aref opad i) (logxor (aref block-key i) #x5C)))
      ;; HMAC = H(opad || H(ipad || message))
      (let* ((inner-input (make-array (+ +hmac-sha256-block-size+ (length message))
                                       :element-type '(unsigned-byte 8)))
             (inner-hash (progn
                           (replace inner-input ipad)
                           (replace inner-input message
                                    :start1 +hmac-sha256-block-size+)
                           (sha256 inner-input)))
             (outer-input (make-array (+ +hmac-sha256-block-size+ 32)
                                       :element-type '(unsigned-byte 8))))
        (replace outer-input opad)
        (replace outer-input inner-hash :start1 +hmac-sha256-block-size+)
        (sha256 outer-input)))))
