(in-package :web-skeleton)

;;; ===========================================================================
;;; Base64 Encoder (RFC 4648)
;;;
;;; Encodes a byte vector into a base64 string.
;;; ===========================================================================

(defparameter *base64-alphabet*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The 64-character alphabet used for encoding. Index 0 = A, index 63 = /.")

(defun base64-encode (bytes)
  "Encode a byte vector into a base64 string."
  (let* ((len (length bytes))
         (out (make-array (* 4 (ceiling len 3))
                          :element-type 'character
                          :fill-pointer 0)))
    ;; Process 3 bytes at a time into 4 base64 characters
    (loop for i from 0 below len by 3
          do (let* ((b0 (aref bytes i))
                    (b1 (if (< (+ i 1) len) (aref bytes (+ i 1)) 0))
                    (b2 (if (< (+ i 2) len) (aref bytes (+ i 2)) 0))
                    ;; Pack 3 bytes into a 24-bit integer
                    (triplet (logior (ash b0 16) (ash b1 8) b2))
                    ;; Extract 4 6-bit indices
                    (c0 (logand #x3F (ash triplet -18)))
                    (c1 (logand #x3F (ash triplet -12)))
                    (c2 (logand #x3F (ash triplet -6)))
                    (c3 (logand #x3F triplet))
                    (remaining (- len i)))
               ;; Always emit first two characters
               (vector-push (char *base64-alphabet* c0) out)
               (vector-push (char *base64-alphabet* c1) out)
               ;; Third character or padding
               (vector-push (if (> remaining 1)
                                (char *base64-alphabet* c2)
                                #\=)
                            out)
               ;; Fourth character or padding
               (vector-push (if (> remaining 2)
                                (char *base64-alphabet* c3)
                                #\=)
                            out)))
    (coerce out 'string)))
