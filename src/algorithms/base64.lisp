(in-package :web-skeleton)

;;; ===========================================================================
;;; Base64 Encoder (RFC 4648)
;;;
;;; Encodes a byte vector into a base64 string.
;;; ===========================================================================

(defparameter *base64-alphabet*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "Standard base64 alphabet (RFC 4648). Index 0 = A, index 63 = /.")

(defparameter *base64url-alphabet*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  "URL-safe base64 alphabet (RFC 4648 §5). + -> -, / -> _.")

(defun %make-decode-table (alphabet)
  "Build a 128-element decode table mapping char-code -> 0-63 or NIL."
  (let ((table (make-array 128 :initial-element nil)))
    (loop for i from 0 below 64
          do (setf (aref table (char-code (char alphabet i))) i))
    table))

(defparameter *base64-decode-table* (%make-decode-table *base64-alphabet*))
(defparameter *base64url-decode-table* (%make-decode-table *base64url-alphabet*))

;;; ---------------------------------------------------------------------------
;;; Encoding
;;; ---------------------------------------------------------------------------

(defun %base64-encode (bytes alphabet padp)
  "Internal encoder. ALPHABET is a 64-char string. PADP controls = padding."
  (let* ((len (length bytes))
         (out (make-array (* 4 (ceiling (max len 1) 3))
                          :element-type 'character
                          :fill-pointer 0)))
    (loop for i from 0 below len by 3
          do (let* ((b0 (aref bytes i))
                    (b1 (if (< (+ i 1) len) (aref bytes (+ i 1)) 0))
                    (b2 (if (< (+ i 2) len) (aref bytes (+ i 2)) 0))
                    (triplet (logior (ash b0 16) (ash b1 8) b2))
                    (c0 (logand #x3F (ash triplet -18)))
                    (c1 (logand #x3F (ash triplet -12)))
                    (c2 (logand #x3F (ash triplet -6)))
                    (c3 (logand #x3F triplet))
                    (remaining (- len i)))
               (vector-push (char alphabet c0) out)
               (vector-push (char alphabet c1) out)
               (if (> remaining 1)
                   (vector-push (char alphabet c2) out)
                   (when padp (vector-push #\= out)))
               (if (> remaining 2)
                   (vector-push (char alphabet c3) out)
                   (when padp (vector-push #\= out)))))
    (coerce out 'string)))

(defun base64-encode (bytes)
  "Encode a byte vector into a base64 string (standard alphabet, with padding)."
  (%base64-encode bytes *base64-alphabet* t))

(defun base64url-encode (bytes)
  "Encode a byte vector into a base64url string (URL-safe alphabet, no padding)."
  (%base64-encode bytes *base64url-alphabet* nil))

;;; ---------------------------------------------------------------------------
;;; Decoding
;;; ---------------------------------------------------------------------------

(defun %base64-decode (string decode-table)
  "Internal decoder. DECODE-TABLE maps char-code -> 0-63 or NIL."
  (let* ((len (length string))
         ;; Strip padding for length calculation
         (data-len (loop for i downfrom (1- len) above 0
                         while (char= (char string i) #\=)
                         finally (return (1+ i)))))
    (when (= (mod data-len 4) 1)
      (error "base64: invalid input length ~d" len))
    (let ((out (make-array (* 3 (ceiling data-len 4))
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0)))
      (loop with i = 0
            while (< i data-len)
            do (let ((vals (loop for j from 0 below 4
                                 collect (if (< (+ i j) data-len)
                                             (or (aref decode-table
                                                       (char-code (char string (+ i j))))
                                                 (error "base64: invalid character ~a at ~d"
                                                        (char string (+ i j)) (+ i j)))
                                             0))))
                 (let ((triplet (logior (ash (first vals) 18)
                                        (ash (second vals) 12)
                                        (ash (third vals) 6)
                                        (fourth vals)))
                       (chars-present (min (- data-len i) 4)))
                   (vector-push (logand #xFF (ash triplet -16)) out)
                   (when (> chars-present 2)
                     (vector-push (logand #xFF (ash triplet -8)) out))
                   (when (> chars-present 3)
                     (vector-push (logand #xFF triplet) out)))
                 (incf i 4)))
      (subseq out 0 (fill-pointer out)))))

(defun base64-decode (string)
  "Decode a base64 string (standard alphabet) into a byte vector."
  (%base64-decode string *base64-decode-table*))

(defun base64url-decode (string)
  "Decode a base64url string (URL-safe, handles missing padding) into a byte vector."
  (%base64-decode string *base64url-decode-table*))
