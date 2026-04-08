(in-package :web-skeleton)

;;; ===========================================================================
;;; SHA-1 (FIPS 180-4)
;;;
;;; Takes a byte vector, returns a 20-byte digest.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; 32-bit word operations
;;;
;;; SHA-1 works entirely in 32-bit unsigned integers. Common Lisp has
;;; arbitrary-precision integers, so we mask to 32 bits after every
;;; operation to stay in range.
;;; ---------------------------------------------------------------------------

(declaim (inline u32+ u32-rotate-left))

(defun u32+ (&rest args)
  "Add any number of values, masked to 32 bits."
  (logand #xFFFFFFFF (apply #'+ args)))

(defun u32-rotate-left (n count)
  "Rotate 32-bit integer N left by COUNT bits."
  (logand #xFFFFFFFF
          (logior (ash n count)
                  (ash n (- count 32)))))

;;; ---------------------------------------------------------------------------
;;; SHA-1 round function and constants
;;;
;;; 80 rounds in 4 groups of 20. Each group uses a different logical
;;; function and a different constant.
;;;
;;;   Rounds  0-19: Ch(b,c,d)     = (b AND c) OR ((NOT b) AND d)   K = 5A827999
;;;   Rounds 20-39: Parity(b,c,d) = b XOR c XOR d                  K = 6ED9EBA1
;;;   Rounds 40-59: Maj(b,c,d)    = (b AND c) OR (b AND d) OR ...  K = 8F1BBCDC
;;;   Rounds 60-79: Parity(b,c,d) = b XOR c XOR d                  K = CA62C1D6
;;; ---------------------------------------------------------------------------

(defun sha1-f (round b c d)
  "SHA-1 logical function for the given ROUND index."
  (cond
    ((< round 20) (logior (logand b c)
                          (logand (logxor b #xFFFFFFFF) d)))
    ((< round 40) (logxor b c d))
    ((< round 60) (logior (logand b c) (logand b d) (logand c d)))
    (t            (logxor b c d))))

(defun sha1-k (round)
  "SHA-1 constant for the given ROUND index."
  (cond
    ((< round 20) #x5A827999)
    ((< round 40) #x6ED9EBA1)
    ((< round 60) #x8F1BBCDC)
    (t            #xCA62C1D6)))

;;; ---------------------------------------------------------------------------
;;; Padding
;;;
;;; The message is padded so its length is a multiple of 512 bits (64 bytes):
;;;   1. Append a 1 bit (byte #x80)
;;;   2. Append zero bytes until length ≡ 56 (mod 64)
;;;   3. Append the original message length in bits as a 64-bit big-endian integer
;;; ---------------------------------------------------------------------------

(defun sha1-pad (message)
  "Pad MESSAGE (a byte vector) per FIPS 180-4. Returns a new byte vector."
  (let* ((len (length message))
         (bit-len (* len 8))
         ;; Space needed: message + 1 byte (0x80) + padding + 8 bytes (length)
         (padded-len (let ((base (+ len 1 8)))
                       (* 64 (ceiling base 64))))
         (padded (make-array padded-len
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    ;; Copy message
    (replace padded message)
    ;; Append 1-bit
    (setf (aref padded len) #x80)
    ;; Append length in bits as 64-bit big-endian at the end
    (let ((offset (- padded-len 8)))
      (loop for i from 0 below 8
            do (setf (aref padded (+ offset i))
                     (logand #xFF (ash bit-len (* -8 (- 7 i)))))))
    padded))

;;; ---------------------------------------------------------------------------
;;; Block processing
;;; ---------------------------------------------------------------------------

(defun sha1-process-block (block h0 h1 h2 h3 h4)
  "Process a single 512-bit (64-byte) BLOCK. Returns updated hash values
   as (values h0 h1 h2 h3 h4)."
  ;; Prepare message schedule — 80 words from the 16-word block
  (let ((w (make-array 80 :element-type '(unsigned-byte 32) :initial-element 0)))
    ;; First 16 words: read from block as big-endian 32-bit integers
    (loop for i from 0 below 16
          for offset = (* i 4)
          do (setf (aref w i)
                   (logior (ash (aref block offset)       24)
                           (ash (aref block (+ offset 1)) 16)
                           (ash (aref block (+ offset 2))  8)
                                (aref block (+ offset 3)))))
    ;; Words 16-79: XOR and rotate
    (loop for i from 16 below 80
          do (setf (aref w i)
                   (u32-rotate-left
                    (logxor (aref w (- i 3))
                            (aref w (- i 8))
                            (aref w (- i 14))
                            (aref w (- i 16)))
                    1)))
    ;; Initialize working variables
    (let ((a h0) (b h1) (c h2) (d h3) (e h4))
      ;; 80 rounds
      (loop for i from 0 below 80
            do (let ((temp (u32+ (u32-rotate-left a 5)
                                 (sha1-f i b c d)
                                 e
                                 (sha1-k i)
                                 (aref w i))))
                 (setf e d
                       d c
                       c (u32-rotate-left b 30)
                       b a
                       a temp)))
      (values (u32+ h0 a) (u32+ h1 b) (u32+ h2 c)
              (u32+ h3 d) (u32+ h4 e)))))

;;; ---------------------------------------------------------------------------
;;; Public interface
;;; ---------------------------------------------------------------------------

(defun sha1 (data)
  "Compute SHA-1 digest of DATA (a byte vector). Returns a 20-byte vector."
  (let ((padded (sha1-pad data)))
    ;; Initial hash values (FIPS 180-4 §5.3.1)
    (let ((h0 #x67452301)
          (h1 #xEFCDAB89)
          (h2 #x98BADCFE)
          (h3 #x10325476)
          (h4 #xC3D2E1F0))
      ;; Process each 64-byte block
      (loop for offset from 0 below (length padded) by 64
            do (let ((block (subseq padded offset (+ offset 64))))
                 (multiple-value-setq (h0 h1 h2 h3 h4)
                   (sha1-process-block block h0 h1 h2 h3 h4))))
      ;; Produce the 20-byte digest
      (let ((digest (make-array 20 :element-type '(unsigned-byte 8))))
        (flet ((pack-u32 (val offset)
                 (setf (aref digest offset)       (logand #xFF (ash val -24))
                       (aref digest (+ offset 1)) (logand #xFF (ash val -16))
                       (aref digest (+ offset 2)) (logand #xFF (ash val -8))
                       (aref digest (+ offset 3)) (logand #xFF val))))
          (pack-u32 h0 0)
          (pack-u32 h1 4)
          (pack-u32 h2 8)
          (pack-u32 h3 12)
          (pack-u32 h4 16))
        digest))))

(defun sha1-hex (data)
  "Compute SHA-1 of DATA and return as a lowercase hex string."
  (bytes-to-hex (sha1 data)))
