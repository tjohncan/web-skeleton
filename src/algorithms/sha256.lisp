(in-package :web-skeleton)

;;; ===========================================================================
;;; SHA-256 (FIPS 180-4)
;;;
;;; Takes a byte vector, returns a 32-byte digest.
;;; Same Merkle-Damgard structure as SHA-1 but with 8 working variables,
;;; 64 rounds, and different logical functions.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; 32-bit word operations (u32+, u32-rotate-left from word32.lisp)
;;; ---------------------------------------------------------------------------

(declaim (inline u32-right-rotate u32-right-shift))

(defun u32-right-rotate (n count)
  "Rotate 32-bit integer N right by COUNT bits."
  (logand #xFFFFFFFF
          (logior (ash n (- count))
                  (ash n (- 32 count)))))

(defun u32-right-shift (n count)
  "Shift 32-bit integer N right by COUNT bits (zero-fill)."
  (ash n (- count)))

;;; ---------------------------------------------------------------------------
;;; SHA-256 logical functions
;;; ---------------------------------------------------------------------------

(declaim (inline sha256-ch sha256-maj sha256-big-sigma0 sha256-big-sigma1
                 sha256-small-sigma0 sha256-small-sigma1))

(defun sha256-ch (x y z)
  (logand #xFFFFFFFF (logxor (logand x y) (logand (logxor x #xFFFFFFFF) z))))

(defun sha256-maj (x y z)
  (logand #xFFFFFFFF (logxor (logand x y) (logand x z) (logand y z))))

(defun sha256-big-sigma0 (x)
  (logand #xFFFFFFFF
          (logxor (u32-right-rotate x 2)
                  (u32-right-rotate x 13)
                  (u32-right-rotate x 22))))

(defun sha256-big-sigma1 (x)
  (logand #xFFFFFFFF
          (logxor (u32-right-rotate x 6)
                  (u32-right-rotate x 11)
                  (u32-right-rotate x 25))))

(defun sha256-small-sigma0 (x)
  (logand #xFFFFFFFF
          (logxor (u32-right-rotate x 7)
                  (u32-right-rotate x 18)
                  (u32-right-shift x 3))))

(defun sha256-small-sigma1 (x)
  (logand #xFFFFFFFF
          (logxor (u32-right-rotate x 17)
                  (u32-right-rotate x 19)
                  (u32-right-shift x 10))))

;;; ---------------------------------------------------------------------------
;;; Round constants — first 32 bits of the fractional parts of the
;;; cube roots of the first 64 primes (2..311)
;;; ---------------------------------------------------------------------------

(defparameter *sha256-k*
  (make-array 64 :element-type '(unsigned-byte 32)
              :initial-contents
              '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
                #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
                #xd807aa98 #x12835b01 #x243185be #x550c7dc3
                #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
                #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
                #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
                #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
                #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
                #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
                #x650a7354 #x766a0abb #x81c2c92e #x92722c85
                #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
                #xd192e819 #xd6990624 #xf40e3585 #x106aa070
                #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
                #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
                #x748f82ee #x78a5636f #x84c87814 #x8cc70208
                #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)))

;;; ---------------------------------------------------------------------------
;;; Padding — identical structure to SHA-1 (64-byte block alignment)
;;; ---------------------------------------------------------------------------

(defun sha256-pad (message)
  "Pad MESSAGE (a byte vector) per FIPS 180-4. Returns a new byte vector."
  (let* ((len (length message))
         (bit-len (* len 8))
         (padded-len (let ((base (+ len 1 8)))
                       (* 64 (ceiling base 64))))
         (padded (make-array padded-len
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (replace padded message)
    (setf (aref padded len) #x80)
    (let ((offset (- padded-len 8)))
      (loop for i from 0 below 8
            do (setf (aref padded (+ offset i))
                     (logand #xFF (ash bit-len (* -8 (- 7 i)))))))
    padded))

;;; ---------------------------------------------------------------------------
;;; Block processing
;;; ---------------------------------------------------------------------------

(defun sha256-process-block (block h0 h1 h2 h3 h4 h5 h6 h7)
  "Process a single 512-bit (64-byte) BLOCK. Returns updated hash values
   as (values h0 h1 h2 h3 h4 h5 h6 h7)."
  (let ((w (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0)))
    ;; First 16 words: big-endian from block
    (loop for i from 0 below 16
          for offset = (* i 4)
          do (setf (aref w i)
                   (logior (ash (aref block offset)       24)
                           (ash (aref block (+ offset 1)) 16)
                           (ash (aref block (+ offset 2))  8)
                                (aref block (+ offset 3)))))
    ;; Words 16-63: message schedule
    (loop for i from 16 below 64
          do (setf (aref w i)
                   (u32+ (sha256-small-sigma1 (aref w (- i 2)))
                         (aref w (- i 7))
                         (sha256-small-sigma0 (aref w (- i 15)))
                         (aref w (- i 16)))))
    ;; Initialize working variables
    (let ((a h0) (b h1) (c h2) (d h3) (e h4) (f h5) (g h6) (h h7))
      ;; 64 rounds
      (loop for i from 0 below 64
            do (let* ((t1 (u32+ h
                                (sha256-big-sigma1 e)
                                (sha256-ch e f g)
                                (aref *sha256-k* i)
                                (aref w i)))
                      (t2 (u32+ (sha256-big-sigma0 a)
                                (sha256-maj a b c))))
                 (setf h g
                       g f
                       f e
                       e (u32+ d t1)
                       d c
                       c b
                       b a
                       a (u32+ t1 t2))))
      (values (u32+ h0 a) (u32+ h1 b) (u32+ h2 c) (u32+ h3 d)
              (u32+ h4 e) (u32+ h5 f) (u32+ h6 g) (u32+ h7 h)))))

;;; ---------------------------------------------------------------------------
;;; Public interface
;;; ---------------------------------------------------------------------------

(defun sha256 (data)
  "Compute SHA-256 digest of DATA (a byte vector). Returns a 32-byte vector."
  (let ((padded (sha256-pad data)))
    ;; Initial hash values (FIPS 180-4 §5.3.3)
    (let ((h0 #x6a09e667) (h1 #xbb67ae85)
          (h2 #x3c6ef372) (h3 #xa54ff53a)
          (h4 #x510e527f) (h5 #x9b05688c)
          (h6 #x1f83d9ab) (h7 #x5be0cd19))
      (loop for offset from 0 below (length padded) by 64
            do (let ((block (subseq padded offset (+ offset 64))))
                 (multiple-value-setq (h0 h1 h2 h3 h4 h5 h6 h7)
                   (sha256-process-block block h0 h1 h2 h3 h4 h5 h6 h7))))
      (let ((digest (make-array 32 :element-type '(unsigned-byte 8))))
        (flet ((pack-u32 (val offset)
                 (setf (aref digest offset)       (logand #xFF (ash val -24))
                       (aref digest (+ offset 1)) (logand #xFF (ash val -16))
                       (aref digest (+ offset 2)) (logand #xFF (ash val -8))
                       (aref digest (+ offset 3)) (logand #xFF val))))
          (pack-u32 h0  0)  (pack-u32 h1  4)
          (pack-u32 h2  8)  (pack-u32 h3 12)
          (pack-u32 h4 16)  (pack-u32 h5 20)
          (pack-u32 h6 24)  (pack-u32 h7 28))
        digest))))

(defun sha256-hex (data)
  "Compute SHA-256 of DATA and return as a lowercase hex string."
  (bytes-to-hex (sha256 data)))
