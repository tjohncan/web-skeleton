(in-package :web-skeleton)

;;; ===========================================================================
;;; SHA-1 (FIPS 180-4)
;;;
;;; Takes a byte vector, returns a 20-byte digest.
;;; ===========================================================================

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

(defun sha1-process-block (data start w h0 h1 h2 h3 h4)
  "Process the 512-bit (64-byte) block at DATA[START..START+64).
   Returns updated hash values as (values h0 h1 h2 h3 h4).

   DATA is the whole padded message and START the block's offset within
   it — not a freshly copied 64-byte block — and W is a caller-owned
   80-word scratch schedule reused across every block of one digest.
   Both exist to keep the hot loop allocation-free: a per-block SUBSEQ
   plus a per-block schedule cost ~350 bytes of garbage for every 64
   bytes of input, which was essentially all of the pure-Lisp digest's
   garbage (measured: 7.5 bytes consed per input byte, down to 1.0 — and
   that 1.0 is the one padded copy). Throughput moves ~10%; the real win
   is the GC pressure a busy server no longer pays. Every W slot is
   written before it is read (0-15 from the block, 16-79 derived from
   those), so reuse needs no clearing. SHA1-LISP allocates W per call, so
   concurrent digests never share one."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type (simple-array (unsigned-byte 32) (*)) w)
           (type fixnum start))
  ;; First 16 words: read from the block as big-endian 32-bit integers
  (loop for i of-type fixnum from 0 below 16
        for offset of-type fixnum = (+ start (* i 4))
        do (setf (aref w i)
                 (logior (ash (aref data offset)       24)
                         (ash (aref data (+ offset 1)) 16)
                         (ash (aref data (+ offset 2))  8)
                              (aref data (+ offset 3)))))
  ;; Words 16-79: XOR and rotate
  (loop for i of-type fixnum from 16 below 80
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
    (loop for i of-type fixnum from 0 below 80
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
            (u32+ h3 d) (u32+ h4 e))))

;;; ---------------------------------------------------------------------------
;;; Public interface
;;; ---------------------------------------------------------------------------

(defun sha1-lisp (data)
  "Pure-Lisp SHA-1 (FIPS 180-4). Takes a byte vector, returns a 20-byte
   digest vector. Always reachable under this name regardless of whether
   web-skeleton-tls has been loaded — the TLS system swaps the public
   SHA1 symbol to a libssl-backed version at load time via SETF
   SYMBOL-FUNCTION, but this function stays accessible directly so the
   framework-dev entry point TEST-PURE-LISP-CRYPTO can re-verify the
   pure-Lisp path on a libssl-enabled machine.

   DO NOT declaim SHA1 inline: the libssl swap works through the
   function cell, and an inlined caller would bypass the cell and keep
   calling whichever implementation was visible at compile time."
  (let ((padded (sha1-pad data))
        ;; One scratch schedule for the whole digest — see
        ;; SHA1-PROCESS-BLOCK. Allocated per call, so concurrent
        ;; digests stay independent.
        (w (make-array 80 :element-type '(unsigned-byte 32)
                          :initial-element 0)))
    ;; Initial hash values (FIPS 180-4 §5.3.1)
    (let ((h0 #x67452301)
          (h1 #xEFCDAB89)
          (h2 #x98BADCFE)
          (h3 #x10325476)
          (h4 #xC3D2E1F0))
      ;; Process each 64-byte block in place — no per-block copy.
      (loop for offset from 0 below (length padded) by 64
            do (multiple-value-setq (h0 h1 h2 h3 h4)
                 (sha1-process-block padded offset w h0 h1 h2 h3 h4)))
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

(defun sha1 (data)
  "Compute SHA-1 digest of DATA (byte vector). Returns a 20-byte vector.
   Delegates to SHA1-LISP by default; web-skeleton-tls replaces this
   function with a libssl-backed version at load time. SHA1-HEX and any
   other caller picks up the swap transparently by routing through the
   function cell."
  (sha1-lisp data))

(defun sha1-hex (data)
  "Compute SHA-1 of DATA and return as a lowercase hex string."
  (bytes-to-hex (sha1 data)))
