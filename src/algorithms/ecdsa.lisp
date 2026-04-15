(in-package :web-skeleton)

;;; ===========================================================================
;;; ECDSA P-256 Signature Verification (FIPS 186-4 / SEC1)
;;;
;;; Pure Lisp using CL's native bignum arithmetic.
;;; Verification only — no signing, no key generation.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; P-256 curve parameters (NIST FIPS 186-4)
;;; ---------------------------------------------------------------------------

(defconstant +p256-p+
  #xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF
  "Field prime p.")

(defconstant +p256-a+
  #xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC
  "Curve coefficient a (= p - 3).")

(defconstant +p256-b+
  #x5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B
  "Curve coefficient b.")

(defconstant +p256-n+
  #xFFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551
  "Order of the generator point.")

(defconstant +p256-gx+
  #x6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296
  "Generator point x coordinate.")

(defconstant +p256-gy+
  #x4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5
  "Generator point y coordinate.")

;;; ---------------------------------------------------------------------------
;;; Modular arithmetic
;;; ---------------------------------------------------------------------------

(declaim (inline mod-add mod-sub mod-mul))

(defun mod-add (a b p)
  (mod (+ a b) p))

(defun mod-sub (a b p)
  (mod (- a b) p))

(defun mod-mul (a b p)
  (mod (* a b) p))

(defun mod-inv (a p)
  "Modular multiplicative inverse of A mod P via extended Euclidean algorithm."
  (let ((old-r p) (r (mod a p))
        (old-s 0) (s 1))
    (loop until (zerop r)
          do (let ((q (floor old-r r)))
               (psetf old-r r  r (- old-r (* q r)))
               (psetf old-s s  s (- old-s (* q s)))))
    (mod old-s p)))

;;; ---------------------------------------------------------------------------
;;; Elliptic curve point operations (Weierstrass form, affine coordinates)
;;;
;;; Points are (x . y) cons cells. The point at infinity is NIL.
;;; ---------------------------------------------------------------------------

(defun ec-double (point)
  "Double a point on P-256. Returns the new point or NIL."
  (when (null point) (return-from ec-double nil))
  (let* ((x (car point)) (y (cdr point)))
    (when (zerop y) (return-from ec-double nil))
    ;; lambda = (3*x^2 + a) / (2*y)
    (let* ((num (mod-add (mod-mul 3 (mod-mul x x +p256-p+) +p256-p+)
                         +p256-a+ +p256-p+))
           (den (mod-inv (mod-mul 2 y +p256-p+) +p256-p+))
           (lam (mod-mul num den +p256-p+))
           ;; x3 = lambda^2 - 2*x
           (x3 (mod-sub (mod-mul lam lam +p256-p+)
                         (mod-mul 2 x +p256-p+) +p256-p+))
           ;; y3 = lambda*(x - x3) - y
           (y3 (mod-sub (mod-mul lam (mod-sub x x3 +p256-p+) +p256-p+)
                         y +p256-p+)))
      (cons x3 y3))))

(defun ec-add (p1 p2)
  "Add two points on P-256. Returns the new point or NIL."
  (cond
    ((null p1) p2)
    ((null p2) p1)
    (t
     (let ((x1 (car p1)) (y1 (cdr p1))
           (x2 (car p2)) (y2 (cdr p2)))
       (cond
         ;; Same point — use doubling formula
         ((and (= x1 x2) (= y1 y2))
          (ec-double p1))
         ;; Additive inverse — result is point at infinity
         ((and (= x1 x2) (= (mod (+ y1 y2) +p256-p+) 0))
          nil)
         ;; General case
         (t
          (let* ((lam (mod-mul (mod-sub y2 y1 +p256-p+)
                               (mod-inv (mod-sub x2 x1 +p256-p+) +p256-p+)
                               +p256-p+))
                 (x3 (mod-sub (mod-sub (mod-mul lam lam +p256-p+)
                                       x1 +p256-p+)
                              x2 +p256-p+))
                 (y3 (mod-sub (mod-mul lam (mod-sub x1 x3 +p256-p+) +p256-p+)
                              y1 +p256-p+)))
            (cons x3 y3))))))))

(defun ec-mul (k point)
  "Scalar multiplication K * POINT using double-and-add.
   WARNING: not constant-time. Safe for verification only.
   Do NOT use for signing or key generation."
  (let ((result nil)
        (addend point))
    (loop while (> k 0)
          do (when (oddp k)
               (setf result (ec-add result addend)))
             (setf addend (ec-double addend))
             (setf k (ash k -1)))
    result))

;;; ---------------------------------------------------------------------------
;;; Byte vector <-> bignum conversion
;;; ---------------------------------------------------------------------------

(defun bytes-to-integer (bytes)
  "Convert a big-endian byte vector to a non-negative integer."
  (let ((n 0))
    (loop for b across bytes
          do (setf n (logior (ash n 8) b)))
    n))

;;; ---------------------------------------------------------------------------
;;; Public interface
;;; ---------------------------------------------------------------------------

(defun ecdsa-verify-p256-lisp (hash sig-bytes pubkey-x pubkey-y)
  "Pure-Lisp ECDSA P-256 signature verification (FIPS 186-4).
   HASH: 32-byte SHA-256 digest of the signed message.
   SIG-BYTES: 64-byte signature (r || s, each 32 bytes big-endian).
   PUBKEY-X, PUBKEY-Y: 32-byte x and y coordinates of the public key.
   Returns T if valid, NIL otherwise.

   Always reachable under this name regardless of whether
   web-skeleton-tls has been loaded — the TLS system swaps the public
   ECDSA-VERIFY-P256 symbol to a libssl-backed version at load time via
   SETF SYMBOL-FUNCTION, but this function stays accessible directly
   so the framework-dev entry point TEST-PURE-LISP-CRYPTO can re-verify
   the pure-Lisp path on a libssl-enabled machine.

   DO NOT declaim ECDSA-VERIFY-P256 inline: the libssl swap works
   through the function cell, and an inlined caller would bypass the
   cell and keep calling whichever implementation was visible at
   compile time."
  ;; Length-gate the signature before indexing into it. ES256 is
  ;; fixed-width r||s = 64 bytes; anything else is malformed, not
  ;; merely 'contains the bytes we wanted plus some'. Without this
  ;; gate the (subseq sig-bytes 0 32) / (subseq sig-bytes 32 64)
  ;; below would silently accept a 65-byte signature by ignoring
  ;; the trailing byte, and a shorter input would crash with a
  ;; subseq error instead of returning NIL.
  (unless (= (length sig-bytes) 64)
    (return-from ecdsa-verify-p256-lisp nil))
  (let ((r (bytes-to-integer (subseq sig-bytes 0 32)))
        (s (bytes-to-integer (subseq sig-bytes 32 64)))
        (qx (bytes-to-integer pubkey-x))
        (qy (bytes-to-integer pubkey-y))
        (z (bytes-to-integer hash)))
    ;; Check r, s in [1, n-1]
    ;; Both (r, s) and (r, n-s) are valid ES256 signatures per
    ;; RFC 7515 / 7518 — JOSE does not mandate low-S normalization
    ;; and mainstream issuers emit high-S roughly half the time.
    ;; A Bitcoin-style malleability wrapper can layer on top at a
    ;; call site that needs it; JWT verification does not.
    (unless (and (<= 1 r (1- +p256-n+))
                 (<= 1 s (1- +p256-n+)))
      (return-from ecdsa-verify-p256-lisp nil))
    ;; Point-on-curve check (FIPS 186-4 §5.6.2.3.3):
    ;; reject invalid-curve points to prevent small-subgroup attacks
    (unless (= (mod-mul qy qy +p256-p+)
               (mod-add (mod-add (mod-mul qx (mod-mul qx qx +p256-p+) +p256-p+)
                                 (mod-mul +p256-a+ qx +p256-p+) +p256-p+)
                         +p256-b+ +p256-p+))
      (return-from ecdsa-verify-p256-lisp nil))
    ;; w = s^-1 mod n
    (let* ((w (mod-inv s +p256-n+))
           ;; u1 = z*w mod n, u2 = r*w mod n
           (u1 (mod (* z w) +p256-n+))
           (u2 (mod (* r w) +p256-n+))
           ;; R = u1*G + u2*Q
           (g (cons +p256-gx+ +p256-gy+))
           (q (cons qx qy))
           (r-point (ec-add (ec-mul u1 g) (ec-mul u2 q))))
      (unless r-point
        (return-from ecdsa-verify-p256-lisp nil))
      ;; Valid if R.x mod n == r
      (= (mod (car r-point) +p256-n+) r))))

(defun ecdsa-verify-p256 (hash sig-bytes pubkey-x pubkey-y)
  "Verify an ECDSA P-256 signature. Delegates to ECDSA-VERIFY-P256-LISP
   by default; web-skeleton-tls replaces this function with a libssl-
   backed version at load time. JWT-VERIFY and any other caller picks
   up the swap transparently by routing through the function cell."
  (ecdsa-verify-p256-lisp hash sig-bytes pubkey-x pubkey-y))
