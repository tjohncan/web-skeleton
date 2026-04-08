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
  "Scalar multiplication K * POINT using double-and-add."
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

(defun ecdsa-verify-p256 (hash sig-bytes pubkey-x pubkey-y)
  "Verify an ECDSA P-256 signature.
   HASH: 32-byte SHA-256 digest of the signed message.
   SIG-BYTES: 64-byte signature (r || s, each 32 bytes big-endian).
   PUBKEY-X, PUBKEY-Y: 32-byte x and y coordinates of the public key.
   Returns T if valid, NIL otherwise."
  (let ((r (bytes-to-integer (subseq sig-bytes 0 32)))
        (s (bytes-to-integer (subseq sig-bytes 32 64)))
        (qx (bytes-to-integer pubkey-x))
        (qy (bytes-to-integer pubkey-y))
        (z (bytes-to-integer hash)))
    ;; Check r, s in [1, n-1]
    (unless (and (<= 1 r (1- +p256-n+))
                 (<= 1 s (1- +p256-n+)))
      (return-from ecdsa-verify-p256 nil))
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
        (return-from ecdsa-verify-p256 nil))
      ;; Valid if R.x mod n == r
      (= (mod (car r-point) +p256-n+) r))))
