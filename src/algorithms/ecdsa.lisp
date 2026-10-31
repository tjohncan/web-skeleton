(in-package :web-skeleton)

;;; ===========================================================================
;;; ECDSA P-256 Signature Verification (FIPS 186-4 / SEC1)
;;;
;;; Pure Lisp using CL's native bignum arithmetic.
;;; Verification only — no signing, no key generation.
;;;
;;; A verification's cost is its field multiplications. u1 G + u2 Q is
;;; computed in Jacobian coordinates by Shamir's trick: about 256 doublings
;;; and 190 additions, each a handful of multiply-and-reduce steps, and two
;;; inversions, s^-1 mod n and the conversion back to an affine x. Affine
;;; arithmetic, which inverts at every step, costs over ten times as much.
;;;
;;; The reduction is SBCL's own bignum MOD. A fold that uses p's special
;;; form, written over bignums, costs several times more than the division
;;; it would replace. What remains to cut is the number of multiplications,
;;; which a windowed recoding would trim at the price of point tables and
;;; signed digits, in a primitive whose first job is to be right. The cost
;;; this leaves is stated in DEPLOYMENT.md; loading web-skeleton-tls
;;; replaces it with OpenSSL's.
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

;;; MOD-ADD and MOD-SUB take A and B already reduced, in [0, P), and every
;;; caller passes them so: the point arithmetic below keeps every value
;;; reduced, and the public key is range-checked before it is used. The
;;; answer is then at most one P away, and a subtraction or addition of P
;;; costs a fraction of the division MOD would make it.

(defun mod-add (a b p)
  (let ((s (+ a b)))
    (if (>= s p) (- s p) s)))

(defun mod-sub (a b p)
  (let ((d (- a b)))
    (if (minusp d) (+ d p) d)))

(defun mod-mul (a b p)
  (mod (* a b) p))

(defun mod-inv (a p)
  "Modular multiplicative inverse of A mod P via extended Euclidean
   algorithm. Raises when A has no inverse mod P (input reduces to 0,
   or gcd(A, P) != 1). Silently returning 0 for non-invertible input
   is a primitive-level footgun — a caller that inverts a value it did
   not check would get a verifier that never errors but never verifies
   either."
  (let ((old-r p) (r (mod a p))
        (old-s 0) (s 1))
    (loop until (zerop r)
          do (let ((q (floor old-r r)))
               (psetf old-r r  r (- old-r (* q r)))
               (psetf old-s s  s (- old-s (* q s)))))
    (unless (= old-r 1)
      (error "mod-inv: ~d has no inverse mod ~d" a p))
    (mod old-s p)))

;;; ---------------------------------------------------------------------------
;;; Elliptic curve point operations (Weierstrass form, Jacobian coordinates)
;;;
;;; A point is three integers X Y Z, passed and returned as multiple values,
;;; standing for the affine point (X/Z^2, Y/Z^3). Any Z of 0 is the point at
;;; infinity; (1 1 0) is the one these functions make. An affine double or
;;; add divides once per step, and a division is a whole extended-Euclid
;;; run. These multiply and reduce instead, leaving one inversion for the
;;; conversion back at the end of a verification.
;;;
;;; Not constant-time, and verification does not need it to be: every input
;;; is public. Nothing here is fit for signing or key generation.
;;; ---------------------------------------------------------------------------

(defun jacobian-double (x y z)
  "2P for P = (X Y Z), as (values X3 Y3 Z3), by dbl-2001-b for a = -3.

   The point at infinity doubles to itself with no test for it, because
   Z3 = 2YZ is 0 whenever Z is. Y = 0, a point of order two, would do the
   same, and P-256 has none."
  (let* ((p +p256-p+)
         (delta (mod-mul z z p))
         (gamma (mod-mul y y p))
         (beta (mod-mul x gamma p))
         ;; 3(X - Z^2)(X + Z^2) = 3X^2 + aZ^4, with a = -3.
         (alpha (mod-mul 3 (mod-mul (mod-sub x delta p) (mod-add x delta p) p) p))
         (x3 (mod-sub (mod-mul alpha alpha p) (mod-mul 8 beta p) p))
         ;; (Y + Z)^2 - Y^2 - Z^2 = 2YZ.
         (z3 (mod-sub (mod-sub (mod-mul (+ y z) (+ y z) p) gamma p) delta p))
         (y3 (mod-sub (mod-mul alpha (mod-sub (mod-mul 4 beta p) x3 p) p)
                      (mod-mul 8 (mod-mul gamma gamma p) p) p)))
    (values x3 y3 z3)))

(defun jacobian-add (x1 y1 z1 x2 y2 z2)
  "P1 + P2 for P1 = (X1 Y1 Z1) and P2 = (X2 Y2 Z2), as (values X3 Y3 Z3),
   by add-1998-cmo-2.

   The general formula is wrong in two cases a verifier can be steered
   into, and both are decided before it. When either point is at
   infinity it answers infinity instead of the other point. When P1 = P2
   it answers infinity instead of 2P1. P1 = P2 and P1 = -P2 both show up
   as H = 0, the same affine x, and are told apart by R. -P2 is answered
   with infinity here, though the formula would get it right with Z3 = 0,
   so all three cases are visible in one place. ECDSA-VERIFY-P256-LISP
   reaches all of them. A public key of G or -G puts P1 = P2 or P1 = -P2
   into Shamir's table, and a digest crafted to make u1 G + u2 Q vanish
   puts P1 = -P2 into the running sum."
  (cond
    ((zerop z1) (values x2 y2 z2))
    ((zerop z2) (values x1 y1 z1))
    (t
     (let* ((p +p256-p+)
            (z1z1 (mod-mul z1 z1 p))
            (z2z2 (mod-mul z2 z2 p))
            (u1 (mod-mul x1 z2z2 p))
            (u2 (mod-mul x2 z1z1 p))
            (s1 (mod-mul y1 (mod-mul z2 z2z2 p) p))
            (s2 (mod-mul y2 (mod-mul z1 z1z1 p) p))
            (h (mod-sub u2 u1 p))
            (r (mod-sub s2 s1 p)))
       (if (zerop h)
           (if (zerop r)
               (jacobian-double x1 y1 z1)
               (values 1 1 0))
           (let* ((hh (mod-mul h h p))
                  (hhh (mod-mul h hh p))
                  (v (mod-mul u1 hh p))
                  (x3 (mod-sub (mod-sub (mod-mul r r p) hhh p)
                               (mod-mul 2 v p) p))
                  (y3 (mod-sub (mod-mul r (mod-sub v x3 p) p)
                               (mod-mul s1 hhh p) p))
                  (z3 (mod-mul h (mod-mul z1 z2 p) p)))
             (values x3 y3 z3)))))))

(defun ec-mul-add (u1 u2 qx qy)
  "U1 G + U2 Q for the affine point Q = (QX, QY), as Jacobian
   (values X Y Z).

   Shamir's trick: one pass over the bits of U1 and U2 together, high to
   low, with a doubling per bit and at most one addition — of G, Q or
   G + Q, by which of the two bits are set. The two scalar
   multiplications share their doublings, so the pass costs about what
   one of them would alone.

   G + Q is built with the same JACOBIAN-ADD, so a Q of G makes it 2G and
   a Q of -G makes it the point at infinity, which then adds as nothing."
  (multiple-value-bind (sx sy sz)
      (jacobian-add +p256-gx+ +p256-gy+ 1 qx qy 1)
    (let ((x 1) (y 1) (z 0))
      (loop for i from (1- (max (integer-length u1) (integer-length u2)))
              downto 0
            do (multiple-value-setq (x y z) (jacobian-double x y z))
               (let ((g (logbitp i u1))
                     (q (logbitp i u2)))
                 (cond ((and g q)
                        (multiple-value-setq (x y z)
                          (jacobian-add x y z sx sy sz)))
                       (g
                        (multiple-value-setq (x y z)
                          (jacobian-add x y z +p256-gx+ +p256-gy+ 1)))
                       (q
                        (multiple-value-setq (x y z)
                          (jacobian-add x y z qx qy 1))))))
      (values x y z))))

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
   Returns T if valid, NIL otherwise. All four are (unsigned-byte 8)
   vectors, taken through OCTETS-ARGUMENT as ECDSA-VERIFY-P256-LIBSSL
   takes them: an argument of another type signals, because it is the
   caller's mistake rather than a signature that failed to verify.

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
  ;; The type contract first, shared with the libssl side: see
  ;; OCTETS-ARGUMENT.
  (setf hash      (octets-argument hash 'ecdsa-verify-p256)
        sig-bytes (octets-argument sig-bytes 'ecdsa-verify-p256)
        pubkey-x  (octets-argument pubkey-x 'ecdsa-verify-p256)
        pubkey-y  (octets-argument pubkey-y 'ecdsa-verify-p256))
  ;; Length-gate the signature before indexing into it. ES256 is
  ;; fixed-width r||s = 64 bytes; anything else is malformed, not
  ;; merely 'contains the bytes we wanted plus some'. Without this
  ;; gate the (subseq sig-bytes 0 32) / (subseq sig-bytes 32 64)
  ;; below would silently accept a 65-byte signature by ignoring
  ;; the trailing byte, and a shorter input would crash with a
  ;; subseq error instead of returning NIL.
  ;; HASH is gated with the other three rather than trusted. The argument
  ;; the comment above makes for gating SIG-BYTES and the coordinates is
  ;; an argument about this primitive's callers in general, and it holds
  ;; for HASH as much as for them. JWT, the one caller, always passes a
  ;; 32-byte SHA-256 digest, so for it the gate is hygiene — but a short
  ;; HASH would otherwise be silently left-padded by BYTES-TO-INTEGER into
  ;; a different message, and verification against the wrong message is
  ;; exactly the answer a verifier must never give quietly.
  (unless (= (length hash) 32)
    (return-from ecdsa-verify-p256-lisp nil))
  (unless (= (length sig-bytes) 64)
    (return-from ecdsa-verify-p256-lisp nil))
  (unless (and (= (length pubkey-x) 32) (= (length pubkey-y) 32))
    (return-from ecdsa-verify-p256-lisp nil))
  (let ((r (bytes-to-integer (subseq sig-bytes 0 32)))
        (s (bytes-to-integer (subseq sig-bytes 32 64)))
        (qx (bytes-to-integer pubkey-x))
        (qy (bytes-to-integer pubkey-y))
        (z (bytes-to-integer hash)))
    ;; FIPS 186-4 §5.6.2.3.3: public-key coordinates must lie in
    ;; [0, p-1]. The 32-byte input range is [0, 2^256 - 1] which
    ;; exceeds +p256-p+ (= 2^256 - 2^224 + 2^192 + 2^96 - 1). An
    ;; unreduced coordinate names the same point as its reduced form —
    ;; the curve-equation check below and every step after it reduce —
    ;; so without this one key would have two spellings that both
    ;; verify. Refused instead.
    (unless (and (< qx +p256-p+) (< qy +p256-p+))
      (return-from ecdsa-verify-p256-lisp nil))
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
           (u2 (mod (* r w) +p256-n+)))
      ;; R = u1*G + u2*Q. The point at infinity verifies nothing.
      (multiple-value-bind (x y zr) (ec-mul-add u1 u2 qx qy)
        (declare (ignore y))
        (unless (zerop zr)
          ;; Valid if R.x mod n == r, R.x being X / Z^2: the one
          ;; inversion the Jacobian arithmetic leaves.
          (= (mod (mod-mul x (mod-inv (mod-mul zr zr +p256-p+) +p256-p+)
                           +p256-p+)
                  +p256-n+)
             r))))))

(defun ecdsa-verify-p256 (hash sig-bytes pubkey-x pubkey-y)
  "Verify an ECDSA P-256 signature. Delegates to ECDSA-VERIFY-P256-LISP
   by default; web-skeleton-tls replaces this function with a libssl-
   backed version at load time. JWT-VERIFY and any other caller picks
   up the swap transparently by routing through the function cell."
  (ecdsa-verify-p256-lisp hash sig-bytes pubkey-x pubkey-y))
