(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; Algorithm tests — SHA-1, SHA-256, Base64, ECDSA, HMAC, Hex
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; SHA-1 test vectors (FIPS 180-4 and RFC 3174)
;;; ---------------------------------------------------------------------------

(defun test-sha1 ()
  (format t "~%SHA-1~%")

  ;; FIPS 180-4 one-block message
  (check "abc"
         (sha1-hex (sb-ext:string-to-octets "abc"))
         "a9993e364706816aba3e25717850c26c9cd0d89d")

  ;; FIPS 180-4 two-block message
  (check "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
         (sha1-hex (sb-ext:string-to-octets
                    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
         "84983e441c3bd26ebaae4aa1f95129e5e54670f1")

  ;; Empty string
  (check "empty string"
         (sha1-hex (sb-ext:string-to-octets ""))
         "da39a3ee5e6b4b0d3255bfef95601890afd80709")

  ;; Single character
  (check "single char 'a'"
         (sha1-hex (sb-ext:string-to-octets "a"))
         "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"))

;;; ---------------------------------------------------------------------------
;;; Base64 test vectors (RFC 4648 §10)
;;; ---------------------------------------------------------------------------

(defun test-base64 ()
  (format t "~%Base64~%")

  (check "empty"
         (base64-encode (sb-ext:string-to-octets ""))
         "")

  (check "f"
         (base64-encode (sb-ext:string-to-octets "f"))
         "Zg==")

  (check "fo"
         (base64-encode (sb-ext:string-to-octets "fo"))
         "Zm8=")

  (check "foo"
         (base64-encode (sb-ext:string-to-octets "foo"))
         "Zm9v")

  (check "foob"
         (base64-encode (sb-ext:string-to-octets "foob"))
         "Zm9vYg==")

  (check "fooba"
         (base64-encode (sb-ext:string-to-octets "fooba"))
         "Zm9vYmE=")

  (check "foobar"
         (base64-encode (sb-ext:string-to-octets "foobar"))
         "Zm9vYmFy")

  ;; WebSocket-relevant: base64 of a SHA-1 digest
  (check "sha1+base64 combined (websocket accept key)"
         (base64-encode (sha1 (sb-ext:string-to-octets
                               "dGhlIHNhbXBsZSBub25jZQ==258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
         "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="))

(defun test-base64-decode ()
  (format t "~%Base64 Decode~%")

  ;; RFC 4648 §10 round-trips
  (check "empty"
         (sb-ext:octets-to-string (base64-decode "") :external-format :utf-8)
         "")

  (check "f"
         (sb-ext:octets-to-string (base64-decode "Zg==") :external-format :utf-8)
         "f")

  (check "fo"
         (sb-ext:octets-to-string (base64-decode "Zm8=") :external-format :utf-8)
         "fo")

  (check "foo"
         (sb-ext:octets-to-string (base64-decode "Zm9v") :external-format :utf-8)
         "foo")

  (check "foob"
         (sb-ext:octets-to-string (base64-decode "Zm9vYg==") :external-format :utf-8)
         "foob")

  (check "fooba"
         (sb-ext:octets-to-string (base64-decode "Zm9vYmE=") :external-format :utf-8)
         "fooba")

  (check "foobar"
         (sb-ext:octets-to-string (base64-decode "Zm9vYmFy") :external-format :utf-8)
         "foobar")

  ;; Invalid: single character (6 bits, can't form a byte)
  (flet ((signals-error-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    (check "single char rejected"
           (signals-error-p (lambda () (base64-decode "A"))) t)

    ;; RFC 4648 §3.5 canonical form. The final group's unused low bits
    ;; must be zero or one byte string has sixteen spellings: "QQ" and
    ;; "QR" differ only in bits the decoder never emits.
    (check "2-char group: canonical trailing bits accepted"
           (bytes-to-hex (base64-decode "QQ")) "41")
    (check "2-char group: non-canonical trailing bits rejected"
           (signals-error-p (lambda () (base64-decode "QR"))) t)
    (check "3-char group: canonical trailing bits accepted"
           (bytes-to-hex (base64-decode "QUE")) "4141")
    (check "3-char group: non-canonical trailing bits rejected"
           (signals-error-p (lambda () (base64-decode "QUF"))) t)
    ;; Padded spellings of those same two groups. Padding and the bit
    ;; check are separate guards, so each verdict has to survive both.
    (check "padded 2-char group accepted"
           (bytes-to-hex (base64-decode "QQ==")) "41")
    (check "padded non-canonical 2-char group rejected"
           (signals-error-p (lambda () (base64-decode "QR=="))) t)

    ;; Padding must complete the final group and then stop. The
    ;; trailing-'=' scan used to swallow a stray pad, so "AAAA=" decoded
    ;; as "AAAA" — a second spelling of three zero bytes.
    (check "stray pad after a complete group rejected"
           (signals-error-p (lambda () (base64-decode "AAAA="))) t)
    (check "short pad rejected"
           (signals-error-p (lambda () (base64-decode "Zg="))) t)
    (check "over-padded group rejected"
           (signals-error-p (lambda () (base64-decode "AAAA===="))) t)
    (check "all-padding input rejected"
           (signals-error-p (lambda () (base64-decode "===="))) t)
    (check "lone pad rejected"
           (signals-error-p (lambda () (base64-decode "="))) t)
    ;; Interior padding was already rejected by the charset check; keep
    ;; it asserted so the new padding guard can't accidentally take over
    ;; and start allowing it.
    (check "interior pad rejected"
           (signals-error-p (lambda () (base64-decode "Zg==Zg=="))) t)))

(defun test-base64url ()
  (format t "~%Base64url~%")

  ;; URL-safe encode: no padding, - instead of +, _ instead of /
  (check "url encode no padding"
         (base64url-encode (sb-ext:string-to-octets "f"))
         "Zg")

  (check "url encode replaces + with -"
         (base64url-encode (sha1 (sb-ext:string-to-octets
                                  "dGhlIHNhbXBsZSBub25jZQ==258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
         "s3pPLMBiTxaQ9kYGzzhZRbK-xOo")

  ;; URL-safe decode: handles missing padding
  (check "url decode no padding"
         (sb-ext:octets-to-string (base64url-decode "Zg") :external-format :utf-8)
         "f")

  (check "url decode with - and _"
         (bytes-to-hex (base64url-decode "s3pPLMBiTxaQ9kYGzzhZRbK-xOo"))
         (sha1-hex (sb-ext:string-to-octets
                    "dGhlIHNhbXBsZSBub25jZQ==258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))

  ;; Round-trip: encode then decode
  (let ((data (sb-ext:string-to-octets "the quick brown ankle leaps over the lazy heel")))
    (check "url round-trip"
           (bytes-to-hex (base64url-decode (base64url-encode data)))
           (bytes-to-hex data)))

  ;; Invalid: single character rejected
  (flet ((signals-error-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    (check "url single char rejected"
           (signals-error-p (lambda () (base64url-decode "A"))) t)

    ;; Token malleability, the reason the canonical check exists. A
    ;; P-256 signature is 64 bytes, so its base64url is 86 characters —
    ;; a final group of two, whose last character carries four bits that
    ;; are never emitted. Rewriting the trailing 'Q' as 'R' left the 64
    ;; decoded bytes untouched, so the mutated token verified against
    ;; the same key: one signature, two token strings, and any check
    ;; keyed on the text (revocation, replay, audit) fooled.
    (let* ((sig "DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmXFCgfTjDxw5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q")
           (twin (concatenate 'string (subseq sig 0 (1- (length sig))) "R")))
      (check "signature segment is 86 chars (2-char final group)"
             (mod (length sig) 4) 2)
      (check "signature segment decodes to 64 bytes"
             (length (base64url-decode sig)) 64)
      ;; Canonical input has exactly one spelling, and it is the one the
      ;; encoder produces — so decode is injective over what it accepts.
      (check "signature segment round-trips to its own spelling"
             (base64url-encode (base64url-decode sig)) sig)
      (check "non-canonical twin of a signature segment rejected"
             (signals-error-p (lambda () (base64url-decode twin))) t))

    ;; base64url omits padding, so unpadded input stays legal — the new
    ;; padding guard must not have quietly made it mandatory.
    (check "url unpadded 2-char group still accepted"
           (bytes-to-hex (base64url-decode "QQ")) "41")
    (check "url non-canonical trailing bits rejected"
           (signals-error-p (lambda () (base64url-decode "QR"))) t)))

;;; ---------------------------------------------------------------------------
;;; SHA-256 test vectors (FIPS 180-4)
;;; ---------------------------------------------------------------------------

(defun test-sha256 ()
  (format t "~%SHA-256~%")

  ;; FIPS 180-4 one-block message
  (check "abc"
         (sha256-hex (sb-ext:string-to-octets "abc"))
         "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

  ;; FIPS 180-4 two-block message
  (check "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
         (sha256-hex (sb-ext:string-to-octets
                      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
         "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")

  ;; Empty string
  (check "empty string"
         (sha256-hex (sb-ext:string-to-octets ""))
         "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

  ;; Single character
  (check "single char 'a'"
         (sha256-hex (sb-ext:string-to-octets "a"))
         "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"))

;;; ---------------------------------------------------------------------------
;;; ECDSA P-256 verification tests
;;; ---------------------------------------------------------------------------

(defun test-ecdsa ()
  (format t "~%ECDSA P-256~%")

  ;; Test vector: RFC 7515 Appendix A.3 (ES256 JWS)
  ;; The signing input, key coordinates, and signature from the RFC example.
  (let* ((signing-input "eyJhbGciOiJFUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ")
         (hash (sha256 (sb-ext:string-to-octets signing-input
                                                 :external-format :ascii)))
         ;; Public key from RFC 7515 A.3
         (x (base64url-decode "f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU"))
         (y (base64url-decode "x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0"))
         ;; Signature from RFC 7515 A.3 as published — high-S, since
         ;; RFC 7515 / 7518 do not mandate low-S normalization and
         ;; mainstream ES256 issuers emit either form.
         (sig (base64url-decode "DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmXFCgfTjDxw5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q")))
    (check "rfc7515 A.3 signature"
           (ecdsa-verify-p256 hash sig x y)
           t)

    ;; Tampered hash should fail
    (let ((bad-hash (copy-seq hash)))
      (setf (aref bad-hash 0) (logxor (aref bad-hash 0) #xFF))
      (check "tampered hash rejects"
             (ecdsa-verify-p256 bad-hash sig x y)
             nil))

    ;; Tampered signature should fail
    (let ((bad-sig (copy-seq sig)))
      (setf (aref bad-sig 10) (logxor (aref bad-sig 10) #xFF))
      (check "tampered signature rejects"
             (ecdsa-verify-p256 hash bad-sig x y)
             nil))

    ;; Wrong-length signature rejects. ES256 is fixed-width r||s
    ;; (64 bytes) — a 65-byte or 63-byte input is malformed, not
    ;; 'contains the right bytes plus/minus some', and both shapes
    ;; must return NIL rather than truncating or crashing deep in
    ;; a subseq call.
    (let ((too-long (concatenate '(simple-array (unsigned-byte 8) (*))
                                  sig #(0))))
      (check "oversized signature rejects"
             (ecdsa-verify-p256 hash too-long x y)
             nil))
    (let ((too-short (subseq sig 0 63)))
      (check "undersized signature rejects"
             (ecdsa-verify-p256 hash too-short x y)
             nil))
    (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
      (check "empty signature rejects"
             (ecdsa-verify-p256 hash empty x y)
             nil)))

  ;; DER encoding strips leading zeros per X.690 8.3.2.
  ;; der-encode-ecdsa-signature lives in the optional TLS system, so this
  ;; asks TLS-LOADED-P whether to run and TLS-SYM for the function. It
  ;; used to gate on (FBOUNDP 'WEB-SKELETON::DER-ENCODE-ECDSA-SIGNATURE),
  ;; which cannot tell "no libssl" from "that name moved" and would have
  ;; dropped this assertion silently on a rename. TLS-SYM raises instead.
  (when (tls-loaded-p)
    (let* ((sig (make-array 64 :element-type '(unsigned-byte 8) :initial-element #x42))
           (dummy (progn (setf (aref sig 0) #x00 (aref sig 1) #x4A) nil))
           (der (funcall (tls-sym "DER-ENCODE-ECDSA-SIGNATURE") sig)))
      (declare (ignore dummy))
      ;; The r INTEGER should be 31 bytes (stripped zero) not 32
      ;; Tag=0x02, then length byte
      (check "der minimal: r length stripped"
             (aref der 3)
             31)))

  ;; Generator point self-test: n*G should be the point at infinity
  (check "n*G = infinity"
         (web-skeleton::ec-mul web-skeleton::+p256-n+
                               (cons web-skeleton::+p256-gx+
                                     web-skeleton::+p256-gy+))
         nil)

  ;; Short pubkey coordinate rejected
  (let ((short-x (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
        (y (base64url-decode "x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0"))
        (hash (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
        (sig (make-array 64 :element-type '(unsigned-byte 8) :initial-element 1)))
    (check "short pubkey-x rejected"
           (ecdsa-verify-p256 hash sig short-x y) nil))

  ;; Invalid-curve point rejected (FIPS 186-4 §5.6.2.3.3)
  ;; Use the valid RFC 7515 key but flip one byte in y — point no longer
  ;; satisfies y² ≡ x³ + ax + b (mod p)
  (let* ((hash (sha256 (sb-ext:string-to-octets "test")))
         (sig (make-array 64 :element-type '(unsigned-byte 8) :initial-element 1))
         (x (base64url-decode "f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU"))
         (bad-y (copy-seq (base64url-decode "x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0"))))
    (setf (aref bad-y 0) (logxor (aref bad-y 0) #xFF))
    (check "invalid-curve point rejected"
           (ecdsa-verify-p256 hash sig x bad-y)
           nil))

  ;; FIPS 186-4 §5.6.2.3.3: public-key coordinates must be < p.
  ;; The 32-byte input range exceeds +p256-p+, so unreduced coords
  ;; slip past the curve-equation check (which uses MOD-MUL). Pass
  ;; all-0xFF coords which integer-wise exceed p; verify rejected
  ;; without raising. Exercised via the pure-Lisp path directly so
  ;; the libssl swap's own range check doesn't shadow this test.
  (let* ((hash (sha256 (sb-ext:string-to-octets "test")))
         (sig  (make-array 64 :element-type '(unsigned-byte 8) :initial-element 1))
         (huge (make-array 32 :element-type '(unsigned-byte 8) :initial-element #xFF)))
    (check "unreduced pubkey-x (>= p) rejected"
           (web-skeleton::ecdsa-verify-p256-lisp hash sig huge huge)
           nil)))

;;; ---------------------------------------------------------------------------
;;; mod-inv raises on non-invertible input (pure-Lisp primitive)
;;;
;;; Exercised separately because the bug only surfaces when a caller
;;; skips coordinate range validation — the ECDSA verifier above does
;;; check, but the primitive's contract should raise for 0 / any input
;;; whose gcd with p isn't 1 rather than silently return 0.
;;; ---------------------------------------------------------------------------

(defun test-mod-inv-raise ()
  (format t "~%mod-inv primitive~%")
  (flet ((raises-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    (check "mod-inv: 0 mod 13 raises"
           (raises-p (lambda () (web-skeleton::mod-inv 0 13))) t)
    (check "mod-inv: 13 mod 13 raises (reduces to 0)"
           (raises-p (lambda () (web-skeleton::mod-inv 13 13))) t)
    ;; Normal-case: inverse of 3 mod 11 is 4 (3 * 4 = 12 ≡ 1 mod 11)
    (check "mod-inv: 3 mod 11 = 4"
           (web-skeleton::mod-inv 3 11) 4)))

;;; ---------------------------------------------------------------------------
;;; HMAC-SHA256 test vectors (RFC 4231)
;;; ---------------------------------------------------------------------------

(defun test-hmac-sha256 ()
  (format t "~%HMAC-SHA256~%")

  ;; RFC 4231 Test Case 1
  (check "rfc4231 case 1"
         (bytes-to-hex
          (hmac-sha256
           (make-array 20 :element-type '(unsigned-byte 8) :initial-element #x0b)
           (sb-ext:string-to-octets "Hi There")))
         "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7")

  ;; RFC 4231 Test Case 2 — key = "Jefe"
  (check "rfc4231 case 2"
         (bytes-to-hex
          (hmac-sha256
           (sb-ext:string-to-octets "Jefe")
           (sb-ext:string-to-octets "what do ya want for nothing?")))
         "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")

  ;; RFC 4231 Test Case 3 — key = 20 bytes of 0xaa
  (check "rfc4231 case 3"
         (bytes-to-hex
          (hmac-sha256
           (make-array 20 :element-type '(unsigned-byte 8) :initial-element #xaa)
           (make-array 50 :element-type '(unsigned-byte 8) :initial-element #xdd)))
         "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe")

  ;; RFC 4231 Test Case 6 — key longer than block size (131 bytes of 0xaa)
  (check "rfc4231 case 6 (long key)"
         (bytes-to-hex
          (hmac-sha256
           (make-array 131 :element-type '(unsigned-byte 8) :initial-element #xaa)
           (sb-ext:string-to-octets
            "Test Using Larger Than Block-Size Key - Hash Key First")))
         "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"))

;;; ---------------------------------------------------------------------------
;;; constant-time-equal tests
;;; ---------------------------------------------------------------------------

(defun test-constant-time-equal ()
  (format t "~%Constant-Time Equal~%")

  (let ((a (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 4)))
        (b (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 4)))
        (c (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 5)))
        (d (make-array 3 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3)))
        (e (make-array 0 :element-type '(unsigned-byte 8))))
    (check "equal inputs" (constant-time-equal a b) t)
    (check "unequal same length" (constant-time-equal a c) nil)
    (check "length mismatch" (constant-time-equal a d) nil)
    (check "empty equal" (constant-time-equal e e) t)
    (check "empty vs non-empty" (constant-time-equal e a) nil)))

;;; ---------------------------------------------------------------------------
;;; Hex encoding tests
;;; ---------------------------------------------------------------------------

(defun test-hex ()
  (format t "~%Hex~%")

  (check "empty"
         (bytes-to-hex (make-array 0 :element-type '(unsigned-byte 8)))
         "")

  (check "single byte"
         (bytes-to-hex (make-array 1 :element-type '(unsigned-byte 8)
                                     :initial-contents '(255)))
         "ff")

  (check "multiple bytes"
         (bytes-to-hex (make-array 3 :element-type '(unsigned-byte 8)
                                     :initial-contents '(0 127 200)))
         "007fc8")

  ;; Confirm sha1-hex still works through bytes-to-hex
  (check "sha1-hex via bytes-to-hex"
         (sha1-hex (sb-ext:string-to-octets "abc"))
         "a9993e364706816aba3e25717850c26c9cd0d89d")

  ;; hex-decode (compare via bytes-to-hex for array equality)
  (check "decode empty"
         (bytes-to-hex (hex-decode "")) "")

  (check "decode round-trip"
         (bytes-to-hex (hex-decode "007fc8")) "007fc8")

  (check "decode uppercase"
         (bytes-to-hex (hex-decode "FF00AB")) "ff00ab")

  (check "decode lowercase"
         (bytes-to-hex (hex-decode "ff00ab")) "ff00ab")

  (flet ((signals-error-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    (check "decode odd-length rejected"
           (signals-error-p (lambda () (hex-decode "abc"))) t)
    (check "decode invalid char rejected"
           (signals-error-p (lambda () (hex-decode "zz"))) t)))

;;; ---------------------------------------------------------------------------
;;; Crypto random tests
;;; ---------------------------------------------------------------------------

(defun test-random ()
  (format t "~%Random~%")
  ;; Length
  (check "random-bytes: 32" (length (random-bytes 32)) 32)
  (check "random-bytes: 16" (length (random-bytes 16)) 16)
  (check "random-bytes: 1"  (length (random-bytes 1))  1)
  ;; Element type
  (check "random-bytes: element-type"
         (array-element-type (random-bytes 16))
         '(unsigned-byte 8))
  ;; Distinctness — successive 32-byte calls must not match. A collision
  ;; is 2^-256, effectively impossible; a match here means /dev/urandom
  ;; is broken or we're reading the wrong thing.
  (let ((a (random-bytes 32))
        (b (random-bytes 32)))
    (check "random-bytes: successive calls differ"
           (equalp a b) nil))
  ;; The worker path. RANDOM-BYTES used to open, read and close
  ;; /dev/urandom on every call, which a session-per-request app pays
  ;; per request; a worker now holds one stream open for its lifetime.
  ;; Outside a worker the binding is NIL and the per-call open remains,
  ;; which is the state every check above runs in.
  (check "random-bytes: no cached stream outside a worker"
         web-skeleton::*urandom-stream* nil)
  (web-skeleton::with-worker-urandom
    (check "with-worker-urandom: binds a stream"
           (and (streamp web-skeleton::*urandom-stream*)
                (open-stream-p web-skeleton::*urandom-stream*))
           t)
    (check "random-bytes: correct length from the cached stream"
           (length (random-bytes 32)) 32)
    (check "random-bytes: cached stream still yields distinct reads"
           (equalp (random-bytes 32) (random-bytes 32)) nil)
    (check "random-token: works through the cached stream"
           (length (random-token)) 43))
  ;; And the binding is unwound, so nothing leaks the stream past the
  ;; worker that opened it.
  (check "with-worker-urandom: unbinds on exit"
         web-skeleton::*urandom-stream* nil)
  ;; The checks above prove the plumbing exists; this one proves
  ;; RANDOM-BYTES is connected to it. Bound to a stream of known bytes,
  ;; it must return those bytes — a RANDOM-BYTES that ignored the
  ;; binding would return four bytes from /dev/urandom instead, and
  ;; every check above would still pass.
  (let ((path (merge-pathnames "tests/tmp-urandom.bin" (truename "."))))
    (unwind-protect
         (progn
           (with-open-file (s path :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-sequence (coerce #(1 2 3 4 5 6 7 8)
                                     '(vector (unsigned-byte 8)))
                             s))
           (with-open-file (s path :direction :input
                                   :element-type '(unsigned-byte 8))
             (let ((web-skeleton::*urandom-stream* s))
               (check "random-bytes: reads from the bound stream"
                      (coerce (random-bytes 4) 'list) '(1 2 3 4))
               ;; Sequential, not re-read from the top — a cached stream
               ;; carries a position, which is the whole reason it is
               ;; per-worker rather than shared.
               (check "random-bytes: advances the bound stream"
                      (coerce (random-bytes 4) 'list) '(5 6 7 8))
               ;; Exhausted. The cached stream is an optimisation, not a
               ;; requirement, so a read that comes up short falls back
               ;; to a per-call open rather than raising — otherwise one
               ;; bad read poisons the binding for the worker's whole
               ;; life, and RANDOM-TOKEN feeds session IDs and CSRF
               ;; tokens. Matches how the open already degrades.
               (let ((bytes (random-bytes 8)))
                 (check "random-bytes: exhausted stream falls back"
                        (length bytes) 8)
                 ;; And the fallback really produced random bytes rather
                 ;; than the zeros a silently-unfilled buffer would hold.
                 (check "random-bytes: fallback bytes are not the empty buffer"
                        (every #'zerop bytes) nil)))))
      (ignore-errors (delete-file path))))
  ;; random-token: expected base64url length (unpadded)
  ;; 32 bytes -> ceil(32*4/3) = 43 chars
  ;; 16 bytes -> ceil(16*4/3) = 22 chars
  (check "random-token: default length 43"
         (length (random-token)) 43)
  (check "random-token: :bytes 16 length 22"
         (length (random-token :bytes 16)) 22)
  ;; random-token output is pure base64url charset
  (let ((token (random-token :bytes 64)))
    (check "random-token: base64url charset"
           (every (lambda (c)
                    (or (char<= #\A c #\Z)
                        (char<= #\a c #\z)
                        (char<= #\0 c #\9)
                        (char= c #\-)
                        (char= c #\_)))
                  token)
           t))
  ;; Distinctness
  (let ((a (random-token))
        (b (random-token)))
    (check "random-token: successive calls differ"
           (string= a b) nil)))

;;; ---------------------------------------------------------------------------
;;; Runner
;;; ---------------------------------------------------------------------------

(defun test-algorithms ()
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
  (format t "~%=== Algorithm Tests ===~%")
  (test-sha1)
  (test-sha256)
  (test-base64)
  (test-base64-decode)
  (test-base64url)
  (test-ecdsa)
  (test-mod-inv-raise)
  (test-hmac-sha256)
  (test-constant-time-equal)
  (test-hex)
  (test-random)
  (report-suite "Algorithms")
  (zerop *tests-failed*))

;;; ---------------------------------------------------------------------------
;;; Pure-Lisp crypto re-verification (framework-dev entry point)
;;;
;;; NOT wired into the default (test) runner. Intended for humans editing
;;; src/algorithms/sha1.lisp, sha256.lisp, or ecdsa.lisp who want to verify
;;; their changes to the pure-Lisp implementations on a machine that has
;;; web-skeleton-tls loaded (and therefore sees the libssl-backed versions
;;; as the active sha1/sha256/ecdsa-verify-p256 by default).
;;;
;;; Mechanism: temporarily swap SYMBOL-FUNCTION for the three public
;;; crypto names back to their *-LISP originals, run the existing
;;; TEST-SHA1 / TEST-SHA256 / TEST-HMAC-SHA256 / TEST-ECDSA functions
;;; unchanged (so the FIPS / RFC vectors live in exactly one place),
;;; then restore via UNWIND-PROTECT. Safe only in a serial test runner.
;;; If libssl isn't loaded, the swap still works — it just replaces
;;; the thin SHA1/SHA256/ECDSA-VERIFY-P256 wrappers with their direct
;;; *-LISP targets, which is effectively a no-op.
;;; ---------------------------------------------------------------------------

(defun test-pure-lisp-crypto ()
  "Framework-dev entry point: re-run the existing crypto tests against
   the pure-Lisp implementations by temporarily swapping SYMBOL-FUNCTION
   for SHA1 / SHA256 / ECDSA-VERIFY-P256. HMAC-SHA256 is exercised for
   free because it calls SHA256 through the function cell.

   Run by TEST automatically whenever libssl is loaded, which is the only
   condition under which it has anything to say — without libssl the
   default function cells already are the pure-Lisp ones and the first
   TEST-ALGORITHMS pass covered them. This docstring said the opposite for
   long enough that three other places — RUN.LISP's header, TEST's own
   comment, and the README — all describe the real behaviour and this one
   did not."
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
  (format t "~%=== Pure-Lisp crypto re-verification ===~%")
  (let ((saved-sha1   (symbol-function 'sha1))
        (saved-sha256 (symbol-function 'sha256))
        (saved-ecdsa  (symbol-function 'ecdsa-verify-p256)))
    (unwind-protect
         (progn
           (setf (symbol-function 'sha1)              #'web-skeleton::sha1-lisp
                 (symbol-function 'sha256)            #'web-skeleton::sha256-lisp
                 (symbol-function 'ecdsa-verify-p256) #'web-skeleton::ecdsa-verify-p256-lisp)
           (test-sha1)
           (test-sha256)
           (test-hmac-sha256)
           (test-ecdsa))
      (setf (symbol-function 'sha1)              saved-sha1
            (symbol-function 'sha256)            saved-sha256
            (symbol-function 'ecdsa-verify-p256) saved-ecdsa)))
  (report-suite "Pure-Lisp Crypto" "(pure-Lisp)")
  (zerop *tests-failed*))
