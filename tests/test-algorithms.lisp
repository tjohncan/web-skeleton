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
         "foobar"))

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
           (bytes-to-hex data))))

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
         ;; Signature (r || s) from RFC 7515 A.3
         (sig (concatenate '(simple-array (unsigned-byte 8) (*))
                (base64url-decode "DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmXFCgfTjDxw5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q")
                )))
    ;; The RFC 7515 A.3 signature is 64 bytes (r=32, s=32)
    (check "rfc7515 A.3 valid signature"
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
             nil)))

  ;; Generator point self-test: n*G should be the point at infinity
  (check "n*G = infinity"
         (web-skeleton::ec-mul web-skeleton::+p256-n+
                               (cons web-skeleton::+p256-gx+
                                     web-skeleton::+p256-gy+))
         nil)

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
           nil)))

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
;;; Runner
;;; ---------------------------------------------------------------------------

(defun test-algorithms ()
  (setf *tests-passed* 0
        *tests-failed* 0)
  (format t "~%=== Algorithm Tests ===~%")
  (test-sha1)
  (test-sha256)
  (test-base64)
  (test-base64-decode)
  (test-base64url)
  (test-ecdsa)
  (test-hmac-sha256)
  (test-hex)
  (format t "~%~d passed, ~d failed~%~%" *tests-passed* *tests-failed*)
  (zerop *tests-failed*))
