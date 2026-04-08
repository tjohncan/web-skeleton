(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; Algorithm tests — SHA-1 and Base64
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
         "a9993e364706816aba3e25717850c26c9cd0d89d"))

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
  (test-hex)
  (format t "~%~d passed, ~d failed~%~%" *tests-passed* *tests-failed*)
  (zerop *tests-failed*))
