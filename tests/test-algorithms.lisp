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
;;; Runner
;;; ---------------------------------------------------------------------------

(defun test-algorithms ()
  (setf *tests-passed* 0
        *tests-failed* 0)
  (format t "~%=== Algorithm Tests ===~%")
  (test-sha1)
  (test-base64)
  (format t "~%~d passed, ~d failed~%~%" *tests-passed* *tests-failed*)
  (zerop *tests-failed*))
