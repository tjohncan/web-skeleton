;;; Framework-dev test entry — verify the pure-Lisp crypto paths.
;;;
;;; Loads the test system and web-skeleton-tls so the libssl swap is in
;;; effect, then calls TEST-PURE-LISP-CRYPTO which temporarily swaps
;;; SYMBOL-FUNCTION back to SHA1-LISP / SHA256-LISP / ECDSA-VERIFY-P256-LISP
;;; and reruns the existing FIPS / RFC vectors against those. Useful when
;;; editing src/algorithms/sha1.lisp, sha256.lisp, or ecdsa.lisp on a
;;; machine with libssl loaded (where the default (test) runner exercises
;;; only the libssl-backed versions). Exits 0 on pass, 1 on any failure.
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "web-skeleton-tests")
(handler-case (asdf:load-system "web-skeleton-tls")
  (error () (format t "Note: TLS not available (libssl not found)~%")))
(unless (web-skeleton-tests:test-pure-lisp-crypto)
  (sb-ext:exit :code 1))
