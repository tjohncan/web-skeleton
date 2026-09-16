(defpackage :web-skeleton-tests
  (:use :cl :web-skeleton :web-skeleton-test-harness)
  (:export #:test
           #:test-algorithms
           #:test-json
           #:test-server
           #:test-store
           #:test-harness
           #:test-tls
           #:test-properties
           ;; Re-verification of the pure-Lisp crypto, which (test) runs
           ;; last and only where libssl displaced it
           #:test-pure-lisp-crypto))
