(in-package :web-skeleton)

;;; ===========================================================================
;;; Test utilities and runner
;;; ===========================================================================

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defmacro check (name expr expected)
  "Assert that EXPR produces EXPECTED. Logs pass/fail."
  `(let ((result ,expr))
     (if (equal result ,expected)
         (progn
           (format t "  PASS  ~a~%" ,name)
           (incf *tests-passed*))
         (progn
           (format t "  FAIL  ~a~%    expected: ~s~%         got: ~s~%"
                   ,name ,expected result)
           (incf *tests-failed*)))))

(defmacro check-error (name expr)
  "Assert that EXPR signals an HTTP-PARSE-ERROR."
  `(if (handler-case (progn ,expr nil)
         (http-parse-error () t))
       (progn
         (format t "  PASS  ~a~%" ,name)
         (incf *tests-passed*))
       (progn
         (format t "  FAIL  ~a (expected http-parse-error, got none)~%" ,name)
         (incf *tests-failed*))))

(defun test ()
  "Run all tests. Returns T if all passed."
  (let ((all-passed t))
    (unless (test-algorithms)
      (setf all-passed nil))
    (unless (test-server)
      (setf all-passed nil))
    (if all-passed
        (format t "=== ALL TESTS PASSED ===~%~%")
        (format t "=== SOME TESTS FAILED ===~%~%"))
    all-passed))
