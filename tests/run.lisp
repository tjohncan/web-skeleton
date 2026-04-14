(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; Test utilities and runner
;;; ===========================================================================

(defvar *crlf* (coerce '(#\Return #\Newline) 'string)
  "CRLF string for constructing test request data.")

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
  "Assert that EXPR signals an HTTP-PARSE-ERROR (or any error as fallback)."
  `(if (handler-case (progn ,expr nil)
         (http-parse-error () t)
         (error (e)
           (format t "  NOTE  ~a caught ~a (not http-parse-error)~%" ,name (type-of e))
           t))
       (progn
         (format t "  PASS  ~a~%" ,name)
         (incf *tests-passed*))
       (progn
         (format t "  FAIL  ~a (expected error, got none)~%" ,name)
         (incf *tests-failed*))))

(defun test ()
  "Run all tests. Returns T if all passed."
  (let ((all-passed t)
        (total-passed 0)
        (total-failed 0))
    (dolist (suite '(test-algorithms test-json test-server test-store
                     test-harness test-tls))
      (unless (funcall suite)
        (setf all-passed nil))
      (incf total-passed *tests-passed*)
      (incf total-failed *tests-failed*))
    (format t "~d passed, ~d failed across all suites~%"
            total-passed total-failed)
    (if all-passed
        (format t "=== ALL TESTS PASSED ===~%~%")
        (format t "=== SOME TESTS FAILED ===~%~%"))
    all-passed))
