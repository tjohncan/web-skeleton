(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; Test utilities and runner
;;; ===========================================================================

(defvar *crlf* (coerce '(#\Return #\Newline) 'string)
  "CRLF string for constructing test request data.")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)
(defvar *failed-names* nil
  "Names of failing tests in the currently-running suite.
   Populated by CHECK / CHECK-ERROR, reset per suite, replayed by
   REPORT-SUITE at the summary line so a long run's failures are
   legible without scrolling.")
(defvar *all-failed-names* nil
  "Names of failing tests across every suite in the current TEST run.
   Appended to by REPORT-SUITE at each suite's summary; reset only
   by TEST at the top of the run. Echoed after the grand total so
   a full-suite run surfaces the complete failure list once more —
   scrolling up through every suite's block is not required.")

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
           (incf *tests-failed*)
           (push ,name *failed-names*)))))

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
         (incf *tests-failed*)
         (push ,name *failed-names*))))

(defun report-suite (suite-name &optional suffix)
  "Emit the per-suite summary line, preceded by the list of failing
   test names when any ran. Replayed names are in declaration order
   (CHECK / CHECK-ERROR push onto the head, so walk in reverse).
   Appends (SUITE-NAME . TEST-NAME) pairs onto *ALL-FAILED-NAMES* so
   TEST can echo the full cross-suite list after the grand total —
   the per-suite block scrolls offscreen on a long run; the replay
   keeps the failing names adjacent to the final pass/fail line,
   tagged with their suite so triage does not require re-scrolling
   to match a name back to where it fired. SUFFIX goes on the
   pass/fail line (e.g. \"(pure-Lisp)\" for the re-verification
   pass). Called at the end of each suite's entry defun."
  (when *failed-names*
    (format t "~%Failed:~%")
    (dolist (name (reverse *failed-names*))
      (format t "  ~a~%" name))
    (setf *all-failed-names*
          (append *all-failed-names*
                  (mapcar (lambda (n) (cons suite-name n))
                          (reverse *failed-names*)))))
  (format t "~%~d passed, ~d failed~@[ ~a~]~%~%"
          *tests-passed* *tests-failed* suffix))

(defun test ()
  "Run all tests. Returns T if all passed."
  (let ((all-passed t)
        (total-passed 0)
        (total-failed 0))
    (setf *all-failed-names* nil)
    (dolist (suite '(test-algorithms test-json test-server test-store
                     test-harness test-tls))
      (unless (funcall suite)
        (setf all-passed nil))
      (incf total-passed *tests-passed*)
      (incf total-failed *tests-failed*))
    ;; When libssl is loaded, the earlier TEST-ALGORITHMS run
    ;; exercised the libssl-backed sha1 / sha256 / ecdsa-verify-p256
    ;; via the swapped symbol cells — the pure-Lisp originals went
    ;; completely untested. Re-run the crypto suite with the
    ;; function cells temporarily swapped back, so edits to
    ;; src/algorithms/*.lisp are caught on libssl-enabled machines
    ;; too. No-op when libssl is absent: the default function
    ;; cells already are the pure-Lisp versions, so there is
    ;; nothing to re-verify.
    (when web-skeleton:*https-fetch-fn*
      (unless (test-pure-lisp-crypto)
        (setf all-passed nil))
      (incf total-passed *tests-passed*)
      (incf total-failed *tests-failed*))
    (when *all-failed-names*
      (format t "Failed across all suites:~%")
      (dolist (entry *all-failed-names*)
        (format t "  [~a] ~a~%" (car entry) (cdr entry)))
      (format t "~%"))
    (format t "~d passed, ~d failed across all suites~%"
            total-passed total-failed)
    (if all-passed
        (format t "=== ALL TESTS PASSED ===~%~%")
        (format t "=== SOME TESTS FAILED ===~%~%"))
    all-passed))
