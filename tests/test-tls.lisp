(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; TLS tests — skipped when web-skeleton-tls is not loaded
;;; ===========================================================================

(defun test-tls ()
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
  (format t "~%=== TLS Tests ===~%")
  (if (null web-skeleton:*https-fetch-fn*)
      (progn
        (format t "~%  SKIP  TLS not loaded (libssl not found)~%")
        (format t "~%0 passed, 0 failed (skipped)~%~%")
        t)
      (progn
        (test-tls-registration)
        (report-suite "TLS")
        (zerop *tests-failed*))))

(defun test-tls-registration ()
  (format t "~%TLS Registration~%")
  (check "https-fetch-fn set"
         (not (null web-skeleton:*https-fetch-fn*)) t)
  (check "https-stream-fn set"
         (not (null web-skeleton:*https-stream-fn*)) t))
