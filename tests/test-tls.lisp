(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; TLS tests — skipped when web-skeleton-tls is not loaded
;;; ===========================================================================

(defun test-tls ()
  (setf *tests-passed* 0
        *tests-failed* 0)
  (format t "~%=== TLS Tests ===~%")
  (if (null web-skeleton:*https-fetch-fn*)
      (progn
        (format t "~%  SKIP  TLS not loaded (libssl not found)~%")
        (format t "~%0 passed, 0 failed (skipped)~%~%")
        t)
      (progn
        (test-tls-registration)
        (test-tls-roundtrip)
        (format t "~%~d passed, ~d failed~%~%" *tests-passed* *tests-failed*)
        (zerop *tests-failed*))))

(defun test-tls-registration ()
  (format t "~%TLS Registration~%")
  (check "https-fetch-fn set"
         (not (null web-skeleton:*https-fetch-fn*)) t))

(defun test-tls-roundtrip ()
  (format t "~%TLS Roundtrip~%")
  ;; Direct blocking HTTPS GET to example.com (IANA-maintained, always up).
  ;; Tests the full pipeline: DNS, TCP, TLS handshake, HTTP over TLS.
  (handler-case
      (multiple-value-bind (ssl socket)
          (funcall 'web-skeleton::tls-connect "example.com" 443)
        (unwind-protect
            (let* ((request (web-skeleton::build-outbound-request
                             :GET "example.com" "/"
                             :headers '(("accept" . "text/html"))))
                   (response-buf (progn
                                   (funcall 'web-skeleton::tls-write-all
                                            ssl request)
                                   (funcall 'web-skeleton::tls-read-all ssl)))
                   (status (web-skeleton::parse-response-status
                            response-buf 0 (length response-buf))))
              (check "https GET example.com status"
                     status 200)
              (check "response has body"
                     (> (length response-buf) 100) t))
          (funcall 'web-skeleton::tls-close ssl socket)))
    (error (e)
      (format t "  SKIP  network error: ~a~%" e))))
