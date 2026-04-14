(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; Test harness tests
;;;
;;; Unit-style tests for MAKE-TEST-REQUEST / MAKE-TEST-WS-FRAME land first
;;; because they are pure functions and run in milliseconds. End-to-end
;;; tests via WITH-TEST-SERVER come after — each one spins a live server
;;; on an ephemeral port, so teardown adds ~1-2 seconds per call.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Unit-style builders
;;; ---------------------------------------------------------------------------

(defun test-harness-make-test-request ()
  (format t "~%Harness: make-test-request~%")
  (let ((req (make-test-request
              :method :POST
              :path "/foo"
              :query "q=1"
              :headers '(("X-Custom" . "bar"))
              :body "data")))
    (check "method"           (http-request-method req)   :POST)
    (check "path"              (http-request-path req)     "/foo")
    (check "query"             (http-request-query req)    "q=1")
    (check "header lowercased" (get-header req "x-custom") "bar")
    (check "body present"
           (not (null (http-request-body req))) t)
    (check "body content"
           (sb-ext:octets-to-string (http-request-body req)
                                    :external-format :utf-8)
           "data")))

(defun test-harness-make-test-ws-frame ()
  (format t "~%Harness: make-test-ws-frame~%")
  (let ((frame (make-test-ws-frame "hi")))
    (check "fin+opcode byte" (aref frame 0) #x81)
    (check "mask bit + length" (aref frame 1) (logior #x80 2))
    ;; Round-trip: parsing the frame recovers the original payload.
    (multiple-value-bind (parsed consumed)
        (web-skeleton::try-parse-ws-frame frame 0 (length frame))
      (declare (ignore consumed))
      (check "round-trip parses" (not (null parsed)) t)
      (check "round-trip opcode" (ws-frame-opcode parsed) 1)
      (check "round-trip fin"    (ws-frame-fin parsed)    t)
      (check "round-trip payload"
             (sb-ext:octets-to-string (ws-frame-payload parsed)
                                      :external-format :utf-8)
             "hi")))
  ;; Non-default opcode and FIN=0 (first fragment)
  (let ((frame (make-test-ws-frame "partial" :opcode 2 :fin nil)))
    (multiple-value-bind (parsed consumed)
        (web-skeleton::try-parse-ws-frame frame 0 (length frame))
      (declare (ignore consumed))
      (check "binary frame opcode" (ws-frame-opcode parsed) 2)
      (check "fin=0 for first fragment" (ws-frame-fin parsed) nil))))

;;; ---------------------------------------------------------------------------
;;; End-to-end server tests
;;; ---------------------------------------------------------------------------

(defun test-harness-basic-get ()
  (format t "~%Harness: basic GET~%")
  (with-test-server
      (:handler (lambda (req)
                  (declare (ignore req))
                  (make-text-response 200 "hello")))
    (multiple-value-bind (status headers body)
        (test-http-request :get "/")
      (declare (ignore headers))
      (check "status 200" status 200)
      (check "body hello"  body   "hello"))))

(defun test-harness-post-with-body ()
  (format t "~%Harness: POST with body~%")
  (with-test-server
      (:handler (lambda (req)
                  (let ((body (http-request-body req)))
                    (make-text-response
                     200
                     (format nil "got ~d bytes"
                             (if body (length body) 0))))))
    (multiple-value-bind (status headers body)
        (test-http-request :post "/echo" :body "hello world")
      (declare (ignore headers))
      (check "POST status 200"         status 200)
      (check "POST body length echoed" body   "got 11 bytes"))))

(defun test-harness-shutdown-hook-e2e ()
  (format t "~%Harness: shutdown hook end-to-end~%")
  ;; A handler-registered cleanup should fire on teardown. The isolation
  ;; inside WITH-TEST-SERVER means this hook belongs to this test's
  ;; server only — restored on exit so no leakage to later tests.
  (let ((fired nil))
    (with-test-server
        (:handler (lambda (req)
                    (declare (ignore req))
                    (register-cleanup (lambda () (setf fired t)))
                    (make-text-response 200 "ok")))
      (test-http-request :get "/"))
    (check "cleanup fired during teardown" fired t)))

(defun test-harness-expect-100-continue-e2e ()
  (format t "~%Harness: Expect: 100-continue end-to-end~%")
  (with-test-server
      (:handler (lambda (req)
                  (let ((body (http-request-body req)))
                    (make-text-response
                     200
                     (format nil "~d bytes received"
                             (if body (length body) 0))))))
    (multiple-value-bind (status headers body)
        (test-http-request :post "/upload"
                           :headers '(("expect" . "100-continue"))
                           :body "payload-data")
      (declare (ignore headers))
      (check "Expect: 100-continue reaches handler" status 200)
      (check "Expect: 100-continue body echoed"
             body "12 bytes received"))))

;;; ---------------------------------------------------------------------------
;;; Runner
;;; ---------------------------------------------------------------------------

(defun test-harness ()
  (setf *tests-passed* 0
        *tests-failed* 0)
  (format t "~%=== Test Harness Tests ===~%")
  ;; Unit tests first (fast)
  (test-harness-make-test-request)
  (test-harness-make-test-ws-frame)
  ;; End-to-end tests (~1-2s each due to server teardown)
  (test-harness-basic-get)
  (test-harness-post-with-body)
  (test-harness-shutdown-hook-e2e)
  (test-harness-expect-100-continue-e2e)
  (format t "~%~d passed, ~d failed~%~%" *tests-passed* *tests-failed*)
  (zerop *tests-failed*))
