;;; The demo's Origin check as a table: a request's Host and Origin, and what
;;; the /ws route owes it, the upgrade or a 403.
;;;
;;;   sbcl --non-interactive --load demo/test-origin.lisp
;;;
;;; from the repository root. CI runs it once the demo has compiled. A wrong
;;; answer is an error, so the run exits non-zero.
;;;
;;; Asked of HANDLE-REQUEST rather than of the predicate beneath it, because
;;; what a browser meets is the route's answer, and the routing is part of it.
;;; The table is its own control: a check that allowed everything would fail
;;; the refusals, and one that refused everything would fail the rest. The
;;; refusals include the two easy mistakes, ignoring the port and cutting an
;;; IPv6 literal at its first colon.

(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "web-skeleton-demo")
(asdf:load-system "web-skeleton-test-harness")

(let ((wrong nil))
  (loop for (host origin owed why)
          in '(("demo.example.com"   "https://demo.example.com"  :upgrade "the page's own origin")
               ("demo.example.com"   nil                         :upgrade "no Origin, so not a browser")
               ("demo.example.com"   "http://demo.example.com"   :upgrade "the scheme is not compared")
               ("localhost:8081"     "http://LOCALHOST:8081"     :upgrade "nor the host's case")
               ("[2001:db8::1]:8081" "http://[2001:db8::1]:8081" :upgrade "an IPv6 literal, whole")
               ("localhost:8081"     "http://localhost:3000"     403      "another port on the same host")
               ("localhost:8081"     "http://localhost"          403      "the port left off")
               ("demo.example.com"   "https://evil.example"      403      "another site")
               ("demo.example.com"   "null"                      403      "null, from a sandboxed or file:// page")
               ("[2001:db8::1]:8081" "http://[2001:dead::bad]"   403      "another IPv6 address"))
        do (let* ((response (web-skeleton-demo::handle-request
                             (web-skeleton-test-harness:make-test-request
                              :path "/ws"
                              :headers (list* (cons "host" host)
                                              (and origin
                                                   (list (cons "origin" origin)))))))
                  (got (if (eq response :upgrade)
                           :upgrade
                           (web-skeleton:http-response-status response))))
             (format t "~:[FAIL~;ok  ~]  ~8a ~a~%" (eql got owed) got why)
             (unless (eql got owed)
               (push why wrong))))
  (when wrong
    (error "the demo's Origin check answered ~d case~:p wrongly: ~{~a~^; ~}"
           (length wrong) (reverse wrong))))
