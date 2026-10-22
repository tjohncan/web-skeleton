;;; The demo's own checks, run from the repository root:
;;;
;;;   sbcl --non-interactive --load demo/test-demo.lisp
;;;
;;; CI runs it once the demo has compiled. A wrong answer is an error, so the
;;; run exits non-zero.

(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "web-skeleton-demo")
(asdf:load-system "web-skeleton-test-harness")

(defvar *wrong* nil
  "What each check below answered wrongly, by its description.")

(defun owed (got owed why)
  "Print GOT against OWED under WHY, and record WHY when they differ."
  (format t "~:[FAIL~;ok  ~]  ~a~:[ (got ~s)~;~*~]~%" (equal got owed) why (equal got owed) got)
  (unless (equal got owed)
    (push why *wrong*)))

;;; ---------------------------------------------------------------------------
;;; The Origin check, as a table: a request's Host and Origin, and what the
;;; /ws route owes it, the upgrade or a 403.
;;;
;;; Asked of HANDLE-REQUEST rather than of the predicate beneath it, because
;;; what a browser meets is the route's answer, and the routing is part of it.
;;; The table is its own control: a check that allowed everything would fail
;;; the refusals, and one that refused everything would fail the rest. The
;;; refusals include the two easy mistakes, ignoring the port and cutting an
;;; IPv6 literal at its first colon.
;;; ---------------------------------------------------------------------------

(format t "~%The /ws Origin check~%")
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
      do (let ((response (web-skeleton-demo::handle-request
                          (web-skeleton-test-harness:make-test-request
                           :path "/ws"
                           :headers (list* (cons "host" host)
                                           (and origin
                                                (list (cons "origin" origin))))))))
           (owed (if (eq response :upgrade)
                     :upgrade
                     (web-skeleton:http-response-status response))
                 owed why)))

;;; ---------------------------------------------------------------------------
;;; What a posted line may hold. The log renders with white-space: pre-wrap,
;;; and each of these would start a second row its sender could dress as
;;; somebody else's line; a TAB would split the line's wire fields.
;;; ---------------------------------------------------------------------------

(format t "~%A posted line is one row~%")
(loop for (code why) in '((#x0A "a newline is stripped")
                          (#x09 "a TAB is stripped")
                          (#x7F "DEL is stripped")
                          (#x85 "NEL, U+0085, is stripped")
                          (#x9B "a C1 control, U+009B, is stripped")
                          (#x2028 "the LINE SEPARATOR is stripped")
                          (#x2029 "the PARAGRAPH SEPARATOR is stripped"))
      do (owed (web-skeleton-demo::%sanitize-line
                (format nil "one~ctwo" (code-char code)))
               "onetwo" why))
(owed (web-skeleton-demo::%sanitize-line
       (format nil "caf~c ~c and spaces" (code-char #xE9) (code-char #x2603)))
      (format nil "caf~c ~c and spaces" (code-char #xE9) (code-char #x2603))
      "letters past ASCII, and spaces, are kept")

;;; ---------------------------------------------------------------------------
;;; The address /demo-fetch dials. Each URL is also put through the fetch
;;; path's own parser, which has to find the address the server is bound to.
;;; ---------------------------------------------------------------------------

(format t "~%/demo-fetch dials the address the demo is bound to~%")
(loop for (host owed dialled why)
        in '((#(0 0 0 0) "127.0.0.1" #(127 0 0 1) "the IPv4 wildcard, on loopback")
             (#(127 0 0 1) "127.0.0.1" #(127 0 0 1) "loopback, as bound")
             (#(10 1 2 3) "10.1.2.3" #(10 1 2 3) "one IPv4 address, itself")
             (#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) "[::1]"
              #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) "the IPv6 wildcard, on IPv6 loopback")
             (#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) "[0:0:0:0:0:0:0:1]"
              #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) "IPv6 loopback, as bound")
             (#(32 1 13 184 0 0 0 0 0 0 0 0 0 0 0 5) "[2001:db8:0:0:0:0:0:5]"
              #(32 1 13 184 0 0 0 0 0 0 0 0 0 0 0 5) "one IPv6 address, bracketed"))
      do (let ((got (web-skeleton-demo::%self-fetch-host host)))
           (owed got owed why)
           (owed (multiple-value-bind (scheme url-host)
                     (web-skeleton:parse-url
                      (format nil "http://~a:8081/robots.txt" got))
                   (declare (ignore scheme))
                   (coerce (or (web-skeleton:parse-ipv4-literal url-host)
                               (web-skeleton:parse-ipv6-literal url-host))
                           'list))
                 (coerce dialled 'list)
                 (format nil "~a, as the fetch path reads the URL" why))))

;;; ---------------------------------------------------------------------------
;;; Before START-DEMO, and an empty batch for the fan-out.
;;; ---------------------------------------------------------------------------

(format t "~%Before the demo starts, and a tick with nothing new~%")
(let ((response (handler-case (web-skeleton-demo::handle-healthz nil)
                  (error (e) (princ-to-string e)))))
  (owed (if (typep response 'web-skeleton:http-response)
            (web-skeleton:http-response-status response)
            response)
        503 "/healthz before START-DEMO is 503, not started"))
;; *BULLETIN-LATEST* names a line the store does not hold, the state the
;; tick's invariant rules out. The tick has to leave *FANNED* where it was
;; for the next pass rather than raise.
(let ((latest web-skeleton-demo::*bulletin-latest*)
      (web-skeleton::*connections* (make-hash-table)))
  (setf web-skeleton-demo::*bulletin* (web-skeleton:make-store :test #'eql)
        web-skeleton-demo::*fanned* (make-array 1 :initial-element 3)
        web-skeleton-demo::*last-tick* (make-array 1 :initial-element 0)
        web-skeleton-demo::*bulletin-latest* 5)
  (unwind-protect
       (owed (list (handler-case (progn (web-skeleton-demo::bulletin-tick 0) :ran)
                     (error (e) (princ-to-string e)))
                   (aref web-skeleton-demo::*fanned* 0))
             '(:ran 3)
             "an empty batch leaves *fanned* where it was")
    (setf web-skeleton-demo::*bulletin-latest* latest
          web-skeleton-demo::*bulletin* nil
          web-skeleton-demo::*fanned* nil
          web-skeleton-demo::*last-tick* nil)))

(when *wrong*
  (error "the demo answered ~d check~:p wrongly: ~{~a~^; ~}"
         (length *wrong*) (reverse *wrong*)))
