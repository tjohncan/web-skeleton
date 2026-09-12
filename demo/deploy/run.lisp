;;; Container entry point.
;;;
;;; Separate from run-server.lisp, which stays the laptop path: loopback, a
;;; fixed port, no environment. This one reads what a container orchestrator
;;; has to be able to set, and nothing else.
;;;
;;; HOST defaults to 0.0.0.0 here and to loopback there, and the difference is
;;; the whole reason this file exists. A socket bound to a container's own
;;; loopback is reachable from nothing outside it.

(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "web-skeleton-demo")

(handler-case (asdf:load-system "web-skeleton-tls")
  (error () (format t "note: TLS not available (libssl not found)~%")))

(defun env-int (name default)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v)))
        (or (parse-integer v :junk-allowed t) default)
        default)))

(defun env-host (name default)
  "Parse a dotted-quad, or return DEFAULT. Only IPv4 literals: a container
   gets told an address, never a name, because resolving one here would make
   startup depend on a resolver that may not be up yet."
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v)))
        (let ((parts (loop with start = 0
                           for dot = (position #\. v :start start)
                           collect (parse-integer v :start start :end dot
                                                    :junk-allowed t)
                           while dot do (setf start (1+ dot)))))
          (if (and (= (length parts) 4) (every #'integerp parts)
                   (every (lambda (n) (<= 0 n 255)) parts))
              (make-array 4 :element-type '(unsigned-byte 8)
                            :initial-contents parts)
              default))
        default)))

(web-skeleton-demo:start-demo
 :host    (env-host "WS_HOST" #(0 0 0 0))
 :port    (env-int  "WS_PORT" 8081)
 :workers (env-int  "WS_WORKERS" 4))
