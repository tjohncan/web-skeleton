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

;; TLS is optional here — the demo makes no outbound HTTPS calls, so the image
;; can run without libssl. Report the actual condition rather than assuming
;; libssl is missing: a read or compile error inside the TLS system (a symbol
;; external on one SBCL and not another, say) is a different failure, and
;; calling it "libssl not found" sends the reader off to debug the wrong thing.
(handler-case (asdf:load-system "web-skeleton-tls")
  (error (e) (format t "note: TLS not loaded (~a)~%" e)))

(defun env-int (name default)
  "Parse a whole integer, or return DEFAULT.

   Set but not an integer warns and falls back, as ENV-HOST does. With
   :JUNK-ALLOWED this read WS_PORT=80a as 80 without a word, while the image's
   healthcheck curls the variable as written, :80a, and marks the container
   unhealthy with nothing in the log to say why."
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v)))
        (handler-case (parse-integer v)
          (parse-error ()
            (format t "note: ~a=~s is not an integer; falling back to ~d~%"
                    name v default)
            default))
        default)))

(defun env-host (name default)
  "Parse a dotted-quad, or return DEFAULT. Only IPv4 literals: a container
   gets told an address, never a name, because resolving one here would make
   startup depend on a resolver that may not be up yet.

   A value that is set but does not parse warns and then falls back, rather
   than falling back in silence. A typo in WS_HOST would otherwise bind
   0.0.0.0 without a word — which is more exposed than whatever was meant, not
   less — and the operator would have no hint their address was ignored."
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
              (progn
                (format t "note: ~a=~s is not a dotted-quad IPv4 literal; ~
                           falling back to the default bind address~%" name v)
                default)))
        default)))

;; WS_INSTANCE only when it is set, so START-DEMO's own default — a fresh
;; random code per process — stays the thing that decides. Passing NIL through
;; would override the default with nothing and every process would be "????".
;; Set it to whatever the deployment already calls this box, and the handles
;; the bulletin shows will agree with the logs somebody reads next to them.
;; WS_ prefixed like the rest of this file's variables, and the same idea as
;; the BACKLINK_URL an operator may already be setting elsewhere. Absent means
;; absent: no link is rendered, rather than a link to nowhere.
(defun env-string (name)
  (let ((v (sb-ext:posix-getenv name)))
    (and v (plusp (length v)) v)))

(let ((instance (env-string "WS_INSTANCE")))
  (apply #'web-skeleton-demo:start-demo
         :host     (env-host "WS_HOST" #(0 0 0 0))
         :port     (env-int  "WS_PORT" 8081)
         :workers  (env-int  "WS_WORKERS" 4)
         :backlink (env-string "WS_BACKLINK_URL")
         (when instance (list :instance instance))))
