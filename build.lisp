(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "web-skeleton-demo")
(handler-case (asdf:load-system "web-skeleton-tls")
  (error (e) (format t "Note: TLS not loaded (~a)~%" e)))
(sb-ext:save-lisp-and-die "web-skeleton"
                           :toplevel #'web-skeleton-demo:main
                           :executable t)
