(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(handler-case (asdf:load-system "web-skeleton-tls")
  (error () nil))
(asdf:load-system "web-skeleton-demo")
(sb-ext:save-lisp-and-die "web-skeleton"
                           :toplevel #'web-skeleton-demo:main
                           :executable t)
