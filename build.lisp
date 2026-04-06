(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "web-skeleton-demo")
(sb-ext:save-lisp-and-die "web-skeleton"
                           :toplevel #'web-skeleton-demo:main
                           :executable t)
