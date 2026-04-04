(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "web-skeleton")
(sb-ext:save-lisp-and-die "web-skeleton"
                           :toplevel #'web-skeleton:main
                           :executable t)
