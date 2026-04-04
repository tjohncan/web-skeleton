(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "web-skeleton")
(web-skeleton:start-server)
