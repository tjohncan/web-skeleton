(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "web-skeleton-demo")
(web-skeleton-demo:start-demo)
