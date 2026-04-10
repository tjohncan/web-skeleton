(defsystem "web-skeleton-demo"
  :description "Demo application for the web-skeleton framework"
  :version "0.0.7"
  :author "Tiger Johnson"
  :license "MIT"
  :depends-on ("web-skeleton")
  :components ((:file "demo/package")
               (:file "demo/handler" :depends-on ("demo/package"))))
