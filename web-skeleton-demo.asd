(defsystem "web-skeleton-demo"
  :description "Demo application for the web-skeleton framework"
  :author "Tiger Johnson"
  :license "MIT"
  :depends-on ("web-skeleton")
  :components ((:file "demo/package")
               (:file "demo/handler" :depends-on ("demo/package"))))
