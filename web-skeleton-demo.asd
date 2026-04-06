(defsystem "web-skeleton-demo"
  :description "Demo application for the web-skeleton framework"
  :depends-on ("web-skeleton")
  :components ((:file "demo/package")
               (:file "demo/handler" :depends-on ("demo/package"))))
