(defsystem "web-skeleton-tests"
  :description "Tests for the web-skeleton framework"
  :depends-on ("web-skeleton")
  :components ((:file "tests/package")
               (:file "tests/run" :depends-on ("tests/package"))
               (:file "tests/test-algorithms" :depends-on ("tests/run"))
               (:file "tests/test-json" :depends-on ("tests/run"))
               (:file "tests/test-server" :depends-on ("tests/run"))))
