(defsystem "web-skeleton-test-harness"
  :description "Test harness for web-skeleton — ephemeral-port live server and unit-style request builders"
  :version "0.0.7"
  :author "Tiger Johnson"
  :license "MIT"
  :depends-on ("web-skeleton")
  :components ((:file "tests/harness")))
