(defsystem "web-skeleton-tests"
  :description "Tests for the web-skeleton framework"
  :version "0.0.7"
  :author "Tiger Johnson"
  :license "MIT"
  :depends-on ("web-skeleton" "web-skeleton-test-harness")
  :components ((:file "tests/package")
               (:file "tests/run" :depends-on ("tests/package"))
               (:file "tests/test-algorithms" :depends-on ("tests/run"))
               (:file "tests/test-json" :depends-on ("tests/run"))
               (:file "tests/test-server" :depends-on ("tests/run"))
               (:file "tests/test-store" :depends-on ("tests/run"))
               (:file "tests/test-harness" :depends-on ("tests/run"))
               ;; The TLS suite reuses the harness fixtures: the chunked
               ;; corpus its :ON-BODY test shares with the plain-TCP one,
               ;; and %SPLIT-WS behind the listen-table readiness check.
               (:file "tests/test-tls" :depends-on ("tests/run"
                                                    "tests/test-harness"))
               ;; The property suite drives the streaming reader through
               ;; MAKE-MOCK-STREAM, which test-server defines.
               (:file "tests/test-properties" :depends-on ("tests/run"
                                                            "tests/test-server"))))
