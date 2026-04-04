(defsystem "web-skeleton"
  :description "SBCL web server framework for Linux"
  :version "0.0.1"
  :depends-on ("sb-bsd-sockets")
  :components ((:file "src/package")
               (:file "src/log" :depends-on ("src/package"))
               (:file "src/epoll" :depends-on ("src/package"))
               (:file "src/algorithms/sha1"  :depends-on ("src/package"))
               (:file "src/algorithms/base64" :depends-on ("src/package"))
               (:file "src/server/http"  :depends-on ("src/package"))
               (:file "src/server/connection" :depends-on ("src/package"
                                                            "src/log"
                                                            "src/epoll"
                                                            "src/server/http"))
               (:file "src/server/websocket" :depends-on ("src/package"
                                                           "src/log"
                                                           "src/epoll"
                                                           "src/server/http"
                                                           "src/server/connection"
                                                           "src/algorithms/sha1"
                                                           "src/algorithms/base64"))
               (:file "src/server/main"  :depends-on ("src/package"
                                                       "src/log"
                                                       "src/epoll"
                                                       "src/server/http"
                                                       "src/server/connection"
                                                       "src/server/websocket"))
               ;; Tests
               (:file "tests/run" :depends-on ("src/package"))
               (:file "tests/test-algorithms" :depends-on ("tests/run"
                                                            "src/algorithms/sha1"
                                                            "src/algorithms/base64"))
               (:file "tests/test-server" :depends-on ("tests/run"
                                                        "src/server/http"))))
