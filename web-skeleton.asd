(defsystem "web-skeleton"
  :description "SBCL web server framework for Linux"
  :version "0.0.7"
  :author "Tiger Johnson"
  :license "MIT"
  :depends-on ("sb-bsd-sockets")
  :components ((:file "src/package")
               (:file "src/log" :depends-on ("src/package"))
               (:file "src/epoll" :depends-on ("src/package"))
               (:file "src/algorithms/hex"   :depends-on ("src/package"))
               (:file "src/algorithms/word32" :depends-on ("src/package"))
               (:file "src/algorithms/sha1"  :depends-on ("src/package"
                                                           "src/algorithms/hex"
                                                           "src/algorithms/word32"))
               (:file "src/algorithms/sha256" :depends-on ("src/package"
                                                            "src/algorithms/hex"
                                                            "src/algorithms/word32"))
               (:file "src/algorithms/hmac"   :depends-on ("src/package"
                                                           "src/algorithms/sha256"))
               (:file "src/algorithms/ecdsa"  :depends-on ("src/package"))
               (:file "src/algorithms/base64" :depends-on ("src/package"))
               (:file "src/server/http"  :depends-on ("src/package"
                                                       "src/algorithms/hex"))
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
               (:file "src/server/static" :depends-on ("src/package"
                                                         "src/log"
                                                         "src/server/http"))
               (:file "src/json" :depends-on ("src/package"))
               (:file "src/server/jwt"  :depends-on ("src/package"
                                                       "src/json"
                                                       "src/algorithms/sha256"
                                                       "src/algorithms/base64"
                                                       "src/algorithms/ecdsa"))
               (:file "src/server/fetch" :depends-on ("src/package"
                                                        "src/log"
                                                        "src/epoll"
                                                        "src/server/http"
                                                        "src/server/connection"))
               (:file "src/server/main"  :depends-on ("src/package"
                                                       "src/log"
                                                       "src/epoll"
                                                       "src/server/http"
                                                       "src/server/connection"
                                                       "src/server/websocket"
                                                       "src/server/static"
                                                       "src/server/fetch"))))
