(defsystem "web-skeleton"
  :description "SBCL web server framework for Linux"
  :version "0.0.7"
  :author "Tiger Johnson"
  :license "MIT"
  :depends-on ("sb-bsd-sockets")
  :components ((:file "src/package")
               (:file "src/log" :depends-on ("src/package"))
               (:file "src/counters" :depends-on ("src/package"))
               (:file "src/epoll" :depends-on ("src/package"))
               (:file "src/address" :depends-on ("src/package"))
               (:file "src/algorithms/hex"   :depends-on ("src/package"))
               (:file "src/algorithms/word32" :depends-on ("src/package"))
               (:file "src/algorithms/octets" :depends-on ("src/package"))
               (:file "src/algorithms/sha1"  :depends-on ("src/package"
                                                           "src/algorithms/hex"
                                                           "src/algorithms/word32"
                                                           "src/algorithms/octets"))
               (:file "src/algorithms/sha256" :depends-on ("src/package"
                                                            "src/algorithms/hex"
                                                            "src/algorithms/word32"
                                                            "src/algorithms/octets"))
               (:file "src/algorithms/hmac"   :depends-on ("src/package"
                                                           "src/algorithms/sha256"
                                                           "src/algorithms/octets"))
               (:file "src/algorithms/ecdsa"  :depends-on ("src/package"
                                                           "src/algorithms/octets"))
               (:file "src/algorithms/base64" :depends-on ("src/package"))
               (:file "src/random" :depends-on ("src/package"
                                                 "src/algorithms/base64"))
               (:file "src/server/http"  :depends-on ("src/counters" "src/package"
                                                       "src/algorithms/hex"))
               ;; src/server/http for *MAX-HEADER-LINE-LENGTH*, which also
               ;; caps a chunk-size line.
               (:file "src/server/chunked" :depends-on ("src/package"
                                                          "src/algorithms/hex"
                                                          "src/server/http"))
               (:file "src/server/connection" :depends-on ("src/package"
                                                            "src/log"
                                                            "src/epoll"
                                                            "src/address"
                                                            "src/algorithms/hex"
                                                            "src/server/http"
                                                            "src/server/chunked"))
               (:file "src/server/websocket" :depends-on ("src/counters" "src/package"
                                                           "src/log"
                                                           "src/epoll"
                                                           "src/server/http"
                                                           "src/server/connection"
                                                           "src/algorithms/sha1"
                                                           "src/algorithms/base64"))
               (:file "src/server/static" :depends-on ("src/counters" "src/package"
                                                         "src/log"
                                                         "src/server/http"
                                                         ;; WRITE-SPAN, for a 206.
                                                         "src/server/connection"))
               (:file "src/json" :depends-on ("src/package"))
               (:file "src/server/jwt"  :depends-on ("src/package"
                                                       "src/json"
                                                       "src/algorithms/sha256"
                                                       "src/algorithms/base64"
                                                       "src/algorithms/ecdsa"))
               ;; src/address supplies FORMAT-IP (address → log string) and
               ;; the IS-PUBLIC-ADDRESS-P classifier the address filter is
               ;; meant to be paired with. Both fetch and dns call it, so
               ;; the dependency is declared rather than left to rely on
               ;; the order of this list.
               ;; src/server/streaming for the chunk encoder a streamed
               ;; request body is framed with, and src/server/body for the
               ;; resume that lets a received body go once the upload it
               ;; feeds has drained.
               (:file "src/server/fetch" :depends-on ("src/package"
                                                        "src/log"
                                                        "src/epoll"
                                                        "src/address"
                                                        "src/server/http"
                                                        "src/server/chunked"
                                                        "src/server/connection"
                                                        "src/server/websocket"
                                                        "src/server/streaming"
                                                        "src/server/body"))
               (:file "src/server/dns" :depends-on ("src/package"
                                                      "src/log"
                                                      "src/epoll"
                                                      "src/address"
                                                      "src/server/http"
                                                      "src/server/connection"
                                                      "src/server/fetch"))
               (:file "src/server/streaming" :depends-on ("src/counters" "src/package"
                                                           "src/log"
                                                           "src/epoll"
                                                           "src/server/http"
                                                           "src/server/connection"))
               (:file "src/server/body" :depends-on ("src/package"
                                                      "src/epoll"
                                                      "src/server/http"
                                                      "src/server/connection"))
               (:file "src/server/multipart" :depends-on ("src/package"
                                                           "src/server/http"
                                                           "src/server/connection"
                                                           "src/server/body"))
               (:file "src/server/main"  :depends-on ("src/counters" "src/package"
                                                       "src/log"
                                                       "src/epoll"
                                                       ;; WITH-WORKER-URANDOM is a macro, so this
                                                       ;; edge has to hold at compile time: without
                                                       ;; it, main.lisp would compile the form as a
                                                       ;; function call, the worker body would
                                                       ;; become an argument, and the binding would
                                                       ;; silently never happen.
                                                       "src/random"
                                                       "src/server/http"
                                                       "src/server/connection"
                                                       "src/server/websocket"
                                                       "src/server/static"
                                                       "src/server/fetch"
                                                       "src/server/dns"
                                                       "src/server/streaming"
                                                       "src/server/body"))
               (:file "src/store" :depends-on ("src/package"
                                                "src/log"
                                                "src/server/main"))))
