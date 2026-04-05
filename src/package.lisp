(defpackage :web-skeleton
  (:use :cl)
  (:export ;; Server
           #:main
           #:start-server
           ;; HTTP request
           #:http-request
           #:http-request-method
           #:http-request-path
           #:http-request-query
           #:http-request-version
           #:http-request-headers
           #:http-request-body
           #:get-header
           #:get-headers
           #:parse-request
           #:find-header-end
           ;; HTTP response
           #:http-response
           #:http-response-status
           #:http-response-headers
           #:http-response-body
           #:set-response-header
           #:format-response
           #:make-text-response
           #:make-html-response
           #:make-error-response
           #:status-reason
           ;; Logging
           #:log-msg
           #:log-debug
           #:log-info
           #:log-warn
           #:log-error
           #:*log-level*
           #:*log-stream*
           ;; Algorithms
           #:sha1
           #:sha1-hex
           #:base64-encode
           ;; epoll FFI
           #:epoll-create
           #:epoll-add
           #:epoll-modify
           #:epoll-remove
           #:epoll-wait
           #:+epollin+
           #:+epollout+
           #:+epollerr+
           #:+epollhup+
           #:+epollet+
           #:set-nonblocking
           #:set-socket-option-int
           #:+sol-socket+
           #:+so-reuseport+
           #:socket-fd
           #:nb-read
           #:nb-write
           ;; Connection
           #:connection
           #:make-client-connection
           #:connection-close
           #:connection-fd
           #:connection-socket
           #:connection-state
           #:connection-request
           #:connection-on-read
           #:connection-on-write
           #:connection-queue-write
           #:connection-parse-request
           ;; WebSocket
           #:websocket-upgrade-p
           #:websocket-on-read
           ;; Tests
           #:test
           #:test-algorithms
           #:test-server
           ;; Conditions
           #:http-parse-error
           ;; Limits
           #:*max-request-line-length*
           #:*max-header-count*
           #:*max-header-line-length*
           #:*max-body-size*
           #:*max-ws-payload-size*))
