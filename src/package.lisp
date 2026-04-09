(defpackage :web-skeleton
  (:use :cl)
  (:export ;; Server
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
           #:get-cookie
           #:get-query-param
           #:parse-request
           #:parse-query-string
           #:url-decode
           #:match-path
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
           #:base64-decode
           #:base64url-encode
           #:base64url-decode
           #:bytes-to-hex
           ;; SHA-256
           #:sha256
           #:sha256-hex
           ;; HMAC
           #:hmac-sha256
           ;; ECDSA
           #:ecdsa-verify-p256
           ;; JWT
           #:jwt-key
           #:make-jwt-key
           #:jwt-key-kid
           #:jwt-key-x
           #:jwt-key-y
           #:parse-jwks
           #:jwt-verify
           #:jwt-claim
           ;; JSON
           #:json-parse
           #:json-serialize
           #:json-get
           ;; Connection (minimal — for ws-handler identity)
           #:connection
           #:connection-fd
           ;; WebSocket
           #:ws-frame
           #:ws-frame-fin
           #:ws-frame-opcode
           #:ws-frame-payload
           #:+ws-op-text+
           #:+ws-op-binary+
           #:build-ws-text
           #:build-ws-frame
           #:build-ws-close
           #:ws-send
           ;; Static files
           #:load-static-files
           #:serve-static
           ;; HTTP client (non-blocking outbound fetch)
           #:http-fetch
           #:http-fetch-stream
           #:*https-fetch-fn*
           #:*https-stream-fn*
           ;; Conditions
           #:http-parse-error
           ;; Limits
           #:*max-request-line-length*
           #:*max-header-count*
           #:*max-header-line-length*
           #:*max-body-size*
           #:*max-ws-payload-size*
           ;; Connection lifecycle
           #:*idle-timeout*
           #:*ws-idle-timeout*
           #:*ws-ping-interval*
           #:*ws-max-missed-pongs*
           #:*fetch-timeout*
           ;; Shutdown
           #:*drain-timeout*))
