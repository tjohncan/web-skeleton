(in-package :web-skeleton)

;;; ===========================================================================
;;; Non-blocking HTTP Client (outbound fetch)
;;;
;;; Integrates with the event loop — outbound connections are registered
;;; with the same epoll fd and processed alongside inbound connections.
;;; Zero blocking in the event loop.
;;;
;;; Usage from a handler:
;;;   (http-fetch :get "http://host/path"
;;;               :then (lambda (status headers body)
;;;                       (make-text-response 200 body)))
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Fetch request descriptor (returned by handler)
;;; ---------------------------------------------------------------------------

(defstruct http-fetch-request
  "Descriptor for an outbound HTTP request. Returned by http-fetch."
  (method   :GET   :type keyword)
  (url      ""     :type string)
  (headers  nil    :type list)
  (body     nil)
  (callback nil    :type function))

(defun http-fetch (method url &key headers body then)
  "Create an outbound HTTP request descriptor.
   Return this from a handler to initiate a non-blocking outbound call.
   THEN is called with (status headers body) when the response arrives;
   it must return an HTTP response, byte vector, or another http-fetch-request."
  (unless then
    (error "http-fetch requires :then callback"))
  (make-http-fetch-request :method method :url url
                            :headers headers :body body
                            :callback then))

;;; ---------------------------------------------------------------------------
;;; URL parsing
;;; ---------------------------------------------------------------------------

(defun parse-url (url)
  "Parse a URL into (values scheme host port path).
   Supports http:// and https:// schemes."
  (let ((scheme nil) (authority-start nil) (default-port nil))
    (cond
      ((and (>= (length url) 8) (string-equal url "https://" :end1 8))
       (setf scheme :https authority-start 8 default-port 443))
      ((and (>= (length url) 7) (string-equal url "http://" :end1 7))
       (setf scheme :http authority-start 7 default-port 80))
      (t (error "unsupported URL scheme: ~a" url)))
    (let* ((path-start (or (position #\/ url :start authority-start)
                           (length url)))
           (authority (subseq url authority-start path-start))
           (path (if (= path-start (length url)) "/" (subseq url path-start)))
           (colon (position #\: authority))
           (host (if colon (subseq authority 0 colon) authority))
           (port (if colon (parse-integer (subseq authority (1+ colon)))
                     default-port)))
      (values scheme host port path))))

;;; ---------------------------------------------------------------------------
;;; Build outbound HTTP request bytes
;;; ---------------------------------------------------------------------------

(defun build-outbound-request (method host path &key headers body)
  "Build an HTTP/1.1 request as a byte vector ready to write."
  (let* ((method-str (symbol-name method))
         (body-bytes (etypecase body
                       (null nil)
                       (string (sb-ext:string-to-octets body
                                                         :external-format :utf-8))
                       ((simple-array (unsigned-byte 8) (*)) body)))
         (all-headers (append (list (cons "host" host)
                                    (cons "connection" "close"))
                              headers
                              (when body-bytes
                                (list (cons "content-length"
                                            (write-to-string
                                             (length body-bytes))))))))
    (serialize-http-message
     (format nil "~a ~a HTTP/1.1" method-str path)
     all-headers body-bytes)))

;;; ---------------------------------------------------------------------------
;;; Parse response status line
;;; ---------------------------------------------------------------------------

(defun parse-response-status (buf start end)
  "Extract the integer status code from a response line in BUF[START..END).
   Expects 'HTTP/1.x NNN reason'. Returns the status code or NIL."
  (let ((crlf (scan-crlf buf start end)))
    (unless crlf (return-from parse-response-status nil))
    ;; Find space after "HTTP/1.x"
    (let ((sp (position 32 buf :start start :end crlf)))
      (unless sp (return-from parse-response-status nil))
      (let ((s (1+ sp)))
        (when (< (+ s 2) (length buf))
          (let ((d1 (- (aref buf s) 48))
                (d2 (- (aref buf (+ s 1)) 48))
                (d3 (- (aref buf (+ s 2)) 48)))
            (when (and (<= 0 d1 9) (<= 0 d2 9) (<= 0 d3 9))
              (+ (* d1 100) (* d2 10) d3))))))))

;;; ---------------------------------------------------------------------------
;;; HTTPS hook — set by web-skeleton-tls when loaded
;;; ---------------------------------------------------------------------------

(defvar *https-fetch-fn* nil
  "When non-NIL, a function (conn epoll-fd fetch-req host port path) that
   performs a blocking HTTPS fetch.  Set by web-skeleton-tls on load.")

;;; ---------------------------------------------------------------------------
;;; Initiate outbound fetch
;;; ---------------------------------------------------------------------------

(defun initiate-fetch (conn epoll-fd fetch-req)
  "Start an outbound HTTP(S) request.
   CONN is the inbound connection to park.
   FETCH-REQ is the http-fetch-request descriptor.
   HTTP uses non-blocking epoll I/O. HTTPS dispatches to *https-fetch-fn*
   (blocking on the worker thread) — requires web-skeleton-tls."
  (handler-case
      (multiple-value-bind (scheme host port path)
          (parse-url (http-fetch-request-url fetch-req))
        (if (eq scheme :https)
            ;; HTTPS — blocking path via TLS hook
            (if *https-fetch-fn*
                (funcall *https-fetch-fn* conn epoll-fd fetch-req host port path)
                (error "HTTPS not available — load web-skeleton-tls"))
            ;; HTTP — non-blocking epoll path
            (initiate-http-fetch conn epoll-fd fetch-req host port path)))
    (error (e)
      (log-error "fetch setup failed: ~a" e)
      (let ((error-response (format-response (make-error-response 502))))
        (connection-queue-write conn error-response)
        (setf (connection-state conn) :write-response)
        (epoll-modify epoll-fd (connection-fd conn)
                     (logior +epollout+ +epollet+))))))

(defun initiate-http-fetch (conn epoll-fd fetch-req host port path)
  "Start a non-blocking outbound HTTP request via epoll."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (set-nonblocking (socket-fd socket))
    (let ((addr (sb-bsd-sockets:host-ent-address
                 (sb-bsd-sockets:get-host-by-name host))))
      (handler-case
          (sb-bsd-sockets:socket-connect socket addr port)
        (error () nil))
      (let* ((out-fd (socket-fd socket))
             (request-bytes (build-outbound-request
                            (http-fetch-request-method fetch-req)
                            host path
                            :headers (http-fetch-request-headers fetch-req)
                            :body (http-fetch-request-body fetch-req)))
             (out-conn (make-connection
                        :fd out-fd
                        :socket socket
                        :state :out-connecting
                        :outbound-p t
                        :inbound-fd (connection-fd conn)
                        :fetch-callback (http-fetch-request-callback fetch-req)
                        :last-active (get-universal-time))))
        (connection-queue-write out-conn request-bytes)
        (register-connection out-conn)
        (epoll-add epoll-fd out-fd (logior +epollout+ +epollet+))
        (setf (connection-state conn) :awaiting
              (connection-awaiting-fd conn) out-fd)
        (log-debug "fetch: fd ~d -> ~a:~d~a (outbound fd ~d)"
                   (connection-fd conn) host port path out-fd)))))

;;; ---------------------------------------------------------------------------
;;; Outbound event handlers
;;; ---------------------------------------------------------------------------

(defun handle-outbound-event (conn epoll-fd flags)
  "Handle an epoll event on an outbound connection."
  (handler-case
      (cond
        ((or (logtest flags +epollerr+) (logtest flags +epollhup+))
         (deliver-fetch-error conn epoll-fd "connection error"))
        ((logtest flags +epollout+)
         (ecase (connection-state conn)
           (:out-connecting (handle-outbound-connect conn epoll-fd))
           (:out-write      (handle-outbound-write conn epoll-fd))))
        ((logtest flags +epollin+)
         (ecase (connection-state conn)
           (:out-read (handle-outbound-read conn epoll-fd)))))
    (error (e)
      (log-error "outbound error fd ~d: ~a" (connection-fd conn) e)
      (deliver-fetch-error conn epoll-fd "outbound request failed"))))

(defun handle-outbound-connect (conn epoll-fd)
  "Check if non-blocking connect succeeded, then start writing the request."
  (let ((err (get-socket-option-int (connection-fd conn)
                                     +sol-socket+ +so-error+)))
    (if (zerop err)
        (progn
          ;; Connect succeeded — start writing request (already queued)
          (setf (connection-state conn) :out-write)
          (handle-outbound-write conn epoll-fd))
        ;; Connect failed
        (deliver-fetch-error conn epoll-fd
                             (format nil "connect failed: errno ~d" err)))))

(defun handle-outbound-write (conn epoll-fd)
  "Flush the outbound HTTP request. When done, switch to reading the response."
  (let ((result (connection-on-write conn)))
    (case result
      (:done
       (setf (connection-state conn) :out-read
             (connection-read-pos conn) 0)
       (epoll-modify epoll-fd (connection-fd conn)
                    (logior +epollin+ +epollet+)))
      ;; :continue — more bytes to write
      )))

(defun handle-outbound-read (conn epoll-fd)
  "Read the outbound HTTP response. When complete, deliver to the inbound connection."
  (let ((result (connection-read-available conn)))
    (case result
      (:eof
       ;; Server closed connection — response is complete (Connection: close)
       (complete-fetch conn epoll-fd))
      (:full
       ;; Buffer full — try to complete with what we have
       (complete-fetch conn epoll-fd))
      (:again nil)  ; wait for more data
      (:ok
       ;; Got data — check if we have a complete response
       (let* ((buf (connection-read-buf conn))
              (pos (connection-read-pos conn))
              (header-end (scan-crlf-crlf buf 0 pos)))
         (when header-end
           ;; Have complete headers — check if body is complete
           (let* ((body-start (+ header-end 4))
                  (content-length (scan-content-length buf header-end)))
             (if (and content-length (> content-length 0))
                 ;; Have Content-Length — wait for full body
                 (when (>= (- pos body-start) content-length)
                   (complete-fetch conn epoll-fd))
                 ;; No Content-Length — wait for EOF (Connection: close)
                 nil))))))))

;;; ---------------------------------------------------------------------------
;;; Deliver fetch result to the parked inbound connection
;;; ---------------------------------------------------------------------------

(defun complete-fetch (out-conn epoll-fd)
  "Parse the outbound response and deliver it to the parked inbound connection."
  (let* ((buf (connection-read-buf out-conn))
         (pos (connection-read-pos out-conn))
         (header-end (scan-crlf-crlf buf 0 pos))
         (status (when header-end (parse-response-status buf 0 pos)))
         (body-start (when header-end (+ header-end 4)))
         ;; Parse response headers
         (headers (when header-end
                    (let ((first-crlf (scan-crlf buf 0 header-end)))
                      (when first-crlf
                        (parse-headers-bytes buf (+ first-crlf 2)
                                             (+ header-end 4))))))
         ;; Extract body
         (body (when (and body-start (> pos body-start))
                 (subseq buf body-start pos)))
         ;; Call the user's callback
         (callback (connection-fetch-callback out-conn))
         (inbound-fd (connection-inbound-fd out-conn)))
    ;; Clean up outbound connection
    (close-outbound out-conn epoll-fd)
    ;; Find and resume inbound connection
    (let ((inbound (lookup-connection inbound-fd)))
      (when inbound
        (handler-case
            (let ((response (funcall callback
                                     (or status 0) (or headers nil)
                                     (or body nil))))
              ;; Queue the response on the inbound connection
              (let ((bytes (cond
                             ((typep response '(simple-array (unsigned-byte 8) (*)))
                              response)
                             ((typep response 'http-fetch-request)
                              ;; Chained fetch — initiate another outbound call
                              (initiate-fetch inbound epoll-fd response)
                              (return-from complete-fetch))
                             (t (format-response response)))))
                (connection-queue-write inbound bytes)
                (setf (connection-state inbound) :write-response
                      (connection-awaiting-fd inbound) -1)
                (epoll-modify epoll-fd (connection-fd inbound)
                             (logior +epollout+ +epollet+))
                (log-debug "fetch: resumed fd ~d" inbound-fd)))
          (error (e)
            (log-error "fetch callback error: ~a" e)
            (let ((err-bytes (format-response (make-error-response 500))))
              (connection-queue-write inbound err-bytes)
              (setf (connection-state inbound) :write-response
                    (connection-awaiting-fd inbound) -1)
              (epoll-modify epoll-fd (connection-fd inbound)
                           (logior +epollout+ +epollet+)))))))))

(defun deliver-fetch-error (out-conn epoll-fd message)
  "Deliver a 502 error to the inbound connection and clean up."
  (log-warn "fetch error fd ~d: ~a" (connection-fd out-conn) message)
  (let ((inbound-fd (connection-inbound-fd out-conn)))
    (close-outbound out-conn epoll-fd)
    (let ((inbound (lookup-connection inbound-fd)))
      (when inbound
        (let ((err-bytes (format-response (make-error-response 502))))
          (connection-queue-write inbound err-bytes)
          (setf (connection-state inbound) :write-response
                (connection-awaiting-fd inbound) -1)
          (epoll-modify epoll-fd (connection-fd inbound)
                       (logior +epollout+ +epollet+)))))))

(defun close-outbound (conn epoll-fd)
  "Close and unregister an outbound connection."
  (let ((fd (connection-fd conn)))
    (when (>= fd 0)
      (ignore-errors (epoll-remove epoll-fd fd))
      (unregister-connection conn)
      (connection-close conn)
      (log-debug "fetch: closed outbound fd ~d" fd))))
