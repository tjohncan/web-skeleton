(in-package :web-skeleton)

;;; ===========================================================================
;;; HTTP Request Parser + Response Builder
;;;
;;; Pure logic — operates on strings/byte-arrays, does no I/O.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Limits
;;; ---------------------------------------------------------------------------

(defparameter *max-request-line-length* 8192
  "Maximum length of the HTTP request line in bytes.")

(defparameter *max-header-count* 100
  "Maximum number of headers allowed in a single request.")

(defparameter *max-header-line-length* 8192
  "Maximum length of a single header line in bytes.")

(defparameter *max-body-size* (* 1 1024 1024)
  "Maximum request body size in bytes. Default 1MB.")

(defparameter *max-ws-payload-size* 65536
  "Maximum WebSocket frame payload size in bytes. Default 64KB.")

;;; ---------------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------------

(define-condition http-parse-error (error)
  ((message :initarg :message :reader http-parse-error-message))
  (:report (lambda (condition stream)
             (format stream "HTTP parse error: ~a"
                     (http-parse-error-message condition)))))

(defun http-parse-error (format-string &rest args)
  "Signal an HTTP-PARSE-ERROR with a formatted message."
  (error 'http-parse-error
         :message (apply #'format nil format-string args)))

;;; ---------------------------------------------------------------------------
;;; HTTP request structure
;;; ---------------------------------------------------------------------------

(defstruct http-request
  (method   nil :type (or null keyword))    ; :GET, :POST, :PUT, etc.
  (path     ""  :type string)               ; "/fu/bar"
  (query    nil :type (or null string))     ; "a=1&b=2" or nil
  (version  "1.1" :type string)             ; "1.0" or "1.1"
  (headers  nil :type list)                 ; alist — ((name . value) ...)
  (body     nil :type (or null (simple-array (unsigned-byte 8) (*)))))
                                              ; raw body bytes or nil

;;; ---------------------------------------------------------------------------
;;; Header access
;;; ---------------------------------------------------------------------------

(defun get-header (request name)
  "Look up a header value by NAME (case-insensitive).
   Returns the value string, or NIL if not present."
  (cdr (assoc name (http-request-headers request)
              :test #'string-equal)))

(defun get-headers (request name)
  "Return a list of all values for headers matching NAME (case-insensitive).
   Handles the case where a client sends the same header multiple times."
  (loop for (n . v) in (http-request-headers request)
        when (string-equal n name) collect v))

;;; ---------------------------------------------------------------------------
;;; CRLF utilities
;;; ---------------------------------------------------------------------------

(defvar *crlf* (coerce '(#\Return #\Newline) 'string))
(defvar *crlf-crlf* (concatenate 'string *crlf* *crlf*))

;;; ---------------------------------------------------------------------------
;;; Byte-level scanning helpers
;;; ---------------------------------------------------------------------------

(declaim (inline scan-crlf))

(defun scan-crlf (buf start end)
  "Find next CRLF (13 10) in BUF[START..END). Returns position of CR, or NIL."
  (loop for i from start below (1- end)
        when (and (= (aref buf i) 13) (= (aref buf (1+ i)) 10))
        return i))

(defun scan-crlf-crlf (buf start end)
  "Find CRLFCRLF in BUF[START..END). Returns position of first CR, or NIL."
  (loop for i from start to (- end 4)
        when (and (= (aref buf i)       13)
                  (= (aref buf (+ i 1)) 10)
                  (= (aref buf (+ i 2)) 13)
                  (= (aref buf (+ i 3)) 10))
        return i))

;;; ---------------------------------------------------------------------------
;;; Byte-to-string conversion (allocation-minimal)
;;; ---------------------------------------------------------------------------

(defun bytes-to-string (buf start end)
  "Convert BUF[START..END) to a UTF-8 string. Single allocation."
  (sb-ext:octets-to-string buf :start start :end end :external-format :utf-8))

(defun bytes-to-lowercase-string (buf start end)
  "Convert BUF[START..END) to a lowercase ASCII string. Single allocation."
  (let ((str (make-string (- end start))))
    (loop for i from start below end
          for j from 0
          do (let ((b (aref buf i)))
               (setf (char str j)
                     (if (<= 65 b 90)       ; A-Z → a-z
                         (code-char (+ b 32))
                         (code-char b)))))
    str))

(defun trim-ows-bounds (buf start end)
  "Return (values trimmed-start trimmed-end) with leading/trailing OWS removed."
  (let ((s start) (e end))
    (loop while (and (< s e) (let ((b (aref buf s))) (or (= b 32) (= b 9))))
          do (incf s))
    (loop while (and (> e s) (let ((b (aref buf (1- e)))) (or (= b 32) (= b 9))))
          do (decf e))
    (values s e)))

;;; ---------------------------------------------------------------------------
;;; Byte-level HTTP method matching
;;; ---------------------------------------------------------------------------

(defun match-method-bytes (buf start end)
  "Match BUF[START..END) against known HTTP methods. Returns keyword or NIL."
  (flet ((match-p (str)
           (let ((len (length str)))
             (and (= (- end start) len)
                  (loop for i from 0 below len
                        always (= (aref buf (+ start i))
                                  (char-code (char str i))))))))
    (cond
      ((match-p "GET")     :GET)
      ((match-p "POST")    :POST)
      ((match-p "PUT")     :PUT)
      ((match-p "DELETE")  :DELETE)
      ((match-p "HEAD")    :HEAD)
      ((match-p "OPTIONS") :OPTIONS)
      ((match-p "PATCH")   :PATCH)
      ((match-p "TRACE")   :TRACE)
      ((match-p "CONNECT") :CONNECT))))

;;; ---------------------------------------------------------------------------
;;; Byte-level header parser (single-pass)
;;; ---------------------------------------------------------------------------

(defun parse-headers-bytes (buf start end)
  "Parse headers from BUF[START..END) into an alist of (lowercase-name . value).
   Single-pass, handles obsolete line folding (RFC 7230 §3.2.4).
   Stops at the first empty line (CRLFCRLF boundary)."
  (let ((headers nil)
        (count 0)
        (pos start))
    (loop
      (let ((crlf (scan-crlf buf pos end)))
        (unless crlf (return))          ; no more complete lines
        (when (= crlf pos) (return))    ; empty line = end of headers
        (let ((line-len (- crlf pos)))
          (when (> line-len *max-header-line-length*)
            (http-parse-error "header line too long (~d bytes, max ~d)"
                              line-len *max-header-line-length*))
          (let ((first-byte (aref buf pos)))
            (if (and headers (or (= first-byte 32) (= first-byte 9)))
                ;; Continuation line — append to previous header value
                (multiple-value-bind (ts te) (trim-ows-bounds buf pos crlf)
                  (when (> te ts)
                    (let* ((prev (car headers))
                           (extra (bytes-to-string buf ts te)))
                      (setf (cdr prev)
                            (concatenate 'string (cdr prev) " " extra)))))
                ;; New header — find colon
                (let ((colon (position 58 buf :start pos :end crlf))) ; 58 = ':'
                  (unless colon
                    (http-parse-error "malformed header (no colon)"))
                  (when (= colon pos)
                    (http-parse-error "empty header name"))
                  ;; Reject whitespace in header name
                  (loop for i from pos below colon
                        when (let ((b (aref buf i))) (or (= b 32) (= b 9)))
                        do (http-parse-error "whitespace in header name"))
                  (let ((name (bytes-to-lowercase-string buf pos colon)))
                    (multiple-value-bind (vs ve)
                        (trim-ows-bounds buf (1+ colon) crlf)
                      (push (cons name (if (= vs ve) ""
                                           (bytes-to-string buf vs ve)))
                            headers)
                      (incf count)))))))
        (setf pos (+ crlf 2))))
    (when (> count *max-header-count*)
      (http-parse-error "too many headers (~d, max ~d)"
                        count *max-header-count*))
    (nreverse headers)))

;;; ---------------------------------------------------------------------------
;;; Byte-level request parser
;;; ---------------------------------------------------------------------------

(defun parse-request-bytes (buf start end)
  "Parse HTTP request directly from bytes BUF[START..END).
   END should be past the CRLFCRLF terminator.
   Returns an HTTP-REQUEST. Body is not extracted."
  (let ((req-end (scan-crlf buf start end)))
    (unless req-end
      (http-parse-error "incomplete request (no CRLF in request line)"))
    (let ((req-line-len (- req-end start)))
      (when (zerop req-line-len)
        (http-parse-error "empty request line"))
      (when (> req-line-len *max-request-line-length*)
        (http-parse-error "request line too long (~d bytes, max ~d)"
                          req-line-len *max-request-line-length*)))
    ;; Parse: METHOD SP URI SP VERSION
    (let ((sp1 (position 32 buf :start start :end req-end)))  ; 32 = space
      (unless sp1
        (http-parse-error "malformed request line"))
      (let ((sp2 (position 32 buf :start (1+ sp1) :end req-end)))
        (unless sp2
          (http-parse-error "malformed request line"))
        (when (position 32 buf :start (1+ sp2) :end req-end)
          (http-parse-error "malformed request line (extra spaces)"))
        ;; Method
        (let ((method (match-method-bytes buf start sp1)))
          (unless method
            (http-parse-error "unrecognized method: ~a"
                              (bytes-to-string buf start sp1)))
          ;; Version — match "HTTP/1.0" or "HTTP/1.1" byte-by-byte
          (let* ((ver-start (1+ sp2))
                 (ver-len (- req-end ver-start))
                 (version
                   (when (and (= ver-len 8)
                              (= (aref buf ver-start)       72)  ; H
                              (= (aref buf (+ ver-start 1)) 84)  ; T
                              (= (aref buf (+ ver-start 2)) 84)  ; T
                              (= (aref buf (+ ver-start 3)) 80)  ; P
                              (= (aref buf (+ ver-start 4)) 47)  ; /
                              (= (aref buf (+ ver-start 5)) 49)  ; 1
                              (= (aref buf (+ ver-start 6)) 46)) ; .
                     (case (aref buf (+ ver-start 7))
                       (49 "1.1")    ; '1'
                       (48 "1.0"))))) ; '0'
            (unless version
              (http-parse-error "unsupported HTTP version"))
            ;; URI → path + query
            (let* ((uri-start (1+ sp1))
                   (qmark (position 63 buf :start uri-start :end sp2))  ; 63 = '?'
                   (path-end (or qmark sp2)))
              (when (= uri-start path-end)
                (http-parse-error "empty request path"))
              (unless (= (aref buf uri-start) 47)  ; 47 = '/'
                (http-parse-error "request path must start with /"))
              (let ((path (bytes-to-string buf uri-start path-end))
                    (query (when qmark
                             (bytes-to-string buf (1+ qmark) sp2))))
                (let ((headers (parse-headers-bytes buf (+ req-end 2) end)))
                  (make-http-request
                   :method method
                   :path path
                   :query query
                   :version version
                   :headers headers))))))))))

;;; ---------------------------------------------------------------------------
;;; String-level convenience interface
;;; ---------------------------------------------------------------------------

(defun find-header-end (data)
  "Find the position of the CRLFCRLF that terminates the header block.
   Returns the index of the first CR, or NIL if not found."
  (search *crlf-crlf* data))

(defun parse-request (raw-data)
  "Parse a raw HTTP request header string into an HTTP-REQUEST struct.
   Convenience wrapper — converts to bytes and calls the byte-level parser."
  (let* ((bytes (sb-ext:string-to-octets raw-data :external-format :utf-8))
         (end (scan-crlf-crlf bytes 0 (length bytes))))
    (unless end
      (http-parse-error "incomplete headers (no CRLFCRLF terminator)"))
    (parse-request-bytes bytes 0 (+ end 4))))

;;; ===========================================================================
;;; HTTP Response Builder
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Status codes
;;; ---------------------------------------------------------------------------

(defparameter *status-reasons*
  '((100 . "Continue")
    (101 . "Switching Protocols")
    (200 . "OK")
    (201 . "Created")
    (204 . "No Content")
    (301 . "Moved Permanently")
    (302 . "Found")
    (304 . "Not Modified")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (408 . "Request Timeout")
    (413 . "Payload Too Large")
    (414 . "URI Too Long")
    (431 . "Request Header Fields Too Large")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable"))
  "Map of HTTP status codes to reason phrases.")

(defun status-reason (code)
  "Return the reason phrase for a status CODE, or \"Unknown\"."
  (or (cdr (assoc code *status-reasons*)) "Unknown"))

;;; ---------------------------------------------------------------------------
;;; Response structure
;;; ---------------------------------------------------------------------------

(defstruct http-response
  (status  200   :type integer)
  (headers nil   :type list)      ; alist — ((name . value) ...)
  (body    nil   :type (or null string)))

;;; ---------------------------------------------------------------------------
;;; Response building helpers
;;; ---------------------------------------------------------------------------

(defun set-response-header (response name value)
  "Set a header on RESPONSE. Replaces any existing header with the same name."
  (let ((key (string-downcase name)))
    (setf (http-response-headers response)
          (cons (cons key value)
                (remove key (http-response-headers response)
                        :key #'car :test #'string=))))
  response)

(defun format-response (response)
  "Serialize an HTTP-RESPONSE into a byte vector ready to write to a socket.
   Writes directly into a single pre-sized buffer — no intermediate strings."
  (let* ((status (http-response-status response))
         (reason (status-reason status))
         (body   (http-response-body response))
         (body-bytes (when body
                       (sb-ext:string-to-octets body :external-format :utf-8)))
         ;; Auto-set Content-Length if there's a body and it's not already set
         (headers (http-response-headers response))
         (headers (if (and body-bytes
                           (not (assoc "content-length" headers :test #'string=)))
                      (cons (cons "content-length"
                                  (write-to-string (length body-bytes)))
                            headers)
                      headers))
         ;; Pre-calculate exact buffer size
         (header-size (+ 9                ; "HTTP/1.1 "
                         3                ; status code (3 digits)
                         1                ; space before reason
                         (length reason)
                         2                ; CRLF
                         (loop for (name . value) in headers
                               sum (+ (length name) 2 (length value) 2))
                         2))              ; final CRLF
         (body-len (if body-bytes (length body-bytes) 0))
         (buf (make-array (+ header-size body-len)
                          :element-type '(unsigned-byte 8)))
         (pos 0))
    (flet ((put-byte (b)
             (setf (aref buf pos) b)
             (incf pos))
           (put-ascii (str)
             (loop for i from 0 below (length str)
                   do (setf (aref buf pos) (char-code (char str i)))
                      (incf pos)))
           (put-crlf ()
             (setf (aref buf pos) 13 (aref buf (1+ pos)) 10)
             (incf pos 2)))
      ;; Status line: "HTTP/1.1 NNN reason\r\n"
      (put-ascii "HTTP/1.1 ")
      (put-byte (+ 48 (floor status 100)))
      (put-byte (+ 48 (mod (floor status 10) 10)))
      (put-byte (+ 48 (mod status 10)))
      (put-byte 32)
      (put-ascii reason)
      (put-crlf)
      ;; Headers: "name: value\r\n"
      (dolist (h headers)
        (put-ascii (car h))
        (put-byte 58) (put-byte 32)
        (put-ascii (cdr h))
        (put-crlf))
      ;; Blank line terminates headers
      (put-crlf)
      ;; Body
      (when body-bytes
        (replace buf body-bytes :start1 pos)))
    buf))

;;; ---------------------------------------------------------------------------
;;; Convenience constructors
;;; ---------------------------------------------------------------------------

(defun make-text-response (status body &key (content-type "text/plain; charset=utf-8"))
  "Build a response with a text body."
  (let ((resp (make-http-response :status status :body body)))
    (set-response-header resp "content-type" content-type)
    (set-response-header resp "connection" "close")
    resp))

(defun make-html-response (status body)
  "Build a response with an HTML body."
  (make-text-response status body :content-type "text/html; charset=utf-8"))

(defun make-error-response (status &optional message)
  "Build a plain-text error response."
  (let ((body (or message
                  (format nil "~d ~a" status (status-reason status)))))
    (make-text-response status body)))
