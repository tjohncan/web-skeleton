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
;;; Recognized methods
;;; ---------------------------------------------------------------------------

(defparameter *http-methods*
  '("GET" "HEAD" "POST" "PUT" "DELETE" "OPTIONS" "PATCH" "TRACE" "CONNECT")
  "HTTP methods we recognize. Anything else is rejected.")

;;; ---------------------------------------------------------------------------
;;; HTTP request structure
;;; ---------------------------------------------------------------------------

(defstruct http-request
  (method   nil :type (or null keyword))    ; :GET, :POST, :PUT, etc.
  (path     ""  :type string)               ; "/fu/bar"
  (query    nil :type (or null string))     ; "a=1&b=2" or nil
  (version  "1.1" :type string)             ; "1.0" or "1.1"
  (headers  nil :type list)                 ; alist — ((name . value) ...)
  (body     nil :type (or null string)))    ; body text or nil

;;; ---------------------------------------------------------------------------
;;; Header access
;;; ---------------------------------------------------------------------------

(defun get-header (request name)
  "Look up a header value by NAME (case-insensitive).
   Returns the value string, or NIL if not present."
  (cdr (assoc (string-downcase name)
              (http-request-headers request)
              :test #'string=)))

(defun get-headers (request name)
  "Return a list of all values for headers matching NAME (case-insensitive).
   Handles the case where a client sends the same header multiple times."
  (let ((key (string-downcase name)))
    (loop for (n . v) in (http-request-headers request)
          when (string= n key) collect v)))

;;; ---------------------------------------------------------------------------
;;; CRLF utilities
;;; ---------------------------------------------------------------------------

(defvar *crlf* (coerce '(#\Return #\Newline) 'string))
(defvar *crlf-crlf* (concatenate 'string *crlf* *crlf*))

(defun crlf-split (text)
  "Split TEXT on CRLF boundaries. Returns a list of strings."
  (let ((lines '())
        (start 0)
        (crlf-len 2)
        (len (length text)))
    (loop
      (let ((pos (search *crlf* text :start2 start)))
        (if pos
            (progn
              (push (subseq text start pos) lines)
              (setf start (+ pos crlf-len)))
            (progn
              (when (< start len)
                (push (subseq text start) lines))
              (return)))))
    (nreverse lines)))

;;; ---------------------------------------------------------------------------
;;; Request line parsing
;;; ---------------------------------------------------------------------------

(defun parse-request-line (line)
  "Parse 'GET /path?query HTTP/1.1' into (values method path query version).
   Signals HTTP-PARSE-ERROR on any malformation."
  (when (zerop (length line))
    (http-parse-error "empty request line"))
  (when (> (length line) *max-request-line-length*)
    (http-parse-error "request line too long (~d bytes, max ~d)"
                      (length line) *max-request-line-length*))
  ;; Split on spaces — exactly two spaces expected
  (let* ((sp1 (position #\Space line))
         (sp2 (when sp1 (position #\Space line :start (1+ sp1)))))
    (unless (and sp1 sp2)
      (http-parse-error "malformed request line (expected METHOD SP URI SP VERSION): ~s"
                        (subseq line 0 (min (length line) 80))))
    ;; Reject extra spaces
    (when (position #\Space line :start (1+ sp2))
      (http-parse-error "malformed request line (extra spaces): ~s"
                        (subseq line 0 (min (length line) 80))))
    (let ((method-str (subseq line 0 sp1))
          (uri        (subseq line (1+ sp1) sp2))
          (ver-str    (subseq line (1+ sp2))))
      ;; Validate method
      (unless (member method-str *http-methods* :test #'string=)
        (http-parse-error "unrecognized method: ~s" method-str))
      ;; Validate version
      (unless (and (>= (length ver-str) 8)    ; "HTTP/X.Y"
                   (string= "HTTP/" ver-str :end2 5))
        (http-parse-error "malformed version: ~s" ver-str))
      (let ((version (subseq ver-str 5)))
        (unless (member version '("1.0" "1.1") :test #'string=)
          (http-parse-error "unsupported HTTP version: ~s" version))
        ;; Split URI into path and query
        (let* ((qmark (position #\? uri))
               (path  (if qmark (subseq uri 0 qmark) uri))
               (query (when qmark (subseq uri (1+ qmark)))))
          ;; Basic path validation
          (when (zerop (length path))
            (http-parse-error "empty request path"))
          (unless (char= (char path 0) #\/)
            (http-parse-error "request path must start with /: ~s" path))
          (values (intern method-str :keyword)
                  path
                  query
                  version))))))

;;; ---------------------------------------------------------------------------
;;; Header parsing
;;; ---------------------------------------------------------------------------

(defun parse-header-line (line)
  "Parse 'Header-Name: value' into (name . value).
   Name is lowercased, value is trimmed of leading/trailing whitespace."
  (when (> (length line) *max-header-line-length*)
    (http-parse-error "header line too long (~d bytes, max ~d)"
                      (length line) *max-header-line-length*))
  (let ((colon (position #\: line)))
    (unless colon
      (http-parse-error "malformed header (no colon): ~s"
                        (subseq line 0 (min (length line) 80))))
    (when (zerop colon)
      (http-parse-error "empty header name"))
    ;; Header name must not contain whitespace before colon (RFC 7230 §3.2.4)
    (let ((name-part (subseq line 0 colon)))
      (when (find-if (lambda (c) (or (char= c #\Space) (char= c #\Tab)))
                     name-part)
        (http-parse-error "whitespace in header name: ~s" name-part))
      (cons (string-downcase name-part)
            (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))))

(defun fold-continuation-lines (lines)
  "Handle obsolete header line folding (RFC 7230 §3.2.4).
   Lines starting with SP or HT are folded into the previous header."
  (let ((result '())
        (current nil))
    (dolist (line lines)
      (if (and current
               (> (length line) 0)
               (let ((ch (char line 0)))
                 (or (char= ch #\Space) (char= ch #\Tab))))
          ;; Continuation — fold into current
          (setf current (concatenate 'string current " "
                                     (string-trim '(#\Space #\Tab) line)))
          ;; New header
          (progn
            (when current (push current result))
            (setf current line))))
    (when current (push current result))
    (nreverse result)))

(defun parse-headers (header-text)
  "Parse the header block (everything between request line and body)
   into an alist of (lowercase-name . value) pairs."
  (when (zerop (length header-text))
    (return-from parse-headers nil))
  (let* ((raw-lines (crlf-split header-text))
         (folded    (fold-continuation-lines raw-lines))
         (headers   (mapcar #'parse-header-line folded)))
    (when (> (length headers) *max-header-count*)
      (http-parse-error "too many headers (~d, max ~d)"
                        (length headers) *max-header-count*))
    headers))

;;; ---------------------------------------------------------------------------
;;; Full request parsing
;;; ---------------------------------------------------------------------------

(defun find-header-end (data)
  "Find the position of the CRLFCRLF that terminates the header block.
   Returns the index of the first CR, or NIL if not found."
  (search *crlf-crlf* data))

(defun parse-request (raw-data)
  "Parse a raw HTTP request string into an HTTP-REQUEST struct.
   RAW-DATA must contain at least the complete headers (terminated by CRLFCRLF).
   Body is extracted based on Content-Length if present."
  (let ((header-end (find-header-end raw-data)))
    (unless header-end
      (http-parse-error "incomplete headers (no CRLFCRLF terminator)"))
    (let* ((header-section (subseq raw-data 0 header-end))
           (body-start     (+ header-end 4))   ; skip past CRLFCRLF
           ;; Split request line from header block
           (first-crlf (search *crlf* header-section))
           (request-line (if first-crlf
                             (subseq header-section 0 first-crlf)
                             header-section))
           (header-text (if (and first-crlf
                                 (< (+ first-crlf 2) (length header-section)))
                            (subseq header-section (+ first-crlf 2))
                            "")))
      ;; Parse the components
      (multiple-value-bind (method path query version)
          (parse-request-line request-line)
        (let* ((headers (parse-headers header-text))
               ;; Determine body
               (cl-value (cdr (assoc "content-length" headers :test #'string=)))
               (content-length (when cl-value
                                 (let ((n (parse-integer cl-value :junk-allowed t)))
                                   (unless n
                                     (http-parse-error "invalid Content-Length: ~s" cl-value))
                                   (when (< n 0)
                                     (http-parse-error "negative Content-Length: ~d" n))
                                   n)))
               (body (when (and content-length (> content-length 0))
                       (when (> content-length *max-body-size*)
                         (http-parse-error "body too large (~d bytes, max ~d)"
                                           content-length *max-body-size*))
                       (let ((available (- (length raw-data) body-start)))
                         (if (>= available content-length)
                             (subseq raw-data body-start
                                     (+ body-start content-length))
                             (http-parse-error
                              "incomplete body (got ~d bytes, Content-Length: ~d)"
                              available content-length))))))
          (make-http-request
           :method method
           :path path
           :query query
           :version version
           :headers headers
           :body body))))))

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
  "Serialize an HTTP-RESPONSE into a byte vector ready to write to a socket."
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
         ;; Build the header block
         (header-str
           (with-output-to-string (s)
             (format s "HTTP/1.1 ~d ~a~a" status reason *crlf*)
             (dolist (h headers)
               (format s "~a: ~a~a" (car h) (cdr h) *crlf*))
             (write-string *crlf* s))))
    (let ((header-bytes (sb-ext:string-to-octets header-str
                                                  :external-format :ascii)))
      (if body-bytes
          (let ((result (make-array (+ (length header-bytes) (length body-bytes))
                                    :element-type '(unsigned-byte 8))))
            (replace result header-bytes)
            (replace result body-bytes :start1 (length header-bytes))
            result)
          header-bytes))))

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
