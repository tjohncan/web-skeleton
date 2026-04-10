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
  (body     nil :type (or null (simple-array (unsigned-byte 8) (*))))) ; raw body bytes or nil

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

(defun get-cookie (request name)
  "Extract the value of cookie NAME from the request's Cookie header.
   Returns the value string, or NIL if not found.
   Scans in-place — one allocation for the return value only."
  (let ((header (get-header request "cookie")))
    (when header
      (let ((name-len (length name))
            (len (length header))
            (pos 0))
        (loop
          ;; Skip whitespace after ;
          (loop while (and (< pos len)
                           (or (char= (char header pos) #\Space)
                               (char= (char header pos) #\Tab)))
                do (incf pos))
          (when (>= pos len) (return nil))
          ;; Check for name=
          (when (and (<= (+ pos name-len 1) len)
                     (string= header name :start1 pos :end1 (+ pos name-len))
                     (char= (char header (+ pos name-len)) #\=))
            (let* ((val-start (+ pos name-len 1))
                   (val-end (or (position #\; header :start val-start) len)))
              (return (subseq header val-start val-end))))
          ;; Skip to next pair
          (let ((semi (position #\; header :start pos)))
            (unless semi (return nil))
            (setf pos (1+ semi))))))))

;;; ---------------------------------------------------------------------------
;;; URL percent-decoding (RFC 3986)
;;; ---------------------------------------------------------------------------

(defun url-decode (string)
  "Decode percent-encoded characters in STRING (RFC 3986).
   %XX sequences are replaced with the corresponding byte, decoded as UTF-8.
   '+' is passed through literally (this is path decoding, not form decoding)."
  (let* ((bytes (sb-ext:string-to-octets string :external-format :ascii))
         (len (length bytes))
         (out (make-array len :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop with i = 0
          while (< i len)
          do (let ((b (aref bytes i)))
               (if (and (= b 37)              ; '%'
                        (< (+ i 2) len))
                   (let ((hi (hex-digit-value (aref bytes (+ i 1))))
                         (lo (hex-digit-value (aref bytes (+ i 2)))))
                     (if (and hi lo)
                         (progn
                           (vector-push (logior (ash hi 4) lo) out)
                           (incf i 3))
                         (progn
                           (vector-push b out)
                           (incf i))))
                   (progn
                     (vector-push b out)
                     (incf i)))))
    (handler-case
        (sb-ext:octets-to-string (subseq out 0 (fill-pointer out))
                                  :external-format :utf-8)
      (sb-int:character-decoding-error ()
        (http-parse-error "invalid UTF-8 in percent-decoded value")))))

;;; ---------------------------------------------------------------------------
;;; Query string parsing
;;; ---------------------------------------------------------------------------

(defun parse-query-string (query)
  "Parse a query string like \"a=1&b=2\" into an alist.
   Percent-decodes both names and values.
   Keys with no = get empty string values."
  (when (and query (> (length query) 0))
    (let ((pairs nil)
          (len (length query))
          (start 0))
      (loop
        (let* ((amp (or (position #\& query :start start) len))
               (segment-start start)
               (eq-pos (position #\= query :start segment-start :end amp)))
          (push (if eq-pos
                    (cons (url-decode (subseq query segment-start eq-pos))
                          (url-decode (subseq query (1+ eq-pos) amp)))
                    (cons (url-decode (subseq query segment-start amp)) ""))
                pairs)
          (if (>= amp len)
              (return (nreverse pairs))
              (setf start (1+ amp))))))))

(defun get-query-param (request name)
  "Look up query parameter NAME from the request's query string.
   Returns the decoded value string, or NIL if not present.
   Reparses the query string on each call; cache the result if calling repeatedly."
  (cdr (assoc name (parse-query-string (http-request-query request))
              :test #'string=)))

;;; ---------------------------------------------------------------------------
;;; Path matching
;;; ---------------------------------------------------------------------------

(defun split-path-segments (path)
  "Split \"/users/42\" into (\"users\" \"42\"). Leading slash is consumed."
  (let ((segments nil)
        (start (if (and (> (length path) 0) (char= (char path 0) #\/)) 1 0))
        (len (length path)))
    (loop
      (let ((slash (position #\/ path :start start)))
        (push (subseq path start (or slash len)) segments)
        (if slash
            (setf start (1+ slash))
            (return (nreverse segments)))))))

(defun match-path (pattern path)
  "Match PATH against PATTERN with :param segment captures.
   Literal segments must match exactly; segments starting with : capture.
   Empty segments are not captured (e.g. /users/ does not match /users/:id).
   Returns NIL if no match, T if match with no captures,
   or an alist of (name . decoded-value) pairs if match with captures.
   Captured values are percent-decoded."
  (let ((pat-segs  (split-path-segments pattern))
        (path-segs (split-path-segments path))
        (bindings nil))
    (when (= (length pat-segs) (length path-segs))
      (when (loop for pat in pat-segs
                  for seg in path-segs
                  always (if (and (> (length pat) 0)
                                  (char= (char pat 0) #\:))
                             (when (> (length seg) 0)
                               (push (cons (subseq pat 1) (url-decode seg))
                                     bindings)
                               t)
                             (string= pat seg)))
        (if bindings (nreverse bindings) t)))))

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
                ;; RFC 7230 §3.2.4: reject obsolete line folding
                (http-parse-error "obsolete line folding not accepted")
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
                      (incf count)
                      (when (> count *max-header-count*)
                        (http-parse-error "too many headers (~d, max ~d)"
                                          count *max-header-count*))))))))
        (setf pos (+ crlf 2))))
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
    (206 . "Partial Content")
    (301 . "Moved Permanently")
    (302 . "Found")
    (304 . "Not Modified")
    (307 . "Temporary Redirect")
    (308 . "Permanent Redirect")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (408 . "Request Timeout")
    (409 . "Conflict")
    (413 . "Payload Too Large")
    (414 . "URI Too Long")
    (429 . "Too Many Requests")
    (431 . "Request Header Fields Too Large")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Timeout"))
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
;;; HTTP message serialization (shared by response builder, static, and fetch)
;;; ---------------------------------------------------------------------------

(defun serialize-http-message (first-line headers body-bytes)
  "Serialize an HTTP message into a byte vector ready to write to a socket.
   FIRST-LINE: status line or request line string (without CRLF).
   HEADERS: alist of (name . value) string pairs.
   BODY-BYTES: byte vector or NIL.
   Single pre-sized buffer — no intermediate allocations."
  (let* ((header-size (+ (length first-line) 2   ; first line + CRLF
                         (loop for (name . value) in headers
                               sum (+ (length name) 2 (length value) 2))
                         2))                      ; final CRLF
         (body-len (if body-bytes (length body-bytes) 0))
         (buf (make-array (+ header-size body-len)
                          :element-type '(unsigned-byte 8)))
         (pos 0))
    (flet ((put-ascii (str)
             (loop for i from 0 below (length str)
                   do (setf (aref buf pos) (char-code (char str i)))
                      (incf pos)))
           (put-crlf ()
             (setf (aref buf pos) 13 (aref buf (1+ pos)) 10)
             (incf pos 2)))
      ;; Reject CRLF in request/status line (prevents request line injection)
      (when (or (find #\Return first-line) (find #\Newline first-line))
        (error "HTTP message line contains CRLF"))
      (put-ascii first-line)
      (put-crlf)
      (dolist (h headers)
        (let ((name (car h))
              (value (cdr h)))
          ;; Reject CRLF in header names and values (prevents injection)
          (when (or (find #\Return name) (find #\Newline name))
            (error "HTTP header name contains CRLF: ~a" name))
          (when (or (find #\Return value) (find #\Newline value))
            (error "HTTP header value contains CRLF: ~a" name))
          (put-ascii name)
          (setf (aref buf pos) 58 (aref buf (1+ pos)) 32)
          (incf pos 2)
          (put-ascii value)
          (put-crlf)))
      (put-crlf)
      (when body-bytes
        (replace buf body-bytes :start1 pos)))
    buf))

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

(defun http-date (&optional (universal-time (get-universal-time)))
  "Return UTC time in RFC 7231 IMF-fixdate format.
   Defaults to current time if no argument given."
  (multiple-value-bind (sec min hour day month year dow)
      (decode-universal-time universal-time 0)
    (format nil "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d GMT"
            (nth dow '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            day
            (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                              "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            year hour min sec)))

(defun format-response (response)
  "Serialize an HTTP-RESPONSE into a byte vector ready to write to a socket."
  (let* ((status (http-response-status response))
         (body   (http-response-body response))
         (body-bytes (when body
                       (sb-ext:string-to-octets body :external-format :utf-8)))
         (headers (http-response-headers response))
         (headers (if (and body-bytes
                           (not (assoc "content-length" headers :test #'string=)))
                      (cons (cons "content-length"
                                  (write-to-string (length body-bytes)))
                            headers)
                      headers))
         ;; RFC 7231 §7.1.1.2: origin server MUST send Date
         (headers (if (assoc "date" headers :test #'string=)
                      headers
                      (cons (cons "date" (http-date)) headers))))
    (serialize-http-message
     (format nil "HTTP/1.1 ~d ~a" status (status-reason status))
     headers body-bytes)))

;;; ---------------------------------------------------------------------------
;;; Convenience constructors
;;; ---------------------------------------------------------------------------

(defun make-text-response (status body &key (content-type "text/plain; charset=utf-8"))
  "Build a response with a text body."
  (let ((resp (make-http-response :status status :body body)))
    (set-response-header resp "content-type" content-type)
    resp))

(defun make-html-response (status body)
  "Build a response with an HTML body."
  (make-text-response status body :content-type "text/html; charset=utf-8"))

(defun make-error-response (status &optional message)
  "Build a plain-text error response."
  (let ((body (or message
                  (format nil "~d ~a" status (status-reason status)))))
    (make-text-response status body)))
