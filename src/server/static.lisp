(in-package :web-skeleton)

;;; ===========================================================================
;;; Static File Serving
;;;
;;; Loads files from a directory into an in-memory cache at startup.
;;; Each file is pre-formatted as a complete HTTP response (status line,
;;; headers, body) so serving requires zero per-request work — just a
;;; hash table lookup and a buffer pointer.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; MIME type detection
;;; ---------------------------------------------------------------------------

(defun file-extension (path)
  "Extract the file extension from PATH (without the dot), or NIL."
  (let ((dot (position #\. path :from-end t))
        (slash (position #\/ path :from-end t)))
    (when (and dot (or (null slash) (> dot slash)))
      (subseq path (1+ dot)))))

(defun mime-type-for-path (path)
  "Return the MIME type for a file path based on its extension."
  (let ((ext (file-extension path)))
    (if (null ext)
        "application/octet-stream"
        (let ((e (string-downcase ext)))
          (cond
            ;; Text
            ((or (string= e "html") (string= e "htm"))
             "text/html; charset=utf-8")
            ((string= e "css")  "text/css; charset=utf-8")
            ((string= e "js")   "application/javascript; charset=utf-8")
            ((string= e "json") "application/json; charset=utf-8")
            ((string= e "txt")  "text/plain; charset=utf-8")
            ((string= e "xml")  "application/xml; charset=utf-8")
            ((string= e "webmanifest") "application/manifest+json")
            ;; Images
            ((string= e "png")  "image/png")
            ((or (string= e "jpg") (string= e "jpeg")) "image/jpeg")
            ((string= e "gif")  "image/gif")
            ((string= e "svg")  "image/svg+xml")
            ((string= e "ico")  "image/x-icon")
            ((string= e "webp") "image/webp")
            ;; Fonts
            ((string= e "woff")  "font/woff")
            ((string= e "woff2") "font/woff2")
            ((string= e "ttf")   "font/ttf")
            ((string= e "otf")   "font/otf")
            ;; Default
            (t "application/octet-stream"))))))

;;; ---------------------------------------------------------------------------
;;; Static file cache
;;; ---------------------------------------------------------------------------

(defstruct static-entry
  "Cached static file with pre-built GET and HEAD responses."
  (get-response  nil :type (simple-array (unsigned-byte 8) (*)))
  (head-response nil :type (simple-array (unsigned-byte 8) (*))))

(defvar *static-cache* (make-hash-table :test #'equal)
  "Maps URL path (string) to STATIC-ENTRY struct.
   Populated at startup by load-static-files, read-only at request time.")

;;; ---------------------------------------------------------------------------
;;; Pre-build complete HTTP response bytes
;;; ---------------------------------------------------------------------------

(defun build-static-response (mime-type content)
  "Build pre-built GET and HEAD response byte vectors for a static file.
   CONTENT is a byte vector of the file data.
   Returns a STATIC-ENTRY with both responses ready to write."
  (let ((headers (list (cons "content-type" mime-type)
                       (cons "content-length" (write-to-string (length content)))
                       (cons "cache-control" "public, max-age=3600")
                       (cons "x-content-type-options" "nosniff"))))
    (make-static-entry
     :get-response  (serialize-http-message "HTTP/1.1 200 OK" headers content)
     :head-response (serialize-http-message "HTTP/1.1 200 OK" headers nil))))

;;; ---------------------------------------------------------------------------
;;; File I/O (startup only)
;;; ---------------------------------------------------------------------------

(defun read-file-bytes (path)
  "Read an entire file into a byte vector."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
           (buf (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buf stream)
      buf)))

;;; ---------------------------------------------------------------------------
;;; Directory scanning and cache loading
;;; ---------------------------------------------------------------------------

(defun load-static-files (directory)
  "Load all files under DIRECTORY into the static file cache.
   Files are read into memory and pre-formatted as complete HTTP responses.
   URL paths are derived by stripping DIRECTORY from the file path.
   Additive — can be called multiple times. Collisions: last wins."
  (let ((dir-path (probe-file (pathname directory))))
    (unless dir-path
      (log-warn "static: directory not found: ~a" directory)
      (return-from load-static-files))
    (let ((base (namestring dir-path)))
      ;; Ensure trailing /
      (unless (char= (char base (1- (length base))) #\/)
        (setf base (concatenate 'string base "/")))
      (let ((wild (merge-pathnames
                   (make-pathname :directory '(:relative :wild-inferiors)
                                  :name :wild :type :wild)
                   base))
            (count 0)
            (total-bytes 0))
        ;; First pass: load all files (skip directories)
        (dolist (file (directory wild))
          (when (pathname-name file)
            (let* ((fs-path (namestring file))
                   (relative (subseq fs-path (length base)))
                   (url-path (concatenate 'string "/" relative))
                   (content (read-file-bytes fs-path))
                   (mime (mime-type-for-path url-path))
                   (response (build-static-response mime content)))
              (when (gethash url-path *static-cache*)
                (log-debug "static: replacing ~a" url-path))
              (setf (gethash url-path *static-cache*) response)
              (incf count)
              (incf total-bytes (length content))
              (log-debug "static: ~a (~a, ~d bytes)" url-path mime (length content)))))
        ;; Second pass: register extensionless aliases for .html files
        ;; e.g., /login.html also serves at /login
        ;; Actual files take priority — don't overwrite existing entries
        (let ((aliases 0))
          (maphash (lambda (url-path response)
                     (let ((len (length url-path)))
                       (when (and (> len 5)
                                  (string= url-path ".html"
                                           :start1 (- len 5)))
                         (let ((alias (subseq url-path 0 (- len 5))))
                           (unless (gethash alias *static-cache*)
                             (setf (gethash alias *static-cache*) response)
                             (incf aliases)
                             (log-debug "static: alias ~a -> ~a"
                                        alias url-path))))))
                   *static-cache*)
          (log-info "loaded ~d static file~:p (~:d bytes cached, ~d ~a)"
                    count total-bytes aliases
                    (if (= aliases 1) "alias" "aliases")))))))

;;; ---------------------------------------------------------------------------
;;; Request serving
;;; ---------------------------------------------------------------------------

(defun serve-static (request)
  "Look up the request path in the static file cache.
   Returns a pre-built HTTP response byte vector ready to write,
   or NIL if the path is not cached.
   Serves GET and HEAD requests. Rejects paths containing '..'.
   Directories (paths ending in /) try index.html as a fallback."
  (let ((method (http-request-method request)))
    (when (or (eq method :GET) (eq method :HEAD))
      (let ((path (http-request-path request)))
        (unless (or (search "/../" path)
                    (and (>= (length path) 3)
                         (string= path "/.." :start1 (- (length path) 3))))
          (let ((entry (or (gethash path *static-cache*)
                           ;; Try index.html for directory paths
                           (when (char= (char path (1- (length path))) #\/)
                             (gethash (concatenate 'string path "index.html")
                                      *static-cache*)))))
            (when entry
              (if (eq method :GET)
                  (static-entry-get-response entry)
                  (static-entry-head-response entry)))))))))
