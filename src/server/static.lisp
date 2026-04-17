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
  "Extract the file extension from PATH (without the dot), or NIL.
   A dotfile like '/.hidden' has no extension — its leading '.' is
   part of the name, not a separator. Loading dotfiles is refused
   elsewhere, but this keeps the helper's contract clean for any
   future caller."
  (let ((dot (position #\. path :from-end t))
        (slash (position #\/ path :from-end t)))
    (when (and dot (or (null slash) (> dot (1+ slash))))
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
            ((string= e "webmanifest") "application/manifest+json; charset=utf-8")
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
  "Cached static file with pre-built responses. Three byte vectors:
   the 200 GET (headers + body), the 200 HEAD (same headers, body
   suppressed), and the 304 Not Modified (etag + cache-control only,
   no body). ETAG is the content's SHA-256 hex wrapped in a quoted
   string — a strong entity tag per RFC 7232 §2.3."
  (get-response          nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (head-response         nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (not-modified-response nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (etag                  nil :type (or null string))
  (last-modified         ""  :type string))

(defvar *static-cache* (make-hash-table :test #'equal)
  "Maps URL path (string) to STATIC-ENTRY struct.
   Populated at startup by load-static-files, read-only at request time.
   Not thread-safe — call load-static-files before start-server only.")

;;; ---------------------------------------------------------------------------
;;; Pre-build complete HTTP response bytes
;;; ---------------------------------------------------------------------------

(defun build-static-response (mime-type content file-mtime)
  "Build pre-built 200 GET, 200 HEAD, and 304 Not Modified response
   byte vectors for a static file. CONTENT is the file's bytes;
   FILE-MTIME is its modification time (universal-time). The ETag
   is a strong entity tag derived from the content's SHA-256 — so
   revalidation via If-None-Match happens automatically the moment
   LOAD-STATIC-FILES runs. Returns a STATIC-ENTRY."
  (let* ((etag (format nil "\"~a\"" (sha256-hex content)))
         (full-headers
          (list (cons "content-type" mime-type)
                (cons "content-length" (write-to-string (length content)))
                (cons "etag" etag)
                (cons "cache-control" "public, max-age=3600")
                (cons "x-content-type-options" "nosniff")
                ;; Date omitted (violates RFC 7231 §7.1.1.2 MUST).
                ;; Pre-built responses cannot carry a per-request timestamp,
                ;; and a stale Date would defeat max-age for caching proxies.
                ;; Omission is the least-harmful option: caches fall back to
                ;; received time (RFC 7234 §4.2.3), preserving correct freshness.
                (cons "last-modified" (http-date file-mtime))))
         ;; 304 responses carry validator and cache headers (ETag,
         ;; Cache-Control, Last-Modified). No Content-Type / Content-Length / body.
         (not-modified-headers
          (list (cons "etag" etag)
                (cons "cache-control" "public, max-age=3600")
                (cons "last-modified" (http-date file-mtime)))))
    (make-static-entry
     :get-response  (serialize-http-message "HTTP/1.1 200 OK"
                                            full-headers content)
     :head-response (serialize-http-message "HTTP/1.1 200 OK"
                                            full-headers nil)
     :not-modified-response (serialize-http-message
                             "HTTP/1.1 304 Not Modified"
                             not-modified-headers nil)
     :etag etag
     :last-modified (http-date file-mtime))))

;;; ---------------------------------------------------------------------------
;;; File I/O (startup only)
;;; ---------------------------------------------------------------------------

(defun read-file-bytes (path)
  "Read an entire regular file into a byte vector.
   READ-SEQUENCE on an SBCL binary file stream fills the buffer in
   one call for regular files, and LOAD-STATIC-FILES only ever reads
   regular files — special streams (pipes, devices) with partial
   availability are out of scope for this helper. The SUBSEQ branch
   handles the edge case where the file shrinks between FILE-LENGTH
   and READ-SEQUENCE."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
           (buf  (make-array size :element-type '(unsigned-byte 8)))
           (n    (read-sequence buf stream)))
      (if (= n size) buf (subseq buf 0 n)))))

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
          (when (and (pathname-name file)
                     (> (length (pathname-name file)) 0)
                     (not (char= (char (pathname-name file) 0) #\.)))
            (let ((fs-path (namestring file)))
              ;; SBCL's DIRECTORY resolves symlinks by default — a symlink
              ;; inside the tree that points outside (demo/static/leak ->
              ;; /etc/passwd) returns the resolved target path, which
              ;; does not prefix-match BASE. A blind (subseq fs-path
              ;; (length base)) on that path either raises on short
              ;; targets or produces a bogus URL that silently serves
              ;; arbitrary filesystem bytes. Skip anything not under BASE.
              (cond
                ((not (and (>= (length fs-path) (length base))
                           (string= fs-path base :end1 (length base))))
                 (log-warn "static: skipping out-of-tree path ~a" fs-path))
                (t
                 (let ((relative (subseq fs-path (length base))))
                   ;; Skip files inside dot-directories (.git/, .secret/, etc.)
                   ;; Search url-path (not relative) to catch root-level dot-dirs
                   (unless (search "/." (concatenate 'string "/" relative))
                     (let* ((url-path (concatenate 'string "/" relative))
                            (content (read-file-bytes fs-path))
                            (mime (mime-type-for-path url-path))
                            (mtime (file-write-date file))
                            (response (build-static-response mime content mtime)))
                       (when (gethash url-path *static-cache*)
                         (log-debug "static: replacing ~a" url-path))
                       (setf (gethash url-path *static-cache*) response)
                       (incf count)
                       (incf total-bytes (length content))
                       (log-debug "static: ~a (~a, ~d bytes)" url-path mime (length content))))))))))
        ;; Second pass: register extensionless aliases for .html files
        ;; e.g., /login.html also serves at /login
        ;; Actual files take priority — don't overwrite existing entries
        (let ((aliases 0)
              (pending nil))
          ;; Collect aliases first — modifying a hash table during maphash
          ;; is undefined per the CL spec
          (maphash (lambda (url-path response)
                     (let ((len (length url-path)))
                       (when (and (> len 5)
                                  (string= url-path ".html"
                                           :start1 (- len 5)))
                         (let ((alias (subseq url-path 0 (- len 5))))
                           (unless (gethash alias *static-cache*)
                             (push (cons alias response) pending))))))
                   *static-cache*)
          (dolist (entry pending)
            (setf (gethash (car entry) *static-cache*) (cdr entry))
            (incf aliases)
            (log-debug "static: alias ~a" (car entry)))
          (log-info "loaded ~d static file~:p (~:d bytes cached, ~d ~a)"
                    count total-bytes aliases
                    (if (= aliases 1) "alias" "aliases")))))))

;;; ---------------------------------------------------------------------------
;;; Request serving
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; If-None-Match parsing (RFC 7232 §3.2)
;;;
;;; The client's header value may be:
;;;   - Wildcard '*' (matches whenever the resource exists)
;;;   - One or more comma-separated entity tags
;;;   - Each tag may carry an optional 'W/' weak-validator prefix
;;; Weak comparison is used for If-None-Match: a weak and a strong tag
;;; with the same opaque body match each other. We normalize by
;;; stripping W/ before comparing, which is equivalent for our case
;;; (we only ever generate strong tags, so the opaque bodies are the
;;; only thing that can differ).
;;; ---------------------------------------------------------------------------

(defun if-none-match-hit-p (header-value our-etag)
  "Return T when HEADER-VALUE (an If-None-Match header value) indicates
   the client already has OUR-ETAG. Handles wildcards, comma-separated
   lists, and the W/ weak prefix per RFC 7232 §3.2. Returns NIL on
   missing inputs or any parse miss."
  (unless (and header-value our-etag (> (length header-value) 0))
    (return-from if-none-match-hit-p nil))
  ;; RFC 7232 §3.2: wildcard is "*" as the entire field-value,
  ;; not mixed with entity-tags. Check upfront before the list walk.
  (when (string= (string-trim '(#\Space #\Tab) header-value) "*")
    (return-from if-none-match-hit-p t))
  (let ((len (length header-value))
        (pos 0))
    (loop
      ;; Skip leading whitespace
      (loop while (and (< pos len)
                       (or (char= (char header-value pos) #\Space)
                           (char= (char header-value pos) #\Tab)))
            do (incf pos))
      (when (>= pos len) (return nil))
      ;; Next comma marks the end of this entry.
      (let ((end (or (position #\, header-value :start pos) len))
            (start pos))
        ;; Trim trailing OWS.
        (loop while (and (> end start)
                         (or (char= (char header-value (1- end)) #\Space)
                             (char= (char header-value (1- end)) #\Tab)))
              do (decf end))
        ;; Strip optional W/ weak prefix — our own ETags are strong,
        ;; so weak comparison reduces to 'same opaque body'.
        (when (and (>= (- end start) 2)
                   (char= (char header-value start) #\W)
                   (char= (char header-value (1+ start)) #\/))
          (incf start 2))
        (when (and (> end start)
                   (= (- end start) (length our-etag))
                   (string= header-value our-etag
                            :start1 start :end1 end))
          (return t))
        (when (>= end len) (return nil))
        (setf pos (1+ end))))))

;;; ---------------------------------------------------------------------------
;;; Request serving
;;; ---------------------------------------------------------------------------

(defun serve-static (request)
  "Look up the request path in the static file cache.
   Returns a pre-built HTTP response byte vector ready to write, or
   NIL if the path is not cached. Serves GET and HEAD requests.
   Honors If-None-Match (RFC 7232) — a cached ETag match returns the
   pre-built 304 Not Modified response instead of the full body.
   Rejects paths containing '..'. Directories (paths ending in /)
   try index.html as a fallback."
  (let ((method (http-request-method request)))
    (when (or (eq method :GET) (eq method :HEAD))
      ;; Decode percent-encoding so /foo%20bar hits a cached /foo bar
      ;; and %2e%2e cannot slip past the traversal check below. The
      ;; cache is keyed on decoded filesystem paths, so this is the
      ;; form that can actually match. Malformed encoding → NIL, which
      ;; falls through to the app handler for a clean 404.
      (let ((path (handler-case (url-decode (http-request-path request))
                    (error () nil))))
        (when (and path (> (length path) 0))
          (unless (or (search "/../" path)
                      (and (>= (length path) 3)
                           (string= path "/.." :start1 (- (length path) 3))))
            (let ((entry (or (gethash path *static-cache*)
                             ;; Try index.html for directory paths
                             (when (char= (char path (1- (length path))) #\/)
                               (gethash (concatenate 'string path "index.html")
                                        *static-cache*)))))
              (when entry
                (cond
                  ((if-none-match-hit-p (get-header request "if-none-match")
                                        (static-entry-etag entry))
                   (static-entry-not-modified-response entry))
                  ;; If-Modified-Since: exact string match against our
                  ;; stored Last-Modified value (no date arithmetic).
                  ;; RFC 7232 §3.3: MUST ignore IMS when INM is present.
                  ((and (null (get-header request "if-none-match"))
                        (let ((ims (get-header request "if-modified-since")))
                          (and ims
                               (string= ims (static-entry-last-modified entry)))))
                   (static-entry-not-modified-response entry))
                  ((eq method :GET)
                   (static-entry-get-response entry))
                  (t
                   (static-entry-head-response entry)))))))))))
