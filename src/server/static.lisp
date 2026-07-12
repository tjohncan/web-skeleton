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
            ((string= e "map")  "application/json; charset=utf-8") ; source maps
            ;; Images
            ((string= e "png")  "image/png")
            ((or (string= e "jpg") (string= e "jpeg")) "image/jpeg")
            ((string= e "gif")  "image/gif")
            ((string= e "svg")  "image/svg+xml")
            ((string= e "ico")  "image/x-icon")
            ((string= e "webp") "image/webp")
            ((string= e "avif") "image/avif")
            ;; Fonts
            ((string= e "woff")  "font/woff")
            ((string= e "woff2") "font/woff2")
            ((string= e "ttf")   "font/ttf")
            ((string= e "otf")   "font/otf")
            ;; Media / documents
            ((string= e "mp4")  "video/mp4")
            ((string= e "webm") "video/webm")
            ((string= e "pdf")  "application/pdf")
            ;; WebAssembly — must be exactly application/wasm: browsers
            ;; refuse WebAssembly.instantiateStreaming() /
            ;; compileStreaming() on any other Content-Type.
            ((string= e "wasm") "application/wasm")
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
   string — a strong entity tag per RFC 7232 §2.3.

   HEADERS, BODY-OFFSET and CONTENT-LENGTH exist for Range requests
   (RFC 7233). A 206 needs a fresh header set — a different
   Content-Length plus a Content-Range — so the 200's header alist is
   kept to build from. BODY-OFFSET is where the body starts inside
   GET-RESPONSE, which lets a range be sliced straight out of the
   pre-built 200: the file is stored once, not twice. It is exactly the
   length of HEAD-RESPONSE, since that is the same headers with the body
   suppressed."
  (get-response          nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (head-response         nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (not-modified-response nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (etag                  nil :type (or null string))
  (last-modified         ""  :type string)
  (headers               nil :type list)
  (body-offset             0 :type fixnum)
  (content-length          0 :type fixnum))

(defvar *static-cache* (make-hash-table :test #'equal)
  "Maps URL path (string) to STATIC-ENTRY struct.
   Populated at startup by load-static-files, read-only at request time.
   Not thread-safe — call load-static-files before start-server only.")

;;; ---------------------------------------------------------------------------
;;; Pre-build complete HTTP response bytes
;;; ---------------------------------------------------------------------------

(defun build-static-response (mime-type content file-mtime
                              &optional (cache-control "public, max-age=3600"))
  "Build pre-built 200 GET, 200 HEAD, and 304 Not Modified response
   byte vectors for a static file. CONTENT is the file's bytes;
   FILE-MTIME is its modification time (universal-time). CACHE-CONTROL
   is the Cache-Control header value — a string, defaulting to
   'public, max-age=3600'. LOAD-STATIC-FILES resolves its :CACHE-CONTROL
   argument (string or function of URL path) before calling here.
   The ETag is a strong entity tag derived from the content's
   SHA-256 — so revalidation via If-None-Match happens automatically
   the moment LOAD-STATIC-FILES runs. Returns a STATIC-ENTRY."
  (let* ((etag (format nil "\"~a\"" (sha256-hex content)))
         (full-headers
          (list (cons "content-type" mime-type)
                (cons "content-length" (write-to-string (length content)))
                (cons "etag" etag)
                (cons "cache-control" cache-control)
                (cons "x-content-type-options" "nosniff")
                ;; Advertise Range support (RFC 7233 §2.3). Without this
                ;; a client has no way to know a byte range is worth
                ;; asking for — media players in particular check it
                ;; before attempting to seek.
                (cons "accept-ranges" "bytes")
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
                (cons "cache-control" cache-control)
                (cons "last-modified" (http-date file-mtime))))
         ;; The HEAD response is the same headers with no body, so its
         ;; length is precisely where the body begins inside the GET
         ;; response. That identity is what lets RANGE-RESPONSE slice a
         ;; byte range out of the pre-built 200 instead of keeping a
         ;; second copy of the file.
         (head-bytes (serialize-http-message "HTTP/1.1 200 OK"
                                             full-headers nil)))
    (make-static-entry
     :get-response  (serialize-http-message "HTTP/1.1 200 OK"
                                            full-headers content)
     :head-response head-bytes
     :not-modified-response (serialize-http-message
                             "HTTP/1.1 304 Not Modified"
                             not-modified-headers nil)
     :headers full-headers
     :body-offset (length head-bytes)
     :content-length (length content)
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
;;; Substitutions (optional literal-string rewrites at load time)
;;;
;;; Lets a deployer inject values (titles, API bases, build ids) into
;;; static files without a template engine. The caller supplies the
;;; literal string to match and the bytes to emit — the framework
;;; never invents a delimiter.
;;; ---------------------------------------------------------------------------

(defun validate-and-normalize-substitutions (subs)
  "Parse and validate the :SUBSTITUTIONS argument to LOAD-STATIC-FILES.
   Returns a hash table keyed on URL path (leading /) whose values are
   alists of (literal . replacement) in caller order. Signals ERROR on
   malformed input, empty keys or patterns, or duplicates."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry subs)
      (unless (and (consp entry) (stringp (first entry)))
        (error "substitutions: malformed entry ~s (expected (\"file\" (\"pat\" . \"rep\") ...))"
               entry))
      (let* ((raw-key (first entry))
             (rules   (rest entry)))
        (when (zerop (length raw-key))
          (error "substitutions: empty file key"))
        (let ((key (if (char= (char raw-key 0) #\/)
                       raw-key
                       (concatenate 'string "/" raw-key))))
          (when (gethash key table)
            (error "substitutions: duplicate file key ~s" raw-key))
          (let ((seen (make-hash-table :test #'equal)))
            (dolist (rule rules)
              (unless (and (consp rule)
                           (stringp (car rule))
                           (stringp (cdr rule)))
                (error "substitutions ~s: malformed rule ~s (expected (\"pat\" . \"rep\"))"
                       raw-key rule))
              (when (zerop (length (car rule)))
                (error "substitutions ~s: empty pattern" raw-key))
              (when (gethash (car rule) seen)
                (error "substitutions ~s: duplicate pattern ~s" raw-key (car rule)))
              (setf (gethash (car rule) seen) t)))
          (setf (gethash key table) rules))))
    table))

(defun apply-substitutions (content rules)
  "Apply RULES to CONTENT byte vector in a single left-to-right pass.
   At each position, RULES are tried in caller order; the first match
   wins, its replacement bytes are emitted, and the scan advances past
   the matched span. Replacement bytes are never re-scanned, so A->B /
   B->A cycles are impossible by construction. Returns a fresh
   (simple-array (unsigned-byte 8)) when any rule fires, or CONTENT
   unchanged when nothing matches. Patterns and replacements are
   UTF-8 encoded before matching against CONTENT."
  (when (null rules)
    (return-from apply-substitutions content))
  (let* ((byte-rules
          (mapcar (lambda (r)
                    (cons (sb-ext:string-to-octets (car r) :external-format :utf-8)
                          (sb-ext:string-to-octets (cdr r) :external-format :utf-8)))
                  rules))
         (len (length content))
         (out (make-array (max 16 len)
                          :element-type '(unsigned-byte 8)
                          :fill-pointer 0
                          :adjustable t))
         (pos 0)
         (any-match nil))
    (loop while (< pos len)
          do (let ((matched nil))
               (dolist (br byte-rules)
                 (let* ((pat (car br))
                        (plen (length pat)))
                   (when (and (<= (+ pos plen) len)
                              (not (mismatch content pat
                                             :start1 pos :end1 (+ pos plen))))
                     (loop for b across (cdr br)
                           do (vector-push-extend b out))
                     (incf pos plen)
                     (setf matched t any-match t)
                     (return))))
               (unless matched
                 (vector-push-extend (aref content pos) out)
                 (incf pos))))
    (if any-match
        (let ((result (make-array (length out) :element-type '(unsigned-byte 8))))
          (replace result out)
          result)
        content)))

;;; ---------------------------------------------------------------------------
;;; Directory scanning and cache loading
;;; ---------------------------------------------------------------------------

(defun hidden-path-component-p (relative)
  "T when any /-separated component of RELATIVE starts with a dot —
   except the exact component .well-known. RFC 8615 reserves
   /.well-known/ as a public namespace that a web server must be able
   to serve (ACME HTTP-01 challenges, security.txt, assetlinks.json,
   apple-app-site-association), so the hide-dot-paths rule exempts
   that one name. Dot-FILES keep answering T through their own
   component — .well-known/.hidden stays hidden."
  (let ((start 0)
        (len (length relative)))
    (loop
      (let* ((slash (position #\/ relative :start start))
             (end (or slash len)))
        (when (and (< start end)
                   (char= (char relative start) #\.)
                   (not (string= relative ".well-known"
                                 :start1 start :end1 end)))
          (return t))
        (unless slash (return nil))
        (setf start (1+ slash))))))

(defun load-static-files (directory &key (cache-control "public, max-age=3600")
                                         substitutions)
  "Load all files under DIRECTORY into the static file cache.
   Files are read into memory and pre-formatted as complete HTTP responses.
   URL paths are derived by stripping DIRECTORY from the file path.
   Additive — can be called multiple times. Collisions: last wins.
   Dotfiles and dot-directories are skipped (.git/, .env) — except
   the RFC 8615 /.well-known/ namespace, which is served.

   CACHE-CONTROL is either a string (used for every file) or a
   function of one argument (the URL path) that returns a string.
   Defaults to 'public, max-age=3600'. The function form lets apps
   vary the header by path — e.g. long max-age + immutable for
   fingerprinted bundle filenames, a short max-age for index.html
   so SPA deploys become visible promptly:

     (load-static-files
       \"static/\"
       :cache-control
       (lambda (url-path)
         (if (search \".immutable.\" url-path)
             \"public, max-age=31536000, immutable\"
             \"public, max-age=60\")))

   The resolved string is validated by SERIALIZE-HTTP-MESSAGE on
   the first request serving, so a value containing CR / LF / NUL
   raises a pointed error at response-build time rather than going
   on the wire.

   SUBSTITUTIONS is an optional list of per-file literal-string rewrites
   applied at load time, before the ETag is computed:

     (load-static-files
       \"static/\"
       :substitutions
       '((\"index.html\" (\"__TITLE__\"   . \"My Great Page\")
                       (\"__TAGLINE__\" . \"A fun page to peruse!\"))
         (\"app.js\"    (\"__API_BASE__\" . \"/api/v1\"))))

   Each entry is (url-path rule ...) where each rule is
   (literal . replacement). Leading slash on the file key is optional.
   A key that matches no loaded file signals ERROR — typos fail at
   deploy time rather than serving unsubstituted bytes. Escaping for
   the target format (HTML, JS, CSS) is the caller's responsibility."
  (let ((dir-path (probe-file (pathname directory)))
        (subs-table (when substitutions
                      (validate-and-normalize-substitutions substitutions))))
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
                   ;; Skip files inside dot-directories (.git/, .secret/, etc.).
                   ;; The RFC 8615 /.well-known/ namespace is exempt — see
                   ;; HIDDEN-PATH-COMPONENT-P.
                   (unless (hidden-path-component-p relative)
                     (let* ((url-path (concatenate 'string "/" relative))
                            (raw-content (read-file-bytes fs-path))
                            (rules (and subs-table (gethash url-path subs-table)))
                            (content (if rules
                                         (apply-substitutions raw-content rules)
                                         raw-content))
                            (mime (mime-type-for-path url-path))
                            (mtime (file-write-date file))
                            (cc (if (functionp cache-control)
                                    (funcall cache-control url-path)
                                    cache-control))
                            (response (build-static-response mime content mtime cc)))
                       (when (gethash url-path *static-cache*)
                         (log-debug "static: replacing ~a" url-path))
                       (setf (gethash url-path *static-cache*) response)
                       (incf count)
                       (incf total-bytes (length content))
                       (log-debug "static: ~a (~a, ~d bytes)" url-path mime (length content))))))))))
        ;; Validate every :substitutions file key matched a file we
        ;; actually loaded. Runs before alias generation so aliases
        ;; can't mask a typo'd key.
        (when subs-table
          (maphash (lambda (key rules)
                     (declare (ignore rules))
                     (unless (gethash key *static-cache*)
                       (error "substitutions: ~s is not a loaded file" key)))
                   subs-table))
        ;; Second pass: register extensionless aliases for .html files
        ;; e.g., /login.html also serves at /login
        ;; Actual files take priority — don't overwrite existing entries
        (let ((aliases 0)
              (pending nil))
          ;; Collect aliases first — modifying a hash table during maphash
          ;; is undefined per the CL spec
          (maphash (lambda (url-path response)
                     (let ((len (length url-path)))
                       ;; /foo.html → /foo
                       (when (and (> len 5)
                                  (string= url-path ".html"
                                           :start1 (- len 5)))
                         (let ((alias (subseq url-path 0 (- len 5))))
                           (unless (gethash alias *static-cache*)
                             (push (cons alias response) pending))))
                       ;; /dir/index.html → /dir (no trailing slash).
                       ;; /dir/ already falls through to index.html via
                       ;; the runtime lookup in SERVE-STATIC; this alias
                       ;; closes the /dir (bare) gap so both forms hit
                       ;; the same entry, consistent with the .html
                       ;; extensionless alias above.
                       (when (and (>= len 11)
                                  (string= url-path "/index.html"
                                           :start1 (- len 11)))
                         (let ((alias (subseq url-path 0 (- len 11))))
                           ;; /index.html at root would alias to empty —
                           ;; skip, the root case is served via exact
                           ;; match on /index.html plus the /-prefix
                           ;; runtime fallback.
                           (when (and (> (length alias) 0)
                                      (not (gethash alias *static-cache*)))
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
;;; Range requests (RFC 7233)
;;;
;;; A client asking for part of a resource instead of the whole thing.
;;; Two things depend on it in practice: seeking in <video> / <audio>
;;; (dragging the scrubber issues a Range request for the bytes at that
;;; timestamp — without it the browser must pull the file from byte 0 to
;;; reach the middle), and resuming an interrupted download.
;;;
;;; The body is sliced out of the pre-built 200 response, so serving a
;;; range costs one header build and one subseq — no second copy of the
;;; file is kept, and a full GET still takes the pre-built zero-work path
;;; untouched.
;;; ---------------------------------------------------------------------------

(defun parse-byte-range (header-value total)
  "Parse a Range header value against a resource of TOTAL bytes. Returns
     (values FIRST LAST) — an inclusive byte range — for a single
                           satisfiable range,
     :UNSATISFIABLE       — well-formed but outside the resource (→ 416),
     NIL                  — ignore the header and serve the full 200.

   Handles the three single-range forms of RFC 7233 §2.1:
     bytes=0-499   explicit first-last (LAST is clamped to the end)
     bytes=500-    first byte through the end of the resource
     bytes=-500    the *final* 500 bytes (suffix form)

   Multi-range (bytes=0-99,200-299) returns NIL. RFC 7233 §3.1 permits a
   server to ignore a Range it does not wish to satisfy, and answering
   with multipart/byteranges is a large amount of machinery for a form
   that media players and download managers do not use. Anything
   unparseable also returns NIL: ignoring a Range is always a safe
   answer, where guessing at one is not."
  (when (or (null header-value) (zerop total))
    (return-from parse-byte-range nil))
  (let ((v (string-trim '(#\Space #\Tab) header-value)))
    (unless (and (> (length v) 6) (string-equal v "bytes=" :end1 6))
      (return-from parse-byte-range nil))
    (let ((spec (string-trim '(#\Space #\Tab) (subseq v 6))))
      ;; Multi-range → ignore (see above).
      (when (find #\, spec)
        (return-from parse-byte-range nil))
      (let ((dash (position #\- spec)))
        (unless dash
          (return-from parse-byte-range nil))
        (let ((first-str (subseq spec 0 dash))
              (last-str  (subseq spec (1+ dash))))
          (flet ((digits-p (s)
                   ;; Bounded length as well as charset: an unbounded run
                   ;; of digits would hand PARSE-INTEGER a bignum built
                   ;; from attacker-supplied bytes for no reason.
                   (and (plusp (length s))
                        (<= (length s) 19)
                        (every (lambda (c) (char<= #\0 c #\9)) s))))
            (cond
              ;; Suffix form: bytes=-N — the last N bytes.
              ((and (zerop (length first-str)) (digits-p last-str))
               (let ((n (parse-integer last-str)))
                 (cond
                   ;; bytes=-0 asks for the last zero bytes: RFC 7233
                   ;; §2.1 says a suffix length of 0 is unsatisfiable.
                   ((zerop n) :unsatisfiable)
                   ((>= n total) (values 0 (1- total)))   ; whole resource
                   (t (values (- total n) (1- total))))))
              ;; bytes=N-  or  bytes=N-M
              ((digits-p first-str)
               (let ((start (parse-integer first-str)))
                 (cond
                   ((>= start total) :unsatisfiable)
                   ((zerop (length last-str)) (values start (1- total)))
                   ((digits-p last-str)
                    (let ((end (min (parse-integer last-str) (1- total))))
                      ;; last < first is malformed, not unsatisfiable —
                      ;; ignore it and serve the whole resource.
                      (when (>= end start)
                        (values start end))))
                   (t nil))))
              (t nil))))))))

(defun if-range-matches-p (request entry)
  "T when a Range request may be honored given its If-Range header
   (RFC 7233 §3.2). No If-Range → T. An If-Range carrying our exact ETag
   → T. Anything else → NIL, and the caller serves the full 200 instead.

   This is what keeps a resumed download honest: the client says 'send me
   bytes 5000000- *if* the file is still the one I started downloading'.
   If the file changed, splicing new bytes onto an old prefix produces a
   corrupt file that no error surfaces. Serving the whole resource is the
   defined and safe answer.

   Our ETags are strong, so a weak (W/-prefixed) validator never matches —
   RFC 7233 §3.2 requires a strong comparison here, unlike If-None-Match."
  (let ((if-range (get-header request "if-range")))
    (or (null if-range)
        (let ((v (string-trim '(#\Space #\Tab) if-range))
              (etag (static-entry-etag entry)))
          (and etag (string= v etag))))))

(defun range-response (entry first last)
  "Build a 206 Partial Content response covering the inclusive byte range
   FIRST..LAST of ENTRY. The body is sliced straight out of the pre-built
   200 response — the file lives in memory once."
  (let* ((total (static-entry-content-length entry))
         (len (1+ (- last first)))
         (start (+ (static-entry-body-offset entry) first))
         (body (subseq (static-entry-get-response entry) start (+ start len)))
         (headers (append
                   ;; Content-Length must describe the range, not the file.
                   (remove "content-length" (static-entry-headers entry)
                           :key #'car :test #'string-equal)
                   (list (cons "content-length" (write-to-string len))
                         (cons "content-range"
                               (format nil "bytes ~d-~d/~d" first last total))))))
    (serialize-http-message "HTTP/1.1 206 Partial Content" headers body)))

(defun range-not-satisfiable-response (entry)
  "Build a 416 Range Not Satisfiable response (RFC 7233 §4.4). The
   Content-Range header reports the resource's true length with '*' as the
   range, which is how a client learns what it should have asked for.

   Built per request rather than cached on the entry: 416 answers
   malformed client input, so it is a rare path and not worth a slot on
   every static file in memory."
  (serialize-http-message
   "HTTP/1.1 416 Range Not Satisfiable"
   (list (cons "content-range"
               (format nil "bytes */~d" (static-entry-content-length entry)))
         (cons "content-length" "0")
         (cons "accept-ranges" "bytes")
         (cons "etag" (or (static-entry-etag entry) "")))
   nil))

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
                  ;; Range (RFC 7233) — GET only, and evaluated after the
                  ;; conditional checks above per RFC 7232 §6: a client
                  ;; that already holds the current bytes gets its 304
                  ;; rather than a slice it did not need. HEAD carries no
                  ;; body, so a Range on it is meaningless and the normal
                  ;; HEAD response answers it.
                  ((and (eq method :GET)
                        (get-header request "range"))
                   (multiple-value-bind (first last)
                       (parse-byte-range (get-header request "range")
                                         (static-entry-content-length entry))
                     (cond
                       ;; If-Range says "only if unchanged", and it changed.
                       ((not (if-range-matches-p request entry))
                        (static-entry-get-response entry))
                       ((eq first :unsatisfiable)
                        (range-not-satisfiable-response entry))
                       (first
                        (range-response entry first last))
                       ;; Unparseable / multi-range → ignore, serve it whole.
                       (t
                        (static-entry-get-response entry)))))
                  ((eq method :GET)
                   (static-entry-get-response entry))
                  (t
                   (static-entry-head-response entry)))))))))))
