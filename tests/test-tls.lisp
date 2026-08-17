(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; TLS tests — skipped when web-skeleton-tls is not loaded
;;; ===========================================================================

(defun test-tls ()
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
  (format t "~%=== TLS Tests ===~%")
  (if (null web-skeleton:*https-fetch-fn*)
      (progn
        (format t "~%  SKIP  TLS not loaded (libssl not found)~%")
        (format t "~%0 passed, 0 failed (skipped)~%~%")
        t)
      (progn
        (test-tls-registration)
        (test-ssl-ctx-init)
        (test-tls-stream-response-parity)
        (report-suite "TLS")
        (zerop *tests-failed*))))

(defun %response-corpus ()
  "Response byte-strings covering every framing the two line readers
   decide between. Each entry is (NAME METHOD BYTES)."
  (let* ((cr (string #\Return))
         (lf (string #\Newline))
         (crlf (concatenate 'string cr lf)))
    (flet ((raw (&rest parts)
             (ascii-bytes (apply #'concatenate 'string parts))))
      (list
       (list "content-length" :GET
             (raw "HTTP/1.1 200 OK" crlf "Content-Length: 12" crlf crlf
                  "one" lf "two" lf "x"))
       (list "chunked" :GET
             (raw "HTTP/1.1 200 OK" crlf "Transfer-Encoding: chunked" crlf crlf
                  "8" crlf "alpha" lf "b" crlf
                  "6" crlf "beta" lf crlf
                  "0" crlf crlf))
       (list "interim 100 then 200" :GET
             (raw "HTTP/1.1 100 Continue" crlf crlf
                  "HTTP/1.1 200 OK" crlf "Content-Length: 4" crlf crlf
                  "hi" lf))
       (list "interim 103 with headers" :GET
             (raw "HTTP/1.1 103 Early Hints" crlf "Link: </s.css>" crlf crlf
                  "HTTP/1.1 204 No Content" crlf "Content-Length: 0" crlf crlf))
       (list "HEAD ignores echoed length" :HEAD
             (raw "HTTP/1.1 200 OK" crlf "Content-Length: 9" crlf crlf))
       (list "close-delimited" :GET
             (raw "HTTP/1.1 200 OK" crlf "Content-Type: text/plain" crlf crlf
                  "tail" lf "end" lf))
       (list "chunked truncated before terminator" :GET
             (raw "HTTP/1.1 200 OK" crlf "Transfer-Encoding: chunked" crlf crlf
                  "6" crlf "beta" lf crlf))
       (list "content-length short body" :GET
             (raw "HTTP/1.1 200 OK" crlf "Content-Length: 40" crlf crlf
                  "not forty bytes" lf))
       (list "bare LF terminators" :GET
             (raw "HTTP/1.1 200 OK" crlf "Content-Length: 8" crlf crlf
                  "a" lf "b" lf "c" lf "d" lf))
       (list "CR-only terminators" :GET
             (raw "HTTP/1.1 200 OK" crlf "Content-Length: 6" crlf crlf
                  "a" cr "b" cr "c" cr))))))

(defun %run-line-reader (fn raw method chunk)
  "Drive FN over RAW with a fresh byte source. Returns
   (STATUS LINES RAISED-P).

   TLS-STREAM-RESPONSE and STREAM-RESPONSE-LINES take the same shape —
   (SOURCE ON-LINE &key METHOD READ-FN) — and ignore the positional
   source when READ-FN is supplied, so one driver runs either.

   RAISED-P rather than the condition text. The two implementations word
   their truncation errors differently and always have; the claim under
   test is that they agree on the verdict and on the lines delivered
   before it, not that they phrase a failure identically."
  (let ((lines nil))
    (handler-case
        (let ((status (funcall fn nil
                              (lambda (line) (push line lines))
                              :method method
                              :read-fn (make-mock-read-fn raw :chunk chunk))))
          (list status (nreverse lines) nil))
      (error () (list nil (nreverse lines) t)))))

(defun test-tls-stream-response-parity ()
  "TLS-STREAM-RESPONSE and STREAM-RESPONSE-LINES agree, on every framing,
   at three byte-source granularities.

   This is the capture that makes deleting TLS-STREAM-RESPONSE checkable.
   Issue #4 added its READ-FN seam for exactly this and nothing else, but
   no test had ever driven it, so there was no recorded behavior to
   compare a replacement against — and swapping HTTPS-FETCH-STREAM over
   would have left every existing assertion passing while proving nothing
   about the ~450 lines being removed.

   The three CHUNK granularities are what give it teeth. At :CHUNK NIL
   each response arrives in one fill and the readers never have to resume
   mid-token; at 1 there is a fill boundary between every pair of bytes,
   so every CRLF pair, chunk-size line and header line is split. A seam
   that mapped :EOF wrongly, or read a short fill as end-of-stream, is
   correct at the first granularity and truncating at the third."
  (format t "~%TLS/plain line-reader parity~%")
  ;; TLS-SYM for the TLS side only: that symbol exists solely once
  ;; web-skeleton-tls is in the image, while STREAM-RESPONSE-LINES ships
  ;; in the core system and can be named literally.
  (let ((tls-reader (tls-sym "TLS-STREAM-RESPONSE"))
        (plain-reader #'web-skeleton::stream-response-lines))
    (dolist (chunk '(nil 7 1))
      (dolist (entry (%response-corpus))
        (destructuring-bind (name method raw) entry
          (check (format nil "parity [~a] ~a"
                         (if chunk (format nil "chunk ~d" chunk) "whole")
                         name)
                 (%run-line-reader plain-reader raw method chunk)
                 (%run-line-reader tls-reader raw method chunk)))))))

(defun test-tls-registration ()
  (format t "~%TLS Registration~%")
  (check "https-fetch-fn set"
         (not (null web-skeleton:*https-fetch-fn*)) t)
  (check "https-stream-fn set"
         (not (null web-skeleton:*https-stream-fn*)) t))

(defun tls-sym (name)
  "Resolve a WEB-SKELETON symbol that exists only once web-skeleton-tls
   is loaded. This test system does not depend on the TLS system — it is
   optional, loaded at runtime by run-tests.lisp — so writing
   WEB-SKELETON::%SSL-CTX-CTRL literally would intern the symbol at read
   time and emit undefined-function / undefined-variable warnings on
   every compile of a tree where TLS is not in the image. Looking the
   name up at run time keeps the compile clean. Only ever called from
   inside the libssl-is-loaded branch, so a miss is a real error."
  (or (find-symbol name :web-skeleton)
      (error "web-skeleton::~a not found — is web-skeleton-tls loaded?"
             name)))

(defun test-ssl-ctx-init ()
  ;; Smoke the shared-context init path for real. Registration checks
  ;; alone can't catch an FFI-level mistake — a binding that names the
  ;; wrong libssl entry point (the historical SSL_ctrl / SSL_CTX_ctrl
  ;; mixup) or passes the wrong struct type loads cleanly and only
  ;; misbehaves when the call executes. ENSURE-SSL-CTX runs the whole
  ;; init sequence (OPENSSL_init_ssl, SSL_CTX_new, TLS 1.2 floor via
  ;; SSL_CTX_ctrl, CA paths, verify mode) with zero network, so it
  ;; belongs in the default suite whenever libssl is present.
  ;; Idempotent — the context is created once per process and cached,
  ;; so running it here just means the first later HTTPS fetch finds
  ;; it warm.
  ;;
  ;; The read-back check is the one with teeth. Empirically (OpenSSL
  ;; 3.5.4): the mixed-up SET returns 1 — success by every visible
  ;; measure — while the floor lands at a garbage offset, so only
  ;; reading min_proto_version back off the context distinguishes a
  ;; correct init from a lucky one.
  (format t "~%SSL context init~%")
  (check "ensure-ssl-ctx initializes shared context"
         (handler-case (and (funcall (tls-sym "ENSURE-SSL-CTX")) t)
           (error (e) (format nil "error: ~a" e)))
         t)
  (check "TLS 1.2 floor reads back off the context"
         (handler-case
             (funcall (tls-sym "%SSL-CTX-CTRL")
                      (funcall (tls-sym "ENSURE-SSL-CTX"))
                      (symbol-value
                       (tls-sym "+SSL-CTRL-GET-MIN-PROTO-VERSION+"))
                      0 (sb-sys:int-sap 0))
           (error (e) (format nil "error: ~a" e)))
         (symbol-value (tls-sym "+TLS1-2-VERSION+"))))
