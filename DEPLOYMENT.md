# Deployment Guide

Practical notes for building on web-skeleton. Covers project setup,
configuration, and non-obvious gotchas that the API alone won't tell you.

## Project setup

A consumer project lives in its own directory alongside web-skeleton
and references it via ASDF's central registry.

```
(DEV)/
  web-skeleton/         # the framework (cloned)
  my-app/               # your project
    my-app.asd
    run.lisp
    src/
      package.lisp
      handler.lisp
    static/             # optional — HTML, CSS, JS, images
```

### ASDF system definition (`my-app.asd`)

```lisp
(defsystem "my-app"
  :depends-on ("web-skeleton")
  :serial t
  :components ((:file "src/package")
               (:file "src/handler")))
```

### Entry point (`run.lisp`)

Register both your project and web-skeleton with ASDF, then load and start:

```lisp
(require :asdf)

;; Register both systems with ASDF
(push (make-pathname :directory (pathname-directory *load-truename*))
      asdf:*central-registry*)
(push (merge-pathnames "../web-skeleton/"
                       (make-pathname :directory (pathname-directory *load-truename*)))
      asdf:*central-registry*)

;; Optional: load TLS support for outbound HTTPS
(handler-case (asdf:load-system "web-skeleton-tls")
  (error () (format t "Note: TLS not available (libssl not found)~%")))

(asdf:load-system "my-app")
(my-app:start)
```

### Package declaration (`src/package.lisp`)

```lisp
(defpackage :my-app
  (:use :cl :web-skeleton)
  (:export #:start))
```

### Handler skeleton (`src/handler.lisp`)

```lisp
(in-package :my-app)

(defun handle-request (request)
  (let ((path (http-request-path request)))
    (cond
      ((and (eq (http-request-method request) :GET)
            (string= path "/ws"))
       :upgrade)
      (t (or (serve-static request)
             (make-error-response 404))))))

(defun handle-ws-message (conn frame)
  (when (= (ws-frame-opcode frame) +ws-op-text+)
    ;; handle WebSocket message
    nil))

(defun start (&key (port 8080))
  (load-static-files "static/")
  (start-server :port port
                :handler #'handle-request
                :ws-handler #'handle-ws-message))
```

### Configuration

Tune parameters before calling `start-server`:

```lisp
(defun start (&key (port 8080))
  ;; Tune limits for your workload
  (setf *idle-timeout* 15
        *max-connections* 5000
        *max-body-size* (* 2 1024 1024))  ; 2MB
  (load-static-files "static/")
  (start-server :port port
                :host #(0 0 0 0)          ; listen on all interfaces
                :handler #'handle-request
                :ws-handler #'handle-ws-message))
```

## Deployment notes

### Reverse proxy

web-skeleton has no inbound TLS. In production, put it behind
nginx, caddy, or a similar reverse proxy for HTTPS termination.
The default bind address is localhost (`#(127 0 0 1)`), which is
correct for this setup. Use `:host #(0 0 0 0)` only if the server
must accept connections directly.

### WebSocket origin validation

The framework validates WebSocket protocol headers but does not
check the `Origin` header — that's application-level. Without it,
any webpage can open a WebSocket to your server (cross-site
WebSocket hijacking). Check Origin in your handler before
returning `:upgrade`:

```lisp
(defun handle-request (request)
  (cond
    ((string= (http-request-path request) "/ws")
     (let ((origin (get-header request "origin")))
       (if (and origin (string= origin "https://mysite.com"))
           :upgrade
           (make-error-response 403))))
    (t (or (serve-static request)
           (make-error-response 404)))))
```

### JWT issuer and audience

`jwt-verify` checks the signature, expiration, and not-before claims.
It does **not** check `iss` (issuer) or `aud` (audience). If your
JWKS key set is shared across services, always verify these manually:

```lisp
(let ((claims (jwt-verify token *keys*)))
  (when (and claims
             (string= (jwt-claim claims "iss") "https://auth.mysite.com")
             (string= (jwt-claim claims "aud") "my-app"))
    ;; token is valid and intended for this service
    ...))
```

### HMAC signature comparison

When verifying webhook signatures or other HMAC-authenticated messages,
use `constant-time-equal` — never `equal` or `equalp`. A timing
side-channel on byte-by-byte comparison can leak the expected MAC:

```lisp
(let ((expected (hmac-sha256 secret-key body))
      (provided (handler-case
                    (hex-decode (get-header request "x-signature"))
                  (error () nil))))
  (when (and provided (constant-time-equal expected provided))
    ;; signature valid
    ...))
```

`hex-decode` signals an error on invalid input (odd length, non-hex
characters). Wrap it in `handler-case` to reject malformed signatures
gracefully.

### Blocking fetch paths

`http-fetch-stream` and HTTPS fetch are **blocking** — they hold the
worker thread for the duration of the upstream call. This is fine for
bounded work inside a `ws-handler` (e.g. streaming an LLM response),
but avoid calling them from HTTP handlers under load. `http-fetch` is
non-blocking for `http://` URLs (epoll event loop). For `https://` URLs
it blocks the worker thread for the full request lifecycle.

### ws-send and worker blocking

`ws-send` writes a WebSocket frame to a connection synchronously,
blocking until all bytes are flushed (up to 10 seconds). Call it from
within `ws-handler` to send multiple frames during a single handler
invocation — the event loop is paused while the handler runs, so there
is no write contention.

```lisp
(defun handle-ws-message (conn frame)
  (when (= (ws-frame-opcode frame) +ws-op-text+)
    ;; Stream results back as they become available
    (dolist (chunk (generate-chunks (ws-frame-payload frame)))
      (ws-send conn (build-ws-text chunk)))
    nil))  ; return nil — we already sent our responses
```

The worker thread is blocked for the duration of the handler call.
With multiple workers this is fine for bounded work (e.g. streaming
an LLM response for a few seconds), but avoid unbounded blocking —
a slow client holds the worker hostage.

### Static files

`load-static-files` reads files into memory at startup and pre-builds
HTTP responses. Call it **before** `start-server`. It is not thread-safe
and must not be called while the server is running.

### JSON empty containers

`json-parse` returns NIL for both `{}` and `[]`. `json-serialize` on NIL
produces `"null"`. This means empty objects and arrays do not round-trip
— they collapse to null. This is a deliberate design choice (CL's NIL
is the natural empty representation). If the distinction matters for
your use case, check the raw JSON string or use `:NULL` for explicit null.

### Query parameter parsing

`get-query-param` reparses the query string on each call. If a handler
needs multiple parameters, parse once and reuse:

```lisp
(let ((params (parse-query-string (http-request-query request))))
  (let ((name (cdr (assoc "name" params :test #'string=)))
        (page (cdr (assoc "page" params :test #'string=))))
    ...))
```
