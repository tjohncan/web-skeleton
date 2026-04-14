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
worker thread for the duration of the upstream call, bounded by
`*fetch-timeout*` (default 30s) via socket-level `SO_RCVTIMEO`/`SO_SNDTIMEO`.
This is fine for bounded work inside a `ws-handler` (e.g. streaming an
LLM response), but avoid calling them from HTTP handlers under load.
`http-fetch` is non-blocking for `http://` URLs (epoll event loop).
For `https://` URLs it blocks the worker thread for the full request lifecycle.

### Fetch URL safety (SSRF)

If your handler constructs fetch URLs from user input, validate the
hostname against an allowlist. The framework does not filter resolved
IP addresses — a user-supplied hostname resolving to `169.254.169.254`
(cloud metadata), `127.0.0.1`, or any private RFC 1918 address will be
connected to directly.

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

### Background work and shutdown cleanup

Apps with their own background threads (session reapers, cache flushers,
metrics exporters) should register a stop function via `register-cleanup`
rather than wrapping `start-server` in an app-side `unwind-protect`.
Cleanup hooks run inside `start-server`'s unwind path after connection
drain and before the function returns:

```lisp
(defun start (&key (port 8080))
  (load-static-files "static/")
  (start-session-reaper)
  (register-cleanup #'stop-session-reaper)
  (start-server :port port
                :handler #'handle-request
                :ws-handler #'handle-ws-message))
```

Hooks fire in LIFO order (last registered, first called), each wrapped
in `handler-case` — a raising hook cannot block the rest or prevent
`start-server` from returning to its caller. SIGTERM from Docker
exercises the same path as Ctrl-C at the REPL.

### Concurrent store

`make-store` returns a thread-safe hash-table-backed store for
app-level state (sessions, caches, rate-limit counters). All operations
hold an internal mutex, so stores are safe to share across worker
threads in the same process.

For stores with a reaper, supply both `:expiry-fn` and `:reap-interval`:

```lisp
(defvar *sessions*
  (make-store :expiry-fn     (lambda (id sess)
                               (declare (ignore id))
                               (session-expired-p sess))
              :reap-interval 60))
```

The reaper thread is spawned during `make-store` and its stop function
is registered in the shutdown hook machinery — no app-side teardown.
Supplying `:expiry-fn` without `:reap-interval` (or vice versa) signals
an error at `make-store` time rather than silently skipping the reaper.

Two locking hazards to know about:

- `store-map` holds the mutex for the entire iteration. Keep the
  callback fast, or use it to collect keys and do slow work outside.
- `expiry-fn` runs under the mutex during every sweep. Keep it a cheap
  predicate — no I/O, no syscalls, nothing that can block.

`store-get` returns two values — `(VALUE PRESENT-P)` — to distinguish
an explicit NIL value from a missing key. Callers that never store NIL
can ignore the second value; everyone else should branch on it.

`store-update` takes a function rather than a plist:

```lisp
(store-update *counts* "visits" (lambda (old) (1+ (or old 0))))
```

The whole read-modify-write runs under the mutex, so concurrent updates
to the same key serialize without lost writes. For the common
"merge plist keys into the stored value" pattern, `store-update-plist`
is a sugar wrapper:

```lisp
(store-update-plist *sessions* sid
                    :access-token tok
                    :expires-at   exp)
```

### Cookies

`get-cookie` reads a named cookie from the request's `Cookie` header.
`build-cookie` / `delete-cookie` emit Set-Cookie header values with
the usual attributes (`HttpOnly`, `Secure`, `SameSite`, `Max-Age`,
`Path`, `Domain`).

The builder's defaults are deliberately the secure ones: `:http-only t`,
`:secure t`, `:same-site :lax`, `:path "/"`. Pass NIL to opt out of
any of them — but know the consequences. Most session cookies should
keep all four.

`:same-site :none` requires `:secure t`; passing the first without the
second signals an error at `build-cookie` time rather than a silent
client-side failure (browsers reject `SameSite=None` without `Secure`).

Name and value are validated against CR, LF, and semicolon — the
three characters that would break the Set-Cookie header structure.
Apps needing stricter RFC 6265 §4.1.1 token validation can layer it
on top.

Attach the result to a response via `set-response-header`:

```lisp
(set-response-header resp "set-cookie"
                     (build-cookie "session" sid
                                   :max-age (* 8 60 60)))
```

For removal — note that `:path` and `:domain` must match the
originally-set cookie, since browsers match Set-Cookie to stored
cookies on those two fields:

```lisp
(set-response-header resp "set-cookie" (delete-cookie "session"))
```

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

### Form body decoding

`parse-query-string` and `get-query-param` decode `+` as space per
`application/x-www-form-urlencoded` (the HTML form encoding standard).
For POST bodies with this content type, parse directly:

```lisp
(let* ((body (sb-ext:octets-to-string (http-request-body request)
                                       :external-format :utf-8))
       (params (parse-query-string body)))
  ...)
```

Note: `url-decode` itself treats `+` as literal (pure RFC 3986 path
decoding). The form-aware decoding is in `parse-query-string` and
`form-decode`.

### Testing your handlers

`web-skeleton-test-harness` is an optional ASDF system for driving
handlers from tests. Depend on it in your test build (typically via a
`my-app-tests.asd` with `:depends-on ("my-app" "web-skeleton-test-harness")`)
and use `with-test-server` to spin an ephemeral-port live server
inside test bodies:

```lisp
(defpackage :my-app-tests
  (:use :cl :web-skeleton :web-skeleton-test-harness))

(in-package :my-app-tests)

(defun test-index ()
  (with-test-server (:handler #'my-app:handle-request)
    (multiple-value-bind (status headers body)
        (test-http-request :get "/")
      (declare (ignore headers))
      (assert (= status 200))
      (assert (search "welcome" body)))))
```

`with-test-server` picks a free port, starts the server in a background
thread, binds `*test-port*` for the body, and tears it down on scope
exit (signal shutdown, bounded join, fallback to `terminate-thread`).
Shutdown hooks registered inside the body are isolated to that
server's teardown — they do not leak into the caller's state.

For unit-style tests that bypass the network entirely,
`make-test-request` constructs an `http-request` struct directly:

```lisp
(defun test-auth-rejection ()
  (let* ((req (make-test-request :method :GET :path "/private"))
         (resp (my-app:handle-request req)))
    (assert (= (http-response-status resp) 401))))
```

`make-test-ws-frame` is the analogue for WebSocket handler unit tests
— it builds a masked client frame that `ws-handler` code can parse
and process.

End-to-end tests are slower than unit-style tests (~1-2 seconds per
`with-test-server` call, mostly shutdown latency). Use unit-style
tests for handler logic, end-to-end for the request/response path
itself and for anything that depends on the event loop or graceful
shutdown machinery.
