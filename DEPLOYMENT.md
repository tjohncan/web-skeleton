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
        *max-body-size* (* 2 1024 1024)          ; 2 MiB inbound request body
        *max-outbound-response-size* (* 16 1024 1024) ; 16 MiB buffered fetch
        *max-streaming-line-size* (* 2 1024 1024))    ; 2 MiB per stream line
  (load-static-files "static/")
  (start-server :port port
                :host #(0 0 0 0)          ; listen on all interfaces
                :handler #'handle-request
                :ws-handler #'handle-ws-message))
```

`*max-body-size*` caps the inbound request body. `*max-outbound-response-size*`
caps the total bytes (headers + body) that `tls-read-all` will buffer for an
HTTPS fetch — tune this when your app's `:then` callback expects responses
larger than the 8 MiB default. `*max-streaming-line-size*` caps one line inside
a streamed response (NDJSON, SSE, chunked text) on the `http-fetch-stream`
paths — the default 1 MiB is generous for JSON, tighten it for known-small
line protocols or raise it for unusual schemas.

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
`*fetch-timeout*` (default 30s) across each of three setup phases:

1. **DNS resolution** — shared `getent ahosts` subprocess, spawned
   with `:wait nil` and deadline-polled until exit or
   `*fetch-timeout*` expires. On expiry, the child is killed with
   SIGKILL so a hung libc resolver (unresponsive nameserver, slow
   NSS module, hung mDNS responder) cannot pin the worker thread.
   Same resolver as the async path, so `/etc/hosts`, NSS, Docker
   DNS, and mDNS all work identically in both modes. IPv4 and IPv6
   both supported.
2. **TCP connect** — non-blocking `connect(2)` plus `poll(2)` with
   the same timeout. `SO_RCVTIMEO` / `SO_SNDTIMEO` do **not** apply
   to `connect(2)` — without this bound a black-holed peer would
   pin the worker for ~120s (Linux `tcp_syn_retries`). The socket
   is returned to blocking mode after the connect completes so the
   subsequent read/write use the familiar blocking semantics.
3. **Request I/O** — bounded by `SO_RCVTIMEO` / `SO_SNDTIMEO` on
   the connected socket.
This is fine for bounded work inside a `ws-handler` (e.g. streaming an
LLM response), but avoid calling them from HTTP handlers under load.
`http-fetch` is non-blocking for `http://` URLs (epoll event loop).
For `https://` URLs it blocks the worker thread for the full request lifecycle.

**Async fetch timeout budget.** On the non-blocking `http-fetch`
path for `http://` URLs, `*fetch-timeout*` applies as a **single
end-to-end budget** rather than per-phase: the inbound connection's
`:awaiting` idle timer covers DNS + TCP connect + request I/O
together. A slow DNS phase shortens the budget remaining for
connect and response read. Blocking paths (`http-fetch-stream`,
HTTPS) get the three per-phase bounds above; the async path gets
one total. Tune `*fetch-timeout*` with this in mind — it is the
worst-case wall time the parked inbound will sit in `:awaiting`
before the idle sweeper hands back a 502.

**Chunked completion on the async path.** The non-blocking `http-fetch`
path detects response completion by Content-Length (immediate) or by
EOF (Connection: close). For chunked responses where the upstream
keeps the TCP connection alive after sending the `0\r\n\r\n`
terminator, completion is detected only when the upstream eventually
closes or `*fetch-timeout*` expires — up to 30 seconds of unnecessary
delay. The framework sends `Connection: close` on all outbound
requests, so well-behaved upstreams close promptly; the stall appears
only against upstreams that ignore the header. A future optimization
could scan for the zero-size chunk terminator in-buffer.

**`SSL_ERROR_SYSCALL` discipline.** OpenSSL returns
`SSL_ERROR_SYSCALL` for four distinct conditions — unexpected peer
close without `close_notify` (benign for legacy HTTP/1.0-style
servers), `SO_RCVTIMEO` firing (`errno = EAGAIN`), real transport
errors (`errno = ECONNRESET` / `EPIPE` / other), and read(2)
failures. `tls-read-all` and `tls-stream-response` inspect `errno`
after each `SSL_ERROR_SYSCALL` and raise loud on the non-benign
cases so `*fetch-timeout*` actually bounds the HTTPS read path
for close-delimited responses and `http-fetch-stream` over HTTPS.
Legitimate unexpected-EOF-without-`close_notify` is still accepted
silently — that's the framing signal for HTTP/1.0-style servers
that never send `close_notify` at all.

### Fetch callback contract

The `:then` closure supplied to `http-fetch` / `defer-to-fetch`
fires **exactly once per fetch** with one of two argument shapes:

- **`(status headers body)`** on the happy path — `status` is an
  integer HTTP status, `headers` is an alist of lowercase-name
  string pairs, `body` is a byte vector (or `NIL` for empty bodies).
  The closure's return value becomes the response delivered to the
  original inbound client.
- **`(NIL NIL NIL)`** as a cleanup sentinel on every abnormal
  teardown path: upstream TCP / TLS / DNS failure, short-body
  truncation, inbound connection closed mid-fetch, drain, worker
  crash. The closure's return value is discarded in this branch
  because there is no inbound to deliver anything to.

The cleanup sentinel exists so apps can release state deterministically
without ambient try/finally bookkeeping: DB transactions, metrics
spans, circuit-breaker counters, rate-limit budgets. Handlers that
interpolate `status` or `body` into log lines or response strings
must check for `NIL` explicitly:

```lisp
:then (lambda (status headers body)
        (declare (ignore headers))
        (if status
            ;; Happy path — shape a real response from the fetch.
            (make-text-response
             status
             (if body
                 (sb-ext:octets-to-string body :external-format :utf-8)
                 ""))
            ;; Cleanup path — fetch never completed. Release any
            ;; resources the closure captured and return NIL; the
            ;; framework discards the value.
            (progn (release-resources) nil)))
```

A raising cleanup closure is logged at WARN and swallowed by
`close-outbound`'s handler-case, so it never blocks the framework's
own teardown. Don't rely on cleanup-path exceptions propagating
back to the caller — they don't.

### Fetch URL safety (SSRF)

If your handler constructs fetch URLs from user input, validate the
destination before dialing. The framework does not filter resolved
IP addresses — a user-supplied hostname resolving to `169.254.169.254`
(cloud metadata), `127.0.0.1`, or any private RFC 1918 address will
be connected to directly unless the app refuses.

`is-public-address-p` is the primitive for doing this refusal
correctly. It takes a byte vector and a family keyword and returns
T only for publicly routable addresses, rejecting loopback, link-local,
RFC 1918 private, RFC 6598 CGNAT, RFC 4193 unique local, multicast,
documentation prefixes, reserved ranges, and cloud metadata IPs. It
unwraps IPv4-mapped IPv6 and NAT64 so an attacker cannot launder
`127.0.0.1` as `::ffff:127.0.0.1`.

The framework exports `parse-url`, `parse-ipv4-literal`, and
`parse-ipv6-literal` specifically so a handler writing this check
doesn't have to reinvent them. They are the same parsers the
outbound fetch path uses internally, so a policy decision on the
inbound side and the actual dial on the outbound side agree on
what "host" means:

```lisp
(defun handle-proxy (req)
  (let ((url (get-query-param req "url")))
    (unless url
      (return-from handle-proxy (make-error-response 400)))
    (multiple-value-bind (scheme host port path)
        (handler-case (parse-url url) (error () (values nil nil nil nil)))
      (declare (ignore port path))
      (unless scheme
        (return-from handle-proxy (make-error-response 400)))
      ;; parse-url + parse-ipv*-literal + is-public-address-p together.
      ;; Only IP-literal hosts are accepted, and only if the address
      ;; classifies as publicly routable. Hostnames are rejected —
      ;; a permissive app would allowlist specific ones up front,
      ;; or resolve via its own DNS path before calling
      ;; is-public-address-p on each resolved address.
      (let* ((v4 (parse-ipv4-literal host))
             (v6 (and (not v4) (parse-ipv6-literal host))))
        (cond
          ((and v4 (is-public-address-p v4 :inet))
           (defer-to-fetch :get url :then my-callback))
          ((and v6 (is-public-address-p v6 :inet6))
           (defer-to-fetch :get url :then my-callback))
          (t
           (make-error-response 403)))))))
```

The helper deliberately does not resolve hostnames — apps that accept
hostnames must resolve first and then call `is-public-address-p` on
each resolved address before dialing.

### DNS resolution and caching

Non-numeric hostnames in `http-fetch` / `defer-to-fetch` URLs are
resolved asynchronously: the framework spawns `getent ahosts <host>`
via `sb-ext:run-program`, registers the subprocess's stdout pipe with
the worker's epoll, and parks the inbound connection in an `:out-dns`
state until the address lands. The first TCP-compatible line
(`<address> STREAM`) wins. IPv4 and IPv6 are both supported, with
`AI_ADDRCONFIG` filtering so addresses for unreachable families never
appear. Numeric literals (including bracketed IPv6 forms like
`http://[::1]:8080/`) skip the subprocess entirely via the numeric
fast path.

`http-fetch-stream` and the HTTPS paths use the same `getent`
subprocess synchronously via `resolve-host-blocking` — same parser,
same parity with `/etc/hosts` and NSS, same family selection logic.
The only difference from the async path is that the subprocess runs
with `:wait nil` and the caller deadline-polls in short slices instead
of parking in epoll. There is one DNS primitive across the framework.

Semantic parity with `sb-bsd-sockets:get-host-by-name` is preserved:
`/etc/hosts`, `/etc/nsswitch.conf`, Docker's embedded DNS, LDAP, mDNS
— every NSS-configured source is queried via the usual `getaddrinfo(3)`
code path underneath. Apps that depend on exotic name sources continue
to work without change.

**Caching is opt-in at the app layer.** `getent` is reinvoked on
every outbound fetch, which is fine for apps making a handful of
calls per inbound request. Apps that pound a small set of upstream
hosts many times can cache DNS themselves using the `store`
primitive in about fifteen lines, then bypass the framework's DNS
path by passing the resolved IP literal at the call site:

```lisp
(defvar *dns-cache*
  (make-store :expiry-fn (lambda (host entry)
                           (declare (ignore host))
                           (> (get-universal-time) (cdr entry)))
              :reap-interval 60))

(defun cached-ip-for (host)
  (car (store-get *dns-cache* host)))

(defun remember-ip (host ip &key (ttl 60))
  (store-set *dns-cache* host (cons ip (+ (get-universal-time) ttl))))
```

At the call site, check `cached-ip-for` first and build the URL with
the IP literal when there's a hit — the framework's numeric fast
path skips `getent` entirely. On a miss, fall through to a hostname
URL (paying the `getent` cost once) and populate the cache when the
response arrives.

The framework deliberately does not ship a DNS cache of its own.
`getent` output does not surface TTL information, so any built-in
cache would have to invent its own expiry policy — a choice that
belongs to the app, not the framework.

### ws-send and worker blocking

`ws-send` writes a WebSocket frame to a connection synchronously,
blocking until all bytes are flushed (fixed 10-second timeout). Call it from
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

Static responses **omit the `Date` header** — the pre-built bytes are
frozen at startup time and the framework will not patch each served
response with a per-request date. This violates the RFC 7231 §7.1.1.2
`MUST`, but a stale `Date` from 14 hours ago would be strictly worse
than none (CDN caches would use it as the freshness anchor). Downstream
caches fall back to the time they received the response, which is
correct. If you place web-skeleton behind a CDN or reverse proxy, the
proxy will stamp its own `Date` on the way out — operators should not
be surprised to see `Date` missing on `/static/*` when watching the
upstream directly with `curl -v`.

### IDN hostnames

`tls-connect` calls `SSL_set1_host` with the hostname as ASCII bytes,
which means **internationalized domain names must be ACE-encoded**
(punycode) by the caller before being handed to `http-fetch`. Passing
`https://café.example/` directly will send raw UTF-8 to the server
and fail verification. Convert to `https://xn--caf-dma.example/` in
the app if you deal with IDN — the framework does not ship a
UTS-46 / Nameprep implementation.

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

Attach cookies via `add-response-header` (not `set-response-header`,
which replaces — a second `set-response-header` for Set-Cookie would
silently drop the first cookie):

```lisp
(add-response-header resp "set-cookie"
                     (build-cookie "session" sid
                                   :max-age (* 8 60 60)))
(add-response-header resp "set-cookie"
                     (build-cookie "theme" "dark" :http-only nil))
```

For removal — note that `:path` and `:domain` must match the
originally-set cookie, since browsers match Set-Cookie to stored
cookies on those two fields:

```lisp
(add-response-header resp "set-cookie" (delete-cookie "session"))
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
