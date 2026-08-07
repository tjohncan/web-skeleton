# Deployment Guide

Practical notes for building on web-skeleton.
Covers project setup, configuration, and non-obvious pitfalls.

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

`*max-body-size*` caps the inbound request body.
`*max-outbound-response-size*` caps the total bytes (headers + body)
that `tls-read-all` will buffer for an HTTPS fetch —
tune this when your app's `:then` callback expects responses
larger than the 8 MiB default.
`*max-streaming-line-size*` caps one line inside a streamed response
(NDJSON, SSE, chunked text) on the `http-fetch-stream` paths —
the default 1 MiB is generous for JSON,
tighten it for known-small line protocols or raise it for unusual schemas.

## Deployment notes

### Reverse proxy

web-skeleton has no inbound TLS. In production, put it behind
nginx, caddy, or a similar reverse proxy for HTTPS termination.
The default bind address is localhost (`#(127 0 0 1)`), correct for this setup.
Use `:host #(0 0 0 0)` only if the server must accept connections directly.

### WebSocket origin validation

The framework validates WebSocket protocol headers
but does not check the `Origin` header — that's application-level.
Without it, any webpage can open a WebSocket to your server
(cross-site WebSocket hijacking).
Check Origin in your handler before returning `:upgrade`:

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

`jwt-verify` checks the signature, and checks `exp` / `nbf` **when those claims
are present**. Both are OPTIONAL per RFC 7519 §4.1, so a token that simply omits
`exp` verifies and never expires — "signature valid" is not the same as "safe to
act on." If your issuer can mint tokens without `exp`, check for it yourself.
`jwt-verify` also does **not** check `iss` (issuer) or `aud` (audience).
If your JWKS key set is shared across services, always verify these:

```lisp
(let ((claims (jwt-verify token *keys*)))
  (when (and claims
             (string= (jwt-claim claims "iss") "https://auth.mysite.com")
             (string= (jwt-claim claims "aud") "my-app"))
    ;; token is valid and intended for this service
    ...))
```

### Keying on the token string

**Do not key a revocation list, replay-dedup cache, rate-limit bucket, or audit
line on the raw token text.** A verified token does not have exactly one
spelling, and it cannot be made to.

If `(r, s)` verifies then so does `(r, n-s)`. Anyone holding the token can
compute that — no key needed, `n` is a public curve constant. JOSE does not
mandate low-S normalization and mainstream issuers emit high-S roughly half the
time, so rejecting it would reject real tokens; `src/algorithms/ecdsa.lisp` says
so at the point where it declines to enforce it. Two token strings, one
signature, both valid.

Key on something canonical under a valid signature instead:

- the `jti` claim (RFC 7519 §4.1.7), if your issuer sets one
- the `header.payload` prefix — the exact bytes the signature is checked
  against, so any mutation invalidates the token

None of this is a forgery risk. The signed input is `header.payload`, so
altering either changes the bytes the signature is checked against.

### JWKS coordinate encoding

`parse-jwks` raises rather than returning NIL, so a key set it rejects takes down
all verification, not one request.

Base64url decoding is strict: it rejects a final group whose unused low bits are
set, and padding that does not exactly complete the last group. A 32-byte EC
coordinate encodes to 43 characters — a three-character final group — so that
check applies to every key set you load. Only a non-conforming issuer is
affected: RFC 4648 §3.5 makes zeroed pad bits a MUST for encoders, and RFC 7515
§2 makes unpadded segments a MUST for JWS. The error names the key and the
coordinate, so a rotation that trips it reads as the issuer bug it is.

### HMAC signature comparison

When verifying webhook signatures or other HMAC-authenticated messages,
use `constant-time-equal` — never `equal` or `equalp`.
A timing side-channel on byte-by-byte comparison can leak the expected MAC:

```lisp
(let ((expected (hmac-sha256 secret-key body))
      (provided (handler-case
                    (hex-decode (get-header request "x-signature"))
                  (error () nil))))
  (when (and provided (constant-time-equal expected provided))
    ;; signature valid
    ...))
```

`hex-decode` signals an error on invalid input (odd length, non-hex characters).
Wrap it in `handler-case` to reject malformed signatures gracefully.

### Blocking fetch paths

`http-fetch-stream` and HTTPS fetch are **blocking** —
they hold the worker thread for the duration of the upstream call,
bounded by `*fetch-timeout*` (default 30s) across each of three setup phases:

1. **DNS resolution** — shared `getent ahosts` subprocess,
   spawned with `:wait nil` and deadline-polled until exit or
   `*fetch-timeout*` expires. On expiry, the child is killed with
   SIGKILL so a hung libc resolver (unresponsive nameserver,
   slow NSS module, hung mDNS responder) cannot pin the worker thread.
   Same resolver as the async path, so `/etc/hosts`, NSS, Docker DNS,
   and mDNS all work identically in both modes. IPv4 and IPv6 are both supported.
2. **TCP connect** — non-blocking `connect(2)` plus `poll(2)` with the same timeout.
   `SO_RCVTIMEO` / `SO_SNDTIMEO` do **not** apply to `connect(2)` —
   without this bound a black-holed peer would pin a worker for ~120s (Linux `tcp_syn_retries`).
   The socket is returned to blocking mode after the connect completes
   so the subsequent read/write use the familiar blocking semantics.
3. **Request I/O** — bounded by `SO_RCVTIMEO` / `SO_SNDTIMEO` on the connected socket.
   This is fine for bounded work inside a `ws-handler`,
   but avoid calling them from HTTP handlers under load.
   `http-fetch` is non-blocking for `http://` URLs (epoll event loop).
   For `https://` URLs it blocks the worker thread for the full request lifecycle.

**Async fetch timeout budget.** On the non-blocking `http-fetch` path for `http://` URLs,
`*fetch-timeout*` applies as a **single end-to-end budget** rather than per-phase:
the inbound connection's `:awaiting` idle timer covers DNS + TCP connect + request I/O
together. A slow DNS phase shortens the budget remaining for connect and response read.
Blocking paths (`http-fetch-stream`, HTTPS) get the three per-phase bounds above;
the async path gets one total. Tune `*fetch-timeout*` with this in mind —
it is the worst-case wall time the parked inbound will sit in `:awaiting`
before the idle sweeper hands back a 502.

**Chunked completion on the async path.** The non-blocking `http-fetch` path
detects response completion by Content-Length (immediate) or by EOF (Connection: close).
For chunked responses where the upstream keeps the TCP connection alive
after sending the `0\r\n\r\n` terminator, completion is detected only
when the upstream eventually closes or `*fetch-timeout*` expires —
up to 30 seconds of unnecessary delay. The framework sends `Connection: close`
on all outbound requests, so well-behaved upstreams close promptly;
the stall appears only against upstreams that ignore the header.
A future optimization could scan for the zero-size chunk terminator in-buffer.

**`SSL_ERROR_SYSCALL` discipline.** OpenSSL returns `SSL_ERROR_SYSCALL`
for four distinct conditions — unexpected peer close without `close_notify`
(benign for legacy HTTP/1.0-style servers), `SO_RCVTIMEO` firing (`errno = EAGAIN`),
real transport errors (`errno = ECONNRESET` / `EPIPE` / other),
and read(2) failures. `tls-read-all` and `tls-stream-response` inspect `errno`
after each `SSL_ERROR_SYSCALL` and raise loud on the non-benign cases
so `*fetch-timeout*` actually bounds the HTTPS read path for close-delimited responses
and `http-fetch-stream` over HTTPS. Legitimate unexpected-EOF-without-`close_notify`
is still accepted silently — that's the framing signal for HTTP/1.0-style servers
that never send `close_notify` at all.

**Framing headers are the framework's, not yours.** Passing either
`Transfer-Encoding` or `Content-Length` in `:headers` signals an error
rather than going on the wire. Both are ways for a request's declared
framing to disagree with the bytes that follow (RFC 7230 §3.3.3), and
either one aimed at an upstream is a request-smuggling primitive:

- `Transfer-Encoding` — the framework never chunk-encodes, so the claim
  is false whatever the body is, and it does not suppress the computed
  `Content-Length`, so the request would have carried both framing
  headers at once.
- `Content-Length` — a caller-supplied one *does* suppress the computed
  one, so `"5"` in front of a ten-byte body would have left `56789` in
  the upstream's buffer as the head of the next request. This is the one
  reached by accident: a stale `content-length` copied along with the
  rest of a header alist.

For a deliberate `Content-Length: 0` on a bodiless POST, pass `:body ""`
— an empty body is still a body, and the header is computed from it.

### Fetch callback contract

The `:then` closure supplied to `http-fetch` / `defer-to-fetch`
fires **exactly once per fetch** with one of two argument shapes:

- **`(status headers body)`** on the happy path — `status` is an integer HTTP status,
  `headers` is an alist of lowercase-name string pairs,
  `body` is a byte vector (or `NIL` for empty bodies).
  The closure's return value becomes the response
  delivered to the original inbound client.
- **`(NIL NIL NIL)`** as a cleanup sentinel on every abnormal teardown path:
  upstream TCP / TLS / DNS failure, short-body truncation,
  inbound connection closed mid-fetch, drain, worker crash.
  The closure's return value is discarded in this branch
  because there is no inbound to deliver anything to.

The cleanup sentinel exists so apps can release state deterministically
without ambient try/finally bookkeeping: DB transactions, metrics spans,
circuit-breaker counters, rate-limit budgets.
Handlers that interpolate `status` or `body` into log lines or response strings
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
`close-outbound`'s handler-case, so it never blocks the framework's own teardown.
Don't rely on cleanup-path exceptions propagating back to the caller — they don't.

### Fetch URL safety (SSRF)

If your handler constructs fetch URLs from user input, the user is choosing
who your server dials — and your server sits inside the trust boundary.
It can reach `169.254.169.254` (cloud metadata, which hands out IAM credentials),
`127.0.0.1` (your own admin endpoints), and RFC 1918 private ranges
(everything else in the VPC). That is server-side request forgery.

**`*fetch-address-filter*` is the enforcement point.**
It is a special holding a function `(ip family host) -> boolean`,
consulted for every address the fetch machinery is about to dial.
Returning NIL refuses the address. The default is NIL — no filter,
every address allowed — which is the right setting when fetch URLs come
from config rather than from users.

Set it once at startup, before `start-server`:

```lisp
(setf web-skeleton:*fetch-address-filter*
      (lambda (ip family host)
        (declare (ignore host))
        (is-public-address-p ip family)))
```

`is-public-address-p` returns T only for publicly routable addresses,
rejecting loopback, link-local, RFC 1918 private, RFC 6598 CGNAT,
RFC 4193 unique local, multicast, documentation prefixes, reserved ranges,
and cloud metadata IPs. It unwraps IPv4-mapped IPv6, NAT64, and 6to4,
so an attacker cannot launder `127.0.0.1` as `::ffff:127.0.0.1`.

**Why the framework has to do this and an app cannot.**
The framework resolves hostnames itself. An app that resolves a name,
approves the address, and then hands the *name* to `defer-to-fetch`
is racing a second, independent resolution: the attacker's nameserver
answers with a public address on the first lookup and `169.254.169.254`
on the second. That is DNS rebinding, and no amount of app-side checking
closes it, because the app does not control the dial. The filter runs on
the resolution that is actually dialed. It also gates the IP-literal
fast paths — `http://169.254.169.254/` skips DNS entirely, so a
resolver-only check would miss the most direct form of the attack.

**What a refusal does.** The address is skipped, not fatal. A hostname
with several addresses falls through to the next one. A lookup where no
address survives fails the fetch exactly as an unresolvable name does:
502 to the inbound caller, with the fetch callback firing its
`(nil nil nil)` cleanup sentinel exactly once. Refusals log at WARN with
the address and host. A filter that *raises* refuses the address
(fail closed) and logs at ERROR — an app-policy bug must not open the gate.

The filter is mechanism, not policy: it does not know what your app should
be allowed to reach. `is-public-address-p` is the common policy, but a
stricter one is usually better where it is possible — an explicit allowlist
of upstream hosts, checked in the handler before `defer-to-fetch` is ever
called, cannot be defeated by rebinding at all because nothing else is
dialable. Use both: allowlist what you can name, and set the filter as the
backstop for everything else.

`parse-url`, `parse-ipv4-literal`, and `parse-ipv6-literal` are exported
so a handler doing its own up-front URL validation uses the same parsers
the fetch path uses internally — the inbound policy decision and the
outbound dial then agree on what "host" means.

### DNS resolution and caching

Non-numeric hostnames in `http-fetch` / `defer-to-fetch` URLs
are resolved asynchronously: the framework spawns `getent ahosts <host>`
via `sb-ext:run-program`, registers the subprocess's stdout pipe
with the worker's epoll, and parks the inbound connection in an `:out-dns` state
until the address lands. The first TCP-compatible line (`<address> STREAM`) wins.
IPv4 and IPv6 are both supported, with `AI_ADDRCONFIG` filtering
so addresses for unreachable families never appear.
Numeric literals (including bracketed IPv6 forms like `http://[::1]:8080/`)
skip the subprocess entirely via the numeric fast path.

`http-fetch-stream` and the HTTPS paths use the same `getent` subprocess
synchronously via `resolve-host-blocking` — same parser,
same parity with `/etc/hosts` and NSS, same family selection logic.
The only difference from the async path is that the subprocess
runs with `:wait nil` and the caller deadline-polls in short slices
instead of parking in epoll. There is one DNS primitive across the framework.

Semantic parity with `sb-bsd-sockets:get-host-by-name` is preserved:
`/etc/hosts`, `/etc/nsswitch.conf`, Docker's embedded DNS, LDAP, mDNS
— every NSS-configured source is queried via the usual `getaddrinfo(3)`
code path underneath. Apps that depend on exotic name sources
continue to work without change.

**Caching is opt-in, and off by default.** With `*dns-cache-ttl*` at its
default of `0`, `getent` is reinvoked on every outbound fetch to a hostname
— fine for apps making a handful of calls per inbound request, and the only
behavior that is correct without knowing your tolerance for stale addresses.

Apps that pound a small set of upstreams turn the cache on by naming a TTL:

```lisp
(setf web-skeleton:*dns-cache-ttl* 60)   ; trust a resolution for 60s
```

Each worker then keeps its own hostname → address table (workers share
nothing in the hot path, so there is no lock and no contention). A worker's
first fetch to a host pays the subprocess; the rest of that window does not.

Note the corollary when picking a TTL: **the miss rate is per-worker.** The kernel
spreads accepts across workers, so with `N` workers a hot host costs up to `N` `getent`
calls per TTL window, not one — each worker has to learn the address for itself. That is
inherent to the share-nothing design (a shared table would need a lock on the hot path),
and it means a very short TTL buys less than it looks like it should on a many-core box.

The framework cannot pick this number for you, which is exactly why it does
not try. `getent` surfaces no TTL, so the value is a judgment about your
upstream: how long may the server keep dialing a remembered address after
DNS has changed? Small values (30–60s) suit an upstream behind a load
balancer that can fail over; larger values suit a pinned host. Leaving it
at `0` is a legitimate answer — it means "always ask".

Details worth knowing:

- **Successes only.** A failed lookup is not cached. Nameserver blips and
  services still coming up are transient; caching the failure would stretch
  an outage well past its cause.
- **Bounded.** `*dns-cache-max-entries*` (default 256) caps each worker's
  table; on overflow, expired entries are swept and the table cleared if
  that is not enough. The cache is a latency optimization, not a source of
  truth — dropping it costs one `getent` per host. Without a bound, a
  handler fetching attacker-chosen hostnames could grow a worker's memory
  without limit.
- **`*fetch-address-filter*` is re-consulted on every cache hit.** A cached
  address is never a way around the filter: if policy changes, or an entry
  was admitted before a filter was installed, the hit is refused and the
  entry evicted. Without this, caching would be a DNS-rebinding accelerator.
- **A worker restart drops its cache.** Harmless — the next fetch to each
  host pays one `getent` again.

The cache is also the *only* way to avoid the subprocess on an HTTPS fetch.
For plain HTTP, an app can resolve a host itself and pass the IP literal at
the call site, hitting the numeric fast path. That does not work for HTTPS:
`parse-url` refuses `https://` with an IP-literal host, because peer
verification is wired to a DNS name and matching an IP SAN is not
implemented. So an HTTPS upstream has no app-side way to skip `getent` —
`*dns-cache-ttl*` is it.

### ws-send and worker blocking

`ws-send` writes a WebSocket frame to a connection synchronously,
blocking until all bytes are flushed or `*ws-send-timeout*` expires
(default 10 seconds).
Call it from within `ws-handler` to send multiple frames
during a single handler invocation — the event loop is paused while the handler runs,
so there is no write contention.

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

**"Blocking" means the worker, not the connection**, and the number is
worth stating plainly. The event loop being paused is the one serving
*every other connection on that worker*, so one peer that stops reading
freezes all of them for up to `*ws-send-timeout*`. With `(cpu-count)`
workers that is 1/N of the server's capacity held by a single slow
client, and N slow clients arriving together is a full stall.

That is a property of the synchronous design rather than a defect in it.
But an app that broadcasts to many peers, or serves any peer it does not
control, wants the number before it picks `ws-send` over its own queue —
in a fan-out broadcast, one unresponsive subscriber is enough.

`*ws-send-timeout*` must be positive. There is no setting that disables
the deadline: it used to accept `0` for no deadline at all, which meant a
peer that never drained its receive window pinned the worker permanently.

### Static files

`load-static-files` reads files into memory at startup and pre-builds HTTP responses.
Call it **before** `start-server`.
It is not thread-safe and must not be called while the server is running.

Static responses **omit the `Date` header** — the pre-built bytes
are frozen at startup time and the framework will not patch each served
response with a per-request date. This violates the RFC 7231 §7.1.1.2 `MUST`,
but a stale `Date` from 14 hours ago would be strictly worse than none
(CDN caches would use it as the freshness anchor).
Downstream caches fall back to the time they received the response,
which is correct.
If you place web-skeleton behind a CDN or reverse proxy,
the proxy will stamp its own `Date` on the way out —
operators should not be surprised to see `Date` missing on `/static/*`
when watching the upstream directly with `curl -v`.

**Range requests are served** (RFC 7233): `Range: bytes=…` returns `206 Partial Content`
with a `Content-Range`, so `<video>`/`<audio>` seeking and resumable downloads work
rather than re-fetching from byte 0. `If-Range` is honored — a client whose validator
no longer matches gets the whole file, so a resumed download cannot splice bytes from
two versions of a file into a corrupt one. An out-of-bounds range gets `416` carrying
the resource's true length. Multi-range (`bytes=0-9,20-29`) is deliberately ignored and
the full file served, which RFC 7233 §3.1 permits; no media player or download manager
asks for it. The byte range is sliced out of the pre-built response, so enabling this
costs no extra memory and a full GET still takes the pre-built path.

**Dotfiles are not served** — `.git/`, `.env` and anything else with a leading-dot path
component is skipped at load time. The one exception is a root-level `/.well-known/`
(RFC 8615), which *is* served, so ACME HTTP-01 challenges and `security.txt` work
without an app-level route.

`load-static-files` accepts an optional `:substitutions` argument
for injecting deploy-time values into static files without a template engine:

```lisp
(load-static-files
  "static/"
  :substitutions
  '(("index.html" ("__APP_TITLE__"   . "My Great Page")
                  ("__APP_TAGLINE__" . "A fun page to peruse!"))
    ("app.js"     ("__API_BASE__"    . "/api/v1"))))
```

Each entry is `(url-path rule ...)` where each rule is `(literal . replacement)`.
The leading slash on the file key is optional.
The framework never invents a delimiter — the caller supplies the exact literal string
to find and the exact bytes to replace it with.
Escaping for the target format (HTML, JS, CSS, JSON) is the caller's responsibility;
if the replacement value contains characters that are special in the target,
quote them upstream.

Scan semantics are single-pass, left-to-right: at each byte position,
rules are tried in caller order; the first match wins and the scan jumps
past the matched span. Emitted replacement bytes are **never re-scanned**,
so `A`→`B` alongside `B`→`A` is safe by construction.
When two patterns could match at the same position,
whichever is listed first in the rules list wins.

The ETag is computed from the **post-substitution** bytes, so two
deploys with different substituted values will cache-bust correctly.
Content-Length reflects the substituted size.

Validation is strict and fails at load time, not serve time:
empty keys or patterns, duplicate file keys, duplicate patterns within one file,
and file keys that don't match a loaded URL path all signal `ERROR`.
Typos in a deploy config never silently serve unsubstituted bytes.

### IDN hostnames

`tls-connect` calls `SSL_set1_host` with the hostname as ASCII bytes,
which means **internationalized domain names must be ACE-encoded**
(punycode) by the caller before being handed to `http-fetch`.
Passing `https://café.example/` directly will send raw UTF-8 to the server
and fail verification. Convert to `https://xn--caf-dma.example/` in the app
if you deal with IDN — the framework does not ship a UTS-46 / Nameprep implementation.

### Background work and shutdown cleanup

Apps with their own background threads
(session reapers, cache flushers, metrics exporters)
should register a stop function via `register-cleanup`
rather than wrapping `start-server` in an app-side `unwind-protect`.
Cleanup hooks run inside `start-server`'s unwind path after connection drain
and before the function returns:

```lisp
(defun start (&key (port 8080))
  (load-static-files "static/")
  (start-session-reaper)
  (register-cleanup #'stop-session-reaper)
  (start-server :port port
                :handler #'handle-request
                :ws-handler #'handle-ws-message))
```

Hooks fire in LIFO order (last registered, first called),
each wrapped in `handler-case` — a raising hook cannot block the rest
or prevent `start-server` from returning to its caller.
SIGTERM from Docker exercises the same path as Ctrl-C at the REPL.

### Concurrent store

`make-store` returns a thread-safe hash-table-backed store for app-level state
(sessions, caches, rate-limit counters).
All operations hold an internal mutex,
so stores are safe to share across worker threads in the same process.

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

- `store-map` holds the mutex for the entire iteration. Keep the callback fast,
  or use it to collect keys and do slow work outside.
- `expiry-fn` runs under the mutex during every sweep. Keep it a cheap predicate —
  no I/O, no syscalls, nothing that can block.

`store-get` returns two values — `(VALUE PRESENT-P)` — to distinguish
an explicit NIL value from a missing key.
Callers that never store NIL can ignore the second value;
everyone else should branch on it.

`store-update` takes a function rather than a plist:

```lisp
(store-update *counts* "visits" (lambda (old) (1+ (or old 0))))
```

The whole read-modify-write runs under the mutex,
so concurrent updates to the same key serialize without lost writes.
For the common "merge plist keys into the stored value" pattern,
`store-update-plist` is a sugar wrapper:

```lisp
(store-update-plist *sessions* sid
                    :access-token tok
                    :expires-at   exp)
```

### Cookies

`get-cookie` reads a named cookie from the request's `Cookie` header.
`build-cookie` / `delete-cookie` emit Set-Cookie header values with
the usual attributes (`HttpOnly`, `Secure`, `SameSite`, `Max-Age`, `Path`, `Domain`).

The builder's defaults are deliberately the secure ones:
`:http-only t`, `:secure t`, `:same-site :lax`, `:path "/"`.
Pass NIL to opt out of any of them — but know the consequences.
Most session cookies should keep all four.

`:same-site :none` requires `:secure t`;
passing the first without the second signals an error at `build-cookie` time
rather than a silent client-side failure
(browsers reject `SameSite=None` without `Secure`).

Name and value are validated against CR, LF, and semicolon —
the three characters that would break the Set-Cookie header structure.
Apps needing stricter RFC 6265 §4.1.1 token validation can layer it on top.

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

For removal — note that `:path` and `:domain` must match the originally-set cookie,
since browsers match Set-Cookie to stored cookies on those two fields:

```lisp
(add-response-header resp "set-cookie" (delete-cookie "session"))
```

### JSON objects are a distinct type

`json-parse` returns a `json-object` struct for a JSON object, and a plain list
for a JSON array:

| JSON | Lisp |
|---|---|
| `{"a":1}` | `json-object` wrapping `(("a" . 1))` |
| `[1,2]` | `(1 2)` |
| `{}` | empty `json-object` |
| `[]` | `NIL` |
| `null` | `:NULL` |

All three empty forms round-trip to themselves.

**Reading is unchanged.** `json-get` accepts a `json-object` *or* a bare alist,
so handler code written against either representation works:

```lisp
(json-get (json-parse body) "user_id")
```

`json-object-alist` gets the underlying alist when you need to walk it, and
`json-object-p` is the shape test — **not** `listp`, which a struct fails.

**Writing needs a wrapper.** A bare alist serializes as an array of pairs:

```lisp
(json-serialize (make-json-object '(("ok" . t) ("count" . 3))))  ; => {"ok":true,"count":3}
(json-serialize '(("ok" . t)))                                    ; => raises
```

The raise is deliberate and names the fix — a dotted pair is not a valid array
element, so the mistake surfaces at the call site rather than as a
well-formed-but-wrong document.

**Why objects are typed rather than guessed.** An alist and an array of
`[string, value]` pairs are the same Lisp object:

```lisp
(json-parse "[[\"a\",1]]")   ; => (("a" 1))   =  (("a" . (1)))
'(("options" . (("temp" . 0.8))))              ;  (("options" ("temp" . 0.8)))
```

Both are `(string . cons)`. A serializer handed a bare list cannot tell an array
of pairs from an alist with a structured value, so any rule that emits `{…}` for
the second emits it for the first — which is how `[["a",1],["b",2]]` used to come
back out as `{"a":[1],"b":[2]}`. Well-formed, silently wrong, no error anywhere.
The information is destroyed at parse time, so no heuristic downstream can
recover it; typing objects is the only fix that works in both directions.

### Query parameter parsing

`get-query-param` reparses the query string on each call.
If a handler needs multiple parameters, parse once and reuse:

```lisp
(let ((params (parse-query-string (http-request-query request))))
  (let ((name (cdr (assoc "name" params :test #'string=)))
        (page (cdr (assoc "page" params :test #'string=))))
    ...))
```

### Form body decoding

`parse-query-string` and `get-query-param` decode `+` as space
per `application/x-www-form-urlencoded` (the HTML form encoding standard).
For POST bodies with this content type, parse directly:

```lisp
(let* ((body (sb-ext:octets-to-string (http-request-body request)
                                       :external-format :utf-8))
       (params (parse-query-string body)))
  ...)
```

Note: `url-decode` itself treats `+` as literal (pure RFC 3986 path decoding).
The form-aware decoding is in `parse-query-string` and `form-decode`.

### Testing your handlers

`web-skeleton-test-harness` is an optional ASDF system for driving handlers from tests.
Depend on it in your test build (typically via a `my-app-tests.asd`
with `:depends-on ("my-app" "web-skeleton-test-harness")`)
and use `with-test-server` to spin an ephemeral-port live server inside test bodies:

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

`with-test-server` picks a free port, starts the server in a background thread,
binds `*test-port*` for the body, and tears it down on scope exit
(signal shutdown, bounded join, fallback to `terminate-thread`).
Shutdown hooks registered inside the body are isolated to that server's teardown —
they do not leak into the caller's state.

For unit-style tests that bypass the network entirely,
`make-test-request` constructs an `http-request` struct directly:

```lisp
(defun test-auth-rejection ()
  (let* ((req (make-test-request :method :GET :path "/private"))
         (resp (my-app:handle-request req)))
    (assert (= (http-response-status resp) 401))))
```

`make-test-ws-frame` is the analogue for WebSocket handler unit tests —
it builds a masked client frame that `ws-handler` code can parse and process.

End-to-end tests are slower than unit-style tests
(~1-2 seconds per `with-test-server` call, mostly shutdown latency).
Use unit-style tests for handler logic,
end-to-end for the request/response path itself
and for anything that depends on the event loop or graceful shutdown machinery.
