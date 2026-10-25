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

;; Optional: load TLS support for outbound HTTPS. Print the error itself:
;; a compile or read error in the TLS system is not a missing libssl.
(handler-case (asdf:load-system "web-skeleton-tls")
  (error (e) (format t "Note: TLS not loaded (~a)~%" e)))

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

### Routes in front of static files

A handler that guards a path, such as a login check on `/admin`, and falls
back to `serve-static` relies on the router and the static server reading a
request's path the same way. They do:

- `http-request-path` is the path as it arrived, escapes and all.
- `match-path` compares a pattern's literal segments with that spelling
  under the default `*match-path-literals*` of `:exact` (with its decoding
  under `:decoded`), and decodes captures. Under `:exact` both sides are
  read through `canonical-path`, which puts each escape's hex digits in
  uppercase and decodes one thing: an escape of
  `[ ] : @ ! $ & ' ( ) * + , ; =`. Those may be written either way,
  because a page that builds its URLs with `encodeURIComponent` escapes
  them, as Next.js does for its asset paths. The case of the hex is no
  part of the URI (RFC 3986 §2.1), and a proxy may re-encode a path in
  lowercase, as Apache httpd does, so `/caf%c3%a9` and `/caf%C3%A9` are
  one path.
- `serve-static` serves a file at the spellings of its path a browser
  sends. A byte is escaped where a browser escapes it (space, `"`, `#`,
  `%`, `<`, `>`, `?`, `` ` ``, `{`, `}`, controls and non-ASCII), with
  hex in either case, the punctuation above is either escaped or not, and
  every other byte is written as itself. `/%61dmin.html`, `/admin%2Ehtml`
  and `/admin%2Freport.txt` name no file, in either case of hex.

So every spelling that reaches a file is one the router reads the same way,
and a guard's literal covers all of them. An application that checks the
raw path itself should compare `(canonical-path path)`, the router's
reading. A guard must also cover the file's aliases. `serve-static` answers
`/x` for `x.html`, and both `/dir` and `/dir/` for `dir/index.html`.

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
an `http-fetch` will buffer for a response, either scheme —
tune this when your app's `:then` callback expects responses
larger than the 8 MiB default.
`*max-streaming-line-size*` caps one line inside a streamed response
(NDJSON, SSE, chunked text) on the `http-fetch-stream` paths —
the default 1 MiB is generous for JSON,
tighten it for known-small line protocols or raise it for unusual schemas.

## Deployment notes

### Runnable artifacts

`demo/deploy/` holds a working set for the demo
(a runnable Dockerfile and nginx server block).

| file | what it settles |
|---|---|
| `Dockerfile` | Debian rather than Alpine, because resolution shells out to glibc's `getent ahosts`; runs as a user of its own rather than root; compiles at build time, as that user, so a compile error fails the build rather than the container and a start finds what the build compiled; healthchecks on `/healthz`, which fails when any worker stops passing its loop — whichever worker takes the probe, since it reads every worker's last pass — where a TCP connect, or a probe that lands on one worker of four, would not |
| `compose.yml` | publishes on `127.0.0.1` only, so the proxy is the only way in; keeps no capability, and sets no-new-privileges |
| `nginx.conf.sample` | listens on IPv6 as well as IPv4; HSTS beside the CSP header, and a note that a location with an `add_header` of its own loses both; the `Upgrade`/`Connection` pair via a `map`, buffering off, a read timeout longer than the server's own ping interval, `limit_req`, which bounds HTTP requests but not the frames on an upgraded WebSocket — one request, however many frames follow — so the bulletin budgets posts per socket in the application; and `limit_conn` on the WebSocket location alone, because a budget per socket does not bound a client that opens many — the budget limits a socket, and bounding a client takes both proxy limits: `limit_conn` for the sockets one address holds at once, `limit_req` for how fast it opens new ones |
| `run.lisp` | the container entry point: binds `0.0.0.0` rather than loopback, because a socket bound to a container's own loopback is reachable from nothing outside it |

They deploy the demo and name no other project. Adapt rather than adopt: the
`server_name` and certificate paths are yours — which is why the proxy config
ships as `.sample`. `.gitignore` excludes `*.conf` so a real one, with real
hostnames in it, cannot be committed by reflex.

### Reverse proxy

web-skeleton has no inbound TLS, and it is
not built to face the internet on its own.
Put it behind nginx, caddy, or a similar reverse proxy.
The proxy terminates TLS, and it is also where request buffering,
rate limits and the patience for slow or hostile clients belong:
the framework bounds everything it parses,
but it does not try to be what absorbs an attack.
The default bind address is localhost (`#(127 0 0 1)`), correct for this setup.
Bind `#(0 0 0 0)` inside a container,
where the published port decides who can arrive —
not to take connections from the internet directly.

Request buffering is a setting, not a given.
nginx buffers a request body before forwarding it, by default.
Caddy, HAProxy and Traefik stream it to the upstream unless told otherwise
(`request_buffers`, `option http-buffer-request`, the `buffering` middleware).
Behind a proxy that streams, the client trickling a body
is this server's to hold after all,
which is the one job above that comes back here.
What holds it is `*idle-timeout*`, and it holds it bluntly:
it is the whole budget for one request to arrive, head and body together,
and the bytes arriving do not extend it.
That is what stops a client that drips a request from keeping its slot,
and it also cuts off an honest upload that is merely slow —
at 20 KiB/s, ten seconds is about 200 KiB —
with a close and no response.
Behind a streaming proxy, have the proxy buffer bodies,
or accept the bodies you expect from a body handler
(see "Request bodies, decided at the head"):
an accepted body is timed by `*body-stall-timeout*` on its progress,
so an upload that keeps arriving is not cut off for its size,
and one that stops is still closed.

A proxy that opens connections in bursts meets the accept queue.
Each worker listens with `*listen-backlog*` (128) completed connections
held for it, so the server holds that times its worker count,
and a burst past that is dropped at the SYN, which the client retries
after a second or more and nothing on this side logs.
Linux clamps the backlog to `net.core.somaxconn`;
raising `*listen-backlog*` past it needs that sysctl raised as well.

### Status codes the framework itself sends

A request the framework rejects before your handler sees it gets a
specific status, not a blanket 400:

| Code | Sent when |
|------|-----------|
| `400 Bad Request` | Syntax it could not parse — malformed request line, bad header, invalid UTF-8, missing or duplicated `Host`. Also every refusable `Transfer-Encoding` shape that is not simply unimplemented: TE with `Content-Length`, `chunked` in a non-final position or repeated, a repeated TE header, an obs-folded TE value, TE on HTTP/1.0, a non-empty trailer section, and malformed chunked framing — a size line outside the grammar, chunk data not followed by its CRLF — which the completion walk refuses as soon as the bytes that show it have arrived, the decoder and the walk accepting exactly the same framing |
| `413 Payload Too Large` | Body over its cap — `*max-body-size*`, or the one a body handler chose for the request — `Content-Length` over fifteen digits, or the read buffer filled without a complete request. A chunked body reaches the first of those incrementally, as its decoded total grows, and also on a single chunk header declaring more than the cap; the message says which of the two limits fired, because for small chunks the buffer fills first |
| `400`, `413`, `415` from a form | `parse-multipart` or `receive-multipart` reading a request the application asked it to: `415 Unsupported Media Type` for a `Content-Type` that is not `multipart/form-data` — from a body handler, before any of the body is read — `400` for a body that is not one as the parser reads it, `413` past one of its limits. See "Forms and file uploads" |
| `414 URI Too Long` | Request line over `*max-request-line-length*` — refused as soon as that many bytes have arrived without it ending, not when it finally does |
| `417 Expectation Failed` | An `Expect` the framework does not implement |
| `431 Request Header Fields Too Large` | One header over `*max-header-line-length*`, headers over `*max-total-header-bytes*`, or more than `*max-header-count*` of them. A head still unterminated at the size the header budgets allow is refused then, rather than buffered toward the larger cap that leaves room for a body |
| `501 Not Implemented` | A method not in the accepted set, or a transfer coding that is legal and unimplemented — `gzip`, or `gzip, chunked` (RFC 7230 §3.3.1). A bare final `chunked` is accepted |
| `503 Service Unavailable` | The worker is at `*max-connections*` — carries `Retry-After: 2` |
| `504 Gateway Timeout` | A handler deferred to a fetch and the fetch never came back within `*fetch-timeout*` — the parked connection is answered rather than closed. For a request answered with a streamed fetch, the wait starts once its body has gone; see "Sending a request body as it is produced" |
| `505 HTTP Version Not Supported` | Anything that is not HTTP/1.0 or HTTP/1.1 — including an HTTP/2 prior-knowledge preface |
| `500 Internal Server Error` | Your handler raised — an error, a timeout it set itself (`sb-ext:with-timeout`, or `sb-sys:with-deadline` around a wait), or a storage condition: recursion past the control stack, or one allocation larger than the heap has left. Any of these costs that request and nothing more. Once a response has begun — a stream's head is out, a WebSocket is open — the connection is closed instead of answered, since a 500 there would arrive inside the first response. A heap worn out by many small allocations can be beyond any catch: the collector can run out of room itself, and that ends the process before anything is signalled |

**The 503 says overloaded, where silence would say crashed.** A worker at
`*max-connections*` answers before it closes, so a client can tell a full
instance from a gone one and has something to back off against. Two
consequences worth knowing before you build an alert on it:

- **The 503 is always written; whether it is seen is not guaranteed.**
  The refusal is one non-blocking write followed by a bounded drain of
  what the client already sent, capped at 8 KiB. The drain is there
  because `close(2)` on a socket holding unread data makes Linux send RST
  rather than FIN, and a client that surfaces a reset in place of the 503
  learns nothing from it.

  Both the drain and the delivery turn on the same race, and neither is
  settled by it. The refusal happens at accept time, which may precede
  the client's request arriving at all. If it does, the request lands on
  a socket that no longer exists, the kernel answers RST, and that reset
  discards whatever the client had not yet read — including the 503. On
  loopback the request always wins that race, which is why the test suite
  sees the response every time; across a real network under real
  overload, which is the only condition any of this fires in, it is a
  race like any other. A client refused mid-upload, or simply slower than
  the accept, can end up with a reset and nothing else.

  So it is not a delivery guarantee, and an alert built on counting 503s
  at the client will undercount.
- **It is not in your access path**, so it will not appear in handler
  metrics or anything else counted after dispatch. If you want to see
  refusals, watch the `connection limit reached` warning. It is logged on
  a worker's first refusal and then at most once a minute with a count,
  because a refusal comes when a worker is out of room, and a line per
  refusal would have every worker queueing on the log lock in a flood.
  For exact numbers read `:refused` in `connection-census`, which counts
  every one.

The refusal carries no `Date`. It is one frozen vector sent whole, and
building a header per refusal would put that work on the path that exists
because the worker is out of room. Static responses start from the same
constraint and answer it the other way: their bytes are stored as pieces,
with a current date line written between them per request.

`status-reason` covers the codes above plus the ones handlers commonly
need — 202, 303, 410, 411, 412, 415, 422, 428 and the usual 2xx/3xx/4xx
set. It is deliberately not exhaustive, and there is no way to supply your
own reason phrase: `format-response` always asks `status-reason`, and a status
outside the table is emitted as `HTTP/1.1 451 Unknown`. That is deliberate —
the table's own docstring says `Unknown` is the answer — but the reason phrase
has no semantic weight in HTTP/1.1, so a client reads the code and ignores it.
If you need a status the table does not carry, use it; the line is well-formed
and the code is what is read.

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

Comparing the whole Origin is right for a site that knows its public
origin. One that does not, served under whatever name its proxy is given,
can compare the Origin's host and port with the `Host` the request arrived
with instead: `%ws-origin-allowed-p` in `demo/handler.lisp` does, and says
why it leaves the scheme out and what the proxy has to pass through.

### Subprotocols, and the rest of the 101

A client that opens `new WebSocket(url, ["graphql-transport-ws"])` offers
subprotocols in `Sec-WebSocket-Protocol`, and fails the connection unless
the 101 names one of them (RFC 6455 §4.1). GraphQL subscriptions, STOMP and
MQTT over WebSocket all negotiate this way. Return `ws-upgrade` in place of
`:upgrade` to answer with one:

```lisp
((string= (http-request-path request) "/graphql")
 (let ((protocol (ws-choose-protocol request '("graphql-transport-ws"
                                               "graphql-ws"))))
   (if protocol
       (ws-upgrade :protocol protocol
                   :headers (list (cons "set-cookie" (session-cookie request))))
       (make-error-response 400 "no subprotocol in common"))))
```

- **`ws-choose-protocol`** answers the first of the names you list, in your
  order of preference, that the client offered, or `nil`.
  `ws-offered-protocols` lists what it offered, in its order. Names are
  compared exactly, as browsers compare the one the 101 returns.
- **`ws-upgrade :protocol`** must be one the client offered. Any other is
  a 500 that names it, since the client would fail the connection with
  nothing said to the server.
- **`ws-upgrade :headers`** adds fields to the 101: a cookie, a correlation
  id. The fields the handshake owns are refused (`Upgrade`, `Connection`,
  `Sec-WebSocket-Accept`, `Sec-WebSocket-Protocol`, `Content-Length`,
  `Transfer-Encoding`), and so is `Sec-WebSocket-Extensions`, which would
  agree to an extension, such as `permessage-deflate`, that the frame reader
  does not implement.
- **The handshake's checks still decide.** A request that is not a valid
  upgrade is answered 400 whatever the handler returned, and the Origin
  check above belongs before either.

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

### What a signature check costs

`jwt-verify` checks the signature on the worker handling the request, after
the cheap checks: `alg`, a known `kid`, and `exp`. Every one of those is in an
attacker's hands. The `kid` is in your public JWKS, and the attacker writes the
claims. So each forged token costs one full ECDSA verification, and sending
forged tokens is a way to spend your workers' CPU.

Without `web-skeleton-tls`, the verification is pure Lisp and takes on the
order of a millisecond of one worker's time. With it loaded,
`ecdsa-verify-p256` is OpenSSL's `EVP_PKEY_verify`, which takes a fraction of
that. Rate limit JWT-guarded routes at the proxy either way. An application
that verifies a token on every request should load `web-skeleton-tls` even if
it never fetches over HTTPS.

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

`http-fetch-stream` is **blocking** — it holds the worker thread for the
duration of the upstream call, bounded by `*fetch-timeout*` (default 30s)
across each of three setup phases. `http-fetch` is not, for either scheme;
see the async budget below.

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
   but avoid calling it from HTTP handlers under load.

**Async fetch timeout budget.** On the `http-fetch` path — **both schemes** —
`*fetch-timeout*` applies as a **single end-to-end budget** rather than per-phase:
the inbound connection's `:awaiting` idle timer covers DNS + TCP connect + TLS
handshake + request I/O together. A slow DNS phase shortens the budget remaining
for everything after it. `http-fetch-stream` still gets the three per-phase bounds
above; `http-fetch` gets one total.

This is not a tuning detail on the HTTPS path, it is the only bound there is:
`SO_RCVTIMEO` does nothing on a non-blocking socket, so nothing per-phase
bounds an encrypted read and the `:awaiting` timer is what stands in its
place. Tune `*fetch-timeout*` with this in mind —
it is the worst-case wall time the parked inbound will sit in `:awaiting`
before the idle sweeper answers **`504 Gateway Timeout`** and closes.

That 504 is the floor under every way a fetch can fail to come back,
including ones with no specific handler: an upstream that accepts and then
says nothing, a DNS lookup that produces no usable address, a response
whose framing never completes. Wherever the fetch machinery has something
more specific to say it says it — `deliver-fetch-error`'s 502 covers a
refused connect, a short body, an unparseable status line — and everything
else lands here. The fetch callback still fires its `(nil nil nil)` cleanup
sentinel exactly once, because the paired outbound is torn down through the
same `close-outbound` path either way.

**Chunked completion on the async path.** The non-blocking `http-fetch` path
detects response completion three ways: by `Content-Length`, by the
zero-size chunk terminator when the response is chunked, and by EOF for a
close-delimited one. A chunked upstream that holds the connection open
after `0\r\n\r\n` therefore completes as soon as the terminator lands,
rather than stalling until the peer closes or `*fetch-timeout*` expires.
`chunked-body-complete-p` walks the framing in the read buffer and resumes
from where the previous read stopped, so each chunk is walked once across
the transfer instead of the body being rescanned on every read.

That is the **response** side — decoding a chunked body an upstream sent
to us. The **request** side reads the same framing with the same walk,
and that sharing is deliberate: one decoder, two directions, so the
acceptance sets cannot drift apart. A disagreement about where a body ends
is only visible when both sides read the same input.

What differs is what happens after the zero-size chunk. Outbound, nothing
follows the body and the walk stops there. Inbound, on a connection that
will be reused, the bytes past that point are the next request's — so a
chunked request is not complete until the CRLF terminating its (empty)
trailer section has arrived. A body ending `...0 CRLF` and nothing else is
still incomplete; ending it two bytes early would shift that CRLF to
offset 0 and let it start the next request.

A non-empty trailer section is refused with 400 rather than consumed.
Consuming it would need its own byte bound and its own header-field
validation — a second header parser, whose disagreement with the first is
the exact hazard the CL-TE rules exist to close — and nothing here
surfaces trailers to an app, so accepting them would silently discard data
the client believed it sent. Refusing also means the request boundary for a
trailer-bearing request is never computed at all, which is the strongest
available answer to a trailer carrying a whole smuggled request.

The CL-TE disagreement stays unrepresentable, and by refusal rather than by
absence: `Transfer-Encoding` together with `Content-Length` is 400 on
presence alone — the Content-Length's *value* is never consulted, so a
value this server would reject for its own reasons still answers as the
pair. Only a bare final `chunked` is accepted. `gzip, chunked` is 501,
legal and unimplemented; `chunked, gzip`, a repeated Transfer-Encoding
header, an obs-folded value, and `Transfer-Encoding` on HTTP/1.0 are 400.

**The body cap for a chunked request.** The cap is `*max-body-size*`, or
the one a body handler chose. The Content-Length path checks
the declared length once, before allocating. A chunked request declares
nothing, so the same limit is applied as the decoded total grows, and also
to any single chunk header that declares more than the cap on its own —
the latter matters because waiting for bytes a client will never send only
ever reaches the read buffer's answer, which describes the buffer and not
what the client did.

Two 413s are therefore reachable for one chunked request and they name
different limits: `chunked body too large` is the body cap, `request too
large (buffer full)` is the read buffer. Neither is redundant. The wire
carries framing the decoded body does not — a 1-byte chunk costs six wire
bytes — so a body made of very small chunks fills the buffer well before
its decoded total reaches the cap, and a body made of large ones
does the opposite. If you alert on 413s, the reason string is the part
that tells you which knob to turn.

**`SSL_ERROR_SYSCALL` discipline.** OpenSSL returns `SSL_ERROR_SYSCALL` for
several distinct conditions and they must not be collapsed. `errno = 0` is
end-of-stream without `close_notify` — benign, and load-bearing, because it
is the framing signal HTTP/1.0-style servers actually use. That is OpenSSL
1.1.1's shape. OpenSSL 3 reports the same close as a fatal error unless told
otherwise, so the shared context sets `SSL_OP_IGNORE_UNEXPECTED_EOF`, under
which 3.x reports it as a clean end instead and the two agree. `errno = EAGAIN`
is would-block. Everything else (`ECONNRESET`, `EPIPE`, `ETIMEDOUT`) is a
real transport failure and raises loudly, because that is the
MITM-RST-mid-stream case: an attacker truncates a response, and a silent
end-of-stream here would hand the application a partial body as success.

What `EAGAIN` *means* depends on the socket, which is why one classifier
answers it and two callers read it. On a socket left in blocking mode with
`SO_RCVTIMEO` installed, a read cannot return would-block unless the receive
timeout expired, so `ssl-blocking-read-eof-or-raise` turns it into the loud
timeout this document promises — that is what bounds the HTTPS read path for
close-delimited responses and for `http-fetch-stream` over HTTPS. OpenSSL
reports that expiry as `SSL_ERROR_WANT_READ` rather than as `EAGAIN` under
`SSL_ERROR_SYSCALL`, so on this socket both read as the timeout, and the
blocking write side reads `WANT_WRITE` the same way. On a
non-blocking socket `SO_RCVTIMEO` does nothing at all, so `EAGAIN` means only
what it says and the event loop waits for readability; there the bound comes
from the parked inbound's timer rather than from the socket.

Operationally, a truncated *framed* response is an error on either path: a
`Content-Length` short of its body meets `complete-fetch`'s truncation guard,
and a chunked body with no terminator meets `decode-chunked-body`'s raise.
Both become a 502.

A close-delimited response has no framing to fall short of. It ends where the
connection ends, so a FIN injected mid-body ends it early and reads as
complete — on 1.1.1 by default, and on 3.x under the option above, which
OpenSSL documents for protocols that can detect a truncation themselves. HTTP
can when it is framed, and cannot when it is not. An RST is still the loud
failure the paragraph above describes; it is the clean close that cannot be
told from an honest one. An upstream whose responses matter should send
`Content-Length` or chunked.

**Framing headers are the framework's, not yours.** Passing either
`Transfer-Encoding` or `Content-Length` in `:headers` signals an error
rather than going on the wire. Both are ways for a request's declared
framing to disagree with the bytes that follow (RFC 7230 §3.3.3), and
either one aimed at an upstream is a request-smuggling primitive:

- `Transfer-Encoding` — the framework chunk-encodes only a streamed body
  with no declared length, and writes that header itself, so a caller's
  claim is false for any other body. It does not suppress the computed
  `Content-Length`, so the request would have carried both framing
  headers at once.
- `Content-Length` — a caller-supplied one *does* suppress the computed
  one, so `"5"` in front of a ten-byte body would have left `56789` in
  the upstream's buffer as the head of the next request. This is the one
  reached by accident: a stale `content-length` copied along with the
  rest of a header alist.

For a deliberate `Content-Length: 0` on a bodiless POST, pass `:body ""`
— an empty body is still a body, and the header is computed from it. A
streamed body's length is declared with `:content-length` instead, and the
bytes are held to it: `fetch-send` signals past it, and `fetch-finish`
short of it.

### Fetch callback contract

The `:then` closure supplied to `http-fetch` / `defer-to-fetch`
fires **exactly once per fetch** with one of two argument shapes:

- **`(status headers body)`** on the happy path — `status` is an integer HTTP status,
  `headers` is an alist of lowercase-name string pairs,
  `body` is a byte vector (or `NIL` for empty bodies).
  A header value is decoded as UTF-8 when it is valid UTF-8, and as
  Latin-1, one character per byte, when it is not, so a legacy server's
  Latin-1 filename arrives as text rather than failing the fetch. A
  request header that is not UTF-8 is still refused with `400`.
  The closure's return value becomes the response
  delivered to the original inbound client.
- **`(NIL NIL NIL)`** as a cleanup sentinel on every abnormal teardown path:
  upstream TCP / TLS / DNS failure, a body that stopped early,
  inbound connection closed mid-fetch, drain, worker crash.
  A body stops early by falling short of a declared `Content-Length`,
  or by ending without its chunked terminator — and `:on-body` having
  already delivered chunks does not make a truncated response complete.
  The closure's return value is discarded on this path
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

### TLS trust anchors

**A missing CA store raises rather than warning.** `SSL_VERIFY_PEER` is
set, so a process with no trust anchors fails every handshake regardless.
Raising is not what makes it fail-closed; it is what makes the failure
legible, said once at the first HTTPS fetch rather than left to be
inferred from an unrelated-looking stream of handshake errors.

This bites on distroless and scratch images. Install a CA bundle
(`ca-certificates`), or point `SSL_CERT_FILE` / `SSL_CERT_DIR` at one. A
plain-HTTP server on such an image still boots: the check runs when a TLS
connection is opened, not at startup, so nothing that never fetches over
HTTPS is affected.

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
6to4 relay anycast (`192.88.99.0/24`), and cloud metadata IPs. It unwraps
IPv4-mapped IPv6, NAT64, and 6to4, so an attacker cannot launder
`127.0.0.1` as `::ffff:127.0.0.1`. On IPv6 it states what public is rather
than what it is not: only global unicast, `2000::/3`, can be public at all,
so a special-purpose block assigned later out of reserved space is refused
before anyone has written a clause for it. Its docstring lists every range.

The 6to4 relay prefix is refused *because* RFC 7526 deprecated it. The
relays are gone, so the prefix is still globally routed but goes nowhere
in particular — whoever announces it receives the traffic. "Deprecated"
reads like a reason to stop worrying about a range; here it is the reason
to refuse it.

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
until the address lands. Every TCP-compatible line (`<address> STREAM`)
that `*fetch-address-filter*` accepts is kept, in the order `getent`
printed them, which is the system's preference (RFC 6724). The first is
dialed, and a connect that fails gives way to the next:
- **Refused or unreachable** — the next address is dialed at once.
- **Unanswered** — a connect that has not completed in
  `*connect-fallback-delay*` seconds (2, RFC 8305's longest attempt delay)
  gives way to the next while there is one. The last address has what is
  left of `*fetch-timeout*`, which runs from the first attempt throughout.
- **The request goes with it.** A streamed body's pieces sent while the
  first attempt was connecting are sent on the next.

So a name whose first address the upstream does not answer on still
reaches it: `localhost` resolving to `::1` first while the service binds
`127.0.0.1` alone, or a name whose IPv6 route goes nowhere.
`AI_ADDRCONFIG` drops a family only when this host has none of its
addresses, so such a name sorts IPv6 first on any host with IPv6. The
unanswered case costs `*connect-fallback-delay*` before the next address
answers, since attempts are made one at a time rather than raced.
Numeric literals (including bracketed IPv6 forms like `http://[::1]:8080/`)
skip the subprocess entirely via the numeric fast path, and have no other
address to fall back on.

`http-fetch-stream`, over either scheme, uses the same `getent` subprocess
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

### The write backlog bound

A connection holds one buffer being flushed and, on the paths that queue,
a list of vectors waiting behind it. `*max-write-backlog*` caps the two
together, 2 MiB by default.

**It has to clear `*max-ws-message-size*` by at least 10 bytes**, the
largest header `build-ws-frame` emits, or a maximal legal message cannot
be sent even onto an empty queue. The receive path accepts a payload of
exactly `*max-ws-message-size*`; framing it for the trip back costs the
extended-length header, so setting the two equal hands an echo handler a
message it is then refused permission to return. Both are exported and
tunable apart, so this is a requirement to keep rather than an identity
to lean on — the default leaves a full MiB of room, not the ten bytes
that would technically satisfy it.

The bound exists because a producer and its peer run at different speeds.
An app pushing events faster than a phone on a train can read them has to
be stopped somewhere, and the alternative to a bound is a per-connection
list that grows until the worker dies.

A send that would cross the bound is refused whole rather than truncated:
a short frame is a protocol error on the peer's side, while a refused one
leaves the stream well-formed and short. What the refusal *means* is the
caller's to decide, and the two answers differ. An app-generated stream
should close — a dropped event is invisible to the client, so its view
diverges from the server's permanently with nothing raised anywhere.
Something forwarding an upstream should stop reading it instead, because
the client has not misbehaved, and letting the upstream's TCP window fill
turns a killed download into a slow one. That second disposition is why
the bound exposes a state rather than picking an answer, and `fetch-into`
below is what builds it: an `:on-body` that answers `:PAUSE` stops reading
the upstream, whose window then fills, and `fetch-resume` starts it again.

Per connection, so the ceiling is `*max-write-backlog*` × `*max-connections*`
× workers — the same shape as the read-buffer arithmetic above, and it takes
every connection on the box being simultaneously backed up to reach it.
Lower it for many connections and a generous `ulimit`; raise it for few
connections and bursty output.

The bound holds what a producer sends, the case it exists for: `ws-send`,
`stream-send` (a streaming response or SSE, and a relay into either), and
`fetch-send` on a streamed request body. A complete response, a handler's
or a static file's, is queued as one act outside it: held to the bound, a
finished file larger than the bound would have its body refused after its
head had gone out promising one.

### Reading a response incrementally

`http-fetch` takes an `:on-body` callback, called `(conn bytes)` with each
chunk of a chunked upstream response as its framing is proved. The handler
sees the body as it arrives rather than only once the whole of it has been
buffered, so a large response can be processed without being held:

```lisp
(defun handle-count (req)
  (declare (ignore req))
  (let ((lines 0))
    (http-fetch :get "http://upstream.internal/feed"
                :on-body (lambda (out chunk)
                           (declare (ignore out))
                           (incf lines (count 10 chunk)))
                :then (lambda (status headers body)
                        (declare (ignore headers body))
                        (if (eql status 200)
                            (make-text-response 200 (format nil "~d~%" lines))
                            (make-error-response 502))))))
```

`status` is tested rather than ignored, and `EQL` rather than `=` because
the cleanup sentinel passes `NIL`. Answering `200` with a count from a
fetch that never completed is the failure that test exists to stop.

**A relay holds one chunk at a time.** Each chunk is freed once `:on-body`
has it, so a stream of any length passes through a buffer the size of the
response head plus its largest chunk. `*max-outbound-response-size*` bounds
that buffer, not the stream. A single chunk larger than the cap still fails
the fetch with a `502`.

**Copying an upstream's headers onto your response takes a filter.**
`:then` receives the body decoded and whole, while `headers` still
describes it the way the upstream sent it: `Transfer-Encoding: chunked`,
or a `Content-Length` for bytes that are no longer there. `format-response`
refuses a caller's `Transfer-Encoding`, and a `Content-Length` the body
contradicts, so a relay that copies everything is answered `500`. Leave out
the framing headers (`transfer-encoding`, `content-length`) and the
hop-by-hop ones (`connection`, `keep-alive`, `te`, `trailer`, `upgrade`,
`proxy-connection`, and any header the upstream's `Connection` names), and
copy the rest.

**To forward the bytes onward, use `fetch-into`.** A handler-returned
continuation feeds the handler that returned it and nothing else. A
`:streaming` connection or a WebSocket owns its own write path, and
`fetch-into` starts a fetch against one:

```lisp
(defun handle-relay (req)
  (declare (ignore req))
  (make-stream-response
   :on-open
   (lambda (client)
     (fetch-into
      client
      (http-fetch :get "http://upstream.internal/feed"
                  :on-body (lambda (out chunk)
                             (declare (ignore out))
                             (stream-send client chunk)
                             nil)
                  :then (lambda (status headers body)
                          (declare (ignore headers body))
                          ;; Only a delivered response ends with a terminator.
                          ;; A NIL status is a failed fetch: closing here would
                          ;; tell the client a truncated body was complete,
                          ;; and left open, the framework closes it without one.
                          (when status
                            (stream-close client))
                          nil))))))
```

It returns `T`, or signals — a wrong state, a second argument that is not
a continuation, no event loop, or a fetch already outstanding on that
connection. It signals as well for a fetch that cannot be started: a URL
that does not parse, `https://` without web-skeleton-tls, an IP literal
`*fetch-address-filter*` refuses, or an address the kernel will not dial
when the dial is made inside the call, for an IP literal or a name the DNS
cache answers. A name that has to be looked up is dialed after
`fetch-into` has returned, and a refusal of its address reaches `:then`
instead, as the abort sentinel. A signalling call has done nothing and
the callback does not fire, so `:then` can never run before `fetch-into`
returns. `:then`'s return value is discarded here; there is no parked
request to answer.

**When the fetch ends, the framework decides what becomes of your
connection.** What it decides depends on the connection's kind and on how
the fetch ended, and the two kinds are not treated alike:

| target | ending | what happens to your connection |
| --- | --- | --- |
| already closed | any | nothing |
| `:streaming` | delivered | `stream-close`, terminator written |
| `:streaming` | failed | closed without a terminator, so the peer's decoder sees truncation rather than being told a failed body was complete |
| `:websocket` | delivered | nothing; you own your framing |
| `:websocket` | failed | a `1011` close frame, then the connection goes — a row you can override, with `:failure-disposition :keep` |
| receiving a body | delivered | nothing; your `:then` resumes or refuses the body |
| receiving a body | failed | closed, the body's `:on-abort` hearing `:upstream-failed` — overridable with `:keep` too, leaving `:then` to refuse it (see "Receiving a body as it arrives") |
| any | stopped | nothing; you asked for the ending, so what follows is yours |

It decides rather than leaving it to you because a handler written for the
happy path will not have a failure path, and a `:then` that only closes on
success leaves a failed stream open until `*stream-idle-timeout*` — or a
failed WebSocket until `*ws-idle-timeout*`, which defaults to a day.

**A fetch lasts no longer than what it was started for.** A body's fetch
ends with the body, once `:on-end` has answered, unless `:on-end` answers
with that fetch. A stream's fetch ends once the stream has closed and its
response has gone, and any fetch ends with its connection. One still
running then is closed, its `:then` hearing the abort sentinel, and
nothing in the table is applied: what it was for has gone, and the
connection may be on to its next request, or to the stream a body was
answered with. A `:then` that answers that sentinel with another
`fetch-into` on the connection is refused. A fetch whose result matters is
one to wait for: a body pauses on it, or `:on-end` answers with it, and a
relay closes its stream from `:then`.

"Failed" above is the framework's `:aborted` outcome, which is the word the
source uses and the one you will see in a log line. It is spelled out here
because the parameter is named `:failure-disposition` and the outcome
keyword is not, and a reader should not have to make that jump unaided.

**On a `:streaming` target this is a protocol obligation.** The framework
owns the terminator and writing one is a claim that the body is complete,
so withholding it on failure is the only honest thing it can do.

**On a WebSocket it is a policy, and it is worth knowing before you port
to `fetch-into` over one.** The framework has already disclaimed your
framing on the delivered row; on the failed one it sends `1011` — RFC
6455's "internal error" — and the connection closes behind it. The frame
carries the code and nothing else, so a client sees a bare `1011` with no
reason string.

**That row, and only that row, has an opt-out.** An application with its
own failure frame — an error rendered in the page, a retry the client
drives — would otherwise send it and have the connection closed underneath
it anyway. Pass `:failure-disposition :keep` and the framework leaves the
connection alone when the fetch fails:

```lisp
;; the default, unchanged
(fetch-into conn continuation :failure-disposition :close)

;; :websocket targets and bodies being received; never :streaming
(fetch-into conn continuation :failure-disposition :keep)
```

The fetch contract does not change: `:then` still fires exactly once with
the abort sentinel under either setting. `:keep` governs the connection,
not the delivery.

**It is refused on a `:streaming` target rather than ignored there.** On
that path the disposition is the framing, and honouring `:keep` would hand
the client a body with no terminator — the failure the whole rule exists
to prevent. `fetch-into` signals at the call, where you are on the stack to
read it, rather than disregarding the argument at a teardown that has
nothing left to answer.

**What `:keep` transfers is the failure path.** The default exists because
an application will not handle a case it has never seen fail, and choosing
`:keep` is asserting that you have. That includes not leaving a dead socket
parked until `*ws-idle-timeout*`, which defaults to a day. If you are not
going to close it, do not keep it.

**A stopped fetch is not a failed one, and this parameter does not govern
it.** Returning `:stop` from `:on-body` leaves the connection alone under
`:close` as well as `:keep` — it never reaches the disposition at all. The
inference the other way is reasonable and wrong: the abort sentinel is
what `:then` receives, `:failure-disposition` is about aborted fetches, so
a stop under the default should close the connection. It does not, because
the sentinel and the disposition are separate facts.

**Closing the connection yourself, or chaining, suppresses all of it.** A
target that is already gone gets nothing. And the outstanding marker is
cleared before `:then` runs precisely so `:then` can set it again by
calling `fetch-into`: that is how a detached fetch chains — a
handler-returned one chains by returning a continuation, this one by
calling `fetch-into` again — and closing the connection the next fetch is
about to produce into would make chaining impossible.

**`:then` still fires exactly once, with a NIL body.** The bytes went out
incrementally; handing them over again would double the memory the
callback exists to avoid.

**Chunked framing only, and it degrades rather than disappearing.** A
`Content-Length` or close-delimited response has no chunk walk to hand
bytes back from, so `:on-body` is never called and `:then` receives the
whole body the ordinary way — not incremental, but not lost. You do not
choose which framing an upstream uses: the same origin will switch by
response size or by whatever proxy sits in front of it. Write the handler
to take the bytes from `:on-body` when they arrive there and from
`:then`'s body when they do not.

`:on-body` behaves identically over `https://`: both schemes take the same
path, so there is no second implementation to differ from. It is the
framing that decides, not the transport.

**As strict as the buffered path.** A relayed body is never decoded after
the fact — its chunks were handed on as they arrived — so the walk that
hands them over is the only reader the body meets, and it refuses every
framing `decode-chunked-body` refuses, before the chunk concerned reaches
`:on-body`. A malformed upstream body fails the relay where a buffered
fetch of it would answer 502: `:then` gets the abort sentinel and the
target its failure disposition.

**Chunk-granular, not line-granular, and deliberately.** Line splitting
already exists once, on the blocking path, with CR/LF/CRLF handling and
partial-line state carried across reads. A second implementation here
would be two readers that could disagree about where a line ends, which
is the disagreement this codebase treats as its threat model. Split what
you are given if you want lines.

**Backpressure is a return value, and resuming it is yours to do.**
Return `:pause` from `:on-body` to stop reading the upstream — its send
window fills and the pressure propagates back without anything being
dropped or buffered. The pause takes effect at that chunk: no later chunk
is handed over, however many the last read brought, until the fetch
resumes, and then those are handed over first. Call `fetch-resume` on the
connection `:on-body` was handed to start again; it is idempotent, and a
no-op once the fetch has ended, so a producer that calls it on every pass
rather than tracking state is not punished for it.

**A relay's pause ends by itself, and the condition is narrower than it
sounds.** Reading resumes when the connection being relayed into drains
its write backlog — the event the pause was waiting for. That needs the
target to have *actually backed up*. `stream-send` and `ws-send` flush
inline, and a flush that completes never reaches the event loop's write
path, so a pause taken while the target's queue was empty has no drain
coming, and only an explicit `fetch-resume` restarts it.

So pause on what the send said. `stream-send` answers `T` when the chunk
went straight to the kernel and `NIL` when part of it is still queued, and
`NIL` is exactly the case with a drain coming:

```lisp
:on-body (lambda (out chunk)
           (declare (ignore out))
           ;; NIL: part of this chunk is queued, the client is behind, and
           ;; its queue draining is what resumes the fetch.
           (unless (stream-send client chunk) :pause))
```

Because the pause stops at that chunk, the client's queue never holds more
than one chunk's remainder, and `stream-send` never meets
`*max-write-backlog*` — unless a single chunk is larger than the backlog
by itself, which it refuses. Size `*max-write-backlog*` above the largest
chunk the upstream sends. `stream-full-p` is not this test: it reports a
queue already at the bound, and a send can be refused before the queue gets
there.

The same shape works for a `:websocket` target, since `ws-send` answers the
same way: `(unless (ws-send client (build-ws-frame +ws-op-binary+ chunk))
:pause)`. What you must not do is return `:pause` unconditionally and rely
on something to lift it: nothing will, unless the queue was non-empty when
you said it.

The deadline runs on unpaused time: `fetch-resume` pushes it out by the
interval spent paused, so a relay is not killed for applying the
backpressure it was told to apply. While the client is still draining, the
pause is bounded by the client's `*write-stall-timeout*` rather than by the
deadline: a slow link draining a full queue can take longer than
`*fetch-timeout*`, and that time is the client's. A queue that stops
moving closes the client, which ends the fetch with it. A pause over an
empty queue has nothing coming to end it, and the deadline still applies.

**Outside a relay, resuming is yours to do, and forgetting is fatal to
that request.** `:pause` is what stops `:on-body` firing, so the callback
cannot be what notices — an app whose only route back was a callback that
is no longer running has no route back. The fetch sits until
`*fetch-timeout*` and the parked caller is answered `504 Gateway
Timeout`, on an upstream that was healthy the whole time.

So pause where a drain or something else will resume it — a timer, a
later request, a queue the app is itself watching. If there is no such
thing, do not pause.

**Stopping a detached fetch: return `:stop` from `:on-body`.** The
outbound is closed, the connection you were fetching into is left exactly
as it was, and `:then` fires with the abort sentinel — a NIL status, the
same one a failed fetch delivers. There is no resume; a stopped fetch is
over. This is the answer to an output cap being reached, or to a result
arriving from somewhere else first: stop paying for a response you have
stopped wanting, without giving up the connection you were producing
into.

The connection can start another fetch immediately, including from inside
the stopped fetch's own `:then`, which is where an application usually
notices it wants to.

**The sentinel is deliberate, and it is not ambiguous in practice.** A
stopped fetch is not a delivered one, and reporting a real status over a
body the caller cut short is the silent truncation the framework refuses
everywhere else. Telling a stop from an upstream failure is the
application's own to do, and it is in a position to: a stop can only
originate inside a callback of the fetch being stopped, so the code that
returns `:stop` can set its own flag on the way.

**It takes effect at that chunk.** As with `:pause`, no later chunk is
handed over, however many the last read brought, and a response whose
terminator had already arrived does not override it. What `:then` is told
never depends on where the upstream's bytes happened to be split: a pass in
which `:on-body` returned `:stop` ends the fetch as stopped.

**Closing the target is the other way to end one, and it costs the
connection.** `close-connection` walks for detached outbounds and reaps
them, which is what makes a client going away an immediate ending rather
than a leak, and is asserted directly — "a client that leaves mid-relay
tears the outbound down, once." Before `:stop` existed it was the only
lever an application had, which is why the difference is worth naming.

**On a handler-returned fetch, `:stop` fails the request.** `:on-body`
belongs to `http-fetch`, so the verdict is reachable from a fetch a handler
returned — where there is no connection of yours to keep, only a client
parked with no answer. There the stop ends the fetch and the parked client
gets `502`, the same code every other ending that produces no response on
that path already gives. `:then` still fires once with the abort sentinel;
its return value is discarded, exactly as it is for any other failed fetch
on that path.

That is a defined outcome rather than a useful one, and the difference is
worth being plain about. Closing the outbound and leaving the client parked
would be worse — a request that hangs to `*fetch-timeout*` and then dies —
and reporting success would be the truncation the verdict exists to
prevent. But `502` says the gateway failed, and what happened is that your
application cancelled. **"Stop the upstream and let me answer the client
myself" is not expressible.** If that is what you want, do not stop
the fetch: let it finish and answer from `:then`, which is the one place a
handler-returned fetch's response comes from.

**Know which primitive you are holding.** A non-local exit from
`http-fetch-stream`'s `:on-line` unwinds through that call's
`unwind-protect`, closing the socket and stopping the upstream while the
caller carries on; it is a blocking call, so there is a stack to leave. A
detached fetch has no call to exit from, and `:stop` is what it has
instead.

A value rather than a condition, for the same reason
`connection-append-write` refuses by return: applying backpressure is
ordinary control flow and should not unwind through the middle of a read
loop.

`fetch-resume` does not wait for more bytes to arrive. A pause can leave
chunks already read in user space, where no readability edge will report
them — the warning elsewhere in these docs about `EPOLL_CTL_MOD` not
re-firing for buffered data is about exactly this. So the resume wakes the
fetch through writability, which its socket has at once, hands those
chunks over first, and then reads.

`*fetch-timeout*` stays a **total** on this path, not an inactivity
bound. It is the only end-to-end network deadline the framework has, and
it is what distinguishes the async path from every blocking one. A fetch
that legitimately runs long wants a larger number, not a different shape
— converting it to idle would delete the guarantee and add one more
entry to the trickling-upstream list.

### Streaming responses

A handler that returns `make-stream-response` gets the head serialized
immediately — status, headers, no `Content-Length` — and is then handed
the connection through `:on-open`. It produces the body with `stream-send`
and ends it with `stream-close`.

```lisp
(defun handle-events (req)
  (declare (ignore req))
  (make-stream-response
   :headers '(("content-type" . "text/plain"))
   :on-close (lambda (conn reason)
               (declare (ignore conn))
               (unsubscribe-from-feed reason))
   :on-open (lambda (conn)
              (subscribe-to-feed
               (lambda (event) (stream-send conn event))))))
```

**The handler returns while the response is still being delivered.** That
is the whole point: `:on-open` queues what it has and returns, and the
worker goes back to the event loop. A stream costs a connection slot, not
a worker. Nothing here is synchronized, though — `stream-send` is safe
from the worker that owns the connection and nowhere else, so an app
doing fan-out holds its own registry and pushes from the owning worker.

`stream-send` will not tell you when you get that wrong. `ws-send`,
`stream-close` and `fetch-into` all check the connection table and raise;
`stream-send` does not, so a cross-worker call returns `T` unless the
write happens to leave a remainder. Treat the rule above as the whole
enforcement for this one function — see README.md's Limitations for why
it is the exception.

Framing follows the client. HTTP/1.1 gets `Transfer-Encoding: chunked`;
HTTP/1.0 cannot read chunked at all, so it gets close-delimited framing
with `Connection: close` and the socket goes when the stream ends. Both
are handled for you — the app sends bytes and never sees a chunk header.

**The framework owns the terminator.** `stream-close` writes it; there is
no way for an app to write one and no way for it to forget. Any other way
a stream ends — the peer disconnects, a deadline fires, the server drains
— closes the socket instead, so an unterminated chunked body is never
followed by a reused connection. That matters more than it sounds: the
next response's status line landing where a downstream reader expects a
chunk-size is a request-smuggling primitive, and it would be one we built
ourselves.

`:on-close` fires exactly once however the stream ends, with a reason:
`:done` when the app closed it, `:disconnected` when the peer went away,
`:idle` or `:stalled` when a deadline took it, `:upstream-failed` when a
`fetch-into` relaying into it failed or its close could not be written,
`:error` when `:on-open` raised after the head went out or the worker
carrying the stream crashed, `:shutdown` on server drain, and `:closed`
for a close that names no reason of its own. Register teardown there
rather than after `stream-close`, because most of those reasons never
reach the app's own code path.

Three deadlines apply, and they answer different questions:

| Knob | Question |
|---|---|
| `*stream-idle-timeout*` | Is the app still producing? |
| `*write-stall-timeout*` | Are bytes still leaving for the peer? |
| `*max-write-backlog*`   | How much may pile up before we give up? |

A stream can be perfectly healthy at the socket and dead at the source,
which is why the first two are separate. `*stream-keepalive-interval*`
refreshes the first — but only if the `make-stream-response` supplied
keepalive bytes, because there is nothing generic to send. A chunked
stream's only zero-content emission is the empty chunk, and that is the
terminator; anything that keeps a stream warm is content at the layer
above.

A `HEAD` to a streaming endpoint gets the headers a `GET` would have got
and no body, per RFC 7231 §4.3.2. No stream starts and `:on-open` is not
called, so a handler that opens a resource there is not left holding one.

Data arriving from the client mid-stream is discarded and the connection
is marked not to be reused. A pipelined request behind a stream would
have to wait for the stream to end, and the stream may never end — a
clean close is retryable, a silently dropped request is not.

### Server-Sent Events

`make-sse-response` is a streaming response with the SSE framing on top.

```lisp
(defun handle-events (req)
  (declare (ignore req))
  (make-sse-response
   :on-open (lambda (conn)
              (subscribe (lambda (row)
                           (sse-send conn :data (row-json row)
                                          :event "update"
                                          :id (row-id row)))))
   :on-close (lambda (conn reason)
               (declare (ignore conn))
               (unsubscribe reason))))
```

Three headers are set for you and win over anything you pass:
`content-type: text/event-stream` defines the protocol, `cache-control:
no-cache` stops an intermediary serving a stale prefix of a response that
never ends, and **`x-accel-buffering: no`** turns off nginx's buffering,
which is on by default for this content type. Without that last one the
events arrive in batches when a buffer fills, which is not a stream. The
deployment story here assumes a proxy in front, so it is not decoration.
An app that needs different proxy hints should build a
`make-stream-response` directly rather than fight these.

**Field values carrying a line break are refused, not escaped.** CR and
LF both end a line for `EventSource`, so either one inside an `event` or
`id` value ends that field early and hands the client whatever follows as
a new field — or, on a blank line, dispatches an event the app never
wrote, with a type it never chose. NUL is refused too, for a quieter
reason: the spec has the client discard an `id` containing one, so the
last-event-ID never updates and a reconnect replays from the wrong point
with nothing raised anywhere.

`data` is the exception and the only one: a line break in it is the
protocol's own mechanism for multi-line payloads, and becomes one `data:`
line per segment, which the client rejoins with LF. A **CR** in `data` is
still refused, because the client's rejoin uses LF and passing one
through would silently rewrite your bytes.

The whole event is validated before a byte is queued, so a rejected field
leaves the stream well-formed and short rather than half-written. Half an
event on the wire is worse than none — the client splices it onto
whatever comes next.

**An event with no data is refused.** `EventSource` returns early on an
empty data buffer, so such an event would look sent from the server and
be dispatched to nobody. For a keepalive use `sse-comment`, which is the
one emission that legitimately carries no data — that is also why it is a
separate function rather than an empty event.

`sse-comment` sends one by hand, for a producer that wants to nudge an
intermediary on its own schedule rather than waiting for the sweep.

By default the response installs a bare comment line as its keepalive,
sent whenever the stream goes quiet for `*stream-keepalive-interval*`.
That is what stops an intermediary reaping an idle stream, and it counts
as production, so `*stream-idle-timeout*` never reaps a stream that is
emitting them. Pass `:keepalive nil` to turn it off.

### ws-send and the write queue

`ws-send` queues a WebSocket frame and flushes whatever the socket will
take right now. It does not block, and it may return with bytes still
queued — that is what a slow peer looks like, not an error. The event
loop finishes the remainder.

Call it from within `ws-handler` to send multiple frames
during a single handler invocation — the event loop is paused while the handler runs,
so there is no write contention.

It is also callable from a fetch callback on a `:websocket` target, which
is what a relay does. That runs on the *outbound* connection's read path
rather than the target's, so nothing downstream is going to arm the
target's write interest afterwards — `ws-send` arms it itself, and only
when a flush leaves a remainder behind.

```lisp
(defun handle-ws-message (conn frame)
  (when (= (ws-frame-opcode frame) +ws-op-text+)
    ;; Stream results back as they become available
    (dolist (chunk (generate-chunks (ws-frame-payload frame)))
      (ws-send conn (build-ws-text chunk)))
    nil))  ; return nil — we already sent our responses
```

The worker thread is blocked for the duration of the handler call. With
multiple workers this is fine for bounded work (e.g. streaming an LLM
response for a few seconds), but avoid unbounded blocking — that is your
code on the worker thread, and no framework change removes it.

**`ws-send` contributes nothing to that.** The frame goes onto that
connection's write queue and `ws-send` returns. A peer that is keeping up
still gets incremental delivery, because the flush happens on the spot
rather than waiting for the handler to finish; a peer that is not
accumulates a backlog instead of freezing anything. A send that waited for
every byte would hold the worker instead — the event loop is paused while
a handler runs, so one peer that stopped reading would freeze every other
connection on that worker, which with `(cpu-count)` workers is 1/N of the
server. **The blast radius is one connection.**

Two limits bound what is left, and they answer different questions.
`*max-write-backlog*` is how much may pile up; `*write-stall-timeout*` is
how long it may sit **without moving**. Cross either and that one
connection is closed. The deadline is measured from the last forward
progress on the queue and not from the connection's last activity, so a
peer that keeps sending while refusing to read cannot keep its own
backlog alive.

Both are checked when the server starts, and `*max-write-backlog*` has a
floor as well as a meaning: it must clear `*max-ws-message-size*` by at
least ten bytes, the largest frame header. Below that the receive path
accepts a payload the send path is then refused permission to return, so
an echo handler is handed a message it cannot give back. The defaults
leave a full MiB of room; the configuration that reaches the floor is the
obvious one, a deployment trimming memory by lowering the backlog and not
the message size. `start-server` refuses to boot rather than letting it
surface on the first maximal message.

`*write-stall-timeout*` applies to every connection with a backlog,
whatever state it is in — WebSocket frames, server-sent streams, ordinary
responses to a client that stopped reading. Scoping it to WebSocket
connections would leave long-lived streams — the state most likely to
build a backlog — as the one state with no stall bound at all, and a
WebSocket-shaped name would send anyone tuning a stalled SSE stream
looking in the wrong place.

It must be positive, and `start-server` refuses to start otherwise.
`ws-send` checks it too, but only `ws-send` does — a handler that returns
a frame rather than pushing one appends through a path that never sees
it, so the startup check is what actually backs the promise.

**It is an inactivity bound, not a total.** It restarts every time the
peer accepts any bytes at all, so a client reading one byte per interval
holds its connection open indefinitely. That is the deliberate trade for
not holding the worker: a total deadline would bound the frame instead of
the stall, and the cost here is capped at one connection slot plus
`*max-write-backlog*` rather than 1/N of the server. If your deployment
needs a hard ceiling on how long a single peer may occupy a slot, that is
the proxy's job, not this one's.

That settles the advice for fan-out: one unresponsive subscriber costs
that subscriber its own connection and nothing else, so a `ws-send` loop
is a reasonable broadcast rather than a stall waiting for its slowest
member. `ws-send` returns NIL when it leaves a remainder, so a broadcast
loop that wants to know which subscribers are falling behind can see it
without tracking anything itself.

`handle-client-read` arms the connection it was woken for — the one whose
handler is running. A frame pushed to *another* connection has nothing
downstream to arm it, so `ws-send` arms the connection it wrote to
whenever it leaves a remainder. Without that, a lagging subscriber would
get a truncated message, with every `ws-send` reporting success, and then
a `*write-stall-timeout*` close.

A `ws-send` to a connection belonging to a *different worker*
raises, and the raise is caught by the handler-case around the handler that
made the call. That closes the sender's connection, not the subscriber's.
Fan out only over connections this worker owns; a registry that spans
workers needs a different mechanism, which the sharing note above says the
framework does not provide.

The check is an explicit one — is this fd on *this* worker's connection
table — and it runs before the frame is queued. Arming was doing that job
by accident for a while, since `epoll_ctl` on an fd this worker's epoll
instance does not hold gives `ENOENT`, and file descriptors are
process-wide while epoll instances are not. But arming only happens when
the flush leaves a remainder, so the accident caught a cross-worker send to
a *backed-up* peer and waved through the far more common one to a peer that
was keeping up — appending to the unsynchronised queue, calling `send(2)`
from the wrong thread, and returning success.

**It catches the wrong worker, not the wrong thread.** Asking whether an fd is
on *this worker's* table requires being on a worker, so a thread that is not
one — an application's own timer, a queue consumer, a background pump feeding a
subscriber registry — finds no table, skips the check, and gets the
unguarded behaviour in full. `stream-send` and `stream-close` are the same.
Write to a connection only from the callback the framework handed it to you in.
The refusal narrows what a mistake inside that rule costs;
it does not replace the rule.

### Logging holds the only shared lock

`log-msg` takes a single global mutex and holds it across both the
`format` and the `force-output`. It is the one lock every worker contends
for — connections, the DNS cache, the scratch buffers and `/dev/urandom`
are per-worker precisely so that the request path needs none.

At the `:info` default this costs nothing measurable, because a request
that parses and dispatches cleanly logs nothing at all. At `:debug` it is
several acquisitions per request with every worker serialized behind
them, which is worth knowing before turning `:debug` on under load rather
than after.

Two operational consequences:

- **`*log-stream*` pointed at a slow consumer stalls the server, not one
  connection.** A pipe to a log shipper that stops reading leaves the
  blocked `force-output` holding the lock while every worker queues
  behind it. A file or the terminal is fine; anything whose reader can
  block deserves a moment's thought, and a bounded local buffer in front
  of it is cheap insurance.
- **An access log would put this on the hot path by construction** — one
  line per request, every request, every worker, through one mutex. The
  framework does not ship one. If you add one, per-worker buffers drained
  on a timer are the shape that avoids the contention; routing it through
  `log-msg` is the shape that does not.

### Static files

`load-static-files` reads files into memory at startup
and pre-builds HTTP responses.
Call it **before** `start-server`.
It is not thread-safe and must not be called while the server is running.

**The whole tree is capped at 256 MiB** (`:max-total-bytes`), and crossing
it signals with the offending path and the running total. The cache is
resident for the life of the process, so without a cap a directory
holding one large video buys a resident set to match — discovered on the
box at deploy time rather than at the call. Range support makes that
likelier rather than less: serving large media is what Range is *for*, so
the invitation to keep large media next to the CSS comes with a number
attached.

There is no serve-from-disk path — every served file lives in this cache.
If the cap fires, either raise it deliberately:

```lisp
(load-static-files "static/" :max-total-bytes (* 2 1024 1024 1024))
```

or leave large media to the reverse proxy, which is already in front of
this server for TLS termination and is better at it. The cap is per-call,
not global, so additive calls each bring their own budget.

Static responses **carry a `Date`**
and the pre-built path survives intact.
RFC 7231 §7.1.1.2 makes it a `MUST`.

The bytes are still built once at startup, but they are stored as pieces
rather than as a finished response: a prefix holding the status line and
headers, and the file's content. At request time the write queue takes
the prefix, a date line, the blank line that ends the headers, and the
body — four entries, no copying, no rebuild. The date line is cached per
second per worker, so a busy second serializes one.

Nothing is patched in place. The pre-built vectors are shared by every
worker and by every request for that file, and the write queue holds them
by reference while it drains — a `Date` rewritten under a half-sent
response would be a torn header with nothing to catch it. A new line is
built when the second turns instead.

Storing the body separately also retires an offset that would otherwise
matter: a range sliced out of the pre-built 200 at a position derived from
the header block's length moves under anything added to the headers of one
pre-built vector and not the other — a `206` with a correct status, a
correct `Content-Length`, and content starting a few bytes early. No offset
depends on header length.

`206` and `416` build their headers per request, so they get a `Date`
from the ordinary serializer along with everything else.

**Range requests are served** (RFC 7233): `Range: bytes=…` returns `206 Partial Content`
with a `Content-Range`, so `<video>`/`<audio>` seeking and resumable downloads work
rather than re-fetching from byte 0. `If-Range` is honored — a client whose validator
no longer matches gets the whole file, so a resumed download cannot splice bytes from
two versions of a file into a corrupt one. An out-of-bounds range gets `416` carrying
the resource's true length. Multi-range (`bytes=0-9,20-29`) is deliberately ignored and
the full file served, which RFC 7233 §3.1 permits; no media player or download manager
asks for it. The byte range is sliced out of the pre-built response, so enabling this
costs no extra memory and a full GET still takes the pre-built path.

A `Range` against a **zero-length** file is `416`, not an empty `200`.
RFC 7233 §2.1 puts every `first-byte-pos` at or past a length of zero, so
no range overlaps and §4.4 answers with 416. Worth knowing if you serve
media: a few players treat 416 as fatal where they would retry an empty
200. Files are read once at startup, so a file would have to be empty at
load time — a segment caught mid-write by a restart, not one being
written while the server runs.

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

A hook fires once. The run that fires the hooks consumes them, so a second
`start-server` in the same image (a restart at the REPL, an app that
restarts its server in-process) runs only the hooks registered since the
last one ended. Something started once at load time and stopped by a hook
is therefore stopped for good after the first run: start it again, and
register its hook again, before the next `start-server`, as the `start`
function above does on every call.

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

The reaper sleeps between sweeps and costs nothing while it waits. It
stops when the `start-server` run it was made before or during ends, since
that run consumes the hook. A store made once at load time, as above, and
used across several runs in one image (a server restarted at the REPL)
reaps during the first only; make it in the function that starts the
server when that matters. `stop-store-reaper` stops one sooner, for a
store the application retires, and lets the store be collected.

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

`delete-cookie` sends `Secure` by default, as `build-cookie` does, and takes
the same `:secure`. It does not take part in matching, but a browser refuses
any Set-Cookie for a `__Secure-` or `__Host-` name that lacks it, deletions
included — so without it, logging out of a `__Host-sid` session would leave
the cookie where it was. Both functions refuse a prefixed name whose
attributes break its prefix (`__Secure-` needs `Secure`; `__Host-` needs
`Secure`, `Path=/` and no `Domain`), for the reason they refuse
`SameSite=None` without `Secure`: the browser would refuse it silently.

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
the second emits it for the first, turning `[["a",1],["b",2]]` into
`{"a":[1],"b":[2]}`. Well-formed, silently wrong, no error anywhere.
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

### Request bodies, decided at the head

A handler is called with a whole request, and for most requests that is
the right shape. It comes too late for two things: a cap that should
differ by route, and a refusal that should happen before the body is read.
`:body-handler` is asked at the head:

```lisp
(defun decide-body (request)
  (let ((path (http-request-path request)))
    (cond ((string= path "/api/import")
           (buffer-body :max-size (* 16 1024 1024)))
          ((string= path "/upload")
           (if (signed-in-p request)
               (buffer-body :max-size (* 64 1024 1024))
               (make-error-response 401)))
          (t nil))))

(start-server :handler #'handle-request
              :body-handler #'decide-body)
```

- **When it is asked.** Once the head of a request with a body — a
  `Content-Length` above zero, or chunked — has arrived, on the worker, with
  the request parsed and its body `nil`. A request without a body never
  reaches it. The framing checks come first, `Transfer-Encoding` with
  `Content-Length` and an unknown `Expect` among them, so it is never shown
  a request the framework is about to refuse.
- **`nil`** reads the body as it would be read without a body handler:
  under `*max-body-size*`, timed by `*idle-timeout*`.
- **`(buffer-body :max-size n)`** reads it whole under `n`, which may be
  above or below `*max-body-size*`, and then `:handler` gets the whole
  request as usual. Buffering means holding: up to `n` bytes per connection
  doing this at once, so a large `n` is memory a request may really use.
- **`(receive-body :on-data … :on-end … :on-abort … :max-size n)`** hands
  the body to the application as it arrives instead of holding it, and its
  `:on-end` answers the request; see "Receiving a body as it arrives".
- **An `http-response`** is sent before the body is read, and the
  connection closes after it, whatever the response's own `Connection`
  header says: the body was never read, so there is no telling where a next
  request would begin. A client that sent `Expect: 100-continue` gets the
  answer instead of a `100 Continue` and never sends the body. One that
  sends it anyway has it read and thrown away until it stops or
  `*linger-timeout*` runs out, so the answer is followed by a FIN rather
  than a reset.
- **`100 Continue`** goes out only once the body is accepted.
- **Accepted bodies are timed on progress.** A body a body handler accepted
  with `buffer-body` or `receive-body` is closed when `*body-stall-timeout*`
  passes without a byte of it, rather than by `*idle-timeout*`'s budget for
  the whole request, so an upload takes as long as it keeps arriving. A body
  the handler answered `nil` for stays on `*idle-timeout*`.
- **Per request.** The next request on a keep-alive connection starts from
  the defaults again — its own head, its own question.
- **Failure.** A body handler that raises, or answers anything other than
  these shapes, fails the request with a 500, as a handler would.

### Receiving a body as it arrives

A buffered body is held whole before anyone sees it, which makes its size
a memory question. `receive-body` hands it over a read at a time instead,
and answers the request from `:on-end`:

```lisp
(defun save-upload (path)
  (let ((out nil))
    (receive-body
     :max-size (* 4 1024 1024 1024)            ; 4 GiB
     :on-data (lambda (conn bytes)
                (declare (ignore conn))
                (unless out
                  (setf out (open path :direction :output
                                       :element-type '(unsigned-byte 8))))
                (write-sequence bytes out)
                nil)
     :on-end (lambda (conn)
               (declare (ignore conn))
               (when out (close out))
               (make-text-response 201 "stored"))
     :on-abort (lambda (conn reason)
                 (declare (ignore conn reason))
                 (when out (close out :abort t))))))
```

- **`:on-data (conn bytes)`** gets the next run of the body, decoded, in a
  fresh vector it may keep. The runs are the size reads happen to be, not
  the client's chunks. Its value is ignored unless it is an
  `http-response`, which refuses the rest of the body: the response is sent
  and the connection closes after it.
- **`:on-end (conn)`** runs once the body is complete and returns the
  answer, in any shape `:handler` may answer with but `:upgrade` — a fetch
  included, so a body can be stored and then announced to an upstream
  before the client hears back.
- **`:on-abort (conn reason)`** runs once if the body ends any other way,
  and never once `:on-end` has returned: `:disconnected`, `:stalled`
  (`*body-stall-timeout*`), `:too-large` (answered 413), `:malformed`
  (chunked framing, answered 400), `:refused` (`:on-data` answered),
  `:error` (`:on-data` or `:on-end` raised; answered 500), `:shutdown`, or
  `:closed` for any other close. It is the one place to release what
  `:on-data` has been building.
- **`:max-size`** caps the decoded body, `*max-body-size*` by default, or
  `nil` for no cap. A `Content-Length` over it is refused before any of the
  body is read; a chunked body is refused the moment its total, or a chunk
  header, would pass it, with none of that chunk handed over. A declared
  length of up to fifteen digits is parsed, so a large upload's length
  reaches the cap rather than being refused at the head.
- **One window of buffer.** The read buffer holds the head and 64 KiB of
  body, whatever the body's size and whatever its chunk sizes — a chunk is
  handed over as it lands, not once it has all arrived. The first run can
  be larger: whatever arrived with the head before the body handler was
  asked.
- **Provisional until `:on-end`.** A body cut short, or found malformed
  partway, has had some of its bytes handed over by the time `:on-abort`
  says so — a chunk's bytes reach `:on-data` before the CRLF that ends the
  chunk has been seen. Clean up in `:on-abort`.
- **The callbacks hold the worker.** Writing what `:on-data` is handed
  somewhere synchronous, as above, is backpressure for free: nothing more is
  read until it returns, the kernel's receive window fills, and the client
  slows. It is also every other connection on that worker waiting, so
  `:on-data` should do what a handler should and nothing slower.
- **Fair to the rest of the worker.** A client that sends faster than the
  worker hands its body over is read a mebibyte at a time, sixteen windows,
  and then waits its turn behind the other events on the loop.
- **The handler never sees a received body.** The request is the one the
  body handler was given; close over it if `:on-data` or `:on-end` need it.
- **An asynchronous sink pauses.** When what `:on-data` is handed goes
  somewhere that answers later — an upstream, a part at a time — it starts
  that work and returns `:pause`. Nothing more is handed over, the
  connection is subscribed to no events, and the client's bytes wait in
  the kernel until `(body-resume conn)`; a pause taken on the body's last
  bytes holds `:on-end` back until then. `(body-refuse conn response)` ends
  the body from outside `:on-data` instead. Both are calls on the worker
  that owns the connection, and both are no-ops once the body has ended or
  the client has gone, so a late callback need not check first.
- **The fetch is `fetch-into` on the receiving connection.** The body
  bounds it — a client that leaves takes the fetch down, and so does the
  body's end — and its result goes to `:then`, which resumes or refuses:

  ```lisp
  :on-data (lambda (conn bytes)
             (fetch-into conn
                         (http-fetch :put (part-url) :body bytes
                                     :then (lambda (status headers body)
                                             (declare (ignore headers body))
                                             (if (eql status 200)
                                                 (body-resume conn)
                                                 (body-refuse conn (make-error-response 502)))))
                         :failure-disposition :keep)
             :pause)
  ```

  A body paused on a fetch it started is bounded by that fetch's
  deadline, not by `*body-stall-timeout*`; a pause waiting on anything
  else is ended as a stall once that long has passed. A real part is
  usually larger than one read — collect `:on-data`'s runs to the size
  the upstream wants, and pause when one is full. `:on-end` may answer
  with a fetch too, for the last part or the call that completes the
  upload.

### Forms and file uploads

A form with a file input is sent as `multipart/form-data`: a run of
parts, each a few header fields and its content, divided by a boundary the
browser chose and named in the `Content-Type`. There are two ways to read
one, and they are one parser.

**Read whole, from the handler.** The body is buffered under its request's
cap, and `parse-multipart` returns the parts:

```lisp
(defun handle-request (request)
  (let* ((parts (parse-multipart request))
         (title (form-part-text (find-form-part parts "title")))
         (photo (find-form-part parts "photo")))
    (store-photo (form-part-body photo))
    (make-text-response 201 title)))
```

- **A part** is a `form-part`. `form-part-name` is the field's name.
  `form-part-filename` is `nil` for a text field, and `""` for a file
  input with no file chosen. `form-part-content-type` is the part's type as
  sent, or `nil`. `form-part-headers` holds every field of the part's head,
  and `form-part-body` its content.
- **`form-part-text`** decodes a part as UTF-8, which is what a browser
  sends from a page served as UTF-8, and raises 400 for bytes that are not.
  `find-form-part` finds the first part of a name, and `find-form-parts`
  all of them, for a file input that takes several files.
- **It raises `http-parse-error`.** The status is 415 for a `Content-Type`
  that is not `multipart/form-data`, 400 for a body that is not one, and
  413 past a limit. Let it go and the framework answers with that status.
  Catch it to answer otherwise.
- **The cost is the body twice.** The body is held under the request's cap
  — `*max-body-size*`, or `buffer-body`'s from a body handler — and the
  parts are copied out of it. `:max-parts` (1000) and `:max-part-head-size`
  (16 KiB) bound the rest.

**Read as it arrives, from a body handler.** `receive-multipart` parses the
body a read at a time: the fields you collect are held and handed to
`:on-end`, and the files you stream go to `:on-part-data` and are never
held.

```lisp
(defun decide-body (request)
  (when (string= (http-request-path request) "/upload")
    (let ((out nil))
      (receive-multipart
       request
       :max-size (* 4 1024 1024 1024)                       ; the whole body
       :on-part (lambda (conn part)
                  (declare (ignore conn))
                  (cond ((null (form-part-filename part)) nil) ; a field: collect it
                        ((string= (form-part-name part) "file")
                         (setf out (open (fresh-upload-path) :direction :output
                                                             :element-type '(unsigned-byte 8)))
                         :stream)
                        (t (make-error-response 400))))        ; nothing else is wanted
       :on-part-data (lambda (conn part bytes)
                       (declare (ignore conn part))
                       (write-sequence bytes out)
                       nil)
       :on-part-end (lambda (conn part)
                      (declare (ignore conn))
                      (when (form-part-filename part)
                        (close out)
                        (setf out nil))
                      nil)
       :on-end (lambda (conn parts)
                 (declare (ignore conn))
                 (make-text-response
                  201 (form-part-text (find-form-part parts "title"))))
       :on-abort (lambda (conn reason)
                   (declare (ignore conn reason))
                   (when out (close out :abort t)))))))
```

- **`:on-part (conn part)`** is called when a part's head has arrived, with
  no body yet. It answers `nil` to collect the part, `:stream` to have its
  content handed to `:on-part-data`, or a response to refuse the body,
  which is sent with the connection closed after it. Anything else is an
  error, so a misspelt `:stream` does not quietly collect a file. Without
  `:on-part`, every part is collected.
- **`:on-part-data (conn part bytes)`** gets the next run of a streamed
  part, in a fresh vector. It answers as `receive-body`'s `:on-data` does:
  `:pause`, a response, or anything else to go on.
- **`:on-part-end (conn part)`** is called for every part, a collected one
  with its body set. A field sent before the file — a CSRF token, the id of
  the record the file belongs to — can refuse the body here, before any of
  the file is read. It answers as `:on-part-data` does.
- **`:on-end (conn parts)`** gets the collected parts in the order they
  were sent, and answers the request in any shape `:handler` may.
- **`:on-abort (conn reason)`** is `receive-body`'s, heard once. `:malformed`
  (400) covers a body that is not `multipart/form-data` as the parser reads
  it, and `:too-large` (413) covers every limit.
- **Its limits.** `:max-size` caps the whole body, as `receive-body`'s
  does. `:max-parts` (1000) and `:max-part-head-size` (16 KiB) are
  `parse-multipart`'s. `:max-collected-size` caps what is held for
  `:on-end`, the heads and content of the collected parts, at
  `*max-body-size*` by default. That last one keeps the memory a received
  form costs known, whatever `:max-size` allows. A part's head is also held
  to sixteen fields.
- **Refused at the head.** `receive-multipart` reads the `Content-Type`
  when the body handler calls it, and raises there for a request that is
  not a form. The request is answered 415 before any of its body is read,
  and a client that sent `Expect: 100-continue` is never told to send it.
- **A pause holds where it falls.** `:pause` from `:on-part-data` or
  `:on-part-end` stops the parse at that event, even partway through a
  read: no more of the body is parsed and no callback runs until
  `body-resume`. A pause on the last part's end holds `:on-end` back. This
  is how a file goes on to object storage a piece at a time:

  ```lisp
  :on-part-data (lambda (conn part bytes)
                  (declare (ignore part))
                  (collect bytes)                     ; into the next piece
                  (when (piece-full-p)
                    (fetch-into conn
                                (http-fetch :put (next-piece-url) :body (take-piece)
                                            :then (lambda (status headers body)
                                                    (declare (ignore headers body))
                                                    (if (eql status 200)
                                                        (body-resume conn)
                                                        (body-refuse conn (make-error-response 502))))))
                    :pause))
  ```

  `:on-part-end` sends the file's last piece the same way, and `:on-end`
  can answer with the fetch that completes the upload. A `:then` that finds
  another full piece already collected can send it at once with another
  `fetch-into` before it resumes. The fetch's own marker is released before
  `:then` runs. A file can also go upstream as one request, its runs sent as
  they arrive: see "Sending a request body as it is produced".

**What is accepted, and what is refused.** One parser serves both
functions. Where the format leaves room for two readings, it refuses the
body rather than choosing one, because a filter in front of the
application that chose the other reading would see different fields.

- A line that begins with the boundary is a delimiter, or the body is
  refused. It is never content. RFC 2046 matches the boundary against the
  start of a line, so `--BOUNDARYxyz` is a malformed delimiter, not data.
- Every line the format defines ends in CRLF.
- A part has exactly one `Content-Disposition`, of type `form-data`, with
  one name and at most one filename. It has at most one `Content-Type`.
  The extended `filename*` is refused: RFC 7578 §4.2 forbids it, and beside
  a `filename` it would be two names for one file.
- A `Content-Transfer-Encoding` other than `7bit`, `8bit` or `binary` is
  refused. None of those three changes the bytes, and RFC 7578 §4.7 says
  no deployed sender uses another.
- What the format says to ignore is ignored: a preamble before the first
  boundary, transport padding after one, an epilogue after the last. A
  part's other header fields are kept in its headers and otherwise
  ignored.
- A form with no fields is the close delimiter alone, which is what
  browsers send for one, and it reads as no parts.

**Names and filenames are returned as the browser sent them.** A quoted
value is the characters between its quotes, and a backslash is an ordinary
character, as in a Windows path. Browsers write a quote as `%22`, CR as
`%0D` and LF as `%0A`, but leave `%` itself alone, so those escapes cannot
be undone reliably and are not undone.

**A filename is a label, not a path.** Browsers send a file's base name.
For a folder upload (`<input type=file webkitdirectory>`) they send the
path below the chosen folder, `photos/2026/a.jpg`. Either way the text is
the client's. Name the stored file yourself. Where the client's path
matters, check every segment of it before any of it names something on
disk: no empty segment, no `.` or `..`, no backslash, no NUL, nothing that
reads as a drive.

### Sending a request body as it is produced

A fetch's body is given whole by default: a string or bytes, measured and
framed with `Content-Length`. `:body :stream` makes it the application's to
send as it has it, so an upload can go on to an upstream as it arrives and
never be held whole:

```lisp
(defun decide-body (request)
  (when (string= (http-request-path request) "/upload")
    (forward-body
     (http-fetch :put (object-url request)
                 :body :stream
                 :content-length (let ((n (get-header request "content-length")))
                                   (and n (parse-integer n)))
                 :then (lambda (status headers body)
                         (declare (ignore headers body))
                         (if (and status (<= 200 status 299))
                             (make-text-response 201 "stored")
                             (make-error-response 502))))
     :max-size (* 5 1024 1024 1024))))             ; 5 GiB
```

- **`forward-body`** is a body plan, for a body handler to return. The
  request's body goes upstream as the fetch's body, a run at a time as it
  arrives, and the client is answered with what `:then` returns. The fetch
  starts with the body's first bytes, and a body with none starts it at
  its end, with an empty body. `:max-size` and `:on-abort` are
  `receive-body`'s.
- **The client is held to the upstream's pace.** When the upstream falls
  behind, the body pauses, and it resumes once the upload has drained. What
  is held between the two is one read of the body at most, plus what the
  sockets hold, whatever the upload's size.
- **An upstream that answers first has answered the client.** One that
  refuses an upload at its head, a 413 or a 401, answers before the body
  has all gone, and so does one that fails partway. `:then` hears that
  answer, and as soon as it returns the rest of the body is refused with
  what it returned if that is an `http-response`, or 502 if not, whether
  or not the client is still sending. A connection that fails reaches
  `:then` as the abort sentinel, and so does one that cannot be made at
  all, to an address the kernel will not dial: `:then`'s answer to it is
  the client's, or 502. A `:then` that raises is a 500. The body's
  `:on-abort` hears `:refused` when it is refused in any of these ways.
- **The answer is read as the body goes out.** `:on-body` gets an early
  answer's chunks as they arrive, as it gets any fetch's. A `:pause` from
  it holds the answer and not the request: the body goes on going out, and
  the answer is read again from `fetch-resume`.
- **Framing.** A fetch that declares `:content-length` sends exactly that
  many bytes, framed by `Content-Length`; without it the body is sent
  chunked. Some upstreams take only the first. S3's PutObject is one, and so
  is a presigned URL to it, so forward the length the client declared, as
  above. A client that sent its body chunked declared no length, and for
  an upstream that needs one the route is the one in "Forms and file
  uploads": a piece at a time, each piece its own request.

**By hand.** `forward-body` is `receive-body` and the three calls a streamed
fetch has, with a `:then` that refuses the body when the upstream has
answered before it ended. In outline:

```lisp
(let* ((conn nil)
       (put (http-fetch :put (object-url) :body :stream
                        :then (lambda (status headers body)
                                (let ((answer (stored-or-502 status headers body)))
                                  ;; Refused if the body is still arriving;
                                  ;; a no-op once it has ended.
                                  (when conn (body-refuse conn answer))
                                  answer)))))
  (receive-body
   :on-data (lambda (c bytes)
              (setf conn c)
              (if (and (not (fetch-started-p put))
                       (not (ignore-errors
                             (fetch-into c put :failure-disposition :keep))))
                  (make-error-response 502)          ; the fetch could not start
                  (let ((sent (fetch-send put bytes)))
                    (cond ((eq sent t) nil)          ; with the kernel: go on
                          ((null sent) :pause)       ; resumed when it drains
                          (t (make-error-response 502)))))) ; the fetch is over
   :on-end (lambda (c)
             (declare (ignore c))
             put)))                                  ; answered by the upstream
```

- **`(fetch-send fetch bytes)`** sends the next piece of the body, a chunk
  of its own unless a length was declared. It answers `t` when everything
  sent so far is with the kernel and `nil` when some of it is waiting. `nil`
  is backpressure, as `stream-send`'s is. A body being received pauses on
  it and is resumed when the upload drains, with nothing more asked of the
  application. Any other producer, an `:on-tick` for one, stops and asks
  again later with an empty send, which sends nothing and answers `t` once
  the upstream has caught up; sending on regardless signals at
  `*max-write-backlog*`. `:ended` means the fetch is over, answered or
  failed or stopped, and the bytes went nowhere. The bytes are copied, so
  the vector may be reused at once.
- **`(fetch-finish fetch)`** ends the body: the chunked terminator, or a
  check that the declared length has all been sent, which signals if not.
- **Answering with the fetch.** An `:on-end`, or a fetch's `:then`, that
  answers with a streamed fetch already started for its request answers
  the request with what that fetch's `:then` returns: once the upstream has
  answered, or at once if it already has. A fetch whose body is not
  finished is finished then.
- **Started once.** `fetch-into` starts a streamed fetch, or it starts as an
  answer, like any fetch. What is sent before it starts is held for it,
  under the same backlog bound. Starting it a second time signals, since
  the fetch is the application's handle on that one exchange, and
  `fetch-started-p` says whether it has started.
- **On the worker.** `fetch-send` and `fetch-finish` are called on the
  worker that carries the fetch, from a body's callbacks, a fetch's `:then`
  or an `:on-tick`, and signal anywhere else. An `:on-tick` runs on every
  worker, so one that feeds a fetch keeps the `*worker-id*` the fetch was
  started on and feeds it there alone.
- **Timing.** While the body is going out, `*fetch-timeout*` bounds it
  standing still, not the upload. Each piece sent, and each piece the
  upstream takes, moves the deadline out, so an upload takes as long as it
  keeps moving. If the upstream stops reading, the fetch ends once that
  long has passed and the client is answered 502. If the client stops
  sending, the fetch ends the same way; the client is answered 502 if it
  sends again, and is ended as a stall under `*body-stall-timeout*` if it
  does not. Once the body has all been written to the socket, the answer is
  waited for under `*fetch-timeout*` as any fetch's is, and a request
  answered with the fetch is answered 504 if it does not come. That wait
  includes the time the upstream takes to read what the kernel was still
  holding for it, which over a slow link can be seconds.

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
A form with a file input sends `multipart/form-data` instead; see
"Forms and file uploads".

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
Besides `:handler` it takes `:ws-handler`, `:body-handler` and `:host`, as `start-server` does;
`:workers`, which defaults to one, so a test can reason about a single connection table;
and `:on-tick`, passed to `start-server` unchanged, for a test of work that runs on a worker's own loop.

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

**Reads are bounded, and yours should be too.** `read-byte` on a socket
stream has no deadline, so a server that answers late, answers partially,
or never answers does not fail a test — it stops the run, and CI kills the
job minutes later with a log ending at the name of the test that started
and nothing said about what it was waiting for.

Every read `test-http-request` performs goes through a deadline
(`*test-read-timeout*`, 10 seconds, rebindable with a plain `let` since it
is read on the calling thread). A test that times out raises immediately,
naming the deadline, the byte count, and the first 200 bytes that did
arrive.

For reads of your own — anything that talks to a handler over a raw
socket — `read-until-bounded` is exported:

```lisp
;; Read to the end of the stream, bounded. Right for a
;; Connection: close response.
(multiple-value-bind (buf reason) (read-until-bounded stream)
  ;; reason is :eof, :error, or :deadline; buf holds whatever arrived
  ...)

;; Read until a predicate is satisfied. Necessary for anything the
;; server keeps open — a kept-alive response or a stream never
;; reaches EOF, so reading to the end means waiting out the deadline.
(read-until-bounded stream
                    :until (lambda (buf fill)
                             (>= (count-events buf fill) 3))
                    :seconds 5)
```

It always returns the buffer, including on the deadline, so a test that
gives up can still assert against the bytes it did get and say what was
missing. `:until` is called after each byte, so keep it cheap.

End-to-end tests are slower than unit-style tests
(~1-2 seconds per `with-test-server` call, mostly shutdown latency).
Use unit-style tests for handler logic,
end-to-end for the request/response path itself
and for anything that depends on the event loop or graceful shutdown machinery.
