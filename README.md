# web-skeleton

SBCL web server framework for Linux.
Provides the network and protocol layer
(TCP socket management, 
HTTP request parsing and response building,
WebSocket handshake and framing)
as a reusable foundation for web services and
real-time applications.

Minimal external dependencies beyond SBCL's built-in libraries.

## Requirements

- SBCL (Steel Bank Common Lisp)
- Linux
- `getent` on PATH — ships with glibc and musl, present on every
  mainstream distro (used via `sb-ext:run-program`
  for non-blocking hostname resolution).

SBCL built-ins used:
 `sb-bsd-sockets` (TCP),
 `sb-ext` (byte conversion, subprocess),
 `sb-alien` (epoll/fcntl/read/write FFI),
 `sb-thread` (log mutex; worker pool).
ASDF ships with SBCL.

## Running

```bash
sbcl --non-interactive --load run-server.lisp
```

Starts the demo server on port 8081.
The demo page (load `http://localhost:8081/` in a browser)
opens a WebSocket connection and echoes messages back.
Ctrl-C or SIGTERM triggers graceful shutdown (drains active connections).

## Building

```bash
sbcl --non-interactive --load build.lisp
./web-skeleton
```

Produces a standalone binary via `save-lisp-and-die`.
No SBCL installation required at runtime.

## Testing

```bash
sbcl --non-interactive --load run-tests.lisp
```

Exits with code 0 on success, 1 on failure.
On a machine with libssl loaded,
the default runner also re-exercises the pure-Lisp crypto paths
so edits to `src/algorithms/*.lisp` are caught even when libssl has
swapped them out of the default function cells.

For framework-dev work focused on the pure-Lisp crypto alone:

```bash
sbcl --non-interactive --load run-pure-lisp-tests.lisp
```

This loads the tests and (if present) `web-skeleton-tls`, then runs
only `test-pure-lisp-crypto` — the SHA-1 / SHA-256 / HMAC / ECDSA
suites with the function cells temporarily swapped back to their
`*-lisp` originals.

## Development (REPL)

```bash
sbcl
```

```lisp
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "web-skeleton-tests")
(web-skeleton-tests:test)                   ; run all tests
(web-skeleton-tests:test-algorithms)        ; SHA-1, SHA-256, Base64, ECDSA, HMAC, hex
(web-skeleton-tests:test-json)              ; JSON parser and serializer
(web-skeleton-tests:test-server)            ; HTTP, URL, query, routing, JWT
(web-skeleton-tests:test-store)             ; concurrent store and reaper
(web-skeleton-tests:test-harness)           ; live-server test harness round-trips
(web-skeleton-tests:test-tls)               ; TLS registration (skips if libssl absent)
(web-skeleton-tests:test-pure-lisp-crypto)  ; re-run crypto suites against the pure-Lisp originals
(asdf:load-system "web-skeleton-demo")
(web-skeleton-demo:start-demo)  ; run the demo server
```

## Project structure

```
web-skeleton.asd                ASDF system definition (the framework)
web-skeleton-tls.asd            ASDF system definition (optional TLS/HTTPS)
web-skeleton-test-harness.asd   ASDF system definition (optional test harness)
web-skeleton-tests.asd          ASDF system definition (framework test suite)
web-skeleton-demo.asd           ASDF system definition (demo app)
build.lisp                   Build standalone demo binary via save-lisp-and-die
run-server.lisp            Entry point — load demo system and start server
run-tests.lisp             Entry point — load test suite and run
run-pure-lisp-tests.lisp   Entry point — verify pure-Lisp crypto paths
src/
  package.lisp     Package (namespace) declaration
  log.lisp         Logging (DEBUG/INFO/WARN/ERROR, UTC timestamps)
  epoll.lisp       Linux epoll + fcntl + read/write FFI bindings
  json.lisp        JSON parser and serializer (RFC 8259)
  random.lisp      Crypto random bytes and tokens (/dev/urandom)
  address.lisp     IP address classification (SSRF allowlisting)
  store.lisp       Concurrent keyed store with optional reaper
  tls.lisp         libssl FFI, TLS connections, HTTPS fetch (optional)
  algorithms/
    word32.lisp      32-bit unsigned word ops (shared by SHA-1, SHA-256)
    hex.lisp         Hex encoding utilities
    sha1.lisp        SHA-1 digest (FIPS 180-4)
    sha256.lisp      SHA-256 digest (FIPS 180-4)
    hmac.lisp        HMAC-SHA256 (RFC 2104)
    base64.lisp      Base64 encoder/decoder + URL-safe variant (RFC 4648)
    ecdsa.lisp       ECDSA P-256 signature verification (FIPS 186-4)
  server/
    connection.lisp    Connection state machine, read/write buffers
    http.lisp          HTTP request parser, response builder, URL/query/routing
    websocket.lisp     WebSocket handshake and incremental frame protocol
    jwt.lisp           JWT validation (ES256) and JWKS parsing
    static.lisp        In-memory static file cache and serving
    fetch.lisp         Outbound HTTP client (non-blocking fetch, streaming fetch)
    dns.lisp           Async DNS via getent ahosts subprocess
    main.lisp          epoll event loop, handler dispatch, server entry point
demo/
  package.lisp     Demo package declaration
  handler.lisp     WebSocket echo handler, demo entry point
  static/          Demo static assets (HTML, CSS, JS, favicon, images)
tests/
  package.lisp           Test package declaration
  run.lisp               Test utilities and combined runner
  harness.lisp           Ephemeral-port live server and request/frame builders
  test-algorithms.lisp   SHA-1, SHA-256, Base64, ECDSA, HMAC, hex test vectors
  test-json.lisp         JSON parser and serializer tests
  test-server.lisp       HTTP, URL, query, routing, JWT tests
  test-store.lisp        Concurrent store and reaper tests
  test-harness.lisp      Test harness self-tests (live-server round-trips)
  test-tls.lisp          TLS registration (skips if libssl absent)
```

## What's implemented

- **Handler abstraction** — `start-server` accepts `:handler` and `:ws-handler`
  functions. The framework handles sockets, HTTP, and WebSocket protocol;
  the application provides routing and message handling
- **Worker thread pool** — one event loop per CPU core, each with its own
  listener socket (`SO_REUSEPORT`), epoll fd, and connection table.
  Kernel distributes accepts across workers. Zero shared state in the hot path
- **epoll event loop** — edge-triggered, non-blocking I/O via `sb-alien`
  FFI to Linux epoll, fcntl, read, write
- **Connection state machine** — per-connection read/write buffers, tracks
  protocol state (HTTP request parsing, response writing, WebSocket framing)
- **Connection lifecycle** — idle timeout for HTTP (slowloris protection),
  server-initiated ping/pong for dead WebSocket detection,
  configurable idle timeout for inactive WebSocket connections
- **TCP listener** — binds a socket (IPv4 or IPv6), accepts connections, clean shutdown
- **HTTP request parser** — method, path, query string, headers, body;
  validates against configurable size limits
- **HTTP response builder** — status codes, headers, body serialization
- **HTTP keep-alive** — persistent connections per HTTP/1.1 default. Connections
  are reused across requests; `Connection: close` and HTTP/1.0 are respected
- **Expect: 100-continue** — interim `100 Continue` sent before reading the
  body when a request carries the `Expect` header. Prevents 1-3s invisible
  latency with curl, Go, Python, and Java HTTP clients on large POSTs
- **URL and query utilities** — percent-decoding (`url-decode`), query string
  parsing (`parse-query-string`, `get-query-param`)
- **Cookies** — `get-cookie` reads a named cookie from the request
  header; `build-cookie` / `delete-cookie` emit Set-Cookie values with
  `HttpOnly`, `Secure`, `SameSite`, `Max-Age`, `Path`, `Domain`.
  Safe defaults (HttpOnly, Secure, SameSite=Lax); structural-character validation
- **Path matching** — `match-path` matches URL paths against patterns
  with `:param` captures (e.g. `/users/:id`), returns bindings alist or NIL
- **JSON** — full parser and serializer (RFC 8259).
  Objects become alists, arrays become lists.
  Handles all escape sequences including `\uXXXX` and surrogate pairs
- **SHA-1** & **SHA-256** — complete implementations per FIPS 180-4
- **HMAC-SHA256** — RFC 2104 keyed-hash message authentication
- **Base64** — encoder/decoder, standard and URL-safe alphabets (RFC 4648)
- **Crypto random** — `random-bytes` reads N bytes from `/dev/urandom`;
  `random-token` returns a base64url-encoded token (default 32 bytes / ~256 bits).
  For session IDs, CSRF tokens, nonces, PKCE verifiers
- **ECDSA P-256** — signature verification per FIPS 186-4.
  Pure Lisp bignum arithmetic, verification only (no signing, no key generation)
- **JWT validation** — ES256 token verification, JWKS key set parsing,
  claim extraction, expiration checking.
- **WebSocket handshake** — validates upgrade request, computes accept key
- **WebSocket frame protocol** — incremental frame parser and builder per RFC 6455,
  handles text, binary, ping/pong, close, and fragmented messages
  (automatic reassembly with size limits)
- **WebSocket server push** — `ws-send` sends a frame to a connection synchronously
  from within the handler, enabling streaming responses
  without returning from the handler until the work is done
- **Static file serving** — `load-static-files` reads a directory tree into memory
  at startup; `serve-static` looks up the request path and returns a pre-built response.
  MIME detection, extensionless HTML aliases (`/login.html` → `/login`),
  directory-index aliases (`/docs/index.html` → `/docs`),
  directory traversal protection, ETag-based revalidation
  (`If-None-Match` → `304 Not Modified` using a SHA-256 strong entity tag
  computed at load time), per-path `:cache-control` override (string or function of URL path)
- **Concurrent keyed store** — `make-store` returns a thread-safe
  hash-table-backed store for app state (sessions, caches, rate-limit counters).
  Optional background reaper sweeps entries on an app-supplied predicate
  and registers its stop via the cleanup hook machinery — no app-side teardown needed
- **HTTP client** — non-blocking outbound HTTP via `http-fetch`. Integrates with
  the event loop — outbound connections use the same epoll, zero blocking.
  Handler returns a fetch descriptor; the framework parks the inbound connection,
  makes the outbound call, and resumes with the callback result
- **Async DNS resolution** — non-numeric hostnames are resolved via a `getent ahosts`
  subprocess whose stdout pipe is registered with the worker's epoll;
  the parked inbound resumes when the address lands.
  Numeric IPv4 and IPv6 literals (including `http://[::1]:8080/`) skip DNS entirely
  via a fast path. Both families supported
- **Streaming fetch** — `http-fetch-stream` reads a response body line by line,
  calling a callback per line. Designed for NDJSON/SSE streaming APIs.
  Blocking — call from within a handler
- **Outbound TLS** — HTTPS support in `http-fetch` and `http-fetch-stream` via
  optional `web-skeleton-tls` system. libssl FFI bindings, system CA verification, SNI.
  Same API — just use `https://` URLs. Loading this system
  also swaps `sha1`, `sha256`, and `ecdsa-verify-p256` for
  libssl-backed implementations via `setf symbol-function`;
  the pure-Lisp versions remain reachable internally and `hmac-sha256` benefits
  transparently because it calls `sha256` through the function cell
- **Graceful shutdown** — on Ctrl-C or SIGTERM, stops accepting,
  sends WebSocket close frames, flushes in-progress writes,
  force-closes after drain timeout
- **Shutdown cleanup hooks** — apps register zero-argument functions via
  `register-cleanup` to run during graceful shutdown.
  Hooks fire in LIFO order inside the shutdown path after connection drain,
  each wrapped in `handler-case` so a raising hook cannot block the rest
- **Standalone binary** — `save-lisp-and-die` produces a single executable
- **Test suite** — FIPS/RFC test vectors for all crypto primitives,
  JSON round-trip tests, HTTP parser tests, JWT validation tests
- **Test harness** — optional `web-skeleton-test-harness` ASDF system.
  `with-test-server` spins an ephemeral-port live server,
  `test-http-request` makes real HTTP round-trips inside test bodies,
  `make-test-request` and `make-test-ws-frame` build structs for
  unit-style handler tests. Downstream apps can depend on it in their test build
  without pulling in the framework's own test suite
- **Demo application** — separate ASDF system with static demo page and echo server

## Configuration

All configurable via `setf` before calling `start-server`.

| Variable                       | Default   | Description                                                                                                                                                                                                                                        |
|--------------------------------|-----------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `*log-level*`                  | `:info`   | Minimum log level (`:debug`, `:info`, `:warn`, `:error`)                                                                                                                                                                                           |
| `*log-stream*`                 | `nil`     | Stream for log output (`nil` uses `*standard-output*`)                                                                                                                                                                                             |
| `*max-request-line-length*`    | `8192`    | Max HTTP request line (bytes)                                                                                                                                                                                                                      |
| `*max-header-count*`           | `100`     | Max number of headers per request                                                                                                                                                                                                                  |
| `*max-header-line-length*`     | `8192`    | Max single header line (bytes)                                                                                                                                                                                                                     |
| `*max-body-size*`              | `1048576` | Max request body (bytes, default 1MB)                                                                                                                                                                                                              |
| `*max-outbound-response-size*` | `8388608` | Max buffered outbound HTTPS response total (headers + body, bytes, default 8MB). Separate from `*max-body-size*` so 1MB+ responses with normal headers don't get rejected                                                                          |
| `*max-streaming-line-size*`    | `1048576` | Max single line in a streamed response (NDJSON, SSE, chunked text, bytes, default 1MB). Per-line cap on the `http-fetch-stream` paths, distinct from `*max-outbound-response-size*` (per-response) and `*max-body-size*` (inbound)                 |
| `*max-ws-payload-size*`        | `65536`   | Max individual WebSocket frame payload (bytes, default 64KB). Per-frame memory bound on the read path                                                                                                                                              |
| `*max-ws-message-size*`        | `1048576` | Max reassembled WebSocket message (bytes, default 1MB). Applies to fragmented messages (opcode TEXT/BINARY + CONTINUATION frames). Separate from `*max-ws-payload-size*` so fragmentation can actually deliver messages larger than a single frame |
| `*max-connections*`            | `10000`   | Max connections per worker (new accepts dropped when full)                                                                                                                                                                                         |
| `*idle-timeout*`               | `10`      | Seconds before an idle HTTP connection is closed                                                                                                                                                                                                   |
| `*ws-idle-timeout*`            | `86400`   | Seconds before an inactive WebSocket is closed                                                                                                                                                                                                     |
| `*ws-ping-interval*`           | `30`      | Seconds between server-initiated WebSocket pings                                                                                                                                                                                                   |
| `*ws-max-missed-pongs*`        | `3`       | Missed pongs before a WebSocket is declared dead                                                                                                                                                                                                   |
| `*fetch-timeout*`              | `30`      | Blocking fetch I/O timeout and :awaiting connection reap deadline                                                                                                                                                                                  |
| `*jwt-clock-skew*`             | `60`      | Seconds of clock skew tolerance for JWT exp/nbf checks                                                                                                                                                                                             |
| `*drain-timeout*`              | `5`       | Seconds to wait for connections to drain on shutdown                                                                                                                                                                                               |
| `*shutdown-poll-interval*`     | `1`       | Seconds between shutdown-signal checks (main-thread sleep + worker epoll timeout)                                                                                                                                                                  |

The `host`, `port`, `workers`, `handler`, and `ws-handler` are passed as keyword arguments:

```lisp
(start-server :host #(127 0 0 1)  ; localhost only (default)
              :port 8081
              :workers 4
              :handler #'my-app:handle-request
              :ws-handler #'my-app:handle-ws-message)
```

`:host` accepts a 4-byte IPv4 vector or a 16-byte IPv6 vector
and dispatches the listener family accordingly.
Use `:host #(0 0 0 0)` to listen on all IPv4 interfaces,
or `:host #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)` to listen on all IPv6 interfaces.
IPv6 loopback is `:host #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)`.
The number of workers defaults to the number of CPU cores.
Without a handler, the server returns 501 for all requests.

## Async HTTP fetch

A handler that needs to call an upstream API
**returns a continuation instead of blocking**.
`defer-to-fetch` (or `http-fetch` directly) builds the continuation — a descriptor
carrying the outbound method, URL, headers, body, and a `:then` callback.
The framework recognizes the continuation as the handler's return value,
parks the inbound connection, resolves the hostname asynchronously via a `getent`
subprocess if needed, makes the outbound call on the same epoll loop
(non-blocking for plain HTTP), then invokes the callback with
`(status headers body-bytes)`. Whatever the callback returns becomes
the final response to the original caller:

```lisp
(defun handle-proxy (request)
  (declare (ignore request))
  (defer-to-fetch :get "http://upstream/api/data"
    :then (lambda (status headers body-bytes)
            (declare (ignore headers))
            (if (= status 200)
                (make-text-response
                 200
                 (sb-ext:octets-to-string body-bytes
                                          :external-format :utf-8))
                (make-error-response 502)))))
```

The callback can itself return another continuation for chained fetches —
the framework detects the nested continuation and runs the chain.
A regular `http-response` object terminates the chain.
See `demo/handler.lisp`'s `/demo-fetch` endpoint for a runnable example.

`http-fetch-stream` is a separate, **blocking** variant that
reads the response body line by line via a per-line callback.
It holds the worker thread for the duration of the upstream call
and is intended for NDJSON / SSE streaming APIs inside a `ws-handler`,
where the event loop is already paused while the handler runs.

## Deployment

See [DEPLOYMENT.md](DEPLOYMENT.md) for project setup, configuration,
and practical notes on building with web-skeleton.
