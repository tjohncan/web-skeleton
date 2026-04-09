# Web Skeleton

SBCL web server framework for Linux.
Provides the network and protocol layer
 (TCP socket management,
 HTTP request parsing and response building,
 WebSocket handshake and framing)
as a reusable foundation for web services and real-time applications.

Minimal external dependencies beyond SBCL's built-in libraries.

## Requirements

- SBCL (Steel Bank Common Lisp)
- Linux

SBCL built-ins used:
 `sb-bsd-sockets` (TCP),
 `sb-ext` (byte conversion),
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

Produces a standalone binary via `save-lisp-and-die`. No SBCL installation
required at runtime.

## Testing

```bash
sbcl --non-interactive --load run-tests.lisp
```

Exits with code 0 on success, 1 on failure.

## Development (REPL)

```bash
sbcl
```

```lisp
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "web-skeleton-tests")
(web-skeleton-tests:test)             ; run all tests
(web-skeleton-tests:test-algorithms)  ; SHA-1, SHA-256, Base64, ECDSA, HMAC, hex
(web-skeleton-tests:test-json)        ; JSON parser and serializer
(web-skeleton-tests:test-server)      ; HTTP, URL, query, routing, JWT
(web-skeleton-tests:test-tls)         ; TLS roundtrip (skips if libssl absent)
(asdf:load-system "web-skeleton-demo")
(web-skeleton-demo:start-demo)  ; run the demo server
```

## Project structure

```
web-skeleton.asd             ASDF system definition (the framework)
web-skeleton-tls.asd         ASDF system definition (optional TLS/HTTPS)
web-skeleton-tests.asd       ASDF system definition (test suite)
web-skeleton-demo.asd        ASDF system definition (demo app)
build.lisp                   Build standalone demo binary via save-lisp-and-die
run-server.lisp              Entry point — load demo system and start server
run-tests.lisp               Entry point — load test suite and run
src/
  package.lisp               Package (namespace) declaration
  log.lisp                   Logging (DEBUG/INFO/WARN/ERROR, UTC timestamps)
  epoll.lisp                 Linux epoll + fcntl + read/write FFI bindings
  json.lisp                  JSON parser and serializer (RFC 8259)
  tls.lisp                   libssl FFI, TLS connections, HTTPS fetch (optional)
  algorithms/
    word32.lisp              32-bit unsigned word ops (shared by SHA-1, SHA-256)
    hex.lisp                 Hex encoding utilities
    sha1.lisp                SHA-1 digest (FIPS 180-4)
    sha256.lisp              SHA-256 digest (FIPS 180-4)
    hmac.lisp                HMAC-SHA256 (RFC 2104)
    base64.lisp              Base64 encoder/decoder + URL-safe variant (RFC 4648)
    ecdsa.lisp               ECDSA P-256 signature verification (FIPS 186-4)
  server/
    connection.lisp          Connection state machine, read/write buffers
    http.lisp                HTTP request parser, response builder, URL/query/routing
    websocket.lisp           WebSocket handshake and incremental frame protocol
    jwt.lisp                 JWT validation (ES256) and JWKS parsing
    static.lisp              In-memory static file cache and serving
    fetch.lisp               Outbound HTTP client (non-blocking fetch, streaming fetch)
    main.lisp                epoll event loop, handler dispatch, server entry point
demo/
  package.lisp               Demo package declaration
  handler.lisp               WebSocket echo handler, demo entry point
  static/                    Demo static assets (HTML, CSS, JS, favicon, images)
tests/
  package.lisp               Test package declaration
  run.lisp                   Test utilities and combined runner
  test-algorithms.lisp       SHA-1, SHA-256, Base64, ECDSA, HMAC, hex test vectors
  test-json.lisp             JSON parser and serializer tests
  test-server.lisp           HTTP, URL, query, routing, JWT tests
  test-tls.lisp              TLS roundtrip (skips gracefully without libssl)
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
  server-initiated ping/pong for dead WebSocket detection, configurable
  idle timeout for inactive WebSocket connections
- **TCP listener** — binds a socket, accepts connections, clean shutdown on Ctrl-C
- **HTTP request parser** — method, path, query string, headers, body;
  validates against configurable size limits
- **HTTP response builder** — status codes, headers, body serialization
- **HTTP keep-alive** — persistent connections per HTTP/1.1 default. Connections
  are reused across requests; `Connection: close` and HTTP/1.0 are respected
- **URL and query utilities** — percent-decoding (`url-decode`), query string
  parsing (`parse-query-string`, `get-query-param`)
- **Path matching** — `match-path` matches URL paths against patterns with
  `:param` captures (e.g. `/users/:id`), returns bindings alist or NIL
- **JSON** — full parser and serializer (RFC 8259). Objects become alists,
  arrays become lists. Handles all escape sequences including `\uXXXX`
  and surrogate pairs
- **SHA-1** — complete implementation per FIPS 180-4
- **SHA-256** — complete implementation per FIPS 180-4
- **HMAC-SHA256** — RFC 2104 keyed-hash message authentication
- **Base64** — encoder/decoder, standard and URL-safe alphabets (RFC 4648)
- **ECDSA P-256** — signature verification per FIPS 186-4. Pure Lisp bignum
  arithmetic, verification only (no signing, no key generation)
- **JWT validation** — ES256 token verification, JWKS key set parsing, claim
  extraction, expiration checking. Pure Lisp — no external dependencies
- **WebSocket handshake** — validates upgrade request, computes accept key
- **WebSocket frame protocol** — incremental frame parser and builder per RFC 6455,
  handles text, binary, ping/pong, close, and fragmented messages (automatic
  reassembly with size limits)
- **WebSocket server push** — `ws-send` sends a frame to a connection synchronously
  from within the handler, enabling streaming responses (e.g. sending LLM tokens
  as they arrive) without returning from the handler until the work is done
- **Static file serving** — `load-static-files` reads a directory tree into memory
  at startup; `serve-static` looks up the request path and returns a pre-built
  response. MIME detection, extensionless HTML aliases, directory traversal protection
- **Standalone binary** — `save-lisp-and-die` produces a single executable
- **Test suite** — FIPS/RFC test vectors for all crypto primitives, JSON
  round-trip tests, HTTP parser tests, JWT validation tests
- **HTTP client** — non-blocking outbound HTTP via `http-fetch`. Integrates with
  the event loop — outbound connections use the same epoll, zero blocking.
  Handler returns a fetch descriptor; the framework parks the inbound connection,
  makes the outbound call, and resumes with the callback result
- **Streaming fetch** — `http-fetch-stream` reads a response body line by line,
  calling a callback per line. Designed for NDJSON/SSE streaming APIs (e.g.
  LLM token streams). Blocking — call from within a handler
- **Outbound TLS** — HTTPS support in `http-fetch` and `http-fetch-stream` via
  optional `web-skeleton-tls` system. libssl FFI bindings, system CA
  verification, SNI. Same API — just use `https://` URLs
- **Graceful shutdown** — on Ctrl-C or SIGTERM, stops accepting, sends WebSocket
  close frames, flushes in-progress writes, force-closes after drain timeout
- **Demo application** — separate ASDF system with static demo page and echo server

## Configuration

All configurable via `setf` before calling `start-server`.

| Variable | Default | Description |
|---|---|---|
| `*log-level*` | `:info` | Minimum log level (`:debug`, `:info`, `:warn`, `:error`) |
| `*log-stream*` | `*standard-output*` | Stream to write log output to |
| `*max-request-line-length*` | `8192` | Max HTTP request line (bytes) |
| `*max-header-count*` | `100` | Max number of headers per request |
| `*max-header-line-length*` | `8192` | Max single header line (bytes) |
| `*max-body-size*` | `1048576` | Max request body (bytes, default 1MB) |
| `*max-ws-payload-size*` | `65536` | Max WebSocket frame payload (bytes, default 64KB) |
| `*idle-timeout*` | `10` | Seconds before an idle HTTP connection is closed |
| `*ws-idle-timeout*` | `86400` | Seconds before an inactive WebSocket is closed |
| `*ws-ping-interval*` | `30` | Seconds between server-initiated WebSocket pings |
| `*ws-max-missed-pongs*` | `3` | Missed pongs before a WebSocket is declared dead |
| `*drain-timeout*` | `5` | Seconds to wait for connections to drain on shutdown |

The `port`, `workers`, `handler`, and `ws-handler` are passed as keyword arguments:

```lisp
(start-server :port 8081
              :workers 4
              :handler #'my-app:handle-request
              :ws-handler #'my-app:handle-ws-message)
```

The number of workers defaults to the number of CPU cores.
Without a handler, the server returns 501 for all requests.

### Server push

`ws-send` writes a WebSocket frame to a connection synchronously,
blocking until all bytes are flushed. Call it from within `ws-handler`
to send multiple frames during a single handler invocation — the event
loop is paused while the handler runs, so there is no write contention.

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
an LLM response for a few seconds), but avoid unbounded blocking.

## Roadmap
- **OpenSSL-accelerated crypto** — when libssl is loaded for outbound TLS,
  use it for SHA-1, SHA-256, and HMAC as well. Pure Lisp implementations
  remain the default when libssl is not present
