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
The built-in test page (load `http://localhost:8081/` in a browser) 
opens a WebSocket connection and echoes messages back. Ctrl-C shuts down cleanly.

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
(web-skeleton-tests:test-algorithms)  ; SHA-1 and Base64 only
(web-skeleton-tests:test-server)      ; HTTP parser and response builder only
(asdf:load-system "web-skeleton-demo")
(web-skeleton-demo:start-demo)  ; run the demo server
```

## Project structure

```
web-skeleton.asd             ASDF system definition (the framework)
web-skeleton-tests.asd       ASDF system definition (test suite)
web-skeleton-demo.asd        ASDF system definition (demo app)
build.lisp                   Build standalone demo binary via save-lisp-and-die
run-server.lisp              Entry point — load demo system and start server
run-tests.lisp               Entry point — load test suite and run
src/
  package.lisp               Package (namespace) declaration
  log.lisp                   Logging (DEBUG/INFO/WARN/ERROR, UTC timestamps)
  epoll.lisp                 Linux epoll + fcntl + read/write FFI bindings
  algorithms/
    sha1.lisp                SHA-1 digest (FIPS 180-4)
    base64.lisp              Base64 encoder (RFC 4648)
  server/
    connection.lisp          Connection state machine, read/write buffers
    http.lisp                HTTP request parser + response builder
    websocket.lisp           WebSocket handshake and incremental frame protocol
    static.lisp              In-memory static file cache and serving
    main.lisp                epoll event loop, handler dispatch, server entry point
demo/
  package.lisp               Demo package declaration
  handler.lisp               Test page, echo WebSocket handler, demo entry point
tests/
  package.lisp               Test package declaration
  run.lisp                   Test utilities and combined runner
  test-algorithms.lisp       SHA-1 and Base64 test vectors (FIPS, RFC)
  test-server.lisp           HTTP parser and response builder tests
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
- **SHA-1** — complete implementation per FIPS 180-4
- **Base64** — encoder per RFC 4648
- **WebSocket handshake** — validates upgrade request, computes accept key
- **WebSocket frame protocol** — incremental frame parser and builder per RFC 6455,
  handles text, binary, ping/pong, and close frames
- **Static file serving** — `load-static-files` reads a directory tree into memory
  at startup; `serve-static` looks up the request path and returns a pre-built
  response. MIME detection, extensionless HTML aliases, directory traversal protection
- **Standalone binary** — `save-lisp-and-die` produces a single executable
- **Test suite** — algorithm test vectors and HTTP parser tests
- **Demo application** — separate ASDF system with test page and echo server

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
| `*idle-timeout*` | `30` | Seconds before an idle HTTP connection is closed |
| `*ws-idle-timeout*` | `86400` | Seconds before an inactive WebSocket is closed |
| `*ws-ping-interval*` | `30` | Seconds between server-initiated WebSocket pings |
| `*ws-max-missed-pongs*` | `3` | Missed pongs before a WebSocket is declared dead |

The `port`, `workers`, `handler`, and `ws-handler` are passed as keyword arguments:

```lisp
(start-server :port 8081
              :workers 4
              :handler #'my-app:handle-request
              :ws-handler #'my-app:handle-ws-message)
```

The number of workers defaults to the number of CPU cores.
Without a handler, the server returns 501 for all requests.

## Roadmap
- **HTTP client** — outbound requests (needed for auth token validation and Ollama integration)
- **Auth middleware** — validate OAuth2 tokens against the C auth server on incoming requests
- **Session management** — map authenticated users to WebSocket connections
- **Broadcast / room abstraction** — send to all connections in a group
- **Graceful shutdown** — drain active connections on SIGTERM
