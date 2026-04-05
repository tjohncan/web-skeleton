# web-skeleton

SBCL web server framework for Linux. Provides the network and protocol layer —
TCP socket management, HTTP request parsing and response building, WebSocket
handshake and framing — as a reusable foundation for web services and
real-time applications.

Minimal external dependencies beyond SBCL's built-in libraries.

## Requirements

- SBCL (Steel Bank Common Lisp)
- Linux

SBCL built-ins used: `sb-bsd-sockets` (TCP), `sb-ext` (byte conversion),
`sb-alien` (epoll/fcntl/read/write FFI), `sb-thread` (log mutex; worker pool — planned).
ASDF ships with SBCL.

## Running

```bash
sbcl --non-interactive --load run-server.lisp
```

Starts the demo server on port 8081. Open `http://localhost:8081/` in a
browser — the built-in test page opens a WebSocket connection and echoes
messages back. Ctrl-C shuts down cleanly.

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
(asdf:load-system "web-skeleton")
(web-skeleton:start-server)         ; run the demo server
(web-skeleton:test)                 ; run all tests
(web-skeleton:test-algorithms)      ; SHA-1 and Base64 only
(web-skeleton:test-server)          ; HTTP parser and response builder only
```

After editing existing files, `(asdf:load-system "web-skeleton")` recompiles
only what changed. A fresh SBCL session is only needed when adding new files
or hitting symbol conflicts.

## Project structure

```
web-skeleton.asd             ASDF system definition
build.lisp                   Build standalone binary via save-lisp-and-die
run-server.lisp              Entry point — load system and start demo server
run-tests.lisp               Entry point — load system and run test suite
src/
  package.lisp               Package (namespace) declaration
  log.lisp                   Logging (DEBUG/INFO/WARN/ERROR, timestamps)
  epoll.lisp                 Linux epoll + fcntl + read/write FFI bindings
  algorithms/
    sha1.lisp                SHA-1 digest (FIPS 180-4) — pure Lisp
    base64.lisp              Base64 encoder (RFC 4648) — pure Lisp
  server/
    connection.lisp          Connection state machine, read/write buffers
    http.lisp                HTTP request parser + response builder
    websocket.lisp           WebSocket handshake and incremental frame protocol
    main.lisp                epoll event loop, routing, server entry point
tests/
  run.lisp                   Test utilities and combined runner
  test-algorithms.lisp       SHA-1 and Base64 test vectors (FIPS, RFC)
  test-server.lisp           HTTP parser and response builder tests
```

## What's implemented

- **Worker thread pool** — one event loop per CPU core, each with its own
  listener socket (`SO_REUSEPORT`), epoll fd, and connection table.
  Kernel distributes accepts across workers. Zero shared state in the hot path
- **epoll event loop** — edge-triggered, non-blocking I/O via `sb-alien`
  FFI to Linux epoll, fcntl, read, write
- **Connection state machine** — per-connection read/write buffers, tracks
  protocol state (HTTP request parsing, response writing, WebSocket framing)
- **TCP listener** — binds a socket, accepts connections, clean shutdown on Ctrl-C
- **HTTP request parser** — method, path, query string, headers, body;
  validates against configurable size limits
- **HTTP response builder** — status codes, headers, body serialization
- **Routing** — serves the demo page on `GET /`, upgrades `/ws` to WebSocket
- **SHA-1** — complete implementation per FIPS 180-4
- **Base64** — encoder per RFC 4648
- **WebSocket handshake** — validates upgrade request, computes accept key
- **WebSocket frame protocol** — incremental frame parser and builder per
  RFC 6455, handles text, ping/pong, and close frames
- **Standalone binary** — `save-lisp-and-die` produces a single executable
- **Test suite** — algorithm test vectors (FIPS, RFC) and HTTP parser tests

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

The `port` and `workers` are passed as keyword arguments:
`(start-server :port 8081 :workers 4)`. Workers defaults to the number of
CPU cores.

## Roadmap
- **HTTP client** — outbound requests (needed for auth token validation and Ollama integration)
- **Auth middleware** — validate OAuth2 tokens against the C auth server on incoming requests
- **PostgreSQL wire protocol** — connect to Postgres without external libraries
- **Session management** — map authenticated users to WebSocket connections
- **Broadcast / room abstraction** — send to all connections in a group
- **Binary WebSocket frames** — opcode 0x2 support
- **Static file serving** — serve files from a directory
- **Graceful shutdown** — drain active connections on SIGTERM
