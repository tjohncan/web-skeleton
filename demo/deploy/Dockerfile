# Build context is the repository root, not this directory:
#
#   docker build -f demo/deploy/Dockerfile -t web-skeleton-demo .
#
# Debian rather than Alpine, deliberately. Hostname resolution shells out to
# `getent ahosts`, which is glibc's. Busybox ships a getent whose output this
# parser has never been tested against, and the failure would be a server that
# starts cleanly and cannot resolve anything, with nothing in the log to say
# why. One base image is one thing to get wrong.
#
# bookworm's SBCL is 2.2.9, which is right for this demo and not for every
# application. The demo never fetches by hostname, so it never spawns the
# getent child SBCL leaks before 2.4.11: measured on 2.2.9, 200 fetches to a
# name that resolves left 14 zombies, and 2.6 left none. An application that
# does fetch by hostname wants a newer SBCL than this base image carries.
FROM debian:bookworm-slim

# sbcl is the runtime. libssl3 and ca-certificates are optional and are what
# turn on outbound HTTPS; without them the server starts and says TLS is
# absent. curl is here only for the healthcheck below.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      sbcl ca-certificates libssl3 curl \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

# Compile at build time. The first visitor should not pay for it, and a
# compile error should fail the build rather than the container — in either
# system the container loads. TLS is compiled here but not loaded: it loads
# libssl only at load time, so this step never needs libssl, and a container
# without it still starts and says TLS is absent. CI compiles on its runner's
# SBCL, so this is the one compile that runs on the SBCL the container does.
RUN sbcl --non-interactive \
      --eval '(require :asdf)' \
      --eval '(push #p"/app/" asdf:*central-registry*)' \
      --eval '(asdf:load-system "web-skeleton-demo")' \
      --eval '(asdf:compile-system "web-skeleton-tls")'

# /healthz, which fails if any worker has stopped passing its loop — whichever
# worker answers the probe, because it reads every worker's last pass rather
# than only its own. A TCP connect would not catch a stuck worker, since the
# listener outlives a loop that has stopped; and a probe of an ordinary page
# lands on one worker of four, so a wedged one would fail about one probe in
# four and almost never three in a row.
HEALTHCHECK --interval=30s --timeout=3s --start-period=20s --retries=3 \
  CMD curl -fsS "http://127.0.0.1:${WS_PORT:-8081}/healthz" > /dev/null || exit 1

EXPOSE 8081
CMD ["sbcl", "--non-interactive", "--load", "demo/deploy/run.lisp"]
