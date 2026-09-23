# Woo

[![CI](https://github.com/fukamachi/woo/actions/workflows/ci.yml/badge.svg)](https://github.com/fukamachi/woo/actions/workflows/ci.yml)

Woo is a fast non-blocking HTTP server built on top of [libev](http://software.schmorp.de/pkg/libev.html). Although Woo is written in Common Lisp, it aims to be the fastest web server written in any programming language.

---

## Fork: WebSocket, HTTP/2 & ALPN Support

This fork adds experimental WebSocket, HTTP/2, and ALPN support on top of
usable HTTP/1.1 Woo. These stacks are not protocol-complete; do not treat
them as production RFC 6455 / RFC 9113 implementations.

- **WebSocket (RFC 6455)** — upgrade helper and framing (client masking,
  control-frame limits, RSV, and large-frame handling are still being hardened)
- **HTTP/2 (RFC 9113)** — frames, HPACK, streams, Clack glue (HPACK Huffman,
  stream-id rules, CONTINUATION, SETTINGS, padding, and flow control still have
  known gaps)
- **SSL/ALPN** — optional TLS with ALPN (protocol must be selected after the
  handshake, not before)
- **Listen backlog** — `*default-backlog-size*` remains **128** (the listen
  `backlog` argument may be set up to 65535; that is not the default)

### WebSocket Usage

```common-lisp
(woo:run
  (lambda (env)
    (if (woo:websocket-p env)
        ;; Handle WebSocket upgrade
        (let* ((headers (getf env :headers))
               (key (gethash "sec-websocket-key" headers))
               (accept (woo:compute-accept-key key))
               (socket (getf env :clack.io)))
          (woo:write-websocket-upgrade-response socket accept)
          (woo:setup-websocket socket
            :on-message (lambda (opcode payload)
                          ;; Echo back text messages
                          (woo:send-text-frame socket
                            (trivial-utf-8:utf-8-bytes-to-string payload)))
            :on-close (lambda (code reason)
                        (declare (ignore code reason))))
          nil)  ; Return nil to prevent normal response
        ;; Normal HTTP response
        '(200 (:content-type "text/plain") ("Hello, World"))))
  :port 5000)
```

### WebSocket API

| Function | Description |
|----------|-------------|
| `(woo:websocket-p env)` | Check if request is a WebSocket upgrade |
| `(woo:compute-accept-key key)` | Compute Sec-WebSocket-Accept from client key |
| `(woo:setup-websocket socket &key on-message on-ping on-pong on-close on-error)` | Set up WebSocket handling |
| `(woo:send-text-frame socket text)` | Send a text message |
| `(woo:send-binary-frame socket data)` | Send binary data |
| `(woo:send-ping socket &optional payload)` | Send a ping frame |
| `(woo:send-pong socket payload)` | Send a pong frame |
| `(woo:send-close socket &optional code reason)` | Send a close frame |

### Running the Tests

```common-lisp
(push #P"path/to/woo/" asdf:*central-registry*)
(push #P"path/to/woo/showcase/" asdf:*central-registry*) ; for the showcase tests
(ql:quickload :woo-test)
(asdf:test-system :woo-test)
```

Prerequisites:

- **libev** and **OpenSSL** (e.g. `brew install libev openssl@3`). If CFFI
  cannot find them, push their `lib/` directory onto
  `cffi:*foreign-library-directories*` before loading.
- **Test certificates** for the TLS/ALPN tests: run `sh t/generate-certificates.sh`
  from the repo root once (creates the gitignored `t/certs/`).

Optional; each suite prints a visible rove `skip` when its tool is missing:

| Needs | Suites | Notes |
|-------|--------|-------|
| `curl` built with HTTP/2 | `t/http2-e2e.lisp` | h2c and TLS+ALPN against a live server |
| Node.js 22+ (`http2`, global `WebSocket`) | `t/http2-e2e.lisp`, `t/websocket-e2e.lisp` | independent client implementations |
| Go (1.23+) | `t/diff/diff.lisp` | differential oracle against `golang.org/x/net/http2`; first build may download modules |
| `woo-showcase` deps (ningle, djula, jonathan, …) | `t/showcase.lisp`, showcase part of `t/websocket-e2e.lisp` | fetch once with `(ql:quickload :woo-showcase)`; a real load error fails, only a missing system skips |

Environment variables:

| Variable | Effect |
|----------|--------|
| `WOO_SKIP_GO=1` | skip the Go differential tests |
| `WOO_GO_BUILD_TIMEOUT` | seconds allowed for building the Go oracle |
| `WOO_PROP_SEED`, `WOO_PROP_ITERS` | seed and iterations for `t/prop/properties.lisp`; a failure prints the seed to replay |
| `WOO_QC_SEED` | seed for the Quickcheck-style properties (`t/prop/qc.lisp`) |
| `WOO_QC_FAILURE_FILE` | where failing Quickcheck cases are saved for replay (default under the XDG cache) |
| `WOO_FUZZ_ITERS` | iterations for the coverage-guided fuzzer (`t/fuzz/guided.lisp`) |

### Test Suites

| Suite | Coverage |
|-------|----------|
| `t/hpack.lisp` | HPACK (RFC 7541): RFC Appendix C vectors, Huffman, integer bounds, dynamic-table eviction, header-list cap |
| `t/http2-frames.lisp` | frame parsing and serialization, size limits |
| `t/http2-stream.lisp` | stream state machine |
| `t/http2-connection.lisp` | connection rules, flow control and WINDOW_UPDATE, CONTINUATION, SETTINGS, GOAWAY/RST, DoS limits |
| `t/http2-clack.lisp` | Clack env and responses, streaming responder, file bodies, trailers, raw sockets |
| `t/http2-e2e.lisp` | live server driven by curl and Node over h2c and TLS |
| `t/websocket.lisp` | RFC 6455 framing, UTF-8, close codes, close handshake, buffer growth |
| `t/websocket-e2e.lisp` | live server driven by Node and raw sockets; showcase `/ws/echo` |
| `t/alpn.lisp` | ALPN parsing and an end-to-end TLS negotiation |
| `t/showcase.lisp` | showcase JSON, limits, and request handling |
| `t/prop/` | property-based tests with shrinking, including random HTTP/2 frame sequences |
| `t/diff/` | differential tests against Go's HTTP/2 and HPACK |
| `t/fuzz/` | coverage-guided fuzzing of the codecs (SBCL `sb-cover`) |
| `t/mutate/` | mutation testing: each HPACK mutant must be caught |

### Showcase Demo

A demo web app is included in `showcase/`:

```common-lisp
(push #P"path/to/woo/" asdf:*central-registry*)
(push #P"path/to/woo/showcase/" asdf:*central-registry*)
(ql:quickload :woo-showcase)
(woo-showcase:start :port 5000)
```

Features:
- Landing page with performance statistics
- Interactive benchmark charts (Woo vs Go, Node.js, etc.)
- Real-time WebSocket echo demo with latency measurement
- HTTP/2 multiplexing visualization

### New Files Added

```
src/
├── websocket.lisp          # WebSocket implementation (RFC 6455)
├── ssl-alpn.lisp           # ALPN protocol negotiation
└── http2/
    ├── constants.lisp      # Frame types, error codes, settings
    ├── hpack.lisp          # Header compression (RFC 7541)
    ├── frames.lisp         # Frame parsing & serialization
    ├── stream.lisp         # Stream state machine
    ├── connection.lisp     # Connection management & flow control
    └── clack.lisp          # Clack integration
```

---

## Original Woo Documentation

## Warning

This software is still BETA quality.

## How fast?

![Benchmark graph](images/benchmark.png)

See [benchmark.md](benchmark.md) for the detail.

## Usage

Use `clack:clackup` or `woo:run` to start a web server. The first argument is a Lack "app". See [Lack's README](https://github.com/fukamachi/lack#readme) for instruction on how to build it.

Remember to pass ":debug nil" to turn off the debugger mode on production environments (it's on by default). Otherwise, your server will go down on internal errors.

### Start a server

```common-lisp
(ql:quickload :woo)

(woo:run
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World"))))
```

### Start with Clack

```common-lisp
(ql:quickload :clack)

(clack:clackup
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World")))
  :server :woo
  :use-default-middlewares nil)
```

### Cluster

```common-lisp
(woo:run
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World")))
  :worker-num 4)
```

### SSL Support

Use SSL key arguments of `woo:run` or `clack:clackup`.

```commonlisp
(woo:run app
         :ssl-cert-file #P"path/to/cert.pem"
         :ssl-key-file #P"path/to/key.pem"
         :ssl-key-password "password")

(clack:clackup app
               :ssl-cert-file #P"path/to/cert.pem"
               :ssl-key-file #P"path/to/key.pem"
               :ssl-key-password "password")
```

To disable the HTTPS support to omit a dependency on CL+SSL, add `woo-no-ssl` to `cl:*features*`.

## Signal handling

When the master process gets these signals, it kills worker processes and quits afterwards.

- QUIT: graceful shutdown, waits for all requests are finished.
- INT/TERM: shutdown immediately.

## Benchmarks

See [benchmark.md](benchmark.md).

## Installation

### Requirements

* UNIX (GNU Linux, Mac, \*BSD)
* SBCL
* [libev](http://libev.schmorp.de)
* OpenSSL or LibreSSL (Optional)
  * To turn off SSL, add `:woo-no-ssl` to `cl:*features*` before loading Woo.

### Installing via Quicklisp

```common-lisp
(ql:quickload :woo)
```

## Docker example

* [Dockerfile](https://github.com/quickdocs/quickdocs-api/blob/master/docker/Dockerfile.production) for Quickdocs's API server.

## See Also

* [Lack](https://github.com/fukamachi/lack): Building a web application
* [Clack](https://github.com/fukamachi/clack): An abstraction layer for web servers
* [libev](http://software.schmorp.de/pkg/libev.html)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi & [contributors](https://github.com/fukamachi/woo/graphs/contributors)

## License

Licensed under the MIT License.
