# Woo

[![CI](https://github.com/pyrex41/woo/actions/workflows/ci.yml/badge.svg)](https://github.com/pyrex41/woo/actions/workflows/ci.yml)

Woo is a Common Lisp HTTP server built on [libev](http://software.schmorp.de/pkg/libev.html). This fork adds HTTP/2, WebSocket, and TLS/ALPN support to the original HTTP/1.1 server.

**Release status:** HTTP/2 and WebSocket support are experimental. Local tests and interoperability checks pass within their stated scope, but production readiness is **UNKNOWN**. See [production readiness](docs/production-readiness.md) for the evidence and open gates.

## Install and run

Woo requires SBCL, libev, and a Unix-like system. TLS support also requires OpenSSL or LibreSSL. To omit the CL+SSL dependency, add `:woo-no-ssl` to `cl:*features*` before loading Woo. On macOS, `brew install libev openssl@3` supplies the native libraries.

To load this checkout instead of the Quicklisp release:

~~~common-lisp
(push #P"path/to/woo/" asdf:*central-registry*)
(ql:quickload :woo)
~~~

Start a server with a Lack application:

~~~common-lisp
(woo:run
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World")))
  :port 5000
  :debug nil)
~~~

`woo:run` defaults to port 5000, loopback address `127.0.0.1`, and listen backlog 128. The `:backlog` argument accepts values up to 65535. Use `:debug nil` outside development; debugger mode is enabled by default.

Clack can start Woo with `:server :woo`:

~~~common-lisp
(clack:clackup
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World")))
  :server :woo
  :use-default-middlewares nil)
~~~

### TLS and workers

~~~common-lisp
(woo:run app
         :ssl-cert-file #P"path/to/cert.pem"
         :ssl-key-file #P"path/to/key.pem"
         :worker-num 4
         :debug nil)
~~~

Woo negotiates HTTP/2 or HTTP/1.1 through ALPN after the TLS handshake. Cleartext HTTP/2 prior knowledge is also supported. The `:worker-num` option runs worker threads.

### Shutdown

For a server started on its own thread, call `(woo:stop-gracefully thread)` and then join the thread. This requests shutdown on the owning event loop and closes accepted sockets. **It does not drain active requests.** `SIGQUIT` requests orderly loop and worker shutdown; `SIGINT` and `SIGTERM` stop workers immediately. Active connections may be interrupted.

The installed Clack threaded stop path destroys the server thread. Before relying on `clack:clackdown` for cleanup, apply and verify the [Clack integration patch](integration/clack/README.md) in the Clack version actually used by the application.

## Protocol extensions

### WebSocket

The application handles the upgrade and then supplies callbacks:

~~~common-lisp
(woo:run
  (lambda (env)
    (if (woo:websocket-p env)
        (let* ((headers (getf env :headers))
               (key (gethash "sec-websocket-key" headers))
               (socket (getf env :clack.io)))
          (woo:write-websocket-upgrade-response
           socket (woo:compute-accept-key key))
          (woo:setup-websocket
           socket
           :on-message
           (lambda (opcode payload)
             (declare (ignore opcode))
             (woo:send-binary-frame socket payload)))
          nil)
        '(200 (:content-type "text/plain") ("Hello, World"))))
  :port 5000
  :debug nil)
~~~

`woo:send-text-frame`, `woo:send-binary-frame`, `woo:send-ping`, `woo:send-pong`, and `woo:send-close` write frames. Call `setup-websocket` and the send helpers on the socket's owning event-loop thread. Callbacks run there; these helpers do not dispatch calls from other threads.

### HTTP/2 response bodies

Woo retains ordinary in-memory response parts and copies bounded slices as send credit becomes available. Keep strings and octet vectors unchanged until the response finishes or its stream closes. A withheld send window does not copy the complete body into the send queue. Streaming writers have per-stream and per-connection queued-byte limits.

## Tests

Generate local TLS test certificates once:

~~~sh
sh t/generate-certificates.sh
~~~

Run the Lisp suite from this checkout:

~~~common-lisp
(push #P"path/to/woo/" asdf:*central-registry*)
(push #P"path/to/woo/showcase/" asdf:*central-registry*)
(ql:quickload :woo-test)
(asdf:test-system :woo-test)
~~~

If CFFI cannot find libev or OpenSSL, add their `lib/` directories to `cffi:*foreign-library-directories*` before loading Woo. The suite includes unit, property, mutation, differential, and live client checks. Some checks skip when an optional tool is unavailable:

| Optional dependency | Checks |
| --- | --- |
| HTTP/2-capable `curl` | live h2c and TLS tests |
| Node.js 22+ | independent HTTP/2 and WebSocket clients |
| Go 1.23+ | Lisp-to-Go HTTP/2 and HPACK differential tests |
| `woo-showcase` dependencies | showcase tests; missing systems skip, other load errors fail |

A green Lisp summary can include skips. Inspect its test output before treating a capability as covered. `WOO_SKIP_GO=1` explicitly skips the Go differential tests.

The separate Hegel suite requires Go 1.26+ and Rust 1.97+. It drives live HTTP/1.1, h2c, and WebSocket cases and compares valid responses with the lockfile-pinned [axum oracle](t/hegel/oracle/README.md):

~~~sh
go -C t/hegel test -count=1 -timeout 10m ./...
~~~

It is required in CI. Each generated HTTP history uses a fresh connection, and HTTP/2 histories cannot silently redial. Both fixtures require a per-run readiness nonce. Hegel shrinks counterexamples; consecutive TCP writes do not guarantee distinct server reads.

Useful test settings:

| Variable | Purpose |
| --- | --- |
| `WOO_PROP_SEED`, `WOO_PROP_ITERS` | Lisp property seed and iteration count |
| `WOO_QC_SEED`, `WOO_QC_FAILURE_FILE` | Quickcheck replay and failure file |
| `WOO_FUZZ_ITERS` | coverage-guided fuzz iterations |
| `WOO_HEGEL_LISP` | Hegel fixture Lisp launcher; `sbcl` by default, `ros` in CI |
| `WOO_HEGEL_FOREIGN_LIB_DIRS` | colon-separated native library directories for the Hegel fixture |
| `WOO_HEGEL_ORACLE_BIN` | prebuilt Rust oracle; otherwise Cargo builds it with `--locked` |

The opt-in [h2spec and Autobahn diagnostics](t/conformance/README.md) exercise separate protocol cases. Their results do not replace the RFC inventory, sustained resource tests, or staged deployment in the [readiness plan](docs/production-readiness.md).

## Project

See [benchmark.md](benchmark.md) for the original benchmark details. Woo was created by Eitaro Fukamachi and [contributors](https://github.com/fukamachi/woo/graphs/contributors). See [Lack](https://github.com/fukamachi/lack) and [Clack](https://github.com/fukamachi/clack) for the application interface.

Licensed under the MIT License.
