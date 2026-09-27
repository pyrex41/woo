# Axum parity oracle

This is a disposable reference service for the Hegel differential harness. It
binds `127.0.0.1` and prints `READY 127.0.0.1:<port>` only after the listener
has been created:

```sh
cargo run --manifest-path t/hegel/oracle/Cargo.toml -- --port 0
```

The harness may set `WOO_HEGEL_PORT`; an explicit `--port` takes precedence.

`axum::serve` accepts HTTP/1.1 and HTTP/2 prior-knowledge connections on the
same cleartext listener. `/ws` accepts the normal HTTP/1 WebSocket upgrade and
the RFC 8441 extended CONNECT path supported by axum. The fixture intentionally
matches `t/hegel/server.lisp`: `/body` returns the request bytes, `/echo/*`
returns the request path, and `/ws` echoes binary messages only.

This is a behavior oracle, not a specification proof. Pin `Cargo.lock` before
using it as a CI baseline, and run h2spec and WebSocket conformance suites as
separate gates.
