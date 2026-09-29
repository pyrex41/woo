# Axum parity oracle

This is a disposable reference service for the Hegel differential harness. It
binds `127.0.0.1` and prints `READY 127.0.0.1:<port>` only after the listener
has been created:

```sh
cargo run --locked --manifest-path t/hegel/oracle/Cargo.toml -- --port 0
```

The harness may set `WOO_HEGEL_PORT`; an explicit `--port` takes precedence.
For fixture identity, it sets `WOO_HEGEL_READY_NONCE` and checks that
`GET /.woo-test-ready` returns that exact value.

`axum::serve` accepts HTTP/1.1 and HTTP/2 prior-knowledge connections on the
same cleartext listener. `/ws` accepts the normal HTTP/1 WebSocket upgrade;
HTTP/2 extended CONNECT has not been exercised by this harness. The fixture intentionally
matches `t/hegel/server.lisp`: `/body` returns the request bytes, `/echo/*`
returns the request path, and `/ws` echoes binary messages only.

This is a valid-path behavior oracle, not a specification proof. `Cargo.lock`
is committed, and CI builds the oracle with `--locked`. The opt-in h2spec and
Autobahn diagnostics cover separate protocol cases; their scope and results
are recorded in the [conformance guide](../../conformance/README.md).
