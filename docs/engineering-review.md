# Engineering review notes

The HTTP/2, WebSocket, managed Lack adapter and test-harness review is reflected
in the source and tests. [Production readiness](production-readiness.md) tracks
release gates. The [managed qualification snapshot](lack-compatibility.md#qualification-snapshot)
records a historical passed local and hosted Linux/macOS matrix; those receipts
do not qualify later source changes.

## Cleanup completed

- HTTP/2 ordinary responses copy bounded slices as send credit becomes available. Asynchronous writers account for bytes across streams; cancellation and pending-copy detachment have deterministic race tests.
- The hardening pass adds focused coverage for final status and HEAD/bodyless
  responses, per-call stat and static pathname preparation, request upload
  ownership/spool lifetime, TLS partial-write readiness, and listener-owned
  certificate/ALPN contexts. These implementation checks do not close the
  current slow-reader or full protocol gates.
- Libev loops are destroyed, and listener, worker, and signal shutdown close sockets from a registry snapshot. Direct and clustered shutdown tests cover open client sockets.
- Hegel uses one bounded subprocess harness and a per-run nonce for both Woo and the Rust oracle. Conformance launchers check fixture identity, use fresh artifacts, and reject missing or failing reports. Launcher timeout tests verify process-group cleanup.
- The README states the WebSocket event-loop thread requirement and distinguishes required CI tests from optional local checks.
- The optional managed adapter owns its Clack lifecycle, bounded application workers, request/output accounting and draining shutdown. Its matrix covers explicit middleware, websocket-driver, SQLite/Redis stores and full transport soaks.
- `woo.compat:call-on-connection` dispatches managed raw socket/driver operations to their owner loop. Legacy `woo:send-*` helpers retain their owner-thread contract.
- The managed dependency archives and Nix inputs are pinned; its Quicklisp bootstrap is hash-checked and its distribution is frozen. Receipts reject another HEAD, changed source/dependencies, short soaks and incomplete gates. Legacy ASDF/Clack suite failures now propagate to CI.
- The legacy lane has an independent `t/qualification/check.py` command with a
  default 1800-second soak, private dependency copy, bounded process groups,
  resource samples and an exact-head receipt verifier. It remains UNKNOWN until
  that command produces and verifies a receipt for the current head.

## Follow-up work

- Legacy threaded `:server :woo` cleanup in the pinned Clack version needs the [integration patch](../integration/clack/README.md) or an upstream equivalent. The managed profile uses its own lifecycle without that patch.
- Legacy WebSocket send helpers still require the owner thread. The managed dispatch API does not make those legacy helpers safe from arbitrary threads.
- The HTTP/2 response budget serializes bounded chunk copies under one connection lock. Profile contention before changing its ownership model.
- The Lisp ASDF test system mixes unit, live client, property, fuzz, and mutation checks; a green summary can include optional skips. Split the gates or emit a capability matrix.
- Add a recorded replay seed for Hegel generated histories.
- Audit remaining mutable CI bootstrap inputs, including action version tags and the legacy Roswell/Rove installation. h2spec and Autobahn remain opt-in diagnostics; a scheduled job should retain their reports.
- Keep the upstream hardening attribution and adaptations reviewable against
  #125/#127/#128/#129/#130/#131/#133/#135/#96/#45. Neither the upstream fixes,
  managed qualification, nor legacy qualification establishes complete RFC
  coverage or production readiness.
