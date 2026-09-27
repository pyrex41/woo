# Engineering review notes

The HTTP/2, WebSocket, and test-harness review is reflected in the current source and tests. [Production readiness](production-readiness.md) tracks release gates and local evidence.

## Cleanup completed

- HTTP/2 ordinary responses copy bounded slices as send credit becomes available. Asynchronous writers account for bytes across streams; cancellation and pending-copy detachment have deterministic race tests.
- Libev loops are destroyed, and listener, worker, and signal shutdown close sockets from a registry snapshot. Direct and clustered shutdown tests cover open client sockets.
- Hegel uses one bounded subprocess harness and a per-run nonce for both Woo and the Rust oracle. Conformance launchers check fixture identity, use fresh artifacts, and reject missing or failing reports. Launcher timeout tests verify process-group cleanup.
- The README states the WebSocket event-loop thread requirement and distinguishes required CI tests from optional local checks.

## Follow-up work

- The installed Clack threaded stop path still destroys the Woo thread. Apply and verify the [integration patch](../integration/clack/README.md) or land its upstream equivalent.
- WebSocket send helpers have an owner-thread contract. Cross-thread dispatch would need a separate API and concurrency tests.
- The HTTP/2 response budget serializes bounded chunk copies under one connection lock. Profile contention before changing its ownership model.
- The Lisp ASDF test system mixes unit, live client, property, fuzz, and mutation checks; a green summary can include optional skips. Split the gates or emit a capability matrix.
- Add a recorded replay seed for Hegel generated histories.
- Pin and authenticate mutable CI dependency inputs. h2spec and Autobahn are opt-in diagnostics; a scheduled job should retain their reports.
