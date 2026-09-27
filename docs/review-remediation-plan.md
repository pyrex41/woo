# Review remediation plan

Target: the current working tree, including the HTTP/2 and WebSocket changes
under review. Keep production qualification UNKNOWN until each gate has live
evidence. Work is bounded and local; remote CI and deployment are separate.

## Runtime defects

- [ ] Stream ordinary HTTP/2 response bodies without treating total body size
  as queued bytes. Prove an active-window body above 8 MiB completes and a
  withheld-window body stays within the queue limit.
- [ ] Account for asynchronous response bytes across all streams before they
  enter dispatcher closures, and release reservations on execution, rejection,
  reset, or teardown. Prove concurrent producers cannot exceed the connection
  cap and no reservation remains after failure.
- [ ] Replace destructive threaded Clack teardown with a graceful Woo event-loop
  stop and bounded join. Prove accepted sockets close at soft fd limit 256.

## Evidence defects

- [ ] Make Autobahn use an empty per-run report directory, fail on missing or
  stale results, and select a reachable Docker network on Linux.
- [ ] Make the Hegel HTTP/2 reuse property assert one physical connection and
  test overlapping streams. Exercise blocked outbound flow control with an
  explicit advertised window and WINDOW_UPDATE. Keep each generated case's
  connection history within that case.
- [ ] State accurately that consecutive TCP writes do not fix server read
  boundaries. Add deterministic parser-boundary coverage where absent.

## Validation

- [ ] Focused Lisp tests for each runtime fix and graceful teardown.
- [ ] Full Hegel suite, selected Autobahn, h2spec diagnostic, and full Lisp
  suite under bounded limits.
- [ ] Diff and artifact review; update production-readiness claims to reflect
  exact evidence and remaining gates.
