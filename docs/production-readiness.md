# Production readiness plan for the protocol extensions

Status: **UNKNOWN**. Passing local tests is not a production qualification.
The active Clack dependency, sustained traffic, remote CI, and deployment
checks have not been qualified for this working tree.

## Scope and reference model

The release target is Woo's HTTP/1.1, HTTP/2 (h2c and TLS/ALPN), and
WebSocket server behavior. HTTP/2 and WebSocket requirements come from RFC 9113
and RFC 6455. A mature independent server can supply a useful behavior oracle
for valid exchanges, but a difference is a finding to investigate, not proof
that Woo is wrong: the RFC permits more than one valid response to some errors.

The current Hegel suite compares generated valid-path behavior with a
lockfile-pinned axum fixture. A broader shared scenario format, resource
observations, and direct assertions for malformed inputs remain work to do.
Parity alone cannot establish RFC compliance.

## Gates

| Gate | Required evidence | Current state |
|------|-------------------|---------------|
| Protocol inventory | RFC MUST/MUST NOT requirements mapped to code and tests, with unsupported features explicit | UNKNOWN |
| Valid-path parity | Shared generated HTTP/1.1, HTTP/2, TLS/ALPN, and WebSocket scenarios against Woo and an independent server | PARTIAL: Hegel drives Woo and the lockfile-pinned axum fixture for HTTP/1.1, h2c, and WebSocket echo. Each generated HTTP history owns a fresh connection; HTTP/2 histories cannot redial. Raw Woo-only tests check two open request streams, stream and connection response-credit stalls/resumption, and an exact body above 8 MiB. TLS/ALPN and broader method/header/stream combinations remain unqualified. |
| Invalid-path conformance | Fragmented and malformed frames, HPACK, stream states, WebSocket masking/fragmentation, request smuggling boundaries, and timeout/error outcomes | PARTIAL: existing Lisp and Hegel tests, the opt-in h2spec baseline, and nine Autobahn control-frame cases below; no complete RFC 9113 or RFC 6455 inventory yet. |
| Resource safety | Connection/stream/body/header limits under slow peers and concurrent load; no unbounded descriptors, memory, or queue growth | PARTIAL: ordinary HTTP/2 bodies copy budgeted slices; asynchronous writers reserve against shared limits, with deterministic reset/copy and pending-detachment race tests. Thirty direct stop cycles held five descriptors, and the full local Lisp suite passed with a soft 256-descriptor limit. Slow peers, sustained concurrency, and the installed Clack threaded stop path remain unqualified. |
| Soak | Sustained representative traffic, forced disconnects, restarts, and clean shutdown with bounded latency and resource use | UNKNOWN |
| Deployment | Exact build and configuration, TLS/ALPN negotiation, observability, staged traffic, rollback, and customer-visible checks | UNKNOWN |

## Immediate work

1. Extend the shared Hegel case format to TLS/ALPN and independent generated
   headers, methods, multiplexed streams, WebSocket control frames, and close
   behavior. Preserve direct RFC assertions: parity can preserve a shared bug
   or compare two permitted but different behaviors.
2. Map RFC 9113 and RFC 6455 requirements to executable tests and an explicit
   unsupported-feature policy; run a current-RFC-aware malformed-input suite.
3. Apply and verify the [Clack integration patch](../integration/clack/README.md)
   in the active dependency, or land its upstream equivalent. Repeat the
   bounded descriptor probe in that installed configuration.
4. Add bounded adversarial and long-running tests. Record time, memory, open
   descriptors, connection count, and error outcomes as artifacts.
5. Run the complete gates in CI and a staged deployment before changing this
   document's status.

## Local validation snapshot

| Check | Result | Limit |
|------|--------|-------|
| Lisp test system at soft descriptor limit 256 | 17 suites passed | Optional checks can skip; inspect the test output. |
| Full Hegel suite | Passed | Valid-path cases and selected Woo-only protocol assertions. |
| Conformance launcher regressions | Nine passed, plus focused JUnit validation cases | Uses fake tools and fixtures. |
| Selected Autobahn control-frame cases | 9/9 passed | Does not cover the full WebSocket suite. |
| h2spec v2.6.0 | 143/146 passed; exit nonzero | Targets older RFC 7540 expectations in the three failing cases. |

These are local results from 2026-09-27, not remote CI or deployed evidence.

## Current conformance evidence and limits

The local h2spec v2.6.0 run against the live Woo fixture executed 146 cases:
143 passed and three failed. The runner and pinned tool are in
`t/conformance/`; this diagnostic is not a required CI gate yet. All three
failures exercise legacy RFC 7540 expectations:

| Case | Observed Woo behavior | Current RFC 9113 interpretation |
|------|-----------------------|---------------------------------|
| Invalid client preface | Closed without GOAWAY | Section 3.4 explicitly allows omitting GOAWAY. |
| HEADERS self dependency | Processed request | Section 5.3.2 deprecates the RFC 7540 priority tree semantics; frame format remains valid. |
| PRIORITY self dependency | Ignored priority signal | Same deprecated priority tree semantics. |

These interpretations follow [RFC 9113 section 3.4](https://www.rfc-editor.org/rfc/rfc9113.html#section-3.4)
and [section 5.3.2](https://www.rfc-editor.org/rfc/rfc9113.html#section-5.3.2).
They do not turn a failing h2spec result into a pass. Keep the
raw report and rerun after changes. A newer spec-aware check and independent
interoperability tests are still needed. The axum oracle exercises valid
application behavior; it does not decide the permitted error response for
malformed frames.

The opt-in Autobahn runner in `t/conformance/` targets a bounded subset of
WebSocket control-frame cases. The pinned container ran nine cases against
this checkout; all nine had `OK` behavior and close behavior in its generated
report. This is narrow WebSocket control-frame evidence, not qualification of
the full RFC 6455 surface. The runner checks the report and fails if cases are
missing or any selected result is not `OK`.

Both conformance launchers now reject an occupied port and require a per-run
nonce from the Woo fixture before running and after successful tool execution.
Their regression tests cover an old listener, startup failure, wrong identity,
and fixture exit during a run. These checks bind the local reports to the
launched fixture; they do not bind them to a deployed build.

The original descriptor failure had two distinct causes. Woo freed libev's
loop memory without calling `ev_loop_destroy`, leaking two backend descriptors
per stopped loop. After the fix, 30 direct graceful start/stop cycles held at
five descriptors, and the full local Lisp suite passed with a soft limit of
256. A clustered graceful-stop test also closed twelve open client sockets.
The earlier `CLOSE_WAIT` sockets observed in that suite were client-side
Dexador connections: Clack's test harness disables pooling while its requests
retain keep-alive, so ignored client streams can wait for garbage collection.
Clack's installed threaded `stop` still destroys the Woo server thread. A
source patch in `integration/clack/` passed 12 local start/stop cycles with
stable descriptors, but has not been applied to the installed Clack or upstream.
These local checks do not establish sustained production resource safety.
