# Production readiness

Status: **UNKNOWN**. The managed Clack/Lack profile passed its required local
and hosted Linux/macOS gates at `fed54b3` on 2026-09-28, including full
30-minute soaks. See the [qualification snapshot](lack-compatibility.md#qualification-snapshot)
for the exact commit, source digest, dependency pins and CI evidence. Complete
RFC coverage and staged production deployment have not been established.

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
| Valid-path parity | Shared generated HTTP/1.1, HTTP/2, TLS/ALPN, and WebSocket scenarios against Woo and an independent server | PARTIAL: Hegel compares HTTP/1.1, h2c and WebSocket echo with the pinned axum oracle. Managed tests cover HTTP/1, HTTPS, h2c and HTTP/2 TLS, with a separate Hunchentoot reference check. TLS oracle parity and broader generated methods/headers/multiplexing remain open. |
| Invalid-path conformance | Fragmented and malformed frames, HPACK, stream states, WebSocket masking/fragmentation, request smuggling boundaries, and timeout/error outcomes | PARTIAL: existing Lisp and Hegel tests, the opt-in h2spec baseline, and nine Autobahn control-frame cases below; no complete RFC 9113 or RFC 6455 inventory yet. |
| Resource safety | Connection/stream/body/header limits under slow peers and concurrent load; no unbounded descriptors, memory, or queue growth | PARTIAL: the managed snapshot passed admission/output budgets, cancellation, drain, uncooperative-worker reporting, 100 lifecycle cycles and descriptor/RSS checks. These budgets exclude application allocations. Broader slow-peer and adversarial connection/header workloads remain open; legacy threaded Clack cleanup needs its separate integration. |
| Soak | Sustained representative traffic, forced disconnects, restarts, and clean shutdown with bounded latency and resource use | PARTIAL: required 30-minute managed transport soaks passed locally and on hosted Linux/macOS. Production-duration, application-specific and adversarial workloads remain unqualified. |
| Deployment | Exact build and configuration, TLS/ALPN negotiation, observability, staged traffic, rollback, and customer-visible checks | UNKNOWN |

## Immediate work

1. Extend the shared Hegel case format to TLS/ALPN and independent generated
   headers, methods, multiplexed streams, WebSocket control frames, and close
   behavior. Preserve direct RFC assertions: parity can preserve a shared bug
   or compare two permitted but different behaviors.
2. Map RFC 9113 and RFC 6455 requirements to executable tests and an explicit
   unsupported-feature policy; run a current-RFC-aware malformed-input suite.
3. For legacy threaded `:server :woo`, apply and verify the
   [Clack integration patch](../integration/clack/README.md) in the active
   dependency, or land its upstream equivalent. The managed profile owns its
   threads without patching Clack and has passed its separate lifecycle matrix
   at the recorded snapshot.
4. Extend the managed resource/soak matrix with slow peers, adversarial
   connection/header loads and longer application-specific traffic. Preserve
   bounded deadlines, resource samples and source-bound receipts.
5. Qualify the release snapshot and configuration in a staged deployment,
   including observability, rollback and customer-visible checks, before
   changing production status.

## Historical local validation: 2026-09-27

| Check | Result | Limit |
|------|--------|-------|
| Lisp test system at soft descriptor limit 256 | 17 suites passed | Optional checks can skip; inspect the test output. |
| Full Hegel suite | Passed | Valid-path cases and selected Woo-only protocol assertions. |
| Conformance launcher regressions | Nine passed, plus focused JUnit validation cases | Uses fake tools and fixtures. |
| Selected Autobahn control-frame cases | 9/9 passed | Does not cover the full WebSocket suite. |
| h2spec v2.6.0 | 143/146 passed; exit nonzero | Targets older RFC 7540 expectations in the three failing cases. |

These are local results from 2026-09-27, not remote CI or deployed evidence.

## Historical conformance evidence and limits

The 2026-09-27 local h2spec v2.6.0 run against the live Woo fixture executed
146 cases: 143 passed and three failed. The runner and pinned tool are in
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
WebSocket control-frame cases. The 2026-09-27 pinned-container run executed nine
cases against that snapshot; all nine had `OK` behavior and close behavior in
its generated report. This is narrow WebSocket control-frame evidence, not
qualification of the full RFC 6455 surface. The runner checks the report and fails if cases are
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
The Clack version used by that legacy test destroys the Woo server thread
when its threaded `stop` path is used. A source patch in `integration/clack/`
passed 12 local start/stop cycles with stable descriptors. Woo does not apply
that patch to installed or upstream Clack.
These local checks do not establish sustained production resource safety.

## Managed Lack profile

The optional `woo-lack-compat` profile has a separate
[compatibility contract and required matrix](lack-compatibility.md). It owns
Clack lifecycle, application workers and draining shutdown. Both hosted
Linux/macOS managed gates and existing protocol gates passed at the
[recorded snapshot](lack-compatibility.md#qualification-snapshot). Future
release snapshots need their own receipts; diagnostic soaks and partial
receipts do not close those gates. Deployment readiness remains UNKNOWN.
