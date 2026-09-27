# Production readiness plan for the protocol extensions

Status: **UNKNOWN**. Passing local tests is not a production qualification.

## Scope and reference model

The release target is Woo's HTTP/1.1, HTTP/2 (h2c and TLS/ALPN), and
WebSocket server behavior. HTTP/2 and WebSocket requirements come from RFC 9113
and RFC 6455. A mature independent server can supply a useful behavior oracle
for valid exchanges, but a difference is a finding to investigate, not proof
that Woo is wrong: the RFC permits more than one valid response to some errors.

Use one Hegel-generated scenario format to drive Woo and the reference server.
For each case, record the generated input, normalized application result,
connection outcome, and resource use. Compare exact bodies, headers that have
defined semantics, stream completion, and error class. Assert mandatory RFC
behavior directly for malformed inputs. Keep the oracle implementation and
version pinned, and replay every shrunk failure against both servers.

## Gates

| Gate | Required evidence | Current state |
|------|-------------------|---------------|
| Protocol inventory | RFC MUST/MUST NOT requirements mapped to code and tests, with unsupported features explicit | UNKNOWN |
| Valid-path parity | Shared generated HTTP/1.1, HTTP/2, TLS/ALPN, and WebSocket scenarios against Woo and an independent server | PARTIAL: Hegel drives Woo and the lockfile-pinned axum fixture for HTTP/1.1, h2c, and WebSocket echo. TLS/ALPN and broader method/header/stream combinations remain unqualified. |
| Invalid-path conformance | Fragmented and malformed frames, HPACK, stream states, WebSocket masking/fragmentation, request smuggling boundaries, and timeout/error outcomes | PARTIAL: existing Lisp and Hegel tests, the opt-in h2spec baseline, and nine Autobahn control-frame cases below; no complete RFC 9113 or RFC 6455 inventory yet. |
| Resource safety | Connection/stream/body/header limits under slow peers and concurrent load; no unbounded descriptors, memory, or queue growth | OPEN: the full Lisp suite exhausts a soft 256-descriptor limit during Clack threaded fixture teardown; passing at 2048 masks that lifecycle defect. |
| Soak | Sustained representative traffic, forced disconnects, restarts, and clean shutdown with bounded latency and resource use | UNKNOWN |
| Deployment | Exact build and configuration, TLS/ALPN negotiation, observability, staged traffic, rollback, and customer-visible checks | UNKNOWN |

## Immediate work

1. Extend the shared Hegel case format to TLS/ALPN and independent generated
   headers, methods, multiplexed streams, WebSocket control frames, and close
   behavior. Preserve direct RFC assertions: parity can preserve a shared bug
   or compare two permitted but different behaviors.
2. Map RFC 9113 and RFC 6455 requirements to executable tests and an explicit
   unsupported-feature policy; run a current-RFC-aware malformed-input suite.
3. Repair the Clack/Woo shutdown integration: request a thread-safe Woo stop,
   wake the event loop, join the server thread, and retain forced thread
   destruction only as a bounded fallback. Repeat the full suite at soft 256
   with descriptor counts to prove cleanup.
4. Add bounded adversarial and long-running tests. Record time, memory, open
   descriptors, connection count, and error outcomes as artifacts.
5. Run the complete gates in CI and a staged deployment before changing this
   document's status.

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

The descriptor finding is reproducible in the test harness: Clack's threaded
`stop` destroys the Woo server thread, which can bypass Woo's event-loop
`unwind-protect` cleanup and leave accepted sockets in `CLOSE_WAIT`. A full
suite run at a soft limit of 2048 completed; that is functional test evidence,
not shutdown or resource-safety qualification.
