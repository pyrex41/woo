# Upstream PR extraction plan

Source: fork master 4d005a686a4a5287acd77050e927cc42a6f07665.
Upstream base: 2ef0d221827becc123b46139d3965d848b04b7ef.
Correctness reference: f1fd591876be816a3f76dc0f1d95b0d5bbab5eee.

## Deliverables

Prepare sixteen clean Woo review branches, one clean Clack shutdown branch,
PR descriptions, a dependency/attribution manifest, reproducible commands,
and exact-head focused test evidence. Push review branches to the fork;
upstream PR publication is outside this preparation task. Preserve master
and the user's untracked .grok directory.

## Sequence

1. Event-loop descriptor destruction, native test gate and bounded fixture support.
2. Existing small fixes: final statuses, per-call stat and worker RNG isolation.
3. Nonblocking TLS write/read retries, bounded pumping and graceful close.
4. Listener TLS context/certificate-chain/ALPN ownership.
5. HTTP/1 response validation, framing and HEAD/bodyless contracts.
6. Static-file preparation errors and descriptor/stream ownership.
7. Request-body limits and completion/disconnect spool lifetime.
8. Server stop/join lifecycle and owning-loop dispatch infrastructure.
9. Pure HPACK and HTTP/2 frame codecs with tests.
10. HTTP/2 stream/connection state engine with tests.
11. HTTP/2 wire detection, TLS/h2c negotiation and Clack mapping.
12. WebSocket frames, upgrade/close handling and resource limits.
13. Optional managed Clack adapter: lifecycle, budgets, cancellation, body ownership.
14. Lack middleware wrappers and service contracts.
15. Extended Hegel/parity/property/fuzz/mutation/conformance/soak tooling.
16. Optional showcase and benchmark tooling.

TLS precedes static/upload extraction because their completed responses use
its graceful-close and bounded stream primitives. ALPN context ownership
precedes HTTP/2 negotiation. Native tests accompany every implementation;
the extended tools remain an explicit test lane. API descriptions are in the topic PR bodies; the complete upstream README
and evidence semantics are consolidated in the final documentation/example topic. Clack's legacy threaded stop is a separate repository PR.

## Acceptance

- Every branch has one clean topic commit over its documented parent and
  contains no generated certificates/private keys or corrective history.
- Each intermediate Woo branch loads independently and passes its focused
  tests. SSL and no-SSL loading are checked where applicable.
- Tests run from that branch's actual ASDF source directory; evidence records
  HEAD, source identity, command, result and scope. Required failures cannot
  be reported as PASS. Historical fork greens are not split-branch evidence.
- Check the assembled series against the final fork and inventory every
  changed path. Explain intentional documentation/test-layout differences.
- Run final full native, no-SSL and Hegel gates, and full managed/legacy
  qualification for the assembled qualification branch. Staged feature
  diagnostics do not qualify production or a different HEAD.
- Preserve upstream author attribution. Earlier standalone preparation
  branches are superseded; do not reuse their stale cleanup receipts.

Production readiness and complete RFC coverage remain UNKNOWN. A prepared
PR, a local gate, a hosted gate, an upstream merge and a deployment are
separate outcomes.
