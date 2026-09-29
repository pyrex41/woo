# Production readiness

Status: **UNKNOWN**. HTTP/2, WebSocket and the managed Lack profile remain
experimental. This repository provides executable test and qualification lanes;
results must be collected and verified from the exact checkout being reviewed.

## Required evidence

| Gate | Evidence | Limit |
|------|----------|-------|
| Native tests | `asdf:test-system :woo-test`, both SSL and no-SSL | Regression coverage, not a complete RFC inventory. |
| Independent properties | `go -C t/hegel test -count=1 -skip '^TestManaged' -timeout 10m ./...` | Pinned axum parity covers generated valid exchanges; differences need spec review. |
| Managed profile | `t/compat/check.py`, default 1800-second run and receipt verification | Requires private pinned dependencies, Redis/SQLite fixtures, transport and cleanup gates. |
| Legacy Clack | `t/qualification/check.py`, default 1800-second run and receipt verification | Requires the separate Clack threaded-stop integration patch. |
| Conformance diagnostics | `t/conformance/h2spec.sh` and `autobahn.sh` | Optional bounded diagnostics; partial suites and nonzero results remain partial/failing. |
| Deployment | Representative staged traffic, adversarial/slow peers, operational rollback | No deployment evidence is supplied by a local or CI pass. |

The protocol targets are RFC 9113 and RFC 6455. A complete requirement-to-test
inventory, broader malformed-input scenarios, TLS oracle parity and
application-specific production workloads remain open. The managed budgets
cover server-owned storage and queues, not arbitrary application allocations.

## Result semantics

Record HEAD, source digest, dependency pins, platform, commands and cleanup.
A load-only result establishes loadability. Focused tests establish their
listed behavior. Short soaks produce DIAGNOSTIC_PASS. A full qualification PASS
requires the validator to accept every required gate at the same source.
Historical fork receipts and results from another branch do not qualify this
PR series. A merged PR and a production rollout are separate outcomes.

See [Lack compatibility](lack-compatibility.md),
[conformance tooling](../t/conformance/README.md) and
[Clack integration](../integration/clack/README.md) for reproducible setup.
