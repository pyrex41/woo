# Upstream hardening implementation

Baseline fork: 4dbfdba; upstream: 2ef0d22. Both legacy and managed adapters are in scope.

## Ordered work and acceptance

- [x] Integrate upstream OpenBSD fallback (#125).
- [ ] Per-call stat (#129), complete final-status wire support (#127/#130), response validation (#45), per-worker random states (#96).
- [ ] Static-file preparation/errors and single-owner cleanup (#133).
- [ ] Upload limits and request-scoped spool lifetime (#135), preserving managed producer lifetime.
- [ ] TLS immutable retry buffers, WANT readiness, bounded file pump, graceful shutdown (#131).
- [ ] Per-listener certificate-chain contexts and ALPN lifetime (#128).
- [ ] Focused regressions; full Lisp/Hegel; local and hosted Linux/macOS managed 1800-second gates; bounded legacy qualification.
- [ ] Accurate README/contracts/evidence; final exact-head receipts and remote ref readback.
- [ ] Independently based upstream branches with tests, attribution, dependencies and diff manifest; separate showcase tooling.

Use private worktrees and test fixtures, bounded I/O/thread joins and resource samples. Required skips, missing prerequisites and cleanup failures cannot pass. Historic receipts do not qualify a new head. No import of #81/#93. Full RFC inventory and staged deployment remain UNKNOWN.
