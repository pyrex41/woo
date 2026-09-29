# Upstream hardening implementation

Baseline fork: 4dbfdba; upstream: 2ef0d22. Both legacy and managed adapters are in scope.

## Ordered work and acceptance

- [x] Integrate upstream OpenBSD fallback (#125).
- [x] Adapt per-call stat (#129), complete final-status wire support (#127/#130),
  response validation (#45), and per-worker random states (#96). The adaptations
  preserve Woo's managed producer dispatch and legacy owner-loop contracts.
- [x] Adapt static-file preparation, error classification and single-owner
  cleanup (#133), with private fixtures and focused regressions.
- [x] Adapt upload limits and request-scoped spool lifetime (#135), preserving
  the managed producer lifetime through wire completion.
- [x] Adapt TLS immutable retry buffers, WANT readiness, bounded file pumping
  and graceful shutdown (#131).
- [x] Adapt per-listener certificate-chain contexts and ALPN lifetime (#128).
- [x] Add focused regressions, the Lisp/Hegel test lanes, and a bounded legacy
  qualification command with an exact-head receipt verifier.
- [ ] Complete current-head managed Linux/macOS 1800-second gates. The
  historical managed receipts do not qualify this source. Focused native and
  independent Go TLS checks pass; full release qualification requires fresh
  exact-head receipts.
- [ ] Run the final legacy 1800-second qualification on the release head,
  record its receipt, and perform exact-head remote ref readback. Existing
  managed receipts do not qualify the legacy lane.
- [x] Document the upstream attribution and safe adaptations, keep showcase
  checks separate from the core `woo-test` gate, and retain bounded logs and
  receipts.

Use private worktrees and test fixtures, bounded I/O/thread joins and resource samples. Required skips, missing prerequisites and cleanup failures cannot pass. Historic receipts do not qualify a new head. No import of #81/#93. Full RFC inventory and staged deployment remain UNKNOWN.

The implementation follows upstream Woo pull requests #125, #127, #128,
#129, #130, #131, #133, #135, #96 and #45. The local commit attribution names
fukamachi for #125, solipsismes and diasbruno for the #127/#129/#130 group,
iwami4438 for #133, skyizwhite for #135, and PuercoPop and Eitaro Fukamachi for
the #45/#96 work. The #128/#131 adaptation retains the upstream PR references
in its commit message. Local changes add Woo-specific ownership, managed
adapter and qualification tests. These are safe adaptations of the upstream
behavior to the fork's HTTP/2, TLS, managed-worker and legacy-thread contracts;
upstream parity is not treated as an RFC or production-completeness claim.
