# Add managed Lack middleware and service contracts

Add explicit managed Lack wrappers for session, mount, compression, logging and backtrace handling, plus middleware and private Redis/SQLite service contracts. Native Woo remains usable without loading this system.

## Stack

Base: `d7585aab637b14a46b482f990e307b687358d4da` (PR 13, `codex/upstream-13-managed-clack`).

Head: `43105d204d1c2243e7197000c55edb89ff43cc5e`. Topic diff: 15 files changed, 835 insertions(+), 6 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/d7585aab637b14a46b482f990e307b687358d4da...43105d204d1c2243e7197000c55edb89ff43cc5e).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/43105d204d1c-no-ssl-woo-test-load.json).
- PASS: woo-lack-compat/tests (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/43105d204d1c-ssl-woo-lack-compat_tests-all.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520529743), exact head `43105d204d1c2243e7197000c55edb89ff43cc5e`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
