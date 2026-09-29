# Add an optional managed Clack runtime

Add an optional managed adapter with application workers, queue/body/output budgets, cancellation, completion accounting and graceful drain. It depends on completed HTTP/1, HTTP/2 and WebSocket ownership and transport foundations.

## Stack

Base: `7c565470a8eb51de4cadca6285edf05c93c44c2a` (PR 12, `codex/upstream-12-websocket`).

Head: `d7585aab637b14a46b482f990e307b687358d4da`. Topic diff: 8 files changed, 1259 insertions(+). [Review exact topic diff](https://github.com/pyrex41/woo/compare/7c565470a8eb51de4cadca6285edf05c93c44c2a...d7585aab637b14a46b482f990e307b687358d4da).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/d7585aab637b-no-ssl-woo-test-load.json).
- PASS: woo-lack-compat/tests (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/d7585aab637b-ssl-woo-lack-compat_tests-all.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520527799), exact head `d7585aab637b14a46b482f990e307b687358d4da`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
