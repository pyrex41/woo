# Add WebSocket upgrades, framing and close handling

Add WebSocket framing, upgrade buffering, masking/fragmentation/control validation, close handling and size limits. Export helpers with their event-loop ownership requirement.

## Stack

Base: `a6352421df46cdfcce74ff127f1e35c5e9e452f8` (PR 11, `codex/upstream-11-h2-server`).

Head: `7c565470a8eb51de4cadca6285edf05c93c44c2a`. Topic diff: 11 files changed, 3921 insertions(+), 351 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/a6352421df46cdfcce74ff127f1e35c5e9e452f8...7c565470a8eb51de4cadca6285edf05c93c44c2a).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/7c565470a8eb-no-ssl-woo-test-load.json).
- PASS: ['woo-test.websocket', 'woo-test.websocket-e2e', 'woo-test'] (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/7c565470a8eb-ssl-woo-test-woo-test.websocket-woo-test.websocket-e2e-woo-test.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520529107), exact head `7c565470a8eb51de4cadca6285edf05c93c44c2a`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
