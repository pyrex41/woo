# Release event-loop descriptors and make the native test gate fail closed

Destroy libev loops on normal and failed cleanup, including their kernel descriptors. Add a descriptor regression and fail CI and ASDF test-op when Rove fails. Freeze the Quicklisp distribution and provide bounded native fixtures and certificate-chain fixtures.

## Stack

Base: `2ef0d221827becc123b46139d3965d848b04b7ef` (upstream master).

Head: `0d27a4ce3c03fe6ba5a7bf93f03966e423fc50f3`. Topic diff: 7 files changed, 150 insertions(+), 21 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/2ef0d221827becc123b46139d3965d848b04b7ef...0d27a4ce3c03fe6ba5a7bf93f03966e423fc50f3).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/0d27a4ce3c03-no-ssl-woo-test-load.json).
- PASS: woo-test (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/0d27a4ce3c03-ssl-woo-test-all.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520530543), exact head `0d27a4ce3c03fe6ba5a7bf93f03966e423fc50f3`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
