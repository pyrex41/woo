# Add HTTP/2 connection and stream state handling

Add stream transitions, connection flow control, settings/continuation validation and bounded request buffers. Server protocol detection and Clack mapping follow separately.

## Stack

Base: `2403a847381cb410db26dfd1d0db38783945b5c6` (PR 09, `codex/upstream-09-h2-codecs`).

Head: `7f15b7a882dbc28847335e923db44532e7b7462e`. Topic diff: 7 files changed, 3976 insertions(+), 2 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/2403a847381cb410db26dfd1d0db38783945b5c6...7f15b7a882dbc28847335e923db44532e7b7462e).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/7f15b7a882db-no-ssl-woo-test-load.json).
- PASS: ['woo-test.http2-stream', 'woo-test.http2-connection'] (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/7f15b7a882db-ssl-woo-test-woo-test.http2-stream-woo-test.http2-connection.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520529008), exact head `7f15b7a882dbc28847335e923db44532e7b7462e`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
