# Map HTTP/2 requests and responses to Clack

Enable h2c prior-knowledge detection and TLS ALPN HTTP/2, map Clack environments/responses and exercise real wire/TLS requests. Keep the legacy HTTP/1 parser and body cleanup intact until the following parser/upgrade change.

## Stack

Base: `7f15b7a882dbc28847335e923db44532e7b7462e` (PR 10, `codex/upstream-10-h2-engine`).

Head: `a6352421df46cdfcce74ff127f1e35c5e9e452f8`. Topic diff: 8 files changed, 4175 insertions(+), 59 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/7f15b7a882dbc28847335e923db44532e7b7462e...a6352421df46cdfcce74ff127f1e35c5e9e452f8).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/a6352421df46-no-ssl-woo-test-load.json).
- PASS: ['woo-test.http2-clack', 'woo-test.http2-e2e', 'woo-test.alpn', 'woo-test', 'woo-test.upload'] (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/a6352421df46-ssl-woo-test-woo-test.http2-clack-woo-test.http2-e2e-woo-test.alpn-woo-test-woo-test.upload.json).
- Hosted CI: [IN_PROGRESS](https://github.com/pyrex41/woo/actions/runs/36520528290), exact head `a6352421df46cdfcce74ff127f1e35c5e9e452f8`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
