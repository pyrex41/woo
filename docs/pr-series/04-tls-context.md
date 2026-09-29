# Own listener TLS contexts and load certificate chains

Create and release listener-owned SSL contexts, load full PEM chains, validate keys, and keep ALPN callback storage per context. HTTP/2 dispatch follows in PR 11.

## Stack

Base: `8af064ce3fc35112312fb2164b7900c6ddf701c2` (PR 03, `codex/upstream-03-tls-io`).

Head: `f31d9bcdd7b8a9b9fefef58bbeee887e8ffbc8a4`. Topic diff: 7 files changed, 814 insertions(+), 40 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/8af064ce3fc35112312fb2164b7900c6ddf701c2...f31d9bcdd7b8a9b9fefef58bbeee887e8ffbc8a4).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/f31d9bcdd7b8-no-ssl-woo-test-load.json).
- PASS: ['woo-test.alpn', 'woo-test.tls-stream'] (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/f31d9bcdd7b8-ssl-woo-test-woo-test.alpn-woo-test.tls-stream.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520527340), exact head `f31d9bcdd7b8a9b9fefef58bbeee887e8ffbc8a4`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Related upstream proposals

[#128](https://github.com/fukamachi/woo/pull/128). These references identify overlapping work, not a claim about current PR status. Coordinate scope and authorship with those contributors before publication.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
