# Preserve nonblocking TLS retries and drain completed responses

Keep pending TLS write bytes immutable, handle partial writes and WANT_READ/WANT_WRITE readiness, pump static streams in bounded chunks, and complete TLS shutdown with a deadline. Generic socket ownership and budget hooks default to inactive.

## Stack

Base: `2ae685cece6d62d838d1d59c14f0650e508723fe` (PR 02, `codex/upstream-02-small-fixes`).

Head: `8af064ce3fc35112312fb2164b7900c6ddf701c2`. Topic diff: 9 files changed, 832 insertions(+), 75 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/2ae685cece6d62d838d1d59c14f0650e508723fe...8af064ce3fc35112312fb2164b7900c6ddf701c2).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/8af064ce3fc3-no-ssl-woo-test-load.json).
- PASS: ['woo-test.tls-stream', 'woo-test.tlsretry', 'woo-test.tlsreadretry'] (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/8af064ce3fc3-ssl-woo-test-woo-test.tls-stream-woo-test.tlsretry-woo-test.tlsreadretry.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520529525), exact head `8af064ce3fc35112312fb2164b7900c6ddf701c2`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Related upstream proposals

[#120](https://github.com/fukamachi/woo/pull/120), [#131](https://github.com/fukamachi/woo/pull/131). These references identify overlapping work, not a claim about current PR status. Coordinate scope and authorship with those contributors before publication.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
