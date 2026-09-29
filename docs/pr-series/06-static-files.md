# Handle static file errors and transfer ownership safely

Classify pathname failures before writing headers and retain the opened descriptor or TLS stream until transfer completion. Cover missing/unreadable/directory paths, declared lengths and graceful TLS error closure.

## Stack

Base: `2ad8b5d2088bae037fbe215dafa5caa66269cce8` (PR 05, `codex/upstream-05-http1-responses`).

Head: `5a5f105e68d9c0b5d77ff1c08e50502c2a2aabe5`. Topic diff: 7 files changed, 426 insertions(+), 30 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/2ad8b5d2088bae037fbe215dafa5caa66269cce8...5a5f105e68d9c0b5d77ff1c08e50502c2a2aabe5).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/5a5f105e68d9-no-ssl-woo-test-load.json).
- PASS: ['woo-test.static', 'woo-test.tls-stream'] (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/5a5f105e68d9-ssl-woo-test-woo-test.static-woo-test.tls-stream.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520529298), exact head `5a5f105e68d9c0b5d77ff1c08e50502c2a2aabe5`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Related upstream proposals

[#133](https://github.com/fukamachi/woo/pull/133). These references identify overlapping work, not a claim about current PR status. Coordinate scope and authorship with those contributors before publication.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
