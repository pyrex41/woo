# Validate HTTP/1 responses before committing headers

Validate response shape, status, header names/values and framing before writing bytes; normalize supported bodies and handle HEAD/bodyless responses. Before-commit errors produce a bounded 500 and later errors close the connection.

## Stack

Base: `f31d9bcdd7b8a9b9fefef58bbeee887e8ffbc8a4` (PR 04, `codex/upstream-04-tls-context`).

Head: `2ad8b5d2088bae037fbe215dafa5caa66269cce8`. Topic diff: 5 files changed, 322 insertions(+), 28 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/f31d9bcdd7b8a9b9fefef58bbeee887e8ffbc8a4...2ad8b5d2088bae037fbe215dafa5caa66269cce8).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/2ad8b5d2088b-no-ssl-woo-test-load.json).
- PASS: woo-test (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/2ad8b5d2088b-ssl-woo-test-all.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520530632), exact head `2ad8b5d2088bae037fbe215dafa5caa66269cce8`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Related upstream proposals

[#93](https://github.com/fukamachi/woo/pull/93). These references identify overlapping work, not a claim about current PR status. Coordinate scope and authorship with those contributors before publication.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
