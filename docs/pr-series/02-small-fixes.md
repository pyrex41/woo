# Fix status coverage, stat races and worker random states

Complete HTTP final-status formatting, remove the shared SBCL stat destination and isolate each worker random state. These are three small existing-defect fixes with separate suites.

## Stack

Base: `0d27a4ce3c03fe6ba5a7bf93f03966e423fc50f3` (PR 01, `codex/upstream-01-tests`).

Head: `2ae685cece6d62d838d1d59c14f0650e508723fe`. Topic diff: 7 files changed, 334 insertions(+), 19 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/0d27a4ce3c03fe6ba5a7bf93f03966e423fc50f3...2ae685cece6d62d838d1d59c14f0650e508723fe).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/2ae685cece6d-no-ssl-woo-test-load.json).
- PASS: ['woo-test.file-size', 'woo-test.worker', 'woo-test.response'] (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/2ae685cece6d-ssl-woo-test-woo-test.file-size-woo-test.worker-woo-test.response.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520529727), exact head `2ae685cece6d62d838d1d59c14f0650e508723fe`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Related upstream proposals

[#96](https://github.com/fukamachi/woo/pull/96), [#127](https://github.com/fukamachi/woo/pull/127), [#129](https://github.com/fukamachi/woo/pull/129), [#130](https://github.com/fukamachi/woo/pull/130). These references identify overlapping work, not a claim about current PR status. Coordinate scope and authorship with those contributors before publication.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
