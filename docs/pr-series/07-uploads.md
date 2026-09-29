# Reject oversized bodies and release request spool files

Return 413 for declared or accumulated oversized bodies, preserve fragmented/pipelined requests, and release saved request-body streams/files on response completion and disconnect, including keepalive.

## Stack

Base: `5a5f105e68d9c0b5d77ff1c08e50502c2a2aabe5` (PR 06, `codex/upstream-06-static-files`).

Head: `a8f96400a32981fe3ea5d1357e6f245834b6784a`. Topic diff: 4 files changed, 389 insertions(+), 25 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/5a5f105e68d9c0b5d77ff1c08e50502c2a2aabe5...a8f96400a32981fe3ea5d1357e6f245834b6784a).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/a8f96400a329-no-ssl-woo-test-load.json).
- PASS: woo-test (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/a8f96400a329-ssl-woo-test-all.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520528369), exact head `a8f96400a32981fe3ea5d1357e6f245834b6784a`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Related upstream proposals

[#135](https://github.com/fukamachi/woo/pull/135). These references identify overlapping work, not a claim about current PR status. Coordinate scope and authorship with those contributors before publication.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
