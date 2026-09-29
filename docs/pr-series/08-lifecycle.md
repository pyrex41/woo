# Stop servers on their owning event loops and join workers

Route stop/drain requests to the owning loop, remove stop controls during cleanup, close worker sockets, and add owning-loop dispatch support for later protocol/managed work.

## Stack

Base: `a8f96400a32981fe3ea5d1357e6f245834b6784a` (PR 07, `codex/upstream-07-uploads`).

Head: `15826f3236e34f36d281bb7ee23611370be06778`. Topic diff: 8 files changed, 401 insertions(+), 21 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/a8f96400a32981fe3ea5d1357e6f245834b6784a...15826f3236e34f36d281bb7ee23611370be06778).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/15826f3236e3-no-ssl-woo-test-load.json).
- PASS: ['woo-test'] (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/15826f3236e3-ssl-woo-test-woo-test.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520530998), exact head `15826f3236e34f36d281bb7ee23611370be06778`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Related upstream proposals

[#81](https://github.com/fukamachi/woo/pull/81). These references identify overlapping work, not a claim about current PR status. Coordinate scope and authorship with those contributors before publication.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
