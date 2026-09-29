# Add protocol property, parity and qualification gates

Add native properties/shrinking, Go/Rust parity, codec fuzz/mutation, bounded conformance launchers and exact-source managed/legacy qualification runners. A short diagnostic is not qualification; complete RFC coverage and deployment remain UNKNOWN.

## Stack

Base: `43105d204d1c2243e7197000c55edb89ff43cc5e` (PR 14, `codex/upstream-14-lack-middleware`).

Head: `564d08d9e1beca7706a81821b0ec2b5660b55313`. Topic diff: 51 files changed, 7927 insertions(+), 43 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/43105d204d1c2243e7197000c55edb89ff43cc5e...564d08d9e1beca7706a81821b0ec2b5660b55313).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/564d08d9e1be-no-ssl-woo-test-load.json).
- Hosted Legacy qualification: [IN_PROGRESS](https://github.com/pyrex41/woo/actions/runs/36520529633), exact head `564d08d9e1beca7706a81821b0ec2b5660b55313`.
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520529577), exact head `564d08d9e1beca7706a81821b0ec2b5660b55313`.
- Hosted Lack compatibility: [IN_PROGRESS](https://github.com/pyrex41/woo/actions/runs/36520529575), exact head `564d08d9e1beca7706a81821b0ec2b5660b55313`.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
