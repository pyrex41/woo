# Add optional protocol showcase and benchmark tooling

Add optional showcase/benchmark systems and upstream usage documentation. Examples and benchmark claims remain distinct from protocol conformance and production evidence.

## Stack

Base: `564d08d9e1beca7706a81821b0ec2b5660b55313` (PR 15, `codex/upstream-15-qualification`).

Head: `c0e27b65e785842b9d673c6668c17a52a2c81b4d`. Topic diff: 25 files changed, 2314 insertions(+), 84 deletions(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/564d08d9e1beca7706a81821b0ec2b5660b55313...c0e27b65e785842b9d673c6668c17a52a2c81b4d).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: conformance-launchers (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/c0e27b65e785-extra-conformance-launchers.json).
- PASS: hegel-native-correct-oracle (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/c0e27b65e785-extra-hegel-native-correct-oracle.json).
- PASS: load-only (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/c0e27b65e785-extra-showcase-load.json).
- PASS: woo-test (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/c0e27b65e785-no-ssl-woo-test-all.json).
- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/c0e27b65e785-no-ssl-woo-test-load.json).
- PASS: woo-test (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/c0e27b65e785-ssl-woo-test-all.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520528021), exact head `c0e27b65e785842b9d673c6668c17a52a2c81b4d`.
- Hosted Legacy qualification: [FAILURE](https://github.com/pyrex41/woo/actions/runs/36520527924), exact head `c0e27b65e785842b9d673c6668c17a52a2c81b4d`.
- Hosted Lack compatibility: [PASS](https://github.com/pyrex41/woo/actions/runs/36520527923), exact head `c0e27b65e785842b9d673c6668c17a52a2c81b4d`.

Review [qualification and CI attempt notes](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/verification.md) before publication. PR 11 has a repeated hosted Linux stall; PR 15 and the assembled head have failed qualification lanes. These outcomes remain unresolved.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
