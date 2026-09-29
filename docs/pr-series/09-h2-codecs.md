# Add bounded HPACK and HTTP/2 frame codecs

Add bounded HPACK encoding/decoding, frame codecs and constants with pure regression coverage. This does not enable a listening HTTP/2 server.

## Stack

Base: `15826f3236e34f36d281bb7ee23611370be06778` (PR 08, `codex/upstream-08-lifecycle`).

Head: `2403a847381cb410db26dfd1d0db38783945b5c6`. Topic diff: 7 files changed, 2898 insertions(+), 1 deletion(-). [Review exact topic diff](https://github.com/pyrex41/woo/compare/15826f3236e34f36d281bb7ee23611370be06778...2403a847381cb410db26dfd1d0db38783945b5c6).

This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.

## Verification

- PASS: load-only (no SSL); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/2403a847381c-no-ssl-woo-test-load.json).
- PASS: woo-test (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/2403a847381c-ssl-woo-test-all.json).
- PASS: ['woo-test.hpack', 'woo-test.http2-frames'] (SSL enabled); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/receipts/2403a847381c-ssl-woo-test-woo-test.hpack-woo-test.http2-frames.json).
- Hosted CI: [PASS](https://github.com/pyrex41/woo/actions/runs/36520530272), exact head `2403a847381cb410db26dfd1d0db38783945b5c6`.

Review [qualification and CI attempt notes](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/docs/pr-series/verification.md) before publication. PR 11 has a repeated hosted Linux stall; PR 15 and the assembled head have failed qualification lanes. These outcomes remain unresolved.

Receipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.

## Provenance

Extracted from fork source `4d005a686a4a5287acd77050e927cc42a6f07665`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.
