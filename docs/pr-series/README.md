# Prepared upstream PR series

Source `4d005a686a4a5287acd77050e927cc42a6f07665`. Upstream `2ef0d221827becc123b46139d3965d848b04b7ef`. Assembled `c0e27b65e785842b9d673c6668c17a52a2c81b4d`.

Sixteen clean Woo topic commits are prepared as a linear dependency stack. Each branch contains its predecessors; its PR size is the delta against the listed parent. Do not open every branch against upstream master at once. Publish in order after rebasing and retesting each next topic.

| Topic | Branch | Topic diff | Evidence |
|---|---|---|---|
| [01: Release event-loop descriptors and make the native test gate fail closed](01-tests.md) | [`codex/upstream-01-tests`](https://github.com/pyrex41/woo/tree/codex/upstream-01-tests) | 7 files changed, 150 insertions(+), 21 deletions(-) | no-SSL load: PASS, full native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520530543) |
| [02: Fix status coverage, stat races and worker random states](02-small-fixes.md) | [`codex/upstream-02-small-fixes`](https://github.com/pyrex41/woo/tree/codex/upstream-02-small-fixes) | 7 files changed, 334 insertions(+), 19 deletions(-) | no-SSL load: PASS, focused native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520529727) |
| [03: Preserve nonblocking TLS retries and drain completed responses](03-tls-io.md) | [`codex/upstream-03-tls-io`](https://github.com/pyrex41/woo/tree/codex/upstream-03-tls-io) | 9 files changed, 832 insertions(+), 75 deletions(-) | no-SSL load: PASS, focused native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520529525) |
| [04: Own listener TLS contexts and load certificate chains](04-tls-context.md) | [`codex/upstream-04-tls-context`](https://github.com/pyrex41/woo/tree/codex/upstream-04-tls-context) | 7 files changed, 814 insertions(+), 40 deletions(-) | no-SSL load: PASS, focused native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520527340) |
| [05: Validate HTTP/1 responses before committing headers](05-http1-responses.md) | [`codex/upstream-05-http1-responses`](https://github.com/pyrex41/woo/tree/codex/upstream-05-http1-responses) | 5 files changed, 322 insertions(+), 28 deletions(-) | no-SSL load: PASS, full native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520530632) |
| [06: Handle static file errors and transfer ownership safely](06-static-files.md) | [`codex/upstream-06-static-files`](https://github.com/pyrex41/woo/tree/codex/upstream-06-static-files) | 7 files changed, 426 insertions(+), 30 deletions(-) | no-SSL load: PASS, focused native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520529298) |
| [07: Reject oversized bodies and release request spool files](07-uploads.md) | [`codex/upstream-07-uploads`](https://github.com/pyrex41/woo/tree/codex/upstream-07-uploads) | 4 files changed, 389 insertions(+), 25 deletions(-) | no-SSL load: PASS, full native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520528369) |
| [08: Stop servers on their owning event loops and join workers](08-lifecycle.md) | [`codex/upstream-08-lifecycle`](https://github.com/pyrex41/woo/tree/codex/upstream-08-lifecycle) | 8 files changed, 401 insertions(+), 21 deletions(-) | no-SSL load: PASS, focused native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520530998) |
| [09: Add bounded HPACK and HTTP/2 frame codecs](09-h2-codecs.md) | [`codex/upstream-09-h2-codecs`](https://github.com/pyrex41/woo/tree/codex/upstream-09-h2-codecs) | 7 files changed, 2898 insertions(+), 1 deletion(-) | no-SSL load: PASS, focused native: PASS; [CI: IN_PROGRESS](https://github.com/pyrex41/woo/actions/runs/36520530272) |
| [10: Add HTTP/2 connection and stream state handling](10-h2-engine.md) | [`codex/upstream-10-h2-engine`](https://github.com/pyrex41/woo/tree/codex/upstream-10-h2-engine) | 7 files changed, 3976 insertions(+), 2 deletions(-) | no-SSL load: PASS, focused native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520529008) |
| [11: Map HTTP/2 requests and responses to Clack](11-h2-server.md) | [`codex/upstream-11-h2-server`](https://github.com/pyrex41/woo/tree/codex/upstream-11-h2-server) | 8 files changed, 4175 insertions(+), 59 deletions(-) | no-SSL load: PASS, focused native: PASS; [CI: IN_PROGRESS](https://github.com/pyrex41/woo/actions/runs/36520528290) |
| [12: Add WebSocket upgrades, framing and close handling](12-websocket.md) | [`codex/upstream-12-websocket`](https://github.com/pyrex41/woo/tree/codex/upstream-12-websocket) | 11 files changed, 3921 insertions(+), 351 deletions(-) | no-SSL load: PASS, focused native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520529107) |
| [13: Add an optional managed Clack runtime](13-managed-clack.md) | [`codex/upstream-13-managed-clack`](https://github.com/pyrex41/woo/tree/codex/upstream-13-managed-clack) | 8 files changed, 1259 insertions(+) | no-SSL load: PASS, managed native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520527799) |
| [14: Add managed Lack middleware and service contracts](14-lack-middleware.md) | [`codex/upstream-14-lack-middleware`](https://github.com/pyrex41/woo/tree/codex/upstream-14-lack-middleware) | 15 files changed, 835 insertions(+), 6 deletions(-) | no-SSL load: PASS, managed native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520529743) |
| [15: Add protocol property, parity and qualification gates](15-qualification.md) | [`codex/upstream-15-qualification`](https://github.com/pyrex41/woo/tree/codex/upstream-15-qualification) | 51 files changed, 7927 insertions(+), 43 deletions(-) | no-SSL load: PASS; [Legacy qualification: IN_PROGRESS](https://github.com/pyrex41/woo/actions/runs/36520529633), [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520529577), [Lack compatibility: IN_PROGRESS](https://github.com/pyrex41/woo/actions/runs/36520529575) |
| [16: Add optional protocol showcase and benchmark tooling](16-showcase.md) | [`codex/upstream-16-showcase`](https://github.com/pyrex41/woo/tree/codex/upstream-16-showcase) | 25 files changed, 2314 insertions(+), 84 deletions(-) | conformance-launchers: PASS, hegel-native-correct-oracle: PASS, SSL load: PASS, full native: PASS, no-SSL load: PASS, full native: PASS; [CI: PASS](https://github.com/pyrex41/woo/actions/runs/36520528021), [Legacy qualification: IN_PROGRESS](https://github.com/pyrex41/woo/actions/runs/36520527924), [Lack compatibility: IN_PROGRESS](https://github.com/pyrex41/woo/actions/runs/36520527923) |

## Publication

Fetch the fork topics and select the next unmerged topic. After its predecessors merge, create a fresh branch from updated upstream master, cherry-pick only that topic commit, inspect the resulting diff and rerun its gates. Use the PR body beside this index. The published heads are pinned in the manifest; rebasing or cherry-picking requires new evidence.

```sh
git fetch origin codex/upstream-01-tests
git fetch upstream master
git switch -c submit/01-tests upstream/master
git cherry-pick 0d27a4ce3c03fe6ba5a7bf93f03966e423fc50f3
```

For fork-only stack drafts, use the previous fork topic as the base. For upstream publication, wait until predecessors land and use current upstream master. No upstream PRs were opened in this preparation task.

## Inventory

All 138 fork-changed paths are represented. Runtime differences from the frozen source: none. Additional files are focused correctness suites retained from corrected staging. The assembled test-suite layout consolidates equivalent lifecycle/detection helpers after WebSocket integration. README/readiness docs intentionally remove fork badges and historical qualification claims. Full provenance and exact SHAs are in [manifest.json](manifest.json).

## Separate Clack PR

The optional legacy Clack stop hook is a separate repository change. Its standalone patch, PR description and test evidence are in `clack/`. No upstream PR has been published.

## Limits

Local tests, managed/legacy diagnostics, full source-bound qualifications, hosted CI, upstream merge and deployment are separate outcomes. Fresh results are attached only to their exact heads. Public copies redact home directory paths as `<HOME>` and retain raw receipt/artifact SHA256 hashes. Unmodified originals remain in the preparation workspace; public scripts show commands with placeholders, while the helper tools generate runnable paths locally. Production readiness and complete RFC coverage remain UNKNOWN.
