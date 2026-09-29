# Verification and publication gates

The extraction is complete: 16 clean linear Woo topics and one independent Clack
topic are pushed to the user forks. All 138 changed fork paths are represented,
and the assembled Woo runtime matches frozen source `4d005a686a4a5287acd77050e927cc42a6f07665`.
The clean commit boundaries, PR descriptions and provenance are ready for review.
The following evidence does **not** establish that every topic is ready to merge.

## Native and hosted checks

All 16 Woo topics passed native no-SSL load checks. Focused native checks are
recorded against each exact topic head. The assembled head passed full native
tests with and without SSL, the Go Hegel/Rust parity gate, the bounded conformance
launcher tests and the optional showcase load check. Scope and commands are in
the linked receipts; these checks do not establish complete RFC coverage.

Hosted native CI passed for 15 of the 16 topics. PR 11 remains UNKNOWN after two
cancelled Linux attempts. The current terminal states and observation time are in
[hosted-ci.json](hosted-ci.json). Attempt logs are preserved in
[ci-attempts/manifest.json](ci-attempts/manifest.json).

### PR 09: lifecycle fixture collision

[Run 36520530272](https://github.com/pyrex41/woo/actions/runs/36520530272)
attempt 1 failed `graceful-stop-by-server-thread`: the randomly selected port
51248 could not be bound (OS error 98). The background thread entered the Lisp
debugger, subsequent checks failed, and the process stalled after Rove reported
a failure. That attempt was cancelled. Attempt 2 passed at the same exact head
`2403a847381cb410db26dfd1d0db38783945b5c6`; the full native suite also passed locally.
The successful retry does not erase the fixture failure. A future test cleanup
should reserve listening ports, capture background-thread startup errors and
bound the native CI step so failures cannot leave it waiting in a debugger.

### PR 11: repeated OpenSSL subprocess stall

[Run 36520528290](https://github.com/pyrex41/woo/actions/runs/36520528290)
attempts 1 and 2 both stopped producing output at
`test-chain-trust-and-missing-intermediate` in `t/alpn.lisp`. No subsequent test
assertion or error was logged. Attempt 2 was cancelled at the ten-minute bound.
The exact head is `a6352421df46cdfcce74ff127f1e35c5e9e452f8`.

Local focused and full native suites passed at that head. Those results do not
replace hosted Linux evidence. The test calls `uiop:run-program` for OpenSSL
chain verification without a subprocess deadline. The cause of the stall is
UNKNOWN; the logs alone do not distinguish process launch, verification or
waiting for process output/exit. Before publication, instrument and bound that
subprocess, reproduce the full Linux sequence and obtain a fresh passing run.

## Full qualification

The qualification runners pin dependencies and bind receipts to source digests
and checkout heads. Their mandatory full soak is 1,800 seconds. A passing Go
workload alone is insufficient: receipt validation and cleanup must also pass.

| Head | Managed Lack macOS | Managed Lack Linux | Legacy macOS | Legacy Linux |
|---|---|---|---|---|
| PR 15 `564d08d9e1be` | FAIL: HTTPS cancellation/budget response | PASS, full soak | PASS, full soak | FAIL: RSS growth |
| PR 16 `c0e27b65e785` | PASS, full soak | PASS, full soak | PASS, full soak | FAIL: RSS growth |

Actual receipts are under [qualification/](qualification/manifest.json), with
intermediate evidence in `qualification/managed-pr15/` and
`qualification/legacy-pr15/`. Public copies redact home-directory prefixes and
retain raw-file SHA256 hashes. Generated private-key/certificate fixtures are
excluded from this packet.

### Managed HTTPS failure on PR 15

[Run 36520529575](https://github.com/pyrex41/woo/actions/runs/36520529575)
failed on macOS in `TestManagedBudgetAndCancellation/https`: the test expected
HTTP 413 but the client received `broken pipe`. Woo logged a 131072-byte buffer
limit rejection. HTTP/1, h2c and h2tls subtests passed. Cleanup passed; the soak
did not run. The Linux lane and both assembled-head lanes passed their full
managed qualification.

PR 15 and PR 16 have identical runtime sources; a later green run is therefore
insufficient evidence that this intermittent HTTPS outcome is fixed. Reproduce
the upload/cancellation ordering, establish whether the client completes its
write before the rejection closes the transport, and verify the intended wire
contract before treating this lane as publication-ready.

### Legacy RSS bound failures on Linux

The validator requires final soak resident memory to be at most baseline +
64 MiB. Both Linux runs exceeded that allowance:

| Head | Baseline RSS bytes | Final RSS bytes | Growth | Soak sample span | FDs |
|---|---:|---:|---:|---:|---|
| PR 15 | 283115520 | 474759168 | 182.7 MiB | 1799.03 s | 33 → 30 |
| PR 16 | 380993536 | 648577024 | 255.2 MiB | 1799.53 s | 33 → 30 |

The duration, descriptor-growth allowance and global 1 GiB peak cap passed.
The Go workload completed successfully, but the writer rejected its resource
predicate before recording passing gates and cleanup; the receipts remain
FAIL with cleanup UNKNOWN. PR 16's RSS increase was sustained across many
samples rather than a single final spike.

These aggregate process-group observations do not identify a Woo leak versus
Go/SBCL heap growth or Linux allocator retention. Before publication of the
qualification topic, add per-process RSS and heap/GC diagnostics, reproduce the
same workload and explain the growth. Preserve the 64 MiB gate until evidence
supports a reviewed policy change. Do not relabel the failed receipts.

## Separate Clack topic

The Clack fork branch has two passing focused tests and 12 passing live Woo
start/request/stop cycles. Its patch, bundle, branch and evidence are in
[clack/README.md](clack/README.md). No full Clack suite or hosted Clack CI pass is
claimed. The legacy qualifier applies the equivalent runtime patch privately;
upstream acceptance of that separate change is a dependency to resolve.

## Next publication steps

Review and submit early topics in order against current upstream master. After
each predecessor merges, cherry-pick only the next topic, inspect its delta and
rerun its gates on the new head. Resolve the PR 11 subprocess stall and PR 15
qualification failures before publishing those topics as ready to merge.
Production readiness and deployed behavior remain UNKNOWN.
