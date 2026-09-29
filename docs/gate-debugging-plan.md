# Remaining gate investigation

Frozen baseline: assembled PR head `c0e27b65e785842b9d673c6668c17a52a2c81b4d`.
PR 11 baseline: `a6352421df46cdfcce74ff127f1e35c5e9e452f8`.
Existing source branches and failed receipts remain unchanged during diagnosis.

## Work

1. OpenSSL: reproduce the repeated Linux stall in the full PR 11 sequence;
   locate the blocked operation, add bounded process diagnostics, then fix the
   cause and verify full native Linux tests at the new head.
2. HTTPS rejection: repeat the managed cancellation/budget sequence and inspect
   the request-write/413/TLS-close ordering. Verify the intended wire contract
   with a focused regression before changing runtime or test expectations.
3. Legacy Linux memory: sample each process in the owned group and correlate
   resident growth with Go/SBCL allocation and GC. Reproduce the full workload,
   attribute the retained memory, and fix the cause without weakening the
   existing 64 MiB growth requirement.
4. Integrate independently reviewed changes into this isolated branch, run
   focused checks followed by necessary source-bound hosted gates, and update
   the PR packet with exact heads, evidence and remaining limitations.

## Constraints and acceptance

- One heavy build per investigation; infrastructure commands and artifact sizes
  stay bounded. Hosted qualification retains the 1,800-second requirement.
- No upstream PR publication or messages. Diagnostic branches may be pushed to
  the user's fork with normal repository hooks.
- No secrets or generated private keys in commits. Raw failed evidence is
  preserved, and qualification remains UNKNOWN until the validators pass.
- Repeated native Linux stalls must be explained, not relabeled by a retry.
- Passing workload tests alone do not establish cleanup or resource stability.
- Memory results remain lane-specific: the legacy gate measures aggregate
  owned-group RSS with a 64 MiB growth allowance; the managed Hegel gate measures
  its fixture process with a separate 256 MiB allowance.
- Any changed runtime must pass both managed and legacy qualification at the
  final integrated head before those gates are marked satisfied.

## Fresh observations and follow-up

At supplemental debug head `7c0a797`, hosted CI and Linux managed full
qualification passed. Linux legacy completed 29,594 workload cycles, 30
lifecycle cycles and cleanup, but aggregate RSS grew by 264,699,904 bytes
(252.4375 MiB), exceeding the unchanged 64 MiB allowance.

The macOS managed lane received a connection reset before reading the 413
response during a concurrent oversized TLS upload. This is a wire failure,
separate from the historical Go request-writer error. Its HTTP/2 drain fixture
also raised an undefined-function condition; the function must be identified
before attributing the failure to readiness. The macOS legacy lane exhausted
its compiler heap before startup, so it provides no soak result.

The candidate adds a shared FD-sampling deadline, fresh group snapshots,
preservation of earlier samples after a sampler failure, and PID-tagged
metrics for every SBCL generation. These changes do not relax qualification.
A separate controlled full-GC experiment gathers memory attribution evidence;
it is diagnostic evidence, regardless of the experiment's default receipt
status, and is not production qualification. Historical OpenSSL stalls remain
unexplained despite successful original-default and pinned reruns.

At candidate `1df477f`, hosted native/conformance/parity/Hegel CI passed, but
macOS managed concurrent HTTPS rejection still lost a response to a reset.
HTTP/2 drain passed in that run. The macOS legacy sampler failed before the
workload after retaining three valid bootstrap samples; its generic error
did not identify whether RSS or FD collection failed. The follow-up classifies
those failures and keeps bounded retries and positive-sample requirements.

The TLS follow-up must drain peer application records with `SSL_read` after
the local close alert, before calling `SSL_shutdown` again. Its acceptance
checks cover mutually exclusive readiness watchers, callback routing, bounded
continuation when OpenSSL buffers input, fatal EOF and the unchanged absolute
deadline. Local tests do not clear the observed hosted macOS reset.

The controlled Linux experiment reclaimed 340.375 MiB of SBCL RSS after full
GC. Most excess RSS was reclaimable; a smaller retained-object leak is still
unexcluded without matching full-GC startup and end snapshots. That experiment
and the matched-baseline follow-up remain diagnostic only. No production GC
endpoint, default heap increase, or relaxed RSS allowance is accepted here.
