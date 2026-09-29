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
