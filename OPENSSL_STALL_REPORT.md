# PR11 OpenSSL chain verification diagnostic

This branch intentionally retains the original `t/alpn.lisp` OpenSSL verification calls from PR11 `a6352421`. It adds a ten minute GitHub Actions job timeout and a 540 second shell timeout around the native test command. On failure, the step records bounded `ps` state, Linux `/proc/<pid>/wchan` values and best effort GDB thread backtraces. It does not collect cores or heap dumps.

The repeated Linux stall is observed at `test-chain-trust-and-missing-intermediate`, but the low level cause remains UNKNOWN. The pinned ASDF/UIOP implementation documents nil input as the default null device and does not establish that stdin is inherited. A separate diagnostic run with explicit stderr capture completed both OpenSSL calls in about 9 to 10 ms; a follow-up without explicit nil input also passed. That verifies stderr capture as a reliable mitigation, but does not prove which lock, stream, or subprocess state caused the original stall.

The final mitigation under review captures OpenSSL stderr and asserts the complete chain exits 0 while the missing intermediate exits 2 with the expected issuer error. This diagnostic branch is for bounded evidence only and is not a production readiness claim.
