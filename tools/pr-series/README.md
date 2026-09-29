# PR preparation helpers

`hosted.py` snapshots hosted checks for the current exact topic heads.
`source.py` fixes the source/base/topic order. `manifest.py` inventories the
prepared Git branches, asserts a clean linear stack and exact assembled runtime,
and generates PR bodies and durable receipts/logs. These tools inspect the
prepared branches; they do not recreate the manually extracted commit series.

The default paths describe this preparation workspace. To use another clone,
set `WOO_PR_SOURCE`, `WOO_PR_STACK_ROOT` and `WOO_PR_EVIDENCE_ROOT` before
running `manifest.py`. Keep the frozen source and upstream commits available.
Fetch the published fork branches in the manifest to recreate review worktrees.

## Native gates

```sh
python3 tools/pr-series/check.py /path/to/topic --suite woo-test.upload
python3 tools/pr-series/check.py /path/to/topic --no-ssl --load-only
python3 tools/pr-series/check.py /path/to/final --no-ssl
python3 tools/pr-series/middleware.py /path/to/middleware-topic
```

Use `WOO_PR_LISP` for SBCL, `WOO_QUICKLISP_SETUP` for the frozen 2026-01-01
Quicklisp setup and `WOO_PR_EVIDENCE_ROOT` for receipts. This host uses
`/tmp/woo-sbcl-runtime/bin/sbcl` and Homebrew/Nix native library paths; set
`WOO_PR_FOREIGN_LIB_DIRS` (colon-separated) on another host. Generate certificates
with the selected topic's `t/generate-certificates.sh` before SSL tests.

The gate checks a clean HEAD, resolves Woo from the selected worktree, and
records HEAD/tree before and after execution. Defaults: 480 seconds, SBCL
4096 MiB heap, 256 file descriptors and 16 MiB maximum individual output.
A load-only PASS does not mean tests ran. Run suites with fixed listening ports
sequentially. `middleware.py` owns a private no-persistence 64 MiB Redis
fixture, verifies its process identity and stops it after the test gate.

For managed suites, set `WOO_COMPAT_DEPENDENCY_ROOT` to the prepared private
archive directory from `t/compat/bootstrap.py`; the middleware helper defaults
to `/tmp/woo-compat-deps`. Full qualifications use the repository's
`t/compat/check.py` and `t/qualification/check.py`, including their receipt
validators and full 1800-second soaks. These helper receipts are focused
checks, not qualification receipts.

`public.py` creates public evidence copies with home-directory prefixes
replaced by `<HOME>`. The raw receipts remain private in the evidence workspace;
public metadata records their SHA256 hashes. The source HEAD/tree and outcomes
are preserved. Public command scripts contain placeholders; rerun the helper
tools to generate actual paths on your host.
