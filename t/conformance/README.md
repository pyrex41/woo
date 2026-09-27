# h2spec conformance runner

`h2spec.sh` runs the pinned upstream h2spec release against the live Woo
fixture at `t/hegel/server.lisp`. It starts the fixture on loopback, waits up
to 120 seconds for a cold Lisp compilation, runs the HTTP/2, HPACK, and generic suites, and
writes the JUnit report and bounded server log under `artifacts/h2spec/`.

This is intentionally an opt in diagnostic runner while the first baseline is
being investigated. It is not a required CI check yet.

The 2026-09-27 local baseline ran 146 cases: 143 passed and three failed.
The failures are invalid-preface GOAWAY and two priority self-dependency
expectations from RFC 7540. RFC 9113 section 3.4 permits omitting GOAWAY for
an invalid preface; section 5.3.2 deprecates the old priority tree semantics.
See `docs/production-readiness.md` for the interpretation and open gates.
The h2spec command still exits nonzero and its report must be retained.

## Run

From the repository root:

```sh
t/conformance/h2spec.sh
```

The launcher downloads and verifies the v2.6.0 release on Intel macOS and
Linux x86-64. On other platforms, install or build the exact v2.6.0 binary and
set `H2SPEC_BIN=/path/to/h2spec`. The source release identifies commit
`70ac229`; the upstream release page is
<https://github.com/summerwind/h2spec/releases/tag/v2.6.0>.
For the local Apple Silicon source build used for this baseline,
`go version -m` reports revision `70ac2294010887f48b18e2d64f5cccd48421fad1`;
its internal `--version` string says `2.0.0 ((Unknown))` because release
metadata was not injected at build time.

The existing fixture contract is used as follows:

* `WOO_HEGEL_LISP` selects `sbcl` (default) or `ros`.
* `WOO_H2SPEC_SERVER_SCRIPT` overrides the fixture path for a checkout whose
  fixture is elsewhere.
* `WOO_H2SPEC_SERVER_ROOT` overrides the working directory used by the fixture;
  set it when invoking this worktree against a fixture in another checkout.
* `WOO_H2SPEC_PORT` fixes the loopback port; otherwise an ephemeral port is
  selected.
* `H2SPEC_TIMEOUT` controls h2spec's per-test timeout and defaults to 2s.
* `H2SPEC_RUN_TIMEOUT` bounds the entire h2spec run (default 600s).
* `H2SPEC_ARTIFACT_DIR` changes the report directory.

h2spec v2.6.0 targets RFC 7540 and RFC 7541. `-S` is deliberately not passed
by this runner; strict `SHOULD` cases should be added to a scheduled run after
the normal baseline is understood. The tool requires a GET or POST endpoint
that returns status 200 with a non-empty body. A passing run is protocol
evidence for this tool's covered cases, not proof of RFC 9113 completeness,
WebSocket behavior, load capacity, or resource exhaustion safety.

## Autobahn WebSocket diagnostic run

`autobahn.sh` runs `crossbario/autobahn-testsuite:25.10.1` pinned by digest
against `/ws`. It uses a short explicit control-frame subset (nine case
families) that matches the current binary WebSocket fixture and writes the
HTML reports, generated config, and Woo log under `artifacts/autobahn/`. The
run is opt in and is not a required CI check.
The 2026-09-27 local baseline reported `OK` behavior and close behavior for
all nine selected cases. The script validates the exact case set and both
result fields in `servers/index.json` before returning success.

```sh
t/conformance/autobahn.sh
```

The runner pulls the image if needed, waits up to 120 seconds for Woo, and
terminates the test container after 120 seconds by default. Set
`WOO_AUTOBAHN_TIMEOUT` to change that bound. OrbStack defaults to host
networking and `127.0.0.1`; other Docker engines default to
`host.docker.internal`. Linux deployments can set `WOO_AUTOBAHN_NETWORK=host`.
The fixture must be reachable from the container, so use
`WOO_AUTOBAHN_HOST` if the local Docker networking setup needs another host
name. `WOO_AUTOBAHN_SERVER_SCRIPT`, `WOO_AUTOBAHN_SERVER_ROOT`,
`WOO_AUTOBAHN_PORT`, `WOO_AUTOBAHN_ARTIFACT_DIR`, and `WOO_AUTOBAHN_LISP` mirror
the h2spec runner's overrides.

The upstream image is intentionally frozen on an old PyPy/OpenSSL stack for
reproducibility. The script uses the verified release digest and the
`linux/amd64` platform. This subset checks WebSocket control-frame behavior;
it does not cover text echo,
fragmented data, invalid UTF-8, compression, performance, or all handshake
cases. Use the upstream `cases: ["*"]` configuration as a separately bounded
nightly run after the fixture supports the corresponding semantics.
