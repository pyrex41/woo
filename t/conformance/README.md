# Optional conformance diagnostics

These launchers exercise a live Woo fixture from `t/hegel/server.lisp`. They are local diagnostics, **not required CI gates**. Their reports do not establish complete RFC 9113 or RFC 6455 coverage; see [production readiness](../../docs/production-readiness.md).

The launcher regression tests below are required in the existing CI workflow.
Running those tests does not execute h2spec or the Autobahn suite.

| Runner | Scope | Local result on 2026-09-27 |
| --- | --- | --- |
| `h2spec.sh` | HTTP/2, HPACK, and generic cases from h2spec v2.6.0 | 143 of 146 passed; command exited nonzero |
| `autobahn.sh` | Nine selected WebSocket control-frame cases | 9 of 9 reported OK behavior and close behavior |

## Run

From the repository root:

~~~sh
t/conformance/h2spec.sh
t/conformance/autobahn.sh
~~~

The h2spec runner verifies the published v2.6.0 archive hash on Intel macOS and Linux x86-64. On other platforms, provide a build of that version with `H2SPEC_BIN=/path/to/h2spec`. The release source commit is `70ac229`. A custom binary is checked for executability; its version string is not independently authenticated.

The Autobahn runner uses a digest-pinned `crossbario/autobahn-testsuite` image and requires Docker. Its selected cases cover ping, pong, and chopped control frames. Text echo, fragmented data, invalid UTF-8, compression, performance, and the complete handshake surface are outside this run.

Each invocation creates a fresh `artifacts/h2spec/run.XXXXXXXX/` or `artifacts/autobahn/run.XXXXXXXX/` directory. The h2spec runner requires a nonempty, parseable JUnit report with positive test count and no reported failures or errors after a zero-exit tool run. The Autobahn runner requires the exact selected case set and OK behavior and close behavior for every case. Existing reports cannot qualify a new run.

## Fixture identity and limits

Both runners reject an occupied loopback port. The launched fixture must return a new per-run nonce from `GET /.woo-test-ready` before the tool runs and after a successful result. An override fixture must honor `WOO_HEGEL_PORT` and return `WOO_HEGEL_READY_NONCE` at that endpoint. This prevents a previous listener from qualifying the wrong checkout.

Fixture startup is limited to 120 seconds. h2spec has a 600-second total tool limit by default and a 2-second per-case timeout; Autobahn has a 120-second container limit by default. The fixture has a 64 MiB per-file write limit, which bounds its server log and fails the run if exceeded. The runners stop the fixture on exit.

| Variable | Effect |
| --- | --- |
| `WOO_HEGEL_LISP` | Fixture launcher, `sbcl` by default or `ros` |
| `WOO_H2SPEC_SERVER_SCRIPT`, `WOO_AUTOBAHN_SERVER_SCRIPT` | Override fixture script |
| `WOO_H2SPEC_SERVER_ROOT`, `WOO_AUTOBAHN_SERVER_ROOT` | Override fixture working directory |
| `WOO_H2SPEC_PORT`, `WOO_AUTOBAHN_PORT` | Fixed loopback port; otherwise an ephemeral port is chosen |
| `H2SPEC_BIN` | Exact h2spec binary on platforms without a verified archive |
| `H2SPEC_TIMEOUT`, `H2SPEC_RUN_TIMEOUT` | h2spec case and total time limits |
| `WOO_AUTOBAHN_TIMEOUT` | Autobahn container time limit |
| `H2SPEC_ARTIFACT_DIR`, `WOO_AUTOBAHN_ARTIFACT_DIR` | Artifact root |

Autobahn uses host networking on native Linux and OrbStack and `host.docker.internal` elsewhere. Set `WOO_AUTOBAHN_NETWORK` and `WOO_AUTOBAHN_HOST` when that default does not reach the fixture.

Run the bounded launcher regressions without Docker or Lisp:

~~~sh
python3 t/conformance/test_autobahn_runner.py
~~~

The h2spec v2.6.0 failures concern an invalid-preface GOAWAY and two RFC 7540 priority self-dependency expectations. The diagnostic remains nonzero; [production readiness](../../docs/production-readiness.md) records the interpretation and open gates.
