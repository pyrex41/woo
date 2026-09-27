#!/bin/sh
set -eu

# Pinned upstream release. The release has no published checksum manifest, so
# keep the asset hashes here and fail closed if GitHub serves different bytes.
H2SPEC_VERSION=v2.6.0
H2SPEC_COMMIT=70ac229
case "$(uname -s):$(uname -m)" in
  Darwin:x86_64) H2SPEC_ASSET=h2spec_darwin_amd64.tar.gz; H2SPEC_SHA256=981cb9f90a6f5e36300063022bd4eb7438d3dcf66d63a146a8541359697d1601 ;;
  Linux:x86_64) H2SPEC_ASSET=h2spec_linux_amd64.tar.gz; H2SPEC_SHA256=157ee0de702e01ad40e752dbf074b366027e550c8e7504f9450da2809e279318 ;;
  *) H2SPEC_ASSET= ;;
esac

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PORT=${WOO_H2SPEC_PORT:-}
SERVER_SCRIPT=${WOO_H2SPEC_SERVER_SCRIPT:-$ROOT/t/hegel/server.lisp}
SERVER_ROOT=${WOO_H2SPEC_SERVER_ROOT:-$(CDPATH= cd -- "$(dirname -- "$SERVER_SCRIPT")/../.." && pwd)}
LISP=${WOO_HEGEL_LISP:-${WOO_H2SPEC_LISP:-sbcl}}
H2SPEC_BIN=${H2SPEC_BIN:-}
ARTIFACT_ROOT=${H2SPEC_ARTIFACT_DIR:-$ROOT/artifacts/h2spec}
mkdir -p "$ARTIFACT_ROOT"
ARTIFACT_DIR=$(mktemp -d "$ARTIFACT_ROOT/run.XXXXXXXX")
echo "h2spec artifacts: $ARTIFACT_DIR"

if [ -z "$H2SPEC_BIN" ]; then
  if [ -z "$H2SPEC_ASSET" ]; then
    echo "h2spec is unavailable on this platform; set H2SPEC_BIN to a v2.6.0 binary" >&2
    exit 2
  fi
  CACHE=${XDG_CACHE_HOME:-${TMPDIR:-/tmp}}/woo-h2spec/$H2SPEC_VERSION
  mkdir -p "$CACHE"
  archive="$CACHE/$H2SPEC_ASSET"
  if [ ! -f "$archive" ]; then
    curl --fail --location --silent --show-error --retry 3 \
      --connect-timeout 10 --max-time 120 \
      -o "$archive" \
      "https://github.com/summerwind/h2spec/releases/download/$H2SPEC_VERSION/$H2SPEC_ASSET"
  fi
  actual=$(shasum -a 256 "$archive" | awk '{print $1}')
  if [ "$actual" != "$H2SPEC_SHA256" ]; then
    echo "h2spec checksum mismatch: expected $H2SPEC_SHA256, got $actual" >&2
    exit 2
  fi
  mkdir -p "$CACHE/bin"
  tar -xzf "$archive" -C "$CACHE/bin"
  H2SPEC_BIN=$CACHE/bin/h2spec
fi

if [ ! -x "$H2SPEC_BIN" ]; then
  echo "h2spec binary is not executable: $H2SPEC_BIN" >&2
  exit 2
fi

# Check fixed ports before launch. The nonce also closes the bind/check race:
# a listener other than this fixture cannot satisfy readiness.
PORT=$(python3 "$ROOT/t/conformance/fixture_readiness.py" port "${PORT:-0}")
READY_NONCE=$(python3 -c 'import secrets; print(secrets.token_hex(24))')

if [ ! -f "$SERVER_SCRIPT" ]; then
  echo "Woo fixture not found: $SERVER_SCRIPT" >&2
  exit 2
fi

log="$ARTIFACT_DIR/server.log"
if [ "$LISP" = ros ]; then
  (cd "$SERVER_ROOT" && exec env WOO_HEGEL_PORT="$PORT" WOO_HEGEL_READY_NONCE="$READY_NONCE" python3 "$ROOT/t/conformance/fixture_readiness.py" exec ros -e "(load \"$SERVER_SCRIPT\")") >"$log" 2>&1 &
else
  (cd "$SERVER_ROOT" && exec env WOO_HEGEL_PORT="$PORT" WOO_HEGEL_READY_NONCE="$READY_NONCE" python3 "$ROOT/t/conformance/fixture_readiness.py" exec "$LISP" --script "$SERVER_SCRIPT") >"$log" 2>&1 &
fi
server_pid=$!
cleanup() { kill "$server_pid" 2>/dev/null || true; wait "$server_pid" 2>/dev/null || true; }
trap cleanup EXIT INT TERM

python3 "$ROOT/t/conformance/fixture_readiness.py" wait "$PORT" "$server_pid" "$READY_NONCE"

"$H2SPEC_BIN" --version
python3 - "$H2SPEC_BIN" "$PORT" "$ARTIFACT_DIR/report.xml" <<'PY'
import os, pathlib, subprocess, sys
import xml.etree.ElementTree as ET
cmd = [sys.argv[1], '-h', '127.0.0.1', '-p', sys.argv[2],
       '-P', '/echo/h2spec', '-o', os.environ.get('H2SPEC_TIMEOUT', '2'),
       '--junit-report', sys.argv[3], 'http2', 'hpack', 'generic']
try:
    result = subprocess.run(cmd, timeout=int(os.environ.get('H2SPEC_RUN_TIMEOUT', '600')))
except subprocess.TimeoutExpired:
    raise SystemExit('h2spec exceeded the total time budget')
if result.returncode:
    raise SystemExit(result.returncode)
report = pathlib.Path(sys.argv[3])
if not report.is_file() or report.stat().st_size == 0:
    raise SystemExit('h2spec exited successfully without a non-empty JUnit report')
try:
    root = ET.parse(report).getroot()
except (ET.ParseError, OSError) as error:
    raise SystemExit('h2spec wrote an invalid JUnit report: ' + str(error))
try:
    suites = list(root.iter('testsuite'))
    tests = sum(int(node.attrib.get('tests', '0')) for node in suites)
    failures = sum(int(node.attrib.get('failures', '0')) for node in suites)
    errors = sum(int(node.attrib.get('errors', '0')) for node in suites)
except ValueError as error:
    raise SystemExit('h2spec wrote invalid JUnit counts: ' + str(error))
if tests <= 0:
    raise SystemExit('h2spec wrote a JUnit report with no tests')
if failures or errors:
    raise SystemExit('h2spec exited successfully but JUnit reports failures or errors')
PY

python3 "$ROOT/t/conformance/fixture_readiness.py" check "$PORT" "$server_pid" "$READY_NONCE"
