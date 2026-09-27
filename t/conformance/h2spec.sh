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
ARTIFACT_DIR=${H2SPEC_ARTIFACT_DIR:-$ROOT/artifacts/h2spec}
mkdir -p "$ARTIFACT_DIR"

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

if [ -z "$PORT" ]; then
  PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()')
fi

if [ ! -f "$SERVER_SCRIPT" ]; then
  echo "Woo fixture not found: $SERVER_SCRIPT" >&2
  exit 2
fi

log="$ARTIFACT_DIR/server.log"
if [ "$LISP" = ros ]; then
  (cd "$SERVER_ROOT" && exec env WOO_HEGEL_PORT="$PORT" ros -e "(load \"$SERVER_SCRIPT\")") >"$log" 2>&1 &
else
  (cd "$SERVER_ROOT" && exec env WOO_HEGEL_PORT="$PORT" "$LISP" --script "$SERVER_SCRIPT") >"$log" 2>&1 &
fi
server_pid=$!
cleanup() { kill "$server_pid" 2>/dev/null || true; wait "$server_pid" 2>/dev/null || true; }
trap cleanup EXIT INT TERM

python3 - "$PORT" <<'PY'
import socket, sys, time
port = int(sys.argv[1])
deadline = time.time() + 120
while time.time() < deadline:
    try:
        with socket.create_connection(("127.0.0.1", port), timeout=.2):
            raise SystemExit(0)
    except OSError:
        time.sleep(.05)
raise SystemExit("Woo did not listen on 127.0.0.1:%d within 120s" % port)
PY

"$H2SPEC_BIN" --version
python3 - "$H2SPEC_BIN" "$PORT" "$ARTIFACT_DIR/report.xml" <<'PY'
import os, subprocess, sys
cmd = [sys.argv[1], '-h', '127.0.0.1', '-p', sys.argv[2],
       '-P', '/echo/h2spec', '-o', os.environ.get('H2SPEC_TIMEOUT', '2'),
       '--junit-report', sys.argv[3], 'http2', 'hpack', 'generic']
try:
    raise SystemExit(subprocess.run(cmd, timeout=int(os.environ.get('H2SPEC_RUN_TIMEOUT', '600'))).returncode)
except subprocess.TimeoutExpired:
    raise SystemExit('h2spec exceeded the total time budget')
PY
