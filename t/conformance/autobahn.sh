#!/bin/sh
set -eu

# Frozen upstream image and verified release digest for reproducible runs.
AUTOBAHN_IMAGE=${WOO_AUTOBAHN_IMAGE:-crossbario/autobahn-testsuite@sha256:519915fb568b04c9383f70a1c405ae3ff44ab9e35835b085239c258b6fac3074}
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PORT=${WOO_AUTOBAHN_PORT:-}
SERVER_SCRIPT=${WOO_AUTOBAHN_SERVER_SCRIPT:-$ROOT/t/hegel/server.lisp}
SERVER_ROOT=${WOO_AUTOBAHN_SERVER_ROOT:-$(CDPATH= cd -- "$(dirname -- "$SERVER_SCRIPT")/../.." && pwd)}
LISP=${WOO_HEGEL_LISP:-${WOO_AUTOBAHN_LISP:-sbcl}}
ARTIFACT_ROOT=${WOO_AUTOBAHN_ARTIFACT_DIR:-$ROOT/artifacts/autobahn}
TIMEOUT=${WOO_AUTOBAHN_TIMEOUT:-120}
mkdir -p "$ARTIFACT_ROOT"
# Never accept a report left by another execution. Keep each run for diagnosis.
ARTIFACT_DIR=$(mktemp -d "$ARTIFACT_ROOT/run.XXXXXXXX")
echo "Autobahn artifacts: $ARTIFACT_DIR"

if ! command -v docker >/dev/null 2>&1; then
  echo "Docker is required for the pinned Autobahn testsuite image" >&2
  exit 2
fi
if ! docker image inspect "$AUTOBAHN_IMAGE" >/dev/null 2>&1; then
  perl -e 'alarm shift; exec @ARGV' 300 docker pull "$AUTOBAHN_IMAGE"
fi

network=${WOO_AUTOBAHN_NETWORK:-}
if [ -z "$network" ]; then
  if [ "$(uname -s)" = Linux ] ||
     [ "$(docker info --format '{{.OperatingSystem}}')" = OrbStack ]; then
    network=host
  else
    network=bridge
  fi
fi
if [ "$network" = host ]; then
  HOST=${WOO_AUTOBAHN_HOST:-127.0.0.1}
else
  HOST=${WOO_AUTOBAHN_HOST:-host.docker.internal}
fi

# Check fixed ports before launch. The nonce also closes the bind/check race:
# a listener other than this fixture cannot satisfy readiness.
PORT=$(python3 "$ROOT/t/conformance/fixture_readiness.py" port "${PORT:-0}")
READY_NONCE=$(python3 -c 'import secrets; print(secrets.token_hex(24))')
if [ ! -f "$SERVER_SCRIPT" ]; then
  echo "Woo fixture not found: $SERVER_SCRIPT" >&2
  exit 2
fi

config_dir="$ARTIFACT_DIR/config"
mkdir -p "$config_dir"
sed -e "s/__HOST__/$HOST/g" -e "s/__PORT__/$PORT/g" \
  "$ROOT/t/conformance/autobahn-fuzzingclient.json.in" \
  >"$config_dir/fuzzingclient.json"
log="$ARTIFACT_DIR/server.log"
if [ "$LISP" = ros ]; then
  (cd "$SERVER_ROOT" && exec env WOO_HEGEL_PORT="$PORT" WOO_HEGEL_READY_NONCE="$READY_NONCE" python3 "$ROOT/t/conformance/fixture_readiness.py" exec ros -e "(load \"$SERVER_SCRIPT\")") >"$log" 2>&1 &
else
  (cd "$SERVER_ROOT" && exec env WOO_HEGEL_PORT="$PORT" WOO_HEGEL_READY_NONCE="$READY_NONCE" python3 "$ROOT/t/conformance/fixture_readiness.py" exec "$LISP" --script "$SERVER_SCRIPT") >"$log" 2>&1 &
fi
server_pid=$!
container_name="woo-autobahn-$$"
cleanup() {
  docker rm -f "$container_name" >/dev/null 2>&1 || true
  kill "$server_pid" 2>/dev/null || true
  wait "$server_pid" 2>/dev/null || true
}
trap cleanup EXIT INT TERM

python3 "$ROOT/t/conformance/fixture_readiness.py" wait "$PORT" "$server_pid" "$READY_NONCE"

if [ "$network" = host ]; then
  network_args="--network host"
else
  network_args="--network $network --add-host host.docker.internal:host-gateway"
fi

set -- docker run --rm --platform linux/amd64 --name "$container_name" $network_args \
  --read-only --memory 512m --pids-limit 128 \
  -v "$config_dir:/config:ro" \
  -v "$ARTIFACT_DIR:/reports" \
  "$AUTOBAHN_IMAGE" wstest -m fuzzingclient -s /config/fuzzingclient.json
perl -e 'alarm shift; exec @ARGV' "$TIMEOUT" "$@"
python3 "$ROOT/t/conformance/fixture_readiness.py" check "$PORT" "$server_pid" "$READY_NONCE"

# wstest can exit successfully even when a case fails. Require the complete
# selected set and both protocol and close behavior to be OK.
python3 - "$ARTIFACT_DIR/servers/index.json" <<'PY'
import json, pathlib, sys
report = pathlib.Path(sys.argv[1])
if not report.is_file():
    raise SystemExit("Autobahn did not write " + str(report))
cases = json.loads(report.read_text()).get("woo", {})
expected = {"2.1", "2.2", "2.3", "2.4", "2.7", "2.8", "2.9", "2.10", "2.11"}
if set(cases) != expected:
    raise SystemExit("Autobahn case set mismatch: " + repr(sorted(cases)))
failed = {name: (case.get("behavior"), case.get("behaviorClose"))
          for name, case in cases.items()
          if case.get("behavior") != "OK" or case.get("behaviorClose") != "OK"}
if failed:
    raise SystemExit("Autobahn failures: " + repr(failed))
print("Autobahn: 9/9 selected cases have OK behavior and close behavior")
PY
