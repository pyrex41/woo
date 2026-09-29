"""Frozen extraction inputs and Git inventory helpers (no source mutations)."""
import os, subprocess
from pathlib import Path
WORKSPACE = Path(__file__).resolve().parents[3]
SOURCE = Path(os.environ.get('WOO_PR_SOURCE', str(WORKSPACE / 'woo')))
ROOT = Path(os.environ.get('WOO_PR_STACK_ROOT', str(WORKSPACE / 'woo-pr-series')))
BASE = '2ef0d221827becc123b46139d3965d848b04b7ef'
TOPICS = [('tests', 'Release event-loop descriptors and make the native test gate fail closed'), ('small-fixes', 'Fix status coverage, stat races and worker random states'), ('tls-io', 'Preserve nonblocking TLS retries and drain completed responses'), ('tls-context', 'Own listener TLS contexts and load certificate chains'), ('http1-responses', 'Validate HTTP/1 responses before committing headers'), ('static-files', 'Handle static file errors and transfer ownership safely'), ('uploads', 'Reject oversized bodies and release request spool files'), ('lifecycle', 'Stop servers on their owning event loops and join workers'), ('h2-codecs', 'Add bounded HPACK and HTTP/2 frame codecs'), ('h2-engine', 'Add HTTP/2 connection and stream state handling'), ('h2-server', 'Map HTTP/2 requests and responses to Clack'), ('websocket', 'Add WebSocket upgrades, framing and close handling'), ('managed-clack', 'Add an optional managed Clack runtime'), ('lack-middleware', 'Add managed Lack middleware and service contracts'), ('qualification', 'Add protocol property, parity and qualification gates'), ('showcase', 'Add optional protocol showcase and benchmark tooling')]

def git(*args, cwd=SOURCE):
    return subprocess.check_output(['git', '-C', str(cwd), *args], text=True).strip()
