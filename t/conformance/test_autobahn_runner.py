"""Bounded conformance launcher checks with fake tools and HTTP fixtures."""
import json
import os
from pathlib import Path
import signal
import socket
import subprocess
import tempfile
import time
import unittest


ROOT = Path(__file__).resolve().parents[2]
CASES = ["2.1", "2.2", "2.3", "2.4", "2.7", "2.8", "2.9", "2.10", "2.11"]


class ConformanceRunnerTest(unittest.TestCase):
    def run_fixture(self, write_report, runner="autobahn", mode="healthy", occupied=False,
                    timeout=60, tool_delay=0, h2spec_report=None):
        with tempfile.TemporaryDirectory(prefix="woo-autobahn-test-") as directory:
            root = Path(directory)
            binaries = root / "bin"
            binaries.mkdir()
            artifacts = root / "artifacts"
            (artifacts / "servers").mkdir(parents=True)
            cases = {key: {"behavior": "OK", "behaviorClose": "OK"} for key in CASES}
            stale = artifacts / "servers/index.json"
            stale.write_text(json.dumps({"woo": cases}))
            os.utime(stale, (1, 1))

            def executable(name, text):
                path = binaries / name
                path.write_text(text)
                path.chmod(0o755)
                return str(path)

            executable("uname", "#!/bin/sh\necho Linux\n")
            executable("h2spec", '''#!/usr/bin/env python3
import json, os, pathlib, signal, sys
if "--version" not in sys.argv:
    pathlib.Path(os.environ["TEST_DOCKER_ARGS"]).write_text(json.dumps(sys.argv[1:]))
    if os.environ["TEST_TOOL_DELAY"]:
        import time; time.sleep(float(os.environ["TEST_TOOL_DELAY"]))
    if os.environ["TEST_WRITE_H2SPEC_REPORT"] == "1":
        report = pathlib.Path(sys.argv[sys.argv.index("--junit-report") + 1])
        report.write_text(os.environ["TEST_H2SPEC_REPORT"])
    if os.environ["TEST_FIXTURE_MODE"] == "exit-during-tool":
        os.kill(int(pathlib.Path(os.environ["TEST_FIXTURE_PID"]).read_text()), signal.SIGKILL)
        import time; time.sleep(.1)
''')
            executable("docker", '''#!/usr/bin/env python3
import json, os, pathlib, sys
args = sys.argv[1:]
if args[0] == "run":
    pathlib.Path(os.environ["TEST_DOCKER_ARGS"]).write_text(json.dumps(args))
    if os.environ["TEST_TOOL_DELAY"]:
        import time; time.sleep(float(os.environ["TEST_TOOL_DELAY"]))
    if os.environ["TEST_FIXTURE_MODE"] == "exit-during-tool":
        import signal, time
        os.kill(int(pathlib.Path(os.environ["TEST_FIXTURE_PID"]).read_text()), signal.SIGKILL)
        time.sleep(.1)
    if os.environ["TEST_WRITE_REPORT"] == "1":
        target = next(a[:-9] for a in args if a.endswith(":/reports"))
        report = pathlib.Path(target) / "servers/index.json"
        report.parent.mkdir()
        report.write_text(os.environ["TEST_REPORT"])
''')
            lisp = executable("fixture", '''#!/usr/bin/env python3
import http.server, os, pathlib, threading
pathlib.Path(os.environ["TEST_FIXTURE_PID"]).write_text(str(os.getpid()))
if os.environ["TEST_FIXTURE_MODE"] == "exit":
    raise SystemExit(7)
if os.environ["TEST_FIXTURE_MODE"] == "wrong-identity":
    threading.Timer(.5, lambda: os._exit(7)).start()
class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        nonce = os.environ["WOO_HEGEL_READY_NONCE"]
        if os.environ["TEST_FIXTURE_MODE"] == "wrong-identity": nonce = "old-instance"
        body = nonce.encode("ascii")
        self.send_response(200)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)
    def log_message(self, *args): pass
http.server.HTTPServer(("127.0.0.1", int(os.environ["WOO_HEGEL_PORT"])), Handler).serve_forever()
''')
            arguments = root / "docker-args.json"
            env = dict(os.environ, PATH=str(binaries) + ":" + os.environ["PATH"],
                       WOO_HEGEL_LISP=lisp, WOO_AUTOBAHN_ARTIFACT_DIR=str(artifacts),
                       H2SPEC_ARTIFACT_DIR=str(artifacts), H2SPEC_BIN=str(binaries / "h2spec"),
                       TEST_FIXTURE_MODE=mode, TEST_FIXTURE_PID=str(root / "fixture.pid"),
                       TEST_DOCKER_ARGS=str(arguments), TEST_WRITE_REPORT=str(int(write_report)),
                       TEST_WRITE_H2SPEC_REPORT=str(int(runner == "h2spec" and write_report)),
                       TEST_H2SPEC_REPORT=h2spec_report or '<testsuite tests="1" failures="0"/>',
                       TEST_TOOL_DELAY=str(tool_delay),
                       TEST_REPORT=json.dumps({"woo": cases}))
            # These tests must exercise the platform default, independent of the caller.
            for key in ("WOO_AUTOBAHN_NETWORK", "WOO_AUTOBAHN_HOST", "WOO_AUTOBAHN_PORT", "WOO_H2SPEC_PORT"):
                env.pop(key, None)
            with socket.socket() as existing:
                if occupied:
                    existing.bind(("127.0.0.1", 0))
                    existing.listen()
                    env["WOO_" + runner.upper() + "_PORT"] = str(existing.getsockname()[1])
                process = subprocess.Popen(
                    ["sh", str(ROOT / ("t/conformance/" + runner + ".sh"))],
                    env=env, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                    start_new_session=True)
                try:
                    stdout, stderr = process.communicate(timeout=timeout)
                except subprocess.TimeoutExpired:
                    # The shell launchers start the fixture in the background.
                    # Kill the complete process group so a timed out test cannot
                    # orphan that fixture outside TemporaryDirectory cleanup.
                    try:
                        os.killpg(process.pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass
                    stdout, stderr = process.communicate()
                    stderr += "\nlauncher test timed out; process group terminated\n"
                result = subprocess.CompletedProcess(
                    process.args, process.returncode, stdout, stderr)
            self.assertEqual(stale.stat().st_mtime, 1)
            if runner == "autobahn" and arguments.exists():
                args = json.loads(arguments.read_text())
                self.assertEqual(args[args.index("--network") + 1], "host")
            self.assertEqual(len(list(artifacts.glob("run.*"))), 1)
            if occupied:
                self.assertFalse((root / "fixture.pid").exists(), "occupied port launched fixture")
            if occupied or mode in ("exit", "wrong-identity"):
                self.assertFalse(arguments.exists(), "tool ran before fixture identity was established")
            if mode == "tool-hang":
                pid_file = root / "fixture.pid"
                if not pid_file.exists():
                    self.fail("fixture never started: " + result.stderr)
                pid = int(pid_file.read_text())
                deadline = time.monotonic() + 2
                while time.monotonic() < deadline:
                    try:
                        os.kill(pid, 0)
                    except ProcessLookupError:
                        break
                    time.sleep(0.05)
                with self.subTest(fixture_pid=pid):
                    self.assertRaises(ProcessLookupError, os.kill, pid, 0)
            return result

    def test_stale_report_cannot_pass(self):
        result = self.run_fixture(False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Autobahn did not write", result.stderr)

    def test_current_complete_report_passes(self):
        result = self.run_fixture(True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("9/9 selected cases", result.stdout)

    def test_occupied_port_rejected(self):
        for runner in ("autobahn", "h2spec"):
            with self.subTest(runner=runner):
                result = self.run_fixture(True, runner=runner, occupied=True)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("port is unavailable", result.stderr)

    def test_fixture_exit_before_readiness_rejected(self):
        for runner in ("autobahn", "h2spec"):
            with self.subTest(runner=runner):
                result = self.run_fixture(True, runner=runner, mode="exit")
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("fixture exited", result.stderr)

    def test_wrong_fixture_identity_rejected(self):
        for runner in ("autobahn", "h2spec"):
            with self.subTest(runner=runner):
                result = self.run_fixture(True, runner=runner, mode="wrong-identity")
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("fixture exited", result.stderr)

    def test_fixture_exit_during_tool_rejected(self):
        for runner in ("autobahn", "h2spec"):
            with self.subTest(runner=runner):
                result = self.run_fixture(True, runner=runner, mode="exit-during-tool")
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("fixture exited", result.stderr)

    def test_live_h2spec_fixture_passes(self):
        result = self.run_fixture(True, runner="h2spec")
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_h2spec_requires_report(self):
        result = self.run_fixture(False, runner="h2spec")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("without a non-empty JUnit report", result.stderr)

    def test_h2spec_rejects_reported_failure_on_zero_exit(self):
        result = self.run_fixture(True, runner="h2spec",
                                  h2spec_report='<testsuite tests="1" failures="1"/>')
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("JUnit reports failures or errors", result.stderr)

    def test_timeout_terminates_fixture_process_group(self):
        result = self.run_fixture(False, mode="tool-hang", timeout=30, tool_delay=60)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("process group terminated", result.stderr)


if __name__ == "__main__":
    unittest.main()
