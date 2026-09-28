#!/usr/bin/env python3
"""Receipts must reject partial, stale and incomplete evidence."""
import copy
import json
import os
import subprocess
import sys
import time
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import check

class Receipts(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.path = Path(self.directory.name) / 'receipt.json'
        self.receipt = {
            'status': 'PASS', 'source_digest': 'current-source', 'head': 'current-head',
            'soak_seconds': 1800, 'soak_elapsed_seconds': 1801, 'elapsed_seconds': 1820, 'cleanup': 'PASS',
            'dependencies': json.loads(Path(__file__).with_name('dependencies.json').read_text()),
            'gates': {gate: 'PASS' for gate in check.REQUIRED_GATES}}

    def verify(self, receipt):
        self.path.write_text(json.dumps(receipt))
        with patch.object(check, 'source_digest', return_value='current-source'), \
             patch.object(check.subprocess, 'check_output', return_value='current-head\n'):
            check.verify_receipt(self.path)

    def test_full_current_receipt(self):
        self.verify(self.receipt)

    def test_incomplete_or_stale_receipts(self):
        changes = [ {'status': 'DIAGNOSTIC_PASS'}, {'source_digest': 'old-source'},
                    {'head': 'old-head'}, {'soak_seconds': 30}, {'elapsed_seconds': 30}, {'soak_elapsed_seconds': 30},
                    {'cleanup': 'UNKNOWN'}, {'source_changed': True}, {'gates': {}},
                    {'dependencies': {}} ]
        for change in changes:
            with self.subTest(change=change):
                receipt = copy.deepcopy(self.receipt)
                receipt.update(change)
                with self.assertRaises(RuntimeError):
                    self.verify(receipt)

class StageCleanup(unittest.TestCase):
    def test_exited_leader_does_not_leave_child(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            pid_file = root / 'child.pid'
            command = [sys.executable, '-c',
                       "import subprocess,sys,os; from pathlib import Path; "
                       "p=subprocess.Popen([sys.executable,'-c','import time; time.sleep(60)']); "
                       "Path(sys.argv[1]).write_text(str(p.pid)); os._exit(1)", str(pid_file)]
            with self.assertRaises(RuntimeError):
                check.run(command, os.environ.copy(), root / 'stage.log', 10)
            pid = int(pid_file.read_text())
            deadline = time.monotonic() + 5
            while True:
                status = subprocess.run(['ps', '-p', str(pid), '-o', 'stat='],
                                        capture_output=True, text=True, timeout=2).stdout.strip()
                if not status or status.startswith('Z'):
                    break
                if time.monotonic() >= deadline:
                    self.fail('stage child remains alive after failed leader exit')
                time.sleep(0.05)

if __name__ == '__main__':
    unittest.main()
