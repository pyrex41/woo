#!/usr/bin/env python3
"""Receipts must reject partial, stale and incomplete evidence."""
import copy
import json
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
            'soak_seconds': 1800, 'elapsed_seconds': 1820, 'cleanup': 'PASS',
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
                    {'head': 'old-head'}, {'soak_seconds': 30}, {'elapsed_seconds': 30},
                    {'cleanup': 'UNKNOWN'}, {'source_changed': True}, {'gates': {}},
                    {'dependencies': {}} ]
        for change in changes:
            with self.subTest(change=change):
                receipt = copy.deepcopy(self.receipt)
                receipt.update(change)
                with self.assertRaises(RuntimeError):
                    self.verify(receipt)

if __name__ == '__main__':
    unittest.main()
