#!/usr/bin/env python3
import copy, json, tempfile, unittest
from pathlib import Path
from unittest.mock import patch
import check

class ReceiptTests(unittest.TestCase):
    def test_stale_and_short_receipts_fail(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'receipt.json'
            receipt = {'status':'PASS','head':'current','source_digest':'current','soak_seconds':1800,
                       'elapsed_seconds':1801,'cleanup':'PASS','cycles':1,
                       'dependencies':{'quicklisp_dist':'2026-01-01'}}
            for change in ({'head':'old'}, {'source_digest':'old'}, {'elapsed_seconds':20},
                           {'cleanup':'UNKNOWN'}, {'status':'DIAGNOSTIC_PASS'}):
                candidate = copy.deepcopy(receipt); candidate.update(change); path.write_text(json.dumps(candidate))
                with patch.object(check, 'source_digest', return_value='current'), \
                     patch.object(check.subprocess, 'check_output', return_value='current\n'):
                    with self.assertRaises(RuntimeError): check.verify_receipt(path)

if __name__ == '__main__': unittest.main()
