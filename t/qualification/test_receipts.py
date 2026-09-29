#!/usr/bin/env python3
import copy, hashlib, json, tempfile, unittest
from pathlib import Path
from unittest.mock import patch
import check

class ReceiptTests(unittest.TestCase):
    def test_pid_exit_requires_positive_observation(self):
        dead = check.subprocess.CalledProcessError(1, ['ps'], output='')
        with patch.object(check.subprocess, 'check_output', side_effect=dead):
            self.assertFalse(check.process_pid_running('99999999'))
        with patch.object(check.subprocess, 'check_output', return_value='S\n'):
            self.assertTrue(check.process_pid_running('123'))
        with patch.object(check.subprocess, 'check_output', side_effect=OSError('unavailable')):
            with self.assertRaises(RuntimeError):
                check.process_pid_running('123')
        with patch.object(check.subprocess, 'check_output',
                          side_effect=check.subprocess.TimeoutExpired(['ps'], 1)):
            with self.assertRaises(RuntimeError):
                check.process_pid_running('123')

    def test_stale_and_short_receipts_fail(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'receipt.json'
            receipt = {'status':'PASS','head':'current','source_digest':'current','soak_seconds':1800,
                       'elapsed_seconds':1801,'cleanup':'PASS','cycles':1,'lifecycle_cycles':30,
                       'soak_elapsed_seconds':1801,
                       'dependencies':json.loads((check.ROOT / 't/compat/dependencies.json').read_text()),
                       'clack_patch_sha256':hashlib.sha256(check.PATCH.read_bytes()).hexdigest(),
                       'peak_rss_bytes':1024,'resource_samples':[{'rss_bytes':1024,'fd_count':3}]*10,
                       'gates':{'legacy_http_https_static_upload_disconnect':'PASS'},
                       'process_group_cleanup':'PASS','socket_cleanup':'PASS','spool_cleanup':'PASS'}
            path.write_text(json.dumps(receipt))
            with patch.object(check, 'source_digest', return_value='current'), \
                 patch.object(check.subprocess, 'check_output', return_value='current\n'):
                check.verify_receipt(path)
            for change in ({'head':'old'}, {'source_digest':'old'}, {'elapsed_seconds':20},
                           {'soak_elapsed_seconds':20}, {'cleanup':'UNKNOWN'}, {'status':'DIAGNOSTIC_PASS'},
                           {'lifecycle_cycles':0}, {'resource_samples':[]}, {'dependencies':{}},
                           {'gates':{}}, {'source_changed':True},
                           {'resource_samples':[{'rss_bytes':0,'fd_count':3}]*10},
                           {'resource_samples':[{'rss_bytes':1024,'fd_count':0}]*10},
                           {'resource_samples':[{'rss_bytes':1024,'fd_count':3},
                                                {'rss_bytes':2 * 1024 * 1024 * 1024,'fd_count':3}]*5}):
                candidate = copy.deepcopy(receipt); candidate.update(change); path.write_text(json.dumps(candidate))
                with patch.object(check, 'source_digest', return_value='current'), \
                     patch.object(check.subprocess, 'check_output', return_value='current\n'):
                    with self.assertRaises(RuntimeError): check.verify_receipt(path)

if __name__ == '__main__': unittest.main()
