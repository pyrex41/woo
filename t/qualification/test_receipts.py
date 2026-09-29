#!/usr/bin/env python3
import copy, hashlib, json, tempfile, unittest
from pathlib import Path
from unittest.mock import patch
import check

class ReceiptTests(unittest.TestCase):
    def test_fd_sampler_retries_timeouts_and_counts_only_descriptors(self):
        timeout = check.subprocess.TimeoutExpired(['lsof'], 5)
        with patch.object(check.subprocess, 'check_output',
                          side_effect=[timeout, 'p123\nfcwd\nf0\nf7\nftxt\n']) as sample:
            self.assertEqual(check.process_pid_fd_count('123'), 2)
            self.assertEqual(sample.call_count, 2)
            self.assertIn('-n', sample.call_args.args[0])
            self.assertIn('-Ff', sample.call_args.args[0])
        with patch.object(check.subprocess, 'check_output', side_effect=timeout) as sample:
            with self.assertRaises(check.subprocess.TimeoutExpired):
                check.process_pid_fd_count('123')
            self.assertEqual(sample.call_count, 3)
        with patch.object(check.subprocess, 'check_output', side_effect=OSError('unavailable')) as sample:
            with self.assertRaises(OSError):
                check.process_pid_fd_count('123')
            self.assertEqual(sample.call_count, 1)

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
                       'peak_rss_bytes':1024,
                       'resource_samples':([{'phase':'bootstrap','elapsed_seconds':0,'rss_bytes':1024,'fd_count':3}] +
                                           [{'phase':'soak','elapsed_seconds':elapsed,'rss_bytes':1024,'fd_count':3}
                                            for elapsed in list(range(1, 1800, 150)) + [1791]] +
                                           [{'phase':'post-soak','elapsed_seconds':1801,'rss_bytes':1024,'fd_count':3}]),
                       'resource_phases':{'soak':{'sample_count':13,
                                                   'baseline':{'phase':'soak','elapsed_seconds':1,'rss_bytes':1024,'fd_count':3},
                                                   'final':{'phase':'soak','elapsed_seconds':1791,'rss_bytes':1024,'fd_count':3},
                                                   'duration_seconds':1790}},
                       'gates':{'legacy_http_https_static_upload_disconnect':'PASS'},
                       'process_group_cleanup':'PASS','socket_cleanup':'PASS','spool_cleanup':'PASS'}
            path.write_text(json.dumps(receipt))
            with patch.object(check, 'source_digest', return_value='current'), \
                 patch.object(check.subprocess, 'check_output', return_value='current\n'):
                check.verify_receipt(path)
            for change in ({'head':'old'}, {'source_digest':'old'}, {'elapsed_seconds':20},
                           {'soak_elapsed_seconds':20}, {'cleanup':'UNKNOWN'}, {'status':'DIAGNOSTIC_PASS'},
                           {'forced_gc_diagnostic':True},
                           {'lifecycle_cycles':0}, {'resource_samples':[]}, {'dependencies':{}},
                           {'gates':{}}, {'source_changed':True},
                           {'resource_phases':{}},
                           {'resource_phases':{'soak': []}},
                           {'resource_samples':[None]},
                           {'resource_phases':{'soak':{'sample_count':2,
                                                       'baseline':{'rss_bytes':1024,'fd_count':3},
                                                       'final':{'rss_bytes':100 * 1024 * 1024,'fd_count':3}}}},
                           {'resource_samples':[{'rss_bytes':0,'fd_count':3}]*10},
                           {'resource_samples':[{'rss_bytes':1024,'fd_count':0}]*10},
                           {'resource_samples':[{'rss_bytes':1024,'fd_count':3},
                                                {'rss_bytes':2 * 1024 * 1024 * 1024,'fd_count':3}]*5}):
                candidate = copy.deepcopy(receipt); candidate.update(change); path.write_text(json.dumps(candidate))
                with patch.object(check, 'source_digest', return_value='current'), \
                     patch.object(check.subprocess, 'check_output', return_value='current\n'):
                    with self.assertRaises(RuntimeError): check.verify_receipt(path)
            growth = copy.deepcopy(receipt)
            soak_indexes = [index for index, sample in enumerate(growth['resource_samples'])
                            if sample['phase'] == 'soak']
            growth['resource_samples'][soak_indexes[-1]]['rss_bytes'] += 65 * 1024 * 1024
            growth['resource_phases']['soak']['final'] = growth['resource_samples'][soak_indexes[-1]]
            path.write_text(json.dumps(growth))
            with patch.object(check, 'source_digest', return_value='current'), \
                 patch.object(check.subprocess, 'check_output', return_value='current\n'):
                with self.assertRaises(RuntimeError): check.verify_receipt(path)
            for value in (None, True, float('nan'), float('inf'), -1, 1802):
                candidate = copy.deepcopy(receipt)
                candidate['resource_samples'][2]['elapsed_seconds'] = value
                path.write_text(json.dumps(candidate))
                with patch.object(check, 'source_digest', return_value='current'), \
                     patch.object(check.subprocess, 'check_output', return_value='current\n'):
                    with self.assertRaises(RuntimeError): check.verify_receipt(path)
            candidate = copy.deepcopy(receipt)
            candidate['resource_samples'][2]['elapsed_seconds'] = 0
            path.write_text(json.dumps(candidate))
            with patch.object(check, 'source_digest', return_value='current'), \
                 patch.object(check.subprocess, 'check_output', return_value='current\n'):
                with self.assertRaises(RuntimeError): check.verify_receipt(path)

if __name__ == '__main__': unittest.main()
