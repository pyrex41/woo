#!/usr/bin/env python3
"""Run the bounded legacy qualification lane and write a source-bound receipt."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import resource
import shutil
import signal
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
MAX_LOG = 8 * 1024 * 1024
PATCH = ROOT / 'integration/clack/threaded-stop.patch'

def source_digest():
    paths = [*ROOT.glob('*.asd'), *(p for p in ROOT.joinpath('src').rglob('*') if p.is_file()),
             *ROOT.joinpath('t/hegel').glob('*.go'), *ROOT.joinpath('t/hegel').glob('*.lisp'),
             ROOT/'t/hegel/go.mod', ROOT/'t/hegel/go.sum',
             ROOT/'t/generate-certificates.sh', ROOT/'flake.nix', ROOT/'flake.lock',
             *ROOT.joinpath('t/qualification').glob('*'),
             ROOT/'integration/clack/threaded-stop.patch', ROOT/'t/compat/dependencies.json',
             ROOT/'.github/workflows/legacy-qualification.yml']
    digest = hashlib.sha256()
    for path in sorted({p for p in paths if p.is_file()}):
        digest.update(str(path.relative_to(ROOT)).encode() + b'\0' + path.read_bytes() + b'\0')
    return digest.hexdigest()

def process_group_alive(pgid):
    rows = subprocess.check_output(['ps', '-A', '-o', 'pgid=,stat='], text=True, timeout=2)
    return any(int(fields[0]) == pgid and not fields[1].startswith('Z')
               for line in rows.splitlines() if len(fields := line.split()) == 2)

def process_group_rss_bytes(pgid):
    rows = subprocess.check_output(['ps', '-A', '-o', 'pgid=,rss='], text=True, timeout=2)
    values = [int(fields[1]) for line in rows.splitlines()
              if len(fields := line.split()) == 2 and int(fields[0]) == pgid]
    return max(values, default=0) * 1024

def process_group_stats(pgid):
    rows = subprocess.check_output(['ps', '-A', '-o', 'pgid=,pid=,rss='], text=True, timeout=2)
    pids = [fields[1] for line in rows.splitlines()
            if len(fields := line.split()) == 3 and int(fields[0]) == pgid]
    rss = sum((int(line.split()[2]) for line in rows.splitlines()
               if len(line.split()) == 3 and int(line.split()[0]) == pgid), 0) * 1024
    fds = 0
    for pid in pids:
        proc_fds = Path('/proc') / pid / 'fd'
        if proc_fds.is_dir():
            try:
                fds += sum(1 for _ in proc_fds.iterdir())
            except OSError:
                if process_pid_running(pid):
                    raise RuntimeError('cannot collect FD samples for live process ' + pid)
        else:
            try:
                fds += process_pid_fd_count(pid)
            except (OSError, subprocess.SubprocessError):
                if process_pid_running(pid):
                    raise RuntimeError('cannot collect FD samples for live process ' + pid)
    if rss <= 0 or fds <= 0:
        raise RuntimeError('resource sampler produced no positive evidence')
    return rss, fds

def process_pid_fd_count(pid):
    # Numeric descriptors only: exclude mapped files, cwd and executable
    # records. Disable DNS/service lookups and bound transient retries.
    command = ['lsof', '-n', '-P', '-a', '-p', pid, '-d', '0-99999', '-Ff']
    for attempt in range(3):
        try:
            output = subprocess.check_output(command, text=True,
                                             stderr=subprocess.DEVNULL, timeout=5)
            return sum(1 for line in output.splitlines()
                       if line.startswith('f') and line[1:].isdigit())
        except subprocess.TimeoutExpired:
            if attempt == 2:
                raise

def process_pid_running(pid):
    try:
        output = subprocess.check_output(['ps', '-p', pid, '-o', 'stat='], text=True,
                                         stderr=subprocess.DEVNULL, timeout=1).strip()
    except subprocess.CalledProcessError as error:
        if error.returncode == 1 and not error.output.strip():
            return False
        raise RuntimeError('cannot confirm resource sampler PID exit: ' + pid) from error
    except (OSError, subprocess.TimeoutExpired) as error:
        raise RuntimeError('cannot confirm resource sampler PID exit: ' + pid) from error
    return bool(output) and not output.startswith('Z')

def stop(process):
    try: os.killpg(process.pid, signal.SIGTERM)
    except ProcessLookupError: pass
    try: process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        try: os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError: pass
        process.wait(timeout=5)
    deadline = time.monotonic() + 5
    while process_group_alive(process.pid):
        if time.monotonic() >= deadline:
            try: os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError: pass
            raise RuntimeError('legacy qualification process group did not stop')
        time.sleep(.05)

def run(command, env, log, timeout, sample_resources=True):
    with log.open('wb') as output:
        process = subprocess.Popen(command, cwd=ROOT, env=env, stdout=output,
                                   stderr=subprocess.STDOUT, start_new_session=True)
        peak_rss = 0
        samples = []
        last_sample = 0.0
        started = time.monotonic()
        phase = 'bootstrap'
        log_offset = 0
        marker_buffer = b''
        saw_start = False
        saw_end = False
        try:
            deadline = time.monotonic() + timeout
            while process.poll() is None:
                if time.monotonic() >= deadline: raise RuntimeError('qualification deadline exceeded')
                if log.stat().st_size > MAX_LOG: raise RuntimeError('qualification log exceeds 8 MiB')
                if sample_resources and time.monotonic() - last_sample >= 1.0:
                    try:
                        rss, fds = process_group_stats(process.pid)
                    except RuntimeError:
                        if process.poll() is not None:
                            break
                        raise
                    with log.open('rb') as stream:
                        stream.seek(log_offset)
                        marker = stream.read()
                        log_offset = stream.tell()
                    marker_buffer += marker
                    start_at = marker_buffer.find(b'LEGACY_PHASE soak-start')
                    end_at = marker_buffer.find(b'LEGACY_PHASE soak-end')
                    if not saw_start and start_at >= 0:
                        if phase != 'bootstrap':
                            raise RuntimeError('legacy phase markers out of order')
                        saw_start = True
                        phase = 'soak'
                    if not saw_end and end_at >= 0:
                        if not saw_start or (start_at >= 0 and end_at < start_at):
                            raise RuntimeError('legacy phase skipped soak-start marker')
                        saw_end = True
                        phase = 'post-soak'
                    marker_buffer = marker_buffer[-64:]
                    samples.append({'phase': phase, 'elapsed_seconds': time.monotonic() - started,
                                    'rss_bytes': rss, 'fd_count': fds})
                    peak_rss = max(peak_rss, rss)
                    last_sample = time.monotonic()
                time.sleep(.2)
        finally:
            stop(process)
    if process.returncode: raise RuntimeError(f'qualification failed: {process.returncode}')
    return peak_rss, samples

def apply_private_clack_patch(dependencies):
    clack = next((candidate for candidate in dependencies.glob('clack-*') if candidate.is_dir()), None)
    if clack is None: raise RuntimeError('private pinned Clack source is missing')
    for check in (True, False):
        command = ['patch', '--batch', '--forward', '-p1']
        if check: command.append('--dry-run')
        with PATCH.open('rb') as source:
            result = subprocess.run(command, cwd=clack, stdin=source,
                                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=20)
        if result.returncode:
            raise RuntimeError('private Clack integration patch did not apply:\n' + result.stdout.decode(errors='replace'))

def resource_phase_ok(receipt):
    phases = receipt.get('resource_phases')
    if not isinstance(phases, dict): return False
    phase = phases.get('soak')
    if not isinstance(phase, dict): return False
    samples = receipt.get('resource_samples')
    if not isinstance(samples, list): return False
    soak_samples = [sample for sample in samples
                    if isinstance(sample, dict) and sample.get('phase') == 'soak']
    baseline, final = phase.get('baseline'), phase.get('final')
    if not isinstance(baseline, dict) or not isinstance(final, dict): return False
    if phase.get('sample_count') != len(soak_samples) or len(soak_samples) < 2: return False
    if baseline != soak_samples[0] or final != soak_samples[-1]: return False
    duration = final.get('elapsed_seconds', 0) - baseline.get('elapsed_seconds', 0)
    if phase.get('duration_seconds') != duration: return False
    return (phase.get('sample_count', 0) >= 2
            and isinstance(baseline.get('rss_bytes'), int) and baseline['rss_bytes'] > 0
            and isinstance(final.get('rss_bytes'), int) and final['rss_bytes'] > 0
            and isinstance(baseline.get('fd_count'), int) and baseline['fd_count'] > 0
            and isinstance(final.get('fd_count'), int) and final['fd_count'] > 0
            and final['fd_count'] <= baseline['fd_count'] + 64
            and final['rss_bytes'] <= baseline['rss_bytes'] + 64 * 1024 * 1024)

def receipt_qualifies(receipt, current, expected):
    samples = receipt.get('resource_samples')
    if not isinstance(samples, list) or not all(isinstance(sample, dict) for sample in samples):
        return False
    times = [sample.get('elapsed_seconds') for sample in samples]
    if not all(isinstance(value, (int, float)) and not isinstance(value, bool)
               and 0 <= value <= receipt.get('elapsed_seconds', 0) for value in times):
        return False
    if times != sorted(times):
        return False
    phases = [sample.get('phase') for sample in samples]
    phase_rank = {'bootstrap': 0, 'soak': 1, 'post-soak': 2}
    return (receipt.get('status') == 'PASS' and receipt.get('head') == current
            and receipt.get('source_digest') == source_digest()
            and not receipt.get('source_changed')
            and receipt.get('soak_seconds') == 1800
            and receipt.get('elapsed_seconds', 0) >= 1800
            and receipt.get('cleanup') == 'PASS'
            and receipt.get('cycles', 0) >= 1
            and receipt.get('lifecycle_cycles', 0) >= 30
            and receipt.get('soak_elapsed_seconds', 0) >= 1800
            and receipt.get('dependencies') == expected
            and receipt.get('clack_patch_sha256') == hashlib.sha256(PATCH.read_bytes()).hexdigest()
            and receipt.get('peak_rss_bytes', 0) > 0
            and receipt.get('gates', {}).get('legacy_http_https_static_upload_disconnect') == 'PASS'
            and receipt.get('process_group_cleanup') == 'PASS'
            and receipt.get('socket_cleanup') == 'PASS'
            and receipt.get('spool_cleanup') == 'PASS'
            and isinstance(samples, list) and len(samples) >= 10
            and all(phase in phase_rank for phase in phases)
            and phases == sorted(phases, key=phase_rank.get)
            and phases[:1] == ['bootstrap']
            and phases[-1:] == ['post-soak']
            and phases.count('soak') >= 10
            and resource_phase_ok(receipt)
            and receipt['resource_phases']['soak']['duration_seconds'] >= 1790
            and all(isinstance(sample, dict)
                    and isinstance(sample.get('rss_bytes'), int) and not isinstance(sample.get('rss_bytes'), bool)
                    and sample['rss_bytes'] > 0
                    and isinstance(sample.get('fd_count'), int) and not isinstance(sample.get('fd_count'), bool)
                    and sample['fd_count'] > 0 for sample in samples)
            and max((sample.get('rss_bytes', 0) for sample in samples), default=0) <= 1024 * 1024 * 1024
            and max((sample.get('fd_count', 0) for sample in samples), default=0) <= 4096)

def verify_receipt(path):
    receipt = json.loads(path.read_text())
    current = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
    expected = json.loads((ROOT / 't/compat/dependencies.json').read_text())
    if not receipt_qualifies(receipt, current, expected):
        raise RuntimeError('receipt does not qualify the current exact source and full legacy gate')
    print('PASS: current source, pinned dependencies, full legacy soak and cleanup')

def write_setup_failure(path, data, dependencies, error):
    data['failure'] = 'qualification setup failed: ' + str(error)
    try:
        shutil.rmtree(dependencies, ignore_errors=False)
        data['dependency_cleanup'] = 'PASS'
    except Exception as cleanup_error:
        data['failure'] += '; dependency cleanup failed: ' + str(cleanup_error)
        data['dependency_cleanup'] = 'UNKNOWN'
    path.write_text(json.dumps(data, indent=2) + '\n')

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--lisp', default=os.environ.get('WOO_HEGEL_LISP', 'sbcl'))
    parser.add_argument('--dependencies', type=Path, required=False)
    parser.add_argument('--artifacts', type=Path, default=ROOT/'.artifacts/legacy')
    parser.add_argument('--soak-seconds', type=int, default=1800)
    parser.add_argument('--verify-receipt', type=Path)
    args = parser.parse_args()
    if args.verify_receipt:
        verify_receipt(args.verify_receipt); return
    if args.dependencies is None: parser.error('--dependencies is required')
    if not 1 <= args.soak_seconds <= 1800: parser.error('--soak-seconds must be 1..1800')
    # Go runs from t/hegel; fixture paths must remain independent of cwd.
    args.artifacts = args.artifacts.resolve()
    args.artifacts.mkdir(parents=True, exist_ok=True)
    dependency_parent = args.dependencies.resolve(); dependency_parent.mkdir(parents=True, exist_ok=True)
    dependencies = Path(tempfile.mkdtemp(prefix='woo-legacy-deps-', dir=dependency_parent))
    setup_receipt = args.artifacts/'receipt.json'
    setup_data = {'status': 'UNKNOWN',
                  'head': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                  'source_digest': source_digest(), 'cleanup': 'UNKNOWN',
                  'failure': 'qualification setup incomplete'}
    # bootstrap.py writes only immutable archives/extractions; this patch is applied
    # to this private lane directory and never to a shared dependency cache.
    sys.path.insert(0, str(ROOT / 't/compat'))
    from bootstrap import bootstrap
    try:
        pins = bootstrap(dependencies)
        apply_private_clack_patch(dependencies)
    except Exception as error:
        write_setup_failure(setup_receipt, setup_data, dependencies, error)
        raise
    cert_root = args.artifacts/'certs'; cert_root.mkdir(parents=True, exist_ok=True)
    try:
        run(['sh', 't/generate-certificates.sh', str(cert_root)], os.environ.copy(), args.artifacts/'certificates.log', 30,
            sample_resources=False)
    except Exception as error:
        write_setup_failure(setup_receipt, setup_data, dependencies, error)
        raise
    fixture = args.artifacts/'fixture.txt'; fixture.write_bytes(b'woo legacy qualification fixture\n')
    result = args.artifacts/'result.json'
    if result.exists(): result.unlink()
    stop_result = args.artifacts/'stop.result'
    if stop_result.exists(): stop_result.unlink()
    spool_root = args.artifacts/'smart-buffer-spool'
    spool_root.mkdir(parents=True, exist_ok=True)
    env = os.environ.copy()
    env.update(WOO_COMPAT_DEPENDENCY_ROOT=str(dependencies), WOO_COMPAT_CERT_ROOT=str(cert_root),
               WOO_LEGACY_STATIC_FILE=str(fixture), WOO_LEGACY_SOAK_SECONDS=str(args.soak_seconds),
               WOO_LEGACY_RESULT=str(result), WOO_LEGACY_STOP_RESULT=str(stop_result),
               WOO_LEGACY_SPOOL_ROOT=str(spool_root),
               WOO_RUN_LEGACY_QUALIFICATION='1', WOO_HEGEL_LISP=shutil.which(args.lisp) or args.lisp,
               WOO_COMPAT_FIXTURE_GROUP='owned-stage')
    started = time.monotonic(); receipt = {'status':'UNKNOWN', 'head': subprocess.check_output(['git','rev-parse','HEAD'], cwd=ROOT, text=True).strip(),
        'source_digest': source_digest(), 'platform': platform.platform(), 'soak_seconds': args.soak_seconds,
        'dependencies': pins, 'clack_patch_sha256': hashlib.sha256(PATCH.read_bytes()).hexdigest(), 'gates': {}}
    succeeded = False
    try:
        receipt['peak_rss_bytes'], receipt['resource_samples'] = run(['go', '-C', 't/hegel', 'test', '-v', '-count=1', '-run', '^TestLegacyQualification$', '-timeout', f'{args.soak_seconds + 300}s'],
            env, args.artifacts/'legacy.log', args.soak_seconds + 300)
        soak_samples = [sample for sample in receipt['resource_samples'] if sample.get('phase') == 'soak']
        receipt['resource_phases'] = {'soak': {'sample_count': len(soak_samples),
                                               'baseline': soak_samples[0] if soak_samples else {},
                                               'final': soak_samples[-1] if soak_samples else {},
                                               'duration_seconds': ((soak_samples[-1].get('elapsed_seconds', 0) -
                                                                     soak_samples[0].get('elapsed_seconds', 0))
                                                                    if len(soak_samples) >= 2 else 0)}}
        if args.soak_seconds == 1800 and not resource_phase_ok(receipt):
            raise RuntimeError('full receipt lacks bounded phase-bound soak resource evidence')
        receipt['gates']['legacy_http_https_static_upload_disconnect'] = 'PASS'
        if not result.exists(): raise RuntimeError('qualification result is missing')
        result_data = json.loads(result.read_text())
        for key in ('cycles', 'lifecycle_cycles', 'duration_seconds', 'soak_elapsed_seconds'):
            if key not in result_data: raise RuntimeError('qualification result is incomplete')
            receipt[key] = result_data[key]
        if not stop_result.exists() or stop_result.read_text().strip() != 'PASS':
            raise RuntimeError('legacy fixture stop did not report PASS')
        if list(spool_root.iterdir()):
            raise RuntimeError('temporary spool artifacts remain')
        receipt['process_group_cleanup'] = 'PASS'
        receipt['socket_cleanup'] = 'PASS'
        receipt['spool_cleanup'] = 'PASS'
        receipt['status'] = 'PASS' if args.soak_seconds == 1800 else 'DIAGNOSTIC_PASS'
        succeeded = True
    except Exception as error:
        receipt['status'] = 'FAIL'; receipt['failure'] = str(error); raise
    finally:
        receipt['elapsed_seconds'] = time.monotonic() - started
        receipt['peak_child_rss'] = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss
        receipt['cleanup'] = 'PASS' if succeeded else 'UNKNOWN'
        if receipt['source_digest'] != source_digest(): receipt['source_changed'] = True; receipt['status'] = 'UNKNOWN'
        try:
            shutil.rmtree(dependencies, ignore_errors=False)
        except Exception as cleanup_error:
            receipt['cleanup'] = 'UNKNOWN'
            receipt['status'] = 'UNKNOWN'
            receipt['failure'] = 'private dependency cleanup failed: ' + str(cleanup_error)
        if succeeded and receipt['cleanup'] == 'PASS' and not receipt.get('source_changed'):
            receipt['status'] = 'PASS' if args.soak_seconds == 1800 else 'DIAGNOSTIC_PASS'
            if args.soak_seconds == 1800:
                expected = json.loads((ROOT / 't/compat/dependencies.json').read_text())
                current = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
                if not receipt_qualifies(receipt, current, expected):
                    receipt['status'] = 'UNKNOWN'
                    receipt['failure'] = 'writer rejected receipt: full qualification predicate failed'
        (args.artifacts/'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    if receipt['status'] not in ('PASS', 'DIAGNOSTIC_PASS'): raise RuntimeError('invalid qualification receipt')

if __name__ == '__main__': main()
