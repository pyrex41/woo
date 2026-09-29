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
                output = subprocess.check_output(['lsof', '-a', '-p', pid], text=True,
                                                 stderr=subprocess.DEVNULL, timeout=2)
                fds += max(0, len(output.splitlines()) - 1)
            except (OSError, subprocess.SubprocessError):
                if process_pid_running(pid):
                    raise RuntimeError('cannot collect FD samples for live process ' + pid)
    if rss <= 0 or fds <= 0:
        raise RuntimeError('resource sampler produced no positive evidence')
    return rss, fds

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
                    samples.append({'rss_bytes': rss, 'fd_count': fds})
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

def verify_receipt(path):
    receipt = json.loads(path.read_text())
    current = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
    expected = json.loads((ROOT / 't/compat/dependencies.json').read_text())
    if not (receipt.get('status') == 'PASS' and receipt.get('head') == current
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
            and len(receipt.get('resource_samples', [])) >= 10
            and all(isinstance(sample.get('rss_bytes'), int) and not isinstance(sample.get('rss_bytes'), bool)
                    and sample['rss_bytes'] > 0
                    and isinstance(sample.get('fd_count'), int) and not isinstance(sample.get('fd_count'), bool)
                    and sample['fd_count'] > 0 for sample in receipt['resource_samples'])
            and receipt['resource_samples'][-1].get('fd_count', 0) <= receipt['resource_samples'][0].get('fd_count', 0) + 64
            and receipt['resource_samples'][-1].get('rss_bytes', 0) <= receipt['resource_samples'][0].get('rss_bytes', 0) + 64 * 1024 * 1024
            and max((sample.get('rss_bytes', 0) for sample in receipt['resource_samples']), default=0) <= 1024 * 1024 * 1024
            and max((sample.get('fd_count', 0) for sample in receipt['resource_samples']), default=0) <= 4096):
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
        (args.artifacts/'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    if receipt['status'] not in ('PASS', 'DIAGNOSTIC_PASS'): raise RuntimeError('invalid qualification receipt')

if __name__ == '__main__': main()
