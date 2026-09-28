#!/usr/bin/env python3
"""Own fixtures, enforce deadlines, and write a source-bound gate receipt."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import resource
import shutil
import signal
import socket
import subprocess
import tempfile
import time
from bootstrap import bootstrap

ROOT = Path(__file__).resolve().parents[2]

def source_digest():
    paths = [*ROOT.glob('*.asd'), *(p for p in ROOT.joinpath('src').rglob('*') if p.is_file()),
             *ROOT.joinpath('t/compat').glob('*'), *ROOT.joinpath('t').rglob('*.lisp'),
             *ROOT.joinpath('t/hegel').glob('*.go'), ROOT/'t/hegel/go.mod', ROOT/'t/hegel/go.sum',
             ROOT/'t/generate-certificates.sh', ROOT/'flake.nix', ROOT/'flake.lock',
             ROOT/'.github/workflows/ci.yml', ROOT/'.github/workflows/lack-compatibility.yml']
    h = hashlib.sha256()
    for path in sorted({p for p in paths if p.is_file()}):
        h.update(str(path.relative_to(ROOT)).encode() + b'\0' + path.read_bytes() + b'\0')
    return h.hexdigest()

REQUIRED_GATES = {'receipt_validator', 'lisp_contract_lifecycle_middleware_services',
                  'http1_https_h2c_h2tls_websocket_soak'}

def verify_receipt(path):
    receipt = json.loads(path.read_text())
    expected_dependencies = json.loads(Path(__file__).with_name('dependencies.json').read_text())
    head = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
    if not (receipt.get('status') == 'PASS' and receipt.get('source_digest') == source_digest()
            and receipt.get('head') == head and receipt.get('soak_seconds') == 1800
            and receipt.get('soak_elapsed_seconds', 0) >= 1800
            and receipt.get('elapsed_seconds', 0) >= 1800 and receipt.get('cleanup') == 'PASS'
            and not receipt.get('source_changed') and receipt.get('dependencies') == expected_dependencies
            and all(receipt.get('gates', {}).get(gate) == 'PASS' for gate in REQUIRED_GATES)):
        raise RuntimeError('Receipt does not qualify the current source and required gates')
    print('PASS: current source, dependencies, full soak, required gates and cleanup')

def group_alive(pgid):
    output = subprocess.check_output(['ps', '-A', '-o', 'pgid=,stat='], text=True, timeout=2)
    return any(int(fields[0]) == pgid and not fields[1].startswith('Z')
               for line in output.splitlines() if len(fields := line.split()) == 2)

def stop(process):
    # Descendants share this owned group, including managed Lisp fixtures.
    # Kill leftovers even if the stage leader already exited on a failure.
    try:
        os.killpg(process.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass
    try:
        process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        process.wait(timeout=5)
    deadline = time.monotonic() + 5
    while group_alive(process.pid):
        if time.monotonic() >= deadline:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            final_deadline = time.monotonic() + 2
            while group_alive(process.pid):
                if time.monotonic() >= final_deadline:
                    raise RuntimeError('Owned fixture process group did not stop')
                time.sleep(0.05)
            return
        time.sleep(0.05)

def run(command, env, log, timeout):
    with log.open('wb') as out:
        process = subprocess.Popen(command, cwd=ROOT, env=env, stdout=out, stderr=subprocess.STDOUT,
                                   start_new_session=True)
        try:
            deadline = time.monotonic() + timeout
            while process.poll() is None:
                if time.monotonic() >= deadline:
                    raise RuntimeError('Gate deadline exceeded: ' + log.name)
                if log.stat().st_size > 8 * 1024 * 1024:
                    raise RuntimeError('Log exceeds artifact budget: ' + log.name)
                time.sleep(0.2)
            if process.returncode:
                raise RuntimeError('Gate failed: ' + log.name)
        finally:
            stop(process)
    if log.stat().st_size > 8 * 1024 * 1024:
        raise RuntimeError('Log exceeds artifact budget: ' + log.name)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--lisp', default=os.environ.get('WOO_HEGEL_LISP', 'sbcl'))
    parser.add_argument('--dependencies', type=Path)
    parser.add_argument('--verify-receipt', type=Path)
    parser.add_argument('--artifacts', type=Path, default=ROOT/'.artifacts/lack')
    parser.add_argument('--soak-seconds', type=int, default=1800)
    args = parser.parse_args()
    if args.verify_receipt:
        verify_receipt(args.verify_receipt)
        return
    if not args.dependencies:
        parser.error('--dependencies is required when running gates')
    if not 1 <= args.soak_seconds <= 1800:
        parser.error('soak duration must be 1..1800 seconds')
    args.artifacts.mkdir(parents=True, exist_ok=True)
    receipt = {'status':'UNKNOWN', 'source_digest':source_digest(), 'platform':platform.platform(),
               'head':subprocess.check_output(['git','rev-parse','HEAD'], cwd=ROOT, text=True).strip(),
               'soak_seconds':args.soak_seconds, 'gates':{}, 'dependencies':bootstrap(args.dependencies),
               'lisp':subprocess.check_output([args.lisp,'--version'],text=True).strip(),
               'go':subprocess.check_output(['go','version'],text=True).strip(),
               'rss_unit':'bytes' if platform.system()=='Darwin' else 'KiB'}
    env = os.environ.copy()
    env.update(WOO_COMPAT_DEPENDENCY_ROOT=str(args.dependencies.resolve()),
               WOO_COMPAT_SOAK_SECONDS=str(args.soak_seconds), WOO_COMPAT_FIXTURE_GROUP='owned-stage',
               WOO_HEGEL_LISP=shutil.which(args.lisp) or args.lisp)
    resource.setrlimit(resource.RLIMIT_NOFILE, (256, 256))
    started = time.monotonic()
    with tempfile.TemporaryDirectory(prefix='woo-lack-fixtures-') as work:
        with socket.socket() as reservation:
            reservation.bind(('127.0.0.1',0)); port=reservation.getsockname()[1]
        redis_log = args.artifacts/'redis.log'
        with redis_log.open('wb') as out:
            redis = subprocess.Popen(['redis-server','--bind','127.0.0.1','--port',str(port),
                                      '--save','','--appendonly','no','--dir',work,'--maxmemory','64mb'],
                                     stdout=out,stderr=subprocess.STDOUT,start_new_session=True)
            try:
                deadline=time.monotonic()+10
                while True:
                    if redis.poll() is not None:
                        raise RuntimeError('Private Redis exited during startup')
                    try:
                        with socket.create_connection(('127.0.0.1',port),timeout=0.2) as client:
                            client.sendall(b'*2\r\n$4\r\nINFO\r\n$6\r\nserver\r\n')
                            info=client.recv(8192)
                            if ('process_id:'+str(redis.pid)+'\r\n').encode() not in info:
                                raise RuntimeError('Redis fixture identity mismatch')
                            break
                    except OSError:
                        if time.monotonic()>=deadline:raise
                        time.sleep(0.05)
                env['WOO_COMPAT_REDIS_PORT']=str(port)
                run(['python3','t/compat/test_receipts.py'],env,args.artifacts/'receipts.log',30)
                receipt['gates']['receipt_validator']='PASS'
                run(['sh','t/generate-certificates.sh'],env,args.artifacts/'certificates.log',30)
                run([env['WOO_HEGEL_LISP'],'--script','t/compat/run.lisp'],env,args.artifacts/'lisp.log',600)
                receipt['gates']['lisp_contract_lifecycle_middleware_services']='PASS'
                run(['go','-C','t/hegel','test','-v','-count=1','-run','^TestManaged',
                     '-skip','^TestManagedSoak$','-timeout','5m'],env,args.artifacts/'transports.log',300)
                soak_started=time.monotonic()
                run(['go','-C','t/hegel','test','-v','-count=1','-run','^TestManagedSoak$','-timeout','35m'],
                    env,args.artifacts/'soak.log',args.soak_seconds+300)
                receipt['soak_elapsed_seconds']=time.monotonic()-soak_started
                receipt['gates']['http1_https_h2c_h2tls_websocket_soak']='PASS'
                receipt['status']='PASS' if args.soak_seconds==1800 else 'DIAGNOSTIC_PASS'
            except Exception as error:
                receipt['status']='FAIL'
                receipt['failure']=str(error)
                raise
            finally:
                stop(redis)
                receipt['elapsed_seconds']=time.monotonic()-started
                receipt['peak_child_rss']=resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss
                receipt['cleanup']='PASS' if redis.poll() is not None else 'UNKNOWN'
                if receipt['source_digest']!=source_digest():
                    receipt['source_changed']=True
                    if receipt['status']!='FAIL':receipt['status']='UNKNOWN'
                (args.artifacts/'receipt.json').write_text(json.dumps(receipt,indent=2)+'\n')
    if receipt['status'] not in ('PASS','DIAGNOSTIC_PASS'):
        raise RuntimeError('Receipt is not valid for this source snapshot')

if __name__=='__main__':
    main()
