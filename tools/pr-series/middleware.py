#!/usr/bin/env python3
"""Own a bounded private Redis fixture for a selected middleware topic gate."""
import os,signal,socket,subprocess,sys,tempfile,time
from pathlib import Path
worktree=Path(sys.argv[1]).resolve()
with tempfile.TemporaryDirectory(prefix='woo-pr-middleware-') as fixture:
 with socket.socket() as reservation:
  reservation.bind(('127.0.0.1',0));port=reservation.getsockname()[1]
 with open(Path(fixture)/'redis.log','wb') as log:
  redis=subprocess.Popen(['redis-server','--bind','127.0.0.1','--port',str(port),'--save','','--appendonly','no','--dir',fixture,'--maxmemory','64mb'],stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
  try:
   deadline=time.monotonic()+10
   while True:
    if redis.poll() is not None:raise RuntimeError('owned Redis exited')
    try:
     with socket.create_connection(('127.0.0.1',port),timeout=.2) as client:
      client.sendall(b'*2\r\n$4\r\nINFO\r\n$6\r\nserver\r\n');info=client.recv(8192)
      assert ('process_id:'+str(redis.pid)+'\r\n').encode() in info,'Redis identity mismatch'
      break
    except OSError:
     if time.monotonic()>=deadline:raise
     time.sleep(.05)
   env=dict(os.environ,WOO_COMPAT_REDIS_PORT=str(port),WOO_COMPAT_DEPENDENCY_ROOT='/tmp/woo-compat-deps')
   cmd=['python3',str(Path(__file__).with_name('check.py')),str(worktree),'--system','woo-lack-compat/tests']
   rc=subprocess.run(cmd,env=env,timeout=500).returncode
  finally:
   try:os.killpg(redis.pid,signal.SIGTERM)
   except ProcessLookupError:pass
   try:redis.wait(timeout=5)
   except subprocess.TimeoutExpired:os.killpg(redis.pid,signal.SIGKILL);redis.wait(timeout=5)
assert redis.poll() is not None
print('Private Redis fixture cleanup: PASS')
raise SystemExit(rc)
