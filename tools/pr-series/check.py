#!/usr/bin/env python3
"""Run a bounded native gate from the selected worktree, record its identity."""
import argparse, hashlib, json, os, resource, signal, subprocess, time
from pathlib import Path
p=argparse.ArgumentParser(); p.add_argument('worktree'); p.add_argument('--suite',action='append',default=[]); p.add_argument('--no-ssl',action='store_true'); p.add_argument('--load-only',action='store_true'); p.add_argument('--system',default='woo-test'); p.add_argument('--timeout',type=int,default=480); a=p.parse_args()
w=Path(a.worktree).resolve(); head=subprocess.check_output(['git','-C',str(w),'rev-parse','HEAD'],text=True).strip()
assert not subprocess.check_output(['git','-C',str(w),'status','--porcelain'],text=True).strip(), 'Commit branch before recording evidence'
root=Path(os.environ.get('WOO_PR_EVIDENCE_ROOT',str(Path(__file__).resolve().parents[3]/'woo-pr-series-evidence'))); root.mkdir(exist_ok=True)
label=head[:12]+'-'+('no-ssl' if a.no_ssl else 'ssl')+'-'+a.system.replace('/','_')+'-'+('load' if a.load_only else '-'.join(a.suite) or 'all')
script=root/(label+'.lisp'); log=root/(label+'.log'); receipt=root/(label+'.json')
q=lambda x: json.dumps(str(x))
setup=os.environ.get('WOO_QUICKLISP_SETUP',str(Path.home()/'quicklisp/setup.lisp'))
s=f'(load {q(setup)})\n(ql:quickload :cffi :silent t)\n'
foreign_dirs=os.environ.get('WOO_PR_FOREIGN_LIB_DIRS', str(Path.home()/'.local/Homebrew/lib')+os.pathsep+str(Path.home()/'.nix-profile/lib')).split(os.pathsep)
s+='(dolist (d (list '+ ' '.join('#P'+q(str(Path(d))+'/') for d in foreign_dirs if d)+')) (pushnew d cffi:*foreign-library-directories* :test #\'equal))\n'
if a.no_ssl:s+='(pushnew :woo-no-ssl *features*)\n'
s+=f'(push #P{q(str(w)+"/")} asdf:*central-registry*)\n(asdf:load-asd #P{q(w/"woo.asd")})\n'
if a.system.startswith('woo-lack-compat'):
 deps=os.environ.get('WOO_COMPAT_DEPENDENCY_ROOT')
 if deps:
  pins=json.loads((Path(__file__).resolve().parents[2]/'t/compat/dependencies.json').read_text())['sources']
  dirs=[Path(deps)/(pin['name']+'-'+pin['commit']) for pin in pins]
  assert all(d.is_dir() for d in dirs), 'Missing pinned dependency sources'
  s+='(dolist (d (list '+ ' '.join('#P'+q(str(d)+'/') for d in dirs)+')) (push d asdf:*central-registry*))\n'
 s+=f'(asdf:load-asd #P{q(w/"woo-lack-compat.asd")})\n'
else:s+=f'(asdf:load-asd #P{q(w/"woo-test.asd")})\n'
s+=f'(ql:quickload {q(a.system)} :silent t)\n(assert (equal (truename (asdf:system-source-directory (asdf:find-system "woo"))) (truename #P{q(str(w)+"/")})))\n'
if a.load_only: pass
elif a.suite:
 s+='(unless (every #\'identity (list '+ ' '.join(f'(rove:run-suite :{x})' for x in a.suite)+')) (uiop:quit 1))\n'
else:s+=f'(unless (rove:run {q(a.system)}) (uiop:quit 1))\n'
s+='(uiop:quit 0)\n';script.write_text(s)
env=dict(os.environ,XDG_CACHE_HOME='/tmp/woo-pr-series-cache'+('-no-ssl' if a.no_ssl else ''))
cmd=[os.environ.get('WOO_PR_LISP','/tmp/woo-sbcl-runtime/bin/sbcl'),'--dynamic-space-size','4096','--noinform','--non-interactive','--load',str(script)]
def limits():
 resource.setrlimit(resource.RLIMIT_NOFILE,(256,resource.getrlimit(resource.RLIMIT_NOFILE)[1]))
 resource.setrlimit(resource.RLIMIT_FSIZE,(16*1024*1024,16*1024*1024))
tree=subprocess.check_output(['git','-C',str(w),'rev-parse','HEAD^{tree}'],text=True).strip()
start=time.time()
with log.open('w') as out:
 child=subprocess.Popen(cmd,cwd=w,env=env,stdout=out,stderr=subprocess.STDOUT,preexec_fn=limits,start_new_session=True)
 try:rc=child.wait(timeout=a.timeout)
 except subprocess.TimeoutExpired:rc=124
 finally:
  try:os.killpg(child.pid,signal.SIGTERM)
  except ProcessLookupError:pass
  try:child.wait(timeout=5)
  except subprocess.TimeoutExpired:
   os.killpg(child.pid,signal.SIGKILL);child.wait(timeout=5)
  try:os.killpg(child.pid,signal.SIGKILL)
  except ProcessLookupError:pass
current_head=subprocess.check_output(['git','-C',str(w),'rev-parse','HEAD'],text=True).strip()
changed=current_head!=head or bool(subprocess.check_output(['git','-C',str(w),'status','--porcelain'],text=True).strip())
if changed:rc=125
receipt.write_text(json.dumps(dict(head=head,worktree=str(w),system=a.system,suites=a.suite,no_ssl=a.no_ssl,load_only=a.load_only,command=cmd,elapsed_seconds=round(time.time()-start,2),exit_code=rc,result='UNKNOWN' if changed else 'PASS' if rc==0 else 'FAIL',source_tree=tree,current_head=current_head,source_changed=changed,runner_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),log=str(log)),indent=2)+'\n')
print(receipt.read_text());
if rc:print(log.read_text()[-10000:])
raise SystemExit(rc)
