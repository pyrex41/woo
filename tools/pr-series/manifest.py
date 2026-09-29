#!/usr/bin/env python3
"""Inventory the prepared stack and build per-PR descriptions from live refs."""
import hashlib,json,subprocess
from pathlib import Path
import source as extract
from public import redact,evidence_copy
HERE=Path(__file__).resolve().parents[2]
OUT=HERE/'docs/pr-series';OUT.mkdir(exist_ok=True)
import os
EVIDENCE=Path(os.environ.get('WOO_PR_EVIDENCE_ROOT', str(extract.ROOT.parent/'woo-pr-series-evidence')))
for old_receipt in (OUT/'receipts').glob('*'):
 if old_receipt.is_file(): old_receipt.unlink()
SOURCE_SHA='4d005a686a4a5287acd77050e927cc42a6f07665'
SUMMARIES=[
'Destroy libev loops on normal and failed cleanup, including their kernel descriptors. Add a descriptor regression and fail CI and ASDF test-op when Rove fails. Freeze the Quicklisp distribution and provide bounded native fixtures and certificate-chain fixtures.',
'Complete HTTP final-status formatting, remove the shared SBCL stat destination and isolate each worker random state. These are three small existing-defect fixes with separate suites.',
'Keep pending TLS write bytes immutable, handle partial writes and WANT_READ/WANT_WRITE readiness, pump static streams in bounded chunks, and complete TLS shutdown with a deadline. Generic socket ownership and budget hooks default to inactive.',
'Create and release listener-owned SSL contexts, load full PEM chains, validate keys, and keep ALPN callback storage per context. HTTP/2 dispatch follows in PR 11.',
'Validate response shape, status, header names/values and framing before writing bytes; normalize supported bodies and handle HEAD/bodyless responses. Before-commit errors produce a bounded 500 and later errors close the connection.',
'Classify pathname failures before writing headers and retain the opened descriptor or TLS stream until transfer completion. Cover missing/unreadable/directory paths, declared lengths and graceful TLS error closure.',
'Return 413 for declared or accumulated oversized bodies, preserve fragmented/pipelined requests, and release saved request-body streams/files on response completion and disconnect, including keepalive.',
'Route stop/drain requests to the owning loop, remove stop controls during cleanup, close worker sockets, and add owning-loop dispatch support for later protocol/managed work.',
'Add bounded HPACK encoding/decoding, frame codecs and constants with pure regression coverage. This does not enable a listening HTTP/2 server.',
'Add stream transitions, connection flow control, settings/continuation validation and bounded request buffers. Server protocol detection and Clack mapping follow separately.',
'Enable h2c prior-knowledge detection and TLS ALPN HTTP/2, map Clack environments/responses and exercise real wire/TLS requests. Keep the legacy HTTP/1 parser and body cleanup intact until the following parser/upgrade change.',
'Add WebSocket framing, upgrade buffering, masking/fragmentation/control validation, close handling and size limits. Export helpers with their event-loop ownership requirement.',
'Add an optional managed adapter with application workers, queue/body/output budgets, cancellation, completion accounting and graceful drain. It depends on completed HTTP/1, HTTP/2 and WebSocket ownership and transport foundations.',
'Add explicit managed Lack wrappers for session, mount, compression, logging and backtrace handling, plus middleware and private Redis/SQLite service contracts. Native Woo remains usable without loading this system.',
'Add native properties/shrinking, Go/Rust parity, codec fuzz/mutation, bounded conformance launchers and exact-source managed/legacy qualification runners. A short diagnostic is not qualification; complete RFC coverage and deployment remain UNKNOWN.',
'Add optional showcase/benchmark systems and upstream usage documentation. Examples and benchmark claims remain distinct from protocol conformance and production evidence.'
]
RELATED={2:[96,127,129,130],3:[120,131],4:[128],5:[93],6:[133],7:[135],8:[81]}
ci_path=EVIDENCE/'hosted-ci.json'
ci_snapshot=json.loads(ci_path.read_text()) if ci_path.exists() else {}
ci_runs=ci_snapshot if isinstance(ci_snapshot,list) else ci_snapshot.get('runs',[])
if ci_path.exists(): (OUT/'hosted-ci.json').write_bytes(ci_path.read_bytes())
records=[]
for n,(slug,title) in enumerate(extract.TOPICS,1):
 w=extract.ROOT/f'{n:02d}-{slug}';head=extract.git('rev-parse','HEAD',cwd=w);parent=extract.git('rev-parse','HEAD^',cwd=w);tree=extract.git('rev-parse','HEAD^{tree}',cwd=w)
 assert not extract.git('status','--porcelain',cwd=w),f'dirty {w}'
 assert parent==(extract.BASE if n==1 else records[-1]['head'])
 evidence=[]
 for p in sorted(EVIDENCE.glob(head[:12]+'-*.json')):
  d=json.loads(p.read_text())
  if d['head']==head and d['source_tree']==tree and not d.get('source_changed'):
   target=OUT/'receipts'/p.name;target.parent.mkdir(exist_ok=True)
   log=Path(d['log']);script=log.with_suffix('.lisp')
   for artifact in (log,script):
    if artifact.exists():
     assert artifact.stat().st_size <= 16*1024*1024, f'oversize {artifact}'
     evidence_copy(artifact,target.parent/artifact.name)
   d['raw_artifact_sha256']={artifact.name:hashlib.sha256(artifact.read_bytes()).hexdigest() for artifact in (log,script) if artifact.exists()}
   d['published_log']=str((target.parent/log.name).relative_to(HERE))
   if script.exists(): d['published_script']=str((target.parent/script.name).relative_to(HERE))
   d['public_copy']={'redactions':['home directory paths'],'raw_sha256':hashlib.sha256(p.read_bytes()).hexdigest()}
   target.write_text(redact(json.dumps(d,indent=2))+'\n')
   evidence.append({'path':str(target.relative_to(HERE)),'result':d['result'],'scope':'load-only' if d.get('load_only') or Path(d['log']).stem.endswith('-load') else d['suites'] or d['system'], 'no_ssl':d.get('no_ssl',False), 'system':d['system']})
 paths=extract.git('diff','--name-only',parent,head,cwd=w).splitlines()
 stats=extract.git('diff','--shortstat',parent,head,cwd=w)
 origins=sorted(set(line for path in paths for line in extract.git('log','--format=%H %an <%ae> %s',f'{extract.BASE}..{SOURCE_SHA}','--',path).splitlines()))
 r=dict(source_commit_records=origins,compare_url=f'https://github.com/pyrex41/woo/compare/{parent}...{head}',number=n,branch=f'codex/upstream-{n:02d}-{slug}',path=str(w),head=head,parent=parent,tree=tree,title=title,stats=stats,paths=paths,evidence=evidence,related_upstream_prs=RELATED.get(n,[]),hosted_checks=[run for run in ci_runs if run['headSha']==head])
 records.append(r)
 body=f'# {title}\n\n{SUMMARIES[n-1]}\n\n## Stack\n\n'
 body+=f'Base: `{parent}`'+(' (upstream master).' if n==1 else f' (PR {n-1:02d}, `{records[-2]["branch"]}`).')+'\n\n'
 body+=f'Head: `{head}`. Topic diff: {stats}. [Review exact topic diff](https://github.com/pyrex41/woo/compare/{parent}...{head}).\n\n'
 body+='This is one clean commit in a dependency-ordered series. Review the topic delta against the named parent. Open upstream PRs in order: after predecessors merge, rebase this topic onto upstream master and rerun the gates before publication. If testing stack drafts on the fork, use the preceding fork branch as the draft base.\n\n## Verification\n\n'
 if evidence:
  for e in evidence:body+=f'- {e["result"]}: {e["scope"]} ({"no SSL" if e["no_ssl"] else "SSL enabled"}); [receipt](https://github.com/pyrex41/woo/blob/codex/pr-series-plan/{e["path"]}).\n'
 else:body+='Exact-head verification pending; do not use another branch\'s green run.\n'
 for run in r['hosted_checks']:
  state='PASS' if run['conclusion']=='success' else run['conclusion'].upper() or run['status'].upper()
  body+=f'- Hosted {run["workflowName"]}: [{state}]({run["url"]}), exact head `{run["headSha"]}`.\n'
 body+='\nReceipts specify the actual ASDF source directory, source tree and command. Load checks establish loadability only. Focused tests cover their named suites; production readiness remains UNKNOWN.\n\n'
 if RELATED.get(n):body+='## Related upstream proposals\n\n'+', '.join(f'[#{x}](https://github.com/fukamachi/woo/pull/{x})' for x in RELATED[n])+'. These references identify overlapping work, not a claim about current PR status. Coordinate scope and authorship with those contributors before publication.\n\n'
 body+='## Provenance\n\nExtracted from fork source `'+SOURCE_SHA+'`, using corrected staging sources where needed. Existing Eitaro Fukamachi/MIT notices are preserved. The original fork commits are retained in the provenance inventory; this squash does not assign authorship of upstream proposals.\n'
 (OUT/f'{n:02d}-{slug}.md').write_text(body)
final=records[-1]['head']
changed=extract.git('diff','--name-only',extract.BASE,SOURCE_SHA).splitlines()
actual=extract.git('diff','--name-only',extract.BASE,final).splitlines()
missing=sorted(set(changed)-set(actual));assert not missing,missing
runtime=extract.git('diff','--name-only',SOURCE_SHA,final,'--','src','woo.asd','woo-lack-compat.asd').splitlines()
assert not runtime, f'assembled runtime differs: {runtime}'
provenance=[]
for path in changed:
 origins=extract.git('log','--format=%H %an <%ae> %s',f'{extract.BASE}..{SOURCE_SHA}','--',path).splitlines()
 provenance.append({'path':path,'source_commits':origins,'review_topics':[r['number'] for r in records if path in r['paths']]})
(OUT/'manifest.json').write_text(redact(json.dumps(dict(source=SOURCE_SHA,upstream=extract.BASE,assembled=final,records=records,missing_paths=missing,runtime_differences=runtime,additional_paths=sorted(set(actual)-set(changed)),provenance=provenance,correctness_reference='f1fd591876be816a3f76dc0f1d95b0d5bbab5eee',intentional_nonruntime_differences=extract.git('diff','--name-only',SOURCE_SHA,final).splitlines(),extraction_notes={'01':'Moved existing libev destructor ahead of expanded suites to prevent intermediate Linux EMFILE failures.', '03':'Kept legacy SSL init signature until listener-context topic.', '04':'Only pure ALPN/context tests are registered before H2 wire integration.', '05':'Kept pathname ownership changes in the following static-file topic.', '07':'Saved actual spool paths/streams in focused correctness tests; bounded cleanup polling accounts for client EOF preceding unlink.', '11':'Preserved native HTTP/1 parser and cleanup; added explicit lack-request reader dependency and bounded detection helpers.', '12':'Restored frozen final Woo source and consolidated lifecycle/detection/descriptor helpers in t/woo.', '13':'Deferred middleware-specific wrappers, dependencies and tests to14.', '15':'Restored full native suite while retaining focused correctness suites; neutralized fork-specific historical qualification claims.', '16':'Neutral upstream documentation; optional showcase and benchmarks remain separate from conformance.'},hosted_observed_at=ci_snapshot.get('observed_at') if isinstance(ci_snapshot,dict) else None),indent=2))+'\n')
lines=['# Prepared upstream PR series','',f'Source `{SOURCE_SHA}`. Upstream `{extract.BASE}`. Assembled `{final}`.','', 'Sixteen clean Woo topic commits are prepared as a linear dependency stack. Each branch contains its predecessors; its PR size is the delta against the listed parent. Do not open every branch against upstream master at once. Publish in order after rebasing and retesting each next topic.','', '| Topic | Branch | Topic diff | Evidence |','|---|---|---|---|']
for r in records:lines.append(f'| [{r["number"]:02d}: {r["title"]}]({r["number"]:02d}-{extract.TOPICS[r["number"]-1][0]}.md) | [`{r["branch"]}`](https://github.com/pyrex41/woo/tree/{r["branch"]}) | {r["stats"]} | '+(', '.join(('no-SSL load' if e['scope']=='load-only' and e['no_ssl'] else 'SSL load' if e['scope']=='load-only' else 'focused native' if isinstance(e['scope'],list) else 'managed native' if e['system'].startswith('woo-lack') else 'full native' if e['system']=='woo-test' else e['system'])+': '+e['result'] for e in r['evidence']) or 'PENDING')+ '; '+', '.join(f'[{run["workflowName"]}: {"PASS" if run["conclusion"]=="success" else run["conclusion"].upper() or run["status"].upper()}]({run["url"]})' for run in r['hosted_checks'])+' |')
lines+=['','## Publication','', 'Fetch the fork topics and select the next unmerged topic. After its predecessors merge, create a fresh branch from updated upstream master, cherry-pick only that topic commit, inspect the resulting diff and rerun its gates. Use the PR body beside this index. The published heads are pinned in the manifest; rebasing or cherry-picking requires new evidence.', '', '```sh', 'git fetch origin codex/upstream-01-tests', 'git fetch upstream master', 'git switch -c submit/01-tests upstream/master', f'git cherry-pick {records[0]["head"]}', '```', '', 'For fork-only stack drafts, use the previous fork topic as the base. For upstream publication, wait until predecessors land and use current upstream master. No upstream PRs were opened in this preparation task.', '', '## Inventory','',f'All {len(changed)} fork-changed paths are represented. Runtime differences from the frozen source: {runtime or "none"}. Additional files are focused correctness suites retained from corrected staging. The assembled test-suite layout consolidates equivalent lifecycle/detection helpers after WebSocket integration. README/readiness docs intentionally remove fork badges and historical qualification claims. Full provenance and exact SHAs are in [manifest.json](manifest.json).','','## Separate Clack PR','','The optional legacy Clack stop hook is a separate repository change. Its standalone patch, PR description and test evidence are in `clack/`. No upstream PR has been published.','','## Limits','','Local tests, managed/legacy diagnostics, full source-bound qualifications, hosted CI, upstream merge and deployment are separate outcomes. Fresh results are attached only to their exact heads. Public copies redact home directory paths as `<HOME>` and retain raw receipt/artifact SHA256 hashes. Unmodified originals remain in the preparation workspace; public scripts show commands with placeholders, while the helper tools generate runnable paths locally. Production readiness and complete RFC coverage remain UNKNOWN.']
(OUT/'README.md').write_text('\n'.join(lines)+'\n')
print(json.dumps({'assembled':final,'branches':len(records),'paths':len(changed),'runtime_differences':runtime},indent=2))
