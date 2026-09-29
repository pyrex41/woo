#!/usr/bin/env python3
"""Snapshot only hosted checks for the current clean prepared topic heads."""
import datetime,json,os,subprocess
from pathlib import Path
import source
heads={source.git('rev-parse','HEAD',cwd=source.ROOT/f'{i:02d}-{slug}') for i,(slug,_) in enumerate(source.TOPICS,1)}
runs=json.loads(subprocess.check_output(['gh','run','list','--repo','pyrex41/woo','--limit','100','--json','databaseId,headBranch,headSha,status,conclusion,workflowName,url'],text=True,timeout=30))
current=[r for r in runs if r['headSha'] in heads and r['headBranch'].startswith('codex/upstream-')]
root=Path(os.environ.get('WOO_PR_EVIDENCE_ROOT',str(source.ROOT.parent/'woo-pr-series-evidence')))
root.mkdir(exist_ok=True)
snapshot={'observed_at':datetime.datetime.now(datetime.timezone.utc).isoformat(),'repository':'pyrex41/woo','runs':current}
(root/'hosted-ci.json').write_text(json.dumps(snapshot,indent=2)+'\n')
print(json.dumps({'runs':len(current),'states':{state:sum(r['status']==state for r in current) for state in ('completed','in_progress','queued')}},indent=2))
