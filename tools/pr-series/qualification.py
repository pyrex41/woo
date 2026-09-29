#!/usr/bin/env python3
"""Publish selected hosted qualification evidence; retain failed gates unchanged."""
import hashlib, json, os
from pathlib import Path
import source
from public import evidence_copy
HERE=Path(__file__).resolve().parents[2]
EVIDENCE=Path(os.environ.get('WOO_PR_EVIDENCE_ROOT', str(source.ROOT.parent/'woo-pr-series-evidence')))
OUT=HERE/'docs/pr-series/qualification'
HEAD=source.git('rev-parse','HEAD',cwd=source.ROOT/'16-showcase')
assert HEAD=='c0e27b65e785842b9d673c6668c17a52a2c81b4d', 'Refresh selected evidence after changing stack heads'
selected=[]
for lane in ('managed', 'legacy'):
 summary=EVIDENCE/f'hosted-{lane}-c0e27b6.json'
 if summary.exists():
  evidence_copy(summary,OUT/summary.name)
 for platform in ('macos-15','ubuntu-24.04'):
  folder=(EVIDENCE/f'hosted-legacy-c0e27b6'/platform if lane=='legacy' else
          EVIDENCE/'hosted-managed-c0e27b6/lack'/f'lack-compatibility-{platform}')
  receipt=folder/'receipt.json'
  if not receipt.exists(): continue
  record=json.loads(receipt.read_text())
  assert record['head']==HEAD and not record.get('source_changed'), 'Evidence belongs to another source'
  for name in ('receipt.json','result.json','stop.result'):
   src=folder/name
   if src.exists():
    assert src.stat().st_size <= 16*1024*1024
    dst=OUT/lane/platform/name
    evidence_copy(src,dst)
    selected.append({'path':str(dst.relative_to(HERE)), 'raw_sha256':hashlib.sha256(src.read_bytes()).hexdigest(),
                     'head':HEAD,'lane':lane,'platform':platform,'status':record['status']})
(OUT/'manifest.json').write_text(json.dumps({'head':HEAD,'artifacts':selected,
 'limits':'Evidence is source-bound qualification only. Failed receipts remain failed. Certificate/key fixtures are excluded.'},indent=2)+'\n')
print(json.dumps({'head':HEAD,'artifacts':len(selected)}))
