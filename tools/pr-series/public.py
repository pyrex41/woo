"""Create explicit public copies of evidence while preserving raw-file identity."""
import hashlib,json,re
from pathlib import Path
HOME_PATH=re.compile(r'/Users/[^/]+/')
def redact(text):
    return HOME_PATH.sub('<HOME>/',text)
def evidence_copy(source,target):
    raw=source.read_bytes()
    target.parent.mkdir(parents=True,exist_ok=True)
    if source.suffix=='.json':
        record=json.loads(redact(raw.decode('utf-8')))
        record['public_copy']={'redactions':['home directory paths'],'raw_sha256':hashlib.sha256(raw).hexdigest()}
        target.write_text(json.dumps(record,indent=2)+'\n')
    else:
        target.write_text(redact(raw.decode('utf-8',errors='replace')))
