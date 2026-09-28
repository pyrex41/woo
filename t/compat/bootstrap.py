#!/usr/bin/env python3
"""Fetch immutable upstream inputs with digest checks and bounded extraction."""
import argparse
import hashlib
import json
from pathlib import Path
import tarfile
import urllib.request

LIMIT = 8 * 1024 * 1024

def bootstrap(root):
    pins = json.loads(Path(__file__).with_name('dependencies.json').read_text())
    root.mkdir(parents=True, exist_ok=True)
    for pin in pins['sources']:
        archive = root / (pin['name'] + '-' + pin['commit'] + '.tar.gz')
        if not archive.exists():
            url = 'https://codeload.github.com/' + pin['repository'] + '/tar.gz/' + pin['commit']
            with urllib.request.urlopen(url, timeout=45) as response:
                data = response.read(LIMIT + 1)
            if len(data) > LIMIT:
                raise RuntimeError('Archive exceeds input budget')
            archive.write_bytes(data)
        if archive.stat().st_size > LIMIT:
            raise RuntimeError('Archive exceeds input budget')
        if hashlib.sha256(archive.read_bytes()).hexdigest() != pin['sha256']:
            raise RuntimeError('Digest mismatch: ' + pin['name'])
        with tarfile.open(archive) as source:
            members = source.getmembers()
            if len(members) > 10000 or sum(m.size for m in members) > 64 * LIMIT:
                raise RuntimeError('Extraction exceeds input budget')
            for member in members:
                target = (root / member.name).resolve()
                if root.resolve() not in target.parents or not (member.isfile() or member.isdir()):
                    raise RuntimeError('Unsafe archive entry')
            source.extractall(root, members=members)
    return pins

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('directory', type=Path)
    args = parser.parse_args()
    bootstrap(args.directory)
