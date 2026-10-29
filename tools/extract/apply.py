"""apply.py: pristine PDF + incremental tail (.upd) -> annotated PDF, after checking that the pristine
file is the one the manifest records (md5).

    apply.py --pristine tcg/<pdf> --upd spec/annot/core-2.01.upd
             --manifest spec/annot/core-2.01.manifest.json --out build/core-2.01.hsed.pdf

The output is the byte concatenation, nothing else: `cat pristine upd` with a safety check.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import sys


def apply(pristine: str, upd: str, manifest: str, out: str) -> dict:
    head = open(pristine, "rb").read()
    want = json.load(open(manifest))["md5"]
    got = hashlib.md5(head).hexdigest()
    if got != want:
        sys.exit(f"{pristine} has md5 {got}; {manifest} was made for {want}")
    tail = open(upd, "rb").read()
    os.makedirs(os.path.dirname(os.path.abspath(out)), exist_ok=True)
    open(out, "wb").write(head + tail)
    return {"out": out, "pristine_bytes": len(head), "tail_bytes": len(tail), "md5": got}


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--pristine", required=True)
    ap.add_argument("--upd", required=True)
    ap.add_argument("--manifest", required=True)
    ap.add_argument("--out", required=True)
    args = ap.parse_args(argv)
    print(json.dumps(apply(args.pristine, args.upd, args.manifest, args.out)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
