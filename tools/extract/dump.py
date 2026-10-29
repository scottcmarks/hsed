"""dump.py: annotated PDF -> the sidecar text on stdout.

    dump.py build/core-2.01.hsed.pdf [--reader pikepdf|pypdf|pymupdf]

Prints every hsed annotation found as `spec | id | payload` lines in canonical order (hsed_annot.py):
for the file annotate.py produced this is exactly the committed `.hsa`.  An annotation whose stored
fields disagree with the format (version, flags, kind) is reported on stderr and makes the exit
status 1; the dump is still printed.
"""
from __future__ import annotations

import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import hsed_annot as H  # noqa: E402


def dump(pdf: str, reader: str = "pikepdf") -> tuple[str, list[str]]:
    """(sidecar text, problems) for the annotations in `pdf`."""
    annots = H.READERS[reader](pdf)
    problems = [f"page {a.page + 1} {a.id}: {p}" for a in annots for p in H.problems(a)]
    return H.format_hsa(a.anchor for a in annots), problems


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("pdf")
    ap.add_argument("--reader", choices=sorted(H.READERS), default="pikepdf")
    args = ap.parse_args(argv)
    text, problems = dump(args.pdf, args.reader)
    sys.stdout.write(text)
    for p in problems:
        print(p, file=sys.stderr)
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
