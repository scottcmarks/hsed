"""annotate.py: pristine PDF + sidecar (.hsa) + IR -> annotated PDF, incremental tail (.upd) and manifest.

    annotate.py --spec core-2.01 --pristine tcg/<pdf> --ir spec/ir/core-2.01.json
                --hsa spec/annot/core-2.01.hsa --out build/core-2.01.hsed.pdf
                --upd spec/annot/core-2.01.upd --manifest spec/annot/core-2.01.manifest.json

Each sidecar line is resolved to its anchor through the IR (docs/ir-v1.md): a table by id to its
struct path and first-page rect, a section to its heading, `doc` to the top-left inch of page 1,
`<section>#<item>` to the paragraph; an anchor the IR does not know is refused with its line.  The
annotations (hsed_annot.py is the format) are written in canonical order as ONE pypdf incremental
section, so the pristine file is an exact byte prefix of the output and two runs are byte-identical.
The tail is exported as the `.upd` file and the manifest records what the pristine PDF implies
about every anchor (design section 3.2): ordinal, struct path, page, printed label, normalised
header, content hash and fragment rects.  Exit status 2 on a refused sidecar.
"""
from __future__ import annotations

import argparse
import hashlib
import io
import json
import os
import re
import struct
import sys
import zlib
from contextlib import contextmanager
from functools import reduce

import pypdf

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import hsed_annot as H  # noqa: E402
from extract_tables import norm_words, words_hash  # noqa: E402

WS = re.compile(r"\s+")
CAPTION_RE = re.compile(r"^\s*Table\s+\d+\s*")


def md5_bytes(b: bytes) -> str:
    return hashlib.md5(b).hexdigest()


def norm_header(texts) -> list[str]:
    """Header cells as the rules compare them (design section 3.4): whitespace stripped, case folded."""
    return [WS.sub("", t).lower() for t in texts]


def cells_hash(t: dict) -> str:
    """sha256 over the table's cell texts, header rows first, tab between cells, newline between rows."""
    rows = t["header_rows"] + t["rows"]
    return hashlib.sha256("\n".join("\t".join(c["text"] for c in row) for row in rows).encode()).hexdigest()


def label_of(page: int, label_offset: int) -> int | None:
    """Printed page label of a 0-based page index, None before the printed numbering starts."""
    label = page + 1 - label_offset
    return label if label >= 1 else None


# ----------------------------------------------------------------------------- resolution against the IR
def resolve_table(ir: dict, anchor: H.Anchor) -> tuple[H.HsedAnnot, dict]:
    parts = [t for t in ir["tables"] if t["id"] == anchor.id]
    if not parts:
        raise KeyError(f"no table {anchor.id} in the IR")
    t = parts[0]                                               # a split table (Core T7) anchors on its first part
    page, rect = t["rects"][0]
    return H.make_annot(anchor, t["struct_path"], page, rect), {
        "ordinal": t["ordinal"], "struct": t["struct_path"], "page": page,
        "title": CAPTION_RE.sub("", t["caption"] or "", count=1) or None,
        "header": norm_header(c["text"] for c in t["header_rows"][0]) if t["header_rows"] else None,
        "hash": cells_hash(t), "rects": [r for p in parts for r in p["rects"]]}


def resolve_heading(ir: dict, anchor: H.Anchor) -> tuple[H.HsedAnnot, dict]:
    hs = [h for h in ir["headings"] if h["section"] == anchor.id]
    if not hs:
        raise KeyError(f"no heading numbered {anchor.id} in the IR")
    if len(hs) > 1:
        raise KeyError(f"{len(hs)} headings numbered {anchor.id} in the IR")
    h = hs[0]
    return H.make_annot(anchor, h["struct_path"], h["page"], h["rect"]), {
        "ordinal": None, "struct": h["struct_path"], "page": h["page"], "title": h["title"], "header": None,
        "hash": words_hash(norm_words(h["title"])), "rects": [[h["page"], h["rect"]]]}


def resolve_para(ir: dict, anchor: H.Anchor) -> tuple[H.HsedAnnot, dict]:
    section, item = anchor.id.split("#", 1)
    in_section = [p for p in ir["paras"] if p["section"] == section]
    if item.startswith("p") and item[1:].isdigit():            # <section>#p<k>: the k-th /P of the section
        plain = [p for p in in_section if p["kind"] == "P"]
        hits = plain[int(item[1:]) - 1: int(item[1:])] if int(item[1:]) >= 1 else []
    else:
        hits = [p for p in in_section if p["item"] == item]
    if not hits:
        raise KeyError(f"no paragraph {anchor.id} in the IR ({len(in_section)} paragraphs in section {section})")
    if len(hits) > 1:
        raise KeyError(f"{len(hits)} paragraphs labelled {anchor.id} in the IR")
    p = hits[0]
    return H.make_annot(anchor, p["struct_path"], p["page"], p["rect"]), {
        "ordinal": None, "struct": p["struct_path"], "page": p["page"], "title": None, "header": None,
        "hash": p["hash"], "rects": [[p["page"], p["rect"]]]}


def resolve_doc(ir: dict, anchor: H.Anchor) -> tuple[H.HsedAnnot, dict]:
    return H.make_annot(anchor, H.DOC_STRUCT, H.DOC_PAGE, H.DOC_RECT), {
        "ordinal": None, "struct": H.DOC_STRUCT, "page": H.DOC_PAGE, "title": None, "header": None, "hash": None,
        "rects": [[H.DOC_PAGE, list(H.DOC_RECT)]]}


RESOLVERS = {"Doc": resolve_doc, "Rule": resolve_heading, "Bind": resolve_table, "Law": resolve_para,
             "Behaviour": resolve_para}


def resolve(ir: dict, anchor: H.Anchor) -> tuple[H.HsedAnnot, dict]:
    """(annotation, manifest record) for one sidecar line; KeyError when the IR has no such anchor."""
    annot, facts = RESOLVERS[anchor.kind](ir, anchor)
    return annot, {"id": anchor.id, "kind": anchor.kind, **facts, "label": label_of(facts["page"], ir["labelOffset"])}


def resolve_all(ir: dict, anchors: list[H.Anchor], spec: str, hsa_name: str) -> list[tuple[H.HsedAnnot, dict]]:
    """Every line resolved, in canonical order; every failure reported, naming the line, before exit."""
    def attempt(a):
        try:
            return resolve(ir, a) if a.spec == spec else KeyError(f"spec is not {spec}")
        except KeyError as e:
            return e

    outcomes = [(a, attempt(a)) for a in anchors]
    failures = [f"{hsa_name}: {a.line!r}: {e.args[0]}" for a, e in outcomes if isinstance(e, KeyError)]
    if failures:
        sys.exit("refused:\n  " + "\n  ".join(failures))
    return sorted((r for _, r in outcomes), key=lambda r: r[0].sort_key)


# ----------------------------------------------------------------------------- writer
REGULAR = re.compile(rb"[^\x00\t\n\x0c\r ()<>\[\]{}/%]")           # PDF 32000 7.2.2: not white space, not a delimiter


def _serialised(obj) -> bytes:
    b = io.BytesIO()
    obj.write_to_stream(b)
    return b.getvalue()


def _joined(parts: list[bytes]) -> bytes:
    """Tokens with a space only where two regular characters would otherwise touch."""
    return reduce(lambda acc, p: acc + (b" " if acc and REGULAR.match(acc[-1:]) and REGULAR.match(p[:1]) else b"") + p,
                  parts, b"")


@contextmanager
def compact_syntax():
    """While active, pypdf writes dictionaries and arrays without the newlines and padding of its
    default pretty form (`<</Type/Annot/F 6/Rect[36 720 108 756]...>>`): the same objects, fewer
    bytes in the tail (design section 1: the additions to the PDF are to be minimal).  Whitespace
    only; every reader parses both forms identically."""
    from pypdf.generic import ArrayObject, DictionaryObject

    def dict_write(self, stream, encryption_key=None):
        items = [(k, v) for k, v in self.items() if not (len(k) > 2 and k[1] == "%" and k[-1] == "%")]
        stream.write(b"<<" + b"".join(_joined([_serialised(k), _serialised(v)]) for k, v in items) + b">>")

    def array_write(self, stream, encryption_key=None):
        stream.write(b"[" + _joined([_serialised(x) for x in self]) + b"]")

    saved = DictionaryObject.write_to_stream, ArrayObject.write_to_stream
    DictionaryObject.write_to_stream, ArrayObject.write_to_stream = dict_write, array_write
    try:
        yield
    finally:
        DictionaryObject.write_to_stream, ArrayObject.write_to_stream = saved


XREF_OBJ_RE = re.compile(rb"(\d+) 0 obj<</Type/XRef")
INDEX_RE = re.compile(rb"/Index\[([0-9 ]*)\]")
LENGTH_RE = re.compile(rb"/Length (\d+)>>\nstream\n")


def self_indexed_xref(data: bytes, increment_start: int) -> bytes:
    """The file with its last xref stream listing itself.  pypdf's incremental writer gives the xref
    stream an /Index and entries for every object of the increment but the stream's own object (qpdf:
    "xref entry for the xref stream itself is missing"; PDF 32000-1 7.5.8 lets a cross-reference
    stream cover itself, and Acrobat's always do).  Rewritten here: the stream's object number joins
    /Index (extending the last subsection when it follows on), a type-1 entry for its own offset
    joins the data, and /Length follows; everything before the stream, and its offset, is unchanged."""
    found = list(XREF_OBJ_RE.finditer(data, increment_start))
    if not found:
        raise ValueError("no xref stream in the increment")
    at, number = found[-1].start(), int(found[-1].group(1))
    index_m, length_m = INDEX_RE.search(data, at), LENGTH_RE.search(data, at)
    begin = length_m.end()
    entries = zlib.decompress(data[begin:begin + int(length_m.group(1))])
    index = [int(x) for x in index_m.group(1).split()]
    if any(first <= number < first + count for first, count in zip(index[::2], index[1::2])):
        return data                                        # already lists itself
    index = index[:-1] + [index[-1] + 1] if index and index[-2] + index[-1] == number else index + [number, 1]
    stream = zlib.compress(entries + struct.pack(">BIB", 1, at, 0))
    head = data[at:length_m.start()].replace(index_m.group(0), b"/Index[" + b" ".join(b"%d" % i for i in index) + b"]")
    return (data[:at] + head + b"/Length %d>>\nstream\n" % len(stream) + stream
            + b"\nendstream\nendobj\nstartxref\n%d\n%%%%EOF\n" % at)


def write_annotated(pristine: str, out: str, annots: list[H.HsedAnnot]) -> None:
    """pristine + ONE incremental section holding the annotations, in the order given (pypdf numbers
    the new objects in that order, so the file order is the canonical order), its xref stream
    listing itself ('self_indexed_xref')."""
    w = pypdf.PdfWriter(pristine, incremental=True)
    for a in annots:
        w.add_annotation(page_number=a.page, annotation=H.to_pypdf(a))
    os.makedirs(os.path.dirname(os.path.abspath(out)), exist_ok=True)
    buf = io.BytesIO()
    with compact_syntax():
        w.write(buf)
    with open(out, "wb") as f:
        f.write(self_indexed_xref(buf.getvalue(), os.path.getsize(pristine)))


def manifest_of(ir: dict, pristine_md5: str, records: list[dict]) -> dict:
    return {"irVersion": ir["irVersion"], "spec": ir["spec"], "source": ir["source"], "md5": pristine_md5,
            "pages": ir["pages"], "labelOffset": ir["labelOffset"], "anchors": records}


def dump_json(obj, path: str) -> None:
    with open(path, "w") as f:
        json.dump(obj, f, indent=1, sort_keys=True, ensure_ascii=False)
        f.write("\n")


def annotate(spec: str, pristine: str, ir_path: str, hsa: str, out: str, upd: str, manifest: str) -> dict:
    """The whole job; returns a summary (also what main prints)."""
    ir = json.load(open(ir_path))
    head = open(pristine, "rb").read()
    if ir["spec"] != spec:
        sys.exit(f"{ir_path} is the IR of {ir['spec']}, not {spec}")
    if ir["pdfMd5"] != md5_bytes(head):
        sys.exit(f"{pristine} has md5 {md5_bytes(head)} but {ir_path} was extracted from {ir['pdfMd5']}")
    try:
        anchors = H.parse_hsa(open(hsa).read(), hsa)
    except ValueError as e:
        sys.exit(f"refused: {e}")
    resolved = resolve_all(ir, anchors, spec, hsa)
    annots = [a for a, _ in resolved]
    write_annotated(pristine, out, annots)
    data = open(out, "rb").read()
    assert data[:len(head)] == head, "pypdf did not keep the pristine bytes as a prefix"
    tail = data[len(head):]
    for p in (upd, manifest):
        os.makedirs(os.path.dirname(os.path.abspath(p)), exist_ok=True)
    open(upd, "wb").write(tail)
    dump_json(manifest_of(ir, ir["pdfMd5"], [r for _, r in resolved]), manifest)
    return {"spec": spec, "anchors": len(annots), "pristine_bytes": len(head), "tail_bytes": len(tail),
            "annotated_md5": md5_bytes(data), "out": out, "upd": upd, "manifest": manifest,
            "pages_1based": sorted({a.page + 1 for a in annots})}


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--spec", required=True)
    ap.add_argument("--pristine", required=True, help="the original PDF under tcg/ (never modified)")
    ap.add_argument("--ir", required=True, help="the pristine IR, spec/ir/<spec>.json")
    ap.add_argument("--hsa", required=True, help="the sidecar, spec/annot/<spec>.hsa")
    ap.add_argument("--out", required=True, help="the annotated PDF to write, build/<spec>.hsed.pdf")
    ap.add_argument("--upd", required=True, help="where to export the incremental tail")
    ap.add_argument("--manifest", required=True, help="where to write the manifest JSON")
    args = ap.parse_args(argv)
    print(json.dumps(annotate(args.spec, args.pristine, args.ir, args.hsa, args.out, args.upd, args.manifest)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
