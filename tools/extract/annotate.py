"""annotate.py: write an annotated COPY of the Core spec with exactly three hidden hsed annotations
(Table 46 typeRow, Table 52 enumValues, Table 240 tableUIDs) via a pypdf incremental update,
then verify that the original is an exact byte prefix and that every touched page renders
identically (Ghostscript png16m 100 dpi md5 + pixel diff, MuPDF md5, CoreGraphics/sips md5 on a
single-page extraction).

Usage: python -I annotate.py <original.pdf> <core_tables.json> <out_dir>
Writes <out_dir>/core_annotated.pdf, <out_dir>/annotate_report.json, renders under <out_dir>/render/.
"""
from __future__ import annotations

import hashlib
import json
import os
import shutil
import subprocess
import sys
from functools import reduce

import pikepdf
import pymupdf
import pypdf
from PIL import Image, ImageChops

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import hsed_annot as H  # noqa: E402

GS = "/opt/local/bin/gs"
SIPS = "/usr/bin/sips"
DPI = 100
TARGETS = ((46, H.TYPE_ROW), (52, H.ENUM_VALUES), (240, H.TABLE_UIDS))   # (table number, directive)


def md5_file(p: str) -> str:
    return hashlib.md5(open(p, "rb").read()).hexdigest()


def md5_bytes(b: bytes) -> str:
    return hashlib.md5(b).hexdigest()


# ----------------------------------------------------------------------------- anchors from the extractor's JSON
def cells_of(t: dict) -> list[dict]:
    return [c for row in t["header_rows"] + t["rows"] for c in row] \
        + [c for r in t["repeated_header_rows"] for c in r["cells"]]


def boxes_on(t: dict, page: int) -> list[list[float]]:
    """Every rectangle the extractor attributed to a cell on `page`: glyph bbox, estimated bbox of an
    empty cell, drawn (shaded) cell rect, and per-page fragments."""
    return [b for c in cells_of(t) if c.get("page") == page
            for b in (c.get("bbox"), c.get("est_bbox"), c.get("fill_bbox")) if b] \
        + [f["bbox"] for c in cells_of(t) for f in c.get("fragments", []) if f.get("page") == page and f.get("bbox")]


def union(boxes) -> tuple[float, float, float, float]:
    return reduce(lambda a, b: (min(a[0], b[0]), min(a[1], b[1]), max(a[2], b[2]), max(a[3], b[3])), boxes)


def anchor(t: dict, directive: str) -> H.HsedAnnot:
    pages = tuple(t["pages"])
    return H.HsedAnnot(directive=directive, path=tuple(t["struct_path"]), caption=t["caption"],
                       pages=pages, rect=tuple(round(v, 2) for v in union(boxes_on(t, pages[0]))))


def select(tables: list[dict]) -> list[H.HsedAnnot]:
    by_num = {t["table_number"]: t for t in tables if t["table_number"] is not None and not t.get("part")}
    return [anchor(by_num[n], d) for n, d in TARGETS]


# ----------------------------------------------------------------------------- writer
def write_annotated(orig: str, out: str, annots: list[H.HsedAnnot]) -> None:
    w = pypdf.PdfWriter(orig, incremental=True)
    for a in annots:
        w.add_annotation(page_number=a.page, annotation=H.to_pypdf(a))
    w.write(out)


# ----------------------------------------------------------------------------- renderers
def gs_render(pdf: str, page1: int, png: str) -> dict:
    r = subprocess.run([GS, "-q", "-dNOPAUSE", "-dBATCH", "-dSAFER", "-sDEVICE=png16m", f"-r{DPI}",
                        f"-dFirstPage={page1}", f"-dLastPage={page1}", f"-sOutputFile={png}", pdf],
                       capture_output=True, text=True)
    return {"png": png, "md5": md5_file(png) if r.returncode == 0 else None, "rc": r.returncode, "stderr": r.stderr[-300:]}


def pixdiff(a: str, b: str) -> dict:
    ia, ib = Image.open(a).convert("RGB"), Image.open(b).convert("RGB")
    if ia.size != ib.size:
        return {"size_mismatch": [ia.size, ib.size]}
    d = ImageChops.difference(ia, ib)
    any_channel = reduce(ImageChops.lighter, d.split())          # per-pixel max over R,G,B: 0 only where all three agree
    return {"differing_pixels": sum(any_channel.histogram()[1:]), "diff_bbox": d.getbbox(), "size": ia.size}


def mupdf_render_md5(pdf: str, page0: int) -> str:
    doc = pymupdf.open(pdf)
    try:
        return md5_bytes(doc[page0].get_pixmap(dpi=DPI, annots=True).tobytes("png"))
    finally:
        doc.close()


def single_page_pdf(src: str, page0: int, out: str) -> None:
    """Copy one page (with its /Annots) into a fresh PDF so CoreGraphics renders just that page."""
    with pikepdf.open(src) as s, pikepdf.new() as d:
        d.pages.append(s.pages[page0])
        d.save(out)


def sips_render(pdf: str, png: str) -> dict:
    r = subprocess.run([SIPS, "-s", "format", "png", pdf, "--out", png], capture_output=True, text=True)
    return {"png": png, "md5": md5_file(png) if r.returncode == 0 and os.path.exists(png) else None,
            "rc": r.returncode, "stderr": r.stderr[-200:]}


def render_checks(orig: str, out: str, page0: int, rdir: str) -> dict:
    p1 = page0 + 1
    g0, g1 = gs_render(orig, p1, f"{rdir}/p{p1}_orig_gs.png"), gs_render(out, p1, f"{rdir}/p{p1}_annot_gs.png")
    single_page_pdf(orig, page0, f"{rdir}/p{p1}_orig.pdf")
    single_page_pdf(out, page0, f"{rdir}/p{p1}_annot.pdf")
    s0, s1 = sips_render(f"{rdir}/p{p1}_orig.pdf", f"{rdir}/p{p1}_orig_sips.png"), sips_render(f"{rdir}/p{p1}_annot.pdf", f"{rdir}/p{p1}_annot_sips.png")
    m0, m1 = mupdf_render_md5(orig, page0), mupdf_render_md5(out, page0)
    return {"pdf_page_1based": p1,
            "gs": {"orig_md5": g0["md5"], "annot_md5": g1["md5"], "identical": g0["md5"] == g1["md5"] and g0["md5"] is not None,
                   "pixdiff": pixdiff(g0["png"], g1["png"]) if g0["md5"] and g1["md5"] else None, "stderr": (g0["stderr"] + g1["stderr"]).strip()},
            "mupdf": {"orig_md5": m0, "annot_md5": m1, "identical": m0 == m1},
            "sips": {"orig_md5": s0["md5"], "annot_md5": s1["md5"], "identical": s0["md5"] == s1["md5"] and s0["md5"] is not None,
                     "single_page_pdfs": [f"{rdir}/p{p1}_orig.pdf", f"{rdir}/p{p1}_annot.pdf"], "stderr": (s0["stderr"] + s1["stderr"]).strip()}}


# ----------------------------------------------------------------------------- read-back
def readback(out: str) -> dict:
    with pikepdf.open(out) as pdf:
        found = H.find_pikepdf(pdf)
        annots_per_page = {i: [str(a.get("/Subtype")) for a in (pdf.pages[i].obj.get("/Annots") or [])]
                           for i in sorted({i for i, _, _ in found})}
        n_objects = len(pdf.objects)
        is_lin = pdf.is_linearized
    with pikepdf.open(out) as pdf:
        raw = {f"page{i}": {str(k): (str(v) if k != "/HSED" else {str(kk): str(vv) for kk, vv in v.items()})
                            for k, v in a.items() if k not in ("/P",)}
               for i, pg in enumerate(pdf.pages) for a in (pg.obj.get("/Annots") or []) if H.from_pikepdf(a)[0] is not None}
    return {"found": [{"page": i, "directive": a.directive, "path": H.format_path(a.path), "caption": a.caption,
                       "pages": list(a.pages), "rect": list(a.rect), "problems": probs} for i, a, probs in found],
            "annots_on_touched_pages": annots_per_page, "n_objects": n_objects, "is_linearized_claimed": is_lin,
            "raw_annotation_dicts": raw}


def tail_text(orig: str, out: str) -> str:
    ob, nb = open(orig, "rb").read(), open(out, "rb").read()
    return nb[len(ob):].decode("latin1")


def main(argv) -> int:
    orig, tables_json, out_dir = argv[1], argv[2], argv[3]
    os.makedirs(f"{out_dir}/render", exist_ok=True)
    out = f"{out_dir}/core_annotated.pdf"
    tables = json.load(open(tables_json))["tables"]
    annots = select(tables)
    write_annotated(orig, out, annots)

    ob, nb = open(orig, "rb").read(), open(out, "rb").read()
    touched = sorted({a.page for a in annots})                       # pages whose /Page dict was rewritten
    listed = sorted({p for a in annots for p in a.pages})           # every page any table touches
    report = {
        "original": {"path": orig, "size": len(ob), "md5": md5_bytes(ob)},
        "annotated": {"path": out, "size": len(nb), "md5": md5_bytes(nb), "delta_bytes": len(nb) - len(ob),
                      "original_is_exact_prefix": nb[:len(ob)] == ob},
        "annotations_written": [{"directive": a.directive, "page0": a.page, "pdf_page_1based": a.page + 1,
                                 "path": H.format_path(a.path), "caption": a.caption, "pages": list(a.pages),
                                 "rect": list(a.rect), "contents": H.format_contents(a.directive)} for a in annots],
        "incremental_tail": tail_text(orig, out),
        "readback": readback(out),
        "gs_version": subprocess.run([GS, "--version"], capture_output=True, text=True).stdout.strip(),
        "renders": {str(p + 1): render_checks(orig, out, p, f"{out_dir}/render") for p in listed},
        "pages_touched_1based": [p + 1 for p in touched], "pages_listed_1based": [p + 1 for p in listed],
    }
    report["all_renders_identical"] = all(r[k]["identical"] for r in report["renders"].values() for k in ("gs", "mupdf", "sips"))
    report["all_pixdiff_zero"] = all(r["gs"]["pixdiff"]["differing_pixels"] == 0 for r in report["renders"].values())
    json.dump(report, open(f"{out_dir}/annotate_report.json", "w"), indent=1, default=str)
    print(json.dumps({k: report[k] for k in ("original", "annotated", "pages_touched_1based", "pages_listed_1based",
                                              "all_renders_identical", "all_pixdiff_zero")}, indent=1))
    print("renders:", json.dumps({p: {k: r[k]["identical"] for k in ("gs", "mupdf", "sips")} | {"pixdiff": r["gs"]["pixdiff"]["differing_pixels"]}
                                  for p, r in report["renders"].items()}))
    print("readback:", json.dumps(report["readback"]["found"], indent=1))
    return 0 if report["annotated"]["original_is_exact_prefix"] and report["all_renders_identical"] and len(report["readback"]["found"]) == 3 else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
