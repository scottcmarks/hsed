"""pdf_checks.py: the CI gates on the annotated PDF (design section 5.4 c-e, M1 acceptance a-c).

    pdf_checks.py --spec core-2.01 --pristine tcg/<pdf> --ir spec/ir/core-2.01.json
                  --hsa spec/annot/core-2.01.hsa --out build/core-2.01.hsed.pdf
                  --upd spec/annot/core-2.01.upd --manifest spec/annot/core-2.01.manifest.json
                  [--pages 1,100,101,102,107] [--budget 400] [--work build/pdf-checks]

Checks, each printed PASS / FAIL / SKIP with its numbers; any FAIL makes the exit status 1:
  * the manifest and the IR were made for this pristine file (md5);
  * the pristine bytes are an exact prefix of the annotated file, and pristine + .upd == annotated;
  * qpdf (through pikepdf) finds no syntax warning in the annotated file that it does not find in
    the pristine one (the tail's xref stream lists itself);
  * the tail is at most --budget bytes per anchor plus one xref stream (the numbers are reported);
  * the annotation objects sit in the tail in canonical order;
  * renders of the listed 1-based pages (plus every page an anchor sits on) are identical to the
    pristine under Ghostscript (png16m 100 dpi: md5 and a pixel diff of 0) and MuPDF, and under
    CoreGraphics (sips on a single-page extraction) where /usr/bin/sips exists, else SKIP;
  * pikepdf, pypdf and pymupdf read back the same annotations, as many as the manifest has, each
    exactly as the format defines it, and each where the manifest says;
  * dump(apply(upd)) == the committed .hsa, byte for byte;
  * a second annotate run reproduces the .upd, the manifest and the PDF byte for byte.
Renders, the applied PDF and the re-run's outputs are left under --work for inspection.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
from functools import reduce

import pikepdf
import pymupdf
from PIL import Image, ImageChops

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import hsed_annot as H  # noqa: E402
from annotate import annotate  # noqa: E402
from apply import apply  # noqa: E402
from dump import dump  # noqa: E402

GS = shutil.which("gs") or "/opt/local/bin/gs"
SIPS = "/usr/bin/sips" if os.path.exists("/usr/bin/sips") else None   # CoreGraphics: macOS only
DPI = 100
DEFAULT_PAGES = "1,100,101,102,107"
RENDERERS = ("gs", "mupdf", "sips")
XREF_RE = re.compile(rb"(\d+) 0 obj<<")                              # pypdf writes the xref stream object this way
ID_RE = re.compile(rb"/Id\s*\(([^)]*)\)")


def md5_file(p: str) -> str:
    return hashlib.md5(open(p, "rb").read()).hexdigest()


def md5_bytes(b: bytes) -> str:
    return hashlib.md5(b).hexdigest()


# ----------------------------------------------------------------------------- renderers
def gs_render(pdf: str, page1: int, png: str) -> str | None:
    r = subprocess.run([GS, "-q", "-dNOPAUSE", "-dBATCH", "-dSAFER", "-sDEVICE=png16m", f"-r{DPI}",
                        f"-dFirstPage={page1}", f"-dLastPage={page1}", f"-sOutputFile={png}", pdf],
                       capture_output=True, text=True)
    return md5_file(png) if r.returncode == 0 and os.path.exists(png) else None


def pixdiff(a: str, b: str) -> int | None:
    """Number of pixels that differ in any channel; None when the sizes differ."""
    ia, ib = Image.open(a).convert("RGB"), Image.open(b).convert("RGB")
    if ia.size != ib.size:
        return None
    any_channel = reduce(ImageChops.lighter, ImageChops.difference(ia, ib).split())
    return sum(any_channel.histogram()[1:])


def mupdf_render_md5(pdf: str, page0: int) -> str:
    doc = pymupdf.open(pdf)
    try:
        return md5_bytes(doc[page0].get_pixmap(dpi=DPI, annots=True).tobytes("png"))
    finally:
        doc.close()


def single_page_pdf(src: str, page0: int, out: str) -> None:
    """One page (with its /Annots) in a fresh PDF, so CoreGraphics renders just that page."""
    with pikepdf.open(src) as s, pikepdf.new() as d:
        d.pages.append(s.pages[page0])
        d.save(out)


def sips_render(pdf: str, png: str) -> str | None:
    r = subprocess.run([SIPS, "-s", "format", "png", pdf, "--out", png], capture_output=True, text=True)
    return md5_file(png) if r.returncode == 0 and os.path.exists(png) else None


def render_page(pristine: str, out: str, page1: int, rdir: str) -> dict:
    """{renderer: {"pristine": md5, "annotated": md5, "identical": bool|None, ...}} for one 1-based page."""
    p0 = page1 - 1
    g0, g1 = gs_render(pristine, page1, f"{rdir}/p{page1}_pristine_gs.png"), gs_render(out, page1, f"{rdir}/p{page1}_annotated_gs.png")
    m0, m1 = mupdf_render_md5(pristine, p0), mupdf_render_md5(out, p0)
    res = {"gs": {"pristine": g0, "annotated": g1, "identical": g0 is not None and g0 == g1,
                  "differing_pixels": pixdiff(f"{rdir}/p{page1}_pristine_gs.png", f"{rdir}/p{page1}_annotated_gs.png") if g0 and g1 else None},
           "mupdf": {"pristine": m0, "annotated": m1, "identical": m0 == m1}}
    if SIPS is None:
        res["sips"] = {"pristine": None, "annotated": None, "identical": None}
    else:
        single_page_pdf(pristine, p0, f"{rdir}/p{page1}_pristine.pdf")
        single_page_pdf(out, p0, f"{rdir}/p{page1}_annotated.pdf")
        s0, s1 = sips_render(f"{rdir}/p{page1}_pristine.pdf", f"{rdir}/p{page1}_pristine_sips.png"), sips_render(f"{rdir}/p{page1}_annotated.pdf", f"{rdir}/p{page1}_annotated_sips.png")
        res["sips"] = {"pristine": s0, "annotated": s1, "identical": s0 is not None and s0 == s1}
    return res


# ----------------------------------------------------------------------------- the checks
def syntax_warnings(pdf: str) -> list[str]:
    """qpdf's syntax warnings for a file, the file name taken out so two files compare."""
    with pikepdf.open(pdf) as p:
        return [w.replace(pdf, "<file>") for w in p.check_pdf_syntax()]


def tail_budget(tail: bytes, n_anchors: int, budget: int) -> dict:
    """Split the tail into the objects (page dictionaries, /Annots arrays, annotations) and the one
    xref stream with its startxref trailer; the budget is on the former, per anchor."""
    xrefs = list(XREF_RE.finditer(tail))
    xref_at = xrefs[-1].start() if xrefs else len(tail)
    objects = xref_at
    return {"tail_bytes": len(tail), "xref_stream_bytes": len(tail) - xref_at, "object_bytes": objects,
            "anchors": n_anchors, "bytes_per_anchor": round(objects / n_anchors, 1) if n_anchors else None,
            "budget_per_anchor": budget, "within_budget": n_anchors > 0 and objects <= budget * n_anchors,
            "one_xref_stream": len(xrefs) == 1 and re.search(rb"/Type\s*/XRef", tail[xref_at:]) is not None}


def check_readers(out: str, manifest: dict) -> tuple[list[tuple[str, bool | None, str]], dict]:
    by_reader = {name: sorted(read(out), key=lambda a: a.sort_key) for name, read in H.READERS.items()}
    ref = by_reader["pikepdf"]
    by_id = {a.id: a for a in ref}
    problems = [f"{a.id}: {p}" for a in ref for p in H.problems(a)]
    misplaced = [f"{r['id']}: stored at struct {by_id[r['id']].struct!r} page {by_id[r['id']].page}, manifest says {r['struct']!r} page {r['page']}"
                 for r in manifest["anchors"] if r["id"] in by_id and (by_id[r["id"]].struct, by_id[r["id"]].page) != (r["struct"], r["page"])]
    missing = [r["id"] for r in manifest["anchors"] if r["id"] not in by_id]
    agree = all(v == ref for v in by_reader.values())
    return [("pikepdf, pypdf and pymupdf read back the same annotations", agree,
             ", ".join(f"{k}: {len(v)}" for k, v in by_reader.items()) + ("" if agree else "; readers differ")),
            ("as many annotations as the manifest has anchors", len(ref) == len(manifest["anchors"]) and not missing,
             f"{len(ref)} read, {len(manifest['anchors'])} in the manifest" + (f"; missing {missing}" if missing else "")),
            ("every annotation exactly as the format defines it", not problems, "; ".join(problems) or "all /V 1, /F 6, kind from id"),
            ("every annotation where the manifest says", not misplaced, "; ".join(misplaced) or "struct paths and pages agree")], \
        {name: [a.__dict__ for a in v] for name, v in by_reader.items()}


def run_checks(args) -> tuple[list[tuple[str, bool | None, str]], dict]:
    work = args.work
    os.makedirs(f"{work}/render", exist_ok=True)
    pristine, annotated, upd = open(args.pristine, "rb").read(), open(args.out, "rb").read(), open(args.upd, "rb").read()
    manifest, ir_meta = json.load(open(args.manifest)), json.load(open(args.ir))
    hsa_text = open(args.hsa).read()
    n = len(manifest["anchors"])
    pmd5 = md5_bytes(pristine)
    tail = annotated[len(pristine):]
    budget = tail_budget(tail, n, args.budget)
    ids_in_tail = [i.decode("latin1") for i in ID_RE.findall(tail)]
    canonical = [r["id"] for r in manifest["anchors"]]

    pristine_warnings, annotated_warnings = syntax_warnings(args.pristine), syntax_warnings(args.out)
    new_warnings = [w for w in annotated_warnings if w not in pristine_warnings]
    checks = [
        ("manifest md5 is the pristine file's", manifest["md5"] == pmd5, f"{pmd5}"),
        ("IR pdfMd5 is the pristine file's", ir_meta["pdfMd5"] == pmd5, f"{ir_meta['pdfMd5']}"),
        ("pristine bytes are an exact prefix of the annotated file", annotated.startswith(pristine),
         f"{len(pristine)} + {len(tail)} = {len(annotated)} bytes"),
        ("pristine + upd == annotated byte for byte", pristine + upd == annotated, f"upd {len(upd)} bytes, md5 {md5_bytes(annotated)}"),
        ("no qpdf syntax warning the pristine file does not have", not new_warnings,
         "; ".join(new_warnings) or f"{len(annotated_warnings)} warnings, as the pristine file"),
        ("one xref stream in the tail", budget["one_xref_stream"], f"{budget['xref_stream_bytes']} bytes including startxref/%%EOF"),
        (f"tail at most {args.budget} bytes per anchor plus the xref stream", budget["within_budget"],
         f"{budget['object_bytes']} object bytes / {n} anchors = {budget['bytes_per_anchor']} per anchor"),
        ("annotations in the tail in canonical order", ids_in_tail == canonical, f"{ids_in_tail}"),
    ]

    pages = sorted({int(p) for p in args.pages.split(",") if p} | {r["page"] + 1 for r in manifest["anchors"]})
    renders = {str(p): render_page(args.pristine, args.out, p, f"{work}/render") for p in pages}
    checks += [(f"page {p} renders identically under {r}",
                None if renders[str(p)][r]["identical"] is None else renders[str(p)][r]["identical"] and renders[str(p)][r].get("differing_pixels", 0) == 0,
                "sips absent" if renders[str(p)][r]["identical"] is None else
                f"md5 {renders[str(p)][r]['annotated']}" + (f", {renders[str(p)][r]['differing_pixels']} differing pixels" if r == "gs" else ""))
               for p in pages for r in RENDERERS]

    reader_checks, readback = check_readers(args.out, manifest)
    checks += reader_checks

    applied = apply(args.pristine, args.upd, args.manifest, f"{work}/applied.pdf")
    text, problems = dump(f"{work}/applied.pdf")
    checks.append(("dump(apply(upd)) == hsa", text == hsa_text and not problems,
                   "identical" if text == hsa_text else f"dump gives {text!r}, the file has {hsa_text!r}"))

    again = annotate(args.spec, args.pristine, args.ir, args.hsa, f"{work}/again.hsed.pdf", f"{work}/again.upd", f"{work}/again.manifest.json")
    same_upd = open(f"{work}/again.upd", "rb").read() == upd
    same_manifest = open(f"{work}/again.manifest.json").read() == open(args.manifest).read()
    same_pdf = md5_file(f"{work}/again.hsed.pdf") == md5_bytes(annotated)
    checks.append(("a second annotate run reproduces upd, manifest and PDF byte for byte", same_upd and same_manifest and same_pdf,
                   f"upd {'same' if same_upd else 'DIFFERS'}, manifest {'same' if same_manifest else 'DIFFERS'}, pdf {'same' if same_pdf else 'DIFFERS'}"))

    summary = {"spec": args.spec, "pristine_md5": pmd5, "annotated_md5": md5_bytes(annotated), **budget,
               "pages_rendered_1based": pages, "gs_version": subprocess.run([GS, "--version"], capture_output=True, text=True).stdout.strip(),
               "mupdf": pymupdf.mupdf_version if hasattr(pymupdf, "mupdf_version") else pymupdf.__version__,
               "sips": SIPS, "renders": renders, "readback": readback, "applied": applied, "again": again}
    return checks, summary


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--spec", required=True)
    ap.add_argument("--pristine", required=True)
    ap.add_argument("--ir", required=True)
    ap.add_argument("--hsa", required=True)
    ap.add_argument("--out", required=True, help="the annotated PDF annotate.py wrote")
    ap.add_argument("--upd", required=True)
    ap.add_argument("--manifest", required=True)
    ap.add_argument("--pages", default=DEFAULT_PAGES, help="1-based PDF pages to render (anchor pages are added)")
    ap.add_argument("--budget", type=int, default=400, help="allowed tail bytes per anchor, xref stream excluded")
    ap.add_argument("--work", default="build/pdf-checks", help="where renders and the re-run's outputs go")
    args = ap.parse_args(argv)
    checks, summary = run_checks(args)
    for name, ok, detail in checks:
        print(f"{'SKIP' if ok is None else 'PASS' if ok else 'FAIL'} {name} ({detail})")
    json.dump(summary, open(f"{args.work}/report.json", "w"), indent=1, sort_keys=True)
    failed = [name for name, ok, _ in checks if ok is False]
    print(json.dumps({k: summary[k] for k in ("tail_bytes", "xref_stream_bytes", "object_bytes", "anchors", "bytes_per_anchor")}
                     | {"checks": len(checks), "failed": failed, "report": f"{args.work}/report.json"}))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
