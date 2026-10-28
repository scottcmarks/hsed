"""probe.py: robustness of the hsed annotations under other PDF writers. The annotated copy is
re-saved through pymupdf (incremental, incremental+edit, full, full+garbage/clean), pikepdf (full
rewrite, object streams regenerated + linearized) and pypdf (full rewrite); each result is fed to
compile.py (discovery + full re-anchoring through the extractor) and its anchor pages are rendered
with Ghostscript to see whether a writer made the hidden annotations visible.
Usage: python -I probe.py <annotated.pdf> <out_dir> <compile.py> <original.pdf>
"""
from __future__ import annotations

import hashlib
import re
import json
import os
import shutil
import subprocess
import sys

import pikepdf
import pymupdf
import pypdf

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import hsed_annot as H  # noqa: E402
from annotate import gs_render  # noqa: E402

PYTHON = sys.executable
ANCHOR_PAGES_1 = (100, 102, 302)


def md5(p):
    return hashlib.md5(open(p, "rb").read()).hexdigest()


def mupdf_incremental(src, out, edit=False):
    shutil.copyfile(src, out)
    doc = pymupdf.open(out)
    if edit:
        doc.set_metadata(doc.metadata | {"subject": "probe"})
    doc.save(out, incremental=True, encryption=pymupdf.PDF_ENCRYPT_KEEP)
    doc.close()


def mupdf_full(src, out, **kw):
    doc = pymupdf.open(src)
    doc.save(out, **kw)
    doc.close()


def pike_full(src, out, **kw):
    with pikepdf.open(src) as pdf:
        pdf.save(out, **kw)


def pypdf_full(src, out):
    w = pypdf.PdfWriter(clone_from=src)
    w.write(out)


WRITERS = {
    "pymupdf_incremental_noop": lambda s, o: mupdf_incremental(s, o),
    "pymupdf_incremental_metadata_edit": lambda s, o: mupdf_incremental(s, o, edit=True),
    "pymupdf_full_save": lambda s, o: mupdf_full(s, o),
    "pymupdf_full_garbage4_clean_deflate": lambda s, o: mupdf_full(s, o, garbage=4, clean=True, deflate=True),
    "pikepdf_full_default": lambda s, o: pike_full(s, o),
    "pikepdf_objstm_generate_linearize": lambda s, o: pike_full(s, o, object_stream_mode=pikepdf.ObjectStreamMode.generate, linearize=True),
    "pypdf_full_rewrite": pypdf_full,
}


def unname(module_text: str) -> str:
    """Provenance names the compiled file and its md5; mask both so modules from re-saved copies compare."""
    return re.sub(r"md5 [0-9a-f]{32}", "md5 X", re.sub(r"[\w.-]+\.pdf", "X.pdf", module_text))


def raw_annots(path):
    with pikepdf.open(path) as pdf:
        return [{"page": i, "keys": sorted(str(k) for k in a.keys()), "F": int(a.get("/F", -1)),
                 "contents": str(a.get("/Contents", "")), "has_HSED": H.PRIVATE_KEY in a,
                 "HSED": {str(k): (str(v) if not isinstance(v, pikepdf.Array) else [int(x) for x in v]) for k, v in a[H.PRIVATE_KEY].items()} if H.PRIVATE_KEY in a else None}
                for i, pg in enumerate(pdf.pages) for a in (pg.obj.get("/Annots") or []) if str(a.get("/Subtype")) == "/Text"]


def main(argv):
    src, out_dir, compile_py, orig = argv[1:5]
    os.makedirs(out_dir, exist_ok=True)
    base = {p: gs_render(src, p, f"{out_dir}/base_p{p}.png")["md5"] for p in ANCHOR_PAGES_1}
    src_bytes = open(src, "rb").read()
    results = {}
    for name, writer in WRITERS.items():
        out = f"{out_dir}/{name}.pdf"
        d = f"{out_dir}/{name}"
        os.makedirs(d, exist_ok=True)
        try:
            writer(src, out)
        except Exception as e:  # noqa: BLE001
            results[name] = {"writer_error": f"{type(e).__name__}: {e}"}
            continue
        nb = open(out, "rb").read()
        disc = subprocess.run([PYTHON, "-I", compile_py, out, d, "--discover-only"], capture_output=True, text=True)
        full = subprocess.run([PYTHON, "-I", compile_py, out, d], capture_output=True, text=True)
        full_rep = json.load(open(f"{d}/compile_report.json")) if os.path.exists(f"{d}/compile_report.json") else {}
        renders = {p: gs_render(out, p, f"{d}/p{p}.png")["md5"] for p in ANCHOR_PAGES_1}
        gen = f"{d}/Hsed/Generated/CoreM1.hs"
        results[name] = {
            "size": len(nb), "delta_vs_annotated": len(nb) - len(src_bytes),
            "annotated_is_prefix": nb[:len(src_bytes)] == src_bytes,
            "raw_text_annots": raw_annots(out),
            "discover_exit": disc.returncode, "discovered": json.loads(disc.stdout)["discovered"] if disc.returncode in (0, 1) and disc.stdout.strip().startswith("{") else disc.stderr[-400:],
            "full_compile_exit": full.returncode, "n_compiled": full_rep.get("n_compiled"),
            "located_problems": [l["problems"] for l in full_rep.get("located", [])],
            "generated_module_identical": os.path.exists(gen) and unname(open(gen).read()) == unname(open(f"{HERE}/Hsed/Generated/CoreM1.hs").read()),
            "gs_render_identical": {p: renders[p] == base[p] for p in ANCHOR_PAGES_1},
        }
    summary = {n: ({"error": r["writer_error"]} if "writer_error" in r else
                   {"found": len(r["raw_text_annots"]), "F": sorted({a["F"] for a in r["raw_text_annots"]}),
                    "HSED_kept": all(a["has_HSED"] for a in r["raw_text_annots"]), "extra_keys": sorted({k for a in r["raw_text_annots"] for k in a["keys"]} - {"/Type", "/Subtype", "/Rect", "/Contents", "/F", "/HSED", "/P"}),
                    "compiled": r["n_compiled"], "module_identical": r["generated_module_identical"], "renders_identical": all(r["gs_render_identical"].values()),
                    "delta_bytes": r["delta_vs_annotated"], "prefix_kept": r["annotated_is_prefix"]})
               for n, r in results.items()}
    json.dump({"base_gs_md5": base, "results": results, "summary": summary}, open(f"{out_dir}/probe_report.json", "w"), indent=1, default=str)
    print(json.dumps(summary, indent=1))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
