# tools/extract — the PDF side of spec-as-source (prototype, 2026-10-25)

Python, run with `./.venv/bin/python -I` (the venv is created by `python3 -m venv .venv &&
.venv/bin/pip install pymupdf pikepdf pypdf`). Design: `docs/spec-as-source.md` §5.
Evidence for everything below: `docs/evidence/2026-10-25/`.

- `extract_tables.py` — walks the tagged-PDF structure tree (pikepdf) joined to the glyph layer
  (pymupdf), repairs PDFMaker's mis-tagging from drawn geometry, and emits every numbered table
  with caption, struct path, pages, header/data rows and per-cell text, bbox, bold, underline and
  fill colour. Output committed as `spec/ir/<spec>.tables.json`:
  `.venv/bin/python -I tools/extract/extract_tables.py tcg/TCG_Storage_Architecture_Core_Spec_v2.01_r1.00.pdf spec/ir/core-2.01.tables.json build/core-index.txt --page-offset 17`
  `.venv/bin/python -I tools/extract/extract_tables.py tcg/TCG_Storage-Opal_SSC_v2.30.pdf spec/ir/opal-2.30.tables.json build/opal-index.txt --page-offset 1`
- `regen_typeuids.py` — regenerates Core Tables 46-165 from the JSON and diffs them against the
  hand transcription in `src/System/SED/MCTP/Common/TypeUIDs.hs` (27 differences, all explained).
- `hsed_annot.py` — the one place that knows the provisional annotation payload (`hsed: <name>`,
  hidden `/F 6`, private `/HSED` dict). `annotate.py` writes hidden annotations as a pypdf
  incremental update; `compile.py` reads them back, re-anchors by struct path and generates a
  Haskell module in hsed's existing shapes; `equiv_check.py` and `probe.py` are the M1 checks.
- `run_all.sh` — the M1 round trip end to end (output under `build/m1/`).

These are the M1 prototypes; the design promotes them to `hsedc` (Haskell) stage by stage.
