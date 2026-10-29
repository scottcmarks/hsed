# tools/extract — the PDF side of spec-as-source (prototype, 2026-10-25; IR v1 since 2026-10-29)

Python, run with `./.venv/bin/python -I` (`make venv` creates the venv from `requirements.txt`).
Design: `docs/spec-as-source.md` §5; the JSON contract: `docs/ir-v1.md`.
Evidence for the extraction method: `docs/evidence/2026-10-25/`.

- `extract_tables.py` — emits IR v1 for one tagged TCG PDF: walks the structure tree (pikepdf)
  joined to the glyph layer (pymupdf), repairs PDFMaker's mis-tagging from drawn geometry, joins
  ragged continuation rows, and writes one sorted-key JSON with `tables` (every /Table: id,
  ordinal, per-page rects, caption, struct path, header/data rows, per-cell text, bbox, bold,
  underline, fill colour), `headings` (/H1../H6 with section number, title, level, page, rect),
  `paras` (every /P and /LI outside tables with section, list-item label, page, rect and a sha256
  of the normalised words) and `annots` (every /Text annotation carrying an /HSED dictionary).
  The run ends with the design's sanity invariants printed PASS/FAIL to stderr (a FAIL makes the
  exit status 1) and a one-line JSON summary. Committed output: `spec/ir/<spec>.json`:
  `.venv/bin/python -I tools/extract/extract_tables.py tcg/TCG_Storage_Architecture_Core_Spec_v2.01_r1.00.pdf spec/ir/core-2.01.json build/core-index.txt --spec core-2.01 --page-offset 17`
  `.venv/bin/python -I tools/extract/extract_tables.py tcg/TCG_Storage-Opal_SSC_v2.30.pdf spec/ir/opal-2.30.json build/opal-index.txt --spec opal-2.30 --page-offset 1 --check-against spec/ir/core-2.01.json`
  (`make extract`). `--pristine <original.pdf>` extracts an annotated copy: the input must start
  with the original's bytes and `pdfMd5`/`source` are the original's, so pristine and annotated
  inputs give IRs that differ in `annots` alone. `--check-against` supplies the other spec's IR for
  the cross-document invariant (Opal T24's header = Core T178's column names); without it that
  check is SKIP. `--pages a-b` restricts a debugging run (invariants skipped).
- `test_extract.py` — unit tests of the pure rules (ragged-row join, section-number parsing,
  item-label concatenation, word normalisation and hashing): `make extract-test`.
- `regen_typeuids.py` — regenerates Core Tables 46-165 from the JSON and diffs them against the
  hand transcription in `src/System/SED/MCTP/Common/TypeUIDs.hs` (27 differences, all explained).
- `hsed_annot.py` — the one place that knows the provisional M1 annotation payload (`hsed: <name>`,
  hidden `/F 6`, private `/HSED` dict). `annotate.py` writes hidden annotations as a pypdf
  incremental update; `compile.py` reads them back, re-anchors by struct path and generates a
  Haskell module in hsed's existing shapes; `equiv_check.py` and `probe.py` are the M1 checks.
- `run_all.sh` — the M1 round trip end to end (output under `build/m1/`): `make m1`.

These are the M1 prototypes; the design promotes them to `hsedc` (Haskell) stage by stage.
