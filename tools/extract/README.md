# tools/extract — the PDF side of spec-as-source (IR v1 since 2026-10-29)

Python, run with `./.venv/bin/python -I` (`make venv` creates the venv from `requirements.txt`).
Design: `docs/spec-as-source.md` §3 (annotation language) and §5 (pipeline); the JSON contract and
the annotation format: `docs/ir-v1.md`. Evidence for the extraction method and for the annotation
mechanics (pypdf incremental update, render identity, three-reader read-back):
`docs/evidence/2026-10-25/` (`extract/findings.md`, `m1_report.md`).

## Extraction

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
  (`make extract`). Core 2.01 and Opal 2.30 are pinned to their PDFs' md5 (`PINNED_MD5`, design
  §5.1 step 1): a PDF of another printing is refused under those spec ids, by make or by hand.
  `--pristine <original.pdf>` extracts an annotated copy: the input must start
  with the original's bytes and `pdfMd5`/`source` are the original's, so pristine and annotated
  inputs give IRs that differ in `annots` alone (that is what `hsedc` reads). `--check-against`
  supplies the other spec's IR for the cross-document invariant (Opal T24's header = Core T178's
  column names); without it that check is SKIP. `--pages a-b` restricts a debugging run
  (invariants skipped).
- `test_extract.py` — unit tests of the pure rules (ragged-row join, section-number parsing,
  item-label concatenation, word normalisation and hashing); `test_annot.py` — the sidecar's
  vocabulary check, kinds and order, the tail's self-listing xref stream, the md5 pins and the
  Table 7 invariant: `make extract-test`.
- `regen_typeuids.py` — regenerates Core Tables 46-165 from the JSON and diffs them against the
  hand transcription in `src/System/SED/MCTP/Common/TypeUIDs.hs` (27 differences, all explained).

## Annotation round trip (design §5.3; `hsedc annotate/apply/dump` until they move to Haskell)

- `hsed_annot.py` — the one place that knows the annotation format: the hidden `/Text` annotation
  (`/F 6`, `/Contents` = the payload verbatim, `/HSED << /V 1 /Kind /Bind /Spec (core-2.01)
  /Id (T75) /Struct (0/1429) >>`), the sidecar line `spec | id | payload`, kind-from-id, the
  canonical order (Doc, Rule, Bind, Law, Behaviour, then natural order of the id), the pypdf
  writer adapter and the pikepdf / pypdf / pymupdf readers. Nothing else parses either form.
  `VOCABULARY` lists, by the first word of a payload, the anchors it may sit on (`doc` on the doc
  anchor, `rule` on a heading, `law`/`fails` on an item, `behaviour` on a heading or an item, the
  table bindings and the modifiers on a table): a sidecar line that breaks it, or starts with a
  word hsedc does not know, is refused before the PDF is written. hsedc's grammar and anchor-kind
  check are the authority; hsed-spec's `VocabularySpec` keeps the table equal to them.
- `annotate.py` — `spec/annot/<spec>.hsa` + `spec/ir/<spec>.json` + the pristine PDF ->
  `build/<spec>.hsed.pdf` (pristine + ONE pypdf incremental section, annotations in canonical
  order, compact object syntax, deterministic; pypdf's xref stream is rewritten to list itself,
  which qpdf otherwise warns of), the tail exported as `spec/annot/<spec>.upd` and
  the manifest `spec/annot/<spec>.manifest.json` (md5, pages, labelOffset, irVersion and per anchor
  id, kind, ordinal, struct path, 0-based page, printed label, title, normalised header, content
  hash, fragment rects). Every sidecar line is resolved through the IR (table id, section number,
  `<section>#<item>`, `doc`); an unknown anchor is refused with its line.
  `.venv/bin/python -I tools/extract/annotate.py --spec core-2.01 --pristine tcg/TCG_Storage_Architecture_Core_Spec_v2.01_r1.00.pdf --ir spec/ir/core-2.01.json --hsa spec/annot/core-2.01.hsa --out build/core-2.01.hsed.pdf --upd spec/annot/core-2.01.upd --manifest spec/annot/core-2.01.manifest.json`
  (`make annotate`).
- `apply.py` — pristine + `.upd` -> the annotated PDF, after checking the pristine's md5 against the
  manifest: `apply.py --pristine <pdf> --upd <upd> --manifest <manifest> --out <pdf>`.
- `dump.py` — annotated PDF -> sidecar text on stdout (`--reader pikepdf|pypdf|pymupdf`); for the
  file `annotate.py` produced this is exactly the committed `.hsa`.
- `pdf_checks.py` — the CI gates on the annotated PDF (`make pdf-checks`; same arguments as
  `annotate.py`, plus `--pages`, `--budget`, `--work`): pristine is an exact byte prefix;
  pristine + upd == annotated; no qpdf syntax warning the pristine file lacks; tail at most 400
  bytes per anchor plus one xref stream (numbers
  reported); the annotations sit in the tail in canonical order; renders of PDF pages 1, 100, 101,
  102, 107 and every anchor page identical to the pristine under Ghostscript (png16m 100 dpi, md5
  and pixel diff 0) and MuPDF, and under CoreGraphics (`sips`) where `/usr/bin/sips` exists (SKIP
  elsewhere); pikepdf, pypdf and pymupdf read back the same annotations, each as the format
  defines it and where the manifest says; `dump(apply(upd)) == hsa`; a second `annotate` run
  reproduces `.upd`, manifest and PDF byte for byte. Renders and the re-run's outputs are left
  under `build/pdf-checks/` with `report.json`. Non-zero exit on any FAIL.

The M1 prototype (`compile.py`, `equiv_check.py`, `probe.py`, `run_all.sh`) was retired on
2026-10-29; its results are recorded in `docs/evidence/2026-10-25/m1_report.md`. Generation and
checking of the bindings now belong to `hsedc` (Haskell, `hsed-spec`).
