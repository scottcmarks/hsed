# Findings: raw table extraction from the Core 2.01 and Opal 2.30 tagged PDFs

All files live in `/private/tmp/claude-501/-Users-scott-hsed/f05c341b-63d7-4ac7-8e79-f36321a58c77/scratchpad/extract/`.
Nothing under `~/hsed` was written; the two source PDFs are unchanged
(md5 `5ceb93ab118a578125e3755eb885b668` Core, `054e505db3895b25c38dd9dfbcc36259` Opal; the runs
used the byte-identical copies in `../pdf-structure/pdfs/`).

## 1. Counts

| | Core 2.01 | Opal 2.30 |
|---|---|---|
| `/Table` struct elements found | **263** (250 top-level + 13 nested in `/TD` cells) | **63** (all top-level) |
| entries with a `Table N` caption | 249 | 50 |
| distinct table numbers | **248 = Tables 1..248, none missing** | **50 = Tables 1..50, none missing** |
| numbers appearing twice | Table 7 (two struct tables, `part` 1/2 and 2/2: Word split it at the page 33/34 break; part 1 is the header-only fragment) | none |
| unnumbered top-level tables | 1 (revision history, PDF p3) | 13 (contributors p3, terms p15-16, five single-cell boxed formula notes p87/88/99/100, two empty layout tables p100, four geometry-example strips p101) |
| cells (header + data rows) | 5,643 (596 empty) | 4,870 (2,571 empty) |
| tables spanning >1 page | 40 (max 3 pages) | 16 (max 23 pages: Table 38) |
| repeated header rows removed | 35 rows in 33 tables | 41 rows in 11 tables |

Expected 248 / 50: met. The 13 Core nested tables are the `Value / Interpretation` sub-tables
inside the token-encoding Tables 5-13; they are emitted as their own records (`nested_in` = parent
table path, caption null) and the parent cell carries `nested_tables` and excludes their text.

## 2. Method (what `extract_tables.py` does)

Two views of each PDF are joined by the structure-tree index path:

* **pikepdf** walks `/StructTreeRoot` for the logical model: every `/Table` (nested ones too), its
  `/TR` rows, `/TH|/TD|/Artifact` cells, the `/K`-array index path of each element, and the pages
  each element's marked-content leaves live on (handles inherited `/Pg`). The tree already stitches
  page-spanning tables.
* **pymupdf 1.28 `TEXT_COLLECT_STRUCTURE`** gives the physical model per page: the XML text page
  wraps every glyph in `<struct idx raw std>` nodes whose `idx` chain is exactly the pikepdf path
  (validated: 7,619/7,632 Core and 6,197/6,198 Opal Table/TR/TD/TH chains resolve to the same-typed
  element; the 14 misses are `idx=-1` dangling elements, see section 3). So each glyph (quad, font,
  size, colour) is attributed to a cell with no content-stream interpretation of our own; underscores
  survive (`AC_element`, `List_Type, *, ACE_expression`), rotated Opal headers come out whole
  (`MandatoryWrite Granularity`).
* Text: glyphs are re-grouped into visual lines geometrically (same page, direction and baseline),
  which re-interleaves nested `/Reference`/`/Span` elements MuPDF emits as separate blocks; a space is
  inserted where glyphs leave a gap > 0.25 em. `text` = lines joined by one space, whitespace
  collapsed; `lines` keeps the physical line breaks.
* Geometry: `bbox` is the glyph union in PDF user space (y up), per page (`fragments` when a cell
  continues on another page); empty cells get `est_bbox` from their column's x-extent and row's
  y-extent.
* Shading: `page.get_drawings()` filled rects; `fill_rgb` = the last-drawn non-thin rect under the
  cell centre (`fill_bbox` = that rect, i.e. the drawn cell), null when nothing is drawn there.
* Bold: MuPDF char flag bit 8 or a `Bold` font name, all ink glyphs. Underline: a thin (< 1.6 pt)
  filled rect whose centre lies 0.10 em above .. 0.22 em below the baseline *in the glyph's writing
  frame* (so rotated cells work) and which does not overhang the text line's ink by more than 0.3 em
  (cell borders are thin too but span the whole cell, and for rotated cells the right border sits
  exactly where an underline would).
* Header rows: leading all-`/TH` rows (Core); else the first row if it is re-stated at the top of a
  later page (`repeat`: Opal 22, 38, 25 ...); else the first row if at least half its inked cells are
  bold (`bold-first-row`: Opal 24 etc.). Rows equal to the header that start a new page are removed
  from `rows` and listed in `repeated_header_rows`.
* Captions: nearest sibling (3 before, 1 after) whose text starts `Table N`; for a Word list (`/L`)
  each `/LI` is tried last-first (Core Table 239's caption is the last item of the list that
  precedes it). Core captions are `/L -> /LI`, Opal `/P`.

## 3. Tables whose cell recovery needed repair (all repairs are marked in the JSON `warnings` / cell keys)

PDFMaker's tagging is wrong in a handful of places; three geometry-based passes fix them:

1. **Mis-tagged merged cells** (`split_from_merged: true`): Core Tables **71, 72, 73, 81** had Name and
   Format in a single `/TD`; Core **32, 33, 36** had FIELD and VALUE in one `/TD`. A cell is split
   only when its glyphs partition cleanly into header column bands *and* a drawn vertical rule
   separates the parts; genuinely merged cells in the byte-layout tables (4, 18, 20, 21, 26, 31,
   37-44, 212, 238; Opal 2-8, 10, 17) are left alone and reported as `ragged rows`.
2. **Stray fragment** (`moved_out` / `adopted_from`): Core Table **70**'s Format cell owned Table
   **71**'s continuation lines `bytes, 64` (second page). The fragment is moved to the cell whose
   drawn grid cell (pymupdf `find_tables(strategy="lines_strict")`) contains it.
3. **Dangling struct elements** (`recovered_by_geometry: true` / `adopted_orphan`): the body rows of
   Core Tables **189** (13 rows), **190** (5 rows) and **191** (14 rows) hang from `/TR`s whose parent
   does not list them in its `/K`, so they are unreachable from the root (MuPDF reports `idx=-1`;
   the struct tables had only their header row). Rows are rebuilt from the drawn grid, glyphs
   assigned by containment. Opal Table **5** lost one `(MSB)` glyph run the same way; it was adopted
   into the existing cell.

Things that still look odd and are left as extracted:

* Narrow Opal cells wrap words without hyphens: `Obje ct` (Kind column of Tables 20/36),
  `DataRemovalMecha nism"`, `C_PIN_Admin 1`. `lines` shows the break; `text` joins with a space.
  A consumer should rejoin from `lines` with knowledge of the column.
* Footnote superscripts are glued to the cell text (`00 00 00 09 00 00 02 00 (+XX)1 (O)`; the `1`
  is a 7 pt superscript).
* Opal mixes curly and straight quotes in names (`"Table"` curly vs `"MethodID"` straight);
  preserved as printed.
* Core numbered tables with no detectable header row: 6, 8, 11, 13, 17, 24 (token/packet layouts
  with no `/TH` and no bold first row); their first row is in `rows`.
* Core Table 7 part 1/2 has two header rows and no data rows (the data row is in part 2/2).
* Neither PDF carries `/ColSpan`/`/RowSpan` attributes, so column alignment inside ragged rows must
  come from `bbox`/`est_bbox` (provided per cell).
* MuPDF's own `TEXT_COLLECT_STYLES` underline flag never fired on these PDFs; underline comes solely
  from the rect test above.

## 4. Page-spanning tables and repeated headers

The struct tree stitches all page-spanning tables (Opal Table 38: 23 pages, 96 data rows, 22
repeated header rows removed; Table 22: 8 pages, 45 rows, 7 repeats; Core Table 87 gen_status and
Table 83 feedback_size each drop one repeated header). Core tags the repeated row as `/TH`, so the
repeat detection also runs over leading `/TH` rows (otherwise Table 47 would have shown two header
rows). A repeated header is recognised as a row whose cell texts equal the header's and which starts
on a different page than the previous row; it is kept in `repeated_header_rows` with its page.
Cells are never merged across pages except the one legitimate case handled in 3.2; after the
passes no cell has `fragments` on two pages. The earlier belief that Core 188/200/202/231 have
repeated captions is wrong: each is one struct table with one caption (231 has a repeated header).

## 5. Shading / bold / underline on Opal Table 24 (Authority) and Table 38 (AccessControl)

* **Table 24** (p48, 1h+6x19, header via `bold-first-row`): header cells bold on grey 193; UID /
  value cells on 231 (legend RGB 230: read-only Opal-specified); Name cells on an explicit white
  rect (255); `Enabled` column: Makers/Admin1/AdminXX on 180-181 (legend RGB 179: writable, fixed
  access control) and SID on 218; the empty CommonName/ClockStart..LogTo cells have no drawn rect
  (`fill_rgb` null = unshaded). No cell is bold or underlined in the body; an earlier version of the
  underline test flagged the rotated `ResponseSign` column (its right-hand border sits where an
  underline would), which the length criterion now rejects. Verified against a 170 dpi render.
* **Table 38** (p56-78): header via `repeat` (first row partly bold: `Table Association` is plain);
  fills 231 (UID, MethodID, ACL, GetACLACL columns), 255 (Name, InvokingID Name) and null elsewhere;
  16 bold cells = the bold-italic group labels (`SP`, `Table`, `SPInfo`, ..., `DataStore`); no
  underlines (the 126 the first run reported were all cell borders). Verified on p57 render.
* **Table 25** (C_PIN) is the positive control: `VU` plain-underlined on 180, `VU`/`MSID`/`0`
  bold-underlined on 231/218, exactly as rendered. Across Opal, 49 cells are underlined (Tables 41,
  25, 36, 45, 47, 48, 28, 40, 42, 46), all bold-underline or plain-underline VU/0/F values; Core
  has none, as expected.
* Colours are MuPDF's sRGB rendering of the ICC-based fills, so the legend's 230 appears as 231 and
  179 as 180/181; 193 is the header grey and 218 (not in the legend) appears on a few cells (Table
  24 SID/Enabled, Table 25 Admin rows). Threshold accordingly.

## 6. Timing

Core: ~10 s (306 pages, 263 tables); Opal: ~6 s. Pages are parsed lazily (XML text page, drawings)
only where tables live; `find_tables` runs only on pages that need the reconciliation passes.
`regen_typeuids.py`: < 1 s.

## 7. Rerun

```sh
cd /private/tmp/claude-501/-Users-scott-hsed/f05c341b-63d7-4ac7-8e79-f36321a58c77/scratchpad/extract
~/hsed/.venv/bin/python -I extract_tables.py /Users/scott/hsed/tcg/TCG_Storage_Architecture_Core_Spec_v2.01_r1.00.pdf core_tables.json core_index.txt --page-offset 17
~/hsed/.venv/bin/python -I extract_tables.py /Users/scott/hsed/tcg/TCG_Storage-Opal_SSC_v2.30.pdf opal_tables.json opal_index.txt --page-offset 1
~/hsed/.venv/bin/python -I regen_typeuids.py core_tables.json /Users/scott/hsed/src/System/SED/MCTP/Common/TypeUIDs.hs regen_report.txt regen_typeuids.json
```
(`--pages 100-101` restricts a run to tables touching those 1-based PDF pages, for debugging.)

## 8. Files

* `extract_tables.py` - the extractor (pikepdf + pymupdf; comprehension-style, mutation confined to
  the in-place reconciliation of cell records).
* `core_tables.json`, `opal_tables.json` - `{"meta": ..., "tables": [...]}`; each table:
  `table_number, caption, caption_path, struct_path, nested_in, part/parts, pages (0-based),
  printed_pages, n_rows, n_cols, header_source, header_rows, repeated_header_rows, rows, warnings`;
  each cell: `kind, text, lines, page, bbox [x0,y0,x1,y1] (PDF user space), fragments?, est_bbox?,
  bold, underline, fonts, color?, fill_rgb, fill_bbox, split_from_merged?, recovered_by_geometry?,
  adopted_from?/moved_out?/adopted_orphan?, nested_tables?`.
* `core_index.txt`, `opal_index.txt` - one line per table: number(.part), PDF pages, printed pages,
  `<header rows>h+<data rows>x<cols>`, caption.
* `regen_typeuids.py`, `regen_report.txt`, `regen_typeuids.json` - the hsed comparison (section 9).
* `probe/` - the throwaway probe scripts and renders used while developing (not deliverables).

## 9. Regeneration vs hsed (summary; full detail in `regen_report.txt`)

100 Type tables -> 100 `[ttype|` blocks and 19 of the 20 enum/set tables -> 19 `[tenum|` blocks pair
up in document order (Table 87 gen_status is hsed's commented-out FIXME; its 12 rows, 3 columns and
`7 to 31` range are in the regenerated data). 27 differences, **0 unexplained, 0 extraction
problems**: 7 hsed transcription errors (UIDs of Tables 83, 85, 86 = date's `18 04`, Table 129
Seconds = zeros, captions 157->147 and 161->160, Table 82 caption `Enumes`), 10 deliberate hsed
fixes (Fractoin, Eror, six `_object _ref` captions, headers of Tables 61 and 128), 10 whitespace-only
(`uidref {X}` vs `uidref{X}`, `General_Reference_Type {7}`, one header and one label in Table 98).
