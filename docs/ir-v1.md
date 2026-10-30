# IR v1: the seam between the PDF extractor and `hsedc`

`tools/extract/extract_tables.py` reads a (pristine or annotated) TCG PDF and writes one JSON
file; `hsedc` reads nothing else. Committed instances live under `spec/ir/`. The shape is frozen
at `irVersion` 1; additive fields bump nothing, a changed meaning bumps the version. M2b filled in
`sigs` (an empty list before, with no schema) and added `lot` and `xrefs`: additive, so the version
stays 1, and `hsedc` reads a file written before them with the three lists empty.
Design: `docs/spec-as-source.md` §4.2 and §5.1. Page fields are 0-based PDF indices. The printed page label
follows from **1-based PDF page = printed page + labelOffset** (Core 17, Opal 1).

```jsonc
{
  "irVersion": 1,
  "spec": "core-2.01",                      // SpecId: core-2.01 | opal-2.30 | ...
  "source": "TCG_Storage_Architecture_Core_Spec_v2.01_r1.00.pdf",
  "pdfMd5": "5ceb93ab118a578125e3755eb885b668",   // md5 of the PRISTINE bytes (an annotated
                                            //   input is pristine + tail; the prefix is hashed)
  "pages": 306,
  "labelOffset": 17,
  "notice": "tcg/TCG-NOTICE.txt",
  "extractor": { "script": "extract_tables.py", "version": 1, "pymupdf": "...", "pikepdf": "..." },
  "wordComplete": [],                       // sections whose paras carry "words" (none at M1)
  "tables":   [ TableIR ],                  // document order, nested tables included
  "headings": [ HeadingIR ],                // document order
  "paras":    [ ParaIR ],                   // document order
  "sigs":     [ SigIR ],                    // method signature blocks, document order (M2b)
  "lot":      [ LotIR ],                    // the List of Tables of the front matter, in order (M2b)
  "xrefs":    [ XrefIR ],                   // every "Table N" in the prose, document order (M2b)
  "annots":   [ AnnotIR ]                   // hsed annotations found in the input PDF
}
```

## TableIR

Today's record (see `docs/evidence/2026-10-25/extract/findings.md` §8) plus three fields:

| field | meaning |
|---|---|
| `id` | anchor id `T<n>` for a numbered table, else null |
| `ordinal` | 1-based position among top-level `/Table` structs in document order (Core T46 = 48; Word's split of Table 7 counts twice) |
| `rects` | `[[page, [x0, y0, x1, y1]], ...]` one per page the table touches, the union of its cell boxes on that page, PDF user space |

`struct_path` is the `/K` index path from `/StructTreeRoot` in the same `"0/1288"` string form the
headings and paras use (since 2026-10-29; it was an int array, which `hsedc` still reads); the
repair records `adopted_from.struct_path` / `moved_out.to_struct_path` use the same form.
Unchanged: `table_number`, `caption`, `caption_path`, `nested_in` (both still int arrays, e.g.
`[0, 274]`), `part`/`parts`, `pages`, `printed_pages`,
`n_rows`, `n_cols`, `header_source` (`leading-th` | `first-of-all-th` | `repeat` |
`bold-first-row` | `none`), `header_rows`, `repeated_header_rows`, `rows`, `warnings`; cells carry
`kind`, `text`, `lines`, `page`, `bbox`, `bold`, `underline`, `fonts`, `fill_rgb`, `fill_bbox` and
the repair flags (`split_from_merged`, `recovered_by_geometry`, `adopted_from`, `moved_out`,
`adopted_orphan`, `ragged_join`).

Ragged-row join (design §5.1 step 5): a data row with fewer cells than the header, all of them
non-empty, is appended to its predecessor in the columns its x-extents overlap and flagged
`ragged_join`; a full-width row is never joined.

## HeadingIR

```jsonc
{ "section": "5.1.3", "title": "Column Types", "level": 3,
  "page": 99, "struct_path": "0/1286", "rect": [x0, y0, x1, y1] }
```
From the `/H1`..`/H6` struct elements; `section` is the leading dotted number of the heading
text (null for unnumbered headings such as "Revision History"); `id` for anchoring is the
section string.

## ParaIR

```jsonc
{ "section": "3.3.10.2.1", "item": "3a", "kind": "LI",
  "page": 81, "struct_path": "0/1012", "rect": [x0, y0, x1, y1],
  "hash": "<sha256 of the normalised words>", "words": null }
```
Every `/P` and `/LI` outside tables. `section` is the nearest preceding heading's section;
`item` is the list label for an `/LI` (nested labels concatenated, `3` then `a` giving `3a`),
null for a `/P`; `hash` is sha256 over the words lower-cased, punctuation stripped, joined by one
space; `words` is the array only for sections listed in `wordComplete`, else null. Anchor id:
`<section>#<item>` for a list item, `<section>#p<k>` for the k-th paragraph (1-based) of a section.

## SigIR

```jsonc
{ "section": "5.3.3.6", "page": 200, "struct_path": "0/3381", "rect": [x0, y0, x1, y1],
  "heads": ["TableUID.Get", "ObjectUID.Get"],
  "text": "TableUID.Get [\nObjectUID.Get [\nCellblock : cell_block ]\n=>\n[ Result : typeOr { ... } ]" }
```
One method signature block (design §5.1 step 7). The PDFs print a signature as consecutive `/P`
paragraphs, one line each: the invocation head(s) `<Invoker>.<Method> [`, the parameters, `=>`, the
result. A block starts at a `/P` matching `^[A-Za-z_]+\.[A-Za-z_]+ *\[` (or at a head printed without
its bracket, Core's `CredentialObjectUID.Sign`, followed by one) and runs over the next `/P` siblings
until the result's brackets close (or, for a block with no `=>`, the parameters'), or until a
paragraph that is not the next `/P` sibling (an empty spacer paragraph, a heading). `heads` are the
block's leading head paragraphs' `Invoker.Method` (two for a table-and-object method); `text` is its
paragraphs as printed, wrapped lines of one paragraph joined by a space, paragraphs by a newline
(brackets, a missing `]` and spacing kept: Core's GetPackage prints no `]` before `=>`). `section` is
the nearest preceding numbered heading's, `page` the 0-based page of the first line, `rect` the union
of the block's lines on that page, `struct_path` the first paragraph's. Every block lies in a
numbered section and no section holds two, so the block's anchor id is `<section>/sig` (an
invariant for every spec: a block before the first numbered heading would be a FAIL, its `section`
null, which hsedc could not read). Core 2.01
has 53 (5.2.2.1, 5.2.3.1-5.2.3.5, 5.3.3.1-5.3.3.18, 5.4.3.1, 5.5.4.1-5.5.4.7, 5.6.4.1-5.6.4.17,
5.8.3.1-5.8.3.4), 52 with `=>` and CloseSession (5.2.3.5), which the TPer sends and which has no
response; Opal 2.30 has 3 (5.1.1 Activate, 5.1.2 Revert, 5.1.3 RevertSP).

## LotIR

```jsonc
{ "number": 185, "title": "C_AES_128/C_AES_256 ResidualData Column Values After Encrypt/Decrypt/EncryptFinalize/DecryptFinalize",
  "printed": 159, "page": 14, "rect": [x0, y0, x1, y1] }
```
One entry of the front matter's List of Tables: every `/TOCI` whose text is `Table N <title> .....
<page>` (a long title wraps onto a second line). `number` (`Table 01` is 1), `title` without the
number, a dash separator (Opal's `Table 1 - …`), the dot leaders and the page, `printed` the page
the entry gives (printed, not PDF), `page` and `rect` the entry's own place. What LOT-VS-BODY
compares with the body captions (`number`, `title`, `printed` against the caption's title and the
printed page of the table's first page). Core 2.01: 248 entries, Tables 1-248 once each; Opal 2.30: 50.

## XrefIR

```jsonc
{ "number": 179, "page": 115, "rect": [x0, y0, x1, y1], "section": "5.1.3.55",
  "sentence": "The enumeration values and their associations defined in Table 179.", "target": 166 }
```
Every `Table N` (`\bTable\s+(\d+)\b`) in a `/P` or `/LI` outside the tables that is not a table's
caption (a reference in a table's cell is not read): `number`, `page` and `rect` of its glyphs, the
paragraph's `section` (as ParaIR's), the `sentence` it is in (a list item's without its label: `a)`,
`•`), and `target`, the 0-based page the PDF's own link over the number goes to (null
when no link covers it). What XREF-RESOLVES reads: the number against the numbered tables, the link
against the table's pages, the sentence and section for what the reference claims. Core 2.01: 108
references (every one with a link, each landing on a page of the table it names); Opal 2.30: 63
(one, Table 1 on PDF 12, with no link).

## AnnotIR

What the extractor found in the input PDF's `/Annots` that carries an `/HSED` dictionary:

```jsonc
{ "kind": "Bind", "spec": "core-2.01", "id": "T75", "struct": "0/1429", "v": 1,
  "page": 106, "rect": [x0, y0, x1, y1], "flags": 6, "contents": "erratum scott [\"Fractoin\" ~> \"Fraction\"]" }
```
`kind` is one of `Doc`, `Rule`, `Bind`, `Law`, `Behaviour`; `contents` is the payload text
exactly as stored (design §3.3 grammar); `struct` is `""` for the `doc` anchor (the structure
tree root). A `/Text` annotation without `/HSED` is not an hsed annotation and is not reported here.

## The annotation itself (what `annotate.py` writes, design §3.1; `tools/extract/hsed_annot.py`)

```
<< /Type /Annot /Subtype /Text /F 6 /Rect [...]
   /Contents (erratum scott ["Fractoin" ~> "Fraction"])
   /HSED << /V 1 /Kind /Bind /Spec (core-2.01) /Id (T75) /Struct (0/1429) >>
   /P <page ref> >>
```
One per anchor, on the page of the anchored element; `/Rect` is the table's first-page rect, the
heading's or paragraph's rect, or for `doc` the top-left inch of page 1 (`[36 720 108 756]`, page
index 0, `/Struct ()`). Strings are written as plain literals (`(0/1429)`, not pypdf's octal
escapes) and the objects in compact syntax (`<</Type/Annot/Subtype/Text/F 6/Rect[...]...>>`), so
the tail stays near 400 bytes per anchor; readers see the same objects either way. The whole set is
ONE incremental section in canonical order (kind order Doc, Rule, Bind, Law, Behaviour, then the
natural order of the id: T9 before T10), so `pristine + spec/annot/<spec>.upd ==
build/<spec>.hsed.pdf` byte for byte and two runs are identical. Alongside, the manifest
`spec/annot/<spec>.manifest.json` records what the pristine PDF implies about each anchor (design
§3.2): `md5`, `pages`, `labelOffset`, `irVersion`, and per anchor `id`, `kind`, `ordinal`,
`struct`, `page` (0-based), `label` (printed; null before the numbering starts), `title` (caption
without its `Table N`, or the heading title), `header` (table header cells, whitespace stripped and
case folded), `hash` (sha256 of the table's cell texts, or the heading's title words / the
paragraph's `hash`) and `rects` (one per page the element touches).

## The sidecar `spec/annot/<spec>.hsa`

One anchor per line, `spec | id | payload`; `#` comments and blank lines are ignored when read:

```
core-2.01 | doc | doc core201
core-2.01 | 5.1.3 | rule typeRow & rule enumValues
core-2.01 | T75 | erratum scott ["Fractoin" ~> "Fraction"]
```
Kind follows from the id: `doc` is Doc, a section number is Rule, `T<n>` is Bind, `<section>#<item>`
is Law; a section or an item whose payload starts with `behaviour`/`fails` is Behaviour (a method
heading or a Fails list, design §3.1); `<section>#p<k>` names the k-th plain paragraph of a
section. What a payload may say depends on its anchor: the doc anchor takes `doc <spec>` (naming
its own spec) first, then rule tokens; a heading takes rule tokens or a `behaviour`; a table takes
a table binding or a bare modifier chain; an item takes `law`, `fails` or `behaviour`. hsedc refuses
anything else (`E-ANCHOR-KIND`), and `annotate.py` refuses what it can tell from the first word
(`hsed_annot.VOCABULARY`). An anchor may carry one annotation. The committed file is the canonical form (lines in canonical
order, single spaces around `|`, no comments): `dump` of the annotated PDF reproduces it byte for
byte, and `annotate` of the file reproduces the `.upd`.
