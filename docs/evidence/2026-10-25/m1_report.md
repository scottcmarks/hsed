# M1 prototype: annotate -> extract -> generate -> typecheck, end to end, on three Core Spec tables

Everything lives in `/private/tmp/claude-501/-Users-scott-hsed/f05c341b-63d7-4ac7-8e79-f36321a58c77/scratchpad/m1/`
(below: `$M1`). Nothing under `~/hsed` was written (checked with a stamp file and `git status`, see 3);
the original PDF is unchanged (md5 `5ceb93ab118a578125e3755eb885b668` before and after everything);
`/dev/disk6` was not touched. `$M1/run_all.sh` re-runs the whole round trip.

## 0. Result in one paragraph

Three hidden `/Text` annotations (`hsed: typeRow`, `hsed: enumValues`, `hsed: tableUIDs`) were added
to a copy of Core 2.01 by a pypdf incremental update (+1,425 bytes; the original is an exact byte
prefix; the writer is deterministic, two runs gave the same md5). All four affected pages render
pixel-identically under Ghostscript, MuPDF and Apple CoreGraphics. A compiler that reads only the
annotated PDF found the three annotations, re-anchored each to its table by the struct-tree path in
a fresh extractor run over the annotated copy (whose 263 table records are deep-equal to the run
over the original), checked each table's shape against its directive and generated one Haskell
module in hsed's own shapes: `uAC_elementType`/`fAC_elementType`, `data Core_adv_key_mode` (8
constructors), and for all 46 rows of Table 240 `u<T>TableObject`/`u<T>Table`/`h<T>` plus the
`nameHalfUID`/`nameUID` maps. The module typechecks against the real hsed library (`ghc -fno-code`
via `stack exec`), and a compiled check program proves every generated value equal to what hsed's
quasiquoters produce today (144 checks, 0 failures); a normalised textual diff against the
`-ddump-splices` transcripts shows 285 declarations on each side, all equal. Seven re-saves of the
annotated copy through pymupdf, pikepdf and pypdf (incremental and full rewrites) all preserved the
three annotations, `/F 6` and `/HSED`; no writer dropped anything.

## 1. What was built (files)

| file | role |
|---|---|
| `$M1/hsed_annot.py` | THE one place that knows the payload: `PREFIX = "hsed:"`, the three directive names `TYPE_ROW="typeRow"`, `ENUM_VALUES="enumValues"`, `TABLE_UIDS="tableUIDs"`, `PRIVATE_KEY="/HSED"`, `FLAGS=6`, `VERSION=1`; `HsedAnnot` dataclass; `format_contents`/`parse_contents`, `format_path`/`parse_path`; adapters `to_pypdf` (writer), `from_pikepdf`/`from_pypdf`/`find_pikepdf` (readers). Renaming anything is a one-file change. |
| `$M1/annotate.py` | original PDF + `core_tables.json` -> `core_annotated.pdf` (pypdf `PdfWriter(orig, incremental=True)`); Rect = union of the extractor's cell boxes on the table's first page; then verification: prefix, delta, pikepdf read-back, gs/MuPDF/sips renders + pixel diff. Report: `annotate_report.json`. |
| `$M1/compile.py` | annotated PDF -> finds hsed annotations (pikepdf) -> runs `../extract/extract_tables.py` on the annotated copy (subprocess, unmodified extractor) -> locates each table by `/HSED /Path`, cross-checks caption and pages -> shape checks per directive -> `Hsed/Generated/CoreM1.hs` and `Equiv.hs`. Report: `compile_report.json`. `--discover-only` for probes, `--compare-json` to deep-compare extractor runs. |
| `$M1/Hsed/Generated/CoreM1.hs` | the generated module (642 lines, 142 value definitions + 1 data declaration, provenance comment on every definition). |
| `$M1/Equiv.hs` (generated) | runtime equivalence program: imports `Hsed.Generated.CoreM1`, `System.SED.MCTP.Common.TypeUIDs`, `System.SED.MCTP.Common.TableUIDs` and compares value by value. |
| `$M1/equiv_check.py` | textual equivalence against `dev/Column_Types.txt` and `dev/TableUIDs.txt`. Output `equiv_check.json`. |
| `$M1/probe.py` | robustness: 7 re-save variants, each recompiled and re-rendered. Output `probe/probe_report.json`, `probe/<writer>.pdf`, `probe/<writer>/...`. |
| `$M1/render/` | the gs/sips PNGs and single-page PDFs behind the md5s below. |
| `$M1/annotated_tables.json`, `annotated_index.txt` | the extractor's fresh run over the annotated copy. |
| `$M1/ghc-out/` | ghc outputs (`typecheck.log`, `equiv.log`, `equiv.run.log`, the `equiv` binary). |

Tool versions: macOS 27.0.1; Python 3.15.0 (`~/hsed/.venv`, run with `-I`): pypdf 6.21.2, pikepdf 10.17.0,
PyMuPDF 1.28.2 (MuPDF 1.28.2), Pillow 13.0.0; Ghostscript 10.08.0 (`/opt/local/bin/gs`); `sips-316`
(CoreGraphics); stack 3.11.1; GHC 9.4.7 (ghcup); hsed-0.1.0.1 registered in
`~/hsed/.stack-work/install/aarch64-osx/62c296cc.../9.4.7/pkgdb` by the earlier `stack build`.

## 2. Step 1: annotate.py, evidence

Command: `cd $M1 && ~/hsed/.venv/bin/python -I annotate.py ~/hsed/tcg/TCG_Storage_Architecture_Core_Spec_v2.01_r1.00.pdf ../extract/core_tables.json .`
(6.2 s including all renders; exit 0; no warnings.)

| | bytes | md5 |
|---|---|---|
| original | 2,401,864 | `5ceb93ab118a578125e3755eb885b668` |
| `core_annotated.pdf` | 2,403,289 | `fa5b4dd61062ce5c79a1b557b6c00906` |
| delta | **+1,425** | original is an exact byte prefix: **true**; second run byte-identical |

Pages (1-based PDF page = printed + 17; the extractor and `/HSED /Pages` use 0-based indices):

| table | directive | PDF page (0-based) | printed | struct path | Rect (PDF user space) | page dict before |
|---|---|---|---|---|---|---|
| Table 46 AC_element | `typeRow` | 100 (99) | 83 | `0/1288` | `[181.44 264.72 457.56 319.26]` | no `/Annots` |
| Table 52 adv_key_mode Enumeration Values | `enumValues` | 102 (101) | 85 | `0/1318` | `[221.04 639.24 417.97 706.74]` | `/Annots` = 3 `/Link` (obj 1175) |
| Table 240 Table UIDs | `tableUIDs` | 302-303 (301, 302) | 285-286 | `0/5357` | `[131.52 83.64 507.36 542.58]` (page 302 part) | `/Annots` = 1 `/Link` (obj 1285) |

Render identity for every listed page (gs `-sDEVICE=png16m -r100`, 850x1100 px; MuPDF `get_pixmap(dpi=100, annots=True)`;
CoreGraphics = `sips -s format png` on a single-page extraction made identically from original and annotated copy with pikepdf):

| PDF page | gs md5 (orig = annotated) | gs differing pixels | MuPDF identical | sips md5 (orig = annotated) |
|---|---|---|---|---|
| 100 | `22362afca08e30c4cbcb645dab487a49` | 0 | yes | `72d97df3a886883ace1ac9a229a0eb78` |
| 102 | `34ca48aafc6da2ef4d9489f1b17b308d` | 0 | yes | `73aa63b1aed30a0300f5527b3bfb9596` |
| 302 | `093e9c1a394aa310cdcb2c1859f2279d` | 0 | yes | `8becef79b614d1198bae5a8e60e71580` |
| 303 | `783d3bc19fb56c1d958e01c2d4be5952` | 0 | yes | `646153224fc1801d6a58f858b2522488` |

(Page 100's gs md5 equals the one from the earlier feasibility run, so the baseline is stable too.)

Read-back with pikepdf: 3 hsed annotations, `/F 6`, `/HSED` intact, `/Annots` on the touched pages now
`['/Text']`, `['/Link','/Link','/Link','/Text']`, `['/Link','/Text']`; object count 29,891 -> 29,895;
pikepdf no longer reports the file as linearized (the `/Linearized` hint is stale after any incremental update).

### The payload, byte for byte

The whole appended section (1,425 bytes) is: page 100's dictionary rewritten with `/Annots [ 29892 0 R ]`
(325 bytes), the two existing `/Annots` arrays rewritten with one more reference (59 and 41 bytes), the three
annotation objects (219, 247, 223 bytes), an xref stream (288 bytes) and `startxref`/`%%EOF`. The three
annotation objects exactly as written by pypdf (object number, then the dictionary):

```
29892 0 obj
<<
/Type /Annot
/Subtype /Text
/Rect [ 181.44 264.72 457.56 319.26 ]
/Contents (hsed\072 typeRow)
/F 6
/HSED <<
/V 1
/Path (0\0571288)
/Caption (Table 46 AC\137element)
/Pages [ 99 ]
>>
/P 278 0 R
>>
endobj
29893 0 obj
<<
/Type /Annot
/Subtype /Text
/Rect [ 221.04 639.24 417.97 706.74 ]
/Contents (hsed\072 enumValues)
/F 6
/HSED <<
/V 1
/Path (0\0571318)
/Caption (Table 52 adv\137key\137mode Enumeration Values)
/Pages [ 101 ]
>>
/P 283 0 R
>>
endobj
29894 0 obj
<<
/Type /Annot
/Subtype /Text
/Rect [ 131.52 83.64 507.36 542.58 ]
/Contents (hsed\072 tableUIDs)
/F 6
/HSED <<
/V 1
/Path (0\0575357)
/Caption (Table 240 Table UIDs)
/Pages [ 301 302 ]
>>
/P 794 0 R
>>
endobj
```

`\072` `\057` `\137` are pypdf's octal escapes for `:` `/` `_` (it escapes every non-alphanumeric in a
literal string); decoded, `/Contents` is exactly `hsed: typeRow` (13 bytes), `hsed: enumValues` (16),
`hsed: tableUIDs` (15), and all three readers (pikepdf, pypdf, pymupdf) decode them identically.
The xref stream: `/Index [ 278 1 1175 1 1285 1 29892 3 ]`, `/Prev 116`, both `/ID` strings unchanged.

### The smallest the payload could be

* The information content is one name per table. `typeRow` is 7 bytes, `enumValues` 10, `tableUIDs` 9;
  the `hsed: ` marker costs 6 more. The marker is needed only if the annotation is recognised by its
  `/Contents`; if recognition is by the presence of `/HSED` (or a `/T (hsed)` author), `/Contents (typeRow)`
  suffices.
* Serialised compactly (no `/Type /Annot`, which is optional; no newlines; no `/P`, which is optional), the
  same annotation dictionaries are 144 / 169 / 151 bytes; without `/HSED` (anchor by page + `/Rect` only)
  79 / 82 / 80 bytes. `/HSED` itself could shrink to `<</V 1/Path(0/1288)>>` (22 bytes): `/Caption` and
  `/Pages` are derivable from the struct element and are only redundancy for re-anchoring if a writer ever
  renumbers the tree. So the floor is roughly 60-80 bytes per table with a struct-path anchor.
* The fixed cost of an incremental update dwarfs that: an xref stream (288 bytes here) plus a rewritten page
  dictionary (325 bytes) for every page that had no `/Annots`, or a rewritten `/Annots` array (about 40-60
  bytes) for pages that had one. Annotating all 248 Core tables in a single update would amortise the xref
  to one stream (about 1.5-3 KB) and cost about 100-350 bytes per page touched.

## 3. Step 2: compile.py, evidence

Command: `cd $M1 && ~/hsed/.venv/bin/python -I compile.py core_annotated.pdf . --compare-json ../extract/core_tables.json`
(8.9 s, of which 8.3 s is the unmodified extractor on the annotated copy; exit 0.)

* Discovery (pikepdf, from the annotated PDF only): 3 annotations, 0 problems (`/F` is 6, `/HSED` present,
  directive known).
* Extractor on the annotated copy: 263 tables, 249 numbered, 248 distinct, 13 nested (same as the original),
  and the `tables` list is **deep-equal** to `../extract/core_tables.json` (every one of the 263 records,
  all cells, boxes, pages). The struct tree, text layer, drawings and the extractor's repairs all survived
  the incremental update.
* Location by `/HSED /Path`: `0/1288` -> exactly one table, caption `Table 46 AC_element`, pages `[99]`;
  `0/1318` -> `Table 52 adv_key_mode Enumeration Values`, `[101]`; `0/5357` -> `Table 240 Table UIDs`,
  `[301, 302]`. Caption and pages in `/HSED` agree with the tree (both checked).
* Shape checks: `typeRow` header `[UID, Name, Format]`, 1 row, UID is 8 hex bytes; `enumValues` 2 columns
  whose first header contains `Value` (`Enumeration Value`, `Behavior`), 3 rows, every value cell an int or
  `int-int` range; `tableUIDs` header `[UID of Table Descriptor Object, UID of Table, Table Name, Template]`,
  46 rows x 4 cells, both UIDs hex, and for every row the lower half of the descriptor-object UID equals the
  upper half of the table UID (hsed's `mkUIDRow` invariant). All pass.
* Generated: `Hsed/Generated/CoreM1.hs`, 642 lines; 142 value definitions (2 + 46x3 + 2 maps), 1 data
  declaration. Shapes copied from hsed: `u<Name>Type :: UID` via `uid b0..b7`, `f<Name>Type :: String`
  (Format = hsed's concatenation of the trimmed physical lines: `"List_Type,*,ACE_expression"`);
  `data Core_adv_key_mode = Core_adv_key_mode_Wait_for_AdvKey_Req | ..._Auto_advance_keys | ..._Reserved |
  ..._Reserved_3 .. ..._Reserved_7 deriving (Bounded, Enum, Eq, Ord, Show)` (a port of `dEnum`: label
  scrub, `_NA` gap fill, `_<v>` suffix for a label's later values, Map key-order consolidation, value sort);
  `u<T>TableObject`, `u<T>Table :: UID`, `h<T> :: HalfUID` for the 46 rows plus `nameHalfUID :: Map HalfUID
  String` and `nameUID :: Map UID String`. Every definition carries
  `-- | <caption>; <pdf> PDF page <n> (printed <p>); struct path <path>; <row/column detail>`.

Excerpt:

```haskell
-- | Table 46 AC_element; core_annotated.pdf PDF page 100 (printed 83); struct path 0/1288; columns UID, Name, Format
uAC_elementType :: UID
uAC_elementType = uid 0x00 0x00 0x00 0x05 0x00 0x00 0x08 0x01

-- | Table 52 adv_key_mode Enumeration Values; core_annotated.pdf PDF page 102 (printed 85); struct path 0/1318; 3 rows, values 0..7
data Core_adv_key_mode
    = Core_adv_key_mode_Wait_for_AdvKey_Req    -- 0
    | Core_adv_key_mode_Auto_advance_keys      -- 1
    | Core_adv_key_mode_Reserved               -- 2
    | Core_adv_key_mode_Reserved_3             -- 3
    ...
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | Table 240 Table UIDs; core_annotated.pdf PDF page 303 (printed 286); struct path 0/5357; row 33/46 SP (Admin)
hSP :: HalfUID
hSP = halfUID 0x00 0x00 0x02 0x05
```

## 4. Step 3: typecheck against the real hsed library

The command that succeeds (hsed is already registered in the stack local package db, so no `-isrc` and no
TH recompilation is needed; `-fno-code` writes nothing; package.yaml has no `default-extensions`, and the
module uses the implicit Prelude, so no `-X` flags are needed):

```
cd /Users/scott/hsed && stack exec -- ghc -fno-code -Wall -package hsed -package containers \
    -outputdir $M1/ghc-out/typecheck $M1/Hsed/Generated/CoreM1.hs
```

Output tail (`$M1/ghc-out/typecheck.log`, exit 0, 0.7 s wall, no warnings):

```
[1 of 1] Compiling Hsed.Generated.CoreM1 ( /private/tmp/claude-501/-Users-scott-hsed/f05c341b-63d7-4ac7-8e79-f36321a58c77/scratchpad/m1/Hsed/Generated/CoreM1.hs, nothing )
```

No write under `~/hsed`: `find /Users/scott/hsed -newer $M1/ghc-out/stamp.before -not -path '*/.venv/*'`
is empty, `git -C ~/hsed status --short | wc -l` is 10 before and after (the pre-existing modified files),
and `.stack-work/stack.sqlite3` keeps its 17:56:19 mtime through every `stack exec`/`stack path` call.
(`stack exec -- ghc-pkg field hsed exposed-modules` lists 15 `System.SED.*` modules, including `UID`,
`TypeUIDs`, `TableUIDs`.)

## 5. Step 4: equivalence with hsed's own quasiquoters

**Runtime (strongest):** `Equiv.hs` imports the generated module as `G` and hsed's `TypeUIDs` as `H`,
`TableUIDs` as `T`, and compares `G.uAC_elementType == H.uAC_elementType`, `G.fAC_elementType ==
H.fAC_elementType`, constructor names and `fromEnum` sequences of `[minBound..maxBound]` for both
`Core_adv_key_mode` types, `G.nameUID == T.nameUID`, `G.nameHalfUID == T.nameHalfUID`, and each of the 138
Table 240 values by name.

```
cd /Users/scott/hsed && stack exec -- ghc -O0 -Wall -package hsed -package containers -i$M1 \
    -outputdir $M1/ghc-out/equiv -o $M1/ghc-out/equiv/equiv $M1/Equiv.hs   # 3.5 s, exit 0, no warnings
$M1/ghc-out/equiv/equiv
144 checks, 0 failures
uAC_elementType = uid 0x00 0x00 0x00 0x05 0x00 0x00 0x08 0x01; fAC_elementType = "List_Type,*,ACE_expression"
Core_adv_key_mode = [Core_adv_key_mode_Wait_for_AdvKey_Req,Core_adv_key_mode_Auto_advance_keys,Core_adv_key_mode_Reserved,Core_adv_key_mode_Reserved_3,Core_adv_key_mode_Reserved_4,Core_adv_key_mode_Reserved_5,Core_adv_key_mode_Reserved_6,Core_adv_key_mode_Reserved_7]
```

**Textual (against the `-ddump-splices` transcripts):** `equiv_check.py` picks the two splice blocks of
`dev/Column_Types.txt` whose quoted table is Table 46 / Table 52 (of 119 blocks) and the one t240 splice
of `dev/TableUIDs.txt`, groups declarations, strips module qualification (`System.SED.MCTP.Common.UID.uid`,
`GHC.Base.String`, `Data.Map.Internal.fromList`), evaluates `uid`/`halfUID` byte lists (decimal in the dumps,
hex in the module), ignores comments and order, and compares name -> value:

```
{"splice_blocks_in_Column_Types": 119, "blocks_picked": 2, "n_mine": 285, "n_theirs": 285,
 "n_common_equal": 285, "only_in_generated": [], "only_in_hsed_splices": [], "differing": {}}
```

The only differences are formatting: hex vs decimal literals, qualified names, the t240 splice emits rows in
reverse document order (its `foldr`), and my provenance comments. None of the known hsed transcription
errors touch these three tables, as expected.

## 6. Step 5: robustness under other writers

`probe.py` re-saves `core_annotated.pdf` seven ways, recompiles each result with `compile.py` (discovery
and the full extractor re-anchoring), and re-renders the three anchor pages with gs:

| writer | found | `/F` | `/HSED` kept | extra keys added | compiled | module identical* | gs renders identical | size delta vs annotated | prefix kept |
|---|---|---|---|---|---|---|---|---|---|
| pymupdf `save(incremental=True)` no edit | 3 | 6 | yes | none | 3/3 | yes | yes | 0 (MuPDF writes nothing) | yes |
| pymupdf incremental + metadata edit | 3 | 6 | yes | none | 3/3 | yes | yes | +426 | yes |
| pymupdf full `save()` | 3 | 6 | yes | none | 3/3 | yes | yes | +2,457,636 | no |
| pymupdf full `garbage=4, clean, deflate` | 3 | 6 | yes | none | 3/3 | yes | yes | +2,372,705 | no |
| pikepdf full rewrite (defaults) | 3 | 6 | yes | none | 3/3 | yes | yes | +37,997 | no |
| pikepdf object streams regenerated + linearize | 3 | 6 | yes | none | 3/3 | yes | yes | +190,486 | no |
| pypdf full rewrite (`PdfWriter(clone_from=)`) | 3 | 6 | yes | none | 3/3 | yes | yes | +2,732,476 | no |

\* identical after masking the compiled file's name and md5 in the provenance comments.

No writer dropped `/HSED` or the hidden flag, none added `/Popup`/`/AP`/`/NM`/`/C` (those appear only when
pymupdf *creates* an annotation, as in the feasibility run), and every re-saved file still re-anchors by
struct path with matching caption and pages, i.e. none of them renumbered the structure tree. Noteworthy:
pymupdf's and pypdf's full rewrites double the file size (they expand object streams / recompress), so
re-saves through those should stay incremental.

## 7. Problems found and observations

1. **pypdf escapes punctuation** in literal strings (`\072` `\057` `\137` for `:` `/` `_`): harmless and
   decoded correctly everywhere, but 2 extra bytes per character and ugly in a hex dump. A hand-rolled or
   Haskell (pdf-toolbox) writer would emit `(hsed: typeRow)`.
2. **Linearization hint goes stale**: after the incremental update pikepdf reports `is_linearized = False`
   (the `/Linearized` dictionary's lengths no longer match). Viewers fall back to a normal load; harmless
   for a 2.4 MB file, but "exact prefix" and "linearized" are in tension: re-linearizing means a full rewrite.
3. **Three page-numbering systems**: the provisional `/HSED /Pages` are 0-based page indices (as in the
   extractor and the task's example), humans and viewers use 1-based PDF pages, and the printed page is
   PDF page - 17. The grammar should state its convention, or drop `/Pages` (derivable from the struct
   element's marked-content `/Pg`s). The generated comments print 1-based PDF page and printed page.
4. **Struct path is a positional address**: `/K` indices renumber if a writer re-tags or prunes the tree.
   None of the seven writers did, but Acrobat's tag editors could. `/Caption` (unique per page) is the
   fallback the compiler already cross-checks; `/Pages` is a second check.
5. **`dev/TableUIDs.txt` is stale** relative to the working-tree `TableUIDs/TH.hs`, which now also emits
   typed `oth<T>` / `bth<T>` half-UIDs (`Object_Table_HalfUID` / `Byte_Table_HalfUID`). M1 generates the
   names the task asked for (the dump's); the runtime check compares against the library as built today
   and still finds every shared name equal.
6. **Format column as a string** is hsed's convention (`"List_Type,*,ACE_expression"` = trimmed physical
   lines concatenated); the real compiler should parse it into `Core_Format` rather than copy text, and the
   10 whitespace-only differences known from the regeneration report (`uidref {X}` vs `uidref{X}`) would
   disappear with a parser.
7. **The directive semantics were ported to Python** for M1 (`enum_constructors` is a transliteration of
   `dEnum`; `typeRow`'s naming and `tableUIDs`' invariant likewise). That is the opposite of the intended
   division of labour: in M2 the directive name should resolve to a Haskell definition that receives the
   table (rows of cells) and produces the declarations, so the PDF says `typeRow` and Haskell says what that
   means. The Python side should shrink to the extractor plus a thin "hand the cells to Haskell" shim (JSON
   or a TH `runIO`), or the compiler itself should be Haskell (pdf-toolbox-core can read the struct tree).
8. **compile.py runs the whole extractor** (8-10 s, 306 pages) to re-anchor three tables; a path-directed
   walk of the three `/Table` elements would be instant. Fine for M1, not for an edit-compile loop.
9. **`python -W error` segfaults** the annotate run (a deprecation warning raised as an exception inside a C
   extension at an unlucky point); without `-W error` the run is clean and warning-free after replacing the
   deprecated `Image.getdata` pixel count. Environmental, noted so nobody chases it later.
10. **Not covered**: Preview.app's GUI save and Acrobat were not driven (sips exercises the same
    CoreGraphics PDF engine Preview renders with, but Preview's annotation sidebar behaviour for
    hidden `/Text` annotations and what its "Save" rewrites remain untested); the Opal PDF was not annotated
    (same machinery; its tables use `/P` captions, which the extractor already handles).

## 8. What the three directives would be in Haskell (the point of "say a lot with a little")

The PDF carries a name and an address; nothing else. The meaning sits in hsed, e.g.

```haskell
typeRow    :: Table -> Q [Dec]   -- expects header [UID, Name, Format]; u<Name>Type, f<Name>Type (later: parsed Core_Format)
enumValues :: Table -> Q [Dec]   -- expects [<x> Value, <label>]; data Core_<name> with _NA fill, fromEnum = spec value
tableUIDs  :: Table -> Q [Dec]   -- expects Table 240's four columns; u<T>Table, u<T>TableObject, h<T>, oth/bth<T>, name maps
```

where `Table` is the extractor's record (caption, struct path, pages, header rows, rows of cells with text
and lines). `hsed_annot.py` already treats the directive purely as a name; `compile.py`'s `GENERATORS`
table is the stand-in for that resolution.
