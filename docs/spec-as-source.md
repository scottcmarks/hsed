# hsed spec-as-source

Design of record for annotating TCG Core 2.01 and Opal SSC 2.30 and compiling the annotated PDFs into a Haskell host jig and reference TPer. Synthesised 2026-10-25 from four candidate designs (fidelity, minimal, jig, delivery) and three judgements; revised the same day against three skeptic reviews (§14). Base: `minimal`.

Evidence: the survey maps and digest under `docs/evidence/2026-10-25/` (`maps/*.txt`, `digest.md`), the tag-tree extraction (`spec/ir/{core-2.01,opal-2.30}.tables.json` produced by `tools/extract/extract_tables.py`; `docs/evidence/2026-10-25/extract/findings.md` and `regen_report.txt`), the M1 round-trip prototype, promoted to `tools/extract/{hsed_annot,annotate,apply,dump,pdf_checks}.py` (its report: `docs/evidence/2026-10-25/m1_report.md`), the skeptics' probes (`docs/evidence/2026-10-25/{classify,decode_di}.py`) and the lts-21.21 scratch build log (`docs/evidence/2026-10-25/lts2121-build.log`). Page numbers are PDF page indices unless marked "printed"; Core printed = PDF - 17, Opal printed = PDF - 1, Test Cases printed = PDF - 1.

## 1 Mandate and constraints

**Program.** Annotate the published PDFs of Core 2.01 and Opal 2.30 with PDF comments so they still read unchanged in an ordinary viewer, and compile the annotated PDFs into a running host (test jig) and simulated drive (reference TPer). Host and drive exchange a completely serialised command/response byte stream over pluggable transports: in-process; a real SED on a USB adapter through the BrightPlaza kext; two machines on the LAN through `~/mh_transport`.

**Hard constraint (Scott, verbatim).** "The pdf comments added to the original pdfs should be minimized and make use of Haskell definitions that allow them to say a lot with a little." Haskell is the target for minimalism and the possibility of proofs. Long-term wish: one source that renders as spec text, simulator and jig by switches.

**Direction of truth.** PDF -> IR -> code, never code -> PDF. hsed's hand transcription (TypeUIDs.hs) carries four wrong UIDs (Seconds all zeros; feedback_size, fraction_enum, gen_status with date's `18 04`) and two mis-numbered captions (157 as 147, 161 as 160); nothing may be bootstrapped from it. Regenerating it from the PDF and comparing at runtime is the first regression test.

**Facts the design rests on** (verified this round; *corrected* marks fixes to the previous draft):

- Both PDFs are tagged (StructTreeRoot: /Table -> /TR -> /TH|/TD -> MCID). Core: 306 pages, 250 top-level /Table plus 13 nested in /TD cells (the Value/Interpretation sub-tables of Tables 5-13), 248 numbered captions, Table 7 split by Word into two struct tables at the page 33/34 break (LoT lists 247; Table 185 missing). Opal: 101 pages, 63 /Table, 50 numbered captions. Captions are unique per page; Core caption = preceding /L->/LI sibling, Opal = preceding /P. No cell carries an /A attribute (no ColSpan/RowSpan); /Table /A /BBox is unreliable.
- *Corrected:* /TH is not a header signal. Core T233, T243, T244, T245 tag every row /TH; T6, T8, T11, T13, T17, T24 have none. PDFMaker also mis-tags merged cells (Core T71, T72, T73, T81 hold Name and Format in one /TD; T32, T33, T36 FIELD and VALUE), lets T70's Format cell own T71's continuation, and leaves the body rows of T189, T190, T191 and one `(MSB)` run of Opal T5 dangling from StructTreeRoot. Three geometry passes in `tools/extract/extract_tables.py` repair all of these (§5.1).
- Hidden /Text annotations with /F 2 or /F 6 render pixel-identically under Ghostscript 10.08, MuPDF and CoreGraphics (Preview); /F 4 draws an icon. pypdf `PdfWriter(incremental=True)` appends about 1,010 bytes per annotation with the original as an exact byte prefix; seven re-saves through pymupdf, pikepdf and pypdf preserve /F 6 and the private dict; three readers recover /Contents and /HSED.
- *Corrected:* Opal Table 1 (p12) gives normative-adjacent meaning to fill and bold-underline, but its "Not Defined" sample is an explicit white rect with black text; there is no dark fill anywhere in the Opal PDF. Fills on preconfiguration cells (pymupdf sRGB): 231 (ICC 0.902, read-only), 180 (0.702, writable), 255 (explicit white under most Name/CommonName cells and all of T36's empty cells), none (T20's empty cells), off-legend 218 (10 cells, T24/T25) and 235 (4). `(N)` is a text marker, not a colour.
- SIIS 1.20 and Opal Family Test Cases v1.02 are NOT tagged. Test Cases §5.2 item 2 and §7.2 item 2 (PDF 29, 76) list "The Locking SP is in Manufactured state" among the common prerequisites of every UCT and ETC case.
- *Corrected (toolchain):* stack 3.11.1 is installed and active, the Hackage index is fetched, and the STAGED, uncommitted hsed tree (stack.yaml `lts-21.25` + `compiler: ghc-9.4.7`, pruned package.yaml, GHC 9.4 edits in eight modules) builds and passes `stack test` (58 specs, 1 pending, 63 doctests). A scratch copy on `resolver: lts-21.21` (exact ghc-9.4.7; 21.22+ pin 9.4.8) with no `compiler:`/`allow-newer` is also green (`docs/evidence/2026-10-25/lts2121-build.log`). lts-21.21 contains websockets, wuss, aeson, base64-bytestring, rio, gdp, singletons 3.0.2, doctest-discover and pdf-toolbox-core 0.1.1 (its Writer exports `writeXRefStream`); "a Haskell PDF path forces LTS 24" is wrong.
- Attached: `/dev/disk6`, Samsung 970 EVO Plus behind a Realtek RTL9210 bridge, 'TPer Interface Type' = SAT, FAT32 volume 'SED NVME', serial `S59BNM0RA15802W` (IORegistry value has trailing spaces). Level 0 decoded from the kext's 436-byte 'DTA Device Info' blob (`docs/evidence/2026-10-25/decode_di.py`): TPer, Locking (LockingEnabled 0: Locking SP Manufactured-Inactive), Geometry, DataStore 0x0202 (9 tables, 0xA00000), Opal V2 0x0203 (base ComID 0x1004, 1 ComID, Initial C_PIN_SID Indicator 0x00, 4 admins, 9 users), Block SID 0x0402, Namespace Locking 0x0403; no SIIS 0x0005, no Data Removal 0x0404. The kext needs root and exclusive open and returns no sense; build 625 publishes the 2024-03 packed-bitfield `DTA_DEVICE_INFO` layout, not today's header.
- sedutil: `DtaDrive::sendCmd` returns only 0x00/0xff; sense and masked status are locals of `DtaScsiDrive::sendCmd` (`DtaScsiDrive.cpp:209-217, 303-334`); `DtaOS::PerformSCSICommand`/`PerformNVMeCommand` (`DtaOS.h:364-370`) take sense, senselen and masked_status out-parameters. sedutil is GPLv3; its submodule points at upstream `develop`, detached today.
- `python/mh_transport.py` is symlinked into the live MHTX backend; the Python hub crashes on a binary frame. SPEC.md L18-21 requires ignoring undecodable frames, missing/unknown types and binary; conformance check 3 sends junk then a ping and expects a pong next; check 7 is skipped unless `--interval-ms`/`--timeout-ms` bring deadTimeout + pingInterval under 10 s. Nothing in `~/mh_transport` changes.
- wapex everyday re-sync is `reset --hard origin/work` + `submodule update --init --force`; `submodule.hsed.update` is unset (default `checkout`), so a re-sync leaves hsed detached and discards its index and working tree. Today: six modified tracked `.claude/**` files, three untracked memory files, an untracked `HUD/`, and `m hsed` (ten staged files). Never `git clean`; stage explicit paths.

**Safety invariant.** No IF-SEND that can mutate TPer state reaches `/dev/disk6` until Scott lifts the gate (§9.3). Two independent mechanisms enforce it, one of them checked by GHC.

## 2 Decisions

Winner per question is the design adopted as base; *corrected* lists what the skeptic round changed. Detail is in the section named.

- **Q1 Annotation language (minimal; grafts fidelity, delivery) -> §3.** One-token or modifier-chain payloads over `Hsed.Spec.DSL`, parsed by `hsedc` and re-checked by GHC under a `{-# LINE 1 "core-2.01.pdf#page=…" #-}` pragma in `hsed-spec:test:bindings`; hidden `/Text`, `/F 6`, `/HSED` identity only, true incremental update; rules anchored by `rule <name>` at a governing heading or on the `doc` annotation, anchors being the only source of scope. *Corrected:* the `Binding s` index is the semantic shape the comment asserts, the extractor's structural form is checked against it by `hsedc check` (`E-SHAPE`), not GHC; ten rules left 79 Core / 15 Opal tables unbound (`docs/evidence/2026-10-25/classify.py`), eighteen now bind 221/248 Core and 45/50 Opal with no comment and `hsedc orphans` is the ledger; `/HSED` redundancy moved to the manifest; the source is the pristine PDF plus the annotation set, the annotated PDF and `.hsa` being equivalent renderings kept equal by CI under one `hsedc gen` target. Anchor key for the synchronous-violation sentence is `core:3.3.10.2.1#3a`; struct ordinals (Core T17=19, T42=44, T46=48, T171=173, T240=242; Opal T17=19, T24=26, T49=53, T50=54) are a manifest redundancy because Core T7 is two struct tables.
- **Q2 Haskell side (minimal; grafts fidelity, delivery) -> §4.** `Binding (s :: Shape)`, `Form`, `accepts`, `Rules`, `Readers`, `Format` (the README's first TODO), `Check` with named checks (C-UID-TABLE, C-UID-UNIQUE, C-UID-RANGE, C-ENUM-RANGE, C-T240-HALVES, C-COL-CONTIG, C-FMT-RESOLVE, C-LAYOUT-TILE, C-LAYOUT-ENDIAN, C-PRECONF-HEADER, C-LEGEND-TOTAL, C-PATCH-UID, C-PATCH-MONOTONE, C-PROP-NAME, C-SIG-METHOD, C-LAW-HASH, C-LAW-BOUND, C-RULE-ANCHORED, C-ACL-REFS, E-SHAPE, E-UNBOUND, E-AMBIGUOUS, LOT-VS-BODY, XREF-RESOLVES); behaviour in hand-written `Hsed.Sim.*`/`Hsed.Law.*` bound by `law`/`behaviour` and checked two-way by a TH splice; errata as switches; provenance Haddocks. *Corrected:* the IR carries the words of every paragraph a reader consumes (`wordComplete`); pins have three sentence schemas and their absence is a finding.
- **Q3 Compile pipeline (minimal; grafts fidelity, delivery, jig) -> §5.** Python extraction (`extract_tables.py` promoted) to frozen IR v1 JSON, committed; ordinary `.hs` under `gen/` from a deterministic printer; Opal as an ordered patch set; runtime regenerate-and-compare with the expected delta enumerated; byte-prefix and render-md5 gates; both round-trip directions; re-anchoring with Lost/Changed as errors. *Corrected:* header cascade, mandatory geometry repairs, shape-conditional ragged-row join; Opal T26/T43/T50 column additions in patch stage 3; the oracle is the runtime equivalence program; byte extents: eight field cells positional, one spans, else x-extents, else `bits`.
- **Q4 Wire contract (minimal/jig) -> §6.** `Request`/`Response`/`IfStatus`/`Event`, bytes at the contract, one command in flight per ComID, `sed.*` JSON frames. *Corrected:* the IF-RECV loop continues while OutstandingData /= 0 and re-issues on MinTransfer whether or not data arrived; `protocol_error` only answers `sed.req`; `transferLength`/`blocks 1`. TPER_RESET and STACK_RESET are IF-SENDs on protocol 0x02.
- **Q5 Transport backends (minimal; grafts jig, delivery) -> §7.** In-process; macOS by direct FFI at `Transport 'ReadOnly` (*corrected:* no `TPerUpdate`/`DTA_DEVICE_INFO`); `libsed` at M6 (*corrected:* shim at the `DtaOS::Perform*Command` boundary, external library behind a cabal flag, GPLv3 declared); Haskell mh_transport client and hub certified by the Dart conformance CLI with `--interval-ms 200 --timeout-ms 600`.
- **Q6 Simulator (minimal; grafts fidelity, delivery) -> §8.** Pure `step`, Opal 2.30 OFS from generated preconfiguration, Table 45, `Tick`, resets as inputs. *Corrected:* IF-SEND outcomes `Prepared | Discarded | SessionAborted` as Laws; static ComIDs `Issued` without sessions; unknown-ComID and unknown-protocol answers per Core §3.3.3; `SimParams.lockingOFS`; `--profile`.
- **Q7 Host jig (jig for discipline, minimal for structure) -> §9.** Test Cases v1.02 IDs; `'ReadOnly` capability + Guard + `--allow-write <serial>` + pre-flights; Pair runner. *Corrected:* per-case prerequisites with printed waivers (otherwise every tier-1 run is skipped on `/dev/disk6` and on the simulator before M5); ETC-21/ETC-07/SPF-02:2 move to M5; `--recover` admits STACK_RESET; descriptor-set check before field diff.
- **Q8 Toolchain and repo (delivery; package split corrected) -> §10.** stack 3.11.1 active; `lts-21.21`; three packages with the generated Bindings in `hsed-spec:test:bindings` (*corrected:* never in the library or `hsedc`; the M1 wrong comment is a negative fixture); explicit module map; `tools/land.sh` per session; `tcg/TCG-NOTICE.txt`; `-flibsed` builds GPLv3.
- **Q9 Milestones (minimal's order, jig's first hardware rung; cut into rungs) -> §11.** M0 green build (met) + codec + CI + hygiene; M1 vertical slice with the package split; M2a-d; M3 wire, in-process TPer, UCT-01/ETC-02/ETC-03/SPF-02:1, IF-RECV-only Level 0 on `/dev/disk6`; M4a-c; M5 Locking SP, resets, LAN; M6 tier 2, libsed, Haskell PDF writer, reanchor.

## 3 Annotation language

### 3.1 Storage

```
<< /Type /Annot /Subtype /Text /F 6 /Rect [176.28 259.38 462.72 318.36]
   /Contents (erratum scott ["Fractoin" ~> "Fraction"])
   /HSED << /V 1 /Spec (core-2.01) /Id (T75) /Struct (0/1429) >> >>
```

- Flags `/F 6`. No `/Popup`, `/NM`, `/AP`, `/C` (pypdf emits a harmless `/Flags 0` beside `/F`).
- `/HSED` is identity only: schema version, spec, anchor id, struct path (`/K` index path, `0/1288` for Core T46). Ordinal, page and label, header, words hash, fragment Rects and caption are derivable from the pristine PDF, so they live in `spec/annot/<spec>.manifest.json` and are recomputed at `hsedc verify`; a table annotation stays near the 150-byte floor the M1 probe measured.
- One annotation per logical table, on its first fragment; the manifest lists fragments (Opal T38 spans PDF 56-78). The struct tree stitches fragments and Word's split tables (Core T7); repeated header rows are dropped by rule.
- Kinds: `/Doc` (one per PDF, also carrying document-wide `rule` tokens), `/Rule` (at a governing heading), `/Bind` (table), `/Law` (sentence or list item), `/Behaviour` (method heading or Fails list). Signature blocks are rule-bound and carry no annotation. The manifest carries `md5`, `pages`, `labelOffset`, `anchors`, `rules`, `wordComplete`, `irVersion`.

### 3.2 Anchor record and re-anchoring

Primary key: `(spec, Id)` where Id is `T<n>`, `<section>#<item>` (`3.3.10.2.1#3a`), `<section>/sig`, or `<section>`. Redundant keys in the manifest: struct ordinal and path, PDF page and printed label, normalised header, words hash, fragment Rects. `hsedc reanchor` matches by caption title (number stripped), then header, then words hash, then ordinal ±k, then geometry; Lost or Changed is an error, never a guess; a changed words hash on a `/Law` anchor blocks the build until the Law's quote is updated.

### 3.3 Grammar

```
payload  ::= binding | modifier+              -- a bare modifier chain applies to the table's rule default
binding  ::= name arg* | modifier binding | binding '&' binding   -- '&' joins independent bindings only (rule a & rule b)
modifier ::= name arg*                        -- erratum scott [...], rangeWords ["to"], cols 0 1, enumInDescription, as @'Shape
arg      ::= name | int | 0xHEX | "text" | (ref) | [arg, ...] | name ~> name
ref      ::= core int | opal int | § "dotted"
```

Names are the smart constructors of `Hsed.Spec.DSL` (§4.2). `hsedc bind` parses each payload with `Hsed.Spec.Grammar` (printer round-trip tested) and prints the same AST into `hsed-spec/bindings/Hsed/Bindings/<Spec>.hs`:

```haskell
{-# LINE 1 "core-2.01.pdf#page=107&T75" #-}
b_core_T75 :: Bound
b_core_T75 = at (anchor @'TypeRow "core-2.01" "T75") $ erratum scott ["Fractoin" ~> "Fraction"] typeRow
```

`stack test hsed-spec:bindings` is GHC's check of every comment (unknown names, modifier arity, a modifier of the wrong shape: "Couldn't match type 'EnumValues with 'TypeRow" at `core-2.01.pdf#page=…`). `hsedc check` adds `E-SHAPE`: the binding's semantic `Shape` must accept the table's measured `Form` (a `feature` comment on a three-column type table is an Error; `as @'Codes` on an odd form demotes it to a Warning with the page). Neither check links generated code into the library or `hsedc`.

### 3.4 Document-level rules

Headers are compared whitespace-stripped and case-insensitive after a typo table (`Colum Type`, `M axSize`, `MandatoryWrite  Granularity`); grid corner cells are normalised before the byteGrid test (`Bit` + a lone `Byte` row in T17, `Bit Byte` in T18/T41-T44, `Byte\Bit` in T212); rules see the extractor's `header_source` (§5.1). Scope comes only from the anchors: a `rule` token on a heading governs its subtree, on the `doc` annotation the whole document, several anchors union, and `C-RULE-ANCHORED` requires an anchor per spec that uses the rule. Precedence: a rule may declare `refines <rule>` (`feature` refines `byteGrid`, `columnsAddition` refines `tableDesc`) and wins where both fire; any other double match is `E-AMBIGUOUS`. `checkExemplars` re-verifies each exemplar's header on every run.

Counts are the classifier's output on today's extraction (`docs/evidence/2026-10-25/classify.py` extended with the new rules); `hsedc orphans` is the acceptance test, not this table.

| rule | fires on | anchored at | exemplar | binds (Core / Opal) |
|---|---|---|---|---|
| `typeRow` | `[UID,Name,Format]`, caption `Table N <ident>` (the seven `<X>_object _ref` captions normalise) | Core §5.1.3; Opal §4.2.5 | core T46 | 100 / T30 |
| `enumValues` | `[Enumeration Value\|Set Value, _]`, 2 columns, joined to the same-ident `typeRow`; `cols a b` for wider tables | Core §5.1.3; Opal §4.2.5 | core T52 | 19 (+T87 by comment) / T31 (+T16 by comment) |
| `tableDesc` | `[Column Number,Column Name,IsUnique,Column Type]`, caption `… Table Description`; UID by name from T240 | Core §5.3-5.8; Opal §4.2.5 | core T171 | 45 / T32 |
| `columnsAddition` (refines tableDesc) | tableDesc header, caption `… Columns`; target from the caption noun | Opal §4 | opal T26 | - / T26, T43 (+T50 by `extends`) |
| `tableUIDs` | `[UID of Table Descriptor Object,UID of Table,Table Name,Template]` | Core §6 | core T240 | 1 / - |
| `uidTable` | first header cell starts with `UID` (`UID`, `UID of Row`, `UID in <T> Table`, `Method UID`); semantics from the other headers | Core §6 | core T242 | 9: T239, T241-T248 / - |
| `statusCodes` | `[Name,Value]` | Core §5.1.5 | core T166 | 1 / - |
| `codes` | 2-3 columns, code-like first header (`… Code`, `ID`, `ComID`, `… Type`, `… Value`, `Bit`, `… Version Number`), every first cell an integer/hex/bit/range | doc-wide | core T25 | 8: T15, T16, T19, T25, T26, T40, T179, T235 / 5: T7, T10-T12, T14 |
| `fieldValue` | `[FIELD,VALUE]` -> constants record | doc-wide | core T27 | 3: T27, T29, T30 / T13 |
| `fieldType` | `[Field,Type]` or `[Value,Type,Range]` -> typed record | doc-wide | core T34 | 3: T22, T23, T34 / - |
| `byteFieldValue` | `Byte(s)`, `Field`, `Value(s)` -> Layout with pinned constants | doc-wide | core T33 | 8: T28, T31-T33, T35-T37, T236 / - |
| `byteGrid` | normalised corner + `7 … 0`; extents from `(MSB)`/`(LSB)`; name from the caption; `layout "Name"` overrides | doc-wide | core T17 | 9: T17, T18, T20, T21, T38, T39, T41, T44, T212 / T2 |
| `feature` (refines byteGrid) | byteGrid whose caption matches `Feature Descriptor`; constants by the `pins` reader; `bits k [...]` fallback | Core §3.3.6; Opal §3.1.1 | core T42 | 2: T42, T43 / 5: T3-T6, T8 |
| `constValues` | `[Column,Value]` -> default-row constants typed by the enclosing description | Core §5.5 | core T190 | 9: T188, T190, …, T204 / - |
| `ranges` | `[<X> Name, … Start, … End]` -> `Map Text (Int,Int)`; feeds C-UID-RANGE | Core §6 | core T238 | 2: T237, T238 / - |
| `preconfig` | caption `… <T> Table Preconfiguration` (Opal) or `Default … Authorities` (Core); columns whose header contains `informative` dropped; group rows (only the first cell non-empty) as context; header ⊆ patched target; `(+XX)` template rows; reader by target noun (AccessControl: `TT TT TT TT` wildcards, `*ACn` footnotes as Notes) | Opal §4; Core §5.3, §5.4 | opal T24 | 2: T209, T216 / 26: T18-T25, T27-T29, T33-T42, T44-T48 |
| `tokenEncoding` | class D: T4 read by the `tokens` reader and checked against `Hsed.Core.Token`; T5-T14 and their 13 nested tables recorded as hand-written diagrams with SpecRef | Core §3.2.2 | core T4 | 11: T4-T14 / - |
| `terminology` | caption ends `Terminology` or `Example` -> prose | doc-wide | core T1 | 6: T1, T210, T217, T219, T224, T232 / - |

Special readers reached by one-token comments: `stateTable` (Core T45), `propsNames` (T167), `propsInitial` (T168), `propsReq` (Opal T17), `legend` (Opal T1), `enumInDescription` (Opal T49), `decision` (Core T230/T231, M6), `prose` (Core T2, T3, T24, T185; Opal T9). Every numbered table must end rule-bound, comment-bound or `prose`, else `E-UNBOUND`. `hsedc explain core 46` prints the rule and exemplar; `hsedc orphans` and `hsedc ledger` print coverage.

### 3.5 Worked examples

**E0/E1 Document and rule anchors.** Core page 1 (`/Kind /Doc`): `doc core201 & rule codes & rule fieldValue & rule fieldType & rule byteFieldValue & rule byteGrid & rule terminology`; document-wide rules ride on the doc anchor, the manifest holds md5 `5ceb93ab118a578125e3755eb885b668`, offset +17, anchor count and word-complete sections, and `hsedc` refuses a PDF whose md5 differs. Core §5.1.3 "Column Types", PDF 100 (`/Kind /Rule`): `rule typeRow & rule enumValues`, so no comment appears on the 100 type tables or 19 enumeration tables; the same tokens plus `rule tableDesc & rule columnsAddition` sit at Opal §4.2.5.

**E2 Type-table row.** Core Table 46 AC_element, PDF 100, ordinal 48, Rect `[181.44 264.72 457.56 319.26]`, cells `00 00 00 05 00 00 08 01 | AC_element | List_Type,  *,  ACE_expression`. **No comment.**
```haskell
-- | core-2.01 T46 AC_element (PDF 100 / p.83, ord 48, sha256:3f1a…)
uAC_elementType :: UID
uAC_elementType = uid 0x00 0x00 0x00 0x05 0x00 0x00 0x08 0x01
fAC_elementType :: String            -- kept for the M2a runtime diff; retired at M2d
fAC_elementType = "List_Type,*,ACE_expression"
tAC_element :: TypeDef
tAC_element = ListType Star (Ref "ACE_expression")
```
Checks: C-UID-TABLE, C-UID-UNIQUE over 100 rows, C-UID-RANGE (LSB inside T238's range), C-FMT-RESOLVE (`ACE_expression` is T48), `*` recorded as an SSC placeholder. T71/T72/T73/T81 reach this rule only through the `split_from_merged` repair, recorded in provenance.

**E3 Erratum on a type row.** Core Table 75 clock_time, PDF 107, Format cell `Fractoin`: `erratum scott ["Fractoin" ~> "Fraction"]`, a bare modifier on the rule default. `--errata scott`: `tclock_time = StructType [Ref "Year", …, Ref "Fraction"]`; `--errata published`: `E-FMT-RESOLVE core-2.01.pdf#page=107&T75: unresolved type name Fractoin (nearest: Fraction, T84)` in `gen/ERRATA.md`. T101's `Verify Eror` is the other; `_object _ref` captions and `uidref {X}` spacing are normalised and reported.

**E4 Enumeration.** Core Table 52 adv_key_mode Enumeration Values, PDF 102, `[Enumeration Value, Behavior]`, rows `0 | Wait for AdvKey_Req`, `1 | Auto-advance keys`, `2-7 | Reserved`. **No comment.**
```haskell
data Core_adv_key_mode
  = Core_adv_key_mode_Wait_for_AdvKey_Req | Core_adv_key_mode_Auto_advance_keys
  | Core_adv_key_mode_Reserved | Core_adv_key_mode_Reserved_3 | … | Core_adv_key_mode_Reserved_7
  deriving (Bounded, Enum, Eq, Ord, Show)
instance Enumerated Core_adv_key_mode where typeUID = uadv_key_modeType; range = (0,7)
```
*Corrected:* names follow today's `dEnum` (first value of a range bare, later ones suffixed) so M2a's runtime comparison is clean; `_Reserved_2` is the M2d rename (§13.15). C-ENUM-RANGE uses the Format's lower bound (reencrypt_state 1..16 gets no `_NA` at 0); duplicate labels are an error. Table 87 (three columns, `7 to 31`, overlapping `34-63`) carries `rangeWords ["to"] (enumValues (cols 0 1))` and yields `W-ENUM-OVERLAP` under `--errata published`.

**E5 Table description and its Opal extension.** Core Table 171 (PDF 156-157, stitched, ordinal 173): **no comment**; compiles to `tableTableDesc :: TableDesc` (UID joined from T240, columns 0x00..0x0C). Opal T50 (PDF 98) is outside the §4 anchor of `columnsAddition` and carries `extends (core 171)`; Opal T26 (`Colum Type` typo, row `0x08 ProgrammaticResetEnable`) and T43 bind by the rule with **no comment**, patching T211/T225 before T27/T44 are typed. Checks: C-COL-CONTIG (0x00..0x0E after the patch), C-FMT-RESOLVE; Core T233 reaches the rule through the header cascade and its 14 data rows are an invariant.

**E6 UID assignment table.** Core Table 240 (PDF 302-303, ordinal 242, 46 rows): **no comment**; `u<T>TableObject`, `u<T>Table`, `h<T>`, `nameUID`, `nameHalfUID`; C-T240-HALVES on all 46 rows (the M1 probe already performs it); byte-vs-object kind from Opal T20/T36's Kind column, not `byteTables = ["MBR"]`. T243 (17 authorities after the cascade), T244, T245 bind by `uidTable`; C-UID-RANGE checks LSBs against T237.

**E7 Byte layout.** Core Table 17 ComPacket Format, PDF 40, ordinal 19, rows `Bit | 7 … 0`, `Byte`, `0 (MSB) Reserved`, …, `3 (LSB)`, `4 (MSB) ComID`, `5 (LSB)`, …, `20 to n+19 Payload`. **No comment.**
```haskell
data ComPacketHeader = ComPacketHeader { cpComID :: Word16, cpComIDExtension :: Word16, cpOutstandingData, cpMinTransfer, cpLength :: Word32 }
instance Layout ComPacketHeader where
  layout = res 4 *> (ComPacketHeader <$> be16 <*> be16 <*> be32 <*> be32 <*> be32); size = 20
```
The payload row names `Length`, so `ComPacket = ComPacket ComPacketHeader ByteString` is emitted beside it. Checks: C-LAYOUT-TILE, C-LAYOUT-ENDIAN. The rule also compiles T18 `Packet` (24), T20 `DataSubpacket` (12 + pad4), T21, T38, T39 `Level0DiscoveryHeader` (48, reserved fields pinned by `pins.coreValue`), T41 `FeatureDescriptorTemplate`, T44 `CommonSSCInformation`, T212 `GUDID` (corner `Byte\Bit`) and Opal T2; the protocol 0x02 payloads (T28, T31-T33, T35-T37) and T236 come out of `byteFieldValue` with constants from the VALUE column.

**E8 Bit-field table.** Core Table 42 TPer Feature Descriptor, PDF 69-70, ordinal 44; byte 4 is nine cells (`4` + eight fields, bit 7 leftmost); byte 2 is `Version | Reserved` (two cells, 4+4 by x-extent); constants are three sentences in §3.3.6.4 (`The Feature Code field SHALL be set to 0x0001.`). **No comment** (`rule feature` at §3.3.6 refines `byteGrid`; the caption filter leaves T38/T39/T41/T44 to byteGrid).
```haskell
data TPerFeature = TPerFeature { tfSync, tfAsync, tfAckNak, tfBufferMgmt, tfStreaming, tfComIDMgmt :: Bool }
instance Feature TPerFeature where
  featureCode = 0x0001; featureVersion = 0x1; featureLength = 0x0C     -- pins.coreSet, §3.3.6.4
  layout = bits 4 [(0,tfSync),(1,tfAsync),(2,tfAckNak),(3,tfBufferMgmt),(4,tfStreaming),(6,tfComIDMgmt)] <* res 11
```
Extent rule: eight field cells positional; one spans the byte; any other count (T43 byte 4 has seven, `Reserved` covering bits 7-6) maps lines_strict x-extents onto the eight header columns; failing that, `bits 5 [(0,"KeyChangeZone"),(1,"IdScope")]` is the declared fallback, surfaced in the findings. `pins` has three schemas: `coreSet` (`The <Field> field SHALL be set to <V>.`, T42/T43), `coreValue` (`The value of this field SHALL be <V>.`, T39), `opalBullets` (`<Field> = <V>` after `SHALL return the following`, Opal T2-T8; `VU` and `0x1 or any version …` kept as VU); no readable pins is `W-PINS-NONE` (T44), not an error. A unit test asserts the bit maps equal sedutil `DtaStructures.h:81-134` and, from M3, decode the drive fixture. Opal T3 carries `refines (core 42)` and pins Streaming=1, Sync=1, others VU; T5, T6, T8 are new descriptors with no comment.

**E9 Method signature in prose.** Core §5.3.3.6 Get (PDF 201): the signature block carries **no comment** (`rule sig` at §5.3.3); the heading carries `behaviour "Base.get"`. Generated: `data Get = Get { getCellblock :: Cell_block }` with `instance Method Get` (`methodUID` joined from T242, `Invokers = '[TableRef, ObjectRef]`, `Result = TypeOr '["Bytes" ::: Bytes, "RowValues" ::: List (Named ColumnNumber Value)]`); required parameters positional, optional ones Named by 0-based index (§3.2.4.2), fixing `Call.hs:210-212` by construction. The splice reifies `Hsed.Sim.Base.get`, requires `Invoker -> Get -> TPerM (Result Get)` and `SpecRef "core-2.01:5.3.3.6"`; the Fails list binds by `fails "Base.getFails"`.

**E10 Normative SHALL sentence that is behaviour.** Core §3.3.10.2.1 item 3.a, PDF 82, anchored to the /LI, `/Kind /Law`: `law "Sync.ifSendWhileBusy"`.
```haskell
ifSendWhileBusy = Law { cites = ["core-2.01:3.3.10.2.1#3a"], quote = "<normalised sentence>"
  , simProp = \c st -> syncOf (comId c) st `elem` [S2,S3] && isIfSend c
               ==> step (Cmd c) st == (Out (Response SynchronousProtocolViolation mempty), st)
  , jigCases = [ETC 2] }
```
C-LAW-BOUND, C-LAW-HASH; runs as a QuickCheck property and as ETC-02 (on hardware "unobservable"). Siblings bind the discard path: `law "Sync.discardToS1"` on §3.3.10.6 S2:S1 items c/d (PDF 85) and `law "Sync.ignorePacket"` on §3.3.10.7 a (PDF 86), what ETC-08/ETC-15 observe as (0,0,0). Table 45 carries `stateTable "Sync.table45"` and is also the host loop's contract (§6.1).

**E11 Opal legend.** Opal Table 1, PDF 12: `legend`.
```haskell
data CellClass = ReadOnlySpecified | ReadOnlyVU | WritePersonalizable | WriteFixed | NotDefined | Unspecified
cellClass :: CellStyle -> Markers -> (CellClass, [Finding])
-- shading ≈231 (±8): plain -> ReadOnlySpecified, bold+underline -> ReadOnlyVU
-- shading ≈180 (±8): bold+underline -> WritePersonalizable, plain -> WriteFixed
-- white (255) or unfilled -> Unspecified; '(N)' marker on the heading or in the cell -> NotDefined (never from colour)
-- any other fill (218, 235 on 14 cells of T24/T25) -> nearest legend grey + W-LEGEND-OFFSCALE (page, Rect)
```
*Corrected:* two independent signals plus text markers; Table 1 is documentation and the `checkExemplars` exemplar, not the training set (its white NotDefined sample would label 838 Name cells NotDefined). C-LEGEND-TOTAL: every cell of T18-T48 maps or produces a finding; an unclassifiable style is an Error with Rect.

**E12 Opal preconfiguration table.** Opal Table 24 Admin SP Authority, PDF 48, ordinal 26, header (`bold-first-row`) = Core T178's 19 columns; rows Anybody `00 00 00 09 00 00 00 01`, Admins, Makers, SID (Operation Password, Credential C_PIN_SID), Admin1 `00 00 00 09 00 00 02 01` (glyph-split `00 00  0 2  01`), AdminXX `(+XX)`. **No comment.**
Generated: `adminSP_Authority :: [Pre Authority_Row]` with `Pre { row = Authority_Row { uid = 0x0000000900000006, name = "SID", enabled = True, operation = Password, credential = Ref uC_PIN_SID, … }, access = [(enabled, ReadOnlySpecified), …] }`; AdminXX expands to Admin2..N for `SimParams.adminSPAdmins`.
Checks: C-PRECONF-HEADER, UID high half `00 00 00 09`, names resolve in T24/T25, cells parse against the column TypeDef; `access` gates `Set`. T23/T39 (ACE) carry a `Table Association - Informative text` column and group rows (`BaseACEs`): the rule drops every `informative` column and carries group rows as context for T22, T23, T38, T39, so T38 keeps 96 rows (80 ACL + 16 groups) and T22 45 (34 + 11). Shadow: Core T243 names `00 00 00 09 00 00 02 01` "Issuers"; Opal reuses it as Admin1; UID cross-checks are per spec.

**E13 Opal override of Core.** Opal Table 49, PDF 98, header `[UID,Name,Format,Size,Description]` (typeRow does not fire): `enumInDescription (replaces [core 102, core 103])` -> `data Opal_life_cycle_state = Issued | … | Manufactured_inactive | Manufactured | … | Reserved_15`, typeUID unchanged, Core's shadowed in the Opal target. C-PATCH-UID, C-ENUM-RANGE (0..15). Opal T29 then types SP.LifeCycleState: Admin SP `Manufactured`, Locking SP `Manufactured-Inactive`.

**E14 Opal Properties requirements.** Opal Table 17, PDF 33-34, 12 TRs of which one is a ragged single-cell continuation of MaxSubpackets (joined by §5.1 step 5; 11 property rows): `propsReq` -> `opalInitialAssumptions = coreInitialAssumptions { maxComPacketSize = 2048, maxResponseComPacketSize = 2048, maxPacketSize = 2028, maxIndTokenSize = 1992, maxPackets = 1, maxSubpackets = 1, maxMethods = 1 }` and `opalPropertyBounds :: Map PropertyName Bounds` ((M)/(N)/(O), minimum, VU). C-PROP-NAME against T167 `propsNames`; C-PATCH-MONOTONE against T168 `propsInitial` (2048 >= 1024, 2028 >= 1004, 1992 >= 968). Siblings: T14 `narrows (core 26)`, T15 `narrows (core 4)`, T16 `replaces [core 127, core 128]`; T13 binds by `fieldValue` with no comment.

### 3.6 Comment budget

Core: 1 `doc` (with the document-wide `rule` tokens); heading anchors for `typeRow`/`enumValues` (§5.1.3), `tableDesc` (§5.3-5.8), `tableUIDs`/`uidTable`/`ranges` (§6), `statusCodes` (§5.1.5), `feature` (§3.3.6), `constValues` (§5.5), `preconfig` (§5.3, §5.4), `tokenEncoding` (§3.2.2); per-table: T45 `stateTable`, T87 `rangeWords …`, T167 `propsNames`, T168 `propsInitial`, T230/T231 `decision` (`prose` until M6), 2 `erratum` (T75, T101), 4 `prose` (T2, T3, T24, T185); plus `behaviour`/`fails`/`law` for the tier-1 sections (about 12 at M4). 236 of 248 Core tables carry nothing. Opal: 1 `doc`; anchors for `feature` (§3.1.1), `typeRow`/`enumValues`/`tableDesc`/`columnsAddition` (§4.2.5), `preconfig` (§4); per-table: T1 `legend`, T2 `refines (core 39)`, T3 `refines (core 42)`, T4 `refines (core 43)`, T9 `prose`, T14 `narrows (core 26)`, T15 `narrows (core 4)`, T16 `replaces [core 127, core 128]`, T17 `propsReq`, T49 `enumInDescription (replaces …)`, T50 `extends (core 171)`. 39 of 50 Opal tables carry nothing.

## 4 Haskell library sketch

Three packages in one stack project (`packages: [hsed-core, hsed-spec, hsed]`). `hsed-spec` depends on `hsed-core` only; its `bindings` test component is the only place generated comment code is compiled; `hsed` depends on `hsed-core` and holds `gen/`.

### 4.1 hsed-core (codec, no generated code)

Module map: hsed-core = StreamItem, Token, Value, Call, UID, Base_Type.*, Simple_Type, Reference_Types, Table, THUtil, Util, Data.BoundedSize.*, Data.Refined, plus the new Layout and Types; hsed = Types (umbrella), Import, TypeUIDs, TableUIDs, SpecialPurposeUIDs (hand versions until M2a, re-exports of `Hsed.Gen.*` afterwards), Hsed.App, Host/Run, TPer/Run, TypeTests, app/Main.hs. The codec never imports the UID modules (only `Types.hs:35-40` does), which keeps the split acyclic. QuickCheck stays a hsed-core dependency: its Arbitrary instances live beside their types in eight modules and moving them would create orphans.

```haskell
-- Hsed.Core.StreamItem   existing: class StreamItem a where parser :: Parser a; generate :: a -> ByteString
-- Hsed.Core.Token / Value / Call / UID   existing, fixed at M0: Parameters wrapped in StartList/EndList (Call.hs:210-212);
--   removeEmpty = filter (Token.hs:359); long-atom bound 0xFFFFFF; optional params as Named uinteger 0..; new MethodResponse
data MethodResponse = MethodResponse { results :: [Value], status :: (Word8, Word8, Word8) }
-- Hsed.Core.Layout       back end for layout/feature/byteFieldValue
class Layout a where layout :: LayoutP a; size :: Proxy a -> Int
res :: Int -> LayoutP (); be16 :: LayoutP Word16; be32 :: LayoutP Word32; const8 :: Word8 -> LayoutP (); bits :: Int -> [(Int, a -> Bool)] -> LayoutP a; pad4 :: LayoutP ()
newtype ViaLayout a = ViaLayout a            -- instance Layout a => StreamItem (ViaLayout a)
-- Hsed.Core.Types        runtime TypeDef (BaseType | SimpleType | EnumType | AltType | ListType | RestrictedRef | GeneralRef | GeneralRefTable | Named* | StructType | SetType)
class Enumerated e where typeUID :: UID; range :: (Int, Int)
-- Hsed.Core.Table        TableDesc, Col, Access = ReadOnlySpecified | ReadOnlyVU | WritePersonalizable | WriteFixed | NotDefined | Unspecified
--                        data Pre r = Pre { row :: r, access :: [(ColNum, Access)] }
-- Hsed.Core.TH           THUtil's dSig/dVal/eUID/dData and Base_Type/TH ci/cu/cb' stay here
```

### 4.2 hsed-spec (compiler; exe `hsedc`; test component `bindings`)

```haskell
-- Hsed.Spec.IR (aeson; the frozen seam, irVersion 1): SpecIR { irVersion, spec, pdfMd5, pages, labelOffset, notice, extractor, wordComplete :: [Section], tables, paras, sigs, annots }
--   TableIR { number, caption, ordinal, struct, parts, nestedIn, pages, rects, headerSource :: LeadingTH|FirstOfAllTH|Repeat|BoldFirstRow|NoHeader, header, rows, warnings }
--   CellIR { text, lines, norm, fill :: Maybe RGB, bold, underline, font, rect, estRect, repair :: [SplitFromMerged|MovedOut|AdoptedFrom|RecoveredByGeometry|AdoptedOrphan|RaggedJoin] }
--   ParaIR { section, item, page, rect, words :: Maybe [Text], hash };  SigIR { section, page, rect, heads, text };  AnnotIR { kind, anchorId, page, rect, contents, hsed }
-- Hsed.Spec.Form (measured; total)   data Form = Form { cols :: Int, headerSource :: HeaderSource, headerNorm :: [Text], grid8 :: Bool, captionKind :: CaptionKind, ragged :: Bool }
-- Hsed.Spec.Shape (asserted)
data Shape = TypeRow | EnumValues | TableDesc | ColumnsAddition | TableUIDs | UidTable | StatusCodes | Codes | FieldValue | FieldType | ByteFieldValue
           | ByteGrid | Feature | ConstValues | Ranges | Preconfig | Tokens | StateTable | Props | PropsReq | Legend | Decision | Prose | Doc | Heading | Sig
accepts :: Shape -> Form -> Maybe Finding            -- E-SHAPE (Error for rule defaults, Warning under `as`)
-- Hsed.Spec.Anchor   data Anchor (s :: Shape) = Anchor { aId :: AnchorId, aSpec :: SpecId, aStruct :: Text, aPage :: Int }  -- rest from the manifest
-- Hsed.Spec.Grammar  parse :: Text -> Either Finding SomeBinding; print :: SomeBinding -> Text; parse . print == id
-- Hsed.Spec.DSL   data Binding (s :: Shape): one constructor per Shape (Doc, Rule, TypeRowB, EnumValues, TableDescB, TableUIDs, UidTableB, StatusCodes, CodesB,
--   FieldValue, FieldType, ByteFieldValue, LayoutB (Maybe TypeName), FeatureB (Maybe TypeName), Bits Int [(Int,Text)], ConstValues, Ranges, PreconfigB, Tokens,
--   StateTable QName, PropsNames, PropsInitial, PropsReq, Legend, Decision QName, SigB, Behaviour QName, Fails QName, Law QName, ProseB) plus shape-polymorphic
--   Refines Ref, Narrows [Ref], Replaces [Ref], Extends Ref :: Binding 'ColumnsAddition, Also (&); type Mod s = Binding s -> Binding s with cols, rangeWords,
--   enumInDescription, expand, erratum, as @s'; data Bound = forall s. Bound (Anchor s) (Binding s); at :: Anchor s -> Binding s -> Bound.
--   Lowercase smart constructors mirror the constructors and are the comment vocabulary.
-- Hsed.Spec.Rules     Rule { ruleName, refinesRule :: Maybe RuleName, exemplar :: Ref, fires :: SpecId -> TableIR -> Bool, bindsTo :: SomeBinding }
--                     rules; scopeOf :: SpecIR -> RuleName -> [Section] (from anchors only); checkExemplars; classify (E-UNBOUND, E-AMBIGUOUS)
-- Hsed.Spec.Readers   pins (coreSet | coreValue | opalBullets), enumInDescription, accessControl, tokens, legend, stateTable, propsReq
-- Hsed.Spec.Format    parseFormat :: Env -> Text -> Either Finding TypeDef; corpus test = dev/Formats.txt (82 strings)
-- Hsed.Spec.Check     Finding { severity :: Error|Warning|Note, code, anchor, page, message }; Compile = RWST Env [Finding] Artefacts (Either Finding)
--                     class Compile' (s :: Shape) where compile :: Anchor s -> TableIR -> Binding s -> Compile [Decl]
-- Hsed.Spec.Overlay   Patch = Pin | Narrow | Replace | AddColumns | AddTable | Rows; applyOverlay ordered: pins, comms, schema (T26/T43/T50), preconfig
-- Hsed.Spec.Errata, Hsed.Spec.Gen (deterministic printer, golden-tested), Hsed.Spec.Sidecar (.hsa, manifest, reanchor), Hsed.Spec.Pdf (M1: python shims; M6: Pdf.Core.Writer)
-- exe hsedc: gen | extract | bind | check | compile | annotate | apply | dump | verify | adopt | reanchor | explain | orphans | ledger | coverage
-- test:bindings  hsed-spec/bindings/Hsed/Bindings/{Core,Opal}.hs (generated, committed) + test/negative/*.hsa fixtures
```

### 4.3 hsed (runtime)

`Hsed.Gen.Core.*` and `Hsed.Gen.Opal.*` (generated), `Hsed.Gen.{Bindings,Provenance,Guard,LawsSpec}`; `Hsed.Law` (`data Law st = Law { cites, quote, simProp :: Maybe (st -> Request -> Property), jigCases }`, `newtype SpecRef`, `$(checkBindings 'bindings)` reifying each QName and checking type and `SpecRef`); `Hsed.Wire`; `Hsed.Transport.{InProc,MacKext,LibSed,Cdb,Mhtx.*}`; `Hsed.Sim.*`; `Hsed.Jig.{Catalogue,Case,Prereq,Run,Guard,Transcript,Pair,Report}`; exes `hsed-sim` (`--listen`, `--units`, `--errata`, `--locking-ofs`, `--profile`, `--heartbeat-interval-ms`, `--dead-timeout-ms`, `--allow-remote-reset`) and `hsed-jig` (`--target`, `--tests`, `--allow-write SERIAL`, `--recover`, `--allow-mounted`, `--allow-error-paths`, `--sessions`).

## 5 Compile pipeline and IR

### 5.1 Stage A: extract (Python, `tools/extract/extract.py`, run with `-I`; `tools/extract/extract_tables.py` promoted)

1. Verify md5 (Core `5ceb93ab118a578125e3755eb885b668`, Opal `054e505db3895b25c38dd9dfbcc36259`). Input is `build/<spec>.hsed.pdf` = `tcg/<original>` + `spec/annot/<spec>.upd`; on a pristine file annots is empty.
2. Tag tree (pikepdf) joined to glyphs (pymupdf `TEXT_COLLECT_STRUCTURE`, whose `idx` chain equals the `/K` path; 7,619/7,632 Core and 6,197/6,198 Opal chains resolve): every /Table gets ordinal, struct path, caption, /TR -> /TH|/TD -> glyphs regrouped into visual lines; `/Artifact` markers dropped; split (Core T7) and page-spanning tables stitched.
3. Header cascade, recorded as `header_source`: leading /TH rows; if every row is /TH, the first row only (T233, T243, T244, T245); else the first row if restated at a later page top (`repeat`: Opal T22, T23, T25, T38, T39); else the first row if at least half its inked cells are bold (`bold-first-row`: Opal T24 and most preconfiguration tables); else `none` (T6, T8, T11, T13, T17, T24). Repeated headers removed and listed (35 rows in 33 Core tables, 41 in 11 Opal).
4. Geometry repairs (`find_tables(strategy='lines_strict')`, only on pages that need them), flagged per cell: (a) split a /TD whose glyphs partition into header column bands separated by a drawn rule (`split_from_merged`: Core T32, T33, T36, T71, T72, T73, T81; genuine byte-layout merges are left alone); (b) move a stray fragment to the drawn cell containing it (T70 -> T71); (c) rebuild rows from the drawn grid when struct rows dangle (`recovered_by_geometry`: T189, T190, T191; `adopted_orphan`: Opal T5). Tag tree and grid disagreeing on column counts after repair is `E-GRID`.
5. Normalisers (unit-tested on observed cases): UID cells strip whitespace and must yield 16 hex digits; glyph-split words re-joined from `lines` (`Obje ct`, `M axSize`, `C_PIN_Admin 1`); superscript footnotes split off (`(+XX)1`); curly quotes; corner cells; displaced byte numbers (Opal T2/T3). Ragged-row join (*corrected*): a row with fewer cells than the header, all non-empty, is appended to the predecessor in the columns its x-extents overlap (`RaggedJoin`; Opal T17 row 7 into MaxSubpackets, 11 property rows remain); a full-width row is never joined, so the 147 AccessControl/ACE rows with an empty informative first cell and the vertically merged Byte cells of Opal T2-T8 are untouched.
6. Style pass per cell: last-drawn non-thin filled rect under the cell centre (sRGB), bold from font flag or name, underline from a thin filled rect at the baseline in the glyph's writing frame (borders rejected by length).
7. Prose: headings, /P and /LI with section numbers from the outline; each ParaIR carries a sha256 of normalised words; words are emitted for anchored paragraphs and for every paragraph in a reader-consumed section (`pins` sections, Opal §3.1.1.x bullets, Fails lists, T49 Descriptions), listed as `wordComplete`; signature blocks match `^[A-Za-z_]+\.[A-Za-z_]+ *\[ … =>` (51 Core + 3 Opal). 8. Annotations in the input are read back into AnnotIR.
9. Emit sorted-key JSON with `irVersion`, `notice`, extractor versions and assert sanity invariants: `5.1.3 Column Types` on PDF 100 / label 83; 248 and 50 numbered captions; Table 185 present; T46 ordinal 48; T7 one table of one data row; T71/T81 three cells per row; T189 14, T233 14, T243 17, T244 4, T245 5 data rows; Opal T24 header = Core T178's columns; Opal T17 11 rows; T38 96; T22 45.

### 5.2 Stage B: compile (Haskell, `hsedc`)

- `hsedc bind spec/ir/*.json` -> `hsed-spec/bindings/Hsed/Bindings/<Spec>.hs` (one `at (anchor @'S …) $ <payload>` per annotation with a LINE pragma). `stack test hsed-spec:bindings` type-checks every comment without touching the library or `hsedc`.
- `hsedc check`: classification (E-UNBOUND, E-AMBIGUOUS), `accepts` (E-SHAPE), rules, every `Compile'` instance's checks, `checkExemplars`, C-RULE-ANCHORED, law hashes, the Format corpus; per-table ledger; non-zero exit on Error.
- `hsedc compile --errata scott --target core --target opal230 -o gen/ --report build/report/` builds CoreModel, applies the Opal patch list in document order, emits `gen/**/*.hs` with provenance Haddocks, `gen/Hsed/Gen/Bindings.hs`, `build/report/{findings,coverage,ERRATA,ledger}.md`. `--target doc` renders bound tables as Markdown with page citations.
- `hsedc gen` = extract -> bind -> check -> compile in one invocation; the pre-commit hook runs it and refuses a commit that changes `spec/ir`, `hsed-spec/bindings` or `gen/` without the corresponding source change. `stack build hsed` then runs the binding-check splice.

### 5.3 Annotation round trip

- `spec/annot/<spec>.hsa`: one line per anchor, `core-2.01 | T75 | erratum scott ["Fractoin" ~> "Fraction"]`; page, Rect, ordinal and hashes are never hand-typed.
- `hsedc annotate` (M1: `tools/extract/annotate.py`, pypdf `PdfWriter(path, incremental=True).add_annotation`, deterministic) writes `build/<spec>.hsed.pdf` as pristine + ONE incremental section with annots sorted by key and exports `spec/annot/<spec>.upd` and `<spec>.manifest.json`. No separate `squash`. `hsedc apply tcg/<pdf> <spec>.upd` verifies md5 and concatenates; `hsedc dump` recovers the `.hsa`.
- `hsedc adopt`: a visible sticky note typed in a viewer (a `/Text` without `/HSED`) is hit-tested by Rect against the page's table Rects, rewritten as a hidden anchored annotation in a fresh incremental section, and its text parsed as a payload; a note hitting no table or failing to parse is reported with its page. This is the PDF-first editing path and the GUI test of §13.19.

### 5.4 CI gates (`.github/workflows/ci.yml`; also `make check`)

a. `stack --system-ghc test` on ubuntu and macos (stack 3.11.1, ghcup 9.4.7 cached), no GHC download. b. regen-diff: pinned venv, `hsedc gen` -> `git diff --exit-code spec/ir hsed-spec/bindings gen/`. c. byte prefix: `cmp -n <size>`; tail equals `.upd`. d. render md5 on the SAME runner (gs `png16m -r100`, pymupdf, `sips`) for every annotated page and a 10-page sample. e. read-back by pikepdf, pypdf, pymupdf; `dump(apply(upd)) == hsa`; `annotate(hsa) == upd`. f. negative fixtures: each `test/negative/*.hsa` fails `hsedc check` or the bindings component with a message containing `core-2.01.pdf#page=`. g. the M2a oracle (§5.5). h. conformance: `dart run mh_transport:conformance … --interval-ms 200 --timeout-ms 600` against hsed-sim (M5; optional where dart exists).

### 5.5 The regenerate-and-compare oracle

The M1 probe's `Equiv.hs`, extended to every definition, compares regenerated `TypeUIDs`/`TableUIDs` values with the hand-written library at runtime; the differences must be exactly: UIDs of Seconds (`00 00 00 05 00 00 14 06`), feedback_size (`02 14`), fraction_enum (`04 1C`), gen_status (`1A 02`); captions 157 and 161; the truncated Table 82 caption; Table 87 now present; the errata-set-dependent corrections (Fractoin, Verify Eror, `_object _ref` in T72/76/112/131/140/142/144, Table 128 header, Table 61 header); the two hand-formatting artefacts (T149 `Simple_Type,uinteger, 2`, T105 split `Restricted_Reference_Type{6}`); the ten whitespace-only Format differences. Committed as `spec/expected-diff.txt`; the job fails on any other line. The `-ddump-splices` dumps are secondary; `dev/TableUIDs.txt` is stale (no `oth<T>`/`bth<T>`) and is regenerated at M2a.

### 5.6 Re-anchoring on a spec revision

`hsedc reanchor --from spec/ir/core-2.01.json --to build/ir/core-2.02.json` matches by caption title, header, words hash, ordinal ±k, then geometry; writes a proposed `.hsa` and an Unchanged/Moved/Changed/Lost report with pages. Changed header or hash marks for review; a changed `/Law` sentence blocks; errata whose text no longer occurs are "possibly fixed upstream". M6 exercises it with Opal 2.01 (tagged).

## 6 Wire contract

### 6.1 Haskell interface (`Hsed.Wire`)

```haskell
data Dir = IfSend | IfRecv
data Request = Request { dir :: Dir, proto :: Word8, comId :: Word16, lenBytes :: Word32 {-IF-SEND payload / IF-RECV allocation-}, payload :: ByteString }
data IfStatus = Good | InvalidSecurityProtocolId | InvalidTransferLengthOnIfSend | OtherInvalidCommandParameter
              | SynchronousProtocolViolation | DataProtectionError | TransportFailure Text {-kext kIOReturn, socket loss-} | Unsupported Text
data Response = Response { status :: IfStatus, rpayload :: ByteString }  -- IF-RECV: exactly lenBytes bytes, zero-padded
data Event = PowerCycle | HardwareReset | HotPlug   -- Opal Table 16 types 0-2; Programmatic (3) is TPER_RESET, an IF-SEND
data Unit = Blocks512 | Bytes;  data Cap = ReadOnly | ReadWrite
data Transport (c :: Cap) = Transport { exchange :: Request -> IO Response, inject :: Event -> IO IfStatus, unit :: Unit, describe :: Text, close :: IO () }
newtype Channel = Channel (IORef (Map Word16 (MVar ())))
withComId :: Channel -> Word16 -> IO a -> IO a       -- one command in flight per ComID; a second IF-SEND queues, never issues
transferLength :: Unit -> Int -> Word32              -- `blocks 1` = 512 on Blocks512, 1 on Bytes (SPF-02 case 2)
exchangeComPacket :: Channel -> Transport c -> Word16 -> ByteString -> IO (Either IfStatus [ByteString])
  -- IF-SEND, then IF-RECV (len = negotiated MaxComPacketSize, default 2048) while OutstandingData /= 0:
  --   Length 0 / OutstandingData 1 / MinTransfer 0 -> sleep 25 ms, retry;
  --   MinTransfer M /= 0, with or without data in this ComPacket -> re-issue with max M len (rounded to units);
  --   OutstandingData 0 -> done; per-request timeout 10 s (the kext's SED_TIMEOUT).
  -- Host half of Law "Sync.table45"; sedutil's `minTransfer == 0` stop (DtaDevOpal.cpp exec loop) drops Table 45 row 3 and is not copied.
```

### 6.2 Units and padding per transport

| transport | unit | IF-SEND | IF-RECV | protocol 0x02 |
|---|---|---|---|---|
| SCSI A2h/B5h | Blocks512, INC_512=1 | round up to 512, zero pad | allocation in whole blocks | one block |
| SAT A1h wrapping 5Ch/5Eh | Blocks512 | count = len/512 | count = len/512 | one block |
| NVMe Security Send/Receive | Bytes | as is | as is | bytes |
| mh_transport, in-process | Bytes | as is; simulator ignores bytes beyond ComPacket.Length | zero-padded to len | as is |

The simulator with `--units blocks512` rejects a non-multiple with OtherInvalidCommandParameter (SIIS §4.3); with `--units bytes` an IF-RECV shorter than the 20-byte header is "insufficient transfer length" per Table 45, not an error. Interface status is never carried in payload. On the kext path every error is `TransportFailure "0xff"`; the jig records expected-status assertions there as "unobservable on this transport".

### 6.3 mh_transport frames (SPEC v1 unchanged; JSON text frames; reply on the originating socket, no `seq`)

```json
{"type":"hello","v":1,"server_id":"q7d2m0s8x1kz","seq":0,"heartbeat":{"interval_ms":20000,"timeout_ms":60000},
 "app":{"sed":{"v":1,"role":"tper","impl":"hsed-sim 0.1.0","units":"bytes","ssc":"opal-2.30","errata":"scott","base_comid":4100,"n_comids":1}}}
{"type":"sed.req","id":2,"op":"if_send","proto":1,"comid":4100,"len":2048,"payload":"AAAAABAEAAAAAAAAAAAAAFQ…"}
{"type":"sed.rsp","id":2,"status":"good","len":0}
{"type":"sed.req","id":3,"op":"if_recv","proto":1,"comid":4100,"len":2048}
{"type":"sed.rsp","id":3,"status":"good","len":2048,"payload":"AAAAABAE…"}
{"type":"sed.rsp","id":4,"status":"synchronous_protocol_violation","len":0}
{"type":"sed.evt","seq":7,"event":"power_cycle","comids_reset":[4100],"sessions_aborted":[[4100,4096]]}
```

- `op` ∈ `if_send | if_recv | reset | describe`. `status` is the snake_case of `IfStatus` plus `protocol_error`, sent only in reply to a frame whose `type` is `sed.req` (bad fields, non-increasing id, len > 1 MiB). Every other frame (unknown type, non-JSON, binary) is ignored with no reply, as SPEC.md L18-21 requires and conformance check 3 tests.
- Ids are positive and strictly increasing per socket. The client subscribes before sending, correlates by id, times out per request, fails all pending on disconnect or on a hello with a new `server_id` (treated as PowerCycle). IF-RECV may be retried after reconnect; IF-SEND is never replayed blindly.
- The hub processes `sed.req` in arrival order per socket; the per-ComID S1/S2/S3 machine is shared across sockets, so two hosts on one ComID legitimately see `synchronous_protocol_violation`. `sed.evt` is a seq-stamped broadcast (SPEC.md L9-14, L35-53) for resets injected by any client or the operator. Optional `?token=` pre-hello check beyond localhost.

## 7 Transport backends

1. **InProc** (M3): `IORef TPer` + `step`; `inject` runs the reset rules; default `Bytes`, `--units blocks512` to test padding; `Tick` after every IF-SEND unless `--hold-processing`. `Transport 'ReadWrite`.
2. **MacKext** (macOS, cabal flag `kext`, M3): `foreign import ccall` of `findBSDName`, `findParent`, `conformsToDriverClass`, `OpenUserClient`, `DriverPerformSCSICommand`, `CloseUserClient` from `/Library/Frameworks/SEDKernelInterface.framework` (build 625). *Corrected:* `TPerUpdate`/`DTA_DEVICE_INFO` are not bound (`TPerUpdate` copies the whole 'DTA Device Info' CFData into the caller's struct, `TPerKernelInterface.c:143-157`, in the 2024-03 bitfield layout, not today's `DtaStructures.h:482-545`); interface type and serial come from `IORegistryEntryCreateCFProperty` ('TPer Interface Type'; 'Identify Device Characteristics' -> 'Serial Number', trimmed). Locate the TPer as `DtaMacOS::openDeviceHandle` does (`DtaMacOS.cpp:37-114`). CDBs in `Hsed.Transport.Cdb`, shared with LibSed: SAT -> `A1h` wrapping `5Ch`/`5Eh` (`DtaOS.h:224-323`); SCSI -> `A2h`/`B5h` (INC_512 = 1). Buffers via `GHC.ForeignPtr.mallocForeignPtrAlignedBytes len 16384` (not in `Foreign.ForeignPtr`; verified aligned for 2048, 61440, 65536). Root and exclusive open (`UserClient.cpp:360-381`); refuse to start while `sedutil-cli` or the SED ToolBox daemon holds the device. No sense (`DtaMacOS.cpp:362-422`): every failure is `TransportFailure`. `Transport 'ReadOnly` only; `inject` returns `Unsupported`.
3. **LibSed** (M6, Linux on errol). *Corrected boundary:* `DtaDrive::sendCmd` returns only 0x00/0xff, so the shim sits at `DtaOS::PerformSCSICommand`/`PerformNVMeCommand` (`DtaOS.h:364-370`): `sed_open`, `sed_scsi(handle, dir, cdb, cdb_len, buf, len, out)`, `sed_nvme(handle, opcode, nsid, cdw10, cdw11, buf, len, out)`, `sed_close`, `out = {rc, masked_status, sense[18], nvme_status}`; Haskell maps per SIIS Tables 14 and 25. *Corrected packaging:* sedutil is GPLv3, so libsed is an externally installed shared library (own repo or an upstream DTA branch) behind cabal flag `libsed` (default off, `extra-libraries: sed`); a `-flibsed` jig is GPLv3 (§10.5); hsed vendors no sedutil source; a fork means a `.gitmodules` change, a wapex landing (§10.4).
4. **Mhtx client** (M5): websockets 0.12.7.3 + wuss (self-signed wss); hello first (3 s), ping every `interval_ms`, any inbound frame is liveness, reconnect with 1-30 s equal-jitter backoff; reduced client contract in `docs/wire.md`.
5. **Mhtx hub** (M5): warp + wai-websockets serving `/ws` (hello, pong, dead-client sweep with close 4000, seq broadcast, unknown/non-JSON/binary frames ignored) plus `POST /emit` and `GET /clients`, so `dart run mh_transport:conformance ws://host:8765/ws --emit … --count … --interval-ms 200 --timeout-ms 600` certifies it with zero Dart changes (check 7 is skipped above 10 s, hence the flags). `exchange` runs off the event thread. `hsed-sim --listen 0.0.0.0:8765 --units bytes|blocks512 --errata scott --heartbeat-interval-ms N --dead-timeout-ms N [--token …] [--allow-remote-reset]`.
6. **Changes in `~/mh_transport`: none.** Recommended separately with its own PR, Dart suite and a "binary frame is ignored" conformance check: `TransportHub.serve` dispatching on text/bytes (`python/mh_transport.py:110,125-129`).
7. **Parked:** MacVFN/xnvme; kext user-client extension to return SCSI status + autosense (§13.12).

## 8 Simulator

### 8.1 State

```haskell
data TPer = TPer { cfg :: SimParams, comIds :: Map Word16 ComIdState, sps :: Map UID SP, rng :: StdGen, clock :: Tick }
data SimParams = SimParams { baseComId :: Word16 {-0x1004-}, nComIds :: Int, msid :: ByteString, sidPin :: Maybe ByteString {-Nothing = MSID-}
  , adminSPAdmins :: Int, lockingAdmins :: Int {->=4-}, users :: Int {->=8-}
  , lockingOFS :: LifeCycle {-ManufacturedInactive (default) | Manufactured: T40/T41 folded, Admin1 PIN := SID PIN-}
  , profile :: Maybe Level0Profile {-descriptor set from a fixture; disk6 adds 0x0202/0x0402/0x0403, drops 0x0005/0x0404-}
  , inactiveComIdDiscard :: Bool {-Core §3.3.3.2 silent discard instead of OtherInvalidCommandParameter-}
  , tperResetEnabled :: Bool, gudid :: ByteString, firmware :: Text, units :: Unit }
data ComIdState = ComIdState { ext :: Word16 {-0x0000 static-}, life :: Inactive | Issued | Associated {-static: Issued without sessions-}
  , sync :: S1 | S2 [Pending] | S3 [ByteString], tperProps, hostProps :: Properties, control :: ControlSession {-nextTSN 4096-}
  , sessions :: Map Word32 Session, comIdResp :: Maybe ByteString {-protocol 0x02 response buffer-} }
-- Session { hsn, tsn, sp, write, authenticated :: Set UID, challenge, txn :: [TPerSnapshot], timeout }; SP { spUid, lifeCycle, factory, frozen, tables :: Map UID Table }
-- Table = ObjectTable TableDesc (Map UID (Row, Map ColNum Access)) | ByteTable ByteString Granularity
data Input = Cmd Request | Ev Event | Tick
data Outcome = Prepared [ByteString] | Discarded | SessionAborted (Maybe ByteString {-CloseSession-})   -- result of an accepted IF-SEND
step :: Input -> TPer -> (Output, TPer)
```

### 8.2 Dispatch

- proto 0x01, ComID 0x0001: Level 0. IF-SEND discarded with Good; IF-RECV renders the 48-byte header + descriptors in feature-code order (TPer, Locking with bits derived live from Locking rows and MBRControl, Geometry, SIIS, 0x0202/0x0402 from hand-written `Hsed.Sim.FeatureSets`, Opal V2 with base ComID and minor 0x3, Data Removal; `profile` overrides the set), truncated or zero-padded to len.
- proto 0x02: ComID 0x0004 TPER_RESET -> `Programmatic` reset when enabled else OtherInvalidCommandParameter (Opal §3.3.6); other ComIDs: HANDLE_COMID_REQUEST / GET_COMID_RESPONSE (VERIFY_COMID_VALID reporting `life` as 1/2/3 in bytes 12-15, STACK_RESET per Core T27-T37); GET_COMID returns zeros. proto 0x00: SPC inventory `{0x00,0x01,0x02}`, hand-written.
- proto 0x01 on a ComID outside {0x0001, base..base+n-1} (*corrected*, Core §3.3.3.2-3, Opal §3.3.3): IF-SEND -> OtherInvalidCommandParameter, or silent discard when `inactiveComIdDiscard`; IF-RECV -> a 20-byte ComPacket with the ComID echoed, extension 0xFFFF, zeros elsewhere. InvalidSecurityProtocolId only for a protocol outside {0x00, 0x01, 0x02}.
- proto 0x01 on a static ComID: S1 + IF-SEND: `lenBytes > MaxComPacketSize` -> InvalidTransferLengthOnIfSend; otherwise accepted with Good and the payload yields an `Outcome` (*corrected*): `Discarded` -> stay in S1, no response, no state change, when the ComPacket/Packet header is malformed, the payload is empty, every Subpacket is empty or all-Empty-atoms, the session cannot be resolved, or a control-session payload is not an SMUID method (Core §3.3.10.6 S2:S1 c/d, §3.3.10.7 a, Opal §3.3.4.1.3; what ETC-08/ETC-15 observe); `SessionAborted` -> the TPer's CloseSession is prepared (S2 -> S3) or nothing is (S1) when a resolvable session violates packet rules (§3.3.10.7 b; ETC-05/09/14 accept either); `Prepared` -> S2, `Tick` -> S3. S1 + IF-RECV -> (0,0,0); S2 + IF-RECV -> (0,1,0); S3 + IF-RECV with insufficient len -> (0,total,min) else deliver per Table 45; S2|S3 + IF-SEND -> SynchronousProtocolViolation, state unchanged. Opal T15: S-bit atoms rejected.
- Methods: control session: Properties (Opal minima echoed, host values clamped), StartSession/SyncSession (Admin SP as Anybody or none; SID with HostChallenge; SP_BUSY on a second write session; inactive Locking SP -> INVALID_PARAMETER; wrong HostChallenge -> NOT_AUTHORIZED), CloseSession. Regular session: Get (cell_block, ACE Columns filtering), Set (type-checked; `ReadOnly*` cells -> NOT_AUTHORIZED regardless of ACL), Next, Authenticate, GenKey, Opal Activate/Revert/RevertSP/Erase. ACL: AccessControl row -> ACL -> ACE BooleanExpr as a postfix list over Authority uidrefs (`evalACE`, Core Table 48); absent or unsatisfied -> NOT_AUTHORIZED. EndOfSession echoed.

### 8.3 Generated tables feeding it

`Hsed.Gen.Opal.Preconfig.AdminSP` exports Opal T18-T25, T27-T29, T33 as `[Pre r]` typed by the patched descriptions (T26 applied to TPerInfo); `LockingSP` likewise from T34-T42, T44-T48 (T43 applied to LockingInfo). `initialTPer :: SimParams -> TPer` folds them: C_PIN_SID `00 00 00 0B 00 00 00 01` PIN = `sidPin` or MSID; C_PIN_MSID `00 00 00 0B 00 00 84 02` PIN = `msid`; Locking C_PIN_Admin1..4 `00 00 00 0B 00 01 00 01..04`, User1..8 `00 00 00 0B 00 03 00 01..08`; Admin SP `00 00 02 05 00 00 00 01` Manufactured; Locking SP `00 00 02 05 00 00 00 02` at `lockingOFS`; Global Range `00 00 08 02 00 00 00 01`; MBRControl `00 00 08 03 00 00 00 01`. The ACL for `(C_PIN_MSID, Get)` comes from Opal T22 (ACE permitting Anybody on the PIN column), so UCT-03 step 1b succeeds unauthenticated and Get C_PIN_SID.PIN as Anybody is NOT_AUTHORIZED; at M4a the two AccessControl rows UCT-03 needs are hand-written with SpecRefs, replaced by the T22 reader at M4b. C-ACL-REFS: every AccessControl row names an existing MethodID row and ACE row. Status codes from Core T166 plus SIIS 0x13/0x14 as a hand-written extension.

### 8.4 Opal life cycle and resets

- Activate (`00 00 00 06 00 00 02 03`, Locking SP object, read-write Admin SP session as SID; ACL from T22): lifeCycle -> Manufactured; current C_PIN_SID PIN copied into Locking C_PIN_Admin1 (Opal §5.1.1.2; the same fold `lockingOFS = Manufactured` performs at boot); Level 0 LockingEnabled flips to 1. Revert on a Manufactured-Inactive SP has no effect (§5.1.2); Revert/RevertSP re-fold the generated preconfiguration; C_PIN_SID on TPer Revert follows 0x0203 byte 14.
- `Ev PowerCycle | HardwareReset | HotPlug`: sessions aborted (no CloseSession), pending responses dropped, sync -> S1, hostProps -> Opal T17 initial assumptions, non-static ComIDs -> Inactive, static ones -> Issued with ext 0, LockOnReset/MBRDoneOnReset/Tries rules applied. TPER_RESET does the same with `Programmatic` (Opal §3.2.3); STACK_RESET the per-ComID subset (Core §3.3.4.7.5). Each rule is a `Law`.

### 8.5 Scope

Tier 1 (M3-M4): Level 0, Properties, StartSession/SyncSession to the Admin SP, Get on C_PIN_MSID (`[3 = 3, 4 = 3]` -> `[[3 = msid]]`), EndOfSession, the S1/S2/S3 machine with Table 45 and the three outcomes, TPER_RESET/STACK_RESET, NOT_AUTHORIZED default, ETC-02/03/08/15/20 paths. M5: the Locking SP at either OFS, ETC-07/21, SPF-02:2, resets. Tier 2 (M6): Authenticate, Set on C_PIN_SID, Activate/Revert/RevertSP/Erase, ranges, GenKey, MBRControl/MBR (Opal §5.3 granularity), Next, transactions, Random, T230/T231 decision tables. Out of scope: Secure Messaging, Clock/Crypto/Log templates, asynchronous protocol, dynamic ComIDs, PSID beyond a hand-written row.

## 9 Host jig and test catalogue

### 9.1 Structure

`Hsed.Jig.Catalogue`: `data TestId = UCT Int | SPF Int | ETC Int` with titles and Test Cases pages (hand-entered; the PDF is untagged). `Hsed.Jig.Case`: `Case { tid, prereqs, waives :: [(Prereq, Text)], applies :: Level0 -> Bool, steps :: Script (), expect }`; the `Script` monad has spec-level verbs on generated `Method` records (`discover`, `properties`, `startSession`, `get`, `set`, `authenticate`, `activate`, `revert`, `endSession`, `rawIfSend`, `ifRecvWith (blocks 1)`, `powerCycle` (simulator only), `expectStatus`); expectations cite anchors from the Test Cases text and the normative spec.

*Corrected prerequisites.* `Hsed.Jig.Prereq` lists the common prerequisites of Test Cases §5.2 (PDF 29) and §7.2 (PDF 76) as data (`SyncIdle`, `NoSessions`, `LockingManufactured`, `SinglePacketProfile`, …). A case's effective set is the common list minus its `waives`, each with a reason printed in the report with the page; the runner checks the effective set and reports `skipped: prerequisite unmet` otherwise. UCT-01, UCT-02, UCT-03 step 1, SPF-02:1, ETC-02, ETC-03, ETC-08, ETC-15 waive `LockingManufactured` ("never touches the Locking SP"); ETC-20 replaces it with `LockingManufacturedInactive`. Without the waivers every tier-1 run would be skipped on `/dev/disk6` (Manufactured-Inactive) and on the simulator before M5. On the simulator `LockingManufactured` is `--locking-ofs manufactured` (M5) or an Activate profile (M6); on the drive it is an assertion from Level 0.

Catalogue by milestone: M3 UCT-01 (PDF 29), ETC-02 (77), ETC-03 (78), SPF-02:1 (52); M4 UCT-02 (31), UCT-03 1a-1c (32; 1d-1f set the SID PIN), ETC-08 (81), ETC-15 (88), plus an Admin-SP wrong-HostChallenge variant reported as `ETC-21/adminsp`; M5 ETC-20 (91), ETC-21 (92; Admin1 on a Manufactured Locking SP), ETC-07 (80; step 2 opens a Locking SP session), SPF-02:2 (53; Locking SP + DataStore Get), SPF-11 (64), SPF-12 (65), ETC-09/13/14; M6 the rest. ETC-01 (76) needs locked ranges and media I/O and is simulator-only until the write gate is lifted.

### 9.2 Same jig, three targets

`hsed-jig --target sim` (default), `sudo hsed-jig --target /dev/disk6` (kext, `'ReadOnly`), `hsed-jig --target ws://errol:8765/ws`. Only the `Transport` value changes. `--tests UCT-01,UCT-02,UCT-03:1a-1c`. Every run writes a hex transcript with decoded ComPackets and tokens (`build/report/jig-<target>-<date>.{json,md,xml}`); `hsed-jig diff a b` masks VU fields (serial, firmware, MSID, base ComID, VU Level 0 bytes, TPer property values). `--target sim+/dev/disk6` (Pair runner) drives both ends step by step. *Corrected:* for UCT-01 it first compares descriptor sets (every code on the drive must be on the simulator; 2.30-only codes the drive lacks, 0x0005 and 0x0404 on disk6, are `extra`, not failed; drive-only 0x0202/0x0402/0x0403 are satisfied by `hsed-sim --profile spec/fixtures/disk6-level0.bin`), then diffs fields per descriptor present on both with VU masking.

### 9.3 Safety against the attached drive

1. Type level: the kext backend only constructs `Transport 'ReadOnly`; `'ReadWrite` cases are skipped with the reason. Lifting requires a code change plus `--allow-write S59BNM0RA15802W` (trimmed IORegistry serial) plus `HSED_DRIVE_WRITE=1`.
2. Guard (`Hsed.Jig.Guard` over `Hsed.Gen.Guard`): every IF-SEND is parsed; proto 0x01 is forwarded only when every method is in {SMUID.Properties FF01, SMUID.StartSession FF02 with SPID = Admin SP and HostSigningAuthority absent or Anybody, Get 06..16 on C_PIN_MSID, bare EndOfSession}; TPER_RESET (ComID 0x0004), Set, Authenticate, Activate, Revert, RevertSP, GenKey, Erase, CreateRow, DeleteRow, Next and anything unparsable are refused before any CDB is built (`blocked by guard`). *Corrected:* `hsed-jig --recover` admits exactly HANDLE_COMID_REQUEST 0x00000002 (STACK_RESET) on the base ComID and its GET_COMID_RESPONSE as a logged pre-flight: the only non-destructive way to free ComID 0x1004 from S3 or an orphaned session after a crash (`sedutil-cli --TCGReset` sends STACK_RESET inside a protocol 0x01 ComPacket; otherwise only a USB power cycle helps). STACK_RESET touches sessions and buffers only (Core §3.3.4.7.5); Opal makes it mandatory.
3. Session mode: read-only is method-level, not Write=False (optional per Opal §4.1.1.2; the Test Cases baseline is read-write; sedutil opens Write=1, `DtaSession.cpp:162,227,289,355`); `--sessions read-only` tries Write=False first and reports rejection (§13.7). 4. Pre-flights: refuse while `sedutil-cli` or the SED ToolBox daemon holds the device; refuse a mounted `/dev/diskN` unless `--allow-mounted` (verify afterwards with `diskutil verifyVolume` and a prior file checksum). 5. MSID logged as length + sha1 unless `--show-msid`. 6. Hardware error paths (ETC-02/03, SPF-02:2) opt-in via `--allow-error-paths`: the RTL9210's behaviour under malformed IF-SENDs is unknown.

Read-only against `/dev/disk6`: M3 Level 0 only (IF-RECV, no IF-SEND) and the raw fixture capture; M4c UCT-01, UCT-02, UCT-03 1a-1c, SPF-02:1, with `--recover` available. Oracles before/after, never concurrently: `sudo sedutil-cli --query --json /dev/disk6` and `sudo sedutil-cli --printDefaultPassword /dev/disk6`.

## 10 Toolchain, repo layout, wapex and licence

### 10.1 Toolchain (M0; mostly in place)

- stack 3.11.1 is installed and set (stack >= 3.9.1 carries the 2025-07-24 Hackage root keys); the index is fetched. Nothing else to install.
- `stack.yaml`: `resolver: lts-21.21` (ghc-9.4.7 exactly), `system-ghc: true`, `install-ghc: false`, `compiler-check: match-exact`, `packages: [hsed-core, hsed-spec, hsed]` (the latter two from M1), `extra-deps: []`, no `allow-newer`. The staged `lts-21.25` + `compiler:` configuration already builds and tests green; the switch is a tidy-up verified by the scratch build and lands in the same commit as the staged changes.
- GHC 9.4 edits (staged, verified green): `PlainTV n SpecifiedSpec` under `MIN_VERSION_template_haskell(2,17,0)` (`Base_Type/TH.hs:101,173`, `Data/BoundedSize/TH.hs:147`); `Nat` from `GHC.TypeNats` (`Simple_Type.hs:71`, `Format.hs:42`, `TypeUIDs/TH.hs:79`); `(m <= l)` in `Data/BoundedSize/Class.hs:54,119`; base16-bytestring 1.0 shim in `Util.hs`.
- Pruning beyond the staged set: `hspec` to test stanzas (QuickCheck stays in hsed-core), drop `singletons`, `heredoc` to a `dev` stanza, review `lens`/`gdp`; add `aeson`, `attoparsec`, `base64-bytestring`, `websockets`, `wuss`, `warp`, `wai`, `wai-websockets`, `stm`, `async`, `optparse-applicative`, `crypton`. Codec fixes land in M0 with QuickCheck round trips. Python wheels pinned in `tools/extract/requirements.txt`; `make venv` recreates `~/hsed/.venv`.

### 10.2 Layout (all inside the hsed submodule)

```
hsed/
  stack.yaml                      lts-21.21, three packages
  hsed-core/   src/Hsed/Core/**   codec, Layout, Types, Table, TH (module map §4.1; renamed at M2d)
  hsed-spec/   src/Hsed/Spec/**   compiler library; app/hsedc; bindings/Hsed/Bindings/{Core,Opal}.hs (generated, committed, compiled
                                  ONLY by test:bindings); test/negative/*.hsa (fixtures that must fail, with expected messages)
  hsed/        src/Hsed/{Law,Wire,Transport,Sim,Jig}/**; gen/Hsed/Gen/** (generated, committed); app/{hsed-sim,hsed-jig}
  spec/ir/{core-2.01,opal-2.30}.json; spec/annot/<spec>.{hsa,upd,manifest.json}; spec/expected-diff.txt
  spec/fixtures/disk6-query.json (M0), disk6-level0.bin (M3)
  tools/extract/{extract,annotate,dump}.py, requirements.txt, render_check.sh; tools/land.sh
  tcg/                                        pristine PDFs, untouched; TCG-NOTICE.txt (verbatim TCG licence text)
  docs/{spec-as-source.md (this), wire.md, annotation-grammar.md, LEDGER.md (generated)}
  build/                                      gitignored: *.hsed.pdf, ir/, report/
```
Untrack and gitignore `doc/` (haddock), `TAGS`, `project_tags`, `unpacked/`, `unpacked_tags`, `hsed.pdf`, `hsedTred.pdf`, `*.graph` (keep `makegraphs`). New code is `Hsed.*`; `System.SED.MCTP.*` stays until M2d (§13.13).

### 10.3 Bootstrap on a clean clone

`stack build hsed-core` -> `stack build hsed-spec` (library + `hsedc`; no generated code, no PDF, no Python) -> `stack test hsed-spec:bindings` (compiles the committed Bindings) -> `stack build hsed` (compiles `gen/`, runs the binding-check splice). A wrong comment breaks only the `bindings` component; `hsedc dump`, `check`, `explain`, `annotate` stay available to fix it. A broken `gen/` never stops `hsedc compile`; `law`/`behaviour` names are strings in hsed-spec, resolved only in hsed's build.

### 10.4 wapex and submodule hygiene

*Corrected:* the landing is `tools/land.sh`, run at the end of every working session, because a wapex re-sync (`submodule update --init --force` with `submodule.hsed.update` unset) leaves hsed detached and discards its index and working tree; commits survive, staged edits do not. The script: (1) `git -C ~/hsed symbolic-ref -q HEAD || git -C ~/hsed checkout -B feature/types`; (2) commit in hsed with explicit paths; (3) `git -C ~/hsed push origin HEAD:feature/types`; (4) refuse the pointer bump unless `git -C ~/hsed branch -r --contains HEAD | grep -q origin/feature/types`; (5) in `~`: `git add hsed && git commit` on `work` -> `git push origin work` (wapex). GitHub push daily at least; pointer bump and wapex push per rung. Set `git config submodule.hsed.update checkout` explicitly and note in CLAUDE.md that re-syncs leave hsed detached. No new submodule and `.gitmodules` untouched unless libsed needs a fork (then itself a landing); `~/mh_transport` untouched. Before any re-sync on this Mac: commit or stash the six modified tracked `.claude/**` files and the three untracked memory files, decide `HUD/`; CLAUDE.md's ".claude still untracked" note is stale. Never `git clean`; no AI attribution trailers.

### 10.5 Licence

All three TCG PDFs carry the same terms (Core p.ii, Opal p.1, Test Cases p.1): copying and distribution are permitted for examining or implementing TCG specifications provided the disclaimers, notices and licence terms travel with the document. *Corrected:* the public repo already relies on that clause (19 tracked files under `tcg/`, including the pristine PDFs and a 769 KB text dump of Core), as do the committed IR cells, generated labels and `ERRATA.md` quotes; what the clause requires and the repo lacks is the notice. So `tcg/TCG-NOTICE.txt` carries the verbatim TCG text, every generated module header and `spec/ir/*.json` (`notice`) point at it, and README gets one line. Shipping `.upd` + manifest rather than a modified PDF stays the default because Scott asked for it ("prefer shipping the annotation set as an incremental-update file … if in doubt"); it costs one byte-prefix gate that doubles as the determinism check. Going further either way is §13.6. sedutil is GPLv3: a `-flibsed` jig is distributed under GPLv3 or not at all; the default build stays BSD3. Opal's docinfo carries AIP labels; the tail does not touch docinfo.

## 11 Milestones

Each rung ends green (`stack test`), with `tools/land.sh` run, and with the acceptance below; each has at most one unproven piece.

**M0 Green build, honest codec, CI, hygiene.** Deliverable: commit the staged tree with stack.yaml on lts-21.21 (§13.1); codec fixes; `make check`; CI build-test; bloat untracked; `tcg/TCG-NOTICE.txt`; `tools/land.sh`; `spec/fixtures/disk6-query.json` from `sudo sedutil-cli --query --json /dev/disk6` (the raw blob needs the M3 kext path). Acceptance: `stack --system-ghc test` green locally and in CI with zero GHC downloads (already true); `-ddump-splices` of TypeUIDs.hs matches `dev/Column_Types.txt` modulo TH 2.17 syntax; `SMUID.Properties[]` encodes to `F8 A8 00 00 00 00 00 00 00 FF A8 00 00 00 00 00 00 FF 01 F0 F1 F9 F0 00 00 00 F1`; `land.sh` refuses a pointer bump for an unpushed commit (dry run).

**M1 Vertical slice: Core Tables 46-56 and 75.** Deliverable: package split (`hsed-core`, `hsed-spec` with `test:bindings`); `tools/extract/extract.py` promoted with the cascade, repairs, ragged join and IR v1 over the whole Core PDF; `hsedc` with `Form`, `Shape`, `Anchor`, `Grammar`, `DSL`, `Rules` (`typeRow`, `enumValues`), `Format`, `Check`, `Gen`, `Sidecar`; `core-2.01.hsa` with `doc core201`, the §5.1.3 rule anchor and the T75 erratum; negative fixtures `wrong-shape.hsa` (feature on T46) and `unknown-name.hsa`; `annotate.py`/`dump.py`; `gen/Hsed/Gen/Core/Types.hs` for T46-T56; CI regen-diff and pdf-checks jobs. Acceptance: (a) the pristine PDF is an exact byte prefix of `build/core-2.01.hsed.pdf`, `cat tcg/Core.pdf spec/annot/core-2.01.upd` reproduces it, tail <= 400 bytes per anchor plus one xref stream; (b) gs/MuPDF/sips renders of pages 1, 100-102, 107 identical to pristine on the same machine; (c) three readers agree, `dump(apply(upd)) == hsa`, `annotate(hsa) == upd`; (d) `uAC_elementType`, `fAC_elementType`, `Core_adv_key_mode` equal today's library at runtime, C-ENUM-RANGE passes for T51/T52; (e) `--errata published` reports `E-FMT-RESOLVE Fractoin` at `core-2.01.pdf#page=107&T75`, `--errata scott` resolves it; (f) `wrong-shape.hsa` fails `hsedc check` with `E-SHAPE core-2.01.pdf#page=100&T46` and `unknown-name.hsa` fails the `bindings` component with a GHC error citing the page, while `stack build hsed-spec` stays green; (g) `hsedc explain core 46` prints `typeRow (exemplar core T46)`; (h) the §5.1 invariants hold; (i) the extractor's records are deep-equal to `spec/ir/core-2.01.tables.json`.

**M2a Whole-Core type layer and the runtime oracle.** `typeRow`, `enumValues`, `tableUIDs` over all of Core; T87 and T101 comments; `gen/Hsed/Gen/Core/{Types,Enums,TableUIDs}`; the equivalence program over every definition; `spec/expected-diff.txt`; `dev/TableUIDs.txt` regenerated. Acceptance: runtime comparison differs by exactly `spec/expected-diff.txt`; every `dev/Formats.txt` string parses under `--errata scott`; T240 halves pass on 46 rows; `git diff --exit-code gen/` after a second `hsedc gen`.

**M2b Remaining data rules; E-UNBOUND empty.** `tableDesc`, `uidTable`, `statusCodes`, `codes`, `fieldValue`, `fieldType`, `constValues`, `ranges`, `preconfig` (T209/T216), `tokenEncoding`, `terminology`, `sig`; the T45/T167/T168 comments and the four `prose`; signatures joined to T242/T241; findings/coverage/ERRATA/ledger reports. Acceptance: `hsedc orphans` empty and `hsedc ledger` reports 221 rule-bound / 12 comment-bound for Core; T233 has 14 columns and T243 17 authorities; LOT-VS-BODY reports Table 185; XREF-RESOLVES reports `Table 179` at PDF 116; C-UID-RANGE passes against T237; T4's token table equals `Hsed.Core.Token`'s constants.

**M2c Byte grids, features, pins.** `byteGrid`, `byteFieldValue`, `feature` with the extent rule and three `pins` schemas; `Hsed.Core.Layout` and `Hsed.Core.Packet`; `gen/Hsed/Gen/Core/{Layouts,Features,Codes}`. Acceptance: C-LAYOUT-TILE passes for T17 (20), T18 (24), T20 (12), T39 (48), T33 (46); TPerFeature/LockingFeature bit maps equal `DtaStructures.h:81-134` (T43 byte 4 via x-extents); visual spot checks of T17/T18/T20/T39/T42/T43 recorded; T44 yields `W-PINS-NONE`, not an error.

**M2d Rename and retirement.** `System.SED.MCTP.Common.{TypeUIDs,TableUIDs,SpecialPurposeUIDs}` become re-exports of `Hsed.Gen.*`; rename to `Hsed.*`; quasiquoters and `fXType` retired; §13.15 rename if chosen. Acceptance: `stack test` green; `git grep '\[ttype|'` empty; expected-diff.txt shrinks to the four UID and two caption fixes.

**M3 Wire contract, in-process TPer, UCT-01; Level 0 on the drive.** `Hsed.Wire` (corrected loop, `transferLength`), `Hsed.Transport.InProc`, `Hsed.Sim` Level 0 (generated layouts + Opal T2-T8 pins, the first Opal comments: `doc opal230`, `rule feature`, `refines`), the sync machine with Table 45, the three outcomes and `Tick`, the unknown-ComID branch, `Hsed.Transport.MacKext` at `'ReadOnly`, `Hsed.Jig` skeleton with `Prereq`/waivers, Guard, Transcript, Pair runner with the descriptor-set check, UCT-01, ETC-02, ETC-03, SPF-02:1; `Hsed.Law.Sync` (`ifSendWhileBusy`, `discardToS1`, `ignorePacket`, `table45`); `gen/Hsed/Gen/LawsSpec.hs`; `spec/fixtures/disk6-level0.bin` captured through the kext and decoded by the generated features. Acceptance: `hsed-jig --target sim --tests UCT-01,ETC-02,ETC-03,SPF-02:1` passes (second IF-SEND -> SynchronousProtocolViolation; IF-RECV in S1 -> (0,0,0), in S2 with `--hold-processing` -> (0,1,0); an all-Empty-atom subpacket leaves S1 and the next IF-RECV is (0,0,0)); `sudo hsed-jig --target /dev/disk6 --tests UCT-01` passes with NO IF-SEND issued (transcript-asserted), its waiver printed, its decode equal to `disk6-query.json`; the Guard refuses a scripted Set with the kext never called; LawsSpec runs every Law; the Pair runner reports `extra 0x0005, 0x0404` and only VU differences with `--profile`.

**M4a Properties, session manager, Get MSID on the simulator.** Session Manager (Properties with `propsReq`/`propsInitial`, StartSession/SyncSession incl. SP_BUSY and wrong HostChallenge, CloseSession), SP dispatch with two hand-written AccessControl rows for UCT-03, Get with cell_block, EndOfSession, TPER_RESET/STACK_RESET; `behaviour`/`fails` for §5.2.2-5.2.3 and §5.3.3.6; jig UCT-02, UCT-03 1a-1c, ETC-08, ETC-15, `ETC-21/adminsp`. Acceptance: UCT-02 returns TPer properties >= 2048/2028/1992 and echoes host 4096/4076/4040; UCT-03 1a-1c returns `[[3 = msid]]` and closes; Get C_PIN_SID.PIN as Anybody -> NOT_AUTHORIZED; ETC-08/ETC-15 observe (0,0,0) next; SID with a wrong challenge -> NOT_AUTHORIZED.

**M4b Opal Admin SP from the PDF.** `legend` (two-signal classifier), `rule preconfig`, the AccessControl/ACE reader (T22/T23), `enumInDescription`, `expand`, `columnsAddition` (T26); T14/T15 `narrows`, T16/T49 `replaces`; `gen/Hsed/Gen/Opal/{Comms,Types,TableDescs,Preconfig.AdminSP}` replacing the hand ACL rows; the binding-check splice. Acceptance: every cell of T18-T29/T33 maps and each off-legend fill is `W-LEGEND-OFFSCALE` (14 expected); every ACE/Authority/C_PIN/MethodID name in T22-T25 resolves and C-ACL-REFS passes on T22's 34 rows; T27 passes C-PRECONF-HEADER with 9 columns; UCT-03 and the NOT_AUTHORIZED case pass unchanged with the generated ACL; every `law`/`behaviour` resolves two-way; Pair runner diff for UCT-02 shows only VU values.

**M4c Hardware: Properties and MSID read-only.** `--recover`, `--allow-mounted` pre-flights, FAT32 checksum. Acceptance: `sudo hsed-jig --target /dev/disk6 --tests UCT-02,UCT-03:1a-1c,SPF-02:1` passes read-only with MSID equal to `sedutil-cli --printDefaultPassword`, no IF-SEND outside the allow-list (Guard log), waivers printed, FAT32 unchanged; a deliberately interrupted run followed by `--recover` succeeds.

**M5 Locking SP overlay, resets, LAN leg.** T33-T48 (T38 over PDF 56-78; T36 Kind column replacing `byteTables = ["MBR"]`; T43), T30-T32, T50; `gen/Hsed/Gen/Opal/Preconfig.LockingSP`; `--locking-ofs manufactured`; reset laws (LockOnReset, MBRDoneOnReset, Properties, Tries, ComID -> Issued); `Hsed.Transport.Mhtx.{Client,Hub}` with heartbeat flags, `hsed-sim --listen`, `docs/wire.md`; jig ETC-20, ETC-21, ETC-07, SPF-02:2, SPF-11, SPF-12. Acceptance: all 31 preconfiguration tables pass C-PRECONF-HEADER (T44 with 11 columns) and C-LEGEND-TOTAL; ETC-20 -> INVALID_PARAMETER at the default OFS; ETC-21 -> NOT_AUTHORIZED and ETC-07 step 2 succeeds with `--locking-ofs manufactured`; SPF-02:2 with `blocks 1` -> "insufficient transfer length" in both unit modes; conformance passes all 8 checks with `--interval-ms 200 --timeout-ms 600`; `--target ws://127.0.0.1:8765/ws` reproduces the `--target sim` transcript; a mid-run restart fails pending requests and the client reconnects with a new server_id; a non-increasing id gets `protocol_error`, a bogus frame nothing; the same run from errol over the LAN passes; `reset power_cycle` then Get Locking.ReadLocked shows LockOnReset; nothing under `~/mh_transport` modified.

**M6 Tier 2, Linux leg, Haskell-native PDF path, re-anchoring.** Authenticate, Set on C_PIN_SID, Activate/Revert/RevertSP/Erase, ranges, GenKey, MBRControl/MBR, transactions, Random, `decision` on T230/T231; jig UCT-03 full, UCT-04..07, UCT-10..13, SPF-01..05, ETC-04..24 on the simulator; libsed external + `Hsed.Transport.LibSed` behind `-flibsed`; `Hsed.Spec.Pdf` writer on pdf-toolbox-core 0.1.1; `hsedc reanchor` against Opal 2.01; `--target doc`. Acceptance: listed cases pass in-process and over ws:// with laws for Activate (Manufactured; C_PIN_Admin1.PIN == C_PIN_SID.PIN) and Revert; libsed passes UCT-01/02/03:1a-1c read-only on errol against a listed test drive with the five statuses differentiated from sense/NVMe status; the Haskell tail is object-equal and render-identical to pypdf's; reanchor maps every Core-shaped anchor from Opal 2.01 to 2.30 with zero silent guesses; no write-profile run touches a real drive without `--allow-write <serial>` from Scott.

## 12 Risks

- **PDFMaker's tagging is wrong in known places** (merged cells, stray fragment, dangling rows, all-/TH and no-/TH tables). Repairs and the header cascade are proven on today's extraction, flagged per cell and guarded by invariants; a new case surfaces as `E-GRID` or a failed invariant, never as a silently empty schema.
- **Span recovery for grids.** No cell carries /A. MSB/LSB rows, the eight-cell rule and x-extents cover T17/T18/T20/T39/T42/T43 and Opal T2-T8; `bits` remains for the rest. Mitigation: M2c spot checks, the `DtaStructures.h` oracle, the M3 fixture.
- **Opal AccessControl/ACE tables (T22, T23, T38, T39)** are the ACL source: informative columns, group rows, wildcards, footnotes, 8- and 23-page spans. Row counts are known (45/16/96/46); M4b de-risks M5; C-ACL-REFS cross-checks every row.
- **Legend recovery** rests on Word's fills and fonts; two independent signals plus text markers, ±8 tolerance, every off-legend fill reported; C-LEGEND-TOTAL fails loudly on an unclassifiable style.
- **GHC 9.4 port**: the staged tree is green; residual risk is untested `(m <= l)` paths. Fallback: `(m <=? l) ~ 'True`, then dropping the `AtLeast` synonym.
- **Kext path**: no sense, root + exclusive open, 625 vs 626 with a differing `DTA_DEVICE_INFO` layout (not bound), rebuild on macOS 27 unverified; ETC statuses certifiable on the simulator or on errol via libsed.
- **Recovery on the drive**: a crash between IF-SEND and IF-RECV strands ComID 0x1004; `--recover` is the explicit lever. **Bridge behaviour** under malformed IF-SENDs is unknown; error paths opt-in.
- **Feature-set gaps**: 0x0202, 0x0402, 0x0403, PSID have no PDFs in `tcg/`; UCT-01 4b/4c, UCT-16, SPF-17/19 depend on hand-written descriptors; parity runs use `--profile`.
- **Catalogue prerequisites vs this drive**: every `LockingManufactured` case is simulator-only until Scott lifts the gate or designates a second drive (§13.7); waivers keep reports honest.
- **wapex re-sync** detaches hsed and discards its index; `tools/land.sh` per session, daily GitHub push, pointer bump per rung; the `.claude` files and `HUD/` are Scott's to settle.
- **Licence**: the repo already redistributes TCG text; the notice file is the obligation (§13.6); `-flibsed` builds are GPLv3. **mh_transport churn**: the binary-frame fix stays out of this program's commits.

## 13 Decisions that need Scott

1. **Staged, uncommitted hsed changes.** They build and test green. Recommended: commit them with stack.yaml switched to `lts-21.21` (no `compiler:`, no `allow-newer`) in the same commit; alternatives: commit as-is, or unstage. No destructive git from a Claude session.
2. **Resolver**: lts-21.21 / GHC 9.4.7 (recommended, zero downloads, verified) versus lts-22.x (GHC 9.6) or LTS 24 now.
3. **Canonical artefact**: pristine PDF + annotation set as the source, annotated PDF and `.hsa` as equivalent renderings kept equal by CI, `.upd` shipped (recommended); confirm this reading of "the annotated PDF compiles".
4. **Payload location**: `/Contents` + `/HSED` identity only, redundancy in the manifest (recommended) versus `/HSED` only or the fuller `/HSED` of the previous draft.
5. **Errata policy**: two sets (`published`, `scott`, seeded from `tcg/Core Spec errata.txt`) with findings auto-proposed (recommended), or a single corrected build.
6. **Redistribution**: (a) rely on the TCG licence as the repo already does, add `TCG-NOTICE.txt`, keep shipping `.upd` (recommended, your stated preference); (b) also commit `build/<spec>.hsed.pdf` and drop the `.upd` indirection; (c) IR build-only and the PDFs out of the public repo. The notice is added in every case.
7. **Read-only on `/dev/disk6`**: method-level allow-list with Write=True sessions (recommended) versus Write=False versus IF-RECV only; whether `--recover` may run without asking each time; whether the FAT32 volume may ever be wiped so tier 2 and the `LockingManufactured` cases run on hardware, or a second empty drive is the target.
8. **Hardware error paths** (ETC-02/03, SPF-02:2 on the RTL9210): opt-in when you say, or simulator-only.
9. **Reference SSC level**: Opal 2.30 (recommended, `--profile` for parity with the 970 EVO Plus) versus a 2.0x profile.
10. **Feature-set PDFs**: add DataStore, Block SID, Namespace Locking and PSID specs to `tcg/`, or accept `Hsed.Sim.FeatureSets` as hand-written.
11. **SIIS 1.20 and the Test Cases**: geometry-mode extraction later, or cited constants forever.
12. **Kext user-client extension** (SCSI status + autosense) and a macOS 27 rebuild before M6, or leave to errol/libsed.
13. **Package split and namespace**: three packages with `hsed-spec:test:bindings` (recommended) versus a fourth package or a separate repo; rename `System.SED.MCTP.*` to `Hsed.*` at M2d (recommended) or keep.
14. **Branch**: continue on `feature/types` or create `spec-as-source` and update `.gitmodules` (needs a wapex push of the home repo).
15. **Generated naming**: keep `uXType`/`fXType`/`Core_X` and dEnum's suffix rule for the clean M2a comparison, rename at M2d (recommended), or rename immediately.
16. **Behaviour binding granularity**: per method heading plus `fails` (recommended) versus per clause; whether every `law` must cite a test id.
17. **`*` placeholders and VU values**: `SimParams` at start (default) versus `param` comments in the Opal PDF, or both with the PDF winning.
18. **mh_transport Python hub binary-frame fix**: file and ship separately now, or leave.
19. **GUI test of hidden annotations** in Acrobat Reader and Preview (comments panel, Preview Save, `hsedc adopt` of a typed sticky note) before M1 acceptance: needs a human; recommended.
20. **Old hand-transcribed prose** in TypeUIDs.hs/Token.hs: delete at M2d or keep under `dev/`.
21. **`--target doc`** early (M4) or after the simulator works.
22. **Home repo hygiene now**: commit the six modified `.claude` files and three untracked memory files, decide `HUD/`, set `submodule.hsed.update checkout`, correct CLAUDE.md's stale note before the next re-sync.
23. **libsed hosting**: an upstream DTA branch (if you have push rights), your own repo, or a fork registered in `.gitmodules`; whether a GPLv3 `-flibsed` build is acceptable.
24. **Rule anchoring style**: document-wide rules on the `doc` annotation (recommended: fewest comments) versus a `rule` anchor at every governing heading.

## 14 Review log

Three skeptics, all `sound-with-fixes`. Evidence was re-checked this round (`classify.py` re-run; `git`, `stack`, sedutil, mh_transport, Core/Opal/Test Cases text and the extraction JSON inspected directly). Every objection was accepted; references say where the change landed.

| # | objection | sev. | what changed |
|---|---|---|---|
| S1-1 | Literal rules leave 79 Core / 15 Opal tables unbound; uidTable matches none of §6; byteGrid fires on 10; tokens, T168, T212, T179, T188-T204, T209/T216, T235-T238, Opal T7/T10-T12/T26/T43 unaccounted | major | 18 rules re-derived from the classifier (`codes`, `fieldValue`, `fieldType`, `byteFieldValue`, `constValues`, `ranges`, `columnsAddition`, `tokenEncoding`, `terminology` added; `uidTable` re-specified; `byteGrid` doc-wide; `preconfig` covers Core default authorities); ledger 221/12, 45/5; `hsedc orphans` is the acceptance test (§3.4, §3.6, M2b). |
| S1-2 | All-/TH tables become header-only; six tables have no /TH; three corner-cell spellings | major | Header cascade with `header_source`, corner normalisation, invariants T233/T243/T244/T245/T7 (§3.4, §5.1, E5, E6). |
| S1-3 | PDFMaker mis-tagging breaks four typeRow tables and empties two schemas | major | Three repair passes mandatory with per-cell `Repair` flags; T71/T81/T189 invariants (§5.1, §4.2, E2, §12). |
| S1-4 | Empty-first-cell merge collapses 163 Opal rows, never fires on T17 | major | Shape-conditional ragged-row join; tests T38 96, T22 45, T17 11 rows (§5.1, E14). |
| S1-5 | No black fill; NotDefined sample is white; nearest-sample mislabels 838 cells; (N) textual; 218/235 silent | major | §1 corrected; classifier = shading band ±8 + bold/underline + `(N)` markers; `W-LEGEND-OFFSCALE` (E11, M4b). |
| S1-6 | T23/T39 informative column and group rows; Opal T26/T43 missing; `Colum Type` | major | `columnsAddition` rule; informative-column drop generalised; typo table (§3.4, E5, E12, Q3). |
| S1-7 | Shape index contradicts override comments; `KeyValue` catch-all; `rangeWords` conjoined | major | `Form` vs `Shape`, `accepts` -> `E-SHAPE`; one shape per reader; modifiers applied, `&` joins only; T13 by `fieldValue` (§3.3, §4.2, E4, E13). |
| S1-8 | byteGrid/feature double-fire; `pinsFrom` fails on T39/T41/T44; Opal pins are bullets | major | `refines` precedence; `feature` limited to `Feature Descriptor` captions; `pins` with three schemas; absence = `W-PINS-NONE` (§3.4, E7, E8). |
| S1-9 | IR restricted to anchored paragraphs cannot feed prose readers | major | Words emitted for every reader-consumed section (`wordComplete`) (Q2, §5.1, §3.1). |
| S1-10 / S3-1 | Generated Bindings inside the package that builds `hsedc`; M1(f) breaks "ends green" | major | Bindings in `hsed-spec:test:bindings`; `hsedc` parses with `Hsed.Spec.Grammar`; wrong comment is a negative fixture (§3.3, §4.2, §5.2, §10.2-10.3, M1 (f), CI f). |
| S1-11 | Scope stated twice; Opal lacks anchors for T30-T32 | minor | Anchors are the only scope source; `C-RULE-ANCHORED`; anchors at Opal §4.2.5 (§3.4, E1, §13.24). |
| S1-12 | Positional bits hold for T42 only; T43 byte 4 has seven cells; Opal T3 displaces the byte number | minor | Extent rule 8 / 1 / x-extents / `bits`; displaced-cell normalisation (Q3, E8, §5.1, M2c). |
| S1-13 | E4 naming contradicts dEnum and M1(d); `dev/TableUIDs.txt` stale | minor | dEnum's names until M2d; runtime equivalence program is the oracle (E4, §5.5, M2a). |
| S1-14 | "The PDF is the source" overstated; `adopt` unspecified; derived artefacts drift; `/HSED` redundancy; ordinals | minor | Source = pristine PDF + annotation set; `hsedc gen` under a pre-commit hook; `adopt` specified; `/HSED` shrunk (Q1, §3.1, §5.2, §5.3). |
| S2-1 | Blanket "Locking SP Manufactured" prerequisite skips every M3/M4 case on both targets | major | Per-case prerequisite sets with printed waivers; `SimParams.lockingOFS` (Q7, §9.1, §8.1, M3). |
| S2-2 | ETC-21 unreachable at M4 | major | ETC-21 at M5 with `--locking-ofs manufactured`; M4a runs `ETC-21/adminsp` (§9.1, M4a, M5). |
| S2-3 | Sync machine omits the S2:S1 discard transitions seven ETC cases expect | major | `Outcome = Prepared | Discarded | SessionAborted` bound as Laws; ETC-08/15 at M4a, ETC-07 at M5 (Q6, §8.2, E10, M3). |
| S2-4 | A shim over `DtaDrive::sendCmd` cannot differentiate statuses | major | Shim at the `DtaOS::Perform*Command` boundary; CDBs in Haskell (Q5, §7.3, M6). |
| S2-5 | `TPerUpdate` copies a blob in an old layout; `mallocForeignPtrAlignedBytes` is in `GHC.ForeignPtr` | minor | `TPerUpdate`/`DTA_DEVICE_INFO` dropped; IORegistry properties; import corrected (§1, §7.2). |
| S2-6 | `protocol_error` on any frame fails check 3; check 7 skipped at defaults | minor | `protocol_error` only for `sed.req`; heartbeat flags; `--interval-ms 200 --timeout-ms 600` (§6.3, §7.5, CI h, M5). |
| S2-7 | IF-RECV loop stops on MinTransfer with data (sedutil's bug) | minor | Loop while OutstandingData /= 0, re-issue on MinTransfer regardless (Q4, §6.1). |
| S2-8 | UCT-01 "only VU differences" impossible given disk6's descriptor set | minor | Descriptor-set check then per-descriptor diff; `hsed-sim --profile` (Q7, §8.1, §9.2, M3). |
| S2-9 | Blocking all of protocol 0x02 leaves no recovery lever | minor | `hsed-jig --recover` admits STACK_RESET on the base ComID, logged; TPER_RESET stays blocked (§9.3, M4c). |
| S2-10 | Static ComIDs not `Associated` without sessions; unknown ComIDs mis-answered | minor | `Issued`/`Associated` semantics; unknown-ComID branch; InvalidSecurityProtocolId only for unknown protocols (Q6, §8.1-8.2, §8.4). |
| S2-11 | SPF-02:2 needs a Locking SP; "transfer length 1" is one block | minor | SPF-02:1 at M3, :2 at M5; `transferLength`/`blocks 1`; Bytes-unit len < 20 is "insufficient" (Q4, §6.1-6.2, §9.1, M5). |
| S3-2 | Landing sequence silently fails after a re-sync; weeks of exposure; `.claude` count and `HUD/` | major | `tools/land.sh` (symbolic-ref guard, `push HEAD:feature/types`, contains-check, per-session commits, daily pushes); `submodule.hsed.update checkout`; hygiene item corrected (§1, §10.4, §13.22). |
| S3-3 | M0/M2/M4 are big-bang steps | major | M2a-d, M4a-c; package split at M1; raw fixture at M3 (the decoded `--query --json` fixture stays at M0); M0 reduced (Q9, §11). |
| S3-4 | Toolchain facts stale; lts-21.21 cosmetic | minor | §1, §10.1, §13.1 rewritten; lts-21.21 is a tidy-up in the staged commit, verified by the scratch build. |
| S3-5 | Module map unstated; orphan Arbitrary instances | minor | Explicit map in §4.1; QuickCheck kept in hsed-core. |
| S3-6 | `.upd`-only shipping buys no legal cover; the gap is the missing TCG notice | minor | Fixed in substance, decision kept: `TCG-NOTICE.txt` plus pointers; `squash` dropped; `.upd` retained as Scott's recorded preference with (a)/(b)/(c) in §13.6 (§10.5). |
| S3-7 | libsed from sedutil sources makes the jig GPLv3; the fork is unreachable without `.gitmodules` | minor | External library behind `-flibsed` (default off), declared GPLv3; hosting and `.gitmodules` in §13.23, §10.4 (§7.3, §10.5). |

Verified and left unchanged (skeptic 1's list): /F 6 render identity, pypdf incremental tails, pdf-toolbox-core 0.1.1 in lts-21.21, Opal 2.01 tagged, T243 "Issuers", Opal T24's header = Core T178's, §3.3.10.2.1 item 3.a is its own /LI, the Get signature is consecutive /P elements, the struct ordinals.