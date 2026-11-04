# From the TCG PDFs to a simulator and a host jig

This guide walks the whole path in order: the published PDFs, their hidden annotations, the extracted IR,
the spec compiler `hsedc`, the generated Haskell, the simulator and the host test jig. It says what each
stage reads and writes and which command runs it. The design of record is
[spec-as-source.md](spec-as-source.md): this guide points into it rather than restating it. To add a rule
to the compiler, see [adding-a-rule.md](adding-a-rule.md); for the IR's format, see [ir-v1.md](ir-v1.md).

State as of 2026-11-03 (M5a): the chain runs end to end for Core 2.01 and for all of Opal 2.30, every
Opal table and signature block bound: its Admin SP and Locking SP, Properties, feature descriptors, and
Tables 14-16 over Core's. The simulator holds both SPs, the Locking SP Manufactured-Inactive or
Manufactured. The Locking SP's
jig cases are M5b's and mh_transport over a LAN is M5c's. Writes to a real drive, the Linux leg and a
Haskell-native PDF reader are M6's (design §11).

## The chain at a glance

```
tcg/*.pdf  (pristine, never modified)
   |  spec/annot/<spec>.hsa          the annotations, one line each, as a sidecar
   v  make annotate                  tools/extract/annotate.py
build/<spec>.hsed.pdf                pristine + one incremental update of hidden annotations
   |                                 (committed as spec/annot/<spec>.upd and .manifest.json)
   v  make ir-annotated              tools/extract/extract_tables.py
build/<spec>.hsed.json               IR v1: every table, heading, paragraph and annotation
   |
   v  make gen                       hsedc bind, check, compile
hsed-spec/bindings/Hsed/Bindings/<Spec>.hs    the annotations as typed Haskell
hsed/gen/Hsed/Gen/{Core,Opal}/*.hs            the generated modules
docs/LEDGER.md, docs/LEDGER-opal.md           what binds each table
   |
   v  stack build hsed
the simulator (Hsed.Sim.*) and the jig (Hsed.Jig.*, exe hsed-jig), talking only through
serialized command and response bytes (Hsed.Wire) over a transport
```

`make check` runs the whole chain and holds every committed output to what the chain produces today.
It is the local form of CI.

## 0. Setting up

- **Toolchain:** stack 3.11.1 with ghcup's GHC 9.4.7, resolver lts-21.21 (design §10.1). Run stack only
  inside the checkout: outside a project directory, ghcup's stack hook installs a GHC of its own.
- **Python:** `make venv` builds `.venv` (Python 3.11 or later, from `tools/extract/requirements.txt`).
  The extraction tools always run as `.venv/bin/python -I`.
- **A clean clone builds without Python or the PDFs** (design §10.3), because the bindings and generated
  modules are committed: `stack build hsed-core`, then `stack build hsed-spec`,
  then `stack test hsed-spec:bindings`, then `stack build hsed`.

## 1. The PDFs (`tcg/`)

The published PDFs are the source text and are never modified: Core 2.01, the Opal 2.30 SSC, the Opal
Test Cases 1.02, and the documents they cite. A page is named by its PDF page number. Core's printed page
numbers are 17 less than its PDF page numbers; Opal's are 1 less.

## 2. Annotations: `spec/annot/<spec>.hsa`

Each annotation is one line of the sidecar: `spec | anchor | payload`. For example, from Opal's twelve:

```
opal-2.30 | doc | doc opal230 & rule feature & rule legend & ... & rule preconfig & rule sig
opal-2.30 | T2 | prose
opal-2.30 | T14 | narrows (core 26)
opal-2.30 | T16 | replaces [core 128]
opal-2.30 | T17 | propsReq
opal-2.30 | T49 | enumInDescription (replaces [core 102, core 103])
```

`narrows (core 26)` and `replaces [core 128]` name a Core table: Opal's run classifies over Core's, so the
binding takes the shape Core binds its table to (design E14).

- **Anchors** are `doc` (the whole document), a section such as `4.2`, a table `T<n>`, or a sentence or
  list item (design §3.2).
- **Payloads** are rule names and modifiers, the smart constructors of `Hsed.Spec.DSL` (grammar in
  design §3.3). Most tables need no annotation at all: a document-wide `rule` on the `doc` line binds
  every table that rule recognises. Annotations stay minimal: Core has 24, about 287 bytes each, and Opal
  12, about 246 (budget in design §3.6).
- **Errata:** Scott's corrections to the text are `erratum scott ["old" ~> "new"]` modifiers, held to
  `tcg/Core Spec errata.txt` by a test. Opal's slips are not errata but readings in the compiler, which cost
  no annotation (Scott's answer of 2026-11-03; see "Read a slip of the PDF" below).

In the PDF, each annotation is a hidden text annotation (flags `/F 6`) whose `/Contents` is the payload,
added in one incremental update after the pristine bytes. An ordinary viewer shows the document unchanged
(design §3.1).

**To change an annotation:** edit the `.hsa` line, then run `make annotate`. That rewrites
`spec/annot/<spec>.upd` and `.manifest.json`, which are committed with the sidecar.

## 3. The annotation gates: `make annotate`, `make pdf-checks`

`make annotate` runs `tools/extract/annotate.py`, which writes `build/<spec>.hsed.pdf`.

`make pdf-checks` (design §5.4) then proves the annotated PDF still reads unchanged:
- its bytes begin with the pristine file's;
- it is exactly the pristine file plus the committed `.upd`, and fits the tail budget;
- it renders identically in Ghostscript, MuPDF and sips where present;
- three PDF readers read the annotations back;
- dumping the annotations reproduces the sidecar, and annotating reproduces the `.upd`.

`tools/extract/dump.py` dumps an annotated PDF's annotations back to a sidecar.

## 4. Extraction to IR: `make extract`, `make ir-annotated`

`tools/extract/extract_tables.py` reads a PDF's structure tree and writes IR v1 JSON
([ir-v1.md](ir-v1.md)): every numbered table with its cells, fonts and colours, every heading and
paragraph, method signature blocks, the List of Tables, and the annotations.

- `make extract` writes `spec/ir/<spec>.json` from the pristine PDFs. These files are committed;
  `make regen-diff` holds them to what the extractor produces today.
- `make ir-annotated` extracts the annotated PDFs into `build/<spec>.hsed.json`. These differ from the
  pristine IR in their annotations alone, and they are what `hsedc` reads.
- `make extract-test` runs the extractor's unit tests.

## 5. The spec compiler: `hsedc` (package `hsed-spec`)

`make gen` runs three commands, Core first, then Opal over Core: Opal's run passes `--base` Core's IR,
so Opal's tables can refer to Core's (design §5.2).

1. **`hsedc bind`** parses each annotation and prints it as typed Haskell into
   `hsed-spec/bindings/Hsed/Bindings/<Spec>.hs`. `stack test hsed-spec:bindings` then type-checks it, so a
   wrong annotation fails as a Haskell type error that points at its PDF page.
2. **`hsedc check`** classifies every numbered table, binds it to its rule and runs the rules' checks.
   Each finding is an Error, Warning or Note with a code (e.g. `C-PRECONF-HEADER`), and any Error stops the
   run. Today: Core 0 Errors, 31 Warnings, 51 Notes; Opal 0, 37, 49.
3. **`hsedc compile`** writes the generated modules and the reports in `build/report/`: `findings.md`,
   `coverage.md`, `ERRATA.md` and `ledger.md`, the last copied to `docs/LEDGER*.md`.

What each table becomes is decided by its **rule**: one module per rule in
`hsed-spec/src/Hsed/Spec/Rules/`, listed in `Hsed.Spec.Registry`. Examples: `typeRow` for type tables,
`tableDesc` for table descriptions, `preconfig` for an SP's preconfigured rows, `sig` for method
signatures. [adding-a-rule.md](adding-a-rule.md) is the recipe for adding one.

Other useful commands:
- `stack exec hsedc -- explain core 46` says how a table binds;
- `orphans` lists what nothing binds;
- `ir-summary` counts an IR file's contents.

## 6. The generated Haskell (`hsed/gen`)

- **Core** (`Hsed.Gen.Core.*`): Types, Enums, TableUIDs, TableDescs, ColumnDefaults, Preconfig, UIDs,
  UIDRanges, Methods, Codes, Fields, Tokens, StateTables, Properties, Layouts, Features, Laws, Behaviours.
  That is 471 declarations in 18 modules.
- **Opal** (`Hsed.Gen.Opal.*`): Types, Enums, TableUIDs, TableDescs, Fields, Features, Properties, Codes,
  Tokens, Methods, `Preconfig.AdminSP` and `Preconfig.LockingSP`. That is 145 declarations in 12 modules.

Every declaration carries its provenance: the spec, the table or section, and the PDF page. These modules
are committed. `make gen-diff` fails if `make gen` would change them, so an edit to an annotation, a rule
or the extractor shows up as a diff to review and commit.

The runtime oracle (design §5.5) compares the generated Core types with the 2019 hand transcription in
`hsed/fixture/Hand`; they must differ by exactly the lines of `spec/expected-diff.txt`.

## 7. The runtime: codec, wire and transports

- **The codec, `hsed-core`:** tokens, values, method calls, UIDs, the token stream reader, and the layouts
  of ComPackets, packets and subpackets (design §4.1).
- **The wire, `Hsed.Wire`:** the contract between host and drive (design §6). A host sends a `Request`
  (IF-SEND or IF-RECV, with its protocol, ComID and transfer length) and gets a `Response` of bytes.
  Nothing else crosses.
- **The transports** (design §7), any of which a host can drive:
  - `Hsed.Transport.InProc` puts the simulator in the same process.
  - `Replay` answers from a captured file.
  - `MacKext` reaches a real SED through Bright Plaza's kext. It is built only with
    `--flag hsed:kext`, and its CDBs come from `hsed/cbits/hsed_cdb.c`.
  - mh_transport over a LAN is M5c's.

## 8. The simulator (`Hsed.Sim.*`)

A TPer built from the generated modules (design §8):
- Level 0 Discovery and its feature descriptors;
- the synchronous protocol and ComID management (protocol 2);
- the Session Manager (Properties, StartSession, sessions);
- method dispatch through each SP's AccessControl;
- the resets.

Neither SP's tables are written by hand: `Hsed.Sim.Preconfig` folds the generated
`Hsed.Gen.Opal.Preconfig.AdminSP` and `Preconfig.LockingSP` into the simulator's state. That folding fills
in templates and vendor-unique cells from `SimParams`, gives every column its default, expands wildcard
AccessControl rows, and leaves out the key table the TPer does not hold. It lists every problem it meets
(a parameter out of bounds, a cell nobody answers), and the simulator refuses to open on any. The Locking
SP starts Manufactured-Inactive, its Original Factory State, or Manufactured with
`--locking-ofs manufactured`; every reset applies its LockOnReset and MBRDoneOnReset columns (design
§8.3, §8.4). Simulator options, such as units, held responses, the Locking SP's state and a SID PIN past
the factory state, are `hsed-jig` flags.

## 9. The host jig (`Hsed.Jig.*`, exe `hsed-jig`)

The jig runs Opal Family Test Cases against a target and writes three reports: JSON, Markdown and JUnit,
to `build/report/` by default (design §9).

```
stack exec hsed-jig -- --target sim                            # every case on the simulator
stack exec hsed-jig -- --target sim --tests UCT-02,UCT-03:1a-1c
stack exec hsed-jig -- --target replay:spec/fixtures/disk6-level0.bin
stack exec hsed-jig -- --target sim+replay:spec/fixtures/disk6-level0.bin   # the Pair: compare the two
stack exec hsed-jig -- verify-transcript build/report/<report>.json --allow-list
stack exec hsed-jig -- level0 decode spec/fixtures/disk6-level0.bin --against spec/fixtures/disk6-query.txt
```

**The cases:** each case is a module under `hsed/src/Hsed/Jig/Cases/`, listed in `Hsed.Jig.Catalogue`.
Ten exist: UCT-01, UCT-02, UCT-03:1a-1c, ETC-02, ETC-03, ETC-04, ETC-08, ETC-15, ETC-21/adminsp and
SPF-02:1. A case declares its prerequisites (`Hsed.Jig.Prereq`) and what it needs of its target. It is
written in `Hsed.Jig.Script`'s verbs: `startSession`, `get`, `properties`, `stackReset` and the like,
each sending generated method calls.

**A real drive** (`--target /dev/diskN`, root, a kext-enabled build) is covered in design §9.3.
- The Guard admits only Level 0 until the drive's own Level 0 gives its base ComID; after that, only a
  method-level read-only list.
- Every command goes into the transcript, which `verify-transcript` re-checks afterwards.
- `tools/m4c-hw.sh SERIAL` prints the hardware steps, the pre-flight checks first.
- Agents never run a drive target.

## 10. Checking and landing

| Command | What it does |
|---|---|
| `make check` | extract-test, regen-diff, annotate, pdf-checks, gen-diff, then every test suite (hsed-core, hsed-spec with its bindings, hsed and the doctests) |
| `make werror` | CI's `-Werror` build and tests, in their own work dir |
| `make werror-cold` | the same from nothing |
| `tools/land.sh` | pushes `feature/types` to GitHub, then bumps and pushes the home repo's pointer to hsed on wapex (design §10.4) |

## 11. Common changes

**Change what a table means.** Edit its rule in `hsed-spec/src/Hsed/Spec/Rules/` (or add one, per
[adding-a-rule.md](adding-a-rule.md)). Then run `make gen`, review the diff in `hsed/gen` and the
ledger, and commit the rule and its generated output together.

**Correct the spec's text.** Add an `erratum scott [...]` to the table's `.hsa` line and record it in
`tcg/Core Spec errata.txt`. Then run `make annotate` and `make gen`.

**Read a slip of the PDF in the compiler (a reading).** Where the PDF means one thing and prints another
(a misspelt header or type name, a footnote number run into its cell, a column named by the end of its
name), a reading in `hsedc` reads it as meant and reports a Note at the cell, costing no annotation. Opal's
slips are all read this way. [adding-a-rule.md](adding-a-rule.md#adding-a-reading) says where readings live
and how to add one; then run `make gen` and review the ledger.

**Bind a table nothing binds.** After `make ir-annotated`, run
`stack exec hsedc -- orphans build/core-2.01.hsed.json` to list them (for Opal, give its IR with
`--base build/core-2.01.hsed.json`, as the Makefile does; since M5a every Opal unit is bound, so an
unbound one is an Error). Then either add the rule's
token to the `doc` line, or add a table annotation if the rule cannot find the table on its own. Run
`make annotate`, `make pdf-checks` and `make gen`.

**Add a jig case.**
1. Write `Hsed/Jig/Cases/<Key>.hs` from the Test Cases PDF's steps, citing its pages.
2. List it in `Hsed.Jig.Catalogue`.
3. Give it its prerequisites and needs.
4. Add its spec under `hsed/spec-tests/Hsed/Jig/`, run on the simulator.
5. If it can reach a drive, decide what the Guard admits for it (design §9.3).

**Add another SSC (for example Pyrite).** Nobody has done this yet. Opal is the precedent:
1. Add its PDF to `tcg/`.
2. Add a sidecar `spec/annot/<spec>.hsa` with a `doc` line naming its rules.
3. Add extraction and annotation targets to the Makefile, as Opal's (`--page-offset`, `--check-against`
   Core's IR).
4. Teach hsedc its name: add its `SpecId` to `DSL.knownSpecs` (beside `core201` and `opal230`; the
   grammar answers any other doc line "unknown specification"), and, where its sidecar must name its own
   tables in a binding (`narrows (<family> N)`, `replaces [...]`), a `Ref` constructor for its family,
   which `RuleDef.refTable` reads (the one place a `Ref` becomes a spec family, matched exactly against the
   `SpecId` up to its `-version`).
5. Run its `hsedc` commands over Core with `--base`, giving a new `--target` for its generated modules.
6. Expect findings where the PDF's tables differ from Opal's, and decide each as a reading in a rule or an
   erratum. Opal's readings are keyed to `opal-2.30` where they are per cell (`Readers.Cell.noteMarkerCells`)
   and global where they are a misspelling (`cellTypos`, `Shape.headerTypos`), so expect some to carry
   over and some to need entries of its own; Pyrite 2.01 and Ruby 1.00 print markers Opal's readers do not
   know (`(ALR O)` and `(MBR O)`, a bare `SID` PIN; design §13.35).
7. Give the simulator a profile of it: today `Hsed.Sim.Types.preconfiguredSPs` is Opal's two SPs, and the
   resets take their types from `Opal_reset_types`; the record a second SSC needs (its preconfigured SPs,
   vendor cells, parameter bounds, Level 0 descriptors and reset types) is designed before the first SSC
   after M5 (design §11, *After M5*).

Scott's plan is every SSC after M5: Pyrite 2.01, Ruby 1.00, Enterprise 1.01 and Key Per I/O (design §11).
Enterprise 1.01 is no SSC over Core 2.01 as Opal is: it is built on a draft Core 1.0, which `tcg/` does not
hold, and captions its preconfiguration tables `<SP> <T> table`, which rule preconfig does not take (design
§11).
