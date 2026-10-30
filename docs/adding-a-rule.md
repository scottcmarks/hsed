# Adding a rule to hsedc

A rule (design `docs/spec-as-source.md` §3.4) is everything hsedc knows about one kind of table: when
it fires, what it checks, what it generates. Since M2a each rule lives in its own module and the
generic compiler only iterates a list of them, so rules can be added in parallel without editing
each other's files or any generic module.

## The files a new rule touches

| file | what you add |
|---|---|
| `hsed-spec/src/Hsed/Spec/Rules/<Rule>.hs` | the rule: one `RuleDef` value, `<rule>Rule` (`rule fooBar` lives in `FooBar.hs` as `fooBarRule`) |
| `hsed-spec/src/Hsed/Spec/Registry.hs` | one import and one list entry, both in alphabetical order |
| `hsed-spec/test/Hsed/Spec/Rules/<Rule>Spec.hs` | its tests, its own counts among them (hspec-discover finds them) |
| `spec/annot/<spec>.hsa` | only if the rule needs an anchor (below) |

and the files `make gen` rewrites, which are committed and never edited by hand: the generated module(s)
`hsed/gen/Hsed/Gen/<Spec>/<Leaf>.hs`, the ledger `docs/LEDGER.md`, and, after an `.hsa` change,
`spec/annot/<spec>.{upd,manifest.json}` and `hsed-spec/bindings/`.

The generic tests (`CheckSpec`, `RulesSpec`, `CompileSpec`, `CliSpec`) hold invariants (every table
classified once, one `W-UNBOUND` per unbound table, the committed modules exactly those compile writes,
the committed ledger exactly what the run gives), not totals: how many tables each rule binds and how
many findings of each code a run gives are in `docs/LEDGER.md`. A new rule edits none of them.

A rule whose definitions have a hand-written counterpart in hsed (the M2a-M2d comparison, design §5.5)
also extends `hsed/spec-tests/Hsed/Gen/Core/EquivTH.hs` (`handDefs`) and adds its expected differences to
`spec/expected-diff.txt`; a new generated module needs one import line in `EquivSpec.hs`, which reads
every module under `gen/Hsed/Gen/Core` and refuses to compile without it as soon as the module declares a
definition the oracle compares (`name :: UID`, `String`, `HalfUID`, the two name maps, a `[(UID, String)]`
list of a table's UIDs, or a `data Core_<enum>`; any other `data`, a record such as a method's, is no
enumeration to it). Generated modules stay ASCII: EquivTH reads them with the locale's `readFile`, so a
text literal prints non-ASCII escaped (`tshow`).

A rule's finding codes are named in `Hsed.Spec.Report.catalogue` by what they report (a defect of the PDF,
text read other than as printed, worth a look, or hsedc's bookkeeping), which sorts `build/report/ERRATA.md`;
a code it does not name is listed there under "Not yet classified", and `ReportSpec` fails while the
committed run gives one. That is one line in a generic module: rules written in parallel leave it to the
integrator, who also adds the codes to design §2 Q2.

## When two rules are added in parallel

- `Registry.hs`: both add an import and a list line; git may report the two insertions at one place as
  a conflict. Keep both lines, in alphabetical order.
- `spec/annot/<spec>.hsa`: union the lines (on the doc line, the tokens).
- `.upd`, the manifest, `hsed-spec/bindings/`, `hsed/gen/` and `docs/LEDGER.md`: take either side and run
  `make gen`, which rewrites them from the merged sources; never merge them by hand.

## Serial changes first

Some changes are generic and are made in a small commit of their own, before the rules that need them:

- **The vocabulary.** It already knows every rule of §3.4: the rule token (`Hsed.Spec.DSL.knownRules`),
  its `Shape` and binding constructor, the grammar and `tools/extract/hsed_annot.py`'s `VOCABULARY`. A
  new shape, binding or modifier changes DSL, Shape (with its `shapeMismatch` line), Grammar,
  `hsed_annot.py` and `VocabularySpec`; a new modifier also gets its field in `Hsed.Spec.RuleDef.Mods`.
- **A unit that is not a table.** `RuleDef` binds two kinds of unit: the numbered tables and, since
  M2b, the method signature blocks (below). Another kind (a Fails list, a law sentence) needs its IR
  schema, the extractor, its classification and its bound unit in `RuleDef` first, as the
  signature blocks had.
- **A milestone's anchors.** Every rule token and heading anchor the milestone's rules need goes into
  the `.hsa` in one commit, with the `.upd`, manifest and bindings `make gen` regenerates, so that the
  tail budget (below) is checked once on the whole set. A token for a rule not yet registered is only
  `W-RULE-UNIMPLEMENTED`, and a table a comment binds to a shape no registered rule reads (T45's
  `stateTable`, T167/T168's `propsNames`/`propsInitial` before their reader) is `W-GEN-EXCLUDED`. M2b's
  are in: an M2b rule or reader edits no `.hsa`. A rule test that edits a table and expects the rule
  to leave it out compares with the committed run (`CoreIR.newlyExcluded`), not with an empty list.
- **A runtime module** generated code imports: named once in `Hsed.Spec.Gen` (`uidModule`,
  `coreTypesModule`; since M2b also `coreTableModule`, `methodModule`, `codeModule`, `fieldModule`,
  `syncModule`, `propertiesModule`), the module itself in hsed-core.

`RegistrySpec` fails if a module under `Rules/` is not in the registry, if two rules share a name or a
shape, or if a generic module (`Rules`, `Scope`, `Check`, `Compile`, `Gen`, `Cli`, `RuleDef`, `Report`,
`Bind`, `Document`, `app/Main.hs`) names in code any rule token of the vocabulary, its `<rule>Rule` value, or any
shape a table can be bound with, registered or not; string literals are code to it (a catalogue line or a
help text says "code table", never the token).

## The record (`Hsed.Spec.RuleDef`)

```haskell
data RuleDef = RuleDef
  { ruleName    :: RuleName                           -- the rule token, RuleName "fooBar"
  , bindsTo     :: SomeBinding                        -- default binding; its shape is the shape the rule reads
  , exemplar    :: (SpecId, AnchorId)                 -- a table it must keep firing on (C-EXEMPLAR)
  , refinesRule :: Maybe RuleName                     -- wins over that rule where both fire
  , joinsTo     :: Maybe RuleName                     -- a table of that rule must name the same identifier
  , fires       :: SpecId -> TableIR -> Form -> Bool  -- on the measured form; the anchors give the scope
  , ruleIdent   :: Form -> Maybe Text                 -- the identifier a table names, for joins
  , ruleModule  :: GenModule                          -- leaf, Haddock description, notes
  , ruleChecks  :: Context -> [BoundTable] -> [Finding]
  , ruleCompile :: Context -> [BoundTable] -> [Emitted] -- Emit Decl | EmitIn GenModule Decl | Excluded AnchorId reason
  }
```

`fires` is written one way: `isNothing (shapeMismatch <Shape> form)` (the structure the shape accepts,
`Hsed.Spec.Shape`) and then the conditions that are the rule's own (a narrower first header, a caption
naming an identifier). The shape's line in `shapeMismatch` is vocabulary; a rule that needs it changed
changes it in a serial commit.

`ruleChecks` and `ruleCompile` get every table bound to the rule's shape (rule-bound or
comment-bound, in anchor order), but not one its binding's shape refuses (`E-SHAPE`, an Error: a
comment binding a table to a shape it is not, whose one finding is then that Error), each as printed
(`btTable`) and read under the errata set
(`btRead`), with its binding (`btBinding`) and its modifiers (`btMods`). The `Context` is read-only and
generic: the IR, spec, errata set, numbered tables, classification, every table's section
(`cxSections`; `sectionOf ctx bt`) and corrected tables. A rule that needs another rule's reading (an
enumeration's range from its type row; a table description's UID from Table 240) imports that rule's
module and calls its reader on the same `Context`; it never adds fields to the `Context`. A rule
exports its readers (`<rule>Reads`, `uidRows`) whether or not another rule calls them yet. Locate a
finding about a table with `atTable ctx bt`, and one about one row with `atCells ctx bt cells`, which
puts it on the page the row is printed on (a table spanning pages prints a row on its own page; the
`stateTable` and `enumValues` readers' findings are about a table's rows together and stay at the
table); give every finding a catalogue code. A check that joins two rules' readings belongs to the
rule whose module imports the other's: a rule imports the readers of the rules it reads and never the
reverse, so no join can make a module cycle, and `atTable`/`atCells` place a finding at any bound
table, whichever rule binds it (`preconfig`'s C-PRECONF-UID reads `uidTable`'s rows, `uidRows`, and
locates its findings at Table 243). A rule test that compares the exact Errors of an edited run keeps
to its own codes, since the edit may fail another rule's check as well (`TypeRowSpec` leaves out
`ranges`' C-UID-RANGE, `UidTableSpec` `preconfig`'s C-PRECONF-UID).

One shape has one `RuleDef`. Where several bindings share a shape (`propsNames` and `propsInitial` both
bind `Props`), the shape's rule reads every table bound with it and tells them apart by `btBinding`.
A reader reached only by a comment (`stateTable`, `propsNames`/`propsInitial`, `propsReq`, `legend`,
`decision`) is such a `RuleDef`: its name is no rule token, `fires` is `\_ _ _ -> False`, and its
exemplar is the table its comment sits on (only acceptance is checked).

What rules may import besides each other: `Hsed.Spec.Scope` (sections, the scope the anchors give,
the validated annotations; it sits below the registry, while `Hsed.Spec.Rules` imports the registry and
so every rule, and a rule importing it would be a module cycle), and `Hsed.Spec.Readers.<X>`, the home
of helpers several rules share and no comment names: `Hsed.Spec.Readers.Text` since M2b (`titleIdent`
and `lowerIdent`, the two conventions of generated names, and `readHexByte`), `pins` and
`accessControl` to come. What every rule needs is in `Hsed.Spec.RuleDef` (`atTable`, `atCells`,
`boundProvenance`, `provenanceOf`, `atRow`) and `Hsed.Spec.Gen` (`listLines`, `listBlock`,
`listDefinition`). A helper never goes under `Rules/`, where every module must be a registered rule.

## A rule over signature units

The method signature blocks of the IR (`sigs`, `docs/ir-v1.md`: one per section, heads, the lines as
printed) are the second kind of unit. `Hsed.Spec.Shape.shapeUnit` says which shape takes them (the
signature shape, `sig`); its rule is an ordinary module under `Rules/` whose `RuleDef` has:

- `bindsTo = SomeBinding sig`, `fires = \_ _ _ -> False` (it never fires on a table, and the table
  classifier passes over a rule whose shape takes signature units whatever its `fires` says), `ruleIdent =
  const Nothing`, and an exemplar that is a signature block, `(SpecId "core-2.01", SigId "5.3.3.6")`
  (`C-EXEMPLAR` asks only that the IR has it);
- `ruleChecks` and `ruleCompile`, which get no tables (`[]`) and read the bound blocks themselves:
  `boundSigs (ruleShape <rule>) ctx :: [BoundSig]`, each with its anchor id (`bsAnchor`, `SigId
  "5.3.3.6"`, rendered `5.3.3.6/sig`), its `SigIR` (`bsSig`: `section`, `page` 0-based, `rect`,
  `structPath`, `heads`, `text`) and its binding. A block carries no annotation, so no modifiers and
  no errata: it is read as printed. Locate a finding with `atSig ctx bs`
  (`core-2.01.pdf#page=201&5.3.3.6/sig`) and give a declaration `sigProvenanceOf ctx bs` (the block's
  heads as its caption, its 1-based place among the blocks as its ordinal, the sha256 of its text).

`Hsed.Spec.Rules.classifySignatures` binds every block whose section a `rule <token>` anchor covers
to that rule (the doc anchor covers them all); a block no such rule covers is `W-UNBOUND` at the
block, as an unbound table is (an Error with E-UNBOUND at the end of M2c), and one two such rules
cover is `E-AMBIGUOUS`. The ledger counts the blocks beside the tables (`docs/LEDGER.md`), `hsedc
orphans` lists the unbound ones after the tables, and the coverage report lists every block. The
blocks are in the `Context` (`cxSigs`, `cxSigClasses`); a rule that needs a table's reading too (the
method UIDs of Table 242) calls that rule's reader on the same `Context`. `SignatureUnitSpec` proves
the interface with a stub rule.

## Generated code (`Hsed.Spec.Gen`)

Print each declaration yourself: `(Gen.decl provenance name lines) { Gen.declRank = row,
Gen.declImports = [...], Gen.declExtensions = [...] }`, its lines starting with
`Gen.provenanceComment provenance` (`boundProvenance ctx bt`, or `provenanceOf ctx aid table caption`;
`atRow ctx cells` moves it to the page a row is printed on), a list literal printed by `Gen.listLines`
(or `listBlock`, `listDefinition`). Name the spec in identifiers with `Gen.specLabel` (`Core`), never a
literal. Name a value with `lowerIdent` (`spTemplatesTableUIDs`, `mbrControlTableDesc`) and a prefixed
name with `titleIdent` (`cComIDAssignments`); refer to another rule's generated names through that
rule's function, never by building the string again (`typeUIDName` of `typeRow`, `tableUIDName` and
`tableObjectUIDName` of `tableUIDs`). A generated text is `Text`, imported from `Data.Text` (not RIO);
`String` only where the oracle compares it with a hand-written `String`. A generated module whose names
other modules export too (`Hsed.Gen.<Spec>.Methods`: `Get`, `Set`, `Next`) is imported qualified, as
are runtime modules whose names meet (`Hsed.Core.Field`'s and `Hsed.Core.Sync`'s `Constant`).
`Gen.renderModule` assembles each module from the declarations of every rule that names it:
pragmas (the declarations' extensions with the two every module has), header, merged imports,
declarations sorted by table number, rank, name. A declaration goes to the rule's module (`Emit`) or to
another one (`EmitIn`, for a rule that writes several). Rules naming one module must give it one
description; their notes are united. A module is written when it has a declaration, and `hsedc compile`
removes the spec's modules it did not write.

## The annotation, if the rule needs an anchor

Scope comes only from anchors. Prefer a token on the existing doc line
(`core-2.01 | doc | doc core201 & rule fooBar`, about 17 bytes) when the firing condition is
unambiguous document-wide (design §13.24); a heading anchor costs about 555 bytes on a page without
`/Annots`, about 264 on one that has them. `make pdf-checks` holds the tail's objects (the bytes before
its xref stream) to 400 bytes per anchor on average; never raise the budget silently. An anchor whose
scope binds nothing to its rule is `W-RULE-SCOPE-EMPTY`, and one inside another anchor's scope of the
same rule `N-RULE-SCOPE-REDUNDANT`: both say the annotation is to move or go.

## Checking it

`make gen` (bind, check, compile, the ledger), `stack exec hsedc -- explain core <n>` (or `<section>/sig` for a
signature block), `make gen-diff`
after a second `make gen`, then `make check`. No spec data in code: tables, UIDs, names and typo lists
come from the PDF or from an annotation.
