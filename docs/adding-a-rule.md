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
every module under `gen/Hsed/Gen/Core` and refuses to compile without it.

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
- **A unit that is not a table.** `RuleDef` covers the numbered tables: the classification binds
  tables and the rule reads `BoundTable`s. The method signatures of rule `sig` (M2b) need the IR's
  `sigs` schema, the extractor, their classification and a non-table bound unit in `RuleDef` first.
- **A milestone's anchors.** Every rule token and heading anchor the milestone's rules need goes into
  the `.hsa` in one commit, with the `.upd`, manifest and bindings `make gen` regenerates, so that the
  tail budget (below) is checked once on the whole set. A token for a rule not yet registered is only
  `W-RULE-UNIMPLEMENTED`.
- **A runtime module** generated code imports: named once in `Hsed.Spec.Gen` (`uidModule`,
  `coreTypesModule`).

`RegistrySpec` fails if a module under `Rules/` is not in the registry, if two rules share a name or a
shape, or if a generic module (`Rules`, `Scope`, `Check`, `Compile`, `Gen`, `Cli`, `RuleDef`, `Report`,
`Bind`, `app/Main.hs`) names in code any rule token of the vocabulary, its `<rule>Rule` value, or any
shape a table can be bound with, registered or not.

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
comment-bound, in anchor order), each as printed (`btTable`) and read under the errata set
(`btRead`), with its binding (`btBinding`) and its modifiers (`btMods`). The `Context` is read-only and
generic: the IR, spec, errata set, numbered tables, classification, every table's section
(`cxSections`; `sectionOf ctx bt`) and corrected tables. A rule that needs another rule's reading (an
enumeration's range from its type row; a table description's UID from Table 240) imports that rule's
module and calls its reader on the same `Context`; it never adds fields to the `Context`. Locate
findings with `atTable ctx bt`; give every finding a catalogue code.

One shape has one `RuleDef`. Where several bindings share a shape (`propsNames` and `propsInitial` both
bind `Props`), the shape's rule reads every table bound with it and tells them apart by `btBinding`.
A reader reached only by a comment (`stateTable`, `propsNames`/`propsInitial`, `propsReq`, `legend`,
`decision`) is such a `RuleDef`: its name is no rule token, `fires` is `\_ _ _ -> False`, and its
exemplar is the table its comment sits on (only acceptance is checked).

What rules may import besides each other: `Hsed.Spec.Scope` (sections, the scope the anchors give,
the validated annotations; it sits below the registry, while `Hsed.Spec.Rules` imports the registry and
so every rule, and a rule importing it would be a module cycle), and `Hsed.Spec.Readers(.<X>)`, the home
of helpers several rules share and no comment names (`pins`, `accessControl`). A helper never goes under
`Rules/`, where every module must be a registered rule.

## Generated code (`Hsed.Spec.Gen`)

Print each declaration yourself: `(Gen.decl provenance name lines) { Gen.declRank = row,
Gen.declImports = [...], Gen.declExtensions = [...] }`, its lines starting with
`Gen.provenanceComment provenance` (`provenanceOf ctx aid table caption`; `atRow ctx cells` moves it to
the page a row is printed on). Name the spec in identifiers with `Gen.specLabel` (`Core`), never a
literal. `Gen.renderModule` assembles each module from the declarations of every rule that names it:
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
its xref stream) to 400 bytes per anchor on average; never raise the budget silently.

## Checking it

`make gen` (bind, check, compile, the ledger), `stack exec hsedc -- explain core <n>`, `make gen-diff`
after a second `make gen`, then `make check`. No spec data in code: tables, UIDs, names and typo lists
come from the PDF or from an annotation.
