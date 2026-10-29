# Adding a rule to hsedc

A rule (design `docs/spec-as-source.md` §3.4) is everything hsedc knows about one kind of table: when
it fires, what it checks, what it generates. Since M2a each rule lives in its own module and the
generic compiler only iterates a list of them, so rules can be added in parallel without touching
each other's files.

## The files a new rule touches, and nothing else

| file | what you add |
|---|---|
| `hsed-spec/src/Hsed/Spec/Rules/<Rule>.hs` | the rule: one `RuleDef` value, `<rule>Rule` (`rule fooBar` lives in `FooBar.hs` as `fooBarRule`) |
| `hsed-spec/src/Hsed/Spec/Registry.hs` | one import and one list entry, both in alphabetical order |
| `hsed-spec/test/Hsed/Spec/Rules/<Rule>Spec.hs` | its tests (hspec-discover finds them) |
| `hsed/gen/Hsed/Gen/<Spec>/<Leaf>.hs` | its generated module, written by `make gen` and committed |
| `spec/annot/<spec>.hsa` | only if the rule needs an anchor (below); `make gen` then rewrites `.upd`, the manifest and `hsed-spec/bindings/` |

The vocabulary already knows every rule of §3.4: the rule token (`Hsed.Spec.DSL.knownRules`), its
`Shape` and binding constructor, the grammar and `tools/extract/hsed_annot.py`'s `VOCABULARY`. A rule
that needs a new shape, binding or modifier changes the vocabulary (DSL, Shape, Grammar,
`hsed_annot.py`, `VocabularySpec`): do that in its own commit first, it is not parallel-safe.

`RegistrySpec` fails if a module under `Rules/` is not in the registry, if two rules share a name
or a shape, or if a generic module (`Rules`, `Check`, `Compile`, `Gen`, `Cli`, `RuleDef`, `Report`,
`Bind`, `app/Main.hs`) names a rule, its shape or its value in code.

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
  , ruleCompile :: Context -> [BoundTable] -> [Emitted] -- Emit Decl | Excluded AnchorId reason
  }
```

`ruleChecks` and `ruleCompile` get every table bound to the rule's shape (rule-bound or
comment-bound, in anchor order), each as printed (`btTable`) and read under the errata set
(`btRead`), with its binding's modifiers (`btMods`). The `Context` is read-only and generic: the IR,
spec, errata set, numbered tables, classification and corrected tables. A rule that needs another
rule's reading (an enumeration's range from its type row; a table description's UID from Table 240)
imports that rule's module and calls its reader on the same `Context`; it never adds fields to the
`Context`. Locate findings with `atTable ctx bt`; give every finding a catalogue code.

A reader reached only by a comment (`stateTable`, `propsNames`) is a `RuleDef` too: its name is no
rule token, `fires` is `\_ _ _ -> False`, and its exemplar is the table its comment sits on (only
acceptance is checked).

## Generated code (`Hsed.Spec.Gen`)

Print each declaration yourself: `(Gen.decl provenance name lines) { Gen.declRank = row, Gen.declImports = [...] }`,
its lines starting with `Gen.provenanceComment provenance` (`provenanceOf ctx aid table caption`).
`Gen.renderModule` assembles each module from the declarations of every rule that names it: header,
merged imports, declarations sorted by table number, rank, name. Two rules may share a module only
with an equal `GenModule`. A module is written when it has a declaration.

## The annotation, if the rule needs an anchor

Scope comes only from anchors. Prefer a token on the existing doc line
(`core-2.01 | doc | doc core201 & rule fooBar`, about 17 bytes) when the firing condition is
unambiguous document-wide (design §13.24); a heading anchor costs about 555 bytes on a page without
`/Annots`. `make pdf-checks` holds the tail to 400 bytes per anchor on average; never raise the
budget silently.

## Checking it

`make gen` (bind, check, compile), `stack exec hsedc -- explain core <n>`, `make gen-diff` after a
second `make gen`, then `make check`. No spec data in code: tables, UIDs, names and typo lists come
from the PDF or from an annotation.
