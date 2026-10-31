{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Compile
Description : Every rule's tables compiled to declarations, and the generated modules
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@hsedc compile@ (design section 5.2): the checks of "Hsed.Spec.Check", then,
for every rule of "Hsed.Spec.Registry", its compilation of the tables bound to
its shape into declarations for its generated module, or for another module
it names ('Hsed.Spec.RuleDef.ruleCompile'). The declarations are grouped by
module and each module printed by 'Gen.renderModule': deterministic, sorted by
table number. A module is written when some rule emits a declaration for it;
@hsedc compile@ removes the modules of the spec's directory it did not write
("Hsed.Spec.Cli"). Rules naming one module must give it one description; their
notes are united, in registry order (@E-GEN-MODULE@ otherwise).

A unit a rule leaves out, a bound table (at the table), a paragraph unit (at
its paragraph) or a heading unit (at its heading), is the Warning
@W-GEN-EXCLUDED@ saying why (its Error is already among the findings); so is
any unit a binding holds whose shape no rule of the registry reads (a table a
comment binds, a paragraph unit, a sentence a @law@ binds, or a heading unit,
a heading a @behaviour@ or a @fails@ binds), at the unit
("Hsed.Spec.RuleDef".'Hsed.Spec.RuleDef.allUnits', in its order: the tables,
then the paragraphs, then the headings); each counts among the units left
out. A table of a shape with nothing to generate
('Hsed.Spec.Shape.shapeGenerates') is not counted. 'compileIRWith' does the
same under another set of rules than the registry.
-}
module Hsed.Spec.Compile
  ( CompileResult (..)
  , compileIR
  , compileIRWith
  , generatedModules
  ) where

import qualified RIO.List          as L
import qualified RIO.Map           as Map
import qualified RIO.Text          as T
import           RIO

import           Hsed.Spec.Anchor  (AnchorId)
import           Hsed.Spec.Check
import           Hsed.Spec.Common  (ErrataSet, Finding, Severity (..), finding)
import           Hsed.Spec.DSL     (someBindingShape)
import qualified Hsed.Spec.Gen     as Gen
import           Hsed.Spec.IR      (SpecIR (..))
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Shape   (shapeGenerates, shapeName)


-- | What @hsedc compile@ writes, with everything it found.
data CompileResult = CompileResult
  { crModules  :: ![(Text, Text)]         -- ^ each generated module's name and text, by name
  , crDecls    :: ![Gen.Decl]             -- ^ every declaration, in registry order
  , crExcluded :: ![(AnchorId, Text)]
  , crFindings :: ![Finding]              -- ^ 'sortFindings' order
  }

-- | Compile one IR under one errata set, over a base Context or none (Core's
--   for an Opal run: "Hsed.Spec.Check".'Hsed.Spec.Check.mkContextWith'); Left
--   when two rules give one module name different descriptions.
compileIR :: Maybe Context -> ErrataSet -> SpecIR -> Either Finding CompileResult
compileIR = compileIRWith registry

-- | 'compileIR' under a set of rules (a test's stub rules, or the registry
--   without the rules of one kind of unit): the units classified, checked and
--   compiled by those rules, and the units of a shape none of them reads left out.
compileIRWith :: [RuleDef] -> Maybe Context -> ErrataSet -> SpecIR -> Either Finding CompileResult
compileIRWith ruleSet base es ir = do
  modules <- generatedModules ctx decls
  pure CompileResult
    { crModules  = [ (name, Gen.renderModule name gm ds) | (name, gm, ds) <- modules ]
    , crDecls    = map snd decls
    , crExcluded = map fst excluded
    , crFindings = sortFindings (checksWith ruleSet ctx <> map snd excluded) }
  where
    ctx      = mkContextWith ruleSet base es ir
    emitted  = [ (r, e) | r <- ruleSet, e <- ruleCompile r ctx (boundTables (ruleShape r) ctx) ]
    decls    = concat [ case e of
                          Emit d      -> [(ruleModule r, d)]
                          EmitIn gm d -> [(gm, d)]
                          Excluded {} -> []
                      | (r, e) <- emitted ]
    excluded = [ ((aid, why), excludedFinding aid ("left out of " <> Gen.moduleName (cxSpec ctx) (ruleModule r) <> ": " <> why))
               | (r, Excluded aid why) <- emitted ]
            <> [ ((unitAnchor u, why), atUnit ctx u (finding Warning "W-GEN-EXCLUDED" why))
               | SomeUnit u <- allUnits ctx, Just shape <- [unread (unitBinding u)]
               , let why = "hsedc generates nothing for a " <> shapeName shape <> " " <> unitNoun (unitKind (proxyOf u))
                             <> " yet (no rule of the registry reads one)" ]
    -- the shape of a binding no rule of the set reads, when it has something to generate
    unread sb = mfilter (\shape -> shapeGenerates shape && Just shape `notElem` map ruleShape ruleSet) (someBindingShape sb)
    -- at the unit the rule left out (a table, a paragraph, a heading)
    excludedFinding aid why = maybe id (\(SomeUnit u) -> atUnit ctx u) (unitNamed ctx aid) (finding Warning "W-GEN-EXCLUDED" why)
    proxyOf :: u -> Proxy u
    proxyOf _ = Proxy

-- | The declarations grouped by the module they go to, by module name, each
--   module's notes those of every descriptor naming it, united in order; Left
--   when two descriptors of one module give it different descriptions.
generatedModules :: Context -> [(Gen.GenModule, Gen.Decl)] -> Either Finding [(Text, Gen.GenModule, [Gen.Decl])]
generatedModules ctx decls = traverse one (Map.toList grouped)
  where
    grouped = Map.fromListWith (flip (<>)) [ (Gen.moduleName (cxSpec ctx) gm, [(gm, d)]) | (gm, d) <- decls ]
    one :: (Text, [(Gen.GenModule, Gen.Decl)]) -> Either Finding (Text, Gen.GenModule, [Gen.Decl])
    one (name, entries) = case (L.nub (map ((.description) . fst) entries), L.nub (map fst entries)) of
      ([_], gms@(gm : _)) -> Right (name, gm { Gen.notes = concatMap (.notes) gms }, map snd entries)
      (ds, _)             -> Left (finding Error "E-GEN-MODULE"
                               ("the rules give the module " <> name <> " " <> tshow (length ds) <> " different descriptions: "
                                 <> T.intercalate "; " ds))
