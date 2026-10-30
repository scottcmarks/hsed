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

A bound table a rule leaves out is the Warning @W-GEN-EXCLUDED@ saying why (its
Error is already among the findings); so is a table bound (by a comment) to a
shape no rule of the registry reads, and so is a paragraph unit (a sentence a
@law@ binds) of a shape no rule reads, at the paragraph; each counts among the
units left out. A table of a shape with nothing to generate
('Hsed.Spec.Shape.shapeGenerates') is not counted.
-}
module Hsed.Spec.Compile
  ( CompileResult (..)
  , compileIR
  , generatedModules
  ) where

import qualified RIO.List          as L
import qualified RIO.Map           as Map
import qualified RIO.Text          as T
import           RIO

import           Hsed.Spec.Anchor  (AnchorId, renderAnchorId)
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
compileIR base es ir = do
  modules <- generatedModules ctx decls
  pure CompileResult
    { crModules  = [ (name, Gen.renderModule name gm ds) | (name, gm, ds) <- modules ]
    , crDecls    = map snd decls
    , crExcluded = map fst excluded
    , crFindings = sortFindings (checks ctx <> map snd excluded) }
  where
    ctx      = mkContextWith registry base es ir
    emitted  = [ (r, e) | r <- registry, e <- ruleCompile r ctx (boundTables (ruleShape r) ctx) ]
    decls    = concat [ case e of
                          Emit d      -> [(ruleModule r, d)]
                          EmitIn gm d -> [(gm, d)]
                          Excluded {} -> []
                      | (r, e) <- emitted ]
    excluded = [ ((aid, why), excludedFinding aid ("left out of " <> Gen.moduleName (cxSpec ctx) (ruleModule r) <> ": " <> why))
               | (r, Excluded aid why) <- emitted ]
            <> [ ((aid, why), excludedFinding aid why)
               | (aid, _) <- Map.toList (cxTables ctx), Just sb <- [bindingOf ctx aid]
               , Just shape <- [unread sb]
               , let why = "hsedc generates nothing for a " <> shapeName shape <> " table yet (no rule of the registry reads one)" ]
            <> [ ((bpAnchor bp, why), atPara ctx bp (finding Warning "W-GEN-EXCLUDED" why))
               | bp <- paragraphUnits ctx, Just shape <- [unread (bpBinding bp)]
               , let why = "hsedc generates nothing for a " <> shapeName shape <> " paragraph yet (no rule of the registry reads one)" ]
    -- the shape of a binding no rule of the registry reads, when it has something to generate
    unread sb = mfilter (\shape -> shapeGenerates shape && Just shape `notElem` map ruleShape registry) (someBindingShape sb)
    excludedFinding aid why = case Map.lookup aid (cxTables ctx) of
      Just t  -> locatedAt (cxSpec ctx) (renderAnchorId aid) (fromMaybe 1 (tablePage t)) (finding Warning "W-GEN-EXCLUDED" why)
      Nothing -> finding Warning "W-GEN-EXCLUDED" why

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
