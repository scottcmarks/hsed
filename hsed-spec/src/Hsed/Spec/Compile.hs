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
its shape into declarations for its generated module
('Hsed.Spec.RuleDef.ruleCompile'). The declarations are grouped by module and
each module printed by 'Gen.renderModule': deterministic, sorted by table
number. A module is written when some rule emits a declaration for it.

A bound table a rule leaves out is the Warning @W-GEN-EXCLUDED@ saying why (its
Error is already among the findings); so is a table bound (by a comment) to a
shape no rule of the registry reads, and both count among the tables left out.
A @prose@ table has nothing to generate and is not counted.
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
import           Hsed.Spec.Shape   (Shape (..), shapeName)


-- | What @hsedc compile@ writes, with everything it found.
data CompileResult = CompileResult
  { crModules  :: ![(Text, Text)]         -- ^ each generated module's name and text, by name
  , crDecls    :: ![Gen.Decl]             -- ^ every declaration, in registry order
  , crExcluded :: ![(AnchorId, Text)]
  , crFindings :: ![Finding]              -- ^ 'sortFindings' order
  }

-- | Compile one IR under one errata set; Left when two rules give one module
--   name different descriptions.
compileIR :: ErrataSet -> SpecIR -> Either Finding CompileResult
compileIR es ir = do
  modules <- generatedModules ctx [ (ruleModule r, d) | (r, Emit d) <- emitted ]
  pure CompileResult
    { crModules  = [ (name, Gen.renderModule name gm ds) | (name, gm, ds) <- modules ]
    , crDecls    = [ d | (_, Emit d) <- emitted ]
    , crExcluded = map fst excluded
    , crFindings = sortFindings (checks ctx <> map snd excluded) }
  where
    ctx      = mkContext es ir
    emitted  = [ (r, e) | r <- registry, e <- ruleCompile r ctx (boundTables (ruleShape r) ctx) ]
    excluded = [ ((aid, why), excludedFinding aid ("left out of " <> Gen.moduleName (cxSpec ctx) (ruleModule r) <> ": " <> why))
               | (r, Excluded aid why) <- emitted ]
            <> [ ((aid, why), excludedFinding aid why)
               | (aid, _) <- Map.toList (cxTables ctx), Just sb <- [bindingOf ctx aid]
               , Just shape <- [someBindingShape sb], shape /= Prose, Just shape `notElem` map ruleShape registry
               , let why = "hsedc generates nothing for a " <> shapeName shape <> " table yet (no rule of the registry reads one)" ]
    excludedFinding aid why = case Map.lookup aid (cxTables ctx) of
      Just t  -> locatedAt (cxSpec ctx) (renderAnchorId aid) (fromMaybe 1 (tablePage t)) (finding Warning "W-GEN-EXCLUDED" why)
      Nothing -> finding Warning "W-GEN-EXCLUDED" why

-- | The declarations grouped by the module they go to, by module name; Left
--   when two rules name one module with different descriptions.
generatedModules :: Context -> [(Gen.GenModule, Gen.Decl)] -> Either Finding [(Text, Gen.GenModule, [Gen.Decl])]
generatedModules ctx decls = traverse one (Map.toList grouped)
  where
    grouped = Map.fromListWith (flip (<>)) [ (Gen.moduleName (cxSpec ctx) gm, [(gm, d)]) | (gm, d) <- decls ]
    one :: (Text, [(Gen.GenModule, Gen.Decl)]) -> Either Finding (Text, Gen.GenModule, [Gen.Decl])
    one (name, entries) = case L.nub (map fst entries) of
      [gm] -> Right (name, gm, map snd entries)
      gms  -> Left (finding Error "E-GEN-MODULE"
                ("the rules give the module " <> name <> " " <> tshow (length gms) <> " different descriptions: "
                  <> T.intercalate "; " (map (\gm -> gm.description) gms)))
