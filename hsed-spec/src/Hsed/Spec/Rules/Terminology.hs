{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Rules.Terminology
Description : The terminology rule, and the reader of every table bound as prose
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4. @terminology@ fires on a table whose caption ends
@Terminology@ or @Example@ ('Hsed.Spec.Shape.CaptionTerminology') and binds it
as @prose@: text a reader of the spec reads, from which hsedc generates
nothing. In Core 2.01 the caption picks out Tables 1, 210, 217, 219, 224 and
232 across the document, so the rule rides on the @doc@ anchor (design 13.24).

The rule owns the @Prose@ shape, so its 'RuleDef' is also the reader of the
tables a @prose@ comment binds (Core Tables 2, 3, 24 and 185, and Tables 230
and 231 until their @decision@ reader, design M6): both kinds are bound, rule
and comment alike carrying the one binding @prose@, and neither has a check or
a declaration ('Hsed.Spec.Shape.shapeGenerates' is False for the shape, so a
table bound with it is never reported as left out). 'proseTables' tells the two
apart by their classification.
-}
module Hsed.Spec.Rules.Terminology
  ( terminologyRule
  , proseModule
    -- * Reading the bound tables
  , ProseTable (..)
  , proseTables
  ) where

import qualified RIO.Map          as Map
import           RIO

import           Hsed.Spec.Anchor (AnchorId (..))
import           Hsed.Spec.Common (RuleName (..), SpecId (..))
import           Hsed.Spec.DSL    (SomeBinding (..), prose)
import qualified Hsed.Spec.Gen    as Gen
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Shape  (CaptionKind (..), Form (..), Shape (..), shapeMismatch)


-- | The rule.
terminologyRule :: RuleDef
terminologyRule = RuleDef
  { ruleName    = RuleName "terminology"
  , bindsTo     = SomeBinding prose
  , exemplar    = (SpecId "core-2.01", TableId 1)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch Prose form) && fCaptionKind form == CaptionTerminology
  , ruleIdent   = const Nothing
  , ruleModule  = proseModule
  , ruleChecks  = \_ _ -> []
  , ruleCompile = \_ _ -> []
  }

-- | The module a prose table would go to; nothing is generated for one, so it is never written.
proseModule :: Gen.GenModule
proseModule = Gen.GenModule
  { Gen.leaf        = "Prose"
  , Gen.description = "Prose tables: none generates a definition"
  , Gen.notes       = [] }


-- * Reading the bound tables

-- | A table bound as prose, and whether the rule or a comment binds it.
data ProseTable = ProseTable
  { ptBound    :: !BoundTable
  , ptByRule   :: !Bool        -- ^ the caption's rule binds it (a terminology table), not a comment
  }

-- | Every table bound as prose, in anchor order.
proseTables :: Context -> [ProseTable]
proseTables ctx =
  [ ProseTable bt (isRuleBound (Map.lookup (btAnchor bt) (cxClasses ctx))) | bt <- boundTables (Just Prose) ctx ]
  where
    isRuleBound (Just (RuleBound r _ _)) = r == ruleName terminologyRule
    isRuleBound _                        = False
