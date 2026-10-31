{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Rules.Legend
Description : The legend rule: the shading and font scale of an SSC's preconfiguration tables
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design example E11. @legend@ fires on a table of at least two columns whose
first header cell mentions the legend: in Opal 2.30 Table 1, @Preconfiguration
Tables Legend@ (PDF 12), its rule's exemplar, alone; Core has none. Table 1
prints the scale the preconfiguration tables' cells are shaded and set by: two
greys, @(230, 230, 230)@ for a read-only cell and @(179, 179, 179)@ for a
writable one, each plain or bold and underlined, and @(N)@ for a column not
defined (its sample drawn white, so a text marker and never a colour). The
coloring "is informative only"; a preconfiguration cell's content is
normative (PDF 12), and Get access on three shaded read-only AccessControl
columns is (N) (PDF 39): the classes are documentation in the generated
@access@ lists and gate nothing (the lead's answer 2 to the M4b plan).

'legendScale' reads the scale from the spec's own legend table; 'cellAccess'
classifies one kept body cell of a preconfiguration table by its fill and its
style ('LegendClass'), for the preconfig rule's @access@ lists. The rule
generates nothing: Table 1 is documentation and the exemplar.

Codes: @C-LEGEND-TOTAL@ (an Error: a legend table that is not its scale, or a
printed cell with no style signal), @W-LEGEND-OFFSCALE@ (a cell shaded with a
grey off the scale, read as the nearest) and @W-LEGEND-STYLE@ (a cell with one
of the two style signals, read as its grey's bold and underlined class). At
M4b's scaffold the record is final and the bodies keep M4a's behaviour: no
scale, every cell 'Unspecified', no finding (the legend group, G1).
-}
module Hsed.Spec.Rules.Legend
  ( legendRule
    -- * The scale
  , LegendClass (..)
  , LegendScale (..)
  , legendScale
  , cellAccess
  ) where

import           RIO

import           Hsed.Core.Table  (Access (..))
import           Hsed.Spec.Anchor (AnchorId (..))
import           Hsed.Spec.Common (RuleName (..), Severity (..), SpecId (..))
import           Hsed.Spec.DSL    (SomeBinding (..), legend)
import qualified Hsed.Spec.Gen    as Gen
import           Hsed.Spec.IR     (CellIR)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Shape  (Shape (..), shapeMismatch)


-- | The rule.
legendRule :: RuleDef
legendRule = RuleDef
  { ruleName    = RuleName "legend"
  , bindsTo     = SomeBinding legend
  , exemplar    = (SpecId "opal-2.30", TableId 1)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch Legend form)
  , ruleIdent   = const Nothing
  , ruleModule  = legendModule
  , ruleCodes   =
      [ CodeDef "C-LEGEND-TOTAL"    [Error]   (Defect "a legend table that is not its scale, or a preconfiguration cell with no style signal")
      , CodeDef "W-LEGEND-OFFSCALE" [Warning] (Suspect "a preconfiguration cell shaded with a grey the legend does not print")
      , CodeDef "W-LEGEND-STYLE"    [Warning] (Suspect "a preconfiguration cell with one of the legend's two style signals") ]
  , ruleChecks  = \_ _ -> []
  , ruleCompile = \_ _ -> []
  }

-- | The module a legend would generate into; the rule generates nothing
--   (Table 1 is documentation), so no such module is written.
legendModule :: Gen.GenModule
legendModule = Gen.GenModule
  { Gen.leaf        = "Legend"
  , Gen.description = "The preconfiguration tables' legend"
  , Gen.notes       = [] }


-- * The scale

-- | One class of the scale: its grey (the printed RGB's channel), whether its
--   cells are bold and underlined, and the access it gives.
data LegendClass = LegendClass
  { lcGrey   :: !Int
  , lcStyled :: !Bool
  , lcAccess :: !Access
  }
  deriving (Eq, Show)

-- | The scale a legend table prints: its classes, the text marking a column
--   not defined (@(N)@), and how far from a printed grey a measured fill may lie.
data LegendScale = LegendScale
  { lsClasses    :: ![LegendClass]
  , lsNotDefined :: !Text
  , lsTolerance  :: !Int
  }
  deriving (Eq, Show)

-- | The scale of the spec's own legend table, when the rule binds one.
--   [WS G1: none]
legendScale :: Context -> Maybe LegendScale
legendScale _ = Nothing

-- | The access one kept body cell of a preconfiguration table has, with what
--   classifying it found (severity, code, message). [WS G1: 'Unspecified',
--   nothing found]
cellAccess :: LegendScale -> CellIR -> (Access, [(Severity, Text, Text)])
cellAccess _ _ = (Unspecified, [])
