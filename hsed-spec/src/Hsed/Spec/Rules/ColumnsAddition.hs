{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.ColumnsAddition
Description : The columnsAddition rule: the columns an SSC adds to a table another spec describes
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design example E5 and section 3.4. @columnsAddition@ fires on a table headed
as a description is (@[Column Number, Column Name, IsUnique, Column Type]@,
Opal T26's @Colum Type@ read through the header typo table) whose caption is
@\<T\> [Table] [Additional] Columns@, an SP's name and a dash before it
allowed: in Opal 2.30 Table 26 (@Admin SP – TPerInfo Columns@, PDF 49, its
exemplar), Table 43 (@Locking SP – LockingInfo Columns@, PDF 85) and Table 50
(@Table Table Additional Columns@, PDF 98-99), each adding columns to a table
Core describes (TPerInfo's 0x08, LockingInfo's 0x07-0x0A, Table's
0x0D-0x0E). The caption names the target ('additionTarget'), so no @extends@
comment is needed; the shape picks the three out across Opal, so the rule
rides on the @doc@ anchor. It refines @tableDesc@, whose shape is the same.

'additions' reads the bound tables as descriptions' rows; 'allDescs' is
every description a spec reads, its own and its base's, with its additions
appended to the description each targets (Opal's TPerInfo: Core T211's eight
columns, then T26's ProgrammaticResetEnable).

Code: @C-COLS-TARGET@ (an Error: a caption naming no table a description
describes, the spec's or its base's). At M4b's scaffold the reads and the
record are final; the checks give nothing and the compilation leaves each
bound table out (the Opal type layer's group, G3, writes both: the checks,
C-COL-CONTIG and C-COL-CELL over the extended columns, and the extended
descriptions in the spec's @TableDescs@).
-}
module Hsed.Spec.Rules.ColumnsAddition
  ( columnsAdditionRule
    -- * Reading the bound tables
  , AdditionRead (..)
  , additions
  , additionTarget
    -- * Descriptions with their additions
  , allDescs
  ) where

import qualified Data.Text                 as DT
import qualified RIO.Char                  as C
import qualified RIO.Set                   as Set
import qualified RIO.Text                  as T
import           RIO

import           Hsed.Spec.Anchor          (AnchorId (..))
import           Hsed.Spec.Common          (RuleName (..), Severity (..), SpecId (..))
import           Hsed.Spec.DSL             (SomeBinding (..), columnsAddition)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.TableDesc (ColRead (..), DescRead (..), descReads, readDescs, tableDescRule,
                                            tableDescsModule)
import           Hsed.Spec.Shape           (CaptionKind (..), Form (..), Shape (..), measure, shapeMismatch)


-- | The rule.
columnsAdditionRule :: RuleDef
columnsAdditionRule = RuleDef
  { ruleName    = RuleName "columnsAddition"
  , bindsTo     = SomeBinding columnsAddition
  , exemplar    = (SpecId "opal-2.30", TableId 26)
  , refinesRule = Just (ruleName tableDescRule)
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch ColumnsAddition form) && isJust (additionIdent form)
  , ruleIdent   = additionIdent
  , ruleModule  = tableDescsModule
  , ruleCodes   =
      [ CodeDef "C-COLS-TARGET" [Error] (Defect "a column addition's caption names no table a description describes, the spec's or its base's") ]
  , ruleChecks  = \_ _ -> []
  , ruleCompile = \_ bts -> [ Excluded (btAnchor bt) "the columns it adds are not compiled yet (M4b's scaffold)" | bt <- bts ]
  }

-- | The table a column addition's caption names.
additionIdent :: Form -> Maybe Text
additionIdent form = case fCaptionKind form of
  CaptionColumns s -> additionTarget s
  _                -> Nothing


-- * Reading the bound tables

-- | One column addition, read under the errata set: its table, the table its
--   caption names, and its rows of four cells, each a column as a
--   description's row is read.
data AdditionRead = AdditionRead
  { adBound   :: !BoundTable
  , adTarget  :: !(Maybe Text)
  , adColumns :: ![ColRead]
  }

-- | Every table bound to the rule's shape, read under the errata set, in anchor order.
additions :: Context -> [AdditionRead]
additions ctx =
  [ AdditionRead (drBound d) (additionIdent (measure (cxSpec ctx) (btRead (drBound d)))) (drColumns d)
  | d <- readDescs ctx (boundTables (Just ColumnsAddition) ctx) ]

-- | The table a column addition's caption names, from the caption's title
--   before @Columns@: an SP's name and a dash before it dropped, then a last
--   word @Additional@, then a last word @Table@; an identifier, or none.
--
-- >>> map additionTarget ["Admin SP \x2013 TPerInfo", "Table Table Additional", "Locking SP \x2013 LockingInfo", "Admin SP \x2013"]
-- [Just "TPerInfo",Just "Table",Just "LockingInfo",Nothing]
additionTarget :: Text -> Maybe Text
additionTarget title = mfilter isIdent (Just (dropLast "Table" (dropLast "Additional" afterDash)))
  where
    afterDash = T.strip (foldl' (\t sep -> snd (DT.breakOnEnd sep t)) title [" - ", " \x2013 ", " \x2014 "])
    dropLast w t = maybe t T.strip (T.stripSuffix (" " <> w) t)
    isIdent n = case T.uncons n of
      Just (c, cs) -> C.isAlpha c && T.all (\x -> C.isAlphaNum x || x == '_') cs
      Nothing      -> False


-- * Descriptions with their additions

-- | Every table description a spec reads, each with the Context that owns
--   it: the spec's own, each with the spec's additions to it appended; then
--   its base's (the base's own and its base's, likewise) whose table the
--   spec's own do not describe, those the spec adds columns to owned by the
--   spec, their columns the base's then the additions' in anchor order
--   (Opal's TPerInfo: Core T211's eight, then T26's 0x08
--   ProgrammaticResetEnable, PDF 49); then the rest of the base's, as the base
--   reads them.
allDescs :: Context -> [(Context, DescRead)]
allDescs ctx = own <> extended <> rest
  where
    adds      = additions ctx
    addsTo d  = [ a | a <- adds, isJust (adTarget a), adTarget a == drTable d ]
    extend d  = d { drColumns = drColumns d <> concatMap adColumns (addsTo d) }
    ownDescs  = descReads ctx
    ownTables = Set.fromList (mapMaybe drTable ownDescs)
    based     = [ cd | Just b <- [cxBase ctx], cd@(_, d) <- allDescs b, maybe True (`Set.notMember` ownTables) (drTable d) ]
    own       = [ (ctx, extend d) | d <- ownDescs ]
    extended  = [ (ctx, extend d) | (_, d) <- based, not (null (addsTo d)) ]
    rest      = [ cd | cd@(_, d) <- based, null (addsTo d) ]
