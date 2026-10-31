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

Checks, each with the code its findings carry:

* @C-COLS-TARGET@ (an Error, claimed here): the caption names a table a
  description describes, the spec's or its base's;
* @C-COL-CONTIG@, @C-COL-CELL@ and @C-FMT-RESOLVE@, @tableDesc@'s
  ("Hsed.Spec.Rules.TableDesc".'columnFindings') over the added columns,
  their numbers running on from the description's (and from those of the
  additions to the same table before them, in anchor order): T26's 0x08
  after TPerInfo's 0x00-0x07 (Core T211, PDF 229), T43's 0x07-0x0A after
  LockingInfo's 0x00-0x06 (Core T225, PDF 272), T50's 0x0D-0x0E after
  Table's 0x00-0x0C (Core T171, PDF 156-157); a name a column of the
  description gives is C-COL-CELL. T43 prints its 0x0A LowestAlignedLBA's
  type @uniteger_8@ (PDF 85), which no type row defines: C-FMT-RESOLVE, a
  Warning, the column generated with its type's name and no UID.

Compiled into 'tableDescsModule' (@Hsed.Gen.Opal.TableDescs@): for each
table the additions extend, the base's description with the added columns
appended, under its name (@tPerInfoTableDesc@, 9 columns; @tableTableDesc@,
15; @lockingInfoTableDesc@, 11), by "Hsed.Spec.Rules.TableDesc".'tableDescDecl'
(each name imported from the module of the spec that defines it), citing
the first addition as its provenance and then the description it extends
and any further addition. An Error of any addition to the table leaves them
all out; so does a table the spec describes itself, which hsedc compiles
with no columns added (the spec's 'tableDescRule' prints it).
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
import qualified RIO.List                  as L
import qualified RIO.Set                   as Set
import qualified RIO.Text                  as T
import           RIO

import           Hsed.Spec.Anchor          (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common          (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.DSL             (SomeBinding (..), columnsAddition)
import qualified Hsed.Spec.Gen             as Gen
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.TableDesc (ColRead (..), ColumnsFrom (..), DescRead (..), columnFindings, descReads,
                                            readDescs, tableDescDecl, tableDescRule, tableDescsModule, tableUIDRowNamed)
import           Hsed.Spec.Rules.TableUIDs (TableUIDRow (..))
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
  , ruleChecks  = \ctx bts -> concatMap (extensionFindings ctx) (extensions ctx bts)
  , ruleCompile = compile
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
additions ctx = [ AdditionRead (drBound d) target (drColumns d) | (d, target) <- addedBy ctx (boundTables (Just ColumnsAddition) ctx) ]

-- | Tables read as column additions, in their order: each one's rows as a
--   description's, with the table its caption names.
addedBy :: Context -> [BoundTable] -> [(DescRead, Maybe Text)]
addedBy ctx bts = [ (d, additionIdent (measure (cxSpec ctx) (btRead (drBound d)))) | d <- readDescs ctx bts ]

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


-- * Checks

-- | One addition with what it extends: the table its caption names, that
--   table's description (the spec's own, else its base's as 'allDescs' of
--   the base gives it), with the Context that owns it, and the columns before
--   its own, the description's and then those of the additions to the same
--   table before it, in anchor order, with the tables they are read from.
data Extension = Extension
  { exRead    :: !DescRead
  , exTarget  :: !(Maybe Text)
  , exDesc    :: !(Maybe (Context, DescRead))
  , exBefore  :: ![ColRead]
  , exSources :: ![Text]          -- ^ @core-2.01 T211@, then any earlier addition's @opal-2.30 T\<n\>@
  }

-- | The bound tables as additions, in anchor order.
extensions :: Context -> [BoundTable] -> [Extension]
extensions ctx bts = zipWith extension reads (L.inits reads)
  where
    reads = addedBy ctx bts
    extension (d, target) earlier =
      let desc  = target >>= describedIn ctx
          prior = [ d' | isJust target, (d', t') <- earlier, t' == target ]
      in Extension
           { exRead    = d
           , exTarget  = target
           , exDesc    = desc
           , exBefore  = maybe [] (drColumns . snd) desc <> concatMap drColumns prior
           , exSources = [ cited c (drBound dd) | Just (c, dd) <- [desc] ] <> map (cited ctx . drBound) prior }
    cited c bt = specIdText (cxSpec c) <> " " <> renderAnchorId (btAnchor bt)

-- | The description of a table: the spec's own, else its base's, as the
--   base reads it ('allDescs'), with the Context that owns it.
describedIn :: Context -> Text -> Maybe (Context, DescRead)
describedIn ctx n = L.find ((== Just n) . drTable . snd) ([ (ctx, d) | d <- descReads ctx ] <> maybe [] allDescs (cxBase ctx))

-- | Every finding about one addition: C-COLS-TARGET at the table, and the
--   description checks of its columns ('columnFindings'), their numbers
--   running on from the columns it extends (where no description is found,
--   only the numbers' form is checked).
extensionFindings :: Context -> Extension -> [Finding]
extensionFindings ctx e = targetFindings <> columnFindings ctx bt from (drShort d) (drColumns d)
  where
    d  = exRead e
    bt = drBound d
    from = case (exTarget e, exDesc e) of
      (Just n, Just _) -> After n (exSources e) (exBefore e)
      _                -> Unplaced
    specs = "of " <> T.intercalate " or " (map (specIdText . cxSpec) (chain ctx))
    chain c = c : maybe [] chain (cxBase c)
    targetFindings = case (exTarget e, exDesc e) of
      (Nothing, _)      -> [atTable ctx bt (finding Error "C-COLS-TARGET" "the caption names no table (\"<T> [Table] [Additional] Columns\")")]
      (Just n, Nothing) -> [atTable ctx bt (finding Error "C-COLS-TARGET"
                              ("the caption names " <> n <> ", which no table description " <> specs <> " describes"))]
      _                 -> []


-- * Declarations

-- | For each table the additions extend (in the order of its first addition),
--   the description they extend with their columns appended, under the first
--   addition's provenance; or each addition left out, and why.
compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = concatMap one (L.nub (map exTarget exts))
  where
    exts = extensions ctx bts
    one target =
      let es       = [ e | e <- exts, exTarget e == target ]
          leaveOut why = [ Excluded (btAnchor (drBound (exRead e))) why | e <- es ]
          failed   = any (any Cm.isError . extensionFindings ctx) es
      in case (target, es, failed, exDesc =<< listToMaybe es) of
           (Just n, e0 : more, False, Just (owner, d))
             | cxSpec owner == cxSpec ctx ->
                 leaveOut (n <> " is described by " <> specIdText (cxSpec ctx) <> " itself, which hsedc compiles with no columns added")
             | Just row@(_, tu) <- tableUIDRowNamed ctx n, isJust (tuObject tu), isJust (tuTable tu) ->
                 [ Emit (tableDescDecl ctx (boundProvenance ctx (drBound (exRead e0)))
                           (citing "extends" (boundProvenance owner (drBound d))
                              : map (citing "with" . boundProvenance ctx . drBound . exRead) more)
                           d { drColumns = drColumns d <> concatMap (drColumns . exRead) es } row) ]
             | otherwise -> leaveOut ("no row of the table of table UIDs gives " <> n <> " its UIDs")
           _ -> leaveOut "C-COL-CONTIG, C-COL-CELL or C-COLS-TARGET failed"

-- | A provenance as a line continuing a declaration's comment:
--   @--   extends core-2.01 T211 TPerInfo Table Description (PDF 229 \/ p.212, ...)@.
citing :: Text -> Gen.Provenance -> Text
citing verb p = "--   " <> verb <> " " <> fromMaybe line (T.stripPrefix "-- | " line)
  where line = Gen.provenanceComment p
