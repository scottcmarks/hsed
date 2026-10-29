{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.ConstValues
Description : The constValues rule: a credential table's default column values, typed by its description
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4. @constValues@ fires on a table headed @[Column, Value]@:
in Core 2.01 the nine tables of elliptic curve domain parameters, T188, T190,
..., T204, each in the @Values for C_EC_\<n\>@ subsection of the section
whose description it fills (5.3.2.17-5.3.2.25; "These values are set as the
default values for the associated columns when a new row is created"). The
header is theirs alone across Core, so the rule rides on the @doc@ anchor
(design 13.24).

Each table belongs to the description its caption names (@AACS Values for
C_EC_160@: the @C_EC_160@ table) that also encloses it (the description's
section is the table's or contains it), read by the @tableDesc@ rule
("Hsed.Spec.Rules.TableDesc"); each row names one of its columns and gives the
column's value. A table prints its values in one radix: hexadecimal, in
FIPS 186's groups of hex digits, when some value has a space between digits
or a hex letter, else decimal (Table 188, whose section says "represented in
decimal format"). A value may carry a remark, checked against it: @(= 74 in
decimal)@, or @= 2192 – 264 – 1@, a sum of @1@ and powers of two whose
exponents the text prints flattened (@2192@ is 2^192).

Checks, each with the code its findings carry:

* @C-CONST-DESC@: the caption names a described table (@… Values for \<T\>@)
  whose description encloses the table, and no other table of values names
  it;
* @C-CONST-VALUE@: every row names a column of the description, no column
  twice, and its value reads as the column's type (an unsigned integer that
  fits its @uinteger_N@) and equals its remark.

Compiled into 'columnDefaultsModule': @\<t\>Defaults :: Row@ per table (the
described table's name, its first letter in lower case), each value a
'Hsed.Core.Table.UIntCell' by its column's number, printed in the table's
radix.
-}
module Hsed.Spec.Rules.ConstValues
  ( constValuesRule
  , columnDefaultsModule
    -- * Reading the bound tables
  , ConstRead (..)
  , ConstRow (..)
  , constReads
  , valuesFor
  , ConstNumber (..)
  , parseConstNumber
  , printedInHex
  ) where

import qualified Data.Text                as DT
import qualified Data.Text.Read           as TR
import qualified RIO.Char                 as C
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T
import           RIO

import           Hsed.Spec.Anchor         (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common         (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.DSL            (SomeBinding (..), constValues)
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.TableDesc (CellValue (..), ColKind (..), ColRead (..), DescRead (..), colKind,
                                           colNumberLit, descReads, descSection, renderCell)
import           Hsed.Spec.Scope          (Section (..), covers)
import           Hsed.Spec.Shape          (Shape (..), captionTitle, normaliseCaption, shapeMismatch)


-- | The rule.
constValuesRule :: RuleDef
constValuesRule = RuleDef
  { ruleName    = RuleName "constValues"
  , bindsTo     = SomeBinding constValues
  , exemplar    = (SpecId "core-2.01", TableId 190)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch ConstValues form)
  , ruleIdent   = const Nothing
  , ruleModule  = columnDefaultsModule
  , ruleChecks  = \ctx bts -> concatMap (constFindings ctx) (readConsts ctx bts)
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.ColumnDefaults@.
columnDefaultsModule :: Gen.GenModule
columnDefaultsModule = Gen.GenModule
  { Gen.leaf        = "ColumnDefaults"
  , Gen.description = "Default column values: what a new row of a table takes where the host gives none"
  , Gen.notes       = [ "Each value is printed in the radix its table prints it in." ] }


-- * Reading the bound tables

-- | One table of values read under the errata set.
data ConstRead = ConstRead
  { kBound :: !BoundTable
  , kNoun  :: !(Maybe Text)              -- ^ the table its caption names (@C_EC_160@)
  , kDesc  :: !(Either Text DescRead)    -- ^ the description it belongs to, or why there is none
  , kHex   :: !Bool                      -- ^ whether it prints its values in hex
  , kRows  :: ![ConstRow]
  }

-- | One row: a column and its value.
data ConstRow = ConstRow
  { cRow    :: !Int                      -- ^ 0-based among the data rows
  , cCells  :: ![CellIR]
  , cColumn :: !Text
  , cValue  :: !Text
  }

-- | Every table bound to the rule's shape, read under the errata set.
constReads :: Context -> [ConstRead]
constReads ctx = readConsts ctx (boundTables (Just ConstValues) ctx)

-- | The table of values whose description names a table, if one does.
valuesFor :: Context -> Text -> Maybe ConstRead
valuesFor ctx n = L.find ((== Just n) . kNoun) (constReads ctx)

readConsts :: Context -> [BoundTable] -> [ConstRead]
readConsts ctx = map readConst
  where
    descs = descReads ctx
    readConst bt = ConstRead
      { kBound = bt, kNoun = noun, kDesc = belongs, kHex = printedInHex (map cValue rows), kRows = rows }
      where
        rows = [ ConstRow i cells (cell 0 cells) (cell 1 cells) | (i, cells) <- zip [0 ..] (btRead bt).rows ]
        cell :: Int -> [CellIR] -> Text
        cell k cells = maybe "" (T.strip . (.text)) (listToMaybe (drop k cells))
        noun = (btRead bt).caption >>= captionTitle >>= valuesNoun
        here = sectionOf ctx bt
        belongs = case (noun, [ d | d <- descs, drTable d == noun, isJust noun ]) of
          (Nothing, _) -> Left "the caption names no table (\"… Values for <T>\")"
          (Just n, []) -> Left ("the caption names " <> n <> ", which no table description describes")
          (Just n, d : _)
            | Just s <- descSection ctx d, covers (Subtree s) here -> Right d
            | otherwise -> Left ("the caption names " <> n <> ", described by " <> renderAnchorId (btAnchor (drBound d))
                                 <> " in " <> fromMaybe "no section" (descSection ctx d) <> ", which does not enclose this table (in "
                                 <> fromMaybe "no section" here <> ")")

-- | The table a caption @\<…\> Values for \<T\>@ names.
valuesNoun :: Text -> Maybe Text
valuesNoun title = case DT.breakOnEnd " Values for " title of
  (before, n) | not (T.null before), not (T.null n), T.all (\c -> C.isAlphaNum c || c == '_') n -> Just n
  _ -> Nothing

-- | Whether a table's values are in hex: some value's number has a space
--   between its digits or a hex letter.
printedInHex :: [Text] -> Bool
printedInHex = any (\v -> let n = numberPart v in T.any C.isSpace n || T.any (`elem` ("abcdefABCDEF" :: String)) n)

-- | A value cell's number and remark: the text before its first @=@ or @(@, and the rest.
splitRemark :: Text -> (Text, Text)
splitRemark cell = let (n, r) = T.break (`elem` ("=(" :: String)) cell in (T.strip n, T.strip r)

-- | The number of a value cell, before its remark.
numberPart :: Text -> Text
numberPart = fst . splitRemark


-- | A value cell read: the number and the remark after it.
data ConstNumber = ConstNumber
  { cnValue  :: !Integer
  , cnRemark :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | Read a value cell in a radix (hex when True): digits, spaces between
--   groups allowed in hex, then an optional remark that must equal the
--   number: @(= N in decimal)@, or @= t ± t ± …@ whose terms are @1@ and
--   powers of two printed with their exponent flattened (@2192@ is 2^192).
parseConstNumber :: Bool -> Text -> Either Text ConstNumber
parseConstNumber hex cell = do
  let (number, remark) = splitRemark cell
  value <- digits number
  if T.null remark
    then Right (ConstNumber value Nothing)
    else do
      said <- remarkValue remark
      if said == value
        then Right (ConstNumber value (Just remark))
        else Left ("the value is " <> tshow value <> " and its remark " <> tshow remark <> " says " <> tshow said)
  where
    digits t
      | T.null t = Left "there is no number"
      | hex, T.all (\c -> C.isHexDigit c || c == ' ') t = readWith TR.hexadecimal (T.filter (/= ' ') t)
      | not hex, T.all C.isDigit t = readWith TR.decimal t
      | otherwise = Left (tshow t <> " is not a " <> (if hex then "hexadecimal" else "decimal") <> " number")
    readWith r t = either (Left . T.pack) (\(v, rest) -> if T.null rest then Right v else Left ("trailing " <> tshow rest)) (r t)
    remarkValue r
      | Just inner <- T.stripPrefix "(=" r >>= T.stripSuffix ")"
      , [n, "in", "decimal"] <- T.words inner
      , T.all C.isDigit n = readWith TR.decimal n
      | Just sumText <- T.stripPrefix "=" r = powerSum (T.words (T.map dash sumText))
      | otherwise = Left ("the remark " <> tshow r <> " is neither (= N in decimal) nor = a sum of powers of two")
    dash c = if c `elem` ("–—" :: String) then '-' else c
    -- t (± t)*: each t is 1 or 2<k> (2^k)
    powerSum ws = case ws of
      t : rest -> do
        v <- term t
        foldM step v (pairs rest)
      [] -> Left "the remark's sum is empty"
    pairs (op : t : rest) = (op, t) : pairs rest
    pairs _               = []
    step acc (op, t) = do
      v <- term t
      case op of
        "+" -> Right (acc + v)
        "-" -> Right (acc - v)
        _   -> Left ("the remark has " <> tshow op <> " where + or - is expected")
    term t
      | t == "1" = Right 1
      | Just k <- T.stripPrefix "2" t, not (T.null k), T.all C.isDigit k
      , Right (e, "") <- TR.decimal k = Right (2 ^ (e :: Int))
      | otherwise = Left ("the remark's term " <> tshow t <> " is neither 1 nor 2 with an exponent")


-- * Checks

-- | A finding about one row, located on the page the row is printed on.
atCells :: Context -> BoundTable -> [CellIR] -> Finding -> Finding
atCells ctx bt cells = case listToMaybe (mapMaybe (\c -> c.page) cells) of
  Just p0 -> locatedAt (cxSpec ctx) (renderAnchorId (btAnchor bt)) (p0 + 1)
  Nothing -> atTable ctx bt

-- | Every finding about one table of values: its description, then each row.
constFindings :: Context -> ConstRead -> [Finding]
constFindings ctx k = case kDesc k of
  Left why -> [atTable ctx (kBound k) (finding Error "C-CONST-DESC" why)]
  Right d  -> others d <> concatMap (either pure (const []) . rowValue ctx k d) (kRows k) <> repeated
  where
    -- another table of values naming the same table, before this one
    others d = [ atTable ctx (kBound k) (finding Error "C-CONST-DESC"
                   ("the caption names " <> fromMaybe "" (drTable d) <> ", whose values "
                     <> renderAnchorId (btAnchor (kBound k0)) <> " gives already"))
               | Just k0 <- [listToMaybe [ k' | k' <- constReads ctx, kNoun k' == kNoun k, isRight (kDesc k')
                                              , btAnchor (kBound k') < btAnchor (kBound k) ]] ]
    repeated = concat
      [ [ atCells ctx (kBound k) (cCells r) (finding Error "C-CONST-VALUE" (rowLabel r <> "the column is also given by row " <> tshow (cRow r0 + 1)))
        | r <- rest ]
      | r0 : rest <- Map.elems (Map.fromListWith (flip (<>)) [ (cColumn r, [r]) | r <- kRows k ]) ]

rowLabel :: ConstRow -> Text
rowLabel r = "row " <> tshow (cRow r + 1) <> " " <> cColumn r <> ": "

-- | One row's column and value, or its C-CONST-VALUE finding.
rowValue :: Context -> ConstRead -> DescRead -> ConstRow -> Either Finding (ColRead, CellValue)
rowValue ctx k d r = first (atCells ctx (kBound k) (cCells r) . finding Error "C-CONST-VALUE" . (rowLabel r <>)) $ do
  col <- maybe (Left ("names no column of " <> renderAnchorId (btAnchor (drBound d)) <> " " <> fromMaybe "" (drTable d))) Right
               (L.find ((== cColumn r) . crName) (drColumns d))
  kind <- colKind ctx (crType col)
  number <- parseConstNumber (kHex k) (cValue r)
  let digits = if kHex k then Just (T.length (T.filter C.isHexDigit (numberPart (cValue r)))) else Nothing
  case kind of
    KUInt n
      | cnValue number < 256 ^ n -> Right (col, CvUInt (cnValue number) digits)
      | otherwise -> Left ("the value does not fit in " <> crType col <> " (" <> tshow n <> " bytes)")
    _ -> Left ("the column's type " <> crType col <> " is not an unsigned integer type")


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = map one (readConsts ctx bts)
  where
    one k = case kDesc k of
      Right d | not (any Cm.isError (constFindings ctx k)), Just n <- drTable d
              , Right values <- traverse (rowValue ctx k d) (kRows k)
              , Right cells <- traverse (renderCell . snd) values ->
          Emit (defaultsDecl ctx k d n (zip (map fst values) cells))
      _ -> Excluded (btAnchor (kBound k)) "C-CONST-DESC or C-CONST-VALUE failed"

-- | @\<t\>Defaults :: Row@, its values in column order.
defaultsDecl :: Context -> ConstRead -> DescRead -> Text -> [(ColRead, Text)] -> Gen.Decl
defaultsDecl ctx k d n values = (Gen.decl p name body)
  { Gen.declImports = [Gen.ImportSome Gen.coreTableModule ["Cell (..)", "Row"]] }
  where
    bt   = kBound k
    p    = provenanceOf ctx (btAnchor bt) (btTable bt) (normaliseCaption (fromMaybe "" (btRead bt).caption))
    name = maybe n (\(c, cs) -> T.cons (C.toLower c) cs) (T.uncons n) <> "Defaults"
    sorted = L.sortOn (crNumber . fst) values
    item i (col, cell) = (if i == (0 :: Int) then "  [ " else "  , ")
      <> "(" <> colNumberLit (fromMaybe 0 (crNumber col)) <> ", " <> cell <> ")  -- " <> crName col
    body =
      [ Gen.provenanceComment p <> ": the defaults of " <> n <> " (" <> renderAnchorId (btAnchor (drBound d)) <> "), in "
          <> (if kHex k then "hex" else "decimal") <> " as printed"
      , name <> " :: Row"
      , name <> " =" ]
      <> zipWith item [0 ..] sorted
      <> [ "  ]" ]
