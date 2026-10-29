{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.FieldValue
Description : The fieldValue rule: a record's fields, each with its constant
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4. @fieldValue@ fires on a table headed @[FIELD, VALUE]@ (the
@FieldValue@ shape) every row of which names its field (the rule's own
condition). In Core 2.01 those are the command blocks of the TPer and
Communication layer commands, Table 27 GET_COMID, Table 29
HANDLE_COMID_REQUEST and Table 30 GET_COMID_RESPONSE (section 3.3.4); the
header is theirs alone, so the rule rides on the @doc@ anchor (design 13.24).

A VALUE cell is a constant in hex, as bytes (@02@, @00 01@) or as a number
(@0x0004@, Opal Table 13), or else a name for a value the text defines
(@IF-RECV@, @Allocated ComID@, @Request_ComID@).

Check @C-FIELD-VALUE@ (Error): every row is a field and a value, both given,
and no field is named twice.

Compiled into 'fieldsModule': for each table @v\<Title\> :: [FieldConst]@ (the
caption's words joined, @vGET_COMIDCommandBlock@), one
'Hsed.Core.Field.FieldConst' per row. The @fieldType@ rule
("Hsed.Spec.Rules.FieldType") writes the same module.
-}
module Hsed.Spec.Rules.FieldValue
  ( fieldValueRule
  , fieldsModule
    -- * Reading the bound tables
  , ValueRead (..)
  , ValueRow (..)
  , valueReads
  , FieldValue' (..)
  , parseFieldValue
  ) where

import qualified Data.Char             as DC
import qualified Data.Text             as DT
import qualified RIO.Char              as C
import qualified RIO.Map               as Map
import qualified RIO.Text              as T
import           RIO

import           Hsed.Spec.Anchor      (AnchorId (..))
import           Hsed.Spec.Common      (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common      as Cm
import           Hsed.Spec.DSL         (SomeBinding (..), fieldValue)
import qualified Hsed.Spec.Gen         as Gen
import           Hsed.Spec.IR          (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.Codes (listLines, titleIdent)
import           Hsed.Spec.Shape       (Shape (..), captionTitle, normaliseCaption, shapeMismatch)


-- | The rule.
fieldValueRule :: RuleDef
fieldValueRule = RuleDef
  { ruleName    = RuleName "fieldValue"
  , bindsTo     = SomeBinding fieldValue
  , exemplar    = (SpecId "core-2.01", TableId 27)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ t form -> isNothing (shapeMismatch FieldValue form) && all named t.rows
  , ruleIdent   = const Nothing
  , ruleModule  = fieldsModule
  , ruleChecks  = \ctx bts -> concatMap (checkValues ctx) (readValues bts)
  , ruleCompile = compile
  }
  where
    named = \case
      [field, _] -> not (T.null (T.strip field.text))
      _          -> False

-- | @Hsed.Gen.\<Spec\>.Fields@, which the @fieldType@ rule writes too.
fieldsModule :: Gen.GenModule
fieldsModule = Gen.GenModule
  { Gen.leaf        = "Fields"
  , Gen.description = "Record tables: each field's constant, or each field's Type object and range"
  , Gen.notes       = [ "A FIELD | VALUE table is a list of FieldConst, a Field | Type (| Range) table a list"
                      , "of FieldDef (Hsed.Core.Field), its rows in order and its text as printed." ] }


-- * Reading the bound tables

-- | One @FIELD | VALUE@ table read under the errata set.
data ValueRead = ValueRead
  { vrBound :: !BoundTable
  , vrTitle :: !Text            -- ^ the caption's title after errata, normalised
  , vrRows  :: ![ValueRow]
  }

-- | One row: a field and its value.
data ValueRow = ValueRow
  { vwIndex :: !Int             -- ^ 0-based among the data rows
  , vwField :: !Text
  , vwCell  :: !Text            -- ^ the VALUE cell
  , vwWidth :: !Int             -- ^ the row's cells
  }

-- | A VALUE cell read.
data FieldValue'
  = Constant' ![Word8]          -- ^ bytes in hex: @02@, @00 01@, @0x0004@
  | Symbolic' !Text             -- ^ a name for a value the text defines
  deriving (Eq, Show)

-- | Every table bound to the rule's shape, read under the errata set.
valueReads :: Context -> [ValueRead]
valueReads ctx = readValues (boundTables (Just FieldValue) ctx)

readValues :: [BoundTable] -> [ValueRead]
readValues = map readOne
  where
    readOne bt = ValueRead
      { vrBound = bt
      , vrTitle = fromMaybe "" ((btRead bt).caption >>= captionTitle)
      , vrRows  = zipWith row [0 ..] (btRead bt).rows }
    row :: Int -> [CellIR] -> ValueRow
    row i cells = ValueRow
      { vwIndex = i
      , vwField = maybe "" (T.strip . (.text)) (listToMaybe cells)
      , vwCell  = maybe "" (T.strip . (.text)) (listToMaybe (drop 1 cells))
      , vwWidth = length cells }

-- | A VALUE cell: bytes when every word is two hex digits (@00 01@) or the
--   cell is one hex number (@0x0004@, two digits a byte, the first byte
--   padded), else a name.
parseFieldValue :: Text -> FieldValue'
parseFieldValue cell
  | not (null ws), all byteWord ws                           = Constant' (map hexByte ws)
  | [w] <- ws, Just h <- T.stripPrefix "0x" (T.toLower w)
  , not (T.null h), T.all C.isHexDigit h                     = Constant' (map hexByte (DT.chunksOf 2 (padded h)))
  | otherwise                                                = Symbolic' (T.unwords ws)
  where
    ws = T.words cell
    byteWord w = T.length w == 2 && T.all C.isHexDigit w
    padded h = if odd (T.length h) then "0" <> h else h
    hexByte = T.foldl' (\acc c -> acc * 16 + fromIntegral (DC.digitToInt c)) 0


-- * Checks

checkValues :: Context -> ValueRead -> [Finding]
checkValues ctx vr = map (atTable ctx (vrBound vr)) (widths <> missing <> twice)
  where
    rows = vrRows vr
    rowLabel r = "row " <> tshow (vwIndex r + 1)
    bad = finding Error "C-FIELD-VALUE"
    widths = [ bad (rowLabel r <> " has " <> tshow (vwWidth r) <> " cells, not a field and a value") | r <- rows, vwWidth r /= 2 ]
    missing = [ bad (rowLabel r <> ": " <> what) | r <- rows, vwWidth r == 2
              , what <- [ "the field has no name" | T.null (vwField r) ] <> [ "the field " <> vwField r <> " has no value" | T.null (vwCell r) ] ]
    twice = concat [ [ bad (rowLabel r <> ": the field " <> vwField r <> " is also row " <> tshow (vwIndex f + 1)) | r <- rest ]
                   | f : rest <- Map.elems (Map.fromListWith (flip (<>)) [ (vwField r, [r]) | r <- rows, not (T.null (vwField r)) ]) ]


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = map one (readValues bts)
  where
    one vr
      | any Cm.isError (checkValues ctx vr) = Excluded (btAnchor (vrBound vr)) "C-FIELD-VALUE failed"
      | otherwise = Emit (valueDecl ctx vr)

-- | @v\<Title\> :: [FieldConst]@.
valueDecl :: Context -> ValueRead -> Gen.Decl
valueDecl ctx vr = (Gen.decl p name body)
  { Gen.declImports = [Gen.ImportSome Gen.fieldModule ["FieldConst (..)", "FieldValue (..)"]] }
  where
    bt = vrBound vr
    p = provenanceOf ctx (btAnchor bt) (btTable bt) (normaliseCaption (fromMaybe "" (btRead bt).caption))
    name = "v" <> titleIdent (vrTitle vr)
    body =
      [ Gen.provenanceComment p
      , name <> " :: [FieldConst]" ]
      <> listLines (name <> " =") [ "FieldConst " <> tshow (vwField r) <> " (" <> valueExpr (parseFieldValue (vwCell r)) <> ")" | r <- vrRows vr ]
    valueExpr = \case
      Constant' bs -> "Constant [" <> T.intercalate ", " (map Gen.hexByte bs) <> "]"
      Symbolic' t  -> "Symbolic " <> tshow t
