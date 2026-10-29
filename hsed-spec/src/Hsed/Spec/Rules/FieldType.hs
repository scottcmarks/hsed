{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.FieldType
Description : The fieldType rule: a record's fields, each with its Type object and range
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4. @fieldType@ fires on a table headed @[Field, Type]@ or
@[Value, Type, Range]@ (the @FieldType@ shape) every row of which names its
field and its type (the rule's own condition). In Core 2.01 those are Table 22
Secure Messaging Packet – Payload Field and Table 23 its SecureData Field
(section 3.2.3.5), and Table 34 Date Values, the ten-byte time of
VERIFY_COMID_VALID (section 3.3.4.7.4); the headers are theirs alone, so the
rule rides on the @doc@ anchor (design 13.24).

A Type cell names a Type object of the spec, a table the @typeRow@ rule binds
("Hsed.Spec.Rules.TypeRow"); a Range cell is one value or a range (@1970 to
9999@, read as a code cell is, "Hsed.Spec.Rules.Codes").

Check @C-FIELD-TYPE@ (Error): every row is as wide as the table and names its
field; no field is named twice; every type names a Type object that has a
UID; every Range cell is one value or an upward range, and its largest value
fits the bytes of a type sized in its Format (@uinteger_2@: two).

Compiled into the fields module ("Hsed.Spec.Rules.FieldValue".'fieldsModule'):
for each table @r\<Title\> :: [FieldDef]@ (the caption's words joined,
@rDateValues@), one 'Hsed.Core.Field.FieldDef' per row: the field, its type's
name and UID (@u\<type\>Type@ of the type rows' module) and its range.
-}
module Hsed.Spec.Rules.FieldType
  ( fieldTypeRule
    -- * Reading the bound tables
  , TypeFieldsRead (..)
  , TypeField (..)
  , typeFieldsReads
  , fieldBytes
  ) where

import qualified RIO.List                as L
import qualified RIO.Map                 as Map
import qualified RIO.Text                as T
import           RIO

import           Hsed.Core.Types         (Operand (..), TypeDef (..))
import           Hsed.Spec.Anchor        (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common        (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common        as Cm
import           Hsed.Spec.DSL           (SomeBinding (..), fieldType)
import qualified Hsed.Spec.Gen           as Gen
import           Hsed.Spec.IR            (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.Codes   (CodeCell (..), Number (..), codeCellExpr, listLines, parseCodeCell, titleIdent)
import           Hsed.Spec.Rules.FieldValue (fieldsModule)
import           Hsed.Spec.Rules.TypeRow (TypeRowRead (..), typeRowReads, typesModule)
import           Hsed.Spec.Shape         (Shape (..), captionTitle, normaliseCaption, shapeMismatch)


-- | The rule.
fieldTypeRule :: RuleDef
fieldTypeRule = RuleDef
  { ruleName    = RuleName "fieldType"
  , bindsTo     = SomeBinding fieldType
  , exemplar    = (SpecId "core-2.01", TableId 34)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ t form -> isNothing (shapeMismatch FieldType form)
                                && all (\r -> length r == t.nCols && all (not . T.null . T.strip . (.text)) (take 2 r)) t.rows
  , ruleIdent   = const Nothing
  , ruleModule  = fieldsModule
  , ruleChecks  = \ctx bts -> concatMap (checkFields ctx) (readFields ctx bts)
  , ruleCompile = compile
  }


-- * Reading the bound tables

-- | One @Field | Type (| Range)@ table read under the errata set.
data TypeFieldsRead = TypeFieldsRead
  { tfBound :: !BoundTable
  , tfTitle :: !Text              -- ^ the caption's title after errata, normalised
  , tfCols  :: !Int
  , tfRows  :: ![TypeField]
  }

-- | One row: a field, its type and the values its Range cell allows.
data TypeField = TypeField
  { fIndex     :: !Int                  -- ^ 0-based among the data rows
  , fName      :: !Text
  , fTypeName  :: !Text
  , fType      :: !(Maybe TypeRowRead)  -- ^ the Type object it names
  , fRangeCell :: !(Maybe Text)         -- ^ the Range cell of a three-column table
  , fRange     :: !(Maybe CodeCell)
  , fWidth     :: !Int                  -- ^ the row's cells
  }

-- | Every table bound to the rule's shape, read under the errata set, each
--   type joined to its Type object.
typeFieldsReads :: Context -> [TypeFieldsRead]
typeFieldsReads ctx = readFields ctx (boundTables (Just FieldType) ctx)

readFields :: Context -> [BoundTable] -> [TypeFieldsRead]
readFields ctx = map readOne
  where
    types = Map.fromListWith (\_ earlier -> earlier) [ (trName tr, tr) | tr <- rights (typeRowReads ctx) ]
    readOne bt = TypeFieldsRead
      { tfBound = bt
      , tfTitle = fromMaybe "" ((btRead bt).caption >>= captionTitle)
      , tfCols  = (btRead bt).nCols
      , tfRows  = zipWith (row (btRead bt).nCols) [0 ..] (btRead bt).rows }
    row :: Int -> Int -> [CellIR] -> TypeField
    row cols i cells =
      let cell :: Int -> Text
          cell k = T.strip (maybe "" (.text) (listToMaybe (drop k cells)))
          range = if cols >= 3 then Just (cell 2) else Nothing
      in TypeField
           { fIndex = i, fName = cell 0, fTypeName = cell 1, fType = Map.lookup (cell 1) types
           , fRangeCell = range, fRange = range >>= parseCodeCell, fWidth = length cells }

-- | The bytes a field's type takes when its Format sizes it (@uinteger_2@ is
--   @Simple_Type, uinteger, 2@): Table 34's ten.
fieldBytes :: TypeField -> Maybe Int
fieldBytes f = case fType f >>= either (const Nothing) Just . trTypeDef of
  Just (SimpleType _ (Lit n)) -> Just n
  _                           -> Nothing


-- * Checks

checkFields :: Context -> TypeFieldsRead -> [Finding]
checkFields ctx tf = map (atTable ctx (tfBound tf)) (concatMap rowProblems (tfRows tf) <> twice)
  where
    bad = finding Error "C-FIELD-TYPE"
    rowLabel f = "row " <> tshow (fIndex f + 1) <> (if T.null (fName f) then "" else " " <> fName f)
    rowProblems f
      | fWidth f /= tfCols tf = [bad (rowLabel f <> " has " <> tshow (fWidth f) <> " cells, not " <> tshow (tfCols tf))]
      | otherwise = map (bad . ((rowLabel f <> ": ") <>)) (catMaybes
          [ guarded (T.null (fName f)) "the field has no name"
          , case fType f of
              Nothing -> Just ("the type " <> tshow (fTypeName f) <> " names no Type object of the spec")
              Just tr -> guarded (isNothing (trUID tr)) ("the Type object " <> renderAnchorId (trAnchor tr) <> " " <> trName tr <> " has no UID")
          , rangeProblem f ])
    rangeProblem f = case (fRangeCell f, fRange f) of
      (Nothing, _) -> Nothing
      (Just cell, Nothing) -> Just ("the range " <> tshow cell <> " is not a value or a range")
      (Just cell, Just code) -> case code of
        CodeOne a -> tooBig f (nValue a)
        CodeBetween a b
          | nValue a > nValue b -> Just ("the range " <> tshow cell <> " runs downwards")
          | otherwise           -> tooBig f (nValue b)
        _ -> Just ("the range " <> tshow cell <> " is not a value or a range")
    tooBig f hi = case fieldBytes f of
      Just n | hi >= 256 ^ n -> Just ("the range reaches " <> tshow hi <> ", more than the " <> tshow n <> " bytes of " <> fTypeName f <> " hold")
      _                      -> Nothing
    guarded c msg = if c then Just msg else Nothing
    twice = concat [ [ bad (rowLabel f <> ": the field is also row " <> tshow (fIndex g + 1)) | f <- rest ]
                   | g : rest <- Map.elems (Map.fromListWith (flip (<>)) [ (fName f, [f]) | f <- tfRows tf, not (T.null (fName f)) ]) ]


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = map one (readFields ctx bts)
  where
    one tf
      | any Cm.isError (checkFields ctx tf) = Excluded (btAnchor (tfBound tf)) "C-FIELD-TYPE failed"
      | otherwise = Emit (fieldsDecl ctx tf)

-- | @r\<Title\> :: [FieldDef]@, each type by its UID's name in the type rows' module.
fieldsDecl :: Context -> TypeFieldsRead -> Gen.Decl
fieldsDecl ctx tf = (Gen.decl p name body)
  { Gen.declImports = [Gen.ImportSome Gen.fieldModule ["FieldDef (..)"], Gen.ImportSome "Prelude" ["Maybe (..)"]]
                      <> [ Gen.ImportSome Gen.codeModule ["Span (..)"] | any (isJust . fRange) (tfRows tf) ]
                      <> [ Gen.ImportSome (Gen.moduleName (cxSpec ctx) typesModule) (L.nub (map uidName (tfRows tf)))
                         | typesModule /= fieldsModule ] }
  where
    bt = tfBound tf
    p = provenanceOf ctx (btAnchor bt) (btTable bt) (normaliseCaption (fromMaybe "" (btRead bt).caption))
    name = "r" <> titleIdent (tfTitle tf)
    uidName f = "u" <> maybe (fTypeName f) trName (fType f) <> "Type"
    body =
      [ Gen.provenanceComment p
      , name <> " :: [FieldDef]" ]
      <> listLines (name <> " =")
           [ "FieldDef " <> tshow (fName f) <> " " <> tshow (fTypeName f) <> " " <> uidName f <> " "
               <> maybe "Nothing" (\c -> "(Just (" <> codeCellExpr c <> "))") (fRange f)
           | f <- tfRows tf ]
