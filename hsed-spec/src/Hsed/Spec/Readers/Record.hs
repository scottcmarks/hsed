{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Readers.Record
Description : A structure read from a table, its names and its record over its layout
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What the @byteGrid@, @byteFieldValue@ and @feature@ rules print the same way
(design examples E7 and E8; docs/adding-a-rule.md): a 'Structure' is a table
read as a byte grid, its layout ("Hsed.Spec.Readers.Layout".'gridLayout',
named by the caption's title), each field of it with the grid field it was
read from, and the name of its record (the structure the caption names, or a
@layout "Name"@ modifier's name as written). From it come one convention of
names ('typeName', 'layoutValue', 'fieldIdent': @ComPacket@,
@comPacketLayout@, @comPacketComID@), the record of the fields that carry a
value, each typed by its width ('haskellType': 'Word8' to 'Word64',
'Natural' beyond, 'Bool' for a one-bit flag, 'ByteString' for a run of more
than eight whole bytes), and its instance of
"Hsed.Core.Record".@LayoutRecord@ ('recordDecl'); a structure with a variable
tail gives two records, the fixed part's (@ComPacketHeader@) and the whole
(@ComPacket@), the whole the instance, as example E7 has them; a reserved tail
is the layout's, as reserved bits are ('recordTail'). A structure whose layout
does not tile has no record ('stTiles').

@C-GRID-NAME@ ('nameFindings'), whichever rule prints the structure: a field
that carries a value without a name that makes an identifier, two fields named
alike, a name the declarations of a structure before it in its module define
first, or a name the module imports unqualified.
-}
module Hsed.Spec.Readers.Record
  ( -- * A structure and its names
    Structure (..)
  , structure
  , typeName
  , valuePrefix
  , fieldIdent
  , layoutValue
  , recordNames
    -- * Its record
  , RecordField (..)
  , recordFieldsOf
  , recordTail
  , haskellType
  , spanText
  , recordDecl
    -- * Checks
  , nameFindings
  , earlierNames
  , declScope
    -- * Codes
  , recordCodes
  ) where

import qualified Data.Text                as DT
import qualified RIO.Char                 as C
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T
import           RIO

import           Hsed.Core.Feature        (FeatureHeader (..))
import           Hsed.Core.Layout         (Field (..), FieldKind (..), Layout (..), Tail (..), bitPosition, fieldEnd)
import           Hsed.Spec.Common         (CodeDef (..), Finding, Reports (..), Severity (..), finding)
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (CellIR)
import           Hsed.Spec.Readers.Grid   (Grid, GridField (..))
import           Hsed.Spec.Readers.Layout (gridFields, gridLayout, gridTiling)
import           Hsed.Spec.Readers.Text   (lowerIdent, titleIdent)
import           Hsed.Spec.RuleDef        (BoundTable (..), Context, atCells, atTable, boundProvenance)


-- * A structure and its names

-- | A table read as a structure of bytes.
data Structure = Structure
  { stBound     :: !BoundTable
  , stName      :: !Text                  -- ^ the record's name: the structure the caption names, or @layout "Name"@ as written
  , stLayout    :: !Layout                -- ^ its constants applied; named by the caption's title
  , stFields    :: ![(Field, GridField)]  -- ^ the layout's fixed fields, in order, each with the grid field it was read from
  , stTiles     :: !Bool                  -- ^ whether its layout tiles, so that it has a record
  , stConstants :: ![Text]                -- ^ what the record's comment says of where the layout's constants come from
  }

-- | A bound table's structure from its grid: the record's name, the
--   layout's name (the caption's title), the grid, and each field as the rule
--   leaves it (its constants pinned), given the grid field it was read from.
structure :: BoundTable -> Text -> Text -> Grid -> ((Field, GridField) -> Field) -> Structure
structure bt name title g pin = Structure
  { stBound     = bt
  , stName      = name
  , stLayout    = l
  , stFields    = pairs
  , stTiles     = null (gridTiling g l)
  , stConstants = [] }
  where
    pairs = [ (pin fg, snd fg) | fg <- gridFields g ]
    l = (gridLayout title g) { layoutFields = map fst pairs }

-- | The text an identifier is made from: parentheses dropped, their letters
--   kept (@Feature Descriptor(s)@ reads @Feature Descriptors@).
identText :: Text -> Text
identText = T.filter (`notElem` ("()" :: String))

-- | The record type's name: @ComPacket@.
typeName :: Structure -> Text
typeName st = titleIdent (identText (stName st))

-- | The prefix of its values' names: @comPacket@.
valuePrefix :: Structure -> Text
valuePrefix st = lowerIdent (identText (stName st))

-- | The record field of a layout field: @comPacketComID@.
fieldIdent :: Structure -> Text -> Text
fieldIdent st n = valuePrefix st <> titleIdent (identText n)

-- | The layout's value: @comPacketLayout@.
layoutValue :: Structure -> Text
layoutValue st = valuePrefix st <> "Layout"

-- | The fixed part's record where the layout has a tail: @ComPacketHeader@,
--   and the whole's field holding it, @comPacketHeader@.
headerType, headerField :: Structure -> Text
headerType st = typeName st <> "Header"
headerField st = valuePrefix st <> "Header"

-- | Every name a structure's declarations define: the layout, the record types
--   and their constructors (one name each), and the record fields.
recordNames :: Structure -> [Text]
recordNames st =
  [layoutValue st, typeName st]
  <> concat [ [headerType st, headerField st, fieldIdent st (tailName t)] | Just t <- [recordTail (stLayout st)] ]
  <> map rfIdent (recordFieldsOf st)


-- * Its record

-- | A field of the record: a field of the layout that carries a value.
data RecordField = RecordField
  { rfName    :: !Text   -- ^ the layout's name for it, the key of its value
  , rfIdent   :: !Text   -- ^ the record field: @comPacketComID@
  , rfType    :: !Text   -- ^ @Word16@, @Bool@, @ByteString@
  , rfKind    :: !FieldKind
  , rfWhere   :: !Text   -- ^ @bytes 4 to 5@
  , rfPrinted :: !Text   -- ^ its name as the grid prints it, the marks removed
  , rfCells   :: ![CellIR]
  }

-- | The tail a record carries: the layout's, unless its bytes are reserved
--   (@byteFieldValue@'s runs to the end of the transfer, @46 to TRNSFLEN - 1
--   | Reserved@), which, like reserved bits, is the layout's: encoding writes
--   none of it, decoding reports it read other than zero.
recordTail :: Layout -> Maybe Tail
recordTail l = mfilter ((/= Reserved) . tailKind) (layoutTail l)

-- | The fields of a structure's record, in order: every field of its layout
--   but the reserved ones, the pinned ones and the one that gives its tail's
--   length.
recordFieldsOf :: Structure -> [RecordField]
recordFieldsOf st =
  [ RecordField (fieldName f) (fieldIdent st (fieldName f)) (haskellType f) (fieldKind f) (spanText f) (gfName gf) (gfCells gf)
  | (f, gf) <- stFields st
  , carriesValue (fieldKind f)
  , Just (fieldName f) /= (layoutTail (stLayout st) >>= tailLength) ]
  where
    carriesValue = \case
      Reserved -> False
      Pinned _ -> False
      _        -> True

-- | The Haskell type of a field's value: a flag a 'Bool', a byte string a
--   'ByteString', an unsigned number the narrowest of @Word8@ to @Word64@ that
--   holds it, 'Natural' when none does.
haskellType :: Field -> Text
haskellType f = case fieldKind f of
  Flag  -> "Bool"
  Bytes -> "ByteString"
  _ | w <= 8    -> "Word8"
    | w <= 16   -> "Word16"
    | w <= 32   -> "Word32"
    | w <= 64   -> "Word64"
    | otherwise -> "Natural"
  where w = fieldWidth f

-- | Where a field lies: @byte 3@, @bytes 4 to 5@, @byte 2 bits 7-4@, @byte 4
--   bit 6@, @byte 4 bit 3 to byte 7 bit 4@.
spanText :: Field -> Text
spanText f
  | h == 7, l == 0, b0 == b1 = "byte " <> tshow b0
  | h == 7, l == 0           = "bytes " <> tshow b0 <> " to " <> tshow b1
  | b0 == b1                 = "byte " <> tshow b0 <> (if h == l then " bit " <> tshow h else " bits " <> tshow h <> "-" <> tshow l)
  | otherwise                = "byte " <> tshow b0 <> " bit " <> tshow h <> " to byte " <> tshow b1 <> " bit " <> tshow l
  where
    (b0, h) = bitPosition (fieldStart f)
    (b1, l) = bitPosition (fieldEnd f - 1)

-- | The record(s) of a structure and the instance over its layout, rank 1
--   among its table's declarations (its layout's is 0).
recordDecl :: Context -> Structure -> Gen.Decl
recordDecl ctx st = (Gen.decl p (typeName st) body)
  { Gen.declRank    = 1
  , Gen.declImports =
      [ Gen.ImportSome Gen.recordModule (["LayoutRecord (..)"] <> L.nub (concatMap helpers fields) <> [ h | isJust tl, h <- ["octets", "octetsField"] ])
      , Gen.ImportSome "Prelude" (["Eq", "Show", "pure"] <> [ "(<*>)" | not (null fields) || isJust tl ] <> [ "Bool" | "Bool" `elem` types ]) ]
      <> [ Gen.ImportSome "Data.Word" ws | let ws = L.nub [ t | t <- types, "Word" `T.isPrefixOf` t ], not (null ws) ]
      <> [ Gen.ImportSome "Data.ByteString" ["ByteString"] | isJust tl || "ByteString" `elem` types ]
      <> [ Gen.ImportSome "Numeric.Natural" ["Natural"] | "Natural" `elem` types ] }
  where
    p = boundProvenance ctx (stBound st)
    l = stLayout st
    tl = recordTail l
    fields = recordFieldsOf st
    types = map rfType fields
    doc = Gen.provenanceComment p
    helpers f = case rfKind f of
      Flag  -> ["flag", "flagField"]
      Bytes -> ["octets", "octetsField"]
      _     -> ["number", "numberField"]
    put f = case rfKind f of
      Flag  -> "flag"
      Bytes -> "octets"
      _     -> "number"
    get f = put f <> "Field"
    fixedType = maybe (typeName st) (const (headerType st)) tl
    -- the layout's constants and what it leaves out, said once
    leftOut = T.intercalate "; " $
      [ "the layout's reserved bits are zeros" | any ((== Reserved) . fieldKind) (layoutFields l) ]
      <> stConstants st
      <> [ n <> " is the length of " <> tailName t | Just t <- [tl], Just n <- [tailLength t] ]
      <> [ "the " <> T.toLower (tailName t) <> " bytes from byte " <> tshow (layoutSize l)
           <> " to the end are the layout's, written as none and, read other than zero, reported"
         | Just t <- [layoutTail l], isNothing tl ]
    fixedDoc = (doc <> ":") : map ("--   " <>) (Gen.wrapText 96 (Gen.ascii (
                 (if isJust tl then "the fixed part's fields" else "the fields") <> " that carry a value"
                 <> (if T.null leftOut then "." else "; " <> leftOut <> "."))))
    record name fs =
      [ "data " <> name <> " = " <> name ]
      <> fieldLines [ (rfIdent f, "!" <> rfType f, Just (rfWhere f <> printedNote f)) | f <- fs ]
      <> [ "  deriving (Eq, Show)" ]
    printedNote f = if rfPrinted f /= rfName f then ": " <> Gen.ascii (rfPrinted f) else ""
    fieldLines = \case
      [] -> []
      fs -> let w = foldl' max 0 [ T.length n | (n, _, _) <- fs ]
                tw = foldl' max 0 [ T.length t | (_, t, _) <- fs ]
            in [ (if i == 0 then "  { " else "  , ") <> T.justifyLeft w ' ' n <> " :: " <> maybe t (\c -> T.justifyLeft tw ' ' t <> "  -- ^ " <> c) note
               | (i, (n, t, note)) <- zip [0 :: Int ..] fs ] <> ["  }"]
    whole = case tl of
      Nothing -> []
      Just t ->
        [ ""
        , doc <> ": the whole, its fixed part and its " <> Gen.ascii (tailName t)
        , "data " <> typeName st <> " = " <> typeName st ]
        <> fieldLines [ (headerField st, "!" <> headerType st, Nothing)
                      , (fieldIdent st (tailName t), "!ByteString", Just (tailNote t)) ]
        <> [ "  deriving (Eq, Show)" ]
    tailNote t = "from byte " <> tshow (layoutSize l) <> ", " <> maybe "to the end" (<> " bytes") (tailLength t)
                 <> (if tailAlign t > 1 then ", padded to a multiple of " <> tshow (tailAlign t) else "")
    -- the instance: the whole's fields through the header's
    viaHeader f = if isJust tl then "(" <> rfIdent f <> " (" <> headerField st <> " r))" else "(" <> rfIdent f <> " r)"
    values = [ "(" <> tshow (rfName f) <> ", " <> put f <> " " <> viaHeader f <> ")" | f <- fields ]
             <> [ "(" <> tshow (tailName t) <> ", octets (" <> fieldIdent st (tailName t) <> " r))" | Just t <- [tl] ]
    applied fs = [ "<*> " <> get f <> " " <> tshow (rfName f) <> " vs" | f <- fs ]
    fromLines = case tl of
      Nothing -> map ("    " <>) (applied fields)
      Just t  -> case applied fields of
        []       -> ["    <*> pure " <> headerType st, "    <*> octetsField " <> tshow (tailName t) <> " vs"]
        a : rest -> ["    <*> (pure " <> headerType st, "           " <> a] <> map ("           " <>) rest
                    <> ["        )", "    <*> octetsField " <> tshow (tailName t) <> " vs"]
    used = not (null values)
    inst =
      [ ""
      , "instance LayoutRecord " <> typeName st <> " where"
      , "  recordLayout _ = " <> layoutValue st ]
      <> (if used then "  recordFields r =" : Gen.listLines "    " values else ["  recordFields _ = []"])
      <> [ "  fromFields " <> (if used then "vs" else "_") <> " = pure " <> typeName st ]
      <> fromLines
    body = fixedDoc <> record fixedType fields <> whole <> inst


-- * Checks

-- | @C-GRID-NAME@: a field that carries a value without a name that makes an
--   identifier; two fields of the structure named alike; a name of its
--   declarations that the declarations of a structure before it in its module
--   define first (the earlier names given); a name its module imports
--   unqualified (the names in scope given, 'declScope').
nameFindings :: Context -> [Text] -> [Text] -> Structure -> [Finding]
nameFindings ctx earlier inScope st = unnamed <> twice <> clashing <> imported
  where
    bt = stBound st
    bad cells = (if null cells then atTable ctx bt else atCells ctx bt cells) . finding Error "C-GRID-NAME"
    fields = recordFieldsOf st
    names = L.nub (recordNames st)
    -- a field's name, after the structure's prefix, needs a letter or a digit; the structure's, a capital first
    unnamed = [ bad (rfCells f) (rfWhere f <> ": the field " <> tshow (rfPrinted f) <> " has no name that makes an identifier")
              | f <- fields, noIdent (rfName f) ]
              <> [ bad [] ("the tail " <> tshow (tailName t) <> " has no name that makes an identifier")
                 | Just t <- [recordTail (stLayout st)], noIdent (tailName t) ]
              <> [ bad [] ("the name " <> tshow (stName st) <> " makes no type name") | not (typeIdent (typeName st)) ]
    twice = [ bad (concatMap rfCells fs) ("the fields " <> T.intercalate " and " (map (\f -> tshow (rfPrinted f) <> " (" <> rfWhere f <> ")") fs)
                                          <> " are named alike: " <> n)
            | (n, fs@(_ : _ : _)) <- Map.toList (Map.fromListWith (flip (<>)) [ (rfIdent f, [f]) | f <- fields ]) ]
    clashing = [ bad [] (n <> " is also a name of an earlier grid's declarations") | n <- names, n `elem` earlier ]
    imported = [ bad [] (n <> " is also a name its module imports") | n <- names, n `elem` inScope ]
    noIdent n = not (T.any C.isAlphaNum (titleIdent (identText n)))
    typeIdent t = maybe False (\(c, cs) -> C.isUpper c && T.all (\x -> C.isAlphaNum x || x == '_') cs) (T.uncons t)

-- | The names the declarations of the structures before one in a list define
--   (each name of a module is defined once: the @byteFieldValue@ rule lists
--   the @byteGrid@ rule's structures, then its own).
earlierNames :: [Structure] -> Structure -> [Text]
earlierNames sts st = concatMap recordNames (takeWhile ((/= btAnchor (stBound st)) . btAnchor . stBound) sts)

-- | The names declarations bring into their module's scope unqualified: each
--   name an import list gives, and the members of a type or class it imports
--   with them (@Layout (..)@: @Layout@, @layoutName@, …), as the runtime
--   shows them.
declScope :: [Gen.Decl] -> [Text]
declScope ds = L.nub (concat [ itemNames x | d <- ds, Gen.ImportSome _ xs <- d.declImports, x <- xs ])
  where
    itemNames x = case T.stripSuffix " (..)" x of
      Just t  -> t : Map.findWithDefault [] t members
      Nothing -> [T.filter (`notElem` ("()" :: String)) x]
    members = Map.fromList
      [ ("FieldKind", map constructor [Unsigned, Flag, Bytes, Reserved, Pinned 0])
      , ("Layout", recordSyntax (Layout "" 0 [] Nothing))
      , ("Tail", recordSyntax (Tail "" Nothing 1 Bytes))
      , ("FeatureHeader", recordSyntax (FeatureHeader 0 ""))
      , ("LayoutRecord", ["recordLayout", "recordFields", "fromFields"])
      , ("Feature", ["featureCode", "featureVersion", "featureLength", "featureHeader"])
      , ("Maybe", ["Just", "Nothing"])
      , ("Either", ["Left", "Right"]) ]
    constructor :: Show a => a -> Text
    constructor = T.takeWhile (/= ' ') . tshow
    -- a record's constructor and fields, from its derived Show: @Layout {layoutName = …, …}@
    recordSyntax :: Show a => a -> [Text]
    recordSyntax v = constructor v : [ T.takeWhileEnd C.isAlphaNum before | (before, _) <- DT.breakOnAll " = " (tshow v) ]


-- * Codes

-- | The structures' naming code, whichever rule prints them.
recordCodes :: [CodeDef]
recordCodes =
  [ CodeDef "C-GRID-NAME" [Error] (Defect "a byte grid's field that carries a value has no name that makes an identifier, or two of its fields, or two grids, are named alike, or a name its module imports") ]
