{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : Hsed.Spec.Shape
Description : The asserted shape of a binding and the measured form of a table
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A 'Shape' is what a binding asserts about its anchor; a 'Form' is what the
extractor measured (docs/spec-as-source.md section 4.2). 'accepts' is the
@E-SHAPE@ check of section 3.3: the shape must accept the form, an Error for a
rule default and a Warning under @as@. Header cells are compared
whitespace-stripped and lower-cased after the typo table of section 3.4, and the
corner cell of a byte grid is normalised before the @grid8@ test. Neither
normalisation is an erratum: both apply under either errata set, and hsedc
reports each one it makes as a Note ('headerNormalisations', 'normaliseCaption').

'SShape' is the singleton of 'Shape', so a shape can be reified from a type
('KnownShape') and a type recovered from a value ('toSomeShape').
-}
module Hsed.Spec.Shape
  ( -- * Shapes
    Shape (..)
  , shapeName
  , readShape
  , SShape (..)
  , KnownShape (..)
  , sShape
  , SomeShape (..)
  , toSomeShape
    -- * Forms
  , Form (..)
  , CaptionKind (..)
  , measure
  , normHeader
  , headerTypos
  , headerNormalisations
  , captionTitle
  , rawCaptionTitle
  , normaliseCaption
  , classifyCaption
  , isGrid8
    -- * The E-SHAPE check
  , accepts
  , acceptsAs
  , acceptsWith
  , shapeMismatch
  , projectColumns
  ) where

import qualified Data.Text          as DT
import           Data.Type.Equality (TestEquality (..), (:~:) (..))
import qualified RIO.Char           as C
import qualified RIO.Text           as T
import           RIO

import           Hsed.Spec.Common   (Finding (..), Severity (..), SpecId (..))
import           Hsed.Spec.IR       (CellIR (..), HeaderSource (..), TableIR (..))


-- * Shapes

-- | Every shape of design section 4.2. The table shapes name what a rule or a
--   comment asserts a table to be; 'Doc', 'Heading', 'Prose' and 'Sig' accept
--   any form.
data Shape
  = TypeRow | EnumValues | TableDesc | ColumnsAddition | TableUIDs | UidTable
  | StatusCodes | Codes | FieldValue | FieldType | ByteFieldValue | ByteGrid
  | Feature | ConstValues | Ranges | Preconfig | Tokens | StateTable | Props
  | PropsReq | Legend | Decision | Prose | Doc | Heading | Sig
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The constructor's name, as @as \@'Codes@ spells it.
shapeName :: Shape -> Text
shapeName = tshow

readShape :: Text -> Maybe Shape
readShape t = lookup t [ (shapeName s, s) | s <- [minBound .. maxBound] ]

-- | The singleton of 'Shape'.
data SShape (s :: Shape) where
  STypeRow         :: SShape 'TypeRow
  SEnumValues      :: SShape 'EnumValues
  STableDesc       :: SShape 'TableDesc
  SColumnsAddition :: SShape 'ColumnsAddition
  STableUIDs       :: SShape 'TableUIDs
  SUidTable        :: SShape 'UidTable
  SStatusCodes     :: SShape 'StatusCodes
  SCodes           :: SShape 'Codes
  SFieldValue      :: SShape 'FieldValue
  SFieldType       :: SShape 'FieldType
  SByteFieldValue  :: SShape 'ByteFieldValue
  SByteGrid        :: SShape 'ByteGrid
  SFeature         :: SShape 'Feature
  SConstValues     :: SShape 'ConstValues
  SRanges          :: SShape 'Ranges
  SPreconfig       :: SShape 'Preconfig
  STokens          :: SShape 'Tokens
  SStateTable      :: SShape 'StateTable
  SProps           :: SShape 'Props
  SPropsReq        :: SShape 'PropsReq
  SLegend          :: SShape 'Legend
  SDecision        :: SShape 'Decision
  SProse           :: SShape 'Prose
  SDoc             :: SShape 'Doc
  SHeading         :: SShape 'Heading
  SSig             :: SShape 'Sig

deriving stock instance Show (SShape s)
deriving stock instance Eq (SShape s)

-- | A shape known at the type level.
class KnownShape (s :: Shape) where
  shapeSing :: SShape s

instance KnownShape 'TypeRow         where shapeSing = STypeRow
instance KnownShape 'EnumValues      where shapeSing = SEnumValues
instance KnownShape 'TableDesc       where shapeSing = STableDesc
instance KnownShape 'ColumnsAddition where shapeSing = SColumnsAddition
instance KnownShape 'TableUIDs       where shapeSing = STableUIDs
instance KnownShape 'UidTable        where shapeSing = SUidTable
instance KnownShape 'StatusCodes     where shapeSing = SStatusCodes
instance KnownShape 'Codes           where shapeSing = SCodes
instance KnownShape 'FieldValue      where shapeSing = SFieldValue
instance KnownShape 'FieldType       where shapeSing = SFieldType
instance KnownShape 'ByteFieldValue  where shapeSing = SByteFieldValue
instance KnownShape 'ByteGrid        where shapeSing = SByteGrid
instance KnownShape 'Feature         where shapeSing = SFeature
instance KnownShape 'ConstValues     where shapeSing = SConstValues
instance KnownShape 'Ranges          where shapeSing = SRanges
instance KnownShape 'Preconfig       where shapeSing = SPreconfig
instance KnownShape 'Tokens          where shapeSing = STokens
instance KnownShape 'StateTable      where shapeSing = SStateTable
instance KnownShape 'Props           where shapeSing = SProps
instance KnownShape 'PropsReq        where shapeSing = SPropsReq
instance KnownShape 'Legend          where shapeSing = SLegend
instance KnownShape 'Decision        where shapeSing = SDecision
instance KnownShape 'Prose           where shapeSing = SProse
instance KnownShape 'Doc             where shapeSing = SDoc
instance KnownShape 'Heading         where shapeSing = SHeading
instance KnownShape 'Sig             where shapeSing = SSig

-- | Demote a singleton.
sShape :: SShape s -> Shape
sShape = \case
  STypeRow         -> TypeRow
  SEnumValues      -> EnumValues
  STableDesc       -> TableDesc
  SColumnsAddition -> ColumnsAddition
  STableUIDs       -> TableUIDs
  SUidTable        -> UidTable
  SStatusCodes     -> StatusCodes
  SCodes           -> Codes
  SFieldValue      -> FieldValue
  SFieldType       -> FieldType
  SByteFieldValue  -> ByteFieldValue
  SByteGrid        -> ByteGrid
  SFeature         -> Feature
  SConstValues     -> ConstValues
  SRanges          -> Ranges
  SPreconfig       -> Preconfig
  STokens          -> Tokens
  SStateTable      -> StateTable
  SProps           -> Props
  SPropsReq        -> PropsReq
  SLegend          -> Legend
  SDecision        -> Decision
  SProse           -> Prose
  SDoc             -> Doc
  SHeading         -> Heading
  SSig             -> Sig

-- | A singleton whose shape is known only at run time, with its 'KnownShape' dictionary.
data SomeShape = forall s. KnownShape s => SomeShape (SShape s)

-- | Promote a shape value.
toSomeShape :: Shape -> SomeShape
toSomeShape = \case
  TypeRow         -> SomeShape STypeRow
  EnumValues      -> SomeShape SEnumValues
  TableDesc       -> SomeShape STableDesc
  ColumnsAddition -> SomeShape SColumnsAddition
  TableUIDs       -> SomeShape STableUIDs
  UidTable        -> SomeShape SUidTable
  StatusCodes     -> SomeShape SStatusCodes
  Codes           -> SomeShape SCodes
  FieldValue      -> SomeShape SFieldValue
  FieldType       -> SomeShape SFieldType
  ByteFieldValue  -> SomeShape SByteFieldValue
  ByteGrid        -> SomeShape SByteGrid
  Feature         -> SomeShape SFeature
  ConstValues     -> SomeShape SConstValues
  Ranges          -> SomeShape SRanges
  Preconfig       -> SomeShape SPreconfig
  Tokens          -> SomeShape STokens
  StateTable      -> SomeShape SStateTable
  Props           -> SomeShape SProps
  PropsReq        -> SomeShape SPropsReq
  Legend          -> SomeShape SLegend
  Decision        -> SomeShape SDecision
  Prose           -> SomeShape SProse
  Doc             -> SomeShape SDoc
  Heading         -> SomeShape SHeading
  Sig             -> SomeShape SSig

instance TestEquality SShape where
  testEquality a b = case (a, b) of
    (STypeRow, STypeRow)                 -> Just Refl
    (SEnumValues, SEnumValues)           -> Just Refl
    (STableDesc, STableDesc)             -> Just Refl
    (SColumnsAddition, SColumnsAddition) -> Just Refl
    (STableUIDs, STableUIDs)             -> Just Refl
    (SUidTable, SUidTable)               -> Just Refl
    (SStatusCodes, SStatusCodes)         -> Just Refl
    (SCodes, SCodes)                     -> Just Refl
    (SFieldValue, SFieldValue)           -> Just Refl
    (SFieldType, SFieldType)             -> Just Refl
    (SByteFieldValue, SByteFieldValue)   -> Just Refl
    (SByteGrid, SByteGrid)               -> Just Refl
    (SFeature, SFeature)                 -> Just Refl
    (SConstValues, SConstValues)         -> Just Refl
    (SRanges, SRanges)                   -> Just Refl
    (SPreconfig, SPreconfig)             -> Just Refl
    (STokens, STokens)                   -> Just Refl
    (SStateTable, SStateTable)           -> Just Refl
    (SProps, SProps)                     -> Just Refl
    (SPropsReq, SPropsReq)               -> Just Refl
    (SLegend, SLegend)                   -> Just Refl
    (SDecision, SDecision)               -> Just Refl
    (SProse, SProse)                     -> Just Refl
    (SDoc, SDoc)                         -> Just Refl
    (SHeading, SHeading)                 -> Just Refl
    (SSig, SSig)                         -> Just Refl
    _                                    -> Nothing


-- * Forms

-- | What the caption says a table is, number stripped (design section 3.4).
data CaptionKind
  = CaptionNone                 -- ^ no caption: a nested or unnumbered table
  | CaptionIdent Text           -- ^ @Table 46 AC_element@: a single identifier, the type's name
  | CaptionEnumValues Text      -- ^ @Table 52 adv_key_mode Enumeration Values@
  | CaptionTableDesc Text       -- ^ @Table 171 Table Table Description@
  | CaptionColumns Text         -- ^ @Table 26 - Admin SP – TPerInfo Columns@
  | CaptionFeature              -- ^ mentions @Feature Descriptor@
  | CaptionPreconfig            -- ^ @… Table Preconfiguration@ (Opal) or @Default … Authorities@ (Core)
  | CaptionTerminology          -- ^ ends @Terminology@ or @Example@
  | CaptionOther Text           -- ^ anything else, the title kept
  deriving stock (Eq, Show)

-- | The measured form of one table, total over every table the extractor writes.
--   The location fields let 'accepts' cite the page the way 'Hsed.Spec.Common.renderFinding' does.
data Form = Form
  { fSpec         :: !SpecId
  , fId           :: !(Maybe Text)      -- ^ anchor id of a numbered table
  , fPage         :: !(Maybe Int)       -- ^ 1-based PDF page of the table's first fragment
  , fCols         :: !Int
  , fHeaderSource :: !HeaderSource
  , fHeaderNorm   :: ![Text]            -- ^ the first header row, normalised
  , fGrid8        :: !Bool              -- ^ a byte grid: normalised corner then @7 … 0@
  , fCaptionKind  :: !CaptionKind
  , fCaption      :: !(Maybe Text)      -- ^ the caption's title, number stripped
  , fRagged       :: !Bool              -- ^ some data row has not 'fCols' cells
  }
  deriving stock (Eq, Show)

-- | Measure a table. Pages in the IR are 0-based; the form's page is 1-based.
measure :: SpecId -> TableIR -> Form
measure sid t = Form
  { fSpec         = sid
  , fId           = t.id
  , fPage         = (+ 1) <$> listToMaybe t.pages
  , fCols         = t.nCols
  , fHeaderSource = t.headerSource
  , fHeaderNorm   = map normHeader header
  , fGrid8        = isGrid8 t.nCols firstRow
  , fCaptionKind  = maybe CaptionNone classifyCaption title
  , fCaption      = title
  , fRagged       = any ((/= t.nCols) . length) t.rows
  }
  where
    header   = maybe [] (map (.text)) (listToMaybe t.headerRows)
    firstRow = maybe [] (map (.text)) (listToMaybe (t.headerRows <> t.rows))
    title    = t.caption >>= captionTitle

-- | The typo table of design section 3.4, applied before whitespace is stripped.
--   The design's other two entries, @M axSize@ and @MandatoryWrite  Granularity@,
--   differ from the right spelling only in whitespace, which 'normHeader' strips
--   anyway; @Colum Type@ (Opal T26) is the one a typo table is needed for.
headerTypos :: [(Text, Text)]
headerTypos =
  [ ("Colum Type", "Column Type")
  ]

-- | Header normalisation: the typo table, then every whitespace character
--   removed, then lower case: @Enumeration Value@ becomes @enumerationvalue@.
normHeader :: Text -> Text
normHeader = T.toLower . T.filter (not . C.isSpace) . untypo
  where untypo t = foldl' (\acc (bad, good) -> DT.replace bad good acc) t headerTypos

-- | The typos of the table a table's header cells contain, each as printed and
--   as read: what hsedc reports as @N-HEADER-NORMALISED@.
headerNormalisations :: TableIR -> [(Text, Text)]
headerNormalisations t =
  [ (bad, good) | row <- t.headerRows, cell <- row, (bad, good) <- headerTypos, bad `T.isInfixOf` cell.text ]

-- | The title of a caption: @Table 46 AC_element@ gives @AC_element@, and Opal's
--   @Table 26 - Admin SP – TPerInfo Columns@ gives @Admin SP – TPerInfo Columns@,
--   normalised ('normaliseCaption'). Nothing for a caption that does not start
--   with @Table N@.
captionTitle :: Text -> Maybe Text
captionTitle = fmap normaliseCaption . rawCaptionTitle

-- | 'captionTitle' before 'normaliseCaption', the title as printed.
rawCaptionTitle :: Text -> Maybe Text
rawCaptionTitle caption = do
  rest <- T.stripPrefix "table" (T.toLower (T.take 5 caption)) >> pure (T.drop 5 caption)
  let (number, afterNumber) = T.span C.isDigit (T.stripStart rest)
  guard (not (T.null number))
  pure (T.strip (T.dropWhile (`elem` ("-–— " :: String)) (T.stripStart afterNumber)))

-- | The seven Core captions printed @\<X\>_object _ref@ read as @\<X\>_object_ref@
--   (design example E3: normalised and reported, not an erratum; hsedc reports
--   each as @N-CAPTION-NORMALISED@).
normaliseCaption :: Text -> Text
normaliseCaption = DT.replace "_object _ref" "_object_ref"

-- | Classify a caption's title (design section 3.4).
classifyCaption :: Text -> CaptionKind
classifyCaption title
  | "feature descriptor" `T.isInfixOf` lower        = CaptionFeature
  | "table preconfiguration" `T.isSuffixOf` lower    = CaptionPreconfig
  | "default " `T.isPrefixOf` lower
    && "authorities" `T.isSuffixOf` lower            = CaptionPreconfig
  | Just s <- strip " Enumeration Values"            = CaptionEnumValues s
  | Just s <- strip " Table Description"             = CaptionTableDesc s
  | Just s <- strip " Columns"                       = CaptionColumns s
  | "terminology" `T.isSuffixOf` lower
    || "example" `T.isSuffixOf` lower                = CaptionTerminology
  | Just s <- strip " Type Table Addition"
  , isIdent s                                        = CaptionIdent s
  | isIdent title                                    = CaptionIdent title
  | otherwise                                        = CaptionOther title
  where
    lower = T.toLower title
    strip suffix = T.strip <$> T.stripSuffix suffix title
    isIdent s = case T.uncons s of
      Just (c, cs) -> (C.isAlpha c || c == '_') && T.all (\x -> C.isAlphaNum x || x == '_') cs
      Nothing      -> False

-- | Is the first row a byte-grid header: a normalised corner cell (@Bit@,
--   @Bit Byte@, @Byte\\Bit@) followed by exactly @7 … 0@, nine cells wide?
isGrid8 :: Int -> [Text] -> Bool
isGrid8 nCols row = case map normHeader row of
  corner : bits -> nCols == 9 && corner `elem` corners && bits == map tshow [7, 6 .. 0 :: Int]
  []            -> False
  where corners = ["bit", "bitbyte", "byte\\bit", "bytebit"]


-- * The E-SHAPE check

-- | The @E-SHAPE@ Error of a rule default: Nothing when the shape accepts the form.
accepts :: Shape -> Form -> Maybe Finding
accepts = acceptsWith Error

-- | The same check demoted to a Warning, for a binding under @as@.
acceptsAs :: Shape -> Form -> Maybe Finding
acceptsAs = acceptsWith Warning

acceptsWith :: Severity -> Shape -> Form -> Maybe Finding
acceptsWith sev shape form = located <$> shapeMismatch shape form
  where
    located reason = Finding sev "E-SHAPE" (Just (fSpec form)) (fId form) (fPage form)
      (shapeName shape <> " does not accept this table: " <> reason <> "; measured " <> describe form)

describe :: Form -> Text
describe form = T.concat
  [ tshow (fCols form), " columns, header ", tshow (fHeaderNorm form)
  , ", caption ", tshow (fCaptionKind form)
  , if fGrid8 form then ", byte grid" else ""
  , if fRagged form then ", ragged" else "" ]

-- | Why a shape rejects a form, or Nothing when it accepts it. Structural only:
--   a rule's caption and row conditions belong to the rule's @fires@.
shapeMismatch :: Shape -> Form -> Maybe Text
shapeMismatch shape form = case shape of
  TypeRow         -> header ["uid", "name", "format"]
  EnumValues      -> columns 2 <|> firstIn ["enumerationvalue", "setvalue", "columnvalue"]
  TableDesc       -> header tableDescHeader
  ColumnsAddition -> header tableDescHeader
  TableUIDs       -> header ["uidoftabledescriptorobject", "uidoftable", "tablename", "template"]
  UidTable        -> atLeast 2 <|> firstSuch "a first header cell naming a UID" ("uid" `T.isInfixOf`)
  StatusCodes     -> header ["name", "value"]
  Codes           -> (columns 2 >> columns 3) <|> firstSuch "a code-like first header cell" codeLike
  FieldValue      -> header ["field", "value"]
  FieldType       -> header ["field", "type"] >> header ["value", "type", "range"]
  ByteFieldValue  -> columns 3 <|> cells "[Byte(s), Field, Value(s)]" byteFieldValue
  ByteGrid        -> grid
  Feature         -> grid
  ConstValues     -> header ["column", "value"]
  Ranges          -> columns 3 <|> cells "[<X> Name, … Start, … End]" ranges
  Preconfig       -> atLeast 2 <|> caption
  Tokens          -> atLeast 4 <|> prefix ["byte", "hex", "acronym", "meaning"]
  StateTable      -> atLeast 2
  Props           -> atLeast 3 <|> firstSuch "a first header cell starting with Property" ("property" `T.isPrefixOf`)
  PropsReq        -> columns 3 <|> prefix ["propertyname"]
  Legend          -> atLeast 2 <|> firstSuch "a first header cell mentioning the legend" ("legend" `T.isInfixOf`)
  Decision        -> atLeast 2 <|> lastIs "requiredbehavior"
  Prose           -> Nothing
  Doc             -> Nothing
  Heading         -> Nothing
  Sig             -> Nothing
  where
    hs = fHeaderNorm form
    tableDescHeader = ["columnnumber", "columnname", "isunique", "columntype"]
    columns n
      | fCols form == n = Nothing
      | otherwise = Just ("expects " <> tshow n <> " columns")
    atLeast n
      | fCols form >= n = Nothing
      | otherwise = Just ("expects at least " <> tshow n <> " columns")
    header expected
      | hs == expected = Nothing
      | otherwise = Just ("expects the header " <> tshow expected)
    prefix expected
      | take (length expected) hs == expected = Nothing
      | otherwise = Just ("expects a header starting " <> tshow expected)
    firstIn options = firstSuch ("a first header cell among " <> tshow options) (`elem` options)
    firstSuch what p = case hs of
      h : _ | p h -> Nothing
      _           -> Just ("expects " <> what)
    lastIs cell = case reverse hs of
      h : _ | h == cell -> Nothing
      _                 -> Just ("expects the last header cell " <> tshow cell)
    cells what p
      | p hs = Nothing
      | otherwise = Just ("expects the header " <> what)
    byteFieldValue = \case
      [b, f, v] -> b `elem` ["byte", "bytes"] && f == "field" && "value" `T.isPrefixOf` v
      _         -> False
    ranges = \case
      [n, s, e] -> "name" `T.isSuffixOf` n && "start" `T.isSuffixOf` s && "end" `T.isSuffixOf` e
      _         -> False
    grid
      | fGrid8 form = Nothing
      | otherwise = Just "expects a byte grid (corner cell then 7 … 0)"
    caption = case fCaptionKind form of
      CaptionPreconfig -> Nothing
      _                -> Just "expects a caption ending Table Preconfiguration or Default … Authorities"
    codeLike h = or
      [ "code" `T.isSuffixOf` h, h == "id", "comid" `T.isInfixOf` h, "type" `T.isSuffixOf` h
      , "value" `T.isSuffixOf` h, h == "bit", "versionnumber" `T.isSuffixOf` h ]

-- | The form of a wider table seen through @cols a b@: two columns, their
--   header cells, the rest unchanged. Left names the column that does not exist.
projectColumns :: Int -> Int -> Form -> Either Text Form
projectColumns a b form = do
  ha <- cell a
  hb <- cell b
  pure form { fCols = 2, fHeaderNorm = [ha, hb], fGrid8 = False }
  where
    cell i
      | i < 0 || i >= fCols form = Left ("cols " <> tshow a <> " " <> tshow b <> ": no column " <> tshow i <> " in " <> tshow (fCols form))
      | otherwise = Right (fromMaybe "" (listToMaybe (drop i (fHeaderNorm form))))
