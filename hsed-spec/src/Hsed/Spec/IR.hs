{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Hsed.Spec.IR
Description : IR v1, the seam between the PDF extractor and hsedc
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Haskell types for the IR v1 JSON that @tools/extract/extract_tables.py@ writes
and @hsedc@ reads: @docs/ir-v1.md@, design @docs/spec-as-source.md@ sections
4.2 and 5.1. Field names follow the JSON: camelCase here, snake_case in the file
where the schema says so ('snakeOptions'). Extra JSON fields are ignored, optional
fields are 'Maybe', and a cell's repair flags default to 'False' when absent;
@null@ for @bold@ and @underline@ (an empty cell) reads as 'False' too. The
signature blocks, the List of Tables and the table references ('SigIR',
'LotIR', 'XrefIR', since M2b) read as empty lists from a file written before
them: additive fields, so the version stays 1; so are, since M2c, a byte
grid's cells' drawn extents ('xExtent') and the words of the paragraphs a
reader consumes ('ParaIR' @words@ in the 'wordComplete' sections). The shapes
follow the committed @spec/ir/*.json@: a repeated header row is an object with its
page and row index, a nested table has no ordinal, an empty cell has no page or
bbox, and structure paths may be written as int arrays. Fields are accessed with
@OverloadedRecordDot@ (@ir.tables@, @cell.text@).
-}
module Hsed.Spec.IR
  ( -- * Document
    SpecIR (..)
  , ExtractorIR (..)
  , irVersion1
    -- * Tables
  , TableIR (..)
  , RepeatedHeaderIR (..)
  , CellIR (..)
  , cellWithText
  , FragmentIR (..)
  , AdoptedFromIR (..)
  , MovedOutIR (..)
  , HeaderSource (..)
  , CellKind (..)
    -- * Headings and paragraphs
  , HeadingIR (..)
  , ParaIR (..)
  , ParaKind (..)
    -- * Signature blocks, the List of Tables, table references
  , SigIR (..)
  , LotIR (..)
  , XrefIR (..)
    -- * Annotations
  , AnnotIR (..)
  , AnnotKind (..)
    -- * Geometry and structure paths
  , Rect (..)
  , PageRect (..)
  , RGB (..)
  , StructPath (..)
    -- * Reading
  , readIR
  ) where

import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Text        as T
import           RIO


-- | The IR version this module describes; 'readIR' accepts no other.
irVersion1 :: Int
irVersion1 = 1

-- | One extracted specification: the top level of the JSON file.
data SpecIR = SpecIR
  { irVersion    :: !Int
  , spec         :: !Text          -- ^ SpecId: @core-2.01@, @opal-2.30@, ...
  , source       :: !Text          -- ^ the PDF's file name
  , pdfMd5       :: !Text          -- ^ md5 of the pristine bytes
  , pages        :: !Int
  , labelOffset  :: !Int           -- ^ 1-based PDF page = printed page + labelOffset
  , notice       :: !Text          -- ^ path of the TCG licence notice
  , extractor    :: !ExtractorIR
  , wordComplete :: ![Text]        -- ^ sections whose paras carry their words
  , tables       :: ![TableIR]     -- ^ document order, nested tables included
  , headings     :: ![HeadingIR]   -- ^ document order
  , paras        :: ![ParaIR]      -- ^ document order
  , sigs         :: ![SigIR]       -- ^ method signature blocks, document order
  , lot          :: ![LotIR]       -- ^ the List of Tables entries of the front matter, in order
  , xrefs        :: ![XrefIR]      -- ^ every @Table N@ in the prose, document order
  , annots       :: ![AnnotIR]     -- ^ hsed annotations found in the input PDF
  } deriving (Eq, Show, Generic)

-- | Which program wrote the file.
data ExtractorIR = ExtractorIR
  { script  :: !Text
  , version :: !Int
  , pymupdf :: !Text
  , pikepdf :: !Text
  } deriving (Eq, Show, Generic)

-- | One table, top-level or nested.
data TableIR = TableIR
  { id                 :: !(Maybe Text)       -- ^ anchor id @T\<n\>@ for a numbered table
  , tableNumber        :: !(Maybe Int)
  , caption            :: !(Maybe Text)
  , captionPath        :: !(Maybe StructPath)
  , structPath         :: !StructPath
  , nestedIn           :: !(Maybe StructPath)  -- ^ the enclosing table's structure path
  , part               :: !(Maybe Int)         -- ^ for a table Word split in two
  , parts              :: !(Maybe Int)
  , pages              :: ![Int]               -- ^ 0-based PDF page indices
  , printedPages       :: ![Int]
  , nRows              :: !Int
  , nCols              :: !Int
  , ordinal            :: !(Maybe Int)         -- ^ 1-based among top-level tables; a nested table has none
  , rects              :: ![PageRect]          -- ^ one per page the table touches
  , headerSource       :: !HeaderSource
  , headerRows         :: ![[CellIR]]
  , repeatedHeaderRows :: ![RepeatedHeaderIR]
  , rows               :: ![[CellIR]]
  , warnings           :: ![Text]
  } deriving (Eq, Show, Generic)

-- | Where a table's header rows came from.
data HeaderSource = LeadingTH | FirstOfAllTH | Repeat | BoldFirstRow | NoHeader
  deriving (Eq, Show, Bounded, Enum, Generic)

-- | A header row repeated where a table continues on a later page.
data RepeatedHeaderIR = RepeatedHeaderIR
  { cells    :: ![CellIR]
  , page     :: !Int
  , rowIndex :: !Int        -- ^ the row's index among the table's rows
  } deriving (Eq, Show, Generic)

-- | One table cell with its text, geometry, styling and repair flags.
data CellIR = CellIR
  { kind                :: !CellKind
  , text                :: !Text
  , lines               :: ![Text]
  , page                :: !(Maybe Int)        -- ^ absent for an empty cell
  , bbox                :: !(Maybe Rect)       -- ^ absent for an empty cell, which carries 'estBbox'
  , estBbox             :: !(Maybe Rect)
  , bold                :: !Bool
  , underline           :: !Bool
  , fonts               :: ![Text]
  , fillRgb             :: !(Maybe RGB)
  , fillBbox            :: !(Maybe Rect)
  , nestedTables        :: ![StructPath]       -- ^ the tables nested in this cell
  , fragments           :: ![FragmentIR]       -- ^ the pieces of a cell that spans pages
  , splitFromMerged     :: !Bool
  , recoveredByGeometry :: !Bool
  , raggedJoin          :: !Bool
  , adoptedOrphan       :: !(Maybe Text)       -- ^ the orphan text adopted into this cell
  , adoptedFrom         :: !(Maybe AdoptedFromIR)
  , movedOut            :: !(Maybe MovedOutIR)
  , xExtent             :: !(Maybe (Double, Double))
    -- ^ in a byte grid, @[x0, x1]@ of the drawn cell it occupies (PDF space): the
    --   grid reader's extent rule maps it onto the header's bit columns
  } deriving (Eq, Show, Generic)

-- | A cell with its text replaced (an erratum's correction): @text@ is a field of
--   several records here, so a record update would be ambiguous.
cellWithText :: Text -> CellIR -> CellIR
cellWithText t (CellIR k _ ls p bb eb b u fs fr fb nt fg sm rg rj ao af mo xe) =
  CellIR k t ls p bb eb b u fs fr fb nt fg sm rg rj ao af mo xe

-- | One piece of a cell that continues on another page.
data FragmentIR = FragmentIR
  { bbox :: !Rect
  , page :: !Int
  } deriving (Eq, Show, Generic)

-- | Where an adopted cell's text came from.
data AdoptedFromIR = AdoptedFromIR
  { structPath :: !StructPath
  , text       :: !Text
  } deriving (Eq, Show, Generic)

-- | Where a cell's text was moved to.
data MovedOutIR = MovedOutIR
  { toStructPath :: !StructPath
  , text         :: !Text
  } deriving (Eq, Show, Generic)

-- | Header or data cell.
data CellKind = TH | TD
  deriving (Eq, Show, Bounded, Enum, Generic)

-- | One @/H1@..@/H6@ structure element.
data HeadingIR = HeadingIR
  { section    :: !(Maybe Text)   -- ^ leading dotted number, absent for an unnumbered heading
  , title      :: !Text
  , level      :: !Int
  , page       :: !Int
  , structPath :: !StructPath
  , rect       :: !Rect
  } deriving (Eq, Show, Generic)

-- | One @/P@ or @/LI@ outside the tables.
data ParaIR = ParaIR
  { section    :: !(Maybe Text)     -- ^ the nearest preceding heading's section
  , item       :: !(Maybe Text)     -- ^ the list label of an @/LI@
  , kind       :: !ParaKind
  , page       :: !Int
  , structPath :: !StructPath
  , rect       :: !Rect
  , hash       :: !Text             -- ^ sha256 of the normalised words
  , words      :: !(Maybe [Text])   -- ^ only for sections in 'wordComplete'
  } deriving (Eq, Show, Generic)

-- | Paragraph or list item.
data ParaKind = P | LI
  deriving (Eq, Show, Bounded, Enum, Generic)

-- | One method signature block (design section 5.1 step 7): consecutive @/P@
--   paragraphs, one printed line each, from the invocation head(s) through the
--   parameters, @=>@ and the result (Core's CloseSession has no @=>@). One per
--   section; its anchor id is @\<section\>/sig@.
data SigIR = SigIR
  { section    :: !Text         -- ^ the section it lies in
  , page       :: !Int          -- ^ 0-based PDF page of its first line
  , rect       :: !Rect         -- ^ its lines on that page
  , structPath :: !StructPath   -- ^ its first paragraph
  , heads      :: ![Text]       -- ^ @[TableUID.Get, ObjectUID.Get]@: the invocations before the parameters
  , text       :: !Text         -- ^ its lines as printed, one per paragraph, joined by newlines
  } deriving (Eq, Show, Generic)

-- | One entry of the List of Tables: what the front matter says a table is called and where.
data LotIR = LotIR
  { number  :: !Int    -- ^ @Table 01@ is 1
  , title   :: !Text   -- ^ as the entry prints it, without the number, a dash separator, the dot leaders and the page
  , printed :: !Int    -- ^ the printed page the entry gives
  , page    :: !Int    -- ^ 0-based PDF page of the entry
  , rect    :: !Rect
  } deriving (Eq, Show, Generic)

-- | One @Table N@ in a paragraph or list item that is not a table's caption.
data XrefIR = XrefIR
  { number   :: !Int
  , page     :: !Int            -- ^ 0-based PDF page of the reference
  , rect     :: !Rect           -- ^ the glyphs of @Table N@
  , section  :: !(Maybe Text)   -- ^ the paragraph's section
  , sentence :: !Text           -- ^ the sentence it is in
  , target   :: !(Maybe Int)    -- ^ 0-based page the link over the number goes to; Nothing without one
  } deriving (Eq, Show, Generic)

-- | One hsed annotation found in the input PDF (a @/Text@ annotation carrying @/HSED@).
data AnnotIR = AnnotIR
  { kind     :: !AnnotKind
  , spec     :: !Text
  , id       :: !Text          -- ^ anchor id
  , struct   :: !StructPath
  , v        :: !Int           -- ^ annotation format version
  , page     :: !Int
  , rect     :: !Rect
  , flags    :: !Int
  , contents :: !Text          -- ^ the payload exactly as stored
  } deriving (Eq, Show, Generic)

-- | The annotation kinds of the design's section 3.3.
data AnnotKind = Doc | Rule | Bind | Law | Behaviour
  deriving (Eq, Show, Bounded, Enum, Generic)

-- | A rectangle in PDF user space, @[x0, y0, x1, y1]@ in the JSON.
data Rect = Rect { x0 :: !Double, y0 :: !Double, x1 :: !Double, y1 :: !Double }
  deriving (Eq, Show, Generic)

-- | A rectangle on a page, @[page, [x0, y0, x1, y1]]@ in the JSON.
data PageRect = PageRect { page :: !Int, rect :: !Rect }
  deriving (Eq, Show, Generic)

-- | A fill colour, @[r, g, b]@ in the JSON.
data RGB = RGB { r :: !Double, g :: !Double, b :: !Double }
  deriving (Eq, Show, Generic)

-- | A @/K@ index path from @/StructTreeRoot@, written @"0/1288"@ (headings,
-- paras, annots) or as the int array @[0, 1288]@ (tables and cells); both read,
-- the string form is written.
newtype StructPath = StructPath Text
  deriving (Eq, Ord, Show, Generic)


-- * JSON instances

-- | camelCase field names here, snake_case in the file.
snakeOptions :: Options
snakeOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- | A string enumeration rendered by @render@, parsed by looking the text up
-- among every constructor's rendering.
enumParser :: (Bounded a, Enum a) => String -> (a -> Text) -> Value -> Parser a
enumParser name render = withText name $ \t ->
  maybe (fail (name <> ": unknown value " <> show t)) pure
        (lookup t [ (render c, c) | c <- [minBound .. maxBound] ])

-- | Hand-written so that a file written before the signature blocks, the List
--   of Tables and the table references (M2b) reads them as empty.
instance FromJSON SpecIR where
  parseJSON = withObject "SpecIR" $ \o ->
    SpecIR <$> o .:  "irVersion"
           <*> o .:  "spec"
           <*> o .:  "source"
           <*> o .:  "pdfMd5"
           <*> o .:  "pages"
           <*> o .:  "labelOffset"
           <*> o .:  "notice"
           <*> o .:  "extractor"
           <*> o .:  "wordComplete"
           <*> o .:  "tables"
           <*> o .:  "headings"
           <*> o .:  "paras"
           <*> o .:? "sigs" .!= []
           <*> o .:? "lot" .!= []
           <*> o .:? "xrefs" .!= []
           <*> o .:  "annots"
instance ToJSON SpecIR where
  toJSON     = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ExtractorIR where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON ExtractorIR where
  toJSON     = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TableIR where
  parseJSON = genericParseJSON snakeOptions
instance ToJSON TableIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

headerSourceName :: HeaderSource -> Text
headerSourceName = \case
  LeadingTH    -> "leading-th"
  FirstOfAllTH -> "first-of-all-th"
  Repeat       -> "repeat"
  BoldFirstRow -> "bold-first-row"
  NoHeader     -> "none"

instance FromJSON HeaderSource where
  parseJSON = enumParser "HeaderSource" headerSourceName
instance ToJSON HeaderSource where
  toJSON = String . headerSourceName

-- | Hand-written so that absent or @null@ flags read as 'False' and absent
-- lists as @[]@; the field order is the record's.
instance FromJSON CellIR where
  parseJSON = withObject "CellIR" $ \o ->
    CellIR <$> o .:  "kind"
           <*> o .:  "text"
           <*> o .:  "lines"
           <*> o .:? "page"
           <*> o .:? "bbox"
           <*> o .:? "est_bbox"
           <*> flag o "bold"
           <*> flag o "underline"
           <*> o .:? "fonts" .!= []
           <*> o .:? "fill_rgb"
           <*> o .:? "fill_bbox"
           <*> o .:? "nested_tables" .!= []
           <*> o .:? "fragments" .!= []
           <*> flag o "split_from_merged"
           <*> flag o "recovered_by_geometry"
           <*> flag o "ragged_join"
           <*> o .:? "adopted_orphan"
           <*> o .:? "adopted_from"
           <*> o .:? "moved_out"
           <*> o .:? "x_extent"
    where flag o k = o .:? k .!= False
instance ToJSON CellIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON CellKind where
  parseJSON = enumParser "CellKind" tshow
instance ToJSON CellKind where
  toJSON = String . tshow

instance FromJSON RepeatedHeaderIR where
  parseJSON = genericParseJSON snakeOptions
instance ToJSON RepeatedHeaderIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON FragmentIR where
  parseJSON = genericParseJSON snakeOptions
instance ToJSON FragmentIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON AdoptedFromIR where
  parseJSON = genericParseJSON snakeOptions
instance ToJSON AdoptedFromIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON MovedOutIR where
  parseJSON = genericParseJSON snakeOptions
instance ToJSON MovedOutIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON HeadingIR where
  parseJSON = genericParseJSON snakeOptions
instance ToJSON HeadingIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON ParaIR where
  parseJSON = genericParseJSON snakeOptions
instance ToJSON ParaIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON ParaKind where
  parseJSON = enumParser "ParaKind" tshow
instance ToJSON ParaKind where
  toJSON = String . tshow

instance FromJSON SigIR where
  parseJSON = genericParseJSON snakeOptions
instance ToJSON SigIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON LotIR where
  parseJSON = genericParseJSON snakeOptions
instance ToJSON LotIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON XrefIR where
  parseJSON = genericParseJSON snakeOptions
instance ToJSON XrefIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON AnnotIR where
  parseJSON = genericParseJSON snakeOptions
instance ToJSON AnnotIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON AnnotKind where
  parseJSON = enumParser "AnnotKind" tshow
instance ToJSON AnnotKind where
  toJSON = String . tshow

instance FromJSON Rect where
  parseJSON v = parseJSON v >>= \case
    [a, c, d, e] -> pure (Rect a c d e)
    xs           -> fail ("Rect: expected 4 numbers, got " <> show (length xs))
instance ToJSON Rect where
  toJSON (Rect a c d e) = toJSON [a, c, d, e]

instance FromJSON PageRect where
  parseJSON v = uncurry PageRect <$> parseJSON v
instance ToJSON PageRect where
  toJSON (PageRect p rc) = toJSON (p, rc)

instance FromJSON RGB where
  parseJSON v = parseJSON v >>= \case
    [a, c, d] -> pure (RGB a c d)
    xs        -> fail ("RGB: expected 3 numbers, got " <> show (length xs))
instance ToJSON RGB where
  toJSON (RGB a c d) = toJSON [a, c, d]

instance FromJSON StructPath where
  parseJSON = \case
    String t    -> pure (StructPath t)
    v@(Array _) -> StructPath . T.intercalate "/" . map tshow <$> (parseJSON v :: Parser [Int])
    v           -> typeMismatch "StructPath" v
instance ToJSON StructPath where
  toJSON (StructPath t) = String t


-- * Reading

-- | Decode an IR v1 file; the message names the file. Another 'irVersion' is an error.
readIR :: FilePath -> IO (Either String SpecIR)
readIR path = checked . eitherDecodeStrict' <$> readFileBinary path
  where
    checked :: Either String SpecIR -> Either String SpecIR
    checked = either (Left . located) versioned
    located msg = path <> ": " <> msg
    versioned :: SpecIR -> Either String SpecIR
    versioned ir
      | ir.irVersion == irVersion1 = Right ir
      | otherwise = Left (located ("irVersion " <> show ir.irVersion <> ", expected " <> show irVersion1))
