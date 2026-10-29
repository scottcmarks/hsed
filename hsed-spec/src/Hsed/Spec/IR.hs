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
fields are 'Maybe', and the repair flags of a cell default to 'False' when absent.
Fields are accessed with @OverloadedRecordDot@ (@ir.tables@, @cell.text@).
-}
module Hsed.Spec.IR
  ( -- * Document
    SpecIR (..)
  , ExtractorIR (..)
  , irVersion1
    -- * Tables
  , TableIR (..)
  , CellIR (..)
  , HeaderSource (..)
  , CellKind (..)
    -- * Headings and paragraphs
  , HeadingIR (..)
  , ParaIR (..)
  , ParaKind (..)
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
  , sigs         :: ![Value]       -- ^ method signature blocks: no schema before M2b
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
  , ordinal            :: !Int                 -- ^ 1-based among top-level tables
  , rects              :: ![PageRect]          -- ^ one per page the table touches
  , headerSource       :: !HeaderSource
  , headerRows         :: ![[CellIR]]
  , repeatedHeaderRows :: ![[CellIR]]
  , rows               :: ![[CellIR]]
  , warnings           :: ![Text]
  } deriving (Eq, Show, Generic)

-- | Where a table's header rows came from.
data HeaderSource = LeadingTH | FirstOfAllTH | Repeat | BoldFirstRow | NoHeader
  deriving (Eq, Show, Bounded, Enum, Generic)

-- | One table cell with its text, geometry, styling and repair flags.
data CellIR = CellIR
  { kind                :: !CellKind
  , text                :: !Text
  , lines               :: ![Text]
  , page                :: !Int
  , bbox                :: !Rect
  , estBbox             :: !(Maybe Rect)
  , bold                :: !Bool
  , underline           :: !Bool
  , fonts               :: ![Text]
  , fillRgb             :: !(Maybe RGB)
  , fillBbox            :: !(Maybe Rect)
  , splitFromMerged     :: !Bool
  , recoveredByGeometry :: !Bool
  , adoptedOrphan       :: !Bool
  , raggedJoin          :: !Bool
  , adoptedFrom         :: !(Maybe Value)      -- ^ where an adopted cell came from
  , movedOut            :: !(Maybe Value)      -- ^ where a moved cell went
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

-- | A @/K@ index path from @/StructTreeRoot@, written @"0/1288"@.
-- Reading also accepts the pre-v1 array form @[0, 1288]@.
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

instance FromJSON SpecIR where
  parseJSON = genericParseJSON defaultOptions
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

-- | Hand-written so that absent repair flags read as 'False' and absent
-- @fonts@ as @[]@; the field order is the record's.
instance FromJSON CellIR where
  parseJSON = withObject "CellIR" $ \o ->
    CellIR <$> o .:  "kind"
           <*> o .:  "text"
           <*> o .:  "lines"
           <*> o .:  "page"
           <*> o .:  "bbox"
           <*> o .:? "est_bbox"
           <*> o .:  "bold"
           <*> o .:  "underline"
           <*> o .:? "fonts" .!= []
           <*> o .:? "fill_rgb"
           <*> o .:? "fill_bbox"
           <*> flag o "split_from_merged"
           <*> flag o "recovered_by_geometry"
           <*> flag o "adopted_orphan"
           <*> flag o "ragged_join"
           <*> o .:? "adopted_from"
           <*> o .:? "moved_out"
    where flag o k = o .:? k .!= False
instance ToJSON CellIR where
  toJSON     = genericToJSON snakeOptions
  toEncoding = genericToEncoding snakeOptions

instance FromJSON CellKind where
  parseJSON = enumParser "CellKind" tshow
instance ToJSON CellKind where
  toJSON = String . tshow

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
