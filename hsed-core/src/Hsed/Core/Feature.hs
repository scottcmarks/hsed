{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Core.Feature
Description : The runtime form of a feature descriptor: a record over its layout, its code, version, length, header and allowances
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The back end of the @feature@ rule (design docs/spec-as-source.md example E8).
A feature descriptor is a byte grid captioned @… Feature Descriptor@ (Core
Tables 42 and 43): a header in the general format its spec defines (Core Table
41: a code, a version, reserved bits and a length), then the feature's own
fields. The spec compiler makes each a record over its layout, as it makes
every byte grid's ("Hsed.Core.Record": a 'LayoutRecord', its fields typed by
their widths, flags 'Bool'), and an instance of 'Feature', the subclass that
adds what a descriptor is besides its fields:

* 'featureCode', the code the header's first field holds, which identifies
  the descriptor among the others of a Level 0 Discovery response; and the
  version and the length the header holds, when the spec pins them
  ('featureVersion', 'featureLength': the length counts the bytes after the
  header);
* 'featureHeader', the general format's size and the name of its length
  field, which say where a descriptor ends: its header and as many bytes as
  its length field holds;
* 'featureAllowances', each field the spec lists a value of other than an
  exact one (an exact one is a pin of the layout), by its name, with what
  the spec allows it ('Allowance': a vendor unique value, any version, a
  minimum, or a condition as printed, as Opal lists a descriptor's values):
  where two TPers' descriptors may differ, which a host comparing them (the
  jig's Pair runner, design section 9.2) allows. None by default, which
  Core's instances keep.

A pinned field (the header's, and any other the spec pins) and reserved bits
are no fields of the record: they are written as the layout has them and, read
as anything else, reported ('decodeFeature') or refused
('decodeFeatureStrict': how a TPer's own descriptor is held to what it SHALL
send). Either way the input after a descriptor starts where its length field
says it ends, whatever its pins say, so that the next descriptor of a Level 0
Discovery response is read from its first byte.
-}
module Hsed.Core.Feature
  ( -- * Feature descriptors
    Feature (..)
  , FeatureHeader (..)
  , decodeFeature
  , decodeFeatureStrict
    -- * Allowances
  , Allowance (..)
    -- * The bit map
  , bitMap
  ) where

import qualified RIO.ByteString   as B
import qualified RIO.List         as L
import           RIO

import           Hsed.Core.Layout (Content (..), Decoded (..), Field (..), Layout (..), LayoutError (..), bitOffset,
                                   decode, decodeStrict, fieldEnd)
import           Hsed.Core.Record (DecodedRecord (..), LayoutRecord (..))


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hsed.Core.Layout (FieldKind (..), spanning)
-- >>> import Hsed.Core.Record (flag, flagField, number, numberField)
-- >>> let small = Layout "Small" 2 [spanning "Code" (0, 7) (0, 4) (Pinned 3), spanning "Reserved" (0, 3) (0, 1) Reserved, spanning "Ready" (0, 0) (0, 0) Flag, spanning "Count" (1, 7) (1, 0) Unsigned] Nothing
-- >>> :{
-- data Small = Small { smallReady :: Bool, smallCount :: Word8 } deriving (Eq, Show)
-- instance LayoutRecord Small where
--   recordLayout _ = small
--   recordFields s = [("Ready", flag (smallReady s)), ("Count", number (smallCount s))]
--   fromFields vs = pure Small <*> flagField "Ready" vs <*> numberField "Count" vs
-- instance Feature Small where
--   featureCode _    = 3
--   featureVersion _ = Nothing
--   featureLength _  = Nothing
--   featureHeader _  = FeatureHeader 2 "Count"
-- :}


-- * Feature descriptors

-- | The general format a feature descriptor's header conforms to (Core Table
--   41): its size in bytes, and the field of it whose value is the count of
--   the descriptor's bytes after the header.
data FeatureHeader = FeatureHeader
  { headerSize        :: !Int
  , headerLengthField :: !Text
  }
  deriving (Eq, Show, Data)

-- | A feature descriptor's record: a record over its layout, the constants
--   of its header, and what the spec allows its fields.
class LayoutRecord f => Feature f where
  -- | The code the header's first field holds: which feature the descriptor describes.
  featureCode       :: proxy f -> Integer
  -- | The version the header holds, when the spec pins one.
  featureVersion    :: proxy f -> Maybe Integer
  -- | The length the header holds, the bytes of the descriptor after its header, when the spec pins it.
  featureLength     :: proxy f -> Maybe Integer
  -- | The header's general format: where a descriptor ends.
  featureHeader     :: proxy f -> FeatureHeader
  -- | Each field the spec lists a value of other than an exact one, by its
  --   name in the layout, with what the spec allows it; none unless the
  --   instance names some (Core's name none):
  --
  -- >>> featureAllowances (Proxy :: Proxy Small)
  -- []
  featureAllowances :: proxy f -> [(Text, Allowance)]
  featureAllowances _ = []

-- | A descriptor from the start of an input, a pin read with another value
--   and reserved bits read other than zero reported (as
--   "Hsed.Core.Record".'Hsed.Core.Record.decodeRecord' reports them), the
--   rest of the input after the bytes its length field gives: how a host
--   reads what a drive sent. Its header is read first: a length field giving
--   more bytes than the input holds is 'Truncated'; one giving fewer than the
--   layout's (an older, shorter version of the descriptor) is
--   'LengthBelowLayout', which says where the descriptor ends, so that a
--   walk over a Level 0 Discovery response can step past it to the next.
decodeFeature :: forall f. Feature f => ByteString -> Either LayoutError (DecodedRecord f)
decodeFeature = decodeWith decode

-- | 'decodeFeature', refusing a pin read with another value.
decodeFeatureStrict :: forall f. Feature f => ByteString -> Either LayoutError (DecodedRecord f)
decodeFeatureStrict = decodeWith decodeStrict

decodeWith :: forall f. Feature f => (Layout -> ByteString -> Either LayoutError Decoded) -> ByteString -> Either LayoutError (DecodedRecord f)
decodeWith reader input = do
  let p = Proxy :: Proxy f
      l = recordLayout p
      h = featureHeader p
      -- the header alone: the layout's fields within its bytes
      header = l { layoutSize = headerSize h, layoutFields = filter ((<= headerSize h * 8) . fieldEnd) (layoutFields l), layoutTail = Nothing }
  hd <- decode header input
  n <- case L.lookup (headerLengthField h) (decodedFields hd) of
    Just (Number v) -> Right v
    _               -> Left (MissingField (headerLengthField h))
  let end = toInteger (headerSize h) + n
      clipped = fromInteger (min end (toInteger (maxBound :: Int)))
  when (end > toInteger (B.length input)) (Left (Truncated clipped (B.length input)))
  when (end < toInteger (layoutSize l)) (Left (LengthBelowLayout (headerLengthField h) clipped (layoutSize l)))
  d <- reader l input
  r <- fromFields (decodedFields d)
  pure (DecodedRecord r (decodedDifferences d) (B.drop clipped input))


-- * Allowances

-- | What a spec allows a feature descriptor's field to hold where the value
--   it lists for the field is not exact (an exact one is a pin of the
--   layout). Opal lists a descriptor's values as bullets,
--   @\<Field\> = \<V\>@, after @SHALL return the following@ (Opal 2.30
--   section 3.1.1, PDF 19-23). A TPer's descriptor holds its own value in
--   such a field, and a host comparing two TPers' descriptors allows them to
--   differ there (a 'Minimum' only at or above it).
data Allowance
  = VendorUnique         -- ^ @VU@: any value, the vendor's (@ComID Mgmt Supported = VU@, Opal 3.1.1.2, PDF 20)
  | AnyVersion !Integer  -- ^ @V or any version that supports the defined features in this SSC@: V, the version printed, or
                         --   another that supports the SSC's features (@Version = 0x1 or any version …@, Opal 3.1.1.2, PDF 20)
  | Minimum !Integer     -- ^ @N or larger@: N, the value printed, or more (@Number of Locking SP Admin Authorities = 4 or
                         --   larger@, Opal 3.1.1.5, PDF 23)
  | AsPrinted !Text      -- ^ any other, its text as printed: the feature's present state (@MBR Done = **@, Opal 3.1.1.3,
                         --   PDF 20-21) or a reference (@Locking Enabled = See section 3.1.1.3.1@, PDF 21; @SSC Minor Version
                         --   Number = As specified in Table 7@, Opal 3.1.1.5, PDF 23)
  deriving (Eq, Show, Data)


-- * The bit map

-- | Each bit of a layout's fixed part, byte 0 first and bit 7 first in each
--   byte, with the field it lies in (none for a bit no field covers).
--
-- >>> [ (pos, fieldName f) | (pos, Just f) <- bitMap small, fst pos == 0 ]
-- [((0,7),"Code"),((0,6),"Code"),((0,5),"Code"),((0,4),"Code"),((0,3),"Reserved"),((0,2),"Reserved"),((0,1),"Reserved"),((0,0),"Ready")]
bitMap :: Layout -> [((Int, Int), Maybe Field)]
bitMap l =
  [ ((byte, bit), L.find (covers (bitOffset byte bit)) (layoutFields l))
  | byte <- [0 .. layoutSize l - 1], bit <- [7, 6 .. 0] ]
  where covers o f = fieldStart f <= o && o < fieldStart f + fieldWidth f
