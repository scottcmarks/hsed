{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Level0
Description : A Level 0 Discovery response split into its header and descriptors, and each descriptor decoded
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The answer to IF-RECV on protocol 1, ComID 1 (Core 3.3.6, PDF 67-71): a
header (Core Table 39), then feature descriptors in increasing code order
(3.3.6.3, PDF 68), each a general header (Table 41: code, version, length)
and the bytes its length counts, then zeros to the transfer length (3.3.6.2,
PDF 67). Where the descriptors end is the header's Length of Parameter Data,
which counts the bytes after itself (3.3.6.2.1, PDF 68): 'splitLevel0'
takes that origin and the descriptor header from the generated layouts
("Hsed.Gen.Core.Layouts": T39 and T41), so that no length or offset is
written here (design 13.26 (b)).

Splitting keeps every descriptor, known or not ('Descriptor': its code,
version and bytes), and reports what does not hold ('Level0Problem');
decoding a descriptor ('decodeDescriptor') reads it through the layout of
its code when hsed has one: Opal's generated descriptors (Opal Tables 3 to
6 and 8) and the hand-written ones of "Hsed.Sim.FeatureSets"; a vendor's or
an unknown code stays undecoded. 'renderLevel0' is the other way: a header
and descriptors to the bytes of a transfer length, as the simulator
answers.

A descriptor is stepped by its own header alone, whatever its code: one
whose Length gives fewer bytes than hsed's layout of its code (an older,
shorter version) is kept and stepped past like any other, and only
'decodeDescriptor' declines it; one whose Length runs past the end of the
response is kept with the bytes there are and reported. The response's
problems are reported, not refused: 'splitLevel0' fails only on a
transfer shorter than Table 39's header. Opal 2.30 allows one of Core's: a
data structure revision other than Table 39's ('allowedByOpal').

The JSON of these types (aeson's 'A.ToJSON') is what the jig's reports
print of a Level 0 response; bytes in hex ("Hsed.Wire".'trimmedHex').
-}
module Hsed.Level0
  ( -- * A response, split
    Level0 (..)
  , Descriptor (..)
  , Level0Problem (..)
  , allowedByOpal
  , cutShort
  , splitLevel0
  , renderLevel0
    -- * Descriptors, decoded
  , DecodedDescriptor (..)
  , decodeDescriptor
  , featureDescriptor
  , setBitsAt
  ) where

import qualified Data.Aeson             as A
import           Data.Bits              (testBit)
import qualified Data.ByteString.Base16 as B16
import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO

import           Hsed.Core.Feature      (Allowance (..), Feature (..), FeatureHeader (..))
import           Hsed.Core.Layout       (Content (..), Decoded (..), Difference (..), Field (..), Layout (..),
                                         LayoutError (..), decode, fieldEnd)
import           Hsed.Core.Record       (DecodedRecord (..), LayoutRecord (..), decodeRecord, encodeRecord, numberField)
import           Hsed.Gen.Core.Features (TPerFeature)
import           Hsed.Gen.Core.Layouts  (FeatureDescriptorTemplate (..), FeatureDescriptorTemplateHeader (..),
                                         Level0DiscoveryHeader (..), featureDescriptorTemplateLayout,
                                         level0DiscoveryHeaderLayout)
import           Hsed.Gen.Opal.Features (Level0DiscoveryGeometryReportingFeature, Level0DiscoveryLockingFeature,
                                         Level0DiscoveryOpalSSCV2Feature,
                                         Level0DiscoverySupportedDataRemovalMechanismFeature,
                                         Level0DiscoveryTPerFeature)
import           Hsed.Sim.FeatureSets   (BlockSIDFeature, DataStoreFeature, NamespaceLockingFeature, SiisFeature,
                                         dataRemovalAllowances, geometryAllowances)
import           Hsed.Wire              (trimmedHex)


-- * A response, split

-- | One feature descriptor of a response: its code and version (Core Table
--   41) and all its bytes, its header and the bytes its length counts.
data Descriptor = Descriptor
  { dCode    :: !Word16
  , dVersion :: !Word8
  , dBytes   :: !ByteString
  }
  deriving (Eq, Show)

-- | What does not hold of a response as split.
data Level0Problem
  = HeaderDiffers ![Difference]          -- ^ the header read other than Table 39 pins it (its versions)
  | ParameterDataShort !Word32           -- ^ Length of Parameter Data counts fewer bytes than the header holds after it
                                         --   (3.3.6.2.1, PDF 68: it counts the valid bytes after itself)
  | BeyondTransfer !Word32 !Int          -- ^ Length of Parameter Data asks for more than the transfer holds: the bytes it
                                         --   asks for (its origin and itself), the bytes the transfer holds
  | OutOfCodeOrder !Word16 !Word16       -- ^ a descriptor's code, then a next one not larger (3.3.6.3, PDF 68)
  | LengthNotFourfold !Word16 !Int       -- ^ a descriptor (its code) whose Length is no multiple of 4 (3.3.6.3.1.3, PDF 69)
  | ReservedCode !Int                    -- ^ the offset of the first descriptor of code 0x0000, Core Table 40's Reserved
                                         --   (PDF 69), which no feature has: likely zeros Length of Parameter Data counts
  | NonZeroPadding !Int                  -- ^ the offset of the first byte other than zero after the descriptors
  | DescriptorTruncated !(Maybe Word16)  -- ^ a descriptor whose length runs past Length of Parameter Data: its code, if
                                         --   its bytes hold it
  deriving (Eq, Show)

-- | Whether Opal 2.30 allows a problem Core's split reports: a header whose
--   data structure revision is other than Core Table 39's 0x0000 and 0x0001,
--   which Opal 2.30 Table 2 allows (\"0x00000001 or any version that
--   supports the defined features in this SSC\", PDF 19), with nothing else
--   of it read otherwise.
allowedByOpal :: Level0Problem -> Bool
allowedByOpal = \case
  HeaderDiffers ds@(_ : _) -> all revision ds
  _                        -> False
  where
    revision = \case
      PinDiffers name _ _ _ -> name `elem` ["Data Structure Major Version", "Data Structure Minor Version"]
      _                     -> False

-- | Whether a problem says the response was cut short, its descriptors some
--   of the drive's or part of one: Length of Parameter Data past the
--   transfer, or a descriptor past Length of Parameter Data.
cutShort :: Level0Problem -> Bool
cutShort = \case
  BeyondTransfer _ _    -> True
  DescriptorTruncated _ -> True
  _                     -> False

-- | A response split: its header (Table 39's bytes), its Length of
--   Parameter Data, its descriptors in the order sent, what follows them to
--   the end of the transfer, and what does not hold.
data Level0 = Level0
  { l0Header                :: !ByteString
  , l0LengthOfParameterData :: !Word32
  , l0Descriptors           :: ![Descriptor]
  , l0Padding               :: !ByteString
  , l0Problems              :: ![Level0Problem]
  }
  deriving (Eq, Show)

-- | A response's header and descriptors, from the start of a transfer: the
--   header through Table 39's layout (its versions read leniently), the
--   descriptors to the end Length of Parameter Data gives, each stepped by
--   Table 41's header.
--
--   The descriptors run from the end of the header to the end Length of
--   Parameter Data gives, or to the end of the transfer if that comes first
--   ('BeyondTransfer'); Length of Parameter Data counts at least the
--   header's bytes after it ('ParameterDataShort'); a descriptor whose
--   length runs past it is kept with the bytes there are
--   ('DescriptorTruncated'); each code must be larger than the one before
--   ('OutOfCodeOrder'), each Length a multiple of 4 ('LengthNotFourfold'),
--   no code the reserved 0x0000 ('ReservedCode'), and what follows the
--   descriptors zeros ('NonZeroPadding'). A transfer shorter than the header
--   is 'Hsed.Core.Layout.Truncated'.
splitLevel0 :: ByteString -> Either LayoutError Level0
splitLevel0 input = do
  DecodedRecord header differences _ <- decodeRecord input
  let lengthOfParameterData = level0DiscoveryHeaderLengthOfParameterData header
      asked                 = toInteger lengthOrigin + toInteger lengthOfParameterData
      held                  = B.length input
      -- where the descriptors end and the padding starts: what Length of Parameter Data asks, as far as the
      -- transfer holds it, and never inside the header
      end                   = max headerEnd (fromInteger (min asked (toInteger held)))
      pieces                = L.unfoldr (piece end) headerEnd
      descriptors           = mapMaybe pieceDescriptor pieces
      offsets               = L.scanl (+) headerEnd (map (B.length . dBytes) descriptors)
      codes                 = map dCode descriptors
      padding               = B.drop end input
  pure Level0
    { l0Header                = B.take headerEnd input
    , l0LengthOfParameterData = lengthOfParameterData
    , l0Descriptors           = descriptors
    , l0Padding               = padding
    , l0Problems              = concat
        [ [ HeaderDiffers differences | not (null differences) ]
        , [ ParameterDataShort lengthOfParameterData | toInteger lengthOfParameterData < toInteger (headerEnd - lengthOrigin) ]
        , [ BeyondTransfer (fromInteger (min asked (toInteger (maxBound :: Word32)))) held | asked > toInteger held ]
        , [ OutOfCodeOrder a b | (a, b) <- zip codes (drop 1 codes), b <= a ]
        , [ LengthNotFourfold (dCode d) n | Whole d <- pieces, let n = B.length (dBytes d) - headerSize generalHeader, n `mod` 4 /= 0 ]
        , take 1 [ ReservedCode at | (d, at) <- zip descriptors offsets, dCode d == 0x0000 ]
        , [ DescriptorTruncated c | Cut _ c <- pieces ]
        , [ NonZeroPadding (end + i) | Just i <- [B.findIndex (/= 0) padding] ]
        ]
    }
  where
    -- the descriptor starting at a byte, up to an end, and where the next one starts: its header read through
    -- Table 41, its bytes the header's and as many more as its length field gives
    piece end at
      | at >= end = Nothing
      | otherwise = Just $ case descriptorHeader generalHeader region of
          Right (code, version, size)
            | size <= B.length region -> (Whole (Descriptor code version (B.take size region)), at + size)
            | otherwise               -> (Cut (Just (Descriptor code version region)) (Just code), end)
          Left _                      -> (Cut Nothing (partialCode region), end)
      where region = B.take (end - at) (B.drop at input)

-- | One step of the walk over a response's descriptors: a descriptor, or one
--   cut short by the end of the response (its bytes, when its header is
--   there, and its code, when its bytes hold it).
data Piece
  = Whole Descriptor
  | Cut (Maybe Descriptor) (Maybe Word16)

pieceDescriptor :: Piece -> Maybe Descriptor
pieceDescriptor = \case
  Whole d -> Just d
  Cut d _ -> d

-- | The code of a descriptor too short for its header, if its bytes hold
--   Table 41's Feature Code whole; none is made up of zeros.
partialCode :: ByteString -> Maybe Word16
partialCode bytes
  | B.length bytes >= codeEnd =
      either (const Nothing) (\(code, _, _) -> Just code) (descriptorHeader generalHeader (fitted (headerSize generalHeader) bytes))
  | otherwise = Nothing
  where
    codeEnd = maybe (headerSize generalHeader) ((`div` 8) . fieldEnd)
                    (L.find ((== "Feature Code") . fieldName) (layoutFields featureDescriptorTemplateLayout))

-- | A response from a header's vendor unique bytes and descriptors, its
--   Length of Parameter Data theirs, truncated or zero-padded to a transfer
--   length (3.3.6.2, PDF 67). The vendor unique bytes are fitted to Table
--   39's field, truncated or zero-padded: 'mempty' for none.
renderLevel0 :: ByteString -> [Descriptor] -> Word32 -> ByteString
renderLevel0 vendorUnique descriptors len = fitted (fromIntegral len) (header <> body)
  where
    body   = foldMap dBytes descriptors
    -- the generated layout tiles and the vendor unique bytes fit their field: the header always encodes, but for
    -- descriptors past what Length of Parameter Data's 32 bits count, an error
    header = either (\e -> error ("Hsed.Level0.renderLevel0: the header does not encode: " <> show e)) id $ encodeRecord Level0DiscoveryHeader
      { level0DiscoveryHeaderLengthOfParameterData = fromIntegral (headerEnd - lengthOrigin + B.length body)
      , level0DiscoveryHeaderVendorUnique          = fitted (B.length noVendorUnique) vendorUnique
      }

-- | Bytes truncated or zero-padded to a length.
fitted :: Int -> ByteString -> ByteString
fitted n bytes = B.take n (bytes <> B.replicate (n - B.length bytes) 0)


-- * Table 39 and Table 41, as the generated layouts give them

-- | Where Length of Parameter Data starts counting: the end of that field of
--   Table 39 (it counts the bytes after itself, 3.3.6.2.1, PDF 68).
lengthOrigin :: Int
lengthOrigin = maybe (error "Hsed.Level0: Core Table 39's layout has no Length of Parameter Data") ((`div` 8) . fieldEnd)
                     (L.find ((== "Length of Parameter Data") . fieldName) header)
  where header = layoutFields level0DiscoveryHeaderLayout

-- | Where the header ends and the first descriptor starts (Table 38, PDF 67).
headerEnd :: Int
headerEnd = layoutSize level0DiscoveryHeaderLayout

-- | Table 39's vendor unique bytes, as many as its layout gives them: zeros.
noVendorUnique :: ByteString
noVendorUnique = either (const mempty) (level0DiscoveryHeaderVendorUnique . drRecord) (decodeRecord (B.replicate headerEnd 0))

-- | Core Table 41's general format of a descriptor's header, as the
--   generated descriptors have it: its size and its length field.
generalHeader :: FeatureHeader
generalHeader = featureHeader (Proxy :: Proxy TPerFeature)

-- | A descriptor's code, version and size (its header and the bytes its
--   length field gives), from bytes that hold at least its header: the
--   header read through Table 41's layout, its size and its length field
--   as a 'FeatureHeader' gives them.
descriptorHeader :: FeatureHeader -> ByteString -> Either LayoutError (Word16, Word8, Int)
descriptorHeader h bytes = do
  Decoded fields _ _ <- decode featureDescriptorTemplateLayout bytes
  FeatureDescriptorTemplate (FeatureDescriptorTemplateHeader code version _) _ <- fromFields fields
  len <- numberField (headerLengthField h) fields
  pure (code, version, headerSize h + len)


-- * Descriptors, decoded

-- | A descriptor read through the layout of its code: which layout read it,
--   its fields (pinned ones included), what differed from the layout's pins
--   and reserved bits, and what the spec allows its fields
--   ("Hsed.Core.Feature".'Hsed.Core.Feature.featureAllowances').
data DecodedDescriptor = DecodedDescriptor
  { ddSource      :: !Text                  -- ^ the layout's spec and table, @opal-2.30 T3@
  , ddFields      :: ![(Text, Content)]
  , ddDifferences :: ![Difference]
  , ddAllowances  :: ![(Text, Allowance)]
  }
  deriving (Eq, Show)

-- | A descriptor decoded by its code: Opal's T3-T6 and T8, the hand-written
--   feature sets; 'Nothing' for a vendor's or an unknown code.
--
--   Read leniently ("Hsed.Core.Layout".'Hsed.Core.Layout.decode'): a pin read
--   otherwise (a drive's older version of a descriptor) and reserved bits not
--   zero are 'ddDifferences'. A descriptor longer than its layout is read as
--   far as the layout goes (its Length a difference); one shorter than its
--   layout is not decoded.
decodeDescriptor :: Descriptor -> Maybe DecodedDescriptor
decodeDescriptor d = do
  (source, layout, allowances) <- L.lookup (dCode d) known
  Decoded fields differences _ <- either (const Nothing) Just (decode layout (dBytes d))
  pure (DecodedDescriptor source fields differences allowances)

-- | The descriptors hsed has a layout of, by code: Opal's (generated from
--   Opal 2.30 Tables 3-6 and 8), what the simulator allows Geometry
--   Reporting until M5 ("Hsed.Sim.FeatureSets".'geometryAllowances') and
--   Supported Data Removal Mechanism ('dataRemovalAllowances', Tables 9-12),
--   and the hand-written feature sets'.
known :: [(Word16, (Text, Layout, [(Text, Allowance)]))]
known =
  [ entry "opal-2.30 T3" (Proxy :: Proxy Level0DiscoveryTPerFeature) []
  , entry "opal-2.30 T4" (Proxy :: Proxy Level0DiscoveryLockingFeature) []
  , entry "opal-2.30 T5" (Proxy :: Proxy Level0DiscoveryGeometryReportingFeature) geometryAllowances
  , entry "siis-1.20 T2" (Proxy :: Proxy SiisFeature) []
  , entry "sedutil Discovery0DatastoreTable" (Proxy :: Proxy DataStoreFeature) []
  , entry "opal-2.30 T6" (Proxy :: Proxy Level0DiscoveryOpalSSCV2Feature) []
  , entry "sedutil Discovery0BlockSIDFeatures" (Proxy :: Proxy BlockSIDFeature) []
  , entry "cnl-1.30 T6" (Proxy :: Proxy NamespaceLockingFeature) []
  , entry "opal-2.30 T8" (Proxy :: Proxy Level0DiscoverySupportedDataRemovalMechanismFeature) dataRemovalAllowances
  ]
  where
    entry :: Feature f => Text -> Proxy f -> [(Text, Allowance)] -> (Word16, (Text, Layout, [(Text, Allowance)]))
    entry source p more = (fromInteger (featureCode p), (source, recordLayout p, featureAllowances p <> more))

-- | The bits a reserved field of a descriptor was read with set, each as the
--   byte of the descriptor it lies in and its bit there (7 the most
--   significant): the field found in the layout of the code by its name and
--   first bit ("Hsed.Core.Layout".'ReservedNotZero' gives those and the
--   field's value), a reserved run of a tail being the one byte the
--   difference names. 'Nothing' for another difference, or a code hsed has no
--   layout of.
--
-- >>> setBitsAt 0x0002 (ReservedNotZero "Reserved" 40 (2 ^ 80))
-- Just [(5,0)]
setBitsAt :: Word16 -> Difference -> Maybe [(Int, Int)]
setBitsAt code = \case
  ReservedNotZero name at v -> do
    (_, layout, _) <- L.lookup code known
    let width = maybe 8 fieldWidth (L.find (\f -> fieldName f == name && fieldStart f == at) (layoutFields layout))
    pure [ ((at + i) `div` 8, 7 - (at + i) `mod` 8) | i <- [0 .. width - 1], testBit v (width - 1 - i) ]
  _ -> Nothing

-- | A feature record as a response carries it: its code, version and bytes,
--   read back from its encoding; an encoding whose length field does not
--   count the bytes after its header is 'LengthDisagrees'.
featureDescriptor :: Feature f => f -> Either LayoutError Descriptor
featureDescriptor f = do
  bytes <- encodeRecord f
  let h = featureHeader (Just f)
  (code, version, size) <- descriptorHeader h bytes
  unless (size == B.length bytes)
    (Left (LengthDisagrees (headerLengthField h) (toInteger (size - headerSize h)) (B.length bytes - headerSize h)))
  pure (Descriptor code version bytes)


-- * JSON

instance A.ToJSON Descriptor where
  toJSON d = A.object
    [ "code"    A..= dCode d
    , "version" A..= dVersion d
    , "len"     A..= B.length (dBytes d)
    , "bytes"   A..= trimmedHex (dBytes d)
    ]

instance A.ToJSON Level0Problem where
  toJSON = \case
    HeaderDiffers ds       -> A.object ["problem" A..= ("header_differs" :: Text), "differences" A..= map differenceJSON ds]
    ParameterDataShort n   -> A.object ["problem" A..= ("parameter_data_short" :: Text), "length_of_parameter_data" A..= n]
    BeyondTransfer need n  -> A.object ["problem" A..= ("beyond_transfer" :: Text), "asked" A..= need, "transferred" A..= n]
    OutOfCodeOrder a b     -> A.object ["problem" A..= ("out_of_code_order" :: Text), "code" A..= a, "next" A..= b]
    LengthNotFourfold c n  -> A.object ["problem" A..= ("length_not_fourfold" :: Text), "code" A..= c, "length" A..= n]
    ReservedCode at        -> A.object ["problem" A..= ("reserved_code" :: Text), "offset" A..= at]
    NonZeroPadding at      -> A.object ["problem" A..= ("non_zero_padding" :: Text), "offset" A..= at]
    DescriptorTruncated c  -> A.object ["problem" A..= ("descriptor_truncated" :: Text), "code" A..= c]

instance A.ToJSON Level0 where
  toJSON l = A.object
    [ "header"                   A..= trimmedHex (l0Header l)
    , "length_of_parameter_data" A..= l0LengthOfParameterData l
    , "descriptors"              A..= l0Descriptors l
    , "padding_len"              A..= B.length (l0Padding l)
    , "padding"                  A..= trimmedHex (l0Padding l)
    , "problems"                 A..= l0Problems l
    ]

instance A.ToJSON DecodedDescriptor where
  toJSON d = A.object
    [ "source"      A..= ddSource d
    , "fields"      A..= map fieldJSON (ddFields d)
    , "differences" A..= map differenceJSON (ddDifferences d)
    , "allowances"  A..= map allowanceJSON (ddAllowances d)
    ]

-- | A field and its value: a number as itself, bytes in hex.
fieldJSON :: (Text, Content) -> A.Value
fieldJSON (name, c) = A.object ["field" A..= name, "value" A..= contentJSON c]

contentJSON :: Content -> A.Value
contentJSON = \case
  Number n -> A.toJSON n
  Octets b -> A.String (decodeUtf8Lenient (B16.encode b))

-- | What a lenient decode reported ("Hsed.Core.Layout".'Difference').
differenceJSON :: Difference -> A.Value
differenceJSON = \case
  PinDiffers n at p v    -> A.object ["difference" A..= ("pin_differs" :: Text), "field" A..= n, "bit" A..= at, "pin" A..= p, "read" A..= v]
  ReservedNotZero n at v -> A.object ["difference" A..= ("reserved_not_zero" :: Text), "field" A..= n, "bit" A..= at, "read" A..= v]
  PaddingNotZero n at v  -> A.object ["difference" A..= ("padding_not_zero" :: Text), "field" A..= n, "bit" A..= at, "read" A..= v]

-- | A field's allowance ("Hsed.Core.Feature".'Allowance').
allowanceJSON :: (Text, Allowance) -> A.Value
allowanceJSON (name, a) = A.object (("field" A..= name) : allowance)
  where
    allowance = case a of
      VendorUnique -> ["allows" A..= ("vendor_unique" :: Text)]
      AnyVersion v -> ["allows" A..= ("any_version" :: Text), "printed" A..= v]
      Minimum v    -> ["allows" A..= ("minimum" :: Text), "printed" A..= v]
      AsPrinted t  -> ["allows" A..= ("as_printed" :: Text), "printed" A..= t]
