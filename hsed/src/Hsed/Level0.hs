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

The JSON of these types (aeson's 'A.ToJSON') is what the jig's reports
print of a Level 0 response; bytes in hex ("Hsed.Wire.Internal".'trimmedHex').

The bodies are milestone M3's group G2's; until it lands they are stubs.
-}
module Hsed.Level0
  ( -- * A response, split
    Level0 (..)
  , Descriptor (..)
  , Level0Problem (..)
  , splitLevel0
  , renderLevel0
    -- * Descriptors, decoded
  , DecodedDescriptor (..)
  , decodeDescriptor
  , featureDescriptor
  ) where

import qualified Data.Aeson             as A
import qualified Data.ByteString.Base16 as B16
import qualified RIO.ByteString         as B
import           RIO

import           Hsed.Core.Feature      (Allowance (..), Feature (featureCode))
import           Hsed.Core.Layout       (Content (..), Difference (..), LayoutError)
import           Hsed.Wire.Internal     (trimmedHex)


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
  = HeaderDiffers ![Difference]       -- ^ the header read other than Table 39 pins it (its versions)
  | BeyondTransfer !Word32 !Int       -- ^ Length of Parameter Data asks for more than the transfer holds: the bytes it
                                      --   asks for (its origin and itself), the bytes the transfer holds
  | OutOfCodeOrder !Word16 !Word16    -- ^ a descriptor's code, then a next one not larger (3.3.6.3, PDF 68)
  | NonZeroPadding !Int               -- ^ the offset of the first byte other than zero after the descriptors
  | DescriptorTruncated !Word16       -- ^ a descriptor (its code) whose length runs past Length of Parameter Data
  deriving (Eq, Show)

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
splitLevel0 :: ByteString -> Either LayoutError Level0
splitLevel0 = error "G2: splitLevel0"

-- | A response from a header's vendor unique bytes and descriptors, its
--   Length of Parameter Data theirs, truncated or zero-padded to a transfer
--   length (3.3.6.2, PDF 67).
renderLevel0 :: ByteString -> [Descriptor] -> Word32 -> ByteString
renderLevel0 = error "G2: renderLevel0"


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
decodeDescriptor :: Descriptor -> Maybe DecodedDescriptor
decodeDescriptor = error "G2: decodeDescriptor"

-- | A feature record as a response carries it: its code, version and bytes.
featureDescriptor :: Feature f => f -> Either LayoutError Descriptor
featureDescriptor f = error ("G2: featureDescriptor, code " <> show (featureCode (Just f)))


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
    BeyondTransfer need n  -> A.object ["problem" A..= ("beyond_transfer" :: Text), "asked" A..= need, "transferred" A..= n]
    OutOfCodeOrder a b     -> A.object ["problem" A..= ("out_of_code_order" :: Text), "code" A..= a, "next" A..= b]
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
