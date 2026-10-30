{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Pair
Description : The simulator's Level 0 response against a drive's: the descriptor sets, then each shared descriptor's fields
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Pair runner's comparison of Level 0 (design 9.2, M3; lockstep at
M4b): first the descriptor sets, every code the drive reports needing to be
the simulator's too ('prMissing' a failure) and the simulator's codes the
drive lacks reported ('prExtra', no failure); then, for each code both
report, its fields, a difference allowed where the spec allows the field
other values ("Hsed.Core.Feature".'Allowance': a vendor unique value, any
version, a minimum the drive's value meets, a condition as printed) and a
failure otherwise.

Each shared descriptor is read through the layout of its code
("Hsed.Level0".'decodeDescriptor'), its fields compared by name and the
allowances its layout lists; a field the drive reads with reserved bits or
padding set that the simulator's does not is a difference no allowance
covers. A code hsed has no layout for is compared byte for byte, a
difference allowed only in Core Table 40's vendor unique codes (PDF 69).
Descriptors whose bytes are equal are not decoded at all.
-}
module Hsed.Jig.Pair
  ( FieldDiff (..)
  , PairReport (..)
  , pairLevel0
    -- * The parts
  , fieldDiffs
  , allowed
  , pairFails
  ) where

import qualified Data.Aeson             as A
import qualified Data.ByteString.Base16 as B16
import qualified RIO.List               as L
import           RIO

import           Hsed.Core.Code         (CodeRow (..), codeRowsNamed, inSpan)
import           Hsed.Core.Feature      (Allowance (..))
import           Hsed.Core.Layout       (Content (..), Difference (..))
import           Hsed.Gen.Core.Codes    (cFeatureCodes)
import           Hsed.Level0            (DecodedDescriptor (..), Descriptor (..), Level0 (..), decodeDescriptor)


-- | A field of a descriptor both report whose values differ, and what the
--   spec allows it, if anything.
data FieldDiff = FieldDiff
  { fdCode      :: !Word16
  , fdField     :: !Text
  , fdSim       :: !Content
  , fdDrive     :: !Content
  , fdAllowance :: !(Maybe Allowance)
  }
  deriving (Eq, Show)

-- | The comparison: the simulator's codes the drive lacks, the drive's the
--   simulator lacks, the differences allowed, the others.
data PairReport = PairReport
  { prExtra   :: ![Word16]
  , prMissing :: ![Word16]
  , prAllowed :: ![FieldDiff]
  , prDiffs   :: ![FieldDiff]
  }
  deriving (Eq, Show)

-- | The simulator's response against the drive's: the codes in increasing
--   order, each code's first descriptor compared.
pairLevel0 :: Level0 -> Level0 -> PairReport
pairLevel0 sim drive = PairReport
  { prExtra   = simCodes L.\\ driveCodes
  , prMissing = driveCodes L.\\ simCodes
  , prAllowed = filter allowed diffs
  , prDiffs   = filter (not . allowed) diffs
  }
  where
    codes      = L.sort . L.nub . map dCode . l0Descriptors
    simCodes   = codes sim
    driveCodes = codes drive
    diffs = concat [ descriptorDiffs s d | c <- simCodes, c `elem` driveCodes, Just s <- [firstOf c sim], Just d <- [firstOf c drive] ]
    firstOf c  = L.find ((== c) . dCode) . l0Descriptors

-- | Two descriptors of a code: none if their bytes are equal; else their
--   fields, or their bytes where hsed reads no layout for the code.
descriptorDiffs :: Descriptor -> Descriptor -> [FieldDiff]
descriptorDiffs s d
  | dBytes s == dBytes d = []
  | otherwise = case (decodeDescriptor s, decodeDescriptor d) of
      (Just ds, Just dd) -> fieldDiffs (dCode s) ds dd
      _                  -> [FieldDiff (dCode s) "bytes" (Octets (dBytes s)) (Octets (dBytes d)) (vendorUnique (dCode s))]

-- | The fields of two decodes of a code whose values differ, each with what
--   the code's layout allows it; then each reserved bit or padding the drive
--   reads set and the simulator does not, which nothing allows.
fieldDiffs :: Word16 -> DecodedDescriptor -> DecodedDescriptor -> [FieldDiff]
fieldDiffs code s d =
  [ FieldDiff code name sv dv (L.lookup name (ddAllowances s <> ddAllowances d))
  | (name, sv) <- ddFields s, Just dv <- [L.lookup name (ddFields d)], sv /= dv ]
  <> [ FieldDiff code (name <> " (" <> what <> " at bit " <> tshow at <> ")") (Number 0) (Number v) Nothing
     | diff <- ddDifferences d, diff `notElem` ddDifferences s, Just (what, name, at, v) <- [setBits diff] ]
  where
    setBits = \case
      ReservedNotZero name at v -> Just ("reserved" :: Text, name, at, v)
      PaddingNotZero name at v  -> Just ("padding", name, at, v)
      PinDiffers {}             -> Nothing  -- a pinned field is among the fields, compared there

-- | Whether the spec allows a difference: a vendor unique value, any
--   version, a condition as printed, or a minimum the drive's value meets.
allowed :: FieldDiff -> Bool
allowed fd = case fdAllowance fd of
  Nothing             -> False
  Just VendorUnique   -> True
  Just (AnyVersion _) -> True
  Just (AsPrinted _)  -> True
  Just (Minimum least) -> case fdDrive fd of
    Number v -> v >= least
    Octets _ -> False

-- | Whether a comparison fails: a code the drive reports that the simulator
--   does not, or a difference not allowed.
pairFails :: PairReport -> Bool
pairFails r = not (null (prMissing r) && null (prDiffs r))

-- | A vendor unique code's allowance: Core Table 40's row so named (PDF 69).
vendorUnique :: Word16 -> Maybe Allowance
vendorUnique code
  | any (\r -> inSpan (codeSpan r) (toInteger code)) (codeRowsNamed cFeatureCodes "Vendor Unique") = Just VendorUnique
  | otherwise = Nothing


-- * JSON

instance A.ToJSON FieldDiff where
  toJSON fd = A.object
    [ "code"      A..= fdCode fd
    , "field"     A..= fdField fd
    , "sim"       A..= content (fdSim fd)
    , "drive"     A..= content (fdDrive fd)
    , "allowance" A..= fmap allowance (fdAllowance fd)
    , "allowed"   A..= allowed fd
    ]
    where
      content = \case
        Number n -> A.toJSON n
        Octets b -> A.String (decodeUtf8Lenient (B16.encode b))
      allowance = \case
        VendorUnique -> A.object ["allows" A..= ("vendor_unique" :: Text)]
        AnyVersion v -> A.object ["allows" A..= ("any_version" :: Text), "printed" A..= v]
        Minimum v    -> A.object ["allows" A..= ("minimum" :: Text), "printed" A..= v]
        AsPrinted t  -> A.object ["allows" A..= ("as_printed" :: Text), "printed" A..= t]

instance A.ToJSON PairReport where
  toJSON r = A.object
    [ "extra"       A..= prExtra r
    , "missing"     A..= prMissing r
    , "allowed"     A..= prAllowed r
    , "differences" A..= prDiffs r
    , "fails"       A..= pairFails r
    ]
