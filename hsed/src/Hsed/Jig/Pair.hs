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
report, its fields, each difference judged by what the spec allows the
field ("Hsed.Core.Feature".'Allowance'): allowed for a vendor unique value,
any version, or a minimum the drive's value meets; unchecked for a condition
as printed that hsed does not evaluate (the present state of a feature, a
LockingInfo column), reported apart and no failure; and a failure
otherwise. A condition as printed that names a table of values hsed holds
is evaluated: the SSC Minor Version Number \"As specified in Table 7\", one
of Opal 2.30 Table 7's ("Hsed.Level0".'sscMinorVersions', PDF 24).

Each shared descriptor is read through the layout of its code
("Hsed.Level0".'decodeDescriptor'), its fields compared by name and the
allowances its layout lists; a field the drive reads with reserved bits or
padding set that the simulator's does not is a difference no allowance
covers, each set bit named by its byte and bit. An allowance of any version
covers the version field alone: a newer descriptor's longer Length, or bits
it sets where hsed's layout has reserved ones, are differences no allowance
covers (Core 3.3.6.3.1.2, PDF 69, asks only that a later version SHOULD be
backward compatible). A code hsed has no layout for is compared byte for
byte, a difference allowed only in Core Table 40's vendor unique codes (PDF
69). Descriptors whose bytes are equal are not decoded at all.
-}
module Hsed.Jig.Pair
  ( FieldDiff (..)
  , PairReport (..)
  , pairLevel0
    -- * The parts
  , fieldDiffs
  , Judgement (..)
  , judged
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
import           Hsed.Level0            (DecodedDescriptor (..), Descriptor (..), Level0 (..), decodeDescriptor, setBitsAt,
                                         sscMinorVersions)


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
--   simulator lacks, the differences allowed, those as printed that hsed
--   does not evaluate, the others.
data PairReport = PairReport
  { prExtra     :: ![Word16]
  , prMissing   :: ![Word16]
  , prAllowed   :: ![FieldDiff]
  , prUnchecked :: ![FieldDiff]
  , prDiffs     :: ![FieldDiff]
  }
  deriving (Eq, Show)

-- | The simulator's response against the drive's: the codes in increasing
--   order, each code's first descriptor compared.
pairLevel0 :: Level0 -> Level0 -> PairReport
pairLevel0 sim drive = PairReport
  { prExtra     = simCodes L.\\ driveCodes
  , prMissing   = driveCodes L.\\ simCodes
  , prAllowed   = filter ((== Allowed) . judged) diffs
  , prUnchecked = filter ((== Unchecked) . judged) diffs
  , prDiffs     = filter ((== NotAllowed) . judged) diffs
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
--   the code's layout allows it; then each reserved bit the drive reads set
--   and the simulator does not, by its byte and bit, and padding read other
--   than zero, which nothing allows.
fieldDiffs :: Word16 -> DecodedDescriptor -> DecodedDescriptor -> [FieldDiff]
fieldDiffs code s d =
  [ FieldDiff code name sv dv (L.lookup name (ddAllowances s <> ddAllowances d))
  | (name, sv) <- ddFields s, Just dv <- [L.lookup name (ddFields d)], sv /= dv ]
  <> concat [ setBits diff | diff <- ddDifferences d, diff `notElem` ddDifferences s ]
  where
    setBits = \case
      diff@(ReservedNotZero name at v) -> case setBitsAt code diff of
        Just bits -> [ FieldDiff code (name <> ", byte " <> tshow byte <> " bit " <> tshow b <> " (reserved)") (Number 0) (Number 1) Nothing
                     | (byte, b) <- bits ]
        Nothing   -> [FieldDiff code (name <> " (reserved at bit " <> tshow at <> ")") (Number 0) (Number v) Nothing]
      PaddingNotZero name at v -> [FieldDiff code (name <> " (padding at bit " <> tshow at <> ")") (Number 0) (Number v) Nothing]
      PinDiffers {}            -> []  -- a pinned field is among the fields, compared there

-- | What the spec says of a difference.
data Judgement
  = Allowed     -- ^ a value the spec allows the field
  | Unchecked   -- ^ a condition as printed that hsed does not evaluate: no failure, reported apart
  | NotAllowed  -- ^ a value the spec does not allow, or a field it allows no other
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A difference judged: allowed for a vendor unique value, any version, a
--   minimum the drive's value meets, or a condition as printed that hsed
--   evaluates and the drive's value meets; unchecked for one it does not
--   evaluate; not allowed otherwise.
judged :: FieldDiff -> Judgement
judged fd = case fdAllowance fd of
  Nothing              -> NotAllowed
  Just VendorUnique    -> Allowed
  Just (AnyVersion _)  -> Allowed
  Just (AsPrinted t)   -> maybe Unchecked (\holds -> if numberIs holds then Allowed else NotAllowed) (evaluated t)
  Just (Minimum least) -> if numberIs (>= least) then Allowed else NotAllowed
  where
    numberIs p = case fdDrive fd of
      Number v -> p v
      Octets _ -> False

-- | A condition as printed that hsed evaluates: \"As specified in Table 7\",
--   the SSC Minor Version Number of Opal 2.30 Table 6's bullets (PDF 23), one
--   of Table 7's values (PDF 24).
evaluated :: Text -> Maybe (Integer -> Bool)
evaluated t
  | t == "As specified in Table 7" = Just (`elem` map fst sscMinorVersions)
  | otherwise                      = Nothing

-- | Whether the spec allows a difference ('judged' 'Allowed').
allowed :: FieldDiff -> Bool
allowed = (== Allowed) . judged

-- | Whether a comparison fails: a code the drive reports that the simulator
--   does not, or a difference not allowed (one unchecked is no failure).
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
    , "judged"    A..= case judged fd of
        Allowed    -> "allowed" :: Text
        Unchecked  -> "unchecked"
        NotAllowed -> "not_allowed"
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
    , "unchecked"   A..= prUnchecked r
    , "differences" A..= prDiffs r
    , "fails"       A..= pairFails r
    ]
