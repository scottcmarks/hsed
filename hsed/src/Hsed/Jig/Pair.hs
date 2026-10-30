{-# LANGUAGE NoImplicitPrelude #-}

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

'pairLevel0' is milestone M3's group G3's; until it lands it is a stub.
-}
module Hsed.Jig.Pair
  ( FieldDiff (..)
  , PairReport (..)
  , pairLevel0
  ) where

import           RIO

import           Hsed.Core.Feature (Allowance)
import           Hsed.Core.Layout  (Content)
import           Hsed.Level0       (Level0)


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

-- | The simulator's response against the drive's.
pairLevel0 :: Level0 -> Level0 -> PairReport
pairLevel0 = error "G3: pairLevel0"
