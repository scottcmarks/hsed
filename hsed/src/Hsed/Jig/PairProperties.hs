{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.PairProperties
Description : The Pair's Properties: the simulator's UCT-02 exchange against the drive's, judged by Opal 2.30 Table 17
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Pair runner's second comparison (design 13.32 (6), M4c; the first is
Level 0's, "Hsed.Jig.Pair"): each end's Properties exchange with the
host's properties (UCT-02's, "Hsed.Jig.Script".@rcNegotiated@: what the
host sent, and the TPer's answer), the drive's values judged by Opal 2.30
Table 17's requirements (PDF 33-34, the generated
"Hsed.Gen.Opal.Properties".@propertyRequirements@): a TPer property at
least its minimum, or 0, no limit (Core Table 167, PDF 141-142); the VU
DefSessionTimeout any value; a host property answered within its minimum
and the value sent (Table 17's host maxima are VU, so a TPer may clamp); a
mandatory one missing a failure; one only an end reports listed, no failure
(Core 5.2.2.2, PDF 141); and, where either end ran no such exchange or the
two sent different host properties, not compared, no failure. Each
difference is judged as the Level 0 pair judges a field ("Hsed.Jig.Pair".'Judgement').

At M4c's scaffold the types are final and the comparison is group G4's:
'pairProperties' compares nothing yet ('NotCompared', naming G4),
'propertiesFail' fails nothing and 'propertiesLines' prints nothing.
-}
module Hsed.Jig.PairProperties
  ( Side (..)
  , PropDiff (..)
  , PropertiesPair (..)
  , pairProperties
  , propertiesFail
  , propertiesLines
  ) where

import qualified Data.Aeson      as A
import           RIO

import           Hsed.Jig.Pair   (Judgement (..))
import           Hsed.Jig.Script (PropertiesAnswer)


-- | Which of Properties' tables a property is in: the TPer's, or the
--   host's as the TPer answered them (Core 5.2.2.1, PDF 140).
data Side
  = TPerSide
  | HostSide
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A property whose values differ between the ends: its name and table,
--   the simulator's value and the drive's (none where an end did not
--   report it), the rule that judged the drive's and its page, and the
--   judgement.
data PropDiff = PropDiff
  { pdName   :: !Text
  , pdSide   :: !Side
  , pdSim    :: !(Maybe Integer)
  , pdDrive  :: !(Maybe Integer)
  , pdRule   :: !Text       -- ^ @TPer minimum 2028 (Opal 2.30 Table 17, PDF 33)@, @host within [2048, 4096 sent]@
  , pdJudged :: !Judgement
  }
  deriving (Eq, Show)

-- | The comparison: not made, and why; or the differences, then the
--   properties the simulator alone reported and those the drive alone did.
data PropertiesPair
  = NotCompared !Text
  | Compared
      { ppDiffs     :: ![PropDiff]
      , ppSimOnly   :: ![Text]
      , ppDriveOnly :: ![Text]
      }
  deriving (Eq, Show)

-- | The simulator's exchange against the drive's, each the host properties
--   sent and the TPer's answer. Group G4's: not compared until then.
pairProperties :: Maybe ([(Text, Natural)], PropertiesAnswer) -> Maybe ([(Text, Natural)], PropertiesAnswer) -> PropertiesPair
pairProperties _ _ = NotCompared "G4: the Pair's Properties, judged by Opal 2.30 Table 17 (design 13.32 (6))"

-- | Whether the comparison fails the run: a difference judged not allowed.
--   Group G4's: none until then.
propertiesFail :: PropertiesPair -> Bool
propertiesFail _ = False

-- | The comparison as the reports print it, a line a difference, then the
--   one-sided lists. Group G4's: nothing until then.
propertiesLines :: PropertiesPair -> [Text]
propertiesLines _ = []


-- * JSON

-- | @{"compared", "diffs", "sim_only", "drive_only", "reason"}@, the reason
--   null where the ends were compared.
instance A.ToJSON PropertiesPair where
  toJSON = \case
    NotCompared why -> A.object
      [ "compared" A..= False, "diffs" A..= ([] :: [PropDiff]), "sim_only" A..= ([] :: [Text]), "drive_only" A..= ([] :: [Text])
      , "reason" A..= why ]
    Compared ds simOnly driveOnly -> A.object
      [ "compared" A..= True, "diffs" A..= ds, "sim_only" A..= simOnly, "drive_only" A..= driveOnly
      , "reason" A..= (Nothing :: Maybe Text) ]

-- | @{"name", "side", "sim", "drive", "rule", "judged"}@, as the Level 0
--   pair prints a field's judgement.
instance A.ToJSON PropDiff where
  toJSON d = A.object
    [ "name"   A..= pdName d
    , "side"   A..= case pdSide d of
        TPerSide -> "tper" :: Text
        HostSide -> "host"
    , "sim"    A..= pdSim d
    , "drive"  A..= pdDrive d
    , "rule"   A..= pdRule d
    , "judged" A..= case pdJudged d of
        Allowed    -> "allowed" :: Text
        Unchecked  -> "unchecked"
        NotAllowed -> "not_allowed"
    ]
