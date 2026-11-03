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
host sent, and the TPer's answer), compared property by property, equal
values no difference and every other difference judged by the drive's
value, as the Level 0 pair judges a field ("Hsed.Jig.Pair".'Judgement'),
against Opal 2.30 Table 17 (PDF 33-34: the generated
"Hsed.Gen.Opal.Properties".@propertyRequirements@, a property's row found
by its name, "Hsed.Core.Properties".'requirement').

* A TPer property (Core 5.2.2.1.2.1, PDF 140): at least its row's minimum,
  or 0, no limit (Core Table 167, PDF 141-142), allowed, and less not;
  DefSessionTimeout's VU any value; one the table makes mandatory, as it
  makes all eleven, that one end alone reports not allowed (UCT-02 step 2
  fails the end that lacks it); one outside the table unchecked; any other
  that one end alone reports listed, no failure (a TPer omits the
  properties it does not support and MAY add others, Core 5.2.2.2, PDF
  141).
* A host property as the TPer answered it, what the TPer will use for what
  it sends the host (Core 5.2.2.1.2.2, PDF 140-141): one the host sent, at
  least its row's minimum and at most the value sent (the table's host
  maxima are VU, so a TPer may answer less); where the host sent 0, no
  limit, at least the minimum, or 0; and where it sent less than the
  initial assumption, that assumption, which the TPer uses and reports in
  its place (Core 5.2.2.4, PDF 145). One the table makes mandatory that
  one end alone answers, not allowed: an Opal TPer accepts it, and answers
  every host property it supports whenever the host sends its own (Core
  5.2.2.3, PDF 144; UCT-02 step 3 fails the end that leaves out one it
  sent). MaxResponseComPacketSize's (N): left out by one end allowed (the
  TPer MAY ignore it and not list it, Table 17), its value unchecked. One
  the host did not send, answered differently, its value what the ComID
  keeps from earlier exchanges (5.2.2.3, PDF 143), unchecked; one outside
  the table unchecked.
* Not compared, failing nothing, where either end made no such exchange or
  the two sent different host properties.

Each difference names the rule that judged it and its page ('pdRule').
In each of Table 17's host rows that the TPer accepts the initial
assumption is the minimum and the maximum is VU, which the host rule reads
of the generated rows (the tests hold both).
-}
module Hsed.Jig.PairProperties
  ( Side (..)
  , PropDiff (..)
  , PropertiesPair (..)
  , pairProperties
  , propertiesFail
  , propertiesLines
    -- * Under other requirements
  , pairPropertiesWith
  ) where

import qualified Data.Aeson               as A
import qualified RIO.List                 as L
import           RIO
import qualified RIO.Text                 as T

import           Hsed.Core.Properties     (HostRequirement (..), PropertyRequirement (..), Requirement (..),
                                           TPerRequirement (..), requirement)
import           Hsed.Gen.Opal.Properties (propertyRequirements)
import           Hsed.Jig.Pair            (Judgement (..))
import           Hsed.Jig.Script          (PropertiesAnswer (..))


-- | Which of Properties' tables a property is in: the TPer's, or the
--   host's as the TPer answered them (Core 5.2.2.1, PDF 140).
data Side
  = TPerSide
  | HostSide
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A property whose values differ between the ends: its name and table,
--   the simulator's value and the drive's (none where an end did not
--   report or answer it), the rule that judged the drive's and its page,
--   and the judgement.
data PropDiff = PropDiff
  { pdName   :: !Text
  , pdSide   :: !Side
  , pdSim    :: !(Maybe Integer)
  , pdDrive  :: !(Maybe Integer)
  , pdRule   :: !Text       -- ^ @TPer minimum 2028 (Opal 2.30 Table 17, PDF 33-34)@, @host within [2048, 4096 sent] (…)@
  , pdJudged :: !Judgement
  }
  deriving (Eq, Show)

-- | The comparison: not made, and why; or the differences, each judged,
--   then the TPer properties the simulator alone reports and those the
--   drive alone does, which fail nothing (Core 5.2.2.2, PDF 141).
data PropertiesPair
  = NotCompared !Text
  | Compared
      { ppDiffs     :: ![PropDiff]
      , ppSimOnly   :: ![Text]
      , ppDriveOnly :: ![Text]
      }
  deriving (Eq, Show)

-- | The simulator's exchange against the drive's, each the host properties
--   sent and the TPer's answer, judged by Opal 2.30 Table 17 as generated
--   (PDF 33-34, 'propertyRequirements').
pairProperties :: Maybe ([(Text, Natural)], PropertiesAnswer) -> Maybe ([(Text, Natural)], PropertiesAnswer) -> PropertiesPair
pairProperties = pairPropertiesWith propertyRequirements

-- | 'pairProperties' under the requirements given, each property's found by
--   its name ('requirement'): the ends compared where both made the exchange
--   with the same host properties, in any order, and otherwise not, saying
--   why.
pairPropertiesWith :: [PropertyRequirement] -> Maybe ([(Text, Natural)], PropertiesAnswer) -> Maybe ([(Text, Natural)], PropertiesAnswer)
                   -> PropertiesPair
pairPropertiesWith reqs sim drive = case (sim, drive) of
  (Just (sent, s), Just (sent', d))
    | L.sort sent == L.sort sent' -> compareAnswers reqs sent s d
    | otherwise                   -> NotCompared ("the ends sent different host properties: the simulator " <> namedText sent
                                                  <> ", the drive " <> namedText sent')
  (Just _, Nothing)  -> NotCompared (noExchange <> " on the drive, only on the simulator")
  (Nothing, Just _)  -> NotCompared (noExchange <> " on the simulator, only on the drive")
  (Nothing, Nothing) -> NotCompared (noExchange <> " on either end")
  where
    noExchange = "no Properties with the host's properties (UCT-02 step 1)"

-- | Whether the comparison fails the run: a difference judged not allowed.
propertiesFail :: PropertiesPair -> Bool
propertiesFail = \case
  NotCompared _   -> False
  Compared ds _ _ -> any ((== NotAllowed) . pdJudged) ds

-- | The comparison as the reports print it: why it was not made; or a line
--   a difference (its table, name, the two values, the judgement and the
--   rule), then the TPer properties one end alone reports; or that nothing
--   differs.
propertiesLines :: PropertiesPair -> [Text]
propertiesLines = \case
  NotCompared why   -> ["not compared: " <> why]
  Compared [] [] [] -> ["no differences"]
  Compared ds simOnly driveOnly ->
    map diffLine ds
    <> [ "TPer properties the simulator alone reports, no failure " <> core5222 <> ": " <> T.intercalate ", " simOnly | not (null simOnly) ]
    <> [ "TPer properties the drive alone reports, no failure " <> core5222 <> ": " <> T.intercalate ", " driveOnly | not (null driveOnly) ]
  where
    diffLine d = sideText (pdSide d) <> " " <> pdName d <> ": simulator " <> valueText (pdSim d) <> ", drive " <> valueText (pdDrive d)
                 <> ", " <> judgementText (pdJudged d) <> ": " <> pdRule d
    sideText = \case
      TPerSide -> "TPer"
      HostSide -> "host"
    valueText = maybe "none" tshow
    judgementText = \case
      Allowed    -> "allowed"
      Unchecked  -> "unchecked"
      NotAllowed -> "not allowed"
    core5222 = "(Core 5.2.2.2, PDF 141)"


-- * The rules

-- | Two answers to the same host properties: the TPer's properties that
--   differ, each judged or, one end alone reporting it, listed; then the
--   host's that differ, each judged.
compareAnswers :: [PropertyRequirement] -> [(Text, Natural)] -> PropertiesAnswer -> PropertiesAnswer -> PropertiesPair
compareAnswers reqs sent sim drive = Compared
  { ppDiffs     = [ PropDiff n TPerSide sv dv rule j | (n, sv, dv, Just (rule, j)) <- tper ]
                  <> [ PropDiff n HostSide sv dv rule j
                     | (n, sv, dv) <- differing (hostOf sim) (hostOf drive)
                     , let (rule, j) = hostRule (row n) (L.lookup n sent) sv dv ]
  , ppSimOnly   = [ n | (n, Just _, Nothing, Nothing) <- tper ]
  , ppDriveOnly = [ n | (n, Nothing, Just _, Nothing) <- tper ]
  }
  where
    row n  = requirement n reqs
    tper   = [ (n, sv, dv, tperRule (row n >>= reqTPer) sv dv) | (n, sv, dv) <- differing (paTPer sim) (paTPer drive) ]
    hostOf = fromMaybe [] . paHost

-- | The names two lists give, in the order they first appear (the first
--   list's, then the second's), each with its value in either (none where
--   that list lacks it; a name given twice read at its first), where the
--   two values differ.
differing :: [(Text, Integer)] -> [(Text, Integer)] -> [(Text, Maybe Integer, Maybe Integer)]
differing a b = [ (n, va, vb) | n <- L.nub (map fst (a <> b)), let va = L.lookup n a, let vb = L.lookup n b, va /= vb ]

-- | A TPer property's difference judged by the drive's value, with the rule
--   that judged it, given the property's row of Table 17 if it has one: at
--   least the row's minimum, or 0, no limit (Core Table 167, PDF 141-142);
--   any value of a VU row; a property the row makes mandatory that an end
--   lacks not allowed; one outside the table unchecked; and none for any
--   other that one end alone reports, which is listed (Core 5.2.2.2, PDF
--   141).
tperRule :: Maybe TPerRequirement -> Maybe Integer -> Maybe Integer -> Maybe (Text, Judgement)
tperRule req sv dv = case (req, sv, dv) of
  (Just t, Just _, Just v)                     -> Just (valueRule (tprMinimum t) v)
  (Just t, _, _) | tprRequired t == Mandatory -> Just ("TPer (M), which every Opal TPer reports " <> table17, NotAllowed)
  (Nothing, Just _, Just _)                    -> Just ("TPer not in Opal 2.30 Table 17 (PDF 33-34)", Unchecked)
  _                                            -> Nothing
  where
    valueRule least v = case least of
      Nothing -> ("TPer VU " <> table17, Allowed)
      Just m
        | v == 0           -> ("TPer minimum " <> tshow m <> ", 0 no limit (Opal 2.30 Table 17, PDF 33-34; Core Table 167, PDF 141-142)", Allowed)
        | v >= toInteger m -> ("TPer minimum " <> tshow m <> " " <> table17, Allowed)
        | otherwise        -> ("TPer minimum " <> tshow m <> " " <> table17, NotAllowed)

-- | A host property's difference judged by the drive's answer, with the
--   rule that judged it, given the property's row of Table 17 if it has one
--   and the value the host sent if it sent the property: a value sent
--   answered as 'sentRule' has it; a row (N), no value required, left out
--   allowed (a TPer MAY ignore it and not list it, Table 17); one not sent
--   unchecked, its value what the ComID keeps from earlier exchanges (Core
--   5.2.2.3, PDF 143); a row (M) left out not allowed, an Opal TPer
--   answering every host property it supports (Core 5.2.2.3, PDF 144); a
--   row (O) left out allowed; a row whose host column requires nothing (N/A,
--   not a host property: MaxSessions and the TPer's other own) unchecked,
--   and one outside the table unchecked, each saying which.
hostRule :: Maybe PropertyRequirement -> Maybe Natural -> Maybe Integer -> Maybe Integer -> (Text, Judgement)
hostRule row sent sv dv = case reqHost <$> row of
  Nothing      -> ("host not in Opal 2.30 Table 17 (PDF 33-34)", Unchecked)
  Just Nothing -> ("no host requirement in Opal 2.30 Table 17, its host column N/A, not a host property (PDF 33-34)", Unchecked)
  Just (Just h) -> case (hrRequired h, sent, sv, dv) of
    (NotRequired, _, Just _, Just _) -> ("host (N), no value required " <> table17, Unchecked)
    (NotRequired, _, _, _)           -> ("host (N), which a TPer MAY leave out of its answer " <> table17, Allowed)
    (_, Just s, Just _, Just v)      -> sentRule h s v
    (_, Nothing, Just _, Just _)     -> ("host not sent, its value what the ComID keeps (Core 5.2.2.3, PDF 143)", Unchecked)
    (Mandatory, _, _, _)             -> ("host (M), which an Opal TPer answers whenever the host sends its own"
                                         <> " (Core 5.2.2.3, PDF 144; Opal 2.30 Table 17, PDF 33-34)", NotAllowed)
    (Optional, _, _, _)              -> ("host (O), which a TPer need not support " <> table17, Allowed)

-- | A host property the host sent, as the drive answered it: at least the
--   row's minimum and at most the value sent (the row's maximum VU); where
--   0 was sent, no limit (Core Table 167, PDF 141-142), at least the
--   minimum, or 0; and where less than the row's initial assumption was
--   sent, that assumption, which the TPer uses and reports in its place
--   (Core 5.2.2.4, PDF 145).
sentRule :: HostRequirement -> Natural -> Integer -> (Text, Judgement)
sentRule h s v
  | s == 0      = ( "host at least " <> tshow least <> ", or 0, the 0 sent no limit (Opal 2.30 Table 17, PDF 33-34; Core Table 167, PDF 141-142)"
                  , judge (v == 0 || v >= toInteger least) )
  | s < initial = ( "host " <> tshow initial <> ", the initial assumption, in place of the " <> tshow s <> " sent below it"
                    <> " (Core 5.2.2.4, PDF 145; Opal 2.30 Table 17, PDF 33-34)"
                  , judge (v == toInteger initial) )
  | otherwise   = ( "host within [" <> tshow least <> ", " <> tshow s <> " sent] " <> table17
                  , judge (v >= toInteger least && v <= toInteger s) )
  where
    least   = fromMaybe 0 (hrMinimum h)
    initial = fromMaybe least (hrInitial h)
    judge ok = if ok then Allowed else NotAllowed

-- | Table 17's citation.
table17 :: Text
table17 = "(Opal 2.30 Table 17, PDF 33-34)"

-- | Named values as a reason prints them: @[MaxComPacketSize = 4096, MaxPacketSize = 4076]@.
namedText :: [(Text, Natural)] -> Text
namedText nvs = "[" <> T.intercalate ", " [ n <> " = " <> tshow v | (n, v) <- nvs ] <> "]"


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
