{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Jig.Cases.UCT01
Description : UCT-01, Level 0 Discovery
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

UCT-01 (Test Cases 1.02, PDF 29-31): one IF-RECV of Level 0
Discovery; the TPer and Locking descriptors, and, for Opal 2.00 to 2.30,
the Opal SSC V2, Additional DataStore Table and Block SID descriptors, each
with the values the case requires (steps 2 and 4); the base ComID recorded
for the cases after it. On a drive at M3 the run waives SyncIdle and
NoSessions for it, a case that needs Level 0 alone, which Level0Only cannot
observe (Core 3.3.10.3 b, PDF 84; 3.3.6.2, PDF 67).

Each descriptor is read leniently through its record (Core's TPer and
Locking features, Opal's T6, the hand-written feature sets): the case holds
it to the values the Test Cases name, not to the pins of the layouts, so a
Version other than Core's is no failure here. Where the case asks for a
value at least some number, the number is the one the spec allows the field
('Hsed.Core.Feature.featureAllowances': Opal 2.30's bullets after Table 6,
PDF 23, the same 1, 4 and 8 as step 4a; the DataStore minima of step 4b).
The case's expected responses for Opal 1.00, Opalite, Pyrite and Ruby
(steps 3 and 5 to 8, PDF 30-31) are not M3's: a response without the Opal
SSC V2 descriptor fails step 4. Step 4c applies where the Block SID
descriptor is reported (the SSC version the case conditions it on is the
vendor's statement, Test Cases 4.3, PDF 28, which Level 0 does not carry).
-}
module Hsed.Jig.Cases.UCT01
  ( uct01
  ) where

import           Numeric                (showHex)
import qualified RIO.List               as L
import           RIO
import qualified RIO.Text               as T

import           Hsed.Core.Feature      (Allowance (..), Feature (..))
import           Hsed.Core.Layout       (Content (..), Layout (..), LayoutError)
import           Hsed.Core.Record       (DecodedRecord (..), LayoutRecord (..))
import           Hsed.Gen.Core.Features (LockingFeature (..), TPerFeature (..))
import           Hsed.Gen.Opal.Features (Level0DiscoveryOpalSSCV2Feature (..))
import           Hsed.Jig.Case          (Case (..), Need (..))
import           Hsed.Jig.Catalogue     (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq        (Section (..), lockingSPUntouched, noSessionOpened)
import           Hsed.Jig.Script        (RunCtx (..), RunEnv (..), Script, Verdict (..), discover, featureIn)
import           Hsed.Level0            (Level0)
import           Hsed.Sim.FeatureSets   (BlockSIDFeature (..), DataStoreFeature)


-- | The case.
uct01 :: Case
uct01 = Case
  { cKey        = CaseKey (UCT 1) Nothing Nothing Nothing
  , cSection    = UseCases
  , cWaives     = lockingSPUntouched : noSessionOpened
  , cNeeds      = [NeedLevel0]
  , cRun        = run
  , cExtensions = []
  }

-- | Step 1 (discovery fails the case when the IF-RECV does not succeed),
--   then the expected responses 2 and 4, every one checked; the base ComID
--   recorded for the cases after it (the case's Notes, PDF 29).
run :: Script Verdict
run = do
  l0 <- discover
  case featureIn l0 of
    Just (Right d) -> asks reCtx >>= \ref ->
      modifyIORef' ref (\c -> c { rcBaseComId = Just (level0DiscoveryOpalSSCV2FeatureBaseComID (drRecord d)) })
    _ -> pure ()
  pure $ case concat [step2a l0, step2b l0, step4 l0] of
    []       -> Pass
    failures -> Fail failures

-- | 2a: the TPer Feature (0x0001), Streaming Supported and Sync Supported 1.
step2a :: Level0 -> [Text]
step2a l0 = described "2a" l0 $ \(r :: TPerFeature) ->
  [ "2a.ii: Streaming Supported = 0, not 1" | not (tPerFeatureStreamingSupported r) ]
  <> [ "2a.iii: Sync Supported = 0, not 1" | not (tPerFeatureSyncSupported r) ]

-- | 2b: the Locking Feature (0x0002), Media Encryption 1 (an Opal SSC's)
--   and Locking Supported 1.
step2b :: Level0 -> [Text]
step2b l0 = described "2b" l0 $ \(r :: LockingFeature) ->
  [ "2b.ii: Media Encryption = 0, not 1" | not (lockingFeatureMediaEncryption r) ]
  <> [ "2b.iv: Locking Supported = 0, not 1" | not (lockingFeatureLockingSupported r) ]

-- | 4: the Opal SSC V2 Feature (0x0203) with its minima (4a), the
--   Additional DataStore Table Feature (0x0202) with its (4b), and, where
--   reported, the Block SID Authentication Feature (0x0402) with SID Value
--   State 0 (4c).
step4 :: Level0 -> [Text]
step4 l0 = concat
  [ described "4a" l0 $ \(r :: Level0DiscoveryOpalSSCV2Feature) ->
      concatMap (atLeast "4a" r)
        [ "Number of ComIDs", "Number of Locking SP Admin Authorities Supported", "Number of Locking SP User Authorities Supported" ]
  , described "4b" l0 $ \(r :: DataStoreFeature) ->
      concatMap (atLeast "4b" r)
        [ "Maximum number of DataStore Tables", "Maximum total size of DataStore Tables", "DataStore Table size alignment" ]
  , case featureIn l0 of
      Nothing       -> []
      Just (Left e) -> [undecoded "4c" (Proxy :: Proxy BlockSIDFeature) e]
      Just (Right d)
        | blockSIDFeatureSIDValueState (drRecord d) -> ["4c.ii: SID Value State = 1, not 0"]
        | otherwise                                -> []
  ]

-- | A feature's descriptor in the response, decoded, and what its checks
--   find; the step fails where it is missing or does not decode.
described :: forall f. Feature f => Text -> Level0 -> (f -> [Text]) -> [Text]
described step l0 checks = case featureIn l0 of
  Nothing        -> [step <> ": no " <> nameOf p <> " (" <> codeOf p <> ") in the response"]
  Just (Left e)  -> [undecoded step p e]
  Just (Right d) -> checks (drRecord d)
  where p = Proxy :: Proxy f

undecoded :: Feature f => Text -> Proxy f -> LayoutError -> Text
undecoded step p e = step <> ": the " <> nameOf p <> " (" <> codeOf p <> ") does not decode: " <> tshow e

-- | A field at least the minimum the spec allows it, both by the field's
--   name in the layout.
atLeast :: forall f. Feature f => Text -> f -> Text -> [Text]
atLeast step r field = case (L.lookup field (recordFields r), L.lookup field (featureAllowances (Proxy :: Proxy f))) of
  (Just (Number v), Just (Minimum least))
    | v >= least -> []
    | otherwise  -> [step <> ": " <> field <> " = " <> tshow v <> ", less than " <> tshow least]
  _ -> [step <> ": " <> field <> ": no value, or no minimum the spec allows it"]

-- | A feature's name: its layout's, less the word Descriptor.
nameOf :: LayoutRecord f => Proxy f -> Text
nameOf p = fromMaybe name (T.stripSuffix " Descriptor" name)
  where name = layoutName (recordLayout p)

-- | A feature's code in hex.
codeOf :: Feature f => Proxy f -> Text
codeOf p = "0x" <> T.justifyRight 4 '0' (T.pack (showHex (featureCode p) ""))
