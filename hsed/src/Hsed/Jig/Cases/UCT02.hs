{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.UCT02
Description : UCT-02, Properties
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

UCT-02 (Test Cases 1.02, PDF 31-32): Properties with the host's own
(MaxComPacketSize 4096, MaxPacketSize 4076, MaxIndTokenSize 4040), the
TPer's eleven properties each at least the Test Cases' minimum (step 2,
MaxTranactionLimit read as Core Table 167's MaxTransactionLimit,
DefSessionTimeout at least 0; a TPer property of 0 where Table 167 says 0
means no limit meets the minimum, said in an observation: the lead's M4a
decision, design 11), and
the host's three answered within their ranges (step 3). Both tables go into
the report's observations. The jig's extension on the simulator: the host's
values echoed exactly (Opal 2.30 Table 17's maxima being VU, the simulator
clamps none by default).

Step 2's eleven properties, 2a to 2k, are Opal 2.30 Table 17's rows in the
same order with the same minima (PDF 33-34), so the case reads them from
the generated 'propertyRequirements' and labels each by its place; Table
17's VU minimum for DefSessionTimeout is the Test Cases' 0. Step 3's lower
bounds are Table 17's least for the host, its upper bounds the values step
1 sent. Core Table 167 reads 0 as no limit for each of the eleven (PDF
141-142: MaxMethods under the asynchronous protocol, and the other ten each
by its own sentence; the two properties whose 0 means unsupported,
MinSessionTimeout and MinTransTimeout, are not among Table 17's rows).
-}
module Hsed.Jig.Cases.UCT02
  ( uct02
  , hostProperties
  , tperExpected
  ) where

import qualified RIO.List                 as L
import           RIO
import qualified RIO.Text                 as T

import           Hsed.Core.Properties     (HostRequirement (..), PropertyRequirement (..), Requirement (..), TPerRequirement (..),
                                           requirement)
import           Hsed.Gen.Opal.Properties (propertyRequirements)
import           Hsed.Jig.Case            (Case (..), Need (..))
import           Hsed.Jig.Catalogue       (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq          (Section (..), lockingSPUntouched, noSessionOpened)
import           Hsed.Jig.Script          (PropertiesAnswer (..), Script, Verdict (..), exchanged, observe, propertiesWith)
import           Hsed.Sim.Types           (maxComPacketSizeName, maxIndTokenSizeName, maxPacketSizeName)


-- | The case.
uct02 :: Case
uct02 = Case
  { cKey        = CaseKey (UCT 2) Nothing Nothing Nothing
  , cSection    = UseCases
  , cWaives     = lockingSPUntouched : noSessionOpened
  , cNeeds      = [NeedIfRecv, NeedIfSend]
  , cRun        = run
  , cExtensions = [("the host's properties echoed exactly: 4096, 4076, 4040 (Opal 2.30 Table 17's maxima are VU)", echoed)]
  }

-- | The host's properties step 1 sends (Test Cases PDF 32): MaxComPacketSize
--   4096, MaxPacketSize 4076 and MaxIndTokenSize 4040 bytes, by Core Table
--   167's names.
hostProperties :: [(Text, Natural)]
hostProperties = [(maxComPacketSizeName, 4096), (maxPacketSizeName, 4076), (maxIndTokenSizeName, 4040)]

-- | Step 2's expected TPer properties, each with its step (2a to 2k), its
--   name and the least value the Test Cases accept (PDF 32): Opal 2.30
--   Table 17's TPer rows, in order, each mandatory, its minimum (VU, 0:
--   DefSessionTimeout at least 0).
tperExpected :: [(Text, Text, Natural)]
tperExpected =
  [ ("2" <> T.singleton letter, reqName r, fromMaybe 0 (tprMinimum t))
  | (letter, r) <- zip ['a' ..] propertyRequirements, Just t <- [reqTPer r], tprRequired t == Mandatory ]

-- | Step 1, then the expected responses 2 and 3, every one checked; both
--   tables observed.
run :: Script Verdict
run = do
  answer <- propertiesWith (Just hostProperties) >>= exchanged "step 1: Properties with the host's properties"
  traverse_ (\(n, v) -> observe ("TPer " <> n) (tshow v)) (paTPer answer)
  traverse_ (\(n, v) -> observe ("host " <> n) (tshow v)) (fromMaybe [] (paHost answer))
  tperFailures <- concat <$> traverse (tperCheck (paTPer answer)) tperExpected
  pure $ case tperFailures <> hostChecks (paHost answer) of
    []       -> Pass
    failures -> Fail failures

-- | 2a-2k: a TPer property reported at least its minimum; one reported as
--   0, no limit (Core Table 167, PDF 141-142), meets the minimum, said in
--   an observation (the lead's M4a decision, design 11); one not reported
--   fails its step.
tperCheck :: [(Text, Integer)] -> (Text, Text, Natural) -> Script [Text]
tperCheck reported (step, name, least) = case L.lookup name reported of
  Nothing -> pure [step <> ": " <> name <> " not reported"]
  Just 0  -> [] <$ observe step (name <> " = 0 (no limit, Core Table 167); the Test Cases' minimum is met")
  Just v
    | v >= toInteger least -> pure []
    | otherwise            -> pure [step <> ": " <> name <> " = " <> tshow v <> ", less than " <> tshow least]

-- | 3a-3c: each host property the TPer answered within its range: at least
--   Opal 2.30 Table 17's least for the host (the generated requirement's
--   minimum), at most the value step 1 sent (Test Cases PDF 32: 2048 to
--   4096, 2028 to 4076, 1992 to 4040); no HostProperties answered fails
--   step 3.
hostChecks :: Maybe [(Text, Integer)] -> [Text]
hostChecks = \case
  Nothing       -> ["3: the TPer answered no HostProperties (Core 5.2.2.1.2.2, PDF 141)"]
  Just answered -> concat
    [ case L.lookup name answered of
        Nothing -> [step <> ": " <> name <> " not answered"]
        Just v
          | v < toInteger least -> [step <> ": " <> name <> " = " <> tshow v <> ", less than " <> tshow least]
          | v > toInteger sent  -> [step <> ": " <> name <> " = " <> tshow v <> ", more than the " <> tshow sent <> " sent"]
          | otherwise           -> []
    | (letter, (name, sent)) <- zip ['a' ..] hostProperties
    , let step  = "3" <> T.singleton letter
    , let least = fromMaybe 0 (requirement name propertyRequirements >>= reqHost >>= hrMinimum) ]

-- | The extension: the host's three values answered exactly as sent.
echoed :: Script Verdict
echoed = do
  answer <- propertiesWith (Just hostProperties) >>= exchanged "Properties with the host's properties"
  let answered = fromMaybe [] (paHost answer)
  pure $ case [ name <> " = " <> maybe "not answered" tshow (L.lookup name answered) <> ", not " <> tshow sent
              | (name, sent) <- hostProperties, L.lookup name answered /= Just (toInteger sent) ] of
    []  -> Pass
    off -> Fail off
