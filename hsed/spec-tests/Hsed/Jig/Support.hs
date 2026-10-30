{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Support
Description : What the jig's tests share: pending on other groups' stubs, a run silenced, a report's JSON read
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The jig is milestone M3's group G3's, written against the interfaces of the
other groups' code (the wire and the simulator, G1; Level 0, the feature
sets, the Session Manager and the profile, G2; the replay and the kext,
G4), which on G3's branch are stubs, @error "Gn: name"@. An example that
needs them pends ('needing') while a probe of what it needs throws such a
stub's error, naming the stubs, and runs once they are merged; any other
error is the example's failure. 'silently' keeps a run's printing out of
the test's output; 'at' reads into a report's JSON.
-}
module Hsed.Jig.Support
  ( -- * Pending on other groups
    needing
  , g1
  , g2
  , g4
    -- * Running quietly
  , silently
    -- * Reports
  , at
  , atPath
  , disk6Like
  ) where

import           Control.Exception        (ErrorCall (..))
import qualified Data.Aeson               as A
import qualified Data.Aeson.Key           as K
import qualified Data.Aeson.KeyMap        as KM
import           GHC.IO.Handle            (hDuplicate, hDuplicateTo)
import qualified RIO.List                 as L
import           RIO
import qualified RIO.Text                 as T
import           Test.Hspec

import           Hsed.Core.Method         (invocation)
import qualified Hsed.Gen.Core.Methods    as Methods
import           Hsed.Gen.Core.UIDs       (uSMUID)
import           Hsed.Level0              (Descriptor (..), decodeDescriptor, splitLevel0)
import           Hsed.Level0.Sedutil      (parseQuery, synthesize)
import           Hsed.Sim.Level0          (simDescriptors)
import           Hsed.Sim.Profile         (loadProfile)
import           Hsed.Sim.SessionManager  (answer)
import           Hsed.Sim.Sync            (t45Rows)
import           Hsed.Sim.Types           (defaultSimParams, initialState)
import           Hsed.Transport.InProc    (InProcOpts (..), newInProc)
import           Hsed.Transport.Replay    (openReplay)
import           Hsed.Wire                (Unit (..), ifRecv, roundUp, transferLength)


-- * Pending on other groups

-- | Run an example once what it needs is merged: pend while a probe throws
--   a group's stub error, naming the stubs.
needing :: [[IO ()]] -> Expectation -> Expectation
needing probes ex = do
  stubs <- catMaybes <$> traverse stubbed (concat probes)
  if null stubs then ex else pendingWith ("until " <> L.intercalate " and " (L.nub (map (L.takeWhile (/= ':')) stubs))
                                          <> " merge: " <> L.intercalate "; " (L.nub stubs))

-- | A probe's stub error, if it throws one; any other error rethrown.
stubbed :: IO () -> IO (Maybe String)
stubbed probe = (Nothing <$ probe) `catch` \e@(ErrorCall m) ->
  if any (`L.isPrefixOf` m) ["G1:", "G2:", "G4:"] then pure (Just m) else throwIO e

-- | G1's: the units, Table 45's rows, the in-process transport and a step of
--   the simulator.
g1 :: [IO ()]
g1 =
  [ void (evaluate (roundUp UnitBlocks512 1 + transferLength UnitBytes 1))
  , void (evaluate (length t45Rows))
  , newInProc defaultSimParams (InProcOpts UnitBytes 0) >>= \(tr, _) -> void (ifRecv tr 1 0x1004 2048 >>= evaluate)
  ]

-- | G2's: the simulator's descriptors, splitting and decoding Level 0,
--   sedutil's query, the profile, and the Session Manager's Properties.
g2 :: [IO ()]
g2 =
  [ void (evaluate (length (simDescriptors (initialState defaultSimParams))))
  , void (evaluate (splitLevel0 mempty))
  , void (evaluate (decodeDescriptor (Descriptor 1 1 mempty)))
  , void (evaluate (parseQuery mempty))
  , void (loadProfile "/nonexistent/hsed-profile.bin")
  , unless (isJust (answer defaultSimParams (invocation uSMUID (Methods.Properties Nothing))))
      (throwIO (ErrorCall "G2: answer (SMUID.Properties answered with nothing)"))
  ]

-- | G4's: the replay transport.
g4 :: [IO ()]
g4 = [ void (openReplay "/nonexistent/hsed-replay.bin") ]


-- * Running quietly

-- | An action with what it prints to stdout and stderr thrown away.
silently :: IO a -> IO a
silently act = do
  hFlush stdout
  hFlush stderr
  withFile "/dev/null" WriteMode $ \devNull ->
    bracket ((,) <$> hDuplicate stdout <*> hDuplicate stderr)
            (\(out, err) -> do
               hFlush stdout >> hFlush stderr
               hDuplicateTo out stdout >> hDuplicateTo err stderr
               hClose out >> hClose err)
            (\_ -> hDuplicateTo devNull stdout >> hDuplicateTo devNull stderr >> act)


-- * Reports

-- | A field of a JSON object.
at :: Text -> A.Value -> Maybe A.Value
at k = \v -> case v of
  A.Object o -> KM.lookup (K.fromText k) o
  _          -> Nothing

-- | Fields one inside the other.
atPath :: [Text] -> A.Value -> Maybe A.Value
atPath ks v = foldM (flip at) v ks

-- | A drive-like Level 0 response, synthesized from what sedutil printed of
--   disk6 (spec/fixtures/disk6-query.txt; tests run in hsed/).
disk6Like :: IO ByteString
disk6Like = do
  query <- readFileUtf8 "../spec/fixtures/disk6-query.txt"
  either (throwString . T.unpack) (pure . synthesize) (parseQuery query)
