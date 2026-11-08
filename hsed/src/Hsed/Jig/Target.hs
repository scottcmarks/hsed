{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Target
Description : A run's target opened: the simulator's transport and control, a drive's, or both
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The transports behind a 'TargetSpec' (design 9.2): the simulator in process
(@'ReadWrite@, with its control); a drive through the kext (@'ReadOnly@,
root required) or a replay of a captured response (@'ReadOnly@); the two
together for the Pair runner; or, since M5c, the simulator served by
hsed-sim's hub over mh_transport frames on 127.0.0.1 (@'ReadWrite@: the LAN
leg's client, "Hsed.Transport.Mhtx.Client", its view and control the hub's
peek and release ops, Option A). Only the transport differs between
targets. A hub is held to the run when it opens ('hubRefusals'): it allows
the peeks the run's reads of the simulator take, and the setup it publishes
is the one the run's simulator flags ask for, which on a @ws:\/\/@ target
assert the hub's setup and choose nothing; else it is closed again and the
run refused (exit 1), each reason named.

'openTarget' puts a drive behind the Guard the moment it is opened
("Hsed.Jig.Guard".'GuardedDrive', design 13.29 (l)): the opener's
'RawDrive' wrapped at once, every command then decided under the drive's
policy, Level0Only until the run binds the drive's base ComID with what
the run admits on it (the allowed list given), and recorded into the run's
transcript (given) and the drive's own; a replay binds nothing, answering
Level 0 Discovery alone. This module alone imports the kext transport and
the replay ("Hsed.Transport.MacKext", "Hsed.Transport.Replay"), so that no
drive reaches a run unguarded; the simulator's transport is opened as it
is, and the run records it. A drive needs root, hsed's own gate (the
kext's build 625 checks no privilege, design 9.3), and the serial it must
have (@--expect-serial@): "Hsed.Jig.Cli" refuses a drive target without
either before the run starts, and 'openTarget' refuses again.
-}
module Hsed.Jig.Target
  ( TargetSpec (..)
  , parseTarget
  , Target (..)
  , openTarget
  , hubRefusals
  , closeTarget
  ) where

import           RIO
import qualified RIO.Text              as T

import           Hsed.Jig.Files        (runningAsRoot)
import           Hsed.Jig.Guard        (Allowed, GuardedDrive, driveClose, guardDrive)
import           Hsed.Jig.Options      (RunOptions (..), TargetSpec (..), WsUrl (..), parseTarget)
import           Hsed.Jig.Simulator    (Opened (..), Simulator (..), inProcess)
import           Hsed.Jig.Transcript   (Transcript)
import           Hsed.Sim.Types        (SimParams (..), foldProblemText, simParamsProblems)
import           Hsed.Sim.View         (setupDifferences, setupOf)
import           Hsed.Transport.Drive  (RawDrive (..))
import           Hsed.Transport.InProc (InProcOpts (..), newInProc)
import           Hsed.Transport.MacKext (KextOpts (..), openKext)
import           Hsed.Transport.Mhtx.Client (clientApp, clientControl, clientNotices, clientOpts, clientSettled, clientTransport,
                                             clientView, closeMhtx, openMhtx)
import           Hsed.Transport.Mhtx.Frame (Allow (..), SedApp (..))
import           Hsed.Transport.Replay (openReplay)
import           Hsed.Wire             (Cap (..), Transport, closeTransport)


-- | An opened target.
data Target
  = SimTarget !(Transport 'ReadWrite) !Simulator  -- ^ the simulator's transport, and the simulator as the jig reads it
                                                --   besides ("Hsed.Jig.Simulator": in process, or over the LAN leg)
  | DriveTarget !GuardedDrive   -- ^ a drive (with what the IORegistry says of it) or a replay, behind the Guard
  | PairTarget !Target !Target  -- ^ the simulator, then the drive

-- | Open a run's target, or why not: the simulator with the run's
--   parameters (refused out of range, or where its SPs' preconfiguration
--   does not fold with them), units and hold; a replay of a file; a drive by its BSD name,
--   held to the serial the run expects (which it must name), as root alone;
--   both ends of a pair, the first closed again when the second cannot be
--   opened; a hub's simulator at @ws:\/\/127.0.0.1:PORT\/PATH@, its client
--   opened by one attempt ("Hsed.Transport.Mhtx.Client".'openMhtx': a hello
--   of a TPer within its wait), then held to the run ('hubRefusals'), and
--   closed again if it is not. A drive or a replay is put behind the Guard at once, recording
--   into the run's transcript, its policy Level0Only until the run binds its
--   base ComID with what the allowed list admits (a replay's bind refuses).
openTarget :: RunOptions -> [Allowed] -> Transcript -> IO (Either Text Target)
openTarget opts admits t = open (roTarget opts)
  where
    open = \case
      TSim -> case simParamsProblems sim of
        []       -> newInProc sim (InProcOpts (spUnits sim) (roHold opts)) <&> \case
          Right (tr, control) -> Right (SimTarget tr (inProcess control))
          Left folds          -> Left ("the simulator's SPs do not fold: " <> T.intercalate "; " (map foldProblemText folds))
        problems -> pure (Left ("the simulator's parameters: " <> T.intercalate "; " problems))
        where sim = roSim opts
      TReplay file -> openReplay file >>= traverse (\tr -> DriveTarget <$> guardDrive t admits Nothing (RawDrive tr replayBind))
      TDevice dev -> do
        root <- runningAsRoot
        case roExpectSerial opts of
          _ | not root -> pure (Left (dev <> " needs root (sudo): hsed's own gate, the kext checking no privilege (design 9.3)"))
          Nothing      -> pure (Left (dev <> " needs --expect-serial SERIAL: a drive is opened only as the one named"))
          Just serial  -> openKext (KextOpts (bsdName dev) serial)
                            >>= traverse (\(raw, di) -> DriveTarget <$> guardDrive t admits (Just di) raw)
      TPair a b -> open a >>= \case
        Left e   -> pure (Left e)
        Right ta -> open b >>= \case
          Left e   -> Left e <$ closeTarget ta
          Right tb -> pure (Right (PairTarget ta tb))
      TWs u -> openMhtx (clientOpts (wsPort u) (wsPath u)) >>= \case
        Left why -> pure (Left why)
        Right c  -> case hubRefusals opts (clientApp c) of
          []      -> pure (Right (SimTarget (clientTransport c) Simulator
                       { simOpened      = OverWs u
                       , simView        = clientView c
                       , simControl     = clientControl c
                       , simNotices     = clientSettled c >> clientNotices c
                       }))
          refusals -> Left (T.intercalate "; " refusals) <$ closeMhtx c
    bsdName dev = fromMaybe dev (T.stripPrefix "/dev/" dev)
    -- a replay carries Level 0 Discovery alone, so binds no base ComID
    replayBind _ = pure (Left "a replay answers Level 0 Discovery alone (Hsed.Transport.Replay)")

-- | Why a @ws:\/\/@ target's hub cannot serve the run, each reason (none:
--   it can): it allows no peek (hsed-sim without @--allow-remote-control@),
--   which the run's reads of the simulator take (design 11 M5c, Option A);
--   it publishes no setup, so that the run's simulator flags cannot be held
--   to it; its setup is not the one the run's simulator flags and hold ask
--   for, each difference named by its flag ("Hsed.Sim.View".'setupDifferences',
--   the profile already read into the run's parameters). A hub that allows
--   no reset serves the run, whose power cycles it answers unsupported.
hubRefusals :: RunOptions -> SedApp -> [Text]
hubRefusals opts app =
  [ "the hub allows no peek: hsed-sim --allow-remote-control, which the run's reads of the simulator take (design 11 M5c, Option A)"
  | AllowPeek `notElem` saAllows app ]
  <> maybe ["the hub publishes no setup (app.sed.setup): the run's simulator flags cannot be held to it"]
           (map ("the hub's setup differs: " <>) . setupDifferences (setupOf (roSim opts) (roHold opts)))
           (saSetup app)

-- | Close a target's transports (a hub's client among them: its transport
--   closes it).
closeTarget :: Target -> IO ()
closeTarget = \case
  SimTarget tr _ -> closeTransport tr
  DriveTarget g  -> driveClose g
  PairTarget a b -> closeTarget a `finally` closeTarget b
