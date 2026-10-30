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
root required) or a replay of a captured response (@'ReadOnly@), every
command of either recorded and guarded by Level0Only at M3; or the two
together for the Pair runner. Only the transport differs between targets.

'openTarget' opens the transports as they are; the run ("Hsed.Jig.Run")
puts its transcript and, on a drive, the Guard in front of each before any
command is issued. A drive needs root, hsed's own gate (the kext's build 625
checks no privilege, design 9.3), and the serial it must have
(@--expect-serial@): "Hsed.Jig.Cli" refuses a drive target without either
before the run starts, and 'openTarget' refuses again.
-}
module Hsed.Jig.Target
  ( TargetSpec (..)
  , parseTarget
  , Target (..)
  , openTarget
  , closeTarget
  ) where

import           RIO
import qualified RIO.Text              as T

import           Hsed.Jig.Files        (runningAsRoot)
import           Hsed.Jig.Options      (RunOptions (..), TargetSpec (..), parseTarget)
import           Hsed.Sim.Types        (SimParams (..))
import           Hsed.Transport.Drive  (DriveInfo)
import           Hsed.Transport.InProc (InProcOpts (..), SimControl, newInProc)
import           Hsed.Transport.MacKext (KextOpts (..), openKext)
import           Hsed.Transport.Replay (openReplay)
import           Hsed.Wire             (Cap (..), Transport, closeTransport)


-- | An opened target.
data Target
  = SimTarget !(Transport 'ReadWrite) !SimControl
  | DriveTarget !(Transport 'ReadOnly) !(Maybe DriveInfo)  -- ^ a drive (with what the IORegistry says of it) or a replay
  | PairTarget !Target !Target                            -- ^ the simulator, then the drive

-- | Open a run's target, or why not: the simulator with the run's
--   parameters, units and hold; a replay of a file; a drive by its BSD name,
--   held to the serial the run expects (which it must name), as root alone;
--   both ends of a pair, the first closed again when the second cannot be
--   opened.
openTarget :: RunOptions -> IO (Either Text Target)
openTarget opts = open (roTarget opts)
  where
    open = \case
      TSim -> do
        let sim = roSim opts
        Right . uncurry SimTarget <$> newInProc sim (InProcOpts (spUnits sim) (roHold opts))
      TReplay file -> fmap (`DriveTarget` Nothing) <$> openReplay file
      TDevice dev -> do
        root <- runningAsRoot
        case roExpectSerial opts of
          _ | not root -> pure (Left (dev <> " needs root (sudo): hsed's own gate, the kext checking no privilege (design 9.3)"))
          Nothing      -> pure (Left (dev <> " needs --expect-serial SERIAL: a drive is opened only as the one named"))
          Just serial  -> fmap (\(tr, di) -> DriveTarget tr (Just di)) <$> openKext (KextOpts (bsdName dev) serial)
      TPair a b -> open a >>= \case
        Left e   -> pure (Left e)
        Right ta -> open b >>= \case
          Left e   -> Left e <$ closeTarget ta
          Right tb -> pure (Right (PairTarget ta tb))
    bsdName dev = fromMaybe dev (T.stripPrefix "/dev/" dev)

-- | Close a target's transports.
closeTarget :: Target -> IO ()
closeTarget = \case
  SimTarget tr _   -> closeTransport tr
  DriveTarget tr _ -> closeTransport tr
  PairTarget a b   -> closeTarget a `finally` closeTarget b
