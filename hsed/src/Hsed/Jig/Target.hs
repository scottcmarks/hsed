{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

'openTarget' is milestone M3's group G3's; until it lands it is a stub.
-}
module Hsed.Jig.Target
  ( TargetSpec (..)
  , parseTarget
  , Target (..)
  , openTarget
  ) where

import           RIO

import           Hsed.Jig.Options      (RunOptions, TargetSpec (..), parseTarget)
import           Hsed.Transport.Drive  (DriveInfo)
import           Hsed.Transport.InProc (SimControl)
import           Hsed.Wire             (Cap (..), Transport)


-- | An opened target.
data Target
  = SimTarget !(Transport 'ReadWrite) !SimControl
  | DriveTarget !(Transport 'ReadOnly) !(Maybe DriveInfo)  -- ^ a drive (with what the IORegistry says of it) or a replay
  | PairTarget !Target !Target                            -- ^ the simulator, then the drive

-- | Open a run's target, or why not.
openTarget :: RunOptions -> IO (Either Text Target)
openTarget = error "G3: openTarget"
