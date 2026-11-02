{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Preflight
Description : A drive's mount pre-flight: what is mounted on it, and whether a run that may send to it goes ahead
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Before a run that may issue IF-SENDs to a drive (its drive cases need more
than Level 0 Discovery), what the system has mounted on the drive (design
9.3 item 4): @/sbin/mount@'s lines read ('readMounts', 'parseMounts': the
device, the mount point between @ on @ and the last @ (@, the file system's
type), those on the whole disk and its slices alone ('mountsOn': @disk22@
and @disk22s1@, never @disk220s1@), and the gate ('mountGate'): clear where
the run cannot send or nothing is mounted; with @--allow-mounted@, allowed,
the mounts noted in the reports; otherwise refused, the run exiting 3
before the drive is opened ('preflightDrive'). A run that sends Level 0
Discovery alone (UCT-01, the capture) needs no pre-flight.

At M4c's scaffold the types are final and the bodies are group G3's:
nothing is parsed or found mounted, the gate refuses naming G3,
'readMounts' reads nothing, and 'preflightDrive' notes nothing and
refuses nothing (no run calls it before G3).
-}
module Hsed.Jig.Preflight
  ( Mounted (..)
  , parseMounts
  , mountsOn
  , MountGate (..)
  , mountGate
  , readMounts
  , preflightDrive
  ) where

import           RIO


-- | A mount: the device, where it is mounted, and its file system's type
--   (@\/dev\/disk22s1@, @\/Volumes\/SED NVME@, @msdos@).
data Mounted = Mounted
  { mDevice     :: !Text
  , mMountPoint :: !Text
  , mFsType     :: !Text
  }
  deriving (Eq, Show)

-- | The mounts @/sbin/mount@ printed, a line each (@DEVICE on MOUNT POINT
--   (TYPE, options…)@). Group G3's: none until then.
parseMounts :: Text -> [Mounted]
parseMounts _ = []

-- | The mounts on a whole disk (its BSD name, @disk22@) and its slices
--   (@disk22s1@) alone. Group G3's: none until then.
mountsOn :: Text -> [Mounted] -> [Mounted]
mountsOn _ _ = []

-- | What a drive's mounts mean for the run: nothing in its way, mounts it
--   goes ahead with (@--allow-mounted@), or why it does not.
data MountGate
  = MountClear
  | MountAllowed ![Mounted]
  | MountRefused !Text
  deriving (Eq, Show)

-- | The gate, given whether the run may send to the drive, whether
--   @--allow-mounted@ was given, the whole disk's BSD name and the mounts.
--   Group G3's: refused until then.
mountGate :: Bool -> Bool -> Text -> [Mounted] -> MountGate
mountGate _ _ _ _ = MountRefused "G3: the mount gate (design 9.3 item 4)"

-- | What @/sbin/mount@ prints, read, or why not. Group G3's: nothing read
--   until then.
readMounts :: IO (Either Text [Mounted])
readMounts = pure (Left "G3: /sbin/mount, read (design 9.3 item 4)")

-- | The pre-flight of a drive, given whether the run may send to it,
--   whether @--allow-mounted@ was given and the whole disk's BSD name: why
--   the run does not go ahead (exit 3), or the notes for its reports (each
--   mount allowed). Group G3's: no note and no refusal until then, as
--   before M4c.
preflightDrive :: Bool -> Bool -> Text -> IO (Either Text [Text])
preflightDrive _ _ _ = pure (Right [])
