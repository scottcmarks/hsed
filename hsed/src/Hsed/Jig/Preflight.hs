{-# LANGUAGE LambdaCase        #-}
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
than Level 0 Discovery, or @--recover@), what the system has mounted on the
drive (design 9.3 item 4): @/sbin/mount@'s lines read ('readMounts',
'parseMounts': the device before the first @ on @, the mount point from
there to the last @ (@, spaces, @ on @ and parentheses and all, the file
system's type the first word inside those last parentheses), those on the
whole disk and its slices alone ('mountsOn': @disk22@ and @disk22s1@, never
@disk2@'s nor @disk220s1@), and the gate ('mountGate'): clear where the run
cannot send or nothing is mounted; with @--allow-mounted@, allowed, each
mount noted in the reports, the volume to be checked unchanged afterwards
(@tools/m4c-hw.sh snapshot@ and @compare@); otherwise refused, the run
exiting 3 before the drive is opened ('preflightDrive'), as it is where the
mount table cannot be read. A run that sends Level 0 Discovery alone
(UCT-01, the capture) needs no pre-flight, as design 9.3 item 4 has had it
since M3: Level 0 reads no media and is answered whatever the sessions
(Core 3.3.6.2, PDF 67).

A gap, recorded: a volume the system synthesizes over a slice (an APFS
container's volumes, mounted from a @diskN@ of their own over the drive's
@disk22s2@) is mounted from no device of the drive's, and is not found.
The drive's FAT32 volume has none.
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

import qualified RIO.Char         as C
import           RIO
import           RIO.Process      (proc, readProcessStdout_, withProcessContextNoLogging)
import qualified RIO.Text         as T
import qualified RIO.Text.Partial as T'


-- $setup
-- >>> :set -XOverloadedStrings

-- | A mount: the device, where it is mounted, and its file system's type
--   (@\/dev\/disk22s1@, @\/Volumes\/SED NVME@, @msdos@).
data Mounted = Mounted
  { mDevice     :: !Text
  , mMountPoint :: !Text
  , mFsType     :: !Text
  }
  deriving (Eq, Show)

-- | The mounts @/sbin/mount@ printed, a line each (@DEVICE on MOUNT POINT
--   (TYPE, options…)@): the device what comes before the first @ on @, the
--   mount point everything from there to the last @ (@, the type the first
--   of the options; a line of another shape is none.
--
-- >>> parseMounts "/dev/disk22s1 on /Volumes/SED NVME (msdos, local, nodev, nosuid, noowners, noatime, fskit)"
-- [Mounted {mDevice = "/dev/disk22s1", mMountPoint = "/Volumes/SED NVME", mFsType = "msdos"}]
-- >>> parseMounts "/dev/disk5s1 on /Volumes/a on b (x) (apfs, local)\nnot a mount line"
-- [Mounted {mDevice = "/dev/disk5s1", mMountPoint = "/Volumes/a on b (x)", mFsType = "apfs"}]
parseMounts :: Text -> [Mounted]
parseMounts = mapMaybe mountLine . T.lines

-- | One line of @/sbin/mount@, if it is a mount's.
mountLine :: Text -> Maybe Mounted
mountLine line = do
  let (device, onRest) = T'.breakOn " on " line
  rest <- T.stripPrefix " on " onRest
  let (pointParen, options) = T'.breakOnEnd " (" rest
  point <- T.stripSuffix " (" pointParen
  inside <- T.stripSuffix ")" (T.stripEnd options)
  let fsType = T.strip (T.takeWhile (/= ',') inside)
  guard (not (T.null device) && not (T.null point) && not (T.null fsType))
  pure (Mounted device point fsType)

-- | The mounts on a whole disk (its BSD name, @disk22@, or its device,
--   @\/dev\/disk22@) and its slices (@disk22s1@, and a slice's own, a
--   volume's snapshot's @disk22s1s1@) alone: so @disk2@'s are never
--   @disk22s1@, nor @disk22@'s @disk220s1@.
--
-- >>> let m d = Mounted d "/Volumes/x" "msdos"
-- >>> map mDevice (mountsOn "disk22" [m "/dev/disk22", m "/dev/disk22s1", m "/dev/disk22s1s1", m "/dev/disk220s1", m "/dev/disk2s1"])
-- ["/dev/disk22","/dev/disk22s1","/dev/disk22s1s1"]
-- >>> mountsOn "disk2" [m "/dev/disk22s1"]
-- []
mountsOn :: Text -> [Mounted] -> [Mounted]
mountsOn disk = filter (maybe False slices . T.stripPrefix whole . mDevice)
  where
    whole = "/dev/" <> fromMaybe disk (T.stripPrefix "/dev/" disk)
    -- nothing after the whole disk's name, or slices: s and a number, as many times as they nest
    slices t = T.null t || case T.stripPrefix "s" t of
      Just r  -> let (n, more) = T.span C.isDigit r in not (T.null n) && slices more
      Nothing -> False

-- | What a drive's mounts mean for the run: nothing in its way, mounts it
--   goes ahead with (@--allow-mounted@), or why it does not.
data MountGate
  = MountClear
  | MountAllowed ![Mounted]
  | MountRefused !Text
  deriving (Eq, Show)

-- | The gate, given whether the run may send to the drive, whether
--   @--allow-mounted@ was given, the whole disk's BSD name and the mounts:
--   clear where the run cannot send, or nothing is mounted on the disk or
--   its slices ('mountsOn'); allowed, the mounts on them, with
--   @--allow-mounted@; refused otherwise, each mount named with its mount
--   point and type, and the flag that would let the run go ahead (design
--   9.3 item 4).
--
-- >>> mountGate True False "disk22" [Mounted "/dev/disk22s1" "/Volumes/SED NVME" "msdos"]
-- MountRefused "/dev/disk22s1 is mounted on /Volumes/SED NVME (msdos): a run that may issue IF-SENDs refuses a mounted drive unless --allow-mounted (design 9.3 item 4); with it, check each volume unchanged afterwards (tools/m4c-hw.sh snapshot and compare)"
mountGate :: Bool -> Bool -> Text -> [Mounted] -> MountGate
mountGate maySend allowMounted disk ms
  | not maySend || null onDisk = MountClear
  | allowMounted               = MountAllowed onDisk
  | otherwise                  = MountRefused
      (T.intercalate ", " [ mDevice m <> " is mounted on " <> mountedAt m | m <- onDisk ]
       <> ": a run that may issue IF-SENDs refuses a mounted drive unless --allow-mounted (design 9.3 item 4); with it, "
       <> "check each volume unchanged afterwards (tools/m4c-hw.sh snapshot and compare)")
  where onDisk = mountsOn disk ms

-- | Where a mount is, and its type: @\/Volumes\/SED NVME (msdos)@.
mountedAt :: Mounted -> Text
mountedAt m = mMountPoint m <> " (" <> mFsType m <> ")"

-- | What @/sbin/mount@ prints, with no argument (the system's mount table,
--   read and changed in nothing), read; or why not. It runs as the run
--   does, root's for a drive.
readMounts :: IO (Either Text [Mounted])
readMounts = tryAny (withProcessContextNoLogging (proc mountProgram [] readProcessStdout_)) <&> \case
  Left e    -> Left (T.pack mountProgram <> ": " <> T.pack (displayException e))
  Right out -> Right (parseMounts (decodeUtf8Lenient (toStrictBytes out)))
  where mountProgram = "/sbin/mount"

-- | The pre-flight of a drive, given whether the run may send to it,
--   whether @--allow-mounted@ was given and the whole disk's BSD name: why
--   the run does not go ahead (exit 3), or the notes for its reports, one
--   for each mount allowed. A run that cannot send reads nothing; one that
--   may is refused where the mount table cannot be read, unless
--   @--allow-mounted@, when that is noted instead (design 9.3 item 4).
preflightDrive :: Bool -> Bool -> Text -> IO (Either Text [Text])
preflightDrive maySend allowMounted disk
  | not maySend = pure (Right [])
  | otherwise   = readMounts <&> \case
      Left why
        | allowMounted -> Right ["the mounts of " <> device <> " unread (" <> why <> "): allowed by --allow-mounted (design 9.3 item 4)"]
        | otherwise    -> Left (device <> ": the mount table unread (" <> why <> "): a run that may issue IF-SENDs refuses a drive "
                                <> "whose mounts it cannot see, unless --allow-mounted (design 9.3 item 4)")
      Right ms -> case mountGate maySend allowMounted disk ms of
        MountClear       -> Right []
        MountAllowed ms' -> Right [ mDevice m <> " mounted on " <> mountedAt m <> ": allowed by --allow-mounted (design 9.3 item 4)" | m <- ms' ]
        MountRefused why -> Left why
  where device = "/dev/" <> fromMaybe disk (T.stripPrefix "/dev/" disk)
