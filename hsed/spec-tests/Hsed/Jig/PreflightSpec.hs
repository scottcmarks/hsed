{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.PreflightSpec
Description : Test specifications for Hsed.Jig.Preflight: a drive's mounts, and the gate a run that may send to it passes
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The mount pre-flight of a run that may send to a drive (design 9.3 item
4): the line @/sbin/mount@ printed for the drive's FAT32 volume on
2026-11-02 reads as its device, mount point and type; a mount point holding
@ on @ and parentheses reads whole, to the last @ (@; a table reads line by
line, a line of another shape as none. The mounts on a disk are its whole
disk's and its slices' alone: @disk2@ takes none of @disk22@'s, @disk22@
none of @disk220@'s. The gate is clear for a run that cannot send whatever
is mounted, and where nothing of the disk is; refuses a mounted drive,
naming the device, the mount point, the type and @--allow-mounted@; allows
it with the flag, the disk's mounts alone. @/sbin/mount@ itself is read
(with no argument: it changes nothing), the root among its mounts on
macOS; a run that cannot send reads nothing; on macOS the disk the root is
mounted from is refused to a run that may send, and allowed with the flag,
noted.
-}
module Hsed.Jig.PreflightSpec (spec) where

import qualified RIO.Char           as C
import qualified RIO.List           as L
import           RIO
import qualified RIO.Text           as T
import           System.Info        (os)
import           Test.Hspec

import           Hsed.Jig.Preflight


-- | The line @/sbin/mount@ printed for the drive's FAT32 volume on
--   2026-11-02 (the M4c pre-flight survey's snapshot): disk22's first
--   slice, FSKit's msdos, noatime.
realLine :: Text
realLine = "/dev/disk22s1 on /Volumes/SED NVME (msdos, local, nodev, nosuid, noowners, noatime, fskit)"

-- | That mount.
sedNvme :: Mounted
sedNvme = Mounted "/dev/disk22s1" "/Volumes/SED NVME" "msdos"

-- | The whole disk a device is a slice of: @disk3@ of @\/dev\/disk3s1s1@.
wholeDisk :: Text -> Text
wholeDisk device = "disk" <> T.takeWhile C.isDigit (T.drop (T.length "/dev/disk") device)

spec :: Spec
spec = do
  describe "parseMounts" $ do
    it "reads the drive's FAT32 volume as /sbin/mount printed it on 2026-11-02: its device, mount point and type" $
      parseMounts realLine `shouldBe` [sedNvme]
    it "reads a mount point holding ' on ' and parentheses whole, to the last ' ('" $
      parseMounts "/dev/disk5s1 on /Volumes/a on b (c) (apfs, local, journaled)" `shouldBe` [Mounted "/dev/disk5s1" "/Volumes/a on b (c)" "apfs"]
    it "reads a table line by line, a device with a space among them, and a line of another shape as none" $
      parseMounts (T.unlines [ realLine, "map auto_home on /System/Volumes/Data/home (autofs, automounted, nobrowse)", ""
                             , "nothing mounted here", "/dev/disk9 on /Volumes/x", "/dev/disk9 on /Volumes/y ()" ])
        `shouldBe` [sedNvme, Mounted "map auto_home" "/System/Volumes/Data/home" "autofs"]

  describe "mountsOn" $ do
    let mounts = [ Mounted d "/Volumes/v" "msdos"
                 | d <- [ "/dev/disk2", "/dev/disk2s1", "/dev/disk22", "/dev/disk22s1", "/dev/disk22s12", "/dev/disk22s1s1"
                        , "/dev/disk220s1", "/dev/disk22s", "/dev/disk22x1", "map disk22" ] ]
    it "takes disk2's whole disk and slices alone, none of disk22's" $
      map mDevice (mountsOn "disk2" mounts) `shouldBe` ["/dev/disk2", "/dev/disk2s1"]
    it "takes disk22's whole disk, slices and a slice's snapshot, none of disk220's nor a name that is no slice" $
      map mDevice (mountsOn "disk22" mounts) `shouldBe` ["/dev/disk22", "/dev/disk22s1", "/dev/disk22s12", "/dev/disk22s1s1"]
    it "takes a disk named by its device as by its BSD name" $
      mountsOn "/dev/disk22" [sedNvme] `shouldBe` [sedNvme]

  describe "mountGate" $ do
    it "is clear for a run that cannot send, whatever is mounted" $
      [ mountGate False allow "disk22" [sedNvme] | allow <- [False, True] ] `shouldBe` [MountClear, MountClear]
    it "is clear where nothing of the disk is mounted, with --allow-mounted or without" $
      [ mountGate True allow "disk22" ms | allow <- [False, True], ms <- [[], [Mounted "/dev/disk2s1" "/Volumes/other" "apfs"]] ]
        `shouldBe` replicate 4 MountClear
    it "refuses a mounted drive to a run that may send, naming the device, the mount point, the type and --allow-mounted" $
      case mountGate True False "disk22" [Mounted "/dev/disk2s1" "/" "apfs", sedNvme] of
        MountRefused why -> do
          for_ ["/dev/disk22s1 is mounted on /Volumes/SED NVME (msdos)", "--allow-mounted", "design 9.3 item 4"] $ \t ->
            why `shouldSatisfy` T.isInfixOf t
          why `shouldNotSatisfy` T.isInfixOf "/dev/disk2s1"
        other -> expectationFailure ("not refused: " <> show other)
    it "allows it with --allow-mounted, the disk's mounts alone" $
      mountGate True True "disk22" [Mounted "/dev/disk2s1" "/" "apfs", sedNvme] `shouldBe` MountAllowed [sedNvme]

  describe "readMounts and preflightDrive, over this machine's mount table" $ do
    it "reads /sbin/mount: the root among its mounts on macOS; elsewhere the table or why not, never an exception" $ do
      mounts <- readMounts
      if os == "darwin"
        then fmap (any ((== "/") . mMountPoint)) mounts `shouldBe` Right True
        else mounts `shouldSatisfy` either (T.isPrefixOf "/sbin/mount") (const True)
    it "notes and refuses nothing for a run that cannot send" $
      preflightDrive False False "disk22" `shouldReturn` Right []
    it "on macOS, refuses a run that may send to the disk the root is mounted from, and allows it with --allow-mounted, each mount noted" $
      if os /= "darwin"
        then pendingWith "the mount table of macOS's /sbin/mount, the drive transport's system's"
        else do
          root <- readMounts >>= either (throwString . T.unpack) pure <&> L.find ((== "/") . mMountPoint)
          disk <- maybe (throwString "no mount of /") (pure . wholeDisk . mDevice) root
          refused <- preflightDrive True False disk
          refused `shouldSatisfy` either (T.isInfixOf "is mounted on / (") (const False)
          allowed <- preflightDrive True True disk
          allowed `shouldSatisfy` either (const False) (any (T.isInfixOf " mounted on / ("))
          allowed `shouldSatisfy` either (const False) (all (T.isInfixOf "allowed by --allow-mounted"))
    it "finds nothing mounted on a disk the system does not have" $ do
      found <- preflightDrive True False "disk999999"
      if os == "darwin" then found `shouldBe` Right [] else found `shouldSatisfy` either (T.isInfixOf "--allow-mounted") null
