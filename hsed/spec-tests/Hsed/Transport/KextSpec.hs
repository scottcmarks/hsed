{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.KextSpec
Description : Test specifications for Hsed.Transport.Kext, the kext transport's decisions that need no kext
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What the kext transport decides before and after each call into the shim,
run here on values alone (nothing here calls the shim, IOKit or a device):
the whole disk a target names; the TPer the registry describes admitted, or
refused for other user clients or another serial, the serial printed only
masked; the buffer's size; one Level 0 IF-RECV's answer judged (success,
a kern_return, a CDB not hsed's, a short transfer) and printed; kern_return
codes named as IOReturn.h names them.
-}
module Hsed.Transport.KextSpec (spec) where

import           Foreign.ForeignPtr    (withForeignPtr)
import           Foreign.Ptr           (castPtr, ptrToWordPtr)
import qualified RIO.ByteString        as B
import           RIO
import qualified RIO.Text              as T
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)

import           Hsed.Transport.Drive  (DriveInfo (..), InterfaceType (..))
import           Hsed.Transport.Kext


-- | disk6 as the registry describes its TPer (an unprivileged ioreg, 2026-10-30).
disk6 :: Located
disk6 = Located
  { lRegistryId   = 0x100000a0d
  , lInterface    = IfSAT
  , lSerial       = "S59BNM0RA15802W     "
  , lKextBuild    = "625"
  , lOtherClients = 0
  }

-- | What hsed admits disk6 as.
disk6Info :: DriveInfo
disk6Info = DriveInfo
  { diBsd        = "disk6"
  , diSerial     = "S59BNM0RA15802W"
  , diKextBuild  = "625"
  , diInterface  = IfSAT
  , diRegistryId = 0x100000a0d
  }

-- | The 16 bytes of CDB the shim writes back for 4 blocks on SAT.
satCdb :: ByteString
satCdb = B.pack [0xA1, 0x08, 0x0E, 0x01, 0x04, 0x00, 0x01, 0x00, 0x00, 0x5C, 0x00, 0x00, 0, 0, 0, 0]

spec :: Spec
spec = describe "Hsed.Transport.Kext" $ do
  describe "diskName" $ do
    it "takes a whole disk, as a device or a BSD name" $
      map diskName ["/dev/disk6", "disk6", "/dev/disk12"] `shouldBe` [Right "disk6", Right "disk6", Right "disk12"]
    it "refuses a partition, a raw device, a volume, no number" $
      filter (isRight . diskName) ["/dev/disk6s1", "/dev/rdisk6", "rdisk6", "/Volumes/X", "disk", "/dev/", "", "disk6 ", "disk6\0"]
        `shouldBe` []

  describe "admit" $ do
    it "admits a TPer no one has open, with the serial expected, the serial trimmed" $ do
      admit (Just "S59BNM0RA15802W") "disk6" disk6 `shouldBe` Right disk6Info
      admit (Just "  S59BNM0RA15802W ") "disk6" disk6 `shouldBe` Right disk6Info
      admit Nothing "disk6" disk6 `shouldBe` Right disk6Info
    it "refuses a TPer whose user clients are already open, or could not be counted" $ do
      admit Nothing "disk6" disk6 { lOtherClients = 1 } `shouldSatisfy` isLeft
      admit Nothing "disk6" disk6 { lOtherClients = -1 } `shouldSatisfy` isLeft
    it "refuses another serial, or none, printing each only masked" $ do
      let refusals = lefts
            [ admit (Just "S59BNM0RA15802X") "disk6" disk6
            , admit (Just "S59BNM0RA15802W") "disk6" disk6 { lSerial = "" }
            ]
      length refusals `shouldBe` 2
      filter (\t -> any (`T.isInfixOf` t) ["S59BNM0RA15802W", "S59BNM0RA15802X"]) refusals `shouldBe` []
      refusals `shouldSatisfy` all ("***********802X" `T.isInfixOf`) . take 1
    it "keeps the interface and the registry entry id, and names a kext build it could not read unknown" $ do
      diInterface <$> admit Nothing "disk6" disk6 { lInterface = IfSCSI } `shouldBe` Right IfSCSI
      diKextBuild <$> admit Nothing "disk6" disk6 { lKextBuild = "" } `shouldBe` Right "unknown"

  describe "the IF-RECV buffer" $ do
    it "is whole 16 KiB pages, one at least" $
      map bufferBytes [0, 512, 2048, 16384, 16385, 130560] `shouldBe` [16384, 16384, 16384, 16384, 32768, 131072]
    prop "holds the transfer, in whole pages" $ \n ->
      let b = bufferBytes n in b `mod` pageBytes == 0 && b >= fromIntegral n && b - fromIntegral n < pageBytes + 1
    it "is allocated zeroed and aligned to the page, whatever its size" $ do
      buffers <- traverse ifRecvBuffer [512, 2048, 16384, 16385, 130560]
      checks <- traverse (\(buf, size) -> withForeignPtr buf $ \p -> do
                            bytes <- B.packCStringLen (castPtr p, size)
                            pure (ptrToWordPtr p `mod` fromIntegral pageBytes, size, B.all (== 0) bytes)) buffers
      checks `shouldBe` [(0, 16384, True), (0, 16384, True), (0, 16384, True), (0, 32768, True), (0, 131072, True)]

  describe "judgeLevel0" $ do
    let payload = B.replicate 2048 0x5a
    it "passes the payload of a call that succeeded with hsed's CDB and every byte moved" $
      judgeLevel0 IfSAT 4 0 satCdb 2048 payload `shouldBe` Right payload
    it "fails a kern_return other than success, by name" $
      judgeLevel0 IfSAT 4 0xe00002bc satCdb 2048 payload `shouldBe` Left "hsed_kext_level0: kIOReturnError (0xe00002bc)"
    it "fails a CDB other than hsed's: another command, another interface's, a count, a byte past the twelfth" $
      filter (\cdb -> isRight (judgeLevel0 IfSAT 4 0 cdb 2048 payload))
        [ B.take 9 satCdb <> B.singleton 0x5E <> B.drop 10 satCdb
        , B.pack [0xA2, 0x01, 0x00, 0x01, 0x80, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0, 0, 0, 0]
        , B.take 4 satCdb <> B.singleton 0x05 <> B.drop 5 satCdb
        , B.take 15 satCdb <> B.singleton 1
        , B.take 12 satCdb
        ]
        `shouldBe` []
    it "fails fewer bytes moved than asked" $
      judgeLevel0 IfSAT 4 0 satCdb 1536 payload `shouldBe` Left "hsed_kext_level0: 1536 of 2048 bytes transferred"
    it "prints the CDB, the kern_return and the bytes moved" $
      level0Line "disk6" satCdb 0 2048
        `shouldBe` ("kext disk6: Level 0 IF-RECV, CDB a1 08 0e 01 04 00 01 00 00 5c 00 00, "
                    <> "kern_return kIOReturnSuccess (0x00000000), 2048 bytes transferred")

  describe "kernReturn" $ do
    it "names IOKit's codes as IOReturn.h does, with their hex" $
      map kernReturn [0, 0xe00002bc, 0xe00002c2, 0xe00002c5, 0xe00002c7, 0xe00002cd, 0xe00002d0, 0xe00002d6, 0xe00002db, 0xe00002f0]
        `shouldBe`
          [ "kIOReturnSuccess (0x00000000)", "kIOReturnError (0xe00002bc)", "kIOReturnBadArgument (0xe00002c2)"
          , "kIOReturnExclusiveAccess (0xe00002c5)", "kIOReturnUnsupported (0xe00002c7)", "kIOReturnNotOpen (0xe00002cd)"
          , "kIOReturnNotAligned (0xe00002d0)", "kIOReturnTimeout (0xe00002d6)", "kIOReturnNoSpace (0xe00002db)"
          , "kIOReturnNotFound (0xe00002f0)" ]
    it "gives any other code its hex alone" $
      map kernReturn [0xe00002cb, 0xe00002f1, 0x12345678]
        `shouldBe` ["kern_return (0xe00002cb)", "kern_return (0xe00002f1)", "kern_return (0x12345678)"]

  describe "hsed_kext_info" $
    it "is laid out as cbits/hsed_kext.h declares it: 120 bytes, each field after the last" $ do
      let l = infoLayout
      (ilBytes l, ilAlignment l) `shouldBe` (120, 8)
      [ilConn l, ilRegistryId l, ilSat l, ilSerial l, ilKextBuild l, ilOtherClients l] `shouldBe` [0, 8, 16, 20, 84, 116]
      (ilSerial l + ilSerialBytes l, ilKextBuild l + ilKextBuildBytes l) `shouldBe` (ilKextBuild l, ilOtherClients l)
