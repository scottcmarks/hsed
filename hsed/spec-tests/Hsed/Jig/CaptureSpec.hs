{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.CaptureSpec
Description : Test specifications for Hsed.Jig.Capture
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A Level 0 capture as @--capture-level0 FILE@ writes it, into a temporary
directory: FILE the payload exactly; FILE.json beside it (the extension
replaced) with the date (UTC), the jig's version, the drive's BSD name, its
serial masked to its last four characters, the kext's build, the
interface, the registry entry id, the CDB in hex, the length, the bytes
transferred, the payload's sha256 (another implementation's, Python's
hashlib, for the 2048 bytes here; FIPS 180-4's for "abc") and its Length
of Parameter Data (Core Table 39's), a key to a line in that order; a line
on stderr naming both, the sha256 and the Length of Parameter Data. Nothing
is written for a response that is not good, or for a capture whose sidecar
would be itself. Without sudo the files stay the current user's, whatever
SUDO_UID says: "Hsed.Jig.Files" hands a file over only when the jig runs as
root.
-}
module Hsed.Jig.CaptureSpec (spec) where

import qualified Data.Aeson            as A
import qualified Data.Aeson.KeyMap     as KM
import           GHC.IO.Handle         (hDuplicate, hDuplicateTo)
import qualified RIO.ByteString        as B
import qualified RIO.Char              as C
import           RIO
import           RIO.Directory         (doesDirectoryExist, getPermissions, listDirectory, writable)
import           RIO.FilePath          ((</>))
import qualified RIO.Text              as T
import           System.Environment    (lookupEnv, setEnv, unsetEnv)
import           Test.Hspec

import           Hsed.Core.Record      (encodeRecord)
import           Hsed.Gen.Core.Layouts (Level0DiscoveryHeader (..))
import           Hsed.Jig.Capture      (captureLevel0, sidecarOf)
import           Hsed.Jig.Files        (runningAsRoot)
import           Hsed.Transport.Drive  (DriveInfo (..), InterfaceType (..))
import           Hsed.Version          (hsedVersion)
import           Hsed.Wire             (IfStatus (..), Response (..))


-- | disk6, as the kext transport admits it.
disk6 :: DriveInfo
disk6 = DriveInfo
  { diBsd        = "disk6"
  , diSerial     = "S59BNM0RA15802W"
  , diKextBuild  = "625"
  , diInterface  = IfSAT
  , diRegistryId = 0x100000a0d
  }

-- | The CDB of a Level 0 IF-RECV of 4 blocks on SAT.
cdb :: ByteString
cdb = B.pack [0xA1, 0x08, 0x0E, 0x01, 0x04, 0x00, 0x01, 0x00, 0x00, 0x5C, 0x00, 0x00]

-- | 2048 bytes: Core Table 39's header, its Length of Parameter Data 44 (the
--   header's bytes after the field), zeros after it.
level0 :: ByteString
level0 = header <> B.replicate (2048 - B.length header) 0
  where header = either (const "") id (encodeRecord (Level0DiscoveryHeader 44 (B.replicate 32 0)))

-- | What a capture wrote: the capture, the sidecar's text, and stderr's.
data Captured = Captured
  { cBytes   :: !ByteString
  , cSidecar :: !Text
  , cStderr  :: !Text
  }

-- | Where a capture goes in a temporary directory: a directory not there yet.
captureFile :: FilePath -> FilePath
captureFile tmp = tmp </> "fixtures" </> "disk6-level0.bin"

-- | Capture a response into a temporary directory.
captureIn :: FilePath -> Response -> IO Captured
captureIn tmp r = do
  let file = captureFile tmp
  ((), err) <- withStderrTo (tmp </> "stderr.txt") (captureLevel0 file disk6 cdb r)
  Captured <$> readFileBinary file <*> (decodeUtf8Lenient <$> readFileBinary (sidecarOf file)) <*> pure err

-- | Run an action with stderr sent to a file: its result, and what it wrote there.
withStderrTo :: FilePath -> IO a -> IO (a, Text)
withStderrTo path act = do
  hFlush stderr
  saved <- hDuplicate stderr
  a <- withBinaryFile path WriteMode $ \h ->
    (hDuplicateTo h stderr >> act) `finally` (hFlush stderr >> hDuplicateTo saved stderr)
  hClose saved
  (,) a . decodeUtf8Lenient <$> readFileBinary path

-- | The keys of a sidecar, one to a line, in order.
keysOf :: Text -> [Text]
keysOf = mapMaybe (fmap (T.takeWhile (/= '"')) . T.stripPrefix "\"" . T.stripStart) . T.lines

-- | A sidecar's fields, decoded.
fieldsOf :: Text -> IO A.Object
fieldsOf text = case A.eitherDecodeStrict (encodeUtf8 text) of
  Right (A.Object o) -> pure o
  other              -> throwString ("not a JSON object: " <> show other)

-- | Whether the files a capture wrote are the current user's to write.
writableBoth :: FilePath -> IO Bool
writableBoth tmp = and <$> traverse (fmap writable . getPermissions) [captureFile tmp, sidecarOf (captureFile tmp)]

-- | Run an action with environment variables set, restoring them after.
withEnv :: [(String, String)] -> IO a -> IO a
withEnv vars act = do
  saved <- traverse (\(k, _) -> (,) k <$> lookupEnv k) vars
  (traverse_ (uncurry setEnv) vars >> act) `finally` traverse_ (\(k, v) -> maybe (unsetEnv k) (setEnv k) v) saved

-- | Pending when the suite runs as root or under sudo, which it never should.
notUnderSudo :: IO () -> IO ()
notUnderSudo act = do
  root <- runningAsRoot
  sudo <- lookupEnv "SUDO_UID"
  if root || isJust sudo then pendingWith "the suite runs as root or under sudo: never (no sudo stack)" else act

spec :: Spec
spec = describe "Hsed.Jig.Capture" $ do
  it "writes the payload exactly, and beside it FILE.json with what produced it, a key to a line" $
    withSystemTempDirectory "hsed-capture" $ \tmp -> do
      c <- captureIn tmp (Response Good level0)
      cBytes c `shouldBe` level0
      keysOf (cSidecar c) `shouldBe`
        [ "capture", "captured", "hsed_jig", "bsd", "serial", "kext_build", "interface", "registry_id", "cdb"
        , "len_bytes", "transferred", "sha256", "length_of_parameter_data" ]
      fields <- fieldsOf (cSidecar c)
      KM.delete "captured" fields `shouldBe` KM.fromList
        [ ("capture", A.String "disk6-level0.bin")
        , ("hsed_jig", A.String hsedVersion)
        , ("bsd", A.String "disk6")
        , ("serial", A.String "***********802W")
        , ("kext_build", A.String "625")
        , ("interface", A.String "sat")
        , ("registry_id", A.Number 0x100000a0d)
        , ("cdb", A.String "a1 08 0e 01 04 00 01 00 00 5c 00 00")
        , ("len_bytes", A.Number 2048)
        , ("transferred", A.Number 2048)
        , ("sha256", A.String "76162884d2b3e67b1ee0f3e7c900ce8111a589ed358473b321a7644f1aaef115")
        , ("length_of_parameter_data", A.Number 44)
        ]
      KM.lookup "captured" fields `shouldSatisfy` maybe False utcStamp
      cSidecar c `shouldNotSatisfy` ("S59BNM0RA15802W" `T.isInfixOf`)
  it "says on stderr what it wrote: both files, the sha256, the Length of Parameter Data" $
    withSystemTempDirectory "hsed-capture" $ \tmp -> do
      c <- captureIn tmp (Response Good level0)
      [ t | t <- [ T.pack (captureFile tmp), T.pack (sidecarOf (captureFile tmp)), "2048 bytes"
                 , "sha256 76162884d2b3e67b1ee0f3e7c900ce8111a589ed358473b321a7644f1aaef115", "Length of Parameter Data 44" ]
          , not (t `T.isInfixOf` cStderr c) ]
        `shouldBe` []
  it "hashes whatever the payload holds, and reads no Length of Parameter Data from fewer bytes than a header" $
    withSystemTempDirectory "hsed-capture" $ \tmp -> do
      fields <- fieldsOf . cSidecar =<< captureIn tmp (Response Good "abc")
      map (`KM.lookup` fields) ["len_bytes", "sha256", "length_of_parameter_data"] `shouldBe`
        [ Just (A.Number 3), Just (A.String "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"), Just A.Null ]
  it "names the sidecar by the capture with its extension replaced" $
    map sidecarOf ["spec/fixtures/disk6-level0.bin", "build/capture"]
      `shouldBe` ["spec/fixtures/disk6-level0.json", "build/capture.json"]
  it "writes nothing for a response that is not good" $
    withSystemTempDirectory "hsed-capture" $ \tmp -> do
      let refused = [ Response (TransportFailure "hsed_kext_level0: kIOReturnError (0xe00002bc)") ""
                    , Response (Unsupported "M3: Level 0 only") "" ]
      forM_ refused $ \r -> captureLevel0 (captureFile tmp) disk6 cdb r `shouldThrow` anyException
      doesDirectoryExist (tmp </> "fixtures") `shouldReturn` False
  it "writes nothing for a capture whose sidecar would be itself" $
    withSystemTempDirectory "hsed-capture" $ \tmp -> do
      captureLevel0 (tmp </> "level0.json") disk6 cdb (Response Good level0) `shouldThrow` anyException
      listDirectory tmp `shouldReturn` []
  it "leaves both files the current user's without sudo (SUDO_UID absent)" $ notUnderSudo $
    withSystemTempDirectory "hsed-capture" $ \tmp -> do
      _ <- captureIn tmp (Response Good level0)
      writableBoth tmp `shouldReturn` True
  it "hands nothing over to SUDO_UID when the jig is not root" $ notUnderSudo $
    withSystemTempDirectory "hsed-capture" $ \tmp -> do
      _ <- withEnv [("SUDO_UID", "0"), ("SUDO_GID", "0")] (captureIn tmp (Response Good level0))
      writableBoth tmp `shouldReturn` True

-- | A UTC stamp as the sidecar writes it: @2026-10-30T12:34:56Z@.
utcStamp :: A.Value -> Bool
utcStamp (A.String t) = T.length t == 20 && all (uncurry fits) (T.zip "dddd-dd-ddTdd:dd:ddZ" t)
  where fits p ch = if p == 'd' then C.isDigit ch else p == ch
utcStamp _ = False
