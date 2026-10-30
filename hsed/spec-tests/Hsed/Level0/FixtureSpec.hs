{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Level0.FixtureSpec
Description : Test specifications for the captured Level 0 response of disk6, once Scott has captured it
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Milestone M3's hardware deliverable (design 11): @spec/fixtures/disk6-level0.bin@,
the IF-RECV payload of disk6's Level 0 Discovery as the kext returned it
(hsed-jig's @--capture-level0@, run by Scott with sudo), and its sidecar
@disk6-level0.json@. Until they are committed each example is pending; then
the capture is exactly the 2048 bytes asked for, its header's versions Table
39's (0x0000 and 0x0001, PDF 68) or another revision Opal 2.30 Table 2
allows (PDF 19), its codes disk6's {1, 2, 3, 0x202, 0x203, 0x402, 0x403}
(the last the kext reports and sedutil does not print), zeros after Length
of Parameter Data and its four bytes, every descriptor decoded, every value
sedutil printed of disk6 (@spec/fixtures/disk6-query.txt@) held, and the
sidecar's sha256 and length the capture's.

Expected of disk6's 0x0403, from the kext's 'DTA Device Info' blob (design 1,
13.29 (i)): version 1, where CNL 1.30 pins 0x2 (5.2.1.2, PDF 52), so it
decodes with a 'PinDiffers' on its version, which fails nothing here; but a
version-1 descriptor may be shorter than CNL 1.30's 20 bytes (its Length
0x0C, not 0x10), which hsed's layout does not decode, and then "decodes
every descriptor" fails: that is the 0x0403 decision 13.29 (i) leaves open,
not a fault of the capture.
-}
module Hsed.Level0.FixtureSpec (spec) where

import qualified Data.Aeson          as A
import qualified RIO.ByteString      as B
import           RIO
import           RIO.Directory       (doesFileExist)
import qualified RIO.Text            as T
import           Test.Hspec

import           Hsed.Jig.Sha256     (sha256Hex)
import           Hsed.Jig.Support    (at)
import           Hsed.Level0         (Descriptor (..), Level0 (..), allowedByOpal, decodeDescriptor, splitLevel0)
import           Hsed.Level0.Sedutil (compareQuery, parseQuery)


spec :: Spec
spec = describe "spec/fixtures/disk6-level0.bin, disk6's captured Level 0 response" $ do
  it "is exactly the 2048 bytes asked for" $ withCapture $ \bytes ->
    B.length bytes `shouldBe` 2048
  it "splits with nothing wrong Opal 2.30 does not allow: codes in order, zeros after Length of Parameter Data + 4" $
    withLevel0 $ \l0 -> filter (not . allowedByOpal) (l0Problems l0) `shouldBe` []
  it "is what its sidecar says: its sha256 and its length" $ withCapture $ \bytes -> do
    there <- doesFileExist sidecar
    unless there (expectationFailure (sidecar <> " is not committed beside the capture"))
    json <- readFileBinary sidecar >>= either throwString pure . A.eitherDecodeStrict'
    (at "sha256" json, at "len_bytes" json)
      `shouldBe` (Just (A.String (sha256Hex bytes)), Just (A.toJSON (B.length bytes)))
  it "holds disk6's codes {1, 2, 3, 0x202, 0x203, 0x402, 0x403}" $ withLevel0 $ \l0 ->
    map dCode (l0Descriptors l0) `shouldBe` [0x0001, 0x0002, 0x0003, 0x0202, 0x0203, 0x0402, 0x0403]
  it "decodes every descriptor" $ withLevel0 $ \l0 ->
    [ dCode d | d <- l0Descriptors l0, isNothing (decodeDescriptor d) ] `shouldBe` []
  it "holds every value sedutil printed of disk6" $ withLevel0 $ \l0 -> do
    text <- readFileUtf8 "../spec/fixtures/disk6-query.txt"
    either (expectationFailure . T.unpack) (\q -> compareQuery q l0 `shouldBe` []) (parseQuery text)


-- | The capture's sidecar.
sidecar :: FilePath
sidecar = "../spec/fixtures/disk6-level0.json"

-- | Run an expectation on the capture, pending until it is committed.
withCapture :: (ByteString -> Expectation) -> Expectation
withCapture check = do
  let capture = "../spec/fixtures/disk6-level0.bin"
  there <- doesFileExist capture
  if there then readFileBinary capture >>= check else pendingWith "spec/fixtures/disk6-level0.bin not captured (Scott, M3)"

-- | Run an expectation on the capture split.
withLevel0 :: (Level0 -> Expectation) -> Expectation
withLevel0 check = withCapture (either (expectationFailure . ("refused: " <>) . show) check . splitLevel0)
