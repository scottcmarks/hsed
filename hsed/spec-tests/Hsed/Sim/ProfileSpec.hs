{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.ProfileSpec
Description : Test specifications for --profile: a drive's Level 0 response added to the simulator's
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Milestone M3's group G2 (design 9.2, corrected at M3): a profile adds the
drive's descriptors of codes the simulator lacks, as they are, and drops or
replaces none of the simulator's, so that with disk6's profile (here the
response synthesized from @spec/fixtures/disk6-query.txt@ until the drive's
own is captured) the simulator gains Configurable Namespace Locking (0x0403)
and keeps SIIS (0x0005) and Supported Data Removal Mechanism (0x0404), the
codes the Pair runner reports as the simulator's extras. A file that is no
Level 0 response, or one cut short, is refused.
-}
module Hsed.Sim.ProfileSpec (spec) where

import qualified RIO.ByteString      as B
import qualified RIO.List            as L
import           RIO
import           RIO.FilePath        ((</>))
import qualified RIO.Text            as T
import           Test.Hspec

import           Hsed.Level0         (Descriptor (..), Level0 (..), splitLevel0)
import           Hsed.Level0.Sedutil (parseQuery, synthesize)
import           Hsed.Sim.Level0     (level0Response, simDescriptors)
import           Hsed.Sim.Profile    (loadProfile)
import           Hsed.Sim.Types      (Level0Profile (..), SimParams (..), defaultSimParams, initialState)


spec :: Spec
spec = describe "a profile (--profile)" $ do
  it "is a response's descriptors, read from its file" $ withDisk6Like $ \path profile -> do
    lpSource profile `shouldBe` path
    map dCode (lpDescriptors profile) `shouldBe` [0x0001, 0x0002, 0x0003, 0x0202, 0x0203, 0x0402, 0x0403]

  it "adds 0x0403 to the simulator's descriptors, which keep 0x0005 and 0x0404" $ withDisk6Like $ \_ profile -> do
    let own  = simDescriptors (initialState defaultSimParams)
        with = simDescriptors (initialState defaultSimParams { spProfile = Just profile })
    map dCode with `shouldBe` [0x0001, 0x0002, 0x0003, 0x0005, 0x0202, 0x0203, 0x0402, 0x0403, 0x0404]
    filter ((== 0x0403) . dCode) with `shouldBe` filter ((== 0x0403) . dCode) (lpDescriptors profile)
    filter ((/= 0x0403) . dCode) with `shouldBe` own

  it "drops or replaces none of the simulator's, though the drive's differ" $ withDisk6Like $ \_ profile -> do
    let with = simDescriptors (initialState defaultSimParams { spProfile = Just profile })
        drive c = L.find ((== c) . dCode) (lpDescriptors profile)
    -- disk6's DataStore and Opal SSC V2 descriptors hold other values than the simulator's (9 tables, 9 users)
    [ fmap dBytes (L.find ((== c) . dCode) with) == fmap dBytes (drive c) | c <- [0x0202, 0x0203] ] `shouldBe` [False, False]
    filter ((/= 0x0403) . dCode) with `shouldBe` simDescriptors (initialState defaultSimParams)

  it "lengthens the simulator's response by the descriptors it adds: Length of Parameter Data 232" $
    withDisk6Like $ \_ profile ->
      either (expectationFailure . show) (\l0 -> (l0LengthOfParameterData l0, l0Problems l0) `shouldBe` (232, []))
        (splitLevel0 (level0Response (initialState defaultSimParams { spProfile = Just profile }) 2048))

  it "refuses a file it cannot read" $ withSystemTempDirectory "hsed-profile" $ \dir ->
    loadProfile (dir </> "absent.bin") >>= (`shouldSatisfy` isLeft)

  it "refuses a file shorter than a header" $ withSystemTempDirectory "hsed-profile" $ \dir -> do
    writeFileBinary (dir </> "short.bin") (B.replicate 10 0)
    loadProfile (dir </> "short.bin") >>= (`shouldSatisfy` either ("not a Level 0 response" `T.isInfixOf`) (const False))

  it "refuses a response cut short" $ withSystemTempDirectory "hsed-profile" $ \dir -> do
    bytes <- disk6Like
    writeFileBinary (dir </> "cut.bin") (B.take 100 bytes)
    loadProfile (dir </> "cut.bin") >>= (`shouldSatisfy` either ("cut short" `T.isInfixOf`) (const False))


-- | The response synthesized from disk6's printed query.
disk6Like :: IO ByteString
disk6Like = do
  text <- readFileUtf8 "../spec/fixtures/disk6-query.txt"
  either (\e -> throwString (T.unpack e)) (pure . synthesize) (parseQuery text)

-- | Run an expectation on the profile of disk6's synthesized response, written to a file.
withDisk6Like :: (FilePath -> Level0Profile -> Expectation) -> Expectation
withDisk6Like check = withSystemTempDirectory "hsed-profile" $ \dir -> do
  let path = dir </> "disk6-like-level0.bin"
  disk6Like >>= writeFileBinary path
  loadProfile path >>= either (expectationFailure . T.unpack) (check path)
