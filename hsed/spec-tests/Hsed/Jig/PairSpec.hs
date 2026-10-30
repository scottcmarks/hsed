{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.PairSpec
Description : Test specifications for Hsed.Jig.Pair
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Pair runner's comparison of Level 0 (design 9.2): what an allowance
allows (a vendor unique value, any version, a minimum the drive's value
meets), what it leaves unchecked (a condition as printed hsed does not
evaluate) and what it evaluates (Table 7's SSC minor versions); the fields
of two decodes compared by name, a reserved bit the drive sets a difference
nothing allows, named by its byte and bit; the
descriptor sets first, a code the drive alone reports missing and a
failure, one the simulator alone reports extra and none. Then the
milestone's comparison: the simulator against a drive-like
response synthesized from what sedutil printed of disk6, 0x0403 missing
without a profile; with the response as the profile, extra 0x0005 and
0x0404 and every difference allowed, the 9 Locking SP users against the
simulator's 8 among them (Opal 2.30 Table 6's "8 or larger", PDF 23).
-}
module Hsed.Jig.PairSpec (spec) where

import qualified RIO.ByteString      as B
import qualified RIO.List            as L
import           RIO
import           RIO.FilePath        ((</>))
import qualified RIO.Text            as T
import           Test.Hspec

import           Hsed.Core.Feature   (Allowance (..))
import           Hsed.Core.Layout    (Content (..), Difference (..))
import           Hsed.Jig.Pair
import           Hsed.Jig.Support    (disk6Like)
import           Hsed.Level0         (DecodedDescriptor (..), Descriptor (..), Level0 (..), splitLevel0)
import           Hsed.Sim.Level0     (level0Response, simDescriptors)
import           Hsed.Sim.Profile    (loadProfile)
import           Hsed.Sim.Types      (SimParams (..), defaultSimParams, initialState)


diffOf :: Maybe Allowance -> Content -> Content -> FieldDiff
diffOf a s d = FieldDiff 0x0203 "a field" s d a

decoded :: [(Text, Content)] -> [Difference] -> [(Text, Allowance)] -> DecodedDescriptor
decoded = DecodedDescriptor "a test"

-- | A response of descriptors, each of a code and its bytes.
level0Of :: [(Word16, ByteString)] -> Level0
level0Of ds = Level0 mempty 0 [ Descriptor c 1 b | (c, b) <- ds ] mempty []

split :: ByteString -> IO Level0
split = either (throwString . show) pure . splitLevel0

spec :: Spec
spec = do
  describe "judged" $ do
    it "allows a vendor unique value and any version, whatever the values" $
      map judged [ diffOf (Just VendorUnique) (Number 0x1004) (Number 0x1000)
                 , diffOf (Just (AnyVersion 2)) (Number 2) (Number 1) ]
        `shouldBe` [Allowed, Allowed]
    it "leaves a condition as printed unchecked, no failure but not allowed" $ do
      judged (diffOf (Just (AsPrinted "**")) (Number 0) (Number 1)) `shouldBe` Unchecked
      allowed (diffOf (Just (AsPrinted "**")) (Number 0) (Number 1)) `shouldBe` False
    it "evaluates the SSC Minor Version Number against Opal 2.30 Table 7: 0x0 to 0x3, the rest reserved" $
      [ judged (diffOf (Just (AsPrinted "As specified in Table 7")) (Number 3) (Number v)) | v <- [0, 1, 2, 4, 0xF] ]
        `shouldBe` [Allowed, Allowed, Allowed, NotAllowed, NotAllowed]
    it "fails nothing for an unchecked difference" $
      pairFails (PairReport [] [] [] [diffOf (Just (AsPrinted "**")) (Number 0) (Number 1)] []) `shouldBe` False
    it "allows a drive's value at or above a minimum, not below it, and no bytes against one" $
      map allowed [ diffOf (Just (Minimum 8)) (Number 9) (Number 8)
                  , diffOf (Just (Minimum 8)) (Number 8) (Number 7)
                  , diffOf (Just (Minimum 8)) (Number 8) (Octets "\x09") ]
        `shouldBe` [True, False, False]
    it "allows nothing where the spec allows the field no other value" $
      allowed (diffOf Nothing (Number 1) (Number 0)) `shouldBe` False

  describe "fieldDiffs" $ do
    it "compares two decodes field by field, each difference with what the layout allows it" $
      fieldDiffs 0x0203
        (decoded [("Base ComID", Number 0x1004), ("Number of ComIDs", Number 1), ("Users", Number 8)] []
                 [("Base ComID", VendorUnique), ("Users", Minimum 8)])
        (decoded [("Base ComID", Number 0x1004), ("Number of ComIDs", Number 2), ("Users", Number 9)] []
                 [("Base ComID", VendorUnique), ("Users", Minimum 8)])
        `shouldBe` [ FieldDiff 0x0203 "Number of ComIDs" (Number 1) (Number 2) Nothing
                   , FieldDiff 0x0203 "Users" (Number 8) (Number 9) (Just (Minimum 8)) ]
    it "reports each reserved bit the drive sets and the simulator does not, by its byte and bit, which nothing allows" $
      fieldDiffs 0x0002 (decoded [] [] []) (decoded [] [ReservedNotZero "Reserved" 40 (2 ^ (80 :: Int) + 2 ^ (3 :: Int))] [])
        `shouldBe` [ FieldDiff 0x0002 "Reserved, byte 5 bit 0 (reserved)" (Number 0) (Number 1) Nothing
                   , FieldDiff 0x0002 "Reserved, byte 15 bit 3 (reserved)" (Number 0) (Number 1) Nothing ]

  describe "pairLevel0" $ do
    it "compares the sets of codes first: the drive's alone missing, a failure; the simulator's alone extra, none" $ do
      let same = B.replicate 16 7
          r    = pairLevel0 (level0Of [(0x0001, same), (0x0005, same), (0x0404, same)])
                            (level0Of [(0x0001, same), (0x0403, same)])
      (prExtra r, prMissing r, prAllowed r, prUnchecked r, prDiffs r) `shouldBe` ([0x0005, 0x0404], [0x0403], [], [], [])
      pairFails r `shouldBe` True
    it "passes descriptors whose bytes are equal, and extra codes, without decoding them" $ do
      let r = pairLevel0 (level0Of [(0x0001, "a"), (0x0005, "b")]) (level0Of [(0x0001, "a")])
      (prExtra r, prMissing r, pairFails r) `shouldBe` ([0x0005], [], False)

  describe "what the descriptors with hand-written allowances allow" $ do
    let own c = L.find ((== c) . dCode) (simDescriptors (initialState defaultSimParams))
        poked c at v = fmap (\d -> d { dBytes = B.take at (dBytes d) <> B.singleton v <> B.drop (at + 1) (dBytes d) }) (own c)
        compared c at v = case (own c, poked c at v) of
          (Just s, Just d) -> pairLevel0 (level0Of [(c, dBytes s)]) (level0Of [(c, dBytes d)])
          _                -> PairReport [] [] [] [] []
    it "Supported Data Removal Mechanism: a time of the drive's own allowed, and its state unchecked (Opal 3.1.1.6)" $ do
      let timed = compared 0x0404 13 30
      (length (prAllowed timed), prDiffs timed, pairFails timed) `shouldBe` (1, [], False)
      let processing = compared 0x0404 5 0x01
      (length (prUnchecked processing), prDiffs processing, pairFails processing) `shouldBe` (1, [], False)
    it "Opal's TPer descriptor: ACK/NAK Supported, VU by Opal 2.30's bullet (its ACK/NACK, PDF 20), allowed" $ do
      let r = compared 0x0001 4 0x15
      (map fdField (prAllowed r), prDiffs r, pairFails r) `shouldBe` (["ACK/NAK Supported"], [], False)
    it "Block SID: a flag of the TPer's present state unchecked, not a failure" $
      -- byte 4 bit 0 SID Value State, bit 1 SID Blocked State; byte 5 bit 0 Hardware Reset
      forM_ [(4, 0x01), (4, 0x02), (5, 0x01)] $ \(at, b) -> do
        let r = compared 0x0402 at b
        (length (prUnchecked r), prDiffs r, pairFails r) `shouldBe` (1, [], False)

  describe "the simulator against a drive-like response synthesized from disk6's sedutil query" $ do
    it "without a profile, finds 0x0403 missing, a failure" $ do
      drive <- disk6Like >>= split
      sim <- split (level0Response (initialState defaultSimParams) 2048)
      let r = pairLevel0 sim drive
      prMissing r `shouldBe` [0x0403]
      pairFails r `shouldBe` True
    it "with the response as the profile, finds extra 0x0005 and 0x0404, and only differences allowed, the users among them" $
      withSystemTempDirectory "hsed-pair" $ \dir -> do
        bytes <- disk6Like
        let file = dir </> "disk6-like-level0.bin"
        writeFileBinary file bytes
        profile <- loadProfile file >>= either (throwString . T.unpack) pure
        drive <- split bytes
        sim <- split (level0Response (initialState defaultSimParams { spProfile = Just profile }) 2048)
        let r = pairLevel0 sim drive
        (prExtra r, prMissing r, prDiffs r) `shouldBe` ([0x0005, 0x0404], [], [])
        prAllowed r `shouldSatisfy` any (\fd -> (fdCode fd, fdField fd, fdSim fd, fdDrive fd, fdAllowance fd)
          == (0x0203, "Number of Locking SP User Authorities Supported", Number 8, Number 9, Just (Minimum 8)))
        pairFails r `shouldBe` False
