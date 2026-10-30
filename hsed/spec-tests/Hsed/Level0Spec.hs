{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Level0Spec
Description : Test specifications for Hsed.Level0: a Level 0 Discovery response split, rendered and decoded
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Milestone M3's group G2 (design 11): a response is its header (Core Table
39, 48 bytes, Length of Parameter Data in bytes 0-3 counting the bytes after
itself, 3.3.6.2.1, PDF 68), then descriptors in increasing code order
(3.3.6.3, PDF 68), each stepped by its general header (Table 41: four bytes,
Length the count of the bytes after them), then zeros to the transfer length
(3.3.6.2, PDF 67). Rendering and splitting are inverses; the simulator's
default response is its eight descriptors, 216 valid bytes; a response cut
short, a descriptor shorter than hsed's layout of its code, codes out of
order and padding not zero are each reported, and splitting goes on.
-}
module Hsed.Level0Spec (spec) where

import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck  (prop)
import           Test.QuickCheck        (Gen, choose, counterexample, forAll, listOf, resize, vectorOf, (.&&.),
                                         (===))

import           Hsed.Core.Feature      (Allowance (..), Feature (..), FeatureHeader (..))
import           Hsed.Core.Layout       (Content (..), Difference (..), Field (..), Layout (..), LayoutError (..),
                                         encode, fieldEnd)
import           Hsed.Gen.Core.Features (LockingFeature, TPerFeature)
import           Hsed.Gen.Core.Layouts  (featureDescriptorTemplateLayout, level0DiscoveryHeaderLayout)
import           Hsed.Gen.Opal.Features (Level0DiscoveryGeometryReportingFeature (..), Level0DiscoveryTPerFeature (..))
import           Hsed.Level0
import           Hsed.Sim.FeatureSets   (geometryAllowances)
import           Hsed.Sim.Level0        (level0Response, simDescriptors)
import           Hsed.Sim.Types         (defaultSimParams, initialState)


spec :: Spec
spec = describe "a Level 0 Discovery response" $ do
  describe "Table 39 and Table 41, as the generated layouts give them" $ do
    it "Length of Parameter Data is bytes 0-3, so it counts from byte 4 (3.3.6.2.1, PDF 68); the header is 48 bytes" $ do
      [ fieldEnd f `div` 8 | f <- layoutFields level0DiscoveryHeaderLayout, fieldName f == "Length of Parameter Data" ]
        `shouldBe` [4]
      layoutSize level0DiscoveryHeaderLayout `shouldBe` 48
    it "a descriptor's general header is Table 41's: four bytes, then Length the count of the bytes after them" $ do
      let general = featureHeader (Proxy :: Proxy TPerFeature)
      general `shouldBe` FeatureHeader (layoutSize featureDescriptorTemplateLayout) "Length"
      map fieldName (layoutFields featureDescriptorTemplateLayout) `shouldContain` [headerLengthField general]
      featureHeader (Proxy :: Proxy LockingFeature) `shouldBe` general

  describe "the simulator's default response" $ do
    let response = level0Response (initialState defaultSimParams) 2048
    it "is exactly the transfer length asked for" $
      B.length response `shouldBe` 2048
    it "holds codes 1, 2, 3, 5, 0x202, 0x203, 0x402 and 0x404, Length of Parameter Data 212, and nothing wrong" $
      withSplit response $ \l0 -> do
        map dCode (l0Descriptors l0) `shouldBe` [0x0001, 0x0002, 0x0003, 0x0005, 0x0202, 0x0203, 0x0402, 0x0404]
        l0LengthOfParameterData l0 `shouldBe` 212
        l0Problems l0 `shouldBe` []
        B.length (l0Header l0) `shouldBe` 48
    it "is zeros from byte 216 to 2048" $
      withSplit response $ \l0 -> do
        B.length (l0Padding l0) `shouldBe` 2048 - 216
        B.all (== 0) (B.drop 216 response) `shouldBe` True
    it "is its descriptors, as the simulator gives them" $
      withSplit response $ \l0 ->
        l0Descriptors l0 `shouldBe` simDescriptors (initialState defaultSimParams)
    it "cut at 100 bytes, asks for 216 (BeyondTransfer) and its third descriptor runs past the end" $
      withSplit (level0Response (initialState defaultSimParams) 100) $ \l0 -> do
        l0Problems l0 `shouldBe` [BeyondTransfer 216 100, DescriptorTruncated 0x0003]
        map dCode (l0Descriptors l0) `shouldBe` [0x0001, 0x0002, 0x0003]
        map (B.length . dBytes) (l0Descriptors l0) `shouldBe` [16, 16, 20]
        l0Padding l0 `shouldBe` B.empty
    it "padded to 4096 bytes, is the same descriptors and more zeros" $
      withSplit (level0Response (initialState defaultSimParams) 4096) $ \l0 -> do
        l0Problems l0 `shouldBe` []
        B.length (l0Padding l0) `shouldBe` 4096 - 216

  describe "rendering and splitting" $ do
    prop "splitLevel0 . renderLevel0 gives back the descriptors, and rendering what it split gives back the response" $
      forAll genDescriptors $ \ds -> forAll (choose (0, 600)) $ \extra -> forAll genVendorUnique $ \vu ->
        let valid = layoutSize level0DiscoveryHeaderLayout + sum (map (B.length . dBytes) ds)
            len   = fromIntegral (valid + extra)
            r     = renderLevel0 vu ds len
        in case splitLevel0 r of
             Left e   -> counterexample (show e) False
             Right l0 ->
               l0Descriptors l0 === ds
               .&&. l0Problems l0 === []
               .&&. l0LengthOfParameterData l0 === fromIntegral (valid - 4)
               .&&. B.take 32 (B.drop 16 (l0Header l0)) === vu
               .&&. B.length r === valid + extra
               .&&. renderLevel0 vu (l0Descriptors l0) len === r
    it "fits the vendor unique bytes to bytes 16-47 of the header" $ do
      B.take 32 (B.drop 16 (renderLevel0 mempty [] 48)) `shouldBe` B.replicate 32 0
      B.take 32 (B.drop 16 (renderLevel0 (B.replicate 40 7) [] 48)) `shouldBe` B.replicate 32 7
    it "refuses a transfer shorter than the header" $
      splitLevel0 (B.replicate 47 0) `shouldBe` Left (Truncated 48 47)

  describe "what does not hold" $ do
    let own   = simDescriptors (initialState defaultSimParams)
        code' c = filter ((== c) . dCode) own
    it "a descriptor whose Length is below its layout is stepped past, and the next one still decodes" $ do
      -- a Locking descriptor (Opal Table 4, 16 bytes) whose Length gives 8 bytes after its header
      let short = Descriptor 0x0002 0x1 (B.pack [0x00, 0x02, 0x10, 0x08, 0x01, 0, 0, 0, 0, 0, 0, 0])
      withSplit (renderLevel0 mempty (short : code' 0x0003) 2048) $ \l0 -> do
        l0Problems l0 `shouldBe` []
        l0Descriptors l0 `shouldBe` short : code' 0x0003
        decodeDescriptor short `shouldBe` Nothing
        fmap (map ddSource) (traverse decodeDescriptor (drop 1 (l0Descriptors l0))) `shouldBe` Just ["opal-2.30 T5"]
    it "a pair out of code order is reported, and both are kept" $
      withSplit (renderLevel0 mempty (code' 0x0003 <> code' 0x0001) 2048) $ \l0 -> do
        l0Problems l0 `shouldBe` [OutOfCodeOrder 0x0003 0x0001]
        map dCode (l0Descriptors l0) `shouldBe` [0x0003, 0x0001]
    it "a repeated code is out of order too" $
      withSplit (renderLevel0 mempty (code' 0x0001 <> code' 0x0001) 2048) $ \l0 ->
        l0Problems l0 `shouldBe` [OutOfCodeOrder 0x0001 0x0001]
    it "padding other than zero is reported at its first such byte" $ do
      let r = level0Response (initialState defaultSimParams) 2048
          dirty = B.take 300 r <> B.singleton 0xAA <> B.drop 301 r
      withSplit dirty $ \l0 -> l0Problems l0 `shouldBe` [NonZeroPadding 300]
    it "a descriptor whose Length runs past Length of Parameter Data is kept with its bytes and reported" $ do
      -- a TPer descriptor whose Length says 12 bytes follow, eight of them there
      let cut = Descriptor 0x0001 0x1 (B.pack [0x00, 0x01, 0x10, 0x0C, 0x11, 0, 0, 0, 0, 0, 0, 0])
      withSplit (renderLevel0 mempty [cut] 2048) $ \l0 -> do
        l0Problems l0 `shouldBe` [DescriptorTruncated 0x0001]
        l0Descriptors l0 `shouldBe` [cut]
    it "a header whose versions are not Table 39's pins is reported" $ do
      let r = level0Response (initialState defaultSimParams) 2048
          major = B.take 4 r <> B.pack [0x00, 0x02] <> B.drop 6 r
      withSplit major $ \l0 -> do
        l0Problems l0 `shouldBe` [HeaderDiffers [PinDiffers "Data Structure Major Version" 32 0 2]]
        map dCode (l0Descriptors l0) `shouldBe` map dCode own

  describe "decoding a descriptor" $ do
    let own = simDescriptors (initialState defaultSimParams)
    it "reads each of the simulator's through the layout of its code, nothing differing" $ do
      let decoded = map decodeDescriptor own
      map (fmap ddSource) decoded `shouldBe` map Just
        [ "opal-2.30 T3", "opal-2.30 T4", "opal-2.30 T5", "siis-1.20 T2", "sedutil Discovery0DatastoreTable"
        , "opal-2.30 T6", "sedutil Discovery0BlockSIDFeatures", "opal-2.30 T8" ]
      map (fmap ddDifferences) decoded `shouldBe` map (const (Just [])) own
    it "gives the pinned fields' values too" $ do
      let tper = L.find ((== 0x0001) . dCode) own >>= decodeDescriptor
      (tper >>= L.lookup "Feature Code" . ddFields) `shouldBe` Just (Number 0x0001)
      (tper >>= L.lookup "Streaming Supported" . ddFields) `shouldBe` Just (Number 1)
      (tper >>= L.lookup "Length" . ddFields) `shouldBe` Just (Number 0x0C)
    it "allows Geometry Reporting what the LockingInfo table gives it, and Opal's descriptors what Opal prints" $ do
      let allowances c = ddAllowances <$> (L.find ((== c) . dCode) own >>= decodeDescriptor)
      allowances 0x0003 `shouldBe` Just geometryAllowances
      allowances 0x0001 `shouldBe` Just (featureAllowances (Proxy :: Proxy Level0DiscoveryTPerFeature))
      fmap (L.lookup "Number of Locking SP User Authorities Supported") (allowances 0x0203)
        `shouldBe` Just (Just (Minimum 8))
    it "reports a drive's pins read otherwise rather than refusing them" $ do
      -- an older Locking descriptor: Opal Table 4 pins Media Encryption (byte 4 bit 3) to 1; this one has it 0
      let older = Descriptor 0x0002 0x1 (B.pack ([0x00, 0x02, 0x10, 0x0C, 0x01] <> replicate 11 0))
      fmap ddDifferences (decodeDescriptor older) `shouldBe` Just [PinDiffers "Media Encryption" 36 1 0]
    it "leaves a vendor's or an unknown code undecoded" $ do
      decodeDescriptor (Descriptor 0xC000 0x1 (B.pack ([0xC0, 0x00, 0x10, 0x0C] <> replicate 12 0))) `shouldBe` Nothing
      decodeDescriptor (Descriptor 0x0100 0x1 (B.pack ([0x01, 0x00, 0x10, 0x0C] <> replicate 12 0))) `shouldBe` Nothing

  describe "a feature record as a descriptor" $
    it "has its code, its version and its bytes, Length counting those after the header" $ do
      let geometry = Level0DiscoveryGeometryReportingFeature True 512 8 0
      fmap (\d -> (dCode d, dVersion d, B.length (dBytes d), B.index (dBytes d) 3)) (featureDescriptor geometry)
        `shouldBe` Right (0x0003, 0x1, 32, 0x1C)
      fmap dCode (featureDescriptor (Level0DiscoveryTPerFeature 0x1 False False False False)) `shouldBe` Right 0x0001


-- | Run an expectation on a response split, failing if it is refused.
withSplit :: ByteString -> (Level0 -> Expectation) -> Expectation
withSplit bytes check = either (expectationFailure . ("refused: " <>) . show) check (splitLevel0 bytes)

-- | Descriptors in increasing code order, each Table 41's header and a
--   payload its Length counts.
genDescriptors :: Gen [Descriptor]
genDescriptors = do
  codes <- L.nub . L.sort <$> resize 12 (listOf (choose (0, maxBound)))
  traverse genDescriptor codes
  where
    genDescriptor code = do
      version <- choose (0, 15)
      payload <- B.pack <$> resize 40 (listOf (choose (0, 255)))
      let fields = [ ("Feature Code", Number (toInteger code)), ("Version", Number version)
                   , ("Length", Number (toInteger (B.length payload))), ("Feature Dependent Data", Octets payload) ]
      either (error . show) (pure . Descriptor code (fromInteger version)) (encode featureDescriptorTemplateLayout fields)

-- | Table 39's vendor unique bytes, 16-47.
genVendorUnique :: Gen ByteString
genVendorUnique = B.pack <$> vectorOf 32 (choose (0, 255))
