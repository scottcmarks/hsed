{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Level0.SedutilSpec
Description : Test specifications for sedutil's printed Level 0 read, compared with a response, and rendered as one
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Milestone M3's group G2 (design 10.2): @spec/fixtures/disk6-query.txt@, what
sedutil-cli 1.49.1 printed of disk6's Level 0 response, read as printed (its
TPer line separates ASYNC with a full stop, its comIDManagement and
HardReset take two spaces before @=@, its granularity a parenthesized
byte count); a drive-like response synthesized from it holds disk6's codes
and every value sedutil printed; and a response that holds other values,
or lacks a code, is told so. What 1.49.1 prints other than as name = value
(the Data Removal block, the count of codes it does not know) is read and
kept, not compared.
-}
module Hsed.Level0.SedutilSpec (spec) where

import qualified RIO.ByteString      as B
import qualified RIO.List            as L
import           RIO
import qualified RIO.Text            as T
import           Test.Hspec

import           Hsed.Level0         (Descriptor (..), Level0 (..), splitLevel0)
import           Hsed.Level0.Sedutil
import           Hsed.Sim.Level0     (level0Response)
import           Hsed.Sim.Types      (defaultSimParams, initialState)


spec :: Spec
spec = describe "sedutil's printed Level 0" $ do
  describe "reading spec/fixtures/disk6-query.txt" $ do
    it "names the device and the six features sedutil printed, by heading" $ withQuery $ \q -> do
      sqDevice q `shouldSatisfy` T.isPrefixOf "/dev/disk6 USB"
      map (\f -> (sfCode f, sfName f)) (sqFeatures q) `shouldBe`
        [ (0x0001, "TPer"), (0x0002, "Locking"), (0x0003, "Geometry"), (0x0202, "DataStore"), (0x0203, "OPAL 2.15")
        , (0x0402, "BlockSID") ]
    it "gives Users 9, base ComID 0x1004, DataStore 9/10485760/1, Granularity 8 and Streaming Y" $ withQuery $ \q -> do
      value 0x0203 "Locking Users" q `shouldBe` Just 9
      value 0x0203 "Base comID" q `shouldBe` Just 0x1004
      map (\l -> value 0x0202 l q) ["Max Tables", "Max Size Tables", "Table size alignment"] `shouldBe` map Just [9, 10485760, 1]
      value 0x0003 "Alignment Granularity" q `shouldBe` Just 8
      value 0x0001 "Streaming" q `shouldBe` Just 1
    it "reads the TPer line's full stop, the double spaces before =, and hex" $ withQuery $ \q -> do
      map fst (maybe [] sfValues (L.find ((== 0x0001) . sfCode) (sqFeatures q)))
        `shouldBe` ["ACKNAK", "ASYNC", "BufferManagement", "comIDManagement", "Streaming", "SYNC"]
      value 0x0402 "HardReset" q `shouldBe` Just 0
      value 0x0203 "Initial PIN" q `shouldBe` Just 0
      length (concatMap sfValues (sqFeatures q)) `shouldBe` 6 + 6 + 4 + 3 + 7 + 3

  describe "refusing what sedutil does not print" $ do
    it "no lines" $ parseQuery "\n\n" `shouldSatisfy` isLeft
    it "values before a heading" $ parseQuery "/dev/disk6 USB\n    Locked = N\n" `shouldSatisfy` isLeft
    it "a pair that is no name = value" $
      parseQuery "/dev/disk6 USB\nLocking function (0x0002)\n    Locked N\n" `shouldSatisfy` isLeft
    it "a value that is no Y, N or number" $
      parseQuery "/dev/disk6 USB\nLocking function (0x0002)\n    Locked = maybe\n" `shouldSatisfy` isLeft
    it "a line at the margin that is no heading" $
      parseQuery "/dev/disk6 USB\nLocking (0x0002)\n    Locked = N\n" `shouldSatisfy` isLeft

  describe "reading what 1.49.1 prints otherwise, as kept and not compared" $ do
    it "the count of codes it does not know, and the Data Removal block (DtaDev.cpp:368-383 at a7eafd5)" $ do
      text <- readFileUtf8 "../spec/fixtures/disk6-query.txt"
      let removal = [ "DataRemoval 1.1 function (0x0404)"
                    , "    DataRemoval OperationProcessing 0x00, DataRemoval Machanisim 0x04"
                    , "    DataRemoval TimeFormat Bit 5 : 0x00 0x0000"
                    , "    DataRemoval TimeFormat Bit 2 : 0x00 0x001e" ]
          unknown = "**** 1 **** Unknown function codes IGNORED "
      case parseQuery (text <> T.unlines (removal <> [unknown])) of
        Left e  -> expectationFailure (T.unpack e)
        Right q -> do
          map sfCode (sqFeatures q) `shouldBe` [0x0001, 0x0002, 0x0003, 0x0202, 0x0203, 0x0402]
          sqSkipped q `shouldBe` map T.strip (removal <> [unknown])
          withSplit (synthesize q) $ \l0 -> compareQuery q l0 `shouldBe` []
    it "nothing, in disk6's fixture" $ withQuery $ \q -> sqSkipped q `shouldBe` []

  describe "a drive-like response synthesized from it" $ do
    it "is a capture's length, disk6's codes {1, 2, 3, 0x202, 0x203, 0x402, 0x403}, nothing wrong" $ withQuery $ \q -> do
      let bytes = synthesize q
      B.length bytes `shouldBe` 2048
      withSplit bytes $ \l0 -> do
        map dCode (l0Descriptors l0) `shouldBe` [0x0001, 0x0002, 0x0003, 0x0202, 0x0203, 0x0402, 0x0403]
        l0Problems l0 `shouldBe` []
        l0LengthOfParameterData l0 `shouldBe` 44 + 16 + 16 + 32 + 16 + 20 + 16 + 20
    it "holds every value sedutil printed: compareQuery finds nothing" $ withQuery $ \q ->
      withSplit (synthesize q) $ \l0 -> compareQuery q l0 `shouldBe` []

  describe "comparing" $ do
    it "says which value a response holds otherwise" $ withQuery $ \q ->
      withSplit (synthesize q) $ \l0 -> do
        let ten (l, v) = (l, if l == "Locking Users" then 10 else v)
            users      = q { sqFeatures = map (\f -> f { sfValues = map ten (sfValues f) }) (sqFeatures q) }
        compareQuery users l0 `shouldSatisfy` \ds -> length ds == 1 && all ("Locking Users = 10" `T.isInfixOf`) ds
    it "finds the simulator's defaults other than disk6's in DataStore's tables and the Users" $ withQuery $ \q ->
      withSplit (level0Response (initialState defaultSimParams) 2048) $ \l0 -> do
        let found = compareQuery q l0
        length found `shouldBe` 2
        map (\l -> any (l `T.isInfixOf`) found) ["Max Tables = 9", "Locking Users = 9"] `shouldBe` [True, True]
    it "says when the response lacks a code sedutil printed" $ withQuery $ \q ->
      withSplit (synthesize q { sqFeatures = filter ((/= 0x0402) . sfCode) (sqFeatures q) }) $ \l0 ->
        compareQuery q l0 `shouldSatisfy` \ds -> length ds == 1 && all ("BlockSID (0x402)" `T.isPrefixOf`) ds
    it "says when sedutil printed a label or a feature hsed reads no value of" $ withQuery $ \q -> do
      let enterprise = SedutilFeature 0x0100 "Enterprise" [("comIDs", 1)]
          shiny f    = if sfCode f == 0x0002 then f { sfValues = sfValues f <> [("Shiny", 1)] } else f
          extra      = q { sqFeatures = map shiny (sqFeatures q) <> [enterprise] }
      withSplit (synthesize extra) $ \l0 -> do
        let found = compareQuery extra l0
        length found `shouldBe` 2
        map (\t -> any (t `T.isInfixOf`) found) ["Shiny", "Enterprise (0x100)"] `shouldBe` [True, True]


-- | The fixture, as the tests run in the hsed package's directory.
withQuery :: (SedutilQuery -> Expectation) -> Expectation
withQuery check = do
  text <- readFileUtf8 "../spec/fixtures/disk6-query.txt"
  either (expectationFailure . T.unpack) check (parseQuery text)

-- | Run an expectation on a response split, failing if it is refused.
withSplit :: ByteString -> (Level0 -> Expectation) -> Expectation
withSplit bytes check = either (expectationFailure . ("refused: " <>) . show) check (splitLevel0 bytes)

-- | A value sedutil printed, by the feature's code and the value's label.
value :: Word16 -> Text -> SedutilQuery -> Maybe Integer
value code label q = L.find ((== code) . sfCode) (sqFeatures q) >>= L.lookup label . sfValues
