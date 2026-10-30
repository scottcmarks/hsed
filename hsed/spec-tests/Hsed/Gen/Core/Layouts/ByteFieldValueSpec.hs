{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Gen.Core.Layouts.ByteFieldValueSpec
Description : The generated layouts of the byteFieldValue rule, at run time
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The layouts hsedc generates from Core's protocol 0x02 payloads (Tables 28,
31-33, 35-37, section 3.3.4.7) as hsed compiles them: they tile, have the sizes
their tables print, and encode and decode through "Hsed.Core.Layout" with the
constants the VALUE column pins, and their records (byteGrid's printer) through
"Hsed.Core.Record"; the reserved bytes to the end of the transfer take no
value and, read other than zero, are reported; a STACK_RESET pending reply is
told from a response by their pins. Table 236 (section 5.8.4.2) is generated
as printed, byte 8 in no row (PDF 298), and the runtime refuses it until an
erratum names that byte.
-}
module Hsed.Gen.Core.Layouts.ByteFieldValueSpec (spec) where

import qualified RIO.ByteString        as B
import           RIO
import           Test.Hspec

import           Hsed.Core.Layout      (Content (..), Decoded (..), Difference (..), Layout (..), LayoutError (..),
                                        TileProblem (..), decode, decodeStrict, encode, matching, tiles)
import           Hsed.Core.Record      (DecodedRecord (..), decodeRecord, decodeRecordStrict, encodeRecord)
import           Hsed.Gen.Core.Layouts (STACK_RESETCommandRequest (..), STACK_RESETCommandResponse (..),
                                        VERIFY_COMID_VALIDCommandResponse (..), get_COMIDPayloadLayout,
                                        noResponseAvailableLayout, stack_RESETCommandRequestLayout,
                                        stack_RESETCommandResponseLayout, stack_RESETPendingLayout,
                                        systemLogEntryStructureLayout, verify_COMID_VALIDCommandResponseLayout,
                                        verify_COMID_VALIDRequestLayout)


spec :: Spec
spec = describe "the byteFieldValue layouts (core-2.01 T28, T31-T33, T35-T37)" $ do
  let layouts = [ get_COMIDPayloadLayout, noResponseAvailableLayout, verify_COMID_VALIDRequestLayout, verify_COMID_VALIDCommandResponseLayout
                , stack_RESETCommandRequestLayout, stack_RESETCommandResponseLayout, stack_RESETPendingLayout ]
  it "tile their bytes" $
    map tiles layouts `shouldBe` replicate 7 True
  it "are as long as their tables' fixed parts (PDF 61-66)" $
    map layoutSize layouts `shouldBe` [4, 12, 8, 46, 8, 16, 12]
  it "STACK_RESET Command Request: the Request Code written as its pin, 00 00 00 02 (T35, PDF 65)" $
    encode stack_RESETCommandRequestLayout [("Extended ComID", Number 0x07FE0000)] `shouldBe` Right (B.pack [0x07, 0xFE, 0, 0, 0, 0, 0, 2])
  it "No Response Available: the rest of the transfer its tail (T31, PDF 63)" $
    fmap decodedFields (decode noResponseAvailableLayout (B.pack ([0x07, 0xFE, 0, 0] <> replicate 12 0)))
      `shouldBe` Right [ ("Extended ComID", Number 0x07FE0000), ("Request Code", Number 0), ("Available Data Length in bytes", Number 0) ]
  it "the reserved rest of the transfer: no value taken, read other than zero reported (T35, PDF 65)" $ do
    encode stack_RESETCommandRequestLayout [("Extended ComID", Number 0x07FE0000), ("Reserved", Octets "\xff")] `shouldBe` Left (ReservedGiven "Reserved")
    fmap drDifferences (decodeRecord (B.pack ([0x07, 0xFE, 0, 0, 0, 0, 0, 2] <> [0, 0xff])) :: Either LayoutError (DecodedRecord STACK_RESETCommandRequest))
      `shouldBe` Right [ReservedNotZero "Reserved" 64 0xff]
  it "tells a STACK_RESET pending reply from a response by their pins (T36, T37, PDF 66), a lenient decode's PinDiffers no match" $ do
    let pendingReply = B.pack ([0x07, 0xFE, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0] <> replicate 8 0)
        response = B.pack ([0x07, 0xFE, 0, 0, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 0] <> replicate 4 0)
        candidates = [stack_RESETCommandResponseLayout, stack_RESETPendingLayout]
    fmap drDifferences (decodeRecord pendingReply :: Either LayoutError (DecodedRecord STACK_RESETCommandResponse))
      `shouldBe` Right [PinDiffers "Available Data Length in bytes" 80 4 0]
    map (layoutName . fst) (matching candidates pendingReply) `shouldBe` ["STACK_RESET Pending"]
    map (layoutName . fst) (matching candidates response) `shouldBe` ["STACK_RESET Command Response"]
  it "STACK_RESET Command Response: a Request Code other than 2 refused when read strictly (T36, PDF 66)" $
    fmap decodedFields (decodeStrict stack_RESETCommandResponseLayout (B.pack ([0x07, 0xFE, 0, 0, 0, 0, 0, 1, 0, 0, 0, 4] <> replicate 4 0)))
      `shouldSatisfy` isLeft
  it "System Log Entry Structure: refused, byte 8 in no field, until an erratum names it (T236, PDF 298)" $
    encode systemLogEntryStructureLayout [] `shouldBe` Left (NotTiled [Gap 64 71])
  it "STACK_RESET Command Request as a record: its Extended ComID, the Request Code pinned, the reserved rest of the transfer skipped (T35, PDF 65)" $ do
    encodeRecord (STACK_RESETCommandRequest 0x07FE0000) `shouldBe` Right (B.pack [0x07, 0xFE, 0, 0, 0, 0, 0, 2])
    decodeRecordStrict (B.pack ([0x07, 0xFE, 0, 0, 0, 0, 0, 2] <> replicate 504 0))
      `shouldBe` Right (DecodedRecord (STACK_RESETCommandRequest 0x07FE0000) [] "")
  it "STACK_RESET Command Response as a record: Success/Failure a number, a Request Code of 1 refused strictly (T36, PDF 66)" $ do
    let bytes = B.pack ([0x07, 0xFE, 0, 0, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 1] <> replicate 4 0)
    fmap drRecord (decodeRecord bytes) `shouldBe` Right (STACK_RESETCommandResponse 0x07FE0000 1)
    (fmap drRecord (decodeRecordStrict (B.take 7 bytes <> "\x01" <> B.drop 8 bytes)) :: Either LayoutError STACK_RESETCommandResponse)
      `shouldBe` Left (PinViolated "Request Code" 2 1)
  it "VERIFY_COMID_VALID Command Response as a record: its three 10-byte times as bytes (T33, PDF 64)" $ do
    let r = VERIFY_COMID_VALIDCommandResponse 0x07FE0000 2 (B.replicate 10 1) (B.replicate 10 2) (B.replicate 10 3)
    bytes <- either (throwString . show) pure (encodeRecord r)
    B.length bytes `shouldBe` 46
    decodeRecordStrict bytes `shouldBe` Right (DecodedRecord r [] "")
