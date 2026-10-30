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
constants the VALUE column pins. Table 236 (section 5.8.4.2) is generated as
printed, byte 8 in no row (PDF 298), and the runtime refuses it until an
erratum names that byte.
-}
module Hsed.Gen.Core.Layouts.ByteFieldValueSpec (spec) where

import qualified RIO.ByteString        as B
import           RIO
import           Test.Hspec

import           Hsed.Core.Layout      (Content (..), Decoded (..), Layout (..), LayoutError (..), TileProblem (..), decode,
                                        decodeStrict, encode, tiles)
import           Hsed.Gen.Core.Layouts (get_COMIDPayload, noResponseAvailable, stack_RESETCommandRequest,
                                        stack_RESETCommandResponse, stack_RESETPending, systemLogEntryStructure,
                                        verify_COMID_VALIDCommandResponse, verify_COMID_VALIDRequest)


spec :: Spec
spec = describe "the byteFieldValue layouts (core-2.01 T28, T31-T33, T35-T37)" $ do
  let layouts = [ get_COMIDPayload, noResponseAvailable, verify_COMID_VALIDRequest, verify_COMID_VALIDCommandResponse
                , stack_RESETCommandRequest, stack_RESETCommandResponse, stack_RESETPending ]
  it "tile their bytes" $
    map tiles layouts `shouldBe` replicate 7 True
  it "are as long as their tables' fixed parts (PDF 61-66)" $
    map layoutSize layouts `shouldBe` [4, 12, 8, 46, 8, 16, 12]
  it "STACK_RESET Command Request: the Request Code written as its pin, 00 00 00 02 (T35, PDF 65)" $
    encode stack_RESETCommandRequest [("Extended ComID", Number 0x07FE0000)] `shouldBe` Right (B.pack [0x07, 0xFE, 0, 0, 0, 0, 0, 2])
  it "No Response Available: the rest of the transfer its tail (T31, PDF 63)" $
    fmap decodedFields (decode noResponseAvailable (B.pack ([0x07, 0xFE, 0, 0] <> replicate 12 0)))
      `shouldBe` Right [ ("Extended ComID", Number 0x07FE0000), ("Request Code", Number 0)
                       , ("Available Data Length in bytes", Number 0), ("Reserved", Octets (B.replicate 4 0)) ]
  it "STACK_RESET Command Response: a Request Code other than 2 refused when read strictly (T36, PDF 66)" $
    fmap decodedFields (decodeStrict stack_RESETCommandResponse (B.pack ([0x07, 0xFE, 0, 0, 0, 0, 0, 1, 0, 0, 0, 4] <> replicate 4 0)))
      `shouldSatisfy` isLeft
  it "System Log Entry Structure: refused, byte 8 in no field, until an erratum names it (T236, PDF 298)" $
    encode systemLogEntryStructure [] `shouldBe` Left (NotTiled [Gap 64 71])
