{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Readers.TextSpec
Description : Test specifications for the text readers several rules share
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The two conventions generated names follow, and a hex byte.
-}
module Hsed.Spec.Readers.TextSpec (spec) where

import           RIO
import           Test.Hspec

import           Hsed.Spec.Readers.Text


spec :: Spec
spec = do
  describe "titleIdent" $
    it "capitalises a title's words and joins them" $
      map titleIdent ["ComID Assignments", "GET_COMID Command Block", "Level 0 Discovery Header"]
        `shouldBe` ["ComIDAssignments", "GET_COMIDCommandBlock", "Level0DiscoveryHeader"]

  describe "lowerIdent" $ do
    it "joins a title's words and lowers its leading capitals" $
      map lowerIdent ["MethodID UIDs", "SPTemplates Table UIDs", "Table Default Rows", "XOR", "HMACInit", "Type Format"]
        `shouldBe` ["methodIDUIDs", "spTemplatesTableUIDs", "tableDefaultRows", "xor", "hmacInit", "typeFormat"]
    it "is the one convention of the generated values: a table description's name too" $
      map lowerIdent ["SPInfo", "MBRControl", "ACE", "SP", "TPerInfo", "C_PIN", "H_SHA_1"]
        `shouldBe` ["spInfo", "mbrControl", "ace", "sp", "tPerInfo", "c_PIN", "h_SHA_1"]

  describe "readHexByte" $
    it "reads exactly two hex digits" $
      map readHexByte ["0B", "ff", "00", "0x", "1", "100", "xx"]
        `shouldBe` [Just 0x0B, Just 0xFF, Just 0, Nothing, Nothing, Nothing, Nothing]
