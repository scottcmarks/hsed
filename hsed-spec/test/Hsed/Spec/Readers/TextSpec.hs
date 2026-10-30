{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Readers.TextSpec
Description : Test specifications for the text readers several rules share
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The two conventions generated names follow, names compared as headers are,
the one reading of a number (hex with @0x@ or an @h@ after it, underscores
between groups, decimal; its digits kept) and of a hex byte, and the one
printing of a number in hex at a width.
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

  describe "nameKey" $
    it "compares names whitespace-free and case-blind" $
      map nameKey ["Feature  Code", "ACK/NAK Supported", "MBR done"] `shouldBe` ["featurecode", "ack/naksupported", "mbrdone"]

  describe "parseNumber, readNumber and numberExpr" $ do
    it "reads hex with 0x or an h after it, underscores between groups, and decimal, keeping the hex digits" $
      map parseNumber ["0x0C", "0001h", "0x0000_0001", "12", "VU", "0x", "0x_1"]
        `shouldBe` [Just (Number 12 (Just 2)), Just (Number 1 (Just 4)), Just (Number 1 (Just 8)), Just (Number 12 Nothing), Nothing, Nothing, Nothing]
    it "reads as a number only what starts with a digit (a name such as Each is no hex eac with an h)" $
      map readNumber ["0x02", "12", "0001h", "Each", " 0x05 ", "Ah"] `shouldBe` [Just 2, Just 12, Just 1, Nothing, Just 5, Nothing]
    it "prints a number as it was printed: hex with its digits, or decimal" $
      map numberExpr [Number 1 (Just 4), Number 0x7ff (Just 4), Number 10 Nothing] `shouldBe` ["0x0001", "0x07ff", "10"]

  describe "hexDigits" $
    it "prints a number in hex with at least so many digits (a pin at its field's width)" $
      map (uncurry hexDigits) [(4, 1), (2, 12), (1, 5), (0, 0), (2, 0x123)] `shouldBe` ["0x0001", "0x0c", "0x5", "0x0", "0x123"]

  describe "readHexByte" $
    it "reads exactly two hex digits" $
      map readHexByte ["0B", "ff", "00", "0x", "1", "100", "xx"]
        `shouldBe` [Just 0x0B, Just 0xFF, Just 0, Nothing, Nothing, Nothing, Nothing]
