{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Readers.PinsSpec
Description : Test specifications for Hsed.Spec.Readers.Pins
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The three schemas of design example E8 on the committed extractions: Core
Table 39's two @The value of this field SHALL be …@ sentences (3.3.6.2.2,
3.3.6.2.3), Tables 42 and 43's three @The … field SHALL be set to …@ each
(3.3.6.4, 3.3.6.5), none for Table 44 (3.3.6.6, @W-PINS-NONE@ for the rule);
Opal Tables 2-6's bullets after "SHALL return the following", VU and "or any
version" kept as VU, "or larger" a minimum, nested bullets left out, and none
for Table 8, whose section lists its values in a table. The values the tests
expect are those the PDFs print, on the pages cited.
-}
module Hsed.Spec.Readers.PinsSpec (spec) where

import qualified RIO.Map                as Map
import           RIO
import           RIO.Directory          (doesFileExist)
import           Test.Hspec

import           Hsed.Spec.Anchor       (AnchorId (..))
import           Hsed.Spec.Check        (mkContext)
import           Hsed.Spec.Common       (ErrataSet (..))
import           Hsed.Spec.CoreIR       (withCore)
import           Hsed.Spec.DSL          (SomeBinding (..), feature)
import           Hsed.Spec.IR           (SpecIR (..), TableIR (..), readIR)
import           Hsed.Spec.Readers.Pins
import           Hsed.Spec.RuleDef      (BoundTable (..), modsOf, numberedTables)
import           Hsed.Spec.Scope        (tableSections)


-- | The pins of the section a numbered table lies in, as (schema, field, value, as printed).
pinsOf :: SpecIR -> Int -> Either Text [(PinSchema, Text, PinValue, Text)]
pinsOf ir n = case Map.lookup (TableId n) (numberedTables ir) >>= \t -> join (Map.lookup t.structPath (tableSections ir)) of
  Nothing  -> Left "no section"
  Just sec -> map (\p -> (pinSchema p, pinField p, pinValue p, pinText p)) <$> pinsInSection ir sec

spec :: Spec
spec = describe "Hsed.Spec.Readers.Pins" $ do
  describe "readPinValue" $
    it "reads a number, VU, a version the device chooses, a minimum, and anything else" $
      map readPinValue ["0x0001", "0x0C", "1", "VU", "0x1 or any version that supports the defined features in this SSC", "4 or larger", "0x0001 or larger", "**", "See section 3.1.1.3.1"]
        `shouldBe` [PinExact 1, PinExact 12, PinExact 1, PinVU, PinVU, PinAtLeast 4, PinAtLeast 1, PinOther, PinOther]

  withCore $ \ir -> do
    describe "Core" $ do
      it "T39 (3.3.6.2): the value of the Major and Minor Version fields, each named by its subsection's heading (PDF 68)" $
        pinsOf ir 39 `shouldBe` Right
          [ (CoreValue, "Data Structure Major Version", PinExact 0, "0x0000"), (CoreValue, "Data Structure Minor Version", PinExact 1, "0x0001") ]
      it "T42 (3.3.6.4): Feature Code, Version and Length set (PDF 70)" $
        pinsOf ir 42 `shouldBe` Right
          [ (CoreSet, "Feature Code", PinExact 1, "0x0001"), (CoreSet, "Version", PinExact 1, "0x01"), (CoreSet, "Length", PinExact 12, "0x0C") ]
      it "T43 (3.3.6.5): Feature Code, Version and Length set (PDF 71)" $
        pinsOf ir 43 `shouldBe` Right
          [ (CoreSet, "Feature Code", PinExact 2, "0x0002"), (CoreSet, "Version", PinExact 1, "0x01"), (CoreSet, "Length", PinExact 12, "0x0C") ]
      it "T44 (3.3.6.6) and T41 (3.3.6.3): none" $
        (pinsOf ir 44, pinsOf ir 41) `shouldBe` (Right [], Right [])
      it "says when the IR carries no words for a section's paragraphs" $
        pinsInSection ir "5.1.3" `shouldSatisfy` isLeft
      it "reads a bound table's pins through its context" $ do
        let ctx = mkContext Scott ir
            t = fromMaybe (error "T42") (Map.lookup (TableId 42) (numberedTables ir))
            bt = BoundTable (TableId 42) t t (SomeBinding feature) (modsOf (SomeBinding feature))
        fmap (map pinField) (tablePins ctx bt) `shouldBe` Right ["Feature Code", "Version", "Length"]
        fmap (map (\p -> (pinSection p, pinPage p))) (tablePins ctx bt) `shouldBe` Right (replicate 3 ("3.3.6.4", 69))

  describe "Opal" $
    it "reads Tables 2-6's bullets (PDF 19-23), and none for Table 8" $ do
      let path = "../spec/ir/opal-2.30.json"
      present <- doesFileExist path
      if not present then pendingWith (path <> " is not checked out") else do
        opal <- readIR path >>= either throwString pure
        let fields n = map (\(_, f, v, _) -> (f, v)) <$> pinsOf opal n
        fields 2 `shouldBe` Right [("Length of parameter data", PinVU), ("Data structure revision", PinVU), ("Vendor Specific", PinVU)]
        fields 3 `shouldBe` Right
          [ ("Feature Code", PinExact 1), ("Version", PinVU), ("Length", PinExact 12), ("ComID Mgmt Supported", PinVU)
          , ("Streaming Supported", PinExact 1), ("Buffer Mgmt Supported", PinVU), ("ACK/NACK Supported", PinVU)
          , ("Async Supported", PinVU), ("Sync Supported", PinExact 1) ]
        fields 4 `shouldBe` Right
          [ ("Feature Code", PinExact 2), ("Version", PinVU), ("Length", PinExact 12), ("HW Reset for LOR/DOR Supported", PinVU)
          , ("MBR Shadowing Not Supported", PinExact 0), ("MBR Done", PinOther), ("MBR Enabled", PinOther), ("Media Encryption", PinExact 1)
          , ("Locked", PinOther), ("Locking Enabled", PinOther), ("Locking Supported", PinExact 1) ]
        fields 5 `shouldBe` Right [("Feature Code", PinExact 3), ("Version", PinExact 1), ("Length", PinExact 28)]
        fields 6 `shouldBe` Right
          [ ("Feature Code", PinExact 0x0203), ("Feature Descriptor Version Number", PinVU), ("SSC Minor Version Number", PinOther)
          , ("Length", PinExact 16), ("Base ComID", PinVU), ("Number of ComIDs", PinAtLeast 1), ("Range Crossing Behavior", PinVU)
          , ("Number of Locking SP Admin Authorities", PinAtLeast 4), ("Number of Locking SP User Authorities", PinAtLeast 8)
          , ("Initial C_PIN_SID PIN Indicator", PinVU), ("Behavior of C_PIN_SID PIN upon TPer Revert", PinVU) ]
        fields 8 `shouldBe` Right []
        fmap (all (\(s, _, _, _) -> s == OpalBullets)) (pinsOf opal 3) `shouldBe` Right True
