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
Opal Tables 2-6's bullets after "SHALL return the following", VU kept as VU,
"V or any version" as a version the device chooses with V kept (since M3), "or
larger" a minimum, nested bullets left out, and none for Table 8, whose
section lists its values in a table. A pin names a field by its name, else by
a unique word prefix of it (Opal 3.1.1.5's admin and user counts, M3). A value in hex with
an @h@ after it or digits grouped by an underscore; a bullet's value ended by
punctuation; a field heading the PDF numbers deeper than its outline, a list
item, naming a CoreValue pin. Every byte grid's section, in both IRs, is
word-complete and its pins read. The values the tests expect are those the
PDFs print, on the pages cited.
-}
module Hsed.Spec.Readers.PinsSpec (spec) where

import qualified RIO.Map                as Map
import qualified RIO.Text               as T
import           RIO                    hiding (words)
import           RIO.Directory          (doesFileExist)
import           Test.Hspec

import           Hsed.Spec.Anchor       (AnchorId (..))
import           Hsed.Spec.Check        (mkContext)
import           Hsed.Spec.Common       (ErrataSet (..))
import           Hsed.Spec.CoreIR       (withCore)
import           Hsed.Spec.DSL          (SomeBinding (..), feature)
import           Hsed.Spec.IR           (CellIR (..), ParaIR (..), SpecIR (..), TableIR (..), readIR)
import           Hsed.Spec.Readers.Pins
import           Hsed.Spec.RuleDef      (BoundTable (..), modsOf, numberedTables)
import           Hsed.Spec.Scope        (tableSections)
import           Hsed.Spec.Shape        (isGrid8)


-- | The pins of the section a numbered table lies in, as (schema, field, value, as printed).
pinsOf :: SpecIR -> Int -> Either Text [(PinSchema, Text, PinValue, Text)]
pinsOf ir n = case Map.lookup (TableId n) (numberedTables ir) >>= \t -> join (Map.lookup t.structPath (tableSections ir)) of
  Nothing  -> Left "no section"
  Just sec -> map (\p -> (pinSchema p, pinField p, pinValue p, pinText p)) <$> pinsInSection ir sec

spec :: Spec
spec = describe "Hsed.Spec.Readers.Pins" $ do
  describe "readPinValue" $ do
    it "reads a number, VU, a version the device chooses (keeping the version printed), a minimum, and anything else" $ do
      map readPinValue ["0x0001", "0x0C", "1", "VU", "0x1 or any version that supports the defined features in this SSC", "4 or larger", "0x0001 or larger", "**", "See section 3.1.1.3.1"]
        `shouldBe` [PinExact 1, PinExact 12, PinExact 1, PinVU, PinAnyVersion 1, PinAtLeast 4, PinAtLeast 1, PinOther, PinOther]
      map readPinValue ["0x00000001 or any version that supports the defined features in this SSC", "a later or any version"]
        `shouldBe` [PinAnyVersion 1, PinVU]
    it "reads hex with an h after it and digits grouped by underscores; a word is no hex number" $
      map readPinValue ["0001h", "0x0000_0004", "Each", "C000h"] `shouldBe` [PinExact 1, PinExact 4, PinOther, PinExact 0xC000]
    it "reads a number followed by its full stop, a parenthesis or a comma, but not by more words" $ do
      map readPinValue ["0x01.", "1.", "0x0001 (one)", "0x0001, the TPer feature", "0x0001 or 0x0002"]
        `shouldBe` [PinExact 1, PinExact 1, PinExact 1, PinExact 1, PinOther]
      map pinWord ["0x0001 (one)", "1.", "0x0001, the TPer feature", " VU "] `shouldBe` ["0x0001", "1", "0x0001", "VU"]

  describe "pinTarget" $ do
    let pin f = Pin OpalBullets f PinVU "VU" "3.1.1.5" 22
        names = [ "Number of Locking SP Admin Authorities Supported", "Number of Locking SP User Authorities Supported"
                , "Number of ComIDs", "ACK/NAK Supported" ]
    it "names the field named as the pin names it, whitespace and case aside" $
      map (pinTarget names . pin) ["Number of ComIDs", "number of  comids"] `shouldBe` replicate 2 (Just "Number of ComIDs")
    it "else the one field whose name's words start with the pin's (Opal 3.1.1.5 against Table 6, PDF 23)" $
      map (pinTarget names . pin) ["Number of Locking SP Admin Authorities", "Number of Locking SP User Authorities"]
        `shouldBe` [Just "Number of Locking SP Admin Authorities Supported", Just "Number of Locking SP User Authorities Supported"]
    it "names the field a reviewed spelling of the pin's names (3.1.1.2's ACK/NACK, Table 3's ACK/NAK, PDF 20)" $ do
      map (pinTarget names . pin) ["ACK/NACK Supported", "ack/nack  supported"] `shouldBe` replicate 2 (Just "ACK/NAK Supported")
      pinTarget (filter (/= "ACK/NAK Supported") names) (pin "ACK/NACK Supported") `shouldBe` Nothing
    it "names none where no field is so named, or several start so" $
      map (pinTarget names . pin) ["ACK/NACKS Supported", "Number of Locking SP", "Number", ""] `shouldBe` replicate 4 Nothing

  describe "every byte grid's section" $
    forM_ [("Core", "../spec/ir/core-2.01.json"), ("Opal", "../spec/ir/opal-2.30.json")] $ \(name, path) ->
      it ("is word-complete in " <> name <> "'s IR, and its pins read") $ do
        present <- doesFileExist path
        if not present then pendingWith (path <> " is not checked out") else do
          ir <- readIR path >>= either throwString pure
          let sections = Map.elems (Map.fromList
                [ (n, join (Map.lookup t.structPath (tableSections ir)))
                | (TableId n, t) <- Map.toList (numberedTables ir), isGrid8 t.nCols (map (.text) (concat (take 1 (t.headerRows <> t.rows)))) ])
          sections `shouldSatisfy` (not . null)
          [ s | Just s <- sections, s `notElem` ir.wordComplete ] `shouldBe` []
          [ (s, e) | Just s <- sections, Left e <- [pinsInSection ir s] ] `shouldBe` []
          length [ () | Nothing <- sections ] `shouldBe` 0

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
      it "names a CoreValue pin by the list item that numbers its field's heading deeper than the outline (3.2.3.4.2.1.2 Kind, PDF 45)" $ do
        -- Kind's paragraph made a CoreValue sentence: the section's heading is 3.2.3.4.2.1's, its field the list item's
        let kindPara :: ParaIR -> Bool
            kindPara p = p.section == Just "3.2.3.4.2.1" && fmap (take 4) p.words == Just ["This", "field", "identifies", "the"]
            edited = ir { paras = [ if kindPara p then p { words = Just (T.words "The value of this field SHALL be 0x8001.") } else p | p <- ir.paras ] }
        length (filter kindPara ir.paras) `shouldBe` 1
        fmap (map (\p -> (pinSchema p, pinField p, pinValue p))) (pinsInSection edited "3.2.3.4.2.1")
          `shouldBe` Right [(CoreValue, "Kind", PinExact 0x8001)]
        pinsInSection ir "3.2.3.4.2.1" `shouldBe` Right []
      it "reads a sentence's value to its full stop: a number with a parenthesis or a comma after it pins, one with more words is kept as printed" $ do
        let kindPara :: ParaIR -> Bool
            kindPara p = p.section == Just "3.2.3.4.2.1" && fmap (take 4) p.words == Just ["This", "field", "identifies", "the"]
            saying s = ir { paras = [ if kindPara p then p { words = Just (T.words s) } else p | p <- ir.paras ] }
            read' s = fmap (map (\p -> (pinValue p, pinText p))) (pinsInSection (saying s) "3.2.3.4.2.1")
        read' "The value of this field SHALL be 0x8001 (credit control). Next." `shouldBe` Right [(PinExact 0x8001, "0x8001 (credit control)")]
        read' "The value of this field SHALL be 0x8001, for credit control." `shouldBe` Right [(PinExact 0x8001, "0x8001, for credit control")]
        read' "The value of this field SHALL be 0x8001 or 0x0000." `shouldBe` Right [(PinOther, "0x8001 or 0x0000")]
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
            fields' ir' n = map (\(_, f, v, t) -> (f, v, t)) <$> pinsOf ir' n
        fields 2 `shouldBe` Right [("Length of parameter data", PinVU), ("Data structure revision", PinAnyVersion 1), ("Vendor Specific", PinVU)]
        fields 3 `shouldBe` Right
          [ ("Feature Code", PinExact 1), ("Version", PinAnyVersion 1), ("Length", PinExact 12), ("ComID Mgmt Supported", PinVU)
          , ("Streaming Supported", PinExact 1), ("Buffer Mgmt Supported", PinVU), ("ACK/NACK Supported", PinVU)
          , ("Async Supported", PinVU), ("Sync Supported", PinExact 1) ]
        fields 4 `shouldBe` Right
          [ ("Feature Code", PinExact 2), ("Version", PinAnyVersion 3), ("Length", PinExact 12), ("HW Reset for LOR/DOR Supported", PinVU)
          , ("MBR Shadowing Not Supported", PinExact 0), ("MBR Done", PinOther), ("MBR Enabled", PinOther), ("Media Encryption", PinExact 1)
          , ("Locked", PinOther), ("Locking Enabled", PinOther), ("Locking Supported", PinExact 1) ]
        fields 5 `shouldBe` Right [("Feature Code", PinExact 3), ("Version", PinExact 1), ("Length", PinExact 28)]
        fields 6 `shouldBe` Right
          [ ("Feature Code", PinExact 0x0203), ("Feature Descriptor Version Number", PinAnyVersion 2), ("SSC Minor Version Number", PinOther)
          , ("Length", PinExact 16), ("Base ComID", PinVU), ("Number of ComIDs", PinAtLeast 1), ("Range Crossing Behavior", PinVU)
          , ("Number of Locking SP Admin Authorities", PinAtLeast 4), ("Number of Locking SP User Authorities", PinAtLeast 8)
          , ("Initial C_PIN_SID PIN Indicator", PinVU), ("Behavior of C_PIN_SID PIN upon TPer Revert", PinVU) ]
        fields 8 `shouldBe` Right []
        fmap (all (\(s, _, _, _) -> s == OpalBullets)) (pinsOf opal 3) `shouldBe` Right True
        -- a bullet's value ended by punctuation reads as the value
        let punctuated = opal { paras = [ if p.words == Just ["Feature", "Code", "=", "0x0001"] && p.section == Just "3.1.1.2"
                                            then p { words = Just ["Feature", "Code", "=", "0x0001;"] } else p | p <- opal.paras ] }
        fmap (take 1) (fields' punctuated 3) `shouldBe` Right [("Feature Code", PinExact 1, "0x0001")]
