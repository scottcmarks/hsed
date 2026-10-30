{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.DocumentSpec
Description : Test specifications for Hsed.Spec.Document: LOT-VS-BODY and XREF-RESOLVES
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The two checks of the document as a whole on the committed extractions, and
on copies with one entry or reference edited: the List of Tables agrees with
the captions in Core and Opal (Table 185 is listed, as a two-line entry on PDF
15), and a dropped, repeated or stray entry, a changed title or page, is
reported; every Core table reference resolves to its table, and two say they
mean a table they do not name (@Table 179@ on PDF 116, @Table 134@ on PDF 121,
the second recorded in Scott's notes as meaning Table 136); a reference to no
table, a link landing elsewhere and a name the caption does not give are
reported, a reference to another document's table is not checked.
-}
module Hsed.Spec.DocumentSpec (spec) where

import qualified Data.Text         as DT
import qualified RIO.List          as L
import qualified RIO.Text          as T
import           RIO
import           RIO.Directory     (doesFileExist)
import           Test.Hspec

import           Hsed.Spec.Check   (checkIR)
import           Hsed.Spec.Common  (ErrataSet (..), Finding, renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.CoreIR  (withCore)
import           Hsed.Spec.Document
import           Hsed.Spec.IR


rendered :: [Finding] -> [Text]
rendered = map renderFinding

-- | Edit the references to Table n printed on a 1-based PDF page.
editXref :: Int -> Int -> (XrefIR -> XrefIR) -> SpecIR -> SpecIR
editXref n pdf f ir = ir { xrefs = map (\x -> if x.number == n && x.page + 1 == pdf then f x else x) ir.xrefs }

withSentence :: Text -> XrefIR -> XrefIR
withSentence s (XrefIR n p r sec _ tgt) = XrefIR n p r sec s tgt

withNumber :: Int -> XrefIR -> XrefIR
withNumber n (XrefIR _ p r sec s tgt) = XrefIR n p r sec s tgt

withTarget :: Maybe Int -> XrefIR -> XrefIR
withTarget tgt (XrefIR n p r sec s _) = XrefIR n p r sec s tgt

-- | Edit the List of Tables entries of Table n.
editLot :: Int -> (LotIR -> LotIR) -> SpecIR -> SpecIR
editLot n f ir = ir { lot = map (\e -> if e.number == n then f e else e) ir.lot }

-- | The committed Opal extraction, read once for a group of tests.
withOpal :: (SpecIR -> Spec) -> Spec
withOpal k = do
  let path = "../spec/ir/opal-2.30.json"
  present <- runIO (doesFileExist path)
  if present
    then runIO (readIR path) >>= either (\e -> it "reads the committed Opal IR" (expectationFailure e)) k
    else it "needs the committed Opal IR" (pendingWith (path <> " is not checked out"))

spec :: Spec
spec = do
  withCore $ \ir -> do
    describe "LOT-VS-BODY on Core" $ do
      it "finds the List of Tables equal to the captions, Table 185 listed (PDF 15, printed page 159)" $ do
        lotChecks ir `shouldBe` []
        [ (e.page + 1, e.printed) | e <- ir.lot, e.number == 185 ] `shouldBe` [(15, 159)]
      it "reports a table the List of Tables leaves out, at the table" $
        rendered (lotChecks ir { lot = filter ((/= 185) . (.number)) ir.lot }) `shouldBe`
          [ "LOT-VS-BODY core-2.01.pdf#page=176&T185: Table 185 (C_AES_128/C_AES_256 ResidualData Column Values After "
            <> "Encrypt/Decrypt/EncryptFinalize/DecryptFinalize, PDF 176) is not in the List of Tables" ]
      it "reports, at the entry, a title or a printed page the caption does not give" $ do
        rendered (lotChecks (editLot 75 (\(LotIR n _ p pg r) -> LotIR n "clock_tme" p pg r) ir)) `shouldBe`
          ["LOT-VS-BODY core-2.01.pdf#page=13&T75: the List of Tables calls Table 75 \"clock_tme\"; its caption on PDF 107 reads \"clock_time\""]
        rendered (lotChecks (editLot 75 (\(LotIR n t _ pg r) -> LotIR n t 91 pg r) ir)) `shouldBe`
          ["LOT-VS-BODY core-2.01.pdf#page=13&T75: the List of Tables gives page 91 for Table 75, which begins on page 90 (PDF 107)"]
      it "reads a title the same however it wraps" $
        lotChecks (editLot 185 (\(LotIR n t p pg r) -> LotIR n (DT.replace " " "\n " t) p pg r) ir) `shouldBe` []
      it "reports an entry listed twice, and one numbering no table" $ do
        let extra = [ LotIR 249 "Nonesuch" 290 e.page e.rect | e <- ir.lot, e.number == 248 ]
            twice = [ e | e <- ir.lot, e.number == 75 ]
        map Cm.anchor (lotChecks ir { lot = ir.lot <> twice <> extra }) `shouldBe` [Just "T75", Just "T249"]
        rendered (lotChecks ir { lot = ir.lot <> extra }) `shouldBe`
          ["LOT-VS-BODY core-2.01.pdf#page=17&T249: the List of Tables lists Table 249 (Nonesuch), which no caption in the body numbers"]
      it "compares nothing in an IR without a List of Tables" $
        lotChecks ir { lot = [] } `shouldBe` []

    describe "XREF-RESOLVES on Core" $ do
      let found = rendered (xrefChecks ir)
      it "reports \"Table 179\" at PDF 116, its own sentence first, then PDF 167's reference that may make it right" $
        found `shouldContain`
          [ "XREF-RESOLVES core-2.01.pdf#page=116&5.1.3.55: \"Table 179\" in "
            <> "\"The enumeration values and their associations defined in Table 179.\" names Secure Column Values "
            <> "(section 5.3.2.10.7), but section 5.1.3.55 defines messaging_type (Table 113) and this is no table of it; "
            <> "no other table's caption starts with messaging_type; yet the reference to Table 179 on PDF 167 "
            <> "(section 5.3.2.10.7) names messaging_type, which may make this one right" ]
      it "reports \"Table 134\" at PDF 121, where section 5.1.3.73's own enumeration is Table 136" $
        found `shouldContain`
          [ "XREF-RESOLVES core-2.01.pdf#page=121&5.1.3.73: \"Table 134\" in "
            <> "\"The enumeration values are associated as defined in Table 134.\" names symmetric_mode Enumeration Values "
            <> "(section 5.1.3.72), but section 5.1.3.73 defines symmetric_mode_media (Table 135) and this is no table of it; "
            <> "its own: Table 136 symmetric_mode_media Enumeration Values (section 5.1.3.73)" ]
      it "finds every other reference resolving to the table it names, by number, link and name" $
        [ (Cm.page f, Cm.anchor f) | f <- xrefChecks ir ] `shouldBe` [(Just 116, Just "5.1.3.55"), (Just 121, Just "5.1.3.73")]
      it "is among the findings of hsedc check, as Warnings" $ do
        let fromCheck = filter ((== "XREF-RESOLVES") . Cm.code) (checkIR Scott ir)
        L.sort (rendered fromCheck) `shouldBe` L.sort found
        map Cm.severity fromCheck `shouldSatisfy` all (== Cm.Warning)
      it "reports a number no table has" $
        rendered (xrefChecks (editXref 25 53 (withNumber 250) ir)) `shouldContain`
          ["XREF-RESOLVES core-2.01.pdf#page=53&3.3: \"Table 250\" names no numbered table: \"These sections address communications on protocols 0x01 and 0x02 only (see Table 25).\""]
      it "reports a link that lands off the table" $
        rendered (xrefChecks (editXref 166 137 (withTarget (Just 99)) ir)) `shouldContain`
          ["XREF-RESOLVES core-2.01.pdf#page=137&5.1.5: \"Table 166\" links to PDF 100, but Table 166 lies on PDF 137: \"Table 166 identifies the value associated with each of these status codes.\""]
      it "reports a name before (Table N) that the caption does not give, and accepts one leaving out a word" $ do
        let renamed = editXref 239 301 (withSentence "Authority UIDs (Table 239) – this descriptive table contains UIDs.") ir
        rendered (xrefChecks renamed) `shouldContain`
          ["XREF-RESOLVES core-2.01.pdf#page=301&6.3: the text calls Table 239 \"Authority UIDs\", its caption \"Special Purpose UIDs\": \"Authority UIDs (Table 239) – this descriptive table contains UIDs.\""]
        [ x.sentence | x <- ir.xrefs, x.number == 244 ] `shouldSatisfy` any ("Single Row Table UIDs (Table 244)" `T.isPrefixOf`)
      it "takes the name before (Table N) from the last clause break, not the sentence's start" $ do
        let clause = editXref 239 301 (withSentence "As the appendix lists them, Special Purpose UIDs (Table 239) – this descriptive table contains UIDs.") ir
        [ f | f <- xrefChecks clause, Cm.page f == Just 301 ] `shouldBe` []
      it "does not check a reference to another document's table" $ do
        let cited = editXref 25 53 (withNumber 250 . withSentence "See Table 250 of [2] for the protocols.") ir
        [ f | f <- xrefChecks cited, Cm.page f == Just 53 ] `shouldBe` []

  withOpal $ \ir -> do
    describe "LOT-VS-BODY on Opal" $
      it "finds the List of Tables equal to the captions" $ do
        length ir.lot `shouldBe` length [ () | t <- ir.tables, isJust t.tableNumber ]
        lotChecks ir `shouldBe` []
    describe "XREF-RESOLVES on Opal" $
      it "finds every reference resolving, Table 1 on PDF 12 included though no link covers it" $ do
        [ x.target | x <- ir.xrefs, x.number == 1 ] `shouldBe` [Nothing]
        xrefChecks ir `shouldBe` []
