{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.ErrataSpec
Description : Test specifications for Hsed.Spec.Errata
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental
-}
module Hsed.Spec.ErrataSpec (spec) where

import qualified Data.Text        as T
import           RIO
import qualified RIO.Set          as Set
import           Test.Hspec

import           Hsed.Spec.Common hiding (spec)
import           Hsed.Spec.Errata


core :: SpecId
core = SpecId "core-2.01"

site :: Text -> Part -> Maybe Int -> Site
site = Site core

t75Published, t75Scott :: Text
t75Published = "Struct_Type, Year, Month, Day, Hour, Minute, Seconds, Fractoin"
t75Scott     = "Struct_Type, Year, Month, Day, Hour, Minute, Seconds, Fraction"

spec :: Spec
spec = do
  describe "corrections" $ do
    it "are empty for the published text" $
      corrections Published `shouldBe` []
    it "hold Scott's eleven corrections to Core 2.01, each to one part of one table" $ do
      length (corrections Scott) `shouldBe` 11
      map errSpec (corrections Scott) `shouldSatisfy` all (== core)
      Set.fromList (map (\e -> (errTable e, errPart e)) (corrections Scott))
        `shouldBe` Set.fromList
          ([("T75", FormatCell), ("T101", RowLabel), ("T61", HeaderCell), ("T128", HeaderCell)]
           <> [ (t, Caption) | t <- ["T72", "T76", "T112", "T131", "T140", "T142", "T144"] ])
    it "change something and say where they come from" $ do
      corrections Scott `shouldSatisfy` all (\e -> errFrom e /= errTo e)
      corrections Scott `shouldSatisfy` all (not . T.null . errSource)

  describe "applyErrata" $ do
    it "corrects Table 75's Format under scott and leaves it under published" $ do
      let (scott, fired) = applyErrata Scott (site "T75" FormatCell (Just 107)) t75Published
      scott `shouldBe` t75Scott
      map (\e -> (errFrom e, errTo e)) fired `shouldBe` [("Fractoin", "Fraction")]
      applyErrata Published (site "T75" FormatCell (Just 107)) t75Published `shouldBe` (t75Published, [])
    it "fires only in its own table and part" $ do
      applyErrata Scott (site "T78" FormatCell Nothing) t75Published `shouldBe` (t75Published, [])
      applyErrata Scott (site "T75" Caption Nothing) t75Published `shouldBe` (t75Published, [])
      fst (applyErrata Scott (site "T54" HeaderCell Nothing) "Set Value") `shouldBe` "Set Value"
      fst (applyErrata Scott (site "T128" HeaderCell Nothing) "Set Value") `shouldBe` "Enumeration Value"
      fst (applyErrata Scott (site "T61" HeaderCell Nothing) "Operator") `shouldBe` "Associated Value"
    it "corrects the label and the seven captions" $ do
      fst (applyErrata Scott (site "T101" RowLabel Nothing) "Verify Eror") `shouldBe` "Verify Error"
      fst (applyErrata Scott (site "T101" RowLabel Nothing) "Read Error") `shouldBe` "Read Error"
      map (\(t, n, noun) -> fst (applyErrata Scott (site t Caption Nothing) ("Table " <> n <> " " <> noun <> "_object _ref"))) captions
        `shouldBe` map (\(_, n, noun) -> "Table " <> n <> " " <> noun <> "_object_ref") captions
      fst (applyErrata Scott (site "T76" Caption Nothing) "Table 76 Certificates_object _ref")
        `shouldBe` "Table 76 Certificates_object _ref"

  describe "errata" $ do
    it "reports each correction applied under scott" $ do
      let (text, findings) = errata Scott (site "T75" FormatCell (Just 107)) t75Published
      text `shouldBe` t75Scott
      map renderFinding findings
        `shouldBe` ["W-ERRATUM-APPLIED core-2.01.pdf#page=107&T75: Format \"Fractoin\" -> \"Fraction\" (tcg/Core Spec errata.txt: Table 75 \"Fractoin\" should be \"Fraction\")"]
      map severity findings `shouldBe` [Warning]
    it "reports the defect left in under published" $ do
      let (text, findings) = errata Published (site "T101" RowLabel (Just 113)) "Verify Eror"
      text `shouldBe` "Verify Eror"
      map renderFinding findings
        `shouldBe` ["W-ERRATUM-KNOWN core-2.01.pdf#page=113&T101: published text left as printed; scott: label \"Verify Eror\" -> \"Verify Error\""]
    it "is silent where nothing is wrong" $ do
      errata Scott (site "T75" FormatCell Nothing) t75Scott `shouldBe` (t75Scott, [])
      errata Published (site "T75" FormatCell Nothing) t75Scott `shouldBe` (t75Scott, [])
  where
    captions = [ ("T72", "72", "Certificates"), ("T76", "76", "Column"), ("T112", "112", "MethodID"), ("T131", "131", "SPTemplates")
               , ("T140", "140", "Table"), ("T142", "142", "Template"), ("T144", "144", "Type") ]
