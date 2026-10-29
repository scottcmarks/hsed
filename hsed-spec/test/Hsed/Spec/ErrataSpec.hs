{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.ErrataSpec
Description : Test specifications for Hsed.Spec.Errata
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The mechanics of a correction (one table's text, in order, reported per set),
and the cross-check of the committed annotations' corrections against Scott's
notes in @tcg/Core Spec errata.txt@: every @erratum scott [...]@ pair the
sidecar carries is one the notes record for that table. The notes are read
only here, never as corrections.
-}
module Hsed.Spec.ErrataSpec (spec) where

import qualified Data.Text         as T
import qualified RIO.List          as L
import qualified RIO.Map           as Map
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId (..))
import           Hsed.Spec.RuleDef (Mods (..), modsOf)
import           Hsed.Spec.Common  hiding (spec)
import           Hsed.Spec.CoreIR  (annotated, committedHsa, withCore)
import           Hsed.Spec.Errata
import           Hsed.Spec.IR      (CellIR (..), TableIR (..))
import           Hsed.Spec.Rules   (boundBinding, classify, numberedTables)


core :: SpecId
core = SpecId "core-2.01"

site :: Text -> Part -> Maybe Int -> Site
site = Site core

-- | Table 75's correction as its annotation carries it.
fraction :: Erratum
fraction = Erratum core "T75" "Fractoin" "Fraction" "annotation at core-2.01.pdf#page=107&T75"

format :: Part
format = DataCell 0 2 "Format"

t75Published, t75Scott :: Text
t75Published = "Struct_Type, Year, Month, Day, Hour, Minute, Seconds, Fractoin"
t75Scott     = "Struct_Type, Year, Month, Day, Hour, Minute, Seconds, Fraction"

spec :: Spec
spec = do
  describe "applyErrata" $ do
    it "corrects the text of its own table, in any part" $ do
      let (scott, fired) = applyErrata [fraction] (site "T75" format (Just 107)) t75Published
      scott `shouldBe` t75Scott
      fired `shouldBe` [fraction]
      fst (applyErrata [fraction] (site "T75" Caption Nothing) "Table 75 Fractoin") `shouldBe` "Table 75 Fraction"
    it "leaves another table's text alone" $
      applyErrata [fraction] (site "T78" format Nothing) t75Published `shouldBe` (t75Published, [])
    it "applies a table's corrections in order, each to what the one before left" $ do
      let chain = [ Erratum core "T75" "Fractoin" "Zq" "a", Erratum core "T75" "Zq" "Fraction" "a" ]
      applyErrata chain (site "T75" format Nothing) t75Published `shouldBe` (t75Scott, chain)

  describe "errata" $ do
    it "reports each correction made under scott, citing the annotation" $ do
      let (text, findings) = errata Scott [fraction] (site "T75" format (Just 107)) t75Published
      text `shouldBe` t75Scott
      map renderFinding findings
        `shouldBe` ["W-ERRATUM-APPLIED core-2.01.pdf#page=107&T75: Format \"Fractoin\" -> \"Fraction\" (annotation at core-2.01.pdf#page=107&T75)"]
      map severity findings `shouldBe` [Warning]
    it "leaves the text as printed under published and reports the defect the annotation records" $ do
      let (text, findings) = errata Published [fraction] (site "T75" format (Just 107)) t75Published
      text `shouldBe` t75Published
      map renderFinding findings
        `shouldBe` ["W-ERRATUM-KNOWN core-2.01.pdf#page=107&T75: published text left as printed; scott: Format \"Fractoin\" -> \"Fraction\" (annotation at core-2.01.pdf#page=107&T75)"]
    it "is silent where nothing is wrong, and without corrections under either set" $ do
      errata Scott [fraction] (site "T75" format Nothing) t75Scott `shouldBe` (t75Scott, [])
      errata Published [fraction] (site "T75" format Nothing) t75Scott `shouldBe` (t75Scott, [])
      errata Scott [] (site "T75" format Nothing) t75Published `shouldBe` (t75Published, [])

  describe "partName" $
    it "names a data cell by its column's header" $
      map partName [Caption, HeaderCell 0 1, DataCell 0 2 "Format", DataCell 3 1 "Associated  Value", DataCell 2 0 ""]
        `shouldBe` ["caption", "header", "Format", "Associated Value", "row 3 column 1"]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let t75 = Map.lookup (TableId 75) (numberedTables ir)

    describe "correctTable" $ do
      it "corrects every cell of the table and reports W-ERRATUM-UNUSED for a correction that fires nowhere" $
        case t75 of
          Nothing -> expectationFailure "no T75"
          Just t -> do
            let unused = Erratum core "T75" "Nonesuch" "X" "a"
                (t', fs) = correctTable Scott core "T75" (Just 107) [fraction, unused] t
            map (map (.text)) t'.rows `shouldBe` [["00 00 00 05 00 00 18 05", "clock_time", t75Scott]]
            map code fs `shouldBe` ["W-ERRATUM-APPLIED", "W-ERRATUM-UNUSED"]
      it "reads nothing differently under published" $
        case t75 of
          Nothing -> expectationFailure "no T75"
          Just t  -> fst (correctTable Published core "T75" (Just 107) [fraction] t) `shouldBe` t

    describe "the committed annotations against tcg/Core Spec errata.txt" $ do
      notes <- runIO (readFileUtf8 "../tcg/Core Spec errata.txt")
      let classes = classify ir
          pairs   = [ (n, from, into) | (TableId n, c) <- Map.toList classes, Just sb <- [boundBinding c]
                                       , (from, into) <- mErrata (modsOf sb) ]
          -- the note lines about one table: "Table N ..." or its section "5.1.3.(N-45) ..."
          about n = [ l | l <- T.lines notes
                        , any (`T.isPrefixOf` T.strip l) ["Table " <> tshow n <> " ", "5.1.3." <> tshow (n - 45) <> " "] ]
          quoted l = [ q | (i, q) <- zip [0 :: Int ..] (T.splitOn "\"" l), odd i ]
          noted (n, from, into) = any (\l -> case quoted l of
                                         [bad, good] -> bad `T.isInfixOf` from && good `T.isInfixOf` into
                                                        && T.replace bad good from == into
                                         _           -> False) (about n)
      it "carry Scott's corrections of Tables 75 and 101, and no others" $
        L.sort [ n | (n, _, _) <- pairs ] `shouldBe` [75, 101]
      it "each correct what Scott's notes record for that table" $
        filter (not . noted) pairs `shouldBe` []
