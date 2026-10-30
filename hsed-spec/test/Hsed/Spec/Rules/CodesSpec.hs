{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.CodesSpec
Description : Test specifications for the codes rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The code cells the rule reads (hex, @…h@, decimal, the four ways of printing a
range, @>value@, @All others@) and the code column it finds; then the rule on
the committed Core extraction with the committed annotations: the eight tables
it binds from the doc anchor (design section 3.4), where it fires across Core
and Opal, none of the tables the M2c rules will take; no finding on the
committed run, and C-CODE-VALUE, W-CODE-OVERLAP and C-CODE-TYPE made to fire by
one edited cell; the rows the codes module declares.
-}
module Hsed.Spec.Rules.CodesSpec (spec) where

import qualified RIO.Map               as Map
import qualified RIO.Text              as T
import           RIO
import           RIO.Directory         (doesFileExist)
import           Test.Hspec

import           Hsed.Core.Code        (Span (..))
import           Hsed.Spec.Anchor      (AnchorId (..))
import           Hsed.Spec.Check       (checkIR)
import           Hsed.Spec.Common      (ErrataSet (..), RuleName (..), SpecId (..), renderFinding)
import qualified Hsed.Spec.Common      as Cm
import           Hsed.Spec.Compile     (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR          (SpecIR (..), readIR)
import           Hsed.Spec.RuleDef     (Classification (..), RuleDef (..), numberedTables)
import           Hsed.Spec.Rules       (classify, explain)
import           Hsed.Spec.Rules.Codes
import           Hsed.Spec.Shape       (measure)


-- | The tables a rule fires on anywhere in an IR, by number, whatever its scope.
firesOn :: RuleDef -> SpecIR -> [Int]
firesOn r ir = [ n | (TableId n, t) <- Map.toList (numberedTables ir), fires r sid t (measure sid t) ]
  where sid = SpecId ir.spec

-- | The tables of the M2c rules (design 3.4: byteGrid, byteFieldValue, feature).
m2cTables :: [Int]
m2cTables = [17, 18, 20, 21, 38, 39, 41, 44, 212] <> [28, 31, 32, 33, 35, 36, 37, 236] <> [42, 43]

spec :: Spec
spec = do
  describe "parseCodeCell" $ do
    it "reads a value in hex, with an h suffix, or in decimal" $ do
      fmap cellSpan (parseCodeCell "0x0A") `shouldBe` Just (One 10)
      fmap cellSpan (parseCodeCell "0400h") `shouldBe` Just (One 0x400)
      fmap cellSpan (parseCodeCell "137") `shouldBe` Just (One 137)
    it "reads a range printed with a hyphen, an en dash, to or two dots" $ do
      fmap cellSpan (parseCodeCell "0x0002-0x07FF") `shouldBe` Just (Between 2 0x7ff)
      fmap cellSpan (parseCodeCell "0003h – 00FFh") `shouldBe` Just (Between 3 0xff)
      fmap cellSpan (parseCodeCell "0400h - BFFFh") `shouldBe` Just (Between 0x400 0xbfff)
      fmap cellSpan (parseCodeCell "10 to 23") `shouldBe` Just (Between 10 23)
      fmap cellSpan (parseCodeCell "1..65534") `shouldBe` Just (Between 1 65534)
    it "reads every value above one, and All others" $ do
      fmap cellSpan (parseCodeCell ">0x01") `shouldBe` Just (Above 1)
      fmap cellSpan (parseCodeCell "All  others") `shouldBe` Just Others
    it "reads no name, meaning or empty cell" $
      map parseCodeCell ["Data", "See [6]", "", "0x", "12h3"] `shouldBe` replicate 5 Nothing
    it "prints the span as Haskell, its numbers as printed" $
      map (fmap codeCellExpr . parseCodeCell) ["0x0002-0x07FF", "0003h – 00FFh", "10 to 23", ">0x00", "All others"]
        `shouldBe` map Just ["Between 0x0002 0x07ff", "Between 0x0003 0x00ff", "Between 10 23", "Above 0x00", "Others"]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let findings ir' = [ renderFinding f | f <- checkIR Nothing Scott ir', Cm.code f `elem` ["C-CODE-VALUE", "W-CODE-OVERLAP", "C-CODE-TYPE"] ]

    describe "the codes rule" $ do
      it "binds the eight code tables of design 3.4, and no other table, by the doc anchor's rule token" $ do
        [ n | (TableId n, RuleBound (RuleName "codes") _ _) <- Map.toList (classify ir) ] `shouldBe` [15, 16, 19, 25, 26, 40, 179, 235]
        explain ir (TableId 25) `shouldBe` "codes (exemplar core T25)"
      it "fires on no other table of Core: not on Table 87, an enumeration's values, which the shape accepts" $
        firesOn codesRule ir `shouldBe` [15, 16, 19, 25, 26, 40, 179, 235]
      it "fires on none of the tables of the M2c rules" $
        filter (`elem` m2cTables) (firesOn codesRule ir) `shouldBe` []
      it "finds the code column: the second in Table 19, the first elsewhere" $
        [ (n, codeColumn t) | (TableId n, t) <- Map.toList (numberedTables ir), n `elem` [15, 19, 25, 179] ]
          `shouldBe` [(15, Just 0), (19, Just 1), (25, Just 0), (179, Just 0)]
      it "fires on Opal's five code tables of design 3.4" $ do
        let path = "../spec/ir/opal-2.30.json"
        present <- doesFileExist path
        if not present then pendingWith (path <> " is not checked out") else do
          opal <- readIR path >>= either throwString pure
          firesOn codesRule opal `shouldBe` [7, 10, 11, 12, 14]

    describe "the checks" $ do
      it "find nothing on the committed run" $
        findings ir `shouldBe` []
      it "C-CODE-VALUE: a range that runs downwards, and the table left out" $ do
        let broken = setCell "T26" 2 0 "0x07FF-0x0002" ir
        findings broken `shouldBe`
          [ "C-CODE-VALUE core-2.01.pdf#page=55&T26: row 3: the range Between 0x07ff 0x0002 runs downwards" ]
        newlyExcluded ir broken `shouldReturn` [(TableId 26, "C-CODE-VALUE or C-CODE-TYPE failed")]
      it "W-CODE-OVERLAP: two rows assigning one value, the first kept" $
        findings (setCell "T26" 2 0 "0x0001-0x07FF" ir) `shouldBe`
          [ "W-CODE-OVERLAP core-2.01.pdf#page=55&T26: row 3 (Between 0x0001 0x07ff) assigns values row 2 (One 0x0001) assigns too; the first is kept" ]
      it "W-CODE-OVERLAP: a value above a lower bound" $
        findings (setCell "T16" 1 0 "0x02" ir) `shouldBe`
          [ "W-CODE-OVERLAP core-2.01.pdf#page=38&T16: row 3 (Above 0x01) assigns values row 2 (One 0x02) assigns too; the first is kept" ]
      it "C-CODE-TYPE: a code column's type that names no Type object" $
        findings (setHeaderCell "T15" 0 0 "Start Transaction Status Code (uint)" ir) `shouldBe`
          [ "C-CODE-TYPE core-2.01.pdf#page=37&T15: the code column's header gives its values the type \"uint\", which names no Type object of the spec" ]
      it "leaves unbound a table one of whose code cells is no code" $
        Map.lookup (TableId 235) (classify (setCell "T235" 3 0 "three" ir)) `shouldBe` Just Unbound

    describe "the generated module" $ do
      cr <- runIO (either (throwString . show) pure (compileIR Nothing Scott ir))
      let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Codes" (crModules cr)))
      it "declares each table's rows, named after its caption" $
        [ T.takeWhile (/= ' ') l | l <- lns, " :: [CodeRow]" `T.isSuffixOf` l ] `shouldBe`
          [ "cStartTransactionStatusCodes", "cEndTransactionStatusCodes", "cSubpacketTypes", "cProtocolIDs"
          , "cComIDAssignments", "cFeatureCodes", "cStatusCodes", "cSecureColumnValues", "cLogKindColumnValues" ]
      it "prints each row's span as printed and its other cells in column order" $ do
        lns `shouldContain` [ "cComIDAssignments :: [CodeRow]", "cComIDAssignments =", "  [ CodeRow (One 0x0000) [\"Reserved\"]"
                            , "  , CodeRow (One 0x0001) [\"Level 0 Device Discovery\"]", "  , CodeRow (Between 0x0002 0x07ff) [\"Reserved for TCG\"]" ]
        lns `shouldContain` [ "  , CodeRow (One 0x8001) [\"Credit Control\"]" ]
        lns `shouldContain` [ "  , CodeRow Others [\"See [6]\"]" ]
        lns `shouldContain` [ "  , CodeRow (Above 0x00) [\"Reserved\"]" ]
        lns `shouldContain` [ "  , CodeRow (Between 0x0003 0x00ff) [\"Reserved\", \"\"]" ]
        lns `shouldContain` [ "  , CodeRow (Between 137 255) [\"RESERVED\", \"Integrity and Confidentiality\"]" ]
        lns `shouldContain` [ "  , CodeRow (Between 10 23) [\"reserved\"]" ]
      it "names the code column and the others on the Haddock line" $
        lns `shouldSatisfy` any (\l -> "-- | core-2.01 T19 Subpacket Types (PDF 43 / p.26" `T.isPrefixOf` l
                                      && ": by Kind Field Value; Subpacket Type" `T.isSuffixOf` l)
