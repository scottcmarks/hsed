{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.StatusCodesSpec
Description : Test specifications for the statusCodes rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The statusCodes rule on the committed Core extraction with the committed
annotations (design section 3.4): it binds Table 166 alone from the doc
anchor; its 19 rows, three of them OBSOLETE, give the pattern names with
each OBSOLETE suffixed with its value (a Note; design 13.15); the
subsections 5.1.5.1-5.1.5.16 describe every status but OBSOLETE (a Note);
C-STATUS-CODE and W-STATUS-SECTION made to fire by one edited cell; the
type, patterns and rows the codes module declares. The comparison with
hsed-core's codec is in hsed's EquivSpec.
-}
module Hsed.Spec.Rules.StatusCodesSpec (spec) where

import qualified RIO.Map                 as Map
import qualified RIO.Text                as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor        (AnchorId (..))
import           Hsed.Spec.Check         (checkIR, mkContext)
import           Hsed.Spec.Common        (ErrataSet (..), RuleName (..), SpecId (..), renderFinding)
import qualified Hsed.Spec.Common        as Cm
import           Hsed.Spec.Compile       (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR            (SpecIR (..))
import           Hsed.Spec.RuleDef       (Classification (..), RuleDef (..), numberedTables)
import           Hsed.Spec.Rules         (classify, explain)
import           Hsed.Spec.Rules.StatusCodes
import           Hsed.Spec.Shape         (measure)


-- | The tables a rule fires on anywhere in an IR, by number, whatever its scope.
firesOn :: RuleDef -> SpecIR -> [Int]
firesOn r ir = [ n | (TableId n, t) <- Map.toList (numberedTables ir), fires r sid t (measure sid t) ]
  where sid = SpecId ir.spec

spec :: Spec
spec = do
  describe "statusTypeName" $
    it "makes the caption's last word singular" $
      map statusTypeName ["Status Codes", "Protocol IDs", "Method Status", "Status Class"] `shouldBe` ["StatusCode", "ProtocolID", "MethodStatus", "StatusClass"]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx = mkContext Scott ir
        findings ir' = [ renderFinding f | f <- checkIR Scott ir'
                       , Cm.code f `elem` ["C-STATUS-CODE", "N-STATUS-NAME-SUFFIXED", "W-STATUS-SECTION", "N-STATUS-UNDESCRIBED"] ]

    describe "the statusCodes rule" $ do
      it "binds Table 166, and no other table, by the doc anchor's rule token" $ do
        [ n | (TableId n, RuleBound (RuleName "statusCodes") _ _) <- Map.toList (classify ir) ] `shouldBe` [166]
        explain ir (TableId 166) `shouldBe` "statusCodes (exemplar core T166)"
      it "fires on no other table of Core" $
        firesOn statusCodesRule ir `shouldBe` [166]
      it "reads 19 statuses, three of them OBSOLETE, each suffixed with its value" $ do
        let rows = concatMap srRows (statusReads ctx)
        length rows `shouldBe` 19
        [ n | (r, n) <- patternNames rows, stName r == "OBSOLETE" ] `shouldBe` ["OBSOLETE_2", "OBSOLETE_13", "OBSOLETE_14"]
        [ n | (_, n) <- patternNames rows ] `shouldSatisfy` \ns -> take 3 ns == ["SUCCESS", "NOT_AUTHORIZED", "OBSOLETE_2"] && drop 18 ns == ["FAIL"]
      it "leaves the table unbound when a value cell holds no single value" $
        Map.lookup (TableId 166) (classify (setCell "T166" 11 1 "0x0C-0x0D" ir)) `shouldBe` Just Unbound

    describe "the checks" $ do
      it "find on the committed run the suffixed OBSOLETEs and that no subsection describes them, nothing else" $
        findings ir `shouldBe`
          [ "N-STATUS-NAME-SUFFIXED core-2.01.pdf#page=137&T166: the name OBSOLETE is given to 0x02, 0x0d, 0x0e: the patterns are OBSOLETE_2, OBSOLETE_13, OBSOLETE_14"
          , "N-STATUS-UNDESCRIBED core-2.01.pdf#page=137&T166: no subsection of 5.1.5 describes OBSOLETE (0x02, 0x0d, 0x0e)" ]
      it "C-STATUS-CODE: a value given twice, and the table left out" $ do
        let broken = setCell "T166" 11 1 "0x0A" ir
        filter ("C-STATUS-CODE" `T.isPrefixOf`) (findings broken) `shouldBe`
          [ "C-STATUS-CODE core-2.01.pdf#page=137&T166: row 12 INVALID_PARAMETER: the value 0x0a is also that of row 11 INSUFFICIENT_ROWS" ]
        newlyExcluded ir broken `shouldReturn` [(TableId 166, "C-STATUS-CODE failed")]
      it "C-STATUS-CODE: a name that cannot name a pattern" $
        filter ("C-STATUS-CODE" `T.isPrefixOf`) (findings (setCell "T166" 3 0 "sp busy" ir)) `shouldBe`
          [ "C-STATUS-CODE core-2.01.pdf#page=137&T166: row 4: the name \"sp busy\" cannot name a pattern (an identifier starting with a capital letter)" ]
      it "W-STATUS-SECTION: a subsection describing a status the table does not give, which is then undescribed" $
        filter (not . ("N-STATUS-NAME-SUFFIXED" `T.isPrefixOf`)) (findings (setCell "T166" 3 0 "SP_BUSIED" ir)) `shouldBe`
          [ "W-STATUS-SECTION core-2.01.pdf#page=138&5.1.5.3: section 5.1.5.3 SP_BUSY describes a status Table 166 does not give"
          , "N-STATUS-UNDESCRIBED core-2.01.pdf#page=137&T166: no subsection of 5.1.5 describes OBSOLETE (0x02, 0x0d, 0x0e)"
          , "N-STATUS-UNDESCRIBED core-2.01.pdf#page=137&T166: no subsection of 5.1.5 describes SP_BUSIED (0x03)" ]

    describe "the generated module" $ do
      cr <- runIO (either (throwString . show) pure (compileIR Scott ir))
      let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Codes" (crModules cr)))
      it "declares the statuses' type, open to any value the wire carries" $
        lns `shouldContain` ["newtype StatusCode = StatusCode Natural", "  deriving (Eq, Ord, Show)"]
      it "declares a pattern per row, named as the table names the value, its value as printed" $ do
        length [ l | l <- lns, "pattern " `T.isPrefixOf` l, " :: StatusCode" `T.isSuffixOf` l ] `shouldBe` 19
        lns `shouldContain` ["pattern NOT_AUTHORIZED :: StatusCode", "pattern NOT_AUTHORIZED = StatusCode 0x01"]
        lns `shouldContain` ["pattern OBSOLETE_2 :: StatusCode", "pattern OBSOLETE_2 = StatusCode 0x02"]
        lns `shouldContain` ["pattern OBSOLETE_13 :: StatusCode", "pattern OBSOLETE_13 = StatusCode 0x0d"]
        lns `shouldContain` ["pattern FAIL :: StatusCode", "pattern FAIL = StatusCode 0x3f"]
      it "declares the table's rows, each value with its name" $
        lns `shouldContain` ["cStatusCodes :: [CodeRow]", "cStatusCodes =", "  [ CodeRow (One 0x00) [\"SUCCESS\"]"]
      it "cites the table on every pattern" $
        lns `shouldContain` ["-- | core-2.01 T166 Status Codes (PDF 137 / p.120, ord 168, sha256:1196fb53069b), row 12: INVALID_PARAMETER"]
