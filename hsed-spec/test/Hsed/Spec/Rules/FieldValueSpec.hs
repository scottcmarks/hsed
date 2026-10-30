{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.FieldValueSpec
Description : Test specifications for the fieldValue rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The VALUE cells the rule reads (hex bytes, a hex number, a name); then the rule
on the committed Core extraction with the committed annotations (design
section 3.4): the three command blocks it binds from the doc anchor, where it
fires across Core and Opal; no finding on the committed run, and C-FIELD-VALUE
made to fire by one edited cell; the constants the fields module declares.
-}
module Hsed.Spec.Rules.FieldValueSpec (spec) where

import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO
import           RIO.Directory             (doesFileExist)
import           Test.Hspec

import           Hsed.Core.Field           (FieldValue (..))
import           Hsed.Spec.Anchor          (AnchorId (..))
import           Hsed.Spec.Check           (checkIR)
import           Hsed.Spec.Common          (ErrataSet (..), RuleName (..), SpecId (..), renderFinding)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.Compile         (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR              (SpecIR (..), readIR)
import           Hsed.Spec.RuleDef         (Classification (..), RuleDef (..), numberedTables)
import           Hsed.Spec.Rules           (classify, explain)
import           Hsed.Spec.Rules.FieldValue
import           Hsed.Spec.Shape           (measure)


-- | The tables a rule fires on anywhere in an IR, by number, whatever its scope.
firesOn :: RuleDef -> SpecIR -> [Int]
firesOn r ir = [ n | (TableId n, t) <- Map.toList (numberedTables ir), fires r sid t (measure sid t) ]
  where sid = SpecId ir.spec

spec :: Spec
spec = do
  describe "parseFieldValue" $ do
    it "reads bytes printed in hex, and a hex number as its bytes" $ do
      map parseFieldValue ["02", "00 01", "0x0004", "0x2"] `shouldBe` map Constant [[0x02], [0x00, 0x01], [0x00, 0x04], [0x02]]
    it "reads anything else as a name, its spacing squeezed" $
      map parseFieldValue ["IF-RECV", "Allocated  ComID", "Request_ComID", "Non-zero", "0x"] `shouldBe`
        map Symbolic ["IF-RECV", "Allocated ComID", "Request_ComID", "Non-zero", "0x"]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let findings ir' = [ renderFinding f | f <- checkIR Scott ir', Cm.code f == "C-FIELD-VALUE" ]

    describe "the fieldValue rule" $ do
      it "binds the three command blocks, and no other table, by the doc anchor's rule token" $ do
        [ n | (TableId n, RuleBound (RuleName "fieldValue") _ _) <- Map.toList (classify ir) ] `shouldBe` [27, 29, 30]
        explain ir (TableId 27) `shouldBe` "fieldValue (exemplar core T27)"
      it "fires on no other table of Core" $
        firesOn fieldValueRule ir `shouldBe` [27, 29, 30]
      it "fires on Opal's TPER_RESET command block" $ do
        let path = "../spec/ir/opal-2.30.json"
        present <- doesFileExist path
        if not present then pendingWith (path <> " is not checked out") else do
          opal <- readIR path >>= either throwString pure
          firesOn fieldValueRule opal `shouldBe` [13]
      it "leaves a table unbound when a row names no field" $
        Map.lookup (TableId 29) (classify (setCell "T29" 1 0 " " ir)) `shouldBe` Just Unbound

    describe "C-FIELD-VALUE" $ do
      it "finds nothing on the committed run" $
        findings ir `shouldBe` []
      it "refuses a field named twice and a value left empty, and the table is left out" $ do
        let broken = setCell "T30" 3 1 "" (setCell "T30" 2 0 "Protocol ID" ir)
        findings broken `shouldBe`
          [ "C-FIELD-VALUE core-2.01.pdf#page=63&T30: row 3: the field Protocol ID is also row 2"
          , "C-FIELD-VALUE core-2.01.pdf#page=63&T30: row 4: the field ComID has no value" ]
        newlyExcluded ir broken `shouldReturn` [(TableId 30, "C-FIELD-VALUE failed")]

    describe "the generated module" $ do
      cr <- runIO (either (throwString . show) pure (compileIR Scott ir))
      let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Fields" (crModules cr)))
      it "declares each command block's fields with their constants, named after its caption" $ do
        [ T.takeWhile (/= ' ') l | l <- lns, " :: [FieldConst]" `T.isSuffixOf` l ] `shouldBe`
          ["vGET_COMIDCommandBlock", "vHANDLE_COMID_REQUESTCommandBlock", "vGET_COMID_RESPONSECommandBlock"]
        lns `shouldContain`
          [ "vGET_COMIDCommandBlock =", "  [ FieldConst \"Command\" (Symbolic \"IF-RECV\")"
          , "  , FieldConst \"Protocol ID\" (Constant [0x02])", "  , FieldConst \"Transfer Length\" (Constant [0x00, 0x01])"
          , "  , FieldConst \"ComID\" (Constant [0x00, 0x00])", "  ]" ]
        lns `shouldContain` ["  , FieldConst \"ComID\" (Symbolic \"Allocated ComID\")"]
