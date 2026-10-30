{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.FieldTypeSpec
Description : Test specifications for the fieldType rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The fieldType rule on the committed Core extraction with the committed
annotations (design section 3.4): the three tables it binds from the doc
anchor, each type joined to its Type object (Table 34's eight fields taking
the ten bytes Table 33 gives them); no finding on the committed run, and
C-FIELD-TYPE made to fire by one edited cell (a type that names no Type
object, a range too wide for its type, a range that runs downwards); the
typed records the fields module declares.
-}
module Hsed.Spec.Rules.FieldTypeSpec (spec) where

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
import           Hsed.Spec.RuleDef       (Classification (..), RuleDef (..), btAnchor, numberedTables)
import           Hsed.Spec.Rules         (classify, explain)
import           Hsed.Spec.Rules.FieldType
import           Hsed.Spec.Shape         (measure)


-- | The tables a rule fires on anywhere in an IR, by number, whatever its scope.
firesOn :: RuleDef -> SpecIR -> [Int]
firesOn r ir = [ n | (TableId n, t) <- Map.toList (numberedTables ir), fires r sid t (measure sid t) ]
  where sid = SpecId ir.spec

spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx = mkContext Scott ir
      findings ir' = [ renderFinding f | f <- checkIR Nothing Scott ir', Cm.code f == "C-FIELD-TYPE" ]

  describe "the fieldType rule" $ do
    it "binds Tables 22, 23 and 34, and no other table, by the doc anchor's rule token" $ do
      [ n | (TableId n, RuleBound (RuleName "fieldType") _ _) <- Map.toList (classify ir) ] `shouldBe` [22, 23, 34]
      explain ir (TableId 34) `shouldBe` "fieldType (exemplar core T34)"
    it "fires on no other table of Core" $
      firesOn fieldTypeRule ir `shouldBe` [22, 23, 34]
    it "joins every field's type to its Type object" $
      [ (fName f, fTypeName f) | tf <- typeFieldsReads ctx, f <- tfRows tf, isNothing (fType f) ] `shouldBe` []
    it "sizes Table 34's fields from their types' Formats: the ten bytes of a time (Table 33)" $ do
      let date = [ f | tf <- typeFieldsReads ctx, btAnchor (tfBound tf) == TableId 34, f <- tfRows tf ]
      map fieldBytes date `shouldBe` map Just [2, 1, 1, 1, 1, 1, 2, 1]
      sum (mapMaybe fieldBytes date) `shouldBe` 10

  describe "C-FIELD-TYPE" $ do
    it "finds nothing on the committed run" $
      findings ir `shouldBe` []
    it "refuses a type that names no Type object, and the table is left out" $ do
      let broken = setCell "T23" 0 1 "uinteger_3" ir
      findings broken `shouldBe`
        [ "C-FIELD-TYPE core-2.01.pdf#page=46&T23: row 1 DataLength: the type \"uinteger_3\" names no Type object of the spec" ]
      newlyExcluded ir broken `shouldReturn` [(TableId 23, "C-FIELD-TYPE failed")]
    it "refuses a range wider than its type's bytes, and one that runs downwards" $
      findings (setCell "T34" 1 2 "12 to 1" (setCell "T34" 0 2 "1970 to 99999" ir)) `shouldBe`
        [ "C-FIELD-TYPE core-2.01.pdf#page=64&T34: row 1 Year: the range reaches 99999, more than the 2 bytes of uinteger_2 hold"
        , "C-FIELD-TYPE core-2.01.pdf#page=64&T34: row 2 Month: the range \"12 to 1\" runs downwards" ]
    it "refuses a range that is no value or range" $
      findings (setCell "T34" 7 2 "zero" ir) `shouldBe`
        [ "C-FIELD-TYPE core-2.01.pdf#page=64&T34: row 8 Reserved: the range \"zero\" is not a value or a range" ]

  describe "the generated module" $ do
    cr <- runIO (either (throwString . show) pure (compileIR Nothing Scott ir))
    let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Fields" (crModules cr)))
    it "declares each table's typed fields, named after its caption" $
      [ T.takeWhile (/= ' ') l | l <- lns, " :: [FieldDef]" `T.isSuffixOf` l ] `shouldBe`
        ["rSecureMessagingPacketPayloadField", "rSecureMessagingPacketPayloadSecureDataField", "rDateValues"]
    it "names each field's Type object as a TypeName, its UID from the type rows' module, and its range" $ do
      lns `shouldContain` ["import           Hsed.Core.TypeName  (TypeName (..))", "import           Hsed.Gen.Core.Types (ubytesType, uuinteger_1Type, uuinteger_2Type, uuintegerType)"]
      lns `shouldContain` [ "rDateValues =", "  [ FieldDef \"Year\" (TypeName \"uinteger_2\" (Just uuinteger_2Type)) (Just (Between 1970 9999))" ]
      lns `shouldContain` [ "  , FieldDef \"Reserved\" (TypeName \"uinteger_1\" (Just uuinteger_1Type)) (Just (One 0))", "  ]" ]
      lns `shouldContain` [ "  [ FieldDef \"IV\" (TypeName \"bytes\" (Just ubytesType)) Nothing" ]
