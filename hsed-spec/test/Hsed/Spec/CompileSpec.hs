{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Hsed.Spec.CompileSpec
Description : Test specifications for Hsed.Spec.Compile
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@hsedc compile@ on the committed Core extraction with the committed annotations:
the 100 type rows and 19 enumerations of section 5.1.3, their provenance (the
cell hash the manifest records), the module under each errata set, a table
left out with W-GEN-EXCLUDED, and the committed @hsed/gen@ module exactly what
the committed annotations give.
-}
module Hsed.Spec.CompileSpec (spec) where

import qualified RIO.Text          as T
import           RIO
import           Test.Hspec

import           Hsed.Core.Types   (Operand (..), TypeDef (..), TypeRef (..))
import           Hsed.Spec.Anchor  (AnchorId (..))
import           Hsed.Spec.Common  (ErrataSet (..), isError, renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.Compile
import           Hsed.Spec.CoreIR
import qualified Hsed.Spec.Gen     as Gen
import           Hsed.Spec.IR      (SpecIR (..))


rowsOf :: CompileResult -> [Gen.TypeRowDecl]
rowsOf cr = [ r | Gen.TypeRow r <- crDecls cr ]

enumsOf :: CompileResult -> [Gen.EnumDecl]
enumsOf cr = [ e | Gen.Enum e <- crDecls cr ]

spec :: Spec
spec = do
  describe "generatedModuleName" $
    it "names the module and its file after the spec" $ do
      generatedModuleName core `shouldBe` "Hsed.Gen.Core.Types"
      generatedModulePath "hsed/gen" core `shouldBe` "hsed/gen/Hsed/Gen/Core/Types.hs"

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let compiled es = either (throwString . T.unpack . renderFinding) pure (compileIR es ir)

    describe "compileIR --errata scott" $ do
      cr <- runIO (compiled Scott)
      it "declares the 100 type rows and the 19 enumerations of section 5.1.3, leaving none out" $ do
        length (rowsOf cr) `shouldBe` 100
        length (enumsOf cr) `shouldBe` 19
        crExcluded cr `shouldBe` []
        filter isError (crFindings cr) `shouldBe` []
      it "records the provenance of design example E2, with the manifest's cell hash" $ do
        let p46 = [ r.provenance | r <- rowsOf cr, r.name == "AC_element" ]
            p75 = [ r.provenance | r <- rowsOf cr, r.name == "clock_time" ]
        map (\p -> (p.tableId, p.pdfPage1, p.printedPage, p.ordinal, p.structPath)) p46 `shouldBe` [("T46", 100, 83, 48, "0/1288")]
        map (.cellsHash) p75 `shouldBe` ["5a5e8787c29a7124035961d972b7b81ab1635117d2d207a0942f1a9c43291655"]
      it "reads T46 as design example E2 does" $
        [ (r.uidBytes, Gen.joinFormat r.formatText, r.typeDef) | r <- rowsOf cr, r.name == "AC_element" ]
          `shouldBe` [([0, 0, 0, 5, 0, 0, 8, 1], "List_Type,*,ACE_expression", Just (ListType Star (Ref "ACE_expression")))]
      it "corrects T75 and types it" $
        [ r.typeDef | r <- rowsOf cr, r.name == "clock_time" ]
          `shouldBe` [Just (StructType (map Ref ["Year", "Month", "Day", "Hour", "Minute", "Seconds", "Fraction"]))]
      it "reads T52 as design example E4 does, with the range of T51" $
        [ (e.ctors, e.range, e.typeUID) | e <- enumsOf cr, e.name == "adv_key_mode" ]
          `shouldBe` [([("Wait for AdvKey_Req", 0), ("Auto-advance keys", 1)] <> [ ("Reserved", v) | v <- [2 .. 7] ], (0, 7), [0, 0, 0, 5, 0, 0, 4, 0x0F])]
      it "takes the range of reencrypt_state from its Format, starting at 1" $
        [ e.range | e <- enumsOf cr, e.name == "reencrypt_state" ] `shouldBe` [(1, 16)]
      it "writes the committed hsed/gen/Hsed/Gen/Core/Types.hs" $ do
        committed <- readFileUtf8 "../hsed/gen/Hsed/Gen/Core/Types.hs"
        crModuleText cr `shouldBe` committed
      it "writes the same text whatever the order of the IR's tables" $ do
        cr' <- either (throwString . T.unpack . renderFinding) pure (compileIR Scott ir { tables = reverse ir.tables })
        crModuleText cr' `shouldBe` crModuleText cr

    describe "compileIR --errata published" $ do
      cr <- runIO (compiled Published)
      it "keeps Fractoin, gives T75 no TypeDef and reports E-FMT-RESOLVE" $ do
        [ (Gen.joinFormat r.formatText, r.typeDef) | r <- rowsOf cr, r.name == "clock_time" ]
          `shouldBe` [("Struct_Type,Year,Month,Day,Hour,Minute,Seconds,Fractoin", Nothing)]
        T.lines (crModuleText cr) `shouldSatisfy` elem "-- tclock_time: no TypeDef, the Format did not parse under this errata set"
        map renderFinding (filter isError (crFindings cr))
          `shouldBe` ["E-FMT-RESOLVE core-2.01.pdf#page=107&T75: unresolved type name Fractoin (nearest: Fraction, T84)"]
      it "names T101's value as printed" $
        [ e.ctors | e <- enumsOf cr, e.name == "last_reenc_stat" ] `shouldSatisfy` all (elem ("Verify Eror", 3))

    describe "a table that cannot be generated" $
      it "is left out with W-GEN-EXCLUDED, its Error among the findings" $ do
        cr <- either (throwString . T.unpack . renderFinding) pure (compileIR Scott (setCell "T52" 2 0 "2-8" ir))
        crExcluded cr `shouldBe` [(TableId 52, "C-ENUM-RANGE failed")]
        [ Cm.code f | f <- crFindings cr, Cm.anchor f == Just "T52" ] `shouldBe` ["C-ENUM-RANGE", "W-GEN-EXCLUDED"]
        length (enumsOf cr) `shouldBe` 18
