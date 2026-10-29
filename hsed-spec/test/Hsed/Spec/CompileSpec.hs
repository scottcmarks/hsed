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
the 100 type rows and 20 enumerations of section 5.1.3 (T87 by its comment; read through their
rules' modules), their provenance (the cell hash the manifest records), the
module under each errata set, the T75 and T101 annotations each changing the
module when dropped, a table left out with W-GEN-EXCLUDED (one that fails, and
one comment-bound to a shape no rule reads), and the committed @hsed/gen@
modules exactly what the committed annotations give.
-}
module Hsed.Spec.CompileSpec (spec) where

import qualified RIO.Text          as T
import           RIO
import           Test.Hspec

import           Hsed.Core.Types   (Operand (..), TypeDef (..), TypeRef (..))
import           Hsed.Spec.Anchor  (AnchorId (..))
import           Hsed.Spec.Check   (mkContext)
import           Hsed.Spec.Common  (ErrataSet (..), isError, renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.Compile
import           Hsed.Spec.CoreIR
import qualified Hsed.Spec.Gen     as Gen
import           Hsed.Spec.IR      (SpecIR (..))
import           Hsed.Spec.RuleDef (Context)
import           Hsed.Spec.Rules.EnumValues (EnumDecl (..), checkEnum, enumDecl, enumReads)
import           Hsed.Spec.Rules.TypeRow (TypeRowDecl (..), joinFormat, typeRowDecl, typeRowReads, typesModule)


-- | The type rows a context declares.
rowsOf :: Context -> [TypeRowDecl]
rowsOf ctx = rights [ typeRowDecl ctx tr | tr <- rights (typeRowReads ctx) ]

-- | The enumerations a context declares.
enumsOf :: Context -> [EnumDecl]
enumsOf ctx = rights [ enumDecl ctx er (checkEnum ctx rows er) | er <- rights (enumReads ctx) ]
  where rows = rights (typeRowReads ctx)

-- | The text of one generated module.
moduleText :: Text -> CompileResult -> Text
moduleText modName cr = fromMaybe "" (lookup modName (crModules cr))

typesText, enumsText :: CompileResult -> Text
typesText = moduleText "Hsed.Gen.Core.Types"
enumsText = moduleText "Hsed.Gen.Core.Enums"

spec :: Spec
spec = do
  describe "the generated module of a rule" $
    it "is named, and filed, after the spec" $ do
      Gen.moduleName core typesModule `shouldBe` "Hsed.Gen.Core.Types"
      Gen.modulePath "hsed/gen" "Hsed.Gen.Core.Types" `shouldBe` "hsed/gen/Hsed/Gen/Core/Types.hs"

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let compiled es = either (throwString . T.unpack . renderFinding) pure (compileIR es ir)

    describe "compileIR --errata scott" $ do
      cr <- runIO (compiled Scott)
      let ctx = mkContext Scott ir
      it "declares the 100 type rows and the 20 enumerations of section 5.1.3, leaving none out" $ do
        length (rowsOf ctx) `shouldBe` 100
        length (enumsOf ctx) `shouldBe` 20
        crExcluded cr `shouldBe` []
        filter isError (crFindings cr) `shouldBe` []
      it "records the provenance of design example E2, with the manifest's cell hash" $ do
        let p46 = [ r.provenance | r <- rowsOf ctx, r.name == "AC_element" ]
            p75 = [ r.provenance | r <- rowsOf ctx, r.name == "clock_time" ]
        map (\p -> (p.tableId, p.pdfPage1, p.printedPage, p.ordinal, p.structPath)) p46 `shouldBe` [("T46", 100, 83, 48, "0/1288")]
        map (.cellsHash) p75 `shouldBe` ["5a5e8787c29a7124035961d972b7b81ab1635117d2d207a0942f1a9c43291655"]
      it "reads T46 as design example E2 does" $
        [ (r.uidBytes, joinFormat r.formatText, r.typeDef) | r <- rowsOf ctx, r.name == "AC_element" ]
          `shouldBe` [([0, 0, 0, 5, 0, 0, 8, 1], "List_Type,*,ACE_expression", Just (ListType Star (Ref "ACE_expression")))]
      it "corrects T75 and types it" $
        [ r.typeDef | r <- rowsOf ctx, r.name == "clock_time" ]
          `shouldBe` [Just (StructType (map Ref ["Year", "Month", "Day", "Hour", "Minute", "Seconds", "Fraction"]))]
      it "reads T52 as design example E4 does, with the range of T51" $
        [ (e.ctors, e.range, e.typeUID) | e <- enumsOf ctx, e.name == "adv_key_mode" ]
          `shouldBe` [([("Wait for AdvKey_Req", 0), ("Auto-advance keys", 1)] <> [ ("Reserved", v) | v <- [2 .. 7] ], (0, 7), [0, 0, 0, 5, 0, 0, 4, 0x0F])]
      it "names T101's value as its annotation corrects it" $
        [ e.ctors | e <- enumsOf ctx, e.name == "last_reenc_stat" ] `shouldSatisfy` all (elem ("Verify Error", 3))
      it "reads T87 through its comment: two columns, \"7 to 31\", value 34 kept for its first label (design E4)" $ do
        let gen = [ e | e <- enumsOf ctx, e.name == "gen_status" ]
        map (.range) gen `shouldBe` [(0, 63)]
        [ map snd e.ctors | e <- gen ] `shouldBe` [[0 .. 63]]
        [ lookup 34 [ (v, l) | (l, v) <- e.ctors ] | e <- gen ] `shouldBe` [Just "active_reset_stop_detect"]
        [ lookup 31 [ (v, l) | (l, v) <- e.ctors ] | e <- gen ] `shouldBe` [Just "reserved"]
        [ e.ctors | e <- enumsOf (mkContext Published ir), e.name == "gen_status" ] `shouldBe` map (.ctors) gen
      it "takes the range of reencrypt_state from its Format, starting at 1" $
        [ e.range | e <- enumsOf ctx, e.name == "reencrypt_state" ] `shouldBe` [(1, 16)]
      it "writes the committed hsed/gen modules: Types, Enums and TableUIDs" $ do
        map fst (crModules cr) `shouldBe` ["Hsed.Gen.Core.Enums", "Hsed.Gen.Core.TableUIDs", "Hsed.Gen.Core.Types"]
        forM_ (crModules cr) $ \(modName, text) -> do
          committed <- readFileUtf8 (Gen.modulePath "../hsed/gen" modName)
          text `shouldBe` committed
      it "writes the same text whatever the order of the IR's tables" $ do
        cr' <- either (throwString . T.unpack . renderFinding) pure (compileIR Scott ir { tables = reverse ir.tables })
        crModules cr' `shouldBe` crModules cr

    describe "compileIR --errata published" $ do
      cr <- runIO (compiled Published)
      let ctx = mkContext Published ir
      it "keeps Fractoin, gives T75 no TypeDef and reports E-FMT-RESOLVE" $ do
        [ (joinFormat r.formatText, r.typeDef) | r <- rowsOf ctx, r.name == "clock_time" ]
          `shouldBe` [("Struct_Type,Year,Month,Day,Hour,Minute,Seconds,Fractoin", Nothing)]
        T.lines (typesText cr) `shouldSatisfy` elem "-- tclock_time: no TypeDef, the Format did not parse under this errata set"
        map renderFinding (filter isError (crFindings cr))
          `shouldBe` ["E-FMT-RESOLVE core-2.01.pdf#page=107&T75: unresolved type name Fractoin (nearest: Fraction, T84)"]
      it "names T101's value as printed" $
        [ e.ctors | e <- enumsOf ctx, e.name == "last_reenc_stat" ] `shouldSatisfy` all (elem ("Verify Eror", 3))

    describe "the per-table annotations" $ do
      cr <- runIO (compiled Scott)
      let without a = do
            ir' <- annotated pristine (withoutLine a hsa)
            either (throwString . T.unpack . renderFinding) pure (compileIR Scott ir')
      it "T75: without it, clock_time has no TypeDef and the module changes" $ do
        cr' <- without "T75"
        typesText cr' `shouldNotBe` typesText cr
        T.lines (typesText cr') `shouldSatisfy` elem "-- tclock_time: no TypeDef, the Format did not parse under this errata set"
        map renderFinding (filter isError (crFindings cr'))
          `shouldBe` ["E-FMT-RESOLVE core-2.01.pdf#page=107&T75: unresolved type name Fractoin (nearest: Fraction, T84)"]
      it "T101: without it, the module names Verify Eror as printed" $ do
        cr' <- without "T101"
        let changed = [ (a, b) | (a, b) <- zip (T.lines (enumsText cr)) (T.lines (enumsText cr')), a /= b ]
        changed `shouldBe` [("  | Core_last_reenc_stat_Verify_Error", "  | Core_last_reenc_stat_Verify_Eror")]
      it "T149: without it, compile finds the Error C-UID-UNIQUE" $ do
        cr' <- without "T149"
        map Cm.code (filter isError (crFindings cr')) `shouldBe` ["C-UID-UNIQUE"]

    describe "a table bound to a shape no rule reads" $
      it "is left out with W-GEN-EXCLUDED and counted, not dropped in silence" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T46 | as @'Codes codes\ncore-2.01 | T166 | statusCodes\n")
        cr <- either (throwString . T.unpack . renderFinding) pure (compileIR Scott ir')
        crExcluded cr `shouldBe`
          [ (TableId 46, "hsedc generates nothing for a Codes table yet (no rule of the registry reads one)")
          , (TableId 166, "hsedc generates nothing for a StatusCodes table yet (no rule of the registry reads one)") ]
        [ Cm.code f | f <- crFindings cr, Cm.anchor f == Just "T166" ] `shouldBe` ["W-GEN-EXCLUDED"]
        length (rowsOf (mkContext Scott ir')) `shouldBe` 99

    describe "a table that cannot be generated" $
      it "is left out with W-GEN-EXCLUDED, its Error among the findings" $ do
        let broken = setCell "T52" 2 0 "2-8" ir
        cr <- either (throwString . T.unpack . renderFinding) pure (compileIR Scott broken)
        crExcluded cr `shouldBe` [(TableId 52, "C-ENUM-RANGE failed")]
        [ Cm.code f | f <- crFindings cr, Cm.anchor f == Just "T52" ] `shouldBe` ["C-ENUM-RANGE", "W-GEN-EXCLUDED"]
        length (enumsOf (mkContext Scott broken)) `shouldBe` 19
