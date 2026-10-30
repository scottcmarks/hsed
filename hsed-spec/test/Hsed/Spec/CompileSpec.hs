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

@hsedc compile@ on the committed Core extraction with the committed
annotations (and on Opal's, over Core's base, its unbound units Warnings:
no Error, its committed modules and @docs/LEDGER-opal.md@ what the run
gives): no Error, and no table left out but those a comment binds to a
shape no rule of the registry reads yet (a reader still to come, each a
W-GEN-EXCLUDED Warning); the committed @hsed/gen@ modules
exactly what the committed annotations give, file for file (a module no rule
emits any more must not stay committed); the committed ledger
@docs/LEDGER.md@, which holds the totals the tests do not pin, exactly what
the run gives; the T75, T101 and T149 annotations each changing the result
when dropped; a table left out with W-GEN-EXCLUDED (one that fails, and one
comment-bound to a shape no rule of the registry reads, whichever shapes those
are); declarations grouped into modules, their notes united. What each rule
declares is tested in @Rules/\<Rule\>Spec.hs@.
-}
module Hsed.Spec.CompileSpec (spec) where

import qualified Data.Text         as DT
import qualified RIO.List          as L
import qualified RIO.Map           as Map
import qualified RIO.Text          as T
import           RIO
import           RIO.Directory     (doesDirectoryExist, listDirectory)
import           RIO.FilePath      (dropExtension, makeRelative, takeExtension, (</>))
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId (..))
import           Hsed.Spec.Check   (mkContext, partially)
import           Hsed.Spec.Common  (ErrataSet (..), isError, renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.Compile
import           Hsed.Spec.CoreIR
import qualified Hsed.Spec.Gen     as Gen
import           Hsed.Spec.IR      (SpecIR (..))
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.Report  (ledger, ledgerMarkdown)
import           Hsed.Spec.DSL     (someBindingShape)
import           Hsed.Spec.RuleDef (Context (..), boundBinding, ruleShape)
import           Hsed.Spec.Rules.TypeRow (typesModule)
import           Hsed.Spec.Shape   (Shape (..), shapeGenerates, shapeName)


-- | The text of one generated module.
moduleText :: Text -> CompileResult -> Text
moduleText modName cr = fromMaybe "" (lookup modName (crModules cr))

typesText, enumsText :: CompileResult -> Text
typesText = moduleText "Hsed.Gen.Core.Types"
enumsText = moduleText "Hsed.Gen.Core.Enums"

-- | The modules committed under a generated-code directory, by name.
committedModules :: FilePath -> FilePath -> IO [Text]
committedModules root d = do
  exists <- doesDirectoryExist d
  if not exists then pure [] else do
    entries <- map (d </>) <$> listDirectory d
    fmap concat $ forM entries $ \e -> do
      isDir <- doesDirectoryExist e
      if isDir then committedModules root e
      else pure [ DT.replace "/" "." (T.pack (dropExtension (makeRelative root e))) | takeExtension e == ".hs" ]

-- | Tables a comment can bind to a shape, for the shapes no rule of the
--   registry reads: anchor, payload, shape.
unreadShapes :: [(Int, Text, Shape)]
unreadShapes =
  [ c | c@(_, _, shape) <- [(46, "as @'Codes codes", Codes), (166, "statusCodes", StatusCodes), (230, "decision", Decision)]
      , Just shape `notElem` map ruleShape registry ]

spec :: Spec
spec = do
  describe "the generated module of a rule" $
    it "is named, and filed, after the spec" $ do
      Gen.moduleName core typesModule `shouldBe` "Hsed.Gen.Core.Types"
      Gen.modulePath "hsed/gen" "Hsed.Gen.Core.Types" `shouldBe` "hsed/gen/Hsed/Gen/Core/Types.hs"
      Gen.specModuleDir "hsed/gen" core `shouldBe` "hsed/gen/Hsed/Gen/Core"

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let compiled es = either (throwString . T.unpack . renderFinding) pure (compileIR Nothing es ir)

    describe "compileIR --errata scott" $ do
      cr <- runIO (compiled Scott)
      it "finds no Error, and leaves out only the tables bound to a shape no rule of the registry reads" $ do
        let ctx = mkContext Scott ir
            unread = [ aid | (aid, c) <- Map.toList (cxClasses ctx), Just sb <- [boundBinding c], Just shape <- [someBindingShape sb]
                           , shapeGenerates shape, Just shape `notElem` map ruleShape registry ]
        map fst (crExcluded cr) `shouldBe` unread
        filter isError (crFindings cr) `shouldBe` []
      it "writes the committed hsed/gen modules, and only those: no other module of the spec is committed" $ do
        committed <- committedModules "../hsed/gen" (Gen.specModuleDir "../hsed/gen" core)
        L.sort committed `shouldBe` map fst (crModules cr)
        forM_ (crModules cr) $ \(modName, text) -> do
          file <- readFileUtf8 (Gen.modulePath "../hsed/gen" modName)
          text `shouldBe` file
      it "writes every module in ASCII (docs/adding-a-rule.md), its comments' captions included" $
        [ (modName, T.take 60 l) | (modName, text) <- crModules cr, l <- T.lines text, T.any (> '\DEL') l ] `shouldBe` []
      it "writes the committed ledger docs/LEDGER.md: the binding and finding totals" $ do
        committed <- readFileUtf8 "../docs/LEDGER.md"
        ledgerMarkdown "build/report/ledger.md" Scott [(ledger ir, crFindings cr)] `shouldBe` committed
      it "writes the same text whatever the order of the IR's tables" $ do
        cr' <- either (throwString . T.unpack . renderFinding) pure (compileIR Nothing Scott ir { tables = reverse ir.tables })
        crModules cr' `shouldBe` crModules cr

    describe "compileIR of Opal over Core's base, Opal annotated in part (hsedc --base, --partial)" $
      withOpal $ \opalPristine -> do
        opal <- runIO (committedOpalHsa >>= annotated opalPristine)
        cr <- runIO (either (throwString . T.unpack . renderFinding) pure (compileIR (Just (mkContext Scott ir)) Scott opal))
        let fs = partially ["opal-2.30"] (crFindings cr)
        it "finds no Error once its unbound units are Warnings" $
          filter isError fs `shouldBe` []
        it "writes the committed hsed/gen/Hsed/Gen/Opal modules, and only those" $ do
          committed <- committedModules "../hsed/gen" (Gen.specModuleDir "../hsed/gen" (Cm.SpecId "opal-2.30"))
          L.sort committed `shouldBe` map fst (crModules cr)
          forM_ (crModules cr) $ \(modName, text) -> do
            file <- readFileUtf8 (Gen.modulePath "../hsed/gen" modName)
            text `shouldBe` file
        it "writes the committed ledger docs/LEDGER-opal.md" $ do
          committed <- readFileUtf8 "../docs/LEDGER-opal.md"
          ledgerMarkdown "build/report/opal/ledger.md" Scott [(ledger opal, fs)] `shouldBe` committed

    describe "the per-table annotations" $ do
      cr <- runIO (compiled Scott)
      let without a = do
            ir' <- annotated pristine (withoutLine a hsa)
            either (throwString . T.unpack . renderFinding) pure (compileIR Nothing Scott ir')
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
      it "is left out with W-GEN-EXCLUDED and counted, not dropped in silence" $
        if null unreadShapes then pendingWith "a rule reads every shape the test can bind" else do
          let others = foldr (\(n, _, _) -> withoutLine ("T" <> tshow n)) hsa unreadShapes
          ir' <- annotated pristine (others <> T.unlines [ "core-2.01 | T" <> tshow n <> " | " <> payload | (n, payload, _) <- unreadShapes ])
          cr <- either (throwString . T.unpack . renderFinding) pure (compileIR Nothing Scott ir')
          crExcluded cr `shouldSatisfy` \ex -> all (`elem` ex)
            [ (TableId n, "hsedc generates nothing for a " <> shapeName shape <> " table yet (no rule of the registry reads one)")
            | (n, _, shape) <- unreadShapes ]
          forM_ unreadShapes $ \(n, _, _) ->
            [ Cm.code f | f <- crFindings cr, Cm.anchor f == Just ("T" <> tshow n), Cm.code f == "W-GEN-EXCLUDED" ] `shouldBe` ["W-GEN-EXCLUDED"]

    describe "a table that cannot be generated" $
      it "is left out with W-GEN-EXCLUDED, its Error among the findings" $ do
        let broken = setCell "T52" 2 0 "2-8" ir
        cr <- either (throwString . T.unpack . renderFinding) pure (compileIR Nothing Scott broken)
        newlyExcluded ir broken `shouldReturn` [(TableId 52, "C-ENUM-RANGE failed")]
        [ Cm.code f | f <- crFindings cr, Cm.anchor f == Just "T52" ] `shouldBe` ["C-ENUM-RANGE", "W-GEN-EXCLUDED"]

    describe "generatedModules" $ do
      cr <- runIO (compiled Scott)
      let ctx = mkContext Scott ir
          gm description notes = Gen.GenModule { Gen.leaf = "Shared", Gen.description = description, Gen.notes = notes }
          d = take 2 (crDecls cr)
      it "puts the declarations of every rule naming a module into it, uniting their notes" $
        case generatedModules ctx (zip [gm "Shared things" ["One."], gm "Shared things" ["Two."]] d) of
          Right [(name, m, ds)] -> do
            name `shouldBe` "Hsed.Gen.Core.Shared"
            m.notes `shouldBe` ["One.", "Two."]
            length ds `shouldBe` 2
          other -> expectationFailure ("not one module: " <> show (fmap (map (\(n, _, _) -> n)) other))
      it "refuses two descriptions of one module (E-GEN-MODULE)" $
        fmap (map (\(n, _, _) -> n)) (first Cm.code (generatedModules ctx (zip [gm "These" [], gm "Those" []] d)))
          `shouldBe` Left "E-GEN-MODULE"
