{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Spec.CliSpec
Description : Test specifications for Hsed.Spec.Cli, the commands of hsedc
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The commands as @hsedc@ runs them, on the committed Core extraction with a
sidecar (@--hsa@): @bind@ and @compile@ write nothing when they report an
Error, so a bad comment cannot overwrite the committed bindings or generated
module; @compile@ removes the modules of its spec's directory it did not
write, and nothing else, and writes its four reports (@ERRATA.md@ among them); @orphans@, @ledger@ and @explain@ report the sidecar's and the
annotations' Errors and exit non-zero on them; @orphans@ lists the invalid
and ambiguous tables beside the unbound ones; @ledger@ with several IRs and one
sidecar applies it to the IR whose spec it names; @--target@ and @explain@ name
a spec only in full (@core@, @core-2.01@, @core201@); @explain@ takes a
signature block as well as a table; @--partial@ makes the unbound units of the
spec it names Warnings (@W-UNBOUND@), and the commands exit zero on them;
@--base@ names a base IR the run reads, refused when it cannot be read.
-}
module Hsed.Spec.CliSpec (spec) where

import           RIO
import qualified RIO.List         as L
import qualified RIO.Text         as T
import           RIO.Directory    (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import           RIO.FilePath     (takeDirectory, (</>))
import           Test.Hspec

import           Hsed.Spec.Cli
import           Hsed.Spec.Common (ErrataSet (..), SpecId (..), matchesSpec)
import qualified Hsed.Spec.Common as Cm
import           Hsed.Spec.CoreIR (committedHsa, withCore, withoutLine)
import           Hsed.Spec.IR     (readIR)
import           Hsed.Spec.Report (ledger, orphans, renderLedger)
import           Hsed.Spec.Sidecar (withSidecar)
import           Hsed.Spec.Anchor (AnchorId (..))


coreIR, opalIR :: FilePath
coreIR = "../spec/ir/core-2.01.json"
opalIR = "../spec/ir/opal-2.30.json"

-- | Run a command with its output discarded; whether it exited non-zero.
quietly :: Command -> IO Bool
quietly cmd = bracket (openFile "/dev/null" WriteMode) hClose $ \null' ->
  (runWith null' null' cmd >> pure False) `catch` \(e :: ExitCode) -> pure (e /= ExitSuccess)

-- | Run a command; whether it exited non-zero, and what it wrote to its first handle.
captured :: Command -> IO (Bool, Text)
captured cmd = withSystemTempDirectory "hsedc-out" $ \dir -> do
  let path = dir </> "out"
  failed <- withFile path WriteMode $ \h -> bracket (openFile "/dev/null" WriteMode) hClose $ \null' ->
    (runWith h null' cmd >> pure False) `catch` \(e :: ExitCode) -> pure (e /= ExitSuccess)
  text <- readFileUtf8 path
  pure (failed, text)

-- | A sidecar file in a temporary directory, and what the test does with it there.
withSidecarFile :: Text -> (FilePath -> FilePath -> IO a) -> IO a
withSidecarFile text k = withSystemTempDirectory "hsedc-cli" $ \dir -> do
  let hsa = dir </> "test.hsa"
  writeFileUtf8 hsa text
  k dir hsa

spec :: Spec
spec = do
  describe "matchesSpec" $
    it "names a spec by its id, its letters or its grammar name, never by a part of one" $ do
      map (`matchesSpec` SpecId "core-2.01") ["core-2.01", "core", "core201"] `shouldBe` [True, True, True]
      map (`matchesSpec` SpecId "core-2.01") ["", "c", "cor", "core-2", "opal"] `shouldBe` replicate 5 False

  withCore $ \_ -> do
    hsa <- runIO committedHsa
    let bad = hsa <> "core-2.01 | T46 | rule typeRow\n"

    describe "bind" $ do
      it "writes the bindings of a sidecar without Errors" $
        withSidecarFile hsa $ \dir file -> do
          failed <- quietly (CmdBind noOptions (Input coreIR (Just file)) (dir </> "Core.hs"))
          failed `shouldBe` False
          written <- readFileUtf8 (dir </> "Core.hs")
          committed <- readFileUtf8 "bindings/Hsed/Bindings/Core.hs"
          written `shouldBe` committed
      it "writes nothing when it reports an Error, leaving the previous file as it was" $
        withSidecarFile bad $ \dir file -> do
          let out = dir </> "Core.hs"
          quietly (CmdBind noOptions (Input coreIR (Just file)) out) `shouldReturn` True
          doesFileExist out `shouldReturn` False
          writeFileUtf8 out "previous"
          quietly (CmdBind noOptions (Input coreIR (Just file)) out) `shouldReturn` True
          readFileUtf8 out `shouldReturn` "previous"

    describe "compile" $ do
      it "writes nothing when it reports an Error (the published set leaves Fractoin unresolved)" $
        withSidecarFile hsa $ \dir file -> do
          quietly (CmdCompile noOptions (Input coreIR (Just file)) Published "core" (dir </> "gen") (dir </> "report")) `shouldReturn` True
          doesDirectoryExist (dir </> "gen") `shouldReturn` False
          doesDirectoryExist (dir </> "report") `shouldReturn` False
      it "removes the modules of its spec's directory it did not write, and nothing else" $
        withSidecarFile hsa $ \dir file -> do
          let gen = dir </> "gen"
              stale = gen </> "Hsed/Gen/Core/Old.hs"
              nested = gen </> "Hsed/Gen/Core/Old/Deeper.hs"
              kept = [gen </> "Hsed/Gen/Core/notes.txt", gen </> "Hsed/Gen/Opal/Types.hs", gen </> "Hsed/Gen/Other.hs"]
          forM_ (stale : nested : kept) $ \f -> do
            createDirectoryIfMissing True (takeDirectory f)
            writeFileUtf8 f "stale"
          quietly (CmdCompile noOptions (Input coreIR (Just file)) Scott "core" gen (dir </> "report")) `shouldReturn` False
          mapM doesFileExist [stale, nested] `shouldReturn` [False, False]
          mapM doesFileExist kept `shouldReturn` [True, True, True]
          mapM (doesFileExist . (gen </>)) ["Hsed/Gen/Core/Types.hs", "Hsed/Gen/Core/Enums.hs", "Hsed/Gen/Core/TableUIDs.hs"]
            `shouldReturn` [True, True, True]
      it "writes its four reports beside the modules" $
        withSidecarFile hsa $ \dir file -> do
          quietly (CmdCompile noOptions (Input coreIR (Just file)) Scott "core" (dir </> "gen") (dir </> "report")) `shouldReturn` False
          mapM (doesFileExist . ((dir </> "report") </>)) ["findings.md", "coverage.md", "ERRATA.md", "ledger.md"]
            `shouldReturn` [True, True, True, True]
      it "refuses a --target that names the spec only in part" $
        withSidecarFile hsa $ \dir file ->
          forM_ ["", "c"] $ \target ->
            quietly (CmdCompile noOptions (Input coreIR (Just file)) Scott target (dir </> "gen") (dir </> "report")) `shouldReturn` True

    describe "orphans, ledger and explain" $ do
      it "exit zero on the committed sidecar (no table or signature block of Core unbound since M2c)" $
        withSidecarFile hsa $ \_ file -> do
          quietly (CmdOrphans noOptions (Input coreIR (Just file))) `shouldReturn` False
          quietly (CmdLedger noOptions [coreIR] (Just file)) `shouldReturn` False
          quietly (CmdExplain noOptions "core" (TableId 46) (Input coreIR (Just file))) `shouldReturn` False
      it "report a sidecar that does not resolve and exit non-zero" $
        withSidecarFile (withoutLine "5.1.3" hsa <> "core-2.01 | 5.1.30 | rule typeRow & rule enumValues\n") $ \_ file -> do
          quietly (CmdOrphans noOptions (Input coreIR (Just file))) `shouldReturn` True
          quietly (CmdLedger noOptions [coreIR] (Just file)) `shouldReturn` True
          quietly (CmdExplain noOptions "core" (TableId 46) (Input coreIR (Just file))) `shouldReturn` True
      it "report an annotation that binds nothing and exit non-zero" $
        withSidecarFile bad $ \_ file -> do
          quietly (CmdOrphans noOptions (Input coreIR (Just file))) `shouldReturn` True
          quietly (CmdLedger noOptions [coreIR] (Just file)) `shouldReturn` True
          quietly (CmdExplain noOptions "core" (TableId 52) (Input coreIR (Just file))) `shouldReturn` True
      it "explain a signature block by its id, and refuse one the IR does not have" $
        withSidecarFile hsa $ \_ file -> do
          (failed, out) <- captured (CmdExplain noOptions "core" (SigId "5.3.3.6") (Input coreIR (Just file)))
          failed `shouldBe` False
          T.lines out `shouldSatisfy` any ("  core-2.01 5.3.3.6/sig TableUID.Get, ObjectUID.Get, PDF 201" `T.isPrefixOf`)
          quietly (CmdExplain noOptions "core" (SigId "9.9") (Input coreIR (Just file))) `shouldReturn` True
      it "explain a unit of a spec with Errors at other units: its own printed, the others counted, and exit non-zero (design 5.2)" $ do
        -- the pristine Opal IR has no anchors: each of its 50 tables and 3 signature blocks is E-UNBOUND
        (failed, out) <- captured (CmdExplain noOptions "opal" (TableId 3) (Input opalIR Nothing))
        failed `shouldBe` True
        [ l | l <- T.lines out, "E-" `T.isPrefixOf` l ] `shouldSatisfy` \ls -> length ls == 1 && all ("E-UNBOUND opal-2.30.pdf#page=" `T.isPrefixOf`) ls
        [ l | l <- T.lines out, "E-" `T.isPrefixOf` l ] `shouldSatisfy` all ("&T3:" `T.isInfixOf`)
      it "refuse a spec named only in part" $
        withSidecarFile hsa $ \_ file ->
          forM_ ["", "c"] $ \name -> quietly (CmdExplain noOptions name (TableId 46) (Input coreIR (Just file))) `shouldReturn` True

  describe "--partial" $ do
    it "makes each unbound unit of the spec it names W-UNBOUND, a Warning (the pristine Opal IR: 50 tables, 3 signature blocks)" $ do
      (failed, out) <- captured (CmdCheck (Options ["opal-2.30"] Nothing) (Input opalIR Nothing) Scott)
      failed `shouldBe` False
      let unbound = [ l | l <- T.lines out, "W-UNBOUND opal-2.30.pdf#page=" `T.isPrefixOf` l ]
      length unbound `shouldBe` 53
      take 1 unbound `shouldBe` ["W-UNBOUND opal-2.30.pdf#page=12&T1: no annotation binds it and no rule in scope fires on it (Table 1 - Preconfiguration Tables Legend)"]
      [ l | l <- T.lines out, "E-" `T.isPrefixOf` l ] `shouldBe` []
    it "names a spec as --target does, and leaves another spec's E-UNBOUND an Error" $ do
      forM_ ["opal", "opal230"] $ \name -> quietly (CmdCheck (Options [name] Nothing) (Input opalIR Nothing) Scott) `shouldReturn` False
      forM_ [[], ["core"], ["o"]] $ \names -> quietly (CmdCheck (Options names Nothing) (Input opalIR Nothing) Scott) `shouldReturn` True
    it "takes --base, the IR of the spec the run reads over, and refuses a file that is no IR" $ do
      quietly (CmdCheck (Options ["opal-2.30"] (Just coreIR)) (Input opalIR Nothing) Scott) `shouldReturn` False
      quietly (CmdCheck (Options ["opal-2.30"] (Just "../spec/annot/core-2.01.manifest.json")) (Input opalIR Nothing) Scott) `shouldReturn` True
    it "lets orphans and explain exit zero over the spec it names, listing its unbound units as Warnings" $ do
      (failed, out) <- captured (CmdOrphans (Options ["opal-2.30"] Nothing) (Input opalIR Nothing))
      failed `shouldBe` False
      length [ l | l <- T.lines out, "W-UNBOUND" `T.isPrefixOf` l ] `shouldBe` 53
      quietly (CmdExplain (Options ["opal-2.30"] Nothing) "opal" (TableId 3) (Input opalIR Nothing)) `shouldReturn` False

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    describe "orphans" $ do
      it "is empty on the committed sidecar: every numbered table and signature block of Core bound (design section 11 M2c)" $ do
        orphans (snd (withSidecar pristine hsa)) `shouldBe` []
        withSidecarFile hsa $ \_ file -> captured (CmdOrphans noOptions (Input coreIR (Just file))) `shouldReturn` (False, "")
      it "lists the invalid tables beside the unbound ones" $ do
        let (_, ir) = withSidecar pristine (hsa <> "core-2.01 | T46 | frobnicate\n")
            os = orphans ir
        fmap Cm.code (lookup (TableId 46) os) `shouldBe` Just "E-GRAMMAR"
        map fst os `shouldBe` L.sort (TableId 46 : map fst (orphans (snd (withSidecar pristine hsa))))

    describe "ledger" $ do
      it "applies one sidecar to the IR whose spec its lines name, and reads the others as they are" $
        withSidecarFile hsa $ \_ file -> do
          opal <- readIR opalIR >>= either throwString pure
          (failed, out) <- captured (CmdLedger noOptions [coreIR, opalIR] (Just file))
          -- the pristine Opal IR has no anchors: each of its 50 tables and 3 signature blocks is E-UNBOUND, an Error since M2c
          failed `shouldBe` True
          out `shouldSatisfy` (renderLedger [ledger (snd (withSidecar pristine hsa)), ledger opal] `T.isSuffixOf`)
          let errs = [ l | l <- T.lines out, "E-" `T.isPrefixOf` l ]
          length errs `shouldBe` 53
          errs `shouldSatisfy` all ("E-UNBOUND opal-2.30.pdf#page=" `T.isPrefixOf`)
      it "under --partial opal-2.30, reports Opal's unbound units as W-UNBOUND and exits zero on them, Core's Errors kept" $
        withSidecarFile hsa $ \_ file -> do
          (failed, out) <- captured (CmdLedger (Options ["opal-2.30"] Nothing) [coreIR, opalIR] (Just file))
          failed `shouldBe` False
          [ l | l <- T.lines out, "E-" `T.isPrefixOf` l || "W-UNBOUND" `T.isPrefixOf` l ] `shouldBe` []
          withSidecarFile (hsa <> "core-2.01 | T46 | rule typeRow\n") $ \_ bad ->
            quietly (CmdLedger (Options ["opal-2.30"] Nothing) [coreIR, opalIR] (Just bad)) `shouldReturn` True
      it "refuses a sidecar line naming none of the IRs' specs" $ do
        withSidecarFile (hsa <> "core-2.02 | T46 | typeRow\n") $ \_ file ->
          quietly (CmdLedger noOptions [coreIR, opalIR] (Just file)) `shouldReturn` True
        withSidecarFile hsa $ \_ file ->
          quietly (CmdLedger noOptions [opalIR] (Just file)) `shouldReturn` True
