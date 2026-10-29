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
module; @orphans@, @ledger@ and @explain@ report the sidecar's and the
annotations' Errors and exit non-zero on them; @orphans@ lists the invalid
and ambiguous tables beside the unbound ones; @ledger@ with several IRs and one
sidecar applies it to the IR whose spec it names; @--target@ and @explain@ name
a spec only in full (@core@, @core-2.01@, @core201@).
-}
module Hsed.Spec.CliSpec (spec) where

import           RIO
import           RIO.Directory    (doesDirectoryExist, doesFileExist)
import           RIO.FilePath     ((</>))
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
          failed <- quietly (CmdBind (Input coreIR (Just file)) (dir </> "Core.hs"))
          failed `shouldBe` False
          written <- readFileUtf8 (dir </> "Core.hs")
          committed <- readFileUtf8 "bindings/Hsed/Bindings/Core.hs"
          written `shouldBe` committed
      it "writes nothing when it reports an Error, leaving the previous file as it was" $
        withSidecarFile bad $ \dir file -> do
          let out = dir </> "Core.hs"
          quietly (CmdBind (Input coreIR (Just file)) out) `shouldReturn` True
          doesFileExist out `shouldReturn` False
          writeFileUtf8 out "previous"
          quietly (CmdBind (Input coreIR (Just file)) out) `shouldReturn` True
          readFileUtf8 out `shouldReturn` "previous"

    describe "compile" $ do
      it "writes nothing when it reports an Error (the published set leaves Fractoin unresolved)" $
        withSidecarFile hsa $ \dir file -> do
          quietly (CmdCompile (Input coreIR (Just file)) Published "core" (dir </> "gen") (dir </> "report")) `shouldReturn` True
          doesDirectoryExist (dir </> "gen") `shouldReturn` False
          doesDirectoryExist (dir </> "report") `shouldReturn` False
      it "refuses a --target that names the spec only in part" $
        withSidecarFile hsa $ \dir file ->
          forM_ ["", "c"] $ \target ->
            quietly (CmdCompile (Input coreIR (Just file)) Scott target (dir </> "gen") (dir </> "report")) `shouldReturn` True

    describe "orphans, ledger and explain" $ do
      it "exit zero on the committed sidecar (the unbound tables are Warnings at M1)" $
        withSidecarFile hsa $ \_ file -> do
          quietly (CmdOrphans (Input coreIR (Just file))) `shouldReturn` False
          quietly (CmdLedger [coreIR] (Just file)) `shouldReturn` False
          quietly (CmdExplain "core" 46 (Input coreIR (Just file))) `shouldReturn` False
      it "report a sidecar that does not resolve and exit non-zero" $
        withSidecarFile (withoutLine "5.1.3" hsa <> "core-2.01 | 5.1.30 | rule typeRow & rule enumValues\n") $ \_ file -> do
          quietly (CmdOrphans (Input coreIR (Just file))) `shouldReturn` True
          quietly (CmdLedger [coreIR] (Just file)) `shouldReturn` True
          quietly (CmdExplain "core" 46 (Input coreIR (Just file))) `shouldReturn` True
      it "report an annotation that binds nothing and exit non-zero" $
        withSidecarFile bad $ \_ file -> do
          quietly (CmdOrphans (Input coreIR (Just file))) `shouldReturn` True
          quietly (CmdLedger [coreIR] (Just file)) `shouldReturn` True
          quietly (CmdExplain "core" 52 (Input coreIR (Just file))) `shouldReturn` True
      it "refuse a spec named only in part" $
        withSidecarFile hsa $ \_ file ->
          forM_ ["", "c"] $ \name -> quietly (CmdExplain name 46 (Input coreIR (Just file))) `shouldReturn` True

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    describe "orphans" $
      it "lists the invalid tables beside the unbound ones" $ do
        let (_, ir) = withSidecar pristine (hsa <> "core-2.01 | T46 | frobnicate\n")
            os = orphans ir
        fmap Cm.code (lookup (TableId 46) os) `shouldBe` Just "E-GRAMMAR"
        length os `shouldBe` 129

    describe "ledger" $ do
      it "applies one sidecar to the IR whose spec its lines name, and reads the others as they are" $
        withSidecarFile hsa $ \_ file -> do
          opal <- readIR opalIR >>= either throwString pure
          (failed, out) <- captured (CmdLedger [coreIR, opalIR] (Just file))
          failed `shouldBe` False
          out `shouldBe` renderLedger [ledger (snd (withSidecar pristine hsa)), ledger opal]
      it "refuses a sidecar line naming none of the IRs' specs" $ do
        withSidecarFile (hsa <> "core-2.02 | T46 | typeRow\n") $ \_ file ->
          quietly (CmdLedger [coreIR, opalIR] (Just file)) `shouldReturn` True
        withSidecarFile hsa $ \_ file ->
          quietly (CmdLedger [opalIR] (Just file)) `shouldReturn` True
