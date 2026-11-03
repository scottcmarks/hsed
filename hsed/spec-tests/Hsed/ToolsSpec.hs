{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.ToolsSpec
Description : Test specifications for the hardware scripts tools/m3-hw.sh and tools/m4c-hw.sh, read as text and parsed by bash
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The hardware runs' checklists (design 11: M3's capture, M4c's steps H0 to
H14), which an agent runs as itself for their pre-flights and Scott for the
rest: neither runs sudo, each sudo they name standing in a here-document of
the commands they print for Scott, or in a comment; @bash -n@ parses each;
each names the Level 0 fixture by the name FixtureSpec reads,
@spec\/fixtures\/disk6-level0.bin@ (the drive as first seen, whatever its BSD
name is now). @tools/m4c-hw.sh@ names no @\/dev\/diskN@ outside its comments
(the drive is found by its serial, its number assigned at each attach),
prints the steps H0 to H14, and knows kext build 625 by its UUID; since the
M4c review its steps begin once M4c has landed, eject the drive before any
replug, tell a latched transport from the TPer's own answer at H6, verify
the volume before H4's snapshot, run the Pair's Properties (H12, its offline
simulator given the drive's Level 0 descriptors) before H13 closes the
window, and key a snapshot by the slice a replug keeps; since its
verification, H0, H6 and H10 order a replug: the snapshot before it while
the drive is attached, the eject, the replug, the snapshot after. Nothing
here runs either script: their pre-flights read the IORegistry, which the
suite does not, and a snapshot reads the drive's volume.

@tools/kext-imports.sh@, CI's check of the flag-on shim's imports (design
7.2), runs here, a copy of it in a directory of its own against a fake
@nm@: it passes an object whose imports are all on its list, and fails one
that imports more, one @nm@ cannot read, @nm@ missing, an object nm reads
no import of, and a work dir with no object (the M4c review's
verification found the earlier one passing an object @nm@ could not read).
-}
module Hsed.ToolsSpec (spec) where

import qualified RIO.Char         as C
import           RIO
import           RIO.Directory    (createDirectoryIfMissing, getPermissions, setOwnerExecutable, setPermissions)
import           RIO.FilePath     ((</>))
import           RIO.Process      (proc, readProcess, withProcessContextNoLogging)
import qualified RIO.Text         as T
import qualified RIO.Text.Partial as T'
import           System.Environment (lookupEnv)
import           Test.Hspec


-- | The hardware scripts, as the suite (run in the package's directory)
--   finds them.
scripts :: [FilePath]
scripts = ["../tools/m3-hw.sh", "../tools/m4c-hw.sh"]

-- | A script's text.
script :: FilePath -> IO Text
script path = decodeUtf8Lenient <$> readFileBinary path

-- | Whether a line is a comment.
comment :: Text -> Bool
comment = ("#" `T.isPrefixOf`) . T.stripStart

-- | The lines of a shell script outside its here-documents (@<<EOF@ to
--   @EOF@), each line that opens one among them (KextSourceSpec's, which
--   checked tools/m3-hw.sh at M3).
outsideHeredocs :: [Text] -> [Text]
outsideHeredocs = go
  where
    go [] = []
    go (l : ls) = case T'.breakOn "<<" l of
      (_, rest) | not (T.null rest), Just tag <- delimiter (T.drop 2 rest) ->
        l : go (drop 1 (dropWhile ((/= tag) . T.strip) ls))
      _ -> l : go ls
    -- @<<EOF@, @<<'EOF'@, @<<-EOF@; not a here-string (@<<<@)
    delimiter t = case T.words (T.filter (`notElem` ("'\"-" :: String)) t) of
      w : _ | not (T.null w), T.all (\c -> C.isAlphaNum c || c == '_') w -> Just w
      _                                                                  -> Nothing

-- | Whether a line names a drive by its BSD number: @\/dev\/disk@ and a
--   digit.
namesDiskN :: Text -> Bool
namesDiskN l = any (maybe False (C.isDigit . fst) . T.uncons . T.drop (T.length "/dev/disk") . snd) (T'.breakOnAll "/dev/disk" l)

-- | A step of the ones a script prints, @Hn. @ to the next step's heading
--   (none where it prints no such step).
stepOf :: Int -> Text -> Text
stepOf n text = case T'.breakOn ("\nH" <> tshow n <> ". ") text of
  (_, rest) | not (T.null rest) -> fst (T'.breakOn "\nH" (T.drop 1 (snd (T'.breakOn ". " rest)))) & \body -> "H" <> tshow n <> "." <> body
  _                             -> ""

spec :: Spec
spec = describe "the hardware scripts" $ do
  for_ scripts $ \path -> do
    it (path <> " runs no sudo: each it names is in a here-document of the commands it prints, or a comment") $ do
      text <- script path
      [ l | l <- outsideHeredocs (T.lines text), "sudo" `T.isInfixOf` l, not (comment l) ] `shouldBe` []
    it (path <> " parses (bash -n)") $ do
      (code, _, err) <- withProcessContextNoLogging (proc "bash" ["-n", path] readProcess)
      (code, decodeUtf8Lenient (toStrictBytes err)) `shouldBe` (ExitSuccess, "")
    it (path <> " names the Level 0 fixture spec/fixtures/disk6-level0.bin, the drive as first seen") $ do
      text <- script path
      text `shouldSatisfy` T.isInfixOf "spec/fixtures/disk6-level0.bin"
      text `shouldNotSatisfy` T.isInfixOf "spec/fixtures/${disk}"

  describe "tools/m4c-hw.sh" $ do
    it "names no /dev/diskN outside its comments: the drive is found by its serial" $ do
      text <- script "../tools/m4c-hw.sh"
      [ l | l <- T.lines text, namesDiskN l, not (comment l) ] `shouldBe` []
    it "prints Scott's steps H0 to H14" $ do
      text <- script "../tools/m4c-hw.sh"
      [ step | n <- [0 .. 14 :: Int], let step = "H" <> tshow n <> ". ", not (T.isInfixOf ("\n" <> step) text) ] `shouldBe` []
    it "begins H0 once M4c has landed with CI green, and ejects the drive before any replug: H0, H6, H10 (the M4c review)" $ do
      text <- script "../tools/m4c-hw.sh"
      stepOf 0 text `shouldSatisfy` T.isInfixOf "H0. After M4c lands on feature/types with CI green"
      [ n | n <- [0, 6, 10], not ("diskutil eject" `T.isInfixOf` stepOf n text || "eject the drive" `T.isInfixOf` stepOf n text) ] `shouldBe` []
      stepOf 0 text `shouldSatisfy` T.isInfixOf "diskutil eject /dev/\\$DISK"
    it "orders a replug in H0, H6 and H10: the snapshot pre-replug while the drive is attached, then the eject, then the snapshot post-replug (the M4c review's verification)" $ do
      text <- script "../tools/m4c-hw.sh"
      let at' needle hay = T.length (fst (T'.breakOn needle hay))
          ordered step = at' "snapshot pre-replug" step < at' "eject" step && at' "eject" step < at' "snapshot post-replug" step
                         && at' "snapshot post-replug" step < T.length step
      [ n | n <- [0, 6, 10], not (ordered (stepOf n text)) ] `shouldBe` []
    it "tells H6's kern_return failure, which latches the transport, from the TPer's own Success/Failure 1 or No Response Available" $ do
      h6 <- stepOf 6 <$> script "../tools/m4c-hw.sh"
      for_ ["a kern_return failure on the STACK_RESET: the transport latched", "'Success/Failure 1' or 'No Response Available': the TPer's own"] $ \l ->
        h6 `shouldSatisfy` T.isInfixOf l
    it "offers H4's verifyVolume before the snapshot it would spoil, never after" $ do
      h4 <- stepOf 4 <$> script "../tools/m4c-hw.sh"
      let at' needle = T.length (fst (T'.breakOn needle h4))
      (at' "diskutil verifyVolume" < at' "snapshot before", at' "snapshot before" < T.length h4) `shouldBe` (True, True)
    it "runs the Pair's Properties as H12, before H13 closes the window, its offline simulator given the drive's Level 0 descriptors" $ do
      text <- script "../tools/m4c-hw.sh"
      stepOf 12 text `shouldSatisfy` T.isInfixOf "build/bin/hsed-jig --target sim --tests UCT-02 --profile spec/fixtures/disk6-level0.bin"
      stepOf 12 text `shouldSatisfy` T.isInfixOf "'missing, the drive's alone' code is a Level 0 difference"
      stepOf 13 text `shouldSatisfy` T.isInfixOf "tools/m4c-hw.sh snapshot after \"$SERIAL\" && tools/m4c-hw.sh compare before after"
      stepOf 4 text `shouldNotSatisfy` T.isInfixOf "H13 need"
    it "keys a snapshot's rows by the slice, which a replug keeps, not by the disk's number; and finds a snapshot mounted from a slice" $ do
      text <- script "../tools/m4c-hw.sh"
      for_ [ "printf '# %s on %s\\n' \"$(slice_of \"$disk\" \"$dev\")\" \"$mp\""
           , "\"$(slice_of \"$disk\" \"/dev/$slice\")\" \"$key\""
           , "s#^(([^ @]*@)?/dev/$1(s[0-9]+)*) on (.*)" ] $ \l ->
        text `shouldSatisfy` T.isInfixOf l
    it "knows kext build 625 by its UUID, read byte for byte (design 13.12)" $ do
      text <- script "../tools/m4c-hw.sh"
      text `shouldSatisfy` T.isInfixOf "373BA509-89F7-3B36-A977-BB65C7635F1B)"
    it "asks build/bin/hsed-jig for the shim's command and bind and the portable C's scalars and bind check, and the shared hsed-jig only for no _hsed_kext_ symbol" $ do
      text <- script "../tools/m4c-hw.sh"
      for_ [ "for s in _hsed_kext_command _hsed_kext_bind _hsed_cdb_scalars _hsed_cdb_bind_check; do"
           , "grep -q '_hsed_kext_' <<<\"$symbols\"" ] $ \l ->
        text `shouldSatisfy` T.isInfixOf l

  describe "tools/kext-imports.sh, a copy in a directory of its own, against a fake nm (the M4c review's verification)" $ do
    it "parses (bash -n)" $ do
      (code, _, err) <- withProcessContextNoLogging (proc "bash" ["-n", "../tools/kext-imports.sh"] readProcess)
      (code, decodeUtf8Lenient (toStrictBytes err)) `shouldBe` (ExitSuccess, "")
    it "passes an object whose imports are all on its list, and fails one importing more, one nm cannot read, nm missing, no import read, no object; exit 2 for a usage error" $
      withSystemTempDirectory "hsed-kext-imports" $ \dir -> do
        for_ ["tools", "w" </> "x", "bin"] (createDirectoryIfMissing True . (dir </>))
        readFileBinary "../tools/kext-imports.sh" >>= writeFileBinary (dir </> "tools" </> "kext-imports.sh")
        writeFileBinary (dir </> "w" </> "x" </> "hsed_kext.o") "no object at all"
        path <- fromMaybe "/usr/bin:/bin" <$> lookupEnv "PATH"
        let nm body = do
              let f = dir </> "bin" </> "nm"
              writeFileUtf8 f ("#!/bin/sh\n" <> body <> "\n")
              getPermissions f >>= setPermissions f . setOwnerExecutable True
            imports args = do
              (code, out, err) <- withProcessContextNoLogging $
                proc "env" (("PATH=" <> (dir </> "bin") <> ":" <> path) : "bash" : (dir </> "tools" </> "kext-imports.sh") : args) readProcess
              pure (code, decodeUtf8Lenient (toStrictBytes out), decodeUtf8Lenient (toStrictBytes err))
            object = "./w/x/hsed_kext.o"
        nm "printf '_IOConnectCallScalarMethod\\n_hsed_cdb_scalars\\n_hsed_cdb_bind_check\\n___stack_chk_guard\\n'"
        imports ["w"] `shouldReturn` (ExitSuccess, "kext-imports: " <> object <> " imports only what the shim may\n", "")
        nm "printf '_IOConnectCallScalarMethod\\n_IOConnectCallMethod\\n'"
        imports ["w"] `shouldReturn` (ExitFailure 1, "", "kext-imports: " <> object <> " imports _IOConnectCallMethod, which the shim may not\n")
        nm "echo 'nm: error: the file was not recognized as a valid object file' >&2; exit 1"
        imports ["w"] >>= (`shouldSatisfy` \(code, out, err) -> code == ExitFailure 1 && T.null out && T.isInfixOf ("nm cannot read " <> object) err)
        nm "echo 'nm: command not found' >&2; exit 127"
        imports ["w"] >>= (`shouldSatisfy` \(code, _, err) -> code == ExitFailure 1 && T.isInfixOf ("nm cannot read " <> object) err)
        nm "exit 0"
        imports ["w"] >>= (`shouldSatisfy` \(code, _, err) -> code == ExitFailure 1 && T.isInfixOf "imports no _IOConnectCallScalarMethod" err)
        imports ["nowhere"] >>= (`shouldSatisfy` \(code, _, err) -> code == ExitFailure 1 && T.isInfixOf "no hsed_kext.o under nowhere" err)
        imports [] >>= (`shouldSatisfy` \(code, _, _) -> code == ExitFailure 2)
