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
prints the steps H0 to H14, and knows kext build 625 by its UUID. Nothing
here runs either script: their pre-flights read the IORegistry, which the
suite does not, and a snapshot reads the drive's volume.
-}
module Hsed.ToolsSpec (spec) where

import qualified RIO.Char         as C
import           RIO
import           RIO.Process      (proc, readProcess, withProcessContextNoLogging)
import qualified RIO.Text         as T
import qualified RIO.Text.Partial as T'
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
    it "knows kext build 625 by its UUID, read byte for byte (design 13.12)" $ do
      text <- script "../tools/m4c-hw.sh"
      text `shouldSatisfy` T.isInfixOf "373BA509-89F7-3B36-A977-BB65C7635F1B)"
    it "asks build/bin/hsed-jig for the shim's command, bind and CDB builder, and the shared hsed-jig only for no _hsed_kext_ symbol" $ do
      text <- script "../tools/m4c-hw.sh"
      for_ ["for s in _hsed_kext_command _hsed_kext_bind _hsed_cdb_build; do", "grep -q '_hsed_kext_' <<<\"$symbols\""] $ \l ->
        text `shouldSatisfy` T.isInfixOf l
