{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.KextSourceSpec
Description : Test specifications for the kext shim's source: it can only receive, and only Level 0
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The third of the jig's safeties against the attached drive (design 9.3),
the one that holds whatever the Haskell above it does: the C shim
(@cbits/hsed_kext.c@, read here as text; nothing here compiles or calls it)
has no path that sends.

* None of the tokens a send would need: TRUSTED SEND's opcode @0x5E@,
  SECURITY PROTOCOL OUT's @0xB5@, ATA's @PIO_DataOut@, IOKit's
  @kIODirectionOut@, @SECURITY_PROTOCOL_OUT@, in either case.
* Its three calls into the kext are Open, Close and the SCSI command, by
  the selectors of the kext's dispatch table (0, 1, 2); only the SCSI
  command passes scalars, and its direction scalar is In (1), the only
  direction the file names.
* The CDBs it can build are hsed's ("Hsed.Transport.Cdb"), with their
  counts left zero.
* The @hsed_kext_info@ its static assertions lay out is the one
  "Hsed.Transport.Kext" reads.

Besides: built without the package's flag @kext@ (as this suite is),
'openKext' refuses before anything, and @tools/m3-hw.sh@, the hardware
run's checklist, runs no sudo: each sudo it names is in the commands it
prints for Scott.
-}
module Hsed.Transport.KextSourceSpec (spec) where

import qualified RIO.ByteString         as B
import qualified RIO.Char               as C
import qualified RIO.List               as L
import           RIO
import qualified RIO.Text               as T
import qualified RIO.Text.Partial       as T'
import           Test.Hspec

import           Hsed.Jig.Files         (runningAsRoot)
import           Hsed.Transport.Cdb     (level0Cdb)
import           Hsed.Transport.Drive   (InterfaceType (..))
import           Hsed.Transport.Kext    (InfoLayout (..), infoLayout)
import           Hsed.Transport.MacKext (KextOpts (..), openKext)


-- | The shim's source (the suite runs in the package's directory).
shim :: IO Text
shim = decodeUtf8Lenient <$> readFileBinary "cbits/hsed_kext.c"

-- | The shim's header.
header :: IO Text
header = decodeUtf8Lenient <$> readFileBinary "cbits/hsed_kext.h"

-- | The text of each call of a function: its arguments, to the parenthesis
--   that closes them.
callsOf :: Text -> Text -> [Text]
callsOf fn = map (T.pack . arguments (0 :: Int) . T.unpack) . drop 1 . T'.splitOn (fn <> "(")
  where
    arguments depth = \case
      ')' : _ | depth == 0 -> []
      c : cs               -> c : arguments (depth + nesting c) cs
      []                   -> []
    nesting = \case
      '(' -> 1
      ')' -> -1
      _   -> 0

-- | A call's arguments, split at the commas, trimmed.
argumentsOf :: Text -> [Text]
argumentsOf = map T.strip . T'.splitOn ","

-- | The value a @#define@ gives a name, if the source defines it.
defined :: Text -> Text -> Maybe Text
defined name src = listToMaybe
  [ T.unwords rest | l <- T.lines src, "#define" : n : rest <- [T.words l], n == name ]

-- | The bytes of a C array's initialiser: every @0x@ number between its @{@ and its @}@.
initialiser :: Text -> Text -> [Word8]
initialiser name src =
  [ b | tok <- T.split (`elem` (" ,\n" :: String)) body, Just h <- [T.stripPrefix "0x" tok], Just b <- [hexByte h] ]
  where
    body = T.takeWhile (/= '}') (T.drop 1 (T.dropWhile (/= '{') (snd (T'.breakOn (name <> "[12] = {") src))))
    hexByte h
      | T.length h == 2, T.all (`elem` ("0123456789abcdefABCDEF" :: String)) h = readMaybe ("0x" <> T.unpack h)
      | otherwise = Nothing

-- | Each offset the source's static assertions give a field of @hsed_kext_info@.
assertedOffsets :: Text -> [(Text, Int)]
assertedOffsets src =
  [ (field, n)
  | l <- T.lines src
  , Just rest <- [T.stripPrefix "_Static_assert(offsetof(hsed_kext_info, " (T.strip l)]
  , let (field, rest') = T'.breakOn ")" rest
  , Just n <- [readMaybe . T.unpack . T.takeWhile (`elem` ("0123456789" :: String)) =<< T.stripPrefix ") == " rest']
  ]

spec :: Spec
spec = describe "the kext shim, cbits/hsed_kext.c" $ do
  describe "can only receive" $ do
    it "names nothing a send would need, in either case: 0x5E, 0xB5, PIO_DataOut, kIODirectionOut, SECURITY_PROTOCOL_OUT" $ do
      texts <- sequence [shim, header]
      [ tok | text <- texts, tok <- ["0x5e", "0xb5", "pio_dataout", "kiodirectionout", "security_protocol_out"]
            , tok `T.isInfixOf` T.toLower text ]
        `shouldBe` []
    it "calls the kext's Open, Close and SCSI command alone, by the dispatch table's selectors 0, 1, 2" $ do
      src <- shim
      let selectors = mapMaybe (listToMaybe . drop 1 . argumentsOf) (callsOf "IOConnectCallScalarMethod" src)
      L.sort selectors `shouldBe` ["HSED_KEXT_SELECTOR_CLOSE", "HSED_KEXT_SELECTOR_OPEN", "HSED_KEXT_SELECTOR_SCSI"]
      map (`defined` src) ["HSED_KEXT_SELECTOR_OPEN", "HSED_KEXT_SELECTOR_CLOSE", "HSED_KEXT_SELECTOR_SCSI"]
        `shouldBe` [Just "0u", Just "1u", Just "2u"]
    it "passes scalars to the SCSI command alone, its direction In, the only direction it names" $ do
      src <- shim
      [ drop 2 (argumentsOf c) | c <- callsOf "IOConnectCallScalarMethod" src ]
        `shouldMatchList` [ ["NULL", "0", "NULL", "NULL"], ["NULL", "0", "NULL", "NULL"], ["input", "6", "output", "&outputs"] ]
      [ l | l <- T.lines src, "[4]" `T.isInfixOf` l ] `shouldSatisfy` ((== ["[4] = HSED_KEXT_DIRECTION_IN,"]) . map T.strip)
      [ n | l <- T.lines src, "#define" : n : _ <- [T.words l], "DIRECTION" `T.isInfixOf` n ] `shouldBe` ["HSED_KEXT_DIRECTION_IN"]
      defined "HSED_KEXT_DIRECTION_IN" src `shouldBe` Just "1u"
    it "builds hsed's Level 0 IF-RECV CDBs and no other, their counts left zero" $ do
      src <- shim
      let zeroed i = fmap (\b -> B.unpack (B.take i b <> B.singleton 0 <> B.drop (i + 1) b))
      Right (initialiser "hsed_level0_sat" src) `shouldBe` zeroed 4 (level0Cdb IfSAT 1)
      Right (initialiser "hsed_level0_scsi" src) `shouldBe` zeroed 9 (level0Cdb IfSCSI 1)
      map (`defined` src) ["HSED_SAT_COUNT", "HSED_SCSI_LENGTH"] `shouldBe` [Just "4", Just "9"]
      length (T'.breakOnAll "static const uint8_t" src) `shouldBe` 2

  describe "hsed_kext_info" $
    it "is laid out by the source's static assertions as Hsed.Transport.Kext reads it" $ do
      src <- shim
      let l = infoLayout
      L.sort (assertedOffsets src) `shouldBe` L.sort
        [ ("conn", ilConn l), ("registry_id", ilRegistryId l), ("sat", ilSat l), ("serial", ilSerial l)
        , ("kext_build", ilKextBuild l), ("other_clients", ilOtherClients l) ]
      [ a | a <- [ "sizeof(hsed_kext_info) == " <> tshow (ilBytes l), "_Alignof(hsed_kext_info) == " <> tshow (ilAlignment l)
                 , "->serial) == " <> tshow (ilSerialBytes l), "->kext_build) == " <> tshow (ilKextBuildBytes l) ]
          , not (a `T.isInfixOf` src) ]
        `shouldBe` []

  describe "openKext, built without the flag kext" $
    it "refuses before anything is located or opened" $ do
      root <- runningAsRoot
      if root
        then pendingWith "the suite runs as root: never (no sudo stack)"
        else openKext (KextOpts "/dev/disk-hsed-test-none" Nothing) >>= (`shouldBe` Left "built without -fkext") . void

  describe "tools/m3-hw.sh, the hardware run's checklist" $
    it "runs no sudo: each it names is in the commands it prints, or a comment" $ do
      script <- decodeUtf8Lenient <$> readFileBinary "../tools/m3-hw.sh"
      [ l | l <- outsideHeredocs (T.lines script), "sudo" `T.isInfixOf` l, not ("#" `T.isPrefixOf` T.stripStart l) ]
        `shouldBe` []

-- | The lines of a shell script outside its here-documents (@<<EOF@ to @EOF@).
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
