{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.KextSourceSpec
Description : Test specifications for the C shim's source: it issues exactly these commands, built and checked in the portable C
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The third of the jig's safeties against the attached drive (design 9.3),
the one that holds whatever the Haskell above it does: the C shim, read here
as text (@cbits/hsed_kext.{c,h}@, compiled only under the flag @kext@, and
@cbits/hsed_cdb.{c,h}@, which "Hsed.Transport.CdbCSpec" runs), issues
exactly design 9.3's five commands, and every byte of what it issues, and
every decision of a bind, comes from the portable C. A check of the
source's tokens against drift, not a proof of what it does: @hsed_kext.c@
never runs here, and what decides is in @hsed_cdb.c@, which the tests run
through FFI (the M4c review found one-line edits of the earlier shim that
these checks, then keyed on exact spellings, let by).

* The kext shim names no CDB byte, no send token and no template: none of
  the opcodes 0x5E, 0xB5, 0xA1, 0xA2 or 0x5C written as hex, nor
  @PIO_DataOut@, @kIODirectionOut@, @SECURITY_PROTOCOL_OUT@, nor a
  @static const uint8_t@ array, in either case.
* Its calls into the kext are exactly three, Open, Close and the SCSI
  command, by the dispatch table's selectors 0, 1 and 2, no other method
  (not the fourth, which has the kernel issue commands of its own, nor any
  struct, asynchronous, mapping or trap call): its code names an IOConnect
  function three times, each time called (spaces or none before the
  parenthesis), never taken by address; only the SCSI command passes
  scalars, all six of them the array @hsed_cdb_scalars@ fills, which the
  shim neither writes nor copies itself.
* In @hsed_kext_command@, @hsed_cdb_scalars@ (the scalars, the echo, the
  buffer and an IF-SEND's payload in it, all in the portable C) comes
  before that one call, which is made only once it answered OK.
* @hsed_kext_open@ tests the effective user id before it opens anything;
  @hsed_kext_bind@ finds the TPer again by its registry id, counts its user
  clients and reads its serial, and binds only once @hsed_cdb_bind_check@
  took them and the base.
* @cbits/hsed_cdb.c@ holds exactly four templates, each
  "Hsed.Transport.Cdb".'cdbFor''s with its protocol, count and ComID bytes
  zeroed, at the offsets it writes them; 5Eh and B5h once each; no IOKit;
  @cbits/hsed_cdb.h@ names two directions, In 1 and Out 2, and numbers the
  shapes as 'shapeCode' does.
* The @hsed_kext_info@ its static assertions lay out is the one
  "Hsed.Transport.Kext" reads.
* No module under src, gen or app imports an @hsed_cdb_@ function, and only
  "Hsed.Transport.MacKext" imports @hsed_kext_@ ones.

Besides: built without the package's flag @kext@ (as this suite is),
'openKext' refuses before anything.
-}
module Hsed.Transport.KextSourceSpec (spec) where

import qualified RIO.ByteString         as B
import qualified RIO.Char               as C
import qualified RIO.List               as L
import           RIO
import           RIO.Directory          (doesDirectoryExist, listDirectory)
import           RIO.FilePath           (takeExtension, (</>))
import qualified RIO.Text               as T
import qualified RIO.Text.Partial       as T'
import           Test.Hspec

import           Hsed.Jig.Files         (runningAsRoot)
import           Hsed.Transport.Cdb     (Shape (..), cdbFor, shapeCode)
import           Hsed.Transport.Drive   (InterfaceType (..))
import           Hsed.Transport.Kext    (InfoLayout (..), infoLayout)
import           Hsed.Transport.MacKext (KextOpts (..), openKext)


-- | A file of the package (the suite runs in the package's directory).
source :: FilePath -> IO Text
source = fmap decodeUtf8Lenient . readFileBinary

-- | The kext shim, its header, the portable C and its header.
shim, header, cdbC, cdbH :: IO Text
shim   = source "cbits/hsed_kext.c"
header = source "cbits/hsed_kext.h"
cdbC   = source "cbits/hsed_cdb.c"
cdbH   = source "cbits/hsed_cdb.h"

-- | The text of each call of a function: its arguments, to the parenthesis
--   that closes them. A call is the function's name as a whole word, then
--   any white space, then the parenthesis that opens its arguments.
callsOf :: Text -> Text -> [Text]
callsOf fn src = [ T.pack (arguments (0 :: Int) (T.unpack rest)) | Just rest <- map called (namedAt fn src) ]
  where
    called rest = T.stripPrefix "(" (T.stripStart rest)
    arguments depth = \case
      ')' : _ | depth == 0 -> []
      c : cs               -> c : arguments (depth + nesting c) cs
      []                   -> []
    nesting = \case
      '(' -> 1
      ')' -> -1
      _   -> 0

-- | What follows each place a name stands in a text as a whole word (no
--   identifier character before or after it).
namedAt :: Text -> Text -> [Text]
namedAt name src =
  [ following
  | (preceding, at') <- T'.breakOnAll name src
  , let following = T.drop (T.length name) at'
  , maybe True (not . identifier) (lastChar preceding)
  , maybe True (not . identifier . fst) (T.uncons following) ]

-- | A text's last character, if it has one.
lastChar :: Text -> Maybe Char
lastChar t = if T.null t then Nothing else Just (T'.last t)

-- | Whether a character may be part of a C identifier.
identifier :: Char -> Bool
identifier c = C.isAlphaNum c || c == '_'

-- | Every identifier of a text that begins with a prefix, as many times as
--   it stands there.
identifiersFrom :: Text -> Text -> [Text]
identifiersFrom prefix src =
  [ prefix <> T.takeWhile identifier following
  | (preceding, at') <- T'.breakOnAll prefix src
  , maybe True (not . identifier) (lastChar preceding)
  , let following = T.drop (T.length prefix) at' ]

-- | C source without its comments, each one a space, so that the checks of
--   its code read its code alone.
code :: Text -> Text
code = T.pack . go . T.unpack
  where
    go = \case
      '/' : '*' : rest -> ' ' : go (closing rest)
      '/' : '/' : rest -> go (dropWhile (/= '\n') rest)
      c : rest         -> c : go rest
      []               -> []
    closing = \case
      '*' : '/' : rest -> rest
      _ : rest         -> closing rest
      []               -> []

-- | A call's arguments, split at the commas, trimmed.
argumentsOf :: Text -> [Text]
argumentsOf = map T.strip . T'.splitOn ","

-- | The value a @#define@ gives a name, if the source defines it: the word
--   after the name, a comment after it left out.
defined :: Text -> Text -> Maybe Text
defined name src = listToMaybe [ v | l <- T.lines src, "#define" : n : v : _ <- [T.words l], n == name ]

-- | The names the source @#define@s.
defines :: Text -> [Text]
defines src = [ n | l <- T.lines src, "#define" : n : _ <- [T.words l] ]

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

-- | A function's body as the source defines it: the lines after the one that
--   begins @int name(@, to its closing brace at the line's start.
bodyOf :: Text -> Text -> Text
bodyOf fn = T.unlines . takeWhile (/= "}") . drop 1 . dropWhile (not . (("int " <> fn <> "(") `T.isPrefixOf`)) . T.lines

-- | Where a text first appears in another, if it does, as a whole word or not.
position :: Text -> Text -> Maybe Int
position needle hay = case T'.breakOn needle hay of
  (ahead, rest) | not (T.null rest) -> Just (T.length ahead)
  _                                  -> Nothing

-- | The names and values of @enum hsed_shape@ in a header's text.
enumValues :: Text -> [(Text, Int)]
enumValues src =
  [ (T.strip name, v)
  | item <- T'.splitOn "," body, [name, value] <- [T'.splitOn "=" item], Just v <- [readMaybe (T.unpack (T.strip value))] ]
  where body = T.takeWhile (/= '}') (T.drop 1 (T.dropWhile (/= '{') (snd (T'.breakOn "enum hsed_shape" src))))

-- | The C name of a shape, as cbits/hsed_cdb.h's enum spells it: Level0Recv, HSED_SHAPE_LEVEL0_RECV.
cName :: Shape -> Text
cName s = "HSED_SHAPE_" <> T.toUpper (T.pack (concatMap snake (zip [0 :: Int ..] (show s))))
  where snake (i, ch) = [ '_' | i > 0, C.isUpper ch ] <> [ch]

-- | The Haskell sources under a directory, every depth.
haskellFiles :: FilePath -> IO [FilePath]
haskellFiles d = do
  isDir <- doesDirectoryExist d
  if not isDir
    then pure [ d | takeExtension d == ".hs" ]
    else listDirectory d >>= fmap concat . traverse (haskellFiles . (d </>)) . L.sort

-- | The C functions a Haskell source imports: each @foreign import@
--   declaration's entity (the quoted name), or its Haskell name where it
--   gives none.
foreignImports :: Text -> [Text]
foreignImports src =
  [ if T.null quoted then lastWord decl else quoted
  | (l, next) <- zip ls (drop 1 ls <> [""]), "foreign import" `T.isPrefixOf` T.stripStart l
  , let decl = fst (T'.breakOn "::" (l <> " " <> next))
  , let quoted = T.takeWhile (/= '"') (T.drop 1 (T.dropWhile (/= '"') decl)) ]
  where
    ls = T.lines src
    lastWord = fromMaybe "" . listToMaybe . reverse . T.words

-- | cdbFor's CDB of a shape on 0x1004 of one block, with the bytes at some
--   offsets zeroed.
zeroedAt :: [Int] -> InterfaceType -> Shape -> [Word8]
zeroedAt offsets i s = [ if k `elem` offsets then 0 else b | (k, b) <- zip [0 ..] (either (const []) B.unpack (cdbFor i s 0x1004 1)) ]

spec :: Spec
spec = describe "the C shim, cbits/" $ do
  describe "hsed_kext.c and hsed_kext.h: exactly these commands" $ do
    it "name no CDB byte, no send token and no template, in either case" $ do
      texts <- sequence [shim, header]
      [ tok | text <- texts, tok <- [ "0x5e", "0xb5", "0xa1", "0xa2", "0x5c", "pio_dataout", "kiodirectionout"
                                    , "security_protocol_out", "static const uint8_t" ]
            , tok `T.isInfixOf` T.toLower text ]
        `shouldBe` []
    it "call the kext's Open, Close and SCSI command, three calls by the dispatch table's selectors 0, 1, 2, and no other method" $ do
      src <- code <$> shim
      let selectors = mapMaybe (listToMaybe . drop 1 . argumentsOf) (callsOf "IOConnectCallScalarMethod" src)
      L.sort selectors `shouldBe` ["HSED_KEXT_SELECTOR_CLOSE", "HSED_KEXT_SELECTOR_OPEN", "HSED_KEXT_SELECTOR_SCSI"]
      [ (n, defined n src) | n <- defines src, "SELECTOR" `T.isInfixOf` n ]
        `shouldBe` [("HSED_KEXT_SELECTOR_OPEN", Just "0u"), ("HSED_KEXT_SELECTOR_CLOSE", Just "1u"), ("HSED_KEXT_SELECTOR_SCSI", Just "2u")]
      texts <- sequence [shim, header]
      [ tok | text <- texts, tok <- [ "IOConnectCallMethod", "IOConnectCallStructMethod", "IOConnectCallAsync"
                                    , "IOConnectMapMemory", "IOConnectTrap", "UpdatePropertiesInIORegistry" ]
            , T.toLower tok `T.isInfixOf` T.toLower text ]
        `shouldBe` []
    it "name an IOConnect function three times in their code, each a call of IOConnectCallScalarMethod, whatever the spacing, never its address" $ do
      src <- code <$> shim
      identifiersFrom "IOConnect" src `shouldBe` replicate 3 "IOConnectCallScalarMethod"
      length (namedAt "IOConnectCallScalarMethod" src) `shouldBe` 3
      length (callsOf "IOConnectCallScalarMethod" src) `shouldBe` 3
      h <- code <$> header
      identifiersFrom "IOConnect" h `shouldBe` []
    it "pass scalars to the SCSI command alone, the six hsed_cdb_scalars fills, which the shim neither writes nor copies" $ do
      src <- code <$> shim
      [ drop 2 (argumentsOf c) | c <- callsOf "IOConnectCallScalarMethod" src ]
        `shouldMatchList` [ ["NULL", "0", "NULL", "NULL"], ["NULL", "0", "NULL", "NULL"], ["input", "6", "output", "&outputs"] ]
      body <- code . bodyOf "hsed_kext_command" <$> shim
      [ T.strip l | l <- T.lines body, "input" `elem` T.words (T.map (\c -> if identifier c then c else ' ') l) ]
        `shouldBe` [ "uint64_t input[6] = { 0, 0, 0, 0, 0, 0 };"
                   , "built = hsed_cdb_scalars(info->sat, shape, info->base_comid, blocks, buf, bufsize, (uint64_t)page, input, cdb_out);"
                   , "ret = IOConnectCallScalarMethod((io_connect_t)info->conn, HSED_KEXT_SELECTOR_SCSI, input, 6, output, &outputs);" ]
      let block = T.drop 1 (snd (T'.breakOn "{" body))
      [ tok | tok <- ["memcpy", "memmove", "memset", "cdb_out[", "input[0]", "input[1]", "input[2]", "input[3]", "input[4]", "input[5]"]
            , tok `T.isInfixOf` block ] `shouldBe` []
      [ n | n <- defines src, "DIRECTION" `T.isInfixOf` n ] `shouldBe` []
    it "fill the scalars and check before the one call in hsed_kext_command: hsed_cdb_scalars, its answer OK, then selector 2" $ do
      body <- code . bodyOf "hsed_kext_command" <$> shim
      map (`position` body) ["hsed_cdb_scalars(", "if (built != HSED_CDB_OK)", "IOConnectCallScalarMethod("]
        `shouldSatisfy` \ps -> all isJust ps && ps == L.sort ps
      map argumentsOf (callsOf "hsed_cdb_scalars" body)
        `shouldBe` [["info->sat", "shape", "info->base_comid", "blocks", "buf", "bufsize", "(uint64_t)page", "input", "cdb_out"]]
      length (callsOf "IOConnectCallScalarMethod" body) `shouldBe` 1
      [ c | c <- ["hsed_cdb_build", "hsed_cdb_check_payload", "hsed_cdb_base_ok"], not (null (namedAt c body)) ] `shouldBe` []
    it "open as root only: hsed_kext_open tests geteuid() before IOServiceOpen" $ do
      body <- code . bodyOf "hsed_kext_open" <$> shim
      map (`position` body) ["geteuid() != 0", "IOServiceOpen("] `shouldSatisfy` \ps -> all isJust ps && ps == L.sort ps
    it "bind after the checks: hsed_kext_bind finds the TPer again, counts its user clients, reads its serial, and binds once hsed_cdb_bind_check took them" $ do
      body <- code . bodyOf "hsed_kext_bind" <$> shim
      let steps = [ "IORegistryEntryIDMatching(info->registry_id)", "count_user_clients(tper)", "read_serial(tper, serial, sizeof serial)"
                  , "checked = hsed_cdb_bind_check(base, info->base_comid, clients, serial, info->serial, sizeof serial);"
                  , "if (checked != HSED_CDB_OK)", "info->base_comid = base;", "return KERN_SUCCESS;" ]
      map (`position` body) steps `shouldSatisfy` \ps -> all isJust ps && ps == L.sort ps
      length (T'.breakOnAll "info->base_comid =" body) `shouldBe` 1
      [ c | c <- ["hsed_cdb_base_ok", "strncmp", "strcmp", "memcmp"], not (null (namedAt c body)) ] `shouldBe` []

  describe "hsed_cdb.c and hsed_cdb.h: the portable C" $ do
    it "holds exactly four templates, each cdbFor's with its protocol, count and ComID zeroed at the offsets it writes them" $ do
      src <- cdbC
      let sat = [3, 4, 6, 7]
          scsi = [1, 2, 3, 9]
      map (`initialiser` src) ["hsed_sat_in", "hsed_sat_out", "hsed_scsi_in", "hsed_scsi_out"]
        `shouldBe` [zeroedAt sat IfSAT PacketRecv, zeroedAt sat IfSAT PacketSend, zeroedAt scsi IfSCSI PacketRecv, zeroedAt scsi IfSCSI PacketSend]
      [ (s, i) | (i, offsets) <- [(IfSAT, sat), (IfSCSI, scsi)], s <- [minBound .. maxBound]
               , zeroedAt offsets i s `notElem` [zeroedAt offsets i PacketRecv, zeroedAt offsets i PacketSend] ]
        `shouldBe` []
      map (`defined` src) [ "HSED_SAT_PROTOCOL", "HSED_SAT_COUNT", "HSED_SAT_COMID_LO", "HSED_SAT_COMID_HI"
                          , "HSED_SCSI_PROTOCOL", "HSED_SCSI_COMID_HI", "HSED_SCSI_COMID_LO", "HSED_SCSI_LENGTH" ]
        `shouldBe` map (Just . tshow) (sat <> scsi)
      length (T'.breakOnAll "static const uint8_t" src) `shouldBe` 4
    it "writes TRUSTED SEND's 5Eh and SECURITY PROTOCOL OUT's B5h once each, and includes no IOKit" $ do
      src <- cdbC
      map (\tok -> length (T'.breakOnAll tok (T.toLower src))) ["0x5e", "0xb5"] `shouldBe` [1, 1]
      [ l | l <- T.lines src, "#include" `T.isPrefixOf` l, any (`T.isInfixOf` l) ["IOKit", "CoreFoundation", "mach"] ] `shouldBe` []
      [ l | l <- T.lines src, "#include" `T.isPrefixOf` l ]
        `shouldBe` ["#include <stddef.h>", "#include <stdint.h>", "#include <string.h>", "#include \"hsed_cdb.h\""]
    it "names two directions in its header, In 1 and Out 2, and no other, and numbers the shapes as shapeCode does" $ do
      h <- cdbH
      c <- cdbC
      [ (n, defined n h) | n <- defines h, "DIRECTION" `T.isInfixOf` n ]
        `shouldBe` [("HSED_DIRECTION_IN", Just "1u"), ("HSED_DIRECTION_OUT", Just "2u")]
      [ n | n <- defines c, "DIRECTION" `T.isInfixOf` n ] `shouldBe` []
      enumValues h `shouldBe` [ (cName s, shapeCode s) | s <- [minBound .. maxBound] ]

  describe "hsed_kext_info" $
    it "is laid out by the source's static assertions as Hsed.Transport.Kext reads it" $ do
      src <- shim
      let l = infoLayout
      L.sort (assertedOffsets src) `shouldBe` L.sort
        [ ("conn", ilConn l), ("registry_id", ilRegistryId l), ("sat", ilSat l), ("serial", ilSerial l)
        , ("kext_build", ilKextBuild l), ("other_clients", ilOtherClients l), ("base_comid", ilBaseComId l) ]
      [ a | a <- [ "sizeof(hsed_kext_info) == " <> tshow (ilBytes l), "_Alignof(hsed_kext_info) == " <> tshow (ilAlignment l)
                 , "->serial) == " <> tshow (ilSerialBytes l), "->kext_build) == " <> tshow (ilKextBuildBytes l) ]
          , not (a `T.isInfixOf` src) ]
        `shouldBe` []

  describe "the Haskell above it" $
    it "imports no hsed_cdb_ function under src, gen or app, and the hsed_kext_ ones in Hsed.Transport.MacKext alone" $ do
      files <- concat <$> traverse haskellFiles ["src", "gen", "app"]
      imports <- traverse (\f -> (,) f . foreignImports <$> source f) files
      [ (f, n) | (f, ns) <- imports, n <- ns, "hsed_cdb_" `T.isPrefixOf` n ] `shouldBe` []
      [ f | (f, ns) <- imports, any ("hsed_kext_" `T.isPrefixOf`) ns ] `shouldBe` ["src/Hsed/Transport/MacKext.hs"]
      [ n | (f, ns) <- imports, f == "src/Hsed/Transport/MacKext.hs", n <- ns ]
        `shouldBe` ["hsed_kext_locate", "hsed_kext_open", "hsed_kext_bind", "hsed_kext_command", "hsed_kext_close"]

  describe "openKext, built without the flag kext" $
    it "refuses before anything is located or opened" $ do
      root <- runningAsRoot
      if root
        then pendingWith "the suite runs as root: never (no sudo stack)"
        else openKext (KextOpts "/dev/disk-hsed-test-none" "S0000000000000") >>= (`shouldBe` Left "built without -fkext") . void
