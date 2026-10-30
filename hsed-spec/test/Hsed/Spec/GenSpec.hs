{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-|
Module      : Hsed.Spec.GenSpec
Description : Test specifications for Hsed.Spec.Gen
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The printer assembles a module from the declarations the rules print. The
golden module is Core Tables 46 (AC_element), 52 (adv_key_mode) and 75
(clock_time) under each errata set, printed by the typeRow and enumValues
rules and assembled into the types module, checked in as
@test/golden/Core-Types-\<set\>.hs@; a missing golden file is written and the
test left pending for review. The printed modules are also type-checked with
@ghc -fno-code -Wall -Werror@ against hsed-core (via @stack exec@ unless the
test already runs inside stack's environment); where neither can run the check
is pending with a message. The generic pieces (imports merged, declarations
ordered, the Haddock paragraph wrapped) are tested on their own.
-}
module Hsed.Spec.GenSpec (spec) where

import qualified Data.Text          as T
import           RIO
import           RIO.Directory      (createDirectoryIfMissing, doesFileExist)
import           RIO.FilePath       (takeDirectory, (</>))
import           RIO.List           (sort)
import           System.Environment (lookupEnv)
import           System.Process     (readProcessWithExitCode)
import           Test.Hspec

import           Hsed.Spec.Common   hiding (spec)
import           Hsed.Spec.Errata
import           Hsed.Spec.Fixtures
import           Hsed.Spec.Format
import           Hsed.Spec.Gen
import           Hsed.Spec.Rules.EnumValues (EnumDecl (..), UIDRef (..), renderEnum)
import           Hsed.Spec.Rules.TypeRow (TypeRowDecl (..), renderTypeRow, typesModule)


-- * The golden tables, as spec/ir/core-2.01.json records them

prov :: Text -> Text -> Int -> Int -> Int -> Text -> Text -> Provenance
prov tid cap pdf printed ord sp h = Provenance
  { spec = core, tableId = tid, caption = cap, pdfPage1 = pdf, printedPage = printed
  , ordinal = ord, structPath = sp, cellsHash = h }

typeRow :: Env -> Text -> [Word8] -> Text -> Provenance -> Decl
typeRow env n bytes f p = renderTypeRow TypeRowDecl
  { name = n, uidBytes = bytes, formatText = f
  , typeDef = either (const Nothing) Just (parseFormat env f), provenance = p }

t46 :: Env -> Decl
t46 env = typeRow env "AC_element" [0, 0, 0, 5, 0, 0, 8, 1] "List_Type, *, ACE_expression"
  (prov "T46" "Table 46 AC_element" 100 83 48 "0/1288" "5b2d8106fd18ac0eac32b19445a2983555e360bafabeb488440472f5d15fa480")

-- | Only its name matters here (the enumeration's instance refers to it); the
--   provenance is not from the IR.
t51 :: Env -> Decl
t51 env = typeRow env "adv_key_mode" [0, 0, 0, 5, 0, 0, 4, 0x0F] "Enumeration_Type, 0, 7"
  (prov "T51" "Table 51 adv_key_mode" 102 85 53 "0/1316" "806db1ea2e8a9f73d3ae112b36a63960f063a3c9820a93fb9039ff90844fea15")

-- | Table 52, its type's UID spelled out (its row is not in the golden module).
t52 :: Decl
t52 = renderEnum Literal t52Decl

-- | Table 52 beside its row T51 in one module.
t52Beside51 :: Decl
t52Beside51 = renderEnum SameModule t52Decl

t52Decl :: EnumDecl
t52Decl = EnumDecl
  { name = "adv_key_mode"
  , ctors = [("Wait for AdvKey_Req", 0), ("Auto-advance keys", 1)] <> [ ("Reserved", v) | v <- [2 .. 7] ]
  , typeUID = [0, 0, 0, 5, 0, 0, 4, 0x0F]
  , range = (0, 7)
  , provenance = prov "T52" "Table 52 adv_key_mode Enumeration Values" 102 85 54 "0/1318"
                      "3aa5da03797880028a1caabb23976011a03d26b3011e4dd05f827a3f4c16d915" }

t75 :: Env -> ErrataSet -> Decl
t75 env es = typeRow env "clock_time" [0, 0, 0, 5, 0, 0, 0x18, 5] corrected
  (prov "T75" "Table 75 clock_time" 107 90 77 "0/1429" "c349b5b0b0bae90ca8cb7ad917b3d90f4c443c408da83d20ed8b16ba9e42e127")
  where
    -- under scott, the correction Table 75's annotation carries
    corrections = [ Erratum core "T75" "Fractoin" "Fraction" "annotation at core-2.01.pdf#page=107&T75" | es == Scott ]
    corrected = fst (applyErrata corrections (Site core "T75" (DataCell 0 2 "Format") (Just 107))
                                 "Struct_Type, Year, Month, Day, Hour, Minute, Seconds, Fractoin")

golden :: Env -> ErrataSet -> [Decl]
golden env es = [t46 env, t52, t75 env es]

goldenFile :: ErrataSet -> FilePath
goldenFile es = "test/golden/Core-Types-" <> T.unpack (errataSetName es) <> ".hs"


coreTypes :: Text
coreTypes = "Hsed.Gen.Core.Types"

-- | The module the golden files print: type rows and an enumeration together,
--   as M1 generated them, so that the printer's output is pinned on its own.
goldenModule :: GenModule
goldenModule = GenModule
  { leaf = "Types", description = "Type objects and their enumerations"
  , notes = [] }


-- * Type-checking a printed module

-- | @ghc -fno-code -Wall -Werror@ on the module, against the hsed-core in stack's
--   package database: directly when this process already runs under stack
--   (GHC_PACKAGE_PATH set, as @stack test@ does), else through @stack exec@.
compiles :: [Decl] -> Expectation
compiles decls = withSystemTempDirectory "hsed-gen" $ \dir -> do
  let file = dir </> "Hsed" </> "Gen" </> "Core" </> "Types.hs"
      ghcArgs = ["-fno-code", "-Wall", "-Werror", "-package", "hsed-core", "-i" <> dir, file]
  createDirectoryIfMissing True (takeDirectory file)
  writeFileUtf8 file (renderModule coreTypes goldenModule decls)
  underStack <- isJust <$> lookupEnv "GHC_PACKAGE_PATH"
  let (prog, args) = if underStack then ("ghc", ghcArgs) else ("stack", ["exec", "--", "ghc"] <> ghcArgs)
  result <- try (readProcessWithExitCode prog args "")
  case result of
    Left (e :: IOException)        -> pendingWith ("cannot run " <> prog <> " here: " <> show e)
    Right (ExitSuccess, _, _)      -> pure ()
    Right (ExitFailure _, out, err) -> expectationFailure (prog <> " rejected the module:\n" <> out <> err)


spec :: Spec
spec = do
  env <- runIO coreEnv

  describe "renderModule" $ do
    forM_ [Scott, Published] $ \es ->
      it ("prints T46, T52 and T75 under " <> T.unpack (errataSetName es) <> " as the golden file has them") $ do
        let actual = renderModule coreTypes goldenModule (golden env es)
            path   = goldenFile es
        exists <- doesFileExist path
        if exists
          then do
            expected <- readFileUtf8 path
            T.lines actual `shouldBe` T.lines expected
          else do
            writeFileUtf8 path actual
            pendingWith ("wrote " <> path <> "; review it and run again")
    it "prints the same text for the same input in any order" $
      renderModule coreTypes goldenModule (reverse (golden env Scott)) `shouldBe` renderModule coreTypes goldenModule (golden env Scott)
    it "orders the definitions by table number" $ do
      let text = renderModule coreTypes goldenModule (reverse (golden env Scott))
          position needle = T.length (fst (T.breakOn needle text))
          positions = map position ["uAC_elementType ::", "data Core_adv_key_mode", "uclock_timeType ::"]
      positions `shouldBe` sort positions
      positions `shouldSatisfy` all (< T.length text)
    it "gives every definition its provenance line" $ do
      let text = renderModule coreTypes goldenModule (golden env Scott)
          count needle = length (T.breakOnAll needle text)
      count "-- | core-2.01 T46 AC_element (PDF 100 / p.83, ord 48, sha256:5b2d8106fd18)" `shouldBe` 2
      count "-- | core-2.01 T52 adv_key_mode Enumeration Values (PDF 102 / p.85, ord 54, sha256:3aa5da037978)" `shouldBe` 2
      count "-- | core-2.01 T75 clock_time (PDF 107 / p.90, ord 77, sha256:c349b5b0b0ba)" `shouldBe` 2
    it "prints the two shapes of a Type row: its UID and, when the Format parsed, its TypeDef" $ do
      let text = renderModule coreTypes goldenModule [t46 env]
      text `shouldSatisfy` T.isInfixOf "uAC_elementType :: UID\nuAC_elementType = uid 0x00 0x00 0x00 0x05 0x00 0x00 0x08 0x01\n"
      text `shouldSatisfy` (not . T.isInfixOf "fAC_elementType")
      text `shouldSatisfy` T.isInfixOf "tAC_element :: TypeDef\ntAC_element = ListType Star (Ref \"ACE_expression\")\n"
    it "omits the TypeDef of a Format that did not parse" $ do
      let text = renderModule coreTypes goldenModule (golden env Published)
      text `shouldSatisfy` T.isInfixOf "-- tclock_time: no TypeDef, the Format did not parse under this errata set"
      text `shouldSatisfy` (not . T.isInfixOf "tclock_time ::")
      renderModule coreTypes goldenModule (golden env Scott)
        `shouldSatisfy` T.isInfixOf "tclock_time = StructType [Ref \"Year\",Ref \"Month\",Ref \"Day\",Ref \"Hour\",Ref \"Minute\",Ref \"Seconds\",Ref \"Fraction\"]"
    it "refers to the row's UID name in its module or another, or spells the UID out" $ do
      t52Beside51.declBody `shouldSatisfy` elem "  typeUID = uadv_key_modeType"
      t52.declBody `shouldSatisfy` elem "  typeUID = uid 0x00 0x00 0x00 0x05 0x00 0x00 0x04 0x0f"
      renderModule coreTypes goldenModule [t51 env, t52Beside51] `shouldSatisfy` T.isInfixOf "  typeUID = uadv_key_modeType\n  range   = (0, 7)\n"
      let elsewhere = renderEnum (InModule "Hsed.Gen.Core.Types") t52Decl
      elsewhere.declBody `shouldSatisfy` elem "  typeUID = uadv_key_modeType"
      elsewhere.declImports `shouldSatisfy` elem (ImportAll "Hsed.Gen.Core.Types")
    it "imports only what its definitions use" $ do
      renderModule coreTypes goldenModule [t52] `shouldSatisfy` T.isInfixOf "import           Prelude       (Bounded, Enum, Eq, Ord, Show)\n"
      renderModule coreTypes goldenModule [t52] `shouldSatisfy` T.isInfixOf "import           Hsed.Core.UID (uid)\n"
      renderModule coreTypes goldenModule [t46 env] `shouldSatisfy` (not . T.isInfixOf "import           Prelude")
      renderModule coreTypes goldenModule [t75 env Published] `shouldSatisfy` (not . T.isInfixOf "import           Hsed.Core.Types")
      renderModule coreTypes goldenModule [] `shouldSatisfy` (not . T.isInfixOf "import")

  describe "the printed module" $
    forM_ [ ("scott", golden env Scott), ("published", golden env Published)
          , ("enumerations only", [t52]), ("rows only, one of them unparsed", [t46 env, t75 env Published])
          , ("an enumeration and its row", [t51 env, t52Beside51]), ("empty", []) ] $ \(label, decls) ->
      it ("type-checks under -Wall -Werror: " <> label) $ compiles decls

  describe "the imports" $ do
    let d imports = (decl (prov "T1" "Table 1 x" 1 1 1 "0/1" "") "x" ["x = ()"]) { declImports = imports }
        importsOf ds = filter ("import" `T.isPrefixOf`) (T.lines (renderModule "M" typesModule ds))
    it "unite the lists of one module, sorted case-insensitively, and align them on the longest module name" $
      importsOf [d [ImportSome "Prelude" ["String"], ImportSome "Hsed.Core.UID" ["uid"]], d [ImportSome "Prelude" ["Eq", "String"], ImportSome "Hsed.Core.UID" ["UID"]]]
        `shouldBe` [ "import           Hsed.Core.UID (UID, uid)"
                   , "import           Prelude       (Eq, String)" ]
    it "let a whole-module import win over a list of the same module" $
      importsOf [d [ImportSome "Data.Map" ["Map"]], d [ImportAll "Data.Map"]] `shouldBe` ["import           Data.Map"]

  describe "the module header" $ do
    it "wraps the module's notes into the generated-by paragraph" $
      wrapText 80 "a bb ccc" `shouldBe` ["a bb ccc"]
    it "keeps every line within the width, words whole" $ do
      let ls = wrapText 20 "Generated by hsedc: do not edit. Each definition names its table"
      ls `shouldSatisfy` all ((<= 20) . T.length)
      T.unwords ls `shouldBe` "Generated by hsedc: do not edit. Each definition names its table"
    it "names the module and its file" $ do
      moduleName core typesModule `shouldBe` "Hsed.Gen.Core.Types"
      moduleName (SpecId "opal-2.30") typesModule `shouldBe` "Hsed.Gen.Opal.Types"
      modulePath "hsed/gen" "Hsed.Gen.Core.Types" `shouldBe` "hsed/gen/Hsed/Gen/Core/Types.hs"
      map specLabel [core, SpecId "opal-2.30"] `shouldBe` ["Core", "Opal"]
    it "unites the language extensions the declarations need with the two every module has, sorted" $ do
      let d exts = (decl (prov "T1" "Table 1 x" 1 1 1 "0/1" "") "x" ["x = ()"]) { declExtensions = exts }
          pragmas ds = filter ("{-# LANGUAGE" `T.isPrefixOf`) (T.lines (renderModule "M" typesModule ds))
      pragmas [] `shouldBe` ["{-# LANGUAGE NoImplicitPrelude #-}", "{-# LANGUAGE OverloadedStrings #-}"]
      pragmas [d ["TypeOperators", "DataKinds"], d ["DataKinds"]]
        `shouldBe` [ "{-# LANGUAGE DataKinds #-}", "{-# LANGUAGE NoImplicitPrelude #-}", "{-# LANGUAGE OverloadedStrings #-}"
                   , "{-# LANGUAGE TypeOperators #-}" ]

  describe "provenanceComment" $ do
    it "names the spec, table, caption title, pages, ordinal and hash prefix" $
      provenanceComment (prov "T46" "Table 46 AC_element" 100 83 48 "0/1288" "3f1a2b3c4d5e6f708192a3b4c5d6e7f8")
        `shouldBe` "-- | core-2.01 T46 AC_element (PDF 100 / p.83, ord 48, sha256:3f1a2b3c4d5e)"
    it "takes the caption's number printed with leading zeros, and keeps a caption of another table whole" $ do
      provenanceComment (prov "T4" "Table 04 Token Types" 32 15 6 "0/1" "00")
        `shouldBe` "-- | core-2.01 T4 Token Types (PDF 32 / p.15, ord 6, sha256:00)"
      provenanceComment (prov "T4" "Table 40 Feature Codes" 32 15 6 "0/1" "00")
        `shouldBe` "-- | core-2.01 T4 Table 40 Feature Codes (PDF 32 / p.15, ord 6, sha256:00)"
    it "prints a caption in ASCII, a dash as -, as every comment of a generated module is (Core Table 22, PDF 46)" $
      provenanceComment (prov "T22" "Table 22 Secure Messaging Packet \x2013 Payload Field" 46 29 24 "0/1" "00")
        `shouldBe` "-- | core-2.01 T22 Secure Messaging Packet - Payload Field (PDF 46 / p.29, ord 24, sha256:00)"

  describe "ascii" $
    it "makes a dash -, an ellipsis ..., curly quotes straight, anything else not ASCII ?" $
      ascii "a \x2013 b \x2014 c\x2026 \x201c\&d\x201d \x2018\&e\x2019 \x00e9" `shouldBe` "a - b - c... \"d\" 'e' ?"

  describe "tableNumber" $
    it "reads the number of an anchor id, and sorts an id without one last" $
      map (tableNumber . \tid -> prov tid "" 0 0 0 "" "") ["T46", "T240", "doc"] `shouldBe` [46, 240, maxBound]
