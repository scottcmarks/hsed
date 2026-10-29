{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-|
Module      : Hsed.Spec.GenSpec
Description : Test specifications for Hsed.Spec.Gen
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The golden module is Core Tables 46 (AC_element), 52 (adv_key_mode) and 75
(clock_time) under each errata set, checked in as
@test/golden/Core-Types-\<set\>.hs@; a missing golden file is written and the
test left pending for review. The printed modules are also type-checked with
@ghc -fno-code -Wall -Werror@ against hsed-core (via @stack exec@ unless the
test already runs inside stack's environment); where neither can run the check
is pending with a message.
-}
module Hsed.Spec.GenSpec (spec) where

import qualified Data.Text          as T
import           RIO
import           RIO.Directory      (createDirectoryIfMissing, doesFileExist)
import           RIO.FilePath       (takeDirectory, (</>))
import           RIO.List           (sort)
import qualified RIO.Set            as Set
import           System.Environment (lookupEnv)
import           System.Process     (readProcessWithExitCode)
import           Test.Hspec

import           Hsed.Spec.Common   hiding (spec)
import           Hsed.Spec.Errata
import           Hsed.Spec.Fixtures
import           Hsed.Spec.Format
import           Hsed.Spec.Gen


-- * The golden tables, as spec/ir/core-2.01.json records them

prov :: Text -> Text -> Int -> Int -> Int -> Text -> Text -> Provenance
prov tid cap pdf printed ord sp h = Provenance
  { spec = core, tableId = tid, caption = cap, pdfPage1 = pdf, printedPage = printed
  , ordinal = ord, structPath = sp, cellsHash = h }

typeRow :: Env -> Text -> [Word8] -> Text -> Provenance -> Decl
typeRow env n bytes f p = TypeRow TypeRowDecl
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

t52 :: Decl
t52 = Enum EnumDecl
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


-- * Type-checking a printed module

-- | @ghc -fno-code -Wall -Werror@ on the module, against the hsed-core in stack's
--   package database: directly when this process already runs under stack
--   (GHC_PACKAGE_PATH set, as @stack test@ does), else through @stack exec@.
compiles :: [Decl] -> Expectation
compiles decls = withSystemTempDirectory "hsed-gen" $ \dir -> do
  let file = dir </> "Hsed" </> "Gen" </> "Core" </> "Types.hs"
      ghcArgs = ["-fno-code", "-Wall", "-Werror", "-package", "hsed-core", "-i" <> dir, file]
  createDirectoryIfMissing True (takeDirectory file)
  writeFileUtf8 file (renderModule coreTypesModule decls)
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
        let actual = renderModule coreTypesModule (golden env es)
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
      renderModule coreTypesModule (reverse (golden env Scott)) `shouldBe` renderModule coreTypesModule (golden env Scott)
    it "orders the definitions by table number" $ do
      let text = renderModule coreTypesModule (reverse (golden env Scott))
          position needle = T.length (fst (T.breakOn needle text))
          positions = map position ["uAC_elementType ::", "data Core_adv_key_mode", "uclock_timeType ::"]
      positions `shouldBe` sort positions
      positions `shouldSatisfy` all (< T.length text)
    it "gives every definition its provenance line" $ do
      let text = renderModule coreTypesModule (golden env Scott)
          count needle = length (T.breakOnAll needle text)
      count "-- | core-2.01 T46 AC_element (PDF 100 / p.83, ord 48, sha256:5b2d8106fd18)" `shouldBe` 3
      count "-- | core-2.01 T52 adv_key_mode Enumeration Values (PDF 102 / p.85, ord 54, sha256:3aa5da037978)" `shouldBe` 2
      count "-- | core-2.01 T75 clock_time (PDF 107 / p.90, ord 77, sha256:c349b5b0b0ba)" `shouldBe` 3
    it "prints the three shapes of a Type row as hsed's TypeUIDs.hs has them" $ do
      let text = renderModule coreTypesModule [t46 env]
      text `shouldSatisfy` T.isInfixOf "uAC_elementType :: UID\nuAC_elementType = uid 0x00 0x00 0x00 0x05 0x00 0x00 0x08 0x01\n"
      text `shouldSatisfy` T.isInfixOf "fAC_elementType :: String\nfAC_elementType = \"List_Type,*,ACE_expression\"\n"
      text `shouldSatisfy` T.isInfixOf "tAC_element :: TypeDef\ntAC_element = ListType Star (Ref \"ACE_expression\")\n"
    it "omits the TypeDef of a Format that did not parse" $ do
      let text = renderModule coreTypesModule (golden env Published)
      text `shouldSatisfy` T.isInfixOf "-- tclock_time: no TypeDef, the Format did not parse under this errata set"
      text `shouldSatisfy` (not . T.isInfixOf "tclock_time ::")
      text `shouldSatisfy` T.isInfixOf "fclock_timeType = \"Struct_Type,Year,Month,Day,Hour,Minute,Seconds,Fractoin\""
      renderModule coreTypesModule (golden env Scott)
        `shouldSatisfy` T.isInfixOf "tclock_time = StructType [Ref \"Year\",Ref \"Month\",Ref \"Day\",Ref \"Hour\",Ref \"Minute\",Ref \"Seconds\",Ref \"Fraction\"]"
    it "refers to the row's UID name when the row is in the module, else spells the UID out" $ do
      renderDecl (Set.fromList ["adv_key_mode"]) t52 `shouldSatisfy` elem "  typeUID = uadv_key_modeType"
      renderDecl Set.empty t52 `shouldSatisfy` elem "  typeUID = uid 0x00 0x00 0x00 0x05 0x00 0x00 0x04 0x0f"
      renderModule coreTypesModule [t51 env, t52] `shouldSatisfy` T.isInfixOf "  typeUID = uadv_key_modeType\n  range   = (0, 7)\n"
    it "imports only what its definitions use" $ do
      renderModule coreTypesModule [t52] `shouldSatisfy` T.isInfixOf "import           Prelude                    (Bounded, Enum, Eq, Ord, Show)\n"
      renderModule coreTypesModule [t52] `shouldSatisfy` T.isInfixOf "import           System.SED.MCTP.Common.UID (uid)\n"
      renderModule coreTypesModule [t46 env] `shouldSatisfy` T.isInfixOf "import           Prelude                    (String)\n"
      renderModule coreTypesModule [t75 env Published] `shouldSatisfy` (not . T.isInfixOf "import           Hsed.Core.Types")
      renderModule coreTypesModule [] `shouldSatisfy` (not . T.isInfixOf "import")

  describe "the printed module" $
    forM_ [ ("scott", golden env Scott), ("published", golden env Published)
          , ("enumerations only", [t52]), ("rows only, one of them unparsed", [t46 env, t75 env Published])
          , ("an enumeration and its row", [t51 env, t52]), ("empty", []) ] $ \(label, decls) ->
      it ("type-checks under -Wall -Werror: " <> label) $ compiles decls

  describe "constructorNames" $ do
    it "names adv_key_mode as today's dEnum does: first value of a label bare, later ones suffixed" $
      map fst (constructorNames "adv_key_mode" (0, 7) advKeyMode)
        `shouldBe` [ "Core_adv_key_mode_Wait_for_AdvKey_Req", "Core_adv_key_mode_Auto_advance_keys", "Core_adv_key_mode_Reserved"
                   , "Core_adv_key_mode_Reserved_3", "Core_adv_key_mode_Reserved_4", "Core_adv_key_mode_Reserved_5"
                   , "Core_adv_key_mode_Reserved_6", "Core_adv_key_mode_Reserved_7" ]
    it "treats a repeated label as one label (padding_type's two None rows)" $
      take 2 (constructorNames "padding_type" (0, 15) paddingType)
        `shouldBe` [("Core_padding_type_None", 0), ("Core_padding_type_None_1", 1)]
    it "keeps a label's values in table order across its rows (life_cycle_state's Unassigned)" $
      filter (T.isInfixOf "Unassigned" . fst) (constructorNames "life_cycle_state" (0, 15) lifeCycleState)
        `shouldBe` [ ("Core_life_cycle_state_Unassigned", 5), ("Core_life_cycle_state_Unassigned_6", 6), ("Core_life_cycle_state_Unassigned_7", 7)
                   , ("Core_life_cycle_state_Unassigned_14", 14), ("Core_life_cycle_state_Unassigned_15", 15) ]
    it "fills the gaps of the range with _NA from the range's lower bound" $ do
      constructorNames "x" (1, 4) [("Idle", 1), ("Active", 3)]
        `shouldBe` [("Core_x_Idle", 1), ("Core_x_NA", 2), ("Core_x_Active", 3), ("Core_x_NA_4", 4)]
      constructorNames "x" (0, 3) [("A", 2)]
        `shouldBe` [("Core_x_NA", 0), ("Core_x_NA_1", 1), ("Core_x_A", 2), ("Core_x_NA_3", 3)]
      map fst (constructorNames "reencrypt_state" (1, 16) reencryptState) `shouldSatisfy` notElem "Core_reencrypt_state_NA"
      map snd (constructorNames "reencrypt_state" (1, 16) reencryptState) `shouldBe` [1 .. 16]
    it "scrubs a label to an identifier: commas dropped, spaces and dashes to underscores" $
      map fst (constructorNames "e" (0, 3) [("RSAES-PKCS1-v1_5", 0), ("SHA 1", 1), ("Issued-Disabled-Frozen", 2), ("ReadLocked, WriteLocked or (none)", 3)])
        `shouldBe` ["Core_e_RSAES_PKCS1_v1_5", "Core_e_SHA_1", "Core_e_Issued_Disabled_Frozen", "Core_e_ReadLocked_WriteLocked_or__none_"]

  describe "joinFormat" $
    it "joins the pieces with commas and no spaces, as hsed's quasiquoter does" $
      map joinFormat [ "List_Type, *, ACE_expression", "Simple_Type,uinteger, 2", "Restricted_Reference_Type{6}, uidref {AuthorityTableUID}", "Base_Type" ]
        `shouldBe` [ "List_Type,*,ACE_expression", "Simple_Type,uinteger,2", "Restricted_Reference_Type{6},uidref {AuthorityTableUID}", "Base_Type" ]

  describe "provenanceComment" $
    it "names the spec, table, caption title, pages, ordinal and hash prefix" $
      provenanceComment (prov "T46" "Table 46 AC_element" 100 83 48 "0/1288" "3f1a2b3c4d5e6f708192a3b4c5d6e7f8")
        `shouldBe` "-- | core-2.01 T46 AC_element (PDF 100 / p.83, ord 48, sha256:3f1a2b3c4d5e)"

  describe "tableNumber" $
    it "reads the number of an anchor id, and sorts an id without one last" $
      map (tableNumber . \tid -> prov tid "" 0 0 0 "" "") ["T46", "T240", "doc"] `shouldBe` [46, 240, maxBound]
  where
    advKeyMode     = [("Wait for AdvKey_Req", 0), ("Auto-advance keys", 1)] <> [ ("Reserved", v) | v <- [2 .. 7] ]
    paddingType    = [("None", 0), ("None", 1), ("RSAES-PKCS1-v1_5", 2), ("RSAES-OAEP", 3), ("RSASSA-PKCS1-v1_5", 4)] <> [ ("Reserved", v) | v <- [5 .. 15] ]
    lifeCycleState = [("Issued", 0), ("Issued-Disabled", 1), ("Issued-Frozen", 2), ("Issued-Disabled-Frozen", 3), ("Issued-Failed", 4)]
                     <> [ ("Unassigned", v) | v <- [5 .. 7] ] <> [ ("Reserved for SSC Usage", v) | v <- [8 .. 13] ] <> [ ("Unassigned", v) | v <- [14, 15] ]
    reencryptState = [("Idle", 1), ("Pending", 2), ("Active", 3), ("Completed", 4), ("Paused", 5)] <> [ ("Reserved", v) | v <- [6 .. 16] ]
