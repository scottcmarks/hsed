{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Spec.Bindings.PrintSpec
Description : Test specifications for Hsed.Spec.Bindings.Print
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A small generated module is printed and then type-checked with the GHC of the
build (@ghc -fno-code -package hsed-spec@), the way @stack test
hsed-spec:bindings@ checks the committed one; a wrong modifier must fail with
a message naming both shapes and citing the PDF page. The compile is skipped
with a message when no GHC can be run from inside the suite.
-}
module Hsed.Spec.Bindings.PrintSpec (spec) where

import qualified Data.Text                as DT
import qualified RIO.ByteString.Lazy      as BL
import           RIO.Directory            (createDirectoryIfMissing)
import           RIO.FilePath             ((</>))
import qualified RIO.Text                 as T
import           RIO                      hiding ((&))
import           System.Environment       (lookupEnv)
import           System.Process.Typed     (proc, readProcess)
import           Test.Hspec

import           Hsed.Spec.Anchor         (AnchorId (..))
import           Hsed.Spec.Bindings.Print
import qualified Hsed.Spec.Common         as C
import           Hsed.Spec.DSL
import           Hsed.Spec.IR             (StructPath (..))


coreSpec :: SpecId
coreSpec = SpecId "core-2.01"

entries :: [Entry]
entries =
  [ (coreSpec, TableId 46, 100, StructPath "0/1288", SomeBinding typeRow)
  , (coreSpec, TableId 52, 102, StructPath "0/1318", SomeBinding enumValues)
  , (coreSpec, TableId 75, 107, StructPath "0/1429", SomeBinding (erratum scott ["Fractoin" ~> "Fraction"] typeRow))
  , (coreSpec, SectionId "5.1.3", 100, StructPath "0/1278", SomeBinding (rule typeRow & rule enumValues :: Binding 'Heading))
  , (coreSpec, DocId, 1, StructPath "0", SomeBinding (doc core201 & rule codes & rule fieldValue & rule fieldType & rule byteFieldValue & rule byteGrid & rule terminology))
  ]

-- | The same module with T52 bound by @cols 0 1 typeRow@: the typed vocabulary rejects it.
wrongText :: Text -> Text
wrongText = DT.replace "(anchor @'EnumValues \"core-2.01\" \"T52\" \"0/1318\" 102) enumValues"
                      "(anchor @'EnumValues \"core-2.01\" \"T52\" \"0/1318\" 102) (cols 0 1 typeRow)"

-- | Type-check a module with the GHC of the build: Nothing when none can be run here.
typeCheck :: FilePath -> IO (Maybe (ExitCode, Text))
typeCheck dir = do
  direct <- isJust <$> lookupEnv "GHC_PACKAGE_PATH"
  let args = ["-fno-code", "-package", "hsed-spec", "-i" <> dir, dir </> "Hsed" </> "Bindings" </> "Core.hs"]
      cmd | direct    = proc "ghc" args
          | otherwise = proc "stack" (["exec", "--", "ghc"] <> args)
  result <- try (timeout (300 * 1000000) (readProcess cmd))
  pure $ case result of
    Left (_ :: SomeException)   -> Nothing
    Right Nothing               -> Nothing
    Right (Just (code, out, err)) -> Just (code, decodeUtf8Lenient (BL.toStrict out) <> decodeUtf8Lenient (BL.toStrict err))

withModule :: Text -> (FilePath -> IO a) -> IO a
withModule text k = withSystemTempDirectory "hsed-bindings" $ \dir -> do
  let moduleDir = dir </> "Hsed" </> "Bindings"
  createDirectoryIfMissing True moduleDir
  writeFileUtf8 (moduleDir </> "Core.hs") text
  k dir

spec :: Spec
spec = do
  let printed = printBindingsModule coreSpec entries
  describe "printBindingsModule" $ do
    it "names the module and the bindings after the spec and the anchors" $ do
      moduleNameFor coreSpec `shouldBe` "Hsed.Bindings.Core"
      moduleNameFor (SpecId "opal-2.30") `shouldBe` "Hsed.Bindings.Opal"
      map (bindingIdent coreSpec) [TableId 46, SectionId "5.1.3", DocId, ItemId "3.3.10.2.1" "3a", SigId "5.3.3.6"]
        `shouldBe` ["b_core_T46", "b_core_5_1_3", "b_core_doc", "b_core_3_3_10_2_1_3a", "b_core_5_3_3_6_sig"]

    it "prints one LINE-pragma'd binding per anchor and the bindings list" $ do
      let ls = either (const []) T.lines printed
      ls `shouldSatisfy` elem "module Hsed.Bindings.Core (bindings) where"
      ls `shouldSatisfy` elem "{-# LINE 1 \"core-2.01.pdf#page=107&T75\" #-}"
      ls `shouldSatisfy` elem "b_core_T75 :: Bound"
      ls `shouldSatisfy` elem "b_core_T75 = at (anchor @'TypeRow \"core-2.01\" \"T75\" \"0/1429\" 107) (erratum scott [\"Fractoin\" ~> \"Fraction\"] typeRow)"
      ls `shouldSatisfy` elem "b_core_5_1_3 = at (anchor @'Heading \"core-2.01\" \"5.1.3\" \"0/1278\" 100) (rule typeRow & rule enumValues)"
      ls `shouldSatisfy` elem "b_core_doc = at (anchor @'Doc \"core-2.01\" \"doc\" \"0\" 1) (doc core201 & rule codes & rule fieldValue & rule fieldType & rule byteFieldValue & rule byteGrid & rule terminology)"
      ls `shouldSatisfy` elem "bindings :: [Bound]"
      ls `shouldSatisfy` elem "  [ b_core_T46"
      ls `shouldSatisfy` elem "  , b_core_doc"

    it "refuses a binding whose shape is unresolved, citing the anchor" $ do
      let unresolved = printBindingsModule coreSpec [(coreSpec, TableId 3, 20, StructPath "0/195", PolyBinding (refines (core 42)))]
      fmap C.renderFinding (either Just (const Nothing) unresolved)
        `shouldBe` Just "E-SHAPE core-2.01.pdf#page=20&T3: the shape of refines (core 42) comes from its target; resolve it before printing"

  describe "the generated module" $ do
    it "type-checks against Hsed.Spec.DSL" $ do
      text <- either (throwString . T.unpack . C.renderFinding) pure printed
      withModule text $ \dir -> typeCheck dir >>= \case
        Nothing -> pendingWith "no GHC with the hsed-spec package reachable from the test; run tools by hand (see the module documentation)"
        Just (code, out) -> (code, out) `shouldSatisfy` \(c, _) -> c == ExitSuccess

    it "fails to type-check cols 0 1 typeRow, naming both shapes and the page" $ do
      text <- either (throwString . T.unpack . C.renderFinding) pure printed
      let wrong = wrongText text
      wrong `shouldNotBe` text
      withModule wrong $ \dir -> typeCheck dir >>= \case
        Nothing -> pendingWith "no GHC with the hsed-spec package reachable from the test"
        Just (code, out) -> do
          code `shouldNotBe` ExitSuccess
          out `shouldSatisfy` T.isInfixOf "'EnumValues"
          out `shouldSatisfy` T.isInfixOf "'TypeRow"
          out `shouldSatisfy` T.isInfixOf "core-2.01.pdf#page=102&T52"
