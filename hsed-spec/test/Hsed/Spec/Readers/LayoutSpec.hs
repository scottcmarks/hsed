{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Spec.Readers.LayoutSpec
Description : Test specifications for Hsed.Spec.Readers.Layout
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The layouts made from the Core grids ('gridLayout'): field kinds (reserved,
pinned constants, flags, byte strings, numbers), the tail with its length
field and padding; their declarations, printed as the grid prints the fields
and type-checked with @ghc -fno-code -Wall -Werror@ against hsed-core in one
@Hsed.Gen.Core.Layouts@ module (pending where ghc cannot run); and the
@C-LAYOUT-TILE@ findings of a layout that does not tile.
-}
module Hsed.Spec.Readers.LayoutSpec (spec) where

import qualified Data.Text                as DT
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T
import           RIO
import           RIO.Directory            (createDirectoryIfMissing)
import           RIO.FilePath             (takeDirectory, (</>))
import           System.Environment       (lookupEnv)
import           System.Process           (readProcessWithExitCode)
import           Test.Hspec

import           Hsed.Core.Layout         (Field (..), FieldKind (..), Layout (..), Tail (..), TileProblem (..))
import           Hsed.Spec.Anchor         (AnchorId (..))
import           Hsed.Spec.Check          (mkContext)
import           Hsed.Spec.Common         (ErrataSet (..))
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.CoreIR         (withCore)
import           Hsed.Spec.DSL            (SomeBinding (..), byteGrid)
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (SpecIR (..), TableIR (..))
import           Hsed.Spec.Readers.Grid   (readGrid)
import           Hsed.Spec.Readers.Layout
import           Hsed.Spec.Readers.Text   (lowerIdent)
import           Hsed.Spec.RuleDef        (BoundTable (..), boundProvenance, modsOf, numberedTables)
import           Hsed.Spec.Shape          (captionTitle)


-- | The grids of the Core tables named, as layouts named by their captions.
layoutOf :: SpecIR -> Int -> (TableIR, Layout)
layoutOf ir n = (t, gridLayout (fromMaybe "" (t.caption >>= captionTitle)) (readGrid [] t))
  where t = fromMaybe (error ("no table " <> show n)) (Map.lookup (TableId n) (numberedTables ir))

-- | @ghc -fno-code -Wall -Werror@ on a printed module against hsed-core (as GenSpec does).
compiles :: Text -> Text -> Expectation
compiles modName text = withSystemTempDirectory "hsed-layouts" $ \dir -> do
  let file = dir </> T.unpack (DT.replace "." "/" modName) <> ".hs"
      ghcArgs = ["-fno-code", "-Wall", "-Werror", "-package", "hsed-core", "-i" <> dir, file]
  createDirectoryIfMissing True (takeDirectory file)
  writeFileUtf8 file text
  underStack <- isJust <$> lookupEnv "GHC_PACKAGE_PATH"
  let (prog, args) = if underStack then ("ghc", ghcArgs) else ("stack", ["exec", "--", "ghc"] <> ghcArgs)
  result <- try (readProcessWithExitCode prog args "")
  case result of
    Left (e :: IOException)         -> pendingWith ("cannot run " <> prog <> " here: " <> show e)
    Right (ExitSuccess, _, _)       -> pure ()
    Right (ExitFailure _, out, err) -> expectationFailure (prog <> " rejected the module:\n" <> out <> err)

spec :: Spec
spec = describe "Hsed.Spec.Readers.Layout" $ do
  it "names the generated module both byteGrid and byteFieldValue write" $
    (layoutsModule.leaf, Gen.moduleName (Cm.SpecId "core-2.01") layoutsModule) `shouldBe` ("Layouts", "Hsed.Gen.Core.Layouts")

  withCore $ \ir -> do
    let grids = [17, 18, 20, 21, 38, 39, 41, 42, 43, 44, 212]
    describe "gridLayout" $ do
      it "T17: reserved bytes, numbers, and the payload whose length Length (n) gives" $ do
        let (_, l) = layoutOf ir 17
        (layoutSize l, map (\f -> (fieldName f, fieldKind f)) (layoutFields l)) `shouldBe`
          (20, [ ("Reserved", Reserved), ("ComID", Unsigned), ("ComID Extension", Unsigned), ("OutstandingData", Unsigned)
               , ("MinTransfer", Unsigned), ("Length", Unsigned) ])
        layoutTail l `shouldBe` Just (Tail "Payload" (Just "Length") 1)
      it "T20: the payload padded to four bytes, as its end's modulo 4 says" $
        layoutTail (snd (layoutOf ir 20)) `shouldBe` Just (Tail "Payload" (Just "Length") 4)
      it "T42: one-bit flags, reserved bits, the 4-bit Version" $ do
        let (_, l) = layoutOf ir 42
        [ (fieldName f, fieldWidth f, fieldKind f) | f <- layoutFields l, fieldName f `elem` ["Version", "Sync Supported", "Feature Code"] ]
          `shouldBe` [("Feature Code", 16, Unsigned), ("Version", 4, Unsigned), ("Sync Supported", 1, Flag)]
      it "T39: Vendor Unique, 32 bytes, a byte string" $
        [ fieldKind f | f <- layoutFields (snd (layoutOf ir 39)), fieldName f == "Vendor Unique" ] `shouldBe` [Bytes]
      it "T212: the constants it prints pinned" $
        [ (fieldName f, fieldKind f) | f <- take 5 (layoutFields (snd (layoutOf ir 212))) ]
          `shouldBe` [("0x02", Pinned 2), ("0x23", Pinned 0x23), ("0x00", Pinned 0), ("0x08", Pinned 8), ("NAA", Pinned 5)]

    describe "layoutDecl" $ do
      let decls = [ layoutDecl (boundProvenance ctx bt) (lowerIdent (layoutName l)) l
                  | n <- grids, let (t, l) = layoutOf ir n
                  , let bt = BoundTable (TableId n) t t (SomeBinding byteGrid) (modsOf (SomeBinding byteGrid)) ]
          ctx = mkContext Scott ir
          text = Gen.renderModule "Hsed.Gen.Core.Layouts" layoutsModule decls
      it "prints each field as the grid prints it, and the tail" $ do
        let ls = T.lines text
        ls `shouldContain` ["comPacketFormat :: Layout"]
        ls `shouldContain` ["      , spanning \"ComID\" (4, 7) (5, 0) Unsigned"]
        ls `shouldContain` ["  , layoutTail   = Just (Tail \"Payload\" (Just \"Length\") 1)"]
        ls `shouldContain` ["      , spanning \"IEEE COMPANY ID\" (4, 3) (7, 4) Unsigned"]
        ls `shouldContain` ["      , spanning \"NAA\" (4, 7) (4, 4) (Pinned 0x05)"]
      it "type-checks under -Wall -Werror" $
        compiles "Hsed.Gen.Core.Layouts" text

    describe "tileFindings" $
      it "reports each gap and overlap at the table, in bytes and bits (C-LAYOUT-TILE)" $ do
        let ctx = mkContext Scott ir
            (t, l) = layoutOf ir 43
            bt = BoundTable (TableId 43) t t (SomeBinding byteGrid) (modsOf (SomeBinding byteGrid))
            broken = l { layoutFields = filter ((/= "Locked") . fieldName) (layoutFields l) }
        tileFindings ctx bt l `shouldBe` []
        map Cm.renderFinding (tileFindings ctx bt broken)
          `shouldBe` ["C-LAYOUT-TILE core-2.01.pdf#page=71&T43: Locking Feature Descriptor: no field covers byte 4 bit 2"]
        describeTileProblem (Overlap "A" "B" 37) `shouldBe` "\"B\" starts at byte 4 bit 2, inside \"A\""
