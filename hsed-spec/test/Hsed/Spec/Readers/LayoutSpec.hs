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
pinned constants, flags, byte strings, numbers), Table 18's Session read as
the two parts its parenthesis names, the tail with its length field and
padding; their declarations, printed as the grid prints the fields, a pin at
its field's width, and type-checked with @ghc -fno-code -Wall -Werror@
against hsed-core in one @Hsed.Gen.Core.Layouts@ module (pending where ghc
cannot run); and the @C-LAYOUT-TILE@ findings of a layout that does not
tile, by the one policy of the grid rules: a gap a Warning, any other
problem an Error, each at its row.
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

import           Hsed.Core.Layout         (Field (..), FieldKind (..), Layout (..), Tail (..), TileProblem (..), byteRange,
                                           spanning)
import           Hsed.Spec.Anchor         (AnchorId (..))
import           Hsed.Spec.Check          (mkContext)
import           Hsed.Spec.Common         (ErrataSet (..), Severity (..))
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.CoreIR         (withCore)
import           Hsed.Spec.DSL            (SomeBinding (..), byteGrid)
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (CellIR (..), SpecIR (..), TableIR (..))
import           Hsed.Spec.Readers.Grid   (GridField (..), readGrid)
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
        layoutTail l `shouldBe` Just (Tail "Payload" (Just "Length") 1 Bytes)
      it "T18: the Session as the two numbers its parenthesis names, TSN bytes 0-3 and HSN 4-7 (PDF 41, 3.2.3.3.1.1)" $ do
        let (t, l) = layoutOf ir 18
        [ (fieldName f, fieldStart f, fieldWidth f, fieldKind f) | f <- take 2 (layoutFields l) ]
          `shouldBe` [("TSN", 0, 32, Unsigned), ("HSN", 32, 32, Unsigned)]
        [ (fieldName f, gfName gf) | (f, gf) <- take 2 (gridFields (readGrid [] t)) ]
          `shouldBe` [("TSN", "Session (0-3 = TSN, 4-7 = HSN)"), ("HSN", "Session (0-3 = TSN, 4-7 = HSN)")]
      it "T20: the payload padded to four bytes, as its end's modulo 4 says" $
        layoutTail (snd (layoutOf ir 20)) `shouldBe` Just (Tail "Payload" (Just "Length") 4 Bytes)
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
      let decls = [ layoutDecl (boundProvenance ctx bt) (lowerIdent (layoutName l)) l []
                  | n <- grids, let (t, l) = layoutOf ir n
                  , let bt = BoundTable (TableId n) t t (SomeBinding byteGrid) (modsOf (SomeBinding byteGrid)) ]
          ctx = mkContext Scott ir
          text = Gen.renderModule "Hsed.Gen.Core.Layouts" layoutsModule decls
      it "prints each field as the grid prints it, and the tail" $ do
        let ls = T.lines text
        ls `shouldContain` ["comPacketFormat :: Layout"]
        ls `shouldContain` ["      , spanning \"ComID\" (4, 7) (5, 0) Unsigned"]
        ls `shouldContain` ["  , layoutTail   = Just (Tail \"Payload\" (Just \"Length\") 1 Bytes)"]
        ls `shouldContain` ["      , spanning \"IEEE COMPANY ID\" (4, 3) (7, 4) Unsigned"]
        ls `shouldContain` ["      , spanning \"NAA\" (4, 7) (4, 4) (Pinned 0x5)"]
      it "prints a pin at its field's width in hex digits: 0x0001 for 16 bits, 0x1 for 4, 0x0c for 8" $ do
        let l = Layout "Pins" 3 [byteRange "Code" 0 1 (Pinned 1), spanning "Version" (2, 7) (2, 4) (Pinned 1), spanning "Low" (2, 3) (2, 0) (Pinned 12)] Nothing
            ls = (layoutDecl (boundProvenance ctx (BoundTable (TableId 42) t42 t42 (SomeBinding byteGrid) (modsOf (SomeBinding byteGrid)))) "pins" l []).declBody
            t42 = fst (layoutOf ir 42)
        filter ("(Pinned" `T.isInfixOf`) ls `shouldBe`
          [ "      [ spanning \"Code\" (0, 7) (1, 0) (Pinned 0x0001)", "      , spanning \"Version\" (2, 7) (2, 4) (Pinned 0x1)"
          , "      , spanning \"Low\" (2, 3) (2, 0) (Pinned 0xc)" ]
      it "type-checks under -Wall -Werror" $
        compiles "Hsed.Gen.Core.Layouts" text

    describe "tileFindings" $ do
      let ctx = mkContext Scott ir
          (t, l) = layoutOf ir 43
          g = readGrid [] t
          bt = BoundTable (TableId 43) t t (SomeBinding byteGrid) (modsOf (SomeBinding byteGrid))
      it "reports a gap at its row, in bytes and bits, a Warning (C-LAYOUT-TILE)" $ do
        let broken = l { layoutFields = filter ((/= "Locked") . fieldName) (layoutFields l) }
        tileFindings ctx bt g l `shouldBe` []
        [ (Cm.severity f, Cm.renderFinding f) | f <- tileFindings ctx bt g broken ]
          `shouldBe` [(Warning, "C-LAYOUT-TILE core-2.01.pdf#page=71&T43: Locking Feature Descriptor: no field covers byte 4 bit 2")]
        describeTileProblem (Overlap "A" "B" 37) `shouldBe` "\"B\" starts at byte 4 bit 2, inside \"A\""
      it "reports an overlap, a pin its field cannot hold, a padded tail without a length field: each an Error" $ do
        let twice = l { layoutFields = layoutFields l <> [spanning "Locked" (4, 2) (4, 2) Flag] }
            unfit = l { layoutFields = [ if fieldName f == "Version" then f { fieldKind = Pinned 16 } else f | f <- layoutFields l ] }
        map tileSeverity [Gap 0 7, Overlap "A" "B" 3, Unfit "V" 16, PadNoLength "T" 4] `shouldBe` [Warning, Error, Error, Error]
        map Cm.severity (tileFindings ctx bt g twice <> tileFindings ctx bt g unfit) `shouldBe` [Error, Error]
      it "finds a variable run that starts inside the fixed part (an overlap), and one past it leaves a gap" $ do
        let (t17, _) = layoutOf ir 17
            early = readGrid [] (setRowLabel "If Length > 0, 20 to n + 19" "If Length > 0, 18 to n + 17" t17)
            late = readGrid [] (setRowLabel "If Length > 0, 20 to n + 19" "If Length > 0, 21 to n + 20" t17)
        gridTiling early (gridLayout "x" early) `shouldBe` [Overlap "Length" "Payload" 144]
        gridTiling late (gridLayout "x" late) `shouldBe` [Gap 160 167]
  where
    setRowLabel old new t = t { rows = [ [ if c.text == old then c { text = new } else c | c <- r ] | r <- t.rows ] }
