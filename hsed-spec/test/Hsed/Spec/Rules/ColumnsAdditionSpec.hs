{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Spec.Rules.ColumnsAdditionSpec
Description : Test specifications for the columnsAddition rule, and for Opal's type layer as generated
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The columnsAddition rule (design example E5): the target a caption names;
the rule's exemplar, Opal Table 26, on which it still fires; and, on the
committed Opal extraction under the M4b sidecar read over the committed Core
run, the three additions it reads (T26, T43, T50) and every description
Opal reads with them applied: its own (T32, DataRemovalMechanism, 2
columns), Core's three it extends (TPerInfo 9 columns, 0x08
ProgrammaticResetEnable a boolean, PDF 49; Table 15, 0x0D and 0x0E
uinteger_4, PDF 98-99; LockingInfo 11, PDF 85) and Core's other 42 as Core
reads them. Its checks: none fails on the three, the numbers running on from
the descriptions they extend, and T43's @uniteger_8@ (PDF 85) is the one
C-FMT-RESOLVE Warning; C-COL-CONTIG, C-COL-CELL and C-COLS-TARGET each made
to fail by one edited cell or caption, the addition then left out. Its
declarations in @Hsed.Gen.Opal.TableDescs@: the three extended descriptions,
Core's columns as Core declares them and then the added ones, each citing its
addition and the description it extends. Then Opal's type layer as one
compiles it: @Hsed.Gen.Opal.{TableDescs,Types,Enums,TableUIDs}@ leave none
of T26, T30-T32, T43, T49 and T50 out, import each name from the module of
the spec that defines it, and type-check under @-Wall -Werror@ against the
committed Core modules (@-i../hsed/gen@).
-}
module Hsed.Spec.Rules.ColumnsAdditionSpec (spec) where

import qualified Data.Text                     as DT
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           RIO
import           RIO.Directory                 (createDirectoryIfMissing)
import           RIO.FilePath                  (takeDirectory, (</>))
import           System.Environment            (lookupEnv)
import           System.Process                (readProcessWithExitCode)
import           Test.Hspec

import           Hsed.Spec.Anchor              (AnchorId (..))
import           Hsed.Spec.Check               (checkIR)
import           Hsed.Spec.Common              (ErrataSet (..), Finding, SpecId (..), isError, renderFinding)
import qualified Hsed.Spec.Common              as Cm
import           Hsed.Spec.Compile             (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR                  (SpecIR (..), TableIR (..))
import           Hsed.Spec.RuleDef             (BoundTable (..), Context (..), RuleDef (..))
import           Hsed.Spec.Rules               (checkExemplarsWith)
import           Hsed.Spec.Rules.ColumnsAddition
import           Hsed.Spec.Rules.TableDesc     (ColRead (..), DescRead (..), descReads, tableDescRule)
import           Hsed.Spec.Rules.TypeRow       (TypeRowRead (..))


-- | A description's columns: number, name and type, as printed.
columnsOf :: DescRead -> [(Text, Text, Text)]
columnsOf d = [ (crNumberText c, crName c, crType c) | c <- drColumns d ]

-- | The description of a table among a spec's, with the spec that owns it.
descOf :: Text -> [(Context, DescRead)] -> Maybe (SpecId, DescRead)
descOf n ds = listToMaybe [ (cxSpec c, d) | (c, d) <- ds, drTable d == Just n ]

opalId :: SpecId
opalId = SpecId "opal-2.30"

-- | The findings with a code, rendered.
coded :: Text -> [Finding] -> [Text]
coded c = map renderFinding . filter ((== c) . Cm.code)

-- | The IR with a table's caption replaced.
withCaption :: Text -> Text -> SpecIR -> SpecIR
withCaption tid caption ir = ir { tables = map (\t -> if t.id == Just tid then t { caption = Just caption } else t) ir.tables }

-- | A declaration's lines in a module's text: from its signature to the blank line after it.
block :: Text -> [Text] -> [Text]
block sig = takeWhile (/= "") . dropWhile (/= sig)

-- | The comment lines over a declaration: those before its signature, back to the blank line.
comments :: Text -> [Text] -> [Text]
comments sig = reverse . takeWhile (/= "") . drop 1 . dropWhile (/= sig) . reverse

-- | The column lines of a declaration.
colLines :: [Text] -> [Text]
colLines = filter ("Col 0x" `T.isInfixOf`)

-- | @ghc -fno-code -Wall -Werror@ on printed modules, against hsed-core and
--   the committed generated modules they import (@-i../hsed/gen@: Core's), as
--   PropsReqSpec's @compiles@ does for one module.
compileAll :: [(Text, Text)] -> Expectation
compileAll mods = withSystemTempDirectory "hsed-opal-types" $ \dir -> do
  files <- forM mods $ \(modName, text) -> do
    let file = dir </> T.unpack (DT.replace "." "/" modName) <> ".hs"
    createDirectoryIfMissing True (takeDirectory file)
    writeFileUtf8 file text
    pure file
  let ghcArgs = ["-fno-code", "-Wall", "-Werror", "-package", "hsed-core", "-package", "containers", "-i" <> dir, "-i../hsed/gen"] <> files
  underStack <- isJust <$> lookupEnv "GHC_PACKAGE_PATH"
  let (prog, args) = if underStack then ("ghc", ghcArgs) else ("stack", ["exec", "--", "ghc"] <> ghcArgs)
  result <- try (readProcessWithExitCode prog args "")
  case result of
    Left (e :: IOException)         -> pendingWith ("cannot run " <> prog <> " here: " <> show e)
    Right (ExitSuccess, _, _)       -> pure ()
    Right (ExitFailure _, out, err) -> expectationFailure (prog <> " rejected the modules:\n" <> out <> err)

spec :: Spec
spec = do
  describe "additionTarget" $
    it "reads the table a caption names before Columns: an SP and its dash dropped, then Additional, then Table" $ do
      map additionTarget ["Admin SP \x2013 TPerInfo", "Table Table Additional", "Locking SP \x2013 LockingInfo", "Admin SP - TPerInfo"]
        `shouldBe` [Just "TPerInfo", Just "Table", Just "LockingInfo", Just "TPerInfo"]
      map additionTarget ["Admin SP \x2013", "", "Two Words"] `shouldBe` [Nothing, Nothing, Nothing]

  describe "the rule" $ do
    it "refines tableDesc, whose shape is the same, and claims C-COLS-TARGET" $ do
      refinesRule columnsAdditionRule `shouldBe` Just (ruleName tableDescRule)
      map Cm.codeName (ruleCodes columnsAdditionRule) `shouldBe` ["C-COLS-TARGET"]
      exemplar columnsAdditionRule `shouldBe` (opalId, TableId 26)
    withOpal $ \opalPristine ->
      it "still fires on its exemplar, Opal Table 26, and its shape accepts it (no C-EXEMPLAR)" $
        checkExemplarsWith [columnsAdditionRule] opalPristine `shouldBe` []

  withM4bOpal $ \base opal -> do
    let octx = opalOver base opal
        ds   = allDescs octx
        fs   = checkIR (Just base) Scott opal
        errorsAt a ir' = [ renderFinding f | f <- checkIR (Just base) Scott ir', isError f, Cm.anchor f == Just a ]
        compiled ir' = either (throwString . show) pure (compileIR (Just base) Scott ir')
        excludedOf a ir' = do
          cr <- compiled ir'
          pure [ why | (aid, why) <- crExcluded cr, aid == a ]
    describe "additions, on Opal under the M4b sidecar" $
      it "reads Tables 26, 43 and 50, each the table its caption names and its columns" $
        [ (btAnchor (adBound a), adTarget a, map crNumberText (adColumns a)) | a <- additions octx ]
          `shouldBe` [ (TableId 26, Just "TPerInfo", ["0x08"]), (TableId 43, Just "LockingInfo", ["0x07", "0x08", "0x09", "0x0A"])
                     , (TableId 50, Just "Table", ["0x0D", "0x0E"]) ]

    describe "allDescs, on Opal under the M4b sidecar, read over Core" $ do
      it "gives TPerInfo Core T211's eight columns and T26's 0x08 ProgrammaticResetEnable, a boolean (PDF 49), as Opal's" $ do
        fmap fst (descOf "TPerInfo" ds) `shouldBe` Just opalId
        fmap (length . columnsOf . snd) (descOf "TPerInfo" ds) `shouldBe` Just 9
        fmap (drop 8 . columnsOf . snd) (descOf "TPerInfo" ds) `shouldBe` Just [("0x08", "ProgrammaticResetEnable", "boolean")]
        fmap (btAnchor . drBound . snd) (descOf "TPerInfo" ds) `shouldBe` Just (TableId 211)
      it "gives Table 15 columns, 0x0D MandatoryWriteGranularity and 0x0E RecommendedAccessGranularity uinteger_4 (PDF 98-99)" $ do
        fmap (length . columnsOf . snd) (descOf "Table" ds) `shouldBe` Just 15
        fmap (drop 13 . columnsOf . snd) (descOf "Table" ds)
          `shouldBe` Just [("0x0D", "MandatoryWriteGranularity", "uinteger_4"), ("0x0E", "RecommendedAccessGranularity", "uinteger_4")]
      it "gives LockingInfo 11 columns, 0x07-0x0A from T43 (PDF 85)" $ do
        fmap (length . columnsOf . snd) (descOf "LockingInfo" ds) `shouldBe` Just 11
        fmap (map (\(n, _, _) -> n) . drop 7 . columnsOf . snd) (descOf "LockingInfo" ds) `shouldBe` Just ["0x07", "0x08", "0x09", "0x0A"]
      it "gives DataRemovalMechanism Opal's own two columns (T32, its caption read through the typo table)" $ do
        fmap fst (descOf "DataRemovalMechanism" ds) `shouldBe` Just opalId
        fmap (columnsOf . snd) (descOf "DataRemovalMechanism" ds)
          `shouldBe` Just [("0x00", "UID", "uid"), ("0x01", "ActiveDataRemovalMechanism", "data_removal_mechanism")]
      it "reads each column's type over the base: Core's boolean for T26's addition, Opal's own T30 for T32's" $ do
        fmap (map (fmap trAnchor . crTypeRow) . drColumns . snd) (descOf "DataRemovalMechanism" ds) `shouldBe` Just [Just (TableId 145), Just (TableId 30)]
        fmap (map (fmap trName . crTypeRow) . drop 8 . drColumns . snd) (descOf "TPerInfo" ds) `shouldBe` Just [Just "boolean"]
      it "gives every other description of Core's as Core reads it, one per table, Opal's first" $ do
        length ds `shouldBe` 1 + length (descReads base)
        L.nub (mapMaybe (drTable . snd) ds) `shouldBe` mapMaybe (drTable . snd) ds
        map (cxSpec . fst) (take 4 ds) `shouldBe` replicate 4 opalId
        [ cxSpec c | (c, _) <- drop 4 ds ] `shouldSatisfy` all (== SpecId "core-2.01")

    describe "the checks, on Opal under the M4b sidecar, read over Core" $ do
      it "find no Error at Tables 26, 43 and 50: the numbers run on from TPerInfo's 0x07, LockingInfo's 0x06 and Table's 0x0C" $ do
        [ renderFinding f | f <- fs, isError f, Cm.anchor f `elem` map Just ["T26", "T43", "T50"] ] `shouldBe` []
        coded "C-COL-CONTIG" fs `shouldBe` []
        coded "C-COL-CELL" fs `shouldBe` []
        coded "C-COLS-TARGET" fs `shouldBe` []
      it "give exactly one C-FMT-RESOLVE, a Warning: T43's uniteger_8 (PDF 85), the nearest type Core's uinteger_8" $ do
        coded "C-FMT-RESOLVE" fs `shouldBe`
          [ "C-FMT-RESOLVE opal-2.30.pdf#page=85&T43: column 0x0A LowestAlignedLBA: the column type uniteger_8 names no Type object"
            <> " (no type row binds it); nearest: uinteger_8, core-2.01 T161; generated with its name and no UID" ]
        [ Cm.severity f | f <- fs, Cm.code f == "C-FMT-RESOLVE" ] `shouldBe` [Cm.Warning]
      it "C-COL-CONTIG: a number that does not run on from the description's, and the addition left out" $ do
        errorsAt "T26" (setCell "T26" 0 0 "0x09" opal) `shouldBe`
          [ "C-COL-CONTIG opal-2.30.pdf#page=49&T26: column 0x09 ProgrammaticResetEnable: the column numbers run 0x00 to 0x08 in row order,"
            <> " TPerInfo's 0x00 to 0x07 first (core-2.01 T211), and row 1 is 0x09 where 0x08 is expected" ]
        errorsAt "T50" (setCell "T50" 1 0 "0x0F" opal) `shouldBe`
          [ "C-COL-CONTIG opal-2.30.pdf#page=99&T50: column 0x0F RecommendedAccessGranularity: the column numbers run 0x00 to 0x0E in row order,"
            <> " Table's 0x00 to 0x0C first (core-2.01 T171), and row 2 is 0x0F where 0x0E is expected" ]
        excludedOf (TableId 26) (setCell "T26" 0 0 "0x09" opal) `shouldReturn` ["C-COL-CONTIG, C-COL-CELL or C-COLS-TARGET failed"]
      it "C-COL-CELL: a name a column of the description gives, and an IsUnique that is not Yes or blank" $ do
        errorsAt "T26" (setCell "T26" 0 1 "UID" opal) `shouldBe`
          [ "C-COL-CELL opal-2.30.pdf#page=49&T26: column 0x08 UID: the column name is also that of TPerInfo's column 0x00 (core-2.01 T211)" ]
        errorsAt "T43" (setCell "T43" 1 2 "No" opal) `shouldBe`
          [ "C-COL-CELL opal-2.30.pdf#page=85&T43: column 0x08 LogicalBlockSize: IsUnique is \"No\", not Yes or blank" ]
        excludedOf (TableId 43) (setCell "T43" 1 2 "No" opal) `shouldReturn` ["C-COL-CONTIG, C-COL-CELL or C-COLS-TARGET failed"]
      it "C-COLS-TARGET: a caption naming a table no description of Opal or Core describes, the addition left out" $ do
        let renamed = withCaption "T26" "Table 26 - Admin SP \x2013 TPerInfos Columns" opal
        errorsAt "T26" renamed `shouldBe`
          [ "C-COLS-TARGET opal-2.30.pdf#page=49&T26: the caption names TPerInfos, which no table description of opal-2.30 or core-2.01 describes" ]
        excludedOf (TableId 26) renamed `shouldReturn` ["C-COL-CONTIG, C-COL-CELL or C-COLS-TARGET failed"]
      it "C-COLS-TARGET on Opal read alone: no description of Opal's names TPerInfo, LockingInfo or Table" $
        [ renderFinding f | f <- checkIR Nothing Scott opal, Cm.code f == "C-COLS-TARGET" ] `shouldBe`
          [ "C-COLS-TARGET opal-2.30.pdf#page=" <> p <> "&T" <> n <> ": the caption names " <> t <> ", which no table description of opal-2.30 describes"
          | (p, n, t) <- [("49", "26", "TPerInfo"), ("85", "43", "LockingInfo"), ("98", "50", "Table")] ]

    describe "the declarations, in Hsed.Gen.Opal.TableDescs" $ do
      cr <- runIO (compiled opal)
      coreDescs <- runIO (T.lines <$> readFileUtf8 "../hsed/gen/Hsed/Gen/Core/TableDescs.hs")
      let modText n = fromMaybe "" (lookup n (crModules cr))
          lns = T.lines (modText "Hsed.Gen.Opal.TableDescs")
          sig n = n <> " :: TableDesc"
      it "extend TPerInfo: Core's eight columns as Core declares them, then T26's 0x08 ProgrammaticResetEnable, a boolean" $ do
        let opalBlock = block (sig "tPerInfoTableDesc") lns
        length (colLines opalBlock) `shouldBe` 9
        take 8 (colLines opalBlock) `shouldBe` colLines (block (sig "tPerInfoTableDesc") coreDescs)
        drop 8 (colLines opalBlock) `shouldBe` ["      , Col 0x08 \"ProgrammaticResetEnable\" False (TypeName \"boolean\" (Just ubooleanType))"]
        take 6 opalBlock `shouldBe`
          [ "tPerInfoTableDesc :: TableDesc", "tPerInfoTableDesc = TableDesc", "  { tdName      = \"TPerInfo\""
          , "  , tdTableUID  = uTPerInfoTable", "  , tdObjectUID = uTPerInfoTableObject", "  , tdColumns   =" ]
      it "cite the addition, then the description it extends" $ do
        map (T.takeWhile (/= '(')) (comments (sig "tPerInfoTableDesc") lns) `shouldBe`
          [ "-- | opal-2.30 T26 - Admin SP - TPerInfo Columns ", "--   extends core-2.01 T211 TPerInfo Table Description " ]
        comments (sig "tPerInfoTableDesc") lns `shouldSatisfy` any ("(PDF 229 / p.212, ord 213, sha256:306f567a406a)" `T.isSuffixOf`)
        comments (sig "tPerInfoTableDesc") lns `shouldSatisfy` any ("-- | opal-2.30 T26 - Admin SP - TPerInfo Columns (PDF 49 / p.48, ord 28, sha256:" `T.isPrefixOf`)
      it "extend Table with T50's two uinteger_4 columns (PDF 98-99), 15 in all" $ do
        let opalBlock = block (sig "tableTableDesc") lns
        length (colLines opalBlock) `shouldBe` 15
        take 13 (colLines opalBlock) `shouldBe` colLines (block (sig "tableTableDesc") coreDescs)
        drop 13 (colLines opalBlock) `shouldBe`
          [ "      , Col 0x0D \"MandatoryWriteGranularity\" False (TypeName \"uinteger_4\" (Just uuinteger_4Type))"
          , "      , Col 0x0E \"RecommendedAccessGranularity\" False (TypeName \"uinteger_4\" (Just uuinteger_4Type))" ]
      it "extend LockingInfo with T43's four (PDF 85), 11 in all, LowestAlignedLBA with its type's name and no UID" $ do
        let opalBlock = block (sig "lockingInfoTableDesc") lns
        length (colLines opalBlock) `shouldBe` 11
        take 7 (colLines opalBlock) `shouldBe` colLines (block (sig "lockingInfoTableDesc") coreDescs)
        drop 7 (colLines opalBlock) `shouldBe`
          [ "      , Col 0x07 \"AlignmentRequired\" False (TypeName \"boolean\" (Just ubooleanType))"
          , "      , Col 0x08 \"LogicalBlockSize\" False (TypeName \"uinteger_4\" (Just uuinteger_4Type))"
          , "      , Col 0x09 \"AlignmentGranularity\" False (TypeName \"uinteger_8\" (Just uuinteger_8Type))"
          , "      , Col 0x0A \"LowestAlignedLBA\" False (TypeName \"uniteger_8\" Nothing)" ]
      it "declare the three extended descriptions and Opal's own DataRemovalMechanism, in table order, none other" $
        [ l | l <- lns, " :: TableDesc" `T.isSuffixOf` l ]
          `shouldBe` map sig ["tPerInfoTableDesc", "dataRemovalMechanismTableDesc", "lockingInfoTableDesc", "tableTableDesc"]
      it "import Core's names from Core's modules by name, and Opal's whole from Opal's" $ do
        lns `shouldSatisfy` any ("import           Hsed.Gen.Core.TableUIDs (uLockingInfoTable, uLockingInfoTableObject, uTableTable, uTableTableObject, uTPerInfoTable, uTPerInfoTableObject)" `T.isPrefixOf`)
        lns `shouldSatisfy` any (\l -> "import           Hsed.Gen.Core.Types " `T.isPrefixOf` l && "ubooleanType" `T.isInfixOf` l && "uuidType" `T.isInfixOf` l)
        lns `shouldContain` ["import           Hsed.Gen.Opal.TableUIDs", "import           Hsed.Gen.Opal.Types"]

    describe "Opal's type layer, compiled over Core" $ do
      cr <- runIO (compiled opal)
      let typeLayer = ["Hsed.Gen.Opal.Enums", "Hsed.Gen.Opal.TableDescs", "Hsed.Gen.Opal.TableUIDs", "Hsed.Gen.Opal.Types"]
      it "writes Hsed.Gen.Opal.{Enums,TableDescs,TableUIDs,Types}" $
        filter (`elem` typeLayer) (map fst (crModules cr)) `shouldBe` typeLayer
      it "leaves none of Tables 26, 30-32, 43, 49 and 50 out" $
        [ x | x@(aid, _) <- crExcluded cr, aid `elem` map TableId [26, 30, 31, 32, 43, 49, 50] ] `shouldBe` []
      it "type-checks under -Wall -Werror against hsed-core and Core's committed modules (-i../hsed/gen)" $
        compileAll [ (n, t) | (n, t) <- crModules cr, n `elem` typeLayer ]
