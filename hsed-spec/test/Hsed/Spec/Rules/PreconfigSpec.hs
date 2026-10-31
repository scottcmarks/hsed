{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-|
Module      : Hsed.Spec.Rules.PreconfigSpec
Description : Test specifications for the preconfig rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The target a preconfiguration caption names, Core's and Opal's; then the rule
(design section 3.4, example E12) on the committed Core extraction with the
committed annotations: Tables 209 and 216, the default authorities of the
Base and Admin templates, bound from the doc anchor's rule token, both rows of
the Authority table (T178), their 17 rows read as the description types their
columns; C-PRECONF-HEADER, C-UID-TABLE, C-UID-UNIQUE and C-PRECONF-CELL made to
fail by one edited cell; Table 243, which the uidTable rule reads, gives the
17 authorities the two tables preconfigure (C-PRECONF-UID); an informative
column dropped, a group row read as context, a template row read as one and
left out of Core's rows; the generated rows, Core's text as it was. Opal's
Table 24, the design's exemplar, fires too.

Then, since M4b, Opal's Admin SP (section 4.2, PDF 35-52) on the committed
Opal extraction under the M4b sidecar, read over the committed Core run: the
twelve tables read against Core's descriptions, Opal's and Core's with
Opal's columns, with no Error and none left out; Table 22's 34 rows (22
plain, 9 for every object, 3 templates) and their references, C-ACL-REFS
and C-ACL-EXPR made to fail by edited cells; Table 23's expressions and
columns; Tables 29, 27, 20 and 24 as the plan lists them; the eight cells
the TPer chooses; the templates; the notes, warnings and the label read
otherwise; the generated module, its twelve PreconfTables, the adminSP
index, the Opal-only UIDs (equal to the hand transcription's), C-UID-NAME,
and the module type-checked under -Wall -Werror against a stand-in Opal
TableDescs (the lead's answer 16); and the legend's classes by column with a
hand-built scale (G1's classifier).
-}
module Hsed.Spec.Rules.PreconfigSpec (spec) where

import qualified Data.Text                 as DT
import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO
import           RIO.Directory             (createDirectoryIfMissing, doesFileExist)
import           RIO.FilePath              (takeDirectory, (</>))
import           System.Environment        (lookupEnv)
import           System.Process            (readProcessWithExitCode)
import           Test.Hspec

import           Hsed.Core.Preconfig       (Placeholder (..))
import           Hsed.Core.Table           (Access (..))
import           Hsed.Spec.Anchor          (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Check           (checkIR, mkContext)
import           Hsed.Spec.Common          (ErrataSet (..), RuleName (..), SpecId (..), isError, renderFinding)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.Compile         (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import qualified Hsed.Spec.Gen             as Gen
import           Hsed.Spec.IR              (CellIR (..), SpecIR (..), TableIR (..), readIR)
import           Hsed.Spec.Readers.Cell    (CellValue (..), ColKind (..), Remark (..), TypeEnv (..))
import           Hsed.Spec.RuleDef         (BoundTable (..), Classification (..), RuleDef (..))
import           Hsed.Spec.Rules           (classify, explain)
import           Hsed.Spec.Rules.Legend    (LegendClass (..), LegendScale (..))
import           Hsed.Spec.Rules.Preconfig
import           Hsed.Spec.Rules.TableDesc (ColRead (..), DescRead (..), typeEnv)
import           Hsed.Spec.Rules.UidTable  (UidRow (..), UidValue (..), uidRows)
import           Hsed.Spec.Shape           (measure)


-- | The Errors of a run, but C-PRECONF-UID, which an edited UID or row also
--   fails where Table 243 lists the authorities (tested on its own below).
errorsOf :: SpecIR -> [Text]
errorsOf = map renderFinding . filter (\f -> isError f && Cm.code f /= "C-PRECONF-UID") . checkIR Nothing Scott

-- | The generated module of an IR.
generated :: SpecIR -> IO [Text]
generated ir = do
  cr <- either (throwString . show) pure (compileIR Nothing Scott ir)
  pure (T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Preconfig" (crModules cr))))

-- | The lines of one declaration.
block :: Text -> [Text] -> [Text]
block n = takeWhile (/= "") . dropWhile (/= (n <> " :: [Pre Row]"))

-- | @ghc -fno-code@ under hsed's warnings and -Werror on a printed module
--   against hsed-core and Core's generated modules (PropsReqSpec's helper,
--   with @-i../hsed/gen@ and containers), beside the modules given.
compiles :: [(Text, Text)] -> Text -> Text -> Expectation
compiles beside modName text = withSystemTempDirectory "hsed-preconfig" $ \dir -> do
  let path m = dir </> T.unpack (DT.replace "." "/" m) <> ".hs"
      file = path modName
      ghcArgs = [ "-fno-code", "-Wall", "-Wcompat", "-Wincomplete-record-updates", "-Wincomplete-uni-patterns"
                , "-Wredundant-constraints", "-Werror", "-package", "hsed-core", "-package", "containers"
                , "-i" <> dir, "-i../hsed/gen", file ]
  forM_ ((modName, text) : beside) $ \(m, t) -> do
    createDirectoryIfMissing True (takeDirectory (path m))
    writeFileUtf8 (path m) t
  underStack <- isJust <$> lookupEnv "GHC_PACKAGE_PATH"
  let (prog, args) = if underStack then ("ghc", ghcArgs) else ("stack", ["exec", "--", "ghc"] <> ghcArgs)
  result <- try (readProcessWithExitCode prog args "")
  case result of
    Left (e :: IOException)         -> pendingWith ("cannot run " <> prog <> " here: " <> show e)
    Right (ExitSuccess, _, _)       -> pure ()
    Right (ExitFailure _, out, err) -> expectationFailure (prog <> " rejected the module:\n" <> out <> err)

-- | A stand-in for G3's @Hsed.Gen.Opal.TableDescs@, which M4b's integration
--   generates (the lead's answer 16): the three descriptions Opal owns, of
--   the type the generated module imports, so that the Admin SP's module
--   type-checks alone. Its values are not Opal's: only their type is held.
opalTableDescs :: Text
opalTableDescs = T.unlines
  [ "{-# LANGUAGE NoImplicitPrelude #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "-- | A stand-in for the generated Hsed.Gen.Opal.TableDescs, for a type check alone."
  , "module Hsed.Gen.Opal.TableDescs (dataRemovalMechanismTableDesc, tableTableDesc, tPerInfoTableDesc) where"
  , ""
  , "import           Hsed.Core.Table          (TableDesc (..))"
  , "import           Hsed.Core.UID            (uid)"
  , "import qualified Hsed.Gen.Core.TableDescs as Core"
  , ""
  , "tPerInfoTableDesc :: TableDesc"
  , "tPerInfoTableDesc = Core.tPerInfoTableDesc"
  , ""
  , "tableTableDesc :: TableDesc"
  , "tableTableDesc = Core.tableTableDesc"
  , ""
  , "dataRemovalMechanismTableDesc :: TableDesc"
  , "dataRemovalMechanismTableDesc ="
  , "  TableDesc \"DataRemovalMechanism\" (uid 0x00 0x00 0x11 0x01 0x00 0x00 0x00 0x00) (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x11 0x01) []" ]

spec :: Spec
spec = do
  describe "targetNoun" $ do
    it "reads Opal's <SP> - <T> Table Preconfiguration and Core's Default … <Ts>" $ do
      targetNoun "Admin SP - Authority Table Preconfiguration" `shouldBe` Just "Authority"
      targetNoun "Admin SP – DataRemovalMechansim Table Preconfiguration" `shouldBe` Just "DataRemovalMechansim"
      targetNoun "Default Base Template Authorities" `shouldBe` Just "Authorities"
      targetNoun "Authority Table Description" `shouldBe` Nothing
    it "names the generated rows after the caption" $ do
      preDeclName "Default Base Template Authorities" `shouldBe` "defaultBaseTemplateAuthorities"
      preDeclName "Admin SP - Authority Table Preconfiguration" `shouldBe` "adminSPAuthorityTablePreconfiguration"

  describe "spOf and preconfigModuleFor (M4b)" $ do
    it "read the SP an Opal caption names before its dash, a hyphen or an en dash; Core's captions name none" $ do
      spOf "Admin SP - C_PIN Table Preconfiguration" `shouldBe` Just "Admin SP"
      spOf "Admin SP \x2013 DataRemovalMechanism Table Preconfiguration" `shouldBe` Just "Admin SP"
      spOf "Locking SP - Locking Table Preconfiguration" `shouldBe` Just "Locking SP"
      spOf "Default Base Template Authorities" `shouldBe` Nothing
      spOf "Admin SP - TPerInfo Columns" `shouldBe` Nothing
    it "give an SP's tables a module of their own beside Core's Preconfig: Hsed.Gen.Opal.Preconfig.AdminSP" $ do
      let gm = preconfigModuleFor "Admin SP"
      Gen.moduleName (SpecId "opal-2.30") gm `shouldBe` "Hsed.Gen.Opal.Preconfig.AdminSP"
      Gen.modulePath "hsed/gen" (Gen.moduleName (SpecId "opal-2.30") gm) `shouldBe` "hsed/gen/Hsed/Gen/Opal/Preconfig/AdminSP.hs"
      Gen.moduleName (SpecId "opal-2.30") (preconfigModuleFor "Locking SP") `shouldBe` "Hsed.Gen.Opal.Preconfig.LockingSP"
      Gen.moduleName core preconfigModule `shouldBe` "Hsed.Gen.Core.Preconfig"

  describe "Opal Table 24, the design's exemplar" $ do
    let path = "../spec/ir/opal-2.30.json"
    present <- runIO (doesFileExist path)
    if not present then it "needs the committed Opal IR" (pendingWith path) else do
      opal <- runIO (readIR path >>= either throwString pure)
      it "fires the rule" $
        [ fires preconfigRule (SpecId "opal-2.30") t (measure (SpecId "opal-2.30") t) | t <- opal.tables, t.id == Just "T24" ]
          `shouldBe` [True]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx = mkContext Scott ir
        ps  = preReads ctx

    describe "the preconfig rule" $ do
      it "binds Tables 209 and 216, the default authorities, by the doc anchor's rule token" $ do
        [ aid | (aid, RuleBound (RuleName "preconfig") _ _) <- Map.toList (classify ir) ] `shouldBe` [TableId 209, TableId 216]
        explain ir (TableId 209) `shouldBe` "preconfig (exemplar core T209)"
      it "reads both as rows of the Authority table, its description T178, for the Base and Admin templates" $ do
        [ (btAnchor (pBound p), either (const Nothing) (Just . btAnchor . drBound) (pTarget p), captionTemplate p) | p <- ps ]
          `shouldBe` [ (TableId 209, Just (TableId 178), Just "Base"), (TableId 216, Just (TableId 178), Just "Admin") ]
      it "gives the 17 default authorities' names and UIDs, for the table of authority UIDs to agree with" $ do
        let rows = [ (peName e, peUID e) | e <- preEntries ctx, peTarget e == "Authority" ]
        length rows `shouldBe` 17
        take 1 rows `shouldBe` [("Anybody", [0, 0, 0, 9, 0, 0, 0, 1])]
        drop 16 rows `shouldBe` [("Reserve3", [0, 0, 0, 9, 0, 0, 2, 8])]
      it "finds no Error on the committed annotations" $
        errorsOf ir `shouldBe` []

    describe "C-PRECONF-UID, Table 243 against the default authorities of Tables 209 and 216" $ do
      let preconf ir' = [ renderFinding f | f <- checkIR Nothing Scott ir', Cm.code f == "C-PRECONF-UID" ]
      it "passes: the 17 rows are those the two tables preconfigure, with their names, UIDs and templates" $ do
        preconf ir `shouldBe` []
        [ (peName e, peUID e, peTemplate e) | e <- preEntries ctx, peTarget e == "Authority" ]
          `shouldBe` [ (fromMaybe "" (urName r), bs, urTemplate r) | r <- uidRows ctx, btAnchor (urBound r) == TableId 243, Exact bs <- [urValue r] ]
        map peWhere (preEntries ctx) `shouldBe` map (\i -> "T209 row " <> tshow i) [1 .. 9 :: Int] <> map (\i -> "T216 row " <> tshow i) [1 .. 8 :: Int]
      it "fails on a name the preconfiguration gives otherwise, at the row" $
        preconf (setCell "T243" 9 1 "Issuer" ir) `shouldBe`
          [ "C-PRECONF-UID core-2.01.pdf#page=305&T243: row 10 \"Issuer | Admin\": it is Issuers in T216 row 1" ]
      it "fails on a UID one side gives and the other lacks" $
        preconf (setCell "T216" 7 1 "00 00 00 09 00 00 02 09" ir) `shouldBe`
          [ "C-PRECONF-UID core-2.01.pdf#page=304&T243: T216 row 8 preconfigures Reserve3, 00 00 00 09 00 00 02 09, which is not among the rows"
          , "C-PRECONF-UID core-2.01.pdf#page=305&T243: row 17 \"Reserve3 | Admin\": no row preconfigured for the Authority table (T209, T216) has this UID" ]
      it "fails on a template other than the one the preconfiguration's caption names" $
        preconf (setCell "T243" 0 2 "Admin" ir) `shouldBe`
          [ "C-PRECONF-UID core-2.01.pdf#page=304&T243: row 1 \"Anybody | Admin\": T209 row 1 preconfigures it for the Base template" ]
      it "matches rows by UID, not by order: Table 243's first two rows swapped still pass" $ do
        let swapped = foldr (\(c, a, b) -> setCell "T243" 1 c b . setCell "T243" 0 c a) ir
                        [ (c, cellOf 1 c, cellOf 0 c) | c <- [0 .. 2] ]
            cellOf r c = maybe "" (\t -> t.text) (do t <- listToMaybe [ t | t <- ir.tables, t.id == Just "T243" ]
                                                     row <- listToMaybe (drop r t.rows)
                                                     listToMaybe (drop c row))
        preconf swapped `shouldBe` []
        take 2 [ urName r | r <- uidRows (mkContext Scott swapped), btAnchor (urBound r) == TableId 243 ] `shouldBe` [Just "Admins", Just "Anybody"]

    describe "C-PRECONF-HEADER" $
      it "fails on a header naming no column of the target, and leaves the table out" $ do
        let broken = setHeaderCell "T209" 0 5 "Operations" ir
        errorsOf broken `shouldBe`
          [ "C-PRECONF-HEADER core-2.01.pdf#page=211&T209: the header \"Operations\" names no column of T178 Authority" ]
        newlyExcluded ir broken `shouldReturn`
          [(TableId 209, "C-PRECONF-HEADER, C-UID-TABLE, C-UID-UNIQUE or C-PRECONF-CELL failed")]

    describe "C-UID-TABLE and C-UID-UNIQUE" $ do
      it "fail on a UID outside the Authority table" $
        errorsOf (setCell "T209" 0 1 "00 00 00 0A 00 00 00 01" ir) `shouldBe`
          [ "C-UID-TABLE core-2.01.pdf#page=211&T209: row 1 Anybody: the UID 00 00 00 0A 00 00 00 01 does not lie in the Authority table, whose half-UID is 00 00 00 09" ]
      it "fail on a UID two rows give, across the tables" $
        errorsOf (setCell "T216" 0 1 "00 00 00 09 00 00 00 01" ir) `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=236&T216: row 1 Issuers: the UID 00 00 00 09 00 00 00 01 is also that of T209 row 1 Anybody" ]

    describe "C-PRECONF-CELL" $ do
      it "fails on a label of no value of the column's enumeration" $
        errorsOf (setCell "T209" 0 5 "Sing" ir) `shouldBe`
          [ "C-PRECONF-CELL core-2.01.pdf#page=211&T209: row 1 Anybody: Operation: \"Sing\" is no label of auth_method's enumeration (None, Password, Exchange, Sign, SymK, HMAC, TPerSign, TPerExchange, Reserved)" ]
      it "fails on a reference naming no preconfigured row of the table it refers to" $
        errorsOf (setCell "T209" 3 4 "Maker" ir) `shouldBe`
          [ "C-PRECONF-CELL core-2.01.pdf#page=211&T209: row 4 MakerSymK: Class: \"Maker\" names no row preconfigured for Authority" ]

    describe "informative columns, group rows and template rows" $ do
      it "drops a column whose header says it is informative" $ do
        let informative = setHeaderCell "T209" 0 5 "Operation - informative text" ir
        errorsOf informative `shouldBe` []
        [ pDropped p | p <- preReads (mkContext Scott informative) ] `shouldBe` [["Operation - informative text"], []]
        lns <- generated informative
        filter ("(0x09," `T.isInfixOf`) (block "defaultBaseTemplateAuthorities" lns) `shouldBe` []
      it "reads a row whose first cell alone is printed as the context of the rows below it" $ do
        let grouped = foldr (\c -> setCell "T209" 0 c "") ir [1 .. 5]
        errorsOf grouped `shouldBe` []
        length [ e | e <- preEntries (mkContext Scott grouped), peTarget e == "Authority" ] `shouldBe` 16
        lns <- generated grouped
        take 1 (drop 2 (block "defaultBaseTemplateAuthorities" lns)) `shouldBe` ["  [ Pre  -- row 2 Admins (group Anybody)"]
      it "reads a row carrying (+XX) as a template, with no finding, which a table of rows leaves out (M4b)" $ do
        let templated = setCell "T216" 7 1 "00 00 00 09 00 00 02 08 (+XX)" ir
        errorsOf templated `shouldBe` []
        [ (prRow r, rdShape rd) | p <- preReads (mkContext Scott templated), btAnchor (pBound p) == TableId 216
                                , (r, rd) <- zip (pRows p) (pReads p), prTemplate r ]
          `shouldBe` [(7, TemplateRow (Placeholder "XX" 1) [0x00])]
        map renderFinding (filter ((== "W-PRECONF-TEMPLATE") . Cm.code) (checkIR Nothing Scott templated)) `shouldBe` []
        lns <- generated templated
        length (filter ("Pre  -- " `T.isInfixOf`) (block "defaultAdminTemplateAuthorities" lns)) `shouldBe` 7

    describe "the generated module" $ do
      lns <- runIO (generated ir)
      it "declares the rows of each table" $ do
        length (filter ("Pre  -- " `T.isInfixOf`) (block "defaultBaseTemplateAuthorities" lns)) `shouldBe` 9
        length (filter ("Pre  -- " `T.isInfixOf`) (block "defaultAdminTemplateAuthorities" lns)) `shouldBe` 8
      it "gives each row's cells by column number, typed by the Authority table's description" $
        take 9 (drop 2 (block "defaultBaseTemplateAuthorities" lns)) `shouldBe`
          [ "  [ Pre  -- row 1 Anybody"
          , "      [ (0x00, UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x01))"
          , "      , (0x01, TextCell \"Anybody\")"
          , "      , (0x02, TextCell \"Anybody\")"
          , "      , (0x03, EnumCell \"False\" 0)"
          , "      , (0x09, EnumCell \"Sign\" 3)"
          , "      ]"
          , "      []"
          , "  , Pre  -- row 2 Admins" ]
      it "resolves a Class by the name of a preconfigured authority" $
        block "defaultBaseTemplateAuthorities" lns `shouldContain`
          ["      , (0x04, RefCell \"Admins\" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02))"]
      it "cites the table and its target" $
        lns `shouldSatisfy` any (\l -> "-- | core-2.01 T216 Default Admin Template Authorities (PDF 236 / p.219, ord 218, sha256:" `T.isPrefixOf` l
                                         && "): rows of the Authority table (T178)" `T.isSuffixOf` l)

  withM4bOpal $ \base opal -> do
    let octx     = opalOver base opal
        ps       = preReads octx
        numbers  = [18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 33] :: [Int]
        anchors  = map TableId numbers
        mine f   = Cm.anchor f `elem` map (Just . renderAnchorId) anchors
        found ir' = filter mine (checkIR (Just base) Scott ir')
        codeOf c ir' = [ renderFinding f | f <- found ir', Cm.code f == c ]
        readOf n = listToMaybe [ p | p <- ps, btAnchor (pBound p) == TableId n ]
        -- a table's rows read: each row's Name (blank without one) and its cells by column name
        rowsOf n = [ (fromMaybe "" (lookup "Name" (prValues r)), [ (crName c, cr) | (_, c, cr) <- rdCells rd ])
                   | Just p <- [readOf n], (r, rd) <- zip (pRows p) (pReads p) ]
        cellAt n rowName col = listToMaybe [ cr | (rn, cs) <- rowsOf n, rn == rowName, (cn, cr) <- cs, cn == col ]
        compiled ir' = either (throwString . show) pure (compileIR (Just base) Scott ir')
        adminSPOf ir' = fromMaybe "" . lookup "Hsed.Gen.Opal.Preconfig.AdminSP" . crModules <$> compiled ir'
        excludedOf ir' = (\cr -> [ (a, w) | (a, w) <- crExcluded cr, a `elem` anchors ]) <$> compiled ir'
        spExcluded = "C-PRECONF-HEADER, C-UID-TABLE, C-UID-UNIQUE, C-UID-NAME, C-PRECONF-CELL, C-ACL-REFS, C-ACL-EXPR or C-LEGEND-TOTAL failed"
        nullUID = replicate 8 0 :: [Word8]
        boolean = KEnum "boolean" [(0, 1)] [("False", 0), ("True", 1)]

    describe "Opal's Admin SP, under the M4b sidecar at section 4.2 (PDF 35-52), read over Core" $ do
      it "binds the twelve tables, each read against its table's description: Core's, Opal's own, or Core's with Opal's columns" $ do
        map (btAnchor . pBound) ps `shouldBe` anchors
        [ (either (const Nothing) drTable (pTarget p), pSP p, specIdText <$> pDescSpec p) | p <- ps ] `shouldBe`
          [ (Just t, Just "Admin SP", Just sp)
          | (t, sp) <- [ ("SPInfo", "core-2.01"), ("SPTemplates", "core-2.01"), ("Table", "opal-2.30"), ("MethodID", "core-2.01")
                       , ("AccessControl", "core-2.01"), ("ACE", "core-2.01"), ("Authority", "core-2.01"), ("C_PIN", "core-2.01")
                       , ("TPerInfo", "opal-2.30"), ("Template", "core-2.01"), ("SP", "core-2.01"), ("DataRemovalMechanism", "opal-2.30") ] ]
      it "reads Table 27 against TPerInfo's nine columns (T26's added, PDF 49) and Table 20 against the Table table's fifteen (T50's, PDF 98-99)" $
        [ either (const 0) (length . drColumns) (pTarget p) | n <- [27, 20], Just p <- [readOf n] ] `shouldBe` [9, 15]
      it "reads them with no Error, and compile leaves none of them out" $ do
        map renderFinding (filter isError (found opal)) `shouldBe` []
        excludedOf opal `shouldReturn` []
      it "generates Hsed.Gen.Opal.Preconfig.AdminSP: twelve PreconfTables, and adminSP lists them in table order" $ do
        lns <- T.lines <$> adminSPOf opal
        let tables = [ T.takeWhile (/= ' ') l | l <- lns, " :: PreconfTable" `T.isSuffixOf` l ]
        length tables `shouldBe` 12
        takeWhile (/= "") (dropWhile (/= "adminSP :: [PreconfTable]") lns) `shouldBe`
          ["adminSP :: [PreconfTable]", "adminSP ="] <> zipWith (\i t -> (if i == (0 :: Int) then "  [ " else "  , ") <> t) [0 ..] tables <> ["  ]"]
        tables `shouldBe`
          [ "adminSPSPInfoTablePreconfiguration", "adminSPSPTemplatesTablePreconfiguration", "adminSPTableTablePreconfiguration"
          , "adminSPMethodIDTablePreconfiguration", "adminSPAccessControlTablePreconfiguration", "adminSPACETablePreconfiguration"
          , "adminSPAuthorityTablePreconfiguration", "adminSPCPINTablePreconfiguration", "adminSPTPerInfoTablePreconfiguration"
          , "adminSPTemplateTablePreconfiguration", "adminSPSPTablePreconfiguration", "adminSPDataRemovalMechanismTablePreconfiguration" ]
      it "prints a module that type-checks under hsed's warnings and -Werror, beside a stand-in Opal TableDescs (the lead's answer 16)" $ do
        text <- adminSPOf opal
        compiles [("Hsed.Gen.Opal.TableDescs", opalTableDescs)] "Hsed.Gen.Opal.Preconfig.AdminSP" text

    describe "Table 22, AccessControl (PDF 39-46)" $ do
      let shapes = [ rdShape rd | Just p <- [readOf 22], rd <- pReads p ]
      it "reads 34 rows: 22 plain, 9 for every object of a table (TT TT TT TT, *AC1-*AC7), 3 templates ((+XX))" $ do
        length shapes `shouldBe` 34
        length [ () | PlainRow <- shapes ] `shouldBe` 22
        [ h | EveryObjectRow h <- shapes ]
          `shouldBe` [[0, 0, 0, 1], [0, 0, 0, 3], [0, 0, 0, 6], [0, 0, 0, 8], [0, 0, 0, 9], [0, 0, 2, 4], [0, 0, 2, 5], [0, 0, 2, 5], [0, 0, 2, 5]]
        [ s' | s'@(TemplateRow _ _) <- shapes ] `shouldBe` replicate 3 (TemplateRow (Placeholder "XX" 1) [0x01])
      it "gives an every-object row no InvokingID cell, and every other row's InvokingID a UID as printed" $
        [ (isEvery, [ () | ("InvokingID", ReadValue (CvUID _) _) <- cs ] == [()])
        | (rd, (_, cs)) <- zip [ rd | Just p <- [readOf 22], rd <- pReads p ] (rowsOf 22)
        , let isEvery = case rdShape rd of EveryObjectRow _ -> True; _ -> False ]
          `shouldSatisfy` all (\(isEvery, hasUID) -> isEvery /= hasUID)
      it "leaves its UID column blank in every row: N-PRECONF-UID-TPER, its UIDs the TPer's (Core 5.3.2.7.1, PDF 162)" $
        codeOf "N-PRECONF-UID-TPER" opal `shouldBe`
          [ "N-PRECONF-UID-TPER opal-2.30.pdf#page=39&T22: every row leaves the UID column blank, its UIDs the TPer's to assign"
            <> " (Core 5.3.2.7.1, PDF 162, the AccessControl table's): none is read or generated" ]
      it "names Table 21's methods: Next 8, Get 14, Set 8, Authenticate, Random, Revert and Activate once each, each at its UID" $ do
        let methods = [ (n, u) | (_, cs) <- rowsOf 22, ("MethodID", ReadValue (CvRef n u) _) <- cs ]
            t21 = [ (n, u) | (n, cs) <- rowsOf 21, ("UID", ReadValue (CvUID u) _) <- cs ]
        Map.toList (Map.fromListWith (+) [ (n, 1 :: Int) | (n, _) <- methods ])
          `shouldBe` [("Activate", 1), ("Authenticate", 1), ("Get", 14), ("Next", 8), ("Random", 1), ("Revert", 1), ("Set", 8)]
        all (\(n, u) -> lookup n t21 == Just u) methods `shouldBe` True
      it "names Table 23's ten ACEs in its ACLs, ACE_Anybody in every GetACLACL, and gives no C-ACL-REFS" $ do
        let acl c = [ n | (_, cs) <- rowsOf 22, (c', ReadValue (CvList vs) _) <- cs, c' == c, CvRef n _ <- vs ]
        L.sort (L.nub (acl "ACL")) `shouldBe` L.sort [ n | (n, _) <- rowsOf 23 ]
        acl "GetACLACL" `shouldBe` replicate 34 "ACE_Anybody"
        codeOf "C-ACL-REFS" opal `shouldBe` []
      it "fails C-ACL-REFS on an ACL or a MethodID naming nothing of the SP, and leaves the table out" $ do
        let noACE = setCell "T22" 22 6 "ACE_Nobody" opal
            noMethod = setCell "T22" 22 4 "Gett" opal
        codeOf "C-ACL-REFS" noACE `shouldBe`
          [ "C-ACL-REFS opal-2.30.pdf#page=42&T22: row 23: ACL: \"ACE_Nobody\" names no row preconfigured for ACE" ]
        codeOf "C-ACL-REFS" noMethod `shouldBe`
          [ "C-ACL-REFS opal-2.30.pdf#page=42&T22: row 23: MethodID: \"Gett\" names no row preconfigured for MethodID" ]
        excludedOf noACE `shouldReturn` [(TableId 22, spExcluded)]
      it "fails C-ACL-REFS at an ACE that lists a column a table it applies to lacks, or that no row names" $ do
        codeOf "C-ACL-REFS" (setCell "T22" 22 6 "ACE_Set_Enabled" opal) `shouldBe`
          [ "C-ACL-REFS opal-2.30.pdf#page=47&T23: row 5 ACE_Set_Enabled: Columns: \"Enabled\" names no column of the C_PIN table (T181),"
            <> " which ACE_Set_Enabled applies to" ]
        codeOf "C-ACL-REFS" (setCell "T22" 31 6 "ACE_Anybody" opal) `shouldBe`
          [ "C-ACL-REFS opal-2.30.pdf#page=47&T23: row 12 ACE_TPerInfo_Set_ProgrammaticResetEnable: Columns:"
            <> " \"ACE_TPerInfo_Set_ProgrammaticResetEnable\" is in no access control row's ACL, so no table holds the columns its Columns name" ]

    describe "Table 23, ACE (PDF 46-47)" $ do
      it "reads a BooleanExpr into postfix: Admins OR SID is Admins, SID, Or (Core 5.3.4.3.2, PDF 226-227)" $
        cellAt 23 "ACE_C_PIN_SID_Get_NOPIN" "BooleanExpr" `shouldBe`
          Just (ReadValue (CvList [CvRef "Admins" [0, 0, 0, 9, 0, 0, 0, 2], CvRef "SID" [0, 0, 0, 9, 0, 0, 0, 6], CvEnum "OR" 1])
                          [LabelRead "boolean_ACE" "OR" "Or" False])
      it "reads Columns as the numbers of the columns of the table the citing rows invoke on, All as none (Core 5.3.2.9.5, PDF 165)" $
        [ (n, [ c | CvUInt c _ <- vs ]) | (n, cs) <- rowsOf 23, ("Columns", ReadValue (CvList vs) _) <- cs ] `shouldBe`
          [ ("ACE_Anybody", []), ("ACE_Admin", []), ("ACE_Set_Enabled", [5]), ("ACE_C_PIN_SID_Get_NOPIN", [0, 4, 5, 6, 7])
          , ("ACE_C_PIN_SID_Set_PIN", [3]), ("ACE_C_PIN_MSID_Get_PIN", [0, 3]), ("ACE_C_PIN_Admins_Set_PIN", [3])
          , ("ACE_TPerInfo_Set_ProgrammaticResetEnable", [8]), ("ACE_SP_SID", [])
          , ("ACE_DataRemovalMechanism_Set_ActiveDataRemovalMechanism", [1]) ]
      it "keeps the 40- and 55-byte names as printed, W-PRECONF-NAME-LENGTH (PDF 47; the lead's answer 5)" $
        codeOf "W-PRECONF-NAME-LENGTH" opal `shouldBe`
          [ "W-PRECONF-NAME-LENGTH opal-2.30.pdf#page=47&T23: row 12 ACE_TPerInfo_Set_ProgrammaticResetEnable: Name:"
            <> " \"ACE_TPerInfo_Set_ProgrammaticResetEnable\" is 40 bytes, longer than the 32 its type (name) allows; kept as printed"
          , "W-PRECONF-NAME-LENGTH opal-2.30.pdf#page=47&T23: row 16 ACE_DataRemovalMechanism_Set_ActiveDataRemovalMechanism: Name:"
            <> " \"ACE_DataRemovalMechanism_Set_ActiveDataRemovalMechanism\" is 55 bytes, longer than the 32 its type (name) allows; kept as printed" ]
      it "fails C-ACL-EXPR on a BooleanExpr that does not parse, and C-ACL-REFS on a column the ACE's table lacks" $ do
        codeOf "C-ACL-EXPR" (setCell "T23" 6 4 "Admins XOR SID" opal) `shouldBe`
          [ "C-ACL-EXPR opal-2.30.pdf#page=47&T23: row 7 ACE_C_PIN_SID_Get_NOPIN: BooleanExpr: \"XOR\" follows an authority where"
            <> " an operator (And, Or, Not) is expected" ]
        codeOf "C-ACL-REFS" (setCell "T23" 8 5 "UID, Colour" opal) `shouldBe`
          [ "C-ACL-REFS opal-2.30.pdf#page=47&T23: row 9 ACE_C_PIN_MSID_Get_PIN: Columns: \"Colour\" names no column of the C_PIN"
            <> " table (T181), which ACE_C_PIN_MSID_Get_PIN applies to" ]

    describe "Tables 29, 24 and the labels" $ do
      it "reads Table 29's LifeCycle as LifeCycleState (N-PRECONF-HEADER), its values Table 49's, Frozen a boolean defaulting to False (PDF 50, 98, 233)" $ do
        map (\n -> cellAt 29 n "LifeCycleState") ["Admin", "Locking"] `shouldBe`
          [ Just (ReadValue (CvEnum "Manufactured" 9) [LabelRead "life_cycle_state" "Manufactured" "manufactured" False])
          , Just (ReadValue (CvEnum "Manufactured-Inactive" 8) [LabelRead "life_cycle_state" "Manufactured-Inactive" "manufactured-inactive" False]) ]
        cellAt 29 "Admin" "Frozen" `shouldBe` Just (ReadValue (CvEnum "FALSE" 0) [LabelRead "boolean" "FALSE" "False" False])
        [ kcKind kc | Just p <- [readOf 29], kc <- pColumns p, kcHeader kc == "Frozen" ] `shouldBe` [Right (KDefault "false" boolean)]
        codeOf "N-PRECONF-HEADER" opal `shouldBe`
          [ "N-PRECONF-HEADER opal-2.30.pdf#page=50&T29: the header \"LifeCycle\" is read as LifeCycleState, the one column of T215 SP whose name it begins" ]
      it "reads Table 24's Null as the NULL UID, Secure's None by Core Table 179 (PDF 167), and C_PIN_Admin 1 as C_PIN_Admin1 (PDF 48)" $ do
        map (cellAt 24 "Anybody") ["Class", "Credential", "ResponseSign", "ResponseExch"] `shouldBe` replicate 4 (Just (ReadValue (CvRef "Null" nullUID) []))
        cellAt 24 "Anybody" "Secure" `shouldBe` Just (ReadValue (CvEnum "None" 0) [])
        cellAt 24 "Admin1" "Credential" `shouldBe` Just (ReadValue (CvRef "C_PIN_Admin1" [0, 0, 0, 0x0B, 0, 0, 2, 1]) [])
      it "reads Table 24's T and F as True and False, by the one label each begins" $ do
        map (cellAt 24 "Anybody") ["IsClass", "Enabled"] `shouldBe`
          [ Just (ReadValue (CvEnum "F" 0) [LabelRead "boolean" "F" "False" True]), Just (ReadValue (CvEnum "T" 1) [LabelRead "boolean" "T" "True" True]) ]
        [ T.drop (T.length "N-PRECONF-LABEL opal-2.30.pdf#page=48&T24: ") f | f <- codeOf "N-PRECONF-LABEL" opal, "&T24:" `T.isInfixOf` f ]
          `shouldBe` ["\"F\" is read as boolean's False, the one label it begins", "\"T\" is read as boolean's True, the one label it begins"]
      it "reports each label read otherwise once per table: nine in all (T18, T23, T24 twice, T25, T27, T29 thrice)" $
        length (codeOf "N-PRECONF-LABEL" opal) `shouldBe` 9
      it "takes messaging_type's labels from Table 179, which its section names (Core 5.1.3.55, PDF 116)" $ do
        take 2 (teLabels (preconfTypeEnv octx) "messaging_type") `shouldBe` [("None", 0), ("HMAC_SHA_256", 1)]
        teLabels (typeEnv octx) "messaging_type" `shouldBe` []

    describe "templates, values the TPer chooses, and notes" $ do
      it "reads (+XX) rows as templates, the columns carrying the placeholder listed, (O) as optional, the digit after it a note (PDF 41-49)" $
        [ (n, prRow r, rdShape rd, prOptional r, prNotes r)
        | n <- [24, 25], Just p <- [readOf n], (r, rd) <- zip (pRows p) (pReads p), prTemplate r ] `shouldBe`
          [ (24, 5, TemplateRow (Placeholder "XX" 1) [0x00, 0x01, 0x0A], True, ["1"])
          , (25, 3, TemplateRow (Placeholder "XX" 1) [0x00, 0x01], True, []) ]
      it "reads a template's Credential as a reference to the C_PIN template's base (PDF 48-49)" $
        cellAt 24 "AdminXX" "Credential" `shouldBe` Just (ReadValue (CvRef "C_PIN_AdminXX" [0, 0, 0, 0x0B, 0, 0, 2, 0]) [])
      it "reads the eight VU and MSID cells as values the TPer chooses (PDF 48-52)" $
        [ (n, rowName, col, m) | n <- numbers, (rowName, cs) <- rowsOf n, (col, ReadVendor m) <- cs ] `shouldBe`
          [ (25, "C_PIN_SID", "PIN", "VU"), (25, "C_PIN_SID", "TryLimit", "VU"), (25, "C_PIN_SID", "Tries", "VU"), (25, "C_PIN_MSID", "PIN", "MSID")
          , (27, "", "GUDID", "VU"), (28, "Base", "Instances", "VU"), (28, "Base", "MaxInstances", "VU"), (33, "", "ActiveDataRemovalMechanism", "VU") ]
      it "keeps every footnote marker as its row's note, informative columns' included (PDF 35-50)" $
        [ (n, notes) | n <- numbers, Just p <- [readOf n], r <- pRows p, let notes = prNotes r, not (null notes) ] `shouldBe`
          [ (19, ["*ST1"]), (19, ["*ST1"]), (21, ["*MT1"]), (21, ["*MT2"]), (22, ["*AC1"]), (22, ["*AC2"]), (22, ["*AC3"]), (22, ["*AC4"])
          , (22, ["*AC5"]), (22, ["*AC6"]), (22, ["*AC7"]), (22, ["*AC8", "*AC7"]), (22, ["*AC9", "*AC7"]), (23, ["*ACE1"]), (23, ["*ACE1"])
          , (24, ["1"]), (27, ["*TP1", "*TP2", "*TP3"]), (29, ["*SP1"]) ]

    describe "the UIDs the Admin SP's tables give" $ do
      it "generates those Core does not name, equal to the hand transcription's (hsed's test fixture Hand.OpalUIDs) and Revert's and Activate's (PDF 38-50)" $ do
        lns <- T.lines <$> adminSPOf opal
        filter (`elem` lns)
          [ "uC_PIN_MSID = uid 0x00 0x00 0x00 0x0b 0x00 0x00 0x84 0x02", "uC_PIN_Admin1 = uid 0x00 0x00 0x00 0x0b 0x00 0x00 0x02 0x01"
          , "uAuthority_Admin1 = uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x01"
          , "uACE_C_PIN_SID_Get_NOPIN = uid 0x00 0x00 0x00 0x08 0x00 0x00 0x8c 0x02"
          , "uACE_C_PIN_MSID_Get_PIN = uid 0x00 0x00 0x00 0x08 0x00 0x00 0x8c 0x04", "uSP_Locking = uid 0x00 0x00 0x02 0x05 0x00 0x00 0x00 0x02"
          , "uMethodID_Revert = uid 0x00 0x00 0x00 0x06 0x00 0x00 0x02 0x02", "uMethodID_Activate = uid 0x00 0x00 0x00 0x06 0x00 0x00 0x02 0x03" ]
          `shouldSatisfy` ((== 8) . length)
      it "names 29 UIDs, u<T>_<Name>, a Name beginning <T>_ alone; none Core names the same (uAuthority_Anybody, uSP_Admin, uMethodID_Get)" $ do
        lns <- T.lines <$> adminSPOf opal
        let uids = [ T.takeWhile (/= ' ') l | l <- lns, " :: UID" `T.isSuffixOf` l ]
        length uids `shouldBe` 29
        filter (`elem` uids) ["uAuthority_Anybody", "uSP_Admin", "uC_PIN_SID", "uMethodID_Get", "uTemplate_Base", "uSPTemplates_Admin"] `shouldBe` []
        filter ("uTable_" `T.isPrefixOf`) uids `shouldSatisfy` ((== 12) . length)
        filter ("uACE_" `T.isPrefixOf`) uids `shouldSatisfy` ((== 10) . length)
        uidDeclName "C_PIN" "C_PIN_MSID" `shouldBe` "uC_PIN_MSID"
        uidDeclName "Authority" "Admin1" `shouldBe` "uAuthority_Admin1"
      it "fails C-UID-NAME on a Name whose UID name Core gives another UID, and leaves the table out" $ do
        let renamed = setCell "T24" 4 1 "\"Editors\"" opal
        codeOf "C-UID-NAME" renamed `shouldBe`
          [ "C-UID-NAME opal-2.30.pdf#page=48&T24: row 5 Editors: it is generated as uAuthority_Editors, as core-2.01 T243 row 11 is,"
            <> " for the UID 00 00 00 09 00 00 02 02" ]
        excludedOf renamed `shouldReturn` [(TableId 24, spExcluded)]

    describe "the legend's classes (G1's classifier, by a hand-built scale; documentation, the lead's answer 2)" $
      it "gives each kept cell of a row its class, by column number in column order" $ do
        let scale = LegendScale [ LegendClass 230 False ReadOnlySpecified, LegendClass 230 True ReadOnlyVU
                                , LegendClass 179 True WritePersonalizable, LegendClass 179 False WriteFixed ] "(N)" 3
        [ map fst (cellClasses scale p r) | Just p <- [readOf 25], r <- pRows p ] `shouldBe` replicate 4 [0 .. 7]
        [ map fst (cellClasses scale p r) | Just p <- [readOf 22], r <- take 1 (pRows p) ] `shouldBe` [[0 .. 14]]
