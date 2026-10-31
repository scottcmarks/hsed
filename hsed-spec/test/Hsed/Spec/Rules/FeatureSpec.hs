{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Spec.Rules.FeatureSpec
Description : Test specifications for the feature rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The feature rule on the committed Core extraction with the committed
annotations (design section 3.4, example E8): it binds Tables 42 and 43 from
the doc anchor, over byteGrid, and fires on no other Core table; each reads
its header against the general format of Table 41 (the sentence of 3.3.6.3,
PDF 69), its pins from 3.3.6.4 and 3.3.6.5 (PDF 70, 71), its code from Table
40 (PDF 69), and finds nothing on the committed run. Each check made to fire
by an edit of the IR (a cell, a paragraph's words); a descriptor without pins
takes its code from Table 40 and leaves its version and length to the record;
a section whose words the IR lacks is an Error; byte 4 of Table 43 read by a
@bits@ modifier when its drawn extents are gone. A descriptor without pins
takes its code from Table 40 and its length from its bytes after the header
(a Note), leaving its version to the record. The generated module (a
record over the layout as byteGrid's, its @LayoutRecord@ and @Feature@
instances, a pin printed at its field's width), and an edited one
type-checked under @-Wall -Werror@.
M3: Opal's Tables 3-6 and 8 read over Core's Context, the base (@hsedc
--base@): each header held by its bits to Core's Table 41, found in the base
(Table 6's version by a name holding Version, its SSC minor over bits Table 41
reserves); codes from the pins, Table 8's from its cell, 0x0001 and 0x0002
found in Core's Table 40 by value; pins matched by word prefix (Table 6's
admin and user counts); Table 8's length its bytes after the header; the
allowances the bullets list; no Error; @Hsed.Gen.Opal.Features@ printed and
type-checked. Read alone, Opal Table 3 finds no general format. The values the
tests expect are those the PDFs print, on the pages cited; the bit maps against
sedutil's are hsed's acceptance test (@Hsed.Gen.Core.FeaturesSpec@).
-}
module Hsed.Spec.Rules.FeatureSpec (spec) where

import qualified Data.Text               as DT
import qualified RIO.Map                 as Map
import qualified RIO.Text                as T
import           RIO                      hiding (words)
import           RIO.Directory           (createDirectoryIfMissing, doesFileExist)
import           RIO.FilePath            (takeDirectory, (</>))
import           System.Environment      (lookupEnv)
import           System.Process          (readProcessWithExitCode)
import           Test.Hspec

import qualified RIO.Char                as C
import qualified RIO.List                as L
import           Hsed.Core.Feature       (Allowance (..))
import           Hsed.Core.Layout        (Field (..), FieldKind (..), Layout (..))
import           Hsed.Spec.Anchor        (AnchorId (..))
import           Hsed.Spec.Check         (checkIR, mkContext, mkContextWith)
import           Hsed.Spec.Common        (ErrataSet (..), RuleName (..), SpecId (..), renderFinding)
import qualified Hsed.Spec.Common        as Cm
import           Hsed.Spec.Compile       (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL           (SomeBinding (..), feature)
import           Hsed.Spec.IR            (CellIR (..), ParaIR (..), SpecIR (..), TableIR (..), XrefIR (..), readIR)
import           Hsed.Spec.Readers.Grid  (GridField (..))
import           Hsed.Spec.Readers.Pins  (Pin (..), PinValue (..))
import qualified Hsed.Spec.Gen           as Gen
import           Hsed.Spec.Readers.Record (RecordField (..), Structure (..), declScope, recordFieldsOf, typeName)
import           Hsed.Spec.Readers.Text  (Number (..))
import           Hsed.Spec.Registry      (registry)
import           Hsed.Spec.RuleDef       (BoundTable (..), Classification (..), RuleDef (..), modsOf, numberedTables)
import           Hsed.Spec.Rules         (classify, explain)
import           Hsed.Spec.Rules.Codes   (CodeRowRead (..))
import           Hsed.Spec.Rules.Feature
import           Hsed.Spec.Shape         (measure)


-- | The tables a rule fires on anywhere in an IR, by number, whatever its scope.
firesOn :: RuleDef -> SpecIR -> [Int]
firesOn r ir = [ n | (TableId n, t) <- Map.toList (numberedTables ir), fires r sid t (measure sid t) ]
  where sid = SpecId ir.spec

-- | The IR with the words of a section's paragraphs edited.
editWords :: Text -> (Maybe [Text] -> Maybe [Text]) -> SpecIR -> SpecIR
editWords sec f ir = ir { paras = map edit ir.paras }
  where
    edit p
      | p.section == Just sec = p { words = f p.words }
      | otherwise             = p

-- | The IR with no drawn extents in the row of a byte of a table.
withoutExtents :: Text -> Text -> SpecIR -> SpecIR
withoutExtents tid label ir = ir { tables = map table ir.tables }
  where
    table t
      | t.id == Just tid = t { rows = map strip t.rows }
      | otherwise        = t
    strip r@(c : _) | c.text == label = map (\x -> x { xExtent = Nothing }) r
    strip r                           = r

-- | The codes the rule and the readers it uses give.
featureCodes :: [Text]
featureCodes = map Cm.codeName (ruleCodes featureRule)
  <> ["C-LAYOUT-EXTENT", "C-LAYOUT-ENDIAN", "N-LAYOUT-BITS", "W-LAYOUT-BITS-UNUSED", "C-LAYOUT-TILE", "C-GRID-NAME", "W-PINS-NONE", "C-PINS-WORDS"]

-- | A line with its spacing squeezed, so that an assertion holds whatever the alignment.
squeezed :: Text -> Text
squeezed = T.unwords . T.words

-- | @ghc -fno-code -Wall -Werror@ on a printed module against hsed-core (as LayoutSpec does).
compiles :: Text -> Text -> Expectation
compiles modName text = withSystemTempDirectory "hsed-features" $ \dir -> do
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
spec = do
  describe "featureStructureName" $
    it "names the record after the caption's title without its last word" $
      map featureStructureName ["TPer Feature Descriptor", "Locking Feature Descriptor", "Level 0 Discovery - TPer Feature Descriptor", "Feature"]
        `shouldBe` ["TPer Feature", "Locking Feature", "Level 0 Discovery - TPer Feature", "Feature"]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx = mkContext Scott ir
        reads' = featureReads ctx
        readOf n = case [ fr | fr <- reads', btAnchor (frBound fr) == TableId n ] of
          fr : _ -> fr
          []     -> error ("no feature read of T" <> show n)
        -- the findings of the rule's codes at its tables (byteGrid gives some of the readers' codes too: T38, T41, T44's W-PINS-NONE)
        findings ir' = [ renderFinding f | f <- checkIR Nothing Scott ir', Cm.code f `elem` featureCodes, Cm.anchor f `elem` [Just "T42", Just "T43"] ]
        moduleOf ir' = do
          cr <- either (throwString . show) pure (compileIR Nothing Scott ir')
          pure (fromMaybe "" (lookup "Hsed.Gen.Core.Features" (crModules cr)))

    describe "the feature rule" $ do
      it "binds Tables 42 and 43, and no other table, by the doc anchor's rule token" $ do
        [ n | (TableId n, RuleBound (RuleName "feature") _ _) <- Map.toList (classify ir) ] `shouldBe` [42, 43]
        explain ir (TableId 42) `shouldBe` "feature (exemplar core T42)"
      it "fires on no other table of Core (Table 41's Feature Descriptor Template Format stays byteGrid's)" $
        firesOn featureRule ir `shouldBe` [42, 43]
      it "refines byteGrid" $
        refinesRule featureRule `shouldBe` Just (RuleName "byteGrid")

    describe "reading Tables 42 and 43" $ do
      it "finds their general format in Table 41, by the sentence of 3.3.6.3 (PDF 69)" $
        forM_ [42, 43] $ \n -> do
          let fr = readOf n
          fmap (\tm -> (tmNumber tm, tmSize tm, map gfBase (tmFields tm))) (frTemplate fr)
            `shouldBe` Right (41, 4, ["Feature Code", "Version", "Reserved", "Length"])
          fmap (\tm -> (tmSentence tm).sentence) (frTemplate fr)
            `shouldBe` Right "All feature descriptors SHALL conform to the general format defined in Table 41."
          frHeader fr `shouldBe` Right (Header "Feature Code" "Version" "Length" [])
      it "takes the codes Table 40 gives 3.3.6.4 and 3.3.6.5 (PDF 69), which their pins give too (PDF 70, 71)" $ do
        [ (frSection fr, map (\(_, _, c) -> nValue c) (frListed fr), nValue <$> frCode fr) | fr <- reads' ]
          `shouldBe` [(Just "3.3.6.4", [1], Just 1), (Just "3.3.6.5", [2], Just 2)]
        [ Map.keys (frPinned fr) | fr <- reads' ] `shouldBe` replicate 2 ["featurecode", "length", "version"]
        map frUnmatched reads' `shouldBe` [[], []]
      it "pins the header in the layout: the code, version 1 and length 12" $
        forM_ [(42, 1), (43, 2)] $ \(n, c) ->
          [ (fieldName f, fieldKind f) | f <- take 4 (layoutFields (frLayout (readOf n))) ]
            `shouldBe` [("Feature Code", Pinned c), ("Version", Pinned 1), ("Reserved", Reserved), ("Length", Pinned 12)]
      it "makes a record of the flags of byte 4, named from the cells, at the bits the grid prints them over (PDF 70, 71)" $ do
        [ (rfIdent f, rfType f, rfWhere f) | f <- recordFieldsOf (frStructure (readOf 42)) ] `shouldBe`
          [ ("tPerFeatureComIDMgmtSupported", "Bool", "byte 4 bit 6"), ("tPerFeatureStreamingSupported", "Bool", "byte 4 bit 4")
          , ("tPerFeatureBufferMgmtSupported", "Bool", "byte 4 bit 3"), ("tPerFeatureACKNAKSupported", "Bool", "byte 4 bit 2")
          , ("tPerFeatureAsyncSupported", "Bool", "byte 4 bit 1"), ("tPerFeatureSyncSupported", "Bool", "byte 4 bit 0") ]
        [ (rfName f, rfWhere f) | f <- recordFieldsOf (frStructure (readOf 43)) ] `shouldBe`
          [ ("MBR Done", "byte 4 bit 5"), ("MBR Enabled", "byte 4 bit 4"), ("Media Encryption", "byte 4 bit 3"), ("Locked", "byte 4 bit 2")
          , ("Locking Enabled", "byte 4 bit 1"), ("Locking Supported", "byte 4 bit 0") ]
        layoutSize (frLayout (readOf 43)) `shouldBe` 16

    describe "the checks" $ do
      it "find nothing on the committed run" $
        findings ir `shouldBe` []
      it "C-FEATURE-TEMPLATE: a header field other than the template's, and the table left out" $ do
        let broken = setCell "T42" 2 1 "Revision" ir
        findings broken `shouldBe`
          [ "C-FEATURE-TEMPLATE core-2.01.pdf#page=69&T42: TPer Feature Descriptor: its fields over bytes 0 to 3 are Feature Code 0.7-1.0, Revision 2.7-2.4, Reserved 2.3-2.0, Length 3.7-3.0, not those of Table 41, Feature Code 0.7-1.0, Version 2.7-2.4, Reserved 2.3-2.0, Length 3.7-3.0"
          , "W-FEATURE-PIN core-2.01.pdf#page=70&T42: TPer Feature Descriptor: 3.3.6.4 pins Version (0x01), no field of it" ]
        newlyExcluded ir broken `shouldReturn` [(TableId 42, "C-FEATURE-TEMPLATE failed")]
      it "C-FEATURE-TEMPLATE: no sentence names the general format" $ do
        let broken = ir { xrefs = filter (\x -> x.number /= 41) ir.xrefs }
        filter ("C-FEATURE-TEMPLATE" `T.isPrefixOf`) (findings broken) `shouldBe`
          [ "C-FEATURE-TEMPLATE core-2.01.pdf#page=69&T42: TPer Feature Descriptor: no sentence of the spec says which general format a TPer Feature Descriptor conforms to"
          , "C-FEATURE-TEMPLATE core-2.01.pdf#page=71&T43: Locking Feature Descriptor: no sentence of the spec says which general format a Locking Feature Descriptor conforms to" ]
      it "C-FEATURE-CODE: the code table gives another code than the pin, and the table left out" $ do
        let broken = setCell "T40" 1 0 "0005h" ir
        findings broken `shouldBe`
          [ "C-FEATURE-CODE core-2.01.pdf#page=69&T42: TPer Feature Descriptor: its Feature Code is pinned to 0x0001, but Table 40 row 2 (PDF 69) gives 0x0005" ]
        newlyExcluded ir broken `shouldReturn` [(TableId 42, "C-FEATURE-CODE failed")]
      it "W-FEATURE-UNLISTED: no code table lists the section; the pin gives the code" $ do
        let unlisted = setCell "T40" 1 2 "See 3.3.6.9" ir
        findings unlisted `shouldBe` [ "W-FEATURE-UNLISTED core-2.01.pdf#page=69&T42: TPer Feature Descriptor: no code table lists 3.3.6.4" ]
        newlyExcluded ir unlisted `shouldReturn` []
      it "C-FEATURE-LENGTH: a length pin other than the bytes after the header" $
        findings (editWords "3.3.6.4" (fmap (map (\w -> if w == "0x0C." then "0x10." else w))) ir) `shouldBe`
          [ "C-FEATURE-LENGTH core-2.01.pdf#page=69&T42: TPer Feature Descriptor: its Length is pinned to 16, but 12 of its 16 bytes follow the 4-byte header" ]
      it "C-FEATURE-PIN: a field pinned to two values" $
        findings (editWords "3.3.6.4" (fmap (\ws -> if "0x01." `elem` ws then ws <> T.words "The Version field SHALL be set to 0x02." else ws)) ir)
          `shouldBe` [ "C-FEATURE-PIN core-2.01.pdf#page=69&T42: TPer Feature Descriptor: Version = 0x01 (3.3.6.4) and Version = 0x02 (3.3.6.4)" ]
      it "C-GRID-NAME, as byteGrid's: two fields of one name, at their row; the table left out" $ do
        let broken = setCell "T42" 4 7 "Sync Supported" ir
        findings broken `shouldBe`
          [ "C-GRID-NAME core-2.01.pdf#page=70&T42: the fields \"Sync Supported\" (byte 4 bit 1) and \"Sync Supported\" (byte 4 bit 0) are named alike: tPerFeatureSyncSupported" ]
        newlyExcluded ir broken `shouldReturn` [(TableId 42, "C-GRID-NAME failed")]

    describe "a descriptor without pins (W-PINS-NONE)" $ do
      let unpinned = editWords "3.3.6.4" (fmap (const ["No", "pins", "here."])) ir
      it "is a Warning, not an error, by the criterion byteGrid's grids share: its code is Table 40's, its length its bytes after the header" $ do
        findings unpinned `shouldBe`
          [ "W-PINS-NONE core-2.01.pdf#page=69&T42: TPer Feature Descriptor: no sentence of 3.3.6.4 or its subsections pins one of its fields,"
            <> " while 3.3.6.2.2, 3.3.6.2.3, 3.3.6.5 pin fields of the grids beside it"
          , "N-FEATURE-LENGTH core-2.01.pdf#page=69&T42: TPer Feature Descriptor: no pin gives its Length: the layout pins it to 12, its 16 bytes less the 4-byte header" ]
        newlyExcluded ir unpinned `shouldReturn` []
      text <- runIO (moduleOf unpinned)
      it "leaves its version to the record, numbers typed by their widths, and pins its length to its bytes after the header" $ do
        T.lines text `shouldContain` ["  featureCode _    = 0x0001", "  featureVersion _ = Nothing", "  featureLength _  = Just 0x0c"]
        map squeezed (T.lines text) `shouldContain` ["{ tPerFeatureVersion :: !Word8 -- ^ byte 2 bits 7-4"]
        T.lines text `shouldContain` ["      , spanning \"Length\" (3, 7) (3, 0) (Pinned 0x0c)"]
        T.lines text `shouldNotContain` ["    <*> numberField \"Length\" vs"]
      it "type-checks under -Wall -Werror" $
        compiles "Hsed.Gen.Core.Features" text

    describe "a section whose words the IR lacks (C-PINS-WORDS)" $
      it "is an Error, the IR breaking its own contract, and the table is left out" $ do
        let wordless = editWords "3.3.6.4" (const Nothing) ir
        findings wordless `shouldBe`
          [ "C-PINS-WORDS core-2.01.pdf#page=69&T42: TPer Feature Descriptor: its section's pins cannot be read:"
            <> " section 3.3.6.4 has a paragraph without its words in the IR (wordComplete)" ]
        newlyExcluded ir wordless `shouldReturn` [(TableId 42, "C-PINS-WORDS failed")]

    describe "the bits fallback" $
      it "reads Table 43's byte 4 by its bits modifier when its drawn extents are gone, as the extent rule does" $ do
        let line = "core-2.01 | T43 | bits 4 [(0, \"Locking Supported\"), (1, \"Locking Enabled\"), (2, \"Locked\"), (3, \"Media Encryption\"), (4, \"MBR Enabled\"), (5, \"MBR Done\")]\n"
        bare <- annotated (withoutExtents "T43" "4" pristine) (hsa <> line)
        findings bare `shouldBe`
          [ "N-LAYOUT-BITS core-2.01.pdf#page=71&T43: byte 4's bits read from its bits modifier, the extent rule reading none: 0 Locking Supported, 1 Locking Enabled, 2 Locked, 3 Media Encryption, 4 MBR Enabled, 5 MBR Done" ]
        [ frLayout fr | fr <- featureReads (mkContext Scott bare), btAnchor (frBound fr) == TableId 43 ] `shouldBe` [frLayout (readOf 43)]

    describe "the layout modifier" $
      it "names the record as written, as byteGrid's does; the layout keeps the caption's title, and the template is still the caption's" $ do
        renamed <- annotated pristine (hsa <> "core-2.01 | T42 | layout \"TPer Descriptor\"\n")
        lns <- T.lines <$> moduleOf renamed
        lns `shouldContain` ["tPerDescriptorLayout :: Layout", "tPerDescriptorLayout = Layout", "  { layoutName   = \"TPer Feature Descriptor\""]
        lns `shouldContain` ["data TPerDescriptor = TPerDescriptor"]
        map squeezed lns `shouldContain` ["{ tPerDescriptorComIDMgmtSupported :: !Bool -- ^ byte 4 bit 6"]
        findings renamed `shouldBe` []

    describe "the generated module" $ do
      text <- runIO (moduleOf ir)
      let lns = T.lines text
      it "declares each descriptor's layout, its header pinned at its fields' widths (0x0001 for the 16-bit Feature Code)" $ do
        lns `shouldContain` ["tPerFeatureLayout :: Layout"]
        lns `shouldContain` ["      [ spanning \"Feature Code\" (0, 7) (1, 0) (Pinned 0x0001)", "      , spanning \"Version\" (2, 7) (2, 4) (Pinned 0x1)"]
        lns `shouldContain` ["      , spanning \"Reserved\" (4, 7) (4, 6) Reserved"]
      it "declares a record of its flags over the layout and its instances, its constants at their fields' widths as the pins, citing where they come from" $ do
        lns `shouldContain` ["data LockingFeature = LockingFeature"]
        lns `shouldContain` ["instance LayoutRecord LockingFeature where", "  recordLayout _ = lockingFeatureLayout"]
        lns `shouldContain` ["-- the header of TPerFeature as pinned by 3.3.6.4 (PDF 70): Feature Code = 0x0001, Version = 0x01, Length = 0x0C"]
        lns `shouldContain` ["-- the code Table 40 row 3 (PDF 69) gives 3.3.6.5: 0002h"]
        lns `shouldContain` [ "instance Feature TPerFeature where", "  featureCode _    = 0x0001", "  featureVersion _ = Just 0x1", "  featureLength _  = Just 0x0c"
                            , "  featureHeader _  = FeatureHeader 4 \"Length\"" ]
      it "type-checks under -Wall -Werror" $
        compiles "Hsed.Gen.Core.Features" text

    describe "Opal's descriptors read over Core's Context, the base (M3, hsedc --base)" $ do
      let opalPath = "../spec/ir/opal-2.30.json"
      present <- runIO (doesFileExist opalPath)
      if not present then it "needs the committed Opal IR" (pendingWith (opalPath <> " is not checked out")) else do
        opalPristine <- runIO (readIR opalPath >>= either throwString pure)
        opal <- runIO (annotated opalPristine "opal-2.30 | doc | doc opal230 & rule feature\n")
        let base = mkContext Scott ir
            octx = mkContextWith registry (Just base) Scott opal
            oreads = featureReads octx
            oread n = case [ fr | fr <- oreads, btAnchor (frBound fr) == TableId n ] of
              fr : _ -> fr
              []     -> error ("no feature read of Opal T" <> show n)
            pinnedOf n = [ (fieldName f, v) | f <- layoutFields (frLayout (oread n)), Pinned v <- [fieldKind f] ]
        it "binds Tables 3-6 and 8, the feature descriptors, by the doc anchor's rule token" $
          [ n | (TableId n, RuleBound (RuleName "feature") _ _) <- Map.toList (classify opal) ] `shouldBe` [3, 4, 5, 6, 8]
        it "finds no general-format sentence in Opal and reads Core's, Table 41 through Core's byteGrid binding, in the base (PDF 69)" $ do
          [ fmap (\tm -> (tmSpec tm, tmNumber tm, tmSize tm)) (frTemplate fr) | fr <- oreads ] `shouldBe` replicate 5 (Right (SpecId "core-2.01", 41, 4))
          frTemplate (oread 3) `shouldBe` frTemplate (readOf 42)
          fmap (const ()) (frTemplate (readFeature (mkContext Scott opal) (frBound (oread 3)))) `shouldSatisfy` isLeft
        it "holds each header to the template by its bits: Table 6's version by a name holding Version, its SSC minor over bits the template reserves (PDF 23)" $ do
          frHeader (oread 3) `shouldBe` Right (Header "Feature Code" "Version" "Length" [])
          frHeader (oread 6) `shouldBe` Right (Header "Feature Code" "Feature Descriptor Version Number" "Length" ["SSC Minor Version Number"])
        it "takes each code from its pin, Table 8's from its cell (PDF 25), and finds 0x0001 and 0x0002 listed by value in Core's Table 40 (PDF 69)" $ do
          [ (nValue <$> frCode fr, nValue <$> frCellCode fr) | fr <- oreads ]
            `shouldBe` [(Just 1, Just 1), (Just 2, Just 2), (Just 3, Just 3), (Just 0x203, Just 0x203), (Just 0x404, Just 0x404)]
          [ map (\(_, r, n) -> (rrIndex r, nValue n)) (frListedBase fr) | fr <- oreads ] `shouldBe` [[(1, 1)], [(2, 2)], [], [], []]
          map (length . frListed) oreads `shouldBe` replicate 5 0
        it "pins each layout as the bullets do, and Table 8's length to its bytes after the header, which no bullet gives" $ do
          map pinnedOf [3, 4, 5, 6, 8] `shouldBe`
            [ [("Feature Code", 1), ("Length", 12), ("Streaming Supported", 1), ("Sync Supported", 1)]
            , [("Feature Code", 2), ("Length", 12), ("MBR Shadowing Not Supported", 0), ("Media Encryption", 1), ("Locking Supported", 1)]
            , [("Feature Code", 3), ("Version", 1), ("Length", 0x1c)]
            , [("Feature Code", 0x203), ("Length", 0x10)]
            , [("Feature Code", 0x404), ("Length", 0x20)] ]
          map frLength oreads `shouldBe` [Nothing, Nothing, Nothing, Nothing, Just 32]
        it "matches Table 6's admin and user counts by word prefix (PDF 23), Table 3's ACK/NACK by its reviewed spelling (PDF 20)" $ do
          [ (n, pinField p) | (n, p) <- frMatched (oread 6), "Number of Locking" `T.isPrefixOf` n ] `shouldBe`
            [ ("Number of Locking SP Admin Authorities Supported", "Number of Locking SP Admin Authorities")
            , ("Number of Locking SP User Authorities Supported", "Number of Locking SP User Authorities") ]
          [ (n, pinField p) | (n, p) <- frMatched (oread 3), "ACK" `T.isPrefixOf` n ] `shouldBe` [("ACK/NAK Supported", "ACK/NACK Supported")]
          map (map pinField . frUnmatched) oreads `shouldBe` [[], [], [], [], []]
        it "lists what the bullets allow each field they give no exact value: VU, a version, a minimum, the text as printed" $ do
          map frAllowances [oread 3, oread 5, oread 8] `shouldBe`
            [ [ ("Version", AnyVersion 1), ("ComID Mgmt Supported", VendorUnique), ("Buffer Mgmt Supported", VendorUnique)
              , ("ACK/NAK Supported", VendorUnique), ("Async Supported", VendorUnique) ]
            , [], [] ]
          frAllowances (oread 4) `shouldBe`
            [ ("Version", AnyVersion 3), ("HW Reset for LOR/DOR Supported", VendorUnique), ("MBR Done", AsPrinted "**"), ("MBR Enabled", AsPrinted "**")
            , ("Locked", AsPrinted "**"), ("Locking Enabled", AsPrinted "See section 3.1.1.3.1") ]
          frAllowances (oread 6) `shouldBe`
            [ ("Feature Descriptor Version Number", AnyVersion 2), ("SSC Minor Version Number", AsPrinted "As specified in Table 7")
            , ("Base ComID", VendorUnique), ("Number of ComIDs", Minimum 1), ("Range Crossing Behavior", VendorUnique)
            , ("Number of Locking SP Admin Authorities Supported", Minimum 4), ("Number of Locking SP User Authorities Supported", Minimum 8)
            , ("Initial C_PIN_SID PIN Indicator", VendorUnique), ("Behavior of C_PIN_SID PIN upon TPer Revert", VendorUnique) ]
        it "names the records after the captions' titles less their last word" $
          map (typeName . frStructure) oreads `shouldBe`
            [ "Level0DiscoveryTPerFeature", "Level0DiscoveryLockingFeature", "Level0DiscoveryGeometryReportingFeature"
            , "Level0DiscoveryOpalSSCV2Feature", "Level0DiscoverySupportedDataRemovalMechanismFeature" ]
        it "finds no Error: three codes Core's Table 40 gives only within a range, Table 8 without pins, and two Notes" $
          [ renderFinding f | f <- checkIR (Just base) Scott opal, Cm.code f `elem` featureCodes ] `shouldBe`
            [ "W-FEATURE-UNLISTED opal-2.30.pdf#page=21&T5: Level 0 Discovery - Geometry Reporting Feature Descriptor: no code table lists 3.1.1.4.1,"
              <> " nor does one of core-2.01 list its code 0x0003 alone (Table 40 row 4 (PDF 69) gives it only within 0003h – 00FFh, Reserved)"
            , "W-FEATURE-UNLISTED opal-2.30.pdf#page=23&T6: Level 0 Discovery - Opal SSC V2 Feature Descriptor: no code table lists 3.1.1.5,"
              <> " nor does one of core-2.01 list its code 0x0203 alone (Table 40 row 5 (PDF 69) gives it only within 0100h – 03FFh, SSCs)"
            , "W-FEATURE-UNLISTED opal-2.30.pdf#page=25&T8: Level 0 Discovery – Supported Data Removal Mechanism Feature Descriptor: no code table lists 3.1.1.6,"
              <> " nor does one of core-2.01 list its code 0x0404 alone (Table 40 row 6 (PDF 69) gives it only within 0400h - BFFFh, Reserved)"
            , "W-PINS-NONE opal-2.30.pdf#page=25&T8: Level 0 Discovery – Supported Data Removal Mechanism Feature Descriptor: no sentence of 3.1.1.6 or its"
              <> " subsections pins one of its fields, while 3.1.1.1, 3.1.1.2, 3.1.1.3, 3.1.1.4.1, 3.1.1.5 pin fields of the grids beside it"
            , "N-FEATURE-HEADER opal-2.30.pdf#page=23&T6: Level 0 Discovery - Opal SSC V2 Feature Descriptor: its SSC Minor Version Number lies over bits"
              <> " core-2.01 Table 41 reserves"
            , "N-FEATURE-LENGTH opal-2.30.pdf#page=25&T8: Level 0 Discovery – Supported Data Removal Mechanism Feature Descriptor: no pin gives its Length:"
              <> " the layout pins it to 32, its 36 bytes less the 4-byte header" ]
        cr <- runIO (either (throwString . show) pure (compileIR (Just base) Scott opal))
        let text = fromMaybe "" (lookup "Hsed.Gen.Opal.Features" (crModules cr))
            lns = T.lines text
            prov = Gen.Provenance (SpecId "opal-2.30") "T1" "Table 1" 1 1 1 "ord" "0/1" ""
            importing xs = (Gen.decl prov "x" []) { Gen.declImports = [Gen.ImportSome "M" xs] }
            imported = L.nub [ T.takeWhileEnd C.isAlphaNum upTo | l <- lns, "import" `T.isPrefixOf` l, (upTo, _) <- DT.breakOnAll " (..)" l ]
        it "compiles to Hsed.Gen.Opal.Features alone: five records and their Feature instances, Opal's allowances among them" $ do
          map fst (crModules cr) `shouldBe` ["Hsed.Gen.Opal.Features"]
          lns `shouldContain` [ "instance Feature Level0DiscoveryOpalSSCV2Feature where", "  featureCode _    = 0x0203", "  featureVersion _ = Nothing"
                              , "  featureLength _  = Just 0x10", "  featureHeader _  = FeatureHeader 4 \"Length\"", "  featureAllowances _ ="
                              , "    [ (\"Feature Descriptor Version Number\", AnyVersion 0x2)" ]
          lns `shouldContain` ["    , (\"Number of Locking SP Admin Authorities Supported\", Minimum 0x0004)"]
          lns `shouldContain` ["  featureCode _    = 0x0404", "  featureVersion _ = Nothing", "  featureLength _  = Just 0x20"]
          lns `shouldContain` ["-- its general format core-2.01 Table 41 (PDF 69): 4 bytes, Length the count of the bytes after them"]
          lns `shouldContain` [ "import           Hsed.Core.Feature (Allowance (..), Feature (featureCode, featureVersion, featureLength, featureHeader,"
                                <> " featureAllowances), FeatureHeader (..))" ]
        it "imports with (..) only types whose members declScope knows, Allowance's from the runtime's Data" $ do
          imported `shouldContain` ["Allowance"]
          declScope [importing ["Allowance (..)"]] `shouldBe` ["Allowance", "VendorUnique", "AnyVersion", "Minimum", "AsPrinted"]
          [ t | t <- imported, length (declScope [importing [t <> " (..)"]]) < 2 ] `shouldBe` []
        it "type-checks under -Wall -Werror" $
          compiles "Hsed.Gen.Opal.Features" text

  describe "Opal Table 3 (for M3)" $
    it "matches the bullets of 3.1.1.2 (PDF 20) to the grid's fields: Streaming and Sync pinned to 1, the others VU, ACK/NACK the grid's ACK/NAK" $ do
      let path = "../spec/ir/opal-2.30.json"
      present <- doesFileExist path
      if not present then pendingWith (path <> " is not checked out") else do
        opal <- readIR path >>= either throwString pure
        let ctx = mkContext Scott opal
            t = fromMaybe (error "T3") (Map.lookup (TableId 3) (numberedTables opal))
            fr = readFeature ctx (BoundTable (TableId 3) t t (SomeBinding feature) (modsOf (SomeBinding feature)))
        layoutName (frLayout fr) `shouldBe` "Level 0 Discovery - TPer Feature Descriptor"
        stName (frStructure fr) `shouldBe` "Level 0 Discovery - TPer Feature"
        [ (fieldName f, fieldKind f) | f <- layoutFields (frLayout fr), fieldKind f /= Reserved ] `shouldBe`
          [ ("Feature Code", Pinned 1), ("Version", Unsigned), ("Length", Pinned 12), ("ComID Mgmt Supported", Flag)
          , ("Streaming Supported", Pinned 1), ("Buffer Mgmt Supported", Flag), ("ACK/NAK Supported", Flag)
          , ("Async Supported", Flag), ("Sync Supported", Pinned 1) ]
        [ (pinField p, pinValue p) | p <- frUnmatched fr ] `shouldBe` []
        [ (n, pinValue p) | (n, p) <- frMatched fr, n == "ACK/NAK Supported" ] `shouldBe` [("ACK/NAK Supported", PinVU)]
        map rfName (recordFieldsOf (frStructure fr)) `shouldBe` ["Version", "ComID Mgmt Supported", "Buffer Mgmt Supported", "ACK/NAK Supported", "Async Supported"]
        -- Opal prints no sentence naming the general format: read alone (no base), it has none (Core's Table 41 over the base above)
        frTemplate fr `shouldSatisfy` isLeft
