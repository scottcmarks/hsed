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
byte 4 of Table 43 read by a @bits@ modifier when its drawn extents are gone.
The generated module, and an edited one type-checked under @-Wall -Werror@.
Opal Table 3's bullets matched to its grid's fields, for M3. The values the
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

import           Hsed.Core.Layout        (Field (..), FieldKind (..), Layout (..))
import           Hsed.Spec.Anchor        (AnchorId (..))
import           Hsed.Spec.Check         (checkIR, mkContext)
import           Hsed.Spec.Common        (ErrataSet (..), RuleName (..), SpecId (..), renderFinding)
import qualified Hsed.Spec.Common        as Cm
import           Hsed.Spec.Compile       (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL           (SomeBinding (..), feature)
import           Hsed.Spec.IR            (CellIR (..), ParaIR (..), SpecIR (..), TableIR (..), XrefIR (..), readIR)
import           Hsed.Spec.Readers.Grid  (GridField (..))
import           Hsed.Spec.Readers.Pins  (Pin (..), PinValue (..))
import           Hsed.Spec.Rules.Codes   (Number (..))
import           Hsed.Spec.RuleDef       (BoundTable (..), Classification (..), RuleDef (..), modsOf, numberedTables)
import           Hsed.Spec.Rules         (classify, explain)
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
  <> ["C-LAYOUT-EXTENT", "C-LAYOUT-ENDIAN", "N-LAYOUT-BITS", "W-LAYOUT-BITS-UNUSED", "C-LAYOUT-TILE", "W-PINS-NONE"]

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
  describe "featureTypeName" $
    it "names the record after the layout's name without its last word" $
      map featureTypeName ["TPer Feature Descriptor", "Locking Feature Descriptor", "Level 0 Discovery - TPer Feature Descriptor", "Feature"]
        `shouldBe` ["TPerFeature", "LockingFeature", "Level0DiscoveryTPerFeature", "Feature"]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx = mkContext Scott ir
        reads' = featureReads ctx
        readOf n = case [ fr | fr <- reads', btAnchor (frBound fr) == TableId n ] of
          fr : _ -> fr
          []     -> error ("no feature read of T" <> show n)
        findings ir' = [ renderFinding f | f <- checkIR Scott ir', Cm.code f `elem` featureCodes ]
        moduleOf ir' = do
          cr <- either (throwString . show) pure (compileIR Scott ir')
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
          frHeader fr `shouldBe` Right (Header "Feature Code" "Version" "Length")
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
        [ (rfIdent f, rfType f, fieldStart (rfField f)) | f <- frFields (readOf 42) ] `shouldBe`
          [ ("tPerFeatureComIDMgmtSupported", FlagField, 33), ("tPerFeatureStreamingSupported", FlagField, 35)
          , ("tPerFeatureBufferMgmtSupported", FlagField, 36), ("tPerFeatureACKNAKSupported", FlagField, 37)
          , ("tPerFeatureAsyncSupported", FlagField, 38), ("tPerFeatureSyncSupported", FlagField, 39) ]
        [ (rfName f, fieldStart (rfField f)) | f <- frFields (readOf 43) ] `shouldBe`
          [ ("MBR Done", 34), ("MBR Enabled", 35), ("Media Encryption", 36), ("Locked", 37), ("Locking Enabled", 38), ("Locking Supported", 39) ]
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
      it "C-FEATURE-FIELD: two fields of one name" $
        findings (setCell "T42" 4 7 "Sync Supported" ir) `shouldBe`
          [ "C-FEATURE-FIELD core-2.01.pdf#page=69&T42: TPer Feature Descriptor: 2 fields, \"Sync Supported\", give the record one field, tPerFeatureSyncSupported" ]

    describe "a descriptor without pins (W-PINS-NONE)" $ do
      let unpinned = editWords "3.3.6.4" (const Nothing) ir
      it "is a Warning, not an error: its code is Table 40's" $ do
        findings unpinned `shouldBe`
          [ "W-PINS-NONE core-2.01.pdf#page=69&T42: TPer Feature Descriptor: no pins read: section 3.3.6.4 has a paragraph without its words in the IR (wordComplete)" ]
        newlyExcluded ir unpinned `shouldReturn` []
      text <- runIO (moduleOf unpinned)
      it "leaves its version and length to the record, numbers" $ do
        T.lines text `shouldContain` ["  featureCode       = 0x0001", "  featureVersion    = Nothing", "  featureLength     = Nothing"]
        T.lines text `shouldContain` ["  { tPerFeatureVersion             :: Natural  -- ^ Version, byte 2 bits 7-4"]
        T.lines text `shouldContain` ["    <*> numberOf \"Length\" vs"]
      it "type-checks under -Wall -Werror" $
        compiles "Hsed.Gen.Core.Features" text

    describe "the bits fallback" $
      it "reads Table 43's byte 4 by its bits modifier when its drawn extents are gone, as the extent rule does" $ do
        let line = "core-2.01 | T43 | bits 4 [(0, \"Locking Supported\"), (1, \"Locking Enabled\"), (2, \"Locked\"), (3, \"Media Encryption\"), (4, \"MBR Enabled\"), (5, \"MBR Done\")]\n"
        bare <- annotated (withoutExtents "T43" "4" pristine) (hsa <> line)
        findings bare `shouldBe`
          [ "N-LAYOUT-BITS core-2.01.pdf#page=71&T43: byte 4's bits read from its bits modifier, the extent rule reading none: 0 Locking Supported, 1 Locking Enabled, 2 Locked, 3 Media Encryption, 4 MBR Enabled, 5 MBR Done" ]
        [ frLayout fr | fr <- featureReads (mkContext Scott bare), btAnchor (frBound fr) == TableId 43 ] `shouldBe` [frLayout (readOf 43)]

    describe "the layout modifier" $
      it "renames the layout and so the record; the template is still the caption's" $ do
        renamed <- annotated pristine (hsa <> "core-2.01 | T42 | layout \"TPer Descriptor\"\n")
        lns <- T.lines <$> moduleOf renamed
        lns `shouldContain` ["tPerDescriptor :: Layout", "tPerDescriptor = Layout", "  { layoutName   = \"TPer Descriptor\""]
        lns `shouldContain` ["data TPer = TPer", "  { tPerComIDMgmtSupported  :: Bool  -- ^ ComID Mgmt Supported, byte 4 bit 6"]
        findings renamed `shouldBe` []

    describe "the generated module" $ do
      text <- runIO (moduleOf ir)
      let lns = T.lines text
      it "declares each descriptor's layout, its header pinned" $ do
        lns `shouldContain` ["tPerFeatureDescriptor :: Layout"]
        lns `shouldContain` ["      [ spanning \"Feature Code\" (0, 7) (1, 0) (Pinned 0x01)"]
        lns `shouldContain` ["      , spanning \"Reserved\" (4, 7) (4, 6) Reserved"]
      it "declares a record of its flags and its instance, citing where its constants come from" $ do
        lns `shouldContain` ["data LockingFeature = LockingFeature"]
        lns `shouldContain` ["-- the header of TPerFeature as pinned by 3.3.6.4 (PDF 70): Feature Code = 0x0001, Version = 0x01, Length = 0x0C"]
        lns `shouldContain` ["-- the code Table 40 row 3 (PDF 69) gives 3.3.6.5: 0002h"]
        lns `shouldContain` ["instance Feature TPerFeature where", "  featureCode       = 0x0001", "  featureVersion    = Just 0x01", "  featureLength     = Just 0x0c"]
      it "type-checks under -Wall -Werror" $
        compiles "Hsed.Gen.Core.Features" text

  describe "Opal Table 3 (for M3)" $
    it "matches the bullets of 3.1.1.2 (PDF 20) to the grid's fields: Streaming and Sync pinned to 1, the others VU, ACK/NACK naming no field" $ do
      let path = "../spec/ir/opal-2.30.json"
      present <- doesFileExist path
      if not present then pendingWith (path <> " is not checked out") else do
        opal <- readIR path >>= either throwString pure
        let ctx = mkContext Scott opal
            t = fromMaybe (error "T3") (Map.lookup (TableId 3) (numberedTables opal))
            fr = readFeature ctx (BoundTable (TableId 3) t t (SomeBinding feature) (modsOf (SomeBinding feature)))
        frName fr `shouldBe` "Level 0 Discovery - TPer Feature Descriptor"
        [ (fieldName f, fieldKind f) | f <- layoutFields (frLayout fr), fieldKind f /= Reserved ] `shouldBe`
          [ ("Feature Code", Pinned 1), ("Version", Unsigned), ("Length", Pinned 12), ("ComID Mgmt Supported", Flag)
          , ("Streaming Supported", Pinned 1), ("Buffer Mgmt Supported", Flag), ("ACK/NAK Supported", Flag)
          , ("Async Supported", Flag), ("Sync Supported", Pinned 1) ]
        [ (pinField p, pinValue p) | p <- frUnmatched fr ] `shouldBe` [("ACK/NACK Supported", PinVU)]
        map rfName (frFields fr) `shouldBe` ["Version", "ComID Mgmt Supported", "Buffer Mgmt Supported", "ACK/NAK Supported", "Async Supported"]
        -- Opal prints no sentence naming the general format: M3 takes Core's Table 41
        frTemplate fr `shouldSatisfy` isLeft
