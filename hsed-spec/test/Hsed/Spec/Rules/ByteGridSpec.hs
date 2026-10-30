{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Spec.Rules.ByteGridSpec
Description : Test specifications for the byteGrid rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The rule on the committed Core extraction with the committed annotations
(design section 3.4, example E7): the nine byte grids it binds from the doc
anchor; their layouts' sizes, which the PDF prints (Table 17 ComPacket
Format, PDF 40, bytes 0-19 then the payload; Table 18, PDF 41-42, 0-23; Table
20, PDF 44, 0-11; Table 39, PDF 68, 0-47), each tiling (C-LAYOUT-TILE
passes); Table 39's two versions pinned by 3.3.6.2.2 and 3.3.6.2.3 (PDF 68);
the committed run's three W-PINS-NONE (Tables 38, 41 and 44, where section
3.3.6 pins other grids' fields) and no other finding of its codes;
C-LAYOUT-TILE, C-LAYOUT-ENDIAN and C-GRID-NAME made to fire by edited cells,
each at the page of its row; the records and instances the layouts module
declares, type-checked with @ghc -fno-code -Wall -Werror@ against hsed-core
(pending where ghc cannot run); and @layout \"Name\"@ renaming a grid.
-}
module Hsed.Spec.Rules.ByteGridSpec (spec) where

import qualified Data.Text                as DT
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T
import           RIO
import           RIO.Directory            (createDirectoryIfMissing, doesFileExist)
import           RIO.FilePath             (takeDirectory, (</>))
import           System.Environment       (lookupEnv)
import           System.Process           (readProcessWithExitCode)
import           Test.Hspec

import           Hsed.Core.Layout         (Field (..), FieldKind (..), Layout (..), Tail (..), tiling)
import           Hsed.Spec.Anchor         (AnchorId (..))
import           Hsed.Spec.Check          (checkIR, mkContext)
import           Hsed.Spec.Common         (ErrataSet (..), RuleName (..), SpecId (..), renderFinding)
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.Compile        (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR             (SpecIR (..), readIR)
import           Hsed.Spec.Readers.Pins   (Pin (..))
import           Hsed.Spec.RuleDef        (BoundTable (..), Classification (..), RuleDef (..), numberedTables)
import           Hsed.Spec.Rules          (classify, explain)
import           Hsed.Spec.Rules.ByteGrid
import           Hsed.Spec.Shape          (measure)


-- | The tables a rule fires on anywhere in an IR, by number, whatever its scope.
firesOn :: RuleDef -> SpecIR -> [Int]
firesOn r ir = [ n | (TableId n, t) <- Map.toList (numberedTables ir), fires r sid t (measure sid t) ]
  where sid = SpecId ir.spec

-- | The nine grids of Core the rule binds (design 3.4).
grids :: [Int]
grids = [17, 18, 20, 21, 38, 39, 41, 44, 212]

-- | @ghc -fno-code -Wall -Werror@ on a printed module against hsed-core (as GenSpec does).
compiles :: Text -> Text -> Expectation
compiles modName text = withSystemTempDirectory "hsed-bytegrid" $ \dir -> do
  let file = dir </> T.unpack (DT.replace "." "/" modName) <> ".hs"
      ghcArgs = ["-fno-code", "-Wall", "-Werror", "-package", "hsed-core", "-package", "bytestring", "-i" <> dir, file]
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
  describe "structureName" $
    it "drops a trailing Format, which names the table as a format of the structure" $
      map structureName ["ComPacket Format", "Level 0 Discovery Header Format", "Common SSC Information", "Format"]
        `shouldBe` ["ComPacket", "Level 0 Discovery Header", "Common SSC Information", "Format"]
  describe "fieldType" $
    it "types a field by its kind and width" $
      map fieldType [ Field "a" 0 1 Flag, Field "b" 0 4 Unsigned, Field "c" 0 16 Unsigned, Field "d" 0 24 Unsigned
                    , Field "e" 0 36 Unsigned, Field "f" 0 256 Bytes, Field "g" 0 72 Unsigned ]
        `shouldBe` ["Bool", "Word8", "Word16", "Word32", "Word64", "ByteString", "Natural"]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx = mkContext Scott ir
        readOf n = fromMaybe (error ("no grid " <> show n)) (L.find ((== TableId n) . btAnchor . grBound) (gridReads ctx))
        ruleCodes' = ["C-LAYOUT-TILE", "C-LAYOUT-ENDIAN", "C-LAYOUT-EXTENT", "C-GRID-NAME", "W-PINS-NONE", "N-LAYOUT-BITS", "W-LAYOUT-BITS-UNUSED"]
        -- the findings of the rule's codes at its tables (feature and byteFieldValue give some of the codes too)
        ours ir' = [ renderFinding f | f <- checkIR Scott ir', Cm.code f `elem` ruleCodes'
                   , Cm.anchor f `elem` map (Just . ("T" <>) . tshow) grids ]
        -- what an edit adds to the committed run's findings
        added ir' = filter (`notElem` ours ir) (ours ir')
        layoutsOf ir' = do
          cr <- either (throwString . show) pure (compileIR Scott ir')
          pure (fromMaybe "" (lookup "Hsed.Gen.Core.Layouts" (crModules cr)))

    describe "the byteGrid rule" $ do
      it "binds the nine byte grids, and no other table, by the doc anchor's rule token" $ do
        [ n | (TableId n, RuleBound (RuleName "byteGrid") _ _) <- Map.toList (classify ir) ] `shouldBe` grids
        explain ir (TableId 17) `shouldBe` "byteGrid (exemplar core T17)"
      it "fires on no other table of Core, and on Opal's Table 2 alone" $ do
        firesOn byteGridRule ir `shouldBe` grids
        let path = "../spec/ir/opal-2.30.json"
        present <- doesFileExist path
        if not present then pendingWith (path <> " is not checked out") else do
          opal <- readIR path >>= either throwString pure
          firesOn byteGridRule opal `shouldBe` [2]

    describe "the layouts" $ do
      it "C-LAYOUT-TILE passes: T17 is 20 bytes, T18 24, T20 12, T39 48, and every grid it binds tiles" $ do
        [ (n, layoutSize (grLayout (readOf n))) | n <- grids ] `shouldBe`
          [(17, 20), (18, 24), (20, 12), (21, 16), (38, 48), (39, 48), (41, 4), (44, 16), (212, 12)]
        [ (n, tiling (grLayout (readOf n))) | n <- grids ] `shouldBe` [ (n, []) | n <- grids ]
      it "gives T17, T18 and T20 their payload, its length the Length field, T20's padded to four bytes" $
        [ layoutTail (grLayout (readOf n)) | n <- [17, 18, 20] ] `shouldBe`
          [ Just (Tail "Payload" (Just "Length") 1), Just (Tail "Payload" (Just "Length") 1), Just (Tail "Payload" (Just "Length") 4) ]
      it "pins T39's versions by 3.3.6.2.2 and 3.3.6.2.3 (PDF 68), and pins nothing of T38, which shares the section" $ do
        let t39 = readOf 39
        [ (fieldName f, fieldKind f) | f <- layoutFields (grLayout t39) ] `shouldBe`
          [ ("Length of Parameter Data", Unsigned), ("Data Structure Major Version", Pinned 0), ("Data Structure Minor Version", Pinned 1)
          , ("Reserved", Reserved), ("Vendor Unique", Bytes) ]
        [ (n, pinSection p, pinPage p + 1) | (n, p) <- grPinned t39 ] `shouldBe`
          [("Data Structure Major Version", "3.3.6.2.2", 68), ("Data Structure Minor Version", "3.3.6.2.3", 68)]
        (length <$> grPins (readOf 38), grPinned (readOf 38)) `shouldBe` (Right 2, [])

    describe "the records" $ do
      let fieldsOf n = [ (rfIdent f, rfType f) | f <- recordFieldsOf (readOf n) ]
      it "T17: the header's fields by width, the reserved bytes and Length (the payload's) left to the layout" $
        fieldsOf 17 `shouldBe`
          [ ("comPacketComID", "Word16"), ("comPacketComIDExtension", "Word16"), ("comPacketOutstandingData", "Word32")
          , ("comPacketMinTransfer", "Word32") ]
      it "T18 and T20: the packet's 8-byte Session, the subpacket's Kind" $ do
        fieldsOf 18 `shouldBe` [ ("packetSession", "Word64"), ("packetSeqNumber", "Word32"), ("packetAckType", "Word16"), ("packetAcknowledgement", "Word32") ]
        fieldsOf 20 `shouldBe` [("dataSubPacketKind", "Word16")]
      it "T39: the pinned versions left to the layout, Vendor Unique bytes; T41: a sub-byte Version; T44: no field" $ do
        fieldsOf 39 `shouldBe` [("level0DiscoveryHeaderLengthOfParameterData", "Word32"), ("level0DiscoveryHeaderVendorUnique", "ByteString")]
        fieldsOf 41 `shouldBe` [ ("featureDescriptorTemplateFeatureCode", "Word16"), ("featureDescriptorTemplateVersion", "Word8")
                               , ("featureDescriptorTemplateLength", "Word8") ]
        fieldsOf 44 `shouldBe` []
      it "T212: the constants pinned, the two identifiers that start and end inside bytes" $
        fieldsOf 212 `shouldBe` [ ("gudidColumnContentsDescriptionIEEECOMPANYID", "Word32")
                                , ("gudidColumnContentsDescriptionVENDORSPECIFICIDENTIFIER", "Word64") ]

    describe "the findings" $ do
      it "on the committed run: W-PINS-NONE for T38, T41 and T44, where 3.3.6 pins other grids' fields, and nothing else" $
        ours ir `shouldBe`
          [ "W-PINS-NONE core-2.01.pdf#page=67&T38: Level 0 Discovery Response Data Format: the pins of 3.3.6.2"
            <> " (Data Structure Major Version, Data Structure Minor Version) name none of its fields"
          , "W-PINS-NONE core-2.01.pdf#page=69&T41: Feature Descriptor Template Format: no sentence of 3.3.6.3 or its subsections"
            <> " pins one of its fields, while 3.3.6.2.2, 3.3.6.2.3, 3.3.6.4, 3.3.6.5 pin fields of the grids beside it"
          , "W-PINS-NONE core-2.01.pdf#page=71&T44: Common SSC Information: no sentence of 3.3.6.6 or its subsections"
            <> " pins one of its fields, while 3.3.6.2.2, 3.3.6.2.3, 3.3.6.4, 3.3.6.5 pin fields of the grids beside it" ]
      it "C-LAYOUT-TILE: an overlap at the page of its row (T18 spans PDF 41-42), and the table left out" $ do
        let broken = setCell "T18" 13 0 "14" ir
        added broken `shouldBe`
          ["C-LAYOUT-TILE core-2.01.pdf#page=42&T18: Packet Format: \"AckType\" starts at byte 14 bit 7, inside \"Reserved\""]
        newlyExcluded ir broken `shouldReturn` [(TableId 18, "C-LAYOUT-TILE failed")]
      it "C-LAYOUT-TILE: a payload not right after the fixed part, a gap no row prints (at the table)" $ do
        added (setCell "T17" 22 0 "If Length > 0, 21 to n + 20" ir) `shouldBe`
          ["C-LAYOUT-TILE core-2.01.pdf#page=40&T17: ComPacket Format: \"Payload\" starts at byte 21, the fixed part ends at byte 19"]
        added (setCell "T39" 10 0 "13" ir) `shouldBe`
          ["C-LAYOUT-TILE core-2.01.pdf#page=68&T39: Level 0 Discovery Header Format: no field covers byte 14 bit 7 to byte 15 bit 0"]
      it "C-LAYOUT-ENDIAN: an (MSB) whose (LSB) is gone, at the page of its row" $
        added (setCell "T18" 13 2 "" ir) `shouldContain`
          ["C-LAYOUT-ENDIAN core-2.01.pdf#page=42&T18: byte 14: an (MSB) inside the run of \"Reserved\""]
      it "C-GRID-NAME: a field with no name, two fields named alike" $ do
        added (setCell "T21" 12 2 "" ir) `shouldBe`
          ["C-GRID-NAME core-2.01.pdf#page=45&T21: bytes 12 to 15: the field \"\" has no name that makes an identifier"]
        let broken = setCell "T21" 12 2 "Kind" ir
        added broken `shouldBe`
          [ "C-GRID-NAME core-2.01.pdf#page=45&T21: the fields \"Kind\" (bytes 6 to 7) and \"Kind\" (bytes 12 to 15)"
            <> " are named alike: creditControlSubpacketKind" ]
        newlyExcluded ir broken `shouldReturn` [(TableId 21, "C-GRID-NAME failed")]

    describe "the generated module" $ do
      text <- runIO (layoutsOf ir)
      let lns = T.lines text
      it "declares each grid's layout, its record(s) and its instance" $ do
        [ T.takeWhile (/= ' ') l | l <- lns, " :: Layout" `T.isSuffixOf` l ] `shouldBe`
          [ "comPacketLayout", "packetLayout", "dataSubPacketLayout", "creditControlSubpacketLayout", "level0DiscoveryResponseDataLayout"
          , "level0DiscoveryHeaderLayout", "featureDescriptorTemplateLayout", "commonSSCInformationLayout", "gudidColumnContentsDescriptionLayout" ]
        [ l | l <- lns, "data " `T.isPrefixOf` l ] `shouldBe`
          [ "data ComPacketHeader = ComPacketHeader", "data ComPacket = ComPacket", "data PacketHeader = PacketHeader", "data Packet = Packet"
          , "data DataSubPacketHeader = DataSubPacketHeader", "data DataSubPacket = DataSubPacket"
          , "data CreditControlSubpacket = CreditControlSubpacket"
          , "data Level0DiscoveryResponseDataHeader = Level0DiscoveryResponseDataHeader", "data Level0DiscoveryResponseData = Level0DiscoveryResponseData"
          , "data Level0DiscoveryHeader = Level0DiscoveryHeader", "data FeatureDescriptorTemplateHeader = FeatureDescriptorTemplateHeader"
          , "data FeatureDescriptorTemplate = FeatureDescriptorTemplate", "data CommonSSCInformation = CommonSSCInformation"
          , "data GUDIDColumnContentsDescription = GUDIDColumnContentsDescription" ]
        lns `shouldContain` [ "instance LayoutRecord ComPacket where", "  recordLayout _ = comPacketLayout", "  recordFields r =" ]
        lns `shouldContain` [ "  { comPacketHeader  :: !ComPacketHeader", "  , comPacketPayload :: !ByteString       -- ^ from byte 20, Length bytes" ]
        lns `shouldContain` [ "  , layoutTail   = Just (Tail \"Payload\" (Just \"Length\") 4)" ]
        lns `shouldContain` [ "      , spanning \"Data Structure Major Version\" (4, 7) (5, 0) (Pinned 0x00)" ]
      it "stays ASCII" $
        T.filter (> '\DEL') text `shouldBe` ""
      it "type-checks under -Wall -Werror" $
        compiles "Hsed.Gen.Core.Layouts" text

    describe "layout \"Name\"" $
      it "names a grid's layout and record where the caption will not do" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T212 | layout \"GUDID\"\n")
        lns <- T.lines <$> layoutsOf ir'
        [ l | l <- lns, "gudid" `T.isInfixOf` T.toLower l, "data " `T.isPrefixOf` l || " :: Layout" `T.isSuffixOf` l ]
          `shouldBe` ["gudidLayout :: Layout", "data GUDID = GUDID"]
