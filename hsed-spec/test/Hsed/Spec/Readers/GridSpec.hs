{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Readers.GridSpec
Description : Test specifications for Hsed.Spec.Readers.Grid
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The byte grids of the committed Core extraction read as the PDF prints them
(design examples E7 and E8): T17 and T18 (multi-byte fields from (MSB) to
(LSB), a variable payload after a length field), T20 (its payload padded to
four bytes), T39 (a run over elided rows), T42 (byte 4 eight cells by
position, byte 2 two cells by their drawn extents, 4 + 4), T43 (byte 4 seven
cells, the first, Reserved, over bits 7-6), T44 (one range), T212 (fields that
start and end inside bytes, constants); every Core and Opal grid reads without
a problem. The bit maps of T42 and T43 byte 4 equal sedutil's
(@Common/DtaStructures.h@ lines 81-134, @Discovery0TPerFeatures@ and
@Discovery0LockingFeatures@, which list each flag from bit 0 up). The extent
rule's fallbacks are tested on T43 with its extents taken away: a finding, or
the @bits@ modifier, which is held to the row it reads (each name a printed
cell, which names the field; its bits descending left to right, each a bit
of a byte listed once; a printed Reserved cell over a bit it leaves). A
field's parenthesis that names its parts (T18's Session), and the fixed
part's size running to the last byte a row prints.
-}
module Hsed.Spec.Readers.GridSpec (spec) where

import qualified RIO.List               as L
import qualified RIO.Map                as Map
import qualified RIO.Text               as T
import           RIO
import           RIO.Directory          (doesFileExist)
import           Test.Hspec

import           Hsed.Core.Layout       (Field (..), Layout (..), TileProblem (..), tiling)
import           Hsed.Spec.Anchor       (AnchorId (..))
import           Hsed.Spec.Check        (mkContext)
import           Hsed.Spec.Common       (ErrataSet (..))
import qualified Hsed.Spec.Common       as Cm
import           Hsed.Spec.CoreIR       (withCore)
import           Hsed.Spec.DSL          (SomeBinding (..), bits, feature)
import           Hsed.Spec.IR           (CellIR (..), SpecIR (..), TableIR (..), readIR)
import           Hsed.Spec.Readers.Grid
import           Hsed.Spec.Readers.Layout (gridLayout)
import           Hsed.Spec.RuleDef      (BoundTable (..), modsOf, numberedTables)
import           Hsed.Spec.Shape        (isGrid8)


-- | A numbered table of an IR.
table :: SpecIR -> Int -> TableIR
table ir n = fromMaybe (error ("no table " <> show n)) (Map.lookup (TableId n) (numberedTables ir))

-- | The fields as (name as printed, first byte and bit, last byte and bit).
spans :: Grid -> [(Text, (Int, Int), Maybe (Int, Int))]
spans g = [ (gfName f, pos (gfFirst f), pos <$> gfLast f) | f <- gFields g ]
  where pos (BitPos b i) = (b, i)

-- | The one-bit fields of a byte, as (bit, name), bit 0 first, as sedutil lists them.
flagsOf :: Int -> Grid -> [(Int, Text)]
flagsOf byte g = L.sortOn fst [ (bpBit (gfFirst f), gfName f) | f <- gFields g, bpByte (gfFirst f) == byte
                            , gfLast f == Just (gfFirst f), not ("Reserved" `T.isPrefixOf` gfName f) ]

-- | The table with no drawn extents in the row of a byte, as an extraction
--   before M2c had it.
withoutExtents :: Text -> TableIR -> TableIR
withoutExtents label t = t { rows = map strip t.rows }
  where
    strip r@(c : _) | c.text == label = map (\x -> x { xExtent = Nothing }) r
    strip r                           = r

-- | The row of a table whose first cell reads so, its cells' text.
rowTexts :: TableIR -> Text -> [Text]
rowTexts t label = concat (take 1 [ map (.text) r | r@(c : _) <- t.rows, c.text == label ])

spec :: Spec
spec = describe "Hsed.Spec.Readers.Grid" $ do
  describe "parseByteLabel" $
    it "reads one byte, a range, a run to an end the text gives, and the rows a run elides" $
      map parseByteLabel ["4", "5 - 15", "0 – 47", "8 to 9", "48 – n", "If Length > 0, 20 to n + 19", "…", "Byte", ""]
        `shouldBe` [ Just (OneByte 4), Just (ByteRange 5 15), Just (ByteRange 0 47), Just (ByteRange 8 9), Just (FromByte 48 "48 – n")
                   , Just (FromByte 20 "If Length > 0, 20 to n + 19"), Just Elided, Nothing, Nothing ]
  describe "cellMarks" $
    it "takes the (MSB) and (LSB) marks off a cell, alone or beside a name" $
      map cellMarks ["(MSB)", "(LSB)", "(MSB) IEEE COMPANY ID", "IEEE COMPANY ID (LSB)", "Length (n)"]
        `shouldBe` [(True, "", False), (False, "", True), (True, "IEEE COMPANY ID", False), (False, "IEEE COMPANY ID", True), (False, "Length (n)", False)]
  describe "extentBits" $
    it "takes the bit columns whose centre a drawn extent covers" $ do
      let columns = zip [7, 6 .. 0] [ Just (127 + 55 * fromIntegral i, 182 + 55 * fromIntegral i) | i <- [0 .. 7 :: Int] ]
      map (extentBits columns) [(127, 237), (237, 292), (127, 566), (20, 120)] `shouldBe` [Just (7, 6), Just (5, 5), Just (7, 0), Nothing]

  withCore $ \ir -> do
    let grid = readGrid [] . table ir
    describe "the Core grids" $ do
      it "read every byte grid of Core without a problem, and their layouts tile" $
        forM_ [ n | (TableId n, t) <- Map.toList (numberedTables ir), isGrid8 t.nCols (map (.text) (concat (take 1 (t.headerRows <> t.rows)))) ] $ \n -> do
          (n, gProblems (grid n)) `shouldBe` (n, [])
          (n, tiling (gridLayout "x" (grid n))) `shouldBe` (n, [])
      it "T17: the corner Bit over a lone Byte row, (MSB) to (LSB) fields of whole bytes, the payload after Length (n)" $ do
        let g = grid 17
        gCorner g `shouldBe` "Bit"
        spans g `shouldBe`
          [ ("Reserved", (0, 7), Just (3, 0)), ("ComID", (4, 7), Just (5, 0)), ("ComID Extension", (6, 7), Just (7, 0))
          , ("OutstandingData", (8, 7), Just (11, 0)), ("MinTransfer", (12, 7), Just (15, 0)), ("Length (n)", (16, 7), Just (19, 0))
          , ("Payload", (20, 7), Nothing) ]
        gSize g `shouldBe` 20
        [ (gfBase f, gfNote f, fieldVariable f) | f <- gFields g, gfName f == "Length (n)" ] `shouldBe` [("Length", Just "n", Just "n")]
        mapMaybe gfEnd (gFields g) `shouldBe` ["If Length > 0, 20 to n + 19"]
      it "T18: a packet header of 24 bytes, its Session's parenthesis kept apart" $ do
        let g = grid 18
        gSize g `shouldBe` 24
        take 1 [ (gfBase f, gfNote f) | f <- gFields g ] `shouldBe` [("Session", Just "0-3 = TSN, 4-7 = HSN")]
        map (\(n, _, l) -> (n, l)) (spans g) `shouldBe`
          [ ("Session (0-3 = TSN, 4-7 = HSN)", Just (7, 0)), ("SeqNumber", Just (11, 0)), ("Reserved", Just (13, 0)), ("AckType", Just (15, 0))
          , ("Acknowledgement", Just (19, 0)), ("Length (n)", Just (23, 0)), ("Payload", Nothing) ]
      it "T20: a data subpacket of 12 bytes, its payload's end padded modulo 4" $ do
        let g = grid 20
        (gSize g, mapMaybe gfEnd (gFields g)) `shouldBe` (12, ["If Length > 0, 12 to (n+(-n modulo 4)) + 11"])
      it "T39: a run of reserved bytes over an elided row, and Vendor Unique to byte 47" $ do
        let g = grid 39
        spans g `shouldBe`
          [ ("Length of Parameter Data", (0, 7), Just (3, 0)), ("Data Structure Major Version", (4, 7), Just (5, 0))
          , ("Data Structure Minor Version", (6, 7), Just (7, 0)), ("Reserved", (8, 7), Just (15, 0)), ("Vendor Unique", (16, 7), Just (47, 0)) ]
        [ brSpan r | r <- gRows g, brLabel r == "…" ] `shouldBe` [Just Elided, Just Elided]
        gSize g `shouldBe` 48
      it "T42: byte 4's nine cells by position, byte 2's two by their drawn extents (4 + 4); byte 4's flags as sedutil's TPer map has them" $ do
        let g = grid 42
            row label = [ r | r <- gRows g, brLabel r == label ]
        rowTexts (table ir 42) "4" `shouldSatisfy` ((== 9) . length)
        map brMapping (row "4" <> row "2") `shouldBe` [Positional, ByExtent]
        [ s | s@(n, (b, _), _) <- spans g, b == 2, n `elem` ["Version", "Reserved"] ] `shouldBe` [("Version", (2, 7), Just (2, 4)), ("Reserved", (2, 3), Just (2, 0))]
        -- DtaStructures.h:96-103: sync, async, acknack, bufferManagement, streaming, reserved02, comIDManagement, reserved01
        flagsOf 4 g `shouldBe` [(0, "Sync Supported"), (1, "Async Supported"), (2, "ACK/NAK Supported"), (3, "Buffer Mgmt Supported"), (4, "Streaming Supported"), (6, "ComID Mgmt Supported")]
        gSize g `shouldBe` 16
      it "T43: byte 4's seven cells by their drawn extents, Reserved over bits 7-6; its flags as sedutil's Locking map has them" $ do
        let g = grid 43
        rowTexts (table ir 43) "4" `shouldSatisfy` ((== 8) . length)
        [ brMapping r | r <- gRows g, brLabel r == "4" ] `shouldBe` [ByExtent]
        [ s | s@("Reserved", (4, _), _) <- spans g ] `shouldBe` [("Reserved", (4, 7), Just (4, 6))]
        -- DtaStructures.h:126-133: lockingSupported, lockingEnabled, locked, mediaEncryption, MBREnabled, MBRDone
        -- (bit 6 is Opal's MBRshadowingNotSupported, Reserved in Core)
        flagsOf 4 g `shouldBe` [(0, "Locking Supported"), (1, "Locking Enabled"), (2, "Locked"), (3, "Media Encryption"), (4, "MBR Enabled"), (5, "MBR Done")]
      it "T44: one range of reserved bytes" $
        spans (grid 44) `shouldBe` [("Reserved for common SSC parameters", (0, 7), Just (15, 0))]
      it "T212: constants for whole bytes, and fields that start and end inside bytes, across rows" $ do
        let g = grid 212
        gCorner g `shouldBe` "Byte\\Bit"
        spans g `shouldBe`
          [ ("0x02", (0, 7), Just (0, 0)), ("0x23", (1, 7), Just (1, 0)), ("0x00", (2, 7), Just (2, 0)), ("0x08", (3, 7), Just (3, 0))
          , ("NAA (0x05)", (4, 7), Just (4, 4)), ("IEEE COMPANY ID", (4, 3), Just (7, 4)), ("VENDOR-SPECIFIC IDENTIFIER", (7, 3), Just (11, 0)) ]
        map fieldConstant (gFields g) `shouldBe` map Just [0x02, 0x23, 0x00, 0x08, 0x05] <> [Nothing, Nothing]

    describe "the extent rule's fallbacks (T43 without byte 4's drawn extents)" $ do
      let bare = withoutExtents "4" (table ir 43)
          locking = [(0, "Locking Supported"), (1, "Locking Enabled"), (2, "Locked"), (3, "Media Encryption"), (4, "MBR Enabled"), (5, "MBR Done")]
      it "finds byte 4's seven cells over no bit column: C-LAYOUT-EXTENT" $ do
        let g = readGrid [] bare
        [ gpCode p | p <- gProblems g ] `shouldSatisfy` (\cs -> not (null cs) && all (== "C-LAYOUT-EXTENT") cs)
        [ gpMessage p | p <- gProblems g ] `shouldContain` ["byte 4: the cell \"MBR Done\" lies over none of the header's bit columns"]
      it "reads byte 4 by its bits modifier, the bits it does not list reserved, and says so (N-LAYOUT-BITS)" $ do
        let g = readGrid [(4, locking)] bare
        [ (gpSeverity p, gpCode p) | p <- gProblems g ] `shouldBe` [(Cm.Note, "N-LAYOUT-BITS")]
        flagsOf 4 g `shouldBe` locking
        [ s | s@("Reserved", (4, _), _) <- spans g ] `shouldBe` [("Reserved", (4, 7), Just (4, 6))]
        tiling (gridLayout "x" g) `shouldBe` []
      it "lists the fields of a byte its bits modifier reads bit 7 first, as the extent rule does, so a layout prints them in order" $ do
        let byModifier = readGrid [(4, locking)] bare
            byExtent = readGrid [] (table ir 43)
        spans byModifier `shouldBe` spans byExtent
        map fieldStart (layoutFields (gridLayout "x" byModifier)) `shouldSatisfy` (\os -> os == L.sort os)
      it "names each field as the row prints it, whatever the modifier's spelling" $ do
        let lowered = [ (b, T.toLower n) | (b, n) <- locking ]
        spans (readGrid [(4, lowered)] bare) `shouldBe` spans (readGrid [(4, locking)] bare)
      it "refuses a name no cell of the row prints, bits against the printed order, a bit outside 0-7 or listed twice (C-LAYOUT-EXTENT)" $ do
        let problems ps = [ gpMessage p | p <- gProblems (readGrid [(4, ps)] bare), gpCode p == "C-LAYOUT-EXTENT" ]
            phantom = (6, "Phantom") : locking
            swapped = [ (b, n) | (b0, n) <- locking, let b = if b0 == 0 then 5 else if b0 == 5 then 0 else b0 ]
        problems phantom `shouldBe` ["byte 4: its bits modifier names \"Phantom\" (bit 6), which no cell of the row prints"]
        problems swapped `shouldSatisfy` \ms -> not (null ms) && all ("against the order the row prints them, bit 7 leftmost" `T.isSuffixOf`) ms
        problems ((9, "Locked") : drop 3 locking <> take 2 locking) `shouldSatisfy`
          elem "byte 4: its bits modifier lists bit 9 (\"Locked\"), not one of a byte's bits 7 to 0"
        problems ((0, "Locked") : locking) `shouldSatisfy`
          any ("byte 4: its bits modifier lists bit 0 2 times" `T.isPrefixOf`)
        problems locking `shouldBe` []
      it "refuses a printed Reserved cell over no bit the modifier leaves reserved (C-LAYOUT-EXTENT)" $ do
        -- every bit listed: the Reserved cell printed before MBR Done lies over none of them
        let full = (7, "Locked") : (6, "Locking Enabled") : [ p | p@(b, _) <- locking, b <= 5, b /= 2, b /= 1 ]
        [ gpMessage p | p <- gProblems (readGrid [(4, full)] bare), gpCode p == "C-LAYOUT-EXTENT" ] `shouldSatisfy`
          elem "byte 4: the cell \"Reserved\" lies where its bits modifier leaves no bit reserved"
      it "reports a bits modifier the extent rule leaves unused (W-LAYOUT-BITS-UNUSED)" $
        [ gpCode p | p <- gProblems (readGrid [(4, locking), (9, [(0, "X")])] (table ir 43)) ]
          `shouldBe` ["W-LAYOUT-BITS-UNUSED", "W-LAYOUT-BITS-UNUSED"]
      it "reports an (MSB) whose (LSB) is gone (C-LAYOUT-ENDIAN)" $ do
        let t = table ir 17
            noLsb = t { rows = map (map (\c -> if c.text == "(LSB)" then c { text = "" } else c)) t.rows }
        take 1 [ (gpCode p, gpMessage p) | p <- gProblems (readGrid [] noLsb) ]
          `shouldBe` [("C-LAYOUT-ENDIAN", "byte 4: an (MSB) inside the run of \"Reserved\"")]

    describe "fieldParts and the fixed part's size" $ do
      it "reads T18's Session (0-3 = TSN, 4-7 = HSN) as two parts, and no other parenthesis as parts" $ do
        [ fieldParts f | f <- take 1 (gFields (grid 18)) ] `shouldBe` [Just [(0, 3, "TSN"), (4, 7, "HSN")]]
        mapMaybe fieldParts (gFields (grid 17) <> gFields (grid 212)) `shouldBe` []
      it "runs the fixed part to the last byte a row prints, so that bytes no field names are a gap, not a shorter layout" $ do
        -- T39 cut after byte 15, then a row for byte 16 that names no field
        let t39 = table ir 39
            unnamed = t39 { rows = take 11 t39.rows <> [ [ c { text = if i == 0 then "16" else "" } | (i, c) <- zip [0 :: Int ..] (take 2 r) ] | r <- take 1 (drop 11 t39.rows) ] }
        gSize (grid 39) `shouldBe` 48
        gSize (readGrid [] unnamed) `shouldBe` 17
        tiling (gridLayout "x" (readGrid [] unnamed)) `shouldBe` [Gap 128 135]

    describe "gridOf and gridFindings" $
      it "read a bound table with its bits modifiers, and locate the problems at their cells' page" $ do
        let ctx = mkContext Scott ir
            t = withoutExtents "4" (table ir 43)
            bt = BoundTable (TableId 43) t t (SomeBinding (bits 9 [(0, "X")] feature)) (modsOf (SomeBinding (bits 9 [(0, "X")] feature)))
            fs = gridFindings ctx bt (gridOf bt)
        map Cm.code fs `shouldSatisfy` elem "W-LAYOUT-BITS-UNUSED"
        [ Cm.renderFinding f | f <- fs, Cm.code f == "C-LAYOUT-EXTENT" ] `shouldSatisfy`
          all ("C-LAYOUT-EXTENT core-2.01.pdf#page=71&T43: " `T.isPrefixOf`)

  describe "the Opal grids" $
    it "read Tables 2-6 and 8 without a problem, the byte numbers Opal prints in a row's second cell included" $ do
      let path = "../spec/ir/opal-2.30.json"
      present <- doesFileExist path
      if not present then pendingWith (path <> " is not checked out") else do
        opal <- readIR path >>= either throwString pure
        forM_ [2, 3, 4, 5, 6, 8] $ \n -> do
          let g = readGrid [] (table opal n)
          (n, gProblems g, tiling (gridLayout "x" g)) `shouldBe` (n, [], [])
        let t3 = readGrid [] (table opal 3)
        [ brSpan r | r <- gRows t3, brLabel r == "1" ] `shouldBe` [Just (OneByte 1)]
        [ (gfBase f, fieldConstant f) | f <- gFields t3, gfBase f == "Feature Code" ] `shouldBe` [("Feature Code", Just 1)]
