{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.LegendSpec
Description : Test specifications for the legend rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The legend rule (design example E11; Opal 1.3.4 and Table 1, PDF 12): its
record and its exemplar, Opal Table 1, on which it still fires; 'cellAccess'
on Opal's scale built by hand, each way a cell is classed (blank, the @(N)@
marker, unshaded, each class, one style signal, a fill off the scale, no
fonts, a class the scale lacks); and, on the committed Opal extraction under
the M4b sidecar read over the committed Core run, the scale 'legendScale'
reads from Table 1, the rule's checks, and the 975 kept body cells of the
Admin SP's twelve preconfiguration tables classed by it (bound by
@boundTables (Just Preconfig)@, informative columns and group rows left out,
as the preconfig rule keeps them): 319 ReadOnlySpecified, 10 ReadOnlyVU, 3
WritePersonalizable, 4 WriteFixed, 639 Unspecified, no NotDefined; 10
W-LEGEND-OFFSCALE (fill 218, PDF 48-50), 4 W-LEGEND-STYLE (T25's underlined
PINs, PDF 48-49, and T27's GUDID, bold, PDF 50, since the M5a review's
extractor judges a cell's style without its note marker), no C-LEGEND-TOTAL.
The Locking SP's fourteen tables (M5's, which no annotation binds at M4b) are
classed here only: 4 W-LEGEND-OFFSCALE (T40's fill 235, PDF 82-83) and 2
W-LEGEND-STYLE (T41's bold PINs, PDF 84); the eight cells a superscript or a
note marker beside a bold and underlined value left plain until the M5a
review (T44's MaxRanges, T46's MBRDoneOnReset, the six Keys of T47 and T48,
PDF 86-89) in the bold and underlined classes of their greys.
Edited IRs: a row of Table 1 removed, a printed cell's fonts nulled (the
IR's form of a cell whose style signals are null, the lead's answer 15) and
a cell's fill set to 200 give C-LEGEND-TOTAL, C-LEGEND-TOTAL and
W-LEGEND-OFFSCALE naming the nearest grey. Cells are edited through their
JSON form, as "Hsed.Spec.CoreIR" edits them.
-}
module Hsed.Spec.Rules.LegendSpec (spec) where

import           Data.Aeson             (Value (..), fromJSON, toJSON, (.=))
import qualified Data.Aeson             as A
import qualified Data.Aeson.KeyMap      as KM
import qualified RIO.List               as L
import qualified RIO.Map                as Map
import qualified RIO.Text               as T
import           RIO
import           Test.Hspec

import           Hsed.Core.Table        (Access (..))
import           Hsed.Spec.Anchor       (AnchorId (..))
import           Hsed.Spec.Check        (checkIR)
import           Hsed.Spec.Common       (CodeDef (..), ErrataSet (..), Severity (..), SpecId (..), renderFinding)
import qualified Hsed.Spec.Common       as Cm
import           Hsed.Spec.Compile      (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR           (CellIR (..), SpecIR (..), TableIR (..))
import           Hsed.Spec.RuleDef      (BoundTable (..), Context (..), RuleDef (..), boundTables, ruleShape)
import           Hsed.Spec.Rules        (checkExemplarsWith)
import           Hsed.Spec.Rules.Legend
import           Hsed.Spec.Shape        (Shape (..), normHeader)


-- * Opal's scale, and cells to class

-- | Opal's scale as Table 1 prints it (PDF 12): two greys, each plain and
--   bold and underlined, the marker @(N)@, the tolerance 3.
opalScale :: LegendScale
opalScale = LegendScale
  { lsClasses    = [ LegendClass 230 False ReadOnlySpecified, LegendClass 230 True ReadOnlyVU
                   , LegendClass 179 True WritePersonalizable, LegendClass 179 False WriteFixed ]
  , lsNotDefined = "(N)"
  , lsTolerance  = 3 }

-- | A printed cell as the extractor writes one, at T24 row 4's Enabled
--   rectangle on PDF 48: its text, its fill (a grey's channel, or none), bold,
--   underlined.
printedCell :: Text -> Maybe Int -> Bool -> Bool -> CellIR
printedCell t fill isBold isUnderlined = case fromJSON cell of
  A.Success c -> c
  A.Error e   -> error ("printedCell: " <> e)
  where
    cell = A.object
      [ "kind" .= ("TD" :: Text), "text" .= t, "lines" .= [t], "page" .= (47 :: Int)
      , "bbox" .= [205.5, 388.76, 210.76, 403.38 :: Double], "bold" .= isBold, "underline" .= isUnderlined
      , "fonts" .= [if isBold then "ArialNarrow-Bold" else "ArialNarrow" :: Text]
      , "fill_rgb" .= fmap (\v -> [v, v, v]) fill ]

-- | Where 'printedCell' puts a cell, as a message names it.
atRect :: Text
atRect = " at [205.5, 388.76, 210.76, 403.38]"


-- * Classing the kept body cells of tables

-- | One kept body cell, classed.
data Classed = Classed
  { clTable    :: !AnchorId
  , clRow      :: !Int                         -- ^ 1-based among the table's rows, group rows counted
  , clColumn   :: !Text                        -- ^ its column's header cell
  , clPage     :: !(Maybe Int)                 -- ^ its 1-based PDF page
  , clAccess   :: !Access
  , clFindings :: ![(Severity, Text, Text)]
  }

-- | Every kept body cell of tables, classed by a scale: each row but a group
--   row (its first cell alone printed), each column but one whose header
--   mentions that it is informative, as the preconfig rule keeps them.
classed :: LegendScale -> [(AnchorId, TableIR)] -> [Classed]
classed scale ts =
  [ Classed aid i h ((+ 1) <$> c.page) acc fs
  | (aid, t) <- ts
  , let header = maybe [] (map (T.strip . (.text))) (listToMaybe t.headerRows)
  , (i, row) <- zip [1 ..] t.rows, not (isGroup row)
  , (h, c) <- zip header row, not ("informative" `T.isInfixOf` normHeader h)
  , let (acc, fs) = cellAccess scale c ]
  where
    isGroup :: [CellIR] -> Bool
    isGroup row = case map (T.strip . (.text)) row of
      firstCell : rest@(_ : _) -> not (T.null firstCell) && all T.null rest
      _                        -> False

-- | How many cells each access has, every access listed.
histogram :: [Classed] -> [(Access, Int)]
histogram cs = [ (a, length (filter ((== a) . clAccess) cs)) | a <- [minBound .. maxBound] ]

-- | Each table's accesses that some cell has, in table order.
perTable :: [Classed] -> [(AnchorId, [(Access, Int)])]
perTable cs = [ (aid, filter ((> 0) . snd) (histogram (filter ((== aid) . clTable) cs))) | aid <- L.nub (map clTable cs) ]

-- | Where a code is found: table, row, column, PDF page.
foundAt :: Text -> [Classed] -> [(AnchorId, Int, Text, Maybe Int)]
foundAt code cs = [ (clTable c, clRow c, clColumn c, clPage c) | c <- cs, (_, k, _) <- clFindings c, k == code ]

-- | What a code's findings say.
messagesOf :: Text -> [Classed] -> [Text]
messagesOf code cs = [ m | c <- cs, (_, k, m) <- clFindings c, k == code ]

-- | The Admin SP's preconfiguration tables, as the preconfig rule binds them
--   (section 4.2 under the M4b sidecar), read under the errata set.
adminTables :: Context -> [(AnchorId, TableIR)]
adminTables ctx = [ (btAnchor bt, btRead bt) | bt <- boundTables (Just Preconfig) ctx ]

-- | The Locking SP's preconfiguration tables (T34-T42, T44-T48: M5's, bound
--   by no annotation at M4b), as printed.
lockingTables :: Context -> [(AnchorId, TableIR)]
lockingTables ctx = [ (aid, t) | n <- [34 .. 42] <> [44 .. 48], let aid = TableId n, Just t <- [Map.lookup aid (cxTables ctx)] ]

-- | The cells of the Admin SP's tables classed by the scale a Context reads
--   (none classes every shaded cell C-LEGEND-TOTAL).
adminCells :: Context -> [Classed]
adminCells ctx = classed (fromMaybe (LegendScale [] "" 0) (legendScale ctx)) (adminTables ctx)


-- * Editing an IR

-- | A numbered table's rows edited.
withRows :: Text -> ([[CellIR]] -> [[CellIR]]) -> SpecIR -> SpecIR
withRows tid f ir = ir { tables = map table ir.tables }
  where table t = if t.id == Just tid then t { rows = f t.rows } else t

-- | One data cell of a numbered table edited through its JSON form: the
--   1-based row among the table's rows and the column's header cell.
editCell :: Text -> Int -> Text -> (KM.KeyMap Value -> KM.KeyMap Value) -> SpecIR -> SpecIR
editCell tid rowNumber column f ir = withRows tid (zipWith editRow [1 ..]) ir
  where
    header = maybe [] (map (T.strip . (.text))) (listToMaybe (concat [ t.headerRows | t <- ir.tables, t.id == Just tid ]))
    target = L.elemIndex column header
    editRow :: Int -> [CellIR] -> [CellIR]
    editRow i row
      | i == rowNumber = zipWith (\j c -> if Just j == target then editJSON f c else c) [0 ..] row
      | otherwise      = row

-- | A cell edited through its JSON form (the extractor's field names).
editJSON :: (KM.KeyMap Value -> KM.KeyMap Value) -> CellIR -> CellIR
editJSON f c = case toJSON c of
  Object o -> case fromJSON (Object (f o)) of
    A.Success c' -> c'
    A.Error e    -> error ("editJSON: " <> e)
  _ -> c

-- | The legend rule's own findings on a Context.
legendFindings :: Context -> [Text]
legendFindings ctx = map renderFinding (ruleChecks legendRule ctx (boundTables (Just Legend) ctx))


spec :: Spec
spec = do
  describe "the rule" $ do
    it "binds the Legend shape and claims C-LEGEND-TOTAL (an Error), W-LEGEND-OFFSCALE and W-LEGEND-STYLE (Warnings)" $ do
      ruleShape legendRule `shouldBe` Just Legend
      map codeName (ruleCodes legendRule) `shouldBe` ["C-LEGEND-TOTAL", "W-LEGEND-OFFSCALE", "W-LEGEND-STYLE"]
      map codeSeverities (ruleCodes legendRule) `shouldBe` [[Error], [Warning], [Warning]]
      exemplar legendRule `shouldBe` (SpecId "opal-2.30", TableId 1)
    withOpal $ \opalPristine ->
      it "still fires on its exemplar, Opal Table 1, and its shape accepts it (no C-EXEMPLAR)" $
        checkExemplarsWith [legendRule] opalPristine `shouldBe` []

  describe "cellAccess, on Opal's scale built by hand" $ do
    let classOf c = cellAccess opalScale c
    it "classes a blank cell Unspecified whatever its shading (PDF 12), and a cell holding (N) NotDefined" $ do
      classOf (printedCell "" (Just 231) False False) `shouldBe` (Unspecified, [])
      classOf (printedCell "  " (Just 180) True True) `shouldBe` (Unspecified, [])
      classOf (printedCell "(N)" (Just 231) False False) `shouldBe` (NotDefined, [])
      classOf (printedCell "(N) *AC8" Nothing False False) `shouldBe` (NotDefined, [])
    it "classes an unshaded cell, no fill or white, Unspecified" $ do
      classOf (printedCell "Get" Nothing False False) `shouldBe` (Unspecified, [])
      classOf (printedCell "\x201C\&Admin\x201D" (Just 255) False False) `shouldBe` (Unspecified, [])
      classOf (printedCell "Admins" (Just 255) True False) `shouldBe` (Unspecified, [])
    it "classes each grey within 3, plain or bold and underlined, as Table 1 does" $ do
      map (fst . classOf)
        [ printedCell "T" (Just 231) False False, printedCell "MSID" (Just 231) True True
        , printedCell "VU" (Just 180) True True, printedCell "Password" (Just 181) False False
        , printedCell "T" (Just 233) False False, printedCell "T" (Just 227) False False ]
        `shouldBe` [ReadOnlySpecified, ReadOnlyVU, WritePersonalizable, WriteFixed, ReadOnlySpecified, ReadOnlySpecified]
      all (null . snd . classOf) [printedCell "T" (Just 233) False False, printedCell "T" (Just 176) True True] `shouldBe` True
    it "reads one style signal as the grey's bold and underlined class, naming the signal present (W-LEGEND-STYLE)" $ do
      classOf (printedCell "VU" (Just 180) False True) `shouldBe`
        ( WritePersonalizable
        , [ (Warning, "W-LEGEND-STYLE", "\"VU\"" <> atRect <> " is underlined but not bold, one of the legend's two style signals: "
                                          <> "read as the bold and underlined class of (179, 179, 179), WritePersonalizable") ] )
      classOf (printedCell "\x201C\x201D" (Just 180) True False) `shouldBe`
        ( WritePersonalizable
        , [ (Warning, "W-LEGEND-STYLE", "\"\x201C\x201D\"" <> atRect <> " is bold but not underlined, one of the legend's two style signals: "
                                          <> "read as the bold and underlined class of (179, 179, 179), WritePersonalizable") ] )
    it "reads a fill off the scale as the nearest grey, with the fill and the cell's rectangle (W-LEGEND-OFFSCALE)" $ do
      classOf (printedCell "T" (Just 218) False False) `shouldBe`
        ( ReadOnlySpecified
        , [ (Warning, "W-LEGEND-OFFSCALE", "\"T\"" <> atRect <> " is shaded (218, 218, 218), not within 3 of a grey the legend prints, "
                                             <> "(230, 230, 230) or (179, 179, 179): read as the nearest, (230, 230, 230), ReadOnlySpecified") ] )
      map (fst . classOf) [printedCell "0" (Just 218) True True, printedCell "\"\"" (Just 235) False False, printedCell "T" (Just 234) False False]
        `shouldBe` [ReadOnlyVU, ReadOnlySpecified, ReadOnlySpecified]
      fst (classOf (printedCell "T" (Just 200) False False)) `shouldBe` WriteFixed
      map (\(_, code, _) -> code) (snd (classOf (printedCell "T" (Just 218) False True))) `shouldBe` ["W-LEGEND-OFFSCALE", "W-LEGEND-STYLE"]
    it "cannot class a printed, shaded cell whose fonts the IR does not record (C-LEGEND-TOTAL)" $ do
      let noFonts = editJSON (KM.insert "fonts" Null) (printedCell "MSID" (Just 231) True True)
      classOf noFonts `shouldBe`
        ( Unspecified
        , [ (Error, "C-LEGEND-TOTAL", "\"MSID\"" <> atRect <> " is shaded (231, 231, 231), but the IR records no font for it, so no style: "
                                         <> "the legend cannot class it") ] )
      classOf (editJSON (KM.insert "fonts" Null) (printedCell "Get" Nothing False False)) `shouldBe` (Unspecified, [])
    it "cannot class a cell by a class the scale lacks (C-LEGEND-TOTAL), nor by a scale without greys" $ do
      let plainOnly = opalScale { lsClasses = filter (not . lcStyled) (lsClasses opalScale) }
      cellAccess plainOnly (printedCell "MSID" (Just 231) True True) `shouldBe`
        ( Unspecified
        , [ (Error, "C-LEGEND-TOTAL", "\"MSID\"" <> atRect <> " is shaded (231, 231, 231) and bold and underlined, a class the legend "
                                         <> "does not print for the grey (230, 230, 230)") ] )
      map (\(_, code, _) -> code) (snd (cellAccess (LegendScale [] "" 0) (printedCell "T" (Just 231) False False))) `shouldBe` ["C-LEGEND-TOTAL"]
      fst (cellAccess (LegendScale [] "" 0) (printedCell "(N)" (Just 231) False False)) `shouldBe` Unspecified

  withM4bOpal $ \base opal -> do
    let octx  = opalOver base opal
        cells = adminCells octx
    describe "legendScale, on Opal under the M4b sidecar" $ do
      it "reads Table 1 (PDF 12): greys 230 and 179 each plain and bold and underlined, the four classes, the marker (N), tolerance 3" $
        legendScale octx `shouldBe` Just opalScale
      it "reads none in Core, which has no legend" $
        legendScale base `shouldBe` Nothing

    describe "the rule's checks, on Opal under the M4b sidecar" $ do
      it "find Table 1 a scale: no finding at T1 in the whole run, and no C-LEGEND-TOTAL anywhere" $ do
        legendFindings octx `shouldBe` []
        let fs = checkIR (Just base) Scott opal
        [ renderFinding f | f <- fs, Cm.anchor f == Just "T1" ] `shouldBe` []
        [ renderFinding f | f <- fs, Cm.code f == "C-LEGEND-TOTAL" ] `shouldBe` []
      it "give C-LEGEND-TOTAL at Table 1 for each of its five rows removed, and read no scale" $ do
        let without k = opalOver base (withRows "T1" (\rs -> [ r | (i, r) <- zip [1 :: Int ..] rs, i /= k ]) opal)
            prefix = "C-LEGEND-TOTAL opal-2.30.pdf#page=12&T1: the legend prints no scale: "
        map (legendFindings . without) [1 .. 5] `shouldBe` map (\m -> [prefix <> m])
          [ "the grey (230, 230, 230) has no plain class"
          , "the grey (230, 230, 230) has no bold and underlined class"
          , "no row's R-W is Not Defined, the marker of a column not defined"
          , "the grey (179, 179, 179) has no bold and underlined class"
          , "the grey (179, 179, 179) has no plain class" ]
        map (legendScale . without) [1 .. 5] `shouldBe` replicate 5 Nothing
      it "give it in the whole run too (hsedc check), the run's one C-LEGEND-TOTAL" $
        [ renderFinding f | f <- checkIR (Just base) Scott (withRows "T1" (drop 1) opal), Cm.code f == "C-LEGEND-TOTAL" ]
          `shouldBe` ["C-LEGEND-TOTAL opal-2.30.pdf#page=12&T1: the legend prints no scale: the grey (230, 230, 230) has no plain class"]
      it "say what a row prints that is no class, no grey or one style word" $ do
        let edited e = legendFindings (opalOver base (e opal))
        edited (editCell "T1" 1 "R-W" (KM.insert "text" (String "Read-write")))
          `shouldBe` [ "C-LEGEND-TOTAL opal-2.30.pdf#page=12&T1: the legend prints no scale: row 1: its R-W \"Read-write\", Value "
                       <> "\"Opal SSC specified\" and Access Control \"Fixed\" name no class; the grey (230, 230, 230) has no plain class" ]
        edited (editCell "T1" 4 "Table Cell Legend" (KM.insert "text" (String "Arial Narrow bold (179, 179, 179)")))
          `shouldBe` [ "C-LEGEND-TOTAL opal-2.30.pdf#page=12&T1: the legend prints no scale: row 4: its sample "
                       <> "\"Arial Narrow bold (179, 179, 179)\" names bold but not under; the grey (179, 179, 179) has no bold and underlined class" ]
        edited (editCell "T1" 5 "Table Cell Legend" (KM.insert "text" (String "Arial-Narrow (179, 180, 179)")))
          `shouldBe` [ "C-LEGEND-TOTAL opal-2.30.pdf#page=12&T1: the legend prints no scale: row 5: its sample's RGB value (179, 180, 179) "
                       <> "is no grey; the grey (179, 179, 179) has no plain class" ]

    describe "cellAccess over the Admin SP's twelve preconfiguration tables (under the M4b sidecar)" $ do
      it "keeps 975 body cells of T18-T25, T27-T29 and T33, informative columns and group rows left out" $ do
        map fst (adminTables octx) `shouldBe` map TableId ([18 .. 25] <> [27, 28, 29, 33])
        length cells `shouldBe` 975
      it "classes them ReadOnlySpecified 319, ReadOnlyVU 10, WritePersonalizable 3, WriteFixed 4, NotDefined 0, Unspecified 639" $
        histogram cells `shouldBe`
          [ (ReadOnlySpecified, 319), (ReadOnlyVU, 10), (WritePersonalizable, 3), (WriteFixed, 4), (NotDefined, 0), (Unspecified, 639) ]
      it "classes each table's cells (T22 136/374; T24 63, 3 WriteFixed, 48; T25 6, 7 VU, 3 WritePersonalizable, 16; T27's GUDID VU; T28 10, 2 VU, 3)" $
        perTable cells `shouldBe`
          [ (TableId 18, [(ReadOnlySpecified, 3), (Unspecified, 4)])
          , (TableId 19, [(ReadOnlySpecified, 6), (Unspecified, 2)])
          , (TableId 20, [(ReadOnlySpecified, 48), (Unspecified, 132)])
          , (TableId 21, [(ReadOnlySpecified, 8), (Unspecified, 24)])
          , (TableId 22, [(ReadOnlySpecified, 136), (Unspecified, 374)])
          , (TableId 23, [(ReadOnlySpecified, 30), (Unspecified, 20)])
          , (TableId 24, [(ReadOnlySpecified, 63), (WriteFixed, 3), (Unspecified, 48)])
          , (TableId 25, [(ReadOnlySpecified, 6), (ReadOnlyVU, 7), (WritePersonalizable, 3), (Unspecified, 16)])
          , (TableId 27, [(ReadOnlySpecified, 3), (ReadOnlyVU, 1), (WriteFixed, 1), (Unspecified, 4)])
          , (TableId 28, [(ReadOnlySpecified, 10), (ReadOnlyVU, 2), (Unspecified, 3)])
          , (TableId 29, [(ReadOnlySpecified, 6), (Unspecified, 10)])
          , (TableId 33, [(Unspecified, 2)]) ]
      it "finds 10 W-LEGEND-OFFSCALE, each at fill 218: T24 row 4 Enabled (PDF 48), T25 rows 3-4 (PDF 49), T27 GUDID (PDF 50)" $ do
        foundAt "W-LEGEND-OFFSCALE" cells `shouldBe`
          [ (TableId 24, 4, "Enabled", Just 48)
          , (TableId 25, 3, "CharSet", Just 49), (TableId 25, 3, "TryLimit", Just 49), (TableId 25, 3, "Tries", Just 49), (TableId 25, 3, "Persistence", Just 49)
          , (TableId 25, 4, "CharSet", Just 49), (TableId 25, 4, "TryLimit", Just 49), (TableId 25, 4, "Tries", Just 49), (TableId 25, 4, "Persistence", Just 49)
          , (TableId 27, 1, "GUDID", Just 50) ]
        take 1 (messagesOf "W-LEGEND-OFFSCALE" cells) `shouldBe`
          [ "\"T\" at [205.5, 388.76, 210.76, 403.38] is shaded (218, 218, 218), not within 3 of a grey the legend prints, "
            <> "(230, 230, 230) or (179, 179, 179): read as the nearest, (230, 230, 230), ReadOnlySpecified" ]
      it "finds 4 W-LEGEND-STYLE, T25's underlined PINs, rows 1, 3 and 4 (PDF 48-49), and T27's bold GUDID (PDF 50), and no C-LEGEND-TOTAL" $ do
        foundAt "W-LEGEND-STYLE" cells `shouldBe`
          [ (TableId 25, 1, "PIN", Just 48), (TableId 25, 3, "PIN", Just 49), (TableId 25, 4, "PIN", Just 49)
          , (TableId 27, 1, "GUDID", Just 50) ]
        foundAt "C-LEGEND-TOTAL" cells `shouldBe` []

    describe "cellAccess over the Locking SP's tables (M5's: classed here only)" $ do
      it "finds 4 W-LEGEND-OFFSCALE, T40's cells at 235 (PDF 82-83), and 2 W-LEGEND-STYLE, T41's bold PINs (PDF 84)" $ do
        let locking = classed opalScale (lockingTables octx)
        map fst (lockingTables octx) `shouldBe` map TableId ([34 .. 42] <> [44 .. 48])
        foundAt "W-LEGEND-OFFSCALE" locking `shouldBe`
          [ (TableId 40, 1, "CommonName", Just 82), (TableId 40, 1, "IsClass", Just 82)
          , (TableId 40, 2, "CommonName", Just 82), (TableId 40, 8, "CommonName", Just 83) ]
        foundAt "W-LEGEND-STYLE" locking `shouldBe` [(TableId 41, 6, "PIN", Just 84), (TableId 41, 7, "PIN", Just 84)]
        foundAt "C-LEGEND-TOTAL" locking `shouldBe` []
      it "classes a bold and underlined value by its style whatever note it prints, eight cells: T44's MaxRanges, T46's DoneOnReset, T47's and T48's six Keys (PDF 86-89)" $ do
        let locking = classed opalScale (lockingTables octx)
            at (t, r, h) = [ clAccess c | c <- locking, clTable c == TableId t, clRow c == r, clColumn c == h ]
            noted = [(44, 1, "MaxRanges"), (46, 1, "DoneOnReset")] <> [ (t, r, "Key") | t <- [47, 48], r <- [1 .. 3] ]
        length noted `shouldBe` 8
        concatMap at noted `shouldBe` ReadOnlyVU : replicate 7 WritePersonalizable

    describe "cellAccess over edited Admin SP tables" $ do
      it "gives C-LEGEND-TOTAL for a printed cell whose fonts are nulled (T25 row 2's MSID, PDF 48), read as Unspecified" $ do
        let edited = adminCells (opalOver base (editCell "T25" 2 "PIN" (KM.insert "fonts" Null) opal))
        foundAt "C-LEGEND-TOTAL" edited `shouldBe` [(TableId 25, 2, "PIN", Just 48)]
        messagesOf "C-LEGEND-TOTAL" edited `shouldBe`
          [ "\"MSID\" at [304.92, 60.86, 326.5, 75.72] is shaded (231, 231, 231), but the IR records no font for it, so no style: "
            <> "the legend cannot class it" ]
        lookup ReadOnlyVU (histogram edited) `shouldBe` Just 9
      it "gives W-LEGEND-OFFSCALE naming the nearest grey for a cell shaded 200 (T18 row 1's UID, PDF 35), read as its class" $ do
        let edited = adminCells (opalOver base (editCell "T18" 1 "UID" (KM.insert "fill_rgb" (toJSON [200, 200, 200 :: Int])) opal))
        length (foundAt "W-LEGEND-OFFSCALE" edited) `shouldBe` 11
        take 1 (foundAt "W-LEGEND-OFFSCALE" edited) `shouldBe` [(TableId 18, 1, "UID", Just 35)]
        take 1 (messagesOf "W-LEGEND-OFFSCALE" edited) `shouldBe`
          [ "\"00 00 00 02 00 00 00 01\" at [172.02, 311.66, 217.54, 339.24] is shaded (200, 200, 200), not within 3 of a grey "
            <> "the legend prints, (230, 230, 230) or (179, 179, 179): read as the nearest, (179, 179, 179), WriteFixed" ]
        lookup WriteFixed (histogram edited) `shouldBe` Just 5

    describe "compile, on Opal under the M4b sidecar" $
      it "generates nothing from Table 1 and leaves it out of nothing: no W-GEN-EXCLUDED at T1" $ do
        ruleCompile legendRule octx (boundTables (Just Legend) octx) `shouldBe` []
        cr <- either (throwString . show) pure (compileIR (Just base) Scott opal)
        [ renderFinding f | f <- crFindings cr, Cm.code f == "W-GEN-EXCLUDED", Cm.anchor f == Just "T1" ] `shouldBe` []
        filter ((== TableId 1) . fst) (crExcluded cr) `shouldBe` []
