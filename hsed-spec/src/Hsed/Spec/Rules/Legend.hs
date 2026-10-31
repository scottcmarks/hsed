{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.Legend
Description : The legend rule: the shading and font scale of an SSC's preconfiguration tables
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design example E11. @legend@ fires on a table of at least two columns whose
first header cell mentions the legend: in Opal 2.30 Table 1, @Preconfiguration
Tables Legend@ (section 1.3.4, PDF 12), its rule's exemplar, alone; Core has
none. Table 1 prints the scale the preconfiguration tables' cells are shaded
and set by. Each row's first cell names a style and prints an RGB value in
parentheses, @Arial-Narrow (230, 230, 230)@ or @Arial Narrow bold- under (179,
179, 179)@; its @R-W@, @Value@ and @Access Control@ cells give the class:

* read-only, its value specified by the SSC: @(230, 230, 230)@, plain
  ('ReadOnlySpecified');
* read-only, its value vendor unique (@VU@): @(230, 230, 230)@, bold and
  underlined ('ReadOnlyVU');
* writable, its access control personalizable: @(179, 179, 179)@, bold and
  underlined ('WritePersonalizable');
* writable, its access control fixed: @(179, 179, 179)@, plain ('WriteFixed');
* not defined, the row whose @R-W@ is @Not Defined@: its @Value@, @(N)@, is
  the marker of a cell not defined ('NotDefined'). Its sample is drawn white,
  so the marker is text and never a colour.

The coloring "is informative only"; a preconfiguration cell's content is
normative, and an empty or blank cell is not specified "regardless of
shading" (PDF 12). Get access on three shaded read-only AccessControl columns
(InvokingID, MethodID, GetACLACL) is @(N)@ (section 4.2.1.5, PDF 39): the
classes are documentation in the generated @access@ lists and gate nothing
(the lead's answer 2 to the M4b plan); the ACL alone decides access.

'legendScale' reads the scale from the spec's own legend table, under the
errata set: each row's grey is the RGB value it prints (three equal
channels), its style whether its first cell names both @bold@ and @under@
(plain when it names neither), and its class its words; a scale has one
marker row and two greys, each with a plain and a bold and underlined class.
Its tolerance, 3, is how far a measured fill may lie from a printed grey:
the extractor measures the printed greys at 231 and at 180 or 181 (PDF 12,
35-89), within 2, and the fills the legend does not print at 218 (PDF 48-50)
and 235 (PDF 82-83), beyond 3.

'cellAccess' classes one kept body cell of a preconfiguration table, for the
preconfig rule's @access@ lists and its findings (the rule reads the scale
through this module and gives the findings, located at the cell's row; the
codes are claimed here, once):

* blank text: 'Unspecified', whatever its shading (PDF 12);
* text holding the marker: 'NotDefined';
* no fill, or white: 'Unspecified';
* a printed, shaded cell whose fonts the IR does not record: its style, and
  so its class, is unknown (@C-LEGEND-TOTAL@, an Error; the cell read as
  'Unspecified'). The extractor records a cell's bold, underline and fonts
  together, from its ink, and an IR @null@ bold or underline reads as
  'False' ("Hsed.Spec.IR"), so a cell with text and no fonts is the cell
  whose style signals the IR leaves null (the lead's answer 15);
* otherwise its grey is the printed grey nearest its fill (every channel
  within the tolerance of it; else @W-LEGEND-OFFSCALE@, a Warning naming the
  fill, the cell's rectangle and the grey read), and its style plain (neither
  bold nor underlined) or bold and underlined (both); one of the two signals
  alone is read as the grey's bold and underlined class, @W-LEGEND-STYLE@, a
  Warning naming the signal present (Opal T25's three underlined PINs, PDF
  48-49; T41's two bold ones, PDF 84). A scale without that class (a
  hand-built one: 'legendScale' gives none) is @C-LEGEND-TOTAL@.

Checks: @C-LEGEND-TOTAL@ at a bound legend table that does not print a scale
(a header without its R-W, Value or Access Control column; a row naming no
class, no grey, or one style word of the two; the marker row missing or
repeated; other than two greys; a grey without its plain or its bold and
underlined class, or with two). The rule generates nothing: Table 1 is
documentation and the exemplar.
-}
module Hsed.Spec.Rules.Legend
  ( legendRule
    -- * The scale
  , LegendClass (..)
  , LegendScale (..)
  , legendScale
  , cellAccess
  ) where

import qualified Data.Text              as DT
import qualified RIO.List               as L
import qualified RIO.Text               as T
import           RIO

import           Hsed.Core.Table        (Access (..))
import           Hsed.Spec.Anchor       (AnchorId (..))
import           Hsed.Spec.Common       (RuleName (..), Severity (..), SpecId (..), finding)
import           Hsed.Spec.DSL          (SomeBinding (..), legend)
import qualified Hsed.Spec.Gen          as Gen
import           Hsed.Spec.IR           (CellIR (..), RGB (..), Rect (..), TableIR (..))
import           Hsed.Spec.Readers.Text (nameKey)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Shape        (Shape (..), normHeader, shapeMismatch)


-- | The rule.
legendRule :: RuleDef
legendRule = RuleDef
  { ruleName    = RuleName "legend"
  , bindsTo     = SomeBinding legend
  , exemplar    = (SpecId "opal-2.30", TableId 1)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch Legend form)
  , ruleIdent   = const Nothing
  , ruleModule  = legendModule
  , ruleCodes   =
      [ CodeDef "C-LEGEND-TOTAL"    [Error]   (Defect "a legend table that is not its scale, or a preconfiguration cell with no style signal")
      , CodeDef "W-LEGEND-OFFSCALE" [Warning] (Suspect "a preconfiguration cell shaded with a grey the legend does not print")
      , CodeDef "W-LEGEND-STYLE"    [Warning] (Suspect "a preconfiguration cell with one of the legend's two style signals") ]
  , ruleChecks  = \ctx bts ->
      [ atTable ctx bt (finding Error "C-LEGEND-TOTAL" ("the legend prints no scale: " <> why))
      | bt <- bts, Left why <- [readScale (btRead bt)] ]
  , ruleCompile = \_ _ -> []
  }

-- | The module a legend would generate into; the rule generates nothing
--   (Table 1 is documentation), so no such module is written.
legendModule :: Gen.GenModule
legendModule = Gen.GenModule
  { Gen.leaf        = "Legend"
  , Gen.description = "The preconfiguration tables' legend"
  , Gen.notes       = [] }


-- * The scale

-- | One class of the scale: its grey (the printed RGB's channel), whether its
--   cells are bold and underlined, and the access it gives.
data LegendClass = LegendClass
  { lcGrey   :: !Int
  , lcStyled :: !Bool
  , lcAccess :: !Access
  }
  deriving (Eq, Show)

-- | The scale a legend table prints: its classes, the text marking a column
--   not defined (@(N)@), and how far from a printed grey a measured fill may lie.
data LegendScale = LegendScale
  { lsClasses    :: ![LegendClass]
  , lsNotDefined :: !Text
  , lsTolerance  :: !Int
  }
  deriving (Eq, Show)

-- | The scale of the spec's own legend table, when the rule binds one that
--   prints a scale (in Opal, Table 1, PDF 12); none in Core, which has no
--   legend, nor where the table is not a scale (its @C-LEGEND-TOTAL@).
legendScale :: Context -> Maybe LegendScale
legendScale ctx = listToMaybe [ s | bt <- boundTables (Just Legend) ctx, Right s <- [readScale (btRead bt)] ]

-- | How far, channel by channel, a measured fill may lie from the grey a
--   legend prints: the extractor measures Opal's printed greys at 231 and at
--   180 or 181 (PDF 12, 35-89), and the fills its legend does not print at
--   218 (PDF 48-50) and 235 (PDF 82-83).
legendTolerance :: Int
legendTolerance = 3

-- | One row of a legend table: a class, or the marker of a column not defined.
data LegendRow = Shading !LegendClass | Marker !Text

-- | The scale a legend table prints, or every reason it prints none.
readScale :: TableIR -> Either Text LegendScale
readScale t = do
  rw      <- column "R-W"
  value   <- column "Value"
  control <- column "Access Control"
  let rowReads = zipWith (readRow rw value control) [1 ..] t.rows
      shades   = [ k | Right (Shading k) <- rowReads ]
      markers  = [ m | Right (Marker m) <- rowReads ]
      problems = lefts rowReads <> markerProblems markers <> greyProblems shades
  case markers of
    [m] | null problems -> Right (LegendScale shades m legendTolerance)
    _                   -> Left (T.intercalate "; " problems)
  where
    header = maybe [] (map (normHeader . (.text))) (listToMaybe t.headerRows)
    column h = maybe (Left ("the header names no " <> h <> " column")) Right (L.elemIndex (normHeader h) header)
    markerProblems = \case
      [_] -> []
      []  -> ["no row's R-W is Not Defined, the marker of a column not defined"]
      ms  -> [tshow (length ms) <> " rows' R-W is Not Defined"]
    greyProblems shades = case L.nub (map lcGrey shades) of
      greys@[_, _] -> concatMap (styleProblems shades) greys
      greys        -> ["its classes print " <> tshow (length greys) <> " greys, not two"]
    styleProblems shades grey =
      [ "the grey " <> greyText grey <> " has " <> classes n (styleText st)
      | st <- [False, True], let n = length [ () | k <- shades, lcGrey k == grey, lcStyled k == st ], n /= 1 ]
    classes n style
      | n == 0    = "no " <> style <> " class"
      | otherwise = tshow n <> " " <> style <> " classes"

-- | One row of a legend table, numbered from 1: its class from its R-W,
--   Value and Access Control cells, its grey and style from its first cell;
--   or, its R-W @Not Defined@, the marker its Value cell prints.
readRow :: Int -> Int -> Int -> Int -> [CellIR] -> Either Text LegendRow
readRow rw value control i cells
  | key rwText == "notdefined" =
      if T.null valueText then Left (inRow "its R-W is Not Defined, but its Value prints no marker") else Right (Marker valueText)
  | otherwise = do
      access <- maybe (Left (inRow ("its R-W " <> quote rwText <> ", Value " <> quote valueText <> " and Access Control "
                                    <> quote controlText <> " name no class"))) Right classOf
      grey   <- greyOf
      styled <- styleOf
      pure (Shading (LegendClass grey styled access))
  where
    cell k      = maybe "" (T.strip . (.text)) (listToMaybe (drop k cells))
    sample      = cell 0
    rwText      = cell rw
    valueText   = cell value
    controlText = cell control
    key         = nameKey
    inRow why   = "row " <> tshow i <> ": " <> why
    readOnly    = key rwText == "read-only"
    writable    = key rwText == "write"
    classOf
      | readOnly, key valueText == "vu"                            = Just ReadOnlyVU
      | readOnly, "specified" `T.isSuffixOf` key valueText         = Just ReadOnlySpecified
      | writable, "personalizable" `T.isSuffixOf` key controlText  = Just WritePersonalizable
      | writable, key controlText == "fixed"                       = Just WriteFixed
      | otherwise                                                  = Nothing
    greyOf = case printedRGB sample of
      Just (red, green, blue)
        | red == green && green == blue -> Right red
        | otherwise                     -> Left (inRow ("its sample's RGB value " <> rgbText red green blue <> " is no grey"))
      Nothing              -> Left (inRow ("its sample " <> quote sample <> " prints no RGB value"))
    styleOf = case (names "bold", names "under") of
      (True, True)   -> Right True
      (False, False) -> Right False
      (True, False)  -> Left (inRow ("its sample " <> quote sample <> " names bold but not under"))
      (False, True)  -> Left (inRow ("its sample " <> quote sample <> " names under but not bold"))
    names w = w `T.isInfixOf` T.toLower sample

-- | The first RGB value a cell prints in parentheses: @(230, 230, 230)@.
printedRGB :: Text -> Maybe (Int, Int, Int)
printedRGB s = listToMaybe (mapMaybe triple (drop 1 (DT.splitOn "(" s)))
  where
    triple piece = case map (readMaybe . T.unpack . T.strip) (DT.splitOn "," (T.takeWhile (/= ')') piece)) of
      [Just red, Just green, Just blue] -> Just (red, green, blue)
      _                                 -> Nothing


-- * Classing a cell

-- | The access one kept body cell of a preconfiguration table has, with what
--   classing it found (severity, code, message): by its text, its fill and
--   its style, as the module's description says.
cellAccess :: LegendScale -> CellIR -> (Access, [(Severity, Text, Text)])
cellAccess s c
  | T.null printed                                                = (Unspecified, [])
  | not (T.null marker), marker `T.isInfixOf` printed             = (NotDefined, [])
  | otherwise = case c.fillRgb of
      Nothing                                                     -> (Unspecified, [])
      Just rgb
        | distance rgb 255 <= tolerance                           -> (Unspecified, [])
        | null c.fonts                                            -> (Unspecified, [unstyled rgb])
        | otherwise                                               -> shaded rgb
  where
    printed   = T.strip c.text
    marker    = T.strip (lsNotDefined s)
    tolerance = fromIntegral (lsTolerance s) :: Double
    greys     = L.nub (map lcGrey (lsClasses s))
    noun      = quote printed <> maybe "" ((" at " <>) . rectText) (c.bbox <|> c.estBbox)
    total why = (Error, "C-LEGEND-TOTAL", noun <> why)
    unstyled rgb = total (" is shaded " <> fillText rgb <> ", but the IR records no font for it, so no style: the legend cannot class it")
    shaded rgb = case L.sortOn (distance rgb) greys of
      [] -> (Unspecified, [total (" is shaded " <> fillText rgb <> ", but the legend prints no grey")])
      grey : _ ->
        let (styled, signal) = case (c.bold, c.underline) of
              (True, True)   -> (True, Nothing)
              (False, False) -> (False, Nothing)
              (True, False)  -> (True, Just "bold but not underlined")
              (False, True)  -> (True, Just "underlined but not bold")
            klass    = L.find (\k -> lcGrey k == grey && lcStyled k == styled) (lsClasses s)
            readAs   = greyText grey <> maybe "" ((", " <>) . tshow . lcAccess) klass
            offScale = [ ( Warning, "W-LEGEND-OFFSCALE"
                         , noun <> " is shaded " <> fillText rgb <> ", not within " <> tshow (lsTolerance s) <> " of a grey the legend prints, "
                             <> T.intercalate " or " (map greyText greys) <> ": read as the nearest, " <> readAs )
                       | distance rgb grey > tolerance ]
            style    = [ ( Warning, "W-LEGEND-STYLE"
                         , noun <> " is " <> sig <> ", one of the legend's two style signals: read as the bold and underlined class of " <> readAs )
                       | Just sig <- [signal] ]
        in case klass of
             Just k  -> (lcAccess k, offScale <> style)
             Nothing -> (Unspecified, offScale <> style <> [total (" is shaded " <> fillText rgb <> " and " <> styleText styled
                                                                   <> ", a class the legend does not print for the grey " <> greyText grey)])

-- | How far a fill lies from a grey: its farthest channel's distance.
distance :: RGB -> Int -> Double
distance rgb grey = foldl' max 0 [ abs (ch - fromIntegral grey) | ch <- [rgb.r, rgb.g, rgb.b] ]


-- * Messages

-- | A cell's text as a message quotes it.
quote :: Text -> Text
quote t = "\"" <> t <> "\""

-- | @(230, 230, 230)@
greyText :: Int -> Text
greyText grey = rgbText grey grey grey

-- | @(230, 200, 230)@
rgbText :: Int -> Int -> Int -> Text
rgbText red green blue = "(" <> T.intercalate ", " (map tshow [red, green, blue]) <> ")"

-- | A measured fill: @(218, 218, 218)@.
fillText :: RGB -> Text
fillText rgb = "(" <> T.intercalate ", " (map numberText [rgb.r, rgb.g, rgb.b]) <> ")"

-- | A cell's rectangle in PDF user space: @[205.5, 388.76, 210.76, 403.38]@.
rectText :: Rect -> Text
rectText rect = "[" <> T.intercalate ", " (map numberText [rect.x0, rect.y0, rect.x1, rect.y1]) <> "]"

-- | A number as the IR writes it: a whole one without its fraction.
numberText :: Double -> Text
numberText d
  | d == fromIntegral whole = tshow whole
  | otherwise               = tshow d
  where whole = round d :: Integer

-- | A class's style.
styleText :: Bool -> Text
styleText styled = if styled then "bold and underlined" else "plain"
