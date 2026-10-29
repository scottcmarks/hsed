{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Errata
Description : Corrections to spec text, as the annotations carry them, and their application
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design decision 13.5: the compiler builds from the published text as printed
('Published') or from Scott's corrections ('Scott'). The corrections live in the
annotated PDF and nowhere else. Each is one pair of an @erratum scott [...]@
modifier on a table's annotation (Table 75's @erratum scott ["Fractoin" ~>
"Fraction"]@), a plain substitution over that table's caption, header cells
and data cells, cited to the annotation that carries it. The published set
takes none: it is the text as printed, so the grammar refuses an @erratum
published [...]@ annotation outright (@E-ERRATUM-PUBLISHED@).
@tcg/Core Spec errata.txt@ stays Scott's notes: a test checks every
annotation's correction against them, and nothing reads corrections from it.

Each correction is reported: under 'Scott' each one made (@W-ERRATUM-APPLIED@),
under 'Published' each defect left as printed (@W-ERRATUM-KNOWN@), and under
both a correction whose text the table does not contain (@W-ERRATUM-UNUSED@).
Under 'Published', @E-FMT-RESOLVE@ on Table 75's @Fractoin@ follows (design
example E3).

Normalisations are not errata and are not here: the @_object _ref@ captions
("Hsed.Spec.Shape".'Hsed.Spec.Shape.normaliseCaption'), the header typo table
and the spacing of a Format cell are read the same way under either set and
reported as Notes. Nor are hsed's own transcription slips: the captions
numbered 147 and 160 in TypeUIDs.hs (the PDF has 157 and 161).
-}
module Hsed.Spec.Errata
  ( -- * Errata
    Erratum (..)
  , Part (..)
  , partName
    -- * Application
  , Site (..)
  , applyErrata
  , errata
  , correctTable
    -- * Findings
  , appliedFinding
  , knownDefectFinding
  , unusedFinding
  ) where

import qualified Data.Text        as T
import qualified RIO.List         as L
import           RIO

import           Hsed.Spec.Common
import           Hsed.Spec.IR     (CellIR (..), TableIR (..), cellWithText)


-- | Which part of a table a correction was made in.
data Part
  = Caption
  | HeaderCell !Int !Int        -- ^ header row and column, 0-based
  | DataCell !Int !Int !Text    -- ^ data row and column, 0-based, and the column's header text
  deriving (Eq, Ord, Show)

-- | How a finding names a part: @caption@, @header@, or a data cell by its
--   column's header (@Format@, @Associated Value@).
partName :: Part -> Text
partName Caption              = "caption"
partName (HeaderCell _ _)     = "header"
partName (DataCell r c h)
  | T.null (T.strip h)        = "row " <> tshow (r + 1) <> " column " <> tshow (c + 1)
  | otherwise                 = T.unwords (T.words h)

-- | One correction: replace every occurrence of 'errFrom' by 'errTo' in the
--   caption and cells of one table.
data Erratum = Erratum
  { errSpec   :: !SpecId
  , errTable  :: !Text    -- ^ anchor id, @T75@
  , errFrom   :: !Text
  , errTo     :: !Text
  , errSource :: !Text    -- ^ where the correction is recorded: the annotation's location
  }
  deriving (Eq, Show)


-- | Where a piece of text came from: enough to select the errata that apply and
--   to cite the finding (@core-2.01.pdf#page=107&T75@).
data Site = Site
  { siteSpec  :: !SpecId
  , siteTable :: !Text        -- ^ anchor id
  , sitePart  :: !Part
  , sitePage  :: !(Maybe Int) -- ^ 1-based PDF page, when known
  }
  deriving (Eq, Show)

appliesAt :: Site -> Erratum -> Bool
appliesAt site e = errSpec e == siteSpec site && errTable e == siteTable site

-- | Correct one piece of text with the errata of its table, in the order they
--   are given (each sees the text the ones before it left): the corrected text
--   and the errata that fired.
applyErrata :: [Erratum] -> Site -> Text -> (Text, [Erratum])
applyErrata es site text = foldl' step (text, []) (filter (appliesAt site) es)
  where
    step (t, fired) e
      | errFrom e `T.isInfixOf` t = (T.replace (errFrom e) (errTo e) t, fired <> [e])
      | otherwise                 = (t, fired)

-- | 'applyErrata' under an errata set, with its report: under 'Scott' the
--   corrected text and one @W-ERRATUM-APPLIED@ per correction made; under
--   'Published' the text as printed and one @W-ERRATUM-KNOWN@ per correction
--   Scott's set makes there.
errata :: ErrataSet -> [Erratum] -> Site -> Text -> (Text, [Finding])
errata es errs site text = let (t, _, fs) = errataFired es errs site text in (t, fs)

errataFired :: ErrataSet -> [Erratum] -> Site -> Text -> (Text, [Erratum], [Finding])
errataFired es errs site text = case es of
  Scott     -> (corrected, fired, map (appliedFinding site) fired)
  Published -> (text, fired, map (knownDefectFinding site) fired)
  where (corrected, fired) = applyErrata errs site text

-- | A table read under an errata set: its caption, header cells and data cells,
--   each corrected by the table's errata ('errata'), with the findings, and
--   @W-ERRATUM-UNUSED@ for a correction that matches no text of the table.
correctTable :: ErrataSet -> SpecId -> Text -> Maybe Int -> [Erratum] -> TableIR -> (TableIR, [Finding])
correctTable es sid tid pg errs t = (t', concatMap third results <> unused)
  where
    site part = Site sid tid part pg
    header    = maybe [] (map (.text)) (listToMaybe t.headerRows)
    column c  = fromMaybe "" (listToMaybe (drop c header))
    one part  = errataFired es errs (site part)
    caption   = (\c -> one Caption c) <$> t.caption
    headers   = [ [ one (HeaderCell r c) cell.text | (c, cell) <- zip [0 ..] row ] | (r, row) <- zip [0 ..] t.headerRows ]
    cells     = [ [ one (DataCell r c (column c)) cell.text | (c, cell) <- zip [0 ..] row ] | (r, row) <- zip [0 ..] t.rows ]
    results   = maybeToList caption <> concat headers <> concat cells
    setText cell (txt, _, _) = cellWithText txt cell
    t' = t { caption    = (\(txt, _, _) -> txt) <$> caption
           , headerRows = zipWith (zipWith setText) t.headerRows headers
           , rows       = zipWith (zipWith setText) t.rows cells }
    fired     = concatMap (\(_, f, _) -> f) results
    third (_, _, fs) = fs
    unused    = [ unusedFinding sid tid pg e | e <- L.nub (filter (appliesAt (site Caption)) errs), e `notElem` fired ]


-- * Findings

appliedFinding :: Site -> Erratum -> Finding
appliedFinding site e = at site (finding Warning "W-ERRATUM-APPLIED" (describe site e <> " (" <> errSource e <> ")"))

knownDefectFinding :: Site -> Erratum -> Finding
knownDefectFinding site e = at site (finding Warning "W-ERRATUM-KNOWN"
  ("published text left as printed; scott: " <> describe site e <> " (" <> errSource e <> ")"))

-- | A correction that matches no text of its table.
unusedFinding :: SpecId -> Text -> Maybe Int -> Erratum -> Finding
unusedFinding sid tid pg e = Finding Warning "W-ERRATUM-UNUSED" (Just sid) (Just tid) pg
  ("the annotation's correction " <> tshow (errFrom e) <> " -> " <> tshow (errTo e) <> " matches no text of the table")

describe :: Site -> Erratum -> Text
describe site e = partName (sitePart site) <> " " <> tshow (errFrom e) <> " -> " <> tshow (errTo e)

at :: Site -> Finding -> Finding
at site f = Finding (severity f) (code f) (Just (siteSpec site)) (Just (siteTable site)) (sitePage site) (message f)
