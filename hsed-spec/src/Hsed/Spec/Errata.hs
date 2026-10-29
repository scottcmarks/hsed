{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Errata
Description : The two errata sets as data, and their application to spec text
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design decision 13.5: the compiler builds from the published text as printed
('Published') or from Scott's corrections ('Scott'), each correction a plain
substitution scoped to one part of one table and cited to where it is recorded
(tcg/Core Spec errata.txt; the regen report of docs/evidence/2026-10-25). Each
application is reported, so that @--errata scott@ lists what it corrected
(@W-ERRATUM-APPLIED@) and @--errata published@ lists the defects it left in
(@W-ERRATUM-KNOWN@) beside whatever else fails on them, such as @E-FMT-RESOLVE@
on Table 75's @Fractoin@ (design example E3).

Two of hsed's differences from the PDF are not errata and are not here: the
captions numbered 147 and 160 in TypeUIDs.hs are hsed's own transcription
slips (the PDF has 157 and 161).
-}
module Hsed.Spec.Errata
  ( -- * Errata
    Erratum (..)
  , Part (..)
  , corrections
    -- * Application
  , Site (..)
  , applyErrata
  , errata
  , appliedFinding
  , knownDefectFinding
  ) where

import qualified Data.Text        as T
import           RIO

import           Hsed.Spec.Common


-- | Which part of a table a correction applies to.
data Part = FormatCell | Caption | HeaderCell | RowLabel
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One correction: replace every occurrence of 'errFrom' by 'errTo' in the
--   given part of the given table.
data Erratum = Erratum
  { errSpec   :: !SpecId
  , errTable  :: !Text    -- ^ anchor id, @T75@
  , errPart   :: !Part
  , errFrom   :: !Text
  , errTo     :: !Text
  , errSource :: !Text    -- ^ where the correction is recorded
  }
  deriving (Eq, Show)

-- | The corrections of a set. The published text has none by definition.
corrections :: ErrataSet -> [Erratum]
corrections Published = []
corrections Scott     = scott

core :: SpecId
core = SpecId "core-2.01"

errataTxt, regenReport :: Text
errataTxt   = "tcg/Core Spec errata.txt"
regenReport = "docs/evidence/2026-10-25/extract/regen_report.txt"

-- | Scott's corrections to Core 2.01, seeded from tcg/Core Spec errata.txt and the
--   deliberate fixes hsed's hand transcription made (regen report, class
--   "deliberate typo fix in hsed").
scott :: [Erratum]
scott =
  [ Erratum core "T75"  FormatCell "Fractoin"    "Fraction"     (errataTxt <> ": Table 75 \"Fractoin\" should be \"Fraction\"")
  , Erratum core "T101" RowLabel   "Verify Eror" "Verify Error" (errataTxt <> ": Table 101 \"Eror\" should be \"Error\"")
  , Erratum core "T61"  HeaderCell "Operator"    "Associated Value"  (regenReport <> ": Table 61 header")
  , Erratum core "T128" HeaderCell "Set Value"   "Enumeration Value" (regenReport <> ": Table 128 header")
  ]
  <> map objectRef
       [ ("T72",  "Certificates", errataTxt <> ": Table 72")
       , ("T76",  "Column",       regenReport <> ": Table 76 caption")
       , ("T112", "MethodID",     regenReport <> ": Table 112 caption")
       , ("T131", "SPTemplates",  errataTxt <> ": Table 131")
       , ("T140", "Table",        errataTxt <> ": 5.1.3.76")
       , ("T142", "Template",     errataTxt <> ": 5.1.3.78")
       , ("T144", "Type",         errataTxt <> ": 5.1.3.80")
       ]
  where
    objectRef (table, noun, source) =
      Erratum core table Caption (noun <> "_object _ref") (noun <> "_object_ref") source


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
appliesAt site e = errSpec e == siteSpec site && errTable e == siteTable site && errPart e == sitePart site

-- | Correct a cell, caption, header or label: the corrected text and the errata
--   that fired, in the order they are listed.
applyErrata :: ErrataSet -> Site -> Text -> (Text, [Erratum])
applyErrata errataSet site text = foldl' step (text, []) (filter (appliesAt site) (corrections errataSet))
  where
    step (t, fired) e
      | errFrom e `T.isInfixOf` t = (T.replace (errFrom e) (errTo e) t, fired <> [e])
      | otherwise                 = (t, fired)

-- | 'applyErrata' with its report: under 'Scott' one @W-ERRATUM-APPLIED@ per
--   correction made; under 'Published' the text unchanged and one
--   @W-ERRATUM-KNOWN@ per correction that would have been made.
errata :: ErrataSet -> Site -> Text -> (Text, [Finding])
errata Published site text = (text, map (knownDefectFinding site) (snd (applyErrata Scott site text)))
errata errataSet site text = second (map (appliedFinding site)) (applyErrata errataSet site text)

appliedFinding :: Site -> Erratum -> Finding
appliedFinding site e = at site (finding Warning "W-ERRATUM-APPLIED" (describe e <> " (" <> errSource e <> ")"))

knownDefectFinding :: Site -> Erratum -> Finding
knownDefectFinding site e = at site (finding Warning "W-ERRATUM-KNOWN" ("published text left as printed; scott: " <> describe e))

describe :: Erratum -> Text
describe e = partName (errPart e) <> " " <> tshow (errFrom e) <> " -> " <> tshow (errTo e)

partName :: Part -> Text
partName FormatCell = "Format"
partName Caption    = "caption"
partName HeaderCell = "header"
partName RowLabel   = "label"

at :: Site -> Finding -> Finding
at site f = f { spec = Just (siteSpec site), anchor = Just (siteTable site), page = sitePage site }
