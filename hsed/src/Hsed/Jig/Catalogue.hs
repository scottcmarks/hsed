{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Catalogue
Description : The Test Cases' cases by key (UCT-01, SPF-02:1, UCT-03:1a-1c, ETC-21/adminsp, SPF-12:1/no-media), with their titles and pages
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The TCG Storage Opal Family Test Cases 1.02 (design section 9.1) name a test
by its family and number (Use Case Tests UCT, Specific Functionality SPF,
Error Cases ETC), some tests by case (SPF-02's cases 1 and 2), and the jig
runs some by step range (UCT-03's steps 1a to 1c) or in a variant of its own
(@ETC-21/adminsp@): a 'CaseKey'. The catalogue is hand-entered, each entry
with its PDF and printed pages: the Test Cases PDF is untagged, and its case
headings are in its outline alone.

@--tests@ is a comma-separated list of keys ('parseCaseKeys'): a family and
a two-digit number, then @:@ and a case number or a step range, then @/@ and
a variant, each optional; a part after @:@ of digits alone is a case, any
other a step range. The keys are "Hsed.CaseKey"'s since M5c, re-exported
here, so that the simulator and the laws, which name the cases they serve by
key, import no module of the jig (design 11 M5c).

At M4a the catalogue holds the ten cases the milestone runs (design 9.1):
M3's UCT-01, SPF-02 case 1, ETC-02 and ETC-03, and UCT-02, UCT-03's steps 1a
to 1c, ETC-04, ETC-08, ETC-15 and the jig's own ETC-21/adminsp; since M5b
(design 11 M5b) the Locking SP's eight, which the simulator alone runs:
SPF-02 case 2, SPF-11, SPF-12 case 1's steps 1 to 5 (the jig's variant
@no-media@, Scott's answer Q2), ETC-07, ETC-09, ETC-14, ETC-20 and ETC-21 as
printed. Each title is the Test Cases' heading, a case's (which its outline
alone names) its test's with the case's number and what its Notes say it
does, a step range's its test's with the steps it runs, a variant's its
test's with what the variant does instead.
-}
module Hsed.Jig.Catalogue
  ( -- * Keys
    TestId (..)
  , CaseKey (..)
  , parseCaseKeys
  , renderCaseKey
    -- * The catalogue
  , CatalogueEntry (..)
  , catalogue
  ) where

import           RIO

import           Hsed.CaseKey (CaseKey (..), TestId (..), parseCaseKeys, renderCaseKey)


-- * The catalogue

-- | A case the jig knows: its key, its title, its first page in the PDF and
--   as printed.
data CatalogueEntry = CatalogueEntry
  { ceKey     :: !CaseKey
  , ceTitle   :: !Text
  , cePdf     :: !Int
  , cePrinted :: !Int
  }
  deriving (Eq, Show)

-- | Every case the jig runs, in the Test Cases' order (Use Case Tests,
--   Specific Functionality, Error Cases: sections 5 to 7), each with the
--   page of its heading in the PDF and as printed (Test Cases 1.02, the PDF's
--   outline and its page footers).
catalogue :: [CatalogueEntry]
catalogue =
  [ -- 5, UCT-01 (PDF 29-31, printed 28-30)
    CatalogueEntry (whole (UCT 1)) "Level 0 Discovery" 29 28
    -- 5, UCT-02 (PDF 31-32, printed 30-31)
  , CatalogueEntry (whole (UCT 2)) "Properties" 31 30
    -- 5, UCT-03 (PDF 32-33, printed 31-32): its step 1 as far as 1c, which reads the MSID; 1d-1f set the SID PIN
  , CatalogueEntry (stepsOf (UCT 3) "1a-1c") "Taking Ownership of an SD, steps 1a to 1c: a session to the Admin SP, the MSID read, the session closed" 32 31
    -- 6, SPF-02: its cases 1 and 2 both begin on PDF 52 (printed 51), case 2's steps on PDF 53
  , CatalogueEntry (caseOf (SPF 2) 1) "IF-RECV Behavior Tests, Case 1: an IF-RECV while the SD is in Awaiting IF-SEND" 52 51
  , CatalogueEntry (caseOf (SPF 2) 2) "IF-RECV Behavior Tests, Case 2: an IF-RECV with an insufficient transfer length" 52 51
    -- 6, SPF-11 (PDF 64-65, printed 63-64)
  , CatalogueEntry (whole (SPF 11)) "Stack Reset" 64 63
    -- 6, SPF-12 case 1 (PDF 65-66, printed 64-65): its steps 1 to 5; 6 and 7, media I/O, are M6's (design 8.5, Scott's
    -- answer Q2), the jig's variant no-media
  , CatalogueEntry (caseVariantOf (SPF 12) 1 "no-media") "TPer Reset, Case 1, steps 1 to 5: no media I/O" 65 64
    -- 7, ETC-02 (PDF 77, printed 76), its heading's dash an en dash
  , CatalogueEntry (whole (ETC 2)) "General \x2013 IF-SEND/IF-RECV Synchronous Protocol" 77 76
    -- 7, ETC-03 (PDF 78, printed 77)
  , CatalogueEntry (whole (ETC 3)) "Invalid IF-SEND Transfer Length" 78 77
    -- 7, ETC-04 (PDF 78-79, printed 77-78), on the same page as ETC-03; its heading's dash a hyphen
  , CatalogueEntry (whole (ETC 4)) "Invalid SessionID - Regular Session" 78 77
    -- 7, ETC-07 (PDF 80, printed 79), its heading's dash an en dash
  , CatalogueEntry (whole (ETC 7)) "Unexpected Token Outside of Method \x2013 Control Session" 80 79
    -- 7, ETC-08 (PDF 81, printed 80), its heading's dash an en dash
  , CatalogueEntry (whole (ETC 8)) "Unexpected Token in the Method Parameter List \x2013 Control Session" 81 80
    -- 7, ETC-09 (PDF 81-82, printed 80-81), on the same page as ETC-08
  , CatalogueEntry (whole (ETC 9)) "Exceeding Transaction Limit" 81 80
    -- 7, ETC-14 (PDF 87, printed 86), its heading's dash an en dash
  , CatalogueEntry (whole (ETC 14)) "Exceed TPer Properties \x2013 Regular Session" 87 86
    -- 7, ETC-15 (PDF 88, printed 87), its heading's dash an en dash
  , CatalogueEntry (whole (ETC 15)) "Exceed TPer Properties \x2013 Control Session" 88 87
    -- 7, ETC-20 (PDF 91-92, printed 90-91)
  , CatalogueEntry (whole (ETC 20)) "StartSession on Inactive Locking SP" 91 90
    -- 7, ETC-21 (PDF 92, printed 91): as printed, Admin1 to a Manufactured Locking SP with a wrong HostChallenge; then
    -- the jig's variant on the Admin SP, SID with a wrong HostChallenge
  , CatalogueEntry (whole (ETC 21)) "StartSession with Incorrect HostChallenge" 92 91
  , CatalogueEntry (variantOf (ETC 21) "adminsp") "StartSession with Incorrect HostChallenge, the Admin SP variant: SID with a wrong HostChallenge" 92 91
  ]
  where
    whole t              = CaseKey t Nothing Nothing Nothing
    caseOf t c           = CaseKey t (Just c) Nothing Nothing
    stepsOf t s          = CaseKey t Nothing (Just s) Nothing
    variantOf t v        = CaseKey t Nothing Nothing (Just v)
    caseVariantOf t c v  = CaseKey t (Just c) Nothing (Just v)
