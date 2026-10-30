{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Catalogue
Description : The Test Cases' cases by key (UCT-01, SPF-02:1, UCT-03:1a-1c, ETC-21/adminsp), with their titles and pages
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
other a step range.

At M4a the catalogue holds the ten cases the milestone runs (design 9.1):
M3's UCT-01, SPF-02 case 1, ETC-02 and ETC-03, and UCT-02, UCT-03's steps 1a
to 1c, ETC-04, ETC-08, ETC-15 and the jig's own ETC-21/adminsp; each title
is the Test Cases' heading, a case's (which its outline alone names) its
test's with the case's number and what its Notes say it does, a step range's
its test's with the steps it runs, a variant's its test's with what the
variant does instead.
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

import qualified Data.Aeson as A
import           RIO
import qualified RIO.Char   as C
import qualified RIO.Text   as T


-- $setup
-- >>> :set -XOverloadedStrings

-- * Keys

-- | A test: its family and number.
data TestId
  = UCT !Int
  | SPF !Int
  | ETC !Int
  deriving (Eq, Ord, Show)

-- | What the jig runs of a test: all of it, one of its cases, a range of its
--   steps, or a variant of the jig's own.
data CaseKey = CaseKey
  { ckTest    :: !TestId
  , ckCase    :: !(Maybe Int)
  , ckSteps   :: !(Maybe Text)
  , ckVariant :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show)

-- | A comma-separated list of keys, or why not.
--
-- >>> parseCaseKeys "UCT-01,SPF-02:1"
-- Right [CaseKey {ckTest = UCT 1, ckCase = Nothing, ckSteps = Nothing, ckVariant = Nothing},CaseKey {ckTest = SPF 2, ckCase = Just 1, ckSteps = Nothing, ckVariant = Nothing}]
parseCaseKeys :: Text -> Either Text [CaseKey]
parseCaseKeys t
  | T.null (T.strip t) = Left "no case keys"
  | otherwise          = traverse (parseCaseKey . T.strip) (T.split (== ',') t)

parseCaseKey :: Text -> Either Text CaseKey
parseCaseKey key = do
  let (body, slashVariant) = T.break (== '/') key
      (ident, colonPart)   = T.break (== ':') body
  test <- case T.split (== '-') ident of
    [family, digits] | Just n <- number digits -> testId family n
    _                                          -> refuse
  (c, steps) <- case T.drop 1 colonPart of
    p | T.null colonPart -> Right (Nothing, Nothing)
      | T.null p         -> refuse
      | Just n <- number p -> Right (Just n, Nothing)
      | otherwise        -> Right (Nothing, Just p)
  variant <- case T.drop 1 slashVariant of
    v | T.null slashVariant -> Right Nothing
      | T.null v            -> refuse
      | otherwise           -> Right (Just v)
  pure (CaseKey test c steps variant)
  where
    refuse = Left ("not a case key: " <> key <> " (UCT-01, SPF-02:1, UCT-03:1a-1c, ETC-21/adminsp)")
    number d
      | not (T.null d), T.all C.isDigit d = readMaybe (T.unpack d)
      | otherwise                         = Nothing
    testId family n = case family of
      "UCT" -> Right (UCT n)
      "SPF" -> Right (SPF n)
      "ETC" -> Right (ETC n)
      _     -> refuse

-- | A key as @--tests@ takes it, its number in two digits at least.
--
-- >>> renderCaseKey (CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing)
-- "UCT-03:1a-1c"
renderCaseKey :: CaseKey -> Text
renderCaseKey (CaseKey test c steps variant) =
  family <> "-" <> T.justifyRight 2 '0' (tshow n)
  <> part
  <> maybe "" ("/" <>) variant
  where
    (family, n) = case test of
      UCT i -> ("UCT", i)
      SPF i -> ("SPF", i)
      ETC i -> ("ETC", i)
    part = case (c, steps) of
      (Nothing, Nothing) -> ""
      _                  -> ":" <> maybe "" tshow c <> fromMaybe "" steps

-- | A key as @--tests@ takes it.
instance A.ToJSON CaseKey where
  toJSON = A.String . renderCaseKey


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
    -- 6, SPF-02: its cases 1 and 2 both begin on PDF 52 (printed 51); case 2 is M5's
  , CatalogueEntry (caseOf (SPF 2) 1) "IF-RECV Behavior Tests, Case 1: an IF-RECV while the SD is in Awaiting IF-SEND" 52 51
    -- 7, ETC-02 (PDF 77, printed 76), its heading's dash an en dash
  , CatalogueEntry (whole (ETC 2)) "General \x2013 IF-SEND/IF-RECV Synchronous Protocol" 77 76
    -- 7, ETC-03 (PDF 78, printed 77)
  , CatalogueEntry (whole (ETC 3)) "Invalid IF-SEND Transfer Length" 78 77
    -- 7, ETC-04 (PDF 78-79, printed 77-78), on the same page as ETC-03; its heading's dash a hyphen
  , CatalogueEntry (whole (ETC 4)) "Invalid SessionID - Regular Session" 78 77
    -- 7, ETC-08 (PDF 81, printed 80), its heading's dash an en dash
  , CatalogueEntry (whole (ETC 8)) "Unexpected Token in the Method Parameter List \x2013 Control Session" 81 80
    -- 7, ETC-15 (PDF 88, printed 87), its heading's dash an en dash
  , CatalogueEntry (whole (ETC 15)) "Exceed TPer Properties \x2013 Control Session" 88 87
    -- 7, ETC-21 (PDF 92, printed 91): the jig's variant on the Admin SP, SID with a wrong HostChallenge; the case as
    -- printed, Admin1 on a Manufactured Locking SP, is M5's
  , CatalogueEntry (variantOf (ETC 21) "adminsp") "StartSession with Incorrect HostChallenge, the Admin SP variant: SID with a wrong HostChallenge" 92 91
  ]
  where
    whole t        = CaseKey t Nothing Nothing Nothing
    caseOf t c     = CaseKey t (Just c) Nothing Nothing
    stepsOf t s    = CaseKey t Nothing (Just s) Nothing
    variantOf t v  = CaseKey t Nothing Nothing (Just v)
