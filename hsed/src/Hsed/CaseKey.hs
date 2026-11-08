{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.CaseKey
Description : A Test Cases case by key (UCT-01, SPF-02:1, UCT-03:1a-1c, ETC-21/adminsp, SPF-12:1/no-media): read, written, its JSON
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The TCG Storage Opal Family Test Cases 1.02 (design section 9.1) name a test
by its family and number (Use Case Tests UCT, Specific Functionality SPF,
Error Cases ETC), some tests by case (SPF-02's cases 1 and 2), and the jig
runs some by step range (UCT-03's steps 1a to 1c) or in a variant of its own
(@ETC-21/adminsp@): a 'CaseKey'. @--tests@ is a comma-separated list of keys
('parseCaseKeys'): a family and a two-digit number, then @:@ and a case
number or a step range, then @/@ and a variant, each optional; a part after
@:@ of digits alone is a case, any other a step range.

Moved here from "Hsed.Jig.Catalogue", which re-exports them and keeps the
catalogue itself, at M5c (design 11 M5c): the simulator's behaviours and the
laws name the cases they serve by key ("Hsed.Sim.Base", "Hsed.Sim.Behaviour",
"Hsed.Sim.SessionManager", "Hsed.Law" and its modules), and hsed-sim, which
serves the simulator over the LAN leg, imports no module of the jig
("Hsed.CapabilitySpec" walks its imports).
-}
module Hsed.CaseKey
  ( TestId (..)
  , CaseKey (..)
  , parseCaseKeys
  , parseCaseKey
  , renderCaseKey
  ) where

import qualified Data.Aeson as A
import           RIO
import qualified RIO.Char   as C
import qualified RIO.Text   as T


-- $setup
-- >>> :set -XOverloadedStrings

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

-- | One key, or why not.
--
-- >>> parseCaseKey "SPF-12:1/no-media"
-- Right (CaseKey {ckTest = SPF 12, ckCase = Just 1, ckSteps = Nothing, ckVariant = Just "no-media"})
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
    refuse = Left ("not a case key: " <> key <> " (UCT-01, SPF-02:1, UCT-03:1a-1c, ETC-21/adminsp, SPF-12:1/no-media)")
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
