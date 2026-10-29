{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Hsed.Spec.Common
Description : The small types every hsedc module shares
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com

Findings, spec identities and errata sets, kept apart so that the grammar, the
rules, the format parser and the generator can all name them without importing
one another (docs/spec-as-source.md section 4.2).
-}
module Hsed.Spec.Common
  ( SpecId (..)
  , matchesSpec
  , RuleName (..)
  , ErrataSet (..)
  , errataSetName
  , parseErrataSet
  , Severity (..)
  , Finding (..)
  , finding
  , renderFinding
  , isError
  ) where

import           RIO
import           RIO.Char (isAlpha, isAlphaNum)
import qualified RIO.Text as T

-- | Which specification an anchor or a table belongs to: @core-2.01@, @opal-2.30@, ...
newtype SpecId = SpecId { specIdText :: Text }
  deriving stock (Eq, Ord, Show)

-- | Does a name given on the command line (@--target core@, @hsedc explain core
--   46@) name this spec: the spec id itself (@core-2.01@), its letters (@core@) or
--   its grammar name (@core201@)? An empty or partial name (@c@) names none.
matchesSpec :: Text -> SpecId -> Bool
matchesSpec name (SpecId s) =
  not (T.null name) && name `elem` [s, T.takeWhile isAlpha s, T.filter isAlphaNum s]

-- | The name of a document-level rule (@typeRow@, @enumValues@, ...).
newtype RuleName = RuleName { ruleNameText :: Text }
  deriving stock (Eq, Ord, Show)

-- | The two errata sets of design section 13.5: the published text as printed,
--   and Scott's corrections (tcg/Core Spec errata.txt).
data ErrataSet = Published | Scott
  deriving stock (Eq, Ord, Show, Enum, Bounded)

errataSetName :: ErrataSet -> Text
errataSetName Published = "published"
errataSetName Scott     = "scott"

parseErrataSet :: Text -> Maybe ErrataSet
parseErrataSet t = lookup t [(errataSetName s, s) | s <- [minBound .. maxBound]]

data Severity = Error | Warning | Note
  deriving stock (Eq, Ord, Show)

-- | One diagnostic. @code@ is the catalogue name (E-SHAPE, C-UID-UNIQUE, W-PINS-NONE, ...);
--   @anchor@ names what it is about (an anchor id such as @T46@ or @5.1.3@, or a rule name);
--   @page@ is the 1-based PDF page when known, so a message can cite
--   @core-2.01.pdf#page=100&T46@.
data Finding = Finding
  { severity :: !Severity
  , code     :: !Text
  , spec     :: !(Maybe SpecId)
  , anchor   :: !(Maybe Text)
  , page     :: !(Maybe Int)
  , message  :: !Text
  }
  deriving stock (Eq, Show)

-- | A finding without a location.
finding :: Severity -> Text -> Text -> Finding
finding sev c msg = Finding sev c Nothing Nothing Nothing msg

isError :: Finding -> Bool
isError = (== Error) . severity

-- | @E-SHAPE core-2.01.pdf#page=100&T46: ...@, the form the design's acceptance tests grep for.
renderFinding :: Finding -> Text
renderFinding f = T.concat [code f, " ", location, ": ", message f]
  where
    location = case (spec f, page f, anchor f) of
      (Just s, Just p, Just a) -> specIdText s <> ".pdf#page=" <> tshow p <> "&" <> a
      (Just s, Just p, Nothing) -> specIdText s <> ".pdf#page=" <> tshow p
      (Just s, Nothing, Just a) -> specIdText s <> "#" <> a
      (Just s, Nothing, Nothing) -> specIdText s
      (Nothing, _, Just a) -> a
      (Nothing, _, Nothing) -> "-"
