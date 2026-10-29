{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Core.Code
Description : The runtime form of a code table: the values each row assigns, and what its other cells say
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A code table gives meanings to the values of one field (design
docs/spec-as-source.md section 3.4, the @codes@ and @statusCodes@ rules: Core
2.01 Tables 15, 16, 19, 25, 26, 40, 166, 179 and 235). Each row assigns a value
or a range of values, its 'Span', and says what they mean in its other cells,
kept as printed. The spec prints a value in hex (@0x07FF@, @0003h@) or in
decimal, a range as @a-b@, @a – b@ or @a to b@, every value above one as
@>0x00@, and the values no other row assigns as @All others@; the generated
modules (@Hsed.Gen.\<Spec\>.Codes@) hold each table as a list of 'CodeRow's.
-}
module Hsed.Core.Code
  ( -- * Spans
    Span (..)
  , inSpan
    -- * Rows
  , CodeRow (..)
  , codeRow
  , codeRowsNamed
  ) where

import           RIO
import qualified RIO.List as L


-- $setup
-- >>> :set -XOverloadedStrings

-- | The values one row of a code table assigns.
data Span
  = One !Integer              -- ^ @0x01@
  | Between !Integer !Integer -- ^ @0x0002-0x07FF@, @10 to 23@: both bounds included
  | Above !Integer            -- ^ @>0x00@: every greater value
  | Others                    -- ^ @All others@: every value no other row of the table assigns
  deriving (Eq, Ord, Show)

-- | Whether a span assigns a value. 'Others' assigns none by itself: it
--   takes what the table's other rows leave ('codeRow').
--
-- >>> map (inSpan (Between 0x0002 0x07ff)) [0x0001, 0x0002, 0x07ff, 0x0800]
-- [False,True,True,False]
-- >>> inSpan (Above 0x00) 0x01
-- True
inSpan :: Span -> Integer -> Bool
inSpan s v = case s of
  One a       -> v == a
  Between a b -> a <= v && v <= b
  Above a     -> v > a
  Others      -> False

-- | One row of a code table: the values it assigns and its other cells, in
--   column order, as printed.
data CodeRow = CodeRow
  { codeSpan  :: !Span
  , codeCells :: ![Text]
  }
  deriving (Eq, Show)

-- | The row of a code table that assigns a value: the first whose span
--   assigns it, else the table's 'Others' row.
--
-- >>> let protocols = [CodeRow (One 0x00) ["See [6]"], CodeRow (One 0x01) ["Defined in this document"], CodeRow Others ["See [6]"]]
-- >>> codeCells <$> codeRow protocols 0x01
-- Just ["Defined in this document"]
-- >>> codeCells <$> codeRow protocols 0x07
-- Just ["See [6]"]
codeRow :: [CodeRow] -> Integer -> Maybe CodeRow
codeRow rows v = L.find (\r -> inSpan (codeSpan r) v) rows <|> L.find ((== Others) . codeSpan) rows

-- | The rows whose first cell (their name or meaning) is the given text, in order.
--
-- >>> map codeSpan (codeRowsNamed [CodeRow (One 0x0000) ["Data"], CodeRow (One 0x8001) ["Credit Control"]] "Credit Control")
-- [One 32769]
codeRowsNamed :: [CodeRow] -> Text -> [CodeRow]
codeRowsNamed rows name = filter ((== Just name) . listToMaybe . codeCells) rows
