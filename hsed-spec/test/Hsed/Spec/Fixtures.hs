{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Fixtures
Description : The Core 2.01 fixtures the hsed-spec tests share
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@test/fixtures/core-2.01-type-rows.txt@ holds the 100 Type table rows as the
PDF prints them (@T46|AC_element|00 00 00 05 00 00 08 01|List_Type, *, ACE_expression@,
Table 75's @Fractoin@ included) and @core-2.01-table-names.txt@ the 46 table
names of Table 240, both written from @spec/ir/core-2.01.json@.
-}
module Hsed.Spec.Fixtures
  ( Row (..)
  , readRows
  , readLines
  , coreEnv
  , core
  ) where

import qualified Data.Text      as T
import qualified Data.Text.Read as TR
import           RIO

import           Hsed.Spec.Common
import           Hsed.Spec.Format


-- | One published Type table row.
data Row = Row { rowTable :: !Int, rowName :: !Text, rowUID :: !Text, rowFormat :: !Text }
  deriving (Eq, Show)

readRows :: IO [Row]
readRows = mapMaybe row <$> readLines "test/fixtures/core-2.01-type-rows.txt"
  where
    row l = case T.splitOn "|" l of
      [tid, n, u, f] -> either (const Nothing) (\(k, _) -> Just (Row k n u f)) (TR.decimal (T.drop 1 tid))
      _              -> Nothing

-- | The non-empty lines of a file.
readLines :: FilePath -> IO [Text]
readLines path = filter (not . T.null) . T.lines <$> readFileUtf8 path

-- | The Type objects of the published rows and the tables of T240.
coreEnv :: IO Env
coreEnv = do
  rows   <- readRows
  tables <- readLines "test/fixtures/core-2.01-table-names.txt"
  pure (mkEnv [ (rowName r, Just (rowTable r)) | r <- rows ] tables)

core :: SpecId
core = SpecId "core-2.01"
