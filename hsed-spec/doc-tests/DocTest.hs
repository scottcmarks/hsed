{-|
Module      : Main
Description : hsed-spec's doctests: every module under src that holds an example, listed when the suite runs
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Runs doctest over every Haskell source file under @src@ that holds an
example (a line that, after its indentation, begins @-- >>>@), the list read
when the suite runs, as hsed's and hsed-core's suites read theirs (a list
compiled into the main goes stale without a rebuild: design
docs/spec-as-source.md section 12). The modules are interpreted with @src@ on
the path; a module whose examples have Text literals carries a @$setup@
block that sets @OverloadedStrings@, as hsed's do. Until the M4b review
hsed-spec had no doctest suite, so its examples (the Readers' text helpers,
the sig rule's type parser, Gen's ascii among them) were read and never run.
-}
module Main (main) where

import           Control.Monad         (filterM)
import qualified Data.ByteString.Char8 as B
import           Data.List             (sort)
import           System.Directory      (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath       (takeExtension, (</>))
import           Test.DocTest          (doctest)


main :: IO ()
main = do
  files <- haskellFiles "src" >>= filterM holdsExample
  doctest ("-isrc" : files)

-- | Whether a source file holds an example: a line that, after its
--   indentation, begins @-- >>>@.
holdsExample :: FilePath -> IO Bool
holdsExample path = any (B.isPrefixOf (B.pack "-- >>>") . B.dropWhile (== ' ')) . B.lines <$> B.readFile path

-- | Every @.hs@ or @.lhs@ file under a directory, at any depth, sorted.
haskellFiles :: FilePath -> IO [FilePath]
haskellFiles dir = do
  entries <- map (dir </>) . sort <$> listDirectory dir
  files   <- filterM doesFileExist entries
  dirs    <- filterM doesDirectoryExist entries
  below   <- concat <$> mapM haskellFiles dirs
  pure (filter ((`elem` [".hs", ".lhs"]) . takeExtension) files <> below)
