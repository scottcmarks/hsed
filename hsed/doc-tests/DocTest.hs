{-|
Module      : Main
Description : hsed's doctests: every module under src that holds an example, listed when the suite runs
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Runs doctest over every Haskell source file under @src@ that holds an
example (a line that, after its indentation, begins @-- >>>@), the list read
when the suite runs, as hsed-core's suite reads its (a list compiled into the
main goes stale without a rebuild: design docs/spec-as-source.md section 12).
The modules are interpreted with @src@ and @gen@ on the path, so that the
generated modules they import are found; none of those holds an example, and
the modules without one are left out, which keeps the run to the examples'
closure. Until the M3 review's verification hsed had no doctest suite, so its
thirteen examples in nine modules (Hsed.Wire, Hsed.Level0 and
Hsed.Transport.Kext among them) were read and never run.
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
  doctest ("-isrc" : "-igen" : files)

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
