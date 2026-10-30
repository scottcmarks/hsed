{-|
Module      : Main
Description : hsed-core's doctests: every module under src, listed when the suite runs
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Runs doctest over every Haskell source file, @.hs@ or @.lhs@, under @src@, the
library's one source directory, read when the suite runs (stack runs it in the
package's directory). Until the M2d review this file was one pragma running
doctest-discover as its preprocessor (@-F -pgmF@), which wrote the list into
the compiled main. GHC runs a preprocessor on every build but compiles the
module again only when the file's own text, its imports or the flags that
shape its code change: a module added under @src@ or moved changed none of
these, nor did a touch or @--force-dirty@, so a work dir built before kept the
old list (CI ran 63 of M2c's 113 examples, and after M2d's rename the list
named modules that no longer exist): design docs/spec-as-source.md section 12.
-}
module Main (main) where

import           Control.Monad    (filterM)
import           Data.List        (sort)
import           System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath  (takeExtension, (</>))
import           Test.DocTest     (doctest)


main :: IO ()
main = haskellFiles "src" >>= doctest . ("-isrc" :)

-- | Every @.hs@ or @.lhs@ file under a directory, at any depth, sorted.
haskellFiles :: FilePath -> IO [FilePath]
haskellFiles dir = do
  entries <- map (dir </>) . sort <$> listDirectory dir
  files   <- filterM doesFileExist entries
  dirs    <- filterM doesDirectoryExist entries
  below   <- concat <$> mapM haskellFiles dirs
  pure (filter ((`elem` [".hs", ".lhs"]) . takeExtension) files <> below)
