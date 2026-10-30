{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.FilesSpec
Description : Test specifications for Hsed.Jig.Files: files written as root follow no planted link and hand over only what they create
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What the jig does when it writes as root (design 9.3), run here as the user
with a writer that says it is root: a path with @..@ refused; a directory
on the path that is a link the user owns refused, nothing written through
it; a link planted at the file replaced, its target untouched; the
directories it creates and the file handed over (to a group of the user's
other than the one they would get), the directories that were there left
as they were. A writer that is not root writes as the jig always did.
-}
module Hsed.Jig.FilesSpec (spec) where

import qualified RIO.List           as L
import           RIO
import           RIO.Directory      (createDirectory, doesFileExist, listDirectory)
import           RIO.FilePath       ((</>))
import           System.Posix.Files (createSymbolicLink, fileGroup, getSymbolicLinkStatus, isSymbolicLink)
import           System.Posix.User  (getEffectiveGroupID, getEffectiveUserID, getGroups)
import           Test.Hspec

import           Hsed.Jig.Files


-- | A writer that says it is root, handing nothing over.
asRoot :: Writer
asRoot = Writer True Nothing

-- | A writer that is not root.
asUser :: Writer
asUser = Writer False Nothing

spec :: Spec
spec = describe "Hsed.Jig.Files" $ do
  it "writes a file and the directories it needs" $ withSystemTempDirectory "hsed-files" $ \tmp -> do
    for_ [asUser, asRoot] $ \w -> do
      writeAs w (tmp </> "a" </> "b" </> "f.bin") "bytes"
      readFileBinary (tmp </> "a" </> "b" </> "f.bin") `shouldReturn` "bytes"
    writeAs asRoot (tmp </> "a" </> "b" </> "f.bin") "again"
    readFileBinary (tmp </> "a" </> "b" </> "f.bin") `shouldReturn` "again"

  it "as root, refuses a path with .. in it; not as root" $ withSystemTempDirectory "hsed-files" $ \tmp -> do
    createDirectory (tmp </> "a")
    writeAs asRoot (tmp </> "a" </> ".." </> "f") "x" `shouldThrow` anyException
    createDirAs asRoot (tmp </> "a" </> "..") `shouldThrow` anyException
    doesFileExist (tmp </> "f") `shouldReturn` False
    writeAs asUser (tmp </> "a" </> ".." </> "f") "x"
    doesFileExist (tmp </> "f") `shouldReturn` True

  it "as root, refuses a directory on the path that is a link the user owns, writing nothing through it" $
    withSystemTempDirectory "hsed-files" $ \tmp -> do
      createDirectory (tmp </> "real")
      createSymbolicLink (tmp </> "real") (tmp </> "link")
      writeAs asRoot (tmp </> "link" </> "f") "x" `shouldThrow` anyException
      createDirAs asRoot (tmp </> "link" </> "sub") `shouldThrow` anyException
      doesFileExist (tmp </> "real" </> "f") `shouldReturn` False
      writeAs asUser (tmp </> "link" </> "f") "x"
      doesFileExist (tmp </> "real" </> "f") `shouldReturn` True

  it "replaces a link planted at the file, its target untouched" $ withSystemTempDirectory "hsed-files" $ \tmp -> do
    writeFileBinary (tmp </> "victim") "keep"
    createSymbolicLink (tmp </> "victim") (tmp </> "out.bin")
    writeAs asRoot (tmp </> "out.bin") "new"
    readFileBinary (tmp </> "victim") `shouldReturn` "keep"
    readFileBinary (tmp </> "out.bin") `shouldReturn` "new"
    isSymbolicLink <$> getSymbolicLinkStatus (tmp </> "out.bin") `shouldReturn` False

  it "leaves nothing beside the file when the write fails" $ withSystemTempDirectory "hsed-files" $ \tmp -> do
    createDirectory (tmp </> "out.bin")
    writeAs asRoot (tmp </> "out.bin") "x" `shouldThrow` anyException
    L.sort <$> listDirectory tmp `shouldReturn` ["out.bin"]

  it "hands over the directories it creates and the file, never a directory that was there" $
    withSystemTempDirectory "hsed-files" $ \tmp -> do
      me <- getEffectiveUserID
      own <- getEffectiveGroupID
      others <- filter (/= own) <$> getGroups
      createDirectory (tmp </> "old")
      was <- fileGroup <$> getSymbolicLinkStatus (tmp </> "old")
      case L.find (/= was) others of
        Nothing -> pendingWith "the user belongs to no group but the one a directory gets: nothing to hand over to"
        Just g  -> do
          writeAs (Writer True (Just (toInteger me, toInteger g))) (tmp </> "old" </> "new" </> "f.bin") "x"
          groups <- traverse (fmap fileGroup . getSymbolicLinkStatus)
                      [tmp </> "old", tmp </> "old" </> "new", tmp </> "old" </> "new" </> "f.bin"]
          groups `shouldBe` [was, g, g]
