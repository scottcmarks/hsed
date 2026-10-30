{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Files
Description : Files the jig writes, owned by whoever ran sudo, written as root without following a planted link
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The jig runs as root against a drive (design 9.3: hsed's own gate, the
kext's build 625 having none), and what it writes then (its reports under
@build/report@, the Level 0 capture under @spec/fixtures@) must stay the
user's, or the next run without sudo cannot write there: a directory it
creates and a file it writes are handed to the user sudo names
(@SUDO_UID@, @SUDO_GID@) when the jig runs as root, and left as they are
otherwise. Never @sudo stack@: that leaves a root-owned @.stack-work@.

As root it writes nothing a link planted by another process could redirect:

* a path with a @..@ component is refused, so that a directory it names
  is the one it seems to be, and only the directories it creates are
  handed over, never one that was there;
* a directory on the path that is a symbolic link is refused unless root
  owns the link (the system's own, @/tmp@ on macOS), a link of the user's
  being one their processes could have planted;
* a file is written to a new name beside the target, created exclusively
  (@O_CREAT|O_EXCL@, which no link satisfies), handed over through its open
  descriptor (@fchown@), then renamed over the target, so that a link at
  the target is replaced, not written through; a directory it creates is
  handed over with @lchown@.

A file is written so whoever writes it ('Writer'): 'writeOwned' and
'createDirOwned' as the process is, the tests as any writer.
-}
module Hsed.Jig.Files
  ( writeOwned
  , createDirOwned
  , runningAsRoot
    -- * As a given writer
  , Writer (..)
  , currentWriter
  , writeAs
  , createDirAs
  ) where

import qualified RIO.ByteString     as B
import qualified RIO.List           as L
import           RIO
import           RIO.FilePath       (joinPath, splitDirectories, takeDirectory, takeFileName, (</>))
#if defined(mingw32_HOST_OS)
import           RIO.Directory      (createDirectoryIfMissing)
#else
import           RIO.Directory      (createDirectory, doesDirectoryExist)
import           System.Environment (lookupEnv)
import           System.IO.Error    (isAlreadyExistsError)
import           System.Posix.Files (fileOwner, getSymbolicLinkStatus, isDirectory, isSymbolicLink, removeLink, rename,
                                     setFdOwnerAndGroup, setSymbolicLinkOwnerAndGroup)
import           System.Posix.IO    (OpenFileFlags (..), OpenMode (..), defaultFileFlags, fdToHandle, openFd)
import           System.Posix.Process (getProcessID)
import           System.Posix.User  (getEffectiveUserID)
#endif


-- | Who writes: whether as root (the checks above apply), and to whom what
--   is written is handed, a user and a group (sudo's), if to anyone.
data Writer = Writer
  { wPrivileged :: !Bool
  , wOwner      :: !(Maybe (Integer, Integer))
  }
  deriving (Eq, Show)

-- | The process as a writer: root or not, and, as root under sudo, the user
--   and group sudo names.
currentWriter :: IO Writer
currentWriter = do
  root <- runningAsRoot
  owner <- if root then sudoer else pure Nothing
  pure (Writer root owner)

-- | Write a file, creating its directory ('createDirOwned'), then hand it
--   to the user who ran sudo.
writeOwned :: FilePath -> ByteString -> IO ()
writeOwned path bytes = currentWriter >>= \w -> writeAs w path bytes

-- | Create a directory and its missing parents, each handed to the user who
--   ran sudo.
createDirOwned :: FilePath -> IO ()
createDirOwned dir = currentWriter >>= (`createDirAs` dir)

-- | Whether the process runs as root (its effective user id 0); never on
--   Windows.
runningAsRoot :: IO Bool
#if defined(mingw32_HOST_OS)
runningAsRoot = pure False
#else
runningAsRoot = (== 0) <$> getEffectiveUserID
#endif

-- | Write a file as a writer: its directory created ('createDirAs'), the
--   bytes written to a new file beside it, created exclusively and handed
--   over through its descriptor, then renamed over the file.
writeAs :: Writer -> FilePath -> ByteString -> IO ()
#if defined(mingw32_HOST_OS)
writeAs _ path bytes = createDirectoryIfMissing True (takeDirectory path) >> writeFileBinary path bytes
#else
writeAs w path bytes = do
  noParent w path
  createDirAs w (takeDirectory path)
  pid <- getProcessID
  let fresh = takeDirectory path </> ("." <> takeFileName path <> ".hsed-" <> show pid)
  fd <- tryIO (openFd fresh WriteOnly (Just 0o666) defaultFileFlags { exclusive = True }) >>=
          either (\e -> throwString (path <> ": " <> fresh <> " could not be created beside it: " <> displayException e)) pure
  h <- fdToHandle fd
  (do for_ (wOwner w) (\(u, g) -> setFdOwnerAndGroup fd (fromInteger u) (fromInteger g))
      B.hPut h bytes
      hClose h
      rename fresh path)
    `onException` (hClose h >> tryIO (removeLink fresh))
#endif

-- | Create a directory and its missing parents as a writer: each it creates
--   handed over, each that was there left as it was; as root, a link among
--   them that root does not own refused.
createDirAs :: Writer -> FilePath -> IO ()
#if defined(mingw32_HOST_OS)
createDirAs _ dir = createDirectoryIfMissing True dir

-- | No sudo on Windows.
sudoer :: IO (Maybe (Integer, Integer))
sudoer = pure Nothing
#else
createDirAs w dir = noParent w dir >> traverse_ made prefixes
  where
    -- the directory and its parents, the outermost first
    prefixes = map joinPath (drop 1 (L.inits (splitDirectories dir)))
    made d = tryIO (getSymbolicLinkStatus d) >>= \case
      Right st
        | isDirectory st -> pure ()
        | isSymbolicLink st, wPrivileged w, fileOwner st /= 0 ->
            throwString (d <> ": a symbolic link root does not own, which the jig does not follow as root")
        | isSymbolicLink st -> doesDirectoryExist d >>= (`unless` throwString (d <> ": not a directory"))
        | otherwise -> throwString (d <> ": not a directory")
      Left _ -> tryIO (createDirectory d) >>= \case
        Right () -> for_ (wOwner w) (\(u, g) -> setSymbolicLinkOwnerAndGroup d (fromInteger u) (fromInteger g))
        Left e
          | isAlreadyExistsError e -> made d  -- another process made it first: checked again, not handed over
          | otherwise              -> throwIO e

-- | As root, refuse a path with a @..@ component.
noParent :: Writer -> FilePath -> IO ()
noParent w path = when (wPrivileged w && ".." `elem` splitDirectories path) $
  throwString (path <> ": a path with .. in it, which the jig does not write as root")

-- | The user and group sudo names, if it names them.
sudoer :: IO (Maybe (Integer, Integer))
sudoer = do
  uid <- lookupEnv "SUDO_UID"
  gid <- lookupEnv "SUDO_GID"
  pure ((,) <$> (readMaybe =<< uid) <*> (readMaybe =<< gid))
#endif
