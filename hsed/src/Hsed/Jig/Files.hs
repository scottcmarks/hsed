{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Files
Description : Files the jig writes, owned by whoever ran sudo, and whether it runs as root
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
-}
module Hsed.Jig.Files
  ( writeOwned
  , createDirOwned
  , runningAsRoot
  ) where

import qualified RIO.List           as L
import           RIO
import           RIO.Directory      (createDirectoryIfMissing, doesDirectoryExist)
import           RIO.FilePath       (joinPath, splitDirectories, takeDirectory)
#if !defined(mingw32_HOST_OS)
import           System.Environment (lookupEnv)
import           System.Posix.Files (setOwnerAndGroup)
import           System.Posix.User  (getEffectiveUserID)
#endif


-- | Write a file, creating its directory ('createDirOwned'), then hand it
--   to the user who ran sudo.
writeOwned :: FilePath -> ByteString -> IO ()
writeOwned path bytes = do
  createDirOwned (takeDirectory path)
  writeFileBinary path bytes
  handOver path

-- | Create a directory and its missing parents, each handed to the user who
--   ran sudo.
createDirOwned :: FilePath -> IO ()
createDirOwned dir = do
  missing <- filterM (fmap not . doesDirectoryExist) prefixes
  createDirectoryIfMissing True dir
  traverse_ handOver missing
  where
    -- the directory and its parents, the outermost first
    prefixes = map joinPath (drop 1 (L.inits (splitDirectories dir)))

-- | Whether the process runs as root (its effective user id 0); never on
--   Windows.
runningAsRoot :: IO Bool
#if defined(mingw32_HOST_OS)
runningAsRoot = pure False
#else
runningAsRoot = (== 0) <$> getEffectiveUserID
#endif

-- | Hand a path to the user who ran sudo, when running as root under sudo.
handOver :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
handOver _ = pure ()
#else
handOver path = do
  root  <- runningAsRoot
  owner <- sudoer
  case owner of
    Just (uid, gid) | root -> setOwnerAndGroup path (fromInteger uid) (fromInteger gid)
    _                      -> pure ()

-- | The user and group sudo names, if it names them.
sudoer :: IO (Maybe (Integer, Integer))
sudoer = do
  uid <- lookupEnv "SUDO_UID"
  gid <- lookupEnv "SUDO_GID"
  pure ((,) <$> (readMaybe =<< uid) <*> (readMaybe =<< gid))
#endif
