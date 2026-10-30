{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Support
Description : What hsed's tests share: a run silenced, a report's JSON read, a drive-like response, an allowance as printed
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

'silently' keeps a run's printing out of the test's output; 'at' and
'atPath' read into a report's JSON; 'disk6Like' is a drive-like Level 0
response synthesized from what sedutil printed of disk6; 'isAsPrinted'
tells an allowance that is a condition as printed.
-}
module Hsed.Jig.Support
  ( -- * Running quietly
    silently
    -- * Reports
  , at
  , atPath
  , disk6Like
    -- * Allowances
  , isAsPrinted
  ) where

import qualified Data.Aeson               as A
import qualified Data.Aeson.Key           as K
import qualified Data.Aeson.KeyMap        as KM
import           GHC.IO.Handle            (hDuplicate, hDuplicateTo)
import           RIO
import qualified RIO.Text                 as T

import           Hsed.Core.Feature        (Allowance (..))
import           Hsed.Level0.Sedutil      (parseQuery, synthesize)


-- * Running quietly

-- | An action with what it prints to stdout and stderr thrown away.
silently :: IO a -> IO a
silently act = do
  hFlush stdout
  hFlush stderr
  withFile "/dev/null" WriteMode $ \devNull ->
    bracket ((,) <$> hDuplicate stdout <*> hDuplicate stderr)
            (\(out, err) -> do
               hFlush stdout >> hFlush stderr
               hDuplicateTo out stdout >> hDuplicateTo err stderr
               hClose out >> hClose err)
            (\_ -> hDuplicateTo devNull stdout >> hDuplicateTo devNull stderr >> act)


-- * Reports

-- | A field of a JSON object.
at :: Text -> A.Value -> Maybe A.Value
at k = \v -> case v of
  A.Object o -> KM.lookup (K.fromText k) o
  _          -> Nothing

-- | Fields one inside the other.
atPath :: [Text] -> A.Value -> Maybe A.Value
atPath ks v = foldM (flip at) v ks

-- | A drive-like Level 0 response, synthesized from what sedutil printed of
--   disk6 (spec/fixtures/disk6-query.txt; tests run in hsed/).
disk6Like :: IO ByteString
disk6Like = do
  query <- readFileUtf8 "../spec/fixtures/disk6-query.txt"
  either (throwString . T.unpack) (pure . synthesize) (parseQuery query)


-- * Allowances

-- | Whether an allowance is a condition as printed ('AsPrinted'), which the
--   Pair compares unchecked unless it evaluates it.
isAsPrinted :: Allowance -> Bool
isAsPrinted = \a -> case a of
  AsPrinted _ -> True
  _           -> False
