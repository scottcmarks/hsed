{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.CoreIR
Description : The committed Core extraction with annotations in memory, for the hsedc tests
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The tests of rules, checks, bindings and the generator run on
@spec\/ir\/core-2.01.json@ (pristine, no annotations) with a sidecar's
annotations resolved into it ("Hsed.Spec.Sidecar"), the committed
@spec\/annot\/core-2.01.hsa@ or a few lines of their own; the same IR the
extractor gives for the annotated PDF. Since M3 the same for Opal
(@spec\/ir\/opal-2.30.json@, @spec\/annot\/opal-2.30.hsa@), which is read over
Core. Cells are edited through their JSON form, the one way to set a field
several IR records share.
-}
module Hsed.Spec.CoreIR
  ( withCore
  , committedHsa
  , withOpal
  , committedOpalHsa
  , annotated
  , annotatedWith
  , setCell
  , setHeaderCell
  , dropTable
  , withoutLine
  , newlyExcluded
  , core
  ) where

import           Data.Aeson        (Value (..), fromJSON, toJSON)
import qualified Data.Aeson        as A
import qualified Data.Aeson.KeyMap as KM
import           RIO
import           RIO.Directory     (doesFileExist)
import qualified RIO.Text          as T
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId)
import           Hsed.Spec.Common  (ErrataSet (..), Finding, SpecId (..))
import           Hsed.Spec.Compile (CompileResult (..), compileIR)
import           Hsed.Spec.IR
import           Hsed.Spec.Sidecar (withSidecar)


core :: SpecId
core = SpecId "core-2.01"

-- | The committed extraction, read once for a group of tests; pending where it is not checked out.
withCore :: (SpecIR -> Spec) -> Spec
withCore k = do
  let path = "../spec/ir/core-2.01.json"
  present <- runIO (doesFileExist path)
  if present
    then runIO (readIR path) >>= either (\e -> it "reads the committed Core IR" (expectationFailure e)) k
    else it "needs the committed Core IR" (pendingWith (path <> " is not checked out"))

-- | The committed sidecar.
committedHsa :: IO Text
committedHsa = readFileUtf8 "../spec/annot/core-2.01.hsa"

-- | The committed Opal extraction, read once for a group of tests; pending where it is not checked out.
withOpal :: (SpecIR -> Spec) -> Spec
withOpal k = do
  let path = "../spec/ir/opal-2.30.json"
  present <- runIO (doesFileExist path)
  if present
    then runIO (readIR path) >>= either (\e -> it "reads the committed Opal IR" (expectationFailure e)) k
    else it "needs the committed Opal IR" (pendingWith (path <> " is not checked out"))

-- | The committed Opal sidecar.
committedOpalHsa :: IO Text
committedOpalHsa = readFileUtf8 "../spec/annot/opal-2.30.hsa"

-- | The IR with a sidecar's annotations; a sidecar that does not resolve is a test failure.
annotated :: SpecIR -> Text -> IO SpecIR
annotated ir hsa = case withSidecar ir hsa of
  ([], ir') -> pure ir'
  (fs, _)   -> throwString ("the sidecar does not resolve: " <> show fs)

-- | The IR with a sidecar's annotations and whatever resolving them found.
annotatedWith :: SpecIR -> Text -> ([Finding], SpecIR)
annotatedWith = withSidecar

-- | Set the text of one data cell (row and column 0-based) of a numbered table.
setCell :: Text -> Int -> Int -> Text -> SpecIR -> SpecIR
setCell tid row col txt ir = ir { tables = map table ir.tables }
  where
    table t
      | t.id == Just tid = t { rows = zipWith (\i r -> if i == row then zipWith (\j c -> if j == col then setText c else c) [0 :: Int ..] r else r) [0 ..] t.rows }
      | otherwise        = t
    setText c = case toJSON c of
      Object o -> case fromJSON (Object (KM.insert "text" (String txt) o)) of
        A.Success c' -> c'
        A.Error e    -> error ("setCell: " <> e)
      _ -> c

-- | Set the text of one header cell (row and column 0-based) of a numbered table.
setHeaderCell :: Text -> Int -> Int -> Text -> SpecIR -> SpecIR
setHeaderCell tid row col txt ir = ir { tables = map table ir.tables }
  where
    table t
      | t.id == Just tid = t { headerRows = zipWith (\i r -> if i == row then zipWith (\j c -> if j == col then cellWithText txt c else c) [0 :: Int ..] r else r) [0 ..] t.headerRows }
      | otherwise        = t

-- | A sidecar without the line of one anchor (@withoutLine "T75"@).
withoutLine :: Text -> Text -> Text
withoutLine anchorId = T.unlines . filter (not . (("| " <> anchorId <> " |") `T.isInfixOf`)) . T.lines

-- | The tables @hsedc compile@ (under Scott's errata) leaves out of an IR and
--   not of a base IR, the committed annotations' run: what a test's edit makes a
--   rule leave out, whatever the comments whose readers are still to come leave out.
newlyExcluded :: SpecIR -> SpecIR -> IO [(AnchorId, Text)]
newlyExcluded base ir = do
  base'  <- excluded base
  after' <- excluded ir
  pure (filter (`notElem` base') after')
  where excluded x = either (throwString . show) (pure . crExcluded) (compileIR Nothing Scott x)

-- | The IR without a numbered table.
dropTable :: Text -> SpecIR -> SpecIR
dropTable tid ir = ir { tables = filter ((/= Just tid) . (.id)) ir.tables }
