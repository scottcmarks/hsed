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
Core. Since M4b's scaffold also Opal under the sidecar M4b commits at its
integration ('m4bOpalHsa', the M4b plan's four lines), read over the
committed Core run's Context ('withM4bOpal'): every M4b group tests its rule
on it before the anchors land. Cells are edited through their JSON form, the
one way to set a field several IR records share.
-}
module Hsed.Spec.CoreIR
  ( withCore
  , committedHsa
  , withOpal
  , committedOpalHsa
  , m4bOpalHsa
  , withM4bOpal
  , m5aOpalHsa
  , withM5aOpal
  , opalOver
  , annotated
  , annotatedWith
  , setCell
  , setHeaderCell
  , duplicateRow
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
import           Hsed.Spec.Check   (mkContext, mkContextWith)
import           Hsed.Spec.Common  (ErrataSet (..), Finding, SpecId (..))
import           Hsed.Spec.Compile (CompileResult (..), compileIR)
import           Hsed.Spec.IR
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.RuleDef (Context)
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

-- | The Opal sidecar of M4b, the four lines its integration commits (the M4b
--   plan's annotations): the type layer's and the legend's rule tokens on the
--   doc line beside feature's, the preconfiguration at section 4.2 (PDF 35:
--   the Admin SP's twelve tables, not the Locking SP's, which are M5's),
--   Table 17's requirements, and Table 49's enumeration in its Description
--   column, replacing Core's type and enumeration (Tables 102 and 103).
m4bOpalHsa :: Text
m4bOpalHsa = T.unlines
  [ "opal-2.30 | doc | doc opal230 & rule feature & rule legend & rule fieldValue & rule columnsAddition & rule typeRow & rule enumValues & rule tableDesc"
  , "opal-2.30 | 4.2 | rule preconfig"
  , "opal-2.30 | T17 | propsReq"
  , "opal-2.30 | T49 | enumInDescription (replaces [core 102, core 103])" ]

-- | The committed Core run's Context (the committed Core extraction under the
--   committed sidecar, Scott's errata) and the committed Opal extraction under
--   'm4bOpalHsa', read once for a group of tests; pending where either IR is
--   not checked out. Read the Opal IR over the Core Context with 'opalOver',
--   or edit it first ('setCell') and then read it.
withM4bOpal :: (Context -> SpecIR -> Spec) -> Spec
withM4bOpal k = withCore $ \corePristine -> withOpal $ \opalPristine -> do
  coreIR <- runIO (committedHsa >>= annotated corePristine)
  opal   <- runIO (annotated opalPristine m4bOpalHsa)
  k (mkContext Scott coreIR) opal

-- | The Opal sidecar of M5a, the M5 plan's annotations (A3), as the groups
--   read it from a scratch copy and the integration committed it
--   (@spec/annot/opal-2.30.hsa@, 5c6e96e): the rule
--   tokens of M4b's doc line with @rule preconfig@ (the Admin SP's tables and
--   the Locking SP's, Tables 34-48) and @rule sig@, the units nothing else
--   binds as @prose@, Tables 14 and 15 narrowing Core's 26 and 4 and Table 16
--   replacing Core's 128 (PDF 30-32), and M4b's Tables 17 and 49.
m5aOpalHsa :: Text
m5aOpalHsa = T.unlines
  ( "opal-2.30 | doc | doc opal230 & rule feature & rule legend & rule fieldValue & rule columnsAddition & rule typeRow & rule enumValues & rule tableDesc & rule preconfig & rule sig"
  : [ "opal-2.30 | T17 | propsReq"
    , "opal-2.30 | T49 | enumInDescription (replaces [core 102, core 103])" ]
  <> [ "opal-2.30 | T" <> tshow n <> " | prose" | n <- [2, 7, 9, 10, 11, 12 :: Int] ]
  <> [ "opal-2.30 | T14 | narrows (core 26)"
     , "opal-2.30 | T15 | narrows (core 4)"
     , "opal-2.30 | T16 | replaces [core 128]" ] )

-- | 'withM4bOpal' with the Opal IR under 'm5aOpalHsa'.
withM5aOpal :: (Context -> SpecIR -> Spec) -> Spec
withM5aOpal k = withCore $ \corePristine -> withOpal $ \opalPristine -> do
  coreIR <- runIO (committedHsa >>= annotated corePristine)
  opal   <- runIO (annotated opalPristine m5aOpalHsa)
  k (mkContext Scott coreIR) opal

-- | An Opal IR read over a Core Context, as @hsedc --base@ reads it (the
--   registry's rules, Scott's errata).
opalOver :: Context -> SpecIR -> Context
opalOver base = mkContextWith registry (Just base) Scott

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

-- | A numbered table with one of its data rows (0-based) printed twice, the
--   copy after it, for an edit to make it a row of its own ('setCell').
duplicateRow :: Text -> Int -> SpecIR -> SpecIR
duplicateRow tid row ir = ir { tables = map table ir.tables }
  where
    table t
      | t.id == Just tid = t { rows = concat (zipWith (\i r -> if i == row then [r, r] else [r]) [0 :: Int ..] t.rows) }
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
