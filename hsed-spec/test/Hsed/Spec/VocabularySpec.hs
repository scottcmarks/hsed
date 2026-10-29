{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.VocabularySpec
Description : hsedc's vocabulary and anchor kinds against the table annotate.py refuses by
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@tools/extract/hsed_annot.py@ keeps, in its @VOCABULARY@ table, the anchors a
payload may sit on by its first word, so that @annotate.py@ refuses a mismatch
before the PDF is written. hsedc's grammar and its anchor-kind check
("Hsed.Spec.Scope".'anchorKindMismatch') are the authority; this keeps the two
equal: the same words, and for each word (through a payload that starts with
it) the same anchors.

The vocabulary is ready for a rule when its shape's line in
'Hsed.Spec.Shape.shapeMismatch' accepts the tables design section 3.4 lists
for the rule and no table another rule takes: on the committed extractions,
each M2b shape accepts exactly its tables across the whole document (where
its rule rides on the doc anchor) or within its heading (tokenEncoding's
section 3.2.2), save tables a comment binds anyway (T3 and T24 @prose@, T87
@enumValues@); the M2b rule tokens and readers parse.
-}
module Hsed.Spec.VocabularySpec (spec) where

import qualified RIO.List          as L
import qualified Data.Text         as T
import           RIO
import           Test.Hspec

import qualified RIO.Map           as Map
import           RIO.Directory     (doesFileExist)

import           Hsed.Spec.Anchor  (AnchorId (..))
import           Hsed.Spec.Common  (RuleName (..), SpecId (..))
import           Hsed.Spec.CoreIR  (withCore)
import           Hsed.Spec.DSL     (knownRules)
import           Hsed.Spec.Grammar (parsePayload, vocabulary)
import           Hsed.Spec.IR      (SpecIR (..), TableIR (..), readIR)
import           Hsed.Spec.RuleDef (numberedTables)
import           Hsed.Spec.Rules   (anchorKindMismatch, covers, Section (..), tableSections)
import           Hsed.Spec.Shape   (CaptionKind (..), Form (..), Shape (..), measure, shapeMismatch)


-- | The @VOCABULARY = {...}@ entries of hsed_annot.py: one @"word": ("class", ...),@ per line.
pythonVocabulary :: Text -> [(Text, [Text])]
pythonVocabulary source = mapMaybe entry (takeWhile (/= "}") (drop 1 (dropWhile (/= "VOCABULARY = {") (map T.strip (T.lines source)))))
  where
    entry l = case T.splitOn "\"" l of
      _ : word : rest -> Just (word, [ c | (i, c) <- zip [0 :: Int ..] rest, odd i ])
      _               -> Nothing

-- | A payload that starts with each word of the vocabulary.
samples :: [(Text, Text)]
samples =
  [ ("doc", "doc core201 & rule typeRow"), ("rule", "rule typeRow & rule enumValues")
  , ("behaviour", "behaviour \"Base.get\""), ("law", "law \"Sync.x\""), ("fails", "fails \"Base.getFails\"")
  , ("sig", "sig"), ("layout", "layout \"ComPacket\""), ("featureNamed", "featureNamed \"TPerFeature\"")
  , ("bits", "bits 5 [(0, \"KeyChangeZone\")]"), ("stateTable", "stateTable \"Sync.table45\"")
  , ("refines", "refines (core 42)"), ("narrows", "narrows (core 26)"), ("replaces", "replaces [core 127]")
  , ("extends", "extends (core 171)"), ("erratum", "erratum scott [\"a\" ~> \"b\"]"), ("cols", "cols 0 1")
  , ("rangeWords", "rangeWords [\"to\"]"), ("enumInDescription", "enumInDescription"), ("expand", "expand")
  , ("as", "as @'Codes"), ("waive", "waive \"C-UID-UNIQUE\" \"00\" \"x\"") ]

-- | The anchor classes hsedc lets a payload sit on.
accepted :: Text -> [Text]
accepted payload =
  [ cls | (cls, aid) <- [("doc", DocId), ("section", SectionId "5.1.3"), ("table", TableId 46), ("item", ItemId "5.1.3" "p1")]
        , Right p <- [parsePayload payload], isNothing (anchorKindMismatch (SpecId "core-2.01") aid p) ]

-- | The tables a shape accepts in an IR, by number, within a scope.
acceptedIn :: Section -> SpecIR -> Shape -> [Int]
acceptedIn scope ir shape =
  [ n | (TableId n, t) <- Map.toList (numberedTables ir), covers scope (join (Map.lookup t.structPath secs))
      , isNothing (shapeMismatch shape (measure (SpecId ir.spec) t)) ]
  where secs = tableSections ir

-- | Design section 3.4's tables for each M2b shape in Core, as its shape line
--   accepts them across the document, or within tokenEncoding's heading.
coreReadiness :: [(Shape, Section, [Int])]
coreReadiness =
  [ (TableDesc, WholeDocument, 3 : [169 .. 178] <> [180 .. 184] <> [186, 187] <> [189, 191 .. 203] <> [205 .. 208]
                                 <> [211, 213, 214, 215, 218] <> [220 .. 223] <> [225 .. 229] <> [233, 234])
  , (UidTable, WholeDocument, 239 : [241 .. 248])
  , (StatusCodes, WholeDocument, [166])
  , (Codes, WholeDocument, [15, 16, 19, 25, 26, 40, 87, 179, 235])
  , (FieldValue, WholeDocument, [27, 29, 30])
  , (FieldType, WholeDocument, [22, 23, 34])
  , (ConstValues, WholeDocument, [188, 190 .. 204])
  , (Ranges, WholeDocument, [237, 238])
  , (Preconfig, WholeDocument, [209, 216])
  , (Tokens, Subtree "3.2.2", [4 .. 14])
  , (Tokens, WholeDocument, [4 .. 14] <> [24])
  , (Props, WholeDocument, [167, 168]) ]

spec :: Spec
spec = do
  describe "the vocabulary of the M2b rules" $ do
    it "parses every M2b rule token on the doc anchor and on a heading" $
      forM_ ["tableDesc", "uidTable", "statusCodes", "codes", "fieldValue", "fieldType", "constValues", "ranges"
            , "preconfig", "tokenEncoding", "terminology", "sig"] $ \r -> do
        RuleName r `shouldSatisfy` (`elem` knownRules)
        (r, accepted ("doc core201 & rule " <> r), accepted ("rule " <> r)) `shouldBe` (r, ["doc"], ["section"])
    it "parses every M2b reader's comment on a table" $
      forM_ ["stateTable \"Sync.table45\"", "propsNames", "propsInitial", "prose"] $ \c ->
        (c, accepted c) `shouldBe` (c, ["table"])
    withCore $ \ir -> do
      forM_ coreReadiness $ \(shape, scope, tables) ->
        it ("accepts with " <> show shape <> " in " <> show scope <> " exactly the tables design 3.4 lists") $
          acceptedIn scope ir shape `shouldBe` tables
      it "reads the terminology captions, the tables of rule terminology" $
        [ n | (TableId n, t) <- Map.toList (numberedTables ir), fCaptionKind (measure (SpecId "core-2.01") t) == CaptionTerminology ]
          `shouldBe` [1, 210, 217, 219, 224, 232]
    it "accepts with Codes and FieldValue exactly Opal's tables design 3.4 lists" $ do
      let path = "../spec/ir/opal-2.30.json"
      present <- doesFileExist path
      if not present then pendingWith (path <> " is not checked out") else do
        opal <- readIR path >>= either throwString pure
        (acceptedIn WholeDocument opal Codes, acceptedIn WholeDocument opal FieldValue) `shouldBe` ([7, 10, 11, 12, 14], [13])

  source <- runIO (readFileUtf8 "../tools/extract/hsed_annot.py")
  let python = pythonVocabulary source
  describe "hsed_annot.py's VOCABULARY" $ do
    it "names exactly the words of hsedc's vocabulary" $
      L.sort (map fst python) `shouldBe` L.sort vocabulary
    it "puts each word on the anchors hsedc accepts it on" $
      [ (word, accepted payload) | (word, _) <- python, let payload = fromMaybe word (lookup word samples) ]
        `shouldBe` python
