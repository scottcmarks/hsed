{-# LANGUAGE NoImplicitPrelude #-}
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
-}
module Hsed.Spec.VocabularySpec (spec) where

import qualified RIO.List          as L
import qualified Data.Text         as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId (..))
import           Hsed.Spec.Common  (SpecId (..))
import           Hsed.Spec.Grammar (parsePayload, vocabulary)
import           Hsed.Spec.Rules   (anchorKindMismatch)


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

spec :: Spec
spec = do
  source <- runIO (readFileUtf8 "../tools/extract/hsed_annot.py")
  let python = pythonVocabulary source
  describe "hsed_annot.py's VOCABULARY" $ do
    it "names exactly the words of hsedc's vocabulary" $
      L.sort (map fst python) `shouldBe` L.sort vocabulary
    it "puts each word on the anchors hsedc accepts it on" $
      [ (word, accepted payload) | (word, _) <- python, let payload = fromMaybe word (lookup word samples) ]
        `shouldBe` python
