{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Scope
Description : Sections, the scope the rule anchors give, and the validated annotations of an IR
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The document structure every rule may read, below the registry so that a rule
module can import it ("Hsed.Spec.Rules" imports "Hsed.Spec.Registry", which
imports every rule, so a rule importing "Hsed.Spec.Rules" would be a module
cycle; design section 3.4, docs/adding-a-rule.md):

* the section of every table, that of the nearest numbered heading before it
  in structure-tree order ('tableSections'; the 'Hsed.Spec.RuleDef.Context'
  carries it as 'Hsed.Spec.RuleDef.cxSections');
* the scope of a rule, computed only from the anchors ('scopeOf'): a @rule X@
  token on a heading anchor governs that section's subtree, on the @doc@
  anchor the whole document;
* every annotation validated once ('parseAnnots'), so that @hsedc check@ and
  @hsedc bind@ refuse the same ones: its id must parse and name something in
  the IR (@E-ANCHOR@, citing the page), no other annotation may sit on the
  same anchor (@E-ANCHOR@), its payload must parse (@E-SYNTAX@, @E-GRAMMAR@,
  @E-ERRATUM-PUBLISHED@), and the binding must be of the anchor's kind
  (@E-ANCHOR-KIND@, 'anchorKindMismatch'): the @doc@ anchor takes @doc
  \<spec\>@ naming this spec, first, and rule tokens; a heading takes rule
  tokens or a @behaviour@; a table takes a table binding or a bare modifier
  chain; a list item or paragraph takes @law@, @fails@ or @behaviour@.
-}
module Hsed.Spec.Scope
  ( -- * Sections
    Section (..)
  , covers
  , tableSections
    -- * Scope
  , scopeOf
  , scopesFrom
  , ruleTokens
    -- * Annotations
  , ParsedAnnot (..)
  , parseAnnots
  , anchorKindMismatch
  ) where

import qualified Data.Text         as DT
import qualified RIO.List          as L
import qualified RIO.Map           as Map
import qualified RIO.Set           as Set
import qualified RIO.Text          as T
import           RIO

import           Hsed.Spec.Anchor  (AnchorId (..), anchorTarget, parseAnchorId)
import           Hsed.Spec.Common  (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import           Hsed.Spec.DSL     (Binding (..), BindingKind (..), SomeBinding (..), someBindingKind, specName)
import           Hsed.Spec.Grammar (Payload (..), parsePayload, printSomeBinding)
import           Hsed.Spec.IR      (AnnotIR (..), HeadingIR (..), SpecIR (..), StructPath (..), TableIR (..))
import           Hsed.Spec.RuleDef (locatedAt)


-- * Sections

-- | What a rule anchor governs.
data Section
  = WholeDocument   -- ^ a @rule@ token on the @doc@ anchor
  | Subtree !Text   -- ^ a @rule@ token on a heading: that section and its subsections
  deriving (Eq, Ord, Show)

-- | Does a scope cover a table in this section (Nothing: before the first numbered heading)?
covers :: Section -> Maybe Text -> Bool
covers WholeDocument _          = True
covers (Subtree s) (Just t)     = t == s || (s <> ".") `T.isPrefixOf` t
covers (Subtree _) Nothing      = False

-- | The section of every table: that of the nearest numbered heading before it in
--   structure-tree order.
tableSections :: SpecIR -> Map StructPath (Maybe Text)
tableSections ir = Map.fromList (catMaybes (snd (L.mapAccumL step Nothing events)))
  where
    events = L.sortOn fst ([ (pathKey h.structPath, Left h.section) | h <- ir.headings ]
                        <> [ (pathKey t.structPath, Right t.structPath) | t <- ir.tables ])
    step current (_, Left sec)  = (sec <|> current, Nothing)
    step current (_, Right sp)  = (current, Just (sp, current))

pathKey :: StructPath -> [Int]
pathKey (StructPath t) = mapMaybe (readMaybe . T.unpack) (DT.splitOn "/" t)


-- * Scope

-- | The rule names a payload's @rule@ tokens give.
ruleTokens :: Payload -> [RuleName]
ruleTokens = \case
  Full (SomeBinding b) -> go b
  _                    -> []
  where
    go :: Binding s -> [RuleName]
    go = \case
      RuleB r  -> [r]
      Also a b -> go a <> go b
      _        -> []

-- | Where a rule is anchored, computed only from the annotations of the IR.
scopeOf :: SpecIR -> RuleName -> [Section]
scopeOf ir = scopesFrom (parseAnnots ir)

-- | 'scopeOf' over annotations already parsed.
scopesFrom :: [ParsedAnnot] -> RuleName -> [Section]
scopesFrom parsed r = Set.toList $ Set.fromList
  [ sec | pa <- parsed, Right payload <- [paPayload pa], r `elem` ruleTokens payload
        , Just sec <- [paAnchor pa >>= sectionOf] ]
  where
    sectionOf = \case
      DocId       -> Just WholeDocument
      SectionId s -> Just (Subtree s)
      _           -> Nothing


-- * Annotations

-- | One annotation of the IR with its anchor and its parsed payload.
data ParsedAnnot = ParsedAnnot
  { paAnnot   :: !AnnotIR
  , paAnchor  :: !(Maybe AnchorId)
  , paPayload :: !(Either Finding Payload)   -- ^ a failure is located at the annotation
  }

-- | Every annotation of the IR, validated: an id that does not parse or names
--   nothing in the IR, a second annotation on one anchor, a payload that does
--   not parse and a binding of the wrong kind for its anchor each make the
--   payload a located Error.
parseAnnots :: SpecIR -> [ParsedAnnot]
parseAnnots ir = map parsed ir.annots
  where
    sid = SpecId ir.spec
    counts = Map.fromListWith (+) [ (aid, 1 :: Int) | a <- ir.annots, Just aid <- [parseAnchorId a.id] ]
    parsed a = ParsedAnnot a anchorId (first (locatedAt sid a.id (a.page + 1)) (validate a))
      where anchorId = parseAnchorId a.id
    validate a = case parseAnchorId a.id of
      Nothing  -> Left (finding Error "E-ANCHOR" ("not an anchor id: " <> tshow a.id))
      Just aid -> do
        let n = Map.findWithDefault 0 aid counts
        when (n > 1) $ Left (finding Error "E-ANCHOR" (tshow n <> " annotations on one anchor"))
        void (first (finding Error "E-ANCHOR") (anchorTarget ir aid))
        payload <- parsePayload a.contents
        maybe (Right payload) (\(c, msg) -> Left (finding Error c msg)) (anchorKindMismatch sid aid payload)

-- | Why a payload does not belong on an anchor, as a code and a message; Nothing
--   when it does (design section 3.1's kinds, 'Hsed.Spec.DSL.bindingKind').
anchorKindMismatch :: SpecId -> AnchorId -> Payload -> Maybe (Text, Text)
anchorKindMismatch sid aid payload = case (aid, payload) of
  (TableId _, Modifiers _) -> Nothing
  (_, Modifiers _)         -> Just ("E-MODIFIER", "a bare modifier chain binds only a table")
  (_, Full sb)             -> kindError <$> mismatch (someBindingKind sb)
  where
    kindError why = ("E-ANCHOR-KIND", why <> ", not " <> printPayloadShort)
    printPayloadShort = case payload of
      Full sb      -> printSomeBinding sb
      Modifiers _  -> "a bare modifier chain"
    mismatch k = case (aid, k) of
      (DocId, DocKind [d] True) | d == sid -> Nothing
                                | otherwise -> Just ("the doc anchor of " <> specIdText sid <> " names its own spec, doc " <> specName sid)
      (DocId, _)               -> Just ("the doc anchor takes doc " <> specName sid <> " first, then rule tokens")
      (SectionId _, RuleKind)      -> Nothing
      (SectionId _, BehaviourKind) -> Nothing
      (SectionId _, _)         -> Just "a heading anchor takes rule tokens or a behaviour"
      (TableId _, TableKind)   -> Nothing
      (TableId _, _)           -> Just "a table anchor takes a table binding or a bare modifier chain"
      (ItemId _ _, LawKind)       -> Nothing
      (ItemId _ _, BehaviourKind) -> Nothing
      (ItemId _ _, _)          -> Just "a list item or paragraph anchor takes law, fails or behaviour"
      (SigId _, _)             -> Just "a signature block is rule-bound and carries no annotation"
