{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Hsed.Spec.Rules
Description : The document-level rules' scope, and the classification of every numbered table
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4. The rules are those of "Hsed.Spec.Registry", each the
'RuleDef' of its own module under @Hsed/Spec/Rules/@; this module knows none of
them by name. A rule fires on a table by its measured 'Form' (and the table
itself); it binds the table to its default binding wherever the rule is in
scope. Scope comes only from the anchors: a @rule X@ token on a heading anchor
governs that section's subtree, on the @doc@ anchor the whole document
('scopeOf'). Precedence: a rule may declare 'refinesRule' and wins where both
fire; any other double match is @E-AMBIGUOUS@. A rule with 'joinsTo' fires only
on a table whose identifier ('ruleIdent') a table of the joined rule names too.

'classify' puts every numbered table in one of four places:

* comment-bound: its annotation is a full binding;
* rule-bound: no annotation and exactly one rule in scope fires, or an
  annotation that is a bare modifier chain, applied in order to the default of
  the single rule that fires;
* ambiguous: two rules fire and neither refines the other (@E-AMBIGUOUS@);
* unbound: nothing binds it, the Warning @W-UNBOUND@ (design M2b promotes it
  to @E-UNBOUND@);

plus 'Invalid' for an annotation that does not parse or whose modifiers do not
apply. A rule name of "Hsed.Spec.DSL" that the registry does not implement
parses but binds nothing (@W-RULE-UNIMPLEMENTED@).

Every annotation is validated once, in 'parseAnnots', so that @hsedc check@ and
@hsedc bind@ refuse the same ones: its id must parse and name something in the
IR (@E-ANCHOR@, citing the page), no other annotation may sit on the same
anchor (@E-ANCHOR@), its payload must parse (@E-SYNTAX@, @E-GRAMMAR@,
@E-ERRATUM-PUBLISHED@), and the binding must be of the anchor's kind
(@E-ANCHOR-KIND@, 'anchorKindMismatch'): the @doc@ anchor takes @doc \<spec\>@
naming this spec, first, and rule tokens; a heading takes rule tokens or a
@behaviour@; a table takes a table binding or a bare modifier chain; a list item
or paragraph takes @law@, @fails@ or @behaviour@.

A rule binds only where an anchor puts it in scope, so design section 3.4's
@C-RULE-ANCHORED@ holds by construction and is not checked separately.
-}
module Hsed.Spec.Rules
  ( -- * Rules
    RuleDef (..)
  , rules
  , ruleNamed
  , tableIdent
    -- * Scope
  , Section (..)
  , covers
  , tableSections
  , scopeOf
  , ruleTokens
    -- * Annotations
  , ParsedAnnot (..)
  , parseAnnots
    -- * Classification
  , Classification (..)
  , classify
  , classifyWith
  , classifyWithRules
  , boundBinding
  , describeClassification
  , explain
  , numberedTables
  , tablePage
  , locatedAt
  , checkExemplars
  , anchorKindMismatch
  ) where

import qualified Data.Text         as DT
import qualified RIO.Char          as C
import qualified RIO.List          as L
import qualified RIO.Map           as Map
import qualified RIO.Set           as Set
import qualified RIO.Text          as T
import           RIO

import           Hsed.Spec.Anchor  (AnchorId (..), anchorTarget, parseAnchorId, renderAnchorId)
import           Hsed.Spec.Common  (Finding, RuleName (..), Severity (..), SpecId (..), finding,
                                    renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.DSL     (Binding (..), BindingKind (..), Modifier (..), Ref (..), SomeBinding (..),
                                    applyModifierSome, atShape, bindingAccepts, knownRules, someBindingKind,
                                    someBindingShape, specName)
import           Hsed.Spec.Grammar (Payload (..), parsePayload, printSomeBinding)
import           Hsed.Spec.IR      (AnnotIR (..), HeadingIR (..), SpecIR (..), StructPath (..),
                                    TableIR (..))
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.RuleDef (Classification (..), RuleDef (..), boundBinding, locatedAt, numberedTables,
                                    tablePage)
import           Hsed.Spec.Shape   (Form (..), KnownShape, SShape, Shape (Doc), SomeShape (..),
                                    measure, projectColumns, toSomeShape)

-- * Rules

-- | The rules hsedc implements: "Hsed.Spec.Registry".
rules :: [RuleDef]
rules = registry

ruleNamed :: RuleName -> Maybe RuleDef
ruleNamed n = L.find ((== n) . ruleName) rules

-- | The identifier a table's caption names, as the first rule that reads one
--   from it reads it (the type of a type row or of an enumeration).
tableIdent :: Form -> Maybe Text
tableIdent form = asum [ ruleIdent r form | r <- rules ]


-- * Scope

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
scopeOf ir r = scopesFrom (parseAnnots ir) r

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


-- * Classification

-- | One line for a ledger or an explanation.
describeClassification :: Classification -> Text
describeClassification = \case
  RuleBound r [] _   -> "rule " <> ruleNameText r
  RuleBound r _ sb   -> "rule " <> ruleNameText r <> ", modified: " <> printSomeBinding sb
  CommentBound sb    -> "comment: " <> printSomeBinding sb
  Ambiguous rs       -> "ambiguous: " <> T.intercalate ", " (map ruleNameText rs)
  Unbound            -> "unbound"
  Invalid f          -> "invalid: " <> renderFinding f

-- | The rule and its exemplar for a rule-bound table, @\<binding\> (exemplar core T46)@ (design acceptance M1 (g)).
explain :: SpecIR -> AnchorId -> Text
explain ir aid = case Map.lookup aid (classify ir) of
  Nothing -> "no table " <> renderAnchorId aid <> " in " <> ir.spec
  Just c@(RuleBound r _ sb) -> case ruleNamed r of
    Just rl -> printSomeBinding sb <> " (exemplar " <> prefix (fst (exemplar rl)) <> " " <> renderAnchorId (snd (exemplar rl)) <> ")"
    Nothing -> describeClassification c
  Just c -> describeClassification c
  where prefix = T.toLower . T.takeWhile C.isAlpha . specIdText

classify :: SpecIR -> Map AnchorId Classification
classify = fst . classifyWith

-- | The classification and its findings: parse failures, @E-AMBIGUOUS@,
--   @W-UNBOUND@, rule names hsedc does not implement yet.
classifyWith :: SpecIR -> (Map AnchorId Classification, [Finding])
classifyWith = classifyWithRules rules

-- | 'classifyWith' under another set of rules (the tests' ambiguous pairs).
classifyWithRules :: [RuleDef] -> SpecIR -> (Map AnchorId Classification, [Finding])
classifyWithRules ruleSet ir = (final, annotFindings <> unimplemented <> concatMap located (Map.toList final))
  where
    sid      = SpecId ir.spec
    parsed   = parseAnnots ir
    tables   = numberedTables ir
    sections = tableSections ir
    scopes   = Map.fromList [ (ruleName r, scopesFrom parsed (ruleName r)) | r <- ruleSet ]
    -- the identifiers of the tables each rule fires on, anywhere in the document, for the joins
    idents   = Map.fromList
      [ (ruleName r, Set.fromList [ i | t <- Map.elems tables, let form = measure sid t
                                      , fires r sid t form, Just i <- [ruleIdent r form] ])
      | r <- ruleSet ]
    commentsOn aid = [ pa | pa <- parsed, paAnchor pa == Just aid ]
    firing t form = precedence
      [ r | r <- ruleSet
          , any (`covers` Map.findWithDefault Nothing t.structPath sections) (Map.findWithDefault [] (ruleName r) scopes)
          , fires r sid t form
          , maybe True (\joined -> maybe False (`Set.member` Map.findWithDefault mempty joined idents) (ruleIdent r form)) (joinsTo r) ]
    precedence rs = [ r | r <- rs, not (any ((== Just (ruleName r)) . refinesRule) rs) ]

    first' = Map.mapWithKey classifyTable tables
    final  = Map.mapWithKey (resolvePoly first') first'

    here aid t = locatedAt sid (renderAnchorId aid) (fromMaybe 1 (tablePage t))

    classifyTable aid t = case commentsOn aid of
      []     -> byRules aid t []
      pa : _ -> case paPayload pa of     -- a second annotation on the anchor makes each one Left
        Left f               -> Invalid f
        Right (Full sb)      -> CommentBound sb
        Right (Modifiers ms) -> byRules aid t ms

    byRules aid t ms = case foldM project (measure sid t) ms of
      Left msg   -> Invalid (here aid t (finding Error "E-MODIFIER" msg))
      Right form -> case firing t form of
        [r] -> either (Invalid . here aid t) (RuleBound (ruleName r) ms) (foldM (flip applyModifierSome) (bindsTo r) ms)
        []  | null ms   -> Unbound
            | otherwise -> Invalid (here aid t (finding Error "E-MODIFIER"
                             "a bare modifier chain applies to the rule default, and no rule in scope fires on this table"))
        rs  -> Ambiguous (map ruleName rs)
    project form = \case
      Cols a b -> projectColumns a b form
      _        -> Right form

    -- a binding shaped by its target (refines, narrows, replaces) takes the target's shape
    resolvePoly :: Map AnchorId Classification -> AnchorId -> Classification -> Classification
    resolvePoly m aid = \case
      CommentBound (PolyBinding b) -> case Map.lookup aid tables of
        Nothing -> CommentBound (PolyBinding b)
        Just t  -> either (Invalid . here aid t) CommentBound (resolveAgainst m b)
      c -> c
    resolveAgainst :: Map AnchorId Classification -> (forall s. KnownShape s => Binding s) -> Either Finding SomeBinding
    resolveAgainst m b = do
      targets <- traverse target (polyTargets (b :: Binding 'Doc))
      shapes  <- traverse (\aid -> maybe (Left (unresolved aid)) Right (Map.lookup aid m >>= boundBinding >>= someBindingShape)) targets
      case L.nub shapes of
        [s] -> case toSomeShape s of
          SomeShape (_ :: SShape t) -> maybe (Left (unresolvedShape s)) Right (SomeBinding <$> atShape @t (PolyBinding b))
        []  -> Left (finding Error "E-SHAPE" "a cross-spec binding names no target")
        ss  -> Left (finding Error "E-SHAPE" ("the targets have different shapes: " <> T.intercalate ", " (map tshow ss)))
    target = \case
      Core n | "core" `T.isPrefixOf` ir.spec -> Right (TableId n)
      Opal n | "opal" `T.isPrefixOf` ir.spec -> Right (TableId n)
      r -> Left (finding Error "E-SHAPE" ("the shape comes from " <> tshow r <> ", which is not in " <> ir.spec <> "; resolve it with that spec's IR"))
    unresolved aid = finding Error "E-SHAPE" ("the shape comes from " <> renderAnchorId aid <> ", which nothing binds")
    unresolvedShape s = finding Error "E-SHAPE" ("cannot take the shape " <> tshow s)

    located (aid, c) = case (c, Map.lookup aid tables) of
      (Invalid f, _)            -> [f]
      (Ambiguous rs, Just t)    -> [here aid t (finding Error "E-AMBIGUOUS"
                                     ("rules " <> T.intercalate " and " (map ruleNameText rs) <> " both fire and neither refines the other"))]
      (Unbound, Just t)         -> [here aid t (finding Warning "W-UNBOUND"
                                     ("no annotation binds it and no rule in scope fires on it (" <> fromMaybe "no caption" t.caption <> ")"))]
      _                         -> []

    -- what is wrong with an annotation that no numbered table's classification reports
    annotFindings = L.nub [ f | pa <- parsed, Left f <- [paPayload pa], maybe True (`Map.notMember` tables) (paAnchor pa) ]
    unimplemented =
      [ locatedAt sid (paAnnot pa).id ((paAnnot pa).page + 1)
          (finding (if r `elem` knownRules then Warning else Error) (if r `elem` knownRules then "W-RULE-UNIMPLEMENTED" else "E-GRAMMAR")
             ("rule " <> ruleNameText r <> " binds nothing yet: hsedc implements " <> T.intercalate ", " (map (ruleNameText . ruleName) ruleSet)))
      | pa <- parsed, Right p <- [paPayload pa], r <- ruleTokens p, isNothing (L.find ((== r) . ruleName) ruleSet) ]

-- | The tables a binding shaped by its target names.
polyTargets :: Binding s -> [Ref]
polyTargets = \case
  Refines r         -> [r]
  Narrows r         -> [r]
  Replaces rs       -> rs
  WithErratum _ _ b -> polyTargets b
  WithAs b          -> polyTargets b
  _                 -> []

-- | Each exemplar still fires its rule, is accepted by the rule's default
--   binding and has its table to join (design section 3.4, re-verified on every
--   run): @C-EXEMPLAR@. A reader reached only by a comment (a name that is no
--   rule token of "Hsed.Spec.DSL") never fires; its exemplar need only be accepted.
checkExemplars :: SpecIR -> [Finding]
checkExemplars ir = concatMap check [ r | r <- rules, fst (exemplar r) == sid ]
  where
    sid    = SpecId ir.spec
    tables = numberedTables ir
    check r = case Map.lookup (snd (exemplar r)) tables of
      Nothing -> [finding Error "C-EXEMPLAR" ("the exemplar " <> renderAnchorId (snd (exemplar r)) <> " of rule "
                    <> ruleNameText (ruleName r) <> " is not in the IR") & \f -> f { Cm.spec = Just sid }]
      Just t  ->
        let form   = measure sid t
            here   = locatedAt sid (renderAnchorId (snd (exemplar r))) (fromMaybe 1 (tablePage t))
            isRule = ruleName r `elem` knownRules
            joins  = case joinsTo r of
              Nothing     -> True
              Just target -> case ruleNamed target of
                Nothing -> False
                Just tr -> any (\u -> let f = measure sid u in fires tr sid u f && ruleIdent tr f == ruleIdent r form) (Map.elems tables)
            shape  = case bindsTo r of
              SomeBinding b -> bindingAccepts b form
              PolyBinding _ -> Nothing
        in map here (catMaybes
             [ guarded (isRule && not (fires r sid t form)) (finding Error "C-EXEMPLAR" ("rule " <> ruleNameText (ruleName r) <> " no longer fires on its exemplar"))
             , guarded (isRule && not joins) (finding Error "C-EXEMPLAR" ("rule " <> ruleNameText (ruleName r) <> ": the exemplar has no table to join"))
             , shape ])
    guarded c f = if c then Just f else Nothing
