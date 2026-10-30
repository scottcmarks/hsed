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
* unbound: nothing binds it, the Error @E-UNBOUND@ (a Warning, @W-UNBOUND@,
  until every numbered table of Core had its rule, design section 11 M2c);

plus 'Invalid' for an annotation that does not parse or whose modifiers do not
apply. A rule name of "Hsed.Spec.DSL" that the registry does not implement
parses but binds nothing (@W-RULE-UNIMPLEMENTED@).

'classifySignatures' does the same for the signature units (the method
signature blocks, IR @sigs@), which carry no annotation and which a rule
binds by its scope alone: each unit whose section a rule anchor covers is
bound to the rule whose shape takes signature units
('Hsed.Spec.Shape.shapeUnit'); a unit no such rule covers is unbound
(@E-UNBOUND@, as an unbound table), one two such rules cover is
@E-AMBIGUOUS@. The paragraph units (a list item or paragraph a @law@ binds)
have no classification: their comments alone bind them
("Hsed.Spec.RuleDef".'Hsed.Spec.RuleDef.boundParas'). A rule whose shape
takes signature or paragraph units fires on no table.

Every annotation is validated once, in "Hsed.Spec.Scope".'parseAnnots', so
that @hsedc check@ and @hsedc bind@ refuse the same ones; the sections, the
scope and that validation live in "Hsed.Spec.Scope", below the registry, so
that a rule module can use them too, and this module re-exports them.

A rule binds only where an anchor puts it in scope, so design section 3.4's
@C-RULE-ANCHORED@ holds by construction and is not checked separately. What
is checked is the converse ('checkRuleAnchors'): a @rule X@ token whose scope
binds nothing to X (@W-RULE-SCOPE-EMPTY@), or whose scope another anchor of X
already covers (@N-RULE-SCOPE-REDUNDANT@), says nothing.
-}
module Hsed.Spec.Rules
  ( -- * Rules
    RuleDef (..)
  , rules
  , ruleNamed
    -- * Scope ("Hsed.Spec.Scope")
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
  , classifySignatures
  , classifySignaturesWith
  , boundBinding
  , describeClassification
  , explain
  , numberedTables
  , tablePage
  , locatedAt
  , checkExemplars
  , checkExemplarsWith
  , checkRuleAnchors
  , bindsWithin
  , anchorKindMismatch
  ) where

import qualified RIO.Char          as C
import qualified RIO.List          as L
import qualified RIO.Map           as Map
import qualified RIO.Set           as Set
import qualified RIO.Text          as T
import           RIO

import           Hsed.Spec.Anchor  (AnchorId (..), anchorParagraph, renderAnchorId)
import           Hsed.Spec.Common  (Finding, RuleName (..), Severity (..), SpecId (..), finding,
                                    renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.DSL     (Binding (..), Modifier (..), Ref (..), SomeBinding (..),
                                    applyModifierSome, atShape, bindingAccepts, knownRules, someBindingShape)
import           Hsed.Spec.Grammar (Payload (..), printSomeBinding)
import           Hsed.Spec.IR      (AnnotIR (..), SigIR (..), SpecIR (..), TableIR (..))
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.RuleDef (Classification (..), RuleDef (..), boundBinding, locatedAt, numberedTables,
                                    ruleShape, signatureUnits, tablePage)
import           Hsed.Spec.Scope
import           Hsed.Spec.Shape   (KnownShape, SShape, Shape (Doc), SomeShape (..), UnitKind (..),
                                    measure, projectColumns, shapeUnit, toSomeShape)

-- * Rules

-- | The rules hsedc implements: "Hsed.Spec.Registry".
rules :: [RuleDef]
rules = registry

ruleNamed :: RuleName -> Maybe RuleDef
ruleNamed n = L.find ((== n) . ruleName) rules


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

-- | The rule and its exemplar for a rule-bound table, @\<rule\> (exemplar core
--   T46)@ (design acceptance M1 (g)), then its binding where that is not the
--   rule's name alone: @tokenEncoding (exemplar core T4), binding tokens@, or
--   a bare modifier comment's @typeRow (exemplar core T46), binding erratum
--   scott [...] typeRow@.
explain :: SpecIR -> AnchorId -> Text
explain ir aid = case Map.lookup aid (classes aid) of
  Nothing -> "no " <> unit aid <> " " <> renderAnchorId aid <> " in " <> ir.spec
  Just c@(RuleBound r _ sb) -> case ruleNamed r of
    Just rl -> ruleNameText r <> " (exemplar " <> prefix (fst (exemplar rl)) <> " " <> renderAnchorId (snd (exemplar rl)) <> ")"
                 <> (if printSomeBinding sb == ruleNameText r then "" else ", binding " <> printSomeBinding sb)
    Nothing -> describeClassification c
  Just c -> describeClassification c
  where
    prefix = T.toLower . T.takeWhile C.isAlpha . specIdText
    classes = \case
      SigId _ -> fst (classifySignatures ir)
      _       -> classify ir
    unit = \case
      SigId _ -> "signature block"
      _       -> "table"

classify :: SpecIR -> Map AnchorId Classification
classify = fst . classifyWith

-- | The classification and its findings: parse failures, @E-AMBIGUOUS@,
--   @E-UNBOUND@, rule names hsedc does not implement yet.
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
    -- a rule whose shape takes signature or paragraph units fires on no table, whatever its 'fires' says
    firing t form = precedence
      [ r | r <- ruleSet
          , maybe True ((== TableUnit) . shapeUnit) (ruleShape r)
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
      (Unbound, Just t)         -> [here aid t (finding Error "E-UNBOUND"
                                     ("no annotation binds it and no rule in scope fires on it (" <> fromMaybe "no caption" t.caption <> ")"))]
      _                         -> []

    -- what is wrong with an annotation that no numbered table's classification reports
    annotFindings = L.nub [ f | pa <- parsed, Left f <- [paPayload pa], maybe True (`Map.notMember` tables) (paAnchor pa) ]
    unimplemented =
      [ locatedAt sid (paAnnot pa).id ((paAnnot pa).page + 1)
          (finding (if r `elem` knownRules then Warning else Error) (if r `elem` knownRules then "W-RULE-UNIMPLEMENTED" else "E-GRAMMAR")
             ("rule " <> ruleNameText r <> " binds nothing yet: hsedc implements " <> T.intercalate ", " (map (ruleNameText . ruleName) ruleSet)))
      | pa <- parsed, Right p <- [paPayload pa], r <- ruleTokens p, isNothing (L.find ((== r) . ruleName) ruleSet) ]

-- | The classification of every signature unit by the rules of "Hsed.Spec.Registry".
classifySignatures :: SpecIR -> (Map AnchorId Classification, [Finding])
classifySignatures = classifySignaturesWith rules

-- | The classification of every signature unit, and its findings, under a set of
--   rules: a unit is bound to the rule whose shape takes signature units and
--   whose scope covers the unit's section; @E-UNBOUND@ where none does,
--   @E-AMBIGUOUS@ where several do.
classifySignaturesWith :: [RuleDef] -> SpecIR -> (Map AnchorId Classification, [Finding])
classifySignaturesWith ruleSet ir = (classes, concatMap located (Map.toList classes))
  where
    sid     = SpecId ir.spec
    parsed  = parseAnnots ir
    units   = signatureUnits ir
    takers  = [ (r, scopesFrom parsed (ruleName r)) | r <- ruleSet, fmap shapeUnit (ruleShape r) == Just SignatureUnit ]
    classes = Map.map unitClass units
    unitClass u = case [ r | (r, scope) <- takers, any (`covers` Just u.section) scope ] of
      [r] -> RuleBound (ruleName r) [] (bindsTo r)
      []  -> Unbound
      rs  -> Ambiguous (map ruleName rs)
    located (aid, c) = case (c, Map.lookup aid units) of
      (Unbound, Just u)      -> [here aid u (finding Error "E-UNBOUND"
                                  ("no rule in scope binds this signature block (" <> T.intercalate ", " u.heads <> ")"))]
      (Ambiguous rs, Just u) -> [here aid u (finding Error "E-AMBIGUOUS"
                                  ("rules " <> T.intercalate " and " (map ruleNameText rs) <> " both bind this signature block"))]
      _                      -> []
    here aid u = locatedAt sid (renderAnchorId aid) (u.page + 1)

-- | How many numbered tables and how many signature units a classification
--   binds to a rule within a scope.
bindsWithin :: SpecIR -> Map AnchorId Classification -> Map AnchorId Classification -> Section -> RuleName -> (Int, Int)
bindsWithin ir classes sigClasses sec r =
  ( length [ () | (aid, RuleBound r' _ _) <- Map.toList classes, r' == r
                , Just t <- [Map.lookup aid tables], covers sec (join (Map.lookup t.structPath sections)) ]
  , length [ () | (aid, RuleBound r' _ _) <- Map.toList sigClasses, r' == r
                , Just s <- [Map.lookup aid (signatureUnits ir)], covers sec (Just s.section) ] )
  where
    tables   = numberedTables ir
    sections = tableSections ir

-- | The rule anchors that say nothing, for the rules of "Hsed.Spec.Registry":
--   a @rule X@ token whose scope binds no table and no signature block to X is
--   @W-RULE-SCOPE-EMPTY@ (bookkeeping: the anchor is misplaced, or the rule no
--   longer fires where it stands), and one whose scope another anchor of X
--   covers is @N-RULE-SCOPE-REDUNDANT@ (an annotation to drop, whose bytes
--   the tail budget pays for). A token naming a rule hsedc does not implement
--   is @W-RULE-UNIMPLEMENTED@'s, not these.
checkRuleAnchors :: SpecIR -> Map AnchorId Classification -> Map AnchorId Classification -> [Finding]
checkRuleAnchors ir classes sigClasses =
  concat [ findingsFor pa sec r | pa <- parsed, Right p <- [paPayload pa], Just sec <- [paAnchor pa >>= scopeAt]
                                , r <- ruleTokens p, isJust (ruleNamed r) ]
  where
    parsed = parseAnnots ir
    sid    = SpecId ir.spec
    scopeAt = \case
      DocId       -> Just WholeDocument
      SectionId s -> Just (Subtree s)
      _           -> Nothing
    here pa = locatedAt sid (paAnnot pa).id ((paAnnot pa).page + 1)
    describe = \case
      WholeDocument -> "the whole document"
      Subtree s     -> "section " <> s
    -- does one scope cover another
    coversScope o = \case
      WholeDocument -> o == WholeDocument
      Subtree s     -> covers o (Just s)
    findingsFor pa sec r =
      [ here pa (finding Warning "W-RULE-SCOPE-EMPTY"
          ("rule " <> ruleNameText r <> " binds no table and no signature block in " <> describe sec))
      | bindsWithin ir classes sigClasses sec r == (0, 0) ]
      <> [ here pa (finding Note "N-RULE-SCOPE-REDUNDANT"
             ("rule " <> ruleNameText r <> " here lies within its anchor's scope at " <> describe other <> ": drop one"))
         | other <- take 1 [ o | o <- scopesFrom parsed r, o /= sec, coversScope o sec ] ]

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
checkExemplars = checkExemplarsWith rules

-- | 'checkExemplars' under a set of rules. The exemplar of a rule binding
--   signature units is a signature unit, which need only be in the IR; that of
--   a rule binding paragraph units is a list item or paragraph, which the IR
--   need only have, once.
checkExemplarsWith :: [RuleDef] -> SpecIR -> [Finding]
checkExemplarsWith ruleSet ir = concatMap check [ r | r <- ruleSet, fst (exemplar r) == sid ]
  where
    sid    = SpecId ir.spec
    tables = numberedTables ir
    check r = case snd (exemplar r) of
      SigId _    -> sigExemplar r
      ItemId _ _ -> itemExemplar r
      _          -> tableExemplar r
    missing r why = [finding Error "C-EXEMPLAR" ("the exemplar " <> renderAnchorId (snd (exemplar r)) <> " of rule "
                       <> ruleNameText (ruleName r) <> " is not in the IR" <> why) & \f -> f { Cm.spec = Just sid }]
    sigExemplar r
      | Map.member (snd (exemplar r)) (signatureUnits ir) = []
      | otherwise = missing r ""
    itemExemplar r = either (missing r . (": " <>)) (const []) (anchorParagraph ir (snd (exemplar r)))
    tableExemplar r = case Map.lookup (snd (exemplar r)) tables of
      Nothing -> missing r ""
      Just t  ->
        let form   = measure sid t
            here   = locatedAt sid (renderAnchorId (snd (exemplar r))) (fromMaybe 1 (tablePage t))
            isRule = ruleName r `elem` knownRules
            joins  = case joinsTo r of
              Nothing     -> True
              Just target -> case L.find ((== target) . ruleName) ruleSet of
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
