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
Description : The document-level rules, their scope, and the classification of every numbered table
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4. A 'Rule' fires on a table by its measured 'Form' (and the
table itself); it binds the table to its default binding wherever the rule is in
scope. Scope comes only from the anchors: a @rule X@ token on a heading anchor
governs that section's subtree, on the @doc@ anchor the whole document
('scopeOf'). Precedence: a rule may declare 'refinesRule' and wins where both
fire; any other double match is @E-AMBIGUOUS@.

'classify' puts every numbered table in one of four places:

* comment-bound: its annotation is a full binding (@feature@, @refines (core 42)@);
* rule-bound: no annotation and exactly one rule in scope fires, or an
  annotation that is a bare modifier chain, applied in order to the default of
  the single rule that fires (Table 75's @erratum scott [...]@);
* ambiguous: two rules fire and neither refines the other (@E-AMBIGUOUS@);
* unbound: nothing binds it. At M1 only section 5.1.3 is anchored, so this is
  the Warning @W-UNBOUND@ (design M2b promotes it to @E-UNBOUND@);

plus 'Invalid' for an annotation that does not parse or whose modifiers do not
apply. M1 implements the pair @typeRow@ and @enumValues@; the other rule names
of "Hsed.Spec.DSL" parse but bind nothing yet (@W-RULE-UNIMPLEMENTED@).
-}
module Hsed.Spec.Rules
  ( -- * Rules
    Rule (..)
  , rules
  , ruleNamed
  , typeRowRule
  , enumValuesRule
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
  , checkRulesAnchored
  ) where

import qualified Data.Text         as DT
import qualified RIO.Char          as C
import qualified RIO.List          as L
import qualified RIO.Map           as Map
import qualified RIO.Set           as Set
import qualified RIO.Text          as T
import           RIO

import           Hsed.Spec.Anchor  (AnchorId (..), parseAnchorId, renderAnchorId)
import           Hsed.Spec.Common  (Finding, RuleName (..), Severity (..), SpecId (..), finding,
                                    renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.DSL     (Binding (..), Modifier (..), Ref (..), SomeBinding (..),
                                    applyModifierSome, atShape, bindingAccepts, enumValues,
                                    knownRules, someBindingShape, typeRow)
import           Hsed.Spec.Grammar (Payload (..), parsePayload, printSomeBinding)
import           Hsed.Spec.IR      (AnnotIR (..), HeadingIR (..), SpecIR (..), StructPath (..),
                                    TableIR (..))
import           Hsed.Spec.Shape   (CaptionKind (..), Form (..), KnownShape, SShape, Shape (Doc), SomeShape (..),
                                    measure, projectColumns, toSomeShape)


-- * Rules

-- | A document-level rule (design section 4.2), plus 'joinsTo': the rule whose
--   table of the same identifier must exist somewhere in the document
--   (@enumValues@ joins the @typeRow@ of its type).
data Rule = Rule
  { ruleName    :: !RuleName
  , refinesRule :: !(Maybe RuleName)
  , exemplar    :: !(SpecId, AnchorId)
  , fires       :: SpecId -> TableIR -> Form -> Bool
  , bindsTo     :: !SomeBinding
  , joinsTo     :: !(Maybe RuleName)
  }

-- | The rules hsedc implements: the M1 pair.
rules :: [Rule]
rules = [typeRowRule, enumValuesRule]

ruleNamed :: RuleName -> Maybe Rule
ruleNamed n = L.find ((== n) . ruleName) rules

-- | @[UID, Name, Format]@ under a caption @Table N \<ident\>@; the seven
--   @\<X\>_object _ref@ captions are normalised by 'Hsed.Spec.Shape.captionTitle'.
typeRowRule :: Rule
typeRowRule = Rule
  { ruleName    = RuleName "typeRow"
  , refinesRule = Nothing
  , exemplar    = (SpecId "core-2.01", TableId 46)
  , fires       = \_ _ form -> fCols form == 3 && fHeaderNorm form == ["uid", "name", "format"]
                              && isJust (typeIdent form)
  , bindsTo     = SomeBinding typeRow
  , joinsTo     = Nothing
  }

-- | Two columns, the first headed @Enumeration Value@ or @Set Value@, under a
--   caption @\<ident\> Enumeration Values@ or @\<ident\> Set Values@, joined to
--   the @typeRow@ of the same identifier. A wider table reaches it only through
--   @cols a b@, which 'classify' applies to the form before asking.
enumValuesRule :: Rule
enumValuesRule = Rule
  { ruleName    = RuleName "enumValues"
  , refinesRule = Nothing
  , exemplar    = (SpecId "core-2.01", TableId 52)
  , fires       = \_ _ form -> fCols form == 2 && take 1 (fHeaderNorm form) `elem` [["enumerationvalue"], ["setvalue"]]
                              && isJust (enumIdent form)
  , bindsTo     = SomeBinding enumValues
  , joinsTo     = Just (RuleName "typeRow")
  }

typeIdent :: Form -> Maybe Text
typeIdent form = case fCaptionKind form of
  CaptionIdent s -> Just s
  _              -> Nothing

enumIdent :: Form -> Maybe Text
enumIdent form = case fCaptionKind form of
  CaptionEnumValues s -> ident s
  CaptionOther t      -> T.stripSuffix " Set Values" t >>= ident . T.strip
  _                   -> Nothing
  where ident s = if not (T.null s) && T.all (\c -> C.isAlphaNum c || c == '_') s then Just s else Nothing

-- | The identifier a table's caption names: the type of a type row or of an enumeration.
tableIdent :: Form -> Maybe Text
tableIdent form = typeIdent form <|> enumIdent form


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
scopeOf ir r = scopesFrom (snd (parseAnnots ir)) r

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

-- | Every annotation of the IR parsed; the findings are those of the ids that do not parse.
parseAnnots :: SpecIR -> ([Finding], [ParsedAnnot])
parseAnnots ir = (badIds, map parsed ir.annots)
  where
    parsed a = ParsedAnnot a (parseAnchorId a.id) (first (located a) (parsePayload a.contents))
    located a = locatedAt (SpecId ir.spec) a.id (a.page + 1)
    badIds = [ locatedAt (SpecId ir.spec) a.id (a.page + 1) (finding Error "E-ANCHOR" ("not an anchor id: " <> tshow a.id))
             | a <- ir.annots, isNothing (parseAnchorId a.id) ]

-- | Give a finding a location unless it has one.
locatedAt :: SpecId -> Text -> Int -> Finding -> Finding
locatedAt sid aid page f = f { Cm.spec = Cm.spec f <|> Just sid, Cm.anchor = Cm.anchor f <|> Just aid, Cm.page = Cm.page f <|> Just page }


-- * Classification

-- | Where a numbered table stands.
data Classification
  = CommentBound !SomeBinding                  -- ^ its annotation is a full binding
  | RuleBound !RuleName ![Modifier] !SomeBinding  -- ^ bound by a rule; the modifiers of a bare chain, and the result
  | Ambiguous ![RuleName]                      -- ^ two or more rules fire, neither refining the other
  | Unbound                                    -- ^ no annotation, no rule
  | Invalid !Finding                           -- ^ the annotation does not parse or does not apply
  deriving (Eq, Show)

-- | The binding a table ends up with, if any.
boundBinding :: Classification -> Maybe SomeBinding
boundBinding = \case
  CommentBound sb    -> Just sb
  RuleBound _ _ sb   -> Just sb
  _                  -> Nothing

-- | One line for a ledger or an explanation.
describeClassification :: Classification -> Text
describeClassification = \case
  RuleBound r [] _   -> "rule " <> ruleNameText r
  RuleBound r _ sb   -> "rule " <> ruleNameText r <> ", modified: " <> printSomeBinding sb
  CommentBound sb    -> "comment: " <> printSomeBinding sb
  Ambiguous rs       -> "ambiguous: " <> T.intercalate ", " (map ruleNameText rs)
  Unbound            -> "unbound"
  Invalid f          -> "invalid: " <> renderFinding f

-- | @typeRow (exemplar core T46)@ for a rule-bound table (design acceptance M1 (g)).
explain :: SpecIR -> AnchorId -> Text
explain ir aid = case Map.lookup aid (classify ir) of
  Nothing -> "no table " <> renderAnchorId aid <> " in " <> ir.spec
  Just c@(RuleBound r _ sb) -> case ruleNamed r of
    Just rl -> printSomeBinding sb <> " (exemplar " <> prefix (fst (exemplar rl)) <> " " <> renderAnchorId (snd (exemplar rl)) <> ")"
    Nothing -> describeClassification c
  Just c -> describeClassification c
  where prefix = T.toLower . T.takeWhile C.isAlpha . specIdText

-- | Every numbered table by its anchor id; a table Word split in two (Core T7)
--   is its first part.
numberedTables :: SpecIR -> Map AnchorId TableIR
numberedTables ir = Map.fromListWith earlier
  [ (aid, t) | t <- ir.tables, Just i <- [t.id], Just aid@(TableId _) <- [parseAnchorId i] ]
  where
    earlier a b = if order a <= order b then a else b
    order :: TableIR -> (Maybe Int, Maybe Int)
    order t = (t.part, t.ordinal)

-- | The 1-based PDF page of a table's first fragment.
tablePage :: TableIR -> Maybe Int
tablePage t = (+ 1) <$> listToMaybe t.pages

classify :: SpecIR -> Map AnchorId Classification
classify = fst . classifyWith

-- | The classification and its findings: parse failures, @E-AMBIGUOUS@,
--   @W-UNBOUND@, rule names hsedc does not implement yet.
classifyWith :: SpecIR -> (Map AnchorId Classification, [Finding])
classifyWith = classifyWithRules rules

-- | 'classifyWith' under another set of rules (the tests' ambiguous pairs).
classifyWithRules :: [Rule] -> SpecIR -> (Map AnchorId Classification, [Finding])
classifyWithRules ruleSet ir = (final, badIds <> orphanAnnots <> otherAnnots <> unimplemented <> concatMap located (Map.toList final))
  where
    sid      = SpecId ir.spec
    (badIds, parsed) = parseAnnots ir
    tables   = numberedTables ir
    sections = tableSections ir
    scopes   = Map.fromList [ (ruleName r, scopesFrom parsed (ruleName r)) | r <- ruleSet ]
    -- the identifiers of the tables each rule fires on, anywhere in the document, for the joins
    idents   = Map.fromList
      [ (ruleName r, Set.fromList [ i | t <- Map.elems tables, let form = measure sid t
                                      , fires r sid t form, Just i <- [tableIdent form] ])
      | r <- ruleSet ]
    commentsOn aid = [ pa | pa <- parsed, paAnchor pa == Just aid ]
    firing t form = precedence
      [ r | r <- ruleSet
          , any (`covers` Map.findWithDefault Nothing t.structPath sections) (Map.findWithDefault [] (ruleName r) scopes)
          , fires r sid t form
          , maybe True (\joined -> maybe False (`Set.member` Map.findWithDefault mempty joined idents) (tableIdent form)) (joinsTo r) ]
    precedence rs = [ r | r <- rs, not (any ((== Just (ruleName r)) . refinesRule) rs) ]

    first' = Map.mapWithKey classifyTable tables
    final  = Map.mapWithKey (resolvePoly first') first'

    here aid t = locatedAt sid (renderAnchorId aid) (fromMaybe 1 (tablePage t))

    classifyTable aid t = case commentsOn aid of
      []   -> byRules aid t []
      [pa] -> case paPayload pa of
        Left f               -> Invalid f
        Right (Full sb)      -> CommentBound sb
        Right (Modifiers ms) -> byRules aid t ms
      pas  -> Invalid (here aid t (finding Error "E-ANCHOR" (tshow (length pas) <> " annotations on one table")))

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

    -- annotations that are not about a table of this IR
    orphanAnnots = [ locatedAt sid a.id (a.page + 1) (finding Error "E-ANCHOR" ("no table " <> a.id <> " in the IR"))
                   | pa <- parsed, let a = paAnnot pa, Just aid@(TableId _) <- [paAnchor pa], not (Map.member aid tables) ]
    otherAnnots  = [ f | pa <- parsed, Just aid <- [paAnchor pa], not (isTable aid)
                       , f <- either pure (nonTablePayload (paAnnot pa)) (paPayload pa) ]
    nonTablePayload a = \case
      Modifiers _ -> [locatedAt sid a.id (a.page + 1) (finding Error "E-MODIFIER" "a bare modifier chain binds only a table")]
      Full _      -> []
    isTable = \case
      TableId _ -> True
      _         -> False
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

-- | Each exemplar still fires its rule and is accepted by the rule's default
--   binding (design section 3.4, re-verified on every run): @C-EXEMPLAR@.
checkExemplars :: SpecIR -> [Finding]
checkExemplars ir = concatMap check [ r | r <- rules, fst (exemplar r) == sid ]
  where
    sid    = SpecId ir.spec
    tables = numberedTables ir
    check r = case Map.lookup (snd (exemplar r)) tables of
      Nothing -> [finding Error "C-EXEMPLAR" ("the exemplar " <> renderAnchorId (snd (exemplar r)) <> " of rule "
                    <> ruleNameText (ruleName r) <> " is not in the IR") & \f -> f { Cm.spec = Just sid }]
      Just t  ->
        let form  = measure sid t
            here  = locatedAt sid (renderAnchorId (snd (exemplar r))) (fromMaybe 1 (tablePage t))
            joins = case joinsTo r of
              Nothing     -> True
              Just target -> case ruleNamed target of
                Nothing -> False
                Just tr -> any (\u -> let f = measure sid u in fires tr sid u f && tableIdent f == tableIdent form) (Map.elems tables)
            shape = case bindsTo r of
              SomeBinding b -> bindingAccepts b form
              PolyBinding _ -> Nothing
        in map here (catMaybes
             [ guarded (not (fires r sid t form)) (finding Error "C-EXEMPLAR" ("rule " <> ruleNameText (ruleName r) <> " no longer fires on its exemplar"))
             , guarded (not joins) (finding Error "C-EXEMPLAR" ("rule " <> ruleNameText (ruleName r) <> ": the exemplar has no table to join"))
             , shape ])
    guarded c f = if c then Just f else Nothing

-- | @C-RULE-ANCHORED@: every rule that binds a table has an anchor in this spec.
checkRulesAnchored :: SpecIR -> Map AnchorId Classification -> [Finding]
checkRulesAnchored ir classes =
  [ (finding Error "C-RULE-ANCHORED" ("rule " <> ruleNameText r <> " binds " <> tshow n <> " tables and has no anchor in " <> ir.spec))
      { Cm.spec = Just (SpecId ir.spec) }
  | (r, n) <- Map.toList usage, null (scopeOf ir r) ]
  where
    usage = Map.fromListWith (+) [ (r, 1 :: Int) | RuleBound r _ _ <- Map.elems classes ]
