{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.PropsReq
Description : The propsReq reader: what an SSC requires of each property, over the base's Properties tables
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 (the special readers) and example E14; M4a. A @propsReq@
comment binds a table of the @PropsReq@ shape (Opal Table 17, Properties
Requirements, PDF 33-34: a property a row, its TPer requirement and its host
requirement), which is read over the Properties tables of the spec or of its
base ("Hsed.Spec.Rules.Props": Core Table 167, @propsNames@, and Table 168,
@propsInitial@, found through @hsedc --base@ for an Opal run, design 13.26 (g)).
The reader is reached only by the comment: its name is no rule token and it
never fires.

The cells read as the table prints them (whitespace normalised):

* the TPer column: @(M|O|N) \<n\> minimum@, or @(M) VU@ (a value the vendor
  chooses), or @N/A@ and any words (not a TPer property);
* the host column: @(M|O) Initial Assumption: \<n\> Minimum allowed: \<n\>
  Maximum allowed: VU|\<n\>@ (each of the three a number or @VU@), @(N)@ and any
  words (a legal host property the TPer need not use), or @N/A@ and any words
  (not a host property).

Checks, each an Error at the row's page unless said otherwise:

* @C-PROPSREQ-ROW@: a property name that is no identifier, or a cell that
  does not read as above;
* @C-PROPSREQ-NAME@: a property the @propsNames@ table (of the spec, else of
  its base) does not give, or gives in two rows of this table; a requirement
  of a communicator that table says the property does not describe, or @N/A@
  for one it says it does; no @propsNames@ table to read at all; a second
  @propsReq@ table of the spec. A name that is one only ignoring case is the
  Warning @W-PROP-NAME-CASE@ (the Props reader's code, its rule applied: the
  property's own spelling is read);
* @C-PATCH-MONOTONE@: an Initial Assumption below the value the @propsInitial@
  table (of the spec, else of its base) gives the TPer's assumption about the
  host for that property, which the initial assumption patches (Opal Table 17
  raises Core Table 168's 1024, 1004 and 968 to 2048, 2028 and 1992, and keeps
  its three 1s).

Compiled into 'propsReqModule' (@Hsed.Gen.\<Spec\>.Properties@, the Props
reader's module, so that a spec's properties stand together):
@initialAssumptions :: [Assumptions]@, the @propsInitial@ table's assumptions
as read (the base's, for Opal: Core Table 168's two rows), the TPer's about the
host patched by name with the Initial Assumption cells (a name the row lacks
appended), each value saying where it comes from; and @propertyRequirements ::
[PropertyRequirement]@, one per row, the runtime types of
"Hsed.Core.Properties". A table with an Error is left out.
-}
module Hsed.Spec.Rules.PropsReq
  ( propsReqRule
  , propsReqModule
    -- * Reading the bound table
  , RequirementRead (..)
  , requirementReads
  , readRequirementWord
  , readTPerCell
  , readHostCell
  , labelled
    -- * The patch
  , Patched (..)
  , patchedAssumptions
  ) where

import qualified Data.Text                 as DT
import qualified RIO.Char                  as C
import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO

import           Hsed.Core.Properties      (Communicator (..), HostRequirement (..), Requirement (..),
                                            TPerRequirement (..))
import           Hsed.Spec.Anchor          (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common          (Finding, RuleName (..), Severity (..), SpecId (..), finding, isError)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.DSL             (SomeBinding (..), propsReq)
import qualified Hsed.Spec.Gen             as Gen
import           Hsed.Spec.IR              (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.Props     (AssumptionsRead (..), PropertyRead (..), assumptionValues, propertyReads,
                                            propsModule)
import           Hsed.Spec.Shape           (Shape (..))


-- | The reader: reached only by a @propsReq@ comment.
propsReqRule :: RuleDef
propsReqRule = RuleDef
  { ruleName    = RuleName "propsReq"
  , bindsTo     = SomeBinding propsReq
  , exemplar    = (SpecId "opal-2.30", TableId 17)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ _ -> False
  , ruleIdent   = const Nothing
  , ruleModule  = propsReqModule
  , ruleCodes   =
      [ CodeDef "C-PROPSREQ-ROW"   [Error] (Defect "a cell of a property requirements table that does not read: a name that is no identifier, a TPer cell other than (M|O|N) n minimum, (M) VU or N/A, a host cell other than (M|O) Initial Assumption: n Minimum allowed: n Maximum allowed: VU|n, (N) or N/A")
      , CodeDef "C-PROPSREQ-NAME"  [Error] (Defect "a property requirement naming no property of the Properties table (of the spec or its base), or a requirement of a communicator that table says the property does not describe")
      , CodeDef "C-PATCH-MONOTONE" [Error] (Defect "an initial assumption below the one it patches, the base's TPer assumption about the host") ]
  , ruleChecks  = \ctx bts -> fst (resolve ctx bts)
  , ruleCompile = compile
  }

-- | The runtime module of the requirement types: @PropertyRequirement@ and its parts.
propertiesModule :: Text
propertiesModule = "Hsed.Core.Properties"

-- | @Hsed.Gen.\<Spec\>.Properties@: the Props reader's module, with this reader's notes.
propsReqModule :: Gen.GenModule
propsReqModule = propsModule
  { Gen.notes = propsModule.notes
      <> [ "What the SSC requires of each property (propsReq) is a PropertyRequirement: the TPer's"
         , "and the host's requirement, each Nothing where the property is not that communicator's,"
         , "a minimum or maximum of VU Nothing; its initial assumptions are the base's, the TPer's"
         , "about the host patched by name with the table's Initial Assumption cells." ] }


-- * Reading the bound table

-- | One row of a @propsReq@ table, read under the errata set.
data RequirementRead = RequirementRead
  { rqBound    :: !BoundTable
  , rqRow      :: !Int                                   -- ^ 1-based among the data rows
  , rqName     :: !Text                                  -- ^ the property as printed
  , rqTPerText :: !Text                                  -- ^ the TPer cell, whitespace normalised
  , rqTPer     :: !(Either Text (Maybe TPerRequirement)) -- ^ Left why it does not read; Right Nothing for N/A
  , rqHostText :: !Text                                  -- ^ the host cell, whitespace normalised
  , rqHost     :: !(Either Text (Maybe HostRequirement)) -- ^ Left why it does not read; Right Nothing for N/A
  , rqCells    :: ![CellIR]
  }

-- | Every row of every @propsReq@ table, in anchor order.
requirementReads :: Context -> [RequirementRead]
requirementReads ctx = concatMap readTable (boundTables (Just PropsReq) ctx)

readTable :: BoundTable -> [RequirementRead]
readTable bt =
  [ RequirementRead
      { rqBound = bt, rqRow = i, rqName = cellAt 0 row
      , rqTPerText = cellAt 1 row, rqTPer = readTPerCell (cellAt 1 row)
      , rqHostText = cellAt 2 row, rqHost = readHostCell (cellAt 2 row)
      , rqCells = row }
  | (i, row) <- zip [1 ..] (btRead bt).rows ]

cellAt :: Int -> [CellIR] -> Text
cellAt i r = maybe "" (T.unwords . T.words . (.text)) (listToMaybe (drop i r))

-- | The requirement a cell starts with, @(M)@, @(O)@ or @(N)@, and the rest of
--   the cell; Nothing when it starts with none.
readRequirementWord :: Text -> Maybe (Requirement, Text)
readRequirementWord cell = case T.words cell of
  w : rest | Just r <- lookup w [("(M)", Mandatory), ("(O)", Optional), ("(N)", NotRequired)] -> Just (r, T.unwords rest)
  _ -> Nothing

-- | Is a cell @N/A@ (not a property of the communicator), whatever follows?
notApplicable :: Text -> Bool
notApplicable cell = take 1 (T.words cell) == ["N/A"]

-- | A decimal number, or @VU@ (Nothing: the vendor chooses).
readBound :: Text -> Maybe (Maybe Natural)
readBound w
  | w == "VU" = Just Nothing
  | not (T.null w), T.all C.isDigit w = Just <$> readMaybe (T.unpack w)
  | otherwise = Nothing

-- | A TPer cell: @(M) 2048 minimum@, @(M) VU@, or @N/A …@ (Right Nothing).
readTPerCell :: Text -> Either Text (Maybe TPerRequirement)
readTPerCell cell
  | notApplicable cell = Right Nothing
  | otherwise = case readRequirementWord cell of
      Just (r, rest) -> case T.words rest of
        [n, "minimum"] | Just (Just v) <- readBound n -> Right (Just (TPerRequirement r (Just v)))
        ["VU"]                                        -> Right (Just (TPerRequirement r Nothing))
        _ -> Left ("expected a minimum (2048 minimum) or VU after " <> T.take 3 cell)
      Nothing -> Left "expected (M), (O) or (N) first, or N/A"

-- | A host cell: @(M) Initial Assumption: 2048 Minimum allowed: 2048 Maximum
--   allowed: VU@ (each value a number or VU), @(N) …@ (a legal property the TPer
--   need not use: no values), or @N/A …@ (Right Nothing).
readHostCell :: Text -> Either Text (Maybe HostRequirement)
readHostCell cell
  | notApplicable cell = Right Nothing
  | otherwise = case readRequirementWord cell of
      Just (NotRequired, _) -> Right (Just (HostRequirement NotRequired Nothing Nothing Nothing))
      Just (r, rest) -> case labelled ["Initial Assumption:", "Minimum allowed:", "Maximum allowed:"] rest of
        Just [i, lo, hi] | Just i' <- readBound i, Just lo' <- readBound lo, Just hi' <- readBound hi ->
          Right (Just (HostRequirement r i' lo' hi'))
        _ -> Left ("expected Initial Assumption: n Minimum allowed: n Maximum allowed: VU|n after " <> T.take 3 cell)
      Nothing -> Left "expected (M), (O) or (N) first, or N/A"

-- | The values between labels that appear in order, the text starting with
--   the first and ending with the last's value: @labelled ["a:", "b:"] "a: 1
--   b: 2"@ is @Just ["1", "2"]@; Nothing where a label is missing or out of
--   place.
labelled :: [Text] -> Text -> Maybe [Text]
labelled [] text = if T.null (T.strip text) then Just [] else Nothing
labelled (l : rest) text = do
  after <- T.stripPrefix l (T.stripStart text)
  case rest of
    []       -> Just [T.strip after]
    next : _ -> do
      let (value, remaining) = DT.breakOn next after
      guard (not (T.null remaining))
      (T.strip value :) <$> labelled rest remaining


-- * Resolution and the checks

-- | One row's resolved reading, ready for a declaration.
data Resolved = Resolved
  { rsRead :: !RequirementRead
  , rsName :: !Text              -- ^ as the names table spells it
  , rsTPer :: !(Maybe TPerRequirement)
  , rsHost :: !(Maybe HostRequirement)
  }

-- | The TPer's assumption about the host for a property, patched or kept.
data Patched = Patched
  { ptName   :: !Text                 -- ^ the property, as the names table spells it
  , ptValue  :: !Natural              -- ^ the value: the table's initial assumption where it gives one, else the base's
  , ptBase   :: !(Maybe Natural)      -- ^ the base's value, where its row exists
  , ptSource :: !(Maybe (Int, Text))  -- ^ the row and host cell of the table that patches it
  }
  deriving (Eq, Show)

-- | A table of a spec as a finding or a comment of another spec names it:
--   @Table 168@ in its own spec, @core-2.01 Table 168@ from another.
tableIn :: Context -> Context -> BoundTable -> Text
tableIn ctx owner bt = (if cxSpec owner == cxSpec ctx then "" else specIdText (cxSpec owner) <> " ") <> case btAnchor bt of
  TableId n -> "Table " <> tshow n
  a         -> renderAnchorId a

-- | The resolved value columns of the @propsInitial@ table read: the spec's
--   where it has one, else its base's, each with the Context that owns it.
assumptionColumns :: Context -> [(Context, AssumptionsRead, [(Text, Text, Natural, Text)])]
assumptionColumns ctx = case (assumptionValues ctx, cxBase ctx) of
  (own@(_ : _), _) -> [ (ctx, c, values) | (c, values) <- own ]
  ([], Just base)  -> [ (base, c, values) | (c, values) <- assumptionValues base ]
  ([], Nothing)    -> []

-- | The findings of every table bound with the shape, and what the one table
--   read resolves to: its rows, or why it is left out.
resolve :: Context -> [BoundTable] -> ([Finding], [(BoundTable, Either Text [Resolved])])
resolve ctx bts = (findings, results)
  where
    reading = take 1 bts
    rows    = concatMap readTable reading
    here r  = atCells ctx (rqBound r) (rqCells r)
    label r = "row " <> tshow (rqRow r) <> " " <> rqName r <> ": "

    -- the Properties table of the spec, else of its base
    names = case (rights (propertyReads ctx), cxBase ctx) of
      (ps@(_ : _), _) -> Just (ctx, ps)
      ([], Just base) -> Just (base, rights (propertyReads base))
      ([], Nothing)   -> Nothing
    namesTable = do { (c, p : _) <- names; pure (tableIn ctx c (prBound p)) }
    byName   = Map.fromList [ (prName p, p) | Just (_, ps) <- [names], p <- ps ]
    byFolded = Map.fromListWith (<>) [ (T.toLower (prName p), [p]) | Just (_, ps) <- [names], p <- ps ]
    -- a property by its name: exactly, or only ignoring case (True)
    lookupProperty n = case (Map.lookup n byName, Map.lookup (T.toLower n) byFolded) of
      (Just p, _)   -> Just (p, False)
      (_, Just [p]) -> Just (p, True)
      _             -> Nothing
    spelled r = maybe (rqName r) (prName . fst) (lookupProperty (rqName r))

    rowFindings r = map (here r) $
      [ finding Error "C-PROPSREQ-ROW" (label r <> "the property's name is not an identifier") | not (identifier (rqName r)) ]
      <> [ finding Error "C-PROPSREQ-ROW" (label r <> "the TPer cell " <> tshow (rqTPerText r) <> " does not read: " <> why) | Left why <- [rqTPer r] ]
      <> [ finding Error "C-PROPSREQ-ROW" (label r <> "the host cell " <> tshow (rqHostText r) <> " does not read: " <> why) | Left why <- [rqHost r] ]
      <> case (namesTable, lookupProperty (rqName r)) of
           (Nothing, _) -> [ finding Error "C-PROPSREQ-NAME" (label r <> "no propsNames table of " <> specIdText (cxSpec ctx)
                                                               <> maybe "" (\b -> " or of its base, " <> specIdText (cxSpec b) <> ",") (cxBase ctx)
                                                               <> " names the properties") ]
           (Just t, Nothing) -> [ finding Error "C-PROPSREQ-NAME" (label r <> "names no property of " <> t) ]
           (Just t, Just (p, folded)) ->
             [ finding Warning "W-PROP-NAME-CASE" (label r <> "the property is " <> prName p <> " in " <> t <> ", which is read") | folded ]
             <> [ finding Error "C-PROPSREQ-NAME" (label r <> what <> ", but " <> t <> " says " <> prName p <> says)
                | (who, given) <- [ (TPer, isJust v) | Right v <- [rqTPer r] ] <> [ (Host, isJust v) | Right v <- [rqHost r] ]
                , let describes = who `elem` prApplies p
                      (what, says) = if given then ("a " <> T.toLower (tshow who) <> " requirement", " does not describe the " <> tshow who)
                                              else ("N/A", " describes the " <> tshow who)
                , given /= describes ]
    duplicates =
      [ here r (finding Error "C-PROPSREQ-NAME" (label r <> "the property is also row " <> tshow (rqRow first')))
      | rs <- Map.elems (Map.fromListWith (flip (<>)) [ (spelled r, [r]) | r <- rows ]), first' : rest <- [rs], r <- rest ]

    -- the TPer's assumptions about the host, by property, with the table they come from
    assumed = Map.fromList [ (n, (v, owner, c)) | (owner, c, values) <- assumptionColumns ctx, arWho c == Just (TPer, Host), (n, _, v, _) <- values ]
    monotone r = map (here r)
      [ finding Error "C-PATCH-MONOTONE" (label r <> "Initial Assumption " <> tshow i <> " is below " <> tableIn ctx owner (arBound c) <> "'s "
                                          <> tshow v <> " (" <> arHeader c <> "), the value it patches")
      | Right (Just h) <- [rqHost r], Just i <- [hrInitial h], Just (v, owner, c) <- [Map.lookup (spelled r) assumed], i < v ]

    secondTables = [ atTable ctx bt (finding Error "C-PROPSREQ-NAME" (tableIn ctx ctx bt <> " is a second propsReq table; " <> tableIn ctx ctx kept <> " is the one read"))
                   | kept : rest <- [bts], bt <- rest ]

    findings = concatMap rowFindings rows <> duplicates <> concatMap monotone rows <> secondTables
    failed bt = L.nub [ Cm.code f | f <- findings, isError f, Cm.anchor f == Just (renderAnchorId (btAnchor bt)) ]
    results =
      [ (bt, case failed bt of
               []    -> Right [ Resolved r (spelled r) t h | r <- rows, Right t <- [rqTPer r], Right h <- [rqHost r] ]
               codes -> Left (T.intercalate ", " codes <> " failed"))
      | bt <- reading ]
      <> [ (bt, Left "a second table of its kind") | bt <- drop 1 bts ]

-- | The TPer's assumptions about the host: the base's row patched by name with
--   a table's initial assumptions (a name the row lacks appended), each with
--   the row and host cell that patches it.
patchedAssumptions :: [(Text, Natural)] -> [Resolved] -> [Patched]
patchedAssumptions baseRow rs =
  [ Patched n (maybe v fst (lookup n patches)) (Just v) (snd <$> lookup n patches) | (n, v) <- baseRow ]
  <> [ Patched n v Nothing (Just src) | (n, (v, src)) <- patches, n `notElem` map fst baseRow ]
  where
    patches = [ (rsName r, (i, (rqRow (rsRead r), rqHostText (rsRead r)))) | r <- rs, Just h <- [rsHost r], Just i <- [hrInitial h] ]

identifier :: Text -> Bool
identifier n = case T.uncons n of
  Just (c, cs) -> (C.isAlpha c || c == '_') && T.all (\x -> C.isAlphaNum x || x == '_') cs
  Nothing      -> False


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = concat
  [ either (pure . Excluded (btAnchor bt)) (map Emit . decls ctx bt) result | (bt, result) <- snd (resolve ctx bts) ]

-- | A table's two declarations: the initial assumptions it patches, and its requirements.
decls :: Context -> BoundTable -> [Resolved] -> [Gen.Decl]
decls ctx bt rs = [assumptionsDecl, requirementsDecl]
  where
    p = boundProvenance ctx bt
    table = tableIn ctx ctx bt
    imports = [ Gen.ImportSome propertiesModule [ "Assumptions (..)", "Communicator (..)", "HostRequirement (..)", "PropertyRequirement (..)"
                                                 , "Requirement (..)", "TPerRequirement (..)" ]
              , Gen.ImportSome "Prelude" ["Maybe (..)"] ]
    columns = [ col | col@(_, c, _) <- assumptionColumns ctx, isJust (arWho c) ]
    source = listToMaybe [ (owner, c) | (owner, c, _) <- columns ]
    from = maybe table (\(owner, c) -> tableIn ctx owner (arBound c)) source

    assumptionsDecl = (Gen.decl p "initialAssumptions" body) { Gen.declImports = imports }
      where
        body = [ Gen.provenanceComment p ]
               <> map Gen.ascii (case source of
                    Just (owner, c) -> [ "--   " <> from <> "'s assumptions (PDF " <> tshow (boundProvenance owner (arBound c)).pdfPage1
                                         <> "), the TPer's about the host patched by name with " <> table <> "'s Initial"
                                       , "--   Assumption cells; each value says where it comes from." ]
                    Nothing -> [ "--   No propsInitial table is read: " <> table <> "'s Initial Assumption cells alone, the TPer's about the host." ])
               <> [ "initialAssumptions :: [Assumptions]", "initialAssumptions =" ]
               <> Gen.listBlock "  " (case columns of
                    [] -> [ ("Assumptions TPer Host  -- " <> table) : map ("  " <>) (Gen.listLines "" (map entry (patchedAssumptions [] rs))) ]
                    cs -> [ column owner c values | (owner, c, values) <- cs ])
        column owner c values = case arWho c of
          Just (TPer, Host) ->
            ("Assumptions TPer Host  -- " <> tableIn ctx owner (arBound c) <> " " <> tshow (arHeader c) <> ", patched by " <> table)
              : map ("  " <>) (Gen.listLines "" (map entry (patchedAssumptions [ (n, v) | (n, _, v, _) <- values ] rs)))
          Just (who, subject) ->
            ("Assumptions " <> tshow who <> " " <> tshow subject <> "  -- " <> tableIn ctx owner (arBound c) <> " " <> tshow (arHeader c) <> ", as it stands")
              : map ("  " <>) (Gen.listLines "" [ "(" <> tshow n <> ", " <> tshow v <> ")  -- " <> tableIn ctx owner (arBound c) | (n, _, v, _) <- values ])
          Nothing -> []
        entry pt = "(" <> tshow (ptName pt) <> ", " <> tshow (ptValue pt) <> ")  -- " <> case (ptSource pt, ptBase pt) of
          (Just (i, cell), Just v)  -> table <> " row " <> tshow i <> " " <> tshow (initialOf cell) <> " (" <> from <> ": " <> tshow v <> ")"
          (Just (i, cell), Nothing) -> table <> " row " <> tshow i <> " " <> tshow (initialOf cell) <> (if isJust source then " (no row of " <> from <> ")" else "")
          (Nothing, _)              -> from
        -- the Initial Assumption of a host cell, as printed: "Initial Assumption: 2048"
        initialOf cell = case DT.breakOn "Initial Assumption:" cell of
          (_, rest) | not (T.null rest) -> T.unwords (take 3 (T.words rest))
          _                             -> cell

    requirementsDecl = (Gen.decl p "propertyRequirements" body) { Gen.declRank = 1, Gen.declImports = imports }
      where
        body = [ Gen.provenanceComment p
               , "--   Each row: the property, as the Properties table spells it, and what " <> table <> " requires of"
               , "--   the TPer's value of it (reported) and of the host's (accepted); Nothing where the property"
               , "--   is not that communicator's, a minimum or maximum of VU Nothing. The cells as printed follow." ]
               <> [ "propertyRequirements :: [PropertyRequirement]", "propertyRequirements =" ]
               <> Gen.listBlock "  " (map row rs)
        row r = [ "PropertyRequirement " <> tshow (rsName r) <> "  -- " <> cells r
                , "  " <> maybe "Nothing" tper (rsTPer r)
                , "  " <> maybe "Nothing" host (rsHost r) ]
        cells r = T.intercalate "; " (map (tshow . Gen.ascii . shortened) [rqTPerText (rsRead r), rqHostText (rsRead r)])
                  <> (if rsName r /= rqName (rsRead r) then "; printed " <> tshow (rqName (rsRead r)) else "")
        shortened t = if T.length t > 84 then T.take 81 t <> "..." else t
        tper t = "(Just (TPerRequirement " <> tshow (tprRequired t) <> " " <> bound (tprMinimum t) <> "))"
        host h = "(Just (HostRequirement " <> tshow (hrRequired h) <> " " <> bound (hrInitial h) <> " " <> bound (hrMinimum h) <> " " <> bound (hrMaximum h) <> "))"
        bound = maybe "Nothing" (\v -> "(Just " <> tshow v <> ")")
