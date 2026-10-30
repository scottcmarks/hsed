{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.Props
Description : The Props reader: the Properties method's properties and the initial assumptions
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 (the special readers) and example E14. Two comments bind a
table with the @Props@ shape, and this one reader reads both, telling them
apart by the binding:

* @propsNames@ (Core Table 167, Properties Method Response): each row a
  property, its Type object (the Type column, a type row's name: @uinteger@,
  @boolean@) and the communicators it describes (the Applicable To column,
  @Host Property and TPer Property@);
* @propsInitial@ (Core Table 168, Communications Initial Assumptions): each
  row a property, each further column what one communicator assumes of the
  other's value of it until a Properties exchange has succeeded (Core
  5.2.2.4), the header naming the two (@Initial Host Assumption About TPer@).

The reader is reached only by the comments: its name is no rule token and it
never fires.

Checks, each with the code its findings carry:

* @C-PROP-NAME@: a property names itself (an identifier) once; every
  property an initial assumption names is a property of the @propsNames@
  table; one table of each kind. A name that is one only ignoring case is
  the Warning @W-PROP-NAME-CASE@, and the property's own spelling is used
  (Core Table 168 prints @AckNAK@, Table 167 @AckNak@);
* @C-PROP-TYPE@: the Type cell names a Type object (a table bound as a type
  row, "Hsed.Spec.Rules.TypeRow") and the Applicable To cell a communicator;
* @C-PROP-INITIAL@: a header names the communicator assuming and the one it
  assumes about; every assumed value reads as a value of the property's type
  (a label of its enumeration, "Hsed.Spec.Rules.EnumValues", for an
  enumeration such as @boolean@; a number otherwise, the last term of a chain
  @a = b = n@ whose every arithmetic term comes to @n@); the property describes
  the communicator assumed about; no row names a property twice.

Compiled into 'propsModule': @properties :: [Property]@ (each type a
@TypeName@, its text and its type row's @u\<type\>Type@,
"Hsed.Spec.Rules.TypeRow".'typeNameExpr') and @initialAssumptions :: [Assumptions]@ (one per
assuming communicator, a boolean as its enumeration value), the runtime
types of "Hsed.Core.Properties".
-}
module Hsed.Spec.Rules.Props
  ( propsRule
  , propsModule
    -- * Reading the bound tables
  , PropsKind (..)
  , propsKind
  , PropertyRead (..)
  , propertyReads
  , AssumptionsRead (..)
  , assumptionsReads
  , communicatorsIn
  , readValue
  ) where

import qualified Data.Text                 as DT
import qualified RIO.Char                  as C
import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO

import           Hsed.Core.Properties      (Communicator (..))
import           Hsed.Core.Types           (TypeDef (..))
import           Hsed.Spec.Anchor          (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common          (Finding, RuleName (..), Severity (..), SpecId (..), finding, isError)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.DSL             (Binding (..), SomeBinding (..), propsNames)
import qualified Hsed.Spec.Gen             as Gen
import           Hsed.Spec.IR              (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.EnumValues (EnumRead (..), enumReads, parseEnumValue)
import           Hsed.Spec.Rules.TypeRow   (TypeRowRead (..), typeNameExpr, typeRowReads, typeUIDName, typesModule)
import           Hsed.Spec.Shape           (Shape (..), normHeader)


-- | The reader: reached only by a @propsNames@ or @propsInitial@ comment.
propsRule :: RuleDef
propsRule = RuleDef
  { ruleName    = RuleName "props"
  , bindsTo     = SomeBinding propsNames
  , exemplar    = (SpecId "core-2.01", TableId 167)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ _ -> False
  , ruleIdent   = const Nothing
  , ruleModule  = propsModule
  , ruleCodes   =
      [ CodeDef "C-PROP-TYPE"      [Error]          (Defect "a property's type names no Type object, or its applicability no communicator")
      , CodeDef "C-PROP-INITIAL"   [Error]          (Defect "an initial assumption does not read by its property's type, or its arithmetic does not hold")
      , CodeDef "C-PROP-NAME"      [Error]          (Defect "a property name the Properties tables do not give")
      , CodeDef "W-PROP-NAME-CASE" [Warning]        (AsPrinted "a property name printed in two spellings, read as the Properties table spells it") ]
  , ruleChecks  = \ctx bts -> fst (resolve ctx bts)
  , ruleCompile = compile
  }

-- | The runtime module of the Properties tables: @Property@, @Communicator@, @Assumptions@.
propertiesModule :: Text
propertiesModule = "Hsed.Core.Properties"

-- | @Hsed.Gen.\<Spec\>.Properties@.
propsModule :: Gen.GenModule
propsModule = Gen.GenModule
  { Gen.leaf        = "Properties"
  , Gen.description = "The Properties method's properties, and what host and TPer assume of each other's"
  , Gen.notes       = [ "A value is the one the Properties method carries: a uinteger as itself, a"
                      , "boolean as its enumeration value." ] }


-- * Reading the bound tables

-- | Which of the two comments binds a table.
data PropsKind = Names | Initial
  deriving (Eq, Show)

-- | The kind of a @Props@ binding, under its modifiers.
propsKind :: SomeBinding -> Maybe PropsKind
propsKind = \case
  SomeBinding b -> go b
  PolyBinding _ -> Nothing
  where
    go :: Binding s -> Maybe PropsKind
    go = \case
      PropsNames        -> Just Names
      PropsInitial      -> Just Initial
      WithErratum _ _ b -> go b
      WithAs b          -> go b
      _                 -> Nothing

-- | One row of a @propsNames@ table, read under the errata set.
data PropertyRead = PropertyRead
  { prBound       :: !BoundTable
  , prRow         :: !Int                    -- ^ 1-based among the data rows
  , prName        :: !Text
  , prTypeText    :: !Text                   -- ^ the Type cell
  , prType        :: !(Maybe TypeRowRead)    -- ^ the type row it names
  , prAppliesText :: !Text                   -- ^ the Applicable To cell
  , prApplies     :: ![Communicator]
  , prCells       :: ![CellIR]
  }

-- | One value column of a @propsInitial@ table, read under the errata set.
data AssumptionsRead = AssumptionsRead
  { arBound  :: !BoundTable
  , arHeader :: !Text
  , arWho    :: !(Maybe (Communicator, Communicator))  -- ^ the one assuming, the one assumed about
  , arValues :: ![(Int, Text, Text)]                    -- ^ each row's number (1-based), property and value cell
  }

-- | Every row of every @propsNames@ table, in anchor order; a table without a
--   Type or an Applicable To column is a finding.
propertyReads :: Context -> [Either Finding PropertyRead]
propertyReads ctx = readNames ctx (boundTables (Just Props) ctx)

-- | Every value column of every @propsInitial@ table, in anchor order.
assumptionsReads :: Context -> [AssumptionsRead]
assumptionsReads ctx = readInitial (boundTables (Just Props) ctx)

ofKind :: PropsKind -> [BoundTable] -> [BoundTable]
ofKind k = filter ((== Just k) . propsKind . btBinding)

headerOf :: BoundTable -> [Text]
headerOf bt = maybe [] (map (.text)) (listToMaybe (btRead bt).headerRows)

cellAt :: Int -> [CellIR] -> Text
cellAt i r = maybe "" (T.unwords . T.words . (.text)) (listToMaybe (drop i r))

readNames :: Context -> [BoundTable] -> [Either Finding PropertyRead]
readNames ctx bts = concatMap readTable (ofKind Names bts)
  where
    types = rights (typeRowReads ctx)
    readTable bt = case (column "type", column' ("applicable" `T.isPrefixOf`)) of
      (Just ti, Just ai) ->
        [ Right PropertyRead
            { prBound = bt, prRow = i, prName = cellAt 0 row, prTypeText = cellAt ti row
            , prType = L.find ((== cellAt ti row) . trName) types
            , prAppliesText = cellAt ai row, prApplies = communicatorsIn (cellAt ai row), prCells = row }
        | (i, row) <- zip [1 ..] (btRead bt).rows ]
      _ -> [Left (atTable ctx bt (finding Error "C-PROP-TYPE"
                   ("the header " <> tshow (headerOf bt) <> " has no Type column or no Applicable To column")))]
      where
        column h = L.elemIndex h (map normHeader (headerOf bt))
        column' p = L.findIndex p (map normHeader (headerOf bt))

readInitial :: [BoundTable] -> [AssumptionsRead]
readInitial bts =
  [ AssumptionsRead
      { arBound = bt, arHeader = h, arWho = case communicatorsIn h of { [a, b] -> Just (a, b); _ -> Nothing }
      , arValues = [ (i, cellAt 0 row, cellAt j row) | (i, row) <- zip [1 ..] (btRead bt).rows ] }
  | bt <- ofKind Initial bts, (j, h) <- drop 1 (zip [0 ..] (map (T.unwords . T.words) (headerOf bt))) ]

-- | The communicators a cell names, in the order it names them: @Host Property
--   and TPer Property@ gives Host and TPer, @Initial TPer Assumption About
--   Host@ TPer and Host.
communicatorsIn :: Text -> [Communicator]
communicatorsIn cell = L.nub [ c | w <- T.split (not . C.isAlphaNum) cell, c <- [minBound .. maxBound], tshow c == w ]

-- | An assumed value of a property of the given type: a label of the type's
--   enumeration (its labelled values given), or a number, the last term of a
--   chain @a = b = n@ whose every arithmetic term comes to @n@ (an en dash read
--   as a minus); Left says why it reads as neither.
readValue :: Maybe [(Text, [Int])] -> Text -> Either Text Natural
readValue labels cell = case labels of
  Just ls -> case [ vs | (l, vs) <- ls, l == cell ] of
    [[v]] | v >= 0 -> Right (fromIntegral v)
    []             -> Left ("is no label of the type's enumeration (" <> T.intercalate ", " (map fst ls) <> ")")
    _              -> Left "labels more than one value of the type's enumeration"
  Nothing -> case reverse (map T.strip (DT.splitOn "=" (T.map dash cell))) of
    final : terms | Just n <- arith final, n >= 0 ->
      if all (maybe True (== n) . arith) terms then Right (fromIntegral n) else Left ("does not come to " <> tshow n)
    _ -> Left "is not a number"
  where
    dash c = if c == '\x2013' then '-' else c
    numeric t = not (T.null t) && T.all (\c -> C.isDigit c || c `elem` ("+- " :: String)) t
    -- a sum of decimal terms: 1024 - 20 - 24 - 12
    arith :: Text -> Maybe Integer
    arith t
      | not (numeric t) = Nothing
      | otherwise = case T.words (spaced t) of
          lead : rest -> (\a b -> a + sum b) <$> number lead <*> pairs rest
          []           -> Nothing
    spaced = DT.replace "-" " - " . DT.replace "+" " + "
    pairs = \case
      op : n : rest -> (:) <$> (signed op <*> number n) <*> pairs rest
      []            -> Just []
      _             -> Nothing
    signed = \case
      "+" -> Just id
      "-" -> Just negate
      _   -> Nothing
    number n = if not (T.null n) && T.all C.isDigit n then readMaybe (T.unpack n) else Nothing


-- * Resolution and the checks

-- | One property's resolved reading, ready for a declaration.
data Resolved = Resolved
  { rsTable  :: !BoundTable
  , rsProps  :: ![(PropertyRead, TypeRowRead)]          -- ^ the names table's rows, every one resolved
  , rsAssume :: ![(AssumptionsRead, [(Text, Text, Natural, Text)])]  -- ^ each value column: property, its name as printed, value, cell
  }

-- | The findings of every table bound with the shape, and what each table that
--   has no Error resolves to: its properties, or its assumptions.
resolve :: Context -> [BoundTable] -> ([Finding], [(BoundTable, Either Text Resolved)])
resolve ctx bts = (findings, results)
  where
    nameTables = ofKind Names bts
    initTables = ofKind Initial bts
    reads      = readNames ctx bts
    rows       = rights reads
    defining   = take 1 nameTables
    definedRows = [ r | r <- rows, btAnchor (prBound r) `elem` map btAnchor defining ]
    byName     = Map.fromList [ (prName r, r) | r <- definedRows ]
    byFolded   = Map.fromListWith (<>) [ (T.toLower (prName r), [r]) | r <- definedRows ]
    enumLabels = Map.fromList [ (i, [ (l, vs) | (c, l) <- erRows e, Just vs <- [parseEnumValue (erRangeWords e) c] ])
                              | Right e <- enumReads ctx, Just i <- [erIdent e] ]

    here bt = atTable ctx bt
    secondTable k kept bt = here bt (finding Error "C-PROP-NAME"
      (describe bt <> " is a second " <> k <> " table; " <> describe kept <> " is the one read"))
    describe bt = case btAnchor bt of
      TableId n -> "Table " <> tshow n
      a         -> renderAnchorId a

    -- the names tables
    nameFindings = lefts reads
      <> [ secondTable "propsNames" kept bt | kept : rest <- [nameTables], bt <- rest ]
      <> concatMap rowFindings rows
      <> [ atCells ctx (prBound r) (prCells r) (finding Error "C-PROP-NAME" ("the property " <> prName r <> " is also row " <> tshow (prRow first')))
         | rs <- Map.elems (Map.fromListWith (flip (<>)) [ ((btAnchor (prBound r), prName r), [r]) | r <- rows ])
         , first' : rest <- [rs], r <- rest ]
    rowFindings r = map (atCells ctx (prBound r) (prCells r)) $
      [ finding Error "C-PROP-NAME" (label r <> "the property's name is not an identifier") | not (identifier (prName r)) ]
      <> [ finding Error "C-PROP-TYPE" (label r <> "the Type " <> tshow (prTypeText r) <> " names no type row") | isNothing (prType r) ]
      <> [ finding Error "C-PROP-TYPE" (label r <> "Applicable To " <> tshow (prAppliesText r) <> " names neither communicator")
         | null (prApplies r) ]
    label r = "row " <> tshow (prRow r) <> " " <> prName r <> ": "

    -- the initial assumptions tables
    columns = readInitial bts
    initFindings =
      [ secondTable "propsInitial" kept bt | kept : rest <- [initTables], bt <- rest ]
      <> [ here (arBound c) (finding Error "C-PROP-NAME" ("no table is bound propsNames to name the properties " <> describe (arBound c) <> " assumes"))
         | null nameTables, c <- take 1 columns ]
      <> concatMap nameFindings' initTables
      <> concatMap columnFindings columns
      <> [ here bt (finding Error "C-PROP-INITIAL" ("the property " <> n <> " is named by rows " <> T.intercalate " and " (map tshow is)))
         | bt <- initTables
         , (n, is) <- Map.toList (Map.fromListWith (flip (<>)) [ (n, [i]) | c <- take 1 (columnsOf bt), (i, n, _) <- arValues c ])
         , length is > 1 ]
    columnsOf bt = [ c | c <- columns, btAnchor (arBound c) == btAnchor bt ]
    -- the names a table's rows give, read once, with its first column
    nameFindings' bt = map (here bt) (mapMaybe (\(i, n, _) -> nameFinding i n) (concatMap arValues (take 1 (columnsOf bt))))
    columnFindings c = map (here (arBound c)) $ case arWho c of
      Nothing -> [ finding Error "C-PROP-INITIAL" ("the header " <> tshow (arHeader c) <> " does not name the communicator assuming and the one assumed about") ]
      Just (_, subject) -> concat [ valueFindings c subject v | v <- arValues c ]
    nameFinding i n = case lookupProperty n of
      Left f            -> Just (f (rowLabel i n))
      Right (p, True)   -> Just (finding Warning "W-PROP-NAME-CASE" (rowLabel i n <> "the property is " <> prName p <> " in "
                                                                     <> describe (prBound p) <> ", which is read"))
      Right (_, False)  -> Nothing
    valueFindings c subject (i, n, cell) = case lookupProperty n of
      Left _ -> []
      Right (p, _) ->
        [ finding Error "C-PROP-INITIAL" (rowLabel i n <> arHeader c <> ": " <> tshow cell <> " " <> why)
           | Just tr <- [prType p], Left why <- [readValue (labelsOf tr) cell] ]
        <> [ finding Error "C-PROP-INITIAL" (rowLabel i n <> arHeader c <> ": " <> describe (prBound p) <> " says " <> prName p
                                             <> " does not describe the " <> tshow subject)
           | subject `notElem` prApplies p ]
    rowLabel i n = "row " <> tshow i <> " " <> n <> ": "

    -- a property by its name: exactly, or only ignoring case (True)
    lookupProperty n = case (Map.lookup n byName, Map.lookup (T.toLower n) byFolded) of
      (Just p, _)  -> Right (p, False)
      (_, Just [p]) -> Right (p, True)
      _ -> Left (\l -> finding Error "C-PROP-NAME" (l <> "names no property of " <> maybe "a propsNames table" describe (listToMaybe defining)))
    labelsOf tr = case trTypeDef tr of
      Right (EnumType _) -> Just (Map.findWithDefault [] (trName tr) enumLabels)
      Right (SetType _)  -> Just (Map.findWithDefault [] (trName tr) enumLabels)
      _                  -> Nothing

    findings = nameFindings <> initFindings

    failed bt = any (\f -> isError f && Cm.anchor f == Just (renderAnchorId (btAnchor bt))) findings
    results =
      [ (bt, if failed bt then Left "C-PROP-NAME, C-PROP-TYPE or C-PROP-INITIAL failed" else Right (resolved bt))
      | bt <- take 1 nameTables <> take 1 initTables ]
      <> [ (bt, Left "a second table of its kind") | bt <- drop 1 nameTables <> drop 1 initTables ]
    resolved bt = Resolved
      { rsTable  = bt
      , rsProps  = [ (r, tr) | r <- definedRows, btAnchor (prBound r) == btAnchor bt, Just tr <- [prType r] ]
      , rsAssume = [ (c, [ (prName p, n, v, cell) | (_, n, cell) <- arValues c, Right (p, _) <- [lookupProperty n]
                                                , Just tr <- [prType p], Right v <- [readValue (labelsOf tr) cell] ])
                   | c <- columnsOf bt ] }

identifier :: Text -> Bool
identifier n = case T.uncons n of
  Just (c, cs) -> (C.isAlpha c || c == '_') && T.all (\x -> C.isAlphaNum x || x == '_') cs
  Nothing      -> False


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts =
  [ either (Excluded (btAnchor bt)) (Emit . decl ctx) result | (bt, result) <- snd (resolve ctx bts) ]

decl :: Context -> Resolved -> Gen.Decl
decl ctx rs = case propsKind (btBinding bt) of
  Just Initial -> assumptionsDecl
  _            -> propertiesDecl
  where
    bt = rsTable rs
    p  = boundProvenance ctx bt
    typeName tr = typeUIDName (trName tr)
    propertiesDecl = (Gen.decl p "properties" body)
      { Gen.declImports = [ Gen.ImportSome propertiesModule ["Communicator (..)", "Property (..)"]
                          , Gen.ImportSome Gen.typeNameModule ["TypeName (..)"]
                          , Gen.ImportSome (Gen.moduleName (cxSpec ctx) typesModule) (L.nub (map (typeName . snd) (rsProps rs)))
                          , Gen.ImportSome "Prelude" ["Maybe (..)"] ] }
      where
        body = [ Gen.provenanceComment p
               , "properties :: [Property]"
               , "properties =" ]
               <> Gen.listLines "  " [ "Property " <> tshow (prName r) <> " " <> typeNameExpr (prTypeText r) (Just tr) <> " " <> communicators (prApplies r)
                                 | (r, tr) <- rsProps rs ]
    assumptionsDecl = (Gen.decl p "initialAssumptions" body)
      { Gen.declImports = [ Gen.ImportSome propertiesModule ["Assumptions (..)", "Communicator (..)"] ] }
      where
        body = [ Gen.provenanceComment p
               , "--   Each communicator's assumptions about the other's properties until a Properties"
               , "--   exchange has succeeded; a boolean as its enumeration value."
               , "initialAssumptions :: [Assumptions]"
               , "initialAssumptions =" ]
               <> concat (zipWith column [0 :: Int ..] (rsAssume rs)) <> ["  ]"]
        column i (c, values) = case arWho c of
          Just (who, subject) ->
            ((if i == 0 then "  [ " else "  , ") <> "Assumptions " <> tshow who <> " " <> tshow subject <> "  -- " <> tshow (arHeader c))
              : map ("      " <>) (Gen.listLines "" [ pair n printed v cell | (n, printed, v, cell) <- values ])
          Nothing -> []
        -- the name as printed where W-PROP-NAME-CASE read it as the names table spells it, and the cell where it is no number
        pair n printed v cell = "(" <> tshow n <> ", " <> tshow v <> ")" <> case
          ([ "printed " <> tshow printed | printed /= n ], [ tshow cell | tshow v /= cell ]) of
            ([], [])  -> ""
            (as, bs)  -> "  -- " <> T.intercalate ": " (as <> bs)
    communicators cs = "[" <> T.intercalate ", " (map tshow cs) <> "]"
