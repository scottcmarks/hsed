{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.StateTable
Description : The stateTable reader: a state table's fields, set in each state, bound to hand-written code
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 (the special readers), example E10 and section 6.1. A
@stateTable "M.name"@ comment (Core Table 45: @stateTable "Sync.table45"@)
binds a table whose first column names states and whose other columns are
fields: each cell is the setting of its column's field in its row's state,
a constant the table prints (@0x01@) or a quantity it names in words (@Data
Length@). The reader is reached only by the comment: its name is no rule
token and it never fires.

The comment's qualified name is the hand-written code the table is bound to:
in Core, the synchronous protocol's law and the host's IF-RECV loop (design
M3, @Hsed.Law.Sync.table45@), which turns each quantity into a computation and
which the binding check of M3 resolves. Until then the name is recorded in the
generated value ('Hsed.Core.Sync.boundTo').

Check @C-STATE-TABLE@: the name is qualified (@Module.name@, its last part a
Haskell variable) and no other state table of the spec ends in the same
name; the table has a state column and at least one field; every row has a
cell for each column, a state, and a setting for each field; no state is
named twice.

Compiled into 'stateTablesModule': one @Hsed.Core.Sync.StateTable@ per table,
named by the comment's name's last part (@table45@), its fields the header
cells less the words they all end with (@Length Field Value@ and
@OutstandingData Field Value@ give @Length@ and @OutstandingData@).
-}
module Hsed.Spec.Rules.StateTable
  ( stateTableRule
  , stateTablesModule
    -- * Reading the bound tables
  , StateTableRead (..)
  , Setting (..)
  , stateTableReads
  , readSetting
  , fieldNames
  ) where

import qualified Data.Char        as DC
import qualified Data.Text        as DT
import qualified RIO.Char         as C
import qualified RIO.List         as L
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO

import           Hsed.Spec.Anchor (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import           Hsed.Spec.DSL    (Binding (..), SomeBinding (..), stateTable)
import qualified Hsed.Spec.Gen    as Gen
import           Hsed.Spec.IR     (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Shape  (Shape (..))


-- | The reader: reached only by a @stateTable@ comment.
stateTableRule :: RuleDef
stateTableRule = RuleDef
  { ruleName    = RuleName "stateTable"
  , bindsTo     = SomeBinding (stateTable "")
  , exemplar    = (SpecId "core-2.01", TableId 45)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ _ -> False
  , ruleIdent   = const Nothing
  , ruleModule  = stateTablesModule
  , ruleChecks  = \ctx bts -> concatMap (\r -> map (atTable ctx (stBound r)) (problems (readAll bts) r)) (readAll bts)
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.StateTables@.
stateTablesModule :: Gen.GenModule
stateTablesModule = Gen.GenModule
  { Gen.leaf        = "StateTables"
  , Gen.description = "State tables: the fields a TPer sets in each state, each table bound to hand-written code"
  , Gen.notes       = [ "Each table's comment names the hand-written code it is bound to (its boundTo),"
                      , "which turns each quantity the table names into a computation." ] }


-- * Reading the bound tables

-- | One state table, read under the errata set.
data StateTableRead = StateTableRead
  { stBound   :: !BoundTable
  , stQName   :: !Text               -- ^ the comment's qualified name, @Sync.table45@
  , stName    :: !Text               -- ^ its last part, the generated value's name
  , stHeaders :: ![Text]             -- ^ the field columns' header cells
  , stFields  :: ![Text]             -- ^ the field names ('fieldNames')
  , stRows    :: ![(Text, [Text])]   -- ^ each row's state and its field cells
  }

-- | What a cell gives its field.
data Setting
  = Constant !Natural !Text  -- ^ a number, with the digits as printed (@0x01@)
  | Quantity !Text           -- ^ a quantity named in words, its spacing normalised
  deriving (Eq, Show)

-- | Every table bound as a state table, in anchor order.
stateTableReads :: Context -> [StateTableRead]
stateTableReads ctx = readAll (boundTables (Just StateTable) ctx)

readAll :: [BoundTable] -> [StateTableRead]
readAll = map readOne
  where
    readOne bt =
      let t       = btRead bt
          header  = maybe [] (map cellText) (listToMaybe t.headerRows)
          qname   = fromMaybe "" (qnameOf bt)
          heads   = drop 1 header
      in StateTableRead
           { stBound = bt, stQName = qname, stName = T.takeWhileEnd (/= '.') qname
           , stHeaders = heads, stFields = fieldNames heads
           , stRows = map (stateAndCells . map cellText) t.rows }
    cellText :: CellIR -> Text
    cellText c = T.unwords (T.words c.text)
    stateAndCells = \case
      s : cells -> (s, cells)
      []        -> ("", [])

-- | The qualified name of a state table's binding, under its modifiers.
qnameOf :: BoundTable -> Maybe Text
qnameOf bt = case btBinding bt of
  SomeBinding b -> go b
  PolyBinding _ -> Nothing
  where
    go :: Binding s -> Maybe Text
    go = \case
      StateTableB q     -> Just q
      WithErratum _ _ b -> go b
      WithAs b          -> go b
      _                 -> Nothing

-- | The field names of the header cells: the cells less the words they all end
--   with, when there are two or more and that leaves each a distinct name; the
--   cells themselves otherwise.
fieldNames :: [Text] -> [Text]
fieldNames heads
  | length heads >= 2, not (null common), all (not . null) stripped, L.nub stripped == stripped = map T.unwords stripped
  | otherwise = heads
  where
    backwards = map (reverse . T.words) heads
    common    = case backwards of
      b : bs -> foldl' commonPrefix b bs
      []     -> []
    stripped  = map (reverse . drop (length common)) backwards
    commonPrefix a b = map fst (takeWhile (uncurry (==)) (zip a b))

-- | A cell's setting: a hexadecimal (@0x01@) or decimal number, else a quantity in words.
readSetting :: Text -> Setting
readSetting cell = case T.stripPrefix "0x" (T.toLower cell) of
  Just hex | not (T.null hex), T.all C.isHexDigit hex -> Constant (T.foldl' (\n c -> n * 16 + fromIntegral (DC.digitToInt c)) 0 hex) cell
  _ | not (T.null cell), T.all C.isDigit cell -> Constant (T.foldl' (\n c -> n * 10 + fromIntegral (DC.digitToInt c)) 0 cell) cell
    | otherwise -> Quantity cell


-- * The check

-- | What C-STATE-TABLE finds wrong with one table, given every state table of the spec.
problems :: [StateTableRead] -> StateTableRead -> [Finding]
problems everyTable r = map (finding Error "C-STATE-TABLE") $
  [ "the comment's name " <> tshow (stQName r) <> " is not a qualified name, Module.name" | not (qualified (stQName r)) ]
  <> [ "the comment's name ends in " <> stName r <> ", as that of " <> renderTable o <> " does" | o <- earlier ]
  <> [ "there is no field column, only the state column" | null (stHeaders r) ]
  <> [ "row " <> tshow i <> " has " <> tshow (1 + length cells) <> " cells, not " <> tshow (1 + length (stHeaders r))
     | (i, (_, cells)) <- numbered, length cells /= length (stHeaders r) ]
  <> [ "row " <> tshow i <> " names no state" | (i, (s, _)) <- numbered, T.null s ]
  <> [ "row " <> tshow i <> " (" <> s <> ") gives no setting for " <> f
     | (i, (s, cells)) <- numbered, (f, c) <- zip (stFields r) cells, T.null c ]
  <> [ "the state " <> tshow s <> " is named by rows " <> T.intercalate " and " (map tshow is)
     | (s, is) <- Map.toList (Map.fromListWith (flip (<>)) [ (s, [i]) | (i, (s, _)) <- numbered, not (T.null s) ]), length is > 1 ]
  where
    numbered = zip [1 :: Int ..] (stRows r)
    earlier  = [ o | o <- takeWhile ((/= btAnchor (stBound r)) . btAnchor . stBound) everyTable, stName o == stName r ]
    renderTable o = case btAnchor (stBound o) of
      TableId n -> "Table " <> tshow n
      a         -> renderAnchorId a

-- | @Module.Sub.name@: two or more parts, each an identifier, the modules
--   capitalised and the last a variable.
qualified :: Text -> Bool
qualified q = case reverse (DT.splitOn "." q) of
  name : modules@(_ : _) -> all ident (name : modules) && all (startsWith C.isUpper) modules && startsWith (\c -> C.isLower c || c == '_') name
  _                      -> False
  where
    ident p = not (T.null p) && T.all (\c -> C.isAlphaNum c || c == '_' || c == '\'') p
    startsWith f p = maybe False (f . fst) (T.uncons p)


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts =
  [ if null (problems reads r) then Emit (stateTableDecl ctx r) else Excluded (btAnchor (stBound r)) "C-STATE-TABLE failed"
  | r <- reads ]
  where reads = readAll bts

-- | @table45 :: StateTable@, its rows in the order printed.
stateTableDecl :: Context -> StateTableRead -> Gen.Decl
stateTableDecl ctx r = (Gen.decl p (stName r) body)
  { Gen.declImports = [Gen.ImportSome Gen.syncModule ["Setting (..)", "StateRow (..)", "StateTable (..)"]] }
  where
    bt   = stBound r
    p    = boundProvenance ctx bt
    body =
      [ Gen.provenanceComment p ]
      <> map ("--   " <>) (Gen.wrapText 86 (T.unwords
           [ "Bound by its comment to", stQName r <> ",", "the hand-written code that turns each Quantity into a"
           , "computation. Its fields, from the header cells:"
           , T.intercalate ", " [ tshow h <> " as " <> f | (h, f) <- zip (stHeaders r) (stFields r) ] <> "." ]))
      <> [ stName r <> " :: StateTable"
      , stName r <> " = StateTable"
      , "  { boundTo = " <> tshow (stQName r)
      , "  , fields  = [" <> T.intercalate ", " (map tshow (stFields r)) <> "]"
      , "  , states  =" ]
      <> Gen.listBlock "      " (map row (stRows r))
      <> [ "  }" ]
    row (s, cells) = ("StateRow " <> tshow s) : map ("    " <>) (Gen.listLines "" (map (setting . readSetting) cells))
    setting = \case
      Constant _ printed -> "Constant " <> printed
      Quantity q         -> "Quantity " <> tshow q
