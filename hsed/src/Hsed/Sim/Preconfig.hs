{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Preconfig
Description : An SP's tables folded from its preconfiguration: templates instantiated, the TPer's cells chosen, every column's default, the parameters' overrides, a row for every object
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What an SP holds when it is created, folded from the SSC's preconfiguration
tables ("Hsed.Core.Preconfig"'s 'PreconfTable': Opal 2.30 4.2-4.3, PDF
35-89) into the simulator's form ("Hsed.Sim.Types".@SPState@): each table's
description and its rows, both by the table's UID (Core Table 240's UID of
Table), every row its cells by column number (design docs/spec-as-source.md
sections 8.1 and 8.3; M4b). 'foldTables' does it in five passes:

* (a) templates: a template row (@(+XX)@) is one row for each instance the
  TPer holds beyond the rows its table prints ('instances'), its UID, names
  and references those of the instance ("Hsed.Core.Preconfig".'instantiate');
* (b) cells: a row keeps the cells its table prints, takes the TPer's
  choice where the table leaves the value to it ('vendorCell': @VU@, @MSID@),
  and every other column's default but the UID's, so that every column a Get
  may return holds a value (Core 5.3.4.2.2, PDF 223; design 13.31 (a));
* (c) overrides: the parameters choose two cells the Admin SP's tables
  print, the Locking SP's life cycle state and TPerInfo's
  ProgrammaticResetEnable;
* (d) every object: a row printed @TT TT TT TT@ after a half-UID is one
  AccessControl row for each object of that table, over the rows (a) to (c)
  give, ordered by UID (Opal 2.30 *AC1-*AC7, PDF 38-39);
* (e) the result: the descriptions and the rows by the table's UID, the rows
  in the tables' order, the fold adding no object a table holds already,
  and every cell evaluated as soon as the tables are.

The fold reads a run's parameters and nothing of the state, so that the
initial state can run it ("Hsed.Sim.Types".@initialState@), and a Revert can
run it again (M5); it imports the parameters ("Hsed.Sim.Params"), hsed-core,
the generated Core modules and the Admin SP's hand-written rows
("Hsed.Sim.AdminSP"), its stand-in until M4b's generated preconfiguration,
never a module that imports the state's types. The stand-in prints no
template, no cell the TPer chooses, no default and no every-object row, and
its cells are those the parameters choose, the Locking SP's life cycle
among them, so it folds to its rows as written.
-}
module Hsed.Sim.Preconfig
  ( adminSPTables
  , foldTables
  , vendorCell
  , instances
  ) where

import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import           RIO
import qualified RIO.Set                  as Set
import qualified RIO.Text                 as T

import           Hsed.Core.Preconfig      (Placeholder (..), PreCell (..), PreconfRow (..), PreconfTable (..), RowKind (..),
                                           instanceUID, instantiate)
import           Hsed.Core.Table          (Cell (..), Col (..), ColNum, Pre (..), Row, TableDesc (..), columnNamed,
                                           columnNumbered)
import           Hsed.Core.Types          (enumValue)
import           Hsed.Core.UID            (HalfUID, UID, uidUpper)
import           Hsed.Gen.Core.Enums      (Core_boolean (..))
import           Hsed.Gen.Core.TableUIDs  (uAuthorityTable, uC_PINTable, uSPTable, uTPerInfoTable, uTemplateTable)
import           Hsed.Gen.Core.UIDs       (uC_PIN_SID, uTemplate_Base)
import           Hsed.OpalUIDs            (uSP_Locking)
import           Hsed.Sim.AdminSP         (handTables)
import           Hsed.Sim.Params          (LifeCycle (..), SimParams (..), lifeCycleValue)


-- $setup
-- >>> :set -XOverloadedStrings


-- | The Admin SP's preconfiguration tables for a run: until M4b's generated
--   Opal tables (Opal 2.30 Tables 18-25, 27-29 and 33, PDF 35-52), the
--   fourteen hand-written rows of six of them, the cells the parameters
--   choose already filled ("Hsed.Sim.AdminSP".'handTables').
adminSPTables :: SimParams -> [PreconfTable]
adminSPTables = handTables

-- | An SP's tables folded from its preconfiguration tables: each table's
--   description and its rows, both by the table's UID ('tdTableUID'), the
--   rows in the order the tables give them, each its cells in column order.
--
--   * (a) A template row is a row for each of the instances 'instances'
--     gives the table its object lies in (its own, or for an AccessControl
--     row the table its InvokingID names), its cells instantiated; an
--     instance whose UID does not fit the placeholder's bytes, or whose
--     object a row the table prints already is, is left out.
--   * (b) A row keeps the cells it prints ('Given'), takes the TPer's value
--     where it leaves one to the TPer ('VendorUnique', 'vendorCell'; one
--     'vendorCell' does not name is an error naming the table, the row and
--     the column), and every column it leaves blank takes its default
--     ('ptDefaults', Core 5.3.4.2.2, PDF 223: a column a Get does not return
--     reads as one the host may not read), but the UID: a UID names a row's
--     object, which no default can, and an AccessControl row's is the TPer's
--     to assign, which Opal prints none of (Core 5.3.2.7.1, PDF 162).
--   * (c) The parameters override two cells: the Locking SP's LifeCycleState
--     is its Original Factory State ('spLockingOFS'; Opal 2.30 Table 29
--     prints Manufactured-Inactive, the OFS 5.2.2 makes normative, PDF 96,
--     whose informative comment allows Manufactured), and TPerInfo's
--     ProgrammaticResetEnable is 'spTPerResetEnabled' (Table 27 prints FALSE,
--     PDF 50), in the table's one row (Core 5.4.2.1, PDF 229), whatever its
--     UID. A cell that holds the parameter's value already is kept as
--     printed.
--   * (d) An every-object row ('EveryObject') is one row for each object
--     whose UID begins with its half-UID, in UID order, among the rows (a) to
--     (c) give, its InvokingID the object (Opal 2.30 *AC1-*AC7, PDF 38-39),
--     each then filled as (b) fills a row.
--   * (e) A row the fold adds (an instance, an object's row) is left out
--     where a row the table prints, or one the fold added before it, is of
--     the same object: by its UID, or, for a row with none (an AccessControl
--     row), by its cells in the table's unique columns (InvokingID and
--     MethodID, Core Table 175, PDF 162). So the fold adds no object a table
--     holds, and no InvokingID and MethodID pair twice.
--
--   Every cell is evaluated as soon as the tables are, so that a cell the
--   TPer would choose and 'vendorCell' does not name fails the fold, not a
--   later Get of it.
foldTables :: SimParams -> [PreconfTable] -> (Map UID TableDesc, Map UID [Row])
foldTables p tables =
  ( Map.fromList [ (tdTableUID (ptDesc t), ptDesc t) | t <- tables ]
  , everyCell (Map.fromListWith (flip (<>)) [ (tdTableUID (ptDesc t), finish t staged) | (t, staged) <- stagedTables ])
  )
  where
    -- (a) to (c): each table's rows as printed and as its templates' instances, complete; its every-object rows kept
    stagedTables = [ (t, concatMap (stage p tables t . row) (ptRows t)) | t <- tables ]
    -- the objects (a) to (c) give, by UID, in order
    objects = Set.toAscList (Set.fromList [ u | (_, staged) <- stagedTables, Just r <- map builtRow staged
                                              , Just (UIDCell u) <- [L.lookup uidColumn r] ])
    -- (d) and (e): a table's rows, each every-object row expanded, no object added twice
    finish t staged = distinctObjects (ptDesc t) (concatMap (expanded t) staged)
    expanded t = \case
      Printed r         -> [(True, r)]
      Instance r        -> [(False, r)]
      Every half cells  -> [ (False, built p t (everyObject t u cells)) | u <- objects, uidUpper u == half ]
    -- the tables, each of their cells evaluated first
    everyCell rows = L.foldl' (flip seq) () [ cell | rs <- Map.elems rows, r <- rs, (_, cell) <- r ] `seq` rows

-- | The cell the TPer chooses where a preconfiguration table leaves a value
--   to it ('VendorUnique': @VU@, Opal 2.30 Table 1, PDF 12, or @MSID@, Table
--   25, PDF 48), from the run's parameters: in a table of a description, in
--   a row (the cells it prints, which name its object), in a column, for the
--   marker printed; 'Nothing' for a cell no entry names. The entries, each
--   with its source (design 13.17; the values the lead decided for M4b,
--   design 13.32):
--
--   * C_PIN_SID's PIN (@VU@): the SID's PIN the parameters give, else the
--     MSID, as automated take ownership needs it (Opal 2.30 4.2.1.8, PDF 49);
--   * C_PIN_MSID's PIN (@MSID@): the MSID (Table 25, PDF 48);
--   * C_PIN_SID's TryLimit (@VU@): the parameters' (Table 25, PDF 48; 0 is no
--     limit, Core 5.3.4.1.1.2, PDF 210);
--   * a C_PIN object's Tries (@VU@): 0, what a new C_PIN object's Tries is
--     (Core 5.3.4.1.1.2, PDF 210);
--   * TPerInfo's GUDID (@VU@): the parameters', in Core Table 212's format
--     (Opal 2.30 Table 27 and *TP1, PDF 50; Core PDF 230);
--   * the Base template's Instances and MaxInstances (@VU@): 2, the SPs that
--     incorporate it, the Admin SP and the Locking SP, a template of an SP in
--     Manufactured-Inactive counting (Opal 2.30 Table 28, PDF 50; 5.2.2.1,
--     PDF 96);
--   * DataRemovalMechanism's ActiveDataRemovalMechanism (@VU@): 2,
--     Cryptographic Erase (Table 31, PDF 51), the mechanism an Opal TPer
--     SHALL support and the one the simulator's Level 0 Supported Data
--     Removal Mechanism descriptor reports ("Hsed.Sim.Level0"; Opal 2.30
--     Table 10, PDF 27; Table 33 and 4.2.6.1.2, PDF 51-52). The table's UID
--     is Opal's (Table 20, PDF 37), which no generated Core module holds, so
--     the table is the one its description names DataRemovalMechanism.
--
-- >>> import Hsed.Gen.Core.TableDescs (c_PINTableDesc)
-- >>> import Hsed.Sim.Params (defaultSimParams)
-- >>> vendorCell defaultSimParams c_PINTableDesc [(0, UIDCell uC_PIN_SID)] 6 "VU"
-- Just (UIntCell 0)
-- >>> vendorCell defaultSimParams c_PINTableDesc [(0, UIDCell uC_PIN_SID)] 7 "VU"
-- Nothing
vendorCell :: SimParams -> TableDesc -> Row -> ColNum -> Text -> Maybe Cell
vendorCell p desc r c marker = do
  column <- colName <$> columnNumbered c desc
  listToMaybe [ vValue v p | v <- vendorCells, vTable v desc, vRow v r, vColumn v == column, vMarker v == marker ]

-- | The instances a template row of a table takes beside the rows its
--   table prints, by number, for the placeholder it prints: the Admin SP's
--   Authority and C_PIN tables under @XX@ (Opal 2.30 Tables 24-25, PDF
--   48-49: @00 00 00 09 00 00 02 00 (+XX)@, @AdminXX@) the Admin authorities
--   after Admin1 and their credentials, 2 to 'spAdminSPAdmins' (PDF 47:
--   Admin1 (M), any additional Admin authority (O)); none for any other, until
--   a parameter numbers it (the Locking SP's @XXXX@ and @MMMM@ are M5's).
--
-- >>> import Hsed.Gen.Core.TableDescs (authorityTableDesc)
-- >>> import Hsed.Sim.Params (defaultSimParams)
-- >>> map (\n -> instances defaultSimParams { spAdminSPAdmins = n } authorityTableDesc (Placeholder "XX" 1)) [1, 3]
-- [[],[2,3]]
instances :: SimParams -> TableDesc -> Placeholder -> [Natural]
instances p desc ph
  | ph == adminXX && tdTableUID desc `elem` [uAuthorityTable, uC_PINTable] = [2 .. spAdminSPAdmins p]
  | otherwise                                                               = []
  where
    -- Table 24's and Table 25's placeholder: one byte, printed XX (PDF 48-49)
    adminXX = Placeholder "XX" 1


-- * The passes

-- | A row on its way through the fold: one the table prints, a template's
--   instance (both complete), or a row for every object whose UID begins
--   with the half, its cells as printed.
data Staged
  = Printed !Row
  | Instance !Row
  | Every !HalfUID ![(ColNum, PreCell)]

-- | A row of an object, as (a) to (c) build it.
builtRow :: Staged -> Maybe Row
builtRow = \case
  Printed r  -> Just r
  Instance r -> Just r
  Every _ _  -> Nothing

-- | (a): a preconfiguration row of a table as the fold stages it: a plain
--   row complete, a template's instances complete, an every-object row as
--   printed. A template's instances are those of the table its object (its
--   UID, or its InvokingID) lies in, among the tables folded, each one whose
--   object the placeholder's bytes can number.
stage :: SimParams -> [PreconfTable] -> PreconfTable -> PreconfRow -> [Staged]
stage p tables t pr = case prKind pr of
  Plain            -> [Printed (built p t (prCells pr))]
  EveryObject half -> [Every half (prCells pr)]
  Template ph cols -> [ Instance (built p t (instantiate ph cols n (prCells pr))) | n <- numbered ph, fits ph n ]
  where
    base         = objectOf (ptDesc t) [ (c, cell) | (c, Given cell) <- prCells pr ]
    numbered ph  = maybe [] (\d -> instances p d ph) (base >>= tableOf . uidUpper)
    fits ph n    = maybe False (\u -> isJust (instanceUID ph u n)) base
    tableOf half = L.find ((== half) . uidUpper . tdTableUID) (map ptDesc tables)

-- | (b) and (c): a row's cells complete, then the parameters' overrides.
built :: SimParams -> PreconfTable -> [(ColNum, PreCell)] -> Row
built p t = override p (ptDesc t) . complete p t

-- | (b): a row's cells, in column order: those it prints, the TPer's where
--   it leaves the value to the TPer ('vendorCell'), and every other column's
--   default but the UID's.
complete :: SimParams -> PreconfTable -> [(ColNum, PreCell)] -> Row
complete p t cells = L.sortOn fst (given <> chosen <> defaults)
  where
    desc     = ptDesc t
    given    = [ (c, cell) | (c, Given cell) <- cells ]
    chosen   = [ (c, fromMaybe (unnamed c marker) (vendorCell p desc given c marker)) | (c, VendorUnique marker) <- cells ]
    defaults = [ d | d@(c, _) <- ptDefaults t, c /= uidColumn, c `notElem` map fst cells ]
    unnamed c marker = error . T.unpack $
      "Hsed.Sim.Preconfig: the " <> tdName desc <> " table's row " <> rowNamed desc given <> " leaves its "
        <> maybe ("column " <> tshow c) colName (columnNumbered c desc) <> " to the TPer (" <> marker
        <> "), and vendorCell names no value for it"

-- | (c): the parameters' overrides of a row: the Locking SP's row of the SP
--   table, its LifeCycleState; TPerInfo's row, its ProgrammaticResetEnable;
--   each column found by name in the table's description.
override :: SimParams -> TableDesc -> Row -> Row
override p desc r
  | tdTableUID desc == uSPTable && objectOf desc r == Just uSP_Locking = put "LifeCycleState" (lifeCycleCell (spLockingOFS p))
  | tdTableUID desc == uTPerInfoTable                                  = put "ProgrammaticResetEnable" (booleanCell (spTPerResetEnabled p))
  | otherwise                                                          = r
  where
    put name new = maybe r (\c -> putCell c new r) (columnOf desc name)

-- | (d): the row an every-object row stands for, for one object: its cells
--   with the object as its InvokingID. An every-object row of a table with no
--   InvokingID column is an error naming the table.
everyObject :: PreconfTable -> UID -> [(ColNum, PreCell)] -> [(ColNum, PreCell)]
everyObject t u cells = case columnOf (ptDesc t) "InvokingID" of
  Just c  -> (c, Given (UIDCell u)) : filter ((/= c) . fst) cells
  Nothing -> error . T.unpack $
    "Hsed.Sim.Preconfig: the " <> tdName (ptDesc t) <> " table has a row for every object of a table, and no InvokingID column to name them in"

-- | (e): a table's rows, the printed ones all kept, each the fold adds kept
--   where its object is none of those before it ('identity').
distinctObjects :: TableDesc -> [(Bool, Row)] -> [Row]
distinctObjects desc rows = catMaybes (snd (L.mapAccumL keep printed rows))
  where
    printed = [ k | (True, r) <- rows, Just k <- [identity desc r] ]
    keep seen (isPrinted, r) = case identity desc r of
      Just k | not isPrinted -> if k `elem` seen then (seen, Nothing) else (k : seen, Just r)
      _                      -> (seen, Just r)


-- * Rows and cells

-- | The UID column, column 0x00 of every object table's description: an
--   object table always has one, by which its rows are addressed (Core
--   3.2.5.1 and 3.2.5.3, PDF 51).
uidColumn :: ColNum
uidColumn = 0

-- | The object a row is of: its UID, or, for a row with none (an
--   AccessControl row, whose UID the TPer assigns, Core 5.3.2.7.1, PDF 162),
--   the object its InvokingID names.
objectOf :: TableDesc -> Row -> Maybe UID
objectOf desc r = (L.lookup uidColumn r >>= uidIn) <|> (columnOf desc "InvokingID" >>= (`L.lookup` r) >>= uidIn)

-- | What a row of a table is: its UID, or, for a row with none, its cells in
--   the table's unique columns (an AccessControl row's InvokingID and
--   MethodID, Core Table 175, PDF 162), a reference by the UID it holds;
--   'Nothing' where neither is there.
identity :: TableDesc -> Row -> Maybe [Either Cell UID]
identity desc r = case L.lookup uidColumn r of
  Just cell -> Just [key cell]
  Nothing   -> case [ key cell | col <- tdColumns desc, colUnique col, Just cell <- [L.lookup (colNumber col) r] ] of
    [] -> Nothing
    ks -> Just ks
  where
    key cell = maybe (Left cell) Right (uidIn cell)

-- | The UID a cell holds or references.
uidIn :: Cell -> Maybe UID
uidIn = \case
  UIDCell u   -> Just u
  RefCell _ u -> Just u
  _           -> Nothing

-- | The number of a column of a name in a description.
columnOf :: TableDesc -> Text -> Maybe ColNum
columnOf desc name = colNumber <$> columnNamed name desc

-- | A row with a cell put in a column, in column order; a cell there that
--   holds the same value already (an enumeration's, whatever its label) is
--   kept.
putCell :: ColNum -> Cell -> Row -> Row
putCell c new r
  | holds (L.lookup c r) = r
  | otherwise            = L.sortOn fst ((c, new) : filter ((/= c) . fst) r)
  where
    holds = \case
      Just (EnumCell _ v) | EnumCell _ v' <- new -> v == v'
      old                                        -> old == Just new

-- | How an error names a row: its Name, else its object's UID, else its
--   cells.
rowNamed :: TableDesc -> Row -> Text
rowNamed desc r = fromMaybe ("with the cells " <> tshow r) (named <|> (tshow <$> objectOf desc r))
  where
    named = columnOf desc "Name" >>= (`L.lookup` r) >>= \case
      TextCell name -> Just name
      _             -> Nothing

-- | An SP's LifeCycleState cell, labelled as Opal 2.30 Table 29 prints it
--   (PDF 50), its value Table 49's ('lifeCycleValue', PDF 98).
lifeCycleCell :: LifeCycle -> Cell
lifeCycleCell lc = EnumCell label (lifeCycleValue lc)
  where
    label = case lc of
      ManufacturedInactive -> "Manufactured-Inactive"
      Manufactured         -> "Manufactured"

-- | A boolean cell, labelled and valued as Core Table 59 has it (PDF 103).
booleanCell :: Bool -> Cell
booleanCell b = if b then EnumCell "True" (enumValue Core_boolean_True) else EnumCell "False" (enumValue Core_boolean_False)


-- * The cells the TPer chooses

-- | One cell the TPer chooses: its table, its row, its column by name, the
--   marker the table prints there, and its value for a run.
data Vendor = Vendor
  { vTable  :: TableDesc -> Bool
  , vRow    :: Row -> Bool
  , vColumn :: Text
  , vMarker :: Text
  , vValue  :: SimParams -> Cell
  }

-- | The cells the TPer chooses in the Admin SP's preconfiguration
--   ('vendorCell', which cites each).
vendorCells :: [Vendor]
vendorCells =
  [ Vendor (isTable uC_PINTable) (isObject uC_PIN_SID) "PIN" "VU" (\p -> BytesCell (fromMaybe (spMsid p) (spSidPin p)))
  , Vendor (isTable uC_PINTable) anyRow "PIN" "MSID" (BytesCell . spMsid)
  , Vendor (isTable uC_PINTable) (isObject uC_PIN_SID) "TryLimit" "VU" (UIntCell . toInteger . spSidTryLimit)
  , Vendor (isTable uC_PINTable) anyRow "Tries" "VU" (const (UIntCell 0))
  , Vendor (isTable uTPerInfoTable) anyRow "GUDID" "VU" (BytesCell . spGudid)
  , Vendor (isTable uTemplateTable) (isObject uTemplate_Base) "Instances" "VU" (const (UIntCell 2))
  , Vendor (isTable uTemplateTable) (isObject uTemplate_Base) "MaxInstances" "VU" (const (UIntCell 2))
  , Vendor ((== "DataRemovalMechanism") . tdName) anyRow "ActiveDataRemovalMechanism" "VU" (const (EnumCell "Cryptographic Erase" 2))
  ]
  where
    isTable u desc = tdTableUID desc == u
    isObject u r   = L.lookup uidColumn r == Just (UIDCell u)
    anyRow         = const True
