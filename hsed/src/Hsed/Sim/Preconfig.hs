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
sections 8.1 and 8.3; M4b). 'foldTables' does it in six passes:

* (a) templates: a template row (@(+XX)@) is one row for each instance the
  TPer holds beyond the rows its table prints ('instances'), its UID, names
  and references those of the instance ("Hsed.Core.Preconfig".'instantiate');
* (b) cells: a row keeps the cells its table prints, takes the TPer's
  choice where the table leaves the value to it ('vendorCell': @VU@, @MSID@),
  and every other column's default but the UID's, so that every column a Get
  may return holds a value (Core 5.3.4.2.2, PDF 223; design 13.31 (a));
* (c) overrides: the parameters choose cells the tables print, the Locking
  SP's life cycle state and TPerInfo's ProgrammaticResetEnable in the Admin
  SP's, and since M5a LockingInfo's MaxRanges and geometry in the Locking
  SP's;
* (d) every object: a row printed @TT TT TT TT@ after a half-UID is one
  AccessControl row for each object of that table, over the rows (a) to (c)
  give, ordered by UID (Opal 2.30 *AC1-*AC7, PDF 38-39);
* (e) the result: the descriptions and the rows by the table's UID, the rows
  in the tables' order, the fold adding no object a table holds already,
  and every cell evaluated as soon as the tables are;
* (f) the Table table's counts: each of its rows' NumColumns and Rows, which
  the preconfiguration leaves blank and Core states (5.3.2.3.7-5.3.2.3.8, PDF
  157): the number of columns in the table, the number of rows created), the
  described table's columns and the rows the fold gives it, a byte table's
  NumColumns 0x01 (since the M4b review; the CommonNames Core states, 5.3.2.3
  and 5.3.2.6.3, PDF 156 and 161, the issuing template's name, stay their
  defaults, design 13.31 (a) and 13.32 (22)).

The fold reads a run's parameters and nothing of the state, so that the
initial state can run it ("Hsed.Sim.Types".@initialState@), and a Revert can
run it again (M6, but C_PIN_SID's PIN on an Admin SP Revert, Opal 5.1.2.2.1,
PDF 93); it imports the parameters ("Hsed.Sim.Params"), hsed-core and the
generated modules, the Admin SP's preconfiguration among them
("Hsed.Gen.Opal.Preconfig.AdminSP": Opal 2.30's Admin SP tables as hsedc
generates them, since M4b, which "Hsed.Sim.Types".@initialState@ folds),
never a module that imports the state's types.

Since M5a the fold takes an SP's preconfiguration whole ('PreconfSP': the
SP's UID and its tables) and gives the SP's tables or every problem that
keeps them from folding ('FoldProblem'), and it folds the Locking SP's
(Opal 2.30 Tables 34-42 and 44-48, PDF 52-89, "Hsed.Gen.Opal.Preconfig.LockingSP",
generated since M5a's integration) as it does the Admin SP's: the cells
those tables print beside the Admin SP's ("Hsed.Core.Preconfig": a value
marked with a note's number, one of two key references, a value the TPer
chooses with a least) read and the TPer's choices answered ('vendorCell',
by SP), its templates numbered by the parameters ('instances', by SP,
placeholder and table), the key table the TPer does not hold left out with
every row that names it (Table 36's *TT1, PDF 54), and its LockingInfo row
the parameters' ranges and geometry.
-}
module Hsed.Sim.Preconfig
  ( -- * An SP's preconfiguration, folded
    PreconfSP (..)
  , FoldProblem (..)
  , foldProblemText
  , foldTables
    -- * The TPer's choices
  , vendorCell
  , instances
  ) where

import qualified RIO.ByteString                  as B
import qualified RIO.List                        as L
import qualified RIO.Map                         as Map
import           RIO
import qualified RIO.Set                         as Set
import qualified RIO.Text                        as T

import           Hsed.Core.Preconfig             (Placeholder (..), PreCell (..), PreconfRow (..), PreconfTable (..),
                                                  RowKind (..), instanceUID, instantiate, unmarked)
import           Hsed.Core.Table                 (Cell (..), Col (..), ColNum, Pre (..), Row, TableDesc (..), columnNumbered, columnOf)
import           Hsed.Core.Types                 (enumValue)
import           Hsed.Core.UID                   (HalfUID, UID, uNull, uidLower, uidUpper)
import           Hsed.Gen.Core.Enums             (Core_boolean (..), Core_symmetric_mode (..), Core_table_kind (..))
import           Hsed.Gen.Core.TableDescs        (k_AES_128TableDesc, k_AES_256TableDesc)
import           Hsed.Gen.Core.TableUIDs         (uACETable, uAuthorityTable, uC_PINTable, uK_AES_128Table, uK_AES_256Table,
                                                  uLockingInfoTable, uLockingTable, uMBRTableObject, uSPTable, uSecretProtectTable,
                                                  uTPerInfoTable, uTableTable, uTemplateTable)
import           Hsed.Gen.Core.UIDs              (uC_PIN_SID, uSP_Admin, uTemplate_Base)
import           Hsed.Gen.Opal.Enums             (Opal_data_removal_mechanism (..))
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Gen.Opal.TableUIDs         (uDataRemovalMechanismTable)
import           Hsed.Sim.Params                 (Geometry (..), LifeCycle (..), LockingKeys (..), SimParams (..), lifeCycleValue,
                                                  lockingBounds)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hsed.Sim.Params (defaultSimParams)


-- | An SP's tables folded from its preconfiguration tables: each table's
--   description and its rows, both by the table's UID ('tdTableUID'), the
--   rows in the order the tables give them, each its cells in column order.
--
--   * (a) A template row is a row for each of the instances 'instances'
--     gives the SP, its placeholder and the table its object lies in (its
--     own, or for an AccessControl row the table its InvokingID names), its
--     cells instantiated; an instance whose UID does not fit the
--     placeholder's bytes, or whose object a row the table prints already
--     is, is left out.
--   * (b) A row keeps the cells it prints ('Given'), takes the TPer's value
--     where it leaves one to the TPer ('VendorUnique', 'VendorAtLeast',
--     'vendorCell'), and every column it leaves blank takes its default
--     ('ptDefaults', Core 5.3.4.2.2, PDF 223: a column a Get does not return
--     reads as one the host may not read), but the UID: a UID names a row's
--     object, which no default can, and an AccessControl row's is the TPer's
--     to assign, which Opal prints none of (Core 5.3.2.7.1, PDF 162). A
--     value with a note's number after it is the value ('unmarked'), and of
--     two key references the one to the key table the TPer holds
--     ('spLockingKeys': K_AES_128's with 'KeysAES128', else K_AES_256's;
--     Opal 2.30 Table 45's *LT1, PDF 86).
--   * (c) The parameters override cells the tables print: the Locking SP's
--     LifeCycleState is its Original Factory State ('spLockingOFS'; Opal
--     2.30 Table 29 prints Manufactured-Inactive, the OFS 5.2.2 makes
--     normative, PDF 96, whose informative comment allows Manufactured);
--     TPerInfo's ProgrammaticResetEnable is 'spTPerResetEnabled' (Table 27
--     prints FALSE, PDF 50), in the table's one row (Core 5.4.2.1, PDF 229),
--     whatever its UID; and LockingInfo's MaxRanges is 'spLockingRanges'
--     (Table 44 prints 8 and its note 1, at least 8, PDF 86), its
--     AlignmentRequired, LogicalBlockSize, AlignmentGranularity and
--     LowestAlignedLBA the geometry's ('spGeometry'; Table 44 leaves them
--     blank, 4.3.5.1, PDF 85, and the Geometry Reporting descriptor reports
--     the same columns, 3.1.1.4.2-5, PDF 22). A cell that holds the
--     parameter's value already is kept as printed.
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
--   * (f) The Table table's rows each count the table they describe (the
--     table whose half-UID is the row's UID's lower half, Core 3.2.5.3 b.i,
--     PDF 51-52): its NumColumns the columns of the table's description and
--     its Rows the rows (a) to (e) give it (Core 5.3.2.3.7-5.3.2.3.8, PDF
--     157), where the row leaves them their default; a byte table's
--     NumColumns 0x01, whatever the fold holds of it (its Kind @Byte@, Core
--     Table 138, PDF 122; 5.3.2.3.7: \"For byte tables this SHALL be
--     0x01\", M5's MBR and DataStore, Opal Table 36, PDF 54, whose Rows its
--     row prints); an object table the fold does not hold keeps 0. No row is
--     created or deleted before M6, so the counts the fold gives hold for a
--     run.
--
--   Before the passes, the key table the TPer does not hold ('spLockingKeys';
--   Opal 2.30 Table 36's *TT1, PDF 54: only one of the two K_AES tables is
--   required; 4.3.5.5, PDF 89) is left out, and with it every row that
--   names it: its row of the Table table and SecretProtect's (Tables 36 and
--   42, PDF 54 and 84), the ACEs named for it (Table 39, PDF 79-80) and the
--   AccessControl rows of its objects (Table 38's *AC6, PDF 55: only the
--   K_AES_128 or the K_AES_256 related rows are mandatory). A cell that
--   names it is a reference to its objects or its Table row, or a name that
--   holds the table's name; a choice between the two tables ('OneOf') names
--   neither.
--
--   Every cell is evaluated as soon as the tables are. The result is the
--   SP's tables, or every problem that keeps the fold from giving them
--   ('FoldProblem'): for the Locking SP, a parameter out of the bounds Opal
--   2.30 and the simulator give it ("Hsed.Sim.Params".'lockingBounds'), which
--   the fold reports before it starts; a cell left to the TPer that
--   'vendorCell' does not answer, or a choice of key no alternative of which
--   is the key table the TPer holds; a value chosen below the least the
--   table prints ('VendorAtLeast', as a Table row's Rows, a uinteger_4,
--   Core Table 171, PDF 156); an every-object row whose half-UID is no table
--   the SP holds; and a template whose base is no table's the SP holds.
--
-- >>> import Hsed.Gen.Core.UIDs (uSP_Admin)
-- >>> either (map foldProblemText) (const []) (foldTables defaultSimParams { spLockingUsers = 7 } (PreconfSP uSP_Locking []))
-- ["the Locking SP's Users 7: out of 8 to 255"]
-- >>> either (map foldProblemText) (const []) (foldTables defaultSimParams { spLockingUsers = 7 } (PreconfSP uSP_Admin []))
-- []
foldTables :: SimParams -> PreconfSP -> Either [FoldProblem] (Map UID TableDesc, Map UID [Row])
foldTables p sp = case bounds of
  [] -> folded p (pspUID sp) (heldKeys p (pspTables sp))
  _  -> Left bounds
  where
    bounds = [ OutOfBounds what v range | pspUID sp == uSP_Locking, (what, v, range@(least, most), _) <- lockingBounds p
                                        , v < least || v > most ]

-- | An SP's preconfiguration: its UID (its row of the Admin SP's SP table,
--   Opal 2.30 Table 29, PDF 50) and its preconfiguration tables, as hsedc
--   generates them (the Admin SP's "Hsed.Gen.Opal.Preconfig.AdminSP".@adminSP@,
--   the Locking SP's "Hsed.Gen.Opal.Preconfig.LockingSP".@lockingSP@).
data PreconfSP = PreconfSP
  { pspUID    :: !UID
  , pspTables :: ![PreconfTable]
  }
  deriving (Eq, Show)

-- | What keeps an SP's preconfiguration from folding, which the fold
--   reports before the transport opens ("Hsed.Transport.InProc"): in an SP,
--   a cell the TPer chooses that no 'vendorCell' entry answers (its table,
--   row and column); in an SP, a table whose every object a row stands for
--   where the SP holds no such table (the table's half-UID); a value out of
--   the bounds the spec gives it (what, the value, the least and the most);
--   in an SP, a template's base UID that is no object of a table the SP
--   holds, so that no instance of it resolves.
data FoldProblem
  = UnansweredVU !UID !Text !Text !Text
  | UnknownEveryObject !UID !Text
  | OutOfBounds !Text !Natural !(Natural, Natural)
  | UnresolvedBase !UID !UID
  deriving (Eq, Show)

-- | A fold problem as a report or an error says it.
--
-- >>> import Hsed.Core.UID (uid)
-- >>> foldProblemText (OutOfBounds "the Locking SP's Users" 7 (8, 255))
-- "the Locking SP's Users 7: out of 8 to 255"
-- >>> foldProblemText (UnansweredVU (uid 0 0 2 5 0 0 0 2) "C_PIN" "C_PIN_Admin1" "PIN")
-- "SP uid 0x00 0x00 0x02 0x05 0x00 0x00 0x00 0x02: the C_PIN table's row C_PIN_Admin1 leaves its PIN to the TPer, and no value is named for it"
foldProblemText :: FoldProblem -> Text
foldProblemText = \case
  UnansweredVU sp table named column ->
    inSP sp <> "the " <> table <> " table's row " <> named <> " leaves its " <> column <> " to the TPer, and no value is named for it"
  UnknownEveryObject sp table -> inSP sp <> "a row for every object of the " <> table <> " table, which the SP does not hold"
  OutOfBounds what v (least, most) -> what <> " " <> tshow v <> ": out of " <> tshow least <> " to " <> tshow most
  UnresolvedBase sp base -> inSP sp <> "no instance resolves the template base " <> tshow base
  where inSP sp = "SP " <> tshow sp <> ": "

-- | 'foldTables' of an SP's tables, its six passes, every problem of every
--   pass reported.
folded :: SimParams -> UID -> [PreconfTable] -> Either [FoldProblem] (Map UID TableDesc, Map UID [Row])
folded p spu tables = do
  stagedTables <- allOf [ (,) t . concat <$> allOf (map (stage p spu tables t . row) (ptRows t)) | t <- tables ]
  let -- the objects (a) to (c) give, by UID, in order
      objects = Set.toAscList (Set.fromList [ u | (_, staged) <- stagedTables, Just r <- map builtRow staged
                                                , Just (UIDCell u) <- [L.lookup uidColumn r] ])
      -- (d): a staged row, each every-object row expanded
      expanded t = \case
        Printed r        -> Right [(True, r)]
        Instance r       -> Right [(False, r)]
        Every half cells -> map ((,) False) <$> allOf [ built p spu t (everyObject t u cells) | u <- objects, uidUpper u == half ]
  finished <- allOf [ (,) (tdTableUID (ptDesc t)) . distinctObjects (ptDesc t) . concat <$> allOf (map (expanded t) staged)
                    | (t, staged) <- stagedTables ]
  -- (e) and (f): the tables by UID, the Table table counted, each cell evaluated
  pure (descs, everyCell (counted descs (Map.fromListWith (flip (<>)) finished)))
  where
    descs = Map.fromList [ (tdTableUID (ptDesc t), ptDesc t) | t <- tables ]
    everyCell rows = L.foldl' (flip seq) () [ cell | rs <- Map.elems rows, r <- rs, (_, cell) <- r ] `seq` rows

-- | Each of a list's results, or every problem any of them has, in order.
allOf :: [Either [FoldProblem] a] -> Either [FoldProblem] [a]
allOf xs = case partitionEithers xs of
  ([], as) -> Right as
  (es, _)  -> Left (concat es)

-- | The cell the TPer chooses where a preconfiguration table leaves a value
--   to it ('VendorUnique': @VU@, Opal 2.30 Table 1, PDF 12, or @MSID@, Table
--   25, PDF 48; 'VendorAtLeast': a value with its least, Table 36, PDF 54),
--   from the run's parameters: in an SP, in a table of a description, in a
--   row (the cells it prints, which name its object), in a column, for the
--   marker printed; 'Nothing' for a cell no entry names. The Admin SP's
--   entries, each with its source (design 13.17; the values the lead
--   decided for M4b, design 13.32):
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
--     Cryptographic Erase (Table 31, PDF 51, the generated
--     @Opal_data_removal_mechanism@), the mechanism an Opal TPer SHALL
--     support and the one the simulator's Level 0 Supported Data Removal
--     Mechanism descriptor reports ("Hsed.Sim.Level0"; Opal 2.30 Table 10,
--     PDF 27; Table 33 and 4.2.6.1.2, PDF 51-52). The table's UID is Opal's
--     (Table 20 row 12, PDF 37), generated in "Hsed.Gen.Opal.TableUIDs".
--
--   The Locking SP's (since M5a; S2's values, kept by G3):
--
--   * the MBR table's Rows (@0x08000000 min@): the parameters' MBR size
--     (Opal 2.30 Table 36, PDF 54; 4.3.5.4, PDF 89), which their bounds hold
--     to that least ("Hsed.Sim.Params".'lockingBounds');
--   * the DataStore table's Rows (@0x00A00000 min@): the parameters'
--     DataStore size, the one Level 0's Additional DataStore Table
--     descriptor reports (Table 36, PDF 54; 4.3.8.1, PDF 90), held to that
--     least likewise;
--   * the MBR's and the DataStore's MandatoryWriteGranularity and
--     RecommendedAccessGranularity (@VU@): the parameters' (Table 36, PDF
--     54; 5.3.1.1.2 and 5.3.1.2.2, PDF 99-100);
--   * SecretProtect's ProtectMechanisms (@VU@): the empty set, reporting no
--     mechanism, which Table 42's note allows (PDF 84: no specific value
--     required);
--   * a K_AES_128 or K_AES_256 Key (@VU@): zeros, of the first length its
--     type allows, 16 or 32 bytes (Core Tables 95-96, key_128 and key_256,
--     PDF 112), the simulator holding no medium to encrypt; its Mode (@VU@):
--     XTS (Core Table 134, symmetric_mode, PDF 120);
--   * C_PIN_Admin1's PIN (@SID or MSID@ and its note 1), the Locking SP's
--     own object (00 00 00 0B 00 01 00 01, Table 41, PDF 83; the Admin SP's
--     C_PIN_Admin1 is 00 00 00 0B 00 00 02 01, Table 25, PDF 48): the MSID at
--     either Original Factory State, the initial value with a Manufactured
--     OFS (Table 41 note 1, PDF 83-84), and the placeholder under
--     Manufactured-Inactive that Activate replaces with SID's PIN (5.1.1.2,
--     PDF 91; M5's D3, M6's Activate).
--
-- >>> import Hsed.Gen.Core.TableDescs (c_PINTableDesc)
-- >>> import Hsed.Gen.Core.UIDs (uSP_Admin)
-- >>> vendorCell defaultSimParams uSP_Admin c_PINTableDesc [(0, UIDCell uC_PIN_SID)] 6 "VU"
-- Just (UIntCell 0)
-- >>> vendorCell defaultSimParams uSP_Admin c_PINTableDesc [(0, UIDCell uC_PIN_SID)] 7 "VU"
-- Nothing
--
--   An entry answers in its own SP alone: the Admin SP's C_PIN_SID has no
--   row in the Locking SP's C_PIN table, and SID's PIN is none of its.
--
-- >>> import Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
-- >>> vendorCell defaultSimParams uSP_Locking c_PINTableDesc [(0, UIDCell uC_PIN_SID)] 3 "VU"
-- Nothing
vendorCell :: SimParams -> UID -> TableDesc -> Row -> ColNum -> Text -> Maybe Cell
vendorCell p spu desc r c marker = do
  column <- colName <$> columnNumbered c desc
  listToMaybe [ vValue v p | v <- vendorCells, vSP v == spu, vTable v desc, vRow v r, vColumn v == column, vMarker v == marker ]

-- | The instances a template row of an SP's table takes beside the rows its
--   table prints, by number, for the placeholder it prints:
--
--   * in the Admin SP, its Authority and C_PIN tables under @XX@ (Opal 2.30
--     Tables 24-25, PDF 48-49: @00 00 00 09 00 00 02 00 (+XX)@, @AdminXX@)
--     the Admin authorities after Admin1 and their credentials, 2 to
--     'spAdminSPAdmins' (PDF 47: Admin1 (M), any additional Admin authority
--     (O));
--   * in the Locking SP (since M5a), its Authority and C_PIN tables under
--     @XXXX@ (Tables 40-41, PDF 82-84: @(+XX XX)@, @AdminXXXX@) the Admin
--     authorities after the four printed, 5 to 'spLockingAdmins' (Table 40
--     note 1, PDF 82: Admin1 to Admin4 required, any more optional); its
--     Authority, C_PIN and ACE tables under @MMMM@ (Tables 39-41, PDF 79-84:
--     @UserMMMM@, @ACE_C_PIN_UserMMMM_Set_PIN@, @ACE_UserMMMM_Set_CommonName@)
--     the User authorities after User1, 2 to 'spLockingUsers'; its Locking,
--     K_AES_128, K_AES_256 and ACE tables under @NNNN@ (Tables 39, 45 and
--     47-48, PDF 79-89: @Locking_RangeNNNN@, @K_AES_256_RangeNNNN_Key@, the
--     range's ACEs) the ranges after Range1, 2 to 'spLockingRanges';
--
--   none for any other placeholder, table or SP. An AccessControl row's
--   instances are those of the table its InvokingID lies in.
--
-- >>> import Hsed.Gen.Core.TableDescs (authorityTableDesc, lockingTableDesc)
-- >>> import Hsed.Gen.Core.UIDs (uSP_Admin)
-- >>> import Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
-- >>> map (\n -> instances defaultSimParams { spAdminSPAdmins = n } uSP_Admin authorityTableDesc (Placeholder "XX" 1)) [1, 3]
-- [[],[2,3]]
-- >>> instances defaultSimParams { spLockingAdmins = 6 } uSP_Locking authorityTableDesc (Placeholder "XXXX" 2)
-- [5,6]
-- >>> instances defaultSimParams uSP_Locking lockingTableDesc (Placeholder "NNNN" 2)
-- [2,3,4,5,6,7,8]
-- >>> instances defaultSimParams uSP_Admin lockingTableDesc (Placeholder "NNNN" 2)
-- []
instances :: SimParams -> UID -> TableDesc -> Placeholder -> [Natural]
instances p spu desc ph = fromMaybe [] (L.lookup (spu, ph, tdTableUID desc) numbered)
  where
    numbered =
      [ ((uSP_Admin, Placeholder "XX" 1, t), [2 .. spAdminSPAdmins p]) | t <- [uAuthorityTable, uC_PINTable] ]
        <> [ ((uSP_Locking, Placeholder "XXXX" 2, t), [5 .. spLockingAdmins p]) | t <- [uAuthorityTable, uC_PINTable] ]
        <> [ ((uSP_Locking, Placeholder "MMMM" 2, t), [2 .. spLockingUsers p]) | t <- [uAuthorityTable, uC_PINTable, uACETable] ]
        <> [ ((uSP_Locking, Placeholder "NNNN" 2, t), [2 .. spLockingRanges p])
           | t <- [uLockingTable, uK_AES_128Table, uK_AES_256Table, uACETable] ]


-- * The key tables

-- | The tables without the key table the TPer does not hold, and without
--   every row that names it ('foldTables'; Opal 2.30 Table 36's *TT1, PDF
--   54; Table 38's *AC6, PDF 55): K_AES_256 with 'KeysAES128', K_AES_128
--   with 'KeysAES256', neither with 'KeysBoth'. A row names the table where
--   a cell it prints references the table's objects or its Table row, or
--   holds the table's name; and a row whose object (its UID, or its
--   InvokingID) is that of a row left out is left out too: the
--   AccessControl rows of the ACEs named for the table (Table 38's ACE rows
--   marked *AC6 (PDF 55), which print the ACE by its UID alone).
heldKeys :: SimParams -> [PreconfTable] -> [PreconfTable]
heldKeys p tables = [ t { ptRows = filter (not . leftOut (ptDesc t) . row) (ptRows t) }
                    | t <- tables, tdTableUID (ptDesc t) `notElem` map tdTableUID unheld ]
  where
    unheld = case spLockingKeys p of
      KeysAES128 -> [k_AES_256TableDesc]
      KeysAES256 -> [k_AES_128TableDesc]
      KeysBoth   -> []
    leftOut desc r = named r || maybe False (`Set.member` namedObjects) (objectOf desc (printedOf r))
    namedObjects   = Set.fromList [ u | t <- tables, Pre r _ <- ptRows t, named r, Just u <- [objectOf (ptDesc t) (printedOf r)] ]
    printedOf r    = [ (c, cell) | (c, Given cell) <- map (second unmarked) (prCells r) ]
    named r        = any (namesTable . snd) (printedOf r)
    namesTable = \case
      UIDCell u   -> unheldObject u
      RefCell n u -> unheldObject u || unheldName n
      TextCell n  -> unheldName n
      ListCell cs -> any namesTable cs
      _           -> False
    unheldObject u = any (\d -> uidUpper u == uidUpper (tdTableUID d) || u == tdObjectUID d) unheld
    unheldName n   = any ((`T.isInfixOf` n) . tdName) unheld


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
--   object the placeholder's bytes can number; a template whose object lies
--   in no table folded, and an every-object row whose half is no table's,
--   are problems.
stage :: SimParams -> UID -> [PreconfTable] -> PreconfTable -> PreconfRow -> Either [FoldProblem] [Staged]
stage p spu tables t pr = case prKind pr of
  Plain            -> pure . Printed <$> built p spu t (prCells pr)
  EveryObject half
    | isJust (tableOf half) -> Right [Every half (prCells pr)]
    | otherwise             -> Left [UnknownEveryObject spu (tshow half)]
  Template ph cols -> case base >>= \b -> (,) b <$> tableOf (uidUpper b) of
    Nothing     -> Left [UnresolvedBase spu (fromMaybe uNull base)]
    Just (b, d) -> map Instance <$> allOf [ built p spu t (instantiate ph cols n (prCells pr))
                                          | n <- instances p spu d ph, isJust (instanceUID ph b n) ]
  where
    base         = objectOf (ptDesc t) [ (c, cell) | (c, Given cell) <- map (second unmarked) (prCells pr) ]
    tableOf half = L.find ((== half) . uidUpper . tdTableUID) (map ptDesc tables)

-- | (b) and (c): a row's cells complete, then the parameters' overrides.
built :: SimParams -> UID -> PreconfTable -> [(ColNum, PreCell)] -> Either [FoldProblem] Row
built p spu t = fmap (override p (ptDesc t)) . complete p spu t

-- | (b): a row's cells, in column order: those it prints, the TPer's where
--   it leaves the value to the TPer ('vendorCell', no less than the least a
--   'VendorAtLeast' cell prints), and every other column's default but the
--   UID's. A cell marked with a note is its value ('unmarked'), and of two
--   key references the one the parameters' key tables decide ('keyChosen').
--   A cell no entry answers, or a key no alternative of which is the TPer's,
--   is a problem naming the SP, the table, the row and the column.
complete :: SimParams -> UID -> PreconfTable -> [(ColNum, PreCell)] -> Either [FoldProblem] Row
complete p spu t printed = L.sortOn fst . (<> defaults) . (given <>) <$> allOf (keys <> chosen)
  where
    desc     = ptDesc t
    cells    = [ (c, unmarked pc) | (c, pc) <- printed ]
    given    = [ (c, cell) | (c, Given cell) <- cells ]
    keys     = [ maybe (unanswered c) (Right . (,) c) (keyChosen p pcs) | (c, OneOf pcs) <- cells ]
    chosen   = [ maybe (unanswered c) (atLeast c least) (vendorCell p spu desc given c marker)
               | (c, least, marker) <- [ (c, 0, m) | (c, VendorUnique m) <- cells ] <> [ (c, n, m) | (c, VendorAtLeast n m) <- cells ] ]
    defaults = [ d | d@(c, _) <- ptDefaults t, c /= uidColumn, c `notElem` map fst cells ]
    unanswered c = Left [UnansweredVU spu (tdName desc) (rowNamed desc given) (columnName c)]
    atLeast c least = \case
      UIntCell v | v < toInteger least -> Left [OutOfBounds (inRow c) (fromInteger (max 0 v)) (least, uinteger4Most)]
      cell                             -> Right (c, cell)
    inRow c = "the " <> tdName desc <> " table's row " <> rowNamed desc given <> "'s " <> columnName c
    columnName c = maybe ("column " <> tshow c) colName (columnNumbered c desc)
    -- a Table row's Rows, a uinteger_4 (Core Table 171, PDF 156): the cells Opal prints a least for
    uinteger4Most = 0xFFFFFFFF

-- | Of a cell's alternatives ('OneOf'), the reference to the key table the
--   TPer holds: K_AES_128's with 'KeysAES128', K_AES_256's with
--   'KeysAES256' and 'KeysBoth' (Opal 2.30 Table 36's *TT1, PDF 54; Table
--   45's *LT1, PDF 86; with both, M5a's decision); 'Nothing' where none of
--   them references that table.
keyChosen :: SimParams -> [PreCell] -> Maybe Cell
keyChosen p pcs = listToMaybe [ cell | Given cell <- map unmarked pcs, Just u <- [uidIn cell], uidUpper u == half ]
  where half = uidUpper (if spLockingKeys p == KeysAES128 then uK_AES_128Table else uK_AES_256Table)

-- | (c): the parameters' overrides of a row: the Locking SP's row of the SP
--   table, its LifeCycleState; TPerInfo's row, its ProgrammaticResetEnable;
--   LockingInfo's row, its MaxRanges and its geometry; each column found by
--   name in the table's description.
override :: SimParams -> TableDesc -> Row -> Row
override p desc r
  | tdTableUID desc == uSPTable && objectOf desc r == Just uSP_Locking = puts [("LifeCycleState", lifeCycleCell (spLockingOFS p))]
  | tdTableUID desc == uTPerInfoTable                                  = puts [("ProgrammaticResetEnable", booleanCell (spTPerResetEnabled p))]
  | tdTableUID desc == uLockingInfoTable                               = puts
      [ ("MaxRanges", UIntCell (toInteger (spLockingRanges p)))
      , ("AlignmentRequired", booleanCell (gAlign geometry))
      , ("LogicalBlockSize", UIntCell (toInteger (gLogicalBlockSize geometry)))
      , ("AlignmentGranularity", UIntCell (toInteger (gGranularity geometry)))
      , ("LowestAlignedLBA", UIntCell (toInteger (gLowestAligned geometry)))
      ]
  | otherwise                                                          = r
  where
    geometry = spGeometry p
    puts     = L.foldl' (\r' (name, new) -> maybe r' (\c -> putCell c new r') (columnOf desc name)) r

-- | (d): the row an every-object row stands for, for one object: its cells
--   with the object as its InvokingID. An every-object row of a table with no
--   InvokingID column is an error naming the table: a generated table no
--   preconfiguration prints (Opal's every-object rows are AccessControl's
--   alone, *AC1-*AC7, PDF 38-39).
everyObject :: PreconfTable -> UID -> [(ColNum, PreCell)] -> [(ColNum, PreCell)]
everyObject t u cells = case columnOf (ptDesc t) "InvokingID" of
  Just c  -> (c, Given (UIDCell u)) : filter ((/= c) . fst) cells
  Nothing -> error . T.unpack $
    "Hsed.Sim.Preconfig: the " <> tdName (ptDesc t) <> " table has a row for every object of a table, and no InvokingID column to name them in"

-- | (f): the Table table's rows, each its NumColumns and Rows counted from
--   the description and the rows of the table it describes, where it leaves
--   them their default (UIntCell 0); a byte table's NumColumns 0x01 (Core
--   5.3.2.3.7, PDF 157).
counted :: Map UID TableDesc -> Map UID [Row] -> Map UID [Row]
counted descs rows = Map.adjust (map count) uTableTable rows
  where
    tableDesc = Map.lookup uTableTable descs
    count r = fromMaybe r $ do
      d <- tableDesc
      let put name v r' = maybe r' (\c -> if L.lookup c r' == Just (UIntCell 0) then putCell c (UIntCell v) r' else r') (columnOf d name)
          byte          = case columnOf d "Kind" >>= (`L.lookup` r) of
            Just (EnumCell _ v) -> v == enumValue Core_table_kind_Byte
            _                   -> False
      if byte
        then pure (put "NumColumns" 1 r)
        else do
          u <- L.lookup uidColumn r >>= uidIn
          (table, desc) <- L.find ((== uidLower u) . uidUpper . fst) (Map.toList descs)
          pure (put "Rows" (toInteger (length (Map.findWithDefault [] table rows))) (put "NumColumns" (toInteger (length (tdColumns desc))) r))

-- | (e): a table's rows, the printed ones all kept, each the fold adds kept
--   where its object is none of those before it ('identity'), the objects
--   seen held in a set.
distinctObjects :: TableDesc -> [(Bool, Row)] -> [Row]
distinctObjects desc rows = catMaybes (snd (L.mapAccumL keep printed rows))
  where
    printed = Set.fromList [ k | (True, r) <- rows, Just k <- [identity desc r] ]
    keep seen (isPrinted, r) = case identity desc r of
      Just k | not isPrinted -> if k `Set.member` seen then (seen, Nothing) else (Set.insert k seen, Just r)
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
--   MethodID, Core Table 175, PDF 162), a reference by the UID it holds, any
--   other cell as it shows; 'Nothing' where neither is there.
identity :: TableDesc -> Row -> Maybe [Either Text UID]
identity desc r = case L.lookup uidColumn r of
  Just cell -> Just [key cell]
  Nothing   -> case [ key cell | col <- tdColumns desc, colUnique col, Just cell <- [L.lookup (colNumber col) r] ] of
    [] -> Nothing
    ks -> Just ks
  where
    key cell = maybe (Left (tshow cell)) Right (uidIn cell)

-- | The UID a cell holds or references.
uidIn :: Cell -> Maybe UID
uidIn = \case
  UIDCell u   -> Just u
  RefCell _ u -> Just u
  _           -> Nothing

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

-- | How a problem names a row: its Name, else its object's UID, else its
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

-- | One cell the TPer chooses: its SP, its table, its row, its column by
--   name, the marker the table prints there, and its value for a run.
data Vendor = Vendor
  { vSP     :: UID
  , vTable  :: TableDesc -> Bool
  , vRow    :: Row -> Bool
  , vColumn :: Text
  , vMarker :: Text
  , vValue  :: SimParams -> Cell
  }

-- | The cells the TPer chooses in the Admin SP's and the Locking SP's
--   preconfiguration ('vendorCell', which cites each).
vendorCells :: [Vendor]
vendorCells =
  [ Vendor uSP_Admin (isTable uC_PINTable) (isObject uC_PIN_SID) "PIN" "VU" (\p -> BytesCell (fromMaybe (spMsid p) (spSidPin p)))
  , Vendor uSP_Admin (isTable uC_PINTable) anyRow "PIN" "MSID" (BytesCell . spMsid)
  , Vendor uSP_Admin (isTable uC_PINTable) (isObject uC_PIN_SID) "TryLimit" "VU" (UIntCell . toInteger . spSidTryLimit)
  , Vendor uSP_Admin (isTable uC_PINTable) anyRow "Tries" "VU" (const (UIntCell 0))
  , Vendor uSP_Admin (isTable uTPerInfoTable) anyRow "GUDID" "VU" (BytesCell . spGudid)
  , Vendor uSP_Admin (isTable uTemplateTable) (isObject uTemplate_Base) "Instances" "VU" (const (UIntCell 2))
  , Vendor uSP_Admin (isTable uTemplateTable) (isObject uTemplate_Base) "MaxInstances" "VU" (const (UIntCell 2))
  , Vendor uSP_Admin (isTable uDataRemovalMechanismTable) anyRow "ActiveDataRemovalMechanism" "VU"
      (const (EnumCell "Cryptographic Erase" (enumValue Opal_data_removal_mechanism_Cryptographic_Erase)))
  , Vendor uSP_Locking (isTable uTableTable) (isObject uMBRTableObject) "Rows" "0x08000000 min" (UIntCell . toInteger . spMbrSize)
  , Vendor uSP_Locking (isTable uTableTable) (isObject LockingSP.uTable_DataStore) "Rows" "0x00A00000 min"
      (\p -> let (_, size, _) = spDataStore p in UIntCell (toInteger size))
  , Vendor uSP_Locking (isTable uTableTable) byteTableRow "MandatoryWriteGranularity" "VU" (UIntCell . toInteger . spMandatoryWriteGranularity)
  , Vendor uSP_Locking (isTable uTableTable) byteTableRow "RecommendedAccessGranularity" "VU" (UIntCell . toInteger . spRecommendedAccessGranularity)
  , Vendor uSP_Locking (isTable uSecretProtectTable) anyRow "ProtectMechanisms" "VU" (const (ListCell []))
  , Vendor uSP_Locking (isTable uK_AES_128Table) anyRow "Key" "VU" (const (BytesCell (B.replicate 16 0)))
  , Vendor uSP_Locking (isTable uK_AES_256Table) anyRow "Key" "VU" (const (BytesCell (B.replicate 32 0)))
  , Vendor uSP_Locking (\desc -> isTable uK_AES_128Table desc || isTable uK_AES_256Table desc) anyRow "Mode" "VU"
      (const (EnumCell "XTS" (enumValue Core_symmetric_mode_XTS)))
  , Vendor uSP_Locking (isTable uC_PINTable) (isObject LockingSP.uC_PIN_Admin1) "PIN" "SID or MSID" (BytesCell . spMsid)
  ]
  where
    isTable u desc = tdTableUID desc == u
    isObject u r   = L.lookup uidColumn r == Just (UIDCell u)
    anyRow         = const True
    -- the Locking SP's Table rows of its byte tables, the MBR's and the DataStore's (Opal 2.30 Table 36, PDF 54)
    byteTableRow r = isObject uMBRTableObject r || isObject LockingSP.uTable_DataStore r
