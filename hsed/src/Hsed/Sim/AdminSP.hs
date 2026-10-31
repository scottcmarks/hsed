{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.AdminSP
Description : The Admin SP's rows the simulator holds until Opal's preconfiguration compiles (M4b): fourteen, hand-written from Opal 2.30's tables
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What UCT-03 step 1 and the simulator's dispatch need of the Admin SP (design
8.3), written by hand from Opal 2.30's preconfiguration tables, each row
citing its table and page, until M4b reads those tables with the compiler
(design 11; the rows are then the generated @Hsed.Gen.Opal@ preconfiguration,
and this module goes):

* MethodID: Get (Table 21, PDF 38);
* AccessControl: Get on C_PIN_SID, whose ACL is ACE_C_PIN_SID_Get_NOPIN, and
  Get on C_PIN_MSID, whose ACL is ACE_C_PIN_MSID_Get_PIN (Table 22, PDF 42);
* ACE: those two, the first Admins OR SID over the columns other than the
  PIN, the second Anybody over the UID and the PIN (Table 23, PDF 47);
* Authority: Anybody, Admins, SID and Admin1 (Table 24, PDF 48);
* C_PIN: SID, MSID and Admin1 (Table 25, PDF 48-49);
* SP: Admin and Locking (Table 29, PDF 50).

The columns are found by name in the generated table descriptions
("Hsed.Gen.Core.TableDescs"), the UIDs are Core's where Core assigns them
("Hsed.Gen.Core.UIDs") and Opal's otherwise ("Hsed.OpalUIDs"), an
enumeration's value is the generated enumeration's ("Hsed.Gen.Core.Enums";
a life cycle's, "Hsed.Sim.Params".'lifeCycleValue'), and the values a run
chooses come from the parameters ("Hsed.Sim.Params": the MSID, the SID's PIN
and TryLimit, the Locking SP's life cycle), which is why this module reads
the parameters and not the state. Where Core and Opal disagree the rows
follow Opal: Anybody's Operation is None (Table 24), not Core Table 209's
Sign (PDF 211); the Admin SP's life cycle is Manufactured (Table 29), where
Core 4.1 has only Issued; M4b settles which wins (the M4a plan's design
corrections, design 11).

Since M4b's scaffold the rows are the Admin SP's preconfiguration tables
('handTables', in the form the spec compiler generates them,
"Hsed.Core.Preconfig"), the stand-in the simulator folds into the Admin SP's
tables ("Hsed.Sim.Preconfig") until the generated ones replace them.
-}
module Hsed.Sim.AdminSP
  ( handTables
  , handRows
  ) where

import qualified RIO.List                as L
import           RIO

import           Hsed.Core.Preconfig     (PreCell (..), PreconfRow (..), PreconfTable (..), RowKind (..))
import           Hsed.Core.Table         (Cell (..), Col (..), ColNum, Pre (..), TableDesc (..), columnNamed)
import           Hsed.Core.Types         (Enumerated, enumValue)
import           Hsed.Core.UID           (uNull)
import           Hsed.Gen.Core.Enums     (Core_auth_method (..), Core_boolean (..), Core_boolean_ACE (..))
import           Hsed.Gen.Core.TableDescs (aceTableDesc, accessControlTableDesc, authorityTableDesc, c_PINTableDesc,
                                          methodIDTableDesc, spTableDesc)
import           Hsed.Gen.Core.UIDs      (uAuthority_Admins, uAuthority_Anybody, uAuthority_SID, uC_PIN_SID, uMethodID_Get,
                                          uSP_Admin)
import           Hsed.OpalUIDs           (uACE_C_PIN_MSID_Get_PIN, uACE_C_PIN_SID_Get_NOPIN, uAuthority_Admin1, uC_PIN_Admin1,
                                          uC_PIN_MSID, uSP_Locking)
import           Hsed.Sim.Params         (LifeCycle (..), SimParams (..), lifeCycleValue)


-- | The fourteen rows as the Admin SP's preconfiguration tables, as a run's
--   parameters fill them: one table for each description, in the order of
--   its first row, its rows in the order written, each a plain row (one
--   object, not optional, no notes) of the cells it gives ('Given'), placed
--   by the description's column numbers in column order, with no access
--   (the legend's, which the generated tables carry); no column has a
--   default, so a row holds exactly the cells written.
handTables :: SimParams -> [PreconfTable]
handTables p =
  [ PreconfTable desc [] [ Pre (PreconfRow Plain False (cellsOf desc cells) []) [] | (d, cells) <- rows, tdTableUID d == tdTableUID desc ]
  | desc <- L.nubBy ((==) `on` tdTableUID) (map fst rows) ]
  where rows = handRows p

-- | The fourteen rows, each its table's description and its cells by column
--   name, in column order. A column a row does not give is absent from the
--   row, and a Get that asks it returns no cell for it, which a host reads
--   as a column it may not read (Core 5.3.4.2.2, PDF 223), so every column
--   an ACE grants is given here where the type has a default the spec's
--   table prints (C_PIN_SID's CharSet, Null: a zero uidref, Opal Table 25,
--   PDF 48; Core 5.3.4.1.1.1, PDF 210); M4b's generated preconfiguration
--   supplies the type's default for every column of the description a row
--   lacks (design 13.31 (a)). Exported for the test that holds every name
--   to its table's description.
handRows :: SimParams -> [(TableDesc, [(Text, Cell)])]
handRows p =
  [ -- Opal 2.30 Table 21, MethodID (PDF 38): the one method M4a dispatches
    (methodIDTableDesc, [ ("UID", UIDCell uMethodID_Get), ("Name", TextCell "Get") ])
    -- Table 22, AccessControl (PDF 42): Get on C_PIN_SID and on C_PIN_MSID, each with its ACL; the table's own UIDs
    -- are the TPer's to assign and Opal prints none
  , (accessControlTableDesc, [ ("InvokingID", RefCell "C_PIN_SID" uC_PIN_SID), ("MethodID", RefCell "Get" uMethodID_Get)
                             , ("ACL", ListCell [RefCell "ACE_C_PIN_SID_Get_NOPIN" uACE_C_PIN_SID_Get_NOPIN]) ])
  , (accessControlTableDesc, [ ("InvokingID", RefCell "C_PIN_MSID" uC_PIN_MSID), ("MethodID", RefCell "Get" uMethodID_Get)
                             , ("ACL", ListCell [RefCell "ACE_C_PIN_MSID_Get_PIN" uACE_C_PIN_MSID_Get_PIN]) ])
    -- Table 23, ACE (PDF 47): BooleanExpr in postfix over the authorities (Core 5.3.4.3.2-3, PDF 226-227; Table 61,
    -- PDF 104), Columns by number in the C_PIN table (Core 5.3.2.9.5, PDF 165: none means all)
  , (aceTableDesc, [ ("UID", UIDCell uACE_C_PIN_SID_Get_NOPIN), ("Name", TextCell "ACE_C_PIN_SID_Get_NOPIN")
                   , ("BooleanExpr", ListCell [ RefCell "Admins" uAuthority_Admins, RefCell "SID" uAuthority_SID
                                              , enumCell "Or" Core_boolean_ACE_Or ])
                   , ("Columns", c_PINColumns ["UID", "CharSet", "TryLimit", "Tries", "Persistence"]) ])
  , (aceTableDesc, [ ("UID", UIDCell uACE_C_PIN_MSID_Get_PIN), ("Name", TextCell "ACE_C_PIN_MSID_Get_PIN")
                   , ("BooleanExpr", ListCell [RefCell "Anybody" uAuthority_Anybody])
                   , ("Columns", c_PINColumns ["UID", "PIN"]) ])
    -- Table 24, Authority (PDF 48): Anybody and Admins with Operation None, SID a Password authority on C_PIN_SID,
    -- Admin1 one on C_PIN_Admin1, of the class Admins and disabled; a Null reference is the column's default
  , (authorityTableDesc, [ ("UID", UIDCell uAuthority_Anybody), ("Name", TextCell "Anybody"), ("IsClass", boolCell False)
                         , ("Enabled", boolCell True), ("Operation", enumCell "None" Core_auth_method_None) ])
  , (authorityTableDesc, [ ("UID", UIDCell uAuthority_Admins), ("Name", TextCell "Admins"), ("IsClass", boolCell True)
                         , ("Enabled", boolCell True), ("Operation", enumCell "None" Core_auth_method_None) ])
  , (authorityTableDesc, [ ("UID", UIDCell uAuthority_SID), ("Name", TextCell "SID"), ("IsClass", boolCell False)
                         , ("Enabled", boolCell True), ("Operation", enumCell "Password" Core_auth_method_Password)
                         , ("Credential", RefCell "C_PIN_SID" uC_PIN_SID) ])
  , (authorityTableDesc, [ ("UID", UIDCell uAuthority_Admin1), ("Name", TextCell "Admin1"), ("IsClass", boolCell False)
                         , ("Class", RefCell "Admins" uAuthority_Admins), ("Enabled", boolCell False)
                         , ("Operation", enumCell "Password" Core_auth_method_Password)
                         , ("Credential", RefCell "C_PIN_Admin1" uC_PIN_Admin1) ])
    -- Table 25, C_PIN (PDF 48-49): C_PIN_SID's PIN the parameters' (the MSID's by default, 4.2.1.8, PDF 49), its
    -- CharSet Null, the zero uidref the column defaults to (Core 5.3.4.1.1.1, PDF 210: zeroes), its TryLimit the
    -- parameters' (VU), its Tries 0, the count a new C_PIN object starts at (Core 5.3.4.1.1.2, PDF 210); C_PIN_MSID's
    -- PIN the MSID, its other cells blank as Table 25 prints them; C_PIN_Admin1's PIN empty, its limits 0
  , (c_PINTableDesc, [ ("UID", UIDCell uC_PIN_SID), ("Name", TextCell "C_PIN_SID")
                     , ("PIN", BytesCell (fromMaybe (spMsid p) (spSidPin p))), ("CharSet", RefCell "Null" uNull)
                     , ("TryLimit", UIntCell (toInteger (spSidTryLimit p))), ("Tries", UIntCell 0)
                     , ("Persistence", boolCell False) ])
  , (c_PINTableDesc, [ ("UID", UIDCell uC_PIN_MSID), ("Name", TextCell "C_PIN_MSID"), ("PIN", BytesCell (spMsid p)) ])
  , (c_PINTableDesc, [ ("UID", UIDCell uC_PIN_Admin1), ("Name", TextCell "C_PIN_Admin1"), ("PIN", BytesCell mempty)
                     , ("TryLimit", UIntCell 0), ("Tries", UIntCell 0), ("Persistence", boolCell False) ])
    -- Table 29, SP (PDF 50): the Admin SP Manufactured, the Locking SP in its Original Factory State
  , (spTableDesc, [ ("UID", UIDCell uSP_Admin), ("Name", TextCell "Admin"), ("LifeCycleState", lifeCycleCell Manufactured)
                  , ("Frozen", boolCell False) ])
  , (spTableDesc, [ ("UID", UIDCell uSP_Locking), ("Name", TextCell "Locking")
                  , ("LifeCycleState", lifeCycleCell (spLockingOFS p)), ("Frozen", boolCell False) ])
  ]

-- | A row's cells from its cells by column name, each placed by the
--   description's column number, in column order; a name the description
--   lacks places no cell (the test over 'handRows' finds every name).
cellsOf :: TableDesc -> [(Text, Cell)] -> [(ColNum, PreCell)]
cellsOf desc cells = L.sortOn fst [ (colNumber col, Given cell) | (name, cell) <- cells, Just col <- [columnNamed name desc] ]

-- | An ACE's Columns: the numbers of the C_PIN table's columns named (Core
--   5.3.2.9.5, PDF 165).
c_PINColumns :: [Text] -> Cell
c_PINColumns names = ListCell [ UIntCell (toInteger (colNumber col)) | name <- names, Just col <- [columnNamed name c_PINTableDesc] ]

-- | An enumeration's cell: its label as the spec prints it, and the value
--   the generated enumeration assigns.
enumCell :: (Enumerated e, Enum e) => Text -> e -> Cell
enumCell label e = EnumCell label (enumValue e)

-- | A boolean cell (Core Table 59, PDF 103).
boolCell :: Bool -> Cell
boolCell b = if b then enumCell "True" Core_boolean_True else enumCell "False" Core_boolean_False

-- | An SP's LifeCycleState cell, labelled as Table 29 prints it, its value
--   Opal 2.30 Table 49's ('lifeCycleValue', PDF 98).
lifeCycleCell :: LifeCycle -> Cell
lifeCycleCell lc = EnumCell label (lifeCycleValue lc)
  where
    label = case lc of
      ManufacturedInactive -> "Manufactured-Inactive"
      Manufactured         -> "Manufactured"
