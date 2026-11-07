{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Sim.Base
Description : The Base Template's methods the simulator runs in a session: Get and Set, each with its Fails list
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The behaviours of Core 5.3.3's Basic Table Method Group that the simulator
runs (design 8.2, 13.16): 'get', bound by @behaviour "Base.get"@ on 5.3.3.6
(PDF 201) since M4a, with its Fails list 5.3.3.6.3 bound by @fails
"Base.getFails"@, one 'FailItem' per item a to d, each with its status and
the QuickCheck property that the simulator fails so; and since M5b 'set',
the minimal object Set of design 11 M5b, bound by @behaviour "Base.set"@ on
5.3.3.7 (PDF 201-203), with its Fails list 5.3.3.7.4 (PDF 203) bound by
@fails "Base.setFails"@, items a to e, each with its property likewise. The
cell_block is read and resolved by "Hsed.Core.CellBlock" (5.1.4.2.3, PDF
129-130), the columns granted by "Hsed.Sim.Access", and the RowValues come
in column order (5.3.3.6.2.2, PDF 201), the cells the caller may not read
left out (5.3.4.2.2, PDF 223).

What Get or Set is invoked on is found among the tables of the session's SP
as the session sees it ("Hsed.Sim.State".'spView': within a transaction its
working copy, since M5b): a byte table, the InvokingID the table's UID
("Hsed.Sim.Types".@spsBytes@: the Locking SP's MBR and DataStore, held since
M5a, which Get reads since M5b, design 11, as Bytes, Core 5.3.3.6.2.1, PDF
201), an object table, the InvokingID a table's UID, or an object, the row
of a table whose UID column holds the InvokingID. The descriptions of those
tables, which give a target its last column and a Set its columns' types,
are the ones the SP holds with them ("Hsed.Sim.Types".@spsDescs@, its
preconfiguration's: "Hsed.Sim.Preconfig").
-}
module Hsed.Sim.Base
  ( get
  , getFails
  , set
  , setFails
  , behaviours
  ) where

import qualified RIO.ByteString           as B
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import           RIO                      hiding (set)
import           Test.QuickCheck          (Gen, Property, arbitrary, chooseInt, counterexample, elements, forAll, listOf, oneof, shuffle,
                                           sublistOf, suchThat, vectorOf, (.&&.), (===))

import           Hsed.Core.CellBlock      (CellBlock (..), CellTarget (..), CellsAsked (..), RowAddress (..), cellBlock,
                                           cellBlockValue, columnRange, noCells, resolveCells)
import           Hsed.Core.Table          (Cell (..), Col (..), Row, TableDesc (..), cellValue, columnOf)
import           Hsed.Core.Token          (IsToken (..), Token (..))
import           Hsed.Core.UID            (UID, hNull, uidLower, (+:+))
import           Hsed.Core.Value          (Datum (..), List (..), Name (..), NamedValue (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes      (StatusCode, pattern FAIL, pattern INVALID_PARAMETER, pattern NOT_AUTHORIZED,
                                           pattern RESPONSE_OVERFLOW, pattern SUCCESS)
import qualified Hsed.Gen.Core.Methods    as Methods
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.UIDs       (uAuthority_Admins, uAuthority_Anybody, uLocking_GlobalRange, uMethodID_Get, uMethodID_Set,
                                           uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uC_PIN_MSID, uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Catalogue       (CaseKey (..), TestId (..))
import           Hsed.Sim.Access          (grantedColumns)
import           Hsed.Sim.Behaviour       (Behaviour (..), FailItem (..), Fails (..), Invoked (..), Reply (..), SomeBehaviour,
                                           forMethod)
import           Hsed.Sim.Bytes           (byteRange, bytesWithin)
import           Hsed.Sim.Cells           (CellRefusal (..), cellFor, putCell)
import           Hsed.Sim.State           (cellOf, openSession, putSPView, spView, updateRowIn)
import           Hsed.Sim.Types           (ByteTable (..), LifeCycle (..), Limits (..), SPState (..), Session (..), SimParams (..),
                                           TPerState (..), comIdHostIndTokenSize, comIdHostLimits, defaultSimParams, initialState)


-- | Get on an object, a table or a byte table (Core 5.3.3.6, PDF 201;
--   @behaviour "Base.get"@): the cells its cell_block asks, among those the
--   session's authorities may read, as RowValues in column order, or a byte
--   table's rows as Bytes. UCT-03 step 1b reads C_PIN_MSID's PIN with it,
--   ETC-04 sends it on a session the TPer does not have, and since M5b
--   SPF-02 case 2 reads the DataStore's first 1024 rows with it (Test Cases
--   PDF 53), SPF-11 User1's Enabled (PDF 64) and SPF-12 case 1 the Global
--   Range's ReadLocked and WriteLocked (PDF 65-66).
--
--   The run, within a session (a call outside one, which the Session Manager
--   ignores, is NOT_AUTHORIZED):
--
--   * the InvokingID is a byte table, an object table or an object of the
--     session's SP as the session sees it ('target', "Hsed.Sim.State".'spView'),
--     else NOT_AUTHORIZED (Fails a, 5.3.3.6.3, PDF 201; the same status the
--     access control gate gives an InvokingID the AccessControl table has no
--     row for, 5.1.5.2, PDF 137);
--   * the columns the session may read of it through Get are what the ACL's
--     satisfied ACEs grant ("Hsed.Sim.Access".'grantedColumns'), else
--     NOT_AUTHORIZED, the gate's answer again;
--   * the cell_block is read ("Hsed.Core.CellBlock".'cellBlock') and
--     resolved against the target (its last column, or a byte table's last
--     row, 'resolveCells'): a component the context forbids, or out of
--     bounds, is INVALID_PARAMETER (5.1.5.11 e, PDF 139; Fails b-d), as is a
--     parameter that is no cell_block (5.1.5.11 d); on a table, the object
--     startRow names must exist (Fails a); a byte table of no rows has none
--     to read (5.1.4.2.3 c.a, PDF 130: endRow defaults to the last row);
--   * on an object or an object table the result is one RowValues list
--     (5.3.3.6.2.2): the columns asked, among those granted (an empty grant
--     meaning all, 5.3.2.9.5, PDF 165) and those the row holds, in column
--     order, each a Named value of the column number and the cell's value
--     ("Hsed.Core.Table".'cellValue'); the cells not granted are left out
--     and that is no error (5.3.4.2.2, PDF 223), so a row of none is
--     @[L (List [])]@;
--   * on a byte table the result is one Bytes value, the rows asked from
--     the lowest to the highest (5.3.3.6.2.1, PDF 201;
--     "Hsed.Sim.Bytes".'byteRange'), unless there are more of them than the
--     host's ComPacket holds (the response buffer, the host's
--     MaxComPacketSize bounded by the TPer's MaxResponseComPacketSize,
--     "Hsed.Sim.Params".'comIdHostLimits') or than one token within the
--     host's MaxIndTokenSize carries, its header and its data counted
--     (5.2.2.4.1.5, PDF 146: the TPer SHALL NOT send a larger token;
--     "Hsed.Sim.Params".'comIdHostIndTokenSize', 0 no limit; the header the
--     least atom's that holds the rows, "Hsed.Sim.Bytes".'bytesWithin'):
--     then the method fails RESPONSE_OVERFLOW with an empty result before a
--     byte is read (Opal 2.30 3.3.1 items 1-3, PDF 29; Core 5.1.5.14, PDF
--     139), a Get of the whole MBR, 128 MiB (Opal Table 36, PDF 54),
--     building nothing; the response's framing decides again, with its
--     overhead, whether what is built fits ("Hsed.Sim.Session").
get :: Behaviour Methods.Get
get = Behaviour
  { behName  = "Base.get"
  , behHash  = "9d67d740e4cb"
  , behCases = [ CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing, CaseKey (SPF 2) (Just 2) Nothing Nothing
               , CaseKey (SPF 11) Nothing Nothing Nothing, CaseKey (SPF 12) (Just 1) Nothing (Just "no-media")
               , CaseKey (ETC 4) Nothing Nothing Nothing ]
  , behRun   = \iv (Methods.Get cb) st -> (getReply iv cb st, st)
  }

-- | Get's reply, the TPer unchanged: the module's description of 'get'.
getReply :: Invoked -> Value -> TPerState -> Reply
getReply iv cb st = case ivSession iv of
  Nothing  -> notAuthorized
  Just ses -> fromMaybe notAuthorized $ do
    sps     <- spView ses st
    found   <- target sps (ivInvoker iv)
    granted <- grantedColumns sps ses (ivInvoker iv) uMethodID_Get
    pure (either (const invalidParameter) id (cells ses found granted))
  where
    -- the cell_block read and resolved against the target, then the cells asked: a row's, granted, or a byte table's
    cells ses found granted = do
      block <- cellBlock cb
      tgt   <- targetOf found
      asked <- resolveCells tgt block
      pure $ case (found, asked) of
        (Object _ row, Columns s e)     -> rowValues granted s e row
        (Table _ rows, ObjectRow u s e) -> maybe notAuthorized (rowValues granted s e) (L.find ((== Just (UIDCell u)) . cellOf 0) rows)
        (BytesTable bt, ByteRows s e)   -> bytes ses s e bt
        _                               -> invalidParameter
    rowValues granted s e row =
      ReplyResults [L (List [ N ((fromIntegral c :: Natural) .: cellValue v) | (c, v) <- L.sortOn fst row
                                                                              , c >= s, c <= e, null granted || c `elem` granted ])] SUCCESS
    -- a byte table's rows, unless more than the host's ComPacket holds or one token within the host's
    -- MaxIndTokenSize carries: then RESPONSE_OVERFLOW before any is read
    bytes ses s e bt
      | e - s + 1 > room = ReplyResults [] RESPONSE_OVERFLOW
      | otherwise        = ReplyResults [cellValue (BytesCell (byteRange s e bt))] SUCCESS
      where
        host = comIdHostLimits (tsParams st) (sesHostProps ses)
        -- the rows must fit the host's ComPacket and, as one Bytes token, header and data, the host's
        -- MaxIndTokenSize (Core 5.2.2.4.1.5, PDF 146), the header the least atom's that holds them ('bytesWithin')
        room = fromIntegral (min (lComPacketSize host) (bytesWithin (comIdHostIndTokenSize (tsParams st) (sesHostProps ses))))

-- | A reply of no results and a status.
notAuthorized, invalidParameter :: Reply
notAuthorized    = ReplyResults [] NOT_AUTHORIZED
invalidParameter = ReplyResults [] INVALID_PARAMETER

-- | What Get or Set is invoked on in an SP: a byte table of the SP, by its
--   UID; a table of the SP, by its UID, with its rows; or an object, by the
--   UID column of its row, with the row; a table or an object with its
--   table's description.
data Found
  = Object !TableDesc !Row
  | Table !TableDesc ![Row]
  | BytesTable !ByteTable

-- | The byte table, table or object an InvokingID names among an SP's
--   tables, if any: a byte table the SP holds ("Hsed.Sim.Types".@spsBytes@,
--   looked in first: a byte table has no description and no rows, Core
--   3.2.5.2, PDF 51, so it is no table of rows and no object), a table the
--   SP describes and holds rows of, by its UID, or the row of one whose UID
--   column holds the UID, with the description the SP holds of its table
--   ("Hsed.Sim.Types".@spsDescs@).
target :: SPState -> UID -> Maybe Found
target sps u = bytes <|> table <|> object
  where
    bytes  = BytesTable <$> Map.lookup u (spsBytes sps)
    table  = Table <$> Map.lookup u (spsDescs sps) <*> Map.lookup u (spsTables sps)
    object = listToMaybe [ Object desc row | desc <- Map.elems (spsDescs sps), row <- rowsOf desc, cellOf 0 row == Just (UIDCell u) ]
    rowsOf desc = fromMaybe [] (Map.lookup (tdTableUID desc) (spsTables sps))

-- | The cell_block target a byte table, a table or an object is: a byte
--   table's last row, a table's or an object's last column (Core 5.1.4.2.3,
--   PDF 129-130); or why there is none, a byte table of no rows (c.a, PDF
--   130: endRow defaults to the last row, which it lacks), which Get answers
--   INVALID_PARAMETER as it does a cell_block out of bounds (Fails d).
targetOf :: Found -> Either Text CellTarget
targetOf = \case
  Object desc _ -> Right (OnObject (lastColumn desc))
  Table desc _  -> Right (OnObjectTable (lastColumn desc))
  BytesTable bt
    | btSize bt == 0 -> Left "a byte table of no rows has no row to read (Core 5.1.4.2.3 c.a, PDF 130)"
    | otherwise      -> Right (OnByteTable (btSize bt - 1))

-- | A table's last column number, 0 for a table of one column.
lastColumn :: TableDesc -> Int
lastColumn = maybe 0 colNumber . L.lastMaybe . tdColumns


-- * The Fails list

-- | Get's Fails list (Core 5.3.3.6.3, PDF 201; @fails "Base.getFails"@): a,
--   the object does not exist, NOT_AUTHORIZED; b to d, a cell_block
--   component the context forbids or out of bounds (5.1.4.2.3's rules, PDF
--   129-130), INVALID_PARAMETER (5.1.5.11 e, PDF 139). Each item's property
--   runs 'get' in an Admin SP session as Anybody on the default TPer
--   ('asAnybody'), except c's, which runs it on the Locking SP's DataStore
--   in a session as Admin1 with the SP Manufactured ('asAdmin1'; since M5b,
--   byte tables being Get's targets):
--
--   * a: Get on a random UID that is no table and no object of the Admin SP
--     is NOT_AUTHORIZED;
--   * b: Get on C_PIN_MSID with a Table, a startRow or an endRow component
--     is INVALID_PARAMETER;
--   * c: Get on the DataStore with a startColumn or an endColumn component
--     is INVALID_PARAMETER, whatever its rows (5.1.4.2.3 d.b and e.b);
--   * d: Get on C_PIN_MSID with a column past the C_PIN table's last, or a
--     startColumn past its endColumn, is INVALID_PARAMETER.
getFails :: Fails Methods.Get
getFails = Fails
  { failsName  = "Base.getFails"
  , failsItems =
      [ FailItem "a" "fbdda8fb3e3a" NOT_AUTHORIZED (Just absentObject)
      , FailItem "b" "ca5584e1ccdf" INVALID_PARAMETER (Just rowComponentOnObject)
      , FailItem "c" "46db34a6034d" INVALID_PARAMETER (Just columnComponentOnByteTable)
      , FailItem "d" "1b371d07bd3b" INVALID_PARAMETER (Just outOfBounds)
      ]
  }

-- | Fails a: a random UID that names no table and no object of the Admin SP
--   gets NOT_AUTHORIZED, whatever the cell_block.
absentObject :: Property
absentObject = forAll absent $ \u -> forAll anyBlock $ \block -> getAsAnybody u block === notAuthorized
  where
    (_, st)  = asAnybody
    absent   = arbitrary `suchThat` \u -> isNothing (Map.lookup uSP_Admin (tsSPs st) >>= \sps -> target sps u)
    anyBlock = oneof [pure noCells, columnRange <$> small <*> small]
    small    = fromIntegral <$> chooseInt (0, 3)

-- | Fails b: a Table, startRow or endRow component on an object gets
--   INVALID_PARAMETER (Core 5.1.4.2.3 a.b, b.c, c.b).
rowComponentOnObject :: Property
rowComponentOnObject = forAll rowComponent $ \block -> getAsAnybody uC_PIN_MSID block === invalidParameter
  where
    rowComponent = oneof
      [ (\u -> noCells { cbTable = Just u }) <$> arbitrary
      , (\r -> noCells { cbStartRow = Just r }) <$> oneof [RowUID <$> arbitrary, RowNumber <$> natural 100]
      , (\n -> noCells { cbEndRow = Just n }) <$> natural 100 ]

-- | Fails c: a startColumn or endColumn component on the Locking SP's
--   DataStore, a byte table, gets INVALID_PARAMETER (Core 5.1.4.2.3 d.b,
--   e.b), with or without rows asked within the table.
columnComponentOnByteTable :: Property
columnComponentOnByteTable = forAll (columnComponent <*> rows) $ \block -> getAsAdmin1 dataStore block === invalidParameter
  where
    rows = oneof [ pure noCells
                 , (\s e -> noCells { cbStartRow = Just (RowNumber s), cbEndRow = Just (s + e) }) <$> natural 1000 <*> natural 1000 ]
    columnComponent = oneof [ (\n block -> block { cbStartColumn = Just n }) <$> natural 20
                            , (\n block -> block { cbEndColumn = Just n }) <$> natural 20 ]

-- | Fails d: a column past the table's last, or a startColumn past the
--   endColumn, on C_PIN_MSID gets INVALID_PARAMETER.
outOfBounds :: Property
outOfBounds = forAll beyond $ \block -> getAsAnybody uC_PIN_MSID block === invalidParameter
  where
    lastN  = fromIntegral (lastColumn c_PINTableDesc) :: Natural
    beyond = oneof
      [ (\n -> noCells { cbStartColumn = Just n }) <$> past lastN
      , (\n -> noCells { cbEndColumn = Just n }) <$> past lastN
      , (columnRange <$> within lastN <*> within lastN) `suchThat` (\b -> cbStartColumn b > cbEndColumn b) ]
    past n   = (n +) <$> natural 100 `suchThat` (> 0)
    within n = fromIntegral <$> chooseInt (0, fromIntegral n)

-- | A uinteger up to a bound.
natural :: Int -> Gen Natural
natural most = fromIntegral <$> chooseInt (0, most)

-- | The default TPer with an Admin SP session open as Anybody on its base
--   ComID, HSN 1: what Get's properties run on.
asAnybody :: (Session, TPerState)
asAnybody = openSession (spBaseComId defaultSimParams) 1 uSP_Admin True [uAuthority_Anybody] (initialState defaultSimParams)

-- | Get on an InvokingID with a cell_block in that session: its reply.
getAsAnybody :: UID -> CellBlock -> Reply
getAsAnybody u block = fst (behRun get invoked (Methods.Get (cellBlockValue block)) st)
  where
    (ses, st) = asAnybody
    invoked   = Invoked { ivComId = sesComId ses, ivSession = Just ses, ivInvoker = u }

-- | The default TPer with its Locking SP Manufactured ("Hsed.Sim.Params".@spLockingOFS@),
--   folded once for every property here ("Hsed.Sim.Gen".@foldedAt@ holds
--   the same TPer for the laws and the specs, which this module cannot
--   import: the generator imports the sessions, which import this).
manufactured :: TPerState
manufactured = initialState defaultSimParams { spLockingOFS = Manufactured }
{-# NOINLINE manufactured #-}

-- | The TPer Manufactured with a Locking SP session open as Admin1 on its
--   base ComID, HSN 1, Admin1's authorities as the Session Manager gives
--   them, with its class (Core 3.4.2.2, PDF 89: Opal 2.30 Table 40's Admin1
--   of class Admins, PDF 82) and Anybody (5.3.4.1.2.1, PDF 211): what Set's
--   properties run on.
asAdmin1 :: (Session, TPerState)
asAdmin1 = openSession (spBaseComId defaultSimParams) 1 uSP_Locking True
             [uAuthority_Anybody, LockingSP.uAuthority_Admin1, uAuthority_Admins] manufactured

-- | Get on an InvokingID with a cell_block in that session: its reply.
getAsAdmin1 :: UID -> CellBlock -> Reply
getAsAdmin1 u block = fst (behRun get invoked (Methods.Get (cellBlockValue block)) st)
  where
    (ses, st) = asAdmin1
    invoked   = Invoked { ivComId = sesComId ses, ivSession = Just ses, ivInvoker = u }

-- | The Locking SP's DataStore byte table, by the UID of the table it is:
--   its Table table row's UID's lower half followed by 00 00 00 00 (Core
--   3.2.5.3 b.i, PDF 51-52; Opal 2.30 Table 36, PDF 54), 00 00 10 01 00 00
--   00 00, as "Hsed.Sim.Types".@byteTables@ keys it.
dataStore :: UID
dataStore = uidLower LockingSP.uTable_DataStore +:+ hNull


-- * Set

-- | Set on an object, a table or a byte table (Core 5.3.3.7, PDF 201-203;
--   @behaviour "Base.set"@), the minimal object Set of design 11 M5b: the
--   cells of one object the session's authorities may Set, all of them or
--   none (Core 5.3.4.2.6, PDF 225; 3.3.7.2, PDF 77), of the types the
--   simulator's Set carries out ("Hsed.Sim.Cells".'cellFor': boolean and
--   reset_types), written into the SP as the session sees it
--   ("Hsed.Sim.State".'putSPView': within a transaction its working copy,
--   committed when the transaction is). SPF-11 observes it on User1's
--   Enabled (Test Cases PDF 64) and SPF-12 case 1 on TPerInfo's
--   ProgrammaticResetEnable and the Global Range's locking columns (PDF 65).
--   Set follows the ACL alone, as Get does (design 13.32 (2)): no M5b Set
--   touches a cell Opal 2.30's legend marks read-only (User1's Enabled, the
--   Global Range's ReadLockEnabled, WriteLockEnabled and LockOnReset and
--   TPerInfo's ProgrammaticResetEnable are writable with fixed access
--   control, ReadLocked and WriteLocked personalizable: Tables 40, 45 and
--   27, PDF 82, 86 and 50).
--
--   The run, within a session (a call outside one is NOT_AUTHORIZED), no
--   change made unless every check passes, the checks in this order
--   (the access control gate's, an AccessControl row for the InvokingID and
--   Set with a satisfied ACE, having passed in the dispatch, 5.1.5.2, PDF
--   137):
--
--   (1) the InvokingID is a byte table, an object table or an object of the
--   session's SP as the session sees it ('target'), else NOT_AUTHORIZED
--   (Fails a, 5.3.3.7.4, PDF 203);
--
--   (2) Where (5.3.3.7.1, PDF 202; a row_address, a uidref or a uinteger
--   bare in the stream, 5.1.4.2.14, PDF 135): on an object, given,
--   INVALID_PARAMETER (5.3.3.7.1.1); on an object table, absent or not a
--   uid, INVALID_PARAMETER (5.3.3.7.1.1), a uid naming no row of the table,
--   NOT_AUTHORIZED (Fails a); on a byte table, given and not a uinteger row
--   of the table, INVALID_PARAMETER (5.3.3.7.1.2; 5.1.5.11 f, PDF 139, a row
--   past the last);
--
--   (3) Values absent: SUCCESS, nothing changed (5.3.3.7.2, PDF 202: an
--   otherwise correct invocation without Values succeeds with no effect);
--
--   (4) on a byte table, Values not Bytes, INVALID_PARAMETER (5.3.3.7.2;
--   Fails d); Bytes, FAIL, the simulator's gap until M6, no byte table
--   written before it (Scott's answer Q3; 5.1.5.16, PDF 139);
--
--   (5) on an object, Values not a list of Named values, each a uinteger
--   column number and a value, INVALID_PARAMETER (5.3.3.7.2.2, PDF 202-203;
--   5.1.5.11 d, PDF 139);
--
--   (6) a column given twice, INVALID_PARAMETER (5.3.4.2.6, PDF 225);
--
--   (7) a column the table's description lacks, INVALID_PARAMETER (5.1.5.11,
--   PDF 139);
--
--   (8) column 0, the UID, INVALID_PARAMETER (Fails b: a UID or other system
--   cell), checked before the grant so that b is observed on Opal's ACEs,
--   none of which grants column 0;
--
--   (9) a column the ACL's satisfied ACEs do not grant for Set
--   ("Hsed.Sim.Access".'grantedColumns', an empty grant meaning all,
--   5.3.2.9.5, PDF 165), NOT_AUTHORIZED for the whole method (5.3.4.2.6,
--   PDF 225; Fails e);
--
--   (10) each value read as its column's cell ('cellFor'): one not of the
--   column's type, INVALID_PARAMETER (Fails c; 5.1.5.11 b), whatever the
--   other cells hold; one of a type the simulator's Set does not carry out
--   decides nothing yet, its FAIL coming after (11);
--
--   (11) a LockOnReset or MBRDoneOnReset cell (the columns by name in the
--   SP's description) holding a set other than {0} or {0,3}, or {0,1} or
--   {0,1,3} where the TPer supports a hardware reset for them
--   ("Hsed.Sim.Params".@spHwResetLorDor@, Opal 2.30 3.1.1.3, PDF 21),
--   INVALID_PARAMETER (Opal 4.3.5.2.2, PDF 88; 4.3.5.3.1, PDF 88-89; Core
--   5.1.5.11 f); then a cell of a type the simulator's Set does not carry
--   out, FAIL (Q3; 5.1.5.16, PDF 139: the status where none of the others
--   applies), so that FAIL answers a Set whose sole fault is such a column,
--   whatever the cells' order, and INVALID_PARAMETER one whose parameters
--   are also wrong;
--
--   (12) else the cells put in the row ("Hsed.Sim.Cells".'putCell', through
--   "Hsed.Sim.State".'updateRowIn') and the SP put back as the session sees
--   it: SUCCESS, an empty result (5.3.3.7.3, PDF 203).
set :: Behaviour Methods.Set
set = Behaviour
  { behName  = "Base.set"
  , behHash  = "4122043deba7"
  , behCases = [CaseKey (SPF 11) Nothing Nothing Nothing, CaseKey (SPF 12) (Just 1) Nothing (Just "no-media")]
  , behRun   = \iv (Methods.Set wh vs) st -> setReply iv wh vs st
  }

-- | Set's reply and the TPer after it, the TPer as it was unless the reply
--   is SUCCESS: the module's description of 'set', its checks in the order
--   given there, (10)'s FAIL after (11).
setReply :: Invoked -> Maybe Value -> Maybe Value -> TPerState -> (Reply, TPerState)
setReply iv wh vs st = either (\code -> (ReplyResults [] code, st)) (\st' -> (ReplyResults [] SUCCESS, st')) $ do
  ses   <- maybe (Left NOT_AUTHORIZED) Right (ivSession iv)
  sps   <- maybe (Left NOT_AUTHORIZED) Right (spView ses st)
  found <- maybe (Left NOT_AUTHORIZED) Right (target sps (ivInvoker iv))             -- (1)
  case found of
    BytesTable bt   -> st <$ onByteTable bt                                              -- (2) to (4): nothing changes
    Object desc _   -> do
      when (isJust wh) (Left INVALID_PARAMETER)                                          -- (2)
      onObject ses sps desc (ivInvoker iv)
    Table desc rows -> do
      u <- case wh of                                                                    -- (2)
        Just (D (Datum t)) | Just u <- fromToken t -> Right (u :: UID)
        _                                          -> Left INVALID_PARAMETER
      unless (any ((== Just (UIDCell u)) . cellOf 0) rows) (Left NOT_AUTHORIZED)         -- (2), Fails a
      onObject ses sps desc u
  where
    onByteTable bt = do
      case wh of                                                                         -- (2)
        Nothing                                      -> Right ()
        Just (D (Datum (Unsigned r))) | r < btSize bt -> Right ()
        Just _                                       -> Left INVALID_PARAMETER
      case vs of
        Nothing                    -> Right ()                                           -- (3)
        Just (D (Datum (Bytes _))) -> Left FAIL                                          -- (4), Q3
        Just _                     -> Left INVALID_PARAMETER                             -- (4), Fails d
    onObject ses sps desc u = case vs of
      Nothing -> Right st                                                                -- (3)
      Just v  -> do
        pairs   <- maybe (Left INVALID_PARAMETER) Right (rowValues v)                    -- (5)
        unless (L.nub (map fst pairs) == map fst pairs) (Left INVALID_PARAMETER)         -- (6)
        cols    <- traverse (\(n, x) -> maybe (Left INVALID_PARAMETER) (\c -> Right (c, x)) (columnNumbered n desc)) pairs  -- (7)
        when (any ((== 0) . colNumber . fst) cols) (Left INVALID_PARAMETER)              -- (8)
        granted <- maybe (Left NOT_AUTHORIZED) Right (grantedColumns sps ses (ivInvoker iv) uMethodID_Set)  -- (9)
        unless (null granted || all ((`elem` granted) . colNumber . fst) cols) (Left NOT_AUTHORIZED)
        let readCells = [ (colNumber c, cellFor c x) | (c, x) <- cols ]                  -- (10)
            cells     = [ (n, cell) | (n, Right cell) <- readCells ]
            refusals  = [ r | (_, Left r) <- readCells ]
        -- a value not of its column's type is INVALID_PARAMETER (Fails c; 5.1.5.11 b), decided over every
        -- cell and with check (11) before a column the simulator does not carry out, which alone is FAIL
        -- (5.1.5.16, PDF 139; Scott's answer Q3), so the status does not turn on the cells' order
        when (any notOfType refusals) (Left INVALID_PARAMETER)                            -- (10), Fails c
        traverse_ (resetSet desc) cells                                                  -- (11)
        unless (null refusals) (Left FAIL)                                               -- (10), Q3: the sole fault
        pure (putSPView ses (updateRowIn (tdTableUID desc) u (\r -> L.foldl' (flip (uncurry putCell)) r cells) sps) st)  -- (12)
    -- the RowValues option: a list of Named values, each named by a uinteger column number
    rowValues = \case
      L (List xs) -> traverse named xs
      _           -> Nothing
    named = \case
      N (NamedValue (Name (Datum (Unsigned n))) x) -> Just (toInteger n, x)
      _                                            -> Nothing
    columnNumbered n desc = L.find ((== n) . toInteger . colNumber) (tdColumns desc)
    -- a value not of its column's type (Fails c; INVALID_PARAMETER), as against one of a type the
    -- simulator's Set does not carry out (NotSimulated; FAIL, Q3)
    notOfType = \case
      NotOfType _    -> True
      NotSimulated _ -> False
    -- a reset_types set in a reset column must be one the TPer supports (Opal 4.3.5.2.2, 4.3.5.3.1)
    resetSet desc (c, cell)
      | Just c `elem` [columnOf desc "LockOnReset", columnOf desc "MBRDoneOnReset"], listed cell `notElem` supported = Left INVALID_PARAMETER
      | otherwise = Right ()
    supported = [[0], [0, 3]] <> [ s | spHwResetLorDor (tsParams st), s <- [[0, 1], [0, 1, 3]] ]
    listed = \case
      ListCell cs -> [ v | EnumCell _ v <- cs ]
      _           -> []

-- | Set's Fails list (Core 5.3.3.7.4, PDF 203; @fails "Base.setFails"@):
--   a, the table or object does not exist, NOT_AUTHORIZED (the status Get's
--   item a and the access control gate give, 5.1.5.2, PDF 137); b, a change
--   to a UID or another system cell, INVALID_PARAMETER; c, a value larger
--   than its cell's type allows, INVALID_PARAMETER (5.1.5.11 b, PDF 139); d,
--   column values on a byte table, INVALID_PARAMETER (5.1.5.11 d, PDF 139);
--   e, Set restricted by access control on any row or column asked,
--   NOT_AUTHORIZED (5.3.4.2.6, PDF 225). Each item's property runs 'set' in
--   a Locking SP session as Admin1 with the SP Manufactured ('asAdmin1'),
--   and holds the TPer unchanged:
--
--   * a: Set on a random UID that is no byte table, no table and no object
--     of the Locking SP is NOT_AUTHORIZED, whatever its Where and Values;
--   * b: Set of column 0, the UID, of User1 or of the Global Range, with or
--     without columns Admin1 is granted, is INVALID_PARAMETER;
--   * c: Set of a boolean column Admin1 is granted (User1's Enabled, the
--     Global Range's ReadLockEnabled to WriteLocked) to a uinteger 2 to 255
--     or to bytes, or of the Global Range's LockOnReset to {32} or to a
--     list naming 0 twice, is INVALID_PARAMETER;
--   * d: Set on the DataStore, a byte table, with RowValues, a list of
--     column values, is INVALID_PARAMETER, Where absent or a row;
--   * e: Set of User1's IsClass or PresentCertificate, booleans Admin1 is
--     not granted (Opal 2.30 Table 38, PDF 66: Admin1 is granted Enabled by
--     ACE_Authority_Set_Enabled and CommonName by ACE_User1_Set_CommonName,
--     Table 39, PDF 79), with or without Enabled, is NOT_AUTHORIZED.
setFails :: Fails Methods.Set
setFails = Fails
  { failsName  = "Base.setFails"
  , failsItems =
      [ FailItem "a" "085e0146f5d8" NOT_AUTHORIZED (Just absentTarget)
      , FailItem "b" "bafcad538d76" INVALID_PARAMETER (Just uidColumn)
      , FailItem "c" "9f12dcd29238" INVALID_PARAMETER (Just valueNotOfType)
      , FailItem "d" "8f8e15b09bf4" INVALID_PARAMETER (Just columnValuesOnByteTable)
      , FailItem "e" "3b9b3a224e69" NOT_AUTHORIZED (Just columnNotGranted)
      ]
  }

-- | Fails a: a random UID that names no byte table, no table and no object
--   of the Locking SP gets NOT_AUTHORIZED, whatever the Where and Values.
absentTarget :: Property
absentTarget = forAll absent $ \u -> forAll arbitrary $ \wh -> forAll arbitrary $ \vs -> setAsAdmin1 u wh vs `fails` NOT_AUTHORIZED
  where
    (_, st) = asAdmin1
    absent  = arbitrary `suchThat` \u -> isNothing (Map.lookup uSP_Locking (tsSPs st) >>= \sps -> target sps u)

-- | Fails b: column 0 of User1 or of the Global Range, with any value,
--   among columns Admin1 is granted or alone, in any order, gets
--   INVALID_PARAMETER.
uidColumn :: Property
uidColumn = forAll (elements [(LockingSP.uAuthority_User1, [5]), (uLocking_GlobalRange, [5 .. 8])]) $ \(u, others) ->
  forAll (arbitrary >>= \x -> sublistOf others >>= \cs -> traverse (\c -> (,) c <$> booleanValue) cs >>= shuffle . ((0, x) :)) $ \pairs ->
    setAsAdmin1 u Nothing (Just (rowValuesOf pairs)) `fails` INVALID_PARAMETER

-- | Fails c: a boolean column Admin1 is granted set to a uinteger 2 to 255
--   or to bytes, or the Global Range's LockOnReset set to {32} or to {0, 0},
--   gets INVALID_PARAMETER.
valueNotOfType :: Property
valueNotOfType = forAll notOfType $ \(u, c, x) -> setAsAdmin1 u Nothing (Just (rowValuesOf [(c, x)])) `fails` INVALID_PARAMETER
  where
    notOfType = oneof
      [ (,,) LockingSP.uAuthority_User1 5 <$> notBoolean
      , (\c x -> (uLocking_GlobalRange, c, x)) <$> elements [5 .. 8] <*> notBoolean
      , (,,) uLocking_GlobalRange 9 <$> elements [uintegers [32], uintegers [0, 0]] ]
    notBoolean = oneof [ uinteger . fromIntegral <$> chooseInt (2, 255)
                       , D . Datum . Bytes . B.pack <$> (chooseInt (1, 4) >>= (`vectorOf` arbitrary)) ]

-- | Fails d: the DataStore, a byte table, with RowValues, a list of Named
--   column values, Where absent or a row of the table, gets
--   INVALID_PARAMETER.
columnValuesOnByteTable :: Property
columnValuesOnByteTable = forAll ((,) <$> whereRow <*> columnValues) $ \(wh, vs) -> setAsAdmin1 dataStore wh (Just vs) `fails` INVALID_PARAMETER
  where
    whereRow     = oneof [pure Nothing, Just . uinteger <$> natural 1023]
    columnValues = rowValuesOf <$> listOf ((,) <$> natural 20 <*> arbitrary)

-- | Fails e: User1's IsClass or PresentCertificate, booleans Admin1 is not
--   granted, set with or without its Enabled, which Admin1 is granted, in
--   any order, gets NOT_AUTHORIZED.
columnNotGranted :: Property
columnNotGranted = forAll notGranted $ \pairs -> setAsAdmin1 LockingSP.uAuthority_User1 Nothing (Just (rowValuesOf pairs)) `fails` NOT_AUTHORIZED
  where
    notGranted = do
      cs <- sublistOf [3, 8] `suchThat` (not . null)
      withEnabled <- sublistOf [5]
      traverse (\c -> (,) c <$> booleanValue) (cs <> withEnabled) >>= shuffle

-- | Set on an InvokingID with a Where and Values in the Admin1 session:
--   its reply and the TPer after it.
setAsAdmin1 :: UID -> Maybe Value -> Maybe Value -> (Reply, TPerState)
setAsAdmin1 u wh vs = behRun set invoked (Methods.Set wh vs) st
  where
    (ses, st) = asAdmin1
    invoked   = Invoked { ivComId = sesComId ses, ivSession = Just ses, ivInvoker = u }

-- | That a Set in the Admin1 session answered an empty result and a status,
--   and left the TPer as it was.
fails :: (Reply, TPerState) -> StatusCode -> Property
fails (reply, st') code = reply === ReplyResults [] code .&&. counterexample "the TPer changed" (st' == snd asAdmin1)

-- | The RowValues option: a list of Named values, each a column number and
--   a value (Core 5.3.3.7.2.2, PDF 202-203).
rowValuesOf :: [(Natural, Value)] -> Value
rowValuesOf pairs = L (List [ N (c .: x) | (c, x) <- pairs ])

-- | A uinteger value, and a list of them.
uinteger :: Natural -> Value
uinteger = D . Datum . Unsigned

uintegers :: [Natural] -> Value
uintegers = L . List . map uinteger

-- | A boolean's value, 0 or 1 (Core Table 59, PDF 103).
booleanValue :: Gen Value
booleanValue = uinteger <$> elements [0, 1]


-- | The Base Template's behaviours a session dispatches to: Get's, and
--   Set's since M5b.
behaviours :: [SomeBehaviour]
behaviours = [forMethod get, forMethod set]
