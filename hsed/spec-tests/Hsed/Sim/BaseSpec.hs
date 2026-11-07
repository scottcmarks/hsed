{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Sim.BaseSpec
Description : Test specifications for Hsed.Sim.Base: Get on the Admin SP's objects and the Locking SP's byte tables, and Set on the Locking SP's objects, through a session's dispatch, their cell_block, Where and Values, their ACLs and their Fails lists
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Get as M4a runs it (design 8.2; Core 5.3.3.6, PDF 201), each invocation
dispatched in an Admin SP session ("Hsed.Sim.Session".@dispatch@): UCT-03
step 1b's Get on C_PIN_MSID's PIN column as Anybody, the columns the ACE
grants where none are asked (Opal 2.30 Table 23, PDF 47), C_PIN_SID's
columns as SID and none as Anybody (5.1.5.2, PDF 137); an object the SP
lacks NOT_AUTHORIZED (Fails a); a cell_block the context forbids or out of
bounds INVALID_PARAMETER (5.1.4.2.3, PDF 129-130; Fails b-d; 5.1.5.11, PDF
139), as is a parameter that is no cell_block; a host status echoed (3.2.4.2,
PDF 49); and the Fails list's four properties present (run by BehaviourSpec).

M5b's group G1 (design 11 M5b), in a Locking SP session as Admin1 with the
SP Manufactured, the shared fold ("Hsed.Sim.Gen".@foldedAt@): Get on the
DataStore, a byte table, gives its rows as one Bytes value, the lowest
first (Core 5.3.3.6.2.1, PDF 201; SPF-02 case 2's 1024 rows, Test Cases PDF
53), an extent written read over the fill, a cell_block out of bounds or
with a column component INVALID_PARAMETER (Fails c-d), the whole MBR, or
any table past the host's ComPacket, RESPONSE_OVERFLOW before a byte is
built (Opal 2.30 3.3.1, PDF 29), and as Anybody NOT_AUTHORIZED at the gate.
Set (5.3.3.7, PDF 201-203), the minimal object Set: User1's Enabled set
and read back (SPF-11, PDF 64), the Global Range's locking columns and
LockOnReset set (SPF-12 case 1, PDF 65) within Opal 4.3.5.2.2's sets (PDF
88), TPerInfo's ProgrammaticResetEnable set by SID and not by Anybody
(Opal Table 23, PDF 47); and each refusal in its place: a column not
granted NOT_AUTHORIZED for the whole method (5.3.4.2.6, PDF 225; Fails e),
the UID column (Fails b), a column twice (5.3.4.2.6), a value not of the
type (Fails c), Where on an object (5.3.3.7.1.1, PDF 202) INVALID_PARAMETER,
Values absent SUCCESS with no effect (5.3.3.7.2), Table.Set with Where the
same as Object.Set, a type the simulator does not carry out and Bytes on a
byte table FAIL (Scott's answer Q3; 5.1.5.16, PDF 139); and three
properties over the shared fold: a successful Set changes exactly the named
cells of one row, a Set answered other than SUCCESS changes nothing
(3.3.7.2, PDF 77), and Get after Set returns the value set, for every
boolean column Admin1 may Set and the reset_types columns.
-}
module Hsed.Sim.BaseSpec (spec) where

import qualified Data.IntMap.Strict       as IntMap
import qualified RIO.ByteString           as B
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck          (Gen, arbitrary, chooseInt, counterexample, elements, forAll, listOf, oneof, shuffle,
                                           sublistOf, (.&&.), (===), (==>))

import           Hsed.Core.Call           (MethodCall (..), MethodResponse (..), Parameters (..))
import           Hsed.Core.CellBlock      (CellBlock (..), RowAddress (..), cellBlockValue, columnRange, noCells)
import           Hsed.Core.Method         (invocation)
import           Hsed.Core.Table          (Cell (..), Col (..), Row, TableDesc (..))
import           Hsed.Core.Token          (IsToken (..), Token (..))
import           Hsed.Core.UID            (UID, hNull, uNull, uid, uidLower, (+:+))
import           Hsed.Core.Value          (Datum (..), List (..), Name (..), NamedValue (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes      (StatusCode (..), pattern FAIL, pattern INVALID_PARAMETER, pattern NOT_AUTHORIZED,
                                           pattern RESPONSE_OVERFLOW, pattern SUCCESS)
import qualified Hsed.Gen.Core.Methods    as Methods
import           Hsed.Gen.Core.TableDescs (accessControlTableDesc, authorityTableDesc, c_PINTableDesc, lockingTableDesc,
                                           mbrControlTableDesc)
import           Hsed.Gen.Core.TableUIDs  (uAccessControlTable, uAuthorityTable, uC_PINTable, uLockingTable, uMBRControlTable, uMBRTable,
                                           uTPerInfoTable)
import           Hsed.Gen.Core.UIDs       (uAuthority_Admins, uAuthority_Anybody, uAuthority_SID, uC_PIN_SID, uLocking_GlobalRange,
                                           uMBRControl_Row, uMethodID_Get, uMethodID_Set, uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uC_PIN_MSID, uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Catalogue       (renderCaseKey)
import qualified Hsed.Sim.Base            as Base
import           Hsed.Sim.Behaviour       (Behaviour (..), FailItem (..), Fails (..), someUID)
import           Hsed.Sim.Cells           (cellFor, putCell)
import           Hsed.Sim.Gen             (foldedAt, genResetColumns)
import           Hsed.Sim.Reset           (ResetKind (..), resetListed)
import           Hsed.Sim.Session         (dispatch)
import           Hsed.Sim.State           (byteTableSize, cellOf, columnOf, openSession, programmaticResetEnabled, rowWithUID, tableRows,
                                           updateRow)
import           Hsed.Sim.Types


base :: Word16
base = spBaseComId defaultSimParams

-- | The default TPer with an Admin SP session as Anybody (HSN 1) and one as
--   SID (HSN 2) open on its base ComID.
anybody, sid :: Session
stA, stS :: TPerState
(anybody, stA) = openSession base 1 uSP_Admin True [uAuthority_Anybody] (initialState defaultSimParams)
(sid, stS)     = openSession base 2 uSP_Admin True [uAuthority_Anybody, uAuthority_SID] stA

-- | The default TPer with its Locking SP Manufactured, the shared fold,
--   with a Locking SP session as Admin1 (HSN 1) open on its base ComID,
--   Admin1's authorities as the Session Manager gives them: with its class
--   Admins (Opal 2.30 Table 40, PDF 82; Core 3.4.2.2, PDF 89) and Anybody.
admin1 :: Session
stL :: TPerState
(admin1, stL) = admin1On (foldedAt Manufactured)

admin1On :: TPerState -> (Session, TPerState)
admin1On = openSession base 1 uSP_Locking True [uAuthority_Anybody, LockingSP.uAuthority_Admin1, uAuthority_Admins]

-- | The Locking SP Manufactured with a session as Anybody alone.
anybodyL :: Session
stAL :: TPerState
(anybodyL, stAL) = openSession base 1 uSP_Locking True [uAuthority_Anybody] (foldedAt Manufactured)

-- | Get on an InvokingID with a Cellblock value, dispatched in a session:
--   the response.
getOn :: Session -> TPerState -> UID -> Value -> MethodResponse
getOn ses st u v = fst (dispatch ses (invocation u (Methods.Get v)) st)

-- | Get with a cell_block.
getWith :: Session -> TPerState -> UID -> CellBlock -> MethodResponse
getWith ses st u = getOn ses st u . cellBlockValue

-- | Set on an InvokingID with a Where and Values, dispatched in a session:
--   the response and the TPer after it.
setOn :: Session -> TPerState -> UID -> Maybe Value -> Maybe Value -> (MethodResponse, TPerState)
setOn ses st u wh vs = dispatch ses (invocation u (Methods.Set wh vs)) st

-- | Set of these columns' values, no Where.
setCells :: Session -> TPerState -> UID -> [(Natural, Value)] -> (MethodResponse, TPerState)
setCells ses st u pairs = setOn ses st u Nothing (Just (rowValuesOf pairs))

-- | The column numbers a RowValues result names, in order.
columnsOf :: MethodResponse -> [Natural]
columnsOf r = case _results r of
  [L (List vs)] -> [ n | N (NamedValue (Name (Datum (Unsigned n))) _) <- vs ]
  _             -> []

-- | A status as the response carries it.
status :: StatusCode -> Natural
status = statusValue

-- | A response of no results and a status.
answered :: StatusCode -> MethodResponse
answered = MethodResponse [] . status

-- | The RowValues option: a list of Named values, each a column number and
--   a value (Core 5.3.3.7.2.2, PDF 202-203).
rowValuesOf :: [(Natural, Value)] -> Value
rowValuesOf pairs = L (List [ N (c .: x) | (c, x) <- pairs ])

-- | A uinteger value, a list of them, and a uid's.
uinteger :: Natural -> Value
uinteger = D . Datum . Unsigned

uintegers :: [Natural] -> Value
uintegers = L . List . map uinteger

uidValue :: UID -> Value
uidValue = D . Datum . token

-- | A cell_block of a byte table's rows from one to another (Core
--   5.1.4.2.3, PDF 129-130).
byteRows :: Natural -> Natural -> CellBlock
byteRows s e = noCells { cbStartRow = Just (RowNumber s), cbEndRow = Just e }

-- | The Locking SP's DataStore byte table, by the UID of the table it is
--   (Core 3.2.5.3 b.i, PDF 51-52; Opal 2.30 Table 36, PDF 54).
dataStore :: UID
dataStore = uidLower LockingSP.uTable_DataStore +:+ hNull

-- | The Admin SP's TPerInfo object, Opal 2.30 Table 27's one row (PDF 50).
tperInfo :: UID
tperInfo = uid 0x00 0x00 0x02 0x01 0x00 0x03 0x00 0x01

-- | A cell of a row of the Locking SP's table, by its column number.
lockingCell :: TPerState -> UID -> UID -> Int -> Maybe Cell
lockingCell st table u c = rowWithUID uSP_Locking table u st >>= cellOf c

-- | The TPer with the Locking SP's byte table changed.
withByteTable :: UID -> (ByteTable -> ByteTable) -> TPerState -> TPerState
withByteTable table f st = st { tsSPs = Map.adjust (\sps -> sps { spsBytes = Map.adjust f table (spsBytes sps) }) uSP_Locking (tsSPs st) }

-- | The TPer with the Locking SP's AccessControl table given a row for
--   TableUID.Set on the Authority table, the ACL User1's Set has (Opal 2.30
--   Table 38 gives the Authority table Next alone, PDF 65, and User1's Set
--   ACE_Authority_Set_Enabled and ACE_User1_Set_CommonName, PDF 66).
withAuthorityTableSet :: TPerState -> TPerState
withAuthorityTableSet st = st { tsSPs = Map.adjust (\sps -> sps { spsTables = Map.adjust (<> added) uAccessControlTable (spsTables sps) }) uSP_Locking (tsSPs st) }
  where
    added = [ putCell invokingID (UIDCell uAuthorityTable) r | r <- tableRows uSP_Locking uAccessControlTable st, user1Set r ]
    user1Set r = (cellOf invokingID r == Just (UIDCell LockingSP.uAuthority_User1)) && (refOf <$> cellOf methodID r) == Just (Just uMethodID_Set)
    invokingID = fromMaybe (-1) (columnOf accessControlTableDesc "InvokingID")
    methodID   = fromMaybe (-1) (columnOf accessControlTableDesc "MethodID")
    refOf = \case
      RefCell _ u -> Just u
      UIDCell u   -> Just u
      _           -> Nothing

msid :: ByteString
msid = spMsid defaultSimParams

pin :: Int
pin = fromMaybe (-1) (columnOf c_PINTableDesc "PIN")

-- | The columns Admin1 may Set of the three objects the M5b cases set, by
--   their tables and descriptions, with a generator of a value of each:
--   User1's Enabled (ACE_Authority_Set_Enabled, Opal 2.30 Table 39, PDF
--   79), the Global Range's ReadLockEnabled to LockOnReset
--   (ACE_Locking_GlblRng_Admins_Set, PDF 80) and MBRControl's Enable, Done
--   and MBRDoneOnReset (ACE_MBRControl_Admins_Set, PDF 81).
settable :: [(UID, UID, TableDesc, [(Natural, Gen Value)])]
settable =
  [ (LockingSP.uAuthority_User1, uAuthorityTable, authorityTableDesc, [(5, boolean)])
  , (uLocking_GlobalRange, uLockingTable, lockingTableDesc, [ (c, boolean) | c <- [5 .. 8] ] <> [(9, resetSet)])
  , (uMBRControl_Row, uMBRControlTable, mbrControlTableDesc, [(1, boolean), (2, boolean), (3, resetSet)])
  ]
  where
    boolean  = uinteger <$> elements [0, 1]
    resetSet = uintegers <$> elements [[0], [0, 3]]

-- | A Set Admin1 is granted of one of those objects: some of its columns,
--   each with a value of its type, in any order.
genGrantedSet :: Gen (UID, UID, TableDesc, [(Natural, Value)])
genGrantedSet = do
  (u, table, desc, columns) <- elements settable
  chosen <- sublistOf columns
  pairs  <- traverse (\(c, g) -> (,) c <$> g) chosen >>= shuffle
  pure (u, table, desc, pairs)

-- | A Set of any shape Admin1 might send: an InvokingID among the objects,
--   the tables, the DataStore and a random UID; a Where absent, a uid, a
--   uinteger or anything; Values absent, RowValues of random columns and
--   values of the types the simulator reads and others, bytes, or anything.
genAnySet :: Gen (UID, Maybe Value, Maybe Value)
genAnySet = (,,) <$> invoker <*> whereOf <*> values
  where
    invoker = oneof ([ pure u | (u, _, _, _) <- settable ] <> [pure dataStore, pure uAuthorityTable, pure uLockingTable, arbitrary])
    whereOf = oneof [pure Nothing, pure (Just (uidValue LockingSP.uAuthority_User1)), Just . uinteger <$> small, Just <$> arbitrary]
    values  = oneof [ pure Nothing, Just . rowValuesOf <$> listOf ((,) <$> small <*> value)
                    , Just . D . Datum . Bytes . B.pack <$> listOf arbitrary, Just <$> arbitrary ]
    value   = oneof [uinteger <$> elements [0, 1], uinteger <$> small, uintegers <$> sublistOf [0 .. 3], uintegers <$> listOf small, arbitrary]
    small   = fromIntegral <$> chooseInt (0, 12)

-- | The TPer with the Locking SP's reset columns varied
--   ("Hsed.Sim.Gen".'genResetColumns'), no fold made.
genLockingVaried :: TPerState -> Gen TPerState
genLockingVaried st = case Map.lookup uSP_Locking (tsSPs st) of
  Nothing  -> pure st
  Just sps -> (\varied -> st { tsSPs = Map.insert uSP_Locking varied (tsSPs st) }) <$> genResetColumns sps

-- | The cells a Set of these columns' values puts, each read by 'cellFor'
--   against the description.
cellsOf :: TableDesc -> [(Natural, Value)] -> [(Int, Cell)]
cellsOf desc pairs = [ (colNumber col, cell) | (c, x) <- pairs, col <- tdColumns desc, toInteger (colNumber col) == toInteger c
                                             , Right cell <- [cellFor col x] ]

-- | A row with cells put.
putCells :: [(Int, Cell)] -> Row -> Row
putCells cells r = L.foldl' (flip (uncurry putCell)) r cells

spec :: Spec
spec = do
  describe "Get on the Admin SP's objects, dispatched in a session" $ do
    it "C_PIN_MSID with [3 = 3, 4 = 3], the PIN column found by name, as Anybody: [L (List [N (3 .: msid)])], SUCCESS (UCT-03 step 1b)" $ do
      pin `shouldBe` 3
      getWith anybody stA uC_PIN_MSID (columnRange 3 3) `shouldBe` MethodResponse [L (List [N ((3 :: Natural) .: msid)])] 0
      cellBlockValue (columnRange 3 3) `shouldBe` L (List [N ((3 :: Natural) .: (3 :: Natural)), N ((4 :: Natural) .: (3 :: Natural))])
    it "C_PIN_MSID with no components gives the columns the ACE grants that the row holds, 0 and 3, in column order (Opal Table 23; Core 5.3.3.6.2.2)" $ do
      let r = getWith anybody stA uC_PIN_MSID noCells
      columnsOf r `shouldBe` [0, 3]
      r `shouldBe` MethodResponse [L (List [N ((0 :: Natural) .: token uC_PIN_MSID), N ((3 :: Natural) .: msid)])] 0
    it "C_PIN_MSID asked for columns 0 to 2 gives the UID alone, the Name not granted and that no error (Core 5.3.4.2.2)" $
      getWith anybody stA uC_PIN_MSID (columnRange 0 2) `shouldBe` MethodResponse [L (List [N ((0 :: Natural) .: token uC_PIN_MSID)])] 0
    it "C_PIN_MSID asked for a column the ACE does not grant, 4, gives an empty RowValues list (the row holds it, its default since M4b)" $
      getWith anybody stA uC_PIN_MSID (columnRange 4 4) `shouldBe` MethodResponse [L (List [])] 0
    it "C_PIN_SID as Anybody is NOT_AUTHORIZED with an empty result (Core 5.1.5.2): the NOPIN ACE wants Admins or SID" $
      getWith anybody stA uC_PIN_SID noCells `shouldBe` answered NOT_AUTHORIZED
    it "C_PIN_SID as SID gives every NOPIN column, never 3: UID, CharSet (Null, Opal Table 25, PDF 48; its default, Core 5.3.4.1.1.1, PDF 210), TryLimit, Tries, Persistence" $ do
      let r = getWith sid stS uC_PIN_SID noCells
      _resultStatus r `shouldBe` 0
      columnsOf r `shouldBe` [0, 4, 5, 6, 7]
      [ v | N (NamedValue (Name (Datum (Unsigned 4))) v) <- concat [ vs | L (List vs) <- _results r ] ] `shouldBe` [D (Datum (token uNull))]
      columnsOf (getWith sid stS uC_PIN_SID (columnRange 3 3)) `shouldBe` []
      columnsOf (getWith sid stS uC_PIN_SID (columnRange 3 7)) `shouldBe` [4, 5, 6, 7]
    it "C_PIN_MSID as SID gives the UID and PIN too, Anybody being authenticated in every session" $
      columnsOf (getWith sid stS uC_PIN_MSID noCells) `shouldBe` [0, 3]
    it "an object the SP does not have, 00 00 08 01 AA BB CC DD, is NOT_AUTHORIZED (Fails a; Core 5.1.5.2: no AccessControl row)" $
      getWith anybody stA (uid 0x00 0x00 0x08 0x01 0xAA 0xBB 0xCC 0xDD) noCells `shouldBe` answered NOT_AUTHORIZED
    it "the C_PIN table itself, with startRow C_PIN_MSID, is NOT_AUTHORIZED: Opal 2.30 Table 22 gives the C_PIN table only Next (PDF 42)" $
      getWith anybody stA uC_PINTable noCells { cbStartRow = Just (RowUID uC_PIN_MSID) } `shouldBe` answered NOT_AUTHORIZED
    it "leaves the TPer as it was" $
      snd (dispatch anybody (invocation uC_PIN_MSID (Methods.Get (cellBlockValue noCells))) stA) `shouldBe` stA

  describe "a cell_block the context forbids or out of bounds (Core 5.1.4.2.3; Fails b-d; 5.1.5.11 e)" $ do
    let invalid = answered INVALID_PARAMETER
    it "Table on an object (a.b)" $
      getWith anybody stA uC_PIN_MSID noCells { cbTable = Just uC_PINTable } `shouldBe` invalid
    it "startRow on an object (b.c), a uid or a row number" $ do
      getWith anybody stA uC_PIN_MSID noCells { cbStartRow = Just (RowUID uC_PIN_MSID) } `shouldBe` invalid
      getWith anybody stA uC_PIN_MSID noCells { cbStartRow = Just (RowNumber 0) } `shouldBe` invalid
    it "endRow on an object (c.b)" $
      getWith anybody stA uC_PIN_MSID noCells { cbEndRow = Just 0 } `shouldBe` invalid
    it "endColumn 8 on C_PIN, whose last column is 7 (d)" $ do
      getWith anybody stA uC_PIN_MSID noCells { cbEndColumn = Just 8 } `shouldBe` invalid
      getWith anybody stA uC_PIN_MSID (columnRange 8 8) `shouldBe` invalid
    it "startColumn past endColumn (d)" $
      getWith anybody stA uC_PIN_MSID (columnRange 4 3) `shouldBe` invalid
    it "components named out of order, [4, 3] (Core 5.1.4.1)" $
      getOn anybody stA uC_PIN_MSID (L (List [N ((4 :: Natural) .: (3 :: Natural)), N ((3 :: Natural) .: (3 :: Natural))])) `shouldBe` invalid
    it "a Cellblock that is no cell_block: a uinteger, a list of a bare value, a sixth component (Core 5.1.5.11 d)" $ do
      getOn anybody stA uC_PIN_MSID (D (Datum (Unsigned 3))) `shouldBe` invalid
      getOn anybody stA uC_PIN_MSID (L (List [D (Datum (Unsigned 3))])) `shouldBe` invalid
      getOn anybody stA uC_PIN_MSID (L (List [N ((5 :: Natural) .: (3 :: Natural))])) `shouldBe` invalid
    it "parameters that are not Get's: none, or two (Core 3.2.4.1)" $ do
      let call = invocation uC_PIN_MSID (Methods.Get (cellBlockValue noCells))
      fst (dispatch anybody call { _parameters = Parameters [] [] } stA) `shouldBe` invalid
      fst (dispatch anybody call { _parameters = Parameters [cellBlockValue noCells, cellBlockValue noCells] [] } stA) `shouldBe` invalid

  describe "a host status other than 0" $
    it "is echoed with an empty result, the method aborted (Core 3.2.4.2)" $
      forM_ [1, 0x0c, 0x3f] $ \s -> do
        let call = (invocation uC_PIN_MSID (Methods.Get (cellBlockValue (columnRange 3 3)))) { _statusCode = s }
        dispatch anybody call stA `shouldBe` (MethodResponse [] s, stA)

  describe "Get on the Locking SP's byte tables as Admin1, the SP Manufactured (M5b; Core 5.3.3.6.2.1, PDF 201)" $ do
    it "the DataStore's rows 0 to 1023 give one Bytes value of 1024 zero bytes, SUCCESS (SPF-02 case 2, Test Cases PDF 53; the DataStore's contents unspecified until written, Opal 4.3.8.1, PDF 90)" $ do
      byteTableSize uSP_Locking dataStore stL `shouldBe` Just 0xA00000
      getWith admin1 stL dataStore (byteRows 0 1023) `shouldBe` MethodResponse [D (Datum (Bytes (B.replicate 1024 0)))] 0
    it "reads an extent written over the fill, the lowest row first (Hsed.Sim.Bytes)" $ do
      let st = withByteTable dataStore (\bt -> bt { btWritten = IntMap.fromList [(1020, "ABCDEFGH")] }) stL
      getWith admin1 st dataStore (byteRows 1016 1027) `shouldBe` MethodResponse [D (Datum (Bytes ("\0\0\0\0ABCDEFGH")))] 0
      getWith admin1 st dataStore (byteRows 1022 1022) `shouldBe` MethodResponse [D (Datum (Bytes "C"))] 0
    it "startRow omitted is the first row and endRow the last (Core 5.1.4.2.3 b.a, c.a): rows 0 to 7 of a DataStore cut to eight" $ do
      let st = withByteTable dataStore (\bt -> bt { btSize = 8, btWritten = IntMap.fromList [(6, "xy")] }) stL
      getWith admin1 st dataStore noCells `shouldBe` MethodResponse [D (Datum (Bytes "\0\0\0\0\0\0xy"))] 0
      getWith admin1 st dataStore noCells { cbStartRow = Just (RowNumber 5) } `shouldBe` MethodResponse [D (Datum (Bytes "\0xy"))] 0
    it "endRow past the last row, or startRow past endRow, is INVALID_PARAMETER (Fails d; 5.1.5.11 e)" $ do
      getWith admin1 stL dataStore (byteRows 0 0xA00000) `shouldBe` answered INVALID_PARAMETER
      getWith admin1 stL dataStore (byteRows 1024 1023) `shouldBe` answered INVALID_PARAMETER
      getWith admin1 stL dataStore noCells { cbStartRow = Just (RowNumber 0xA00000) } `shouldBe` answered INVALID_PARAMETER
    it "a startColumn or an endColumn is INVALID_PARAMETER (Fails c; 5.1.4.2.3 d.b, e.b), as is a startRow that is a uid, or a Table" $ do
      getWith admin1 stL dataStore (byteRows 0 1023) { cbStartColumn = Just 0 } `shouldBe` answered INVALID_PARAMETER
      getWith admin1 stL dataStore (byteRows 0 1023) { cbEndColumn = Just 0 } `shouldBe` answered INVALID_PARAMETER
      getWith admin1 stL dataStore noCells { cbStartRow = Just (RowUID uC_PIN_MSID) } `shouldBe` answered INVALID_PARAMETER
      getWith admin1 stL dataStore noCells { cbTable = Just dataStore } `shouldBe` answered INVALID_PARAMETER
    it "a byte table of no rows has none to read: INVALID_PARAMETER" $
      getWith admin1 (withByteTable dataStore (\bt -> bt { btSize = 0 }) stL) dataStore noCells `shouldBe` answered INVALID_PARAMETER
    it "the whole MBR, 128 MiB (Opal Table 36, PDF 54), is RESPONSE_OVERFLOW with an empty result at once (Opal 3.3.1, PDF 29): more rows than the host's ComPacket of 2048 holds (Table 17, PDF 34)" $ do
      byteTableSize uSP_Locking uMBRTable stL `shouldBe` Just 0x08000000
      Map.lookup maxComPacketSizeName (sesHostProps admin1) `shouldBe` Just 2048
      getWith admin1 stL uMBRTable noCells `shouldBe` answered RESPONSE_OVERFLOW
      getWith admin1 stL dataStore noCells `shouldBe` answered RESPONSE_OVERFLOW
      getWith admin1 stL dataStore (byteRows 0 2048) `shouldBe` answered RESPONSE_OVERFLOW
    it "answers RESPONSE_OVERFLOW before building a byte: a table of 2^40 rows, which no process could build, is answered the same" $
      getWith admin1 (withByteTable dataStore (\bt -> bt { btSize = 2 ^ (40 :: Int) }) stL) dataStore noCells `shouldBe` answered RESPONSE_OVERFLOW
    it "the DataStore as Anybody alone is NOT_AUTHORIZED with an empty result at the gate (ACE_DataStore_Get_All wants Admins, Opal Table 39, PDF 81; Core 5.3.4.2.2, PDF 223)" $
      getWith anybodyL stAL dataStore (byteRows 0 1023) `shouldBe` answered NOT_AUTHORIZED
    it "the MBR's rows 0 to 7 as Anybody alone give eight zero bytes, its Get ACE_Anybody's (Opal Table 38, PDF 71; its contents VU, 4.3.5.4, PDF 89), and the whole MBR RESPONSE_OVERFLOW" $ do
      getWith anybodyL stAL uMBRTable (byteRows 0 7) `shouldBe` MethodResponse [D (Datum (Bytes (B.replicate 8 0)))] 0
      getWith anybodyL stAL uMBRTable noCells `shouldBe` answered RESPONSE_OVERFLOW
    it "leaves the TPer as it was" $
      snd (dispatch admin1 (invocation dataStore (Methods.Get (cellBlockValue (byteRows 0 1023)))) stL) `shouldBe` stL

  describe "Set on the Locking SP's objects as Admin1, the SP Manufactured (M5b; Core 5.3.3.7, PDF 201-203)" $ do
    let user1  = LockingSP.uAuthority_User1
        global = uLocking_GlobalRange
        unchanged (r, st') code = (r, st' == stL) `shouldBe` (answered code, True)
    it "User1's Enabled set to 1 gives SUCCESS, an empty result (5.3.3.7.3, PDF 203), and Get then reads 1 (SPF-11 steps 2-3, Test Cases PDF 64)" $ do
      lockingCell stL uAuthorityTable user1 5 `shouldBe` Just (EnumCell "F" 0)
      let (r, st') = setCells admin1 stL user1 [(5, uinteger 1)]
      r `shouldBe` MethodResponse [] 0
      lockingCell st' uAuthorityTable user1 5 `shouldBe` Just (EnumCell "True" 1)
      getWith admin1 st' user1 (columnRange 5 5) `shouldBe` MethodResponse [L (List [N ((5 :: Natural) .: (1 :: Natural))])] 0
      st' `shouldBe` updateRow uSP_Locking uAuthorityTable user1 (putCell 5 (EnumCell "True" 1)) stL
    it "User1's IsClass with Enabled, {3, 5}, a column Admin1 is not granted (Opal Table 38, PDF 66: Enabled and CommonName), is NOT_AUTHORIZED for the whole method (5.3.4.2.6, PDF 225; Fails e), the TPer unchanged" $ do
      unchanged (setCells admin1 stL user1 [(3, uinteger 1), (5, uinteger 1)]) NOT_AUTHORIZED
      unchanged (setCells admin1 stL user1 [(3, uinteger 0)]) NOT_AUTHORIZED
    it "User1's CommonName with Enabled, {2, 5}, both granted, is FAIL: a name is a type the simulator's Set does not carry out until M6 (5.1.5.16, PDF 139; Scott's answer Q3), the TPer unchanged" $
      unchanged (setCells admin1 stL user1 [(2, D (Datum (Bytes "a name"))), (5, uinteger 1)]) FAIL
    it "Enabled twice, {5, 5}, is INVALID_PARAMETER (5.3.4.2.6, PDF 225)" $
      unchanged (setCells admin1 stL user1 [(5, uinteger 1), (5, uinteger 1)]) INVALID_PARAMETER
    it "the UID column, {0}, is INVALID_PARAMETER (Fails b, PDF 203), before the grant: alone, or with Enabled" $ do
      unchanged (setCells admin1 stL user1 [(0, uidValue user1)]) INVALID_PARAMETER
      unchanged (setCells admin1 stL user1 [(5, uinteger 1), (0, uidValue user1)]) INVALID_PARAMETER
    it "Enabled set to 2, or to bytes, is INVALID_PARAMETER (Fails c; 5.1.5.11 b, PDF 139)" $ do
      unchanged (setCells admin1 stL user1 [(5, uinteger 2)]) INVALID_PARAMETER
      unchanged (setCells admin1 stL user1 [(5, D (Datum (Bytes "\1")))]) INVALID_PARAMETER
    it "a column the Authority table lacks, 0x20, is INVALID_PARAMETER (5.1.5.11, PDF 139)" $
      unchanged (setCells admin1 stL user1 [(0x20, uinteger 1)]) INVALID_PARAMETER
    it "Values absent gives SUCCESS with the TPer unchanged (5.3.3.7.2, PDF 202), as does an empty RowValues list" $ do
      setOn admin1 stL user1 Nothing Nothing `shouldBe` (MethodResponse [] 0, stL)
      setCells admin1 stL user1 [] `shouldBe` (MethodResponse [] 0, stL)
    it "Values that are no RowValues list, a uinteger, bytes or a list of bare values, are INVALID_PARAMETER (5.3.3.7.2.2, PDF 202-203; 5.1.5.11 d)" $ do
      unchanged (setOn admin1 stL user1 Nothing (Just (uinteger 1))) INVALID_PARAMETER
      unchanged (setOn admin1 stL user1 Nothing (Just (D (Datum (Bytes "\1"))))) INVALID_PARAMETER
      unchanged (setOn admin1 stL user1 Nothing (Just (uintegers [5, 1]))) INVALID_PARAMETER
      unchanged (setOn admin1 stL user1 Nothing (Just (L (List [N (("Enabled" :: ByteString) .: (1 :: Natural))])))) INVALID_PARAMETER
    it "Where given on an object is INVALID_PARAMETER (5.3.3.7.1.1, PDF 202), a uid or a row number" $ do
      unchanged (setOn admin1 stL user1 (Just (uidValue user1)) (Just (rowValuesOf [(5, uinteger 1)]))) INVALID_PARAMETER
      unchanged (setOn admin1 stL user1 (Just (uinteger 0)) (Just (rowValuesOf [(5, uinteger 1)]))) INVALID_PARAMETER
    it "parameters that are not Set's, a positional one, are INVALID_PARAMETER (Core 3.2.4.1)" $ do
      let call = invocation user1 (Methods.Set Nothing (Just (rowValuesOf [(5, uinteger 1)])))
      unchanged (dispatch admin1 call { _parameters = Parameters [rowValuesOf [(5, uinteger 1)]] [] } stL) INVALID_PARAMETER
    it "TableUID.Set on the Authority table with Where = User1's uid equals ObjectUID.Set on User1, given an AccessControl row for it (Opal Table 38 gives the Authority table Next alone, PDF 65)" $ do
      let st = withAuthorityTableSet stL
          values = Just (rowValuesOf [(5, uinteger 1)])
          (rTable, stTable)   = setOn admin1 st uAuthorityTable (Just (uidValue user1)) values
          (rObject, stObject) = setOn admin1 st user1 Nothing values
      rTable `shouldBe` MethodResponse [] 0
      rTable `shouldBe` rObject
      stTable `shouldBe` stObject
      lockingCell stTable uAuthorityTable user1 5 `shouldBe` Just (EnumCell "True" 1)
      setOn admin1 stL uAuthorityTable (Just (uidValue user1)) values `shouldBe` (answered NOT_AUTHORIZED, stL)
    it "TableUID.Set with Where absent or a row number is INVALID_PARAMETER (5.3.3.7.1.1), with Where a uid of no row NOT_AUTHORIZED (Fails a)" $ do
      let st = withAuthorityTableSet stL
          values = Just (rowValuesOf [(5, uinteger 1)])
      setOn admin1 st uAuthorityTable Nothing values `shouldBe` (answered INVALID_PARAMETER, st)
      setOn admin1 st uAuthorityTable (Just (uinteger 1)) values `shouldBe` (answered INVALID_PARAMETER, st)
      setOn admin1 st uAuthorityTable (Just (uidValue (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0xFF))) values `shouldBe` (answered NOT_AUTHORIZED, st)
    it "the Global Range {5: 1, 6: 1, 7: 0, 8: 0, 9: [0, 3]} gives SUCCESS (SPF-12 case 1 step 2, Test Cases PDF 65), and resetListed OnTPerReset holds of the new LockOnReset" $ do
      let (r, st') = setCells admin1 stL global [(5, uinteger 1), (6, uinteger 1), (7, uinteger 0), (8, uinteger 0), (9, uintegers [0, 3])]
      r `shouldBe` MethodResponse [] 0
      map (lockingCell st' uLockingTable global) [5 .. 9]
        `shouldBe` [ Just (EnumCell "True" 1), Just (EnumCell "True" 1), Just (EnumCell "False" 0), Just (EnumCell "False" 0)
                   , Just (ListCell [EnumCell "Power Cycle" 0, EnumCell "Programmatic" 3]) ]
      fmap (resetListed OnTPerReset) (lockingCell st' uLockingTable global 9) `shouldBe` Just True
      fmap (resetListed OnTPerReset) (lockingCell stL uLockingTable global 9) `shouldBe` Just False
      getWith admin1 st' global (columnRange 9 9) `shouldBe` MethodResponse [L (List [N ((9 :: Natural) .: uintegers [0, 3])])] 0
    it "LockOnReset {0, 2} is INVALID_PARAMETER (Opal 4.3.5.2.2, PDF 88), as are {} and {3}; {0, 1} is INVALID_PARAMETER at the defaults and SUCCESS with HW Reset for LOR/DOR (Opal 3.1.1.3, PDF 21), and so is {0, 1, 3}" $ do
      unchanged (setCells admin1 stL global [(9, uintegers [0, 2])]) INVALID_PARAMETER
      unchanged (setCells admin1 stL global [(9, uintegers [])]) INVALID_PARAMETER
      unchanged (setCells admin1 stL global [(9, uintegers [3])]) INVALID_PARAMETER
      unchanged (setCells admin1 stL global [(9, uintegers [0, 1])]) INVALID_PARAMETER
      unchanged (setCells admin1 stL global [(9, uintegers [1, 0, 3])]) INVALID_PARAMETER
      let hw = stL { tsParams = (tsParams stL) { spHwResetLorDor = True } }
      fst (setCells admin1 hw global [(9, uintegers [0, 1])]) `shouldBe` MethodResponse [] 0
      fst (setCells admin1 hw global [(9, uintegers [1, 0, 3])]) `shouldBe` MethodResponse [] 0
      fst (setCells admin1 hw global [(9, uintegers [0, 2])]) `shouldBe` answered INVALID_PARAMETER
    it "LockOnReset {32} and {0, 0} are INVALID_PARAMETER (Fails c: not a set over 0 to 31), checked before the supported sets" $ do
      unchanged (setCells admin1 stL global [(9, uintegers [32])]) INVALID_PARAMETER
      unchanged (setCells admin1 stL global [(9, uintegers [0, 0])]) INVALID_PARAMETER
    it "a column the simulator does not carry out decides FAIL only when it is the sole fault, whatever the cells' order (Core 5.1.5.16 vs 5.1.5.11, PDF 139; Scott's answer Q3)" $ do
      -- CommonName (column 2) is granted but not carried out: a name is FAIL on its own
      unchanged (setCells admin1 stL global [(2, D (Datum (Bytes "a name")))]) FAIL
      -- a bad reset set on LockOnReset (column 9, check 11) outweighs CommonName's FAIL, in either order
      unchanged (setCells admin1 stL global [(2, D (Datum (Bytes "a name"))), (9, uintegers [0, 2])]) INVALID_PARAMETER
      unchanged (setCells admin1 stL global [(9, uintegers [0, 2]), (2, D (Datum (Bytes "a name")))]) INVALID_PARAMETER
      -- a non-boolean on ReadLockEnabled (column 5, check 10) outweighs CommonName's FAIL, in either order
      unchanged (setCells admin1 stL global [(2, D (Datum (Bytes "a name"))), (5, uinteger 2)]) INVALID_PARAMETER
      unchanged (setCells admin1 stL global [(5, uinteger 2), (2, D (Datum (Bytes "a name")))]) INVALID_PARAMETER
    it "MBRControl's Enable, Done and MBRDoneOnReset {1: 1, 2: 1, 3: [0, 3]} give SUCCESS (ACE_MBRControl_Admins_Set, Opal Table 39, PDF 81), and MBRDoneOnReset {0, 2} INVALID_PARAMETER (Opal 4.3.5.3.1, PDF 88-89)" $ do
      let (r, st') = setCells admin1 stL uMBRControl_Row [(1, uinteger 1), (2, uinteger 1), (3, uintegers [0, 3])]
      r `shouldBe` MethodResponse [] 0
      map (lockingCell st' uMBRControlTable uMBRControl_Row) [1 .. 3]
        `shouldBe` [Just (EnumCell "True" 1), Just (EnumCell "True" 1), Just (ListCell [EnumCell "Power Cycle" 0, EnumCell "Programmatic" 3])]
      unchanged (setCells admin1 stL uMBRControl_Row [(3, uintegers [0, 2])]) INVALID_PARAMETER
    it "the DataStore with Bytes is FAIL, the simulator's gap until M6 (Scott's answer Q3), Where absent or a row; with RowValues INVALID_PARAMETER (Fails d); with Values absent SUCCESS and no effect (5.3.3.7.2)" $ do
      unchanged (setOn admin1 stL dataStore Nothing (Just (D (Datum (Bytes "abc"))))) FAIL
      unchanged (setOn admin1 stL dataStore (Just (uinteger 1023)) (Just (D (Datum (Bytes "abc"))))) FAIL
      unchanged (setOn admin1 stL dataStore Nothing (Just (rowValuesOf [(0, uinteger 1)]))) INVALID_PARAMETER
      unchanged (setOn admin1 stL dataStore Nothing (Just (uinteger 1))) INVALID_PARAMETER
      setOn admin1 stL dataStore Nothing Nothing `shouldBe` (MethodResponse [] 0, stL)
      setOn admin1 stL dataStore (Just (uinteger 0)) Nothing `shouldBe` (MethodResponse [] 0, stL)
    it "the DataStore with Where a uid, or a row past the last, is INVALID_PARAMETER (5.3.3.7.1.2, PDF 202)" $ do
      unchanged (setOn admin1 stL dataStore (Just (uidValue user1)) (Just (D (Datum (Bytes "abc"))))) INVALID_PARAMETER
      unchanged (setOn admin1 stL dataStore (Just (uinteger 0xA00000)) Nothing) INVALID_PARAMETER
    it "an object the SP does not have is NOT_AUTHORIZED (Fails a; 5.1.5.2, PDF 137)" $
      unchanged (setCells admin1 stL (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0xFF) [(5, uinteger 1)]) NOT_AUTHORIZED
    it "User1's Enabled as Anybody alone is NOT_AUTHORIZED at the gate (ACE_Authority_Set_Enabled wants Admins)" $
      setCells anybodyL stAL user1 [(5, uinteger 1)] `shouldBe` (answered NOT_AUTHORIZED, stAL)
    it "a host status other than 0 is echoed with an empty result, the method aborted (Core 3.2.4.2)" $ do
      let call = (invocation user1 (Methods.Set Nothing (Just (rowValuesOf [(5, uinteger 1)])))) { _statusCode = 1 }
      dispatch admin1 call stL `shouldBe` (MethodResponse [] 1, stL)

  describe "Set on the Admin SP's TPerInfo, Opal 2.30 Table 27's row (PDF 50)" $ do
    it "is that row of the default TPer" $
      fmap (cellOf 0) (listToMaybe (tableRows uSP_Admin uTPerInfoTable stS)) `shouldBe` Just (Just (UIDCell tperInfo))
    it "as SID, {8: 1} gives SUCCESS and programmaticResetEnabled is Just True (SPF-12 case 1 step 1, Test Cases PDF 65; ACE_TPerInfo_Set_ProgrammaticResetEnable, Opal Table 23, PDF 47)" $ do
      programmaticResetEnabled stS `shouldBe` Just False
      let (r, st') = setCells sid stS tperInfo [(8, uinteger 1)]
      r `shouldBe` MethodResponse [] 0
      programmaticResetEnabled st' `shouldBe` Just True
      fst (setCells sid st' tperInfo [(8, uinteger 0)]) `shouldBe` MethodResponse [] 0
      programmaticResetEnabled (snd (setCells sid st' tperInfo [(8, uinteger 0)])) `shouldBe` Just False
    it "as Anybody it gives NOT_AUTHORIZED with an empty result (5.1.5.2, PDF 137), the TPer unchanged" $
      setCells anybody stA tperInfo [(8, uinteger 1)] `shouldBe` (answered NOT_AUTHORIZED, stA)
    it "as SID, a column the ACE does not grant, 5 ProtocolVersion, is NOT_AUTHORIZED (Fails e)" $
      setCells sid stS tperInfo [(5, uinteger 1), (8, uinteger 1)] `shouldBe` (answered NOT_AUTHORIZED, stS)

  describe "properties over the shared fold (Hsed.Sim.Gen.foldedAt), as Admin1 at manufactured" $ do
    prop "frame: a successful Set changes exactly the named cells of one row, to the cells cellFor gives (Core 3.3.7.2, PDF 77; 5.3.4.2.6, PDF 225)" $
      forAll genGrantedSet $ \(u, table, desc, pairs) -> forAll (genLockingVaried (foldedAt Manufactured)) $ \st0 ->
        let (ses, st)   = admin1On st0
            (r, st')    = setCells ses st u pairs
            expected    = updateRow uSP_Locking table u (putCells (cellsOf desc pairs)) st
        in r === MethodResponse [] 0 .&&. counterexample "the TPer is not the one with exactly those cells put" (st' == expected)
    prop "all or nothing: a Set answered other than SUCCESS leaves the TPer as it was, whatever it asked (Core 3.3.7.2, PDF 77)" $
      forAll genAnySet $ \(u, wh, vs) ->
        let (r, st') = setOn admin1 stL u wh vs
        in counterexample (show r) $ _resultStatus r `elem` map status [SUCCESS, NOT_AUTHORIZED, INVALID_PARAMETER, FAIL]
             .&&. (_resultStatus r /= 0 ==> counterexample "the TPer changed" (st' == stL))
    prop "round trip: Get after Set returns the value set, for every boolean column Admin1 may Set and for LockOnReset and MBRDoneOnReset" $
      forAll (elements settable >>= \(u, _, _, columns) -> elements columns >>= \(c, g) -> (,) (u, c) <$> g) $ \((u, c), x) ->
        let (r, st') = setCells admin1 stL u [(c, x)]
        in r === MethodResponse [] 0 .&&. getWith admin1 st' u (columnRange c c) === MethodResponse [L (List [N (c .: x)])] 0

  describe "the behaviours and their Fails lists" $ do
    it "are Get's, the one behaviour of the Base Template at M4a, and Set's since M5b" $
      map someUID Base.behaviours `shouldBe` [uMethodID_Get, uMethodID_Set]
    it "have a property for each of Get's items a to d and Set's a to e (run by BehaviourSpec and BehavioursSpec)" $ do
      map (isJust . fiProp) (failsItems Base.getFails) `shouldBe` [True, True, True, True]
      map (isJust . fiProp) (failsItems Base.setFails) `shouldBe` [True, True, True, True, True]
    it "name the cases that observe them: Get's UCT-03 steps 1a-1c, SPF-02 case 2, SPF-11, SPF-12 case 1 no-media and ETC-04, Set's SPF-11 and SPF-12 case 1 no-media (BehavioursSpec finds each in the catalogue)" $ do
      map renderCaseKey (behCases Base.get) `shouldBe` ["UCT-03:1a-1c", "SPF-02:2", "SPF-11", "SPF-12:1/no-media", "ETC-04"]
      map renderCaseKey (behCases Base.set) `shouldBe` ["SPF-11", "SPF-12:1/no-media"]
