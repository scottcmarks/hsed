{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Sim.BaseSpec
Description : Test specifications for Hsed.Sim.Base: Get on the Admin SP's objects through a session's dispatch, its cell_block, its ACL and its Fails list
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
-}
module Hsed.Sim.BaseSpec (spec) where

import           RIO
import           Test.Hspec

import           Hsed.Core.Call           (MethodCall (..), MethodResponse (..), Parameters (..))
import           Hsed.Core.CellBlock      (CellBlock (..), RowAddress (..), cellBlockValue, columnRange, noCells)
import           Hsed.Core.Method         (invocation)
import           Hsed.Core.Token          (IsToken (..), Token (..))
import           Hsed.Core.UID            (UID, uNull, uid)
import           Hsed.Core.Value          (Datum (..), List (..), Name (..), NamedValue (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes      (StatusCode (..), pattern INVALID_PARAMETER, pattern NOT_AUTHORIZED)
import qualified Hsed.Gen.Core.Methods    as Methods
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs  (uC_PINTable)
import           Hsed.Gen.Core.UIDs       (uAuthority_Anybody, uAuthority_SID, uC_PIN_SID, uMethodID_Get, uSP_Admin)
import           Hsed.OpalUIDs            (uC_PIN_MSID)
import qualified Hsed.Sim.Base            as Base
import           Hsed.Sim.Behaviour       (FailItem (..), Fails (..), someUID)
import           Hsed.Sim.Session         (dispatch)
import           Hsed.Sim.State           (columnOf, openSession)
import           Hsed.Sim.Types


base :: Word16
base = spBaseComId defaultSimParams

-- | The default TPer with an Admin SP session as Anybody (HSN 1) and one as
--   SID (HSN 2) open on its base ComID.
anybody, sid :: Session
stA, stS :: TPerState
(anybody, stA) = openSession base 1 uSP_Admin True [uAuthority_Anybody] (initialState defaultSimParams)
(sid, stS)     = openSession base 2 uSP_Admin True [uAuthority_Anybody, uAuthority_SID] stA

-- | Get on an InvokingID with a Cellblock value, dispatched in a session:
--   the response.
getOn :: Session -> TPerState -> UID -> Value -> MethodResponse
getOn ses st u v = fst (dispatch ses (invocation u (Methods.Get v)) st)

-- | Get with a cell_block.
getWith :: Session -> TPerState -> UID -> CellBlock -> MethodResponse
getWith ses st u = getOn ses st u . cellBlockValue

-- | The column numbers a RowValues result names, in order.
columnsOf :: MethodResponse -> [Natural]
columnsOf r = case _results r of
  [L (List vs)] -> [ n | N (NamedValue (Name (Datum (Unsigned n))) _) <- vs ]
  _             -> []

-- | A status as the response carries it.
status :: StatusCode -> Natural
status = statusValue

msid :: ByteString
msid = spMsid defaultSimParams

pin :: Int
pin = fromMaybe (-1) (columnOf c_PINTableDesc "PIN")

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
    it "C_PIN_MSID asked for a column it does not hold, 4, gives an empty RowValues list" $
      getWith anybody stA uC_PIN_MSID (columnRange 4 4) `shouldBe` MethodResponse [L (List [])] 0
    it "C_PIN_SID as Anybody is NOT_AUTHORIZED with an empty result (Core 5.1.5.2): the NOPIN ACE wants Admins or SID" $
      getWith anybody stA uC_PIN_SID noCells `shouldBe` MethodResponse [] (status NOT_AUTHORIZED)
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
      getWith anybody stA (uid 0x00 0x00 0x08 0x01 0xAA 0xBB 0xCC 0xDD) noCells `shouldBe` MethodResponse [] (status NOT_AUTHORIZED)
    it "the C_PIN table itself, with startRow C_PIN_MSID, is NOT_AUTHORIZED at M4a: the Admin SP has no AccessControl row for Get on a table" $
      getWith anybody stA uC_PINTable noCells { cbStartRow = Just (RowUID uC_PIN_MSID) } `shouldBe` MethodResponse [] (status NOT_AUTHORIZED)
    it "leaves the TPer as it was" $
      snd (dispatch anybody (invocation uC_PIN_MSID (Methods.Get (cellBlockValue noCells))) stA) `shouldBe` stA

  describe "a cell_block the context forbids or out of bounds (Core 5.1.4.2.3; Fails b-d; 5.1.5.11 e)" $ do
    let invalid = MethodResponse [] (status INVALID_PARAMETER)
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

  describe "the behaviour and its Fails list" $ do
    it "is Get's, the one behaviour of the Base Template at M4a" $
      map someUID Base.behaviours `shouldBe` [uMethodID_Get]
    it "has a property for each of items a to d (run by BehaviourSpec)" $
      map (isJust . fiProp) (failsItems Base.getFails) `shouldBe` [True, True, True, True]
