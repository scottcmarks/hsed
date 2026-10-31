{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Sim.Base
Description : The Base Template's methods the simulator runs in a session: Get, with its Fails list
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The behaviours of Core 5.3.3's Basic Table Method Group that M4a runs
(design 8.2, 13.16): 'get', bound by @behaviour "Base.get"@ on 5.3.3.6 (PDF
201), with its Fails list 5.3.3.6.3 bound by @fails "Base.getFails"@, one
'FailItem' per item a to d, each with its status and the QuickCheck property
that the simulator fails so. The cell_block is read and resolved by
"Hsed.Core.CellBlock" (5.1.4.2.3, PDF 129-130), the columns granted by
"Hsed.Sim.Access", and the RowValues come in column order (5.3.3.6.2.2, PDF
201), the cells the caller may not read left out (5.3.4.2.2, PDF 223).

What Get is invoked on is found among the tables the session's SP holds: an
object, the row of a table whose UID column holds the InvokingID, or an
object table, the InvokingID a table's UID (byte tables: none at M4a). The
descriptions of those tables, which give a target its last column, are the
generated ones of the Admin SP's hand-written rows ("Hsed.Sim.AdminSP",
until M4b's generated preconfiguration).
-}
module Hsed.Sim.Base
  ( get
  , getFails
  , behaviours
  ) where

import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import           RIO
import           Test.QuickCheck          (Gen, Property, arbitrary, chooseInt, counterexample, forAll, oneof, suchThat, (===))

import           Hsed.Core.CellBlock      (CellBlock (..), CellTarget (..), CellsAsked (..), RowAddress (..), cellBlock,
                                           cellBlockValue, columnRange, noCells, resolveCells)
import           Hsed.Core.Table          (Cell (..), Col (..), Row, TableDesc (..), cellValue)
import           Hsed.Core.UID            (UID)
import           Hsed.Core.Value          (List (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes      (pattern INVALID_PARAMETER, pattern NOT_AUTHORIZED, pattern SUCCESS)
import qualified Hsed.Gen.Core.Methods    as Methods
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.UIDs       (uAuthority_Anybody, uMethodID_Get, uSP_Admin)
import           Hsed.Jig.Catalogue       (CaseKey (..), TestId (..))
import           Hsed.OpalUIDs            (uC_PIN_MSID)
import           Hsed.Sim.Access          (grantedColumns)
import           Hsed.Sim.AdminSP         (handRows)
import           Hsed.Sim.Behaviour       (Behaviour (..), FailItem (..), Fails (..), Invoked (..), Reply (..), SomeBehaviour,
                                           forMethod)
import           Hsed.Sim.State           (cellOf, openSession)
import           Hsed.Sim.Types           (SPState (..), Session (..), SimParams (..), TPerState (..), defaultSimParams,
                                           initialState)


-- | Get on an object or a table (Core 5.3.3.6, PDF 201; @behaviour
--   "Base.get"@): the cells its cell_block asks, among those the session's
--   authorities may read, as RowValues in column order. UCT-03 step 1b reads
--   C_PIN_MSID's PIN with it, and ETC-04 sends it on a session the TPer does
--   not have.
--
--   The run, within a session (a call outside one, which the Session Manager
--   ignores, is NOT_AUTHORIZED):
--
--   * the InvokingID is an object or an object table of the session's SP
--     ('target'), else NOT_AUTHORIZED (Fails a, 5.3.3.6.3, PDF 201; the same
--     status the access control gate gives an InvokingID the AccessControl
--     table has no row for, 5.1.5.2, PDF 137);
--   * the columns the session may read of it through Get are what the ACL's
--     satisfied ACEs grant ("Hsed.Sim.Access".'grantedColumns'), else
--     NOT_AUTHORIZED, the gate's answer again;
--   * the cell_block is read ("Hsed.Core.CellBlock".'cellBlock') and
--     resolved against the target (its last column, 'resolveCells'): a
--     component the context forbids, or out of bounds, is INVALID_PARAMETER
--     (5.1.5.11 e, PDF 139; Fails b-d), as is a parameter that is no
--     cell_block (5.1.5.11 d); on a table, the object startRow names must
--     exist (Fails a);
--   * the result is one RowValues list (5.3.3.6.2.2): the columns asked,
--     among those granted (an empty grant meaning all, 5.3.2.9.5, PDF 165)
--     and those the row holds, in column order, each a Named value of the
--     column number and the cell's value ("Hsed.Core.Table".'cellValue');
--     the cells not granted are left out and that is no error (5.3.4.2.2,
--     PDF 223), so a row of none is @[L (List [])]@.
get :: Behaviour Methods.Get
get = Behaviour
  { behName  = "Base.get"
  , behHash  = "9d67d740e4cb"
  , behCases = [CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing, CaseKey (ETC 4) Nothing Nothing Nothing]
  , behRun   = \iv (Methods.Get cb) st -> (getReply iv cb st, st)
  }

-- | Get's reply, the TPer unchanged: the module's description of 'get'.
getReply :: Invoked -> Value -> TPerState -> Reply
getReply iv cb st = case ivSession iv of
  Nothing  -> notAuthorized
  Just ses -> fromMaybe notAuthorized $ do
    sps             <- Map.lookup (sesSP ses) (tsSPs st)
    (desc, found)   <- target (tsParams st) sps (ivInvoker iv)
    granted         <- grantedColumns sps ses (ivInvoker iv) uMethodID_Get
    pure (either (const (ReplyResults [] INVALID_PARAMETER)) id (cells desc found granted))
  where
    notAuthorized = ReplyResults [] NOT_AUTHORIZED
    -- the cell_block read and resolved against the target, then the row's cells asked and granted
    cells desc found granted = do
      block <- cellBlock cb
      asked <- resolveCells (targetOf desc found) block
      pure $ case (found, asked) of
        (Object row, Columns s e)      -> rowValues granted s e row
        (Table rows, ObjectRow u s e)  -> maybe notAuthorized (rowValues granted s e) (L.find ((== Just (UIDCell u)) . cellOf 0) rows)
        _                              -> ReplyResults [] INVALID_PARAMETER
    rowValues granted s e row =
      ReplyResults [L (List [ N ((fromIntegral c :: Natural) .: cellValue v) | (c, v) <- L.sortOn fst row
                                                                              , c >= s, c <= e, null granted || c `elem` granted ])] SUCCESS

-- | What Get is invoked on in an SP: a table of the SP, by its UID, with
--   its rows, or an object, by the UID column of its row, with the row;
--   each with its table's description.
data Found
  = Object !Row
  | Table ![Row]

-- | The table or object an InvokingID names among an SP's tables, if any,
--   and its description ('tableDescs').
target :: SimParams -> SPState -> UID -> Maybe (TableDesc, Found)
target p sps u = table <|> object
  where
    table  = (\desc -> (desc, Table (rowsOf desc))) <$> L.find (\desc -> tdTableUID desc == u && Map.member u (spsTables sps)) (tableDescs p)
    object = listToMaybe [ (desc, Object row) | desc <- tableDescs p, row <- rowsOf desc, cellOf 0 row == Just (UIDCell u) ]
    rowsOf desc = fromMaybe [] (Map.lookup (tdTableUID desc) (spsTables sps))

-- | The cell_block target a table or an object is: its last column (Core
--   5.1.4.2.3, PDF 129-130; a byte table is none of the SP's at M4a).
targetOf :: TableDesc -> Found -> CellTarget
targetOf desc = \case
  Object _ -> OnObject (lastColumn desc)
  Table _  -> OnObjectTable (lastColumn desc)

-- | The descriptions of the tables an SP holds at M4a: those of the Admin
--   SP's hand-written rows ("Hsed.Sim.AdminSP"; M4b: the generated
--   preconfiguration's), each once.
tableDescs :: SimParams -> [TableDesc]
tableDescs = L.nubBy ((==) `on` tdTableUID) . map fst . handRows

-- | A table's last column number, 0 for a table of one column.
lastColumn :: TableDesc -> Int
lastColumn = maybe 0 colNumber . L.lastMaybe . tdColumns


-- * The Fails list

-- | Get's Fails list (Core 5.3.3.6.3, PDF 201; @fails "Base.getFails"@): a,
--   the object does not exist, NOT_AUTHORIZED; b to d, a cell_block
--   component the context forbids or out of bounds (5.1.4.2.3's rules, PDF
--   129-130), INVALID_PARAMETER (5.1.5.11 e, PDF 139). Each item's property
--   runs 'get' in an Admin SP session as Anybody on the default TPer
--   ('asAnybody'), except c's, which holds the cell_block reader to a byte
--   table's rule, the simulator having no byte table at M4a:
--
--   * a: Get on a random UID that is no table and no object of the Admin SP
--     is NOT_AUTHORIZED;
--   * b: Get on C_PIN_MSID with a Table, a startRow or an endRow component
--     is INVALID_PARAMETER;
--   * c: a cell_block with a startColumn or an endColumn is refused on any
--     byte table ("Hsed.Core.CellBlock".'resolveCells'), which 'get' answers
--     INVALID_PARAMETER as it does every cell_block the reader refuses;
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
absentObject = forAll absent $ \u -> forAll anyBlock $ \block -> getAsAnybody u block === ReplyResults [] NOT_AUTHORIZED
  where
    (_, st)  = asAnybody
    absent   = arbitrary `suchThat` \u -> isNothing (Map.lookup uSP_Admin (tsSPs st) >>= \sps -> target (tsParams st) sps u)
    anyBlock = oneof [pure noCells, columnRange <$> small <*> small]
    small    = fromIntegral <$> chooseInt (0, 3)

-- | Fails b: a Table, startRow or endRow component on an object gets
--   INVALID_PARAMETER (Core 5.1.4.2.3 a.b, b.c, c.b).
rowComponentOnObject :: Property
rowComponentOnObject = forAll rowComponent $ \block -> getAsAnybody uC_PIN_MSID block === ReplyResults [] INVALID_PARAMETER
  where
    rowComponent = oneof
      [ (\u -> noCells { cbTable = Just u }) <$> arbitrary
      , (\r -> noCells { cbStartRow = Just r }) <$> oneof [RowUID <$> arbitrary, RowNumber <$> natural 100]
      , (\n -> noCells { cbEndRow = Just n }) <$> natural 100 ]

-- | Fails c: a startColumn or endColumn component on a byte table of any
--   size is refused by the cell_block reader (Core 5.1.4.2.3 d.b, e.b), and
--   what the reader refuses 'get' answers INVALID_PARAMETER (b and d).
columnComponentOnByteTable :: Property
columnComponentOnByteTable = forAll ((,) <$> natural 1000 <*> columnComponent) $ \(lastRow, block) ->
  counterexample (show (resolveCells (OnByteTable lastRow) block)) (isLeft (resolveCells (OnByteTable lastRow) block))
  where
    columnComponent = oneof [ (\n -> noCells { cbStartColumn = Just n }) <$> natural 20
                            , (\n -> noCells { cbEndColumn = Just n }) <$> natural 20 ]

-- | Fails d: a column past the table's last, or a startColumn past the
--   endColumn, on C_PIN_MSID gets INVALID_PARAMETER.
outOfBounds :: Property
outOfBounds = forAll beyond $ \block -> getAsAnybody uC_PIN_MSID block === ReplyResults [] INVALID_PARAMETER
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
--   ComID, HSN 1: what the properties run on.
asAnybody :: (Session, TPerState)
asAnybody = openSession (spBaseComId defaultSimParams) 1 uSP_Admin True [uAuthority_Anybody] (initialState defaultSimParams)

-- | Get on an InvokingID with a cell_block in that session: its reply.
getAsAnybody :: UID -> CellBlock -> Reply
getAsAnybody u block = fst (behRun get invoked (Methods.Get (cellBlockValue block)) st)
  where
    (ses, st) = asAnybody
    invoked   = Invoked { ivComId = sesComId ses, ivSession = Just ses, ivInvoker = u, ivStatus = 0 }


-- | The Base Template's behaviours a session dispatches to.
behaviours :: [SomeBehaviour]
behaviours = [forMethod get]
