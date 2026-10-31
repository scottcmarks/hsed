{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Access
Description : Access control over an SP's tables: the AccessControl row of an invocation, its ACL's ACEs evaluated, the columns granted
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The access control of Core 3.4 and 5.3.4.3 over the rows an SP holds
("Hsed.Sim.State"; the Admin SP's hand-written until M4b, "Hsed.Sim.AdminSP"):
the AccessControl row of an InvokingID and MethodID and its ACL (5.3.2.9,
PDF 160-165); an ACE's BooleanExpr, a postfix list of authorities and
And/Or/Not (5.3.4.3.2-3, PDF 226-227; Table 61, PDF 104), in which an
authority is true when it is authenticated in the session (3.4.2.1, PDF 88),
Anybody always (5.3.4.1.2.1, PDF 211) and a class when a member is (3.4.2.2,
PDF 89), an empty expression false (5.3.4.3.3, PDF 227); the ACL satisfied
when any ACE is; and the columns the satisfied ACEs grant, their union, an
empty set meaning all (5.3.2.9.5, PDF 165).

The rows are read by the columns' names in the generated table descriptions
("Hsed.Gen.Core.TableDescs"): an AccessControl row's InvokingID, MethodID and
ACL, an ACE's BooleanExpr and Columns, an Authority's Class. A reference
cell holds the object's UID (a 'RefCell', or a 'UIDCell' where a row gives
the UID itself), an ACE's operators are 'EnumCell's of the generated
@boolean_ACE@ enumeration ("Hsed.Gen.Core.Enums": And 0, Or 1, Not 2), and
its Columns a 'ListCell' of column numbers.
-}
module Hsed.Sim.Access
  ( accessControlList
  , authenticated
  , aceGrant
  , grantedColumns
  ) where

import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import           RIO

import           Hsed.Core.Table          (Cell (..), ColNum, Row, TableDesc (..))
import           Hsed.Core.Types          (enumValue)
import           Hsed.Core.UID            (UID)
import           Hsed.Gen.Core.Enums      (Core_boolean_ACE (..))
import           Hsed.Gen.Core.TableDescs (aceTableDesc, accessControlTableDesc, authorityTableDesc)
import           Hsed.Gen.Core.UIDs       (uAuthority_Anybody)
import           Hsed.Sim.State           (cellOf, columnOf)
import           Hsed.Sim.Types           (SPState (..), Session (..))


-- | The ACL of an InvokingID and MethodID in an SP: the ACEs its
--   AccessControl row names, in order, or 'Nothing' where there is no row
--   for the combination (Core 5.1.5.2, PDF 137: NOT_AUTHORIZED), or the row
--   has no ACL that reads as a list of references. An empty ACL is 'Just'
--   @[]@: an InvokingID/MethodID combination whose ACL holds no ACE is not
--   invocable (5.3.4.3, PDF 226), which 'grantedColumns' answers.
accessControlList :: SPState -> UID -> UID -> Maybe [UID]
accessControlList sps invoker method =
  L.find isRow (rowsOf accessControlTableDesc sps) >>= cellNamed accessControlTableDesc "ACL" >>= references
  where
    isRow r = (cellNamed accessControlTableDesc "InvokingID" r >>= refOf) == Just invoker
           && (cellNamed accessControlTableDesc "MethodID" r >>= refOf) == Just method
    references = \case
      ListCell cs -> traverse refOf cs
      _           -> Nothing

-- | The authorities a session's authenticated ones make true: themselves,
--   every class a member of them belongs to and the class that class belongs
--   to, one level more (Core 3.4.2.2, PDF 89; 5.3.4.1.2, PDF 211: a class
--   authority is authenticated when any member is, and classes nest one
--   level), and Anybody, which is always authenticated within a session
--   (5.3.4.1.2.1, PDF 211). Each once, Anybody first, then as given.
--
-- >>> import qualified RIO.Map as Map
-- >>> import Hsed.Gen.Core.UIDs (uAuthority_Admins, uSP_Admin)
-- >>> import Hsed.OpalUIDs (uAuthority_Admin1)
-- >>> import Hsed.Sim.Types (TPerState (..), defaultSimParams, initialState)
-- >>> let Just adminSP = Map.lookup uSP_Admin (tsSPs (initialState defaultSimParams))
-- >>> authenticated adminSP [uAuthority_Admin1] == [uAuthority_Anybody, uAuthority_Admin1, uAuthority_Admins]
-- True
authenticated :: SPState -> [UID] -> [UID]
authenticated sps auths = L.nub (uAuthority_Anybody : auths <> classes <> mapMaybe classOf classes)
  where
    classes   = mapMaybe classOf auths
    classOf a = rowWith authorityTableDesc a sps >>= cellNamed authorityTableDesc "Class" >>= refOf

-- | What an ACE grants the authenticated authorities: its Columns where its
--   BooleanExpr holds (an empty list meaning every column, Core 5.3.2.9.5,
--   PDF 165), 'Nothing' where it does not, or where there is no such ACE.
--
--   The BooleanExpr is read in postfix (5.3.4.3.2, Table 210, PDF 226-227;
--   the AC_element's authorities and operators are a list of Named values in
--   the stream, a list of references and enumerations in the row): an
--   authority pushes whether it is among the authenticated (3.4.2.1, PDF
--   88), And and Or take two, Not takes one; the expression holds when
--   exactly one value is left and it is true. An empty expression is false
--   (5.3.4.3.3, PDF 227), as is one that does not evaluate (an operator
--   short of its operands, or values left over).
aceGrant :: SPState -> [UID] -> UID -> Maybe [ColNum]
aceGrant sps auths ace = do
  row  <- rowWith aceTableDesc ace sps
  expr <- cellNamed aceTableDesc "BooleanExpr" row >>= elements
  guard (holds expr)
  pure (maybe [] columns (cellNamed aceTableDesc "Columns" row))
  where
    elements = \case
      ListCell cs -> Just cs
      _           -> Nothing
    holds expr = foldM step [] expr == Just [True]
    -- the stack of values after one element, or Nothing where the expression does not evaluate
    step stack = \case
      RefCell _ a  -> Just ((a `elem` auths) : stack)
      UIDCell a    -> Just ((a `elem` auths) : stack)
      EnumCell _ v
        | v == enumValue Core_boolean_ACE_And -> binary (&&) stack
        | v == enumValue Core_boolean_ACE_Or  -> binary (||) stack
        | v == enumValue Core_boolean_ACE_Not -> case stack of
            b : rest -> Just (not b : rest)
            []       -> Nothing
      _ -> Nothing
    binary op = \case
      b : a : rest -> Just (op a b : rest)
      _            -> Nothing
    columns = \case
      ListCell cs -> [ fromInteger n | UIntCell n <- cs ]
      _           -> []

-- | The columns a session may Get of an object through a method: the union
--   of what the ACL's satisfied ACEs grant, in column order (an empty list
--   every column, where any satisfied ACE grants them all), or 'Nothing'
--   where no ACE is satisfied or there is no AccessControl row (Core 5.1.5.2,
--   PDF 137: NOT_AUTHORIZED). The authorities are the session's, made true
--   with their classes and Anybody ('authenticated').
grantedColumns :: SPState -> Session -> UID -> UID -> Maybe [ColNum]
grantedColumns sps ses invoker method = do
  acl <- accessControlList sps invoker method
  let grants = mapMaybe (aceGrant sps (authenticated sps (sesAuthorities ses))) acl
  guard (not (null grants))
  pure (if any null grants then [] else L.nub (L.sort (concat grants)))


-- * Rows and cells of an SP

-- | The rows an SP holds of a table, by the table's description.
rowsOf :: TableDesc -> SPState -> [Row]
rowsOf desc sps = fromMaybe [] (Map.lookup (tdTableUID desc) (spsTables sps))

-- | The row of a table whose UID column (0x00) holds a UID.
rowWith :: TableDesc -> UID -> SPState -> Maybe Row
rowWith desc u = L.find ((== Just (UIDCell u)) . cellOf 0) . rowsOf desc

-- | A row's cell in a column named in the table's description.
cellNamed :: TableDesc -> Text -> Row -> Maybe Cell
cellNamed desc name r = columnOf desc name >>= (`cellOf` r)

-- | The UID a reference cell holds: a reference printed as the object's
--   name, or the UID itself.
refOf :: Cell -> Maybe UID
refOf = \case
  RefCell _ u -> Just u
  UIDCell u   -> Just u
  _           -> Nothing
