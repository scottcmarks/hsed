{-# LANGUAGE NoImplicitPrelude #-}

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

/At the M4a scaffold/ every function is a stub that group G1 writes (design
11).
-}
module Hsed.Sim.Access
  ( accessControlList
  , authenticated
  , aceGrant
  , grantedColumns
  ) where

import           RIO

import           Hsed.Core.Table (ColNum)
import           Hsed.Core.UID   (UID)
import           Hsed.Sim.Types  (SPState, Session)


-- | The ACL of an InvokingID and MethodID in an SP: the ACEs its
--   AccessControl row names, or 'Nothing' where there is no row (5.1.5.2,
--   PDF 137: NOT_AUTHORIZED).
accessControlList :: SPState -> UID -> UID -> Maybe [UID]
accessControlList = error "G1: Access.accessControlList"

-- | The authorities a session's authenticated ones make true: themselves
--   and every class a member of them belongs to (3.4.2.2, PDF 89).
authenticated :: SPState -> [UID] -> [UID]
authenticated = error "G1: Access.authenticated"

-- | What an ACE grants the authenticated authorities: its Columns where its
--   BooleanExpr holds (an empty list meaning every column), 'Nothing' where
--   it does not.
aceGrant :: SPState -> [UID] -> UID -> Maybe [ColNum]
aceGrant = error "G1: Access.aceGrant"

-- | The columns a session may Get of an object through a method: the union
--   of what the ACL's satisfied ACEs grant (an empty list every column), or
--   'Nothing' where no ACE is satisfied or there is no AccessControl row.
grantedColumns :: SPState -> Session -> UID -> UID -> Maybe [ColNum]
grantedColumns = error "G1: Access.grantedColumns"
