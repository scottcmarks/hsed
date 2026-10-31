{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.AccessSpec
Description : Test specifications for Hsed.Sim.Access: the AccessControl row of an invocation, an ACE's postfix BooleanExpr, the columns an ACL grants
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The access control of M4a (design 8.2) over the Admin SP's hand-written rows
and over rows built here: the ACL of an InvokingID and MethodID (Core
5.3.2.9, PDF 160-165; none, NOT_AUTHORIZED, 5.1.5.2, PDF 137); the
authorities a session makes true, their classes with them and Anybody always
(3.4.2.1-2, PDF 88-89; 5.3.4.1.2.1, PDF 211); an ACE's BooleanExpr read in
postfix, And, Or and Not over authenticated and unauthenticated authorities
(5.3.4.3.2, Table 210, PDF 226-227), an empty expression false (5.3.4.3.3,
PDF 227); the ACL satisfied when any ACE is; and the columns granted, the
union of the satisfied ACEs' Columns, an empty set meaning all (5.3.2.9.5,
PDF 165).
-}
module Hsed.Sim.AccessSpec (spec) where

import qualified RIO.Map                  as Map
import           RIO
import           Test.Hspec

import           Hsed.Core.Table          (Cell (..), Col (..), Row, columnNamed)
import           Hsed.Core.Types          (enumValue)
import           Hsed.Core.UID            (UID, uid)
import           Hsed.Gen.Core.Enums      (Core_boolean_ACE (..))
import           Hsed.Gen.Core.TableDescs (aceTableDesc, accessControlTableDesc, c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs  (uACETable, uAccessControlTable, uC_PINTable)
import           Hsed.Gen.Core.UIDs       (uAuthority_Admins, uAuthority_Anybody, uAuthority_SID, uC_PIN_SID, uMethodID_Get,
                                           uMethodID_Next, uSP_Admin)
import           Hsed.OpalUIDs            (uACE_C_PIN_MSID_Get_PIN, uACE_C_PIN_SID_Get_NOPIN, uAuthority_Admin1, uC_PIN_MSID)
import           Hsed.Sim.Access
import           Hsed.Sim.State           (columnOf)
import           Hsed.Sim.Types


-- | The Admin SP of the default TPer, with its hand-written rows.
adminSP :: SPState
adminSP = fromMaybe (error "the default TPer has no Admin SP") (Map.lookup uSP_Admin (tsSPs (initialState defaultSimParams)))

-- | A session on the Admin SP with these authorities authenticated at its
--   startup.
sessionWith :: [UID] -> Session
sessionWith auths = Session (spBaseComId defaultSimParams) firstTSN 1 uSP_Admin True auths initialHostProperties

-- | Two authorities and an ACE of no table but this test's.
a, b, e1, e2, anObject :: UID
a        = uid 0x00 0x00 0x00 0x09 0x00 0x00 0x77 0x01
b        = uid 0x00 0x00 0x00 0x09 0x00 0x00 0x77 0x02
e1       = uid 0x00 0x00 0x00 0x08 0x00 0x00 0x77 0x01
e2       = uid 0x00 0x00 0x00 0x08 0x00 0x00 0x77 0x02
anObject = uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x77 0x01

-- | An authority in a BooleanExpr, and an operator of the generated
--   boolean_ACE enumeration (Core Table 61, PDF 104).
ref :: UID -> Cell
ref = RefCell "an authority"

op :: Core_boolean_ACE -> Cell
op o = EnumCell (tshow o) (enumValue o)

-- | An ACE row: its UID, its BooleanExpr and its Columns, each in the
--   column the generated description numbers.
aceRow :: UID -> [Cell] -> [Integer] -> Row
aceRow u expr cols =
  [(0, UIDCell u)] <> [ (c, ListCell expr) | Just c <- [columnOf aceTableDesc "BooleanExpr"] ]
                   <> [ (c, ListCell (map UIntCell cols)) | Just c <- [columnOf aceTableDesc "Columns"] ]

-- | An AccessControl row: its InvokingID, MethodID and ACL, no UID cell, as
--   the Admin SP's hand rows have them.
aclRow :: UID -> UID -> [UID] -> Row
aclRow invoker method aces =
  [ (c, RefCell "the object" invoker) | Just c <- [columnOf accessControlTableDesc "InvokingID"] ]
  <> [ (c, RefCell "the method" method) | Just c <- [columnOf accessControlTableDesc "MethodID"] ]
  <> [ (c, ListCell (map (RefCell "an ACE") aces)) | Just c <- [columnOf accessControlTableDesc "ACL"] ]

-- | The Admin SP with these ACE rows, and these AccessControl rows where
--   given, in place of its own.
withRows :: [Row] -> Maybe [Row] -> SPState
withRows aces acl = adminSP { spsTables = Map.insert uACETable aces (maybe id (Map.insert uAccessControlTable) acl (spsTables adminSP)) }

-- | The numbers of the C_PIN table's columns named.
c_PINNumbers :: [Text] -> [Int]
c_PINNumbers names = [ colNumber col | name <- names, Just col <- [columnNamed name c_PINTableDesc] ]

spec :: Spec
spec = do
  describe "the ACL of an InvokingID and MethodID (accessControlList)" $ do
    it "is the ACEs the Admin SP's AccessControl row names: Get on C_PIN_SID and on C_PIN_MSID (Opal 2.30 Table 22)" $ do
      accessControlList adminSP uC_PIN_SID uMethodID_Get `shouldBe` Just [uACE_C_PIN_SID_Get_NOPIN]
      accessControlList adminSP uC_PIN_MSID uMethodID_Get `shouldBe` Just [uACE_C_PIN_MSID_Get_PIN]
    it "is Nothing where there is no row: another method, a table, an object the SP lacks (Core 5.1.5.2)" $ do
      accessControlList adminSP uC_PIN_SID uMethodID_Next `shouldBe` Nothing
      accessControlList adminSP uC_PINTable uMethodID_Get `shouldBe` Nothing
      accessControlList adminSP anObject uMethodID_Get `shouldBe` Nothing
    it "reads an empty ACL as Just [], which grants nothing" $ do
      let sps = withRows [] (Just [aclRow anObject uMethodID_Get []])
      accessControlList sps anObject uMethodID_Get `shouldBe` Just []
      grantedColumns sps (sessionWith [a]) anObject uMethodID_Get `shouldBe` Nothing

  describe "the authorities a session makes true (authenticated)" $ do
    it "are Anybody always, then the session's (Core 5.3.4.1.2.1)" $ do
      authenticated adminSP [] `shouldBe` [uAuthority_Anybody]
      authenticated adminSP [uAuthority_SID] `shouldBe` [uAuthority_Anybody, uAuthority_SID]
      authenticated adminSP [uAuthority_Anybody, uAuthority_SID] `shouldBe` [uAuthority_Anybody, uAuthority_SID]
    it "include the class a member belongs to: Admin1 authenticated makes Admins true (Core 3.4.2.2)" $ do
      authenticated adminSP [uAuthority_Admin1] `shouldBe` [uAuthority_Anybody, uAuthority_Admin1, uAuthority_Admins]
      authenticated adminSP [uAuthority_SID, uAuthority_Admin1] `shouldBe` [uAuthority_Anybody, uAuthority_SID, uAuthority_Admin1, uAuthority_Admins]
    it "add no class for an authority of none, or one the SP has no row for" $ do
      authenticated adminSP [uAuthority_SID] `shouldNotSatisfy` elem uAuthority_Admins
      authenticated adminSP [a] `shouldBe` [uAuthority_Anybody, a]

  describe "an ACE's BooleanExpr in postfix (aceGrant)" $ do
    let grant expr auths = aceGrant (withRows [aceRow e1 expr [1]] Nothing) auths e1
        truth = [ (ta, tb) | ta <- [False, True], tb <- [False, True] ]
        authsOf (ta, tb) = [ a | ta ] <> [ b | tb ]
        granted holds = if holds then Just [1] else Nothing
    it "evaluates And, Or and Not over authenticated and unauthenticated authorities (Core 3.4.2.1; Table 61)" $
      forM_ truth $ \t@(ta, tb) -> do
        grant [ref a, ref b, op Core_boolean_ACE_And] (authsOf t) `shouldBe` granted (ta && tb)
        grant [ref a, ref b, op Core_boolean_ACE_Or] (authsOf t) `shouldBe` granted (ta || tb)
        grant [ref a, op Core_boolean_ACE_Not] (authsOf t) `shouldBe` granted (not ta)
        grant [ref a] (authsOf t) `shouldBe` granted ta
    it "evaluates Table 210's shape, ((a And b) Or c) And d, in postfix" $ do
      let c = uAuthority_SID
          d = uAuthority_Admins
          expr = [ref a, ref b, op Core_boolean_ACE_And, ref c, op Core_boolean_ACE_Or, ref d, op Core_boolean_ACE_And]
      grant expr [c, d] `shouldBe` Just [1]
      grant expr [a, b, d] `shouldBe` Just [1]
      grant expr [a, b, c] `shouldBe` Nothing
      grant expr [a, d] `shouldBe` Nothing
    it "is false for an empty expression (Core 5.3.4.3.3), for an operator short of its operands and for values left over" $ do
      grant [] [a, b] `shouldBe` Nothing
      grant [op Core_boolean_ACE_And] [a, b] `shouldBe` Nothing
      grant [ref a, op Core_boolean_ACE_And] [a, b] `shouldBe` Nothing
      grant [ref a, ref b] [a, b] `shouldBe` Nothing
    it "reads a UID cell as an authority too, and is Nothing for an ACE the SP lacks" $ do
      aceGrant (withRows [aceRow e1 [UIDCell a] [1]] Nothing) [a] e1 `shouldBe` Just [1]
      aceGrant adminSP [a] e1 `shouldBe` Nothing
    it "grants the ACE's Columns where it holds, [] meaning every column (Core 5.3.2.9.5)" $ do
      aceGrant (withRows [aceRow e1 [ref a] [0, 3, 5]] Nothing) [a] e1 `shouldBe` Just [0, 3, 5]
      aceGrant (withRows [aceRow e1 [ref a] []] Nothing) [a] e1 `shouldBe` Just []
    it "[Admins, SID, Or] (ACE_C_PIN_SID_Get_NOPIN) is true for a SID session and false for Anybody" $ do
      aceGrant adminSP (authenticated adminSP [uAuthority_SID]) uACE_C_PIN_SID_Get_NOPIN
        `shouldBe` Just (c_PINNumbers ["UID", "CharSet", "TryLimit", "Tries", "Persistence"])
      aceGrant adminSP (authenticated adminSP []) uACE_C_PIN_SID_Get_NOPIN `shouldBe` Nothing
    it "Admin1 authenticated makes Admins, and so the NOPIN ACE, true; Admin1 alone, without its class, does not" $ do
      aceGrant adminSP (authenticated adminSP [uAuthority_Admin1]) uACE_C_PIN_SID_Get_NOPIN `shouldSatisfy` isJust
      aceGrant adminSP [uAuthority_Admin1] uACE_C_PIN_SID_Get_NOPIN `shouldBe` Nothing
    it "[Anybody] (ACE_C_PIN_MSID_Get_PIN) is true for any session, granting the UID and PIN columns" $
      aceGrant adminSP (authenticated adminSP []) uACE_C_PIN_MSID_Get_PIN `shouldBe` Just (c_PINNumbers ["UID", "PIN"])

  describe "the columns an ACL grants a session (grantedColumns)" $ do
    let two cols1 cols2 = withRows [aceRow e1 [ref a] cols1, aceRow e2 [ref b] cols2] (Just [aclRow anObject uMethodID_Get [e1, e2]])
    it "holds when any ACE holds, granting that ACE's columns" $ do
      grantedColumns (two [0, 3] [3, 5]) (sessionWith [a]) anObject uMethodID_Get `shouldBe` Just [0, 3]
      grantedColumns (two [0, 3] [3, 5]) (sessionWith [b]) anObject uMethodID_Get `shouldBe` Just [3, 5]
      grantedColumns (two [0, 3] [3, 5]) (sessionWith []) anObject uMethodID_Get `shouldBe` Nothing
    it "grants the union of the satisfied ACEs' columns, in order, [] meaning all where any grants all" $ do
      grantedColumns (two [0, 3] [3, 5]) (sessionWith [a, b]) anObject uMethodID_Get `shouldBe` Just [0, 3, 5]
      grantedColumns (two [5, 3] [0]) (sessionWith [a, b]) anObject uMethodID_Get `shouldBe` Just [0, 3, 5]
      grantedColumns (two [] [3, 5]) (sessionWith [a, b]) anObject uMethodID_Get `shouldBe` Just []
      grantedColumns (two [] [3, 5]) (sessionWith [b]) anObject uMethodID_Get `shouldBe` Just [3, 5]
    it "is Nothing where there is no AccessControl row (Core 5.1.5.2)" $ do
      grantedColumns (two [0] [3]) (sessionWith [a, b]) anObject uMethodID_Next `shouldBe` Nothing
      grantedColumns adminSP (sessionWith [uAuthority_SID]) uC_PINTable uMethodID_Get `shouldBe` Nothing
    it "on the Admin SP: Get on C_PIN_MSID as Anybody grants the UID and PIN; on C_PIN_SID nothing as Anybody, the NOPIN columns as SID" $ do
      grantedColumns adminSP (sessionWith []) uC_PIN_MSID uMethodID_Get `shouldBe` Just (c_PINNumbers ["UID", "PIN"])
      grantedColumns adminSP (sessionWith []) uC_PIN_SID uMethodID_Get `shouldBe` Nothing
      grantedColumns adminSP (sessionWith [uAuthority_SID]) uC_PIN_SID uMethodID_Get
        `shouldBe` Just (c_PINNumbers ["UID", "CharSet", "TryLimit", "Tries", "Persistence"])
