{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}

{-|
Module      : Hsed.Spec.Readers.AccessControlSpec
Description : Test specifications for the AccessControl and ACE reader
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The AccessControl and ACE reader: an InvokingID's table, a wildcard's
(Opal *AC1-*AC7, PDF 38-39) included; the one table an ACE applies to, by
the Get and Set rows whose ACL names it, or the tables when they are not
one, and the tables of every row naming it, a meta-ACL's and a method's that
reads no columns included; which tables are access control and ACE tables,
by their kinds; the codes; and, on Opal's Tables 22 and 23 (PDF 39-47) read
by the reader's functions, the table every ACE that lists columns applies
to, which is the one its informative group row names, and ACE_Anybody's
ten, which its All needs none of.
-}
module Hsed.Spec.Readers.AccessControlSpec (spec) where

import qualified RIO.List                        as L
import qualified RIO.Map                         as Map
import qualified RIO.Text                        as T
import           RIO
import           Test.Hspec

import           Hsed.Core.Types                 (TableRef (..))
import qualified Hsed.Spec.Common                as Cm
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR                    (CellIR (..), SpecIR (..), TableIR (..))
import           Hsed.Spec.Readers.AccessControl
import           Hsed.Spec.Readers.Cell          (ColKind (..), Problem (..), joinedText, listItems, printedCell, printedText)
import           Hsed.Spec.Readers.Text          (nameKey)
import           Hsed.Spec.Rules.TableUIDs       (TableUIDRow (..), allTableUIDRows)


spec :: Spec
spec = do
  describe "everyObject and invokedHalf" $ do
    it "read TT TT TT TT after a table's half-UID as every object of the table, its spacing aside (Opal *AC1-*AC7, PDF 38-39)" $ do
      everyObject "00 00 00 01 TT TT TT TT" `shouldBe` Just [0, 0, 0, 1]
      everyObject "00 00 02 04\nTT TT TT\nTT" `shouldBe` Just [0, 0, 2, 4]
      everyObject "00 00 00 01 00 00 00 00" `shouldBe` Nothing
      everyObject "00 01 TT TT TT TT" `shouldBe` Nothing
    it "give the table an InvokingID invokes on: an object's or a table's UID's upper half, or a wildcard's" $ do
      invokedHalf "00 00 00 0B 00 00 84 02" `shouldBe` Just [0, 0, 0, 0x0B]
      invokedHalf "00 00 00 09 00 00 00 00" `shouldBe` Just [0, 0, 0, 9]
      invokedHalf "00 00 02 05 TT TT TT TT" `shouldBe` Just [0, 0, 2, 5]
      invokedHalf "ThisSP" `shouldBe` Nothing

  describe "aceHalf, aceHalves and aceNamedHalves" $ do
    let invocations = [ Invocation (Just [0, 0, 0, 0x0B]) True ["ACE_C_PIN_SID_Get_NOPIN", "ACE_Anybody"] ["ACE_Anybody"]
                      , Invocation (Just [0, 0, 0, 0x0B]) True ["ace_c_pin_sid_get_nopin"] []
                      , Invocation (Just [0, 0, 0, 9]) True ["ACE_Anybody", "ACE_Set_Enabled"] []
                      , Invocation Nothing True ["ACE_Set_Enabled"] []
                      -- Next on the SPTemplates table, which reads no columns
                      , Invocation (Just [0, 0, 0, 3]) False ["ACE_Set_Enabled", "ACE_Only_Next"] []
                      -- Get on the SPInfo object, naming two ACEs in a meta-ACL alone
                      , Invocation (Just [0, 0, 0, 2]) True ["ACE_Anybody"] ["ACE_Set_Enabled", "ACE_Only_Meta"] ]
    it "gives the one table the Get and Set rows whose ACL names an ACE invoke on, names compared case aside" $ do
      aceHalf invocations "ACE_C_PIN_SID_Get_NOPIN" `shouldBe` Right [0, 0, 0, 0x0B]
      aceHalf invocations "ACE_Set_Enabled" `shouldBe` Right [0, 0, 0, 9]
    it "gives the tables when they are not one: several, or none" $ do
      aceHalf invocations "ACE_Anybody" `shouldBe` Left [[0, 0, 0, 0x0B], [0, 0, 0, 9], [0, 0, 0, 2]]
      aceHalf invocations "ACE_Nobody" `shouldBe` Left []
    it "gives every table, in the rows' order, for the column names to be read in each (M5's Table 39, PDF 78)" $ do
      aceHalves invocations "ACE_Anybody" `shouldBe` [[0, 0, 0, 0x0B], [0, 0, 0, 9], [0, 0, 0, 2]]
      aceHalves invocations "ACE_Set_Enabled" `shouldBe` [[0, 0, 0, 9]]
    it "holds no ACE's Columns to the table of a method that reads none or of a meta-ACL's row (Core 5.3.2.9.5, PDF 165; GetACL, 5.3.3.13, PDF 206)" $ do
      map (aceHalves invocations) ["ACE_Only_Next", "ACE_Only_Meta"] `shouldBe` [[], []]
      map (aceNamedHalves invocations) ["ACE_Set_Enabled", "ACE_Only_Next", "ACE_Only_Meta", "ACE_Nobody"]
        `shouldBe` [[[0, 0, 0, 9], [0, 0, 0, 3], [0, 0, 0, 2]], [[0, 0, 0, 3]], [[0, 0, 0, 2]], []]
    it "takes Get and Set, case aside, and no other method, for the methods that read or write columns" $
      map columnDependent ["Get", "set", "SET", "Next", "Authenticate", "GetACL", "Revert"] `shouldBe` [True, True, True, False, False, False, False]

  describe "the tables, by their kinds" $
    it "an access control table holds an invoking ID; an ACE table an expression or column names; an ACL column references" $ do
      isAccessControl [KUID, KUIDRef, KList (KRef [Table "ACE"])] `shouldBe` True
      isAccessControl [KUID, KText 32] `shouldBe` False
      isACE [KUID, KText 32, KColumns] `shouldBe` True
      isACE [KUID, KExpr "boolean_ACE" [] [Table "Authority"]] `shouldBe` True
      isACE [KUID, KUIDRef] `shouldBe` False
      map aclColumn [KList (KRef [Table "ACE"]), KRef [Table "MethodID"], KList (KText 32)] `shouldBe` [True, False, False]

  describe "aclCode" $
    it "gives an expression's problem C-ACL-EXPR, a name naming nothing C-ACL-REFS in the access control tables, else nothing" $ do
      aclCode False KColumns (BadExpr "x") `shouldBe` Just "C-ACL-EXPR"
      aclCode True (KRef [Table "MethodID"]) (Unresolved "x") `shouldBe` Just "C-ACL-REFS"
      aclCode False KColumns (Unresolved "x") `shouldBe` Just "C-ACL-REFS"
      aclCode False (KExpr "boolean_ACE" [] [Table "Authority"]) (Unresolved "x") `shouldBe` Just "C-ACL-REFS"
      aclCode False (KRef [Table "Authority"]) (Unresolved "x") `shouldBe` Nothing
      aclCode True (KRef [Table "MethodID"]) (Unread "x") `shouldBe` Nothing

  describe "accessControlCodes" $
    it "claims C-ACL-REFS and C-ACL-EXPR, both Errors" $
      [ (Cm.codeName c, Cm.codeSeverities c) | c <- accessControlCodes ] `shouldBe` [("C-ACL-REFS", [Cm.Error]), ("C-ACL-EXPR", [Cm.Error])]

  withM4bOpal $ \base opal -> do
    let tableOf tid = L.find (\t -> t.id == Just tid) opal.tables
        -- the rows that are not group rows, each with the group row above it, its name's lines joined
        grouped :: TableIR -> [(Maybe Text, [CellIR])]
        grouped t = catMaybes (snd (L.mapAccumL step Nothing t.rows))
          where
            step g cells = case cells of
              c0 : rest@(_ : _) | not (T.null (T.strip c0.text)), all (T.null . T.strip . (.text)) rest
                -> (Just (joinedText (Just (KText 32)) (printedCell c0)), Nothing)
              _ -> (g, Just (g, cells))
        at :: Int -> [CellIR] -> Text
        at i cells = maybe "" (printedText . printedCell) (listToMaybe (drop i cells))
        -- Table 22's rows (PDF 39-46): InvokingID column 2, MethodID 4, ACL 6, the meta-ACLs 8 to 11
        aceList = listItems (KRef [Table "ACE"])
        invocations = [ Invocation (invokedHalf (at 2 cells)) (columnDependent (at 4 cells)) (aceList (at 6 cells))
                                   (concatMap (aceList . (`at` cells)) [8 .. 11])
                      | Just t <- [tableOf "T22"], (_, cells) <- grouped t ]
        -- Table 23's rows (PDF 46-47): the group, Name column 2, Columns column 5
        aces = [ (fromMaybe "" g, joinedText (Just (KText 32)) (printedCell c2), at 5 cells)
               | Just t <- [tableOf "T23"], (g, cells@(_ : _ : c2 : _)) <- grouped t ]
        halves = Map.fromList [ (nameKey (tuName tu), take 4 h) | (_, tu) <- allTableUIDRows (opalOver base opal), Just h <- [tuTable tu] ]
    describe "on Opal's Tables 22 and 23 under the M4b sidecar (PDF 39-47)" $ do
      it "reads Table 22's 34 rows' invocations and Table 23's 10 ACEs" $ do
        length invocations `shouldBe` 34
        length aces `shouldBe` 10
        length [ () | Invocation (Just _) _ _ _ <- invocations ] `shouldBe` 34
        length [ () | Invocation _ True _ _ <- invocations ] `shouldBe` 22
      it "gives every ACE that lists columns the table its informative group row names" $
        [ (name, aceHalf invocations name == maybe (Left []) Right (Map.lookup (nameKey g) halves))
        | (g, name, columns) <- aces, nameKey columns /= "all" ]
          `shouldBe` map (, True)
            [ "ACE_Set_Enabled", "ACE_C_PIN_SID_Get_NOPIN", "ACE_C_PIN_SID_Set_PIN", "ACE_C_PIN_MSID_Get_PIN"
            , "ACE_C_PIN_Admins_Set_PIN", "ACE_TPerInfo_Set_ProgrammaticResetEnable"
            , "ACE_DataRemovalMechanism_Set_ActiveDataRemovalMechanism" ]
      it "names ACE_Admin and ACE_SP_SID on the SP table in Revert's and Activate's ACLs alone, and ACE_Anybody on the twelve tables, in ten Get rows' ACLs, which All needs none of" $ do
        map (aceHalf invocations) ["ACE_Admin", "ACE_SP_SID"] `shouldBe` replicate 2 (Left [])
        map (aceNamedHalves invocations) ["ACE_Admin", "ACE_SP_SID"] `shouldBe` replicate 2 [[0, 0, 2, 5]]
        either length (const 0) (aceHalf invocations "ACE_Anybody") `shouldBe` 10
        length (aceNamedHalves invocations "ACE_Anybody") `shouldBe` 12
