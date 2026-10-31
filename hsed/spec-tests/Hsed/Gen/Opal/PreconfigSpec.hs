{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Gen.Opal.PreconfigSpec
Description : The generated Admin SP (Hsed.Gen.Opal.Preconfig.AdminSP) and the simulator's initial Admin SP, held to each other both ways
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Admin SP's preconfiguration as @hsedc compile@ generates it from Opal 2.30
(Tables 18-25, 27-29 and 33, PDF 35-52, bound by @rule preconfig@ at section
4.2: "Hsed.Gen.Opal.Preconfig.AdminSP") and the Admin SP the simulator starts
with ("Hsed.Sim.Types".@initialState@, which folds it, "Hsed.Sim.Preconfig"),
held to each other both ways (design 8.3; M4b, as BehavioursSpec and
LawsSpec hold the generated behaviours and laws to the dispatch):

* every generated table is one the SP holds, by its UID, with its
  description, and the SP holds no other;
* every object the generated rows give (a printed row's UID, a template's
  instance for each Admin authority past Admin1, PDF 47-49) is a row of the
  SP's table, and the SP's tables hold no other;
* every AccessControl row the generated rows give (a printed row, a
  template's instance, one for each object of the table an every-object row
  names, *AC1-*AC7, PDF 38-39), by its InvokingID and MethodID, is one the SP
  holds, with the ACL the generated row gives it as "Hsed.Sim.Access" reads
  it, and the SP holds no other;
* 12 tables and 116 rows at the defaults (T22's 22 printed rows and 46 for
  every object of seven tables, PDF 39-46), 128 with three Admin
  authorities;
* every cell the one the table prints, the TPer's where the table leaves it
  to the TPer (the eight @VU@ and @MSID@ cells, PDF 48-52,
  "Hsed.Sim.Preconfig".@vendorCell@, which answers for no other column), or
  the column's default (Core 5.3.4.2.2, PDF 223), in every column but an
  AccessControl row's UID, the TPer's to assign (Core 5.3.2.7.1, PDF 162).

UCT-03's pair holds on the generated rows: Get on C_PIN_MSID grants Anybody
the UID and the PIN, Get on C_PIN_SID grants Admins OR SID every column but
the PIN (Table 22 rows 23 and 25, PDF 42; Table 23, PDF 47). And the jig's
and Level 0's readers find their rows: SID's credential, the SPs' life
cycles, TPerInfo's ProgrammaticResetEnable.

And the values M4a wrote by hand before the Admin SP was generated, kept as
the test fixture "Hand.OpalUIDs", are the generated ones: its six UIDs, by
name, and TPER_RESET's ComID, which "Hsed.Wire" reads from Opal Table 13
(design 9.3: the Guard's C_PIN_MSID among them, before M4c's drive run).
-}
module Hsed.Gen.Opal.PreconfigSpec (spec) where

import qualified RIO.List                        as L
import qualified RIO.Map                         as Map
import           RIO
import           Test.Hspec

import qualified Hand.OpalUIDs                   as H
import           Hsed.Core.Preconfig             (PreCell (..), PreconfRow (..), PreconfTable (..), RowKind (..), instanceUID)
import           Hsed.Core.Table                 (Cell (..), Col (..), ColNum, Pre (..), Row, TableDesc (..), columnNamed)
import           Hsed.Core.UID                   (HalfUID, UID, halfUID, uidUpper)
import           Hsed.Gen.Core.TableDescs        (aceTableDesc, accessControlTableDesc)
import           Hsed.Gen.Core.TableUIDs         (uACETable, uAccessControlTable)
import           Hsed.Gen.Core.UIDs              (uAuthority_Admins, uAuthority_Anybody, uAuthority_SID, uC_PIN_SID, uMethodID_Get,
                                                  uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP (adminSP, uACE_C_PIN_MSID_Get_PIN, uACE_C_PIN_SID_Get_NOPIN, uAuthority_Admin1,
                                                  uC_PIN_Admin1, uC_PIN_MSID, uSP_Locking)
import           Hsed.Jig.Script                 (Credential (..), simCredential)
import           Hsed.Sim.Access                 (accessControlList, aceGrant, authenticated)
import           Hsed.Sim.Preconfig              (adminSPTables, instances, vendorCell)
import           Hsed.Sim.State                  (programmaticResetEnabled, spLifeCycle)
import           Hsed.Sim.Types                  (LifeCycle (..), SPState (..), SimParams (..), TPerState (..), defaultSimParams,
                                                  initialState, lifeCycleValue)
import           Hsed.Wire                       (tperResetComId)


-- * The generated tables

-- | The generated table of a table UID.
generated :: UID -> Maybe PreconfTable
generated u = L.find ((== u) . tdTableUID . ptDesc) adminSP

-- | A generated table's rows, without their access lists.
rowsOf :: PreconfTable -> [PreconfRow]
rowsOf t = [ r | Pre r _ <- ptRows t ]

-- | The cells a generated row prints, given.
givenOf :: PreconfRow -> Row
givenOf r = [ (c, cell) | (c, Given cell) <- prCells r ]

-- | A generated row's Name, as printed; none where its table has no Name
--   column or the row prints none.
nameOf :: PreconfTable -> PreconfRow -> Text
nameOf t r = case cellNamed (ptDesc t) "Name" (givenOf r) of
  Just (TextCell n) -> n
  _                 -> ""

-- | The generated table whose table UID's upper half is a half-UID.
tableOfHalf :: HalfUID -> Maybe TableDesc
tableOfHalf h = L.find ((== h) . uidUpper . tdTableUID) (map ptDesc adminSP)

-- | The generated AccessControl table's rows (Table 22, PDF 39-46).
accessRows :: [PreconfRow]
accessRows = maybe [] rowsOf (generated uAccessControlTable)

-- | A generated AccessControl row's InvokingID, MethodID and ACL, each as
--   the UIDs it references.
invokingOf, methodOf :: PreconfRow -> Maybe UID
invokingOf r = cellNamed accessControlTableDesc "InvokingID" (givenOf r) >>= refOf
methodOf r   = cellNamed accessControlTableDesc "MethodID" (givenOf r) >>= refOf

aclOf :: PreconfRow -> Maybe [UID]
aclOf r = cellNamed accessControlTableDesc "ACL" (givenOf r) >>= \case
  ListCell cs -> traverse refOf cs
  _           -> Nothing

-- | The pairs of InvokingID and MethodID the generated AccessControl table
--   prints.
printedPairs :: [(UID, UID)]
printedPairs = [ (inv, m) | r <- accessRows, Plain <- [prKind r], Just inv <- [invokingOf r], Just m <- [methodOf r] ]

-- | The objects the generated tables give under some parameters, by table
--   UID, the AccessControl table aside: each printed row's UID, and each
--   template's instances' ("Hsed.Core.Preconfig".'instanceUID' of its base,
--   numbered as "Hsed.Sim.Preconfig".'instances' numbers a run's).
objectsOf :: SimParams -> Map UID [UID]
objectsOf p = Map.fromList
  [ (tdTableUID d, printed <> templated)
  | t <- adminSP, let d = ptDesc t, tdTableUID d /= uAccessControlTable
  , let printed   = [ u | r <- rowsOf t, Plain <- [prKind r], Just (UIDCell u) <- [L.lookup 0 (givenOf r)] ]
        templated = [ u | r <- rowsOf t, Template ph _ <- [prKind r], Just (UIDCell base) <- [L.lookup 0 (givenOf r)]
                        , n <- instances p d ph, Just u <- [instanceUID ph base n] ] ]

-- | The AccessControl rows the generated table gives under some parameters,
--   each by its InvokingID and MethodID with the ACL the generated row gives
--   it: the printed rows', then, in the table's order, a template's instances'
--   (numbered by its InvokingID's table) and an every-object row's for each
--   object whose UID begins with its half, among every object the generated
--   tables give; a pair a row before it gives counted once, the TPer holding
--   one row for each (Core Table 175, PDF 162: InvokingID and MethodID
--   unique).
accessOf :: SimParams -> [((UID, UID), Maybe [UID])]
accessOf p = L.nubBy ((==) `on` fst) (printed <> concatMap added accessRows)
  where
    objects = L.sort (concat (Map.elems (objectsOf p)))
    printed = [ ((inv, m), aclOf r) | r <- accessRows, Plain <- [prKind r], Just inv <- [invokingOf r], Just m <- [methodOf r] ]
    added r = case prKind r of
      Plain            -> []
      Template ph _    -> [ ((u, m), aclOf r) | Just base <- [invokingOf r], Just m <- [methodOf r], Just d <- [tableOfHalf (uidUpper base)]
                                               , n <- instances p d ph, Just u <- [instanceUID ph base n] ]
      EveryObject half -> [ ((u, m), aclOf r) | Just m <- [methodOf r], u <- objects, uidUpper u == half ]

-- | Each cell a generated table leaves to the TPer: its table, its row, its
--   column, the marker printed.
vendorCells :: [(PreconfTable, PreconfRow, ColNum, Text)]
vendorCells = [ (t, r, c, m) | t <- adminSP, r <- rowsOf t, (c, VendorUnique m) <- prCells r ]


-- * The simulator's Admin SP

-- | The Admin SP a run starts with.
adminSPOf :: SimParams -> SPState
adminSPOf p = fromMaybe (error "the simulator holds no Admin SP") (Map.lookup uSP_Admin (tsSPs (initialState p)))

-- | The rows the SP holds of a table.
heldRows :: SPState -> UID -> [Row]
heldRows sps table = fromMaybe [] (Map.lookup table (spsTables sps))

-- | The row the SP holds of an object.
heldRow :: SPState -> UID -> UID -> Maybe Row
heldRow sps table u = L.find ((== Just (UIDCell u)) . L.lookup 0) (heldRows sps table)

-- | The AccessControl row the SP holds of an InvokingID and MethodID.
heldAccess :: SPState -> (UID, UID) -> Maybe Row
heldAccess sps pr = L.find ((== Just pr) . pairOf) (heldRows sps uAccessControlTable)

-- | An AccessControl row's InvokingID and MethodID.
pairOf :: Row -> Maybe (UID, UID)
pairOf r = (,) <$> (cellNamed accessControlTableDesc "InvokingID" r >>= refOf) <*> (cellNamed accessControlTableDesc "MethodID" r >>= refOf)

-- | A row's cell in a column of a name.
cellNamed :: TableDesc -> Text -> Row -> Maybe Cell
cellNamed desc name r = columnNamed name desc >>= \col -> L.lookup (colNumber col) r

-- | The UID a reference holds.
refOf :: Cell -> Maybe UID
refOf = \case
  UIDCell u   -> Just u
  RefCell _ u -> Just u
  _           -> Nothing

-- | The parameters with so many Admin authorities.
admins :: Natural -> SimParams
admins n = defaultSimParams { spAdminSPAdmins = n }


spec :: Spec
spec = describe "the generated Admin SP and the simulator's" $ do
  let sps0 = adminSPOf defaultSimParams

  describe "the tables and their rows, both ways" $ do
    it "are the simulator's Admin SP's: adminSPTables gives the generated tables, whatever the parameters" $
      forM_ [defaultSimParams, admins 3, defaultSimParams { spLockingOFS = Manufactured, spTPerResetEnabled = True }] $ \p ->
        adminSPTables p `shouldBe` adminSP
    it "hold every generated table, by its UID, with its description, and no other" $ do
      spsDescs sps0 `shouldBe` Map.fromList [ (tdTableUID (ptDesc t), ptDesc t) | t <- adminSP ]
      Map.keys (spsTables sps0) `shouldBe` Map.keys (spsDescs sps0)
    it "hold every object the generated rows give, a printed row's or a template's instance's, by its UID, and no other" $
      forM_ [defaultSimParams, admins 3] $ \p ->
        Map.map L.sort (Map.delete uAccessControlTable (Map.map (map (\r -> L.lookup 0 r >>= refOf)) (spsTables (adminSPOf p))))
          `shouldBe` Map.map (L.sort . map Just) (objectsOf p)
    it "hold every AccessControl row the generated rows give, printed, a template's instance or one for every object, by InvokingID and MethodID, and no other" $
      forM_ [defaultSimParams, admins 3] $ \p -> do
        let held = map pairOf (heldRows (adminSPOf p) uAccessControlTable)
        L.sort held `shouldBe` L.sort (map (Just . fst) (accessOf p))
        L.nub held `shouldBe` held
    it "read each AccessControl row's ACL as the generated row gives it (Hsed.Sim.Access.accessControlList), none empty" $
      forM_ [defaultSimParams, admins 3] $ \p -> do
        let sps = adminSPOf p
        [ (pr, accessControlList sps (fst pr) (snd pr)) | (pr, _) <- accessOf p ] `shouldBe` accessOf p
        [ pr | (pr, acl) <- accessOf p, maybe True null acl ] `shouldBe` []
    it "are 12 tables and 116 rows at the defaults: T22's 22 printed rows and 46 for every object of seven tables, no template's instance" $ do
      [ (tdName (ptDesc t), length (heldRows sps0 (tdTableUID (ptDesc t)))) | t <- adminSP ] `shouldBe`
        [ ("SPInfo", 1), ("SPTemplates", 2), ("Table", 12), ("MethodID", 8), ("AccessControl", 68), ("ACE", 10), ("Authority", 5)
        , ("C_PIN", 3), ("TPerInfo", 1), ("Template", 3), ("SP", 2), ("DataRemovalMechanism", 1) ]
      sum (map length (Map.elems (spsTables sps0))) `shouldBe` 116
      let kinds = map prKind accessRows
          added = [ u | Just (u, m) <- map pairOf (heldRows sps0 uAccessControlTable), (u, m) `notElem` printedPairs ]
      (length [ () | Plain <- kinds ], length [ () | EveryObject _ <- kinds ], length [ () | Template _ _ <- kinds ]) `shouldBe` (22, 9, 3)
      length printedPairs `shouldBe` 22
      -- Get on every object of Table (T20's 12), SPTemplates (2), MethodID (8), ACE (10), Authority (5) and Template (3),
      -- and Get, Revert and Activate on each of the two SPs (*AC1-*AC9, PDF 38-39)
      Map.toList (Map.fromListWith (+) [ (uidUpper u, 1 :: Int) | u <- added ]) `shouldBe`
        [ (halfUID 0x00 0x00 0x00 0x01, 12), (halfUID 0x00 0x00 0x00 0x03, 2), (halfUID 0x00 0x00 0x00 0x06, 8)
        , (halfUID 0x00 0x00 0x00 0x08, 10), (halfUID 0x00 0x00 0x00 0x09, 5), (halfUID 0x00 0x00 0x02 0x04, 3)
        , (halfUID 0x00 0x00 0x02 0x05, 6) ]
    it "are 128 rows with three Admin authorities: Admin2 and Admin3, their credentials, Set on each, Get and Set on each credential, Get on each by *AC5" $ do
      let sps = adminSPOf (admins 3)
      sum (map length (Map.elems (spsTables sps))) `shouldBe` 128
      [ (tdName (ptDesc t), length (heldRows sps (tdTableUID (ptDesc t)))) | t <- adminSP, tdName (ptDesc t) `elem` ["AccessControl", "Authority", "C_PIN"] ]
        `shouldBe` [("AccessControl", 76), ("Authority", 7), ("C_PIN", 5)]

  describe "the cells" $ do
    it "are every column of the row's table, but an AccessControl row's UID, which the TPer assigns (Core 5.3.2.7.1, PDF 162)" $
      forM_ [defaultSimParams, admins 3] $ \p ->
        forM_ (Map.toList (spsTables (adminSPOf p))) $ \(table, rows) -> do
          let columns = maybe [] (map colNumber . tdColumns . ptDesc) (generated table)
              wanted  = if table == uAccessControlTable then filter (/= 0) columns else columns
          map (map fst) rows `shouldBe` map (const wanted) rows
    it "are, in a printed row, each cell printed, the TPer's where the table leaves it to the TPer, and every other column's default" $
      forM_ adminSP $ \t -> forM_ [ r | r <- rowsOf t, Plain <- [prKind r] ] $ \r -> do
        let desc   = ptDesc t
            given  = givenOf r
            held   = if tdTableUID desc == uAccessControlTable
                       then pairOf given >>= heldAccess sps0
                       else L.lookup 0 given >>= refOf >>= heldRow sps0 (tdTableUID desc)
            wanted = L.sortOn fst
              ( given
             <> [ (c, cell) | (c, VendorUnique m) <- prCells r, Just cell <- [vendorCell defaultSimParams desc given c m] ]
             <> [ d | d@(c, _) <- ptDefaults t, c `notElem` map fst (prCells r) ] )
        (tdName desc, nameOf t r, held) `shouldBe` (tdName desc, nameOf t r, Just wanted)
    it "leave nothing to the TPer: the eight VU and MSID cells the tables print (PDF 48-52) are each vendorCell's" $ do
      [ (tdName (ptDesc t), nameOf t r, maybe "" colName (L.find ((== c) . colNumber) (tdColumns (ptDesc t))), m) | (t, r, c, m) <- vendorCells ]
        `shouldBe` [ ("C_PIN", "C_PIN_SID", "PIN", "VU"), ("C_PIN", "C_PIN_SID", "TryLimit", "VU"), ("C_PIN", "C_PIN_SID", "Tries", "VU")
                   , ("C_PIN", "C_PIN_MSID", "PIN", "MSID"), ("TPerInfo", "", "GUDID", "VU"), ("Template", "Base", "Instances", "VU")
                   , ("Template", "Base", "MaxInstances", "VU"), ("DataRemovalMechanism", "", "ActiveDataRemovalMechanism", "VU") ]
      [ (tdName (ptDesc t), c, m) | (t, r, c, m) <- vendorCells, isNothing (vendorCell defaultSimParams (ptDesc t) (givenOf r) c m) ] `shouldBe` []
    it "have vendorCell answer for no column and marker the generated tables do not leave to the TPer" $ do
      let printedVU = [ (tdTableUID (ptDesc t), c, m) | (t, _, c, m) <- vendorCells ]
      [ (tdName (ptDesc t), colName col, m)
        | t <- adminSP, r <- rowsOf t, col <- tdColumns (ptDesc t), m <- ["VU", "MSID"]
        , isJust (vendorCell defaultSimParams (ptDesc t) (givenOf r) (colNumber col) m)
        , (tdTableUID (ptDesc t), colNumber col, m) `notElem` printedVU ]
        `shouldBe` []

  describe "UCT-03's pair, on the generated rows (Table 22 rows 23 and 25, PDF 42; Table 23, PDF 47)" $ do
    let booleanExpr ace = heldRow sps0 uACETable ace >>= cellNamed aceTableDesc "BooleanExpr"
    it "Get on C_PIN_MSID: ACE_C_PIN_MSID_Get_PIN, which grants Anybody the UID and the PIN, [0, 3]" $ do
      accessControlList sps0 uC_PIN_MSID uMethodID_Get `shouldBe` Just [uACE_C_PIN_MSID_Get_PIN]
      booleanExpr uACE_C_PIN_MSID_Get_PIN `shouldBe` Just (ListCell [RefCell "Anybody" uAuthority_Anybody])
      aceGrant sps0 (authenticated sps0 []) uACE_C_PIN_MSID_Get_PIN `shouldBe` Just [0, 3]
    it "Get on C_PIN_SID: ACE_C_PIN_SID_Get_NOPIN, which grants Admins OR SID every column but the PIN, [0, 4, 5, 6, 7], and Anybody none" $ do
      accessControlList sps0 uC_PIN_SID uMethodID_Get `shouldBe` Just [uACE_C_PIN_SID_Get_NOPIN]
      booleanExpr uACE_C_PIN_SID_Get_NOPIN `shouldBe` Just (ListCell [RefCell "Admins" uAuthority_Admins, RefCell "SID" uAuthority_SID, EnumCell "OR" 1])
      aceGrant sps0 (authenticated sps0 []) uACE_C_PIN_SID_Get_NOPIN `shouldBe` Nothing
      aceGrant sps0 (authenticated sps0 [uAuthority_SID]) uACE_C_PIN_SID_Get_NOPIN `shouldBe` Just [0, 4, 5, 6, 7]
      aceGrant sps0 (authenticated sps0 [uAuthority_Admin1]) uACE_C_PIN_SID_Get_NOPIN `shouldBe` Just [0, 4, 5, 6, 7]

  describe "what the jig and Level 0 read of it" $ do
    it "SID's credential (Script.simCredential): the MSID, or the parameters' PIN, with the parameters' TryLimit and no tries (Opal 2.30 4.2.1.8, PDF 49)" $ do
      simCredential uAuthority_SID (initialState defaultSimParams) `shouldBe` Just (Credential (spMsid defaultSimParams) 0 0)
      simCredential uAuthority_SID (initialState defaultSimParams { spSidPin = Just "SID!", spSidTryLimit = 3 }) `shouldBe` Just (Credential "SID!" 3 0)
      simCredential uAuthority_Anybody (initialState defaultSimParams) `shouldBe` Nothing
    it "the SPs' life cycles, which Level 0's Locking descriptor reads: the Admin SP Manufactured, the Locking SP Manufactured-Inactive or the parameters' (Table 29, PDF 50)" $ do
      map (`spLifeCycle` initialState defaultSimParams) [uSP_Admin, uSP_Locking]
        `shouldBe` map (Just . lifeCycleValue) [Manufactured, ManufacturedInactive]
      spLifeCycle uSP_Locking (initialState defaultSimParams { spLockingOFS = Manufactured }) `shouldBe` Just (lifeCycleValue Manufactured)
    it "TPER_RESET's enabling, TPerInfo's ProgrammaticResetEnable: FALSE as Table 27 prints it (PDF 50), TRUE where the parameters enable it" $ do
      programmaticResetEnabled (initialState defaultSimParams) `shouldBe` Just False
      programmaticResetEnabled (initialState defaultSimParams { spTPerResetEnabled = True }) `shouldBe` Just True

  describe "the values M4a wrote by hand, kept as the test fixture Hand.OpalUIDs (design 9.3)" $ do
    it "are the generated UIDs, by name: C_PIN_MSID and C_PIN_Admin1, Admin1, UCT-03's two ACEs, the Locking SP (Tables 23-25 and 29, PDF 47-50)" $
      ([ ("uC_PIN_MSID", H.uC_PIN_MSID), ("uC_PIN_Admin1", H.uC_PIN_Admin1), ("uAuthority_Admin1", H.uAuthority_Admin1)
      , ("uACE_C_PIN_SID_Get_NOPIN", H.uACE_C_PIN_SID_Get_NOPIN), ("uACE_C_PIN_MSID_Get_PIN", H.uACE_C_PIN_MSID_Get_PIN)
      , ("uSP_Locking", H.uSP_Locking) ] :: [(Text, UID)])
        `shouldBe`
      [ ("uC_PIN_MSID", uC_PIN_MSID), ("uC_PIN_Admin1", uC_PIN_Admin1), ("uAuthority_Admin1", uAuthority_Admin1)
      , ("uACE_C_PIN_SID_Get_NOPIN", uACE_C_PIN_SID_Get_NOPIN), ("uACE_C_PIN_MSID_Get_PIN", uACE_C_PIN_MSID_Get_PIN)
      , ("uSP_Locking", uSP_Locking) ]
    it "is TPER_RESET's ComID, Hsed.Wire's, read from the generated Table 13's row (PDF 29)" $
      tperResetComId `shouldBe` H.tperResetComId
