{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Gen.Opal.PreconfigSpec
Description : The generated Admin and Locking SPs and the simulator's initial SPs, held to each other both ways
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
* every cell of every row the SP holds, printed, a template's instance or
  one for every object (since the M4b review, which found the cells of the
  printed rows alone held), the one the table prints, the TPer's where the
  table leaves it to the TPer (the eight @VU@ and @MSID@ cells, PDF 48-52,
  "Hsed.Sim.Preconfig".@vendorCell@, which answers for no other column), the
  column's default (Core 5.3.4.2.2, PDF 223), or, in the Table table's rows,
  the described table's NumColumns and Rows (Core 5.3.2.3.7-5.3.2.3.8, PDF
  157), in every column but an AccessControl row's UID, the TPer's to assign
  (Core 5.3.2.7.1, PDF 162), at the defaults and with three Admin
  authorities; every every-object half and template base a table the SP
  holds.

Since M5a the Locking SP's preconfiguration (Opal 2.30 Tables 34-42 and
44-48, PDF 52-89, "Hsed.Gen.Opal.Preconfig.LockingSP", a stand-in until M5a's
integration) is held to the simulator's Locking SP the same ways, at either
Original Factory State and with each choice of key tables, the key table the
parameters do not hold read apart from the fold (Table 36's *TT1, PDF 54;
*AC6, PDF 55), and with more Admins, Users and ranges than the defaults: its
tables, objects, AccessControl rows and every cell, a value with a note its
value, the ActiveKey the key tables decide, LockingInfo's MaxRanges and
geometry the parameters'; 393 rows over 13 object tables with K_AES_256
alone and 451 over 14 with both at the defaults (the M5 plan's fold model);
its 21 cells left to the TPer each answered; and C_PIN_Admin1's PIN the
MSID at either OFS, its life cycle 8 or 9 (Table 41 note 1, PDF 83-84; Table
49, PDF 98).

UCT-03's pair holds on the generated rows: Get on C_PIN_MSID grants Anybody
the UID and the PIN, Get on C_PIN_SID grants Admins OR SID its UID, CharSet,
TryLimit, Tries and Persistence, not its Name, CommonName or PIN (Table 22
rows 23 and 25, PDF 42; Table 23, PDF 47). And the jig's
and Level 0's readers find their rows: SID's credential, the SPs' life
cycles, TPerInfo's ProgrammaticResetEnable.

One generated name for each object: @uAuthority_Admin1@ is Admin1's own,
another object than the Issuers Core Table 243 gives the same UID (Opal Table
24, PDF 48; Core Table 216, PDF 236), where the Table table's rows and the
SPInfo row keep the names Core's and Opal's generated modules give them
(@u\<T\>TableObject@, @uSPInfo_Row@) and get no second.

And the values M4a wrote by hand before the Admin SP was generated, kept as
the test fixture "Hand.OpalUIDs", are the generated ones: its six UIDs, by
name, and TPER_RESET's ComID, which "Hsed.Wire" reads from Opal Table 13
(design 9.3: the Guard's C_PIN_MSID among them, before M4c's drive run).
-}
module Hsed.Gen.Opal.PreconfigSpec (spec) where

import qualified RIO.List                        as L
import qualified RIO.Map                         as Map
import           RIO
import qualified RIO.Set                         as Set
import qualified RIO.Text                        as T
import           Test.Hspec

import qualified Hand.OpalUIDs                   as H
import           Hsed.Core.Preconfig             (PreCell (..), PreconfRow (..), PreconfTable (..), RowKind (..), instanceUID, instantiate,
                                                  unmarked)
import           Hsed.Core.Table                 (Cell (..), Col (..), ColNum, Pre (..), Row, TableDesc (..), columnNamed)
import           Hsed.Core.Types                 (enumValue)
import           Hsed.Core.UID                   (HalfUID, UID, halfUID, uid, uidLower, uidUpper)
import           Hsed.Gen.Core.Enums             (Core_table_kind (..))
import           Hsed.Gen.Core.TableDescs        (aceTableDesc, accessControlTableDesc, authorityTableDesc, c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs         (uACETable, uAccessControlTable, uAuthorityTable, uC_PINTable, uK_AES_128Table,
                                                  uK_AES_256Table, uLockingInfoTable, uLockingTable, uMBRControlTable, uSPTable, uTableTable)
import           Hsed.Gen.Core.UIDs              (uAuthority_Admins, uAuthority_Anybody, uAuthority_Issuers, uAuthority_SID, uC_PIN_SID,
                                                  uMethodID_Get, uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP (adminSP, uACE_C_PIN_MSID_Get_PIN, uACE_C_PIN_SID_Get_NOPIN, uAuthority_Admin1,
                                                  uC_PIN_Admin1, uC_PIN_MSID, uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Script                 (Credential (..), simCredential)
import           Hsed.Sim.Access                 (accessControlList, aceGrant, authenticated)
import           Hsed.Sim.Preconfig              (instances, vendorCell)
import           Hsed.Sim.State                  (programmaticResetEnabled, spLifeCycle)
import           Hsed.Sim.Types                  (Geometry (..), LifeCycle (..), LockingKeys (..), SPState (..), SimParams (..),
                                                  TPerState (..), defaultSimParams, initialState, lifeCycleValue)
import           Hsed.Wire                       (tperResetComId)


-- * The generated tables

-- | An SP's generated preconfiguration as a run holds it: the SP's UID and
--   its tables. The Admin SP's are all of its generated tables; the Locking
--   SP's leave out the key table the run's parameters do not hold, and every
--   row that names it ('lockingGen').
data Gen = Gen
  { gSP     :: UID
  , gTables :: [PreconfTable]
  }

-- | The Admin SP's generated preconfiguration (Opal 2.30 Tables 18-25,
--   27-29 and 33, PDF 35-52).
adminGen :: Gen
adminGen = Gen uSP_Admin adminSP

-- | The Locking SP's generated preconfiguration (Opal 2.30 Tables 34-42 and
--   44-48, PDF 52-89) under some parameters, read here apart from the fold,
--   as the M5 plan's fold model reads the PDF's tables: the key table the
--   parameters do not hold (Table 36's *TT1, PDF 54) left out, and every row
--   of the others whose object lies in it (its UID, or its InvokingID,
--   begins with that table's half-UID) or whose printed cells, a choice of
--   key aside, say that table's name (*AC6, PDF 55), and every row whose
--   object is one of those rows' (the AccessControl rows of the ACEs named
--   for the table, which the generated rows print by UID alone, as the PDF
--   prints their names beside).
lockingGen :: SimParams -> Gen
lockingGen p = Gen uSP_Locking
  [ t { ptRows = [ pr | pr@(Pre r _) <- ptRows t, not (names r), all (`notElem` namedObjects) (objectUID r) ] }
  | t <- LockingSP.lockingSP, tdName (ptDesc t) `notElem` unheld ]
  where
    namedObjects = [ u | t <- LockingSP.lockingSP, r <- rowsOf t, names r, u <- objectUID r ]
    unheld = case spLockingKeys p of
      KeysAES128 -> ["K_AES_256"]
      KeysAES256 -> ["K_AES_128"]
      KeysBoth   -> []
    halves = [ uidUpper u | (n, u) <- [("K_AES_128", uK_AES_128Table), ("K_AES_256", uK_AES_256Table)], n `elem` unheld ]
    names r = any (\u -> uidUpper u `elem` halves) (objectUID r)
              || or [ n `T.isInfixOf` tshow cell | n <- unheld, (_, Given cell) <- map (second unmarked) (prCells r) ]
    objectUID r = maybeToList (L.lookup 0 (givenOf r) >>= refOf) <> maybeToList (invokingOf r)

-- | The generated table of a table UID.
generated :: Gen -> UID -> Maybe PreconfTable
generated g u = L.find ((== u) . tdTableUID . ptDesc) (gTables g)

-- | A generated table's rows, without their access lists.
rowsOf :: PreconfTable -> [PreconfRow]
rowsOf t = [ r | Pre r _ <- ptRows t ]

-- | The cells a generated row prints, given, a value with a note's number
--   after it the value.
givenOf :: PreconfRow -> Row
givenOf r = [ (c, cell) | (c, Given cell) <- map (second unmarked) (prCells r) ]

-- | A generated row's Name, as printed; none where its table has no Name
--   column or the row prints none.
nameOf :: PreconfTable -> PreconfRow -> Text
nameOf t r = case cellNamed (ptDesc t) "Name" (givenOf r) of
  Just (TextCell n) -> n
  _                 -> ""

-- | The generated table whose table UID's upper half is a half-UID.
tableOfHalf :: Gen -> HalfUID -> Maybe TableDesc
tableOfHalf g h = L.find ((== h) . uidUpper . tdTableUID) (map ptDesc (gTables g))

-- | The generated AccessControl table's rows (Table 22, PDF 39-46; Table
--   38, PDF 56-78).
accessRows :: Gen -> [PreconfRow]
accessRows g = maybe [] rowsOf (generated g uAccessControlTable)

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
printedPairs :: Gen -> [(UID, UID)]
printedPairs g = [ (inv, m) | r <- accessRows g, Plain <- [prKind r], Just inv <- [invokingOf r], Just m <- [methodOf r] ]

-- | The objects the generated tables give under some parameters, by table
--   UID, the AccessControl table aside: each printed row's UID, and each
--   template's instances' ("Hsed.Core.Preconfig".'instanceUID' of its base,
--   numbered as "Hsed.Sim.Preconfig".'instances' numbers a run's).
objectsOf :: SimParams -> Gen -> Map UID [UID]
objectsOf p g = Map.fromList
  [ (tdTableUID d, printed <> templated)
  | t <- gTables g, let d = ptDesc t, tdTableUID d /= uAccessControlTable
  , let printed   = [ u | r <- rowsOf t, Plain <- [prKind r], Just (UIDCell u) <- [L.lookup 0 (givenOf r)] ]
        templated = [ u | r <- rowsOf t, Template ph _ <- [prKind r], Just (UIDCell base) <- [L.lookup 0 (givenOf r)]
                        , n <- instances p (gSP g) d ph, Just u <- [instanceUID ph base n] ] ]

-- | The AccessControl rows the generated table gives under some parameters,
--   each by its InvokingID and MethodID with the ACL the generated row gives
--   it: the printed rows', then, in the table's order, a template's instances'
--   (numbered by its InvokingID's table, the ACL instantiated where the
--   template carries the placeholder in it, as the Locking SP's do: Table 38,
--   PDF 56-78) and an every-object row's for each
--   object whose UID begins with its half, among every object the generated
--   tables give; a pair a row before it gives counted once, the TPer holding
--   one row for each (Core Table 175, PDF 162: InvokingID and MethodID
--   unique).
accessOf :: SimParams -> Gen -> [((UID, UID), Maybe [UID])]
accessOf p g = L.nubBy ((==) `on` fst) (printed <> concatMap added (accessRows g))
  where
    objects = L.sort (concat (Map.elems (objectsOf p g)))
    printed = [ ((inv, m), aclOf r) | r <- accessRows g, Plain <- [prKind r], Just inv <- [invokingOf r], Just m <- [methodOf r] ]
    added r = case prKind r of
      Plain            -> []
      Template ph cols -> [ ((u, m), aclOf r { prCells = instantiate ph cols n (prCells r) })
                          | Just base <- [invokingOf r], Just m <- [methodOf r], Just d <- [tableOfHalf g (uidUpper base)]
                          , n <- instances p (gSP g) d ph, Just u <- [instanceUID ph base n] ]
      EveryObject half -> [ ((u, m), aclOf r) | Just m <- [methodOf r], u <- objects, uidUpper u == half ]

-- | Every row the SP holds under some parameters, derived from the generated
--   tables apart from the fold (the M4b review's model, its skeptic's S2;
--   since M5a, the Locking SP's too): each printed row complete (its cells,
--   a value with a note's number its value, of two key references the one
--   to the key table the parameters hold, the TPer's where it leaves them to
--   the TPer, every other column's default but the UID's), a template's
--   instances likewise (numbered by the table of its base, its UID or its
--   InvokingID), an every-object row once for each object whose UID begins
--   with its half, among the objects the others give, in UID order, a row of
--   an object or pair the table holds already left out, and the Table
--   table's NumColumns and Rows the described table's columns and rows (Core
--   5.3.2.3.7-5.3.2.3.8, PDF 157); and the parameters' overrides: the Locking
--   SP's LifeCycleState its OFS (Opal 2.30 5.2.2, PDF 96), LockingInfo's
--   MaxRanges and geometry the parameters' (Table 44, PDF 85-86); TPerInfo's
--   ProgrammaticResetEnable aside, which at these parameters keeps the cell
--   Table 27 prints.
expectedTables :: SimParams -> Gen -> Map UID [Row]
expectedTables p g = counts (Map.fromList [ (tdTableUID (ptDesc t), dedup t (concatMap (rowsFor t) (rowsOf t))) | t <- gTables g ])
  where
    complete t cells = overrides t (L.sortOn fst (given <> chosen <> defaults))
      where
        marked   = map (second unmarked) cells
        given    = [ (c, x) | (c, Given x) <- marked ] <> [ (c, x) | (c, OneOf xs) <- marked, Given x <- map unmarked xs, keyHeld x ]
        chosen   = [ (c, x) | (c, m) <- [ (c, m) | (c, VendorUnique m) <- marked ] <> [ (c, m) | (c, VendorAtLeast _ m) <- marked ]
                            , Just x <- [vendorCell p (gSP g) (ptDesc t) given c m] ]
        defaults = [ x | x@(c, _) <- ptDefaults t, c /= 0, c `notElem` map fst cells ]
    keyHeld x = (uidUpper <$> refOf x) == Just (uidUpper (if spLockingKeys p == KeysAES128 then uK_AES_128Table else uK_AES_256Table))
    overrides t r
      | tdTableUID (ptDesc t) == uSPTable, L.lookup 0 r == Just (UIDCell uSP_Locking) =
          put t "LifeCycleState" (EnumCell (if spLockingOFS p == Manufactured then "Manufactured" else "Manufactured-Inactive")
                                           (lifeCycleValue (spLockingOFS p))) r
      | tdTableUID (ptDesc t) == uLockingInfoTable =
          let geo = spGeometry p
          in put t "MaxRanges" (UIntCell (toInteger (spLockingRanges p)))
           . put t "AlignmentRequired" (if gAlign geo then EnumCell "True" 1 else EnumCell "False" 0)
           . put t "LogicalBlockSize" (UIntCell (toInteger (gLogicalBlockSize geo)))
           . put t "AlignmentGranularity" (UIntCell (toInteger (gGranularity geo)))
           . put t "LowestAlignedLBA" (UIntCell (toInteger (gLowestAligned geo))) $ r
      | otherwise = r
    put t name x r = case columnNamed name (ptDesc t) of
      Just col | maybe True (not . sameValue x) (L.lookup (colNumber col) r) -> L.sortOn fst ((colNumber col, x) : filter ((/= colNumber col) . fst) r)
      _ -> r
    sameValue (EnumCell _ a) (EnumCell _ b) = a == b
    sameValue a b                           = a == b
    invoking t = colNumber <$> columnNamed "InvokingID" (ptDesc t)
    baseOf t r = (L.lookup 0 (givenOf r) <|> (invoking t >>= (`L.lookup` givenOf r))) >>= refOf
    instancesOf t r ph cols =
      [ complete t (instantiate ph cols n (prCells r))
      | Just b <- [baseOf t r], Just d <- [tableOfHalf g (uidUpper b)], n <- instances p (gSP g) d ph, Just _ <- [instanceUID ph b n] ]
    objects = L.sort (L.nub [ u | t <- gTables g, r <- rowsOf t, built <- objectRows t r, Just (UIDCell u) <- [L.lookup 0 built] ])
    objectRows t r = case prKind r of
      Plain            -> [complete t (prCells r)]
      Template ph cols -> instancesOf t r ph cols
      EveryObject _    -> []
    rowsFor t r = case prKind r of
      Plain            -> [(True, complete t (prCells r))]
      Template ph cols -> map ((,) False) (instancesOf t r ph cols)
      EveryObject half -> [ (False, complete t ((c, Given (UIDCell u)) : filter ((/= c) . fst) (prCells r)))
                          | Just c <- [invoking t], u <- objects, uidUpper u == half ]
    identity t built = case L.lookup 0 built of
      Just x  -> [refOf x]
      Nothing -> [ L.lookup (colNumber col) built >>= refOf | col <- tdColumns (ptDesc t), colUnique col ]
    dedup t rows = go (Set.fromList [ identity t r | (True, r) <- rows ]) rows
      where
        go _ [] = []
        go seen ((printed', r) : more)
          | printed'                         = r : go seen more
          | identity t r `Set.member` seen   = go seen more
          | otherwise                        = r : go (Set.insert (identity t r) seen) more
    counts held = Map.adjust (map count) uTableTable held
      where
        column name = maybe 0 colNumber (generated g uTableTable >>= columnNamed name . ptDesc)
        byte r = case L.lookup (column "Kind") r of
          Just (EnumCell _ v) -> v == enumValue Core_table_kind_Byte
          _                   -> False
        count r
          | byte r    = [ (c, if c == column "NumColumns" && cell == UIntCell 0 then UIntCell 1 else cell) | (c, cell) <- r ]
          | otherwise = case L.lookup 0 r >>= refOf >>= \u -> L.find ((== uidLower u) . uidUpper . tdTableUID) (map ptDesc (gTables g)) of
              Just d  -> L.sortOn fst [ (c, if c == column "NumColumns" then UIntCell (toInteger (length (tdColumns d)))
                                           else if c == column "Rows" then UIntCell (toInteger (length (Map.findWithDefault [] (tdTableUID d) held)))
                                           else cell) | (c, cell) <- r ]
              Nothing -> r

-- | Each cell a generated table leaves to the TPer: its table, its row, its
--   column, the marker printed (@VU@, @MSID@, Table 41's @SID or MSID@, or
--   Table 36's value with its least).
vendorCells :: Gen -> [(PreconfTable, PreconfRow, ColNum, Text)]
vendorCells g = [ (t, r, c, m) | t <- gTables g, r <- rowsOf t, (c, pc) <- prCells r, Just m <- [markerOf (unmarked pc)] ]
  where
    markerOf = \case
      VendorUnique m    -> Just m
      VendorAtLeast _ m -> Just m
      _                 -> Nothing


-- | The columns of the Locking SP's tables to which the stand-in module
--   gives neither a default nor a printed cell, by table UID: the set-typed
--   reset columns hsedc cannot yet read (G1's Set_Type readings, Core Table
--   128's reset_types, PDF 119; M5a's integration replaces the stand-in).
--   Pending: an empty list once the generated module lands.
standInGaps :: [(UID, [ColNum])]
standInGaps = [(uLockingTable, [0x09, 0x10, 0x13]), (uMBRControlTable, [0x03])]


-- * The simulator's SPs

-- | The SP a run starts with, by its UID.
spOf :: UID -> SimParams -> SPState
spOf u p = fromMaybe (error ("the simulator holds no SP " <> show u)) (Map.lookup u (tsSPs (initialState p)))

-- | The Admin SP a run starts with.
adminSPOf :: SimParams -> SPState
adminSPOf = spOf uSP_Admin

-- | The rows the SP holds of a table.
heldRows :: SPState -> UID -> [Row]
heldRows sps table = fromMaybe [] (Map.lookup table (spsTables sps))

-- | The row the SP holds of an object.
heldRow :: SPState -> UID -> UID -> Maybe Row
heldRow sps table u = L.find ((== Just (UIDCell u)) . L.lookup 0) (heldRows sps table)

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

-- | The Table table's row of a table, its descriptor object: 00 00 00 01
--   and the table's half-UID (Core 3.2.5.3 b.i, PDF 51-52).
halfObject :: UID -> UID
halfObject table = fromMaybe table (L.find ((== uidUpper table) . uidLower) [ u | r <- maybe [] rowsOf (generated adminGen uTableTable), Just u <- [L.lookup 0 (givenOf r) >>= refOf] ])

-- | The parameters with so many Admin authorities.
admins :: Natural -> SimParams
admins n = defaultSimParams { spAdminSPAdmins = n }

-- | The parameters of a run at each Original Factory State and with each
--   choice of key tables, the others the defaults.
everyLocking :: [SimParams]
everyLocking = [ defaultSimParams { spLockingOFS = o, spLockingKeys = k } | o <- [minBound .. maxBound], k <- [minBound .. maxBound] ]

-- | Both SPs, each with the parameters it is held to the generated tables
--   under: the Admin SP at the defaults and with three Admin authorities;
--   the Locking SP at either OFS with each choice of key tables, and with
--   ten Admins, twelve Users and sixteen ranges.
bothWays :: [(Text, SimParams, Gen)]
bothWays = [ ("the Admin SP", p, adminGen) | p <- [defaultSimParams, admins 3] ]
  <> [ ("the Locking SP " <> tshow (spLockingOFS p, spLockingKeys p), p, lockingGen p) | p <- everyLocking ]
  <> [ ("the Locking SP, 10 Admins, 12 Users, 16 ranges", grown, lockingGen grown) ]
  where grown = defaultSimParams { spLockingAdmins = 10, spLockingUsers = 12, spLockingRanges = 16 }

spec :: Spec
spec = describe "the generated Admin and Locking SPs and the simulator's" $ do
  let sps0 = adminSPOf defaultSimParams

  describe "the tables and their rows, both ways (both SPs since M5a: the Locking SP at either OFS and with each choice of key tables)" $ do
    it "hold every generated table, by its UID, with its description, and no other, whatever the parameters; the Locking SP the key tables the parameters choose" $
      forM_ bothWays $ \(what, p, g) -> do
        (what, spsDescs (spOf (gSP g) p)) `shouldBe` (what, Map.fromList [ (tdTableUID (ptDesc t), ptDesc t) | t <- gTables g ])
        Map.keys (spsTables (spOf (gSP g) p)) `shouldBe` Map.keys (spsDescs (spOf (gSP g) p))
    it "hold every object the generated rows give, a printed row's or a template's instance's, by its UID, and no other" $
      forM_ bothWays $ \(what, p, g) ->
        (what, Map.map L.sort (Map.delete uAccessControlTable (Map.map (map (\r -> L.lookup 0 r >>= refOf)) (spsTables (spOf (gSP g) p)))))
          `shouldBe` (what, Map.map (L.sort . map Just) (objectsOf p g))
    it "hold every AccessControl row the generated rows give, printed, a template's instance or one for every object, by InvokingID and MethodID, and no other" $
      forM_ bothWays $ \(what, p, g) -> do
        let held = map pairOf (heldRows (spOf (gSP g) p) uAccessControlTable)
        (what, L.sort held) `shouldBe` (what, L.sort (map (Just . fst) (accessOf p g)))
        L.nub held `shouldBe` held
    it "read each AccessControl row's ACL as the generated row gives it (Hsed.Sim.Access.accessControlList), none empty" $
      forM_ bothWays $ \(what, p, g) -> do
        let sps = spOf (gSP g) p
        (what, [ (pr, accessControlList sps (fst pr) (snd pr)) | (pr, _) <- accessOf p g ]) `shouldBe` (what, accessOf p g)
        [ pr | (pr, acl) <- accessOf p g, maybe True null acl ] `shouldBe` []
    it "are 12 tables and 116 rows at the defaults: T22's 22 printed rows and 46 for every object of seven tables, no template's instance" $ do
      [ (tdName (ptDesc t), length (heldRows sps0 (tdTableUID (ptDesc t)))) | t <- adminSP ] `shouldBe`
        [ ("SPInfo", 1), ("SPTemplates", 2), ("Table", 12), ("MethodID", 8), ("AccessControl", 68), ("ACE", 10), ("Authority", 5)
        , ("C_PIN", 3), ("TPerInfo", 1), ("Template", 3), ("SP", 2), ("DataRemovalMechanism", 1) ]
      sum (map length (Map.elems (spsTables sps0))) `shouldBe` 116
      let kinds = map prKind (accessRows adminGen)
          added = [ u | Just (u, m) <- map pairOf (heldRows sps0 uAccessControlTable), (u, m) `notElem` printedPairs adminGen ]
      (length [ () | Plain <- kinds ], length [ () | EveryObject _ <- kinds ], length [ () | Template _ _ <- kinds ]) `shouldBe` (22, 9, 3)
      length (printedPairs adminGen) `shouldBe` 22
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
    it "are, in the Locking SP at the defaults, 393 rows over 13 object tables with K_AES_256 alone and 451 over 14 with both (the M5 plan's fold model, Opal 2.30 Tables 34-48, PDF 52-89)" $ do
      let tablesAt k = [ (tdName (ptDesc t), length (heldRows sps (tdTableUID (ptDesc t))))
                       | let sps = spOf uSP_Locking defaultSimParams { spLockingKeys = k }, t <- gTables (lockingGen defaultSimParams { spLockingKeys = k }) ]
      tablesAt KeysAES256 `shouldBe`
        [ ("SPInfo", 1), ("SPTemplates", 2), ("Table", 15), ("MethodID", 8), ("AccessControl", 250), ("ACE", 69), ("Authority", 15)
        , ("C_PIN", 12), ("SecretProtect", 1), ("LockingInfo", 1), ("Locking", 9), ("MBRControl", 1), ("K_AES_256", 9) ]
      sum (map snd (tablesAt KeysAES256)) `shouldBe` 393
      tablesAt KeysBoth `shouldBe`
        [ ("SPInfo", 1), ("SPTemplates", 2), ("Table", 16), ("MethodID", 8), ("AccessControl", 288), ("ACE", 78), ("Authority", 15)
        , ("C_PIN", 12), ("SecretProtect", 2), ("LockingInfo", 1), ("Locking", 9), ("MBRControl", 1), ("K_AES_128", 9), ("K_AES_256", 9) ]
      sum (map snd (tablesAt KeysBoth)) `shouldBe` 451
      map (length . tablesAt) [KeysAES128, KeysAES256, KeysBoth] `shouldBe` [13, 13, 14]

  describe "the cells" $ do
    it "are every column of the row's table, but an AccessControl row's UID, which the TPer assigns (Core 5.3.2.7.1, PDF 162), and the columns the Locking SP's stand-in gives no default" $
      forM_ bothWays $ \(what, p, g) ->
        forM_ (Map.toList (spsTables (spOf (gSP g) p))) $ \(table, rows) -> do
          let columns = maybe [] (map colNumber . tdColumns . ptDesc) (generated g table)
              gaps    = if gSP g == uSP_Locking then fromMaybe [] (L.lookup table standInGaps) else []
              wanted  = filter (`notElem` gaps) (if table == uAccessControlTable then filter (/= 0) columns else columns)
          (what, map (map fst) rows) `shouldBe` (what, map (const wanted) rows)
    it "lack, in the Locking SP's stand-in, only the set-typed reset columns no row prints and no default gives (Table 45's LockOnReset, ContOnReset and GeneralStatus; Table 46's DoneOnReset, PDF 86-87): G1's Set_Type readings and the generated module retire them" $
      [ (tdTableUID (ptDesc t), missing) | t <- LockingSP.lockingSP
                                         , let printed = L.nub (map fst (ptDefaults t) <> [ c | r <- rowsOf t, (c, _) <- prCells r ])
                                               missing = [ colNumber c | c <- tdColumns (ptDesc t), colNumber c /= 0, colNumber c `notElem` printed ]
                                         , not (null missing) ]
        `shouldBe` standInGaps
    it "are, in every row the SP holds, printed, a template's instance or one for every object, each cell printed, the TPer's, its column's default, the parameters' or the Table table's count, in order (the M4b review; both SPs since M5a)" $
      forM_ bothWays $ \(what, p, g) ->
        forM_ (Map.toList (expectedTables p g)) $ \(table, rows) ->
          (what, table, Map.lookup table (spsTables (spOf (gSP g) p))) `shouldBe` (what, table, Just rows)
    it "count, in the Table table's rows, each table's columns and rows: NumColumns and Rows, which the preconfiguration leaves blank and Core states (5.3.2.3.7-5.3.2.3.8, PDF 157)" $ do
      let counted sps table = do
            desc <- ptDesc <$> generated adminGen uTableTable
            held <- heldRow sps uTableTable (halfObject table)
            (,) <$> cellNamed desc "NumColumns" held <*> cellNamed desc "Rows" held
      map (counted sps0) [uAccessControlTable, uTableTable, uAuthorityTable]
        `shouldBe` [Just (UIntCell 15, UIntCell 68), Just (UIntCell 15, UIntCell 12), Just (UIntCell 19, UIntCell 5)]
      counted (adminSPOf (admins 3)) uAuthorityTable `shouldBe` Just (UIntCell 19, UIntCell 7)
    it "name, in every every-object row's half and every template's base, a table the SP holds, so that none expands to nothing unread (both SPs, each choice of key tables)" $
      forM_ bothWays $ \(what, _, g) -> do
        (what, [ h | r <- concatMap rowsOf (gTables g), EveryObject h <- [prKind r], isNothing (tableOfHalf g h) ]) `shouldBe` (what, [])
        (what, [ (tdName (ptDesc t), b) | t <- gTables g, r <- rowsOf t, Template _ _ <- [prKind r]
                                        , Just b <- [L.lookup 0 (givenOf r) <|> (columnNamed "InvokingID" (ptDesc t) >>= \c -> L.lookup (colNumber c) (givenOf r)) >>= refOf]
                                        , isNothing (tableOfHalf g (uidUpper b)) ]) `shouldBe` (what, [])
    it "leave nothing to the TPer: the eight VU and MSID cells the Admin SP's tables print (PDF 48-52) are each vendorCell's" $ do
      [ (tdName (ptDesc t), nameOf t r, maybe "" colName (L.find ((== c) . colNumber) (tdColumns (ptDesc t))), m) | (t, r, c, m) <- vendorCells adminGen ]
        `shouldBe` [ ("C_PIN", "C_PIN_SID", "PIN", "VU"), ("C_PIN", "C_PIN_SID", "TryLimit", "VU"), ("C_PIN", "C_PIN_SID", "Tries", "VU")
                   , ("C_PIN", "C_PIN_MSID", "PIN", "MSID"), ("TPerInfo", "", "GUDID", "VU"), ("Template", "Base", "Instances", "VU")
                   , ("Template", "Base", "MaxInstances", "VU"), ("DataRemovalMechanism", "", "ActiveDataRemovalMechanism", "VU") ]
      [ (tdName (ptDesc t), c, m) | (t, r, c, m) <- vendorCells adminGen, isNothing (vendorCell defaultSimParams uSP_Admin (ptDesc t) (givenOf r) c m) ] `shouldBe` []
    it "leave nothing to the TPer in the Locking SP: its 21 cells, VU, a size with its least (Table 36, PDF 54) and C_PIN_Admin1's SID or MSID (Table 41, PDF 83), each vendorCell's" $ do
      let g = lockingGen defaultSimParams { spLockingKeys = KeysBoth }
      [ (tdName (ptDesc t), nameOf t r, maybe "" colName (L.find ((== c) . colNumber) (tdColumns (ptDesc t))), m) | (t, r, c, m) <- vendorCells g ]
        `shouldBe` [ ("Table", "MBR", "Rows", "0x08000000 min"), ("Table", "MBR", "MandatoryWriteGranularity", "VU")
                   , ("Table", "MBR", "RecommendedAccessGranularity", "VU"), ("Table", "DataStore", "Rows", "0x00A00000 min")
                   , ("Table", "DataStore", "MandatoryWriteGranularity", "VU"), ("Table", "DataStore", "RecommendedAccessGranularity", "VU")
                   , ("C_PIN", "C_PIN_Admin1", "PIN", "SID or MSID"), ("SecretProtect", "", "ProtectMechanisms", "VU")
                   , ("SecretProtect", "", "ProtectMechanisms", "VU") ]
                   <> concat [ [ ("K_AES_" <> b, "K_AES_" <> b <> "_" <> rng <> "_Key", "Key", "VU"), ("K_AES_" <> b, "K_AES_" <> b <> "_" <> rng <> "_Key", "Mode", "VU") ]
                             | b <- ["128", "256"], rng <- ["GlobalRange", "Range1", "RangeNNNN"] ]
      [ (tdName (ptDesc t), c, m) | (t, r, c, m) <- vendorCells g, isNothing (vendorCell defaultSimParams uSP_Locking (ptDesc t) (givenOf r) c m) ] `shouldBe` []
    it "have vendorCell answer for no column and marker the generated tables do not leave to the TPer" $
      forM_ [adminGen, lockingGen defaultSimParams { spLockingKeys = KeysBoth }] $ \g -> do
        let printedVU = [ (tdTableUID (ptDesc t), c, m) | (t, _, c, m) <- vendorCells g ]
            markers   = L.nub ("VU" : "MSID" : [ m | (_, _, _, m) <- vendorCells g ])
        [ (tdName (ptDesc t), colName col, m)
          | t <- gTables g, r <- rowsOf t, col <- tdColumns (ptDesc t), m <- markers
          , isJust (vendorCell defaultSimParams (gSP g) (ptDesc t) (givenOf r) (colNumber col) m)
          , (tdTableUID (ptDesc t), colNumber col, m) `notElem` printedVU ]
          `shouldBe` []

  describe "the Locking SP at its Original Factory State (Opal 2.30 5.2.2, PDF 96; Table 41 note 1, PDF 83-84)" $ do
    it "gives C_PIN_Admin1, the Locking SP's own (00 00 00 0B 00 01 00 01), the MSID at either OFS, and the Admin SP's C_PIN_Admin1 (00 00 00 0B 00 00 02 01) its printed empty PIN" $ do
      forM_ [ManufacturedInactive, Manufactured] $ \o ->
        (heldRow (spOf uSP_Locking defaultSimParams { spLockingOFS = o }) uC_PINTable LockingSP.uC_PIN_Admin1 >>= cellNamed c_PINTableDesc "PIN")
          `shouldBe` Just (BytesCell (spMsid defaultSimParams))
      (LockingSP.uC_PIN_Admin1, uC_PIN_Admin1) `shouldBe` (uid 0x00 0x00 0x00 0x0B 0x00 0x01 0x00 0x01, uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x02 0x01)
      (heldRow sps0 uC_PINTable uC_PIN_Admin1 >>= cellNamed c_PINTableDesc "PIN") `shouldBe` Just (BytesCell "")
    it "is Manufactured, 9, with --locking-ofs manufactured, and Manufactured-Inactive, 8, by default, in the Admin SP's SP table (Table 29, PDF 50; Table 49, PDF 98)" $
      map (\o -> spLifeCycle uSP_Locking (initialState defaultSimParams { spLockingOFS = o })) [ManufacturedInactive, Manufactured]
        `shouldBe` [Just 8, Just 9]

  describe "UCT-03's pair, on the generated rows (Table 22 rows 23 and 25, PDF 42; Table 23, PDF 47)" $ do
    let booleanExpr ace = heldRow sps0 uACETable ace >>= cellNamed aceTableDesc "BooleanExpr"
    it "Get on C_PIN_MSID: ACE_C_PIN_MSID_Get_PIN, which grants Anybody the UID and the PIN, [0, 3]" $ do
      accessControlList sps0 uC_PIN_MSID uMethodID_Get `shouldBe` Just [uACE_C_PIN_MSID_Get_PIN]
      booleanExpr uACE_C_PIN_MSID_Get_PIN `shouldBe` Just (ListCell [RefCell "Anybody" uAuthority_Anybody])
      aceGrant sps0 (authenticated sps0 []) uACE_C_PIN_MSID_Get_PIN `shouldBe` Just [0, 3]
    it "Get on C_PIN_SID: ACE_C_PIN_SID_Get_NOPIN, which grants Admins OR SID its UID, CharSet, TryLimit, Tries and Persistence, [0, 4, 5, 6, 7], not its Name, CommonName or PIN, and Anybody none" $ do
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

  describe "one generated name for each object (the lead's decision at the M4b review, design 13.32 (22))" $
    it "names Admin1 itself at the UID Core Table 243 gives Issuers: another object there, an individual authority of the class Admins (Opal Table 24, PDF 48), where Core's Admin template has its class authority Issuers (Table 216, PDF 236)" $ do
      uAuthority_Admin1 `shouldBe` uAuthority_Issuers
      let admin1 column = heldRow sps0 uAuthorityTable uAuthority_Admin1 >>= cellNamed authorityTableDesc column
      map admin1 ["Name", "IsClass", "Class"] `shouldBe` [Just (TextCell "Admin1"), Just (EnumCell "F" 0), Just (RefCell "Admins" uAuthority_Admins)]

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
