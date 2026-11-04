{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.PreconfigSpec
Description : Test specifications for Hsed.Sim.Preconfig: an SP's tables folded from its preconfiguration tables
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The fold of M4b (design 8.1 and 8.3), the Admin SP's tables the generated
ones (held to the initial state both ways in Hsed.Gen.Opal.PreconfigSpec),
on preconfiguration tables built here as M4b's generated ones have them (the
cells' form the lead decided for M4b, design 13.32: an every-object row
without an InvokingID cell, a template's UID or InvokingID its base,
MethodID and the ACLs references, no default for the UID), from Opal 2.30's
Admin SP (Tables 22-25 and 27-29, PDF 39-50, and Table 33, PDF 52): each
table's description by the table's UID, and its rows in the order the
tables give them, each the cells its table prints, the TPer's where it
leaves them to the TPer (C_PIN_SID's PIN the MSID or the SID's, Opal
4.2.1.8, PDF 49), every other column's default but an AccessControl row's
UID (Core 5.3.4.2.2, PDF 223; 5.3.2.7.1, PDF 162; design 13.31 (a)), a
template's instances for each Admin authority past Admin1 (PDF 47), a row
for every object of a table (*AC1-*AC7, PDF 38-39) and the parameters'
overrides of the Locking SP's life cycle and of TPerInfo's
ProgrammaticResetEnable; the Table table's counts, a byte table's NumColumns
0x01 (Core 5.3.2.3.7, PDF 157, M5's MBR and DataStore); and the active data
removal mechanism one Level 0 reports. Since M5a's scaffold, the Locking
SP's tables, folded from its generated preconfiguration (Opal 2.30 Tables
34-42 and 44-48, PDF 52-89) at either Original Factory State and with each
choice of key tables: no problem, the same tables either way, a value
marked with a note its value, the ActiveKey the key tables decide, the MBR
and DataStore sizes and the byte tables they give.
-}
module Hsed.Sim.PreconfigSpec (spec) where

import           Data.Bits               (testBit)
import qualified RIO.List                as L
import qualified RIO.Map                 as Map
import           RIO
import qualified RIO.Text                as T
import           Test.Hspec

import           Hsed.Core.Feature       (Feature (..), decodeFeatureStrict)
import           Hsed.Core.Layout        (LayoutError)
import           Hsed.Core.Preconfig     (Placeholder (..), PreCell (..), PreconfRow (..), PreconfTable (..), RowKind (..))
import           Hsed.Core.Record        (DecodedRecord (..))
import           Hsed.Core.Table         (Cell (..), Col (..), ColNum, Pre (..), Row, TableDesc (..), columnNamed)
import           Hsed.Core.TypeName      (TypeName (..))
import           Hsed.Core.Types         (enumValue)
import           Hsed.Core.UID           (UID, halfUID, uNull, uid, uidUpper)
import           Hsed.Gen.Core.TableDescs (aceTableDesc, accessControlTableDesc, authorityTableDesc, c_PINTableDesc, lockingTableDesc,
                                          spTableDesc, tPerInfoTableDesc, templateTableDesc)
import           Hsed.Gen.Core.Enums     (Core_table_kind (..))
import           Hsed.Gen.Core.TableUIDs (uAccessControlTable, uAuthorityTable, uC_PINTable, uK_AES_128Table, uLockingInfoTable,
                                          uLockingTable, uMBRTable, uMBRTableObject, uSPTable, uTPerInfoTable, uTableTable, uTemplateTable)
import           Hsed.Gen.Core.Types     (ubooleanType)
import           Hsed.Gen.Core.UIDs      (uAuthority_Admins, uAuthority_Anybody, uAuthority_Makers, uAuthority_SID, uC_PIN_SID,
                                          uLocking_GlobalRange, uMethodID_Get, uMethodID_Set, uSP_Admin, uTemplate_Admin,
                                          uTemplate_Base, uTemplate_Locking)
import           Hsed.Gen.Opal.Features  (Level0DiscoverySupportedDataRemovalMechanismFeature (..))
import qualified Hsed.Gen.Opal.Preconfig.AdminSP as Generated
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Gen.Opal.TableDescs (lockingInfoTableDesc, tableTableDesc)
import           Hsed.Level0             (Descriptor (..))
import           Hsed.Sim.Level0         (simDescriptors)
import           Hsed.Sim.Preconfig      (FoldProblem (..), PreconfSP (..), foldTables, instances, vendorCell)
import           Hsed.Sim.Types          (ByteTable (..), Geometry (..), LifeCycle (..), LockingKeys (..), SPState (..), SimParams (..), TPerState (..),
                                          defaultSimParams, initialState, initialStateProblems, lifeCycleValue)


-- * The scaffold's tables

-- | A plain row of printed cells, with no access.
plain :: [(Int, Cell)] -> Pre PreconfRow
plain cells = Pre (PreconfRow Plain False [ (c, Given cell) | (c, cell) <- cells ] []) []

-- | A C_PIN table whose CharSet, TryLimit, Tries and Persistence (columns 4
--   to 7) have defaults, the zero values of their types (Core 5.3.4.1.1.1-2,
--   PDF 210; the boolean False), and two rows that give some of them.
c_PIN :: PreconfTable
c_PIN = PreconfTable c_PINTableDesc defaults
  [ plain [(0, UIDCell msid), (1, TextCell "C_PIN_MSID"), (3, BytesCell "an MSID")]
  , plain [(5, UIntCell 3), (0, UIDCell sid), (1, TextCell "C_PIN_SID")] ]
  where defaults = [(4, RefCell "Null" uNull), (5, UIntCell 0), (6, UIntCell 0), (7, EnumCell "False" 0)]

msid, sid :: UID
msid = uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x84 0x02
sid  = uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x00 0x01

-- | An SP table with no defaults and one row.
sp :: PreconfTable
sp = PreconfTable spTableDesc [] [plain [(0, UIDCell (uid 0x00 0x00 0x02 0x05 0x00 0x00 0x00 0x01)), (1, TextCell "Admin")]]

-- | The fold of tables as the Admin SP's preconfiguration, its problems an
--   error: since M5a 'foldTables' takes an SP's preconfiguration whole and
--   returns every problem that keeps it from folding.
foldOf :: SimParams -> [PreconfTable] -> (Map UID TableDesc, Map UID [Row])
foldOf p tables = either (error . show) id (foldTables p (PreconfSP uSP_Admin tables))

-- | The rows a fold gives a table, by its description.
rowsIn :: [PreconfTable] -> TableDesc -> [Row]
rowsIn tables desc = fromMaybe [] (Map.lookup (tdTableUID desc) (snd (foldOf defaultSimParams tables)))


-- * Opal 2.30's Admin SP, in part, as M4b's generated preconfiguration has it

-- | A row of a kind and of cells by column name, with no access.
rowOf :: TableDesc -> RowKind -> [(Text, PreCell)] -> Pre PreconfRow
rowOf desc kind cells = Pre (PreconfRow kind False (L.sortOn fst [ (number desc name, c) | (name, c) <- cells ]) []) []

-- | A plain row of cells by column name.
printed :: TableDesc -> [(Text, PreCell)] -> Pre PreconfRow
printed desc = rowOf desc Plain

-- | A template row, @(+XX)@, carrying the placeholder in the columns named.
template :: TableDesc -> [Text] -> [(Text, PreCell)] -> Pre PreconfRow
template desc names = rowOf desc (Template xx (map (number desc) names))

-- | Table 24's and 25's placeholder, @XX@ (PDF 48-49).
xx :: Placeholder
xx = Placeholder "XX" 1

-- | The number of a column of a name; a name the description lacks is the test's error.
number :: TableDesc -> Text -> ColNum
number desc name = maybe (error ("no column " <> T.unpack name <> " in " <> T.unpack (tdName desc))) colNumber (columnNamed name desc)

-- | Each column's default but the UID's, the zero of its type as M4b's
--   generated preconfiguration gives it where Core states it (Core
--   5.3.4.1.1.1-2, PDF 210: a CharSet of zeroes, the Null reference; Tries
--   and TryLimit 0; the boolean False), and a cell naming the column for the
--   others.
defaultsOf :: TableDesc -> Row
defaultsOf desc = [ (colNumber c, zero c) | c <- tdColumns desc, colNumber c /= 0 ]
  where
    zero c
      | t == "boolean" || "boolean_def" `T.isPrefixOf` t        = EnumCell "False" 0
      | "uinteger" `T.isPrefixOf` t                              = UIntCell 0
      | "_ref" `T.isSuffixOf` t || "_uidref" `T.isSuffixOf` t    = RefCell "Null" uNull
      | otherwise                                                = TextCell ("the default of " <> colName c)
      where t = typeNameText (colType c)

-- | A table of a description, its defaults, its rows.
tableOf :: TableDesc -> [Pre PreconfRow] -> PreconfTable
tableOf desc = PreconfTable desc (defaultsOf desc)

-- | Cells as a table prints them.
uidG :: UID -> PreCell
uidG = Given . UIDCell

textG :: Text -> PreCell
textG = Given . TextCell

refG :: Text -> UID -> PreCell
refG name = Given . RefCell name

vu :: PreCell
vu = VendorUnique "VU"

nullG, falseG, trueG :: PreCell
nullG  = refG "Null" uNull
falseG = Given (EnumCell "False" 0)
trueG  = Given (EnumCell "True" 1)

-- | UIDs of Opal 2.30's Admin SP that no generated Core module holds, the
--   generated Admin SP's where it names them ("Hsed.Gen.Opal.Preconfig.AdminSP":
--   Admin1, its credential and C_PIN_MSID, Tables 24-25, PDF 48-49; the ACEs,
--   Table 23, PDF 46-47; the Locking SP, Table 29, PDF 50; since the M4b
--   review, so that these tables are held to the PDF's values).
uAdmin1, uC_PIN_Admin1, uC_PIN_MSID, uLocking :: UID
uAdmin1       = Generated.uAuthority_Admin1
uC_PIN_Admin1 = Generated.uC_PIN_Admin1
uC_PIN_MSID   = Generated.uC_PIN_MSID
uLocking      = Generated.uSP_Locking

uACE_Anybody, uACE_Set_Enabled, uACE_C_PIN_SID_Get_NOPIN, uACE_C_PIN_MSID_Get_PIN, uACE_C_PIN_Admins_Set_PIN :: UID
uACE_Anybody               = Generated.uACE_Anybody
uACE_Set_Enabled           = Generated.uACE_Set_Enabled
uACE_C_PIN_SID_Get_NOPIN   = Generated.uACE_C_PIN_SID_Get_NOPIN
uACE_C_PIN_MSID_Get_PIN    = Generated.uACE_C_PIN_MSID_Get_PIN
uACE_C_PIN_Admins_Set_PIN  = Generated.uACE_C_PIN_Admins_Set_PIN

-- | The UIDs no generated module names, hand-written with their pages: the
--   AdminXX template's base and its credential's (Tables 24-25, PDF 48-49: a
--   template's base is no object's, and has no name), TPerInfo's row (Table
--   27, PDF 50) and DataRemovalMechanism's row (Table 33, PDF 52), rows with
--   no Name.
uAdminXX, uC_PIN_AdminXX, uTPerInfoRow, uDataRemovalRow :: UID
uAdminXX        = uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00
uC_PIN_AdminXX  = uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x02 0x00
uTPerInfoRow    = uid 0x00 0x00 0x02 0x01 0x00 0x03 0x00 0x01
uDataRemovalRow = uid 0x00 0x00 0x11 0x01 0x00 0x00 0x00 0x01

-- | Table 24, the Admin SP's Authority table (PDF 48): its six rows, AdminXX
--   the template, its UID, Name and Credential carrying the placeholder.
authority :: PreconfTable
authority = tableOf d
  [ printed d [ ("UID", uidG uAuthority_Anybody), ("Name", textG "Anybody"), ("IsClass", falseG), ("Class", nullG), ("Enabled", trueG) ]
  , printed d [ ("UID", uidG uAuthority_Admins), ("Name", textG "Admins"), ("IsClass", trueG), ("Class", nullG), ("Enabled", trueG) ]
  , printed d [ ("UID", uidG uAuthority_Makers), ("Name", textG "Makers"), ("IsClass", trueG), ("Class", nullG), ("Enabled", trueG) ]
  , printed d [ ("UID", uidG uAuthority_SID), ("Name", textG "SID"), ("IsClass", falseG), ("Class", nullG), ("Enabled", trueG)
              , ("Credential", refG "C_PIN_SID" uC_PIN_SID) ]
  , printed d [ ("UID", uidG uAdmin1), ("Name", textG "Admin1"), ("IsClass", falseG), ("Class", refG "Admins" uAuthority_Admins)
              , ("Enabled", falseG), ("Credential", refG "C_PIN_Admin1" uC_PIN_Admin1) ]
  , template d ["UID", "Name", "Credential"]
              [ ("UID", uidG uAdminXX), ("Name", textG "AdminXX"), ("IsClass", falseG), ("Class", refG "Admins" uAuthority_Admins)
              , ("Enabled", falseG), ("Credential", refG "C_PIN_AdminXX" uC_PIN_AdminXX) ]
  ]
  where d = authorityTableDesc

-- | Table 25, the Admin SP's C_PIN table (PDF 48-49): C_PIN_SID's PIN,
--   TryLimit and Tries VU, C_PIN_MSID's PIN the MSID and its other cells
--   blank, C_PIN_Admin1, and C_PIN_AdminXX the template.
cPIN :: PreconfTable
cPIN = tableOf d
  [ printed d [ ("UID", uidG uC_PIN_SID), ("Name", textG "C_PIN_SID"), ("PIN", vu), ("CharSet", nullG), ("TryLimit", vu), ("Tries", vu)
              , ("Persistence", falseG) ]
  , printed d [ ("UID", uidG uC_PIN_MSID), ("Name", textG "C_PIN_MSID"), ("PIN", VendorUnique "MSID") ]
  , printed d [ ("UID", uidG uC_PIN_Admin1), ("Name", textG "C_PIN_Admin1"), ("PIN", Given (BytesCell "")), ("CharSet", nullG)
              , ("TryLimit", Given (UIntCell 0)), ("Tries", Given (UIntCell 0)), ("Persistence", falseG) ]
  , template d ["UID", "Name"]
              [ ("UID", uidG uC_PIN_AdminXX), ("Name", textG "C_PIN_AdminXX"), ("PIN", Given (BytesCell "")), ("CharSet", nullG)
              , ("TryLimit", Given (UIntCell 0)), ("Tries", Given (UIntCell 0)), ("Persistence", falseG) ]
  ]
  where d = c_PINTableDesc

-- | Table 22, the Admin SP's AccessControl table (PDF 39-46), in part: Get
--   on every Authority object (*AC5, PDF 40), Set on Admin1 and on AdminXX
--   (PDF 41), Get on C_PIN_SID and C_PIN_MSID (PDF 42), Get and Set on
--   C_PIN_Admin1 and C_PIN_AdminXX (PDF 43), Get on every Template object
--   (*AC6, PDF 44) and every SP object (*AC7, PDF 45). No row prints a UID.
accessControl :: PreconfTable
accessControl = tableOf d
  [ every (halfUID 0x00 0x00 0x00 0x09) "Get" uMethodID_Get [("ACE_Anybody", uACE_Anybody)]
  , fixed uAdmin1 "Set" uMethodID_Set [("ACE_Set_Enabled", uACE_Set_Enabled)]
  , templated uAdminXX "Set" uMethodID_Set [("ACE_Set_Enabled", uACE_Set_Enabled)]
  , fixed uC_PIN_SID "Get" uMethodID_Get [("ACE_C_PIN_SID_Get_NOPIN", uACE_C_PIN_SID_Get_NOPIN)]
  , fixed uC_PIN_MSID "Get" uMethodID_Get [("ACE_C_PIN_MSID_Get_PIN", uACE_C_PIN_MSID_Get_PIN)]
  , fixed uC_PIN_Admin1 "Get" uMethodID_Get [("ACE_C_PIN_SID_Get_NOPIN", uACE_C_PIN_SID_Get_NOPIN)]
  , templated uC_PIN_AdminXX "Get" uMethodID_Get [("ACE_C_PIN_SID_Get_NOPIN", uACE_C_PIN_SID_Get_NOPIN)]
  , fixed uC_PIN_Admin1 "Set" uMethodID_Set [("ACE_C_PIN_Admins_Set_PIN", uACE_C_PIN_Admins_Set_PIN)]
  , templated uC_PIN_AdminXX "Set" uMethodID_Set [("ACE_C_PIN_Admins_Set_PIN", uACE_C_PIN_Admins_Set_PIN)]
  , every (halfUID 0x00 0x00 0x02 0x04) "Get" uMethodID_Get [("ACE_Anybody", uACE_Anybody)]
  , every (halfUID 0x00 0x00 0x02 0x05) "Get" uMethodID_Get [("ACE_Anybody", uACE_Anybody)]
  ]
  where
    d = accessControlTableDesc
    cells method m acl = [ ("MethodID", refG method m), ("ACL", Given (ListCell [ RefCell n u | (n, u) <- acl ]))
                         , ("GetACLACL", Given (ListCell [RefCell "ACE_Anybody" uACE_Anybody])) ]
    fixed invoker method m acl     = printed d (("InvokingID", uidG invoker) : cells method m acl)
    templated invoker method m acl = template d ["InvokingID"] (("InvokingID", uidG invoker) : cells method m acl)
    every half method m acl        = rowOf d (EveryObject half) (cells method m acl)

-- | Table 28, the Admin SP's Template table (PDF 50): Base's Instances and
--   MaxInstances VU.
templates :: PreconfTable
templates = tableOf d
  [ printed d [ ("UID", uidG uTemplate_Base), ("Name", textG "Base"), ("RevisionNumber", Given (UIntCell 1)), ("Instances", vu), ("MaxInstances", vu) ]
  , printed d [ ("UID", uidG uTemplate_Admin), ("Name", textG "Admin"), ("RevisionNumber", Given (UIntCell 1)), ("Instances", Given (UIntCell 1))
              , ("MaxInstances", Given (UIntCell 1)) ]
  , printed d [ ("UID", uidG uTemplate_Locking), ("Name", textG "Locking"), ("RevisionNumber", Given (UIntCell 1)), ("Instances", Given (UIntCell 1))
              , ("MaxInstances", Given (UIntCell 1)) ]
  ]
  where d = templateTableDesc

-- | Table 29, the Admin SP's SP table (PDF 50): the Admin SP Manufactured,
--   the Locking SP Manufactured-Inactive (Table 49's 9 and 8, PDF 98).
sps :: PreconfTable
sps = tableOf d
  [ printed d [ ("UID", uidG uSP_Admin), ("Name", textG "Admin"), ("LifeCycleState", Given (EnumCell "Manufactured" 9)), ("Frozen", falseG) ]
  , printed d [ ("UID", uidG uLocking), ("Name", textG "Locking"), ("LifeCycleState", Given (EnumCell "Manufactured-Inactive" 8))
              , ("Frozen", falseG) ]
  ]
  where d = spTableDesc

-- | Opal's TPerInfo description: Core Table 211's with Opal 2.30 Table 26's
--   ProgrammaticResetEnable (PDF 49) added, as M4b's generated one has it.
tPerInfoDesc :: TableDesc
tPerInfoDesc = tPerInfoTableDesc
  { tdColumns = tdColumns tPerInfoTableDesc <> [Col 0x08 "ProgrammaticResetEnable" False (TypeName "boolean" (Just ubooleanType))] }

-- | Table 27, the Admin SP's TPerInfo table (PDF 50): GUDID VU,
--   ProgrammaticResetEnable FALSE.
tPerInfo :: PreconfTable
tPerInfo = tableOf tPerInfoDesc
  [ printed tPerInfoDesc [ ("UID", uidG uTPerInfoRow), ("GUDID", vu), ("ProtocolVersion", Given (UIntCell 1))
                         , ("SSC", Given (ListCell [TextCell "Opal"])), ("ProgrammaticResetEnable", falseG) ] ]

-- | Opal 2.30's DataRemovalMechanism description (Table 32, PDF 51; its UIDs
--   Table 20's, PDF 37), and Table 33, its preconfiguration (PDF 52): the
--   one row, its ActiveDataRemovalMechanism VU.
dataRemovalDesc :: TableDesc
dataRemovalDesc = TableDesc
  { tdName      = "DataRemovalMechanism"
  , tdTableUID  = uid 0x00 0x00 0x11 0x01 0x00 0x00 0x00 0x00
  , tdObjectUID = uid 0x00 0x00 0x00 0x01 0x00 0x00 0x11 0x01
  , tdColumns   = [ Col 0x00 "UID" False (TypeName "uid" Nothing)
                  , Col 0x01 "ActiveDataRemovalMechanism" False (TypeName "data_removal_mechanism" Nothing) ]
  }

dataRemoval :: PreconfTable
dataRemoval = tableOf dataRemovalDesc [printed dataRemovalDesc [("UID", uidG uDataRemovalRow), ("ActiveDataRemovalMechanism", vu)]]

-- | The Admin SP's tables built here, in Opal's order.
adminSP :: [PreconfTable]
adminSP = [accessControl, authority, cPIN, tPerInfo, templates, sps, dataRemoval]

-- | The rows of a table a fold of the tables built here gives, under some
--   parameters.
foldedWith :: SimParams -> UID -> [Row]
foldedWith p table = fromMaybe [] (Map.lookup table (snd (foldOf p adminSP)))

-- | A row's cell in the column of a name.
cellNamed :: TableDesc -> Text -> Row -> Maybe Cell
cellNamed desc name r = L.lookup (number desc name) r

-- | The row of an object.
rowWith :: UID -> [Row] -> Maybe Row
rowWith u = L.find ((== Just (UIDCell u)) . L.lookup 0)

-- | An AccessControl row's InvokingID and MethodID, each the UID it holds or references.
pair :: Row -> (Maybe UID, Maybe UID)
pair r = (ref "InvokingID", ref "MethodID")
  where
    ref name = cellNamed accessControlTableDesc name r >>= \case
      UIDCell u   -> Just u
      RefCell _ u -> Just u
      _           -> Nothing

-- | The parameters with so many Admin authorities.
admins :: Natural -> SimParams
admins n = defaultSimParams { spAdminSPAdmins = n }

-- | An Admin authority's or its credential's UID, a template's instance.
adminUID, credentialUID :: Word8 -> UID
adminUID      = uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02
credentialUID = uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x02


spec :: Spec
spec = do
  describe "an SP's tables folded from its preconfiguration tables" $ do
    it "give each row the cells its table prints and every other column's default, in column order" $
      rowsIn [c_PIN] c_PINTableDesc `shouldBe`
        [ [ (0, UIDCell msid), (1, TextCell "C_PIN_MSID"), (3, BytesCell "an MSID")
          , (4, RefCell "Null" uNull), (5, UIntCell 0), (6, UIntCell 0), (7, EnumCell "False" 0) ]
        , [ (0, UIDCell sid), (1, TextCell "C_PIN_SID")
          , (4, RefCell "Null" uNull), (5, UIntCell 3), (6, UIntCell 0), (7, EnumCell "False" 0) ] ]
    it "hold each table's description and its rows by the table's UID, the rows in the table's order" $ do
      let (descs, tables) = foldOf defaultSimParams [c_PIN, sp]
      descs `shouldBe` Map.fromList [(tdTableUID c_PINTableDesc, c_PINTableDesc), (tdTableUID spTableDesc, spTableDesc)]
      Map.keys tables `shouldBe` Map.keys descs
      map (L.lookup 0) (rowsIn [c_PIN, sp] c_PINTableDesc) `shouldBe` [Just (UIDCell msid), Just (UIDCell sid)]
      rowsIn [c_PIN, sp] spTableDesc `shouldBe` [[(0, UIDCell (uid 0x00 0x00 0x02 0x05 0x00 0x00 0x00 0x01)), (1, TextCell "Admin")]]
    it "of the Admin SP are Opal 2.30's as hsedc generates them (Hsed.Gen.Opal.Preconfig.AdminSP), folded whatever the parameters" $
      forM_ [defaultSimParams, admins 3, defaultSimParams { spLockingOFS = Manufactured, spTPerResetEnabled = True }] $ \p ->
        fmap (\held -> (spsDescs held, spsTables held)) (Map.lookup uSP_Admin (tsSPs (initialState p))) `shouldBe` Just (foldOf p Generated.adminSP)
    it "count a byte table's NumColumns 0x01 in the Table table, whatever the fold holds of it (Core 5.3.2.3.7, PDF 157; M5's MBR, Opal T36, PDF 54), an object table it does not hold 0, and its own 15 and 14 (the M4b review's verification)" $ do
      let tables  = Generated.adminSPTableTablePreconfiguration
          mbr     = uid 0x00 0x00 0x00 0x01 0x00 0x00 0x08 0x04
          locking = uid 0x00 0x00 0x00 0x01 0x00 0x00 0x08 0x02
          byte    = EnumCell "Byte" (enumValue Core_table_kind_Byte)
          object  = EnumCell "Object" (enumValue Core_table_kind_Object)
          more    = tables { ptRows = ptRows tables <> [ plain [(0, UIDCell mbr), (1, TextCell "MBR"), (4, byte)]
                                                        , plain [(0, UIDCell locking), (1, TextCell "Locking"), (4, object)] ] }
          counts u = [ (cellNamed (ptDesc tables) "NumColumns" r, cellNamed (ptDesc tables) "Rows" r)
                     | Just r <- [rowWith u (Map.findWithDefault [] uTableTable (snd (foldOf defaultSimParams [more])))] ]
      map counts [mbr, locking, uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x01]
        `shouldBe` [[(Just (UIntCell 1), Just (UIntCell 0))], [(Just (UIntCell 0), Just (UIntCell 0))], [(Just (UIntCell 15), Just (UIntCell 14))]]

  describe "the cells the TPer chooses (vendorCell)" $ do
    let pin    = cellNamed c_PINTableDesc "PIN"
        column name u p = rowWith u (foldedWith p uC_PINTable) >>= cellNamed c_PINTableDesc name
    it "give C_PIN_SID's PIN the MSID, or the SID's PIN where the parameters give one (Opal 2.30 4.2.1.8, PDF 49)" $ do
      (rowWith uC_PIN_SID (foldedWith defaultSimParams uC_PINTable) >>= pin) `shouldBe` Just (BytesCell (spMsid defaultSimParams))
      column "PIN" uC_PIN_SID defaultSimParams { spSidPin = Just "a PIN of its own" } `shouldBe` Just (BytesCell "a PIN of its own")
    it "give C_PIN_MSID's PIN, printed MSID, the MSID, whatever the SID's (Table 25, PDF 48)" $ do
      column "PIN" uC_PIN_MSID defaultSimParams `shouldBe` Just (BytesCell (spMsid defaultSimParams))
      column "PIN" uC_PIN_MSID defaultSimParams { spSidPin = Just "a PIN of its own" } `shouldBe` Just (BytesCell (spMsid defaultSimParams))
    it "give C_PIN_SID's TryLimit the parameters' and its Tries 0, a new C_PIN object's (Core 5.3.4.1.1.2, PDF 210)" $ do
      column "TryLimit" uC_PIN_SID defaultSimParams { spSidTryLimit = 5 } `shouldBe` Just (UIntCell 5)
      column "TryLimit" uC_PIN_SID defaultSimParams `shouldBe` Just (UIntCell 0)
      column "Tries" uC_PIN_SID defaultSimParams { spSidTryLimit = 5 } `shouldBe` Just (UIntCell 0)
    it "give TPerInfo's GUDID the parameters', the Base template's Instances and MaxInstances 2, and DataRemovalMechanism Cryptographic Erase" $ do
      (listToMaybe (foldedWith defaultSimParams uTPerInfoTable) >>= cellNamed tPerInfoDesc "GUDID")
        `shouldBe` Just (BytesCell (spGudid defaultSimParams))
      fmap (\r -> map (\name -> cellNamed templateTableDesc name r) ["Instances", "MaxInstances"])
           (rowWith uTemplate_Base (foldedWith defaultSimParams uTemplateTable))
        `shouldBe` Just [Just (UIntCell 2), Just (UIntCell 2)]
      (listToMaybe (foldedWith defaultSimParams (tdTableUID dataRemovalDesc)) >>= cellNamed dataRemovalDesc "ActiveDataRemovalMechanism")
        `shouldBe` Just (EnumCell "Cryptographic Erase" 2)
    it "name no cell but those: another column or another marker is Nothing" $ do
      vendorCell defaultSimParams uSP_Admin c_PINTableDesc [(0, UIDCell uC_PIN_SID)] (number c_PINTableDesc "Persistence") "VU" `shouldBe` Nothing
      vendorCell defaultSimParams uSP_Admin c_PINTableDesc [(0, UIDCell uC_PIN_SID)] (number c_PINTableDesc "PIN") "VU*" `shouldBe` Nothing
      vendorCell defaultSimParams uSP_Admin c_PINTableDesc [(0, UIDCell uC_PIN_Admin1)] (number c_PINTableDesc "TryLimit") "VU" `shouldBe` Nothing
      vendorCell defaultSimParams uSP_Admin c_PINTableDesc [] 0x20 "VU" `shouldBe` Nothing
    it "fill every cell the Admin SP's tables leave to the TPer, and a cell no entry names is a problem naming the SP, the table, the row and the column" $ do
      T.length (tshow (foldOf defaultSimParams adminSP)) `shouldSatisfy` (> 0)
      let unnamed = cPIN { ptRows = [printed c_PINTableDesc [("UID", uidG uC_PIN_SID), ("Name", textG "C_PIN_SID"), ("Persistence", vu)]] }
      either id (const []) (foldTables defaultSimParams (PreconfSP uSP_Admin [unnamed]))
        `shouldBe` [UnansweredVU uSP_Admin "C_PIN" "C_PIN_SID" "Persistence"]
    it "answer each SP's cells alone: the Admin SP's C_PIN_SID PIN is no cell of the Locking SP's, whose C_PIN_Admin1 PIN is none of the Admin SP's" $ do
      let unanswered spu row' = either id (const []) (foldTables defaultSimParams (PreconfSP spu [cPIN { ptRows = [row'] }]))
          sidPIN    = printed c_PINTableDesc [("UID", uidG uC_PIN_SID), ("Name", textG "C_PIN_SID"), ("PIN", vu)]
          admin1PIN = printed c_PINTableDesc [ ("UID", uidG LockingSP.uC_PIN_Admin1), ("Name", textG "C_PIN_Admin1")
                                             , ("PIN", NoteMarked 1 (VendorUnique "SID or MSID")) ]
      map length [unanswered uSP_Admin sidPIN, unanswered uLocking admin1PIN] `shouldBe` [0, 0]
      unanswered uLocking sidPIN `shouldBe` [UnansweredVU uLocking "C_PIN" "C_PIN_SID" "PIN"]
      unanswered uSP_Admin admin1PIN `shouldBe` [UnansweredVU uSP_Admin "C_PIN" "C_PIN_Admin1" "PIN"]
    it "make active a data removal mechanism the simulator's Level 0 descriptor reports as supported (Opal 2.30 Table 10, PDF 27; design 13.32)" $ do
      let proxy     = Proxy :: Proxy Level0DiscoverySupportedDataRemovalMechanismFeature
          reported  = do
            d <- L.find ((== fromInteger (featureCode proxy)) . dCode) (simDescriptors (initialState defaultSimParams))
            either (const Nothing) (Just . level0DiscoverySupportedDataRemovalMechanismFeatureSupportedDataRemovalMechanism . drRecord)
                   (decodeFeatureStrict (dBytes d) :: Either LayoutError (DecodedRecord Level0DiscoverySupportedDataRemovalMechanismFeature))
          active    = vendorCell defaultSimParams uSP_Admin dataRemovalDesc [(0, UIDCell uDataRemovalRow)] 1 "VU"
      case (reported, active) of
        (Just bits, Just (EnumCell _ v)) -> (v, testBit bits v) `shouldBe` (v, True)
        other                            -> expectationFailure ("no mechanism, or none reported: " <> show other)

  describe "each column's default" $ do
    let folded = snd (foldOf (admins 3) adminSP)
        descs  = fst (foldOf (admins 3) adminSP)
    it "gives every row a cell in every column of its description, but an AccessControl row its UID, which the TPer assigns (Core 5.3.2.7.1, PDF 162)" $
      forM_ (Map.toList folded) $ \(table, rows) -> do
        let columns = maybe [] (map colNumber . tdColumns) (Map.lookup table descs)
            wanted  = if table == uAccessControlTable then filter (/= 0) columns else columns
        rows `shouldSatisfy` (not . null)
        map (map fst) rows `shouldBe` map (const wanted) rows
    it "gives C_PIN_MSID, whose other cells Table 25 leaves blank, a CharSet of Null, a TryLimit and Tries of 0, and Persistence False" $
      fmap (\r -> map (\name -> cellNamed c_PINTableDesc name r) ["CharSet", "TryLimit", "Tries", "Persistence"])
           (rowWith uC_PIN_MSID (fromMaybe [] (Map.lookup uC_PINTable folded)))
        `shouldBe` Just [Just (RefCell "Null" uNull), Just (UIntCell 0), Just (UIntCell 0), Just (EnumCell "False" 0)]
    it "never gives a row a UID, even where the defaults hold one" $ do
      let withUID = accessControl { ptDefaults = (0, UIDCell uNull) : ptDefaults accessControl }
      [ r | r <- fromMaybe [] (Map.lookup uAccessControlTable (snd (foldOf defaultSimParams (withUID : drop 1 adminSP)))), isJust (L.lookup 0 r) ]
        `shouldBe` []

  describe "a template's instances" $ do
    let names p table desc = mapMaybe (cellNamed desc "Name") (foldedWith p table)
    it "are numbered 2 to spAdminSPAdmins in the Admin SP's Authority and C_PIN tables under XX (Opal 2.30 PDF 47-49), and none elsewhere" $ do
      instances (admins 3) uSP_Admin authorityTableDesc xx `shouldBe` [2, 3]
      instances (admins 3) uSP_Admin c_PINTableDesc xx `shouldBe` [2, 3]
      instances (admins 1) uSP_Admin authorityTableDesc xx `shouldBe` []
      instances (admins 3) uSP_Admin templateTableDesc xx `shouldBe` []
      instances (admins 3) uSP_Admin authorityTableDesc (Placeholder "XXXX" 2) `shouldBe` []
    it "are none with one Admin authority: Admin1 printed, AdminXX standing for no row, nor any AccessControl row of it" $ do
      names (admins 1) uAuthorityTable authorityTableDesc `shouldBe` map TextCell ["Anybody", "Admins", "Makers", "SID", "Admin1"]
      names (admins 1) uC_PINTable c_PINTableDesc `shouldBe` map TextCell ["C_PIN_SID", "C_PIN_MSID", "C_PIN_Admin1"]
      [ u | (Just u, _) <- map pair (foldedWith (admins 1) uAccessControlTable), u `elem` [uAdminXX, uC_PIN_AdminXX, adminUID 2, credentialUID 2] ]
        `shouldBe` []
    it "with three Admin authorities are Admin2 and Admin3, 00 00 00 09 00 00 02 02 and 02 03, their credentials' references stepped" $ do
      let admin n = rowWith (adminUID n) (foldedWith (admins 3) uAuthorityTable)
      names (admins 3) uAuthorityTable authorityTableDesc `shouldBe` map TextCell ["Anybody", "Admins", "Makers", "SID", "Admin1", "Admin2", "Admin3"]
      fmap (cellNamed authorityTableDesc "Credential") (admin 2) `shouldBe` Just (Just (RefCell "C_PIN_Admin2" (credentialUID 2)))
      fmap (cellNamed authorityTableDesc "Credential") (admin 3) `shouldBe` Just (Just (RefCell "C_PIN_Admin3" (credentialUID 3)))
      fmap (cellNamed authorityTableDesc "Class") (admin 3) `shouldBe` Just (Just (RefCell "Admins" uAuthority_Admins))
    it "with three Admin authorities are C_PIN_Admin2 and C_PIN_Admin3, 00 00 00 0B 00 00 02 02 and 02 03, as C_PIN_AdminXX prints them" $ do
      names (admins 3) uC_PINTable c_PINTableDesc `shouldBe` map TextCell ["C_PIN_SID", "C_PIN_MSID", "C_PIN_Admin1", "C_PIN_Admin2", "C_PIN_Admin3"]
      fmap (cellNamed c_PINTableDesc "PIN") (rowWith (credentialUID 3) (foldedWith (admins 3) uC_PINTable)) `shouldBe` Just (Just (BytesCell ""))
    it "with three Admin authorities give each its AccessControl rows: Set on Admin2 and Admin3, Get and Set on their credentials, the ACLs as printed" $ do
      let acl = foldedWith (admins 3) uAccessControlTable
          aclOf invoker method = L.find ((== (Just invoker, Just method)) . pair) acl >>= cellNamed accessControlTableDesc "ACL"
      [ aclOf invoker method | invoker <- map adminUID [2, 3], method <- [uMethodID_Set] ]
        `shouldBe` replicate 2 (Just (ListCell [RefCell "ACE_Set_Enabled" uACE_Set_Enabled]))
      [ aclOf invoker uMethodID_Get | invoker <- map credentialUID [2, 3] ]
        `shouldBe` replicate 2 (Just (ListCell [RefCell "ACE_C_PIN_SID_Get_NOPIN" uACE_C_PIN_SID_Get_NOPIN]))
      [ aclOf invoker uMethodID_Set | invoker <- map credentialUID [2, 3] ]
        `shouldBe` replicate 2 (Just (ListCell [RefCell "ACE_C_PIN_Admins_Set_PIN" uACE_C_PIN_Admins_Set_PIN]))
    it "leave out an instance a printed row already is, which keeps its printed cells" $ do
      let admin2 = printed authorityTableDesc [("UID", uidG (adminUID 2)), ("Name", textG "the printed Admin2")]
          table  = authority { ptRows = ptRows authority <> [admin2] }
          rows   = fromMaybe [] (Map.lookup uAuthorityTable (snd (foldOf (admins 3) [table])))
      mapMaybe (cellNamed authorityTableDesc "Name") rows
        `shouldBe` map TextCell ["Anybody", "Admins", "Makers", "SID", "Admin1", "Admin3", "the printed Admin2"]

  describe "the rows of every object (EveryObject, Opal 2.30 *AC1-*AC7)" $ do
    let getsOf p half = [ u | (Just u, Just m) <- map pair (foldedWith p uAccessControlTable), m == uMethodID_Get, uidUpper u == half ]
    it "are one AccessControl row for each object of the table the half names, in UID order: Table 28's three templates" $
      getsOf defaultSimParams (halfUID 0x00 0x00 0x02 0x04) `shouldBe` [uTemplate_Base, uTemplate_Admin, uTemplate_Locking]
    it "are over the objects the fold gives, a template's instances among them: Get on every Authority object with three Admin authorities" $
      getsOf (admins 3) (halfUID 0x00 0x00 0x00 0x09)
        `shouldBe` [uAuthority_Anybody, uAuthority_Admins, uAuthority_Makers, uAuthority_SID, adminUID 1, adminUID 2, adminUID 3]
    it "take the row's cells, its InvokingID the object, and every other column's default" $ do
      let row' = L.find ((== (Just uSP_Admin, Just uMethodID_Get)) . pair) (foldedWith defaultSimParams uAccessControlTable)
      fmap (cellNamed accessControlTableDesc "ACL") row' `shouldBe` Just (Just (ListCell [RefCell "ACE_Anybody" uACE_Anybody]))
      fmap (cellNamed accessControlTableDesc "InvokingID") row' `shouldBe` Just (Just (UIDCell uSP_Admin))
      fmap (cellNamed accessControlTableDesc "CommonName") row' `shouldBe` Just (Just (TextCell "the default of CommonName"))
    it "hold no InvokingID and MethodID pair twice: a printed row's object, or a second row for the same objects, adds none" $ do
      let printedGet = printed accessControlTableDesc
                         [ ("InvokingID", uidG uTemplate_Admin), ("MethodID", refG "Get" uMethodID_Get)
                         , ("ACL", Given (ListCell [RefCell "ACE_Admin" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x02)])) ]
          twice = accessControl { ptRows = ptRows accessControl <> [printedGet] <> take 1 (reverse (ptRows accessControl)) }
          acl   = fromMaybe [] (Map.lookup uAccessControlTable (snd (foldOf (admins 3) (twice : drop 1 adminSP))))
          pairs = map pair acl
      L.nub pairs `shouldBe` pairs
      [ cellNamed accessControlTableDesc "ACL" r | r <- acl, pair r == (Just uTemplate_Admin, Just uMethodID_Get) ]
        `shouldBe` [Just (ListCell [RefCell "ACE_Admin" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x02)])]
      length [ () | (Just u, Just m) <- pairs, m == uMethodID_Get, uidUpper u == halfUID 0x00 0x00 0x02 0x05 ] `shouldBe` 2
    it "are a problem, as a template's base is, for a table the SP does not hold: the AccessControl table alone names three every-object tables and two bases it lacks (M5a)" $
      either id (const []) (foldTables (admins 3) (PreconfSP uSP_Admin [accessControl]))
        `shouldBe` [ UnknownEveryObject uSP_Admin (tshow (halfUID 0x00 0x00 0x00 0x09)), UnresolvedBase uSP_Admin uAdminXX
                   , UnresolvedBase uSP_Admin uC_PIN_AdminXX, UnresolvedBase uSP_Admin uC_PIN_AdminXX
                   , UnknownEveryObject uSP_Admin (tshow (halfUID 0x00 0x00 0x02 0x04)), UnknownEveryObject uSP_Admin (tshow (halfUID 0x00 0x00 0x02 0x05)) ]

  describe "the Locking SP's tables (M5a: its generated preconfiguration, Opal 2.30 Tables 34-42 and 44-48, PDF 52-89)" $ do
    let lockingSP      = PreconfSP uLocking LockingSP.lockingSP
        lockingOf p    = either (error . show) id (foldTables p lockingSP)
        lockingRows p table = Map.findWithDefault [] table (snd (lockingOf p))
        heldLocking p  = Map.lookup uLocking (tsSPs (initialState p))
        every          = [ defaultSimParams { spLockingOFS = o, spLockingKeys = k } | o <- [minBound .. maxBound], k <- [minBound .. maxBound] ]
        dataStore      = uid 0x00 0x00 0x10 0x01 0x00 0x00 0x00 0x00
    it "fold with no problem at either Original Factory State and with each choice of key tables, and the initial state holds them" $ do
      [ either Just (const Nothing) (foldTables p lockingSP) | p <- every ] `shouldBe` map (const Nothing) every
      map initialStateProblems every `shouldBe` map (const []) every
      forM_ every $ \p -> fmap (\held -> (spsDescs held, spsTables held)) (heldLocking p) `shouldBe` Just (lockingOf p)
    it "are the same tables at either Original Factory State, one for each of its preconfiguration tables but the key table the TPer does not hold (Opal 2.30 5.2.2.1, PDF 96; Table 36's *TT1, PDF 54)" $ do
      let at o k = fmap spsTables (heldLocking defaultSimParams { spLockingOFS = o, spLockingKeys = k })
      forM_ [minBound .. maxBound] $ \k -> at Manufactured k `shouldBe` at ManufacturedInactive k
      map (fmap Map.size . at ManufacturedInactive) [KeysAES128, KeysAES256, KeysBoth]
        `shouldBe` map Just [length LockingSP.lockingSP - 1, length LockingSP.lockingSP - 1, length LockingSP.lockingSP]
    it "read a value printed with a note's number after it as the value: LockingInfo's MaxRanges 8 (Table 44 and its note 1, PDF 86)" $
      (listToMaybe (lockingRows defaultSimParams uLockingInfoTable) >>= \r -> L.lookup 0x04 r) `shouldBe` Just (UIntCell 8)
    it "reference the K_AES_256 key with K_AES_256 alone or both, the K_AES_128 key with K_AES_128 alone (Table 45's *LT1, PDF 86)" $ do
      let activeKey k = rowWith uLocking_GlobalRange (lockingRows defaultSimParams { spLockingKeys = k } uLockingTable)
                          >>= cellNamed lockingTableDesc "ActiveKey"
          aes128 = RefCell "K_AES_128_GlobalRange_Key" LockingSP.uK_AES_128_GlobalRange_Key
          aes256 = RefCell "K_AES_256_GlobalRange_Key" LockingSP.uK_AES_256_GlobalRange_Key
      map activeKey [KeysAES128, KeysAES256, KeysBoth] `shouldBe` map Just [aes128, aes256, aes256]
    it "give the MBR and DataStore tables' Rows the parameters' sizes (Table 36, PDF 54: 0x08000000 min, 0x00A00000 min) and their granularities 1" $ do
      let counts u = rowWith u (lockingRows defaultSimParams uTableTable) <&> \r -> map (`L.lookup` r) [0x07, 0x0D, 0x0E]
      counts uMBRTableObject `shouldBe` Just (map (Just . UIntCell) [0x08000000, 1, 1])
      counts LockingSP.uTable_DataStore `shouldBe` Just (map (Just . UIntCell) [0xA00000, 1, 1])
    it "hold the MBR and the DataStore as byte tables of those sizes, nothing written, each byte zero; the Admin SP none" $ do
      fmap spsBytes (heldLocking defaultSimParams { spMbrSize = 0x10000000 })
        `shouldBe` Just (Map.fromList [(uMBRTable, ByteTable 0x10000000 0 mempty), (dataStore, ByteTable 0xA00000 0 mempty)])
      fmap spsBytes (Map.lookup uSP_Admin (tsSPs (initialState defaultSimParams))) `shouldBe` Just mempty
    it "give the Locking SP's C_PIN_Admin1, printed SID or MSID with its note 1, the MSID (Table 41, PDF 83-84), at either OFS (M5's D3)" $ do
      (rowWith LockingSP.uC_PIN_Admin1 (lockingRows defaultSimParams uC_PINTable) >>= cellNamed c_PINTableDesc "PIN")
        `shouldBe` Just (BytesCell (spMsid defaultSimParams))
      (rowWith LockingSP.uC_PIN_Admin1 (lockingRows defaultSimParams { spLockingOFS = Manufactured } uC_PINTable) >>= cellNamed c_PINTableDesc "PIN")
        `shouldBe` Just (BytesCell (spMsid defaultSimParams))
    it "number the templates by the parameters: Admin5 on past the four printed, User2 on to the Users, Range2 on to the ranges (Tables 40-41 and 45, PDF 82-86)" $ do
      let p        = defaultSimParams { spLockingAdmins = 6, spLockingUsers = 9, spLockingRanges = 9 }
          names t d = mapMaybe (cellNamed d "Name") (lockingRows p t)
      names uAuthorityTable authorityTableDesc
        `shouldBe` map TextCell (["Anybody", "Admins"] <> [ "Admin" <> tshow n | n <- [1 .. 6 :: Int] ] <> ["Users"] <> [ "User" <> tshow n | n <- [1 .. 9 :: Int] ])
      names uC_PINTable c_PINTableDesc
        `shouldBe` map TextCell ([ "C_PIN_Admin" <> tshow n | n <- [1 .. 6 :: Int] ] <> [ "C_PIN_User" <> tshow n | n <- [1 .. 9 :: Int] ])
      names uLockingTable lockingTableDesc `shouldBe` map TextCell ("Locking_GlobalRange" : [ "Locking_Range" <> tshow n | n <- [1 .. 9 :: Int] ])
      instances p uLocking authorityTableDesc (Placeholder "XXXX" 2) `shouldBe` [5, 6]
      instances p uLocking aceTableDesc (Placeholder "MMMM" 2) `shouldBe` [2 .. 9]
      instances p uLocking aceTableDesc (Placeholder "NNNN" 2) `shouldBe` [2 .. 9]
      instances p uLocking templateTableDesc (Placeholder "NNNN" 2) `shouldBe` []
    it "instantiate a range's ActiveKey from the key the parameters choose: Locking_Range2's K_AES_256_Range2_Key (Table 45's *LT1, PDF 86)" $
      (rowWith (uid 0x00 0x00 0x08 0x02 0x00 0x03 0x00 0x02) (lockingRows defaultSimParams uLockingTable) >>= cellNamed lockingTableDesc "ActiveKey")
        `shouldBe` Just (RefCell "K_AES_256_Range2_Key" (uid 0x00 0x00 0x08 0x06 0x00 0x03 0x00 0x02))
    it "leave out the key table the TPer does not hold and every row naming it: its Table row, SecretProtect's, its ACEs and its objects' AccessControl rows (Table 36's *TT1, PDF 54; *AC6, PDF 55)" $ do
      let named k n = [ r | rows <- Map.elems (snd (lockingOf defaultSimParams { spLockingKeys = k })), r <- rows
                          , any (\(_, cell) -> n `T.isInfixOf` tshow cell || n == "K_AES_128" && "0x08 0x05 0x00" `T.isInfixOf` tshow cell
                                                 || n == "K_AES_256" && "0x08 0x06 0x00" `T.isInfixOf` tshow cell) r ]
      map (\k -> length (named k "K_AES_128")) [KeysAES128, KeysAES256, KeysBoth] `shouldSatisfy` \case
        [a, 0, b] -> a > 0 && b > 0
        _         -> False
      map (\k -> length (named k "K_AES_256")) [KeysAES128, KeysAES256, KeysBoth] `shouldSatisfy` \case
        [0, a, b] -> a > 0 && b > 0
        _         -> False
      Map.member uK_AES_128Table (snd (lockingOf defaultSimParams)) `shouldBe` False
    it "make LockingInfo's MaxRanges the parameters' ranges and its geometry the parameters' (Table 44, PDF 85-86; 3.1.1.4.2-5, PDF 22)" $ do
      let info p = listToMaybe (lockingRows p uLockingInfoTable) <&> \r ->
                     map (\n -> cellNamed lockingInfoTableDesc n r) ["MaxRanges", "AlignmentRequired", "LogicalBlockSize", "AlignmentGranularity", "LowestAlignedLBA"]
      info defaultSimParams `shouldBe` Just [Just (UIntCell 8), Just (EnumCell "True" 1), Just (UIntCell 512), Just (UIntCell 8), Just (UIntCell 0)]
      info defaultSimParams { spLockingRanges = 20, spGeometry = Geometry False 4096 1 3 }
        `shouldBe` Just [Just (UIntCell 20), Just (EnumCell "False" 0), Just (UIntCell 4096), Just (UIntCell 1), Just (UIntCell 3)]
    it "refuse, before folding, a parameter out of its bounds, each named with its value and bounds; the Admin SP's fold reads none of them" $ do
      let problems p = either id (const []) (foldTables p lockingSP)
      problems defaultSimParams { spLockingAdmins = 3 } `shouldBe` [OutOfBounds "the Locking SP's Admins" 3 (4, 255)]
      problems defaultSimParams { spLockingUsers = 256, spLockingRanges = 7 }
        `shouldBe` [OutOfBounds "the Locking SP's Users" 256 (8, 255), OutOfBounds "the Locking SP's ranges" 7 (8, 255)]
      problems defaultSimParams { spMbrSize = 0x07FFFFFF } `shouldBe` [OutOfBounds "the MBR's size" 0x07FFFFFF (0x08000000, 0xFFFFFFFF)]
      problems defaultSimParams { spDataStore = (1, 0x9FFFFF, 1) } `shouldBe` [OutOfBounds "the DataStore's size" 0x9FFFFF (0xA00000, 0xFFFFFFFF)]
      problems defaultSimParams { spMandatoryWriteGranularity = 2 }
        `shouldBe` [OutOfBounds "the byte tables' MandatoryWriteGranularity" 2 (1, 1)]
      problems defaultSimParams { spMandatoryWriteGranularity = 8193, spRecommendedAccessGranularity = 16384 }
        `shouldBe` [OutOfBounds "the byte tables' MandatoryWriteGranularity" 8193 (1, 8192)]
      initialStateProblems defaultSimParams { spLockingAdmins = 3 } `shouldBe` [OutOfBounds "the Locking SP's Admins" 3 (4, 255)]
      map (\p -> either id (const []) (foldTables p (PreconfSP uSP_Admin Generated.adminSP))) [defaultSimParams { spLockingAdmins = 3 }] `shouldBe` [[]]
    it "refuse a size the TPer chooses below the least the table prints (0x08000000 min, Table 36, PDF 54)" $ do
      let mbrRow = rowOf tableTableDesc Plain [("UID", uidG uMBRTableObject), ("Name", textG "MBR"), ("Rows", VendorAtLeast 0x10000000 "0x08000000 min")]
      either id (const []) (foldTables defaultSimParams (PreconfSP uLocking [tableOf tableTableDesc [mbrRow]]))
        `shouldBe` [OutOfBounds "the Table table's row MBR's Rows" 0x08000000 (0x10000000, 0xFFFFFFFF)]
    it "fold at the bounds, 255 Admins, Users and ranges with both key tables, in under a second" $ do
      let p = defaultSimParams { spLockingAdmins = 255, spLockingUsers = 255, spLockingRanges = 255, spLockingKeys = KeysBoth }
      done <- timeout 1000000 (evaluate (either (const 0) (sum . map length . Map.elems . snd) (foldTables p lockingSP)))
      done `shouldSatisfy` maybe False (> 451)

  describe "the parameters' overrides" $ do
    let lifeCycle p = rowWith uLocking (foldedWith p uSPTable) >>= cellNamed spTableDesc "LifeCycleState"
        resetEnable p = listToMaybe (foldedWith p uTPerInfoTable) >>= cellNamed tPerInfoDesc "ProgrammaticResetEnable"
    it "make the Locking SP's LifeCycleState its Original Factory State: Manufactured gives 9, over Table 29's Manufactured-Inactive (Opal 2.30 5.2.2, PDF 96)" $ do
      lifeCycle defaultSimParams { spLockingOFS = Manufactured } `shouldBe` Just (EnumCell "Manufactured" (lifeCycleValue Manufactured))
      lifeCycleValue Manufactured `shouldBe` 9
      (rowWith uSP_Admin (foldedWith defaultSimParams { spLockingOFS = Manufactured } uSPTable) >>= cellNamed spTableDesc "LifeCycleState")
        `shouldBe` Just (EnumCell "Manufactured" 9)
    it "keep the cell as printed where it holds the parameter's value already: Manufactured-Inactive by default" $
      lifeCycle defaultSimParams `shouldBe` Just (EnumCell "Manufactured-Inactive" 8)
    it "make TPerInfo's ProgrammaticResetEnable spTPerResetEnabled: True gives True, over Table 27's FALSE (PDF 50), whatever the row's UID" $ do
      resetEnable defaultSimParams { spTPerResetEnabled = True } `shouldBe` Just (EnumCell "True" 1)
      resetEnable defaultSimParams `shouldBe` Just (EnumCell "False" 0)
      let otherUID = tPerInfo { ptRows = [printed tPerInfoDesc [("UID", uidG (uid 0x00 0x00 0x02 0x01 0x00 0x00 0x00 0x01))]] }
      (Map.lookup uTPerInfoTable (snd (foldOf defaultSimParams { spTPerResetEnabled = True } [otherUID]))
         >>= listToMaybe >>= cellNamed tPerInfoDesc "ProgrammaticResetEnable")
        `shouldBe` Just (EnumCell "True" 1)
