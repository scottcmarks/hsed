{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.StateSpec
Description : Test specifications for Hsed.Sim.State and the M4a state of Hsed.Sim.Types: sessions, a ComID's life, rows and cells, the limits a ComID's host properties give
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The shared state helpers of M4a (design 8.1): a TPer starts with no session,
its first TSN 4096 (Core 3.3.7.1.1, PDF 72), the Admin SP Manufactured and
the Locking SP in its Original Factory State; sessions open with distinct
TSNs from 4096, snapshot their ComID's host properties, resolve by TSN and
HSN alone, close and abort; a ComID is Associated while a session is open on
it (Core 3.3.3, PDF 56); an SP's rows, cells and table descriptions are read
and changed, and its life cycle read from its row of the Admin SP's SP table
alone (Core 4.1, PDF 91; since M4b's scaffold); TPER_RESET's enabling is
read from the ProgrammaticResetEnable cell of the Admin SP's TPerInfo row
(Opal 2.30 3.2.3 and Table 26, PDF 28-29 and 49), a row's cell by its
column's name in the description the SP holds; and the limits a ComID's host
properties give follow Core Table 167's 0, no limit, and Opal 2.30 Table
17's initial assumptions. The parameters M4b adds have defaults the spec
allows and are refused out of range: one Admin authority, 1 to 255 (PDF
47-48), and a GUDID in Core Table 212's format, its company ID locally
administered (PDF 230).
-}
module Hsed.Sim.StateSpec (spec) where

import           Data.Bits           (shiftR, testBit)
import qualified RIO.ByteString      as B
import qualified RIO.List            as L
import qualified RIO.Map             as Map
import           RIO
import           Test.Hspec

import           Hsed.Core.Preconfig (PreCell (..), PreconfRow (..), PreconfTable (..), RowKind (..))
import           Hsed.Core.Record    (DecodedRecord (..), decodeRecordStrict, encodeRecord)
import           Hsed.Core.Table     (Cell (..), Col (..), Pre (..), Row, TableDesc (..), columnNamed)
import           Hsed.Core.TypeName  (TypeName (..))
import           Hsed.Core.UID       (uid)
import           Hsed.Gen.Core.Layouts (GUDIDColumnContentsDescription (..))
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc, spTableDesc, tPerInfoTableDesc)
import           Hsed.Gen.Core.TableUIDs (uC_PINTable, uSPTable, uTPerInfoTable)
import           Hsed.Gen.Core.Types (ubooleanType)
import           Hsed.Gen.Core.UIDs  (uAuthority_Anybody, uAuthority_SID, uC_PIN_SID, uSP_Admin)
import           Hsed.OpalUIDs       (uC_PIN_MSID, uSP_Locking)
import           Hsed.Sim.Preconfig  (foldTables)
import           Hsed.Sim.State
import           Hsed.Sim.Types


base :: Word16
base = spBaseComId defaultSimParams

st0 :: TPerState
st0 = initialState defaultSimParams

-- | Opal's TPerInfo description: Core Table 211's with Opal 2.30 Table 26's
--   ProgrammaticResetEnable (PDF 49) added, as M4b's generated one has it.
tPerInfoDesc :: TableDesc
tPerInfoDesc = tPerInfoTableDesc
  { tdColumns = tdColumns tPerInfoTableDesc <> [Col 0x08 "ProgrammaticResetEnable" False (TypeName "boolean" (Just ubooleanType))] }

-- | A TPerInfo row as Opal 2.30 Table 27 prints it (PDF 50), its UID and
--   ProgrammaticResetEnable, a boolean (Core Table 59, PDF 103).
tPerInfoRow :: Bool -> Row
tPerInfoRow enabled = [(0, UIDCell (uid 0x00 0x00 0x02 0x01 0x00 0x03 0x00 0x01)), (0x08, EnumCell (tshow enabled) (if enabled then 1 else 0))]

-- | The TPer with the Admin SP holding a TPerInfo table of a description
--   and rows, which the hand-written stand-in has none of.
withTPerInfo :: TableDesc -> [Row] -> TPerState -> TPerState
withTPerInfo desc rows st = st { tsSPs = Map.adjust holding uSP_Admin (tsSPs st) }
  where
    holding sps = sps { spsDescs  = Map.insert uTPerInfoTable desc (spsDescs sps)
                      , spsTables = Map.insert uTPerInfoTable rows (spsTables sps) }

spec :: Spec
spec = do
  describe "a TPer as it starts" $ do
    it "has no session, its first TSN 4096 (Core 3.3.7.1.1), its ComIDs Issued with the host's initial properties and an empty management buffer" $ do
      tsSessions st0 `shouldBe` mempty
      tsNextTSN st0 `shouldBe` firstTSN
      firstTSN `shouldBe` 4096
      map (\cs -> (cLife cs, cSync cs, cHostProps cs, cMgmt cs)) (Map.elems (tsComIds st0))
        `shouldBe` [(Issued, S1, initialHostProperties, MgmtBuffer False Nothing)]
      initialHostProperties `shouldBe` Map.fromList hostRequirements
    it "has the Admin SP Manufactured with its hand-written tables, and the Locking SP in its Original Factory State with none" $ do
      spLifeCycle uSP_Admin st0 `shouldBe` Just (lifeCycleValue Manufactured)
      fmap (Map.size . spsTables) (Map.lookup uSP_Admin (tsSPs st0)) `shouldBe` Just 6
      spLifeCycle uSP_Locking st0 `shouldBe` Just (lifeCycleValue ManufacturedInactive)
      spLifeCycle uSP_Locking (initialState defaultSimParams { spLockingOFS = Manufactured }) `shouldBe` Just (lifeCycleValue Manufactured)
      fmap (Map.size . spsTables) (Map.lookup uSP_Locking (tsSPs st0)) `shouldBe` Just 0
    it "describes each table an SP holds, and no other, by the table's UID" $ do
      fmap (Map.keys . spsDescs) (Map.lookup uSP_Admin (tsSPs st0)) `shouldBe` fmap (Map.keys . spsTables) (Map.lookup uSP_Admin (tsSPs st0))
      fmap (\sps -> and [ k == tdTableUID d | (k, d) <- Map.toList (spsDescs sps) ]) (Map.lookup uSP_Admin (tsSPs st0)) `shouldBe` Just True
      fmap (Map.size . spsDescs) (Map.lookup uSP_Locking (tsSPs st0)) `shouldBe` Just 0
    it "records a life cycle state by the value Opal 2.30 Table 49 gives it (PDF 98): Manufactured-Inactive 8, Manufactured 9" $
      map lifeCycleValue [ManufacturedInactive, Manufactured] `shouldBe` [8, 9]

  describe "sessions" $ do
    let (s1, st1) = openSession base 1 uSP_Admin True [uAuthority_Anybody] st0
        (s2, st2) = openSession base 7 uSP_Admin True [uAuthority_Anybody, uAuthority_SID] st1
    it "open with distinct TSNs from 4096, the HSN the host's, the ComID's host properties snapshotted" $ do
      (sesTSN s1, sesHSN s1, sesComId s1, sesSP s1, sesWrite s1, sesAuthorities s1) `shouldBe` (4096, 1, base, uSP_Admin, True, [uAuthority_Anybody])
      (sesTSN s2, sesHSN s2) `shouldBe` (4097, 7)
      tsNextTSN st2 `shouldBe` 4098
      sesHostProps s1 `shouldBe` initialHostProperties
      let louder = st0 { tsComIds = Map.adjust (\cs -> cs { cHostProps = Map.insert maxPacketsName 4 (cHostProps cs) }) base (tsComIds st0) }
          (s3, _) = openSession base 2 uSP_Admin True [] louder
      Map.lookup maxPacketsName (sesHostProps s3) `shouldBe` Just 4
    it "wrap back to 4096 past the last TSN" $ do
      let (s, st') = openSession base 1 uSP_Admin True [] st0 { tsNextTSN = maxBound }
      (sesTSN s, tsNextTSN st') `shouldBe` (maxBound, firstTSN)
    it "skip a TSN a session of the ComID still holds, so that every session on a ComID has its own (Core 3.3.7.1.1)" $ do
      let (s3, st3) = openSession base 3 uSP_Admin True [] st1 { tsNextTSN = maxBound }
          (s4, st4) = openSession base 4 uSP_Admin True [] st3
      (sesTSN s3, sesTSN s4, tsNextTSN st4) `shouldBe` (maxBound, 4097, 4098)
      Map.keys (tsSessions st4) `shouldBe` [(base, 4096), (base, 4097), (base, maxBound)]
      resolveSession base 4096 1 st4 `shouldBe` Just s1
    it "resolve by the ComID, TSN and HSN a packet names, and by nothing else (Core 3.3.7.1.2)" $ do
      resolveSession base 4096 1 st2 `shouldBe` Just s1
      resolveSession base 4097 7 st2 `shouldBe` Just s2
      resolveSession base 4096 7 st2 `shouldBe` Nothing
      resolveSession base 4097 1 st2 `shouldBe` Nothing
      resolveSession (base + 1) 4096 1 st2 `shouldBe` Nothing
      resolveSession base 0 0 st2 `shouldBe` Nothing
    it "are listed by ComID, and make it Associated while one is open (Core 3.3.3)" $ do
      map sesTSN (sessionsOn base st2) `shouldBe` [4096, 4097]
      sessionsOn (base + 1) st2 `shouldBe` []
      (lifeOf base st0, lifeOf base st1, lifeOf (base + 1) st1) `shouldBe` (Just Issued, Just Associated, Nothing)
      let inactive = st1 { tsComIds = Map.adjust (\cs -> cs { cLife = Inactive }) base (tsComIds st1) }
      lifeOf base inactive `shouldBe` Just Inactive
    it "close by ComID and TSN, and abort by a predicate" $ do
      let closed = closeSession base 4096 st2
      map sesTSN (sessionsOn base closed) `shouldBe` [4097]
      lifeOf base (closeSession base 4097 closed) `shouldBe` Just Issued
      closeSession base 5000 st2 `shouldBe` st2
      sessionsOn base (abortSessions ((== uSP_Admin) . sesSP) st2) `shouldBe` []
      sessionsOn base (abortSessions ((== uSP_Locking) . sesSP) st2) `shouldBe` [s1, s2]
      tsNextTSN (abortSessions (const True) st2) `shouldBe` 4098

  describe "an SP's rows and cells" $ do
    let pin   = maybe (-1) colNumber (columnNamed "PIN" c_PINTableDesc)
        tries = maybe (-1) colNumber (columnNamed "Tries" c_PINTableDesc)
    it "are read by the SP's and the table's UIDs, and an object's row by its UID" $ do
      length (tableRows uSP_Admin uC_PINTable st0) `shouldBe` 3
      tableRows uSP_Locking uC_PINTable st0 `shouldBe` []
      tableRows uSP_Admin uSP_Admin st0 `shouldBe` []
      (rowWithUID uSP_Admin uC_PINTable uC_PIN_MSID st0 >>= cellOf pin) `shouldBe` Just (BytesCell (spMsid defaultSimParams))
      rowWithUID uSP_Admin uC_PINTable uSP_Locking st0 `shouldBe` Nothing
    it "are changed one row at a time, the others as they were" $ do
      let bump r = [ (n, if n == tries then UIntCell 1 else c) | (n, c) <- r ]
          st'    = updateRow uSP_Admin uC_PINTable uC_PIN_SID bump st0
      (rowWithUID uSP_Admin uC_PINTable uC_PIN_SID st' >>= cellOf tries) `shouldBe` Just (UIntCell 1)
      (rowWithUID uSP_Admin uC_PINTable uC_PIN_SID st0 >>= cellOf tries) `shouldBe` Just (UIntCell 0)
      rowWithUID uSP_Admin uC_PINTable uC_PIN_MSID st' `shouldBe` rowWithUID uSP_Admin uC_PINTable uC_PIN_MSID st0
      updateRow uSP_Admin uC_PINTable uSP_Locking bump st0 `shouldBe` st0
      updateRow uSP_Locking uC_PINTable uC_PIN_SID bump st0 `shouldBe` st0
    it "find a column by name in a generated description" $ do
      columnOf c_PINTableDesc "PIN" `shouldBe` Just pin
      columnOf c_PINTableDesc "UID" `shouldBe` Just 0
      columnOf c_PINTableDesc "NoSuchColumn" `shouldBe` Nothing
    it "have their tables' descriptions as the SP holds them, by the SP's and the table's UIDs" $ do
      descOf uSP_Admin uC_PINTable st0 `shouldBe` Just c_PINTableDesc
      descOf uSP_Admin uSPTable st0 `shouldBe` Just spTableDesc
      descOf uSP_Locking uC_PINTable st0 `shouldBe` Nothing
      descOf uSP_Admin uSP_Admin st0 `shouldBe` Nothing
      descOf uC_PIN_SID uC_PINTable st0 `shouldBe` Nothing
    it "give a row's cell by its column's name in the description the SP holds, an SSC's added columns among them (cellNamedIn)" $ do
      let adminSP st = fromMaybe (SPState uSP_Admin mempty mempty) (Map.lookup uSP_Admin (tsSPs st))
          msidRow    = fromMaybe [] (rowWithUID uSP_Admin uC_PINTable uC_PIN_MSID st0)
      cellNamedIn (adminSP st0) uC_PINTable "PIN" msidRow `shouldBe` Just (BytesCell (spMsid defaultSimParams))
      cellNamedIn (adminSP st0) uC_PINTable "NoSuchColumn" msidRow `shouldBe` Nothing
      cellNamedIn (SPState uSP_Admin mempty mempty) uC_PINTable "PIN" msidRow `shouldBe` Nothing
      cellNamedIn (adminSP (withTPerInfo tPerInfoDesc [tPerInfoRow True] st0)) uTPerInfoTable "ProgrammaticResetEnable" (tPerInfoRow True)
        `shouldBe` Just (EnumCell "True" 1)
      cellNamedIn (adminSP (withTPerInfo tPerInfoTableDesc [tPerInfoRow True] st0)) uTPerInfoTable "ProgrammaticResetEnable" (tPerInfoRow True)
        `shouldBe` Nothing

  describe "an SP's life cycle (Core 4.1, PDF 91)" $ do
    let lifeCycleState = maybe (-1) colNumber (columnNamed "LifeCycleState" spTableDesc)
        setLife v r    = [ (n, if n == lifeCycleState then EnumCell "Manufactured" v else c) | (n, c) <- r ]
    it "is the LifeCycleState cell of its row of the Admin SP's SP table, read nowhere else: changing the cell changes it" $ do
      let activated = updateRow uSP_Admin uSPTable uSP_Locking (setLife (lifeCycleValue Manufactured)) st0
      spLifeCycle uSP_Locking activated `shouldBe` Just (lifeCycleValue Manufactured)
      spLifeCycle uSP_Admin activated `shouldBe` spLifeCycle uSP_Admin st0
      Map.lookup uSP_Locking (tsSPs activated) `shouldBe` Map.lookup uSP_Locking (tsSPs st0)
    it "is none for a UID the Admin SP's SP table has no row for" $ do
      spLifeCycle uC_PIN_SID st0 `shouldBe` Nothing
      spLifeCycle uSP_Locking st0 { tsSPs = Map.adjust (\sps -> sps { spsTables = Map.insert uSPTable [] (spsTables sps) }) uSP_Admin (tsSPs st0) }
        `shouldBe` Nothing

  describe "TPER_RESET's enabling (programmaticResetEnabled; Opal 2.30 3.2.3, PDF 28-29)" $ do
    it "is none on the hand-written stand-in, which has no TPerInfo table" $
      programmaticResetEnabled st0 `shouldBe` Nothing
    it "is the ProgrammaticResetEnable cell of the Admin SP's TPerInfo row, its column by name in the Admin SP's description (Opal 2.30 Table 26)" $ do
      programmaticResetEnabled (withTPerInfo tPerInfoDesc [tPerInfoRow False] st0) `shouldBe` Just False
      programmaticResetEnabled (withTPerInfo tPerInfoDesc [tPerInfoRow True] st0) `shouldBe` Just True
    it "is none where the description has no such column, Core Table 211's alone, or the cell holds no boolean" $ do
      programmaticResetEnabled (withTPerInfo tPerInfoTableDesc [tPerInfoRow True] st0) `shouldBe` Nothing
      programmaticResetEnabled (withTPerInfo tPerInfoDesc [take 1 (tPerInfoRow True) <> [(0x08, UIntCell 1)]] st0) `shouldBe` Nothing
      programmaticResetEnabled (withTPerInfo tPerInfoDesc [] st0) `shouldBe` Nothing
    it "reads what the fold seeds from spTPerResetEnabled over Table 27's FALSE (PDF 50)" $ do
      let table   = PreconfTable tPerInfoDesc [] [Pre (PreconfRow Plain False [ (c, Given cell) | (c, cell) <- tPerInfoRow False ] []) []]
          folded p = st0 { tsSPs = Map.insert uSP_Admin (uncurry (SPState uSP_Admin) (foldTables p [table])) (tsSPs st0) }
      programmaticResetEnabled (folded defaultSimParams) `shouldBe` Just False
      programmaticResetEnabled (folded defaultSimParams { spTPerResetEnabled = True }) `shouldBe` Just True

  describe "the properties" $ do
    it "read the TPer's by name from the parameters, and the host's from what a ComID keeps" $ do
      tperProperty maxComPacketSizeName defaultSimParams `shouldBe` Just 2048
      tperProperty defSessionTimeoutName defaultSimParams `shouldBe` Just 0
      tperProperty "NoSuchProperty" defaultSimParams `shouldBe` Nothing
      hostProperty maxPacketsName initialHostProperties `shouldBe` Just 1
      hostProperty maxResponseComPacketSizeName initialHostProperties `shouldBe` Nothing
    it "give the host's limits from a ComID's properties: a value kept as itself, 0 as no limit (Core Table 167), one never sent as Opal Table 17's assumption, the ComPacket within the TPer's response buffer" $ do
      let limits props = comIdHostLimits defaultSimParams (Map.fromList props)
      limits [] `shouldBe` hostLimits defaultSimParams
      lPackets (limits [(maxPacketsName, 3)]) `shouldBe` 3
      lPackets (limits [(maxPacketsName, 0)]) `shouldBe` maxBound
      lComPacketSize (limits [(maxComPacketSizeName, 4096)]) `shouldBe` 2048
      lComPacketSize (comIdHostLimits defaultSimParams { spTPerProperties = [(maxResponseComPacketSizeName, 8192)] } (Map.fromList [(maxComPacketSizeName, 4096)])) `shouldBe` 4096
      lPacketSize (limits [(maxPacketSizeName, 100)]) `shouldBe` 100
    it "report DefSessionTimeout 0 among the defaults, the eleventh of Opal Table 17's TPer column" $ do
      length tperRequirements `shouldBe` 11
      L.lookup defSessionTimeoutName (spTPerProperties defaultSimParams) `shouldBe` Just 0
      simParamsProblems defaultSimParams `shouldBe` []
    it "refuse an MSID of no bytes, and an MSID or a SID PIN past the PIN column's 32 bytes (Core Table 122)" $ do
      simParamsProblems defaultSimParams { spMsid = "" } `shouldSatisfy` (not . null)
      simParamsProblems defaultSimParams { spMsid = "123456789012345678901234567890123" } `shouldSatisfy` (not . null)
      simParamsProblems defaultSimParams { spSidPin = Just "123456789012345678901234567890123" } `shouldSatisfy` (not . null)
      simParamsProblems defaultSimParams { spSidPin = Just "12345678901234567890123456789012" } `shouldBe` []

  describe "the parameters M4b's preconfiguration reads" $ do
    it "give the Admin SP one Admin authority, Admin1, the one Opal 2.30 requires (PDF 47)" $
      spAdminSPAdmins defaultSimParams `shouldBe` 1
    it "give TPerInfo a GUDID of 12 bytes in Core Table 212's format (PDF 230), which decodes strictly, the identifier HSED" $ do
      B.length (spGudid defaultSimParams) `shouldBe` 12
      fmap (\d -> (drRecord d, drRest d)) (decodeRecordStrict (spGudid defaultSimParams))
        `shouldBe` Right (GUDIDColumnContentsDescription 0x020000 0x48534544, "")
      spGudid defaultSimParams `shouldBe` B.pack [0x02, 0x23, 0x00, 0x08, 0x50, 0x20, 0x00, 0x00, 0x48, 0x53, 0x45, 0x44]
      B.drop 8 (spGudid defaultSimParams) `shouldBe` "HSED"
    it "give the GUDID a company ID no real company has: its first octet's U/L bit set, a locally administered ID, which no OUI the IEEE assigns carries" $
      fmap (\d -> testBit (gudidColumnContentsDescriptionIEEECOMPANYID (drRecord d) `shiftR` 16) 1) (decodeRecordStrict (spGudid defaultSimParams))
        `shouldBe` Right True
    it "refuse no Admin authority, and more than the 255 Opal 2.30 Table 24's AdminXX numbers in its one byte (PDF 47-48)" $ do
      simParamsProblems defaultSimParams { spAdminSPAdmins = 0 } `shouldSatisfy` (not . null)
      simParamsProblems defaultSimParams { spAdminSPAdmins = 256 } `shouldSatisfy` (not . null)
      map (\n -> simParamsProblems defaultSimParams { spAdminSPAdmins = n }) [1, 2, 255] `shouldBe` [[], [], []]
    it "refuse a GUDID other than Core Table 212's 12 bytes, its constant bytes and NAA as the layout pins them (PDF 230)" $ do
      simParamsProblems defaultSimParams { spGudid = B.take 11 (spGudid defaultSimParams) } `shouldSatisfy` (not . null)
      simParamsProblems defaultSimParams { spGudid = spGudid defaultSimParams <> "\x00" } `shouldSatisfy` (not . null)
      simParamsProblems defaultSimParams { spGudid = "\x02\x23\x00\x09" <> B.drop 4 (spGudid defaultSimParams) } `shouldSatisfy` (not . null)
      simParamsProblems defaultSimParams { spGudid = B.take 4 (spGudid defaultSimParams) <> "\x60" <> B.drop 5 (spGudid defaultSimParams) }
        `shouldSatisfy` (not . null)
      simParamsProblems defaultSimParams { spGudid = either (const "") id (encodeRecord (GUDIDColumnContentsDescription 0x123456 1)) }
        `shouldBe` []
