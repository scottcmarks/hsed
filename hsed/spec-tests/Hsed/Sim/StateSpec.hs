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
it (Core 3.3.3, PDF 56); an SP's rows and cells are read and changed; and
the limits a ComID's host properties give follow Core Table 167's 0, no
limit, and Opal 2.30 Table 17's initial assumptions.
-}
module Hsed.Sim.StateSpec (spec) where

import qualified RIO.List            as L
import qualified RIO.Map             as Map
import           RIO
import           Test.Hspec

import           Hsed.Core.Table     (Cell (..), Col (..), columnNamed)
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs (uC_PINTable)
import           Hsed.Gen.Core.UIDs  (uAuthority_Anybody, uAuthority_SID, uC_PIN_SID, uSP_Admin)
import           Hsed.OpalUIDs       (uC_PIN_MSID, uSP_Locking)
import           Hsed.Sim.State
import           Hsed.Sim.Types


base :: Word16
base = spBaseComId defaultSimParams

st0 :: TPerState
st0 = initialState defaultSimParams

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
      fmap spsLife (Map.lookup uSP_Admin (tsSPs st0)) `shouldBe` Just Manufactured
      fmap (Map.size . spsTables) (Map.lookup uSP_Admin (tsSPs st0)) `shouldBe` Just 6
      fmap spsLife (Map.lookup uSP_Locking (tsSPs st0)) `shouldBe` Just ManufacturedInactive
      fmap spsLife (Map.lookup uSP_Locking (tsSPs (initialState defaultSimParams { spLockingOFS = Manufactured }))) `shouldBe` Just Manufactured
      fmap (Map.size . spsTables) (Map.lookup uSP_Locking (tsSPs st0)) `shouldBe` Just 0

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
