{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Gen
Description : QuickCheck generators of TPer states and requests, for the laws
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What the laws of "Hsed.Law.Sync" quantify over: states of the simulated
TPer, with sessions open and the host's properties kept per ComID; requests
on their static ComIDs; and IF-SEND payloads that ask nothing, name no
session the TPer has, or break a control session packet's requirements.
Generators ('Gen' values), not 'Test.QuickCheck.Arbitrary' instances, so
that no type here or in the codec gets an orphan instance (design 12).
-}
module Hsed.Sim.Gen
  ( genState
  , genStaticRequest
  , genNoActionPayload
  , genUnresolvablePayload
  , genOverfullPayload
    -- * Parts the specs quantify over too
  , genHostProperties
  , genStagedPackets
  ) where

import qualified RIO.ByteString        as B
import qualified RIO.Map               as Map
import           RIO
import           Test.QuickCheck       (Gen, arbitrary, chooseInt, elements, oneof, suchThat, suchThatMap, vectorOf)

import           Hsed.Core.Method      (invocation)
import           Hsed.Core.StreamItem  (StreamItem (generate))
import           Hsed.Core.Token       (Token (Empty))
import           Hsed.Gen.Core.Layouts (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..), PacketHeader (..))
import qualified Hsed.Gen.Core.Methods as Methods
import           Hsed.Gen.Core.UIDs    (uAuthority_Anybody, uAuthority_SID, uSMUID, uSP_Admin)
import           Hsed.OpalUIDs         (uSP_Locking)
import           Hsed.Packet           (ComPacketFrame (..), PacketFrame (..), encodeComPacket, encodePacket)
import           Hsed.Sim.Session      (packets)
import           Hsed.Sim.State        (openSession, sessionsOn)
import           Hsed.Sim.Types        (ComIdState (..), LifeCycle (..), Limits (..), Session (..), SimParams (..), SyncState (..),
                                        TPerState (..), comIdHostLimits, defaultSimParams, initialHostProperties, initialState,
                                        maxComPacketSizeName, maxMethodsName, maxPacketSizeName, maxPacketsName, maxSubpacketsName,
                                        tperLimits)
import           Hsed.Wire             (Dir (..), Request (..), Unit (..), comPacketHeaderSize, comPacketProtocol, dataKind, roundUp,
                                        transferLength)


-- | A TPer of the default parameters but for its unit (either), its number
--   of static ComIDs (one to three), what it does with a command to an
--   inactive ComID (either), its MaxComPacketSize (Opal 2.30 Table 17's
--   least, 2048, or twice that, so that a packet over its MaxPacketSize can
--   arrive within it) and its Locking SP Manufactured (so that a session
--   can open to it); each static ComID keeping the host's properties as the
--   host may have sent them ('genHostProperties') and awaiting IF-SEND,
--   processing or awaiting IF-RECV with one to three response packets
--   prepared, as the Session Manager might stage them or as 'packets'
--   frames them within those properties ('genStagedPackets'); and none to
--   two regular sessions open on its ComIDs ("Hsed.Sim.State".'openSession'),
--   one per SP, the Admin SP's first, as Anybody or as SID, with any HSN (a
--   second read-write session to one SP is SP_BUSY, Core 5.1.5.3, PDF 138,
--   so no state the protocol reaches holds two on one SP).
genState :: Gen TPerState
genState = do
  units   <- elements [minBound .. maxBound]
  n       <- chooseInt (1, 3)
  discard <- arbitrary
  roomier <- arbitrary
  let p  = defaultSimParams
             { spUnits = units, spNComIds = fromIntegral n, spInactiveComIdDiscard = discard, spLockingOFS = Manufactured
             , spTPerProperties = [ (name, if roomier && name == maxComPacketSizeName then 2 * v else v)
                                  | (name, v) <- spTPerProperties defaultSimParams ] }
      st = initialState p
  comIds   <- traverse (\cs -> genHostProperties >>= \props -> (\s -> cs { cSync = s, cHostProps = props }) <$> genSync p props) (tsComIds st)
  sessions <- chooseInt (0, 2)
  foldM opened st { tsComIds = comIds } (take sessions [uSP_Admin, uSP_Locking])
  where
    opened st sp = do
      c           <- elements (Map.keys (tsComIds st))
      hsn         <- arbitrary
      authorities <- elements [[uAuthority_Anybody], [uAuthority_Anybody, uAuthority_SID]]
      pure (snd (openSession c hsn sp True authorities st))

-- | An IF-SEND or IF-RECV of protocol 1 (the ComPacket protocol, SIIS 1.20
--   Tables 12-13, PDF 30-31) on one of a state's static ComIDs (on its base
--   ComID if it has none), of a length its unit allows: 1 to 4096 bytes, or
--   1 to 8 blocks; an IF-SEND's payload random bytes of that length.
genStaticRequest :: TPerState -> Gen Request
genStaticRequest st = do
  c   <- elements (case Map.keys (tsComIds st) of
                     [] -> [spBaseComId (tsParams st)]
                     cs -> cs)
  d   <- elements [minBound .. maxBound]
  len <- case spUnits (tsParams st) of
           UnitBlocks512 -> transferLength UnitBlocks512 <$> chooseInt (1, 8)
           UnitBytes     -> transferLength UnitBytes <$> chooseInt (1, 4096)
  body <- case d of
            IfSend -> B.pack <$> vectorOf (fromIntegral len) arbitrary
            IfRecv -> pure mempty
  pure (Request d comPacketProtocol c len body)

-- | A ComPacket on a ComID that asks nothing (Core 3.3.10.6 S2:S1 d, PDF
--   85-86): no packets; control session packets without payload; or such
--   packets whose data subpackets have no payload or Empty atoms alone (one
--   to sixteen of them).
genNoActionPayload :: Word16 -> Gen ByteString
genNoActionPayload c = comPacketOn c =<< oneof
  [ pure []
  , chooseInt (1, 3) >>= (`vectorOf` pure (PacketFrame controlHeader []))
  , chooseInt (1, 3) >>= (`vectorOf` (PacketFrame controlHeader <$> (chooseInt (1, 3) >>= (`vectorOf` emptyAtoms))))
  ]
  where
    emptyAtoms = dataSubPacket . mconcat . (`replicate` generate Empty) <$> chooseInt (0, 16)

-- | A ComPacket on a ComID of a TPer whose packets each name a session the
--   TPer does not have on that ComID (Core 3.3.10.7 a, PDF 86): a Session
--   other than 0, the control session's (3.3.7.1.2, PDF 73), and other than
--   any session open on the ComID; among them an open session's TSN with
--   another HSN (what ETC-04 sends, Test Cases PDF 78) and a session open
--   on another ComID, where the TPer has them. One to three packets, each
--   of up to two data subpackets holding a Properties call, no payload or
--   random bytes.
genUnresolvablePayload :: TPerState -> Word16 -> Gen ByteString
genUnresolvablePayload st c = comPacketOn c =<< (chooseInt (1, 3) >>= (`vectorOf` sessionPacket))
  where
    open          = [ (sesTSN s, sesHSN s) | s <- sessionsOn c st ]
    elsewhere     = [ (sesTSN s, sesHSN s) | ((c', _), s) <- Map.toList (tsSessions st), c' /= c ]
    sessionPacket = PacketFrame <$> (header <$> session) <*> (chooseInt (0, 2) >>= (`vectorOf` (dataSubPacket <$> genControlBody)))
    header (tsn, hsn) = PacketHeader tsn hsn 0 0 0
    session       = oneof ([random] <> [ otherHSN | not (null open) ] <> [ elements elsewhere | not (null elsewhere) ])
                      `suchThat` (\s -> s /= (0, 0) && s `notElem` open)
    random        = (,) <$> arbitrary <*> arbitrary
    otherHSN      = do
      (tsn, hsn) <- elements open
      (,) tsn <$> arbitrary `suchThat` (/= hsn)

-- | A ComPacket on a ComID of a TPer whose one control session packet
--   breaks the TPer's packet requirements (Core 5.2.2.4.1.1-2, PDF 145): more
--   data subpackets than its MaxSubpackets (one or two more, each a
--   Properties call the Session Manager answers, no payload or random
--   bytes), or, where the TPer's MaxComPacketSize leaves room for one
--   ('genState' doubles it in half the states), one data subpacket making
--   the packet larger than its MaxPacketSize, a Properties call padded with
--   Empty atoms, the ComPacket still within its MaxComPacketSize when its
--   length is rounded up to the unit. The TPer SHALL discard and ignore
--   such a packet, a violation of packet requirements (3.3.10.7 a, PDF 86);
--   the first is what ETC-15 sends (Test Cases PDF 88).
genOverfullPayload :: TPerState -> Word16 -> Gen ByteString
genOverfullPayload st c = oneof ([tooMany] <> [ tooLarge | leastData <= mostData ])
  where
    p       = tsParams st
    tper    = tperLimits p
    tooMany = comPacketOn c . pure . PacketFrame controlHeader
                =<< (chooseInt (lSubpackets tper + 1, lSubpackets tper + 2) >>= (`vectorOf` (dataSubPacket <$> genControlBody)))
    -- a data subpacket's bytes (a multiple of four) that make the packet exceed MaxPacketSize, the packet and subpacket
    -- headers counted, and the most the ComPacket carries within MaxComPacketSize, in whole units
    packetOverhead = 36
    leastData      = 4 * ((lPacketSize tper - packetOverhead) `div` 4 + 1)
    mostData       = 4 * ((wholeUnits (lComPacketSize tper) - comPacketHeaderSize - packetOverhead) `div` 4)
    wholeUnits n   = (n `div` unitBytes) * unitBytes
    unitBytes      = fromIntegral (roundUp (spUnits p) 1)
    tooLarge = do
      size <- (* 4) <$> chooseInt (leastData `div` 4, mostData `div` 4)
      let call = generate (invocation uSMUID (Methods.Properties Nothing))
      comPacketOn c [PacketFrame controlHeader [dataSubPacket (B.take size (call <> B.replicate size 0xFF))]]


-- * Parts

-- | A data subpacket's payload as a control session packet carries it: a
--   Properties call the Session Manager answers, no payload, or random
--   bytes.
genControlBody :: Gen ByteString
genControlBody = oneof [ pure (generate (invocation uSMUID (Methods.Properties Nothing)))
                       , pure mempty
                       , B.pack <$> (chooseInt (1, 32) >>= (`vectorOf` arbitrary)) ]

-- | A ComID's synchronous protocol state, its staged packets 'genResponse''s
--   or 'genStagedPackets'' within the host's properties it keeps.
genSync :: SimParams -> Map Text Natural -> Gen SyncState
genSync p props = oneof [pure S1, S2 <$> staged, S3 <$> staged]
  where staged = oneof [genResponse, genStagedPackets p props]

-- | The host's properties a ComID keeps, as the host may have sent them
--   (Core 5.2.2.3-4, PDF 143-145): each of the five the TPer's framing
--   reads (MaxComPacketSize, MaxPacketSize, MaxPackets, MaxSubpackets,
--   MaxMethods) either never sent (Opal 2.30 Table 17's initial assumption
--   applies), sent as 0, no limit (Core Table 167, PDF 141-142), or sent as
--   a value from its initial assumption up (a value below it is raised to
--   it, 5.2.2.4).
genHostProperties :: Gen (Map Text Natural)
genHostProperties = Map.fromList . catMaybes <$> traverse one [maxComPacketSizeName, maxPacketSizeName, maxPacketsName, maxSubpacketsName, maxMethodsName]
  where
    one name = oneof [ pure Nothing, pure (Just (name, 0)), (\v -> Just (name, v)) <$> from (fromMaybe 1 (Map.lookup name initialHostProperties)) ]
    from least = (least +) . fromIntegral <$> chooseInt (0, if least > 16 then 2048 else 3)

-- | Response packets as the Session Manager prepares them, one to three
--   control session packets, each of one or two data subpackets of up to
--   600 bytes: so that a transfer of one block holds none, some or all of
--   them.
genResponse :: Gen [ByteString]
genResponse = chooseInt (1, 3) >>= (`vectorOf` packet)
  where
    packet = (PacketFrame controlHeader <$> (chooseInt (1, 2) >>= (`vectorOf` subPacket))) `suchThatMap` (either (const Nothing) Just . encodePacket)
    subPacket = dataSubPacket . B.pack <$> (chooseInt (0, 600) >>= (`vectorOf` arbitrary))

-- | Response packets as "Hsed.Sim.Session".'packets' frames one to eight
--   token streams of up to 600 bytes for a host keeping these properties on
--   its ComID, on the control session: within its MaxSubpackets,
--   MaxPacketSize and the ComPacket its MaxComPacketSize and the TPer's
--   MaxResponseComPacketSize allow (Core 5.2.2.4.1.1-2 and 4, PDF 145-146;
--   the host properties 'genHostProperties' gives leave room for every such
--   stream, so the framing never refuses one).
genStagedPackets :: SimParams -> Map Text Natural -> Gen [ByteString]
genStagedPackets p props = do
  streams <- chooseInt (1, 8) >>= (`vectorOf` (B.pack <$> (chooseInt (0, 600) >>= (`vectorOf` arbitrary))))
  pure (either (const []) id (packets (comIdHostLimits p props) controlHeader streams))

-- | A ComPacket's bytes on a ComID, its extension a static ComID's (0x0000,
--   Core 3.3.3.1, PDF 57), its packets these.
comPacketOn :: Word16 -> [PacketFrame] -> Gen ByteString
comPacketOn c ps = pure (ComPacketFrame (ComPacketHeader c 0 0 0) ps) `suchThatMap` (either (const Nothing) Just . encodeComPacket)

-- | A control session packet's header: Session 0 (Core 3.3.7.1.2, PDF 73).
controlHeader :: PacketHeader
controlHeader = PacketHeader 0 0 0 0 0

-- | A data subpacket of a payload: Core Table 19's Data Kind (PDF 43).
dataSubPacket :: ByteString -> DataSubPacket
dataSubPacket = DataSubPacket (DataSubPacketHeader dataKind)
