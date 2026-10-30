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
TPer, requests on their static ComIDs, and IF-SEND payloads that ask
nothing, name no session the TPer has, or break a control session packet's
requirements. Generators ('Gen' values), not
'Test.QuickCheck.Arbitrary' instances, so that no type here or in the codec
gets an orphan instance (design 12).
-}
module Hsed.Sim.Gen
  ( genState
  , genStaticRequest
  , genNoActionPayload
  , genUnresolvablePayload
  , genOverfullPayload
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
import           Hsed.Gen.Core.UIDs    (uSMUID)
import           Hsed.Packet           (ComPacketFrame (..), PacketFrame (..), encodeComPacket, encodePacket)
import           Hsed.Sim.Types        (ComIdState (..), SimParams (..), SyncState (..), TPerState (..), defaultSimParams,
                                        initialState)
import           Hsed.Wire             (Dir (..), Request (..), Unit (..), comPacketProtocol, dataKind, transferLength)


-- | A TPer of the default parameters but for its unit (either), its number
--   of static ComIDs (one to three) and what it does with a command to an
--   inactive ComID (either), each static ComID awaiting IF-SEND, processing
--   or awaiting IF-RECV, with one to three response packets prepared.
genState :: Gen TPerState
genState = do
  units   <- elements [minBound .. maxBound]
  n       <- chooseInt (1, 3)
  discard <- arbitrary
  let st = initialState defaultSimParams { spUnits = units, spNComIds = fromIntegral n, spInactiveComIdDiscard = discard }
  comIds <- traverse (\cs -> (\s -> cs { cSync = s }) <$> genSync) (tsComIds st)
  pure st { tsComIds = comIds }

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

-- | A ComPacket on a ComID whose packets each name a session (Session other
--   than 0, the control session's, Core 3.3.7.1.2, PDF 73), which no TPer
--   has before M4a opens sessions (Core 3.3.10.7 a, PDF 86): one to three
--   packets, each of up to two data subpackets holding a Properties call, no
--   payload or random bytes.
genUnresolvablePayload :: Word16 -> Gen ByteString
genUnresolvablePayload c = comPacketOn c =<< (chooseInt (1, 3) >>= (`vectorOf` sessionPacket))
  where
    sessionPacket = PacketFrame <$> session <*> (chooseInt (0, 2) >>= (`vectorOf` (dataSubPacket <$> body)))
    session       = (PacketHeader <$> arbitrary <*> arbitrary <*> pure 0 <*> pure 0 <*> pure 0)
                      `suchThat` (\h -> packetTSN h /= 0 || packetHSN h /= 0)
    body          = oneof [ pure (generate (invocation uSMUID (Methods.Properties Nothing)))
                          , pure mempty
                          , B.pack <$> (chooseInt (1, 32) >>= (`vectorOf` arbitrary)) ]


-- | A ComPacket on a ComID whose one control session packet holds more
--   subpackets than the default TPer's MaxSubpackets, one (Opal 2.30 Table 17,
--   PDF 33): two or three data subpackets, each a Properties call the
--   Session Manager answers, no payload or random bytes. The TPer SHALL
--   discard and ignore such a packet (Core 5.2.2.4.1.1, PDF 145), a violation
--   of packet requirements (3.3.10.7 a, PDF 86); what ETC-15 sends (Test Cases
--   PDF 88).
genOverfullPayload :: Word16 -> Gen ByteString
genOverfullPayload c = comPacketOn c . pure . PacketFrame controlHeader =<< (chooseInt (2, 3) >>= (`vectorOf` (dataSubPacket <$> body)))
  where
    body = oneof [ pure (generate (invocation uSMUID (Methods.Properties Nothing)))
                 , pure mempty
                 , B.pack <$> (chooseInt (1, 32) >>= (`vectorOf` arbitrary)) ]


-- * Parts

-- | A ComID's synchronous protocol state.
genSync :: Gen SyncState
genSync = oneof [pure S1, S2 <$> genResponse, S3 <$> genResponse]

-- | Response packets as the Session Manager prepares them, one to three
--   control session packets, each of one or two data subpackets of up to
--   600 bytes: so that a transfer of one block holds none, some or all of
--   them.
genResponse :: Gen [ByteString]
genResponse = chooseInt (1, 3) >>= (`vectorOf` packet)
  where
    packet = (PacketFrame controlHeader <$> (chooseInt (1, 2) >>= (`vectorOf` subPacket))) `suchThatMap` (either (const Nothing) Just . encodePacket)
    subPacket = dataSubPacket . B.pack <$> (chooseInt (0, 600) >>= (`vectorOf` arbitrary))

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
