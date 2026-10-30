{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Wire
Description : The wire contract: interface commands over an abstract Transport, units, one command per ComID, the host's ComPacket exchange
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The host's side of design docs/spec-as-source.md section 6.1, over the types
of "Hsed.Wire.Internal", whose 'Transport' this module exports without its
constructor: a transport is built by its backend (@Hsed.Transport.*@) and
used here, IF-RECV at any capability ('ifRecv'), IF-SEND and events only at
@'ReadWrite@ ('ifSend', 'inject').

* Units (design 6.2): a transfer length in bytes from a count of the
  transport's units ('transferLength'), rounded up to them ('roundUp'), and
  the count a length asks for, refused when zero or over the transport's
  maximum ('unitsFor').
* One command in flight per ComID ('Channel', 'withComId'): a second IF-SEND
  on a ComID waits for the first exchange to end rather than being issued
  (which is why the jig's ETC-02, whose second IF-SEND is the point, sends
  past the channel).
* The host's exchange of a ComPacket ('exchangeComPacket'): IF-SEND, then
  IF-RECV until the TPer has returned everything, as Core 3.3.10 and Table 45
  (PDF 82-86) have the host do, polling while the TPer is busy and asking
  again with a larger length when the TPer names a minimum transfer
  (sedutil stops there instead, design 6.1).
-}
module Hsed.Wire
  ( -- * Requests and responses
    Dir (..)
  , Request (..)
  , IfStatus (..)
  , ifStatusName
  , ifStatusDetail
  , Response (..)
    -- * Events, units and capabilities
  , Event (..)
  , Unit (..)
  , Cap (..)
  , Describe (..)
    -- * Transports, abstract
  , Transport
  , ifRecv
  , ifSend
  , inject
  , unitOf
  , describeTransport
  , closeTransport
    -- * Units
  , transferLength
  , roundUp
  , unitsFor
    -- * One command in flight per ComID
  , Channel
  , newChannel
  , withComId
    -- * The host's exchange of a ComPacket
  , ExchangeOpts (..)
  , defaultExchangeOpts
  , ExchangeError (..)
  , exchangeComPacket
    -- * Bytes in JSON
  , trimmedHex
  ) where

import qualified Data.Aeson            as A
import qualified GHC.Clock             as Clock
import qualified RIO.ByteString        as B
import qualified RIO.Map               as Map
import           RIO

import           Hsed.Gen.Core.Layouts (ComPacketHeader (..))
import           Hsed.Packet           (ComPacketFrame (..), DecodedFrame (..), PacketFrame, decodeComPacket)
import           Hsed.Wire.Internal


-- * Transports

-- | IF-RECV: the TPer's answer to a security protocol and ComID, exactly the
--   transfer length's bytes.
ifRecv :: Transport c -> Word8 -> Word16 -> Word32 -> IO Response
ifRecv t p c len = tExchange t (Request IfRecv p c len mempty)

-- | IF-SEND of a payload, zero-padded to the transfer length (a payload longer
--   than the length is cut to it: the transfer carries that many bytes): a
--   @'ReadWrite@ transport's alone.
ifSend :: Transport 'ReadWrite -> Word8 -> Word16 -> Word32 -> ByteString -> IO Response
ifSend t p c len bytes = tExchange t (Request IfSend p c len (exactly len bytes))

-- | Cause an event (a power cycle of the simulator): a @'ReadWrite@
--   transport's alone.
inject :: Transport 'ReadWrite -> Event -> IO IfStatus
inject = tInject

-- | The unit a transport's transfer lengths count.
unitOf :: Transport c -> Unit
unitOf = tUnit

-- | What a transport says it is.
describeTransport :: Transport c -> Describe
describeTransport = tDescribe

-- | Close a transport (the kext's user client; nothing for the simulator).
closeTransport :: Transport c -> IO ()
closeTransport = tClose


-- * Units

-- | The bytes of a count of a transport's units: @n * 512@ on
--   'UnitBlocks512', @n@ on 'UnitBytes' (the Test Cases' @blocks 1@ is 512
--   bytes or one, SPF-02 case 2). A negative count is none.
transferLength :: Unit -> Int -> Word32
transferLength u n = fromIntegral (max 0 n) * unitBytes u

-- | A byte length rounded up to a whole number of the unit's bytes (a length
--   past the last whole number a 32-bit length holds, 0xFFFFFE00 bytes in
--   blocks, to that number).
roundUp :: Unit -> Word32 -> Word32
roundUp u len = fromInteger (min mostWhole (unitCount u len * b))
  where
    b         = toInteger (unitBytes u)
    mostWhole = toInteger (maxBound :: Word32) `div` b * b

-- | The count of units a byte length asks for, rounded up, given the most
--   bytes the transport carries (@unitsFor unit most len@): zero bytes, or
--   more than the most once rounded up, is 'OtherInvalidCommandParameter'
--   (SIIS 1.20 4.3.1.1-4.3.2.2, PDF 30-31).
unitsFor :: Unit -> Word32 -> Word32 -> Either IfStatus Word32
unitsFor u most len
  | len == 0                             = Left OtherInvalidCommandParameter
  | unitCount u len * b > toInteger most = Left OtherInvalidCommandParameter
  | otherwise                            = Right (fromInteger (unitCount u len))
  where b = toInteger (unitBytes u)

-- | The bytes of one of a unit.
unitBytes :: Unit -> Word32
unitBytes = \case
  UnitBlocks512 -> 512
  UnitBytes     -> 1

-- | How many of a unit a byte length takes, rounded up.
unitCount :: Unit -> Word32 -> Integer
unitCount u len = (toInteger len + b - 1) `div` b
  where b = toInteger (unitBytes u)


-- * One command in flight per ComID

-- | A lock per ComID: whoever holds a ComID's lock has the one exchange in
--   flight on it.
newtype Channel = Channel (IORef (Map Word16 (MVar ())))

-- | A channel with no ComID held.
newChannel :: IO Channel
newChannel = Channel <$> newIORef mempty

-- | Run an exchange holding a ComID's lock: another on the same ComID waits
--   until it ends.
withComId :: Channel -> Word16 -> IO a -> IO a
withComId (Channel locks) c act = do
  fresh <- newMVar ()
  lock  <- atomicModifyIORef' locks $ \m -> maybe (Map.insert c fresh m, fresh) (\l -> (m, l)) (Map.lookup c m)
  withMVar lock (const act)


-- * The host's exchange of a ComPacket

-- | How the host asks for a response: the IF-RECV length it starts with (the
--   TPer's MaxComPacketSize, once known), how long it waits before asking
--   again while the TPer has nothing ready, and how long it waits for one
--   command's answer.
data ExchangeOpts = ExchangeOpts
  { xRecvLen       :: !Word32
  , xPollMicros    :: !Int
  , xTimeoutMicros :: !Int
  }
  deriving (Eq, Show)

-- | 2048 bytes, the least MaxComPacketSize an Opal TPer reports (Opal 2.30
--   Table 17, PDF 33); 25 ms between polls, as sedutil waits
--   (DtaDevOpal.cpp's exec loop, design 6.1); 10 s for a command, the kext's
--   own limit (@SED_TIMEOUT@, UserKernelShared.h, design 6.1).
defaultExchangeOpts :: ExchangeOpts
defaultExchangeOpts = ExchangeOpts
  { xRecvLen       = 2048
  , xPollMicros    = 25000
  , xTimeoutMicros = 10000000
  }

-- | Why an exchange did not return the TPer's packets.
data ExchangeError
  = IfError !IfStatus  -- ^ a command answered with a status other than 'Good'
  | Malformed !Text    -- ^ a response that is no ComPacket of the ComID
  | TimedOut           -- ^ no answer within 'xTimeoutMicros'
  | Refused !Text      -- ^ the IF-SEND path refused the payload (the Guard), or a length the transport cannot carry
  deriving (Eq, Show)

instance A.ToJSON ExchangeError where
  toJSON = \case
    IfError s   -> A.object ["error" A..= ("if_error" :: Text), "status" A..= s]
    Malformed t -> A.object ["error" A..= ("malformed" :: Text), "detail" A..= t]
    TimedOut    -> A.object ["error" A..= ("timed_out" :: Text)]
    Refused t   -> A.object ["error" A..= ("refused" :: Text), "detail" A..= t]

-- | Send a ComPacket's bytes on a ComID and collect the packets of the
--   TPer's response: the IF-SEND through the path given (the transport's
--   own for the simulator, the Guard's for a drive), then IF-RECV, holding
--   the ComID's lock throughout. Each command is protocol 1's (the ComPacket
--   protocol, SIIS 1.20 Tables 12-13, PDF 30-31) and is given
--   'xTimeoutMicros' to answer. The IF-SEND's transfer length is the
--   ComPacket's bytes rounded up to the transport's unit, the payload zero
--   padded to it. Then each IF-RECV's answer is a ComPacket of the ComID
--   whose header says what to do (Core Table 45, PDF 83):
--
--   * MinTransfer not zero (row 2, or row 3 with this ComPacket's packets):
--     ask again at once, for that many bytes if more than were asked,
--     rounded up to the unit; a length the transport cannot carry is
--     'Refused', and a MinTransfer no larger than the length asked, with no
--     packets, is no row of the table ('Malformed');
--   * OutstandingData zero (rows 5 and 6): done, the packets so far
--     returned (none, on row 6 at once);
--   * otherwise (rows 1 and 4): the TPer is still processing; ask again
--     after 'xPollMicros', for as long as 'xTimeoutMicros' since the last
--     packets came ('TimedOut' after it).
--
--   Unlike sedutil's loop, which stops at any MinTransfer (DtaDevOpal.cpp's
--   exec loop, design 6.1), row 3 is followed to its end. An answer whose
--   extension is 0xFFFF says the ComID is inactive (Core 3.3.3.3, PDF 58):
--   'Malformed', as is one that is no ComPacket of the ComID.
exchangeComPacket
  :: ExchangeOpts
  -> (Request -> IO (Either Text Response))
  -> Channel
  -> Transport c
  -> Word16
  -> ByteString
  -> IO (Either ExchangeError [PacketFrame])
exchangeComPacket opts send chan t c bytes = withComId chan c $
  case (,) <$> carriable sendLen <*> carriable recvLen of
    Left why -> pure (Left (Refused why))
    Right _  -> timed (send (Request IfSend comPacketProtocol c sendLen (exactly sendLen bytes))) >>= \case
      Nothing         -> pure (Left TimedOut)
      Just (Left why) -> pure (Left (Refused why))
      Just (Right r)
        | status r /= Good -> pure (Left (IfError (status r)))
        | otherwise        -> Clock.getMonotonicTime >>= receive [] recvLen
  where
    -- the time in seconds is GHC.Clock's: RIO's (unliftio 0.2.25's) counts nanoseconds on macOS on Apple silicon
    u       = tUnit t
    sendLen = roundUp u (fromIntegral (B.length bytes))
    recvLen = roundUp u (xRecvLen opts)
    timed :: IO a -> IO (Maybe a)
    timed   = timeout (xTimeoutMicros opts)
    -- a length the transport carries, or why not
    carriable len = either (const (Left ("a transfer of " <> tshow len <> " bytes, which the transport cannot carry"))) Right
                           (unitsFor u (mostCarried u) len)
    -- IF-RECV of a length, the packets so far, the time the last ones came
    receive acc len since = timed (tExchange t (Request IfRecv comPacketProtocol c len mempty)) >>= \case
      Nothing -> pure (Left TimedOut)
      Just r
        | status r /= Good -> pure (Left (IfError (status r)))
        | otherwise        -> either (pure . Left . Malformed) (answered acc len since) (comPacketOf c (rpayload r))
    answered acc len since (h, ps)
      | comPacketMinTransfer h /= 0 =
          let len' = roundUp u (max (comPacketMinTransfer h) len)
          in if null ps && len' <= len
               then pure (Left (Malformed ("MinTransfer " <> tshow (comPacketMinTransfer h) <> " with no packets, to a request of "
                                           <> tshow len <> " bytes: no row of Core Table 45")))
               else either (pure . Left . Refused) (const (Clock.getMonotonicTime >>= receive acc' len')) (carriable len')
      | comPacketOutstandingData h == 0 = pure (Right acc')
      | otherwise = do
          now <- Clock.getMonotonicTime
          let since' = if null ps then since else now
          if now - since' > fromIntegral (xTimeoutMicros opts) / 1e6
            then pure (Left TimedOut)
            else threadDelay (xPollMicros opts) *> receive acc' len since'
      where acc' = acc <> ps

-- | The header and packets of an IF-RECV's payload, if it is a ComPacket of
--   the ComID, its padding after it.
comPacketOf :: Word16 -> ByteString -> Either Text (ComPacketHeader, [PacketFrame])
comPacketOf c bytes = case decodeComPacket bytes of
  Left e -> Left ("not a ComPacket: " <> tshow e)
  Right (DecodedFrame (ComPacketFrame h ps) _ _)
    | comPacketComID h /= c                          -> Left ("a ComPacket of ComID " <> tshow (comPacketComID h) <> ", not " <> tshow c)
    | comPacketComIDExtension h == inactiveExtension -> Left ("ComID " <> tshow c <> " is inactive: its ComPacket's extension is 0xFFFF (Core 3.3.3.3, PDF 58)")
    | otherwise                                      -> Right (h, ps)

-- | The ComID extension that tells the host its ComID is inactive (Core
--   3.3.3.1, PDF 57; 3.3.3.3 a.i.1, PDF 58).
inactiveExtension :: Word16
inactiveExtension = 0xFFFF

-- | The security protocol of ComPackets and Level 0 Discovery: TCG's 0x01
--   (SIIS 1.20 Tables 12-13, PDF 30-31; Core Table 25, PDF 53).
comPacketProtocol :: Word8
comPacketProtocol = 0x01

-- | The most bytes one command carries on a transport of a unit: 255 blocks
--   in 512-byte blocks, the count a SAT CDB and the kext shim carry (design
--   6.2, cbits/hsed_kext.h); 1 MiB in bytes, mh_transport's largest @len@
--   (design 6.3).
mostCarried :: Unit -> Word32
mostCarried = \case
  UnitBlocks512 -> 255 * 512
  UnitBytes     -> 1024 * 1024

-- | Exactly so many bytes: cut to the length, or zero-padded to it.
exactly :: Word32 -> ByteString -> ByteString
exactly len bytes = B.take n (bytes <> B.replicate (n - B.length bytes) 0)
  where n = fromIntegral len
