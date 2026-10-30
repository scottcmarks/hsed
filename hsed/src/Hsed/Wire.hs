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
  (sedutil stops there instead, design 6.1), within a bound on the whole.
* The wire's constants, each defined here alone: the ComPacket protocol, Level
  0 Discovery's ComID, the Data subpacket's Kind, the extension of an inactive
  ComID, the ComPacket header's size, the most a transport carries; and
  'exactly', a transfer's bytes, and 'hex', a number as the jig prints it.
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
    -- * The wire's constants
  , comPacketProtocol
  , level0ComId
  , dataKind
  , staticExtension
  , inactiveExtension
  , comPacketHeaderSize
  , mostCarried
  , exactly
    -- * Bytes in JSON, numbers in hex
  , trimmedHex
  , hex
  ) where

import qualified Data.Aeson            as A
import           Data.Bits             (FiniteBits (..))
import qualified GHC.Clock             as Clock
import           Numeric               (showHex)
import qualified RIO.ByteString        as B
import qualified RIO.Map               as Map
import           RIO
import qualified RIO.Text              as T

import           Hsed.Core.Code        (CodeRow (..), Span (..), codeRowsNamed)
import           Hsed.Core.Layout      (Layout (..))
import           Hsed.Core.Record      (LayoutRecord (..))
import           Hsed.Gen.Core.Codes   (cComIDAssignments, cSubpacketTypes)
import           Hsed.Gen.Core.Layouts (ComPacket, ComPacketHeader (..))
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
--   bytes or one, SPF-02 case 2). A negative count is none; a count past
--   what a 32-bit length holds is the last whole number of units it holds,
--   as 'roundUp' gives it, never a length that wrapped.
--
-- >>> map (transferLength UnitBlocks512) [1, 8388607, 8388608]
-- [512,4294966784,4294966784]
transferLength :: Unit -> Int -> Word32
transferLength u n = fromInteger (min (mostWhole u) (toInteger (max 0 n) * toInteger (unitBytes u)))

-- | A byte length rounded up to a whole number of the unit's bytes (a length
--   past the last whole number a 32-bit length holds, 0xFFFFFE00 bytes in
--   blocks, to that number).
roundUp :: Unit -> Word32 -> Word32
roundUp u len = fromInteger (min (mostWhole u) (unitCount u len * toInteger (unitBytes u)))

-- | The last length a 32-bit transfer length holds that is a whole number of
--   a unit's bytes.
mostWhole :: Unit -> Integer
mostWhole u = toInteger (maxBound :: Word32) `div` b * b
  where b = toInteger (unitBytes u)

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
--   again while the TPer has nothing ready, how long it waits for one
--   command's answer, and the bounds on the whole exchange: its time from
--   the IF-SEND, and the bytes of packets it collects.
data ExchangeOpts = ExchangeOpts
  { xRecvLen        :: !Word32
  , xPollMicros     :: !Int
  , xTimeoutMicros  :: !Int
  , xTotalMicros    :: !Int  -- ^ the whole exchange, from its IF-SEND: past it, 'TimedOut'
  , xResponseBytes  :: !Int  -- ^ the most bytes of packets it collects: past it, 'Malformed'
  }
  deriving (Eq, Show)

-- | 2048 bytes, the least MaxComPacketSize an Opal TPer reports (Opal 2.30
--   Table 17, PDF 33); 25 ms between polls, as sedutil waits
--   (DtaDevOpal.cpp's exec loop, design 6.1); 10 s for a command, the kext's
--   own limit (@SED_TIMEOUT@, UserKernelShared.h, design 6.1); a minute for
--   the whole exchange, six commands' time; and 1 MiB of packets, the most
--   one command carries on any transport ('mostCarried'), however many
--   IF-RECVs bring them.
defaultExchangeOpts :: ExchangeOpts
defaultExchangeOpts = ExchangeOpts
  { xRecvLen        = 2048
  , xPollMicros     = 25000
  , xTimeoutMicros  = 10000000
  , xTotalMicros    = 60000000
  , xResponseBytes  = fromIntegral (mostCarried UnitBytes)
  }

-- | Why an exchange did not return the TPer's packets.
data ExchangeError
  = IfError !IfStatus  -- ^ a command answered with a status other than 'Good'
  | Malformed !Text    -- ^ a response that is no ComPacket of the ComID, or more than 'xResponseBytes' of packets
  | TimedOut           -- ^ no answer within 'xTimeoutMicros', or the exchange past 'xTotalMicros'
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
--   'Malformed', as is one that is no ComPacket of the ComID, and one whose
--   extension is not the one the host sent (its ComPacket's; a static
--   ComID's 0x0000 if the bytes sent are no ComPacket), which tells the host
--   its ComID is in use elsewhere (Core 3.3.3.1, PDF 58).
--
--   The whole exchange is bounded, so that a TPer that keeps answering
--   cannot hold the host: past 'xTotalMicros' from the IF-SEND it is
--   'TimedOut', and past 'xResponseBytes' of packets collected 'Malformed'.
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
    Right _  -> do
      started <- Clock.getMonotonicTime
      timed (send (Request IfSend comPacketProtocol c sendLen (exactly sendLen bytes))) >>= \case
        Nothing         -> pure (Left TimedOut)
        Just (Left why) -> pure (Left (Refused why))
        Just (Right r)
          | status r /= Good -> pure (Left (IfError (status r)))
          | otherwise        -> Clock.getMonotonicTime >>= receive started [] 0 recvLen
  where
    -- the time in seconds is GHC.Clock's: RIO's (unliftio 0.2.25's) counts nanoseconds on macOS on Apple silicon
    u        = tUnit t
    sendLen  = roundUp u (fromIntegral (B.length bytes))
    recvLen  = roundUp u (xRecvLen opts)
    -- the extension the host's ComPacket carries, which every answer must carry back
    sentExt  = either (const staticExtension) (comPacketComIDExtension . cpfHeader . dfFrame) (decodeComPacket bytes)
    seconds  = (/ 1e6) . fromIntegral
    timed :: IO a -> IO (Maybe a)
    timed    = timeout (xTimeoutMicros opts)
    -- a length the transport carries, or why not
    carriable len = either (const (Left ("a transfer of " <> tshow len <> " bytes, which the transport cannot carry"))) Right
                           (unitsFor u (mostCarried u) len)
    -- IF-RECV of a length, within the whole exchange's time; the packets so far, their bytes, the time the last
    -- ones came
    receive started acc held len since = do
      now <- Clock.getMonotonicTime
      if now - started > seconds (xTotalMicros opts)
        then pure (Left TimedOut)
        else timed (tExchange t (Request IfRecv comPacketProtocol c len mempty)) >>= \case
          Nothing -> pure (Left TimedOut)
          Just r
            | status r /= Good -> pure (Left (IfError (status r)))
            | otherwise        -> either (pure . Left . Malformed) (answered started acc held len since)
                                         (comPacketOf c sentExt (rpayload r))
    answered started acc held len since (h, ps, n)
      | held' > xResponseBytes opts =
          pure (Left (Malformed ("more than " <> tshow (xResponseBytes opts) <> " bytes of packets in answer to one ComPacket")))
      | comPacketMinTransfer h /= 0 =
          let len' = roundUp u (max (comPacketMinTransfer h) len)
          in if null ps && len' <= len
               then pure (Left (Malformed ("MinTransfer " <> tshow (comPacketMinTransfer h) <> " with no packets, to a request of "
                                           <> tshow len <> " bytes: no row of Core Table 45")))
               else either (pure . Left . Refused) (const (Clock.getMonotonicTime >>= receive started acc' held' len'))
                           (carriable len')
      | comPacketOutstandingData h == 0 = pure (Right acc')
      | otherwise = do
          now <- Clock.getMonotonicTime
          let since' = if null ps then since else now
          if now - since' > seconds (xTimeoutMicros opts)
            then pure (Left TimedOut)
            else threadDelay (xPollMicros opts) *> receive started acc' held' len since'
      where
        acc'  = acc <> ps
        held' = held + n

-- | The header and packets of an IF-RECV's payload, and the bytes the
--   packets take, if it is a ComPacket of the ComID and of the extension
--   expected, its padding after it.
comPacketOf :: Word16 -> Word16 -> ByteString -> Either Text (ComPacketHeader, [PacketFrame], Int)
comPacketOf c ext bytes = case decodeComPacket bytes of
  Left e -> Left ("not a ComPacket: " <> tshow e)
  Right (DecodedFrame (ComPacketFrame h ps) _ rest)
    | comPacketComID h /= c                          -> Left ("a ComPacket of ComID " <> tshow (comPacketComID h) <> ", not " <> tshow c)
    | comPacketComIDExtension h == inactiveExtension -> Left ("ComID " <> tshow c <> " is inactive: its ComPacket's extension is 0xFFFF (Core 3.3.3.3, PDF 58)")
    | comPacketComIDExtension h /= ext               -> Left ("a ComPacket of extension " <> hex (comPacketComIDExtension h) <> ", not the "
                                                              <> hex ext <> " sent: the ComID is in use elsewhere (Core 3.3.3.1, PDF 58)")
    | otherwise                                      -> Right (h, ps, B.length bytes - comPacketHeaderSize - B.length rest)


-- * The wire's constants

-- | The security protocol of ComPackets and Level 0 Discovery: TCG's 0x01
--   (SIIS 1.20 Tables 12-13, PDF 30-31; Core 3.3.6.1, PDF 67; Test Cases
--   Table 5, PDF 26). Core Table 25 (PDF 53) prints 0x01 and 0x02 alike,
--   "Defined in this document", so the value is written here, once.
comPacketProtocol :: Word8
comPacketProtocol = 0x01

-- | Level 0 Discovery's ComID: Core Table 26's row \"Level 0 Device
--   Discovery\" (PDF 55), 0x0001. Were the row ever missing, 0x0000, the
--   reserved ComID, which no drive path admits: the Guard's Level0Only
--   refuses it, and the kext's transport admits only the ComID its shim's
--   templates carry ("Hsed.Transport.Cdb").
level0ComId :: Word16
level0ComId = fromMaybe 0 (assigned cComIDAssignments "Level 0 Device Discovery")

-- | The Kind of a Data subpacket: Core Table 19's row \"Data\" (PDF 43),
--   which 3.2.3.4.1.1.2 (PDF 44) says is zeroes.
dataKind :: Word16
dataKind = fromMaybe 0 (assigned cSubpacketTypes "Data")

-- | The one value a code table's row of a name assigns.
assigned :: Num a => [CodeRow] -> Text -> Maybe a
assigned rows name = case [ v | CodeRow (One v) _ <- codeRowsNamed rows name ] of
  [v] -> Just (fromInteger v)
  _   -> Nothing

-- | The extension of a ComID the TPer does not assign dynamically, a static
--   one (Core 3.3.3.1, PDF 57; Opal 2.30 3.3.3, PDF 30).
staticExtension :: Word16
staticExtension = 0x0000

-- | The ComID extension that tells the host its ComID is inactive (Core
--   3.3.3.1, PDF 57; 3.3.3.3 a.i.1, PDF 58).
inactiveExtension :: Word16
inactiveExtension = 0xFFFF

-- | The size of a ComPacket's header, Core Table 17's fixed part (PDF 40).
comPacketHeaderSize :: Int
comPacketHeaderSize = layoutSize (recordLayout (Proxy :: Proxy ComPacket))

-- | The most bytes one command carries on a transport of a unit: 255 blocks
--   in 512-byte blocks, the count a SAT CDB and the kext shim carry (design
--   6.2, cbits/hsed_kext.h); 1 MiB in bytes, mh_transport's largest @len@
--   (design 6.3). No transport carries more: the in-process one answers a
--   longer transfer Other Invalid Command Parameter.
mostCarried :: Unit -> Word32
mostCarried = \case
  UnitBlocks512 -> 255 * 512
  UnitBytes     -> 1024 * 1024

-- | Exactly so many bytes: cut to the length, or zero-padded to it.
exactly :: Word32 -> ByteString -> ByteString
exactly len bytes = B.take n (bytes <> B.replicate (n - B.length bytes) 0)
  where n = fromIntegral len

-- | A number in hex, as many digits as its type has: @0x1004@, @0x01@.
--
-- >>> hex (0x1004 :: Word16)
-- "0x1004"
hex :: (Integral a, FiniteBits a) => a -> Text
hex n = "0x" <> T.justifyRight (finiteBitSize n `div` 4) '0' (T.pack (showHex n ""))
