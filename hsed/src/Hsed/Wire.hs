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

The bodies are milestone M3's group G1's; until it lands they are stubs.
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

import qualified Data.Aeson         as A
import           RIO

import           Hsed.Packet        (PacketFrame)
import           Hsed.Wire.Internal


-- * Transports

-- | IF-RECV: the TPer's answer to a security protocol and ComID, exactly the
--   transfer length's bytes.
ifRecv :: Transport c -> Word8 -> Word16 -> Word32 -> IO Response
ifRecv = error "G1: ifRecv"

-- | IF-SEND of a payload, zero-padded to the transfer length: a @'ReadWrite@
--   transport's alone.
ifSend :: Transport 'ReadWrite -> Word8 -> Word16 -> Word32 -> ByteString -> IO Response
ifSend = error "G1: ifSend"

-- | Cause an event (a power cycle of the simulator): a @'ReadWrite@
--   transport's alone.
inject :: Transport 'ReadWrite -> Event -> IO IfStatus
inject = error "G1: inject"

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
--   bytes or one, SPF-02 case 2).
transferLength :: Unit -> Int -> Word32
transferLength = error "G1: transferLength"

-- | A byte length rounded up to a whole number of the unit's bytes.
roundUp :: Unit -> Word32 -> Word32
roundUp = error "G1: roundUp"

-- | The count of units a byte length asks for, rounded up, given the most
--   bytes the transport carries: zero bytes, or more than the most, is
--   'OtherInvalidCommandParameter' (SIIS 1.20 4.3.1.1-4.3.2.2, PDF 30-31).
unitsFor :: Unit -> Word32 -> Word32 -> Either IfStatus Word32
unitsFor = error "G1: unitsFor"


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
withComId = error "G1: withComId"


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
--   the ComID's lock throughout.
exchangeComPacket
  :: ExchangeOpts
  -> (Request -> IO (Either Text Response))
  -> Channel
  -> Transport c
  -> Word16
  -> ByteString
  -> IO (Either ExchangeError [PacketFrame])
exchangeComPacket = error "G1: exchangeComPacket"
