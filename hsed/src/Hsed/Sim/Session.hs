{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Sim.Session
Description : A regular session's packets processed: each invocation dispatched through the access control and the behaviours, End of Session, an abort
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The regular-session half of the synchronous protocol (design 8.2, M4a): a
packet whose Session resolves to an open session ("Hsed.Sim.State") is read
as a session reads it ("Hsed.Core.Stream", 'SessionStream'), each invocation
dispatched ('dispatch': the access control gate first, "Hsed.Sim.Access",
then the behaviour of the method's record, "Hsed.Sim.Base"), the TPer's End
of Session echoed and the session closed (Core 3.2.2.3.3.3, PDF 37;
3.3.7.1.5, PDF 75), and a fault aborting the session with the results
completed before it and the TPer's CloseSession where the run prepares one
(3.2.2.4.2, PDF 39; 3.3.10.7 b, PDF 86; Opal 2.30 3.3.4.1.3, PDF 31).
"Hsed.Sim.Sync" resolves the packets and reads their subpackets' atoms;
'sessionPayload' takes one data subpacket's items.

The responses are framed within the host's properties the session keeps
('sesHostProps', Core 5.2.2.4, PDF 144-145): each response a data subpacket
of its own, the subpackets packed into packets of the session within the
host's MaxSubpackets and MaxPacketSize ('packets'; 5.2.2.4.1.1-2, PDF 145),
a response that fits no packet, or no ComPacket the host takes, answered
instead with an empty list and RESPONSE_OVERFLOW (Opal 2.30 3.3.1, PDF 29).
The control session's answers are framed the same way, by the same code
('fits', 'packets', 'controlHeader').
-}
module Hsed.Sim.Session
  ( -- * A session's invocations and packets
    dispatch
  , sessionPayload
    -- * Framing responses
  , controlHeader
  , sessionHeader
  , fits
  , packets
  ) where

import qualified RIO.ByteString          as B
import qualified RIO.Map                 as Map
import           RIO

import           Hsed.Core.Call          (MethodCall (..), MethodHeader (..), MethodResponse (..))
import           Hsed.Core.Stream        (Fault (..), Item (..), Place (..))
import           Hsed.Core.StreamItem    (StreamItem (generate))
import           Hsed.Core.Token         (Token (..))
import           Hsed.Gen.Core.Codes     (StatusCode (..), pattern FAIL, pattern INVALID_PARAMETER, pattern NOT_AUTHORIZED,
                                          pattern RESPONSE_OVERFLOW, pattern SUCCESS)
import           Hsed.Gen.Core.Layouts   (DataSubPacket (..), DataSubPacketHeader (..), PacketHeader (..))
import           Hsed.Packet             (PacketFrame (..), encodePacket)
import           Hsed.Sim.Access         (grantedColumns)
import qualified Hsed.Sim.Base           as Base
import           Hsed.Sim.Behaviour      (Invoked (..), Reply (..), runSome)
import           Hsed.Sim.SessionManager (closeSessionCall)
import           Hsed.Sim.State          (abortSessions, closeSession)
import           Hsed.Sim.Types          (ComIdState (..), Limits (..), Outcome (..), Session (..), SimParams (..), TPerState (..),
                                          comIdHostLimits, initialHostProperties, tperLimits)
import           Hsed.Wire               (comPacketHeaderSize, dataKind)


-- * A session's invocations and packets

-- | One invocation in a session: a host status other than 0 echoed with an
--   empty result, the method aborted (Core 3.2.4.2, PDF 49); no
--   AccessControl row for the InvokingID and MethodID in the session's SP,
--   or an ACL unsatisfied by the session's authorities, NOT_AUTHORIZED with
--   an empty result (5.1.5.2, PDF 137; which also answers a method the TPer
--   does not recognize, 3.2.4.2); else the behaviour of the method's record
--   run ("Hsed.Sim.Base".'Base.behaviours', through
--   "Hsed.Sim.Behaviour".'runSome'), its parameters read against the
--   signature, a ParamError INVALID_PARAMETER (5.1.5.11 d, PDF 139); a
--   behaviour that answers other than results, FAIL.
dispatch :: Session -> MethodCall -> TPerState -> (MethodResponse, TPerState)
dispatch ses mc@(MethodCall (MethodHeader invoker method) _ status) st
  | status /= statusValue SUCCESS = (MethodResponse [] status, st)
  | not granted                   = (MethodResponse [] (statusValue NOT_AUTHORIZED), st)
  | otherwise = case mapMaybe (\b -> runSome b invoked mc st) Base.behaviours of
      []                                  -> (MethodResponse [] (statusValue NOT_AUTHORIZED), st)
      [Right (ReplyResults vs code, st')] -> (MethodResponse vs (statusValue code), st')
      [Left _]                            -> (MethodResponse [] (statusValue INVALID_PARAMETER), st)
      _                                   -> (MethodResponse [] (statusValue FAIL), st)
  where
    granted = isJust (Map.lookup (sesSP ses) (tsSPs st) >>= \sps -> grantedColumns sps ses invoker method)
    invoked = Invoked { ivComId = sesComId ses, ivSession = Just ses, ivInvoker = invoker, ivStatus = status }

-- | One data subpacket of a session's packet, its items as
--   "Hsed.Core.Stream".@items@ read them with the fault that stopped the
--   reading: the outcome and the TPer after it.
--
--   The items in order, the TPer's state threaded:
--
--   * an invocation past the TPer's MaxMethods aborts the session, the
--     results completed before it sent (5.2.2.4.1.7, PDF 146);
--   * an invocation read whole is dispatched ('dispatch');
--   * one whose header does not read answers an empty result and
--     NOT_AUTHORIZED, one whose parameter list does not read an empty result
--     and INVALID_PARAMETER (3.2.2.4.2 items 3-4, PDF 39);
--   * the host's End of Session is answered with the TPer's own, after the
--     results, and the session closes (3.2.2.3.3.3, PDF 37; 3.3.7.1.5, PDF
--     75; what follows was ignored by the reading);
--   * a transaction control token aborts the session: the TPer starts no
--     transaction at M4a (3.2.2.3.3.4, PDF 37);
--   * the fault that stopped the reading, a token out of place or a stream
--     ending inside an invocation, aborts the session (3.2.2.4.2 items 1, 2
--     and 5; 3.3.10.3 a, PDF 83-84), and so does a fault the caller found
--     before reading, a packet over the TPer's MaxSubpackets or
--     MaxPacketSize (5.2.2.4.1.1-2, PDF 145).
--
--   The responses are framed as the module's description says, each
--   checked to fit as it is made: one that does not is answered with an
--   empty list and RESPONSE_OVERFLOW and the processing of the payload ends
--   there (Opal 2.30 3.3.1, PDF 29). No items and no fault is 'Prepared' of
--   no packets, the TPer unchanged (no action, Core 3.3.10.6 S2:S1 d, PDF
--   85-86).
--
--   An abort is 'SessionAborted': the packets of the results completed
--   before it, the TPer's CloseSession on the control session, with the
--   session's HSN and TSN ("Hsed.Sim.SessionManager".'closeSessionCall'),
--   where the run prepares one ('spCloseSessionOnAbort'; 3.3.10.7 b, PDF 86;
--   3.3.7.1.5 item 3, PDF 75), and the session gone
--   ("Hsed.Sim.State".'abortSessions').
sessionPayload :: Session -> [Item] -> Maybe Fault -> TPerState -> (Outcome, TPerState)
sessionPayload ses items fault st0 = go st0 [] (0 :: Int) items
  where
    p    = tsParams st0
    tper = tperLimits p
    host = comIdHostLimits p (sesHostProps ses)
    hdr  = sessionHeader ses
    go st done n = \case
      []          -> maybe (prepared st done Nothing) (const (abort st done)) fault
      item : rest -> case item of
        Invoke mc
          | n >= lMethods tper -> abort st done
          | otherwise          -> let (resp, st') = dispatch ses mc st in respond st' done (n + 1) resp rest
        BadCall f
          | n >= lMethods tper -> abort st done
          | otherwise -> case faultPlace f of
              InHeader       -> respond st done (n + 1) (MethodResponse [] (statusValue NOT_AUTHORIZED)) rest
              InParameters _ -> respond st done (n + 1) (MethodResponse [] (statusValue INVALID_PARAMETER)) rest
              _              -> abort st done
        EndOfSessionItem       -> prepared (closeSession (sesComId ses) (sesTSN ses) st) done (Just (generate EndOfSession))
        StartTransactionItem _ -> abort st done
        EndTransactionItem _   -> abort st done
    -- a response that fits goes on; one that does not is RESPONSE_OVERFLOW's, and ends the payload's processing
    respond st done n resp rest
      | fits host hdr (generate resp) = go st (done <> [generate resp]) n rest
      | otherwise                     = prepared st (done <> [generate (MethodResponse [] (statusValue RESPONSE_OVERFLOW))]) Nothing
    prepared st done eos = case packets host hdr (withEnd done eos) of
      Right ps -> (Prepared ps, st)
      Left _   -> abort st []
    -- the TPer's End of Session after the last response, in its subpacket where it fits, else in one of its own
    withEnd done = \case
      Nothing  -> done
      Just eos -> case reverse done of
        []                                        -> [eos]
        lastOne : before | fits host hdr (lastOne <> eos) -> reverse before <> [lastOne <> eos]
                         | otherwise                      -> done <> [eos]
    abort st done = (SessionAborted (either (const []) id (packets host hdr done)) closing, abortSessions same st)
      where
        closing = if spCloseSessionOnAbort p then closeSessionPacket st else Nothing
        same s  = sesComId s == sesComId ses && sesTSN s == sesTSN ses
    -- the TPer's CloseSession framed as a control session packet within the properties its ComID keeps of the host
    closeSessionPacket st = either (const Nothing) listToMaybe (packets (comIdHostLimits p props) controlHeader [generate (closeSessionCall ses)])
      where props = maybe initialHostProperties cHostProps (Map.lookup (sesComId ses) (tsComIds st))


-- * Framing responses

-- | The header of a response packet on the control session: Session 0
--   (Core 3.3.7.1.2, PDF 73), SeqNumber, AckType and Acknowledgement 0, the
--   synchronous protocol using neither sequence numbers nor
--   acknowledgements (5.2.2.4.1.9-10, PDF 146-147).
controlHeader :: PacketHeader
controlHeader = PacketHeader 0 0 0 0 0

-- | The header of a response packet on a regular session: its TSN then its
--   HSN (Core 3.3.7.1.2, PDF 73), the rest 0 as the control session's.
sessionHeader :: Session -> PacketHeader
sessionHeader ses = PacketHeader (sesTSN ses) (sesHSN ses) 0 0 0

-- | Whether a token stream, as the one data subpacket of a packet with this
--   header, fits the host: the packet within its MaxPacketSize (Core
--   5.2.2.4.1.2, PDF 145) and, with the ComPacket header, within its
--   MaxComPacketSize (5.2.2.4.1.4, PDF 146; Opal 2.30 3.3.1, PDF 29: a
--   response fits the response buffer or is RESPONSE_OVERFLOW's).
fits :: Limits -> PacketHeader -> ByteString -> Bool
fits host hdr stream =
  either (const False) (\bytes -> B.length bytes <= lPacketSize host && comPacketHeaderSize + B.length bytes <= lComPacketSize host)
         (framed hdr [stream])

-- | Token streams, each a data subpacket, packed in order into packets with
--   this header within the host's MaxSubpackets and MaxPacketSize (Core
--   5.2.2.4.1.1-2, PDF 145): each packet takes the next subpackets while
--   both hold. Left where a stream fits no packet of its own ('fits'), with
--   why; the packets are then for an IF-RECV to return, as many as the
--   host's MaxPackets at a time ("Hsed.Sim.Sync").
packets :: Limits -> PacketHeader -> [ByteString] -> Either Text [ByteString]
packets host hdr streams = do
  unless (all (fits host hdr) streams) $
    Left ("the host's packet of " <> tshow (lPacketSize host) <> " bytes, or its ComPacket of " <> tshow (lComPacketSize host)
          <> ", cannot carry a response (Core 5.2.2.4.1.2 and 4, PDF 145-146)")
  traverse (framed hdr) (grouped streams)
  where
    grouped = \case
      []       -> []
      s : rest -> let (group, after) = extend [s] rest in group : grouped after
    extend group = \case
      s : rest | length group < lSubpackets host, fitsPacket (group <> [s]) -> extend (group <> [s]) rest
      rest -> (group, rest)
    fitsPacket ss = either (const False) ((<= lPacketSize host) . B.length) (framed hdr ss)

-- | A packet with a header of data subpackets of these streams, encoded.
framed :: PacketHeader -> [ByteString] -> Either Text ByteString
framed hdr streams = first (("the response does not frame: " <>) . tshow) (encodePacket (PacketFrame hdr (map (DataSubPacket (DataSubPacketHeader dataKind)) streams)))
