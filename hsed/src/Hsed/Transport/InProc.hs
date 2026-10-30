{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.InProc
Description : The simulated TPer as a 'ReadWrite transport in the jig's own process
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The in-process transport (design 7.1): the TPer's state in a variable, each
command a step of "Hsed.Sim", the unit the run chooses, and the release of a
ComID's prepared response ('Hsed.Sim.Types.Tick') after every IF-SEND,
unless the run holds it for a number of IF-RECVs (@--hold-processing@), so
that the host's loop sees the TPer still processing (Core Table 45 row 1,
PDF 83). A @'ReadWrite@ transport, with a 'SimControl' the jig's simulator
extensions use: release a ComID now, look at the state.

Like every transport it carries at most "Hsed.Wire".'mostCarried' bytes in
one command (255 blocks, or 1 MiB in bytes): a longer transfer, either way,
is answered Other Invalid Command Parameter (SIIS 1.20 4.3.1.2 and 4.3.2.2,
PDF 30-31) before the TPer sees it, so that no answer is built of a length
no transport could carry.
-}
module Hsed.Transport.InProc
  ( InProcOpts (..)
  , SimControl (..)
  , newInProc
  ) where

import qualified RIO.Map            as Map
import           RIO

import           Hsed.Sim           (step, syncOf)
import           Hsed.Sim.Types     (Input (..), Output (..), SimParams (..), SyncState (..), TPerState, initialState)
import           Hsed.Version       (hsedVersion)
import           Hsed.Wire          (mostCarried)
import           Hsed.Wire.Internal (Cap (..), Describe (..), Dir (..), Event, IfStatus (..), Request (..), Response (..),
                                     Transport (..), Unit)


-- | The transport's unit, and how many IF-RECVs a ComID's prepared response
--   is held in S2 for (0: released at once).
data InProcOpts = InProcOpts
  { ioUnits       :: !Unit
  , ioHoldIfRecvs :: !Int
  }
  deriving (Eq, Show)

-- | What the jig may do to the simulator besides commands: release a ComID's
--   prepared response (S2 to S3), and read the state.
data SimControl = SimControl
  { scRelease :: Word16 -> IO ()
  , scPeek    :: IO TPerState
  }

-- | The simulator behind the transport: the TPer, and for each ComID holding
--   a prepared response, how many IF-RECVs it has answered in S2.
data Sim = Sim !TPerState !(Map Word16 Int)

-- | A simulated TPer from its parameters, as a transport and its control.
--   The TPer counts transfer lengths in the transport's unit ('ioUnits'
--   stands for 'spUnits'). After an IF-SEND that leaves its ComID
--   processing (S2), the prepared response is released at once
--   ('ioHoldIfRecvs' 0), or after that many IF-RECVs on the ComID have been
--   answered Response(s) to come (0, 1, 0), before the next one. An event
--   resets the TPer ("Hsed.Sim".'step') and drops what is held. The
--   transport says it is a TPer, the simulator of this build, of Opal 2.30,
--   with the parameters' ComIDs.
newInProc :: SimParams -> InProcOpts -> IO (Transport 'ReadWrite, SimControl)
newInProc params opts = do
  let p = params { spUnits = ioUnits opts }
  var <- newMVar (Sim (initialState p) mempty)
  let transport = MkTransport
        { tExchange = \r -> if lenBytes r > mostCarried (ioUnits opts)
                                then pure (Response OtherInvalidCommandParameter mempty)
                                else modifyMVar var (pure . exchange (ioHoldIfRecvs opts) r)
        , tInject   = \e -> modifyMVar var (pure . event e)
        , tUnit     = ioUnits opts
        , tDescribe = Describe
            { dRole      = "tper"
            , dImpl      = "hsed-sim " <> hsedVersion
            , dUnits     = ioUnits opts
            , dSsc       = Just "opal-2.30"
            , dBaseComId = Just (spBaseComId p)
            , dNComIds   = Just (spNComIds p)
            , dDevice    = Nothing
            }
        , tClose    = pure ()
        }
      control = SimControl
        { scRelease = \c -> modifyMVar_ var (\(Sim st held) -> pure (Sim (tick c st) (Map.delete c held)))
        , scPeek    = (\(Sim st _) -> st) <$> readMVar var
        }
  pure (transport, control)

-- | One command against the simulator, holding or releasing a ComID's
--   prepared response as the transport's hold says: an IF-SEND that
--   prepares one starts its count of IF-RECVs at none, and releases it at
--   once if the hold is none; an IF-RECV first releases it if its count has
--   reached the hold, and is counted if it finds the ComID still
--   processing.
exchange :: Int -> Request -> Sim -> (Sim, Response)
exchange hold r (Sim st held) = case dir r of
  IfSend ->
    let (resp, st') = command st
    in (if processing st' && not (processing st) then due (Sim st' (Map.insert c 0 held)) else Sim st' held, resp)
  IfRecv ->
    let Sim st' held' = due (Sim st held)
        (resp, st'')  = command st'
    in (Sim st'' (if processing st'' then Map.insertWith (+) c 1 held' else Map.delete c held'), resp)
  where
    c = comId r
    -- a response held for as many IF-RECVs as the hold says is released
    due (Sim s h)
      | processing s && Map.findWithDefault 0 c h >= hold = Sim (tick c s) (Map.delete c h)
      | otherwise                                         = Sim s h
    processing s = case syncOf c s of
      Just (S2 _) -> True
      _           -> False
    command s = case step (Cmd r) s of
      (Out resp, s') -> (resp, s')
      (_, s')        -> (Response (TransportFailure "the simulator answered a command with no response") mempty, s')

-- | An event against the simulator: the TPer reset, nothing held.
event :: Event -> Sim -> (Sim, IfStatus)
event e (Sim st _) = case step (Ev e) st of
  (Evented s, st') -> (Sim st' mempty, s)
  (_, st')         -> (Sim st' mempty, TransportFailure "the simulator answered an event with no status")

-- | A ComID's prepared response released.
tick :: Word16 -> TPerState -> TPerState
tick c = snd . step (Tick c)
