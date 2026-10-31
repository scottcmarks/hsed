{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC04
Description : ETC-04, a packet on a session the TPer does not have
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-04 (Test Cases 1.02, PDF 78-79): a session opened to the Admin SP, then
Get on C_PIN_MSID in a packet whose Session names the session's TSN and the
HSN plus one, which the TPer cannot resolve and ignores (Core 3.3.10.7 a,
PDF 86: the law @Sync.ignorePacket@'s first clause), the next IF-RECV
answering (0, 0, 0); then the session's End of Session. The session is left
open through the bad packet, so the case opens one and closes it, the End
of Session its step 3 whatever step 2 came to.
-}
module Hsed.Jig.Cases.ETC04
  ( etc04
  ) where

import           RIO

import           Hsed.Core.CellBlock    (cellBlockValue, columnRange)
import           Hsed.Core.Method       (invocation)
import           Hsed.Core.StreamItem   (StreamItem (..))
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import qualified Hsed.Gen.Core.Methods  as Methods
import           Hsed.Jig.Case          (Case (..), Need (..))
import           Hsed.Jig.Catalogue     (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq        (Section (..), lockingSPUntouched)
import           Hsed.Jig.Script        (Script, SessionHandle (..), Verdict (..), adminSPAsAnybody, attempt, closingStep,
                                         exchangeOn, exchanged, expectNoResponse, namedColumn, observe, opened,
                                         sessionComPacket, startSession, unmet)
import           Hsed.OpalUIDs          (uC_PIN_MSID)
import           Hsed.Wire              (hex)


-- | The case.
etc04 :: Case
etc04 = Case
  { cKey        = CaseKey (ETC 4) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = [lockingSPUntouched]
  , cNeeds      = [NeedIfRecv, NeedIfSend]
  , cRun        = run
  , cExtensions = []
  }

-- | Step 1 StartSession, step 2 the Get in a packet of another Session,
--   step 3 End of Session.
run :: Script Verdict
run = do
  h <- startSession adminSPAsAnybody >>= exchanged "step 1: StartSession to the Admin SP" >>= opened "step 1: StartSession to the Admin SP"
  v <- attempt (Pass <$ step2 h)
  closingStep "step 3: End of Session" h v

-- | Step 2: Get on C_PIN_MSID's PIN column in a packet whose Session is
--   the session's TSN and its HSN plus one, which the TPer has no session
--   for: no response, the IF-RECV answering (0, 0, 0) (Core Table 45, PDF
--   83; 3.3.10.7 a, PDF 86).
step2 :: SessionHandle -> Script ()
step2 h = do
  pin <- namedColumn c_PINTableDesc "PIN"
  let other = h { shHSN = shHSN h + 1 }
      call  = generate (invocation uC_PIN_MSID (Methods.Get (cellBlockValue (columnRange pin pin))))
      name  = "step 2: Get on C_PIN_MSID in a packet of Session (" <> hex (shTSN other) <> ", " <> hex (shHSN other) <> "), not the session's ("
              <> hex (shTSN h) <> ", " <> hex (shHSN h) <> ")"
  bytes <- either (unmet . ("step 2: the Get ComPacket does not encode: " <>) . tshow) pure (sessionComPacket other call)
  exchangeOn (shComId h) bytes >>= expectNoResponse name
  observe "step 2" "the IF-RECV answered (0, 0, 0): all responses returned, no further data (Core Table 45, PDF 83)"
