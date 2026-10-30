{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.ComIdMgmt
Description : ComID management, protocol 0x02: TPER_RESET, GET_COMID, HANDLE_COMID_REQUEST and GET_COMID_RESPONSE
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The commands of security protocol 0x02 ("Hsed.Wire".'Hsed.Wire.comIdManagementProtocol'),
outside the synchronous protocol (Core 3.3.10.3 b, PDF 84): TPER_RESET on
ComID 0x0004 (Opal 2.30 3.2.3 and Table 13, PDF 28-29, enabled by TPerInfo's
ProgrammaticResetEnable, "Hsed.Sim.Params".@spTPerResetEnabled@), GET_COMID
on 0x0000 (Core 3.3.4.3, PDF 61), and on a static ComID HANDLE_COMID_REQUEST
(VERIFY_COMID_VALID, STACK_RESET) and GET_COMID_RESPONSE (3.3.4.7, PDF
62-66, through the generated layouts of Tables 28-37), their responses kept
in the ComID's management buffer ("Hsed.Sim.Types".@cMgmt@).

/At the M4a scaffold/ every command is answered 'Unsupported', as M3 did;
group G2 writes them (design 11).
-}
module Hsed.Sim.ComIdMgmt
  ( command
  ) where

import           RIO

import           Hsed.Sim.Types (TPerState)
import           Hsed.Wire      (IfStatus (..), Request, Response (..))


-- | A command of protocol 0x02 answered, and the TPer after it.
command :: Request -> TPerState -> (Response, TPerState)
command _ st = (Response (Unsupported "protocol 0x02, ComID management (TPER_RESET, STACK_RESET): group G2 of M4a") mempty, st)
