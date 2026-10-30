{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Law.Sync
Description : The synchronous protocol's laws (Core 3.3.10): what the simulator's S1, S2 and S3 hold to
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The laws the comments of Core 3.3.10 bind (design example E10, milestone
M3): each names the paragraph or table it implements by the first 12 hex
digits of its IR hash (@spec/ir/core-2.01.json@), and the Test Cases'
cases that observe it. Their properties ('lawProp') are milestone M3's group
G1's, over "Hsed.Sim" and "Hsed.Sim.Gen"; until it lands they are none.
-}
module Hsed.Law.Sync
  ( ifSendWhileBusy
  , discardToS1
  , ignorePacket
  , table45
  , laws
  ) where

import           RIO

import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Law           (Law (..))


-- | Core 3.3.10.2.1 item 3 a (PDF 82, @core-2.01 | 3.3.10.2.1#3a@): an
--   IF-SEND on a ComID whose previous IF-SEND's response the host has not
--   retrieved SHALL be aborted with Synchronous Protocol Violation; the
--   TPer's state is as it was. ETC-02 observes it.
ifSendWhileBusy :: Law
ifSendWhileBusy = Law
  { lawName  = "Sync.ifSendWhileBusy"
  , lawHash  = "37bedfb19343"
  , lawProp  = Nothing
  , lawCases = [whole (ETC 2)]
  }

-- | Core 3.3.10.6, S2:S1 item d.c (PDF 86, @core-2.01 | 3.3.10.6#dc@): an
--   IF-SEND whose packets' subpackets have no payload or Empty atoms alone
--   returns the ComID to Awaiting IF-SEND, and the next IF-RECV has nothing
--   to return (design 8.2 decides it at the IF-SEND). SPF-02 case 1's
--   simulator extension observes it.
discardToS1 :: Law
discardToS1 = Law
  { lawName  = "Sync.discardToS1"
  , lawHash  = "cb4d5d489fc0"
  , lawProp  = Nothing
  , lawCases = [caseOf (SPF 2) 1]
  }

-- | Core 3.3.10.7 item a (PDF 86, @core-2.01 | 3.3.10.7#a@): a packet whose
--   session the TPer cannot resolve is ignored, and the TPer awaits an
--   IF-SEND at once. ETC-08 and ETC-15 observe it (M4a).
ignorePacket :: Law
ignorePacket = Law
  { lawName  = "Sync.ignorePacket"
  , lawHash  = "cda4e92af591"
  , lawProp  = Nothing
  , lawCases = [whole (ETC 8), whole (ETC 15)]
  }

-- | Core Table 45 (PDF 83), bound by its comment @stateTable
--   "Sync.table45"@ ("Hsed.Gen.Core.StateTables".@table45@): the ComPacket
--   header fields an IF-RECV is answered with in each state. SPF-02 case 1
--   and ETC-02 observe it.
table45 :: Law
table45 = Law
  { lawName  = "Sync.table45"
  , lawHash  = "498ea00e3c6a"
  , lawProp  = Nothing
  , lawCases = [caseOf (SPF 2) 1, whole (ETC 2)]
  }

-- | Every law of the module.
laws :: [Law]
laws = [ifSendWhileBusy, discardToS1, ignorePacket, table45]

-- | A test, all of it.
whole :: TestId -> CaseKey
whole t = CaseKey t Nothing Nothing Nothing

-- | One case of a test.
caseOf :: TestId -> Int -> CaseKey
caseOf t c = CaseKey t (Just c) Nothing Nothing
