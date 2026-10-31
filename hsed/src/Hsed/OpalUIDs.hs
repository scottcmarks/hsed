{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.OpalUIDs
Description : The Opal 2.30 UIDs the simulator and the Guard need before the Admin SP's preconfiguration compiles (M4b), hand-written with their pages
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Opal 2.30's Admin SP preconfiguration (its Tables 18-29) is read by no rule
until M4b (design 11), so the few of its UIDs that M4a's simulator rows
("Hsed.Sim.AdminSP"), the Guard's allow list ("Hsed.Jig.Guard") and the jig's
UCT-03 need are written here by hand, each with the table and page it is
printed on, as design 13.17 has hand-written values cite theirs. Core 2.01's
own UIDs ("Hsed.Gen.Core.UIDs") are generated and not repeated: C_PIN_SID,
the Admin SP, the Base Template authorities and Get. M4b replaces this
module with the generated @Hsed.Gen.Opal@ preconfiguration, and a one-line
test then holds the Guard's hand UID to the generated one before M4c's drive
run (design 9.3).
-}
module Hsed.OpalUIDs
  ( -- * Objects of the Admin SP
    uC_PIN_MSID
  , uC_PIN_Admin1
  , uAuthority_Admin1
  , uACE_C_PIN_SID_Get_NOPIN
  , uACE_C_PIN_MSID_Get_PIN
  , uSP_Locking
    -- * ComIDs
  , tperResetComId
  ) where

import           RIO

import           Hsed.Core.UID (UID, uid)


-- | C_PIN_MSID, the C_PIN object holding the MSID (Opal 2.30 Table 25, PDF 48).
uC_PIN_MSID :: UID
uC_PIN_MSID = uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x84 0x02

-- | C_PIN_Admin1, Admin1's credential (Opal 2.30 Table 25, PDF 49).
uC_PIN_Admin1 :: UID
uC_PIN_Admin1 = uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x02 0x01

-- | Admin1, the Admin SP's first Admin authority, disabled at the factory
--   (Opal 2.30 Table 24, PDF 48).
uAuthority_Admin1 :: UID
uAuthority_Admin1 = uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x01

-- | ACE_C_PIN_SID_Get_NOPIN: Admins or SID may Get C_PIN_SID's columns other
--   than its PIN (Opal 2.30 Table 23, PDF 47).
uACE_C_PIN_SID_Get_NOPIN :: UID
uACE_C_PIN_SID_Get_NOPIN = uid 0x00 0x00 0x00 0x08 0x00 0x00 0x8C 0x02

-- | ACE_C_PIN_MSID_Get_PIN: Anybody may Get C_PIN_MSID's UID and PIN (Opal
--   2.30 Table 23, PDF 47), the ACE UCT-03 step 1b relies on.
uACE_C_PIN_MSID_Get_PIN :: UID
uACE_C_PIN_MSID_Get_PIN = uid 0x00 0x00 0x00 0x08 0x00 0x00 0x8C 0x04

-- | The Locking SP's row of the Admin SP's SP table (Opal 2.30 Table 29, PDF
--   50), Manufactured-Inactive at the factory.
uSP_Locking :: UID
uSP_Locking = uid 0x00 0x00 0x02 0x05 0x00 0x00 0x00 0x02

-- | The ComID of the TPER_RESET command, an IF-SEND of protocol 0x02 (Opal
--   2.30 Table 13, PDF 29; Table 14, PDF 30).
tperResetComId :: Word16
tperResetComId = 0x0004
