{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : Hand.SpecialPurposeUIDs
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Special Purpose UIDs.

The 2019 hand transcription, kept as the regenerate-and-compare oracle's test
fixture (docs/spec-as-source.md sections 5.5 and 13.20); compiled only by
hsed's test suite. TCG's text: see tcg/TCG-NOTICE.txt.

-}


module Hand.SpecialPurposeUIDs where

import           Data.Word

import           Hsed.Core.UID


{-

                                   Table 239 Special Purpose UIDs
    +-----------------------+--------------------------------------------------------------------+
    |UID                    |Purpose                                                             |
    +-----------------------+--------------------------------------------------------------------+
    |00 00 00 00 00 00 00 00|Used to represent null uid                                          |
    +-----------------------+--------------------------------------------------------------------+
    |00 00 00 00 00 00 00 01|Used as the SPUID, the UID that identifies "This SP" -- used as the |
    |                       |InvokingID for invocation of SP methods                             |
    +-----------------------+--------------------------------------------------------------------+
    |00 00 00 00 00 00 00 FF|Used as the SMUID, the UID that identifies "the Session manager" -- |
    |                       |used as InvokingID for invocation of Session Manager layer methods  |
    +-----------------------+--------------------------------------------------------------------+
    |00 00 00 00 00 00 FF xx|Identifies UIDs assigned to Session Manager layer methods, where xx |
    |                       |is the UID assigned to a particular method (see Table 241)          |
    +-----------------------+--------------------------------------------------------------------+
    |00 00 00 0B 00 00 00 01|Used in the C_PIN table's CharSet column to indicate that the GenKey|
    |                       |character set is not restricted (all byte values are legal).        |
    +-----------------------+--------------------------------------------------------------------+



-}

-- | Indicate that no object is being referenced.
uNULL   :: UID
uNULL                        = uid  0x00 0x00 0x00 0x00  0x00 0x00 0x00 0x00

-- | For SP methods invoked within a session
uThisSP :: UID
uThisSP                      = uid  0x00 0x00 0x00 0x00  0x00 0x00 0x00 0x01

-- | For methods invoked at the Session Manager Layer
uSMUID  :: UID
uSMUID                       = uid  0x00 0x00 0x00 0x00  0x00 0x00 0x00 0xFF

-- |
uSMMethodUID :: Word8 -> UID
uSMMethodUID                 = uid  0x00 0x00 0x00 0x00  0x00 0x00 0xFF

-- | For SP methods invoked within a session
uC_PINCharSetUnrestrictedUID :: UID
uC_PINCharSetUnrestrictedUID = uid  0x00 0x00 0x00 0x00B  0x00 0x00 0x00 0x01
{-

        Table 241 Session Manager Method UIDs
    +-----------------------+-------------------+
    |Method UID             |Method Name        |
    +-----------------------+-------------------+
    |00 00 00 00 00 00 FF 01|Properties         |
    +-----------------------+-------------------+
    |00 00 00 00 00 00 FF 02|StartSession       |
    +-----------------------+-------------------+
    |00 00 00 00 00 00 FF 03|SyncSession        |
    +-----------------------+-------------------+
    |00 00 00 00 00 00 FF 04|StartTrustedSession|
    +-----------------------+-------------------+
    |00 00 00 00 00 00 FF 05|SyncTrustedSession |
    +-----------------------+-------------------+
    |00 00 00 00 00 00 FF 06|CloseSession       |
    +-----------------------+-------------------+

-}
uProperties          :: UID
uProperties                  = uSMMethodUID 0x01

uStartSession        :: UID
uStartSession                = uSMMethodUID 0x02

uSyncSession         :: UID
uSyncSession                 = uSMMethodUID 0x03

uStartTrustedSession :: UID
uStartTrustedSession         = uSMMethodUID 0x04

uSyncTrustedSession  :: UID
uSyncTrustedSession          = uSMMethodUID 0x05

uCloseSession        :: UID
uCloseSession                = uSMMethodUID 0x06
