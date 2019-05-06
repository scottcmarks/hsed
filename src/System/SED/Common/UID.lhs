\documentstyle{article}
\begin{document}
\chapter{UID}

Table, row, etc. identifiers.


\begin{code}
{-|
Module      : System.SED.Common.UID
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for UIDs and HalfUIDs.

-}

{-# LANGUAGE NoImplicitPrelude #-}

module System.SED.Common.UID where

import           Data.Attoparsec.ByteString
import           RIO
import           RIO.ByteString               hiding (length,map)
import           Test.QuickCheck              hiding (generate)

import           System.SED.Common.StreamItem
-- import           System.SED.Common.Integral
import           System.SED.Common.Token
-- import           System.SED.Common.Value

\end{code}
3.2.4.1 Method Syntax

A method invocation is made up of the following parts:

1. Method Header Ð The method header is made up of the InvokingID and the MethodID, and
identifies what method is being called and on what the method is operating.
1. InvokingID Ð This is the 8-byte UID of the table, object, or SP upon which the method is
being invoked.
a. For SP methods invoked within a session, the InvokingID SHALL be 0x00 0x00 0x00
0x00 0x00 0x00 0x00 0x01, which is used to signify "this SP".
b. For methods invoked at the Session Manager Layer, the InvokingID SHALL be 0x00
0x00 0x00 0x00 0x00 0x00 0x00 0xFF, known as the "SMUID".
c. For other methods, this is the 8-byte UID of the table or object upon which the method
is being invoked.



2. MethodID Ð This is the 8-byte UID of the method being invoked.
a. For methods invoked within a session, this SHALL be the UID column value of the
object that represents the methed as assigned in the MethodID table.
b. For Session Manager Layer methods, this SHALL be the UID as assigned in Table
241. There SHALL NOT be rows in the MethodID table that represent these methods.


\begin{code}

data HalfUID = HalfUID{_b3,_b2,_b1,_b0::Word8} -- not an independent Token
    deriving(Show,Eq)

data UID = UID{_hi,_lo::HalfUID} -- is isomorphic to Bytes(...8 bytes...)
    deriving(Show,Eq)

instance StreamItem HalfUID where
    parser = HalfUID <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8
    generate (HalfUID b3 b2 b1 b0) = pack [b3, b2, b1, b0]

instance StreamItem UID where
    parser = do
        tok <- parser
        case tok of
            Bytes bs -> uidbs $ unpack bs
            _        -> fail $ "Wrong token type for UID: " <> show tok
          where uidbs (u3:u2:u1:u0:l3:l2:l1:l0:[]) =
                    pure $ uid u3 u2 u1 u0 l3 l2 l1 l0
                uidbs w8s =
                    fail $ mconcat [ "Bytes for UID of wrong length = "
                                   , show (length w8s)
                                   , ": "
                                   , show w8s
                                   ]
    generate = generate . token


instance IsToken UID where
    token (UID hi lo) = Bytes $ generate hi <> generate lo
    fromToken = undefined


instance Arbitrary HalfUID where
    arbitrary = HalfUID <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UID where
    arbitrary = UID <$> arbitrary <*> arbitrary

halfUID :: Word8 -> Word8 -> Word8 -> Word8 -> HalfUID
halfUID = HalfUID

uid ::
    Word8 -> Word8 -> Word8 -> Word8
 -> Word8 -> Word8 -> Word8 -> Word8
 -> UID
uid u3 u2 u1 u0 l3 l2 l1 l0 =
    UID (HalfUID u3 u2 u1 u0) (HalfUID l3 l2 l1 l0)
\end{code}



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


\begin{code}

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
uSMMethodUID xx              = uid  0x00 0x00 0x00 0x00  0x00 0x00 0xFF   xx

-- | For SP methods invoked within a session
uC_PINCharSetUnrestrictedUID :: UID
uC_PINCharSetUnrestrictedUID = uid  0x00 0x00 0x00 0x00B  0x00 0x00 0x00 0x01
\end{code}

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

\begin{code}
uProperties          :: UID
uProperties                  = uSMMethodUID 01

uStartSession        :: UID
uStartSession                = uSMMethodUID 02

uSyncSession         :: UID
uSyncSession                 = uSMMethodUID 03

uStartTrustedSession :: UID
uStartTrustedSession         = uSMMethodUID 04

uSyncTrustedSession  :: UID
uSyncTrustedSession          = uSMMethodUID 05

uCloseSession        :: UID
uCloseSession                = uSMMethodUID 06
\end{code}
\end{document}
