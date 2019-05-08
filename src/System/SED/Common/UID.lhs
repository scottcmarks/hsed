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

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module System.SED.Common.UID where

import           Data.Attoparsec.ByteString
import           Data.Maybe(fromJust)
import           GHC.TypeNats
import           RIO
import           RIO.ByteString               hiding (count,length,map)
import           Test.QuickCheck              hiding (generate)

import           Extras.Bytes
import qualified Extras.Bytes as Bytes
import           Extras.Hex
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


-- | Type-level to value-level for all Integrals, from the Natural
intVal :: (Num b, KnownNat n) => proxy n -> b
intVal p = fromIntegral $ (natVal p)


fixed :: (KnownNat n) => ByteString -> Fixed_bytes n
fixed = fromJust . create

fpack :: (KnownNat n) => [Word8] -> Fixed_bytes n
fpack = fixed . pack

instance (KnownNat n) => IsToken (Fixed_bytes n) where
    token  = Bytes . unwrap
    fromToken (Bytes bs) = Just $ fixed bs
    fromToken _ = Nothing

instance (KnownNat n) => StreamItem (Fixed_bytes n) where
    parser = do
        tok <- parser
        case tok of
            Bytes bs -> pure $ fixed bs
            _        -> fail $ mconcat [ "Wrong token type for Fixed_bytes "
                                       , show (intVal (Proxy :: Proxy n) :: Int)
                                       , ": "
                                       , show tok
                                       ]
    generate = generate . token

instance (KnownNat n) => Arbitrary (Fixed_bytes n) where
    arbitrary = fpack <$> count (intVal (Proxy :: Proxy n)) arbitrary



newtype HalfUID = HalfUID (Fixed_bytes 4)
    deriving(Eq,Ord)
    deriving(IsToken,StreamItem,Arbitrary) via (Fixed_bytes 4)
instance Show(HalfUID) where
    show (HalfUID fb) = mconcat $ "halfUID" : RIO.concat (map h (unpack $ unwrap fb))
       where h w = [" 0x", hex w]

halfUID :: Word8 -> Word8 -> Word8 -> Word8 -> HalfUID
halfUID b3 b2 b1 b0 = HalfUID $ fpack [b3, b2, b1, b0]


newtype UID = UID (Fixed_bytes 8)
    deriving(Eq,Ord)
    deriving(IsToken,StreamItem,Arbitrary) via (Fixed_bytes 8)
instance Show(UID) where
    show (UID fb) = mconcat $ "uid" : RIO.concat (map h (unpack $ unwrap fb))
       where h w = [" 0x", hex w]

uid ::
    Word8 -> Word8 -> Word8 -> Word8
 -> Word8 -> Word8 -> Word8 -> Word8
 -> UID
uid u3 u2 u1 u0 l3 l2 l1 l0 = UID $ fpack [u3, u2, u1, u0, l3, l2, l1, l0]


uidUpper :: UID -> HalfUID
uidUpper (UID fb) = (HalfUID (Bytes.take fb))

uidLower :: UID -> HalfUID
uidLower (UID fb) = (HalfUID (Bytes.drop fb))

uidPlus :: HalfUID -> HalfUID -> UID
uidPlus (HalfUID fbl) (HalfUID fbr) = (UID (Bytes.append fbl fbr))

(+:+) :: HalfUID -> HalfUID -> UID
(+:+) = uidPlus
infix +:+

hNull :: HalfUID
hNull = halfUID 0x00 0x00 0x00 0x00

uNull :: UID
uNull = hNull +:+ hNull


\end{code}
\end{document}
