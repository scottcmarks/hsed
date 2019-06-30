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



module System.SED.Common.UID where

import           Data.Attoparsec.ByteString()
import           Data.ByteString(ByteString)
import           Data.Foldable(concatMap)
import           Data.Maybe(Maybe(..))
import           Data.Proxy(Proxy(..))
import           Data.String(String)
import           GHC.Base(($), (.), fail, mconcat, pure)
import           GHC.Classes(Eq(..),Ord(..))
import           GHC.Show(Show(..))
import           GHC.Types(Int)
import           GHC.TypeNats(KnownNat)
import           GHC.Word(Word8(..))

import           Test.QuickCheck (Arbitrary(..))

import           Extras.Bytes ()
import           Extras.Bytes (Fixed_bytes(..), HasFixed_bytes(..), fromFixed_bytes, toFixed_bytes, take, drop, append)
import           Extras.Hex (hex)
import           Extras.Integral (intVal)
import           System.SED.Common.StreamItem (StreamItem(..))
-- import           System.SED.Common.Integral
import           System.SED.Common.Token (Token(..),IsToken(..))
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

fpack :: (KnownNat n) => ByteString -> Fixed_bytes n
fpack = toFixed_bytes

instance (KnownNat n) => IsToken (Fixed_bytes n) where
    token fb  = Bytes $ fromFixed_bytes fb
    fromToken (Bytes bs) = Just $ toFixed_bytes bs
    fromToken _ = Nothing

instance (KnownNat n) => StreamItem (Fixed_bytes n) where
    parser = do
        tok <- parser
        case tok of
            Bytes bs -> pure $ toFixed_bytes bs
            _        -> fail $ mconcat [ "Wrong token type for Fixed_bytes "
                                       , show (intVal (Proxy :: Proxy n) :: Int)
                                       , ": "
                                       , show tok
                                       ]
    generate = generate . token

showFixed_bytesHex :: (KnownNat n) => Fixed_bytes n -> [String]
showFixed_bytesHex fb =  concatMap h (fromFixed_bytes fb::[Word8])
       where h :: Word8 -> [String]
             h w = [" 0x", hex w]

showFixed_bytes :: (KnownNat n) => String -> Fixed_bytes n -> String
showFixed_bytes c fb = mconcat $ c : showFixed_bytesHex fb

newtype HalfUID = HalfUID (Fixed_bytes 4)
    deriving(Eq,Ord)
    deriving(IsToken,StreamItem,Arbitrary) via (Fixed_bytes 4)
instance Show(HalfUID) where
    show (HalfUID fb) = showFixed_bytes "halfUID" fb

halfUID :: Word8 -> Word8 -> Word8 -> Word8 -> HalfUID
halfUID b3 b2 b1 b0 = HalfUID $ toFixed_bytes [b3, b2, b1, b0]


newtype UID = UID (Fixed_bytes 8)
    deriving(Eq,Ord)
    deriving(IsToken,StreamItem,Arbitrary) via (Fixed_bytes 8)
instance Show(UID) where
    show (UID fb) = showFixed_bytes "uid" fb

uid ::
    Word8 -> Word8 -> Word8 -> Word8
 -> Word8 -> Word8 -> Word8 -> Word8
 -> UID
uid u3 u2 u1 u0 l3 l2 l1 l0 = UID $ toFixed_bytes [u3, u2, u1, u0, l3, l2, l1, l0]


uidUpper :: UID -> HalfUID
uidUpper (UID fb) = HalfUID (take fb)

uidLower :: UID -> HalfUID
uidLower (UID fb) = HalfUID (drop fb)

uidPlus :: HalfUID -> HalfUID -> UID
uidPlus (HalfUID fbl) (HalfUID fbr) = UID (append fbl fbr)

(+:+) :: HalfUID -> HalfUID -> UID
(+:+) = uidPlus
infix +:+

hNull :: HalfUID
hNull = halfUID 0x00 0x00 0x00 0x00

uNull :: UID
uNull = hNull +:+ hNull


\end{code}
\end{document}
