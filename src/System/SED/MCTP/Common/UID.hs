{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : System.SED.MCTP.Common.UID
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for UIDs and HalfUIDs.

-}




module System.SED.MCTP.Common.UID where

import           Data.ByteString                    (singleton)
import           Data.ByteString.Base16             (encode)
import           Data.ByteString.Char8              (unpack)
import           Data.Foldable                      (foldl)
import           Data.Functor                       ((<$>))

import           Data.String                        (String)
import           GHC.Base                           (($), (.))
import           GHC.Classes                        (Eq (..), Ord (..))
import           GHC.Exts                           (IsList (..))
import           GHC.Show                           (Show (..), showString)
import           GHC.TypeLits                       (KnownNat)
import           GHC.Word                           (Word8)

import           Test.QuickCheck                    (Arbitrary (..))

import           System.SED.MCTP.Common.Base_Type   (Core_bytes (..), append,
                                                     core_bytes_4, core_bytes_8,
                                                     drop, take)
import           System.SED.MCTP.Common.Simple_Type (Core_halfuid, Core_uid)

import           System.SED.MCTP.Common.StreamItem  (StreamItem (..))
import           System.SED.MCTP.Common.Token       (IsToken (..))


{-
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

-}

showCore_bytes :: (KnownNat n) => String -> Core_bytes n -> String
showCore_bytes tag = foldl rollUp tag . toList
  where rollUp s = showString s . showString " 0x" . unpack . encode . singleton


newtype HalfUID = HalfUID (Core_halfuid)
    deriving(Eq, Ord)
    deriving(IsToken, StreamItem, IsList) via (Core_halfuid)
instance Show HalfUID where
    show (HalfUID fb) = showCore_bytes "halfUID" fb
instance Arbitrary HalfUID where
    arbitrary = HalfUID <$> Core_bytes <$> arbitrary

halfUID :: Word8 -> Word8 -> Word8 -> Word8 -> HalfUID
halfUID b3 b2 b1 b0 = HalfUID $ core_bytes_4 b3 b2 b1 b0


newtype UID = UID (Core_uid)
    deriving(Eq, Ord)
    deriving(IsToken, StreamItem, IsList) via (Core_uid)
instance Show UID where
    show (UID fb) = showCore_bytes "uid" fb
instance Arbitrary UID where
    arbitrary = UID <$> Core_bytes <$> arbitrary

uid ::
    Word8 -> Word8 -> Word8 -> Word8
 -> Word8 -> Word8 -> Word8 -> Word8
 -> UID
uid u3 u2 u1 u0 l3 l2 l1 l0 =
    UID $ core_bytes_8 u3 u2 u1 u0 l3 l2 l1 l0


uidUpper :: UID -> HalfUID
uidUpper (UID (Core_bytes fb)) = HalfUID (Core_bytes (take fb))

uidLower :: UID -> HalfUID
uidLower (UID (Core_bytes fb)) = HalfUID (Core_bytes (drop fb))

uidPlus :: HalfUID -> HalfUID -> UID
uidPlus (HalfUID (Core_bytes fbl)) (HalfUID (Core_bytes fbr)) =
    UID (Core_bytes (append fbl fbr))

(+:+) :: HalfUID -> HalfUID -> UID
(+:+) = uidPlus
infix +:+

hNull :: HalfUID
hNull = halfUID 0x00 0x00 0x00 0x00

uNull :: UID
uNull = hNull +:+ hNull
