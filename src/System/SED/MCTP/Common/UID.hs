{-|
Module      : System.SED.MCTP.Common.UID
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for UIDs and HalfUIDs.

-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}



module System.SED.MCTP.Common.UID where

import           Data.Attoparsec.ByteString        ()
import           Data.BoundedSize                  (append, drop, take)
import qualified Data.ByteString                   as B (pack, unpack)
import qualified Data.ByteString.Char8             as C (unpack)
import           Data.Foldable                     (concatMap)
import           Data.Functor                      ((<$>))
import           Data.Hex                          (hex)
import           Data.Smart                        (unsafeCreate, unwrap)
import           Data.String                       (String)
import           GHC.Base                          (mconcat, ($))
import           GHC.Classes                       (Eq (..), Ord (..))
import           GHC.Show                          (Show (..))
import           GHC.TypeLits                      (KnownNat)
import           GHC.Word                          (Word8 (..))

import           Test.QuickCheck                   (Arbitrary (..))

import           System.SED.MCTP.Common.Base_Type  (Core_bytes (..))
import           System.SED.MCTP.Common.Instances  ()
import           System.SED.MCTP.Common.StreamItem (StreamItem (..))
import           System.SED.MCTP.Common.Token      (IsToken (..))


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

showCore_bytesHex :: (KnownNat n) => Core_bytes n -> [String]
showCore_bytesHex (Core_bytes b) =  concatMap h $ B.unpack $ unwrap b
       where h :: Word8 -> [String]
             h w = [" 0x", C.unpack $ hex $ B.pack [w] ]

showCore_bytes :: (KnownNat n) => String -> Core_bytes n -> String
showCore_bytes c fb = mconcat $ c : showCore_bytesHex fb


newtype HalfUID = HalfUID (Core_bytes 4)
    deriving(Eq,Ord)
    deriving(IsToken,StreamItem) via (Core_bytes 4)
instance Show HalfUID where
    show (HalfUID fb) = showCore_bytes "halfUID" fb
instance Arbitrary HalfUID where
    arbitrary = HalfUID <$> Core_bytes <$> arbitrary

halfUID :: Word8 -> Word8 -> Word8 -> Word8 -> HalfUID
halfUID b3 b2 b1 b0 = HalfUID (Core_bytes (unsafeCreate $ B.pack [b3, b2, b1, b0]))


newtype UID = UID (Core_bytes 8)
    deriving(Eq,Ord)
    deriving(IsToken, StreamItem) via (Core_bytes 8)
instance Show UID where
    show (UID fb) = showCore_bytes "uid" fb
instance Arbitrary UID where
    arbitrary = UID <$> Core_bytes <$> arbitrary

uid ::
    Word8 -> Word8 -> Word8 -> Word8
 -> Word8 -> Word8 -> Word8 -> Word8
 -> UID
uid u3 u2 u1 u0 l3 l2 l1 l0 =
    UID (Core_bytes (unsafeCreate $ B.pack [u3, u2, u1, u0, l3, l2, l1, l0]))


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
