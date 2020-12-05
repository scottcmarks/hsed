{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RoleAnnotations           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-|
Module      : System.SED.MCTP.Common.UID
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for UIDs and HalfUIDs.

-}




module System.SED.MCTP.Common.UID where

import           Data.BoundedSize                   (fromNat)
import           Data.ByteString                    (singleton)
import           Data.ByteString.Base16             (encode)
import           Data.ByteString.Char8              (unpack)
import           Data.Foldable                      (foldl)
import           Data.Functor                       ((<$>))
import           Data.HasSize                       (HasSize (..))
import           Data.Proxy                         (Proxy (..))
import           Data.String                        (String)
import           GHC.Base                           (Maybe (..), pure, ($),
                                                     (&&), (.), (>>))
import           GHC.Classes                        (Eq (..), Ord (..))
import           GHC.Enum                           (Bounded (..))
import           GHC.Exts                           (IsList (..))
import           GHC.Num                            (Num (..))
import           GHC.Read                           (Read (..))
import           GHC.Real                           (fromIntegral)
import           GHC.Show                           (Show (..), showString)
import           GHC.TypeLits                       (KnownNat)
import           GHC.Word                           (Word8)

import           Text.ParserCombinators.ReadP       (ReadP, count, pfail,
                                                     string)
import           Text.ParserCombinators.ReadPrec    (lift)
import           Text.Read.Lex

import           Test.QuickCheck                    (Arbitrary (..))

import           System.SED.MCTP.Common.Base_Type   (Core_bytes (..), append,
                                                     core_bytes_4, core_bytes_8,
                                                     drop, take)
import           System.SED.MCTP.Common.Simple_Type (Core_halfuid, Core_uid)

import           System.SED.MCTP.Common.StreamItem  (StreamItem (..))
import           System.SED.MCTP.Common.Token       (IsToken (..))



showCore_bytes :: (KnownNat n) => String -> Core_bytes n -> String
showCore_bytes tag = foldl rollUp tag . toList
  where rollUp s = showString s . showString " 0x" . unpack . encode . singleton

readPCore_bytes :: forall n. (KnownNat n) => String -> ReadP (Core_bytes n)
readPCore_bytes tag = string tag >> core_bytesP


word8P :: ReadP Word8
word8P = do
    Number n <- lex
    case numberToInteger n of
      Just i -> if isWord8 i
                   then pure (fromInteger i)
                   else pfail
      _      -> pfail
  where
      isWord8 i = (fromIntegral (minBound::Word8)) <= i && i <= (fromIntegral (maxBound::Word8))


core_bytesP :: forall n. (KnownNat n) => ReadP (Core_bytes n)
core_bytesP = do
    bytes <- count (fromNat (Proxy @n)) word8P
    pure (fromList bytes)


newtype HalfUID = HalfUID (Core_halfuid)
    deriving(Eq, Ord, IsToken, StreamItem, IsList, HasSize, Arbitrary) via (Core_halfuid)
instance Show HalfUID where
    show (HalfUID fb) = showCore_bytes "halfUID" fb
instance Read HalfUID where
    readPrec  = lift (HalfUID <$> (readPCore_bytes "halfUID"))

halfUID ::
    Word8 -> Word8 -> Word8 -> Word8
 -> HalfUID
halfUID b3 b2 b1 b0 =
    HalfUID $ core_bytes_4 b3 b2 b1 b0




newtype UID = UID (Core_uid)
    deriving(Eq, Ord, IsToken, StreamItem, IsList, HasSize, Arbitrary) via (Core_uid)
instance Show UID where
    show (UID fb) = showCore_bytes "uid" fb
instance Read UID where
    readPrec  = lift (UID <$> (readPCore_bytes "uid"))

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
    UID (Core_bytes (fbl `append` fbr))

(+:+) :: HalfUID -> HalfUID -> UID
(+:+) = uidPlus
infix +:+

hNull :: HalfUID
hNull = halfUID 0x00 0x00 0x00 0x00

uNull :: UID
uNull = hNull +:+ hNull
