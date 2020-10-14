{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}


{-|
Module      : System.SED.MCTP.Common.Base_Type.Types
Description : Core data type formats
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Internal file with all types and instances.

-}

module System.SED.MCTP.Common.Base_Type.Types
  ( Implementation_integer  (..)
  , Implementation_uinteger (..)
  , Implementation_bytes    (..)
  , Core_integer       (..)
  , Core_uinteger      (..)
  , Core_max_bytes     (..)
  , Core_bytes         (..)
  , Core_uinteger_at_least
  , Core_integer_at_least
  , Core_max_bytes_at_least
  , size
  , safeCreate
  , toList
  , append
  , replicate
  , map
  , take
  , drop
  , bounds
  , length
  , padLeft
  , padRight
  , unsafeCreate
  )
where

import           Control.Monad                     (fail)

import           Data.BoundedSize                  (type (?), AtLeast,
                                                    FixedSize, HasSize (..),
                                                    MaxSize, Predicate (..),
                                                    append, bounds, drop,
                                                    fromNat, length, map,
                                                    padLeft, padRight, plain,
                                                    replicate, size, take,
                                                    unsafeCreate)
import           Data.ByteString                   (ByteString)
import           Data.Either                       (Either (..))
import           Data.Functor                      ((<$>))
import qualified Data.ListLike                     as LL (FoldableLL (..),
                                                          ListLike (..))
import           Data.Maybe                        (Maybe (..))
import           Data.Proxy                        (Proxy (..))
import           Data.String                       (IsString (..), String)
import           GHC.Base                          (Int, Monoid (..),
                                                    Semigroup (..), pure, ($),
                                                    (.))
import           GHC.Classes                       (Eq (..), Ord (..))
import           GHC.Exts                          (IsList (..))
import           GHC.Num                           (Integer, Num (..))
import           GHC.Read                          (Read (..))
import           GHC.Show                          (Show (..), showString,
                                                    shows)
import           GHC.TypeLits                      (KnownNat)
import           GHC.Word                          (Word8)
import           Numeric.Natural                   (Natural)

import           System.SED.MCTP.Common.StreamItem
import           System.SED.MCTP.Common.Token      (IsToken (..), Token (..))

import           Test.QuickCheck                   (Arbitrary (..))

newtype Implementation_integer = Implementation_integer {unImplementation_integer :: Integer}
    deriving (Eq, Ord, Num, Read, Show, HasSize, Arbitrary) via Integer

newtype Core_integer   n = Core_integer   (Implementation_integer ? MaxSize n)
    deriving (Eq, Ord, Num, Show) via (Implementation_integer ? MaxSize n)
type Core_integer_at_least n = AtLeast Core_integer n


newtype Implementation_uinteger = Implementation_uinteger {unImplementation_uinteger :: Natural}
    deriving (Eq, Ord, Num, Read, Show, HasSize, Arbitrary) via Natural

newtype Core_uinteger  n = Core_uinteger  (Implementation_uinteger ? MaxSize n)
    deriving (Eq, Ord, Num, Show) via (Implementation_uinteger ? MaxSize n) -- Read,
type Core_uinteger_at_least n = AtLeast Core_uinteger n


newtype Implementation_bytes = Implementation_bytes {unImplementation_bytes :: ByteString}
    deriving (Eq, Ord, Show, IsList, IsString, Semigroup, Monoid, Arbitrary) via ByteString

instance LL.FoldableLL Implementation_bytes Word8 where
    foldl f acc   =                        LL.foldl f acc   . unImplementation_bytes
    foldr f acc   =                        LL.foldr f acc   . unImplementation_bytes
instance LL.ListLike Implementation_bytes Word8 where
    singleton     = Implementation_bytes . LL.singleton
    head          =                        LL.head          . unImplementation_bytes
    tail          = Implementation_bytes . LL.tail          . unImplementation_bytes
    genericLength =                        LL.genericLength . unImplementation_bytes


newtype Core_max_bytes n = Core_max_bytes (Implementation_bytes ? MaxSize n)
    deriving (Eq, Ord, Show, IsList, IsString, Arbitrary)  via (Implementation_bytes ? MaxSize n)
type Core_max_bytes_at_least n = AtLeast Core_max_bytes n


newtype Core_bytes     n = Core_bytes     (Implementation_bytes ? FixedSize n)
    deriving (Eq, Ord, Show, IsList, IsString, Arbitrary) via (Implementation_bytes ? FixedSize n)
instance KnownNat n => HasSize (Core_bytes n) where
    size = fromNat




instance (KnownNat n) => StreamItem (Core_uinteger n ) where
    parser = do
        tok <- parser
        case tok of
            Unsigned u -> case (safeCreate (Implementation_uinteger u) :: Either String (Implementation_uinteger ? MaxSize n)) of
                            Right cn    -> pure $ Core_uinteger cn
                            Left errMsg -> fail errMsg
            _          -> fail $ showString "Wrong token type for Core_uinteger " .
                                 shows (fromNat (Proxy @n) ::Int) .
                                 showString ": " .
                                 shows tok $ ""
    generate = generate . token

instance (KnownNat n) => StreamItem (Core_integer n  ) where
    parser = do
        tok <- parser
        case tok of
            Signed i -> case (safeCreate (Implementation_integer i) :: Either String (Implementation_integer ? MaxSize n)) of
                          Right ci    -> pure $ Core_integer ci
                          Left errMsg -> fail errMsg
            _        -> fail $ showString "Wrong token type for Core_integer " .
                               shows (fromNat (Proxy @n) ::Int) .
                               showString ": " .
                               shows tok $ ""
    generate = generate . token

instance (KnownNat n) => StreamItem (Core_bytes n) where
    parser = do
        tok <- parser
        case tok of
            Bytes bs -> case (safeCreate (Implementation_bytes bs) :: Either String (Implementation_bytes ? FixedSize n)) of
                          Right cb    -> pure $ Core_bytes cb
                          Left errMsg -> fail errMsg
            _        -> fail $ showString "Wrong token type for Core_bytes " .
                               shows (fromNat (Proxy @n) ::Int) .
                               showString ": " .
                               shows tok $ ""
    generate = generate . token


instance (KnownNat n) => StreamItem (Core_max_bytes n) where
    parser = do
        tok <- parser
        case tok of
            Bytes bs -> case (safeCreate (Implementation_bytes bs) :: Either String (Implementation_bytes ? MaxSize n)) of
                          Right cb    -> pure $ Core_max_bytes cb
                          Left errMsg -> fail errMsg
            _        -> fail $ showString "Wrong token type for Core_max_bytes " .
                               shows (fromNat (Proxy @n) ::Int) .
                               showString ": " .
                               shows tok $ ""
    generate = generate . token



instance (KnownNat n) => IsToken (Core_integer n) where
    token (Core_integer i)  = Signed $ unImplementation_integer $ plain i
    fromToken (Signed i) = Core_integer <$> create (Implementation_integer i)
    fromToken _          = Nothing


instance (KnownNat n) => IsToken (Core_uinteger n) where
    token (Core_uinteger u)  = Unsigned $ unImplementation_uinteger $ plain u
    fromToken (Unsigned u) = Core_uinteger <$> create (Implementation_uinteger u)
    fromToken _            = Nothing


instance (KnownNat n) => IsToken (Core_bytes n) where
    token (Core_bytes b)  = Bytes $ unImplementation_bytes $ plain b
    fromToken (Bytes b) = Core_bytes <$> create (Implementation_bytes b)
    fromToken _         = Nothing


instance (KnownNat n) => IsToken (Core_max_bytes n) where
    token (Core_max_bytes b)  = Bytes $ unImplementation_bytes $ plain b
    fromToken (Bytes b) = Core_max_bytes <$> create (Implementation_bytes b)
    fromToken _         = Nothing
