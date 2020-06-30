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
Module      : System.SED.MCTP.Common.Base_Type.Class
Description : Core data type formats
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Formats.

-}

module System.SED.MCTP.Common.Base_Type.Class
  ( Core_some_integer  (..)
  , Core_some_uinteger (..)
  , Core_some_bytes    (..)
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
-- TODO: Work around needing this, please
  , unsafeCreate
  )
where

import           Control.Monad                     (fail)

import           Data.BoundedSize                  (AtLeast, FixedSize,
                                                    HasSize (..), MaxSize,
                                                    append, bounds, drop,
                                                    fromNat, length, map,
                                                    padLeft, padRight,
                                                    replicate, size, take)
import           Data.ByteString                   (ByteString)
import           Data.Either                       (Either (..))
import           Data.Functor                      ((<$>))
import qualified Data.ListLike                     as LL (FoldableLL (..),
                                                          ListLike (..))
import           Data.Maybe                        (Maybe (..))
import           Data.Proxy                        (Proxy (..))
import           Data.Refined
import           Data.String                       (IsString (..), String)
import           GHC.Base                          (Int, Monoid (..),
                                                    Semigroup (..), pure, ($),
                                                    (.))
import           GHC.Classes                       (Eq (..), Ord (..))
import           GHC.Exts                          (IsList (..))
import           GHC.Num                           (Integer, Num (..))
import           GHC.Show                          (Show (..), showString,
                                                    shows)
import           GHC.TypeLits                      (KnownNat)
import           GHC.Word                          (Word8)
import           Numeric.Natural                   (Natural)

import           System.SED.MCTP.Common.StreamItem
import           System.SED.MCTP.Common.Token      (IsToken (..), Token (..))

import           Test.QuickCheck                   (Arbitrary (..))

newtype Core_some_integer = Core_some_integer {unCore_some_integer :: Integer}
    deriving (Eq, Ord, Show, Num, HasSize, Arbitrary) via Integer

newtype Core_integer   n = Core_integer   (Core_some_integer ? MaxSize n)
    deriving (Eq, Ord, Show) via (Core_some_integer ? MaxSize n)  -- Num, Read,
type Core_integer_at_least n = AtLeast Core_integer n


newtype Core_some_uinteger = Core_some_uinteger {unCore_some_uinteger :: Natural}
    deriving (Eq, Ord, Show, Num, HasSize, Arbitrary) via Natural

newtype Core_uinteger  n = Core_uinteger  (Core_some_uinteger ? MaxSize n)
    deriving (Eq, Ord, Show) via (Core_some_uinteger ? MaxSize n) -- Num, Read,
type Core_uinteger_at_least n = AtLeast Core_uinteger n


newtype Core_some_bytes = Core_some_bytes {unCore_some_bytes :: ByteString}
    deriving (Eq, Ord, Show, IsList, IsString, Semigroup, Monoid, Arbitrary)  via ByteString

instance LL.FoldableLL Core_some_bytes Word8 where
    foldl f acc   =                   LL.foldl f acc   . unCore_some_bytes
    foldr f acc   =                   LL.foldr f acc   . unCore_some_bytes
instance LL.ListLike Core_some_bytes Word8 where
    singleton     = Core_some_bytes . LL.singleton
    head          =                   LL.head          . unCore_some_bytes
    tail          = Core_some_bytes . LL.tail          . unCore_some_bytes
    genericLength =                   LL.genericLength . unCore_some_bytes


newtype Core_max_bytes n = Core_max_bytes (Core_some_bytes ? MaxSize n)
    deriving (Eq, Ord, Show, IsList, IsString)  via (Core_some_bytes ? MaxSize n)
type Core_max_bytes_at_least n = AtLeast Core_max_bytes n


newtype Core_bytes     n = Core_bytes     (Core_some_bytes ? FixedSize n)
    deriving (Eq, Ord, Show, IsList, IsString) via (Core_some_bytes ? FixedSize n)




instance (KnownNat n) => StreamItem (Core_uinteger n ) where
    parser = do
        tok <- parser
        case tok of
            Unsigned u -> case (safeCreate (Core_some_uinteger u) :: Either String (Core_some_uinteger ? MaxSize n)) of
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
            Signed i -> case (safeCreate (Core_some_integer i) :: Either String (Core_some_integer ? MaxSize n)) of
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
            Bytes bs -> case (safeCreate (Core_some_bytes bs) :: Either String (Core_some_bytes ? FixedSize n)) of
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
            Bytes bs -> case (safeCreate (Core_some_bytes bs) :: Either String (Core_some_bytes ? MaxSize n)) of
                          Right cb    -> pure $ Core_max_bytes cb
                          Left errMsg -> fail errMsg
            _        -> fail $ showString "Wrong token type for Core_max_bytes " .
                               shows (fromNat (Proxy @n) ::Int) .
                               showString ": " .
                               shows tok $ ""
    generate = generate . token



instance (KnownNat n) => IsToken (Core_integer n) where
    token (Core_integer i)  = Signed $ unCore_some_integer $ plain i
    fromToken (Signed i) = Core_integer <$> create (Core_some_integer i)
    fromToken _          = Nothing


instance (KnownNat n) => IsToken (Core_uinteger n) where
    token (Core_uinteger u)  = Unsigned $ unCore_some_uinteger $ plain u
    fromToken (Unsigned u) = Core_uinteger <$> create (Core_some_uinteger u)
    fromToken _            = Nothing


instance (KnownNat n) => IsToken (Core_bytes n) where
    token (Core_bytes b)  = Bytes $ unCore_some_bytes $ plain b
    fromToken (Bytes b) = Core_bytes <$> create (Core_some_bytes b)
    fromToken _         = Nothing


instance (KnownNat n) => IsToken (Core_max_bytes n) where
    token (Core_max_bytes b)  = Bytes $ unCore_some_bytes $ plain b
    fromToken (Bytes b) = Core_max_bytes <$> create (Core_some_bytes b)
    fromToken _         = Nothing
