{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


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
  ( Core_integer       (..)
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
-- TODO: Work around needing these, please
  , plain
  , unsafeCreate
  )
where

import           Control.Monad                     (fail)

import           Data.ByteString                   (ByteString)
import           Data.Either                       (Either (..))
import           Data.Functor                      ((<$>))
import           Data.Maybe                        (Maybe (..))
import           Data.Proxy                        (Proxy (..))
import           Data.String                       (IsString (..), String)
import           GHC.Base                          (Int, mconcat, pure, ($),
                                                    (.))
import           GHC.Classes                       (Eq (..), Ord (..))
import           GHC.Exts                          (IsList (..))
import           GHC.Num                           (Integer)
import           GHC.Show                          (Show (..))
import           GHC.TypeLits                      (KnownNat)
import           Numeric.Natural                   (Natural)

import           Data.BoundedSize                  (type (?), AtLeast,
                                                    FixedSize, MaxSize,
                                                    Predicate (..), append,
                                                    bounds, drop, fromNat,
                                                    length, map, padLeft,
                                                    padRight, replicate, size,
                                                    take)

import           System.SED.MCTP.Common.StreamItem
import           System.SED.MCTP.Common.Token      (IsToken (..), Token (..))


newtype Core_integer   n = Core_integer   (Integer ? MaxSize n)
        deriving (Eq, Ord, Show) via (Integer ? MaxSize n)  -- Num, Read,
type Core_integer_at_least n = AtLeast Core_integer n

newtype Core_uinteger  n = Core_uinteger  (Natural ? MaxSize n)
        deriving (Eq, Ord, Show) via (Natural ? MaxSize n) -- Num, Read,
type Core_uinteger_at_least n = AtLeast Core_uinteger n

newtype Core_max_bytes n = Core_max_bytes (ByteString ? MaxSize n)
        deriving (Eq, Ord, Show, IsList, IsString)  via (ByteString ? MaxSize n)
type Core_max_bytes_at_least n = AtLeast Core_max_bytes n

newtype Core_bytes     n = Core_bytes     (ByteString ? FixedSize n)
        deriving (Eq, Ord, Show, IsList, IsString) via (ByteString ? FixedSize n)




instance (KnownNat n) => StreamItem (Core_uinteger n ) where
    parser = do
        tok <- parser
        case tok of
            Unsigned u -> case (safeCreate u :: Either String (Natural ? MaxSize n)) of
                            Right cn    -> pure $ Core_uinteger cn
                            Left errMsg -> fail errMsg
            _          -> fail $ mconcat [ "Wrong token type for Core_uinteger "
                                         , show (fromNat (Proxy @n) ::Int)
                                         , ": "
                                         , show tok
                                         ]
    generate = generate . token

instance (KnownNat n) => StreamItem (Core_integer n  ) where
    parser = do
        tok <- parser
        case tok of
            Signed i -> case (safeCreate i :: Either String (Integer ? MaxSize n)) of
                          Right ci    -> pure $ Core_integer ci
                          Left errMsg -> fail errMsg
            _        -> fail $ mconcat [ "Wrong token type for Core_integer "
                                       , show (fromNat (Proxy @n) ::Int)
                                       , ": "
                                       , show tok
                                       ]
    generate = generate . token

instance (KnownNat n) => StreamItem (Core_bytes n) where
    parser = do
        tok <- parser
        case tok of
            Bytes bs -> case (safeCreate bs :: Either String (ByteString ? FixedSize n)) of
                          Right cb    -> pure $ Core_bytes cb
                          Left errMsg -> fail errMsg
            _        -> fail $ mconcat [ "Wrong token type for Core_bytes "
                                       , show (fromNat (Proxy @n) ::Int)
                                       , ": "
                                       , show tok
                                       ]
    generate = generate . token


instance (KnownNat n) => StreamItem (Core_max_bytes n) where
    parser = do
        tok <- parser
        case tok of
            Bytes bs -> case (safeCreate bs :: Either String (ByteString ? MaxSize n)) of
                          Right cb    -> pure $ Core_max_bytes cb
                          Left errMsg -> fail errMsg
            _        -> fail $ mconcat [ "Wrong token type for Core_max_bytes "
                                       , show (fromNat (Proxy @n) ::Int)
                                       , ": "
                                       , show tok
                                       ]
    generate = generate . token



instance (KnownNat n) => IsToken (Core_integer n) where
    token (Core_integer i)  = Signed $ plain i
    fromToken (Signed i) = Core_integer <$> create i
    fromToken _          = Nothing


instance (KnownNat n) => IsToken (Core_uinteger n) where
    token (Core_uinteger u)  = Unsigned $ plain u
    fromToken (Unsigned u) = Core_uinteger <$> create u
    fromToken _            = Nothing


instance (KnownNat n) => IsToken (Core_bytes n) where
    token (Core_bytes b)  = Bytes $ plain b
    fromToken (Bytes b) = Core_bytes <$> create b
    fromToken _         = Nothing


instance (KnownNat n) => IsToken (Core_max_bytes n) where
    token (Core_max_bytes b)  = Bytes $ plain b
    fromToken (Bytes b) = Core_max_bytes <$> create b
    fromToken _         = Nothing
