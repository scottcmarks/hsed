{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
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
  ( Core_some_integer  (..)
  , Core_some_uinteger (..)
  , Core_some_max_bytes (..)
  , Core_some_bytes    (..)
  , Core_integer       (..)
  , Core_uinteger      (..)
  , Core_max_bytes     (..)
  , Core_bytes         (..)
  , Core_uinteger_at_least
  , Core_integer_at_least
  , Core_max_bytes_at_least
  )
where

import           Control.Monad                     (fail)
import           Data.Attoparsec.ByteString        (Parser, parseOnly)

import           Data.BoundedSize                  (AtLeast, FixedSize,
                                                    HasSize (..), MaxSize)
import           Data.ByteString                   (ByteString)
import           Data.Either                       (Either (..))
import           Data.IsBytes                      (IsBytes (..))
import           Data.Maybe                        (Maybe (..))
import           Data.Proxy                        (Proxy (..))
import           Data.Smart                        (Smart (..))
import           Data.String                       (IsString (..))
import           GHC.Base                          (Int, mconcat, pure,
                                                    undefined, ($), (.))
import           GHC.Classes                       (Eq (..), Ord (..))
import           GHC.Num                           (Integer, Num)
import           GHC.Read                          (Read (..))
import           GHC.Show                          (Show (..))
import           GHC.TypeLits                      (KnownNat)
import           GHC.TypeLits.Extras               (fromNat)
import           Numeric.Natural                   (Natural)

import           System.SED.MCTP.Common.StreamItem
import           System.SED.MCTP.Common.Token      (IsToken (..), Token (..))

newtype Core_some_integer  = Core_some_integer  Integer
        deriving (Eq, Ord, HasSize, Num, Read, Show) via (Integer)
newtype Core_some_uinteger = Core_some_uinteger Natural
        deriving (Eq, Ord, HasSize, Num, Read, Show) via (Natural)
newtype Core_some_bytes    = Core_some_bytes    ByteString
        deriving (Eq, Ord, HasSize, IsBytes, IsString, Read, Show) via ByteString
newtype Core_some_max_bytes = Core_some_max_bytes ByteString
        deriving (Eq, Ord, HasSize, IsBytes, IsString, Read, Show) via ByteString


newtype Core_integer   n = Core_integer   (MaxSize   n Core_some_integer )
        deriving (Eq, Ord, HasSize, Num, Read, Show) via (MaxSize n Core_some_integer )
type Core_integer_at_least n = AtLeast Core_integer n

newtype Core_uinteger  n = Core_uinteger  (MaxSize   n Core_some_uinteger)
        deriving (Eq, Ord, HasSize, Num, Read, Show) via (MaxSize n Core_some_uinteger)
type Core_uinteger_at_least n = AtLeast Core_uinteger n

newtype Core_max_bytes n = Core_max_bytes (MaxSize   n Core_some_max_bytes)
        deriving (Eq, Ord, HasSize, IsString, Show) via (MaxSize   n Core_some_max_bytes)
type Core_max_bytes_at_least n = AtLeast Core_max_bytes n

newtype Core_bytes     n = Core_bytes     (FixedSize n Core_some_bytes   )
        deriving (Eq, Ord, HasSize, IsString, Show) via (FixedSize n Core_some_bytes)




instance StreamItem (Core_uinteger n ) where
    parser = undefined
    generate _ = "<uinteger_n>"
instance StreamItem (Core_integer n  ) where
    parser = undefined
    generate _ = "<integer_n>"
instance (KnownNat n) => StreamItem (Core_bytes n) where
    parser = do
        tok <- parser
        case tok of
            Bytes bs -> case safeCreate bs of
                          Right cb    -> pure cb
                          Left errMsg -> fail errMsg
            _        -> fail $ mconcat [ "Wrong token type for Core_bytes "
                                       , show (fromNat (Proxy @n) ::Int)
                                       , ": "
                                       , show tok
                                       ]
    generate = generate . token
instance StreamItem (Core_max_bytes n) where
    parser = undefined
    generate _ = "<max_bytes_n>"


instance (KnownNat n) => IsToken (Core_bytes n) where
    token cb  = Bytes $ _ cb
    fromToken (Bytes bs) = Just $ _ bs
    fromToken _          = Nothing
