\documentstyle{article}
\begin{document}
\chapter{Bytes}

Define the fixed-length ByteStrings used by the TPer


\begin{code}
{-|
Module      : Extras.Bytes
Description : Fixed-Length ByteStrings
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for Tokens.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Extras.Bytes
  (
    Fixed_bytes(..)
  , S.Static
  , unwrap
  , create
  , take
  , drop
  , append
  , fixed
  , fpack
  , naturalToFixedBytes
  )
where

import           RIO                   (otherwise, (<=), Int, Natural, Eq, Ord, Show(..), error, mconcat,
                                        (<$>), ($), ($!), (.), (-))
import           Data.ByteString       hiding (take, drop, append)
import           Data.ByteString.Short hiding (length, pack)
import           Data.Maybe            (fromJust, Maybe(..))
import           Data.Proxy
import qualified Data.StaticText as S  (Static(..),create,take,drop,append)
import qualified Data.StaticText.Class as SC
import           Data.String           (IsString(..))
import           GHC.TypeNats
import           GHC.Word

import Extras.Hex
import Extras.Integral

data Fixed_bytes n = Fixed_bytes !(S.Static ShortByteString n)
    deriving (Eq, Ord, Show)

instance (KnownNat n) => IsString (Fixed_bytes n) where
    fromString !s = Fixed_bytes $! fromString s

instance (KnownNat n) => IsString (S.Static ShortByteString n) where
    fromString !s = case S.create (fromString s) of
                      Just ssbs -> ssbs
                      Nothing -> error $
                          mconcat [ show (natVal (Proxy :: Proxy n))
                                  , " is not the length of "
                                  , show s
                                  ]


instance HasHex ShortByteString  where
    hex = hex . fromShort
    fromHex hs = toShort <$> fromHex hs

instance (KnownNat n) => HasHex (S.Static ShortByteString n) where
    hex = hex . SC.unwrap
    fromHex hs =  fromString <$> fromHex hs

instance (KnownNat n) => HasHex (Fixed_bytes n) where
    hex (Fixed_bytes bs) = hex bs
    fromHex hs = fromString <$> fromHex hs

unwrap :: Fixed_bytes n -> ByteString
unwrap (Fixed_bytes sbs) = fromShort $ SC.unwrap sbs

create :: (KnownNat n) => ByteString -> Maybe (Fixed_bytes n)
create bs = Fixed_bytes <$> S.create (toShort bs)

take :: (KnownNat m, KnownNat n, n <= m) =>
        Fixed_bytes m -> Fixed_bytes n
take (Fixed_bytes sbs) = (Fixed_bytes (S.take sbs))

drop :: (KnownNat m, KnownNat n, n <= m) =>
        Fixed_bytes m -> Fixed_bytes n
drop (Fixed_bytes sbs) = (Fixed_bytes (S.drop sbs))

append :: Fixed_bytes m -> Fixed_bytes n -> Fixed_bytes (m + n)
append (Fixed_bytes sbsl) (Fixed_bytes sbsr) = (Fixed_bytes (S.append sbsl sbsr))


fixed :: (KnownNat n) => ByteString -> Fixed_bytes n
fixed = fromJust . create

fpack :: (KnownNat n) => [Word8] -> Fixed_bytes n
fpack = fixed . pack

naturalToFixedBytes :: (KnownNat n) => Natural -> Fixed_bytes n
naturalToFixedBytes nat = fixed $ mconcat [ initialZeros ,  natBytes]
  where
    initialZeros = replicate nZerosNeeded 0
    nZerosNeeded
        | natLen <= len = len - natLen
        | otherwise = error $ mconcat [ show nat
                                      , " is too large to represent in "
                                      , show len
                                      , " bytes"
                                          ]
    natLen   = length natBytes :: Int
    len = 2
    natBytes = naturalToByteString nat




\end{code}
\end{document}
