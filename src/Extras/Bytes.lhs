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

{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans       #-}

module Extras.Bytes
  (
    Fixed_bytes(..)
  , HasFixed_bytes(..)
  , S.Static
  , unwrap
  , create
  , take
  , drop
  , append
  , fixed
  , fpack
  , naturalToFixed_bytes
  )
where

import           Data.ByteString       (ByteString, pack)
import           Data.ByteString.Short (ShortByteString, fromShort, toShort)
import           Data.Foldable         ()
import           Data.Functor          ((<$>))
import           Data.Maybe            (fromJust)
import           Data.Proxy            (Proxy(..))
import qualified Data.StaticText as S  (createRight, drop, take, Static,append,create)
import qualified Data.StaticText.Class as SC (unwrap)
import           Data.String           (IsString(..))
import           GHC.Base              (undefined, error, mconcat, (.), ($), ($!))
import           GHC.Classes           (Eq(..),Ord(..))
import           GHC.Maybe             (Maybe(..))
import           GHC.Natural           (Natural(..))
import           GHC.Show              (Show(..))
import           GHC.TypeNats          (type (+), type (<=), KnownNat,
                                        natVal)
import           GHC.Word              (Word8)

import Extras.Hex                      (HasHex(..))
import Extras.Integral

data Fixed_bytes n = Fixed_bytes !(S.Static ShortByteString n)
    deriving (Eq, Ord, Show)

instance (KnownNat n) => IsString (Fixed_bytes n) where
    fromString s = Fixed_bytes $! fromString s

instance (KnownNat n) => IsString (S.Static ShortByteString n) where
    fromString s = case S.create (fromString s) of
                      Just ssbs -> ssbs
                      Nothing -> error $
                          mconcat [ show (natVal (Proxy :: Proxy n))
                                  , " is not the length of "
                                  , show s
                                  ]

class (KnownNat n) => HasFixed_bytes n a where
    toFixed_bytes   :: a -> Fixed_bytes n
    fromFixed_bytes :: Fixed_bytes n -> a

instance (KnownNat n) => HasFixed_bytes n Natural where
    toFixed_bytes nat =
        let fbs = (createRight . naturalToByteString) nat
            nat' = fixed_bytesToNatural fbs
            n' = natVal (Proxy :: Proxy n)
        in if nat == nat'
           then fbs
           else error $ mconcat
                        [ "Can not represent "
                        , show nat
                        , " in a value of type Fixed_bytes "
                        , show n'
                        ]

    fromFixed_bytes = byteStringToNatural . unwrap

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

createRight :: (KnownNat n) => ByteString -> Fixed_bytes n
createRight = Fixed_bytes <$> S.createRight 0 . toShort

take :: (KnownNat m, KnownNat n, n <= m) => Fixed_bytes m -> Fixed_bytes n
take (Fixed_bytes sbs) = (Fixed_bytes (S.take sbs))

drop :: (KnownNat m, KnownNat n, n <= m) => Fixed_bytes m -> Fixed_bytes n
drop (Fixed_bytes sbs) = (Fixed_bytes (S.drop sbs))

append :: Fixed_bytes m -> Fixed_bytes n -> Fixed_bytes (m + n)
append (Fixed_bytes sbsl) (Fixed_bytes sbsr) = (Fixed_bytes (S.append sbsl sbsr))


fixed :: (KnownNat n) => ByteString -> Fixed_bytes n
fixed = fromJust . create

fpack :: (KnownNat n) => [Word8] -> Fixed_bytes n
fpack = fixed . pack

fixed_bytesToNatural :: (KnownNat n) => Fixed_bytes n -> Natural
fixed_bytesToNatural = fromFixed_bytes

naturalToFixed_bytes :: (KnownNat n) => Natural -> Fixed_bytes n
naturalToFixed_bytes = toFixed_bytes

\end{code}
\end{document}
