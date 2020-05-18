\documentstyle{article}
\begin{document}
\chapter{Bytes}

Define the fixed-length ByteStrings used by the TPer


\begin{code}
{-|
Module      : Extras.Sized
Description : Fixed- and Bounded-Length ByteStrings and Nums
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for Tokens.

-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}




module Extras.Sized
  -- (
  --   Fixed_bytes(..)
  -- , HasFixed_bytes(..)
  -- , take
  -- , drop
  -- , append
  -- , fpack
  -- , funpack
  -- )
where


import qualified Data.ByteString       as B (ByteString, pack)
import qualified Data.ByteString.Short as S (ShortByteString, fromShort, length, toShort, unpack)
import qualified Data.Foldable         as F (length)
import           Data.Functor               ((<$>))
import           Data.Proxy                 (Proxy(..))
import qualified Data.SizedText        as T (Static, append, create, drop, take, unwrap)
import           Data.String                (IsString(..))

import           GHC.Base                   (undefined, error, mconcat, (.), ($), ($!))
import           GHC.Classes                (Eq(..),Ord(..))
import           GHC.Maybe                  (Maybe(..))
import           GHC.Natural                (Natural)
import           GHC.Show                   (Show(..), showString, shows)
import           GHC.TypeLits               (type (+), type (<=), KnownNat,
                                             natVal)
import           GHC.Types                  (Nat)
import           GHC.Word                   (Word8)

import           Test.QuickCheck            (Arbitrary(..))
import           Test.QuickCheck.Instances.ByteString ()


import           Data.ByteString.Integral   (byteStringToNatural, naturalToByteString)
import           Extras.Hex                 (HasHex(..))



data Fixed_bytes (n :: Nat) = Fixed_bytes !(T.Static S.ShortByteString n)
    deriving (Eq, Ord)


take :: (KnownNat m, KnownNat n, n <= m) => Fixed_bytes m -> Fixed_bytes n
take (Fixed_bytes sbs) = (Fixed_bytes (T.take sbs))

drop :: (KnownNat m, KnownNat n, n <= m) => Fixed_bytes m -> Fixed_bytes n
drop (Fixed_bytes sbs) = (Fixed_bytes (T.drop sbs))

append :: (KnownNat m, KnownNat n) => Fixed_bytes m -> Fixed_bytes n -> Fixed_bytes (m + n)
append (Fixed_bytes sbsl) (Fixed_bytes sbsr) = (Fixed_bytes (T.append sbsl sbsr))


instance (KnownNat n) => Arbitrary (Fixed_bytes n) where
    arbitrary = fpack <$> arbitrary

-- | Show instances
--
instance (KnownNat n) => Show (Fixed_bytes n) where
    showsPrec _ (Fixed_bytes sbs) =
        shows sbs
      . showString " :: Fixed_bytes "
      . shows (natVal (Proxy @n))

-- | IsString instances, allowing string literal denotations
--
instance (KnownNat n) => IsString (Fixed_bytes n) where
    fromString s = Fixed_bytes $! case T.create (fromString s) of
                      Just ssbs -> ssbs
                      Nothing -> error $
                          mconcat [ show (natVal (Proxy @n))
                                  , " is not the length of "
                                  , show s
                                  , " (which length is "
                                  , show (F.length s)
                                  , ")"
                                  ]



-- | HasFixed_bytes class and instances for conversion to/from Fixed_bytes n
--
class (KnownNat n) => HasFixed_bytes n a where
    toFixed_bytes   :: a -> Fixed_bytes n
    fromFixed_bytes :: Fixed_bytes n -> a

instance (KnownNat n) => HasFixed_bytes n Natural where
    toFixed_bytes nat =
        let fbs = (toFixed_bytes . naturalToByteString) nat
        in if nat == fromFixed_bytes fbs
           then fbs
           else error $ mconcat
                        [ "Can not represent "
                        , show nat
                        , " by a value of type Fixed_bytes "
                        , show (natVal (Proxy @n))
                        ]
    fromFixed_bytes = byteStringToNatural . fromFixed_bytes

instance (KnownNat n) => HasFixed_bytes n (T.Static S.ShortByteString n) where
    toFixed_bytes sbs = Fixed_bytes sbs
    fromFixed_bytes (Fixed_bytes sbs)  = sbs

instance (KnownNat n) => HasFixed_bytes n S.ShortByteString where
    toFixed_bytes sbs =
        case (T.create sbs) :: Maybe (T.Static S.ShortByteString n) of
          Just ssbs -> toFixed_bytes ssbs
          Nothing   -> error $ mconcat [ show (natVal (Proxy @n))
                                       , " is not the length of "
                                       , show sbs
                                       , " (which is of length "
                                       , show (S.length sbs)
                                       , ")"
                                       ]
    fromFixed_bytes fbs =
        T.unwrap (fromFixed_bytes fbs :: T.Static S.ShortByteString n)

instance (KnownNat n) => HasFixed_bytes n B.ByteString where
    toFixed_bytes = toFixed_bytes . S.toShort
    fromFixed_bytes = S.fromShort . fromFixed_bytes

instance (KnownNat n) => HasFixed_bytes n [Word8] where
    toFixed_bytes = toFixed_bytes . B.pack
    fromFixed_bytes = S.unpack . fromFixed_bytes


-- | HasHex instances for conversion to/from Fixed_bytes n
--

instance (KnownNat n) => HasHex (Fixed_bytes n) where
    hex (Fixed_bytes ssbs) = undefined ssbs -- FIXME  (hex :: T.Static S.ShortByteString n -> String) ssbs
    fromHex hs = fromString <$> fromHex hs

-- | T.Static S.ShortByteString n methods lifted to Fixed_bytes n

fpack :: (KnownNat n) => B.ByteString -> Fixed_bytes n
fpack = toFixed_bytes

funpack :: (KnownNat n) => Fixed_bytes n -> B.ByteString
funpack = fromFixed_bytes


\end{code}
\end{document}
