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

{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}

module Extras.Bytes
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


import           Data.ByteString            (ByteString, pack)
import           Data.ByteString.Short      (ShortByteString, fromShort, toShort, unpack)
import qualified Data.ByteString.Short as S (length)
import qualified Data.Foldable         as F (length)
import           Data.Functor               ((<$>))
import           Data.Proxy                 (Proxy(..))
import           Data.StaticText            (Static, create, unwrap)
import qualified Data.StaticText       as T (append, drop, take)
import           Data.String                (IsString(..))

import           GHC.Base                   (undefined, error, mconcat, (.), ($), ($!))
import           GHC.Classes                (Eq(..),Ord(..))
import           GHC.Maybe                  (Maybe(..))
import           GHC.Natural                (Natural(..))
import           GHC.Show                   (Show(..), showParen, showString)
import           GHC.TypeNats               (type (+), type (<=), KnownNat,
                                             natVal)
import           GHC.Types                  (Nat)
import           GHC.Word                   (Word8)

import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Instances.ByteString ()


import           Extras.Hex                 (HasHex(..))
import           Extras.Hex                 ()
import           Extras.Integral            (byteStringToNatural, naturalToByteString)



data Fixed_bytes (n :: Nat) = Fixed_bytes !(Static ShortByteString n)
    deriving (Eq, Ord)

instance (KnownNat n) => Arbitrary (Fixed_bytes n) where
    arbitrary = fpack <$> arbitrary

-- | Show instances
--
instance (KnownNat n) => Show (Fixed_bytes n) where
    showsPrec d (Fixed_bytes sbs) = showParen (app_prec < d) $
            showString "fpack "
          . showList (unpack $ unwrap sbs)
          . showString " :: Fixed_bytes "
          . showString (show (natVal (Proxy :: Proxy n)))
         where app_prec = 10

-- | ISString instances, allowing string literal denotations
--
instance (KnownNat n) => IsString (Fixed_bytes n) where
    fromString s = Fixed_bytes $! case create (fromString s) of
                      Just ssbs -> ssbs
                      Nothing -> error $
                          mconcat [ show (natVal (Proxy :: Proxy n))
                                  , " is not the length of "
                                  , show s
                                  , " (which is of length "
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
                        , show (natVal (Proxy :: Proxy n))
                        ]
    fromFixed_bytes = byteStringToNatural . fromFixed_bytes

instance (KnownNat n) => HasFixed_bytes n (Static ShortByteString n) where
    toFixed_bytes sbs = Fixed_bytes sbs
    fromFixed_bytes (Fixed_bytes sbs)  = sbs

instance (KnownNat n) => HasFixed_bytes n ShortByteString where
    toFixed_bytes sbs =
        case (create sbs) :: Maybe (Static ShortByteString n) of
          Just ssbs -> toFixed_bytes ssbs
          Nothing   -> error $ mconcat [ show (natVal (Proxy :: Proxy n))
                                       , " is not the length of "
                                       , show sbs
                                       , " (which is of length "
                                       , show (S.length sbs)
                                       , ")"
                                       ]
    fromFixed_bytes fbs =
        unwrap (fromFixed_bytes fbs :: Static ShortByteString n)

instance (KnownNat n) => HasFixed_bytes n ByteString where
    toFixed_bytes = toFixed_bytes . toShort
    fromFixed_bytes = fromShort . fromFixed_bytes

instance (KnownNat n) => HasFixed_bytes n [Word8] where
    toFixed_bytes = toFixed_bytes . pack
    fromFixed_bytes = unpack . fromFixed_bytes


-- | HasHex instances for conversion to/from Fixed_bytes n
--

instance (KnownNat n) => HasHex (Fixed_bytes n) where
    hex (Fixed_bytes ssbs) = undefined ssbs -- FIXME  (hex :: Static ShortByteString n -> String) ssbs
    fromHex hs = fromString <$> fromHex hs

-- | Static ShortByteString n methods lifted to Fixed_bytes n


take :: (KnownNat m, KnownNat n, n <= m) => Fixed_bytes m -> Fixed_bytes n
take (Fixed_bytes sbs) = (Fixed_bytes (T.take sbs))

drop :: (KnownNat m, KnownNat n, n <= m) => Fixed_bytes m -> Fixed_bytes n
drop (Fixed_bytes sbs) = (Fixed_bytes (T.drop sbs))

append :: Fixed_bytes m -> Fixed_bytes n -> Fixed_bytes (m + n)
append (Fixed_bytes sbsl) (Fixed_bytes sbsr) = (Fixed_bytes (T.append sbsl sbsr))


fpack :: (KnownNat n) => ByteString -> Fixed_bytes n
fpack = toFixed_bytes

funpack :: (KnownNat n) => Fixed_bytes n -> ByteString
funpack = fromFixed_bytes

\end{code}
\end{document}
