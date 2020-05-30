{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples     #-}


{-|
Module      : Data.HasSize
Description : Things that have a "size"
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Things that have a "size"

-}

module Data.HasSize
  ( HasSize (..)
  )
where

import           Data.ByteString       as B
import           Data.ByteString.Short as S
import           Data.Vector           as V
import           GHC.Base              (Ord (..), (.), (/=))
-- import           GHC.Exts               (quotRemInt#, (+#), (/=#))
import           GHC.Float             (Double, logBase)
import           GHC.Int               (Int (..), Int16, Int32, Int64, Int8)
import           GHC.Integer           (Integer)
-- import           GHC.Integer.Logarithms (integerLog2#)
import qualified GHC.List              as L
import           GHC.Num               ((+))
import           GHC.Real              (floor, fromIntegral, quotRem)



class HasSize a where size :: a -> Int

instance HasSize B.ByteString      where size   = B.length
instance HasSize S.ShortByteString where size   = S.length
instance HasSize [a]               where size   = L.length
instance HasSize (V.Vector a)      where size   = V.length
instance HasSize Integer
  where size 0 = 1
        size n | n < 0 = size (- n)
        -- size n = case quotRemInt# (integerLog2# n) 8# of
        --     (# q#, r# #) -> I# (q# +# (r# /=# 0#))
        size n = case quotRem (integerLog2 n) 8 of
          (q, r) -> q + if r /= 0 then 1 else 0
          where integerLog2 = (floor::Double->Int) . logBase 2.0 . fromIntegral

instance HasSize Int8              where size _ =  1
instance HasSize Int16             where size _ =  2
instance HasSize Int32             where size _ =  4
instance HasSize Int64             where size _ =  8
instance HasSize Int               where size _ =  8   -- TODO: CPP?  obv mostly true lately
