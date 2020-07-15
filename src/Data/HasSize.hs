{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

#include "MachDeps.h"

{-|
Module      : Data.HasSize
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Things that have a @size@

-}

module Data.HasSize
  ( HasSize (..)
  )
where

import           Data.ListLike                (ListLike (..))
import           Data.Set                     (Set)
import qualified Data.Set                     as Set (size)
import           GHC.Base                     (Ord (..), quotInt)
import           GHC.Int                      (Int, Int16, Int32, Int64, Int8)
import           GHC.Integer                  (Integer, complementInteger)
import           GHC.Natural                  (Natural, naturalToInteger)
import           GHC.Num                      ((+))
import           GHC.Word                     (Word, Word16, Word32, Word64,
                                               Word8)
import           Math.NumberTheory.Logarithms (integerLog2)


-- | @size@ is an abstract 'Int'-valued property of a type that
--   can be used to refine values of that type.
--
--  The size of an 'Integer' is roughly the number of bytes required to
--   represent
--
--  The size of a 'ListLike' is just the number of 'Item's
--



class HasSize a where size :: a -> Int

-- * Instances
--

instance HasSize Int8              where size _ =  1
instance HasSize Int16             where size _ =  2
instance HasSize Int32             where size _ =  4
instance HasSize Int64             where size _ =  8
#if WORD_SIZE_IN_BITS == 64
instance HasSize Int               where size _ =  8
#else
#error Only handle word size 64 -- sorry
#endif
instance {-# OVERLAPPING #-} HasSize Integer
  where size 0 = 1
        size n | n < 0 =  size (complementInteger n)
        size n = 1 + (1 + integerLog2 n) `quotInt` 8   -- internal (1 +) for the sign bit

instance HasSize Word8             where size _ =  1
instance HasSize Word16            where size _ =  2
instance HasSize Word32            where size _ =  4
instance HasSize Word64            where size _ =  8
#if WORD_SIZE_IN_BITS == 64
instance HasSize Word              where size _ =  8
#else
#error Only handle word size 64 -- sorry
#endif
instance {-# OVERLAPPING #-} HasSize Natural
  where size 0 = 1
        size n = 1 + integerLog2 (naturalToInteger n) `quotInt` 8

instance {-# OVERLAPPABLE #-} ListLike full e => HasSize full where size = length

instance HasSize (Set a)           where size = Set.size
