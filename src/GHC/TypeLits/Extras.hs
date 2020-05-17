{-
Conversions between numbers used in tokens and
bytestrings used to represent them.
-}

{-|
Module      : GHC.TypeList.Extras
Description : Type-level to value-level
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Conversions from type-level Nat to value-level Num

-}


{-# LANGUAGE NoImplicitPrelude #-}

module GHC.TypeLits.Extras
  (
      fromNat
  )
where

import           GHC.Base     ((.))
import           GHC.Num      (Num)
import           GHC.Real     (fromIntegral)
import           GHC.TypeLits (KnownNat, natVal)

-- | Type-level Nat to value-level Num
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> import           Data.Proxy(Proxy(..))
-- >>> import           GHC.Types    (Int)
-- >>> fromNat (Proxy @5) :: Int
-- 5
fromNat :: (Num b, KnownNat n) => proxy n -> b
fromNat = fromIntegral . natVal
