{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeOperators         #-}


module Data.UserOfRefined  where

import           Data.Refined
import           Prelude      (Eq (..), Int, Integer, Num (..), Ord (..),
                               Show (..), shows, (<=))




newtype Nonnegative a = N a
    deriving (Eq,Num,Ord,Show) via a

instance (Num a, Show a, Ord a) => IsPredicate Nonnegative a where
    predicate = (0 <=)
    failMsg = (`shows` " is negative!")

i :: Refined Nonnegative Integer
i = 1

j :: Refined Nonnegative Integer
j = (-2)  -- lazy evaluation sets a bit of a land mine

k :: Int ? Nonnegative
k = 3

l :: Int ? Nonnegative
l = (-4)
