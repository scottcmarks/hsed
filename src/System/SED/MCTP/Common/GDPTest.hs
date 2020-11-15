{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE TypeOperators     #-}
-- {-# OPTIONS_GHC -ddump-splices #-}


module System.SED.MCTP.Common.GDPTest
where

import           GDP

import qualified Data.List as L
import           Data.Ord
import qualified Data.Set  as S
import           GHC.Base  (Int, error, undefined)

-- Introduce a predicate `SortedBy comp`, indicating that
-- the value has been sorted by the comparator named `comp`.
newtype SortedBy comp name = SortedBy Defn
type role SortedBy nominal nominal

-- Sort a value using the comparator named `comp`. The
-- resulting value will satisfy `SortedBy comp`.
sortBy :: ((a -> a -> Ordering) ~~ comp)
       -> [a]
       -> ([a] ?SortedBy comp)
sortBy (The comp) xs = assert (L.sortBy comp xs)

-- Introduce a predicate `MemberOf set`, indicating that
-- the value has been sorted by the setarator named `set`.
newtype MemberOf set name = MemberOf Defn
type role MemberOf nominal nominal

-- Sort a value using the setarator named `set`. The
-- resulting value will satisfy `MemberOf set`.
isMemb :: Ord a =>
          (S.Set a ~~ set)
       -> a
       -> (a ?MemberOf set)
isMemb (The set) x = assert (if S.member x set then x else error "x is not a member of set")
