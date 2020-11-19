{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE TypeOperators     #-}
-- {-# OPTIONS_GHC -ddump-splices #-}


module System.SED.MCTP.Common.GDPTest
where

import           GDP

import           Data.Ord
import qualified Data.Set as S
import           GHC.Base (error)


-- Introduce a predicate `MemberOf set`, indicating that the value
-- has been validated by membership in the collection named `set`.
newtype MemberOf set name = MemberOf Defn
type role MemberOf nominal nominal

-- Validate a value using the collection named `set`. The
-- resulting value will satisfy `MemberOf set`.
isMemb :: Ord a =>
          (S.Set a ~~ set)
       -> a
       -> (a ?MemberOf set)
isMemb (The set) x = assert (if S.member x set then x else error "x is not a member of set") -- TODO shows

-- @
-- src/System/SED/MCTP/Common/GDPTest.hs:28:1-92: warning: [-Wincomplete-patterns] …
--     Pattern match(es) are non-exhaustive
--     In an equation for ‘isMemb’: Patterns not matched: _ _
-- @
--
-- This warning does not appear if the view pattern @the -> set@ is used instead.
-- Odd, since that is what @The set@ expands to.

isMemb _ _ = assert (error "Shouldn't happen -- x is x")  -- TODO shows
