{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE TypeOperators     #-}


module System.SED.MCTP.Common.GDPTest
where

import           GDP      (type (?), Defn, pattern The, type (~~), assert)

import           Data.Ord (Ord (..))
import           Data.Set (Set, member)
import           GHC.Base (error, undefined, ($), (.))
import           GHC.Show (Show (..), showString, shows)

-- Introduce a predicate `MemberOf set`, indicating that the value
-- has been validated by membership in the collection named `set`.
newtype MemberOf set name = MemberOf Defn
type role MemberOf nominal nominal

-- Validate a value using the collection named `set`. The
-- resulting value will satisfy `MemberOf set`.
--
-- Unicode info for the set-with-name-contains operator:
-- @
--   ⋺
--   CONTAINS WITH LONG HORIZONTAL STROKE
--   Unicode: U+22FA, UTF-8: E2 8B BA
-- @
--
(⋺) :: (Ord a, Show a) => (Set a ~~ set) -> a -> (a ?MemberOf set)
The set ⋺ x = if member x set
              then assert x
              else error ( shows x .
                           showString " is not a member of " $
                           show set )
_       ⋺ _ = undefined -- error ( showString "Shouldn't happen -- x is " $
                        --         show x )  -- silence -Wincomplete-patterns …
