{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Capability.Refused
Description : A 'ReadOnly transport where a 'ReadWrite one is asked: what GHC refuses, its refusal deferred to run time
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Two definitions GHC does not accept, compiled with their type errors
deferred (@-fdefer-type-errors@, the warning it gives off, so that CI's
@-Werror@ does not make it an error again), so that "Hsed.CapabilitySpec"
can show that they are refused: evaluating either throws the type error.
A @'ReadOnly@ transport is no @'ReadWrite@ one, and 'Coerce.coerce' does not make
it one ('Hsed.Wire.Transport''s role is nominal). The transport is never
built: only the types are under test.
-}
module Hsed.Capability.Refused
  ( readOnlyAsReadWrite
  , coercedToReadWrite
  ) where

import qualified Data.Coerce as Coerce
import           RIO

import           Hsed.Wire   (Cap (..), Transport)


readOnly :: Transport 'ReadOnly
readOnly = error "never built: only the types are under test"

-- | A @'ReadOnly@ transport given where a @'ReadWrite@ one is asked.
readOnlyAsReadWrite :: Transport 'ReadWrite
readOnlyAsReadWrite = readOnly

-- | A @'ReadOnly@ transport coerced to a @'ReadWrite@ one.
coercedToReadWrite :: Transport 'ReadWrite
coercedToReadWrite = Coerce.coerce readOnly
