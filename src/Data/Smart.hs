{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : Data.Smart
Description : Smart constructors
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Smart constructors refined with predicates.

-}

module Data.Smart
  (
    Smart (..)
  )
where

import           Data.Either
import           Data.Maybe
import           GHC.Base    (const, (.))


-- | Smart constructor.
--
--   Wrap a @a@ in a @c@ under the control of a refinement predicate.

class Smart c a where

    -- | Forget type-level minimum and maximum size, obtaining the underlying value.
    unwrap :: c a -> a


    -- | Simply wrap a value in a @c@ as is, assuming @predicate@.
    --
    -- __WARNING__ Use it only when you know what you're doing.
    --
    unsafeCreate :: a -> c a


    -- | Refinement predicate error message type. Commonly would be @String@.
    --
    type ErrorMessage c a

    -- | Refinement predicate.  Either pass a wrapped value as Right or
    --   give an error message as Left.
    --
    --   Note that this is passed the value after being wrapped, and therefore
    --   assumes that the wrapping operation is mostly harmless.
    --
    predicate :: c a -> Either (ErrorMessage c a) (c a)

    -- | Primary smart constructor into Either.
    --
    safeCreate :: a -> Either (ErrorMessage c a) (c a)
    safeCreate = predicate . unsafeCreate

    -- | Simplified smart constructor into Maybe.
    --
    create :: a -> Maybe (c a)
    create = either (const Nothing) Just . safeCreate
