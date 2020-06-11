{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
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
Module      : Data.Refined
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatype refined by predicate

-}
module Data.Refined
  ( Refined(..)
  , validate
  ) where

import           Data.Either (Either (..), either)
import           Data.Kind   (Type)
import           Data.Maybe  (Maybe (..))
import           Data.String (String)
import           GHC.Base    (Bool (..), Ord (..), const, ($), (.))
import           GHC.Real    (Integral)
import           GHC.Show    (Show (..), shows)

-- $setup
--
-- >>> import GHC.Base(Int)
-- >>> import GHC.Integer(Integer)


-- | Refined constructor.
--
--   Wrap a @a@ in a @c@ under the control of a refinement predicate.
class Refined p a where

    -- | Forget type-level minimum and maximum size, obtaining the underlying value.
    unwrap :: p a -> a

    -- | Simply wrap a value in a @c@ as is, assuming @predicate@.
    --
    -- __WARNING__ Use it only when you know what you're doing.
    --
    unsafeCreate :: a -> p a

    -- | Refinement predicate error message type. Commonly would be @String@.
    --

    type ErrorMessage p a
    -- | Refinement predicate.  Either pass a wrapped value as Right or
    --   give an error message as Left.
    --
    --   Note that this is passed the value after being wrapped, and therefore
    --   assumes that the wrapping operation is mostly harmless.
    --
    predicate :: p a -> Either (ErrorMessage p a) (p a)
    -- | Primary smart constructor into Either.
    --
    safeCreate :: a -> Either (ErrorMessage p a) (p a)
    safeCreate = predicate . unsafeCreate
    -- | Simplified smart constructor into Maybe.
    --
    create :: a -> Maybe (p a)
    create = either (const Nothing) Just . safeCreate


-- | Restarting from very grounded ground zero, and abstracting
--   back up to a minimally complex notion of 'Refined'
class IsPredicate a where
  type ErrMessage a :: Type
  type ErrMessage a = String -- default

  validate :: a -> Either (ErrMessage a) a
  validate x = -- default validation uses predicate and pred msg
    if predicate' x
      then Right x
      else Left $ predicateFailureMessage x
  predicate' :: a -> Bool
  predicateFailureMessage :: a -> ErrMessage a


-- | Test with IsPredicate Integer
--
-- Note that doctest is perfectly OK with these being comments
-- inside the instance code, where they are correctly indented,
-- but dante-eval-block puts the results on the left margin, and
-- while doctest is again fine with that, I'm not.

-- >>> validate (1 :: Int)
-- Right 1
--
-- >>> validate (-1 ::Int)
-- Left "-1 is negative"
--
-- >>> validate (1 :: Integer)
-- Right 1
--
-- >>> validate (-1 ::Integer)
-- Left "-1 is negative"

instance (Integral a, Show a) => IsPredicate a where
    predicate' = (0 <=)
    predicateFailureMessage = (`shows` " is negative")
