{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
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
import           GHC.Base    (Bool (..), Eq (..), Ord (..), const, id, ($), (.))
import           GHC.Enum    (Bounded (..))
import           GHC.Err     (error)
import           GHC.Num     (Num (..))
import           GHC.Read    (Read (..))
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

  err :: ErrMessage a -> a
  default err :: (ErrMessage a ~ String) => ErrMessage a -> a
  err = error

  enforce :: a -> a
  enforce = either err id . validate


-- | Test with IsPredicate Integer
--
-- Note that doctest is perfectly OK with these being comments
-- inside the instance code, where they are correctly indented,
-- but dante-eval-block puts the results on the left margin, and
-- while doctest is again fine with that, I'm not.

-- >>> validate (1 ::NonNegative Int)
-- Right 1
--
-- >>> validate (-2 ::NonNegative Int)
-- *** Exception: -2 is negative
-- CallStack (from HasCallStack):
--   error, called at /var/folders/6n/6q645lsj4nj29h559pmrwm1r0000gp/T/dantegO6kcO.hs:96:9 in main:Data.Refined
--
-- >>> validate (3 :: NonNegative Integer)
-- Right 3
--
-- >>> validate (-4 ::NonNegative Integer)
-- *** Exception: -4 is negative
-- CallStack (from HasCallStack):
--   error, called at /var/folders/6n/6q645lsj4nj29h559pmrwm1r0000gp/T/dantegO6kcO.hs:96:9 in main:Data.Refined

newtype  NonNegative a = NonNegative {n :: a}
  deriving (Eq, Ord, Bounded, Read)
  deriving Show via a
instance (Num a, Ord a, Show a)  => Num (NonNegative a) where
  (NonNegative x) + (NonNegative y) = enforce $ NonNegative (x + y)
  (NonNegative x) - (NonNegative y) = enforce $ NonNegative (x - y)
  (NonNegative x) * (NonNegative y) = enforce $ NonNegative (x * y)
  negate (NonNegative x) = enforce $ NonNegative (negate x)
  abs (NonNegative x) = enforce $ NonNegative (abs x)
  signum (NonNegative x) = enforce $ NonNegative (signum x)
  fromInteger = enforce . NonNegative . fromInteger


instance (Eq a, Num a, Ord a, Show a) => IsPredicate (NonNegative a) where
    predicate' = (0 <=) . n
    predicateFailureMessage = (`shows` " is negative") . n
