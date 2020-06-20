{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Data.Refined
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatype refined by predicate

-}
module Data.Refined
  ( IsPredicate(..), Refined
  , type (?)
  ) where

import           Data.Coerce  (Coercible, coerce)
import           Data.HasSize (HasSize (..))
import           Data.Kind    (Type)

import           Prelude      (Bool (..), Either (..), Eq (..), Maybe (..),
                               Num (..), Ord (..), Show (..), String, error,
                               ($), (.))
-- $setup
--
-- >>> import GHC.Base(Int)
-- >>> import GHC.Integer(Integer)


-- -- | Refined constructor.
-- --
-- --   Wrap a @a@ in a @c@ under the control of a refinement predicate.
-- class Refined p a where

--     -- | Forget type-level minimum and maximum size, obtaining the underlying value.
--     unwrap :: p a -> a

--     -- | Simply wrap a value in a @c@ as is, assuming @predicate@.
--     --
--     -- __WARNING__ Use it only when you know what you're doing.
--     --
--     unsafeCreate :: a -> p a

--     -- | Refinement predicate error message type. Commonly would be @String@.
--     --

--     type ErrorMessage p a
--     -- | Refinement predicate.  Either pass a wrapped value as Right or
--     --   give an error message as Left.
--     --
--     --   Note that this is passed the value after being wrapped, and therefore
--     --   assumes that the wrapping operation is mostly harmless.
--     --
--     predicate :: p a -> Either (ErrorMessage p a) (p a)
--     -- | Primary smart constructor into Either.
--     --
--     safeCreate :: a -> Either (ErrorMessage p a) (p a)
--     safeCreate = predicate . unsafeCreate
--     -- | Simplified smart constructor into Maybe.
--     --
--     create :: a -> Maybe (p a)
--     create = either (const Nothing) Just . safeCreate


-- -- | Restarting from very grounded ground zero, and abstracting
-- --   back up to a minimally complex notion of 'Refined'
-- class IsPredicate a where
--   type ErrMessage a :: Type
--   type ErrMessage a = String -- default

--   validate :: a -> Either (ErrMessage a) a
--   validate x = -- default validation uses predicate and pred msg
--     if predicate' x
--       then Right x
--       else Left $ predicateFailureMessage x
--   predicate' :: a -> Bool
--   predicateFailureMessage :: a -> ErrMessage a

--   err :: ErrMessage a -> a
--   default err :: (ErrMessage a ~ String) => ErrMessage a -> a
--   err = error

--   enforce :: a -> a
--   enforce = either err id . validate


-- | Test with IsPredicate Integer
--
-- Note that doctest is perfectly OK with these being comments
-- inside the instance code, where they are correctly indented,
-- but dante-eval-block puts the results on the left margin, and
-- while doctest is again fine with that, I'm not.

-- >>> 1 ::NonNegative Int
-- 1
--
-- >>> -2 ::NonNegative Int
-- *** Exception: -2 is negative
-- CallStack (from HasCallStack):
--   error, called at /var/folders/6n/6q645lsj4nj29h559pmrwm1r0000gp/T/danteUBO4FW.hs:96:9 in main:Data.Refined
--
-- >>> 3 :: NonNegative Integer
-- 3
--
-- >>> -4 ::NonNegative Integer
-- *** Exception: -4 is negative
-- CallStack (from HasCallStack):
--   error, called at /var/folders/6n/6q645lsj4nj29h559pmrwm1r0000gp/T/danteUBO4FW.hs:96:9 in main:Data.Refined

-- newtype  NumericPredicate a = NP {n :: a}
--   deriving (Eq, Ord, Bounded, Read)
--   deriving Show via a
--   deriving NumericPredicate a via IsPedicate a

-- instance (Num a, Ord a, Show a, IsPredicate a)  => Num (NumericPredicate a) where
--   (NP x) + (NP y) = enforce $ NP (x + y)
--   (NP x) - (NP y) = enforce $ NP (x - y)
--   (NP x) * (NP y) = enforce $ NP (x * y)
--   negate (NP x) = enforce $ NP (negate x)
--   abs (NP x) = enforce $ NP (abs x)
--   signum (NP x) = enforce $ NP (signum x)
--   fromInteger = enforce . NP . fromInteger

-- newtype NonNegative a = NN {p :: NumericPredicate a}
--     deriving (Eq, Ord, Bounded, Read)
--     deriving Show via NumericPredicate a
--     deriving Num via NumericPredicate a

-- instance (Eq a, Num a, Ord a, Show a) => IsPredicate (NonNegative a) where
--     predicate' = (0 <=) . n . p
--     predicateFailureMessage = (`shows` " is negative") . n . p


-- data Predicate a -- some condition on type a
-- type role IsRefinedType phantom _
-- class IsRefinedType (p::Type->Type) a where-- class of types which are conditions on type a
--     predicate :: a -> Bool
--     predicateFailure :: a -> String
--     validate :: a -> Either String a
--     validate x = if predicate x then Right x else Left $ predicateFailure x
--     require :: a -> a
--     require = either error id . validate

-- type role Refined phantom _
-- data Refined (p::Type->Type) a = Refined a -- data of type a which pass conditions p -- p can be ghost
-- class (IsRefinedType p a) => IsRefined p a where -- class of types of data of type a which pass conditions p -- p can be ghost

--     safeCreate :: a -> Either String (Refined p a)
--     safeCreate = either Left (Right . Refined) . validate

--     create :: a -> Maybe (Refined p a)
--     create = either (const Nothing) Just . safeCreate

--     unsafeCreate :: a -> (Refined p a)
--     unsafeCreate = Refined

--     unwrap :: (Refined p a) -> a
--     unwrap (Refined x) = x



-- class (Num a, Ord a, Show a) => Numeric a
-- instance (Num a, Ord a, Show a) => Numeric a

-- newtype NonNegative a = NonNegative a
-- instance (Numeric a) => IsRefinedType NonNegative a where
--     predicate = ( 0 <= )
--     predicateFailure = (`shows` " is negative")

-- instance Num(NonNegative a)

type role Refined phantom nominal
newtype Refined (p :: Type -> Type) a = Refined a
    deriving (Eq,Ord,Show) via a
-- instance The (Refined p a) a

-- | An infix alias for 'Refined'.
type a ?p = Refined p a
infixr 1 ?


class (Coercible (p a) a) => IsPredicate p a where
    predicate :: p a -> Bool
    failMsg :: p a -> String
    examine :: Refined p a -> a
    examine = coerce
    require :: a -> Refined p a
    require = require' . consider
      where
        require' :: IsPredicate p a => p a -> Refined p a
        require' x  = if predicate x then accept x else error $ failMsg x
    safeCreate :: a -> Either String (Refined p a)
    safeCreate = safeCreate' . consider
      where
        safeCreate' :: IsPredicate p a => p a -> Either String (Refined p a)
        safeCreate' x  = if predicate x then Right $ accept x else Left $ failMsg x
    create :: a ->  Maybe (Refined p a)
    create = create' . consider
      where
        create' :: IsPredicate p a => p a -> Maybe (Refined p a)
        create' x  = if predicate x then Just $ accept x else Nothing





class IsPredicate p a => IsPredicate_Internal p a where
    consider :: a -> p a
    consider = coerce
    accept :: p a -> Refined p a
    accept = coerce
instance IsPredicate p a => IsPredicate_Internal p a where







instance (IsPredicate p a, Num a) => Num (Refined p a)
  where
    (Refined x) + (Refined y) = require (x + y)
    (Refined x) - (Refined y) = require (x - y)
    (Refined x) * (Refined y) = require (x * y)
    negate (Refined x)        = require (negate x)
    abs (Refined x)           = require (abs x)
    signum (Refined x)        = require (signum x)
    fromInteger x             = require (fromInteger x :: Num a => a)



class HasSize a => FixedSize n a
instance HasSize a => FixedSize n a
