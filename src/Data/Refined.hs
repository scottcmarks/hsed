{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
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
  ( Predicate(..)
  , Refined
  , type (?)
  ) where

import           Data.Coerce (Coercible, coerce)
import           Data.Kind   (Type)
import           Data.String (IsString (..), String)
import           GHC.Exts    (IsList (..))
import           GHC.Read    (Read (..))
import           Prelude     (Bool (..), Either (..), Eq (..), Integer,
                              Maybe (..), Num (..), Ord (..), Show (..), error,
                              ($), (.), (<$>))

type role Refined phantom nominal
newtype Refined (p :: Type -> Type) a = Refined a
    deriving (Eq, Ord, Show) via a

-- | An infix alias for 'Refined'.
type a ?p = Refined p a
infixr 1 ?



class (Coercible (p a) a) => Predicate (p :: Type -> Type) a where
    predicate :: p a -> Bool

    failMsg :: p a -> String

    plain :: Refined p a -> a
    plain = coerce

    refine :: a -> Refined p a
    refine = refine' . consider
      where
        refine' x  = if predicate x then accept x else error $ failMsg x

    safeCreate :: a -> Either String (Refined p a)
    safeCreate = safeCreate' . consider
      where
        safeCreate' x  = if predicate x then Right $ accept x else Left $ failMsg x

    create :: a ->  Maybe (Refined p a)
    create = create' . consider
      where
        create' x  = if predicate x then Just $ accept x else Nothing

    unsafeCreate :: a -> Refined p a
    unsafeCreate = accept . consider





class Predicate p a => Predicate_Internal p a where
    consider :: a -> p a
    consider = coerce
    accept :: p a -> Refined p a
    accept = coerce
instance Predicate p a => Predicate_Internal p a where


instance (Num a, Predicate p a) => Num (Refined p a)
  where
    (Refined x) + (Refined y) = refine (x + y)
    (Refined x) - (Refined y) = refine (x - y)
    (Refined x) * (Refined y) = refine (x * y)
    negate (Refined x)        = refine (negate x)
    abs (Refined x)           = refine (abs x)
    signum (Refined x)        = refine (signum x)
    fromInteger               = refine . (fromInteger :: Integer -> a)


instance (Read a, Predicate p a) => Read (Refined p a) where
    readPrec = refine <$> readPrec


instance (IsString a, Predicate p a) => IsString (Refined p a) where
    fromString = refine . (fromString :: String -> a)


instance (IsList a, Predicate p a) => IsList (Refined p a) where
    type Item (Refined p a) = Item a
    fromList = refine . fromList
    toList = toList . plain
