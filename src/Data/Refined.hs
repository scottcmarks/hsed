{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE PolyKinds              #-}
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
  ( Refined
  , type (?)
  , IsRefined(..)
  , coerceToProxyTypeOf
  , Predicate(..)
  )
where

import           Data.Coerce     (Coercible, coerce)
import           Data.HasSize    (HasSize (..))
import           Data.Kind       (Type)
import           Data.ListLike   (FoldableLL (..), ListLike (..))
import           Data.Proxy      (Proxy (..), asProxyTypeOf)
import           Data.String     (IsString (..), String)
import           GHC.Base        (Monoid (..), Semigroup (..), const)
import           GHC.Exts        (IsList (..))
import           GHC.Read        (Read (..))
import           Prelude         (Bool (..), Either (..), Eq (..), Maybe (..),
                                  Num (..), Ord (..), Show (..), error, (.),
                                  (<$>))

import           Test.QuickCheck (Arbitrary (..), suchThatMap)


type role Refined phantom _
newtype Refined p a = Refined a
    deriving (Eq, Ord, Show) via a

-- | An infix alias for 'Refined'.
infixr 1 ?
type a ? p = Refined p a


class IsRefined p a where
    plain :: Refined p a -> a
    plain = coerce

    unsafeCreate :: a -> Refined p a
    unsafeCreate = coerce

    reifyRefinedPhantomAsProxy :: Refined p a -> Proxy p
    reifyRefinedPhantomAsProxy _ = Proxy

    reifyRefinedContextAsProxy :: Refined p a -> Proxy (p a)
    reifyRefinedContextAsProxy _ = Proxy

    accept :: Coercible (p a) a => p a -> Refined p a
    accept = coerce


instance IsRefined (Refined p) a


instance (HasSize a, IsRefined p a) => HasSize (a ? p) where
    size = size . plain


coerceToProxyTypeOf :: Coercible a b => a -> proxy b -> b
coerceToProxyTypeOf = asProxyTypeOf . coerce


class Coercible (p a) a => Predicate (p :: Type -> Type) a where
    predicate :: p a -> Bool

    failMsg :: p a -> String

    safeCreate :: IsRefined p a => a -> Either String (Refined p a)
    safeCreate = _test  (Right . accept)  (Left . failMsg)

    create :: IsRefined p a => a -> Maybe (Refined p a)
    create = _test  (Just . accept)  (const Nothing)

    refine :: IsRefined p a => a -> Refined p a
    refine = _test  accept  (error . failMsg)

    pred :: a -> Bool
    pred = predicate . (coerce :: a -> p a)

_test :: forall p a t. (Predicate p a) => (p a -> t) -> (p a -> t) -> a -> t
_test f g x = if predicate x' then f x' else g x' where x' = coerce x


class (IsRefined p a, Predicate p a) => IsRefinedByPredicate p a where


instance (IsRefined p a, Predicate p a) => IsRefinedByPredicate p a where



-- instance Predicate (p :: Type -> Type) a => IsRefined p a where


-- instance (Coercible a (p a), IsRefined p a) => Predicate p a where
--     predicate = predicate . (`coerceToProxyTypeOf` (Proxy :: Proxy (p a)))
--     failMsg = failMsg . (`coerceToProxyTypeOf` (Proxy :: Proxy (p a)))


instance (Num a, IsRefinedByPredicate p a) => Num (Refined p a)
  where
    (Refined x) + (Refined y) = refine (x + y)
    (Refined x) - (Refined y) = refine (x - y)
    (Refined x) * (Refined y) = refine (x * y)
    negate (Refined x)        = refine (negate x)
    abs    (Refined x)        = refine (abs    x)
    signum (Refined x)        = refine (signum x)
    fromInteger               = refine . fromInteger


instance (Read a, IsRefinedByPredicate p a) => Read (Refined p a) where
    readPrec = refine <$> readPrec


instance (IsString a, IsRefinedByPredicate p a) => IsString (Refined p a) where
    fromString = refine . fromString


instance (IsList a, IsRefinedByPredicate p a) => IsList (Refined p a) where
    type Item (Refined p a) = Item a
    fromList = refine . fromList
    toList = toList . plain


instance (ListLike full item, IsRefinedByPredicate p full) => FoldableLL (Refined p full) item where
    foldl f acc = foldl f acc . plain
    foldr f acc = foldr f acc . plain

instance (ListLike full item, IsRefinedByPredicate p full) => Semigroup (Refined p full)  where
    (Refined x) <> (Refined y) = refine (x <> y)

instance (ListLike full item, IsRefinedByPredicate p full) => Monoid (Refined p full)  where
    mempty = refine mempty

instance (ListLike full item, IsRefinedByPredicate p full) => ListLike (Refined p full) item where
    singleton = refine . singleton
    head = head . plain
    tail = refine . tail . plain
    null = null . plain
    genericLength = genericLength . plain


instance (Arbitrary a, IsRefinedByPredicate p a) => Arbitrary (Refined p a) where
    arbitrary = arbitrary `suchThatMap` create
