{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
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
  ( Refined
  , plain
  , unsafeCreate
  , type (?)
  , Predicate(..)
  )
where

import           Data.Coerce     (Coercible, coerce)
import           Data.Kind       (Type)
import           Data.ListLike   (FoldableLL (..), ListLike (..))
import           Data.String     (IsString (..), String)
import           GHC.Base        (Monoid (..), Semigroup (..), const)
import           GHC.Exts        (IsList (..))
import           GHC.Read        (Read (..))
import           Prelude         (Bool (..), Either (..), Eq (..), Maybe (..),
                                  Num (..), Ord (..), Show (..), error, (.),
                                  (<$>))

import           Test.QuickCheck (Arbitrary (..), suchThatMap)


type role Refined phantom _
newtype Refined (p :: Type -> Type) a = Refined a
    deriving (Eq, Ord, Show) via a

plain :: Refined p a -> a
plain = coerce

unsafeCreate :: a -> Refined p a
unsafeCreate = coerce


-- | An infix alias for 'Refined'.
type a ? p = Refined p a
infixr 1 ?


test :: forall p a t. (Predicate p a)
 => (p a -> t) -> (p a -> t) -> a -> t
test f g x = if predicate x' then f x' else g x' where x' = coerce x

accept :: Coercible (p a) a => p a -> Refined p a
accept = coerce

class Coercible (p a) a => Predicate (p :: Type -> Type) a where
    predicate :: p a -> Bool

    failMsg :: p a -> String

    safeCreate :: a -> Either String (Refined p a)
    safeCreate = test  (Right . accept)  (Left . failMsg)

    create :: a -> Maybe (Refined p a)
    create = test  (Just . accept)  (const Nothing)

    refine :: a -> Refined p a
    refine = test  accept  (error . failMsg)


instance (Num a, Predicate p a) => Num (Refined p a)
  where
    (Refined x) + (Refined y) = refine (x + y)
    (Refined x) - (Refined y) = refine (x - y)
    (Refined x) * (Refined y) = refine (x * y)
    negate (Refined x)        = refine (negate x)
    abs    (Refined x)        = refine (abs    x)
    signum (Refined x)        = refine (signum x)
    fromInteger               = refine . fromInteger


instance (Read a, Predicate p a) => Read (Refined p a) where
    readPrec = refine <$> readPrec


instance (IsString a, Predicate p a) => IsString (Refined p a) where
    fromString = refine . fromString


instance (IsList a, Predicate p a) => IsList (Refined p a) where
    type Item (Refined p a) = Item a
    fromList = refine . fromList
    toList = toList . plain


instance (ListLike full item, Predicate p full) => FoldableLL (Refined p full) item where
    foldl f acc = foldl f acc . plain
    foldr f acc = foldr f acc . plain

instance (ListLike full item, Predicate p full) => Semigroup (Refined p full)  where
    (Refined x) <> (Refined y) = refine (x <> y)

instance (ListLike full item, Predicate p full) => Monoid (Refined p full)  where
    mempty = refine mempty

instance (ListLike full item, Predicate p full) => ListLike (Refined p full) item where
    singleton = refine . singleton
    head = head . plain
    tail = refine . tail . plain
    null = null . plain
    genericLength = genericLength . plain


instance (Arbitrary a, Predicate p a) => Arbitrary (Refined p a) where
    arbitrary = arbitrary `suchThatMap` create
