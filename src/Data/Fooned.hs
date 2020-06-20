{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}


{-
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE IncoherentInstances   #-}

{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-}

module Data.Fooned  where

{-

What am I trying to say with my predicates?

Basically, instead of

3 :: Integer

which says that the type of 3, which might otherwise be

λ> :t 3
3 :: Num p => p

a polymorphic type constrained by the typeclass Num,
and hence can be narrowed to e.g.


λ> import Prelude(Num,Integer)
λ> :i Num
class Num a where
  (GHC.Num.+) :: a -> a -> a
  (GHC.Num.-) :: a -> a -> a
  (GHC.Num.*) :: a -> a -> a
  GHC.Num.negate :: a -> a
  GHC.Num.abs :: a -> a
  GHC.Num.signum :: a -> a
  GHC.Num.fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
  	-- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
λ> :t 3::Integer
3::Integer :: Integer



would instead look like

3 :: Refined NonNegative Integer

with things like

instance Num a => Num (NonNegative a)

and

instance

-}


import           Data.Coerce
import           Data.Kind

import           Prelude     (Bool (..), Eq (..), Integer, Num (..), Ord (..),
                              Show (..), String, error, shows, ($))



type role Satisfies nominal nominal
newtype Satisfies (p :: Type -> Type) a = Satisfies a
    deriving (Eq,Ord,Show) via a
-- instance The (Satisfies p a) a

-- | An infix alias for 'Satisfies'.
type a ?p = Satisfies p a
infixr 1 ?


class (Coercible (p a) a) => IsPredicate p a where
    predicate :: p a -> Bool
    failMsg :: p a -> String
    require :: p a -> Satisfies p a
    require x  = if predicate x then coerce x else error $ failMsg x

instance (IsPredicate p a, Num a, Coercible a (p a)) => Num (Satisfies p a)
  where
    (Satisfies x) + (Satisfies y) = require $ coerce (x + y)
    (Satisfies x) - (Satisfies y) = require $ coerce (x - y)
    (Satisfies x) * (Satisfies y) = require $ coerce (x * y)
    negate (Satisfies x) = require $ coerce (negate x)
    abs (Satisfies x) = require $ coerce (abs x)
    signum (Satisfies x) = require $ coerce (signum x)
    fromInteger x = require $ coerce $ (fromInteger x :: a)





newtype Nonnegative a = N a
    deriving (Eq,Num,Ord,Show) via a

instance (Num a, Show a, Ord a) => IsPredicate Nonnegative a where
    predicate = (0 <=)
    failMsg = (`shows` " is negative!")

i :: Satisfies Nonnegative Integer
i = 1

j :: Satisfies Nonnegative Integer
j = (-2)
