{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE TypeOperators         #-}


{-
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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



import           Data.Kind

import           Data.Proxy
import           Prelude    (Bool (..), Eq (..), Integer, Num (..), Ord (..),
                             Show (..), String, shows, (.))






class IsPredicate (p :: Type -> Type) a where
    predicate :: Proxy p -> a -> Bool
    failMsg :: Proxy p -> a -> String




newtype Nonnegative a = N a
    deriving (Eq,Num,Ord,Show) via a

instance (Num a, Show a, Ord a) => IsPredicate Nonnegative a where
    predicate _  = (0 <=)
    failMsg _  = (`shows` " is negative!")





newtype Satisfies (p :: Type -> Type) a = Satisfies a
    deriving (Eq,Ord,Show) via a


instance Num a => Num (Satisfies p a) where
  (Satisfies x) + (Satisfies y) = Satisfies (x + y)

  (Satisfies x) - (Satisfies y) = Satisfies (x - y)

  (Satisfies x) * (Satisfies y) = Satisfies (x * y)

  negate (Satisfies x) = Satisfies (negate x)

  abs (Satisfies x) = Satisfies (abs x)

  signum (Satisfies x) = Satisfies (signum x)

  fromInteger = Satisfies . fromInteger




type role Satisfies nominal nominal
-- instance The (Satisfies p a) a

-- | An infix alias for 'Satisfies'.
type a ?p = Satisfies p a
infixr 1 ?




i :: Integer ? Nonnegative
i = 1

j :: Integer ? Nonnegative
j = (-2)
