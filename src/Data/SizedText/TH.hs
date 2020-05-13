{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|

Template Haskell helpers for SizedText.

-}
module Data.SizedText.TH
  ( st
  , sz
  , szfe
  ) where

import           Prelude
import qualified Prelude              as P (length)

import           Data.SizedText.Class
import           Data.String

import           Language.Haskell.TH

-- | A type with IsString instance to allow string literals in 'sz'
-- argument without quoting.
newtype LitS =
  LitS String
  deriving (IsString)

-- | Type-safe Sized constructor macros for string literals.
--
-- Example:
--
-- > $(sz "Foobar")
--
-- compiles to
--
-- > unsafeCreate "Foobar" :: forall a. (IsString a, IsSizedText a) => Sized a 0 6
--
-- where 6 is the string length obtained at compile time.
sz :: LitS -> Q Exp
sz (LitS s) = do
  at <- newName "a"
  let len = P.length s
  return $ szfe uqType at 0 len s


-- > $(st "Foobar")
--
-- compiles to
--
-- > unsafeCreate "Foobar" :: forall a. (IsString a, IsSizedText a) => Sized a 6 6
--

st :: LitS -> Q Exp
st (LitS s) = do
  at <- newName "a"
  let len = P.length s
  return $ szfe uqType at len len s


-- | Construct
-- > unsafeCreate "Foobar" :: forall a. (IsString a, IsSizedText a) => typef a l u
--   where l and u are the type-level KnownNat versions of the bounds of s
szfe :: (Name -> Int -> Int -> Type) -> Name -> Int -> Int -> String -> Exp
szfe typef at l u s =
    SigE
      (AppE (VarE 'unsafeCreate) (LitE $ StringL s))
      (ForallT
         [PlainTV at]
#if MIN_VERSION_template_haskell(2,10,0)
         [AppT (ConT ''IsString) (VarT at), AppT (ConT ''IsSizedText) (VarT at)] $
#else
         [ClassP ''IsString [VarT at], ClassP ''IsSizedText [VarT at]] $
#endif
       typef at l u) -- create the final type expression, e.g. Sized a l u

-- | Create the final expression for Sized a l l
--
uqType ::
    Name -- name of the wrapped type, e.g. ByteString
 -> Int  -- type-level value for the min length
 -> Int  -- type-level value for the max length
 -> Type -- type expression Sized l u
uqType a l u = AppT (AppT (AppT (ConT ''Sized) (VarT a)) (wx l)) (wx u)
  where wx = LitT . NumTyLit . fromIntegral
