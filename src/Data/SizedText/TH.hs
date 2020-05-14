{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|

Template Haskell helpers for SizedText.

-}
module Data.SizedText.TH
  ( st
  , sz
  , unsafeCreateExp
  , typeFromInt
  ) where

import           Prelude
import qualified Prelude              as P (length)

import           Data.SizedText.Class
import           Data.String

import           Language.Haskell.TH


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> :set -Wno-type-defaults


-- | A type with IsString instance to allow string literals in 'sz'
-- argument without quoting.
newtype LitS =
  LitS String
  deriving (IsString)

-- | Type-safe constructor for bounded-length string literals: Sized a l u
-- Data.SizedText.Class.unsafeCreate "Foobar" :: forall a_0 . (Data.String.IsString a_0,
--                                                             Data.SizedText.Class.IsSizedText a_0) =>
--                                               Data.SizedText.Class.Sized a_0 3 6
--
-- >>> runQ $ ppr <$> sb 3 6 "Foobar"
-- Data.SizedText.Class.unsafeCreate "Foobar" :: forall a_0 . (Data.String.IsString a_0,
--                                                             Data.SizedText.Class.IsSizedText a_0) =>
--                                               Data.SizedText.Class.Sized a_0 3 6
--
-- >>> :t sb 3 6 "Foobar"
-- sb 3 6 "Foobar" :: Q Exp
sb :: Int -> Int -> String -> Q Exp
sb l u s = do
  ta <- newName "a"
  let tl = typeFromInt l
      tu = typeFromInt u
  return $ unsafeCreateExp universallyQuantifiedSizedType ta tl tu s


-- | Type-safe constructor for bounded-length string literals: Sized a 0 l
--
-- >>> $(sz "Foobar")
-- "Foobar"
--
-- >>> szFoo = $(sz "Foobar")
-- >>> :type szFoo
-- szFoo :: (IsString a, IsSizedText a) => Sized a 0 6
--
-- >>> runQ $ ppr <$> sz "Foobar"
-- Data.SizedText.Class.unsafeCreate "Foobar" :: forall a_0 . (Data.String.IsString a_0,
--                                                             Data.SizedText.Class.IsSizedText a_0) =>
--                                               Data.SizedText.Class.Sized a_0 0 6
--
-- >>> :t sz "Foobar"
-- sz "Foobar" :: Q Exp
--
-- where 6 is the string length obtained at compile time.
sz :: LitS -> Q Exp
sz (LitS s) = sb 0 l s  where l = P.length s


-- | Type-safe constructor for fixed-length string literals: Sized a l l
--
-- >>> $(st "Foobar")
-- "Foobar"
--
-- >>> stFoo = $(st "Foobar")
-- >>> :type stFoo
-- stFoo :: (IsString a, IsSizedText a) => Sized a 6 6
--
-- >>> runQ $ ppr <$> st "Foobar"
-- Data.SizedText.Class.unsafeCreate "Foobar" :: forall a_0 . (Data.String.IsString a_0,
--                                                             Data.SizedText.Class.IsSizedText a_0) =>
--                                               Data.SizedText.Class.Sized a_0 6 6
--
-- >>> :t st "Foobar"
-- st "Foobar" :: Q Exp
--
-- where 6 is the string length obtained at compile time.
--
st :: LitS -> Q Exp
st (LitS s) = sb l l s  where l = P.length s

-- | Transform a data-level Int to a Type value
--
-- >>> typeFromInt 3
-- LitT (NumTyLit 3)
-- >>> ppr $ typeFromInt 3
-- 3
typeFromInt :: Int -> Type
typeFromInt = LitT . NumTyLit . fromIntegral

-- | Construct
-- > unsafeCreate "Foobar" :: forall a. (IsString a, IsSizedText a) => typef a l u
--   where l and u are the type-level KnownNat versions of the bounds of s
--
-- >>> at <- runQ $ newName "a"
-- >>> ppr $ unsafeCreateExp universallyQuantifiedSizedType at (typeFromInt 0) (typeFromInt 4) "Boo!"
-- Data.SizedText.Class.unsafeCreate "Boo!" :: forall a_0 . (Data.String.IsString a_0,
--                                                           Data.SizedText.Class.IsSizedText a_0) =>
--                                             Data.SizedText.Class.Sized a_0 0 4
unsafeCreateExp ::
    (Name -> Type -> Type -> Type) -- type expression constructor
 -> Name   -- name of the wrapped type, e.g. ByteString
 -> Type   -- type-level value for the min length
 -> Type   -- type-level value for the max lengthName
 -> String -- literal IsString value to be wrapped
 -> Exp    -- type express  unsafeCreate <s> :: forall a ...
unsafeCreateExp typef at l u s =
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


-- | Create the final expression for Sized a l u
--
-- >>> universallyQuantifiedSizedType (mkName "a") (typeFromInt 3) (typeFromInt 6)
-- AppT (AppT (AppT (ConT Data.SizedText.Class.Sized) (VarT a)) (LitT (NumTyLit 3))) (LitT (NumTyLit 6))
-- >>> ppr $ universallyQuantifiedSizedType (mkName "a") (typeFromInt 3) (typeFromInt 6)
-- Data.SizedText.Class.Sized a 3 6
universallyQuantifiedSizedType ::
    Name -- name of the wrapped type, e.g. ByteString
 -> Type  -- type-level value for the min length
 -> Type  -- type-level value for the max length
 -> Type  -- type expression Sized l u
universallyQuantifiedSizedType a l u = AppT (AppT (AppT (ConT ''Sized) (VarT a)) l) u
