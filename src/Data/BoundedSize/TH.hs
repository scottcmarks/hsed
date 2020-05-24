{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|

Template Haskell helpers for BoundedSize.

-}
module Data.BoundedSize.TH
  ( st
  , sz
  , unsafeCreateExp
  , typeFromInt
  ) where

import           Prelude
import qualified Prelude                as P (length)

import           Data.BoundedSize.Class
import           Data.String

import           Language.Haskell.TH


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> :set -Wno-type-defaults
-- >>> import Data.ByteString(ByteString)
-- >>> import Data.ByteString.Internal()
-- >>> import Language.Haskell.TH.Syntax(Quasi(..))
-- >>> import Language.Haskell.TH.PprLib(Doc)


-- | A type with IsString instance to allow string literals in 'sz'
-- argument without quoting.
newtype LitS =
  LitS String
  deriving (IsString)

-- | Type-safe constructor for bounded-length string literals: BoundedSize a l u
-- Data.BoundedSize.Class.unsafeCreate "Foobar" :: forall a_0 . (Data.String.IsString a_0,
--                                                               Data.BoundedSize.Class.IsBoundedSize a_0
--                                                                                                    3
--                                                                                                    6) =>
--                                                 Data.BoundedSize.Class.BoundedSize a_0 3 6
--
-- >>> runQ $ ppr <$> sb 3 6 "Foobar"
-- Data.BoundedSize.Class.unsafeCreate "Foobar" :: forall a_0 . (Data.String.IsString a_0,
--                                                               Data.BoundedSize.Class.IsBoundedSize a_0
--                                                                                                    3
--                                                                                                    6) =>
--                                                 Data.BoundedSize.Class.BoundedSize a_0 3 6
--
-- >>> :t sb 3 6 "Foobar"
-- sb 3 6 "Foobar" :: Q Exp
sb :: Int -> Int -> String -> Q Exp
sb l u s = do
  ta <- newName "a"
  let tl = typeFromInt l
      tu = typeFromInt u
  return $ unsafeCreateExp universallyQuantifiedBoundedSizeType ta tl tu s



-- | Type-safe constructor for fixed-length string literals: BoundedSize a l l
--
-- >>> :t "Foobar"
-- "Foobar" :: IsString p => p
-- >>> :t LitS "Foobar"
-- LitS "Foobar" :: LitS
-- >>> :t st "Foobar"
-- st "Foobar" :: Q Exp
-- >>> $(st "Foobar") :: BoundedSize ByteString 6 6
-- "Foobar"
--
-- >>> runQ $ ppr <$> st "Foobar"
-- Data.BoundedSize.Class.unsafeCreate "Foobar" :: forall a_0 . (Data.String.IsString a_0,
--                                                               Data.BoundedSize.Class.IsBoundedSize a_0
--                                                                                                    6
--                                                                                                    6) =>
--                                                 Data.BoundedSize.Class.BoundedSize a_0 6 6
--
-- >>> :t runQ $ ppr <$> st "Foobar"
-- runQ $ ppr <$> st "Foobar" :: Quasi m => m Doc
--
-- where 6 is the string length obtained at compile time.
--
st :: LitS -> Q Exp
st (LitS s) = sb l l s  where l = P.length s


-- | Type-safe constructor for bounded-length string literals: BoundedSize a 0 l
--
-- >>> :t "Foobar"
-- "Foobar" :: IsString p => p
-- >>> :t LitS "Foobar"
-- LitS "Foobar" :: LitS
-- >>> :t sz "Foobar"
-- sz "Foobar" :: Q Exp
-- >>> $(sz "Foobar") :: BoundedSize ByteString 0 6
-- "Foobar"
--
-- >>> runQ $ ppr <$> sz "Foobar"
-- Data.BoundedSize.Class.unsafeCreate "Foobar" :: forall a_0 . (Data.String.IsString a_0,
--                                                               Data.BoundedSize.Class.IsBoundedSize a_0
--                                                                                                    0
--                                                                                                    6) =>
--                                                 Data.BoundedSize.Class.BoundedSize a_0 0 6
--
-- >>> :t runQ $ ppr <$> sz "Foobar"
-- runQ $ ppr <$> sz "Foobar" :: Quasi m => m Doc
--
-- where 6 is the string length obtained at compile time.
--
sz :: LitS -> Q Exp
sz (LitS s) = sb 0 l s  where l = P.length s

-- | Transform a data-level Int to a Type value
-- LitT (NumTyLit 3)
--
-- >>> typeFromInt 3
-- LitT (NumTyLit 3)
-- >>> ppr $ typeFromInt 3
-- 3
typeFromInt :: Int -> Type
typeFromInt = LitT . NumTyLit . fromIntegral

-- | Construct
-- > unsafeCreate "Foobar" :: forall a. (IsString a, IsBoundedSize a) => typef a l u
--   where l and u are the type-level KnownNat versions of the bounds of s
--
-- >>> at <- runQ $ newName "a"
-- >>> ppr $ unsafeCreateExp universallyQuantifiedBoundedSizeType at (typeFromInt 0) (typeFromInt 4) "Boo!"
-- Data.BoundedSize.Class.unsafeCreate "Boo!" :: forall a_0 . (Data.String.IsString a_0,
--                                                             Data.BoundedSize.Class.IsBoundedSize a_0
--                                                                                                  0
--                                                                                                  4) =>
--                                               Data.BoundedSize.Class.BoundedSize a_0 0 4
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
         [AppT (ConT ''IsString) (VarT at), AppT (AppT (AppT (ConT ''IsBoundedSize) (VarT at)) l) u] $
#else
         [ClassP ''IsString [VarT at], ClassP ''IsBoundedSize [VarT at] l u] $
#endif
       typef at l u) -- create the final type expression, e.g. BoundedSize a l u


-- | Create the final expression for BoundedSize a l u
--
-- >>> universallyQuantifiedBoundedSizeType (mkName "a") (typeFromInt 3) (typeFromInt 6)
-- AppT (AppT (AppT (ConT Data.BoundedSize.Class.BoundedSize) (VarT a)) (LitT (NumTyLit 3))) (LitT (NumTyLit 6))
-- >>> ppr $ universallyQuantifiedBoundedSizeType (mkName "a") (typeFromInt 3) (typeFromInt 6)
-- Data.BoundedSize.Class.BoundedSize a 3 6
universallyQuantifiedBoundedSizeType ::
    Name -- name of the wrapped type, e.g. ByteString
 -> Type  -- type-level value for the min length
 -> Type  -- type-level value for the max length
 -> Type  -- type expression, e.g. BoundedSize ByteString l u
universallyQuantifiedBoundedSizeType a l u = AppT (AppT (AppT (ConT ''BoundedSize) (VarT a)) l) u
