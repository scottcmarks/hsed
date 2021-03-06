{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|

Template Haskell helpers for 'BoundedSize'.

-}
module Data.BoundedSize.TH
  ( fx
  , mx
  , unsafeCreateExp
  , typeFromInt
  ) where

import           Prelude
import qualified Prelude                as P (length)

import           Data.BoundedSize.Class (type (?), BoundedSize, HasSize,
                                         unsafeCreate)
import           Data.String

import           Language.Haskell.TH


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> :set -XMonoLocalBinds
-- >>> :set -XTypeOperators
-- >>> :set -Wno-type-defaults
-- >>> import Data.ByteString(ByteString)
-- >>> import Data.ByteString.Internal()
-- >>> import Language.Haskell.TH.Syntax(Quasi(..))
-- >>> import Language.Haskell.TH.PprLib(Doc)


-- | A type with 'IsString' instance to allow string literals in 'mx'
-- argument without quoting.
newtype LitS =
  LitS String
  deriving (IsString)

-- | Type-safe constructor for bounded-length string literals: @a ? BoundedSize l u@
--

-- >>> runQ $ ppr <$> sb 3 6 "Foobar"
-- Data.Refined.unsafeCreate "Foobar" :: forall a_0 . (Data.HasSize.HasSize a_0,
--                                                     Data.String.IsString a_0) =>
--                                       (Data.Refined.?) a_0 (Data.BoundedSize.Class.BoundedSize 3 6)
--
-- >>> :t sb 3 6 "Foobar"
-- sb 3 6 "Foobar" :: Q Exp
sb :: Int -> Int -> String -> Q Exp
sb l u s = do
  let tl = typeFromInt l
      tu = typeFromInt u
  ta <- newName "a"
  return $ unsafeCreateExp universallyQuantifiedBoundedSizeType tl tu ta s



-- | Type-safe constructor for fixed-length string literals: @a ? BoundedSize l l@
--

-- >>> :t "Foobar"
-- "Foobar" :: IsString p => p
-- >>> :t LitS "Foobar"
-- LitS "Foobar" :: LitS
-- >>> :t fx "Foobar"
-- fx "Foobar" :: Q Exp
-- >>> $(fx "Foobar") ::  ByteString ? BoundedSize 6 6
-- "Foobar"
--
-- >>> runQ $ ppr <$> fx "Foobar"
-- Data.Refined.unsafeCreate "Foobar" :: forall a_0 . (Data.HasSize.HasSize a_0,
--                                                     Data.String.IsString a_0) =>
--                                       (Data.Refined.?) a_0 (Data.BoundedSize.Class.BoundedSize 6 6)
--
-- >>> :t runQ $ ppr <$> fx "Foobar"
-- runQ $ ppr <$> fx "Foobar" :: Quasi m => m Doc
--
-- where @6@ is the string length obtained at compile time.
--
fx :: LitS -> Q Exp
fx (LitS s) = sb l l s  where l = P.length s


-- | Type-safe constructor for bounded-length string literals: @a ? BoundedSize 0 l@
--
-- >>> :t "Foobar"
-- "Foobar" :: IsString p => p
-- >>> :t LitS "Foobar"
-- LitS "Foobar" :: LitS
-- >>> :t mx "Foobar"
-- mx "Foobar" :: Q Exp
-- >>> $(mx "Foobar") :: ByteString ? BoundedSize 0 6
-- "Foobar"
--
-- >>> runQ $ ppr <$> mx "Foobar"
-- Data.Refined.unsafeCreate "Foobar" :: forall a_0 . (Data.HasSize.HasSize a_0,
--                                                     Data.String.IsString a_0) =>
--                                       (Data.Refined.?) a_0 (Data.BoundedSize.Class.BoundedSize 0 6)
--
-- >>> :t runQ $ ppr <$> mx "Foobar"
-- runQ $ ppr <$> mx "Foobar" :: Quasi m => m Doc
--
-- where @6@ is the string length obtained at compile time.
--
mx :: LitS -> Q Exp
mx (LitS s) = sb 0 l s  where l = P.length s

-- | Transform a data-level Int to a Type value
--
-- >>> typeFromInt 3
-- LitT (NumTyLit 3)
-- >>> ppr $ typeFromInt 3
-- 3
typeFromInt :: Int -> Type
typeFromInt = LitT . NumTyLit . fromIntegral

-- | Construct
--   @unsafeCreate "Foobar" :: forall a. (IsString a, HasSize a) => typef l u a@
--   where @l@ and @u@ are the type-level KnownNat versions of the bounds of @s@
--
-- >>> :set -Wno-name-shadowing
-- >>> at <- runQ $ newName "a"
-- >>> ppr $ unsafeCreateExp universallyQuantifiedBoundedSizeType (typeFromInt 0) (typeFromInt 4) at "Boo!"
-- Data.Refined.unsafeCreate "Boo!" :: forall a_0 . (Data.HasSize.HasSize a_0,
--                                                   Data.String.IsString a_0) =>
--                                     (Data.Refined.?) a_0 (Data.BoundedSize.Class.BoundedSize 0 4)
unsafeCreateExp ::
    (Type -> Type -> Name -> Type) -- type expression constructor
 -> Type   -- type-level value for the min length
 -> Type   -- type-level value for the max lengthName
 -> Name   -- name of the wrapped type, e.g. 'ByteString'
 -> String -- literal 'IsString' value to be wrapped
 -> Exp    -- explicitly typed expression  unsafeCreate <s> :: forall a ...
unsafeCreateExp typef l u a s =
    SigE
      (AppE (VarE 'unsafeCreate) (LitE $ StringL s))
      (ForallT
         [PlainTV a]
#if MIN_VERSION_template_haskell(2,10,0)
         [AppT (ConT ''HasSize) (VarT a), AppT (ConT ''IsString) (VarT a)] $
#else
         [ClassP ''HasSize [VarT a], ClassP ''IsString [VarT a]] $
#endif
       typef l u a) -- create the final type expression, e.g. a ? BoundedSize l u


-- | Create the final expression for @a ? BoundedSize l u@
--
-- >>> universallyQuantifiedBoundedSizeType (typeFromInt 3) (typeFromInt 6) (mkName "a")
-- AppT (AppT (ConT Data.Refined.?) (VarT a)) (AppT (AppT (ConT Data.BoundedSize.Class.BoundedSize) (LitT (NumTyLit 3))) (LitT (NumTyLit 6)))
-- >>> ppr $ universallyQuantifiedBoundedSizeType (typeFromInt 3) (typeFromInt 6) (mkName "a")
-- (Data.Refined.?) a (Data.BoundedSize.Class.BoundedSize 3 6)
universallyQuantifiedBoundedSizeType ::
    Type  -- type-level value for the min length
 -> Type  -- type-level value for the max length
 -> Name  -- name of the wrapped type, e.g. 'ByteString'
 -> Type  -- type expression, e.g.  @ByteString ? BoundedSize l u@
universallyQuantifiedBoundedSizeType l u a = AppT (AppT (ConT ''(?)) (VarT a)) (AppT (AppT (ConT ''BoundedSize) l) u)
