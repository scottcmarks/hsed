{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|

Template Haskell helpers for BoundedSize.

-}
module System.SED.MCTP.Common.Base_Type.TH
  ( ci  -- $(ci 42) ==> (unsafeCreate l) :: (KnownNat l, 1<=l) => Core_integer l, where 1 == size 42
  , ci' -- $(ci 42) ==> (unsafeCreate 42) :: Core_integer 1 == ($ci 42)::Core_integer 1
  , cu  -- like ci, but Core_uinteger
  , cu' -- like ci'
  , cb  -- like ci, but Core_bytes
  , cb' -- like cb
  , cm  -- like ci, but Core_max_bytes
  , cm' -- like cm
  ) where

import           Language.Haskell.TH

import           Data.HasSize                           (size)
import           Data.Refined                           (unsafeCreate)
import           Data.String                            (IsString (..))
import           GHC.TypeLits                           (type (<=), KnownNat)

import           System.SED.MCTP.Common.Base_Type.Class



-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString(ByteString)
-- >>> import Data.ByteString.Internal()
-- >>> import Data.Functor                           ((<$>))
-- >>> import Language.Haskell.TH.Syntax(Quasi(..))
-- >>> import Language.Haskell.TH.PprLib(Doc)


-- | A type with IsString instance to allow string literals in
-- argument without quoting.
newtype LitS =
  LitS String
  deriving (IsString)


-- | A type with Num instance to allow integer literals in
-- argument without quoting.
newtype LitI =
     LitI Integer
  deriving (Num)



-- | Type-safe constructor for fixed-length byte literals: Core_bytes n
--

-- >>> :t "Foobar"
-- "Foobar" :: IsString p => p
-- >>> :t LitI 142857
-- LitI 142857 :: LitI
-- >>> :t ci 142857
-- ci 142857 :: Q Exp
-- >>> runQ $ ppr <$> ci 142857
-- System.SED.MCTP.Common.Base_Type.Class.Core_integer (Data.Refined.unsafeCreate 142857) :: forall n_0 . (GHC.TypeNats.KnownNat n_0,
--                                                                                                       (GHC.TypeNats.<=) 3
--                                                                                                                         n_0) =>
--                                                                                         System.SED.MCTP.Common.Base_Type.Class.Core_integer n_0
--
-- >>> :t runQ $ ppr <$> ci 142857
-- runQ $ ppr <$> ci 142857 :: Quasi m => m Doc
--
-- where 3 is the "byte size" of the integer 142857 obtained at compile time.
--

ci :: LitI -> Q Exp
ci  = cx 'Core_integer ''Core_integer

ci' :: LitI -> Q Exp
ci'  = cx' 'Core_integer ''Core_integer

cu :: LitI -> Q Exp
cu li@(LitI i)
    | 0 <= i = cx 'Core_uinteger ''Core_uinteger li
    | otherwise = error "negative unsigned integer"

cu' :: LitI -> Q Exp
cu' li@(LitI i)
    | 0 <= i = cx' 'Core_uinteger ''Core_uinteger li
    | otherwise = error "negative unsigned integer"

cx ::  Name -> Name -> LitI -> Q Exp
cx cn tn (LitI i) = do
    n <- newName "n"
    return $ SigE (AppE (ConE cn) (AppE (VarE 'unsafeCreate) (LitE (IntegerL i))))
                  (ForallT [PlainTV n]
                   [AppT (ConT ''KnownNat) (VarT n),
                    AppT (AppT (ConT ''(<=)) (LitT (NumTyLit l))) (VarT n) ]
                   (AppT (ConT tn) (VarT n)))
  where l = toInteger (size i)

cx' ::  Name -> Name -> LitI -> Q Exp
cx' cn tn (LitI i) =
    pure $ SigE (AppE (ConE cn) (AppE (VarE 'unsafeCreate) (LitE (IntegerL i))))
                (AppT (ConT tn) (LitT (NumTyLit l)))
  where l = toInteger (size i)

-- >>> :t "Foobar"
-- "Foobar" :: IsString p => p
-- >>> :t LitS "Foobar"
-- LitS "Foobar" :: LitS
-- >>> :t cb "Foobar"
-- cb "Foobar" :: Q Exp
-- >>> runQ $ ppr <$> cb "Foobar"
-- System.SED.MCTP.Common.Base_Type.Class.Core_bytes (Data.Refined.unsafeCreate "Foobar") :: forall n_0 . (GHC.TypeNats.KnownNat n_0,
--                                                                                                       (GHC.TypeNats.<=) 6
--                                                                                                                         n_0) =>
--                                                                                         System.SED.MCTP.Common.Base_Type.Class.Core_bytes n_0
--
-- >>> :t runQ $ ppr <$> cb "Foobar"
-- runQ $ ppr <$> cb "Foobar" :: Quasi m => m Doc
--
-- where 6 is the string length obtained at compile time.
--
cb :: LitS -> Q Exp
cb = ct 'Core_bytes ''Core_bytes

-- >>> :t "Foobar"
-- "Foobar" :: IsString p => p
-- >>> :t LitS "Foobar"
-- LitS "Foobar" :: LitS
-- >>> :t cb' "Foobar"
-- cb' "Foobar" :: Q Exp
-- >>> runQ $ ppr <$> cb' "Foobar"
-- System.SED.MCTP.Common.Base_Type.Class.Core_bytes (Data.Refined.unsafeCreate "Foobar") :: System.SED.MCTP.Common.Base_Type.Class.Core_bytes 6
--
-- >>> :t runQ $ ppr <$> cb' "Foobar"
-- runQ $ ppr <$> cb' "Foobar" :: Quasi m => m Doc
--
-- where 6 is the string length obtained at compile time.
--
cb' :: LitS -> Q Exp
cb' = ct' 'Core_bytes ''Core_bytes

-- >>> :t "Foobar"
-- "Foobar" :: IsString p => p
-- >>> :t LitS "Foobar"
-- LitS "Foobar" :: LitS
-- >>> :t cm "Foobar"
-- cm "Foobar" :: Q Exp
-- >>> runQ $ ppr <$> cm "Foobar"
-- System.SED.MCTP.Common.Base_Type.Class.Core_max_bytes (Data.Refined.unsafeCreate "Foobar") :: forall n_0 . (GHC.TypeNats.KnownNat n_0,
--                                                                                                           (GHC.TypeNats.<=) 6
--                                                                                                                             n_0) =>
--                                                                                             System.SED.MCTP.Common.Base_Type.Class.Core_max_bytes n_0
--
-- >>> :t runQ $ ppr <$> cm "Foobar"
-- runQ $ ppr <$> cm "Foobar" :: Quasi m => m Doc
--
-- where 6 is the string length obtained at compile time.
--
cm :: LitS -> Q Exp
cm = ct 'Core_max_bytes ''Core_max_bytes

-- >>> :t "Foobar"
-- "Foobar" :: IsString p => p
-- >>> :t LitS "Foobar"
-- LitS "Foobar" :: LitS
-- >>> :t cm' "Foobar"
-- cm' "Foobar" :: Q Exp
-- >>> runQ $ ppr <$> cm' "Foobar"
-- System.SED.MCTP.Common.Base_Type.Class.Core_max_bytes (Data.Refined.unsafeCreate "Foobar") :: System.SED.MCTP.Common.Base_Type.Class.Core_max_bytes 6
--
-- >>> :t runQ $ ppr <$> cm' "Foobar"
-- runQ $ ppr <$> cm' "Foobar" :: Quasi m => m Doc
--
-- where 6 is the string length obtained at compile time.
--
cm' :: LitS -> Q Exp
cm' = ct' 'Core_max_bytes ''Core_max_bytes


ct ::  Name -> Name -> LitS -> Q Exp
ct cn tn (LitS s) = do
    n <- newName "n"
    return $ SigE (AppE (ConE cn) (AppE (VarE 'unsafeCreate) (LitE (StringL s))))
                  (ForallT [PlainTV n]
                   [AppT (ConT ''KnownNat) (VarT n),AppT (AppT (ConT ''(<=)) (LitT (NumTyLit l))) (VarT n)]
                   (AppT (ConT tn) (VarT n)))
  where l = toInteger (size s)

ct' ::  Name -> Name -> LitS -> Q Exp
ct' cn tn (LitS s) =
    pure $ SigE (AppE (ConE cn) (AppE (VarE 'unsafeCreate) (LitE (StringL s))))
                (AppT (ConT tn) (LitT (NumTyLit l)))
  where l = toInteger (size s)
