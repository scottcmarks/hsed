{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|

Template Haskell helpers for BoundedSize.

-}
module System.SED.MCTP.Common.Base_Type.TH
  ( ci
  , cu
  , cb
  , cm
  ) where

import           Language.Haskell.TH

import           Data.HasSize                           (size)
import           Data.Smart                             (unsafeCreate)
import           Data.String                            (IsString (..))

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
-- >>> :t LitS "Foobar"
-- LitS "Foobar" :: LitS
-- >>> :t cb "Foobar"
-- cb "Foobar" :: Q Exp
-- >>> runQ $ ppr <$> cb "Foobar"
-- System.SED.MCTP.Common.Base_Type.Class.Core_bytes (Data.Smart.unsafeCreate "Foobar") :: System.SED.MCTP.Common.Base_Type.Class.Core_bytes 6
--
-- >>> :t runQ $ ppr <$> cb "Foobar"
-- runQ $ ppr <$> cb "Foobar" :: Quasi m => m Doc
--
-- where 6 is the string length obtained at compile time.
--

ci :: LitI -> Q Exp
ci  = cx 'Core_integer ''Core_integer

cu :: LitI -> Q Exp
cu li@(LitI i)
    | 0 <= i = cx 'Core_uinteger ''Core_uinteger li
    | otherwise = error "negative unsigned integer"

cx ::  Name -> Name -> LitI -> Q Exp
cx cn tn (LitI i) =
    pure $ SigE (AppE (ConE cn) (AppE (VarE 'unsafeCreate) (LitE (IntegerL i))))
                (AppT (ConT tn) (LitT (NumTyLit l)))
  where l = toInteger (size i)

cb :: LitS -> Q Exp
cb = ct 'Core_bytes ''Core_bytes

cm :: LitS -> Q Exp
cm = ct 'Core_max_bytes ''Core_max_bytes



ct ::  Name -> Name -> LitS -> Q Exp
ct cn tn (LitS s) =
    pure $ SigE (AppE (ConE cn) (AppE (VarE 'unsafeCreate) (LitE (StringL s))))
                (AppT (ConT tn) (LitT (NumTyLit l)))
  where l = toInteger (size s)
