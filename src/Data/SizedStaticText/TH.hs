{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|

Template Haskell helpers for SizedStaticText.

-}

module Data.SizedStaticText.TH
       ( sz
       )

where

import           Prelude
import qualified Prelude              as P (length)

import           Data.SizedStaticText.Class
import           Data.String

import           Language.Haskell.TH


-- | A type with IsString instance to allow string literals in 'sz'
-- argument without quoting.
newtype LitS = LitS String deriving IsString


-- | Type-safe SizedStatic constructor macro for string literals.
--
-- Example:
--
-- > $(sz "Foobar")
--
-- compiles to
--
-- > unsafeCreate "Foobar" :: forall a. (IsString a, IsSizedStaticText a) => SizedStatic a 6 6
--
-- where 6 is the string length obtained at compile time.
sz :: LitS -> Q Exp
sz (LitS s) =
  do
    at <- newName "a"
    let len = LitT $ NumTyLit (fromIntegral $ P.length s)
    return $ SigE (AppE (VarE 'unsafeCreate) (LitE $ StringL s))
                (ForallT
                 [PlainTV at]
#if MIN_VERSION_template_haskell(2,10,0)
                 [ AppT (ConT ''IsString) (VarT at)
                 , AppT (ConT ''IsSizedStaticText) (VarT at)] $
#else
                 [ ClassP ''IsString [VarT at]
                 , ClassP ''IsSizedStaticText [VarT at]] $
#endif
                 AppT
                 (AppT
                  (AppT
                   (ConT ''SizedStatic)
                   (VarT at))
                  len)
                 len)
