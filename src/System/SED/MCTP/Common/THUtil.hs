{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-|
Module      : System.SED.MCTP.Common.THUtil
Description : SED table UIDs Template Haskell
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

TableUIDs Template Haskell
-}


module System.SED.MCTP.Common.THUtil
where

import           Data.Attoparsec.ByteString             (Parser, parseOnly)
import           Data.Either                            (either)
import           Data.List                              (foldl)
import           Data.String                            (IsString (..))

import           GHC.Base                               (Maybe (..), String, id,
                                                         map, (.))
import           GHC.Err                                (error)
import           GHC.Integer                            (Integer)
import           GHC.Real                               (Integral (..),
                                                         toInteger)
import           GHC.TypeLits                           (KnownNat)
import           GHC.Word                               (Word8 (..))

import           Language.Haskell.TH                    (Body (..), Con (..),
                                                         Dec (..),
                                                         DerivClause (..),
                                                         Exp (..), Lit (..),
                                                         Name, Pat (..),
                                                         TyLit (..), Type (..))
import           System.SED.MCTP.Common.Base_Type       (Core_bytes (..),
                                                         toList)
import           System.SED.MCTP.Common.Reference_Types (btHalfUID, otHalfUID)
import           System.SED.MCTP.Common.UID             (HalfUID (..), UID (..),
                                                         halfUID, uid)

-- | Run a table parser (any parser, really) producing a result or throwing an error.
--   This is safe only in TH code, where an error is a compilation error.
parseTable :: Parser c -> String -> c
parseTable tableParser = either error id . parseOnly tableParser . fromString

-- | Data declaration, essentially [d| data $n $c0 | $c1 ... deriving ($d0,$d1,...) |]
dData :: Name -> [Name] -> [Name] -> Dec
dData n cs ds =  DataD [] n [] Nothing (map (`NormalC` []) cs)
                       [DerivClause Nothing (map ConT ds)]

-- | Signature declaration, essentially [d| $n :: $t |]
dSig :: Name -> Name -> Dec
dSig n t = SigD n (ConT t)

-- | Value declaration, essentially [d| $n = $e |]
dVal :: Name -> Exp -> Dec
dVal n e = ValD (VarP n) (NormalB e) []

-- | Type synonym declaration, essentially [d| type $n = $t $v |]
dTyp :: Name -> Name -> Integer -> Dec
dTyp n t v = TySynD n [] (AppT (ConT t) (LitT (NumTyLit v)))

-- | HalfUID as Exp
eHalfUID :: HalfUID -> Exp
eHalfUID (HalfUID fb) = eID 'halfUID fb

-- | Byte_Table_HalfUID as Exp
eByte_Table_HalfUID :: HalfUID -> Exp
eByte_Table_HalfUID (HalfUID fb) = eID 'btHalfUID fb

-- | Object_Table_HalfUID as Exp
eObject_Table_HalfUID :: HalfUID -> Exp
eObject_Table_HalfUID (HalfUID fb) = eID 'otHalfUID fb

-- | UID as Exp
eUID :: UID -> Exp
eUID (UID fb) = eID 'uid fb

-- | Wrap a Core_bytes n
eID :: (KnownNat n) => Name -> Core_bytes n -> Exp
eID wrapperName = foldl arg (VarE wrapperName) . toList
  where
    arg  :: Exp -> Word8 -> Exp
    arg e b = AppE e (LitE (IntegerL (toInteger b)))

-- | String literal as Exp
eLitS :: String -> Exp
eLitS ts = LitE (StringL ts)


-- | (Name, String) pair as Exp
eValP :: Name -> String -> Exp
eValP hn ts = TupE [VarE hn, eLitS ts]
