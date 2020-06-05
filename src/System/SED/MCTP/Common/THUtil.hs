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

import           Data.Attoparsec.ByteString         (Parser, parseOnly)
import           Data.ByteString                    (unpack)
import           Data.Either                        (either)
import           Data.List                          (foldl)
import           Data.String                        (IsString (..))

import           GHC.Base                           (Maybe (..), String, id,
                                                     map, (.))
import           GHC.Err                            (error)
import           GHC.Real                           (toInteger)
import           GHC.TypeLits                       (KnownNat)

import           Language.Haskell.TH                (Body (..), Con (..),
                                                     Dec (..), DerivClause (..),
                                                     Exp (..), Lit (..), Name,
                                                     Pat (..), Type (..))


import           System.SED.MCTP.Common.Simple_Type (Core_bytes (..))
import           System.SED.MCTP.Common.UID         (HalfUID (..), UID (..),
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

-- | HalfUID as Exp
eHalfUID :: HalfUID -> Exp
eHalfUID (HalfUID fb) = eID 'halfUID fb

-- | UID as Exp
eUID :: UID -> Exp
eUID (UID fb) = eID 'uid fb

-- | Wrap a Core_bytes n
eID :: (KnownNat n) => Name -> Core_bytes n -> Exp
eID wrapperName = foldl arg (VarE wrapperName) . unpack . _
  where arg e b = AppE e (LitE (IntegerL (toInteger b)))

-- | (Name, String) pair as Exp
eValP :: Name -> String -> Exp
eValP hn ts = TupE [VarE hn, LitE (StringL ts)]
