\documentstyle{article}
\begin{document}
\chapter{Template Haskell Utilities}

TableUIDs Template Haskell


\begin{code}
{-|
Module      : System.SED.Common.THUtil
Description : SED table UIDs Template Haskell
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Template Haskell utilities.

-}

{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module System.SED.Common.THUtil
where

import           Data.Attoparsec.ByteString      (Parser, parseOnly)
import qualified Data.ByteString            as B (unpack)
import           Data.Either                     (either)
import           Data.List                       (foldl)
import           Data.String                     (IsString (..))

import           GHC.Base                        (String, id, (.))
import           GHC.Err                         (error)
import           GHC.Real                        (toInteger)
import           GHC.TypeNats                    (KnownNat)

import           Language.Haskell.TH             (Body(..), Lit(..),
                                                  Type(..), Exp(..),
                                                  Dec(..), Pat(..),
                                                  Name)

import           Extras.Bytes                    (Fixed_bytes(..), funpack)

import           System.SED.Common.UID           (HalfUID(..), UID(..),
                                                  halfUID, uid)


-- | Run a table parser (any parser, really) producing a result or throwing an error.
--   This is safe only in TH code, where an error is a compilation error.
parseTable :: Parser c -> String -> c
parseTable tableParser = either error id . parseOnly tableParser . fromString


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

-- | Wrap a Fixed_bytes n
eID :: (KnownNat n) => Name -> Fixed_bytes n -> Exp
eID wrapperName = foldl arg (VarE wrapperName) . B.unpack . funpack
  where arg e b = AppE e (LitE (IntegerL (toInteger b)))

-- | (Name, String) pair as Exp
eValP :: Name -> String -> Exp
eValP hn ts = TupE [VarE hn, LitE (StringL ts)]


\end{code}
\end{document}
