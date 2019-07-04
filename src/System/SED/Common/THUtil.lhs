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

import qualified Data.ByteString                 as B (unpack)
import           Data.List                            (foldl)
import           GHC.Base                             ((.), String)
import           GHC.Real                             (toInteger)
import           GHC.TypeNats                         (KnownNat)

import           Language.Haskell.TH                  (Body(..), Lit(..),
                                                       Type(..), Exp(..),
                                                       Dec(..), Pat(..),
                                                       Name)

import           Extras.Bytes(Fixed_bytes(..), funpack)

import           System.SED.Common.UID                (HalfUID(..), UID(..),
                                                       halfUID, uid)

dSig :: Name -> Name -> Dec
dSig n t = SigD n (ConT t)

dVal :: Name -> Exp -> Dec
dVal n e = ValD (VarP n) (NormalB e) []

eHalfUID :: HalfUID -> Exp
eHalfUID (HalfUID fb) = eID 'halfUID fb

eUID :: UID -> Exp
eUID (UID fb) = eID 'uid fb

eID :: (KnownNat n) => Name -> Fixed_bytes n -> Exp
eID name = foldl arg (VarE name) . B.unpack . funpack
  where arg e b = AppE e (LitE (IntegerL (toInteger b)))

eValP :: Name -> String -> Exp
eValP hn ts = TupE [VarE hn, LitE (StringL ts)]


\end{code}
\end{document}
