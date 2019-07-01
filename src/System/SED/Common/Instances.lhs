\documentstyle{article}
\begin{document}
\chapter{UID}

Orphan instances.



\begin{code}
{-|
Module      : System.SED.Common.Instances
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Orphan instances.

-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}



module System.SED.Common.Instances where

import           Data.Attoparsec.ByteString()
import           Data.Maybe(Maybe(..))
import           Data.Proxy(Proxy(..))
import           GHC.Base(($), (.), fail, mconcat, pure)
import           GHC.Show(Show(..))
import           GHC.Types(Int)
import           GHC.TypeNats(KnownNat)


import           Extras.Bytes ()
import           Extras.Bytes (Fixed_bytes(..), fpack, funpack)
import           Extras.Integral (intVal)
import           System.SED.Common.StreamItem (StreamItem(..))
-- import           System.SED.Common.Integral
import           System.SED.Common.Token (Token(..),IsToken(..))
-- import           System.SED.Common.Value

\end{code}

Orphan instances, usually of otherwise unrelated classes and types.

\begin{code}

instance (KnownNat n) => IsToken (Fixed_bytes n) where
    token fb  = Bytes $ funpack fb
    fromToken (Bytes bs) = Just $ fpack bs
    fromToken _ = Nothing

instance (KnownNat n) => StreamItem (Fixed_bytes n) where
    parser = do
        tok <- parser
        case tok of
            Bytes bs -> pure $ fpack bs
            _        -> fail $ mconcat [ "Wrong token type for Fixed_bytes "
                                       , show (intVal (Proxy :: Proxy n) :: Int)
                                       , ": "
                                       , show tok
                                       ]
    generate = generate . token
\end{code}
\end{document}