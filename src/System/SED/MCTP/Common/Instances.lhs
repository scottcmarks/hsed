\documentstyle{article}
\begin{document}
\chapter{UID}

Orphan instances.



\begin{code}
{-|
Module      : System.SED.MCTP.Common.Instances
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Orphan instances.

-}

{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}



module System.SED.MCTP.Common.Instances where

import           Control.Monad(fail)
import           Data.Attoparsec.ByteString()
import           Data.Maybe(Maybe(..))
import           Data.Proxy(Proxy(..))
import           GHC.Base(($), (.), mconcat, pure)
import           GHC.Show(Show(..))
import           GHC.Types(Int)
import           GHC.TypeLits(KnownNat)


import           GHC.TypeLits.Extras (fromNat)
import           System.SED.MCTP.Common.Base_Type (Core_bytes(..))
import           System.SED.MCTP.Common.StreamItem (StreamItem(..))
-- import           System.SED.MCTP.Common.Integral
import           System.SED.MCTP.Common.Token (Token(..),IsToken(..))
-- import           System.SED.MCTP.Common.Value

\end{code}

Orphan instances, usually of otherwise unrelated classes and types.

\begin{code}

instance (KnownNat n) => IsToken (Core_bytes n) where
    token fb  = Bytes $ funpack fb
    fromToken (Bytes bs) = Just $ fpack bs
    fromToken _ = Nothing

instance (KnownNat n) => StreamItem (Core_bytes n) where
    parser = do
        tok <- parser
        case tok of
            Bytes bs -> pure $ fpack bs
            _        -> fail $ mconcat [ "Wrong token type for Core_bytes "
                                       , show (fromNat (Proxy @n) ::Int)
                                       , ": "
                                       , show tok
                                       ]
    generate = generate . token
\end{code}
\end{document}
