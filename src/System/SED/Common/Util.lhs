\documentstyle{article}
\begin{document}
\chapter{Util}

Utilities


\begin{code}
{-|
Module      : System.SED.Common.Util
Description : Utilities
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Utilities

-}

{-# LANGUAGE NoImplicitPrelude   #-}

module System.SED.Common.Util where

import           Data.Attoparsec.ByteString.Char8 (isSpace_w8)
import           RIO.ByteString (ByteString, spanEnd)

trimTrailingWhitespace :: ByteString -> ByteString
trimTrailingWhitespace s =
    case spanEnd isSpace_w8 s of (s',_) -> s'

\end{code}
\end{document}
