\documentstyle{article}
\begin{document}
\chapter{Types}

Parse and generate method invocations and responses.


\begin{code}
{-|
Module      : System.SED.Common.Method
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for Tokens.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.SED.Common.Method where

import           RIO                          hiding (foldr, map, length, mask,
                                                      reverse, take)
import           Test.QuickCheck              hiding (generate)

import           System.SED.Common.StreamItem
import           System.SED.Common.Token

\end{code}

Due to the nature of method parameters and results, there are two additional constructs defined for
messaging that serve as grouping mechanisms for the basic types: Named values and List values.

  a. Named values. The name (a byte-string/integer/uinteger value) followed by its value (any
messaging type, i.e. byte-string values, N length signed or unsigned integer values, list values,
or Named values).
\begin{code}

newtype Name = Name Final
    deriving (Show,Eq)

instance StreamItem Name where
    parse             = Name <$> parse
    generate (Name f) = generate f

instance Arbitrary Name where
    arbitrary = Name <$> arbitrary

\end{code}
\end{document}
