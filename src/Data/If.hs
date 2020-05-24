{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Data.If
Description : ? operator
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

if' in the form of the (?) operator

-}

module Data.If ((?)) where

import           GHC.Types (Bool)

-- | @if'@ function as operator @(?)@
--
-- Since @infixr 0 $@, we can use @?@..@$@ like @?@..@:@ in C-family languages.
--
(?) :: Bool -> p -> p -> p
(?) p t f = if p then t else f
infixr 1 ?
