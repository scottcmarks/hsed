{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Control.If
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

if' function, and in the form of the (?) operator

-}

module Control.If
  (
    (?)
  , if'
  ) where

import           GHC.Types (Bool)

-- $setup
-- >>> import GHC.Base
-- >>> :set -Wno-type-defaults

-- | @if'@ function as operator @(?)@
--
-- Since @infixr 0 $@, we can use @ ... ? ... $ ... @ like @ ... ?... : ... @ in C-family languages.
--
-- >>> 1 == 2 ? 3 $ 4
-- 4
-- >>> 'a' == 'a' ? "Yes!" $ "no?"
-- "Yes!"
(?) :: Bool -> p -> p -> p
(?) = if'
infixr 1 ?


-- | Lazy @if'@ function
if' :: Bool -> p -> p -> p
if' p t f = if p then t else f
