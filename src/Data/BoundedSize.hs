{-# LANGUAGE NoImplicitPrelude #-}
{-|

sized-text combinators are defined for members of 'IsBoundedSize'
class. The package includes 'IsBoundedSize' instances for several
common types.

This module is meant to be imported qualifed, e.g.

> import qualified Data.BoundedSize as S

-}
module Data.BoundedSize
       ( HasSize(..)
       , IsBytes(..)
       , IsBoundedSize(..)
       , IsFixedSize
       , FixedSize
       , IsMaxSize
       , MaxSize
       , IsBoundedSizeBytes
       , IsFixedSizeBytes
       , IsMaxSizeBytes
       , st
       , sz
       , unsafeCreateExp
       , typeFromInt
       )
where

import           Data.BoundedSize.Class (FixedSize, HasSize (..),
                                         IsBoundedSize (..), IsBoundedSizeBytes,
                                         IsBytes (..), IsFixedSize,
                                         IsFixedSizeBytes, IsMaxSize,
                                         IsMaxSizeBytes, MaxSize)
import           Data.BoundedSize.TH    (st, sz, typeFromInt, unsafeCreateExp)
