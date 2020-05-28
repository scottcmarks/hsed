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
  , FixedSize
  , MaxSize
  , IsBoundedSizeBytes
  , BoundedSizeBytes
  , FixedSizeBytes
  , MaxSizeBytes
  , BoundedSizeByteString
  , FixedSizeByteString
  , MaxSizeByteString
  , st
  , sz
  , unsafeCreateExp
  , typeFromInt
  )
where

import           Data.BoundedSize.Class (BoundedSizeByteString,
                                         BoundedSizeBytes, FixedSize,
                                         FixedSizeByteString, FixedSizeBytes,
                                         HasSize (..), IsBoundedSize (..),
                                         IsBoundedSizeBytes, IsBytes (..),
                                         MaxSize, MaxSizeByteString,
                                         MaxSizeBytes)
import           Data.BoundedSize.TH    (st, sz, typeFromInt, unsafeCreateExp)
