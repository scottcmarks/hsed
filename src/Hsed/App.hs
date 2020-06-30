
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Hsed.App
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for @hsed@ main.
Constructors and lenses.
-}

module Hsed.App
where

import           Control.Lens
import           RIO
import           RIO.Process

-- | Command line arguments
newtype Options = Options
  { _verbose :: Bool
  } deriving (Show)
makeLenses ''Options

-- | Global command environment
data App = App
  { _logFunc        :: !LogFunc
  , _processContext :: !ProcessContext
  , _options        :: !Options
  }

makeLenses ''App
makeApp :: LogFunc -> ProcessContext -> Options -> App
makeApp = App

instance HasLogFunc App where logFuncL = logFunc
