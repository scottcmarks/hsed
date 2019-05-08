\documentstyle{article}
\begin{document}
\chapter{App}

Define the types used in SEDs, and in the hsed program in particular.


\begin{code}
{-|
Module      : Hsed.App
Description : hsed types, field lenses, field types
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for @hsed@ main.
Constructors and lenses.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hsed.App
where

import           Control.Lens
import           RIO
import           RIO.Process

-- | Command line arguments
data Options = Options
  { _verbose :: !Bool
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


\end{code}
