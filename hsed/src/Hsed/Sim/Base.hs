{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Sim.Base
Description : The Base Template's methods the simulator runs in a session: Get, with its Fails list
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The behaviours of Core 5.3.3's Basic Table Method Group that M4a runs
(design 8.2, 13.16): 'get', bound by @behaviour "Base.get"@ on 5.3.3.6 (PDF
201), with its Fails list 5.3.3.6.3 bound by @fails "Base.getFails"@, one
'FailItem' per item a to d, each with its status and, once written, the
property that the simulator fails so. The cell_block is read and resolved by
"Hsed.Core.CellBlock" (5.1.4.2.3, PDF 129-130), the columns granted by
"Hsed.Sim.Access", and the RowValues come in column order (5.3.3.6.2.2, PDF
201), the cells the caller may not read left out (5.3.4.2.2, PDF 223).

/At the M4a scaffold/ the names, hashes, cases, labels and statuses are
final and the runs and properties are group G1's (design 11).
-}
module Hsed.Sim.Base
  ( get
  , getFails
  , behaviours
  ) where

import           RIO

import           Hsed.Gen.Core.Codes   (pattern INVALID_PARAMETER, pattern NOT_AUTHORIZED)
import qualified Hsed.Gen.Core.Methods as Methods
import           Hsed.Jig.Catalogue    (CaseKey (..), TestId (..))
import           Hsed.Sim.Behaviour    (Behaviour (..), FailItem (..), Fails (..), SomeBehaviour, forMethod)


-- | Get on an object or a table (Core 5.3.3.6, PDF 201; @behaviour
--   "Base.get"@): the cells its cell_block asks, among those the session's
--   authorities may read, as RowValues in column order. UCT-03 step 1b reads
--   C_PIN_MSID's PIN with it, and ETC-04 sends it on a session the TPer does
--   not have.
get :: Behaviour Methods.Get
get = Behaviour
  { behName  = "Base.get"
  , behHash  = "9d67d740e4cb"
  , behCases = [CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing, CaseKey (ETC 4) Nothing Nothing Nothing]
  , behRun   = error "G1: Base.get"
  }

-- | Get's Fails list (Core 5.3.3.6.3, PDF 201; @fails "Base.getFails"@): a,
--   the object does not exist, NOT_AUTHORIZED; b to d, a cell_block
--   component the context forbids or out of bounds (5.1.4.2.3's rules, PDF
--   129-130), INVALID_PARAMETER (5.1.5.11 e, PDF 139). Each item's property
--   is group G1's.
getFails :: Fails Methods.Get
getFails = Fails
  { failsName  = "Base.getFails"
  , failsItems =
      [ FailItem "a" "fbdda8fb3e3a" NOT_AUTHORIZED Nothing
      , FailItem "b" "ca5584e1ccdf" INVALID_PARAMETER Nothing
      , FailItem "c" "46db34a6034d" INVALID_PARAMETER Nothing
      , FailItem "d" "1b371d07bd3b" INVALID_PARAMETER Nothing
      ]
  }

-- | The Base Template's behaviours a session dispatches to.
behaviours :: [SomeBehaviour]
behaviours = [forMethod get]
