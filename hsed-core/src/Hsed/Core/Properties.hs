{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Core.Properties
Description : The runtime form of the Properties method's properties and the initial assumptions (Core 5.2.2)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What the spec compiler generates from the tables @propsNames@ and
@propsInitial@ comments bind (Core Table 167, the properties the Properties
method reports; Core Table 168, what host and TPer assume of each other until
a Properties exchange has succeeded, Core 5.2.2.4): each 'Property' with its
Type object and the communicators it describes, and each communicator's
'Assumptions' about the other. A value is the one the Properties method
carries: a uinteger as itself, a boolean as its enumeration value.
-}
module Hsed.Core.Properties
  ( Communicator (..)
  , Property (..)
  , Assumptions (..)
  , assumption
  ) where

import           RIO

import           System.SED.MCTP.Common.UID (UID)


-- | The two ends of a ComID's communications.
data Communicator = Host | TPer
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One property of the Properties method.
data Property = Property
  { propertyName :: !Text            -- ^ as the table prints it, the name it is sent under
  , propertyType :: !UID             -- ^ its Type object
  , appliesTo    :: ![Communicator]  -- ^ the communicators it describes
  }
  deriving (Eq, Show)

-- | What one communicator assumes of the other's properties.
data Assumptions = Assumptions
  { assumer :: !Communicator
  , about   :: !Communicator
  , assumed :: ![(Text, Natural)]    -- ^ each property's assumed value, in the order printed
  }
  deriving (Eq, Show)

-- | The value a communicator assumes for a property, by the property's name.
assumption :: Text -> Assumptions -> Maybe Natural
assumption name = lookup name . assumed
