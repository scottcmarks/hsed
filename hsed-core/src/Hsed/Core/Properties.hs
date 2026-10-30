{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Core.Properties
Description : The runtime form of the Properties method's properties and the initial assumptions (Core 5.2.2)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What the spec compiler generates from the tables @propsNames@, @propsInitial@
and @propsReq@ comments bind (Core Table 167, the properties the Properties
method reports; Core Table 168, what host and TPer assume of each other until
a Properties exchange has succeeded, Core 5.2.2.4; Opal Table 17, what the
SSC requires of each property, M4a): each 'Property' with its Type object
("Hsed.Core.TypeName") and the communicators it describes, each
communicator's 'Assumptions' about the other, and each property's
'PropertyRequirement', what is required of the TPer's value of it and of the
host's. A value is the one the Properties method carries: a uinteger as
itself, a boolean as its enumeration value.
-}
module Hsed.Core.Properties
  ( Communicator (..)
  , Property (..)
  , Assumptions (..)
  , assumption
    -- * What an SSC requires of a property (Opal Table 17)
  , Requirement (..)
  , TPerRequirement (..)
  , HostRequirement (..)
  , PropertyRequirement (..)
  , requirement
  ) where

import           RIO

import           Hsed.Core.TypeName (TypeName)


-- | The two ends of a ComID's communications.
data Communicator = Host | TPer
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One property of the Properties method.
data Property = Property
  { propertyName :: !Text            -- ^ as the table prints it, the name it is sent under
  , propertyType :: !TypeName        -- ^ its Type object, by name and UID
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


-- * What an SSC requires of a property

-- | Whether a communicator's property is required: Opal Table 17's @(M)@
--   (mandatory), @(O)@ (optional) and @(N)@ (a legal property the TPer need
--   not use).
data Requirement = Mandatory | Optional | NotRequired
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | What is required of the TPer's value of a property: whether it reports
--   it, and the least value it may report (Nothing where the table says VU).
data TPerRequirement = TPerRequirement
  { tprRequired :: !Requirement
  , tprMinimum  :: !(Maybe Natural)
  }
  deriving (Eq, Show)

-- | What is required of the host's value of a property: whether the TPer
--   accepts it, what the TPer assumes of it until a Properties exchange has
--   succeeded, and the least and greatest values it accepts (each Nothing
--   where the table says VU, or gives none: a property the TPer need not use).
data HostRequirement = HostRequirement
  { hrRequired :: !Requirement
  , hrInitial  :: !(Maybe Natural)
  , hrMinimum  :: !(Maybe Natural)
  , hrMaximum  :: !(Maybe Natural)
  }
  deriving (Eq, Show)

-- | One row of an SSC's property requirements: the property, by the name the
--   Properties table spells it, and what is required of the TPer's value of it
--   and of the host's (Nothing where the property is not that communicator's).
data PropertyRequirement = PropertyRequirement
  { reqName :: !Text
  , reqTPer :: !(Maybe TPerRequirement)
  , reqHost :: !(Maybe HostRequirement)
  }
  deriving (Eq, Show)

-- | A property's requirements, by its name.
requirement :: Text -> [PropertyRequirement] -> Maybe PropertyRequirement
requirement name = listToMaybe . filter ((== name) . reqName)
