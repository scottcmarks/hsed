{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Hsed.Core.Method
Description : The runtime form of a method signature (Core 3.2.4, 5.x): what a generated method record is
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The spec compiler turns every method signature block of the spec (@Get@ in
Core 5.3.3.6: @TableUID.Get [ Cellblock : cell_block ] => [ Result : … ]@)
into a record with one field per parameter and two instances (design
docs/spec-as-source.md example E9):

* 'MethodSignature': the signature as printed ('Signature': the invoking IDs
  the heads name, the required and the optional parameters and the response,
  each type as its text), and the record's parameters as 'Value's, required
  ones in order and optional ones by their 0-based position;
* 'Method': its UID, joined by name from the MethodID UIDs or the Session
  Manager Method UIDs. A method the spec gives a signature and no UID has no
  'Method' instance.

'parameters' and 'invocation' build the codec's 'Parameters' and
'MethodCall': required parameters positional, optional ones Named by their
0-based index among the signature's optional parameters (3.2.4.1 orders and
numbers them, 3.2.4.2 encodes the Named value), so an invocation cannot name
an optional parameter wrongly.

The fields are untyped ('Value'): the Type objects a parameter's type names
are 'Hsed.Core.Types.TypeDef' values, not yet Haskell types, so the types
stay text here and the result types are not yet at the type level.

The generated records are named after their methods (@Get@, @Set@, @Next@,
@Random@), names other modules export too (RIO exports Data.Set's @Set@):
import @Hsed.Gen.\<Spec\>.Methods@ qualified wherever such a name is in scope.
-}
module Hsed.Core.Method
  ( -- * Signatures
    Param (..)
  , SigResponse (..)
  , Signature (..)
    -- * Method records
  , MethodSignature (..)
  , Method (..)
  , parameters
  , invocation
    -- * Re-exported for the generated records
  , Value
  ) where

import           RIO

import           System.SED.MCTP.Common.Call  (MethodCall (..), MethodHeader (..), Parameters (..),
                                               optionalParameter)
import           System.SED.MCTP.Common.UID   (UID)
import           System.SED.MCTP.Common.Value (Value)


-- | One parameter or result as a signature prints it: @Cellblock : cell_block@
--   is @Param "Cellblock" "cell_block"@.
data Param = Param
  { paramName :: !Text
  , paramType :: !Text   -- ^ the type as printed, spacing collapsed
  }
  deriving (Eq, Show)

-- | What answers an invocation. (Not @Response@, the name design section 6.1
--   gives the wire's IF-SEND/IF-RECV answer.)
data SigResponse
  = Results ![Param] ![Param]
    -- ^ @=> [ … ]@: the results, the required then the optional ones
  | Invocation !Text ![Param] ![Param]
    -- ^ @=> SMUID.Properties [ … ]@: the TPer answers by invoking a Session
    --   Manager method with these parameters
  | InvocationOf !Text
    -- ^ @=> SMUID.SyncSession [ see … ]@: the TPer answers by invoking that
    --   method, whose own signature gives its parameters
  | NoResponse
    -- ^ no @=>@: nothing answers (CloseSession, and a method that is itself an answer)
  deriving (Eq, Show)

-- | A method signature as the spec prints it.
data Signature = Signature
  { sigMethod   :: !Text          -- ^ @Get@
  , sigInvokers :: ![Text]        -- ^ the invoking IDs its heads name: @TableUID@, @ObjectUID@
  , sigRequired :: ![Param]       -- ^ positional
  , sigOptional :: ![Param]       -- ^ Named by their 0-based index
  , sigResponse :: !SigResponse
  , sigAnswers  :: !(Maybe Text)  -- ^ the method it answers (SyncSession answers StartSession)
  , sigSpecRef  :: !Text          -- ^ @core-2.01:5.3.3.6@
  }
  deriving (Eq, Show)

-- | A generated method record: its signature, and its fields as the
--   parameters of an invocation. Use 'signature' with a type application,
--   @signature \@Get@.
class MethodSignature m where
  signature :: Signature
  -- | The required parameters, in the signature's order.
  requiredValues :: m -> [Value]
  -- | The optional parameters, one per optional parameter of the signature, in its order.
  optionalValues :: m -> [Maybe Value]

-- | A method record whose method has a UID: @methodUID \@Get@.
class MethodSignature m => Method m where
  methodUID :: UID

-- | The parameter list of an invocation: the required parameters, then each
--   optional one given as a Named value whose name is its 0-based index among
--   the signature's optional parameters (3.2.4.1).
parameters :: MethodSignature m => m -> Parameters
parameters m = Parameters (requiredValues m) [ optionalParameter i v | (i, Just v) <- zip [0 ..] (optionalValues m) ]

-- | An invocation of a method on an invoking ID, with status 0.
invocation :: forall m. Method m => UID -> m -> MethodCall
invocation invoker m = MethodCall (MethodHeader invoker (methodUID @m)) (parameters m) 0
