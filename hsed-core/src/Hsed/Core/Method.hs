{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

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
  each type as the spec names it, 'Hsed.Core.TypeName.TypeName'), the
  record's parameters as 'Value's, required ones in order and optional ones
  by their 0-based position, and the record from such values ('fromValues');
* 'Method': its UID, joined by name from the MethodID UIDs or the Session
  Manager Method UIDs. A method the spec gives a signature and no UID has no
  'Method' instance.

'parameters' and 'invocation' build the codec's 'Parameters' and
'MethodCall': required parameters positional, optional ones Named by their
0-based index among the signature's optional parameters (3.2.4.1 orders and
numbers them, 3.2.4.2 encodes the Named value), so an invocation cannot name
an optional parameter wrongly; 'invocationWith' gives the invocation another
status than 0 (a Session Manager's failed answer carries its error status,
3.2.4.2, PDF 49). 'responseParameters' builds the parameters of an answer
given as an invocation (Properties', 5.2.2.1).

The other direction, a TPer's, is 'positional': the parameters of a call
read against a signature, the required values counted, each optional one
found by its index, in order, a 'ParamError' where they are not what 3.2.4.1
says (INVALID_PARAMETER, 5.1.5.11); 'decodeParameters' makes the record of
them, and 'fromCall' takes the invocation of a record's method and no
other. 'namedComponents' does the same for the Named components of a grouped
abstract type, which 5.1.4.1 (PDF 128) numbers as 3.2.4.1 numbers optional
parameters. The generated @methods :: ['SomeMethod']@ lists every record with
a UID, so that a TPer finds the record of a call by its MethodID
('methodWithUID') and reads the call with it.

The fields are untyped ('Value'): the Type objects a parameter's type names
are 'Hsed.Core.Types.TypeDef' values, not yet Haskell types, so a type is
its name here, with the Type object's UID where the spec compiler joined one,
and the result types are not yet at the type level.

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
  , invocationWith
  , responseParameters
    -- * Reading parameters
  , ParamError (..)
  , paramErrorText
  , positional
  , namedComponents
  , decodeParameters
  , fromCall
    -- * The methods
  , SomeMethod (..)
  , someMethodUID
  , someSignature
  , methodWithUID
    -- * Re-exported for the generated records
  , Value
  ) where

import           RIO
import qualified RIO.List           as L

import           Hsed.Core.Call     (MethodCall (..), MethodHeader (..), Parameters (..),
                                     optionalParameter)
import           Hsed.Core.Token    (Token (..))
import           Hsed.Core.TypeName (TypeName)
import           Hsed.Core.UID      (UID)
import           Hsed.Core.Value    (Datum (..), Name (..), NamedValue (..), Value (..))


-- | One parameter or result as a signature prints it: @Cellblock : cell_block@
--   is @Param "Cellblock" (TypeName "cell_block" Nothing)@, and @HostSessionID
--   : uinteger@ names the type row @uinteger@'s UID.
data Param = Param
  { paramName :: !Text
  , paramType :: !TypeName   -- ^ the type as printed, spacing collapsed, with its Type object's UID when it is a type row's name
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

-- | A generated method record: its signature, its fields as the parameters
--   of an invocation, and the record from such parameters. Use 'signature'
--   with a type application, @signature \@Get@.
class MethodSignature m where
  signature :: Signature
  -- | The required parameters, in the signature's order.
  requiredValues :: m -> [Value]
  -- | The optional parameters, one per optional parameter of the signature, in its order.
  optionalValues :: m -> [Maybe Value]
  -- | The record from its required values and its optional ones, each in its
  --   place: Nothing unless there are as many of each as the signature has.
  fromValues :: [Value] -> [Maybe Value] -> Maybe m

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
invocation = invocationWith 0

-- | An invocation of a method on an invoking ID, with a status: the first
--   value of its status list (3.2.4.2 item 5, PDF 49), 0 for a call the host
--   expects to complete, an error status in the answer a Session Manager
--   gives a call that failed (a SyncSession carrying NOT_AUTHORIZED).
invocationWith :: forall m. Method m => Natural -> UID -> m -> MethodCall
invocationWith status invoker m = MethodCall (MethodHeader invoker (methodUID @m)) (parameters m) status

-- | The parameters of an answer given as an invocation (Properties',
--   5.2.2.1: @=> SMUID.Properties [ Properties : …, HostProperties = … ]@),
--   from the response's required and optional parameters ('Invocation's)
--   and the values for them: the required values as given, as many as the
--   parameters, each optional value given Named by its index among the
--   optional parameters, as 'parameters' names a record's. 'positional'
--   over the same parameters reads them back.
responseParameters :: [Param] -> [Param] -> [Value] -> [Maybe Value] -> Parameters
responseParameters reqs opts rs os =
  Parameters (zipWith const rs reqs) [ optionalParameter i v | (i, _, Just v) <- L.zip3 [0 ..] opts os ]


-- * Reading parameters

-- | Why a call's parameters are not a signature's (INVALID_PARAMETER, Core
--   5.1.5.11 d-e, PDF 139), or a value's components not an abstract type's.
data ParamError
  = RequiredCount !Int !Int
    -- ^ this many required parameters expected, this many given (3.2.4.1: they come first, positional)
  | OptionalNotIndexed !Name
    -- ^ an optional parameter, or a component, Named by other than a uinteger (3.2.4.1 a; 5.1.4.1 a)
  | OptionalIndex !Natural !Int
    -- ^ a Name past the optional parameters, or the components: the name, and how many there are
  | OptionalOrder !Natural !Natural
    -- ^ a Name not above the one before it: they appear in the order defined (3.2.4.1; 5.1.4.1 b)
  | WrongShape !Text
    -- ^ a value that is not what the type asks: a component that is no Named value, a list where a datum is
  deriving (Eq, Show)

-- | A parameter error said in words, for a transcript or a report.
paramErrorText :: ParamError -> Text
paramErrorText = \case
  RequiredCount expected given ->
    tshow given <> " required parameters where the signature has " <> tshow expected <> " (Core 3.2.4.1, PDF 47)"
  OptionalNotIndexed nm ->
    "a Named value named " <> tshow nm <> ", not by a uinteger index (Core 3.2.4.1, PDF 47-48; 5.1.4.1, PDF 128)"
  OptionalIndex i count ->
    "a Named value indexed " <> tshow i <> " where there are " <> tshow count <> ", 0 first (Core 3.2.4.1, PDF 47-48)"
  OptionalOrder before i ->
    "a Named value indexed " <> tshow i <> " after one indexed " <> tshow before
      <> ": they appear in the order defined, each once (Core 3.2.4.1, PDF 48; 5.1.4.1, PDF 128)"
  WrongShape why -> why

-- | A call's parameters read against a signature's required and optional
--   parameters (Core 3.2.4.1, PDF 47-48): the required values, as many as
--   the signature has, and the optional ones each in its place, found by
--   the index its Name gives, in ascending order; or why they are not the
--   signature's.
positional :: [Param] -> [Param] -> Parameters -> Either ParamError ([Value], [Maybe Value])
positional reqs opts (Parameters rs named)
  | length rs /= length reqs = Left (RequiredCount (length reqs) (length rs))
  | otherwise                = (\os -> (rs, os)) <$> namedComponents (length opts) (map N named)

-- | The Named components of a grouped abstract type (Core 5.1.4.1, PDF 128:
--   each a Named value whose name is its 0-based position among the type's
--   components, given in order, any left out), placed: one slot per
--   component, this many, the value given for it or Nothing; or why the
--   values are not such components. The optional parameters of an
--   invocation are read the same way ('positional').
namedComponents :: Int -> [Value] -> Either ParamError [Maybe Value]
namedComponents count vs = do
  given <- traverse component vs
  traverse_ (\(a, b) -> if b > a then Right () else Left (OptionalOrder a b)) (zip (map fst given) (drop 1 (map fst given)))
  pure [ L.lookup i given | i <- take count [0 ..] ]
  where
    component = \case
      N (NamedValue (Name (Datum (Unsigned i))) v)
        | i < fromIntegral count -> Right (i, v)
        | otherwise              -> Left (OptionalIndex i count)
      N (NamedValue nm _) -> Left (OptionalNotIndexed nm)
      v -> Left (WrongShape ("a component that is no Named value: " <> tshow v <> " (Core 5.1.4.1, PDF 128)"))

-- | A record from a call's parameters, read against its signature
--   ('positional'), or why they are not its method's.
decodeParameters :: forall m. MethodSignature m => Parameters -> Either ParamError m
decodeParameters ps = do
  (rs, os) <- positional (sigRequired sg) (sigOptional sg) ps
  maybe (Left (WrongShape (sigMethod sg <> "'s record takes no values of its signature's arity"))) Right (fromValues rs os)
  where sg = signature @m

-- | A record from an invocation of its method: Nothing for an invocation of
--   another method (by its MethodID), else the record or why the parameters
--   are not its. Use it with a type application, @fromCall \@SyncSession@.
fromCall :: forall m. Method m => MethodCall -> Maybe (Either ParamError m)
fromCall (MethodCall (MethodHeader _ method) ps _)
  | method == methodUID @m = Just (decodeParameters ps)
  | otherwise              = Nothing


-- * The methods

-- | A method record with a UID, its type hidden: an entry of the generated
--   list of methods, which names each record by a proxy of its type.
data SomeMethod = forall m. (Method m, Eq m, Show m) => SomeMethod (Proxy m)

-- | The method's UID.
someMethodUID :: SomeMethod -> UID
someMethodUID (SomeMethod (_ :: Proxy m)) = methodUID @m

-- | The method's signature.
someSignature :: SomeMethod -> Signature
someSignature (SomeMethod (_ :: Proxy m)) = signature @m

-- | The method a MethodID names, among the given.
methodWithUID :: [SomeMethod] -> UID -> Maybe SomeMethod
methodWithUID ms u = L.find ((== u) . someMethodUID) ms
