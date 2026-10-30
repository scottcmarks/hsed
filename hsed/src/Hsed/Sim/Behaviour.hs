{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

{-|
Module      : Hsed.Sim.Behaviour
Description : A behaviour: a method's heading in the spec, bound by its comment to the hand-written code that runs the method
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Where the spec says what a method does in words under its heading (Core
5.2.2.1, Properties; 5.3.3.6, Get), the code that does it is hand-written
(@Hsed.Sim.*@) and the heading is a behaviour: a @behaviour "Module.name"@
comment on the heading binds it to a 'Behaviour' of the method's generated
record ("Hsed.Gen.Core.Methods"), which names what it implements, by the
hash of the heading's signature block, says which of the Test Cases observe
it, and runs the method over the TPer's state (design 4.3, 13.16, milestone
M4a). A method's Fails list (5.3.3.6.3) is bound by a @fails@ comment to a
'Fails': one 'FailItem' per labelled item, its status and, once written, a
QuickCheck property that the simulator fails so.

The spec compiler generates, from the comments, the list of what they bind
('BehaviourRef': @Hsed.Gen.Core.Behaviours@), each entry naming its
behaviour through 'forMethod' at the record's type, so that a behaviour the
comments name and the code lacks, or one of another method's record, fails
to compile; a hand-written test (@BehavioursSpec@) holds the two lists to
each other by name and hash, as @LawsSpec@ holds the laws. The simulator
dispatches a call through 'runSome': the behaviour whose record the call's
MethodID names reads the call's parameters against its signature
("Hsed.Core.Method".'fromCall') and runs.
-}
module Hsed.Sim.Behaviour
  ( -- * A method invoked
    Invoked (..)
  , Reply (..)
    -- * Behaviours and Fails
  , Behaviour (..)
  , FailItem (..)
  , Fails (..)
    -- * The method's type hidden
  , SomeBehaviour (..)
  , SomeFails (..)
  , forMethod
  , forFails
  , someName
  , someHash
  , someUID
  , runSome
    -- * What the comments bind, as generated
  , BehaviourRef (..)
  , FailsRef (..)
  ) where

import           Prelude            (Show (showsPrec), showParen, showString, shows)
import           RIO
import           Test.QuickCheck    (Property)

import           Hsed.Core.Call     (MethodCall)
import           Hsed.Core.Method   (Method (..), ParamError, fromCall)
import           Hsed.Core.UID      (UID)
import           Hsed.Core.Value    (Value)
import           Hsed.Gen.Core.Codes (StatusCode)
import           Hsed.Jig.Catalogue (CaseKey)
import           Hsed.Sim.Types     (Session, TPerState)


-- * A method invoked

-- | What a call arrives with: the ComID, the session it is in (none on the
--   control session), its InvokingID and the host's status, the first value
--   of its status list (Core 3.2.4.2, PDF 49).
data Invoked = Invoked
  { ivComId   :: !Word16
  , ivSession :: !(Maybe Session)
  , ivInvoker :: !UID
  , ivStatus  :: !Natural
  }
  deriving (Eq, Show)

-- | What a behaviour answers: a method's results and status, a Session
--   Manager's response as an invocation (3.2.4.2; 5.2.1, PDF 140), or
--   nothing, the call ignored (3.3.7.1.3, PDF 73-74).
data Reply
  = ReplyResults ![Value] !StatusCode
  | ReplyCall !MethodCall
  | NoReply
  deriving (Eq, Show)


-- * Behaviours and Fails

-- | A behaviour, as its code states it: the name its comment binds
--   (@SessionManager.properties@), the first 12 hex digits of the sha256 of
--   the method's signature block in the IR, the Test Cases' cases that
--   observe it, and what it does with the method's record.
data Behaviour m = Behaviour
  { behName  :: !Text
  , behHash  :: !Text
  , behCases :: ![CaseKey]
  , behRun   :: Invoked -> m -> TPerState -> (Reply, TPerState)
  }

-- | Its run shown as present.
instance Show (Behaviour m) where
  showsPrec d b = showParen (d > 10) $
      showString "Behaviour {behName = " . shows (behName b)
    . showString ", behHash = " . shows (behHash b)
    . showString ", behCases = " . shows (behCases b)
    . showString ", behRun = <run>}"

-- | One item of a method's Fails list: its label as printed (@a@), the first
--   12 hex digits of the item's sha256 in the IR, the status the method
--   fails with, and, once written, the property that the simulator fails so.
data FailItem = FailItem
  { fiLabel  :: !Text
  , fiHash   :: !Text
  , fiStatus :: !StatusCode
  , fiProp   :: !(Maybe Property)
  }

-- | Its property shown as whether it has one.
instance Show FailItem where
  showsPrec d i = showParen (d > 10) $
      showString "FailItem {fiLabel = " . shows (fiLabel i)
    . showString ", fiHash = " . shows (fiHash i)
    . showString ", fiStatus = " . shows (fiStatus i)
    . showString ", fiProp = " . showString (maybe "Nothing" (const "Just <property>") (fiProp i))
    . showString "}"

-- | A method's Fails list, as its code states it: the name its comment
--   binds (@Base.getFails@) and its items in the list's order.
data Fails m = Fails
  { failsName  :: !Text
  , failsItems :: ![FailItem]
  }
  deriving (Show)


-- * The method's type hidden

-- | A behaviour of some method with a UID, its record's type hidden: an
--   entry of a dispatch list.
data SomeBehaviour = forall m. Method m => SomeBehaviour !(Behaviour m)

instance Show SomeBehaviour where
  showsPrec d (SomeBehaviour b) = showParen (d > 10) (showString "SomeBehaviour " . showsPrec 11 b)

-- | A Fails list of some method, its record's type hidden.
data SomeFails = forall m. Method m => SomeFails !(Fails m)

instance Show SomeFails where
  showsPrec d (SomeFails f) = showParen (d > 10) (showString "SomeFails " . showsPrec 11 f)

-- | A behaviour as an entry: the generated list names each behaviour's
--   record with a type application, @forMethod \@Get Base.get@, so that a
--   behaviour of another record fails to compile.
forMethod :: forall m. Method m => Behaviour m -> SomeBehaviour
forMethod = SomeBehaviour

-- | A Fails list as an entry, likewise.
forFails :: forall m. Method m => Fails m -> SomeFails
forFails = SomeFails

-- | The name a behaviour's comment binds.
someName :: SomeBehaviour -> Text
someName (SomeBehaviour b) = behName b

-- | The hash a behaviour states.
someHash :: SomeBehaviour -> Text
someHash (SomeBehaviour b) = behHash b

-- | The UID of the method a behaviour runs.
someUID :: SomeBehaviour -> UID
someUID (SomeBehaviour (_ :: Behaviour m)) = methodUID @m

-- | Run a behaviour on a call, if the call is its method's (by MethodID):
--   the call's parameters read against the record's signature, a
--   'ParamError' where they are not its (INVALID_PARAMETER, Core 5.1.5.11,
--   PDF 139, or ignored on the control session, 3.2.4.2), else the reply and
--   the TPer after it.
runSome :: SomeBehaviour -> Invoked -> MethodCall -> TPerState -> Maybe (Either ParamError (Reply, TPerState))
runSome (SomeBehaviour b) iv mc st = fmap (\m -> behRun b iv m st) <$> fromCall mc


-- * What the comments bind, as generated

-- | What a behaviour comment binds, as the spec compiler generates it: the
--   heading's anchor, its PDF page (1-based), the hash of the heading's
--   signature block, the name the comment gives, the behaviour that name
--   refers to, and its Fails list where a @fails@ comment binds one.
data BehaviourRef = BehaviourRef
  { brAnchor    :: !Text  -- ^ @core-2.01:5.3.3.6@
  , brPage      :: !Int
  , brHash      :: !Text
  , brName      :: !Text
  , brBehaviour :: !SomeBehaviour
  , brFails     :: !(Maybe FailsRef)
  }
  deriving (Show)

-- | What a fails comment binds: the Fails heading's anchor and page, the
--   name the comment gives, each item's label and hash as the IR has them,
--   and the Fails list that name refers to.
data FailsRef = FailsRef
  { frAnchor :: !Text  -- ^ @core-2.01:5.3.3.6.3@
  , frPage   :: !Int
  , frName   :: !Text
  , frItems  :: ![(Text, Text)]
  , frFails  :: !SomeFails
  }
  deriving (Show)
