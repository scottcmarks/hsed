{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Law
Description : A law: a normative sentence or state table of a spec, bound by its comment to hand-written code that holds it
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Where the spec says how the TPer behaves in words, not in a table hsedc
compiles (design 4.3, example E10), the behaviour is hand-written
(@Hsed.Sim.*@) and the sentence that says it is a law: a @law
"Module.name"@ comment on the sentence's paragraph (or a @stateTable@
comment on a table) binds it to a 'Law' of @Hsed.Law.\<Module\>@, which
names what it implements, by the hash of the sentence's text, and says it
as a QuickCheck property over the simulator. The spec compiler generates,
from the comments, the list of what they bind ('LawRef':
@Hsed.Gen.Core.Laws@, design 13.27 (b)), each entry referring to its law by
its qualified name, so that a law the comments name and the code lacks
fails to compile; a hand-written test (@LawsSpec@) holds the two lists to
each other by name and hash, runs every property, and finds every case of
the Test Cases a law names in the jig's catalogue.
-}
module Hsed.Law
  ( Law (..)
  , LawRef (..)
  ) where

import           Prelude            (Show (showsPrec), showParen, showString, shows)
import           RIO
import           Test.QuickCheck    (Property)

import           Hsed.Jig.Catalogue (CaseKey)


-- | A law, as its code states it.
data Law = Law
  { lawName  :: !Text              -- ^ the name its comment binds, @Sync.ifSendWhileBusy@
  , lawHash  :: !Text              -- ^ the first 12 hex digits of the sha256 its paragraph or table has in the IR
  , lawProp  :: !(Maybe Property)  -- ^ what holds, over the simulator; none until it is written
  , lawCases :: ![CaseKey]         -- ^ the Test Cases' cases that observe it
  }

-- | Its property shown as whether it has one.
instance Show Law where
  showsPrec d l = showParen (d > 10) $
      showString "Law {lawName = " . shows (lawName l)
    . showString ", lawHash = " . shows (lawHash l)
    . showString ", lawProp = " . showString (maybe "Nothing" (const "Just <property>") (lawProp l))
    . showString ", lawCases = " . shows (lawCases l)
    . showString "}"

-- | What a law comment binds, as the spec compiler generates it: the
--   anchor, its PDF page (1-based), the hash of its paragraph or table, the
--   name the comment gives, and the law that name refers to.
data LawRef = LawRef
  { lrAnchor :: !Text  -- ^ @core-2.01:3.3.10.2.1#3a@
  , lrPage   :: !Int
  , lrHash   :: !Text
  , lrName   :: !Text
  , lrLaw    :: !Law
  }
  deriving (Show)
