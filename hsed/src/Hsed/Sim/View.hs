{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Module      : Hsed.Sim.View
Description : The simulator seen from outside: eight typed queries of its state, its control, its setup (M5c, Option A)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What the jig reads of the simulator besides its answers on the wire (design
9.2, 11 M5c, Option A: the simulator's peek and release as control ops), as
a closed set of typed queries ('SimQuery'), each answered from the TPer's
state by 'answer', whose bodies are the jig's own readers as M5b had them,
moved here verbatim:

* 'ComIds': each ComID with its life and its synchronous state, which
  @establish@'s SyncIdle and NoSessions read ("Hsed.Jig.Run", the ComIDs
  not in S1 drained, the ComIDs a session holds reset) and SPF-02's
  extension reads of the base ComID;
* 'Sessions': the sessions open, by ComID and TSN (ETC-20, ETC-21);
* 'LifeCycleOf': an SP's life cycle, its LifeCycleState cell
  ("Hsed.Sim.State".'spLifeCycle'), which LockingManufactured and
  LockingManufacturedInactive read;
* 'CredentialOf': an authority's credential, its C_PIN object's PIN,
  TryLimit and Tries, which every session as an authority reads its PIN
  from ("Hsed.Jig.Script".@asAuthority@, CredentialsKnown, ETC-21);
* 'CellOf': a committed cell by SP, table, row and column name (SPF-11's
  User1 Enabled);
* 'ByteTableOf': a byte table (SPF-02's and ETC-14's DataStore);
* 'Msid': the MSID the simulator was given (UCT-03's extension);
* 'LockingRanges': the Locking table's rows' UIDs (SPF-12).

Seen from outside, the simulator is a 'SimView', which answers a query or
says why not, and a 'Control', which releases a ComID's prepared response
or says why not: in process, straight from the state
("Hsed.Transport.InProc".'Hsed.Transport.InProc.inProcView', never refused);
over the LAN leg, as the hub's control ops @peek@ and @release@, behind
hsed-sim's @--allow-remote-control@ (design 7 item 5). Its 'SimSetup' is what
a hub publishes of how it was started, so that a run whose simulator flags
are its own can be held to them ('setupDifferences'): the unit, the hold,
the Locking SP's Original Factory State, the inactive ComIDs' discard, the
SID's PIN and the MSID by their length and digest alone, the profile's
digest, and a digest of every other parameter. The module imports nothing
of the jig: hsed-sim serves it ("Hsed.CapabilitySpec").
-}
module Hsed.Sim.View
  ( -- * Queries
    ComIdView (..)
  , Credential (..)
  , SimQuery (..)
  , SomeQuery (..)
  , answer
    -- * The simulator seen from outside
  , SimView (..)
  , Control (..)
    -- * Its setup
  , SimSetup (..)
  , setupOf
  , setupDifferences
  ) where

import           Prelude                  (Show (showsPrec), showParen, showString)
import qualified RIO.ByteString           as B
import qualified RIO.Map                  as Map
import           RIO
import qualified RIO.Text                 as T

import           Hsed.Core.Table          (Cell (..), TableDesc (..))
import           Hsed.Core.UID            (UID)
import           Hsed.Gen.Core.TableDescs (authorityTableDesc, c_PINTableDesc)
import           Hsed.Sha256              (sha256Hex)
import           Hsed.Sim.Flags           (lockingOFSName, unitName)
import           Hsed.Sim.State           (cellNamedIn, cellOf, columnOf, lifeOf, lockingRows, rowWithUID, spLifeCycle)
import           Hsed.Sim.Types           (ByteTable, ComIdLife, ComIdState (..), LifeCycle, Level0Profile (..), SimParams (..),
                                           SPState (..), SyncState, TPerState (..))
import           Hsed.Wire                (Unit)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
-- >>> import Hsed.Sim.Types (defaultSimParams, initialState)

-- * Queries

-- | A ComID as the TPer holds it: its life (Core 3.3.3, PDF 56, Associated
--   while a session is open on it, "Hsed.Sim.State".'lifeOf') and its
--   synchronous protocol state (3.3.10.4, PDF 84).
data ComIdView = ComIdView
  { cvComId :: !Word16
  , cvLife  :: !(Maybe ComIdLife)
  , cvSync  :: !SyncState
  }
  deriving (Eq, Show)

-- | An authority's credential as an SP of the simulator holds it: its
--   C_PIN object's PIN, TryLimit and Tries (Core 5.3.4.1.1.2, PDF 210).
data Credential = Credential
  { crPIN      :: !ByteString
  , crTryLimit :: !Natural
  , crTries    :: !Natural
  }
  deriving (Eq, Show)

-- | What the jig reads of the simulator's state, each query with the type
--   of its answer.
data SimQuery a where
  -- | Every ComID the TPer has, in order.
  ComIds        :: SimQuery [ComIdView]
  -- | The sessions open, by ComID and TSN, in order.
  Sessions      :: SimQuery [(Word16, Word32)]
  -- | An SP's life cycle, the value of its LifeCycleState cell (Core 4.1, PDF
  --   91; Opal 2.30 Table 49, PDF 98), by the SP's UID.
  LifeCycleOf   :: !UID -> SimQuery (Maybe Int)
  -- | An authority's credential, by the SP's UID and the authority's.
  CredentialOf  :: !UID -> !UID -> SimQuery (Maybe Credential)
  -- | A committed cell, by the SP's, the table's and the row's UIDs and the
  --   column's name.
  CellOf        :: !UID -> !UID -> !UID -> !Text -> SimQuery (Maybe Cell)
  -- | A byte table, by the SP's UID and the table's.
  ByteTableOf   :: !UID -> !UID -> SimQuery (Maybe ByteTable)
  -- | C_PIN_MSID's PIN as the simulator was given it.
  Msid          :: SimQuery ByteString
  -- | The UIDs of the Locking SP's Locking table's rows, the global range
  --   first.
  LockingRanges :: SimQuery [UID]

deriving instance Eq (SimQuery a)
deriving instance Show (SimQuery a)

-- | A query, whatever its answer's type: what a peek op carries.
data SomeQuery where
  SomeQuery :: SimQuery a -> SomeQuery

instance Eq SomeQuery where
  SomeQuery a == SomeQuery b = sameQuery a b

instance Show SomeQuery where
  showsPrec d (SomeQuery q) = showParen (d > 10) (showString "SomeQuery " . showsPrec 11 q)

-- | Whether two queries of any answer types are one: the same constructor
--   with the same arguments.
sameQuery :: SimQuery a -> SimQuery b -> Bool
sameQuery a b = case (a, b) of
  (ComIds, ComIds)                               -> True
  (Sessions, Sessions)                           -> True
  (LifeCycleOf s, LifeCycleOf s')                -> s == s'
  (CredentialOf s u, CredentialOf s' u')         -> (s, u) == (s', u')
  (CellOf s t r n, CellOf s' t' r' n')           -> (s, t, r, n) == (s', t', r', n')
  (ByteTableOf s t, ByteTableOf s' t')           -> (s, t) == (s', t')
  (Msid, Msid)                                   -> True
  (LockingRanges, LockingRanges)                 -> True
  _                                              -> False

-- | A query's answer from the TPer's state: the readers the jig had at M5b
--   (Run.hs:890-914, Script.hs:1187-1191 and :1495-1506, SPF02.hs:131 and
--   :233, ETC14.hs:167, ETC20.hs:63, ETC21.hs:150, SPF12.hs:314, UCT03.hs:195,
--   as of be239b5), moved here verbatim. A credential is the SP's Authority
--   row of the authority's UID, its Credential a reference to a C_PIN
--   object of the SP (the Admin SP's, Opal 2.30 Table 24, PDF 48; the
--   Locking SP's, Table 40, PDF 82-83), that object's PIN, TryLimit and
--   Tries (Tables 25 and 41, PDF 48-49 and 84; a limit the row does not
--   give is 0), the columns found by name in the generated descriptions;
--   none for an authority the SP lacks or one with no credential. A cell is
--   found in the description the SP holds ("Hsed.Sim.State".'cellNamedIn');
--   none where the state holds no such SP, row or cell.
--
-- >>> answer (LifeCycleOf uSP_Locking) (initialState defaultSimParams)
-- Just 8
answer :: SimQuery a -> TPerState -> a
answer q st = case q of
  ComIds                   -> [ ComIdView comid (lifeOf comid st) (cSync s) | (comid, s) <- Map.toList (tsComIds st) ]
  Sessions                 -> Map.keys (tsSessions st)
  LifeCycleOf sp           -> spLifeCycle sp st
  CredentialOf sp authority -> do
    authorityRow <- rowWithUID sp (tdTableUID authorityTableDesc) authority st
    RefCell _ credential <- cellNamed authorityTableDesc "Credential" authorityRow
    pinRow <- rowWithUID sp (tdTableUID c_PINTableDesc) credential st
    BytesCell pin <- cellNamed c_PINTableDesc "PIN" pinRow
    pure (Credential pin (count "TryLimit" pinRow) (count "Tries" pinRow))
  CellOf sp table row name -> do
    sps <- Map.lookup sp (tsSPs st)
    r   <- rowWithUID sp table row st
    cellNamedIn sps table name r
  ByteTableOf sp table     -> Map.lookup sp (tsSPs st) >>= Map.lookup table . spsBytes
  Msid                     -> spMsid (tsParams st)
  LockingRanges            -> [ u | r <- lockingRows st, Just (UIDCell u) <- [cellOf 0 r] ]
  where
    cellNamed desc name row = columnOf desc name >>= (`cellOf` row)
    count name row = case cellNamed c_PINTableDesc name row of
      Just (UIntCell n) | n >= 0 -> fromInteger n
      _                          -> 0


-- * The simulator seen from outside

-- | The simulator's state as queries answer it, or why a query was not
--   answered (a hub that refused it, a connection lost).
newtype SimView = SimView
  { peekView :: forall a. SimQuery a -> IO (Either Text a)
  }

-- | What the simulator does besides commands and events: release a ComID's
--   prepared response now (S2 to S3, "Hsed.Sim.Types".@Tick@), or why not.
newtype Control = Control
  { controlRelease :: Word16 -> IO (Either Text ())
  }


-- * Its setup

-- | How a simulator was started, as a hub publishes it in its hello's
--   @app.sed@: its unit, its hold (the IF-RECVs a prepared response is held
--   for), its Locking SP's Original Factory State, whether it discards a
--   command to an inactive ComID; the SID's PIN (none: the MSID's) and the
--   MSID, each by its length and the first 12 hex digits of its SHA-256
--   alone (a hub publishes them only with @--allow-remote-control@, whose
--   peeks read them anyway); the profile's digest; and the digest of every
--   other parameter, the secrets left out.
data SimSetup = SimSetup
  { ssUnits      :: !Unit
  , ssHold       :: !Int
  , ssLockingOFS :: !LifeCycle
  , ssDiscard    :: !Bool
  , ssSidPin     :: !(Maybe Text)
  , ssMsid       :: !(Maybe Text)
  , ssProfile    :: !(Maybe Text)
  , ssParams     :: !Text
  }
  deriving (Eq, Show)

-- | The setup of a simulator of these parameters and this hold. The
--   profile is digested by its descriptors alone, and the parameters with
--   the profile's file name, the SID's PIN and the MSID left out, so that
--   two runs of one profile read from two paths agree and no digest stands
--   for a secret.
setupOf :: SimParams -> Int -> SimSetup
setupOf p hold = SimSetup
  { ssUnits      = spUnits p
  , ssHold       = hold
  , ssLockingOFS = spLockingOFS p
  , ssDiscard    = spInactiveComIdDiscard p
  , ssSidPin     = secretText <$> spSidPin p
  , ssMsid       = Just (secretText (spMsid p))
  , ssProfile    = digest . lpDescriptors <$> spProfile p
  , ssParams     = digest (p { spSidPin = Nothing, spMsid = mempty, spProfile = (\lp -> lp { lpSource = "" }) <$> spProfile p })
  }
  where
    digest :: Show s => s -> Text
    digest = sha256Hex . encodeUtf8 . tshow
    secretText s = tshow (B.length s) <> " bytes, sha256 " <> T.take 12 (sha256Hex s)

-- | What differs between the setup a run asks for (first) and a hub's
--   (second), each named by its flag: @--units@, @--hold-ifrecvs@,
--   @--locking-ofs@, @--inactive-comid-discard@, @--sim-sid-pin@,
--   @--profile@, and the MSID, which no flag sets; and where none of the
--   flags that are parameters differs but the parameters' digest does,
--   that difference, which only another build or its defaults makes.
--
-- >>> setupDifferences (setupOf defaultSimParams 0) (setupOf defaultSimParams 0)
-- []
-- >>> setupDifferences (setupOf defaultSimParams 0) (setupOf defaultSimParams 1)
-- ["--hold-ifrecvs: 0 asked, 1 on the hub"]
setupDifferences :: SimSetup -> SimSetup -> [Text]
setupDifferences want have = flagged <> [ paramsAlone | not paramFlagDiffers, ssParams want /= ssParams have ]
  where
    named =
      [ (True,  "--units",                  unitName . ssUnits)
      , (False, "--hold-ifrecvs",           tshow . ssHold)
      , (True,  "--locking-ofs",            lockingOFSName . ssLockingOFS)
      , (True,  "--inactive-comid-discard", \s -> if ssDiscard s then "on" else "off")
      , (False, "--sim-sid-pin",            fromMaybe "none (the MSID's)" . ssSidPin)
      , (False, "the MSID (no flag)",       fromMaybe "not published" . ssMsid)
      , (True,  "--profile",                fromMaybe "none" . ssProfile)
      ]
    differing = [ (isParam, flag <> ": " <> f want <> " asked, " <> f have <> " on the hub") | (isParam, flag, f) <- named, f want /= f have ]
    flagged = map snd differing
    paramFlagDiffers = any fst differing
    paramsAlone = "the simulator's other parameters: sha256 " <> T.take 12 (ssParams want) <> " asked, " <> T.take 12 (ssParams have)
                  <> " on the hub: another build or its defaults"
