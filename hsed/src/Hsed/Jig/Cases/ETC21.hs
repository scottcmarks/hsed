{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Jig.Cases.ETC21
Description : ETC-21, StartSession with a wrong HostChallenge: as printed, Admin1 to the Locking SP; the Admin SP variant, SID
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-21 (Test Cases 1.02, PDF 92) authenticates Admin1 to a Manufactured
Locking SP with a wrong HostChallenge, which SyncSession answers
NOT_AUTHORIZED (Core 5.1.5.2, PDF 138; 5.3.4.1.10, PDF 217), an abort
accepted (Test Cases 7.1, PDF 76), and no session is left open: the case
as printed, the simulator's alone since M5b (design 11 M5b), its session
needing the Locking SP Manufactured, the common prerequisite of 7.2 (PDF
76). Its variant of the jig's own, @ETC-21/adminsp@ (M4a), does the same to
the Admin SP as SID. Both are one script over what each authenticates
('Authenticating'): the SP, the authority, its C_PIN object, and how the
PIN is varied into a wrong challenge. As printed, the challenge is the
PIN with its last byte changed, of the PIN's own length, so that a PIN at
the password type's 32 bytes (Core Table 122, PDF 118) is varied within
them; the Admin SP variant keeps M4a's PIN plus a byte, which a strict TPer
could refuse as too long (noted for M6's drive runs; design 13.34). The
simulator alone runs either: a drive's policy admits neither authority
(design 9.3), and the PIN comes from the simulator's state
("Hsed.Jig.Script".@simCredential@; the run's CredentialsKnown reads the
same and keeps it among the secrets). The case's own prerequisite is the
C_PIN's TryLimit 0 or Tries below it (prerequisite 1, PDF 92), read from
the state, the case skipped where it does not hold; the jig's extension is
the Tries rule (Core 5.3.4.1.1.2, PDF 210: a failure counts while TryLimit
is not 0, never past it).
-}
module Hsed.Jig.Cases.ETC21
  ( etc21
  , etc21adminsp
  , lastByteChanged
  ) where

import           Data.Bits          (xor)
import qualified RIO.ByteString     as B
import qualified RIO.Map            as Map
import           RIO

import           Hsed.Core.UID      (UID)
import           Hsed.Gen.Core.Codes (pattern NOT_AUTHORIZED)
import           Hsed.Gen.Core.UIDs (uAuthority_SID, uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..), lockingSPUntouched)
import           Hsed.Jig.Script    (Credential (..), Script, StartArgs (..), Verdict (..), exchanged, expectSyncStatus, observe,
                                     peekSim, simCredential, startSession, unmet)
import           Hsed.Sim.Types     (TPerState (..))


-- $setup
-- >>> :set -XOverloadedStrings

-- | The case as printed: Admin1 to the Locking SP, its own prerequisite
--   C_PIN_Admin1's TryLimit 0 or Tries below it (PDF 92). Its session needs
--   the Locking SP Manufactured, the common prerequisite of 7.2 (PDF 76);
--   the simulator alone runs it, its PIN read from the simulator's state
--   (design 9.3).
etc21 :: Case
etc21 = Case
  { cKey        = CaseKey (ETC 21) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedSimOnly]
  , cRun        = run asPrinted
  , cExtensions = [(triesRule, tries asPrinted)]
  }

-- | The Admin SP variant.
etc21adminsp :: Case
etc21adminsp = Case
  { cKey        = CaseKey (ETC 21) Nothing Nothing (Just "adminsp")
  , cSection    = ErrorCases
  , cWaives     = [lockingSPUntouched]
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedSimOnly]
  , cRun        = run adminSPVariant
  , cExtensions = [(triesRule, tries adminSPVariant)]
  }

-- | The extension's name.
triesRule :: Text
triesRule = "the Tries rule: a failure counts while TryLimit is not 0, never past it (Core 5.3.4.1.1.2, PDF 210)"

-- | What the case authenticates: the SP and its name, the authority and its
--   name, its C_PIN object's name, where Opal preconfigures the authority,
--   and how the PIN is varied into a wrong challenge, in bytes and in
--   words.
data Authenticating = Authenticating
  { aSP        :: !UID
  , aSPName    :: !Text
  , aAuthority :: !UID
  , aName      :: !Text
  , aCPin      :: !Text
  , aCited     :: !Text
  , aWrong     :: !(ByteString -> ByteString)
  , aWrongSaid :: !Text
  }

-- | As printed: Admin1 to the Locking SP (Opal 2.30 Table 40, PDF 82; its
--   C_PIN_Admin1, Table 41, PDF 84), the challenge the PIN with its last
--   byte changed.
asPrinted :: Authenticating
asPrinted = Authenticating uSP_Locking "Locking SP" LockingSP.uAuthority_Admin1 "Admin1" "C_PIN_Admin1" "Opal 2.30 Table 40, PDF 82"
                           lastByteChanged "the PIN's last byte changed"

-- | The Admin SP variant: SID to the Admin SP (Opal 2.30 Table 24, PDF 48),
--   the challenge the PIN plus a byte, as M4a sent it.
adminSPVariant :: Authenticating
adminSPVariant = Authenticating uSP_Admin "Admin SP" uAuthority_SID "SID" "C_PIN_SID" "Opal 2.30 Table 24, PDF 48" (<> "!") "the PIN plus '!'"

-- | A challenge other than a PIN, of the PIN's own length: its last byte
--   with its lowest bit flipped, so that a PIN at the password type's 32
--   bytes (Core Table 122, PDF 118) is varied within them; an empty PIN,
--   which nothing of its length differs from, varied to one byte.
--
-- >>> lastByteChanged "MSID"
-- "MSIE"
-- >>> lastByteChanged ""
-- "!"
lastByteChanged :: ByteString -> ByteString
lastByteChanged pin = maybe "!" (\(front, b) -> B.snoc front (b `xor` 0x01)) (B.unsnoc pin)

-- | The case's own prerequisite checked, then step 1: StartSession to the
--   SP as the authority with a wrong challenge, NOT_AUTHORIZED (an abort
--   accepted), and no session left open.
run :: Authenticating -> Script Verdict
run a = peekSim >>= \case
  Nothing -> pure (Skipped ("the simulator's alone: " <> aName a <> "'s PIN is read from its state (design 9.3)"))
  Just st -> do
    cr <- credential a st
    case locked a cr of
      Just why -> pure (Skipped ("prerequisite unmet: " <> why))
      Nothing  -> do
        observe "prerequisite" (aCPin a <> " TryLimit " <> tshow (crTryLimit cr) <> ", Tries " <> tshow (crTries cr)
                                <> " (Test Cases ETC-21 prerequisite 1, PDF 92)")
        answer <- startSession (wrongChallenge a cr) >>= exchanged step1
        expectSyncStatus step1 NOT_AUTHORIZED answer
        after <- peekSim
        for_ after $ \s -> unless (Map.null (tsSessions s)) $
          unmet (tshow (Map.size (tsSessions s)) <> " session(s) open after the failed StartSession, where none should be (Core 5.3.4.1.5, PDF 215)")
        pure Pass
  where step1 = "step 1: StartSession to the " <> aSPName a <> " as " <> aName a <> " with " <> aWrongSaid a

-- | The authority's credential in the simulator's SP.
credential :: Authenticating -> TPerState -> Script Credential
credential a = maybe (unmet ("the simulator's " <> aSPName a <> " holds no credential for " <> aName a <> " (" <> aCited a <> ")")) pure
             . simCredential (aSP a) (aAuthority a)

-- | Why the case's own prerequisite does not hold, if it does not: a
--   TryLimit other than 0 that Tries has reached (Test Cases PDF 92; Core
--   5.3.4.1.1.2, PDF 210).
locked :: Authenticating -> Credential -> Maybe Text
locked a cr
  | crTryLimit cr /= 0 && crTries cr >= crTryLimit cr =
      Just (aCPin a <> "'s Tries " <> tshow (crTries cr) <> " is not below its TryLimit " <> tshow (crTryLimit cr))
  | otherwise = Nothing

-- | StartSession to the SP as the authority with a HostChallenge other than
--   its PIN, read-write.
wrongChallenge :: Authenticating -> Credential -> StartArgs
wrongChallenge a cr = StartArgs (aSP a) (Just (aAuthority a)) (Just (aWrong a (crPIN cr))) True

-- | The extension: Tries before and after a failed StartSession (Core
--   5.3.4.1.1.2, PDF 210): unchanged while TryLimit is 0, one more
--   otherwise, never past TryLimit.
tries :: Authenticating -> Script Verdict
tries a = peekSim >>= \case
  Nothing -> pure (Skipped "the simulator's alone")
  Just before -> do
    cr0 <- credential a before
    case locked a cr0 of
      Just why -> pure (Skipped why)
      Nothing  -> do
        answer <- startSession (wrongChallenge a cr0) >>= exchanged name
        expectSyncStatus name NOT_AUTHORIZED answer
        cr1 <- peekSim >>= maybe (unmet "no simulator state to read Tries from") (credential a)
        let expected
              | crTryLimit cr0 == 0 = crTries cr0
              | otherwise           = min (crTryLimit cr0) (crTries cr0 + 1)
        observe "Tries" (tshow (crTries cr0) <> " before, " <> tshow (crTries cr1) <> " after, TryLimit " <> tshow (crTryLimit cr0))
        unless (crTries cr1 == expected) $
          unmet ("Tries " <> tshow (crTries cr1) <> " after the failure, not " <> tshow expected
                 <> " (Core 5.3.4.1.1.2, PDF 210: a failure counts while TryLimit is not 0, never past it)")
        pure Pass
  where name = "StartSession to the " <> aSPName a <> " as " <> aName a <> " with a wrong HostChallenge"
