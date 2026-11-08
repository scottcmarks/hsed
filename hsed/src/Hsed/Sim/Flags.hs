{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Flags
Description : The simulator's flags read and named, for both executables: units, the Locking SP's OFS, the SID's PIN
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What hsed-jig's simulator flags and hsed-sim's (design 7 item 5, 11 M5c)
read, apart from the command lines that read them: @--units@ ('parseUnits',
'unitName'), @--locking-ofs@ ('parseLockingOFS', 'lockingOFSName') and
@--sim-sid-pin@ ('parseSidPin'), each a pure reader giving the value or why
not, as optparse-applicative's @eitherReader@ takes it (the library uses no
optparse). Moved at M5c from "Hsed.Jig.Options", which re-exports the last
three, and from hsed-jig's Main, so that hsed-sim reads them as the jig does
and imports no module of the jig ("Hsed.CapabilitySpec" walks its imports).
-}
module Hsed.Sim.Flags
  ( -- * @--units@
    parseUnits
  , unitName
    -- * @--locking-ofs@
  , parseLockingOFS
  , lockingOFSName
    -- * @--sim-sid-pin@
  , parseSidPin
  ) where

import qualified Data.ByteString.Base16 as B16
import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO
import qualified RIO.Text               as T

import           Hsed.Sim.Params        (LifeCycle (..))
import           Hsed.Wire              (Unit (..))


-- $setup
-- >>> :set -XOverloadedStrings

-- * @--units@

-- | @--units bytes|blocks512@: what the simulator's transfer lengths count
--   (design 6.2), or why not.
--
-- >>> map parseUnits ["bytes", "blocks512"]
-- [Right UnitBytes,Right UnitBlocks512]
-- >>> parseUnits "blocks"
-- Left "--units blocks: bytes or blocks512"
parseUnits :: String -> Either String Unit
parseUnits = \case
  "bytes"     -> Right UnitBytes
  "blocks512" -> Right UnitBlocks512
  u           -> Left ("--units " <> u <> ": bytes or blocks512")

-- | A unit as @--units@ names it.
--
-- >>> map unitName [minBound .. maxBound]
-- ["blocks512","bytes"]
unitName :: Unit -> Text
unitName = \case
  UnitBytes     -> "bytes"
  UnitBlocks512 -> "blocks512"


-- * @--locking-ofs@

-- | @--locking-ofs manufactured-inactive|manufactured@: the simulated
--   Locking SP's Original Factory State ("Hsed.Sim.Params".@spLockingOFS@),
--   or why not. Manufactured-Inactive is the default and the one Opal 2.30
--   5.2.2 says a Manufactured Locking SP SHALL have (PDF 96); Manufactured,
--   which its informative comment allows, is outside that SHALL, and the
--   reports say so ("Hsed.Jig.Report").
--
-- >>> map parseLockingOFS ["manufactured-inactive", "manufactured"]
-- [Right ManufacturedInactive,Right Manufactured]
-- >>> parseLockingOFS "inactive"
-- Left "--locking-ofs inactive: manufactured-inactive or manufactured"
parseLockingOFS :: String -> Either String LifeCycle
parseLockingOFS s = maybe (Left ("--locking-ofs " <> s <> ": manufactured-inactive or manufactured")) Right
  (L.lookup s [ (T.unpack (lockingOFSName lc), lc) | lc <- [minBound .. maxBound] ])

-- | An Original Factory State as @--locking-ofs@ and the reports name it.
--
-- >>> map lockingOFSName [minBound .. maxBound]
-- ["manufactured-inactive","manufactured"]
lockingOFSName :: LifeCycle -> Text
lockingOFSName = \case
  ManufacturedInactive -> "manufactured-inactive"
  Manufactured         -> "manufactured"


-- * @--sim-sid-pin@

-- | @--sim-sid-pin HEX@: the simulator's C_PIN_SID PIN as hex digits, two a
--   byte, an optional @0x@ before them, 4 to 32 bytes, or why not: at most
--   the PIN column's (Core Table 122, password, PDF 118), and at least
--   four, since the reports mask the PIN in their every text as well as in
--   the transcript's bytes ("Hsed.Jig.Transcript".'Hsed.Jig.Transcript.maskText', design 9.3
--   item 5) and a shorter one would be masked out of their own words (a
--   3-byte @SID@ masks @C_PIN_SID@ and @MSID@, a 1-byte @1@ every 1 of a
--   page number). The bytes go to "Hsed.Sim.Params".@spSidPin@, so that
--   the simulated TPer's Initial C_PIN_SID PIN Indicator is other than the
--   MSID's and UCT-03's step 1 does not apply (Test Cases PDF 33); a set
--   SID PIN also fails UCT-01's step 4c.ii, SID Value State 1 (PDF 30),
--   the Test Cases taking the drive at its original factory state (4.2,
--   PDF 28). A PIN of four bytes or more that is itself a word of the
--   reports' prose (@Core@, @Opal@, @MSID@) is still masked out of it
--   wherever it stands, the user's choice; the floor is the jig's own rule,
--   reversible (Core Table 122 allows 0 to 32 bytes), and the MSID a drive
--   reports passes through the same masking with no floor (M4c).
--
-- >>> parseSidPin "53494421"
-- Right "SID!"
-- >>> parseSidPin "0x53494421"
-- Right "SID!"
-- >>> parseSidPin "534944"
-- Left "--sim-sid-pin 534944: 3 bytes; at least 4, so that masking the PIN in the reports' texts rewrites none of their short words or page numbers (design 9.3 item 5)"
-- >>> parseSidPin "31"
-- Left "--sim-sid-pin 31: 1 byte; at least 4, so that masking the PIN in the reports' texts rewrites none of their short words or page numbers (design 9.3 item 5)"
-- >>> parseSidPin "5349442"
-- Left "--sim-sid-pin 5349442: hex digits, two a byte (an even count)"
-- >>> parseSidPin "zz"
-- Left "--sim-sid-pin zz: hex digits, two a byte (an even count)"
parseSidPin :: String -> Either String ByteString
parseSidPin s = case B16.decode (fromString digits) of
  Right bytes
    | B.null bytes           -> Left (prefix <> "no bytes")
    | B.length bytes < least -> Left (prefix <> bytesWord (B.length bytes) <> "; at least " <> show least
                                      <> ", so that masking the PIN in the reports' texts rewrites none of their short words or page numbers (design 9.3 item 5)")
    | B.length bytes > most  -> Left (prefix <> "more than " <> show most <> " bytes, the PIN column's most (Core Table 122, PDF 118)")
    | otherwise              -> Right bytes
  Left _ -> Left (prefix <> "hex digits, two a byte (an even count)")
  where
    digits = fromMaybe s (L.stripPrefix "0x" s <|> L.stripPrefix "0X" s)
    prefix = "--sim-sid-pin " <> s <> ": "
    -- the size limitation of the password type (Core 5.1.3.63, Table 122, PDF 118: max_bytes, 32)
    most = 32 :: Int
    -- the jig's own floor: the PIN is masked wherever the reports' texts have it (design 9.3 item 5), and one under
    -- four bytes is a word or a number of theirs
    least = 4 :: Int
    bytesWord n = show n <> (if n == 1 then " byte" else " bytes")
