{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Report
Description : A run's reports (json, md, JUnit xml), and what reads them again: verify-transcript, diff
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What a run writes (design 9.2): @build/report/jig-<target>-<UTC stamp>@ as
JSON (the wire types' own, "Hsed.Wire.Internal"), as Markdown (the
verdicts, each end's beside a pair's joined one, each waiver with its reason
and page, the observations, the decodes, the pair comparison, the run's
problems) and as JUnit
XML, through "Hsed.Jig.Files", the JSON's name claimed exclusively and the
Markdown's and JUnit's following it, so that a run that started in the same
second as another, or together with it, takes the next free suffix (@-2@,
@-3@) and neither writes over the other's. The JUnit report holds a test case for each case and extension, for the
pair, for each problem (a capture, the pair not computed), and one for the
exit code, so that its failures never say less than the exit code does.

Since M4c the reports give what a run that may send to a drive did besides
its cases (design 9.3): each drive's policy as the run ended
("Hsed.Jig.Guard", @Level0Only@, or the base ComID and the methods its
allow list admits), the commands its own transcript holds that the policy
does not admit (exit 3), the commands the Guard refused (blocked in the
transcript), why the run was stopped (exit 130), the pre-flight's notes
(the mounts @--allow-mounted@ let by, "Hsed.Jig.Preflight"), what
@--recover@ did ("Hsed.Jig.Recover"), each end's Properties exchange with
the host's properties (the JSON's alone: UCT-02's observations print them),
and a pair's comparison of them ("Hsed.Jig.PairProperties", judged by Opal
2.30 Table 17). The Markdown prints each in a section of its own where the
run had it (Interrupted, Pre-flight, Recover, Pair: Properties, and
Policies where a drive was an end); the JUnit report has a test case for no
drive command outside its policy and one for no command the Guard refused
wherever a drive was an end, each failed where there was one, one for
@--recover@ where it ran, one for an interruption (failed) where the run
was stopped, and one for a pair's Properties where they were compared; the
exit code's test case names each of them that fails the run, and the
summary prints them all.

And the commands that read a report back without privilege:
@verify-transcript@ (the commands issued, each with its end; a failure if a
drive's end was issued an IF-SEND, or anything but Level 0 Discovery's
IF-RECV, when asked; and with @--allow-list@, each command a drive's end
was issued re-decided by the Guard ('Hsed.Jig.Guard.decide') under that
end's own policy as the report gives it, its base the ComID that end's
Level 0 in the report names, @Level0Only@ where it gives none, a failure
for any it refuses or cannot read, for any command the Guard blocked, and
for a policy bound to another base than its Level 0 names) and @diff@ (two
reports' Level 0 decodes compared as the Pair runner compares them, and,
where both carry them, their Properties exchanges as the Pair's Properties
are). Both read each transcript entry one way ('rereadEntry'), an entry
the report gives to the simulator's end (@tper@) a simulator's only where
one of the report's targets is the simulator.

The target's part of a report's name is @sim@, @dev-disk22@,
@replay-<file>@, or the two ends' joined by @+@ (@sim+dev-disk22@); the stamp
is the run's start, @yyyymmddThhmmssZ@. Each transcript entry is printed
with a decode: a Level 0 response's length and codes for ComID 1, otherwise
the ComPacket, its packets and subpackets, and each subpacket's tokens read
as a method invocation or a method response where they are one; an
IF-RECV's ComPacket header also as the row of Core Table 45 it is. A waiver
is printed with the item and page of its section's list ("Hsed.Jig.Prereq").

The secrets a run read ('rrSecrets': UCT-03's MSID, a PIN read from the
simulator's state) are masked in every report (design 9.3 item 5): each
occurrence in an entry's bytes becomes 0x2A bytes before the entry is
printed or decoded ("Hsed.Jig.Transcript".'maskSecrets'), each occurrence
in a verdict's reasons, an extension's, an observation, a waiver's reason,
a note or a problem is masked as text, hex or as Haskell shows it
('maskText'; 'printable' is the result as the reports and the summary
print it), and each secret is given by its length and the first 12 hex
digits of its SHA-256 ("Hsed.Jig.Sha256") in the JSON, the Markdown and the
JUnit properties, never by its bytes; @--show-msid@ leaves everything in
clear and says so. A secret is masked wherever its bytes stand, so one
shorter than four bytes (a drive's MSID has no floor) takes the reports'
own words and numbers with it: the reports and the summary say so beside
it, and its JSON entry is marked @short@.

A subpacket's tokens are decoded as the host reads a TPer's stream
("Hsed.Core.Stream": the atoms under the host's policy, then its returned
items, which read a host's invocations too): one invocation or one method
response as such, anything else as its items in order, with any fault.

@verify-transcript@ and @diff@ read a report as JSON values (there is no
FromJSON for the wire types): the transcript's entries, each command's
request rebuilt from its op, protocol, ComID, length and an IF-SEND's
payload (its hex, trailing zeros left out, padded again to the length);
each end's Level 0 descriptors, their bytes restored from the hex and
length printed; and each end's Properties exchange, its tables read back
by name.
-}
module Hsed.Jig.Report
  ( CaseResult (..)
  , RunResult (..)
  , printable
  , writeReports
  , verifyTranscript
  , verifyTranscriptWith
  , diffReports
  , reportProperties
    -- * What a run prints
  , summary
  , renderTarget
  , exitNumber
  , describeSecret
  ) where

import qualified Data.Aeson             as A
import qualified Data.Aeson.Key         as K
import qualified Data.Aeson.KeyMap      as KM
import qualified Data.Aeson.Types       as A (Parser, parseEither, parseFail)
import qualified Data.ByteString.Base16 as B16
import           Data.Time              (UTCTime, defaultTimeLocale, formatTime)
import qualified RIO.ByteString         as B
import qualified RIO.Char               as C
import           RIO.FilePath           (takeBaseName, (<.>), (</>))
import qualified RIO.List               as L
import           RIO
import qualified RIO.Text               as T

import           Hsed.Core.Call         (MethodCall (..), MethodHeader (..), MethodResponse (..))
import           Hsed.Core.Feature      (Allowance (..))
import           Hsed.Core.Layout       (Content (..))
import           Hsed.Core.Stream       (Atom (..), Fault (..), Returned (..), atoms, hostPolicy, returnedItems)
import           Hsed.Core.UID          (UID)
import           Hsed.Gen.Core.Layouts  (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..), PacketHeader (..))
import           Hsed.Gen.Core.UIDs     (methodIDUIDs, sessionManagerMethodUIDs)
import           Hsed.Gen.Opal.Features (Level0DiscoveryOpalSSCV2Feature (..))
import           Hsed.Jig.Catalogue     (CaseKey (..), CatalogueEntry (..), catalogue, renderCaseKey)
import           Hsed.Jig.Files         (createOwned, writeOwned)
import           Hsed.Jig.Guard         (Allowed (..), Policy (..), decide)
import           Hsed.Jig.Options       (RunOptions (..), TargetSpec (..))
import           Hsed.Jig.Pair          (FieldDiff (..), Judgement (..), PairReport (..), judged, pairFails, pairLevel0)
import           Hsed.Jig.PairProperties (PropDiff (..), PropertiesPair (..), Side (..), pairProperties, propertiesFail, propertiesLines)
import           Hsed.Jig.Prereq        (Prereq, Waiver (..), common, listAt, sectionOf)
import           Hsed.Jig.Script        (PropertiesAnswer (..), Verdict (..), describeStatus, opalV2Of, t45Of, t45RowOf, verdictText)
import           Hsed.Jig.Sha256        (sha256Hex)
import           Hsed.Jig.Transcript    (TEntry (..), maskSecrets, maskText)
import           Hsed.Level0            (DecodedDescriptor (..), Descriptor (..), Level0 (..), decodeDescriptor, describeProblem,
                                         splitLevel0)
import           Hsed.Packet            (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket)
import           Hsed.Sim.Sync          (T45Row (..))
import           Hsed.Sim.Types         (SimParams (..))
import           Hsed.Transport.Drive   (maskSerial)
import           Hsed.Wire              (Describe (..), Dir (..), IfStatus (..), Request (..), Response (..), Unit (..),
                                         comPacketProtocol, hex, level0ComId)


-- $setup
-- >>> :set -XOverloadedStrings

-- | What one case came to: its verdict, what it waived, its extensions' and
--   what it observed (a table a case reads, UCT-02's properties: name and
--   value, reported apart from the verdict; an extension's under the
--   extension's name); on a pair, each end's verdict beside the joined one;
--   and whether its prerequisites were established or checked before it ran.
data CaseResult = CaseResult
  { crKey          :: !CaseKey
  , crVerdict      :: !Verdict
  , crWaivers      :: ![Waiver]
  , crExtensions   :: ![(Text, Verdict)]
  , crObservations :: ![(Text, Text)]
  , crEnds         :: ![(Text, Verdict)]  -- ^ each end's verdict, by the end's name, its transport's role (@tper@, @drive@,
                                          --   @drive-replay@, as the transcript's entries name it); none for a run of one end
  , crEstablished  :: !Bool               -- ^ whether the prerequisites not waived were established or checked on the
                                          --   end (false where the target lacked what the case needs or one was unmet,
                                          --   so that none after it was reached)
  }
  deriving (Eq, Show)

-- | What a run came to.
data RunResult = RunResult
  { rrOptions        :: !RunOptions
  , rrVersion        :: !Text               -- ^ "Hsed.Version"'s
  , rrStarted        :: !UTCTime
  , rrTargets        :: ![Describe]         -- ^ what each transport said it is (two for a pair)
  , rrCases          :: ![CaseResult]
  , rrTranscript     :: ![TEntry]           -- ^ as the wire had it; printed masked ('rrSecrets') unless @--show-msid@
  , rrLevel0         :: ![(Text, Level0)]   -- ^ each end's Level 0 response, by the end's name (its transport's role)
  , rrPair           :: !(Maybe PairReport)
  , rrIfSends        :: !Int                -- ^ the IF-SENDs the Guard passed to a drive's transport, each within its
                                            --   policy but those 'rrOutsidePolicy' names (M4c; none at M3): the drive's
                                            --   own transcript's, a latched kext transport's answers among them, which
                                            --   never reach the kext
  , rrSecrets        :: ![ByteString]       -- ^ the secrets the run read (the MSID, a PIN), each masked in the reports and
                                            --   given by its length and digest alone, unless @--show-msid@
  , rrProblems       :: ![Text]             -- ^ what else failed the run: the capture, a pair not computed
  , rrExit           :: !ExitCode
  , rrPolicies       :: ![(Text, Policy)]   -- ^ each drive's policy as the run ended, by the end's name ("Hsed.Jig.Guard")
  , rrOutsidePolicy  :: ![(Text, Request)]  -- ^ the commands a drive's own transcript holds that its policy does not admit,
                                            --   by the end's name: a safety violation (design 9.3)
  , rrBlocked        :: !Int                -- ^ the commands the Guard refused, each Blocked in the transcript
  , rrInterrupted    :: !(Maybe Text)       -- ^ why the run stopped short, if it was stopped (a signal, @--interrupt-in-session@)
  , rrPreflight      :: ![Text]             -- ^ what a drive's pre-flight noted (the mounts @--allow-mounted@ let by)
  , rrRecover        :: !(Maybe (Either Text Text))
                                            -- ^ what @--recover@ did, or why it did not recover; none without it
  , rrProperties     :: ![(Text, [(Text, Natural)], PropertiesAnswer)]
                                            -- ^ each end's Properties with the host's properties (UCT-02's), by the end's
                                            --   name: the host properties sent, and the TPer's answer
  , rrPairProperties :: !(Maybe PropertiesPair)
                                            -- ^ a pair's Properties compared ("Hsed.Jig.PairProperties", design 13.32 (6))
  }
  deriving (Eq, Show)


-- * Writing

-- | Write a run's reports into a directory: the files written, named for
--   the target and the start's second, with the first suffix (@-2@, @-3@)
--   whose JSON report no run has written. The JSON report's name is
--   claimed exclusively ("Hsed.Jig.Files".'createOwned'), so that two runs
--   of the same second, or started together, take different names and
--   neither writes over the other's; the Markdown and JUnit reports follow
--   their JSON's name (one of that name alone, left by a run that was
--   stopped, is replaced).
writeReports :: FilePath -> RunResult -> IO [FilePath]
writeReports outDir rr0 = claim (1 :: Int)
  where
    rr = printable rr0
    claim n = do
      let base = suffixed n
      claimed <- createOwned (base <.> "json") (toStrictBytes (A.encode rr))
      if claimed
        then do
          writeOwned (base <.> "md") (encodeUtf8 (markdown rr))
          writeOwned (base <.> "xml") (encodeUtf8 (junit rr))
          pure [base <.> "json", base <.> "md", base <.> "xml"]
        else claim (n + 1)
    suffixed n = stamped <> (if n == 1 then "" else "-" <> show n)
    stamped    = outDir </> ("jig-" <> T.unpack (slug (roTarget (rrOptions rr))) <> "-" <> formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" (rrStarted rr))

-- | A target as a report's name has it.
slug :: TargetSpec -> Text
slug = \case
  TSim      -> "sim"
  TDevice d -> T.intercalate "-" (filter (not . T.null) (T.split (== '/') d))
  TReplay f -> "replay-" <> T.map (\c -> if C.isAlphaNum c || c `elem` ['-', '_', '.'] then c else '_') (T.pack (takeBaseName f))
  TPair a b -> slug a <> "+" <> slug b

-- | A target as @--target@ takes it.
renderTarget :: TargetSpec -> Text
renderTarget = \case
  TSim      -> "sim"
  TDevice d -> d
  TReplay f -> "replay:" <> T.pack f
  TPair a b -> renderTarget a <> "+" <> renderTarget b

-- | An exit code as a number.
exitNumber :: ExitCode -> Int
exitNumber = \case
  ExitSuccess   -> 0
  ExitFailure n -> n


-- * Secrets

-- | Whether a run's reports show its secrets in clear (@--show-msid@).
shown :: RunResult -> Bool
shown = roShowMsid . rrOptions

-- | A run's result as the reports and the summary print it: as it is where
--   the run shows its secrets (@--show-msid@); else each transcript entry
--   with its secrets masked in its bytes ("Hsed.Jig.Transcript".'maskSecrets'),
--   and each command a drive was issued outside its policy as the
--   transcript's entries are, and every text a case or the run came to, a
--   verdict's reasons, an extension's, an observation, a waiver's reason, a
--   problem, why the run was stopped, a pre-flight's note, what @--recover@
--   did, the names of the properties an end's Properties exchange reported
--   and the pair's comparison of them, with them masked as text, hex or as
--   shown ('maskText'), so that a reason quoting an answer that did not
--   read never prints one (design 9.3 item 5).
printable :: RunResult -> RunResult
printable rr
  | shown rr  = rr
  | otherwise = rr { rrCases = map maskCase (rrCases rr), rrTranscript = map (maskSecrets secrets) (rrTranscript rr)
                   , rrProblems = map text (rrProblems rr), rrOutsidePolicy = map (second request) (rrOutsidePolicy rr)
                   , rrInterrupted = fmap text (rrInterrupted rr), rrPreflight = map text (rrPreflight rr)
                   , rrRecover = fmap (bimap text text) (rrRecover rr)
                   , rrProperties = [ (end, map (first text) sent, maskAnswer answer) | (end, sent, answer) <- rrProperties rr ]
                   , rrPairProperties = fmap maskPair (rrPairProperties rr) }
  where
    secrets = rrSecrets rr
    text    = maskText secrets
    maskAnswer (PropertiesAnswer tper host) = PropertiesAnswer (map (first text) tper) (fmap (map (first text)) host)
    maskPair = \case
      NotCompared why             -> NotCompared (text why)
      Compared ds simOnly driveOnly ->
        Compared [ d { pdName = text (pdName d), pdRule = text (pdRule d) } | d <- ds ] (map text simOnly) (map text driveOnly)
    -- a command's payload masked as a blocked entry's is
    request q = case maskSecrets secrets (Blocked 0 "" q "") of
      Blocked _ _ masked _ -> masked
      _                    -> q
    maskCase cr = cr
      { crVerdict      = verdict (crVerdict cr)
      , crWaivers      = map (\w -> w { wReason = text (wReason w) }) (crWaivers cr)
      , crExtensions   = map (bimap text verdict) (crExtensions cr)
      , crObservations = map (bimap text text) (crObservations cr)
      , crEnds         = map (second verdict) (crEnds cr)
      }
    verdict = \case
      Pass           -> Pass
      Fail rs        -> Fail (map text rs)
      Skipped r      -> Skipped (text r)
      Unobservable r -> Unobservable (text r)

-- | A secret as the reports give it (design 9.3 item 5): its length and
--   the first 12 hex digits of its SHA-256, never its bytes.
--
-- >>> describeSecret "HSED-SIM-MSID-0123456789ABCDEF01"
-- "32 bytes, sha256 a6c4ca3911e4"
describeSecret :: ByteString -> Text
describeSecret s = tshow (B.length s) <> " bytes, sha256 " <> T.take 12 (sha256Hex s)

-- | How a run's secrets are printed, in words.
secretsText :: RunResult -> Text
secretsText rr
  | shown rr  = "shown in clear (--show-msid)"
  | otherwise = "masked as 0x2A in the transcript (--show-msid shows them)"

-- | Whether a secret is short: under four bytes, the floor
--   "Hsed.Jig.Options".'Hsed.Jig.Options.parseSidPin' sets the simulator's
--   SID PIN, so that its masking rewrites none of the reports' short words
--   or page numbers; a drive's MSID has none (design 9.3 item 5).
shortSecret :: ByteString -> Bool
shortSecret s = B.length s < 4

-- | What the reports say beside a short secret.
shortWarning :: Text
shortWarning = "short: masked wherever its bytes stand, the reports' own words and numbers among them (design 9.3 item 5)"

-- | A secret as the reports give it, with the warning a short one carries.
secretLine :: ByteString -> Text
secretLine s = describeSecret s <> (if shortSecret s then "; " <> shortWarning else "")


-- * JSON

instance A.ToJSON CaseResult where
  toJSON cr = A.Object (KM.fromList fields <> verdictFields)
    where
      fields =
        [ "key"        A..= crKey cr
        , "title"      A..= fmap ceTitle (entryOf (crKey cr))
        , "pdf"        A..= fmap cePdf (entryOf (crKey cr))
        , "printed"    A..= fmap cePrinted (entryOf (crKey cr))
        , "waivers"    A..= map (waiverJSON (crKey cr)) (crWaivers cr)
        , "extensions" A..= [ A.Object (KM.fromList ["name" A..= n] <> objectOf v) | (n, v) <- crExtensions cr ]
        , "observations" A..= [ A.object ["name" A..= n, "value" A..= v] | (n, v) <- crObservations cr ]
        , "ends"       A..= [ A.Object (KM.fromList ["end" A..= n] <> objectOf v) | (n, v) <- crEnds cr ]
        , "prerequisites" A..= (if crEstablished cr then "established" else "not reached" :: Text)
        ]
      verdictFields = objectOf (crVerdict cr)
      objectOf v = case A.toJSON v of
        A.Object o -> o
        _          -> mempty

-- | A run's result as given: 'writeReports' prints 'printable'.
instance A.ToJSON RunResult where
  toJSON rr = A.object
    [ "hsed"            A..= rrVersion rr
    , "started"         A..= rrStarted rr
    , "options"         A..= optionsJSON (rrOptions rr)
    , "targets"         A..= rrTargets rr
    , "cases"           A..= rrCases rr
    , "transcript"      A..= map entryJSON (rrTranscript rr)
    , "level0"          A..= [ A.object ["end" A..= end, "response" A..= l0, "decoded" A..= map decodedJSON (l0Descriptors l0)]
                             | (end, l0) <- rrLevel0 rr ]
    , "pair"            A..= rrPair rr
    , "if_send_issued"  A..= rrIfSends rr
    , "secrets"         A..= [ A.object (["len" A..= B.length s, "sha256" A..= T.take 12 (sha256Hex s), "shown" A..= shown rr]
                                         <> [ "short" A..= True | shortSecret s ])
                             | s <- rrSecrets rr ]
    , "problems"        A..= rrProblems rr
    , "exit"            A..= exitNumber (rrExit rr)
    , "policies"        A..= [ A.Object (KM.fromList ["end" A..= end] <> objectOf (A.toJSON p)) | (end, p) <- rrPolicies rr ]
    , "outside_policy"  A..= [ A.object ["end" A..= end, "request" A..= q] | (end, q) <- rrOutsidePolicy rr ]
    , "blocked"         A..= rrBlocked rr
    , "interrupted"     A..= rrInterrupted rr
    , "preflight"       A..= rrPreflight rr
    , "recover"         A..= fmap (either (outcome False) (outcome True)) (rrRecover rr)
    , "properties"      A..= [ A.object [ "end" A..= end, "host_sent" A..= named (map (second toInteger) sent)
                                        , "tper" A..= named (paTPer answer), "host" A..= fmap named (paHost answer) ]
                             | (end, sent, answer) <- rrProperties rr ]
    , "pair_properties" A..= rrPairProperties rr
    ]
    where
      decodedJSON d = A.object ["code" A..= dCode d, "decode" A..= decodeDescriptor d]
      outcome ok text = A.object ["ok" A..= (ok :: Bool), "text" A..= (text :: Text)]
      -- properties as an object of their names
      named ps = A.object [ K.fromText name A..= (v :: Integer) | (name, v) <- ps ]
      objectOf v = case v of
        A.Object o -> o
        _          -> mempty

-- | The options a run was given, the expected serial masked and the
--   simulator's SID PIN given by its length and digest alone.
optionsJSON :: RunOptions -> A.Value
optionsJSON o = A.object
  [ "target"                   A..= renderTarget (roTarget o)
  , "tests"                    A..= roTests o
  , "units"                    A..= spUnits (roSim o)
  , "hold"                     A..= roHold o
  , "inactive_comid_discard"   A..= spInactiveComIdDiscard (roSim o)
  , "sim_sid_pin"              A..= fmap describeSecret (spSidPin (roSim o))
  , "profile"                  A..= roProfile o
  , "report_dir"               A..= roReportDir o
  , "capture"                  A..= roCapture o
  , "expect_serial"            A..= fmap maskSerial (roExpectSerial o)
  , "level0_len"               A..= roLevel0Len o
  , "show_msid"                A..= roShowMsid o
  , "recover"                  A..= roRecover o
  , "allow_mounted"            A..= roAllowMounted o
  , "interrupt_in_session"     A..= roInterruptInSession o
  , "stack_reset_before_cases" A..= roStackResetBeforeCases o
  ]

-- | A waiver with the item and page of its section's list.
waiverJSON :: CaseKey -> Waiver -> A.Value
waiverJSON key w = A.object
  [ "prereq" A..= tshow (wPrereq w)
  , "item"   A..= L.lookup (wPrereq w) (common section)
  , "list"   A..= fst (listAt section)
  , "pdf"    A..= snd (listAt section)
  , "reason" A..= wReason w
  ]
  where section = sectionOf (ckTest key)

-- | A transcript entry with its decode.
entryJSON :: TEntry -> A.Value
entryJSON e = case A.toJSON e of
  A.Object o -> A.Object (o <> KM.fromList [ ("decode", d) | Just d <- [decodeOf e] ])
  v          -> v

-- | What an entry's bytes hold: an IF-SEND's ComPacket; an IF-RECV's Level 0
--   response or ComPacket.
decodeOf :: TEntry -> Maybe A.Value
decodeOf = \case
  Issued _ _ q r  -> objectIf (requestDecode q <> responseDecode q r)
  Blocked _ _ q _ -> objectIf (requestDecode q)
  Note _ _      -> Nothing
  where
    objectIf ps = if null ps then Nothing else Just (A.object ps)
    requestDecode q = [ "request" A..= comPacketJSON False (payload q) | dir q == IfSend, proto q == comPacketProtocol ]
    responseDecode q r
      | dir q /= IfRecv || status r /= Good || proto q /= comPacketProtocol = []
      | comId q == level0ComId = ["response" A..= level0Summary (rpayload r)]
      | otherwise              = ["response" A..= comPacketJSON True (rpayload r)]

-- | A Level 0 response's Length of Parameter Data, codes and problems.
level0Summary :: ByteString -> A.Value
level0Summary bytes = case splitLevel0 bytes of
  Left e   -> A.object ["error" A..= tshow e]
  Right l0 -> A.object [ "length_of_parameter_data" A..= l0LengthOfParameterData l0
                       , "codes" A..= map dCode (l0Descriptors l0)
                       , "problems" A..= l0Problems l0 ]

-- | A ComPacket: its header, an IF-RECV's as a row of Table 45, its packets.
comPacketJSON :: Bool -> ByteString -> A.Value
comPacketJSON recv bytes = case decodeComPacket bytes of
  Left e -> A.object ["error" A..= tshow e]
  Right (DecodedFrame (ComPacketFrame h ps) ds _) -> A.object $
    [ "comid"            A..= comPacketComID h
    , "comid_extension"  A..= comPacketComIDExtension h
    , "outstanding_data" A..= comPacketOutstandingData h
    , "min_transfer"     A..= comPacketMinTransfer h
    , "packets"          A..= map packetJSON ps
    ]
    <> [ "differences" A..= map tshow ds | not (null ds) ]
    <> concat [ ["length" A..= l, "t45" A..= fmap t45Name (t45RowOf answer)] | recv, Right answer@(l, _, _) <- [t45Of (Response Good bytes)] ]
  where
    packetJSON (PacketFrame ph sps) = A.object
      [ "tsn" A..= packetTSN ph, "hsn" A..= packetHSN ph, "seq_number" A..= packetSeqNumber ph, "subpackets" A..= map subJSON sps ]
    subJSON sp = A.object
      (("kind" A..= dataSubPacketKind (dataSubPacketHeader sp)) : ("length" A..= B.length (dataSubPacketPayload sp)) : tokensJSON (dataSubPacketPayload sp))

-- | A subpacket's tokens, read as the host reads a TPer's stream
--   ("Hsed.Core.Stream".'returnedItems', which reads a host's invocations
--   too, a Session Manager response being one): one invocation as
--   @method_call@, one method response as @method_response@, else the items
--   in order as @items@ (each a call, a response, @end_of_session@ or a
--   transaction token with its status) with any fault at its token
--   (@fault@), Empty atoms alone counted (@empty_atoms@); bytes whose atoms
--   do not read have the fault and the bytes from it (@undecoded@).
tokensJSON :: ByteString -> [(A.Key, A.Value)]
tokensJSON bytes = case atoms hostPolicy bytes of
  Left (off, why) -> [ "items" A..= ([] :: [A.Value]), "fault" A..= ("at byte " <> tshow off <> ": " <> why)
                     , "undecoded" A..= decodeUtf8Lenient (B16.encode (B.drop off bytes)) ]
  Right as -> case returnedItems (map atomToken as) of
    ([ReturnedCall mc], Nothing)    -> ["method_call" A..= callJSON mc]
    ([ReturnedResults mr], Nothing) -> ["method_response" A..= tshow mr]
    (items', fault) -> ("items" A..= map itemJSON items')
                       : [ "fault" A..= ("at token " <> tshow (faultAt f) <> ": " <> faultWhy f) | Just f <- [fault] ]
                       <> [ "empty_atoms" A..= B.length bytes | not (B.null bytes), B.all (== 0xFF) bytes ]
  where
    callJSON (MethodCall (MethodHeader invoker method) ps st) = A.object
      [ "invoking_id" A..= tshow invoker, "method_id" A..= tshow method, "method" A..= methodName method
      , "status" A..= st, "parameters" A..= tshow ps ]
    itemJSON = \case
      ReturnedCall mc         -> A.object ["method_call" A..= callJSON mc]
      ReturnedResults mr      -> A.object ["method_response" A..= tshow mr]
      ReturnedEndOfSession    -> A.object ["end_of_session" A..= True]
      ReturnedTransaction t n -> A.object ["transaction" A..= tshow t, "status" A..= n]

-- | A method's name, from the generated UID tables (Core Tables 241 and
--   242).
methodName :: UID -> Maybe Text
methodName u = T.pack <$> L.lookup u (sessionManagerMethodUIDs <> methodIDUIDs)

t45Name :: T45Row -> Text
t45Name = \case
  ResponsesToCome     -> "responses_to_come"
  InsufficientLength  -> "insufficient_length"
  AdditionalAvailable -> "additional_available"
  AdditionalToCome    -> "additional_to_come"
  AllReturnedWithData -> "all_returned_with_data"
  AllReturnedNoData   -> "all_returned_no_data"


-- * Markdown

-- | The Markdown report: the run's header; why it was stopped, if it was,
--   and the drive's pre-flight notes; the targets; what @--recover@ did;
--   the cases; each end's Level 0; the pair's comparisons, of Level 0 and of
--   Properties; each drive's policy, the commands outside it and those the
--   Guard refused; the problems; the secrets; the transcript. A section the
--   run has nothing for is left out.
markdown :: RunResult -> Text
markdown rr = T.unlines $
  [ "# hsed-jig run: " <> renderTarget (roTarget o)
  , ""
  , "- hsed: " <> rrVersion rr
  , "- started: " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (rrStarted rr))
  ]
  <> [ "- simulator: units " <> unitName (spUnits (roSim o)) <> ", hold " <> tshow (roHold o)
         <> maybe "" ((", profile " <>) . T.pack) (roProfile o)
         <> maybe "" ((", SID PIN given: " <>) . describeSecret) (spSidPin (roSim o))
     | hasSimulator rr ]
  <> [ "- IF-SENDs passed to a drive's transport: " <> tshow (rrIfSends rr)
  , "- secrets read: " <> tshow (length (rrSecrets rr)) <> (if null (rrSecrets rr) then "" else ", " <> secretsText rr)
  , "- exit: " <> tshow (exitNumber (rrExit rr))
  , ""
  ]
  <> mdSection "Interrupted" [ "- " <> why | Just why <- [rrInterrupted rr] ]
  <> mdSection "Pre-flight" (map ("- " <>) (rrPreflight rr))
  <> [ "## Targets"
  , ""
  , "| role | impl | units | ssc | base ComID | ComIDs | device |"
  , "|---|---|---|---|---|---|---|"
  ]
  <> [ row [ dRole d, dImpl d, unitName (dUnits d), fromMaybe "" (dSsc d), maybe "" hex (dBaseComId d)
           , maybe "" tshow (dNComIds d), fromMaybe "" (dDevice d) ]
     | d <- rrTargets rr ]
  <> [""]
  <> mdSection "Recover: STACK_RESET on the base ComID (design 9.3)" (either (\why -> ["- not recovered: " <> why]) (\t -> ["- recovered: " <> t])
                                                                            =<< maybeToList (rrRecover rr))
  <> concat [ ["## Cases", ""] <> concatMap caseLines (rrCases rr) | not (null (rrCases rr)) ]
  <> level0Lines (rrLevel0 rr)
  <> maybe [] pairLines (rrPair rr)
  <> mdSection "Pair: Properties, the simulator's against the drive's (Opal 2.30 Table 17)"
             (concatMap (map listItem . propertiesText) (maybeToList (rrPairProperties rr)))
  <> policyLines rr
  <> concat [ ["## Problems", ""] <> map ("- " <>) (rrProblems rr) <> [""] | not (null (rrProblems rr)) ]
  <> secretLines rr
  <> transcriptLines (rrTranscript rr)
  where
    o = rrOptions rr
    -- a line as a list item, unless it is one, or a table's row
    listItem l = if any (`T.isPrefixOf` l) ["- ", "|"] then l else "- " <> l

-- | A section of the Markdown report: its heading and its lines; none where
--   it has no lines.
mdSection :: Text -> [Text] -> [Text]
mdSection _ []       = []
mdSection title body = ["## " <> title, ""] <> body <> [""]

-- | Each drive's policy as the run ended, the commands its own transcript
--   holds that the policy does not admit, and how many the Guard refused
--   (design 9.3); none where the run had no drive.
policyLines :: RunResult -> [Text]
policyLines rr
  | null (rrPolicies rr) && null (rrOutsidePolicy rr) && rrBlocked rr == 0 = []
  | otherwise = mdSection "Policies" $
      [ "Each drive's policy as the run ended, the commands its own transcript holds that the policy does not admit, and the "
          <> "commands the Guard refused (design 9.3):", "" ]
      <> [ "- " <> end <> ": " <> policyText p | (end, p) <- rrPolicies rr ]
      <> (if null (rrOutsidePolicy rr) then ["- outside a drive's policy: none"]
          else [ "- outside its policy, on the " <> end <> ": " <> commandText q | (end, q) <- rrOutsidePolicy rr ])
      <> [ "- commands the Guard refused: " <> tshow (rrBlocked rr) ]

-- | A policy in words: Level0Only, or the base ComID and each method the
--   allow list admits.
policyText :: Policy -> Text
policyText = \case
  Level0Only            -> "Level0Only: IF-RECV of Level 0 Discovery alone"
  AllowList base admits -> "AllowList on base ComID " <> hex base <> ": " <> T.intercalate ", " (map allowedText admits)

-- | A method an allow list admits, in words.
allowedText :: Allowed -> Text
allowedText = \case
  AllowProperties          -> "Properties"
  AllowStartSessionAdminSP -> "StartSession to the Admin SP as Anybody"
  AllowGetCPinMSID         -> "Get on C_PIN_MSID's UID and PIN"
  AllowEndOfSession        -> "End of Session"
  AllowStackReset          -> "STACK_RESET and its GET_COMID_RESPONSE"

-- | A pair's Properties as the reports print them: not compared, and why;
--   or how many differences of each judgement, each difference and the
--   properties one end alone reported ("Hsed.Jig.PairProperties".'propertiesLines'),
--   and whether the comparison passes.
propertiesText :: PropertiesPair -> [Text]
propertiesText = \case
  NotCompared why -> ["not compared: " <> why]
  pp@(Compared ds _ _) ->
    [ "differences: " <> count Allowed <> " allowed, " <> count Unchecked <> " unchecked, " <> count NotAllowed <> " not allowed" ]
    <> propertiesLines pp
    <> [ if propertiesFail pp then "fail" else "pass" ]
    where count j = tshow (length [ () | d <- ds, pdJudged d == j ])

-- | The run's secrets, each by its length and digest, a short one warned
--   of (design 9.3 item 5).
secretLines :: RunResult -> [Text]
secretLines rr
  | null (rrSecrets rr) = []
  | otherwise = ["## Secrets", "", "Each " <> secretsText rr <> "; given here by length and SHA-256 alone:", ""]
                <> [ "- secret " <> tshow i <> ": " <> secretLine s | (i, s) <- zip [1 :: Int ..] (rrSecrets rr) ]
                <> [""]

-- | Whether a run had the simulator for an end: a target that says it is a
--   TPer ("Hsed.Transport.InProc").
hasSimulator :: RunResult -> Bool
hasSimulator rr = any ((== "tper") . dRole) (rrTargets rr)

-- | Whether a run had a drive for an end, its own or a replay.
hasDrive :: RunResult -> Bool
hasDrive rr = any ((/= "tper") . dRole) (rrTargets rr)

-- | A case: its verdict and why, each item of its section's list, its
--   extensions, its observations.
caseLines :: CaseResult -> [Text]
caseLines cr =
  [ "### " <> caseTitle (crKey cr) <> ": " <> verdictWord (crVerdict cr), "" ]
  <> [ "- " <> r | r <- verdictReasons (crVerdict cr) ]
  <> [ "" | not (null (verdictReasons (crVerdict cr))) ]
  <> concat [ ["Each end:", ""] <> [ "- " <> n <> ": " <> verdictText v | (n, v) <- crEnds cr ] <> [""] | not (null (crEnds cr)) ]
  <> [ "Prerequisites, Test Cases " <> fst (listAt section) <> " (PDF " <> tshow (snd (listAt section)) <> "):", "" ]
  <> [ "- " <> prereqLine p | (p, _) <- common section ]
  <> concat [ ["", "Extensions, apart from the TCG verdict:", ""] <> [ "- " <> n <> ": " <> verdictText v | (n, v) <- crExtensions cr ]
            | not (null (crExtensions cr)) ]
  <> concat [ ["", "Observations:", ""] <> [ "- " <> n <> ": " <> v | (n, v) <- crObservations cr ] | not (null (crObservations cr)) ]
  <> [""]
  where
    section = sectionOf (ckTest (crKey cr))
    prereqLine p = case L.find ((== p) . wPrereq) (crWaivers cr) of
      Just w  -> waiverLine (crKey cr) w
      Nothing -> tshow p <> " (" <> itemRef (crKey cr) p <> "): " <> if crEstablished cr then "established or checked by the run" else "not reached"

-- | A waiver as the reports print it: the prerequisite, its item and page,
--   the reason.
waiverLine :: CaseKey -> Waiver -> Text
waiverLine key w = tshow (wPrereq w) <> " (" <> itemRef key (wPrereq w) <> ") waived: " <> wReason w

-- | Where a case's section lists a prerequisite: @Test Cases 5.2 item 2,
--   PDF 29@.
itemRef :: CaseKey -> Prereq -> Text
itemRef key p = "Test Cases " <> listNo <> maybe "" ((" item " <>) . tshow) (L.lookup p (common section)) <> ", PDF " <> tshow page
  where
    section        = sectionOf (ckTest key)
    (listNo, page) = listAt section

-- | Each end's Level 0 response: its descriptors and what decoding read.
level0Lines :: [(Text, Level0)] -> [Text]
level0Lines []  = []
level0Lines l0s = ["## Level 0", ""] <> concatMap end l0s
  where
    end (name, l0) =
      [ "### " <> name <> ": Length of Parameter Data " <> tshow (l0LengthOfParameterData l0) <> ", "
          <> tshow (length (l0Descriptors l0)) <> " descriptors", "" ]
      <> [ "- " <> describeProblem p | p <- l0Problems l0 ]
      <> [ "" | not (null (l0Problems l0)) ]
      <> [ "| code | version | bytes | read as | fields |", "|---|---|---|---|---|" ]
      <> map descriptorRow (l0Descriptors l0)
      <> [""]
    descriptorRow d = row $ [hex (dCode d), tshow (dVersion d), tshow (B.length (dBytes d))] <> case decodeDescriptor d of
      Nothing -> ["", "not decoded"]
      Just dd -> [ ddSource dd
                 , T.intercalate ", " ([ n <> " " <> contentText c | (n, c) <- ddFields dd ] <> [ "differs: " <> tshow x | x <- ddDifferences dd ]) ]

-- | The pair comparison.
pairLines :: PairReport -> [Text]
pairLines r =
  [ "## Pair: the simulator against the drive", ""
  , "- extra, the simulator's alone (no failure): " <> codesText (prExtra r)
  , "- missing, the drive's alone (a failure): " <> codesText (prMissing r)
  , "- " <> pairCounts r
  , "- " <> if pairFails r then "fail" else "pass"
  , "" ]
  <> concat [ [ "| code | field | sim | drive | allowance | judged |", "|---|---|---|---|---|---|" ] <> map diffRow ds <> [""]
            | let ds = prAllowed r <> prUnchecked r <> prDiffs r, not (null ds) ]
  where
    diffRow fd = row [ hex (fdCode fd), fdField fd, contentText (fdSim fd), contentText (fdDrive fd)
                     , maybe "none" allowanceText (fdAllowance fd), judgementText (judged fd) ]

-- | A comparison's differences counted: allowed, unchecked (as printed,
--   not evaluated), not allowed.
pairCounts :: PairReport -> Text
pairCounts r = tshow (length (prAllowed r)) <> " differences allowed, " <> tshow (length (prUnchecked r))
               <> " unchecked (as printed, not evaluated), " <> tshow (length (prDiffs r)) <> " not"

judgementText :: Judgement -> Text
judgementText = \case
  Allowed    -> "allowed"
  Unchecked  -> "unchecked"
  NotAllowed -> "not allowed"

-- | The transcript, an entry a row, each command with the end it went to.
transcriptLines :: [TEntry] -> [Text]
transcriptLines es =
  [ "## Transcript", "", "| # | entry | end | command | answer |", "|---|---|---|---|---|" ]
  <> map entryRow es
  <> [""]
  where
    entryRow = \case
      Issued n e q r  -> row [tshow n, "issued", e, commandText q, answerText q r]
      Blocked n e q w -> row [tshow n, "blocked", e, commandText q, "blocked by the Guard: " <> w]
      Note n t        -> row [tshow n, "note", "", "", t]

-- | A command in words, an IF-SEND's methods with it.
commandText :: Request -> Text
commandText q = T.unwords [dirName (dir q), "protocol", hex (proto q), "ComID", hex (comId q) <> ",", tshow (lenBytes q), "bytes"]
  <> case dir q of
       IfSend | proto q == comPacketProtocol -> ": " <> comPacketText (payload q)
       _                                     -> ""

-- | An answer in words: its status and length, and what it holds.
answerText :: Request -> Response -> Text
answerText q r = describeStatus (status r) <> ", " <> tshow (B.length (rpayload r)) <> " bytes" <> held
  where
    held
      | dir q /= IfRecv || status r /= Good || proto q /= comPacketProtocol = ""
      | comId q == level0ComId = case splitLevel0 (rpayload r) of
          Left e   -> ": Level 0 that does not split, " <> tshow e
          Right l0 -> ": Level 0, Length of Parameter Data " <> tshow (l0LengthOfParameterData l0) <> ", codes " <> codesText (map dCode (l0Descriptors l0))
      | otherwise = case t45Of r of
          Left e       -> ": " <> e
          Right answer -> ": (Length, OutstandingData, MinTransfer) " <> triple answer <> maybe "" ((", Table 45 " <>) . t45Name) (t45RowOf answer)
                          <> case comPacketText (rpayload r) of
                               "" -> ""
                               t  -> "; " <> t

-- | A ComPacket's subpackets, each its items as the host reads them
--   ("Hsed.Core.Stream"): a method by name with its status, a method's
--   results with its status (a regular session's answer, UCT-03's @[[3 =
--   the MSID]]@ among them, its secrets masked as the JSON's decode has
--   them), the TPer's End of Session, a transaction token; and any fault.
comPacketText :: ByteString -> Text
comPacketText bytes = case decodeComPacket bytes of
  Left e -> "no ComPacket: " <> tshow e
  Right (DecodedFrame (ComPacketFrame _ ps) _ _) -> T.intercalate "; " (map subText (concatMap pfSubPackets ps))
  where
    subText sp = let b = dataSubPacketPayload sp in case atoms hostPolicy b of
      Left (off, why) -> "tokens that do not read at byte " <> tshow off <> ": " <> why
      Right as -> case returnedItems (map atomToken as) of
        ([], Nothing)
          | B.null b          -> "an empty subpacket"
          | B.all (== 0xFF) b -> tshow (B.length b) <> " Empty atoms"
          | otherwise         -> "no invocation or response in " <> tshow (B.length b) <> " bytes"
        (items', fault) -> T.intercalate "; " (map itemText items' <> [ "a fault at token " <> tshow (faultAt f) <> ": " <> faultWhy f | Just f <- [fault] ])
    itemText = \case
      ReturnedCall (MethodCall (MethodHeader _ m) _ st) -> fromMaybe (tshow m) (methodName m) <> ", status " <> tshow st
      ReturnedResults resp    -> "results " <> tshow (_results resp) <> ", status " <> tshow (_resultStatus resp)
      ReturnedEndOfSession    -> "EndOfSession"
      ReturnedTransaction t n -> tshow t <> " " <> tshow n


-- * JUnit XML

-- | The JUnit report: a test case for each case and for each extension, a
--   skip or an unobservable verdict skipped, a failure failed; one for the
--   pair, when there was one, failed where it fails, and one for its
--   Properties where they were compared, failed where a difference is not
--   allowed; one for each problem (the capture, a pair not computed),
--   failed; where a drive was an end, one for no drive command outside its
--   policy and one for no command the Guard refused (design 9.3), each
--   failed where there was one; one for @--recover@ where it ran, failed
--   where it did not recover; one for an interruption, failed, where the
--   run was stopped; and one for the exit code, failed when it is not 0, so
--   that the failures never say less than the exit code. Its properties
--   give the version, the IF-SENDs passed to a drive's transport, the drive commands
--   outside their policy, the commands the Guard refused, and each secret
--   read by its length and digest.
junit :: RunResult -> Text
junit rr = T.unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<testsuites name=\"hsed-jig\" " <> counts <> ">"
  , "  <testsuite name=\"hsed-jig " <> attr (renderTarget (roTarget (rrOptions rr))) <> "\" " <> counts
      <> " timestamp=\"" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (rrStarted rr)) <> "\">"
  , "    <properties>"
  , "      <property name=\"hsed\" value=\"" <> attr (rrVersion rr) <> "\"/>"
  , "      <property name=\"if_send_issued\" value=\"" <> tshow (rrIfSends rr) <> "\"/>"
  , "      <property name=\"outside_policy\" value=\"" <> tshow (length (rrOutsidePolicy rr)) <> "\"/>"
  , "      <property name=\"blocked\" value=\"" <> tshow (rrBlocked rr) <> "\"/>"
  , "      <property name=\"secrets\" value=\"" <> tshow (length (rrSecrets rr)) <> "\"/>"
  ]
  <> [ "      <property name=\"secret_" <> tshow i <> "\" value=\"" <> attr (secretOf s) <> "\"/>"
     | (i, s) <- zip [1 :: Int ..] (rrSecrets rr) ]
  <> [ "    </properties>" ]
  <> concatMap testcase tests
  <> [ "  </testsuite>", "</testsuites>" ]
  where
    target = slug (roTarget (rrOptions rr))
    secretOf s = describeSecret s <> ", " <> secretsText rr <> (if shortSecret s then "; " <> shortWarning else "")
    tests  = [ (caseTitle (crKey cr), crVerdict cr) | cr <- rrCases rr ]
             <> [ (renderCaseKey (crKey cr) <> " extension: " <> n, v) | cr <- rrCases rr, (n, v) <- crExtensions cr ]
             <> [ ("Pair: the simulator's Level 0 against the drive's", pairVerdict r) | Just r <- [rrPair rr] ]
             <> [ ("Pair: the simulator's Properties against the drive's (Opal 2.30 Table 17)", propertiesVerdict pp)
                | Just pp@(Compared {}) <- [rrPairProperties rr] ]
             <> [ ("problem " <> tshow i, Fail [p]) | (i, p) <- zip [1 :: Int ..] (rrProblems rr) ]
             <> [ ("no drive command outside its policy (design 9.3)", if null (rrOutsidePolicy rr) then Pass else Fail [outsideText rr])
                | hasDrive rr ]
             <> [ ("the Guard refused no command (design 9.3)", if rrBlocked rr == 0 then Pass else Fail [refusedText rr]) | hasDrive rr ]
             <> [ ("recover: STACK_RESET", either (\why -> Fail [why]) (const Pass) outcome) | Just outcome <- [rrRecover rr] ]
             <> [ ("interrupted", Fail [why]) | Just why <- [rrInterrupted rr] ]
             <> [ ("exit code", case rrExit rr of
                     ExitSuccess -> Pass
                     code        -> Fail ["exit " <> tshow (exitNumber code) <> ": " <> T.intercalate "; " (exitReasons rr)]) ]
    pairVerdict r
      | pairFails r = Fail ([ "missing " <> codesText (prMissing r) | not (null (prMissing r)) ]
                            <> [ hex (fdCode fd) <> " " <> fdField fd <> " not allowed" | fd <- prDiffs r ])
      | otherwise   = Pass
    propertiesVerdict pp
      | propertiesFail pp = Fail (if null notAllowed then ["the comparison fails"] else notAllowed)
      | otherwise            = Pass
      where notAllowed = [ pdName d <> " (" <> sideName (pdSide d) <> "): the drive's " <> maybe "absent" tshow (pdDrive d)
                           <> ", not allowed by " <> pdRule d
                         | Compared ds _ _ <- [pp], d <- ds, pdJudged d == NotAllowed ]
    sideName = \case
      TPerSide -> "tper" :: Text
      HostSide -> "host"
    counts = "tests=\"" <> tshow (length tests) <> "\" failures=\"" <> tshow (length [ () | (_, Fail _) <- tests ])
             <> "\" skipped=\"" <> tshow (length [ () | (_, v) <- tests, skipped v ]) <> "\""
    skipped = \case
      Skipped _      -> True
      Unobservable _ -> True
      _              -> False
    testcase (name, v) =
      let open = "    <testcase classname=\"hsed-jig." <> attr target <> "\" name=\"" <> attr name <> "\""
      in case v of
        Pass           -> [open <> "/>"]
        Fail rs        -> [open <> ">", "      <failure message=\"" <> attr (T.intercalate "; " rs) <> "\"/>", "    </testcase>"]
        Skipped r      -> [open <> ">", "      <skipped message=\"" <> attr r <> "\"/>", "    </testcase>"]
        Unobservable r -> [open <> ">", "      <skipped message=\"unobservable: " <> attr r <> "\"/>", "    </testcase>"]

-- | Why a run's exit code is not 0, in the order "Hsed.Jig.Run".@runExit@
--   ranks them: the drive commands outside their policy (3); the
--   interruption (130); each case not passed, each extension failed, the
--   pair's failure, its Properties', the problems, the commands the Guard
--   refused, @--recover@ not recovering (1).
exitReasons :: RunResult -> [Text]
exitReasons rr =
  [ outsideText rr | not (null (rrOutsidePolicy rr)) ]
  <> [ "interrupted: " <> why | Just why <- [rrInterrupted rr] ]
  <> [ renderCaseKey (crKey cr) <> " " <> verdictWord (crVerdict cr) | cr <- rrCases rr, crVerdict cr /= Pass ]
  <> [ renderCaseKey (crKey cr) <> " extension " <> n <> " failed" | cr <- rrCases rr, (n, Fail _) <- crExtensions cr ]
  <> [ "the pair fails" | Just r <- [rrPair rr], pairFails r ]
  <> [ "the pair's Properties fail (Opal 2.30 Table 17)" | Just pp <- [rrPairProperties rr], propertiesFail pp ]
  <> rrProblems rr
  <> [ refusedText rr | rrBlocked rr > 0 ]
  <> [ "--recover did not recover: " <> why | Just (Left why) <- [rrRecover rr] ]

-- | The commands a drive was issued outside its policy, in words (design
--   9.3: exit 3).
outsideText :: RunResult -> Text
outsideText rr = "drive commands outside their policy (design 9.3): " <> tshow (length (rrOutsidePolicy rr)) <> ": "
                 <> T.intercalate "; " [ "on the " <> end <> ", " <> commandText q | (end, q) <- rrOutsidePolicy rr ]

-- | How many commands the Guard refused, in words.
refusedText :: RunResult -> Text
refusedText rr = "commands the Guard refused, each blocked in the transcript (design 9.3): " <> tshow (rrBlocked rr)

-- | Text as an XML attribute's value.
attr :: Text -> Text
attr = T.concatMap $ \case
  '&'  -> "&amp;"
  '<'  -> "&lt;"
  '>'  -> "&gt;"
  '"'  -> "&quot;"
  '\'' -> "&apos;"
  c    -> T.singleton c


-- * What a run prints

-- | What a run prints when it ends: each target; why the run was stopped,
--   if it was; the drive's pre-flight notes; what @--recover@ did; each
--   case's verdict with its waivers and extensions; the pair's comparisons,
--   of Level 0 and of Properties; each drive's policy; the IF-SENDs passed
--   to a drive's transport, the drive commands outside their policy (each named) and
--   the commands the Guard refused; the secrets read (by length and digest,
--   a short one warned of); the reports written; the exit code.
summary :: RunResult -> [FilePath] -> [Text]
summary rr0 paths =
  [ "hsed-jig " <> renderTarget (roTarget (rrOptions rr)) <> ": " <> T.intercalate "; " (map targetText (rrTargets rr)) ]
  <> [ "interrupted: " <> why | Just why <- [rrInterrupted rr] ]
  <> [ "pre-flight: " <> n | n <- rrPreflight rr ]
  <> [ "recover: " <> either ("not recovered: " <>) id outcome | Just outcome <- [rrRecover rr] ]
  <> concatMap caseText (rrCases rr)
  <> maybe [] (\r -> [ "pair: extra " <> codesText (prExtra r) <> "; missing " <> codesText (prMissing r) <> "; "
                       <> pairCounts r <> if pairFails r then ": fail" else ": pass" ]) (rrPair rr)
  <> [ "pair properties: " <> T.intercalate "; " (propertiesText pp) | Just pp <- [rrPairProperties rr] ]
  <> [ "policy on the " <> end <> ": " <> policyText p | (end, p) <- rrPolicies rr ]
  <> [ "IF-SENDs passed to a drive's transport: " <> tshow (rrIfSends rr) <> "; drive commands outside their policy: "
         <> tshow (length (rrOutsidePolicy rr)) <> "; refused by the Guard: " <> tshow (rrBlocked rr) ]
  <> [ "  outside its policy, on the " <> end <> ": " <> commandText q | (end, q) <- rrOutsidePolicy rr ]
  <> [ "secrets read: " <> tshow (length (rrSecrets rr)) <> ", " <> secretsText rr <> ": "
         <> T.intercalate "; " (map secretLine (rrSecrets rr))
     | not (null (rrSecrets rr)) ]
  <> [ "reports: " <> T.intercalate ", " (map T.pack paths) | not (null paths) ]
  <> [ "exit " <> tshow (exitNumber (rrExit rr)) ]
  where
    targetText d = dRole d <> " " <> dImpl d <> ", " <> unitName (dUnits d) <> maybe "" (", " <>) (dDevice d)
    caseText cr = (caseTitle (crKey cr) <> ": " <> verdictText (crVerdict cr))
      : [ "  on the " <> n <> ": " <> verdictText v | (n, v) <- crEnds cr ]
      <> map (("  " <>) . waiverLine (crKey cr)) (crWaivers cr)
      <> [ "  extension, " <> n <> ": " <> verdictText v | (n, v) <- crExtensions cr ]
      <> [ "  observed, " <> n <> ": " <> v | (n, v) <- crObservations cr ]
    rr = printable rr0


-- * Reading reports again

-- | @verify-transcript REPORT.json [--no-if-send] [--level0-only]@: print
--   the commands a report's transcript issued, each with the end it went to
--   (its transport's role); with @--no-if-send@, fail if a drive's end was
--   issued an IF-SEND; with @--level0-only@, fail if a drive's end was
--   issued anything but Level 0 Discovery's IF-RECV (protocol 0x01, ComID
--   0x0001). A drive's end is any but the simulator's (@tper@, where one of
--   the report's targets is the simulator): a pair's simulator IF-SENDs
--   count for neither, and an entry that names no end (a report from before
--   ends were recorded) counts as a drive's. An entry that does not read as
--   a report prints one ('rereadEntry': no kind it has, an issued command
--   whose request does not read, one given to a simulator the report has
--   none of) may have been anything, so it counts against both checks:
--   either fails on it. 'verifyTranscriptWith' without @--allow-list@.
verifyTranscript :: FilePath -> Bool -> Bool -> IO ExitCode
verifyTranscript path noIfSend level0Only = verifyTranscriptWith path noIfSend level0Only False

-- | @verify-transcript REPORT.json [--no-if-send] [--level0-only]
--   [--allow-list]@: 'verifyTranscript', and with @--allow-list@ each
--   command a drive's end was issued re-decided offline by the Guard
--   ("Hsed.Jig.Guard".'decide') under that end's own policy as the report
--   gives it (@policies@: an allow list's methods; @Level0Only@ for an end
--   it gives none, or gives @level0_only@), on the base ComID that end's
--   Level 0 response in the report names (its Opal SSC V2 descriptor's Base
--   ComID, Opal 2.30 Table 6, PDF 23), @Level0Only@ where it names none. It
--   fails (exit 1) where the Guard refuses a command, an entry does not
--   read (a command whose request, an IF-SEND's payload among it, cannot be
--   rebuilt; a policy that does not read), the run's Guard blocked any
--   command, or a drive's allow list is bound to another base ComID than
--   its Level 0 names (design 9.3; the M4c acceptance's check of the
--   Guard's log): so an end given anything beyond Level 0 Discovery's
--   IF-RECV passes only under an allow list of its base that admits it. A
--   report's IF-SEND whose payload carries a secret is masked there (design
--   9.3 item 5), so reads as another command: the M4c drive cases send
--   none.
verifyTranscriptWith :: FilePath -> Bool -> Bool -> Bool -> IO ExitCode
verifyTranscriptWith path noIfSend level0Only allowList = readReport path >>= \case
  Left e       -> failWith ("verify-transcript: " <> e)
  Right report -> case A.parseEither (A.withObject "report" (A..: "transcript")) report of
    Left e   -> failWith ("verify-transcript: " <> T.pack path <> ": " <> T.pack e)
    Right es -> do
      traverse_ (say . entryLine) es
      let rereads   = map (rereadEntry (simulatorEnds report)) es
          issued    = [ q | ReIssued _ _ q <- rereads ]
          unread    = length [ () | ReUnread _ <- rereads ]
          sends     = length [ () | q <- issued, dir q == IfSend ]
          notLevel0 = length [ () | q <- issued, not (isLevel0Recv q) ]
      say ("IF-SENDs issued on a drive's end: " <> tshow sends)
      when level0Only $
        say ("commands issued on a drive other than Level 0 Discovery's IF-RECV: " <> tshow notLevel0)
      when (unread > 0) $
        say ("entries that do not read as a report prints them, each counted against both checks: " <> tshow unread)
      allowed <- if allowList then allowListCheck report rereads else pure True
      pure (if noIfSend && sends + unread > 0 || level0Only && notLevel0 + unread > 0 || not allowed then ExitFailure 1 else ExitSuccess)
  where
    entryLine v = either (const ("an entry not as a report prints one: " <> decodeUtf8Lenient (toStrictBytes (A.encode v))))
                         id (A.parseEither entryText v)
    entryText = A.withObject "entry" $ \o -> do
      n    <- o A..: "n" :: A.Parser Int
      kind <- o A..: "entry"
      case kind :: Text of
        "note" -> (\t -> "#" <> tshow n <> " note: " <> t) <$> o A..: "text"
        _      -> do
          end <- fromMaybe "an end unnamed" <$> o A..:? "end"
          command <- o A..: "request" >>= A.withObject "request" (\q -> do
            op  <- q A..: "op"
            p   <- q A..: "proto"
            c   <- q A..: "comid"
            len <- q A..: "len"
            pure (T.unwords [op, "protocol", hex (p :: Word8), "ComID", hex (c :: Word16) <> ",", tshow (len :: Word32), "bytes"]))
          outcome <- if kind == "blocked"
            then ("blocked by the Guard: " <>) <$> o A..: "reason"
            else o A..: "response" >>= A.withObject "response" (\r -> (\st n' -> st <> ", " <> tshow (n' :: Int) <> " bytes")
                                                                         <$> r A..: "status" <*> r A..: "len")
          pure ("#" <> tshow n <> " " <> kind <> " on the " <> end <> " " <> command <> ": " <> outcome)
    isLevel0Recv q = dir q == IfRecv && proto q == comPacketProtocol && comId q == level0ComId

-- | A transcript entry read again from a report.
data Reread
  = ReNote                                  -- ^ a note
  | ReBlocked                               -- ^ a command the run's Guard refused
  | ReSimulator                             -- ^ a command issued on the simulator's end
  | ReIssued !Int !(Maybe Text) !Request    -- ^ a command issued on a drive's end: its number, its end (none named), its request
  | ReUnread !Int                           -- ^ an entry that does not read, by its number (0 where it has none)

-- | The ends a report's targets name the simulator (@tper@): an entry given
--   to @tper@ is the simulator's only where the report has one.
simulatorEnds :: A.Value -> [Text]
simulatorEnds = either (const []) id . A.parseEither (A.withObject "report" $ \o -> do
  targets <- fromMaybe [] <$> o A..:? "targets"
  filter (== "tper") <$> traverse (A.withObject "target" (A..: "role")) targets)

-- | A transcript entry as a report prints it, read again, given the ends
--   the report's targets name the simulator: a note; a command blocked; one
--   issued on the simulator's end; one issued on a drive's (any other end,
--   or none named), its request rebuilt ('requestParser'); or one that does
--   not read: no kind a report prints, a request that cannot be rebuilt,
--   or a command given to a simulator the report's targets do not name.
rereadEntry :: [Text] -> A.Value -> Reread
rereadEntry simEnds v = either (const (ReUnread numbered)) id $ flip A.parseEither v $ A.withObject "entry" $ \o -> do
  n    <- fromMaybe 0 <$> o A..:? "n"
  kind <- o A..: "entry"
  end  <- o A..:? "end"
  case kind :: Text of
    "note"    -> pure ReNote
    "blocked" -> pure ReBlocked
    "issued"
      | end == Just "tper", "tper" `elem` simEnds -> pure ReSimulator
      | end == Just "tper" -> A.parseFail "a command the simulator's end was issued, in a report whose targets name no simulator"
      | otherwise          -> ReIssued n end <$> (o A..: "request" >>= requestParser)
    _         -> A.parseFail "an entry of no kind a report prints"
  where numbered = either (const 0) id (A.parseEither (A.withObject "entry" (\o -> fromMaybe 0 <$> o A..:? "n")) v)

-- | @--allow-list@ over a report's transcript, read again: each drive end's
--   policy as the report gives it, its allow list's base held to the base
--   its Level 0 names, each command a drive's end was issued re-decided
--   under that policy on that base; whether all is well, each finding
--   printed.
allowListCheck :: A.Value -> [Reread] -> IO Bool
allowListCheck report rereads = do
  let bases    = either (const []) id (A.parseEither endBases report)
      reported = either (const []) id (A.parseEither policiesOf report)
      baseOf e = join (e >>= (`L.lookup` bases))
      -- an end's policy as the report gives it, on the base its Level 0 names; Level0Only where it gives none
      policyOf e = case e >>= (`L.lookup` reported) of
        Just (Right (AllowList _ admits)) -> maybe Level0Only (`AllowList` admits) (baseOf e)
        _                                 -> Level0Only
      verdicts = [ (n, end, decide (policyOf end) q) | ReIssued n end q <- rereads ]
      refused  = [ (n, end, why) | (n, end, Left why) <- verdicts ]
      unread   = [ n | ReUnread n <- rereads ]
      blocked  = length [ () | ReBlocked <- rereads ]
      unparsed = [ (end, why) | (end, Left why) <- reported ]
      mismatch = [ (end, bound, L.lookup end bases) | (end, Right (AllowList bound _)) <- reported, join (L.lookup end bases) /= Just bound ]
  for_ bases $ \(end, base) -> say ("--allow-list: the " <> end <> "'s base ComID, as its Level 0 names it: "
                                     <> maybe "none (no Opal SSC V2 descriptor that decodes): Level 0 Discovery alone admitted" hex base)
  for_ reported $ \(end, policy) -> say ("--allow-list: the " <> end <> "'s policy, as the report gives it: " <> either id policyText policy)
  for_ unparsed $ \(end, why) -> say ("--allow-list: the " <> end <> "'s policy does not read (" <> why <> "): refused")
  for_ mismatch $ \(end, bound, base) -> say ("--allow-list: the " <> end <> "'s policy is bound to " <> hex bound <> ", not "
                                               <> maybe "the base ComID its Level 0 names: it names none" (("the " <>) . (<> " its Level 0 names") . hex) (join base))
  for_ refused $ \(n, end, why) -> say ("--allow-list: #" <> tshow n <> " on the " <> fromMaybe "an end unnamed" end <> " refused: "
                                        <> (if allowedListed end && isNothing (baseOf end) then "no base ComID in its Level 0, so Level 0 Discovery alone: " else "")
                                        <> why)
  for_ unread $ \n -> say ("--allow-list: #" <> tshow n <> " does not read as a command a report prints: refused")
  say ("--allow-list: the commands issued on a drive, re-decided by the Guard under each drive end's policy as the report gives it, "
       <> "on the base its Level 0 names [" <> T.intercalate "; " [ e <> ": " <> policyText (policyOf (Just e)) | e <- drives ] <> "]: "
       <> tshow (length verdicts - length refused) <> " admitted, " <> tshow (length refused) <> " refused, " <> tshow (length unread)
       <> " that do not read; blocked by the run's Guard: " <> tshow blocked)
  pure (null refused && null unread && blocked == 0 && null mismatch && null unparsed)
  where
    -- each end's base ComID, by its Level 0 in the report (none for the simulator's)
    endBases = A.withObject "report" $ \o -> do
      ends <- fromMaybe [] <$> o A..:? "level0"
      fmap catMaybes . for ends $ A.withObject "end" $ \e -> do
        end <- e A..: "end"
        if end == ("tper" :: Text) then pure Nothing else do
          l0 <- e A..: "response" >>= level0Parser
          pure (Just (end, level0DiscoveryOpalSSCV2FeatureBaseComID <$> opalV2Of l0))
    -- each drive end's policy as the report gives it, or why it does not read
    policiesOf = A.withObject "report" $ \o -> do
      ps <- fromMaybe [] <$> o A..:? "policies"
      for ps $ A.withObject "policy" $ \p -> do
        end <- p A..: "end"
        kind <- p A..: "policy"
        (,) (end :: Text) <$> case kind :: Text of
          "level0_only" -> pure (Right Level0Only)
          "allow_list"  -> do
            base <- p A..: "base_comid"
            names <- p A..: "allowed"
            pure (AllowList base <$> traverse allowedNamed names)
          other         -> pure (Left ("a policy of no kind a report prints: " <> other))
    allowedNamed name = maybe (Left ("a method no allow list names: " <> name)) Right
                              (L.lookup name [ (allowedName a, a) | a <- [minBound .. maxBound] ])
    allowedName a = case A.toJSON a of
      A.String t -> t
      _          -> ""
    -- whether the report gives an end an allow list
    allowedListed e = case e >>= (`L.lookup` either (const []) id (A.parseEither policiesOf report)) of
      Just (Right (AllowList _ _)) -> True
      _                            -> False
    -- the drive ends the transcript issued commands on
    drives = L.nub [ fromMaybe "an end unnamed" end | ReIssued _ end _ <- rereads ]

-- | A command as a report prints it, rebuilt: its op, protocol, ComID and
--   length, and an IF-SEND's payload from its hex ("Hsed.Wire".'trimmedHex',
--   trailing zeros left out) padded with zeros to the length.
requestParser :: A.Value -> A.Parser Request
requestParser = A.withObject "request" $ \q -> do
  op     <- q A..: "op"
  d      <- case op :: Text of
    "if_send" -> pure IfSend
    "if_recv" -> pure IfRecv
    _         -> A.parseFail ("an op of no kind a report prints: " <> T.unpack op)
  p      <- q A..: "proto"
  c      <- q A..: "comid"
  len    <- q A..: "len"
  digits <- if d == IfSend then q A..: "payload" else pure ""
  bytes  <- either A.parseFail pure (B16.decode (encodeUtf8 digits))
  pure (Request d p c len (bytes <> B.replicate (fromIntegral len - B.length bytes) 0))

-- | A Level 0 response as a report prints it, its descriptors' bytes
--   restored from the hex and length printed (the rest of the response is
--   not needed by those who read it back).
level0Parser :: A.Value -> A.Parser Level0
level0Parser = A.withObject "response" $ \r -> do
  ds <- r A..: "descriptors" >>= traverse descriptor
  pure (Level0 mempty 0 ds mempty [])
  where
    descriptor = A.withObject "descriptor" $ \d -> do
      code    <- d A..: "code"
      version <- d A..: "version"
      len     <- d A..: "len"
      digits  <- d A..: "bytes"
      bytes   <- either A.parseFail pure (B16.decode (encodeUtf8 digits))
      pure (Descriptor code version (bytes <> B.replicate (len - B.length bytes) 0))

-- | A report's Properties exchanges ("properties"), each end's read back:
--   the host properties sent, and the TPer's answer, its own properties and
--   the host's as it answered them; each table by name, in the names'
--   order (the JSON's objects keep no other, and a name once).
--
--   'Nothing' for a report that carries none (one from before M4c).
reportProperties :: A.Value -> Either Text (Maybe [(Text, [(Text, Natural)], PropertiesAnswer)])
reportProperties = first T.pack . A.parseEither (A.withObject "report" $ \o -> o A..:? "properties" >>= traverse (traverse exchange))
  where
    exchange = A.withObject "properties" $ \e -> do
      end  <- e A..: "end"
      sent <- e A..: "host_sent" >>= named
      tper <- e A..: "tper" >>= named
      host <- e A..:? "host" >>= traverse named
      pure (end, sent, PropertiesAnswer tper host)
    named :: A.FromJSON n => A.Value -> A.Parser [(Text, n)]
    named = A.withObject "properties" $ \ps -> for (KM.toList ps) $ \(k, v) -> (,) (K.toText k) <$> A.parseJSON v

-- | @diff A.json B.json@: two reports' Level 0 decodes compared as the Pair
--   runner compares them, A's as the simulator's and B's as the drive's,
--   each report's last end (a pair's drive); and where both reports carry
--   Properties exchanges (M4c's), each report's last end's compared as the
--   Pair's Properties are ("Hsed.Jig.PairProperties", Opal 2.30 Table 17),
--   an end that ran none not compared; a failure where either comparison
--   fails.
diffReports :: FilePath -> FilePath -> IO ExitCode
diffReports a b = do
  ra <- readReport a
  rb <- readReport b
  case (,) <$> (ra >>= read' a) <*> (rb >>= read' b) of
    Left e -> failWith ("diff: " <> e)
    Right ((x, px), (y, py)) -> do
      let r = pairLevel0 x y
      traverse_ say (pairLines r)
      propertiesFailed <- case (px, py) of
        (Just ea, Just eb) -> do
          let pp = pairProperties ea eb
          say "## Pair: Properties, A's simulator's against B's drive's (Opal 2.30 Table 17)"
          say ""
          traverse_ (say . ("- " <>)) (propertiesText pp)
          pure (propertiesFail pp)
        _ -> pure False
      pure (if pairFails r || propertiesFailed then ExitFailure 1 else ExitSuccess)
  where
    read' path v = first ((T.pack path <> ": ") <>) $ (,) <$> first T.pack (A.parseEither lastLevel0 v) <*> lastProperties v
    lastLevel0 = A.withObject "report" $ \o -> do
      ends <- o A..: "level0"
      case L.lastMaybe ends of
        Nothing  -> A.parseFail "the report holds no Level 0 response"
        Just end -> A.withObject "end" (A..: "response") end >>= level0Parser
    -- the last end's Properties exchange, if the report carries them: that end's, if it ran one
    lastProperties v = reportProperties v <&> fmap (\exchanges ->
      listToMaybe [ (sent, answer) | Just end <- [lastEndOf v], (end', sent, answer) <- exchanges, end' == end ])
    -- the report's last end: its last target's role
    lastEndOf = either (const Nothing) id . A.parseEither (A.withObject "report" $ \o -> do
      targets <- fromMaybe [] <$> o A..:? "targets"
      traverse (A.withObject "target" (A..: "role")) (L.lastMaybe targets))

-- | A report read as JSON, or why not.
readReport :: FilePath -> IO (Either Text A.Value)
readReport path = tryIO (readFileBinary path) <&> \case
  Left e      -> Left (T.pack path <> ": " <> tshow e)
  Right bytes -> first (((T.pack path <> ": ") <>) . T.pack) (A.eitherDecodeStrict' bytes)


-- * Words

caseTitle :: CaseKey -> Text
caseTitle key = renderCaseKey key <> maybe "" titled (entryOf key)
  where titled e = " " <> ceTitle e <> " (Test Cases PDF " <> tshow (cePdf e) <> ", printed " <> tshow (cePrinted e) <> ")"

entryOf :: CaseKey -> Maybe CatalogueEntry
entryOf key = L.find ((== key) . ceKey) catalogue

verdictWord :: Verdict -> Text
verdictWord = \case
  Pass           -> "pass"
  Fail _         -> "fail"
  Skipped _      -> "skipped"
  Unobservable _ -> "unobservable"

verdictReasons :: Verdict -> [Text]
verdictReasons = \case
  Pass           -> []
  Fail rs        -> rs
  Skipped r      -> [r]
  Unobservable r -> [r]

unitName :: Unit -> Text
unitName = \case
  UnitBytes     -> "bytes"
  UnitBlocks512 -> "blocks512"

dirName :: Dir -> Text
dirName = \case
  IfSend -> "if_send"
  IfRecv -> "if_recv"

contentText :: Content -> Text
contentText = \case
  Number n -> tshow n
  Octets b -> decodeUtf8Lenient (B16.encode b)

allowanceText :: Allowance -> Text
allowanceText = \case
  VendorUnique -> "VU"
  AnyVersion v -> tshow v <> " or any version"
  Minimum v    -> tshow v <> " or larger"
  AsPrinted t  -> "as printed: " <> t

codesText :: [Word16] -> Text
codesText [] = "none"
codesText cs = T.intercalate ", " (map hex cs)

triple :: (Integer, Integer, Integer) -> Text
triple (l, o, m) = "(" <> tshow l <> ", " <> tshow o <> ", " <> tshow m <> ")"

-- | A Markdown table's row, a bar in a cell escaped.
row :: [Text] -> Text
row cs = "| " <> T.intercalate " | " (map (T.concatMap (\c -> if c == '|' then "\\|" else T.singleton c)) cs) <> " |"

say :: Text -> IO ()
say t = hPutBuilder stdout (getUtf8Builder (display t <> "\n"))

failWith :: Text -> IO ExitCode
failWith t = hPutBuilder stderr (getUtf8Builder (display t <> "\n")) >> pure (ExitFailure 1)
