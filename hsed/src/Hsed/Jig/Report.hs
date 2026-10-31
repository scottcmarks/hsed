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
pair, for each problem (a capture, the pair not computed), for no IF-SEND on
a drive, and one for the exit code, so that its failures never say less
than the exit code does. And the commands
that read a report back without privilege: @verify-transcript@ (the
commands issued, each with its end; a failure if a drive's end was issued
an IF-SEND, or anything but Level 0 Discovery's IF-RECV, when asked) and
@diff@ (two reports' Level 0 decodes compared as the Pair runner compares
them).

The target's part of a report's name is @sim@, @dev-disk6@,
@replay-<file>@, or the two ends' joined by @+@ (@sim+dev-disk6@); the stamp
is the run's start, @yyyymmddThhmmssZ@. Each transcript entry is printed
with a decode: a Level 0 response's length and codes for ComID 1, otherwise
the ComPacket, its packets and subpackets, and each subpacket's tokens read
as a method invocation or a method response where they are one; an
IF-RECV's ComPacket header also as the row of Core Table 45 it is. A waiver
is printed with the item and page of its section's list ("Hsed.Jig.Prereq").

The secrets a run read ('rrSecrets': UCT-03's MSID, a PIN read from the
simulator's state) are masked in every report (design 9.3 item 5): each
occurrence in an entry's bytes becomes 0x2A bytes before the entry is
printed or decoded ("Hsed.Jig.Transcript".'maskSecrets'), and each secret is
given by its length and the first 12 hex digits of its SHA-256
("Hsed.Jig.Sha256") in the JSON, the Markdown and the JUnit properties, never
by its bytes; @--show-msid@ leaves the bytes in clear and says so.

@verify-transcript@ and @diff@ read a report as JSON values (there is no
FromJSON for the wire types): the transcript's entries, and the Level 0
descriptors of a report's last end (a pair's drive), their bytes restored
from the hex and length printed.
-}
module Hsed.Jig.Report
  ( CaseResult (..)
  , RunResult (..)
  , writeReports
  , verifyTranscript
  , diffReports
    -- * What a run prints
  , summary
  , renderTarget
  , exitNumber
  , describeSecret
  ) where

import qualified Data.Aeson             as A
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

import           Hsed.Core.Call         (MethodCall (..), MethodHeader (..), MethodResponse)
import           Hsed.Core.Feature      (Allowance (..))
import           Hsed.Core.Layout       (Content (..))
import           Hsed.Core.StreamItem   (StreamItem (..))
import           Hsed.Core.Token        (Token)
import           Hsed.Core.UID          (UID)
import           Hsed.Gen.Core.Layouts  (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..), PacketHeader (..))
import           Hsed.Gen.Core.UIDs     (methodIDUIDs, sessionManagerMethodUIDs)
import           Hsed.Jig.Catalogue     (CaseKey (..), CatalogueEntry (..), catalogue, renderCaseKey)
import           Hsed.Jig.Files         (createOwned, writeOwned)
import           Hsed.Jig.Options       (RunOptions (..), TargetSpec (..))
import           Hsed.Jig.Pair          (FieldDiff (..), Judgement (..), PairReport (..), judged, pairFails, pairLevel0)
import           Hsed.Jig.Prereq        (Prereq, Waiver (..), common, listAt, sectionOf)
import           Hsed.Jig.Script        (Verdict (..), describeStatus, t45Of, t45RowOf, verdictText)
import           Hsed.Jig.Sha256        (sha256Hex)
import           Hsed.Jig.Transcript    (TEntry (..), maskSecrets)
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
--   value, reported apart from the verdict); on a pair, each end's verdict
--   beside the joined one.
data CaseResult = CaseResult
  { crKey          :: !CaseKey
  , crVerdict      :: !Verdict
  , crWaivers      :: ![Waiver]
  , crExtensions   :: ![(Text, Verdict)]
  , crObservations :: ![(Text, Text)]
  , crEnds         :: ![(Text, Verdict)]  -- ^ each end's verdict, by the end's name, its transport's role (@tper@, @drive@,
                                          --   @drive-replay@, as the transcript's entries name it); none for a run of one end
  }
  deriving (Eq, Show)

-- | What a run came to.
data RunResult = RunResult
  { rrOptions    :: !RunOptions
  , rrVersion    :: !Text               -- ^ "Hsed.Version"'s
  , rrStarted    :: !UTCTime
  , rrTargets    :: ![Describe]         -- ^ what each transport said it is (two for a pair)
  , rrCases      :: ![CaseResult]
  , rrTranscript :: ![TEntry]           -- ^ as the wire had it; printed masked ('rrSecrets') unless @--show-msid@
  , rrLevel0     :: ![(Text, Level0)]   -- ^ each end's Level 0 response, by the end's name (its transport's role)
  , rrPair       :: !(Maybe PairReport)
  , rrIfSends    :: !Int                -- ^ the IF-SENDs issued on a drive (none at M3)
  , rrSecrets    :: ![ByteString]       -- ^ the secrets the run read (the MSID, a PIN), each masked in the reports and
                                        --   given by its length and digest alone, unless @--show-msid@
  , rrProblems   :: ![Text]             -- ^ what else failed the run: the capture, a pair not computed
  , rrExit       :: !ExitCode
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
writeReports outDir rr = claim (1 :: Int)
  where
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

-- | A transcript entry as the reports print it: its secrets masked unless
--   the run shows them.
masked :: RunResult -> TEntry -> TEntry
masked rr
  | shown rr  = id
  | otherwise = maskSecrets (rrSecrets rr)

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
        ]
      verdictFields = objectOf (crVerdict cr)
      objectOf v = case A.toJSON v of
        A.Object o -> o
        _          -> mempty

instance A.ToJSON RunResult where
  toJSON rr = A.object
    [ "hsed"           A..= rrVersion rr
    , "started"        A..= rrStarted rr
    , "options"        A..= optionsJSON (rrOptions rr)
    , "targets"        A..= rrTargets rr
    , "cases"          A..= rrCases rr
    , "transcript"     A..= map (entryJSON . masked rr) (rrTranscript rr)
    , "level0"         A..= [ A.object ["end" A..= end, "response" A..= l0, "decoded" A..= map decodedJSON (l0Descriptors l0)]
                            | (end, l0) <- rrLevel0 rr ]
    , "pair"           A..= rrPair rr
    , "if_send_issued" A..= rrIfSends rr
    , "secrets"        A..= [ A.object ["len" A..= B.length s, "sha256" A..= T.take 12 (sha256Hex s), "shown" A..= shown rr]
                            | s <- rrSecrets rr ]
    , "problems"       A..= rrProblems rr
    , "exit"           A..= exitNumber (rrExit rr)
    ]
    where
      decodedJSON d = A.object ["code" A..= dCode d, "decode" A..= decodeDescriptor d]

-- | The options a run was given, the expected serial masked and the
--   simulator's SID PIN given by its length and digest alone.
optionsJSON :: RunOptions -> A.Value
optionsJSON o = A.object
  [ "target"                 A..= renderTarget (roTarget o)
  , "tests"                  A..= roTests o
  , "units"                  A..= spUnits (roSim o)
  , "hold"                   A..= roHold o
  , "inactive_comid_discard" A..= spInactiveComIdDiscard (roSim o)
  , "sim_sid_pin"            A..= fmap describeSecret (spSidPin (roSim o))
  , "profile"                A..= roProfile o
  , "report_dir"             A..= roReportDir o
  , "capture"                A..= roCapture o
  , "expect_serial"          A..= fmap maskSerial (roExpectSerial o)
  , "level0_len"             A..= roLevel0Len o
  , "show_msid"              A..= roShowMsid o
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

-- | A subpacket's tokens: a method invocation (a Session Manager response
--   is one), a method response, or the tokens one by one.
tokensJSON :: ByteString -> [(A.Key, A.Value)]
tokensJSON bytes = case parseByteString bytes of
  Right call | generate call == bytes -> ["method_call" A..= callJSON call]
  _ -> case parseByteString bytes :: Either String MethodResponse of
    Right resp | generate resp == bytes -> ["method_response" A..= tshow resp]
    _ -> let (ts, rest) = tokensOf bytes
         in ("tokens" A..= map tshow ts) : [ "undecoded" A..= decodeUtf8Lenient (B16.encode rest) | not (B.null rest) ]
  where
    callJSON (MethodCall (MethodHeader invoker method) ps st) = A.object
      [ "invoking_id" A..= tshow invoker, "method_id" A..= tshow method, "method" A..= methodName method
      , "status" A..= st, "parameters" A..= tshow ps ]

-- | A method's name, from the generated UID tables (Core Tables 241 and
--   242).
methodName :: UID -> Maybe Text
methodName u = T.pack <$> L.lookup u (sessionManagerMethodUIDs <> methodIDUIDs)

-- | Tokens from the start of bytes, as far as each reads in its canonical
--   form; the rest.
tokensOf :: ByteString -> ([Token], ByteString)
tokensOf bytes
  | B.null bytes = ([], bytes)
  | otherwise = case parseByteString bytes of
      Right t | let g = generate (t :: Token), not (B.null g), g `B.isPrefixOf` bytes -> first (t :) (tokensOf (B.drop (B.length g) bytes))
      _ -> ([], bytes)

t45Name :: T45Row -> Text
t45Name = \case
  ResponsesToCome     -> "responses_to_come"
  InsufficientLength  -> "insufficient_length"
  AdditionalAvailable -> "additional_available"
  AdditionalToCome    -> "additional_to_come"
  AllReturnedWithData -> "all_returned_with_data"
  AllReturnedNoData   -> "all_returned_no_data"


-- * Markdown

-- | The Markdown report.
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
  <> [ "- IF-SENDs issued on a drive: " <> tshow (rrIfSends rr)
  , "- secrets read: " <> tshow (length (rrSecrets rr)) <> (if null (rrSecrets rr) then "" else ", " <> secretsText rr)
  , "- exit: " <> tshow (exitNumber (rrExit rr))
  , ""
  , "## Targets"
  , ""
  , "| role | impl | units | ssc | base ComID | ComIDs | device |"
  , "|---|---|---|---|---|---|---|"
  ]
  <> [ row [ dRole d, dImpl d, unitName (dUnits d), fromMaybe "" (dSsc d), maybe "" hex (dBaseComId d)
           , maybe "" tshow (dNComIds d), fromMaybe "" (dDevice d) ]
     | d <- rrTargets rr ]
  <> ["", "## Cases", ""]
  <> concatMap caseLines (rrCases rr)
  <> level0Lines (rrLevel0 rr)
  <> maybe [] pairLines (rrPair rr)
  <> concat [ ["## Problems", ""] <> map ("- " <>) (rrProblems rr) <> [""] | not (null (rrProblems rr)) ]
  <> secretLines rr
  <> transcriptLines (map (masked rr) (rrTranscript rr))
  where o = rrOptions rr

-- | The run's secrets, each by its length and digest (design 9.3 item 5).
secretLines :: RunResult -> [Text]
secretLines rr
  | null (rrSecrets rr) = []
  | otherwise = ["## Secrets", "", "Each " <> secretsText rr <> "; given here by length and SHA-256 alone:", ""]
                <> [ "- secret " <> tshow i <> ": " <> describeSecret s | (i, s) <- zip [1 :: Int ..] (rrSecrets rr) ]
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
      Nothing -> tshow p <> " (" <> itemRef (crKey cr) p <> "): " <> case crVerdict cr of
        Skipped _ -> "not reached"
        _         -> "established or checked by the run"

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

-- | A ComPacket's methods, or its tokens' count.
comPacketText :: ByteString -> Text
comPacketText bytes = case decodeComPacket bytes of
  Left e -> "no ComPacket: " <> tshow e
  Right (DecodedFrame (ComPacketFrame _ ps) _ _) -> T.intercalate "; " (map subText (concatMap pfSubPackets ps))
  where
    subText sp = let b = dataSubPacketPayload sp in case parseByteString b of
      Right call@(MethodCall (MethodHeader _ m) _ st)
        | generate call == b -> fromMaybe (tshow m) (methodName m) <> ", status " <> tshow st
      _ -> let (ts, rest) = tokensOf b
           in (if length ts <= 8 then T.unwords (map tshow ts) else tshow (length ts) <> " tokens")
              <> (if B.null rest then "" else ", then bytes that are no token")


-- * JUnit XML

-- | The JUnit report: a test case for each case and for each extension, a
--   skip or an unobservable verdict skipped, a failure failed; one for the
--   pair, when there was one, failed where it fails; one for each problem
--   (the capture, a pair not computed), failed; one for no IF-SEND issued on
--   a drive, when there was a drive, failed if one was; and one for the exit
--   code, failed when it is not 0, so that the failures never say less than
--   the exit code. Its properties give the version, the IF-SENDs issued and
--   each secret read by its length and digest.
junit :: RunResult -> Text
junit rr = T.unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<testsuites name=\"hsed-jig\" " <> counts <> ">"
  , "  <testsuite name=\"hsed-jig " <> attr (renderTarget (roTarget (rrOptions rr))) <> "\" " <> counts
      <> " timestamp=\"" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (rrStarted rr)) <> "\">"
  , "    <properties>"
  , "      <property name=\"hsed\" value=\"" <> attr (rrVersion rr) <> "\"/>"
  , "      <property name=\"if_send_issued\" value=\"" <> tshow (rrIfSends rr) <> "\"/>"
  , "      <property name=\"secrets\" value=\"" <> tshow (length (rrSecrets rr)) <> "\"/>"
  ]
  <> [ "      <property name=\"secret_" <> tshow i <> "\" value=\"" <> attr (describeSecret s <> ", " <> secretsText rr) <> "\"/>"
     | (i, s) <- zip [1 :: Int ..] (rrSecrets rr) ]
  <> [ "    </properties>" ]
  <> concatMap testcase tests
  <> [ "  </testsuite>", "</testsuites>" ]
  where
    target = slug (roTarget (rrOptions rr))
    tests  = [ (caseTitle (crKey cr), crVerdict cr) | cr <- rrCases rr ]
             <> [ (renderCaseKey (crKey cr) <> " extension: " <> n, v) | cr <- rrCases rr, (n, v) <- crExtensions cr ]
             <> [ ("Pair: the simulator's Level 0 against the drive's", pairVerdict r) | Just r <- [rrPair rr] ]
             <> [ ("problem " <> tshow i, Fail [p]) | (i, p) <- zip [1 :: Int ..] (rrProblems rr) ]
             <> [ ("no IF-SEND issued on a drive (design 9.3)", if rrIfSends rr > 0 then Fail [tshow (rrIfSends rr) <> " issued"] else Pass)
                | hasDrive rr ]
             <> [ ("exit code", case rrExit rr of
                     ExitSuccess -> Pass
                     code        -> Fail ["exit " <> tshow (exitNumber code) <> ": " <> T.intercalate "; " (exitReasons rr)]) ]
    pairVerdict r
      | pairFails r = Fail ([ "missing " <> codesText (prMissing r) | not (null (prMissing r)) ]
                            <> [ hex (fdCode fd) <> " " <> fdField fd <> " not allowed" | fd <- prDiffs r ])
      | otherwise   = Pass
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

-- | Why a run's exit code is not 0: each case not passed, each extension
--   failed, the pair's failure, the problems, the IF-SENDs issued on a
--   drive.
exitReasons :: RunResult -> [Text]
exitReasons rr =
  [ renderCaseKey (crKey cr) <> " " <> verdictWord (crVerdict cr) | cr <- rrCases rr, crVerdict cr /= Pass ]
  <> [ renderCaseKey (crKey cr) <> " extension " <> n <> " failed" | cr <- rrCases rr, (n, Fail _) <- crExtensions cr ]
  <> [ "the pair fails" | Just r <- [rrPair rr], pairFails r ]
  <> rrProblems rr
  <> [ tshow (rrIfSends rr) <> " IF-SENDs issued on a drive" | rrIfSends rr > 0 ]

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

-- | What a run prints when it ends: each target, each case's verdict with
--   its waivers and extensions, the pair comparison, the IF-SENDs issued on
--   a drive, the secrets read (by length and digest), the reports written,
--   the exit code.
summary :: RunResult -> [FilePath] -> [Text]
summary rr paths =
  [ "hsed-jig " <> renderTarget (roTarget (rrOptions rr)) <> ": " <> T.intercalate "; " (map targetText (rrTargets rr)) ]
  <> concatMap caseText (rrCases rr)
  <> maybe [] (\r -> [ "pair: extra " <> codesText (prExtra r) <> "; missing " <> codesText (prMissing r) <> "; "
                       <> pairCounts r <> if pairFails r then ": fail" else ": pass" ]) (rrPair rr)
  <> [ "IF-SENDs issued on a drive: " <> tshow (rrIfSends rr) ]
  <> [ "secrets read: " <> tshow (length (rrSecrets rr)) <> ", " <> secretsText rr <> ": "
         <> T.intercalate "; " (map describeSecret (rrSecrets rr))
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


-- * Reading reports again

-- | @verify-transcript REPORT.json [--no-if-send] [--level0-only]@: print
--   the commands a report's transcript issued, each with the end it went to
--   (its transport's role); with @--no-if-send@, fail if a drive's end was
--   issued an IF-SEND; with @--level0-only@, fail if a drive's end was
--   issued anything but Level 0 Discovery's IF-RECV (protocol 0x01, ComID
--   0x0001). A drive's end is any but the simulator's (@tper@): a pair's
--   simulator IF-SENDs count for neither, and an entry that names no end (a
--   report from before ends were recorded) counts as a drive's. An entry
--   that does not read as a report prints one (no kind it has, an issued
--   command whose request does not read) may have been anything, so it
--   counts against both checks: either fails on it.
verifyTranscript :: FilePath -> Bool -> Bool -> IO ExitCode
verifyTranscript path noIfSend level0Only = readReport path >>= \case
  Left e       -> failWith ("verify-transcript: " <> e)
  Right report -> case A.parseEither (A.withObject "report" (A..: "transcript")) report of
    Left e   -> failWith ("verify-transcript: " <> T.pack path <> ": " <> T.pack e)
    Right es -> do
      traverse_ (say . entryLine) es
      let issued    = mapMaybe issuedOnDrive es
          unread    = length [ () | Nothing <- issued ]
          sends     = length [ () | Just (op, _, _) <- issued, op == "if_send" ]
          notLevel0 = length [ () | Just q <- issued, q /= ("if_recv", comPacketProtocol, level0ComId) ]
      say ("IF-SENDs issued on a drive: " <> tshow sends)
      when level0Only $
        say ("commands issued on a drive other than Level 0 Discovery's IF-RECV: " <> tshow notLevel0)
      when (unread > 0) $
        say ("entries that do not read as a report prints them, each counted against both checks: " <> tshow unread)
      pure (if noIfSend && sends + unread > 0 || level0Only && notLevel0 + unread > 0 then ExitFailure 1 else ExitSuccess)
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
    -- a command issued on a drive's end, its op, protocol and ComID, or 'Nothing' for an entry that does not
    -- read; nothing for a note, a blocked command, or one issued on the simulator's end
    issuedOnDrive v = either (const (Just Nothing)) id $ flip A.parseEither v $ A.withObject "entry" $ \o -> do
      kind <- o A..: "entry"
      end  <- o A..:? "end"
      case kind :: Text of
        "note"    -> pure Nothing
        "blocked" -> pure Nothing
        "issued"
          | end == Just ("tper" :: Text) -> pure Nothing
          | otherwise -> o A..: "request" >>= A.withObject "request" (\q ->
              (\op p c -> Just (Just (op :: Text, p :: Word8, c :: Word16))) <$> q A..: "op" <*> q A..: "proto" <*> q A..: "comid")
        _         -> A.parseFail "an entry of no kind a report prints"

-- | @diff A.json B.json@: two reports' Level 0 decodes compared as the Pair
--   runner compares them, A's as the simulator's and B's as the drive's,
--   each report's last end (a pair's drive); a failure where the
--   comparison fails.
diffReports :: FilePath -> FilePath -> IO ExitCode
diffReports a b = do
  la <- level0Of a
  lb <- level0Of b
  case (,) <$> la <*> lb of
    Left e       -> failWith ("diff: " <> e)
    Right (x, y) -> do
      let r = pairLevel0 x y
      traverse_ say (pairLines r)
      pure (if pairFails r then ExitFailure 1 else ExitSuccess)
  where
    level0Of path = readReport path <&> (>>= first (((T.pack path <> ": ") <>) . T.pack) . A.parseEither lastEnd)
    lastEnd = A.withObject "report" $ \o -> do
      ends <- o A..: "level0"
      case L.lastMaybe ends of
        Nothing  -> A.parseFail "the report holds no Level 0 response"
        Just end -> do
          ds <- A.withObject "end" (A..: "response") end >>= A.withObject "response" (A..: "descriptors") >>= traverse descriptor
          pure (Level0 mempty 0 ds mempty [])
    descriptor = A.withObject "descriptor" $ \d -> do
      code    <- d A..: "code"
      version <- d A..: "version"
      len     <- d A..: "len"
      digits  <- d A..: "bytes"
      bytes   <- either A.parseFail pure (B16.decode (encodeUtf8 digits))
      pure (Descriptor code version (bytes <> B.replicate (len - B.length bytes) 0))

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
