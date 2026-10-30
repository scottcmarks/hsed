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
verdicts, each waiver with its reason and page, the decodes, the pair
comparison) and as JUnit XML, through "Hsed.Jig.Files"; and the commands
that read a report back without privilege: @verify-transcript@ (the
commands issued; a failure if an IF-SEND was) and @diff@ (two reports' Level
0 decodes compared as the Pair runner compares them).

The target's part of a report's name is @sim@, @dev-disk6@,
@replay-<file>@, or the two ends' joined by @+@ (@sim+dev-disk6@); the stamp
is the run's start, @yyyymmddThhmmssZ@. Each transcript entry is printed
with a decode: a Level 0 response's length and codes for ComID 1, otherwise
the ComPacket, its packets and subpackets, and each subpacket's tokens read
as a method invocation where they are one; an IF-RECV's ComPacket header
also as the row of Core Table 45 it is. A waiver is printed with the item
and page of its section's list ("Hsed.Jig.Prereq").

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
  ) where

import qualified Data.Aeson             as A
import qualified Data.Aeson.KeyMap      as KM
import qualified Data.Aeson.Types       as A (Parser, parseEither, parseFail)
import qualified Data.ByteString.Base16 as B16
import           Data.Bits              (FiniteBits (..))
import           Data.Time              (UTCTime, defaultTimeLocale, formatTime)
import           Numeric                (showHex)
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
import           Hsed.Jig.Files         (writeOwned)
import           Hsed.Jig.Guard         (comPacketProtocol, level0ComId)
import           Hsed.Jig.Options       (RunOptions (..), TargetSpec (..))
import           Hsed.Jig.Pair          (FieldDiff (..), PairReport (..), allowed, pairFails, pairLevel0)
import           Hsed.Jig.Prereq        (Prereq, Waiver (..), common, listAt, sectionOf)
import           Hsed.Jig.Script        (Verdict (..), describeStatus, t45Of, t45RowOf, verdictText)
import           Hsed.Jig.Transcript    (TEntry (..))
import           Hsed.Level0            (DecodedDescriptor (..), Descriptor (..), Level0 (..), decodeDescriptor, splitLevel0)
import           Hsed.Packet            (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket)
import           Hsed.Sim.Sync          (T45Row (..))
import           Hsed.Sim.Types         (SimParams (..))
import           Hsed.Transport.Drive   (maskSerial)
import           Hsed.Wire              (Describe (..), Dir (..), IfStatus (..), Request (..), Response (..), Unit (..))


-- | What one case came to: its verdict, what it waived, and its extensions'.
data CaseResult = CaseResult
  { crKey        :: !CaseKey
  , crVerdict    :: !Verdict
  , crWaivers    :: ![Waiver]
  , crExtensions :: ![(Text, Verdict)]
  }
  deriving (Eq, Show)

-- | What a run came to.
data RunResult = RunResult
  { rrOptions    :: !RunOptions
  , rrVersion    :: !Text               -- ^ "Hsed.Version"'s
  , rrStarted    :: !UTCTime
  , rrTargets    :: ![Describe]         -- ^ what each transport said it is (two for a pair)
  , rrCases      :: ![CaseResult]
  , rrTranscript :: ![TEntry]
  , rrLevel0     :: ![(Text, Level0)]   -- ^ each end's Level 0 response, by the end's name
  , rrPair       :: !(Maybe PairReport)
  , rrIfSends    :: !Int                -- ^ the IF-SENDs issued on a drive (none at M3)
  , rrExit       :: !ExitCode
  }
  deriving (Eq, Show)


-- * Writing

-- | Write a run's reports into a directory: the files written.
writeReports :: FilePath -> RunResult -> IO [FilePath]
writeReports outDir rr = for files (\(path, bytes) -> path <$ writeOwned path bytes)
  where
    base  = outDir </> ("jig-" <> T.unpack (slug (roTarget (rrOptions rr))) <> "-" <> formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" (rrStarted rr))
    files = [ (base <.> "json", toStrictBytes (A.encode rr))
            , (base <.> "md", encodeUtf8 (markdown rr))
            , (base <.> "xml", encodeUtf8 (junit rr)) ]

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
    , "transcript"     A..= map entryJSON (rrTranscript rr)
    , "level0"         A..= [ A.object ["end" A..= end, "response" A..= l0, "decoded" A..= map decodedJSON (l0Descriptors l0)]
                            | (end, l0) <- rrLevel0 rr ]
    , "pair"           A..= rrPair rr
    , "if_send_issued" A..= rrIfSends rr
    , "exit"           A..= exitNumber (rrExit rr)
    ]
    where
      decodedJSON d = A.object ["code" A..= dCode d, "decode" A..= decodeDescriptor d]

-- | The options a run was given, the expected serial masked.
optionsJSON :: RunOptions -> A.Value
optionsJSON o = A.object
  [ "target"                 A..= renderTarget (roTarget o)
  , "tests"                  A..= roTests o
  , "units"                  A..= spUnits (roSim o)
  , "hold"                   A..= roHold o
  , "inactive_comid_discard" A..= spInactiveComIdDiscard (roSim o)
  , "profile"                A..= roProfile o
  , "report_dir"             A..= roReportDir o
  , "capture"                A..= roCapture o
  , "expect_serial"          A..= fmap maskSerial (roExpectSerial o)
  , "level0_len"             A..= roLevel0Len o
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
  Issued _ q r  -> objectIf (requestDecode q <> responseDecode q r)
  Blocked _ q _ -> objectIf (requestDecode q)
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
  , "- simulator: units " <> unitName (spUnits (roSim o)) <> ", hold " <> tshow (roHold o)
      <> maybe "" ((", profile " <>) . T.pack) (roProfile o)
  , "- IF-SENDs issued on a drive: " <> tshow (rrIfSends rr)
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
  <> transcriptLines (rrTranscript rr)
  where o = rrOptions rr

-- | A case: its verdict and why, each item of its section's list, its
--   extensions.
caseLines :: CaseResult -> [Text]
caseLines cr =
  [ "### " <> caseTitle (crKey cr) <> ": " <> verdictWord (crVerdict cr), "" ]
  <> [ "- " <> r | r <- verdictReasons (crVerdict cr) ]
  <> [ "" | not (null (verdictReasons (crVerdict cr))) ]
  <> [ "Prerequisites, Test Cases " <> fst (listAt section) <> " (PDF " <> tshow (snd (listAt section)) <> "):", "" ]
  <> [ "- " <> prereqLine p | (p, _) <- common section ]
  <> concat [ ["", "Extensions, apart from the TCG verdict:", ""] <> [ "- " <> n <> ": " <> verdictText v | (n, v) <- crExtensions cr ]
            | not (null (crExtensions cr)) ]
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
      <> [ "- problem: " <> tshow p | p <- l0Problems l0 ]
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
  , "- " <> tshow (length (prAllowed r)) <> " differences allowed, " <> tshow (length (prDiffs r)) <> " not"
  , "- " <> if pairFails r then "fail" else "pass"
  , "" ]
  <> concat [ [ "| code | field | sim | drive | allowance | allowed |", "|---|---|---|---|---|---|" ] <> map diffRow ds <> [""]
            | let ds = prAllowed r <> prDiffs r, not (null ds) ]
  where
    diffRow fd = row [ hex (fdCode fd), fdField fd, contentText (fdSim fd), contentText (fdDrive fd)
                     , maybe "none" allowanceText (fdAllowance fd), if allowed fd then "yes" else "no" ]

-- | The transcript, an entry a row.
transcriptLines :: [TEntry] -> [Text]
transcriptLines es =
  [ "## Transcript", "", "| # | entry | command | answer |", "|---|---|---|---|" ]
  <> map entryRow es
  <> [""]
  where
    entryRow = \case
      Issued n q r  -> row [tshow n, "issued", commandText q, answerText q r]
      Blocked n q w -> row [tshow n, "blocked", commandText q, "blocked by the Guard: " <> w]
      Note n t      -> row [tshow n, "note", "", t]

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
--   skip or an unobservable verdict skipped, a failure failed.
junit :: RunResult -> Text
junit rr = T.unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<testsuites name=\"hsed-jig\" " <> counts <> ">"
  , "  <testsuite name=\"hsed-jig " <> attr (renderTarget (roTarget (rrOptions rr))) <> "\" " <> counts
      <> " timestamp=\"" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (rrStarted rr)) <> "\">"
  , "    <properties>"
  , "      <property name=\"hsed\" value=\"" <> attr (rrVersion rr) <> "\"/>"
  , "      <property name=\"if_send_issued\" value=\"" <> tshow (rrIfSends rr) <> "\"/>"
  , "    </properties>"
  ]
  <> concatMap testcase tests
  <> [ "  </testsuite>", "</testsuites>" ]
  where
    target = slug (roTarget (rrOptions rr))
    tests  = [ (caseTitle (crKey cr), crVerdict cr) | cr <- rrCases rr ]
             <> [ (renderCaseKey (crKey cr) <> " extension: " <> n, v) | cr <- rrCases rr, (n, v) <- crExtensions cr ]
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
--   a drive, the reports written, the exit code.
summary :: RunResult -> [FilePath] -> [Text]
summary rr paths =
  [ "hsed-jig " <> renderTarget (roTarget (rrOptions rr)) <> ": " <> T.intercalate "; " (map targetText (rrTargets rr)) ]
  <> concatMap caseText (rrCases rr)
  <> maybe [] (\r -> [ "pair: extra " <> codesText (prExtra r) <> "; missing " <> codesText (prMissing r) <> "; "
                       <> tshow (length (prAllowed r)) <> " differences allowed, " <> tshow (length (prDiffs r)) <> " not"
                       <> if pairFails r then ": fail" else ": pass" ]) (rrPair rr)
  <> [ "IF-SENDs issued on a drive: " <> tshow (rrIfSends rr) ]
  <> [ "reports: " <> T.intercalate ", " (map T.pack paths) | not (null paths) ]
  <> [ "exit " <> tshow (exitNumber (rrExit rr)) ]
  where
    targetText d = dRole d <> " " <> dImpl d <> ", " <> unitName (dUnits d) <> maybe "" (", " <>) (dDevice d)
    caseText cr = (caseTitle (crKey cr) <> ": " <> verdictText (crVerdict cr))
      : map (("  " <>) . waiverLine (crKey cr)) (crWaivers cr)
      <> [ "  extension, " <> n <> ": " <> verdictText v | (n, v) <- crExtensions cr ]


-- * Reading reports again

-- | @verify-transcript REPORT.json [--no-if-send]@: print the commands a
--   report's transcript issued; with the flag, fail if one was an IF-SEND
--   (a pair's transcript holds the simulator's commands too).
verifyTranscript :: FilePath -> Bool -> IO ExitCode
verifyTranscript path noIfSend = readReport path >>= \case
  Left e       -> failWith ("verify-transcript: " <> e)
  Right report -> case A.parseEither (A.withObject "report" (A..: "transcript")) report of
    Left e   -> failWith ("verify-transcript: " <> T.pack path <> ": " <> T.pack e)
    Right es -> do
      traverse_ (say . entryLine) es
      let sends = length (filter issuedSend es)
      say ("IF-SENDs issued: " <> tshow sends)
      pure (if noIfSend && sends > 0 then ExitFailure 1 else ExitSuccess)
  where
    entryLine v = either (const ("an entry not as a report prints one: " <> decodeUtf8Lenient (toStrictBytes (A.encode v))))
                         id (A.parseEither entryText v)
    entryText = A.withObject "entry" $ \o -> do
      n    <- o A..: "n" :: A.Parser Int
      kind <- o A..: "entry"
      case kind :: Text of
        "note" -> (\t -> "#" <> tshow n <> " note: " <> t) <$> o A..: "text"
        _      -> do
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
          pure ("#" <> tshow n <> " " <> kind <> " " <> command <> ": " <> outcome)
    issuedSend v = either (const False) id $ flip A.parseEither v $ A.withObject "entry" $ \o -> do
      kind <- o A..: "entry"
      if kind /= ("issued" :: Text)
        then pure False
        else o A..: "request" >>= A.withObject "request" (\q -> (== ("if_send" :: Text)) <$> q A..: "op")

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

hex :: (Integral a, FiniteBits a) => a -> Text
hex n = "0x" <> T.justifyRight (finiteBitSize n `div` 4) '0' (T.pack (showHex n ""))

say :: Text -> IO ()
say t = hPutBuilder stdout (getUtf8Builder (display t <> "\n"))

failWith :: Text -> IO ExitCode
failWith t = hPutBuilder stderr (getUtf8Builder (display t <> "\n")) >> pure (ExitFailure 1)
