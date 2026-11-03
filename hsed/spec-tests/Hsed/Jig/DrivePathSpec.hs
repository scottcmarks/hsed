{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.DrivePathSpec
Description : Test specifications for a drive's whole path without the drive: hsed-jig's runs over the fake kext, through the kext transport and the real C of the CDBs, the simulator where the kernel and the drive would be
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

M4c's acceptance without the drive (design 11): the jig's runs as a run
opens a drive ("Hsed.Jig.Support".'onFakeDrive': the drive behind the
Guard, Level0Only until its base ComID is bound), over the fake kext
("Hsed.Transport.FakeKext"), so that every command the Guard lets through
goes down a drive's path: the kext transport
("Hsed.Transport.Kext".'Hsed.Transport.Kext.kextExchange', its judge and
its latch), the real portable C of the CDBs (@hsed_cdb_build@, and
@hsed_cdb_check_payload@ on the very buffer), and the simulator where the
kernel and the drive would be. Each item runs both ways a drive's run has
its cases' SyncIdle and NoSessions (@roStackResetBeforeCases@): with
STACK_RESET before each case, the way shipped (Scott's decision of
2026-11-02, design 13.7), and with SyncIdle probed by an IF-RECV and
NoSessions waived, the way kept built.

* @--tests UCT-02,UCT-03:1a-1c,SPF-02:1@ exits 0, each case passing, and
  the CDBs the kext was issued are the golden SAT lists, byte for byte: 16
  with STACK_RESET before each case, 13 without; every command the run
  issued is one of them, answered Good; nothing outside the drive's policy
  (AllowList 0x1004) and nothing the Guard refused. @verify-transcript
  --allow-list@ passes the run's report, and fails it with one drive
  IF-RECV moved onto ComID 0x1005 (refused by M4c's Guard alone, which
  holds an IF-RECV to the base) or one drive IF-SEND made a Set.
* @--interrupt-in-session@ with UCT-03:1a-1c exits 130, the session left
  open on 0x1004; then @--recover@ exits 0, Level 0, STACK_RESET's pair, an
  idle IF-RECV, StartSession and its End of Session, the session gone and
  its report passing @--allow-list@; then the acceptance exits 0 with its
  golden CDBs: one TPer, a user client of the kext's opened anew for each
  run, as each run of the jig opens it.
* A kext that answers the first RESET_SEND kIOReturnError: with STACK_RESET
  before each case the run exits 1, every command after it answered a
  transport failure and the shim not called again (the latch); without,
  the acceptance passes, sending none; @--recover@ exits 1 either way, not
  recovered.
* A kext that says it moved none of the 512 bytes of the first ComPacket's
  IF-SEND, its kern_return success: the command is judged a residue that
  reached the drive, design 13.29 (d) named in the report; the commands
  after it are answered transport failures, none reaching the kext; and the
  run exits 1.
* A kext that answers UCT-03's Get a second after it was asked, the host
  waiting 50 ms for each of the drive's commands (the M4c review): the
  answer judged and recorded, the exchange given up all the same, and the
  drive latched, so that nothing more reaches the kext, End of Session and
  STACK_RESET included, each command after noted as not issued; the run
  exits 1, naming the session the drive may still hold for @--recover@.

The golden CDBs are ATA PASS-THROUGH (12), A1h: TRUSTED RECEIVE (5Ch, PIO
Data-In, bytes 1-2 08h 0Eh) or TRUSTED SEND (5Eh, PIO Data-Out, 0Ah 06h),
byte 3 the security protocol, byte 4 the 512-byte blocks, bytes 6 and 7
the ComID's low and high byte (the bytes SAT-5's and ACS-5's, SIIS 1.20's
references [21] and [22], PDF 17; the fields' values its Tables 16-17, PDF
40-41): Level 0
Discovery, 4 blocks on ComID 0x0001 (Core Table 26, PDF 55; 3.3.6, PDF
67); a ComPacket's IF-SEND, one block on the base 0x1004, and an IF-RECV
there of 2048 bytes, the host's MaxComPacketSize as Opal assumes it before
Properties and STACK_RESET puts it back (Opal 2.30 Table 17, PDF 33; Core
3.3.4.7.5 item 7, PDF 65); STACK_RESET's HANDLE_COMID_REQUEST and
GET_COMID_RESPONSE, protocol 2, one block each (Core Tables 29-30, PDF
62-63).
-}
module Hsed.Jig.DrivePathSpec (spec) where

import qualified Data.Aeson              as A
import qualified Data.Aeson.Key          as K
import qualified Data.Aeson.KeyMap       as KM
import           GHC.IO.Handle           (hDuplicate, hDuplicateTo)
import qualified RIO.ByteString          as B
import qualified RIO.List                as L
import           RIO
import           RIO.FilePath            ((</>))
import qualified RIO.Text                as T
import qualified RIO.Text.Partial        as T'
import           Test.Hspec

import           Hsed.Core.Method        (invocation)
import           Hsed.Core.StreamItem    (StreamItem (..))
import           Hsed.Core.Value         (List (..), Value (..), (.:))
import qualified Hsed.Gen.Core.Methods   as Methods
import           Hsed.Gen.Core.UIDs      (uC_PIN_SID)
import           Hsed.Jig.Catalogue      (CaseKey, parseCaseKeys)
import           Hsed.Jig.Options        (RunOptions (..), defaultRunOptions)
import           Hsed.Jig.Report         (verifyTranscriptWith)
import           Hsed.Jig.Script         (controlComPacket)
import           Hsed.Jig.Support        (at, atPath, onFakeDrive)
import           Hsed.Sim.State          (lifeOf)
import           Hsed.Sim.Types          (ComIdLife (Associated), SimParams (..), defaultSimParams)
import           Hsed.Transport.Cdb      (Shape (..), cdbHex)
import           Hsed.Transport.Drive    (InterfaceType (..))
import           Hsed.Transport.FakeKext (FakeFaults (..), fakeDriveInfo, fakeKextOn, noFaults)
import           Hsed.Transport.InProc   (InProcOpts (..), SimControl (..), newInProc)
import           Hsed.Wire.Internal      (Cap (..), Dir (..), Request (..), Transport, Unit (..))


-- * The runs

-- | Case keys as @--tests@ takes them.
keys :: Text -> [CaseKey]
keys = either (error . T.unpack) id . parseCaseKeys

-- | M4c's acceptance on a drive, with STACK_RESET before each case or not.
acceptance :: Bool -> RunOptions
acceptance reset = defaultRunOptions { roTests = keys "UCT-02,UCT-03:1a-1c,SPF-02:1", roStackResetBeforeCases = reset }

-- | UCT-03:1a-1c stopped once StartSession has opened its session.
interrupting :: Bool -> RunOptions
interrupting reset = defaultRunOptions { roTests = keys "UCT-03:1a-1c", roStackResetBeforeCases = reset, roInterruptInSession = True }

-- | UCT-03:1a-1c, the host waiting 50 ms for each of the drive's commands.
impatient :: Bool -> RunOptions
impatient reset = defaultRunOptions { roTests = keys "UCT-03:1a-1c", roStackResetBeforeCases = reset, roDriveTimeoutMicros = 50000 }

-- | @--recover@, alone on the drive.
recovering :: Bool -> RunOptions
recovering reset = defaultRunOptions { roRecover = True, roStackResetBeforeCases = reset }

-- | What each way of a drive's SyncIdle and NoSessions is called.
way :: Bool -> String
way = \case
  True  -> "with STACK_RESET before each case (roStackResetBeforeCases True, shipped)"
  False -> "with SyncIdle probed and NoSessions waived (roStackResetBeforeCases False)"

-- | A TPer of the simulator, in 512-byte blocks as the kext's transport
--   counts them.
newTPer :: IO (Transport 'ReadWrite, SimControl)
newTPer = newInProc defaultSimParams { spUnits = UnitBlocks512 } (InProcOpts UnitBlocks512 0)

-- | A run over the fake kext's user client, opened anew over a TPer with
--   some faults, as a run opens a drive (Support's 'onFakeDrive'): its exit
--   code, its JSON report, and the CDBs the kext was issued, in order, as
--   the jig prints them.
overKext :: FakeFaults -> (Transport 'ReadWrite, SimControl) -> RunOptions -> IO (ExitCode, A.Value, [Text])
overKext faults (sim, control) opts = do
  cdbs <- newIORef []
  raw <- fakeKextOn IfSAT sim faults cdbs
  (code, json) <- onFakeDrive opts (raw, fakeDriveInfo, control)
  (,,) code json . map cdbHex <$> readIORef cdbs

-- | Whether a session holds 0x1004, the TPer's base ComID.
held :: SimControl -> IO Bool
held control = (== Just Associated) . lifeOf 0x1004 <$> scPeek control


-- * The golden CDBs

-- | Level 0 Discovery's IF-RECV, 4 blocks on ComID 0x0001; an IF-RECV of 4
--   blocks on 0x1004 and a ComPacket's IF-SEND of one; STACK_RESET's
--   HANDLE_COMID_REQUEST and GET_COMID_RESPONSE on 0x1004, one block each.
level0Cdb, recvCdb, sendCdb, resetSendCdb, resetRecvCdb :: Text
level0Cdb    = "a1 08 0e 01 04 00 01 00 00 5c 00 00"
recvCdb      = "a1 08 0e 01 04 00 04 10 00 5c 00 00"
sendCdb      = "a1 0a 06 01 01 00 04 10 00 5e 00 00"
resetSendCdb = "a1 0a 06 02 01 00 04 10 00 5e 00 00"
resetRecvCdb = "a1 08 0e 02 01 00 04 10 00 5c 00 00"

-- | Before each case: STACK_RESET's pair, or the IF-RECV that probes the
--   base.
beforeCase :: Bool -> [Text]
beforeCase reset = if reset then [resetSendCdb, resetRecvCdb] else [recvCdb]

-- | The acceptance's CDBs: Level 0; then each case after what comes before
--   it ('beforeCase'): UCT-02's Properties, sent and received; UCT-03:1a-1c's
--   StartSession, Get and End of Session, each sent and received; SPF-02:1's
--   IF-RECV. 16 with STACK_RESET before each case, 13 without.
golden :: Bool -> [Text]
golden reset = concat
  [ [level0Cdb]
  , beforeCase reset, exchanged 1
  , beforeCase reset, exchanged 3
  , beforeCase reset, [recvCdb] ]
  where exchanged n = concat (replicate n [sendCdb, recvCdb])

-- | UCT-03:1a-1c's CDBs, stopped once StartSession has opened its session:
--   Level 0, what comes before the case, StartSession sent and received.
interruptedCdbs :: Bool -> [Text]
interruptedCdbs reset = [level0Cdb] <> beforeCase reset <> [sendCdb, recvCdb]

-- | @--recover@'s CDBs: Level 0; STACK_RESET's pair; the IF-RECV that finds
--   the base idle; StartSession and End of Session, each sent and received.
recoverCdbs :: [Text]
recoverCdbs = [level0Cdb, resetSendCdb, resetRecvCdb, recvCdb, sendCdb, recvCdb, sendCdb, recvCdb]


-- * Reports

-- | A JSON array's elements.
elements :: Maybe A.Value -> [A.Value]
elements = \case
  Just (A.Array xs) -> toList xs
  _                 -> []

-- | The commands a report's transcript says the run issued, in order: each
--   one's answer, its status and the detail of a failure.
answers :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
answers json = [ (atPath ["response", "status"] e, atPath ["response", "detail"] e)
               | e <- elements (at "transcript" json), at "entry" e == Just "issued" ]

-- | Each case's verdict.
verdicts :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
verdicts json = [ (at "key" c, at "verdict" c) | c <- elements (at "cases" json) ]

-- | Whether a JSON value is a text holding another.
holding :: Text -> Maybe A.Value -> Bool
holding t = \case
  Just (A.String s) -> t `T.isInfixOf` s
  _                 -> False

-- | A JSON object's field edited.
field :: Text -> (A.Value -> A.Value) -> A.Value -> A.Value
field k f = \case
  A.Object o -> A.Object (KM.insert (K.fromText k) (f (fromMaybe A.Null (KM.lookup (K.fromText k) o))) o)
  v          -> v

-- | A report with the first command its transcript says the drive was
--   issued whose request holds edited.
editDriveRequest :: (A.Value -> Bool) -> (A.Value -> A.Value) -> A.Value -> A.Value
editDriveRequest holds edit = field "transcript" (A.toJSON . firstOnly . elements . Just)
  where
    firstOnly es = case break driveCommand es of
      (earlier, e : later) -> earlier <> (field "request" edit e : later)
      (earlier, [])        -> earlier
    driveCommand e = at "entry" e == Just "issued" && at "end" e == Just "drive" && maybe False holds (at "request" e)

-- | Whether a request, as a report prints it, has this op, protocol and
--   ComID.
isCommand :: Text -> Int -> Int -> A.Value -> Bool
isCommand op p c q = (at "op" q, at "proto" q, at "comid" q) == (Just (A.String op), number p, number c)
  where number = Just . A.Number . fromIntegral

-- | @verify-transcript --allow-list@ over a report's JSON, written to a file
--   of its own in a directory: its exit code, and the counts it prints last
--   (after the methods it re-decided the drive's commands under).
allowList :: FilePath -> String -> A.Value -> IO (ExitCode, Text)
allowList tmp name json = do
  let file = tmp </> (name <> ".json")
  writeFileBinary file (toStrictBytes (A.encode json))
  (code, out) <- capturing (verifyTranscriptWith file False False True)
  pure (code, maybe out (snd . T'.breakOnEnd "]: ") (L.lastMaybe (T.lines out)))

-- | The counts @--allow-list@ prints last for a report whose drive's
--   commands all read and none was blocked: so many admitted, so many
--   refused.
counted :: Int -> Int -> Text
counted admitted refused =
  tshow admitted <> " admitted, " <> tshow refused <> " refused, 0 that do not read; blocked by the run's Guard: 0"

-- | What an action prints to stdout, with what it prints to stderr thrown
--   away; and what it came to.
capturing :: IO a -> IO (a, Text)
capturing act = withSystemTempFile "hsed-stdout" $ \path h ->
  withFile "/dev/null" WriteMode $ \devNull -> do
    hFlush stdout >> hFlush stderr
    r <- bracket ((,) <$> hDuplicate stdout <*> hDuplicate stderr)
                 (\(out, err) -> do
                    hFlush stdout >> hFlush stderr
                    hDuplicateTo out stdout >> hDuplicateTo err stderr
                    hClose out >> hClose err)
                 (\_ -> hDuplicateTo h stdout >> hDuplicateTo devNull stderr >> act)
    hClose h
    (,) r <$> readFileUtf8 path

-- | Set on C_PIN_SID's PIN, in a block on 0x1004 as the drive's
--   ComPackets are sent: a method no drive's policy admits.
setSend :: Request
setSend = Request IfSend 1 0x1004 512 (bytes <> B.replicate (512 - B.length bytes) 0)
  where
    bytes  = either (error . show) id (controlComPacket 0x1004 (generate setPin))
    setPin = invocation uC_PIN_SID (Methods.Set Nothing (Just (L (List [N ((3 :: Natural) .: ("not the PIN" :: ByteString))]))))


spec :: Spec
spec = describe "a drive's path without the drive: hsed-jig over the fake kext, through the kext transport and the real C of the CDBs" $
  for_ [True, False] $ \reset -> describe (way reset) $ do

    it ("--tests UCT-02,UCT-03:1a-1c,SPF-02:1: exit 0; the kext issued the " <> show (length (golden reset)) <> " golden SAT CDBs, "
        <> "every command of the run one of them, answered; nothing outside the drive's policy, nothing refused") $ do
      (code, json, cdbs) <- newTPer >>= \tper -> overKext noFaults tper (acceptance reset)
      code `shouldBe` ExitSuccess
      verdicts json `shouldBe` [ (Just (A.String k), Just "pass") | k <- ["UCT-02", "UCT-03:1a-1c", "SPF-02:1"] ]
      cdbs `shouldBe` golden reset
      answers json `shouldBe` replicate (length (golden reset)) (Just "good", Nothing)
      [ (at "policy" p, at "base_comid" p) | p <- elements (at "policies" json) ] `shouldBe` [(Just "allow_list", Just (A.Number 0x1004))]
      (at "outside_policy" json, at "blocked" json) `shouldBe` (Just (A.Array mempty), Just (A.Number 0))

    it "verify-transcript --allow-list: 0 on the run's report; 1 with a drive IF-RECV moved onto ComID 0x1005, or a drive IF-SEND made a Set" $
      withSystemTempDirectory "hsed-drive-path" $ \tmp -> do
        (code, json, _) <- newTPer >>= \tper -> overKext noFaults tper (acceptance reset)
        code `shouldBe` ExitSuccess
        let n         = length (golden reset)
            elsewhere = editDriveRequest (isCommand "if_recv" 1 0x1004) (field "comid" (const (A.Number 0x1005))) json
            aSet      = editDriveRequest (isCommand "if_send" 1 0x1004) (const (A.toJSON setSend)) json
        [elsewhere, aSet] `shouldSatisfy` all (/= json)
        allowList tmp "run" json `shouldReturn` (ExitSuccess, counted n 0)
        for_ [("recv-0x1005", elsewhere), ("set", aSet)] $ \(name, edited) ->
          ((,) name <$> allowList tmp name edited) `shouldReturn` (name, (ExitFailure 1, counted (n - 1) 1))

    it "--interrupt-in-session with UCT-03:1a-1c: 130, the session left open; then --recover: 0, the session gone; then the acceptance: 0; one TPer, the kext's user client opened anew for each run" $
      withSystemTempDirectory "hsed-drive-path" $ \tmp -> do
        tper@(_, control) <- newTPer
        (stopped, _, cdbs) <- overKext noFaults tper (interrupting reset)
        (stopped, cdbs) `shouldBe` (ExitFailure 130, interruptedCdbs reset)
        held control `shouldReturn` True
        (recovered, report, cdbs') <- overKext noFaults tper (recovering reset)
        (recovered, cdbs', atPath ["recover", "ok"] report) `shouldBe` (ExitSuccess, recoverCdbs, Just (A.Bool True))
        held control `shouldReturn` False
        allowList tmp "recover" report `shouldReturn` (ExitSuccess, counted (length recoverCdbs) 0)
        (code, _, cdbs'') <- overKext noFaults tper (acceptance reset)
        (code, cdbs'') `shouldBe` (ExitSuccess, golden reset)

    it (if reset
          then "a kext answering the first RESET_SEND kIOReturnError: the run exits 1, every command after it a transport failure, the shim not called again; --recover exits 1"
          else "a kext answering the first RESET_SEND kIOReturnError: the acceptance passes, sending none; --recover exits 1") $ do
      let failing = noFaults { ffKernReturn = Just (ResetSend, 1, 0xe00002bc) }
      (code, json, cdbs) <- newTPer >>= \tper -> overKext failing tper (acceptance reset)
      if reset
        then do
          code `shouldBe` ExitFailure 1
          cdbs `shouldBe` [level0Cdb, resetSendCdb]
          case answers json of
            (Just "good", Nothing) : (Just "transport_failure", failed) : later -> do
              failed `shouldSatisfy` holding "kIOReturnError (0xe00002bc) for STACK_RESET's HANDLE_COMID_REQUEST"
              later `shouldNotBe` []
              later `shouldSatisfy` all (\(st, why) -> st == Just "transport_failure" && holding "a command failed before (" why)
            other -> expectationFailure ("not Level 0, then the failed RESET_SEND: " <> show other)
        else (code, cdbs) `shouldBe` (ExitSuccess, golden False)
      (recovered, report, cdbs') <- newTPer >>= \tper -> overKext failing tper (recovering reset)
      (recovered, cdbs', atPath ["recover", "ok"] report) `shouldBe` (ExitFailure 1, [level0Cdb, resetSendCdb], Just (A.Bool False))
      atPath ["recover", "text"] report `shouldSatisfy` holding "kIOReturnError (0xe00002bc)"

    it "a kext answering UCT-03's Get after the host gave up on it: the answer recorded, the drive latched, nothing more reaching the kext (no End of Session, no STACK_RESET), each command after noted not issued; exit 1, the session named for --recover" $ do
      -- the Get's answer is the second ComPacket IF-RECV with STACK_RESET before the case, the third after the idle probe
      -- a second against 50 ms, so that a loaded runner's timer has the rest to fire in
      let slow = noFaults { ffSlow = Just (PacketRecv, if reset then 2 else 3, 1000000) }
      tper@(_, control) <- newTPer
      (code, json, cdbs) <- overKext slow tper (impatient reset)
      code `shouldBe` ExitFailure 1
      cdbs `shouldBe` [level0Cdb] <> beforeCase reset <> [sendCdb, recvCdb, sendCdb, recvCdb]
      answers json `shouldBe` replicate (length cdbs) (Just "good", Nothing)
      let notesOf = [ t | e <- elements (at "transcript" json), at "entry" e == Just "note", Just (A.String t) <- [at "text" e] ]
      [ t | t <- notesOf, "not issued on the drive, an IF-SEND of protocol 0x01 on ComID 0x1004: the host gave up on an exchange on 0x1004" `T.isPrefixOf` t ]
        `shouldSatisfy` (not . null)
      [ r | c <- elements (at "cases" json), A.String r <- elements (at "reasons" c) ] `shouldSatisfy` any ("1b: " `T.isPrefixOf`)
      [ p | A.String p <- elements (at "problems" json) ]
        `shouldSatisfy` any (\p -> "the drive may hold session TSN " `T.isPrefixOf` p && "hsed-jig --recover" `T.isSuffixOf` p)
      held control `shouldReturn` True

    it "a kext saying it moved none of the 512 bytes of the first ComPacket's IF-SEND, kern_return success: a residue that reached the drive, design 13.29 (d) named; nothing more reaches the kext; exit 1" $ do
      (code, json, cdbs) <- newTPer >>= \tper -> overKext noFaults { ffMoveNone = Just (PacketSend, 1) } tper (acceptance reset)
      code `shouldBe` ExitFailure 1
      cdbs `shouldBe` [level0Cdb] <> beforeCase reset <> [sendCdb]
      let (earlier, rest) = L.splitAt (length cdbs - 1) (answers json)
      map fst earlier `shouldSatisfy` all (== Just "good")
      case rest of
        (Just "transport_failure", residue) : later -> do
          for_ [ "the kext moved 0 of the 512 bytes of a ComPacket's IF-SEND", "the command reached the drive, but not as hsed asked it"
               , "(design 13.29 (d))", "settle design 13.29 (d) before another run" ] $ \words' ->
            residue `shouldSatisfy` holding words'
          later `shouldNotBe` []
          later `shouldSatisfy` all (\(st, why) -> st == Just "transport_failure" && holding "a command failed before (" why)
        other -> expectationFailure ("not the residue, then failures: " <> show other)
