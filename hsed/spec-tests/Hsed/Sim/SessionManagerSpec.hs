{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Sim.SessionManagerSpec
Description : Test specifications for the simulated Session Manager: SMUID.Properties stateful, StartSession and SyncSession, what it ignores
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Milestone M4a's group G2 (design 11). Properties: the simulator answers the
invocation the jig's UCT-02 sends (Test Cases PDF 31-32), HostProperties
MaxComPacketSize 4096, MaxPacketSize 4076 and MaxIndTokenSize 4040, with the
eleven TPer properties Opal 2.30 Table 17 has it report (PDF 33-34), each at
least the table's minimum, DefSessionTimeout among them, and the six host
properties it supports with their current values, kept on the invoking ComID
alone and accumulated over invocations (Core 5.2.2.3-4, PDF 143-145): a value
below Table 17's least raised to it, 0 never raised (no limit, Core Table
167; the lead's M4a decision), the run's maximum bounding both, an unsupported
name ignored and not returned; ETC-02's and ETC-03's invocation without
HostProperties changes nothing. StartSession (Core 5.2.3.1, PDF 149) is
answered by SyncSession (5.2.3.2): the Admin SP opened with HostSessionID 1
and SPSessionID 4096, the statuses of 5.1.5 (PDF 137-139) for a second
session to the SP, the Locking SP Manufactured-Inactive, an unknown SPID, a
read-only session or an unsupported parameter, as SID the PIN authenticated
with the Tries rule (5.3.4.1.1.2, PDF 210-211), a class, a disabled
authority, Anybody with a challenge, and the TPer's MaxSessions. A host's
abort is echoed in the normal form (3.2.4.2, PDF 49) and the rest is no
answer (3.3.7.1.3, PDF 73-74), the TPer unchanged either way.
-}
module Hsed.Sim.SessionManagerSpec (spec) where

import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck          (counterexample, elements, forAll, listOf, (.&&.), (===))

import           Hsed.Core.Call           (MethodCall (..), MethodHeader (..), Parameters (..), optionalParameter)
import           Hsed.Core.Method         (fromCall, invocation)
import           Hsed.Core.Properties     (Assumptions (..), Communicator (..), Property (..), PropertyRequirement (..),
                                           TPerRequirement (..))
import           Hsed.Core.StreamItem     (StreamItem (..))
import           Hsed.Core.Table          (Cell (..))
import           Hsed.Core.Token          (IsToken (..), Token (..))
import           Hsed.Core.UID            (UID, uid)
import           Hsed.Core.Value          (Datum (..), IsValue (..), List (..), Name (..), NamedValue (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes      (StatusCode (..), pattern AUTHORITY_LOCKED_OUT, pattern INVALID_PARAMETER,
                                           pattern NOT_AUTHORIZED, pattern NO_SESSIONS_AVAILABLE, pattern SP_BUSY,
                                           pattern SUCCESS)
import qualified Hsed.Gen.Core.Methods    as Methods
import           Hsed.Gen.Core.Properties (initialAssumptions, properties)
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs  (uC_PINTable)
import           Hsed.Gen.Core.UIDs       (uAuthority_Admins, uAuthority_Anybody, uAuthority_SID, uC_PIN_SID, uProperties,
                                           uSMUID, uSP_Admin, uStartSession, uThisSP)
import           Hsed.Gen.Opal.Properties (propertyRequirements)
import           Hsed.OpalUIDs            (uAuthority_Admin1, uSP_Locking)
import           Hsed.Sim.SessionManager  (answer)
import           Hsed.Sim.State           (abortSessions, cellOf, columnOf, lifeOf, rowWithUID, sessionsOn)
import           Hsed.Sim.Types           (ComIdLife (..), ComIdState (..), LifeCycle (..), Session (..), SimParams (..),
                                           TPerState (..), defaultSimParams, hostRequirements, initialHostProperties,
                                           initialState, maxComPacketSizeName, maxIndTokenSizeName, maxMethodsName,
                                           maxPacketSizeName, maxPacketsName, maxResponseComPacketSizeName,
                                           maxSessionsName, maxSubpacketsName, tperRequirements)


spec :: Spec
spec = describe "the simulated Session Manager" $ do
  describe "SMUID.Properties with no HostProperties (ETC-02, ETC-03)" $ do
    let call = invocation uSMUID (Methods.Properties Nothing)
    it "is answered by a Properties invocation, status SUCCESS, which the codec reads back" $
      withAnswer defaultSimParams call $ \r -> do
        (parseByteString (generate r) :: Either String MethodCall) `shouldBe` Right r
        _header r `shouldBe` MethodHeader uSMUID uProperties
        _statusCode r `shouldBe` 0
        _optional (_parameters r) `shouldBe` []
    it "reports MaxComPacketSize 2048, and every name is a property Core Table 167 gives the TPer" $
      withAnswer defaultSimParams call $ \r -> do
        let reported = tperProperties r
        L.lookup "MaxComPacketSize" reported `shouldBe` Just 2048
        filter (`notElem` tperNames) (map fst reported) `shouldBe` []
    it "reports the parameters' properties, each of them" $
      withAnswer defaultSimParams call $ \r ->
        tperProperties r `shouldBe` [ (n, fromInteger v) | (n, v) <- spTPerProperties defaultSimParams ]
    it "leaves out a property Core Table 167 does not give the TPer, and one no uinteger holds" $ do
      let odd' = defaultSimParams { spTPerProperties = [("MaxComPacketSize", 4096), ("Bogus", 1), ("MaxSessions", -1)] }
      withAnswer odd' call $ \r -> tperProperties r `shouldBe` [("MaxComPacketSize", 4096)]
    it "changes nothing: no HostProperties, none come back and the TPer is as it was" $ do
      let (r, st') = answer base call st0
      fmap hostProperties r `shouldBe` Just Nothing
      st' `shouldBe` st0

  describe "SMUID.Properties with UCT-02's HostProperties (Test Cases PDF 32)" $ do
    let call = propertiesWith [(maxComPacketSizeName, 4096), (maxPacketSizeName, 4076), (maxIndTokenSizeName, 4040)]
        (r1, st1) = answer base call st0
    it "reports eleven TPer properties, each at least Opal 2.30 Table 17's minimum, DefSessionTimeout among them (UCT-02 step 2)" $ do
      let reported = maybe [] tperProperties r1
      length reported `shouldBe` 11
      map fst reported `shouldBe` map fst tperRequirements
      [ (n, v, least) | (n, v) <- reported, Just least <- [tperMinimum n], v < least ] `shouldBe` []
      L.lookup "DefSessionTimeout" reported `shouldBe` Just 0
      [ reqName pr | pr <- propertyRequirements, Just (TPerRequirement _ _) <- [reqTPer pr] ] `shouldBe` map fst reported
    it "answers the six host properties it supports with their current values, in Table 17's order: 4096, 4076, 4040, 1, 1, 1" $ do
      fmap hostProperties r1 `shouldBe` Just (Just (zip supportedNames [4096, 4076, 4040, 1, 1, 1]))
      fmap (parseByteString . generate) r1 `shouldBe` fmap Right r1
    it "keeps them on the invoking ComID alone, the others' as they were (Core 5.2.2.1)" $ do
      let two      = initialState defaultSimParams { spNComIds = 2 }
          (_, two') = answer base call two
      hostPropsOn base two' `shouldBe` Map.fromList (zip supportedNames [4096, 4076, 4040, 1, 1, 1])
      hostPropsOn (base + 1) two' `shouldBe` initialHostProperties
      hostPropsOn base st1 `shouldBe` Map.fromList (zip supportedNames [4096, 4076, 4040, 1, 1, 1])
    it "accumulates: a later MaxPackets 2 supersedes that property alone (Core 5.2.2.3)" $ do
      let (r2, st2) = answer base (propertiesWith [(maxPacketsName, 2)]) st1
      fmap hostProperties r2 `shouldBe` Just (Just (zip supportedNames [4096, 4076, 4040, 2, 1, 1]))
      Map.lookup maxPacketsName (hostPropsOn base st2) `shouldBe` Just 2
      Map.lookup maxComPacketSizeName (hostPropsOn base st2) `shouldBe` Just 4096
    it "raises a value below Table 17's least to it: MaxComPacketSize 1000 becomes 2048 (Core 5.2.2.3-4)" $ do
      let (r, st') = answer base (propertiesWith [(maxComPacketSizeName, 1000)]) st0
      fmap (fmap (L.lookup maxComPacketSizeName) . hostProperties) r `shouldBe` Just (Just (Just 2048))
      Map.lookup maxComPacketSizeName (hostPropsOn base st') `shouldBe` Just 2048
    it "never raises 0, which is no limit wherever Core Table 167 says so (the lead's M4a decision)" $ do
      let (r, st') = answer base (propertiesWith [(maxPacketsName, 0), (maxComPacketSizeName, 0)]) st0
      fmap (fmap (L.lookup maxPacketsName) . hostProperties) r `shouldBe` Just (Just (Just 0))
      Map.lookup maxComPacketSizeName (hostPropsOn base st') `shouldBe` Just 0
    it "bounds a value, and a 0, by the most the run accepts (spHostPropertyMax)" $ do
      let capped = initialState defaultSimParams { spHostPropertyMax = [(maxComPacketSizeName, 8192)] }
          (r, _)  = answer base (propertiesWith [(maxComPacketSizeName, 0), (maxPacketSizeName, 0)]) capped
          (r', _) = answer base (propertiesWith [(maxComPacketSizeName, 10000)]) capped
      fmap (fmap (L.lookup maxComPacketSizeName) . hostProperties) r `shouldBe` Just (Just (Just 8192))
      fmap (fmap (L.lookup maxPacketSizeName) . hostProperties) r `shouldBe` Just (Just (Just 0))
      fmap (fmap (L.lookup maxComPacketSizeName) . hostProperties) r' `shouldBe` Just (Just (Just 8192))
    it "ignores an unsupported name, MaxResponseComPacketSize (N) among them, and does not return it (Core 5.2.2.4; Opal Table 17)" $ do
      let (r, st') = answer base (propertiesWith [("NoSuchProperty", 7), (maxResponseComPacketSizeName, 4096), (maxMethodsName, 3)]) st0
      fmap hostProperties r `shouldBe` Just (Just (zip supportedNames [2048, 2028, 1992, 1, 1, 3]))
      Map.keys (hostPropsOn base st') `shouldBe` L.sort supportedNames
    it "reports host properties Core Table 167 gives the host, none below Core Table 168's assumption" $ do
      let reported    = fromMaybe [] (r1 >>= hostProperties)
          tperAssumes = concat [ a | Assumptions TPer Host a <- initialAssumptions ]
      filter (`notElem` hostNames) (map fst reported) `shouldBe` []
      [ (n, v, a) | (n, v) <- reported, Just a <- [L.lookup n tperAssumes], v < a ] `shouldBe` []
    it "leaves a session already open with the properties it started with (Core 5.2.2.3)" $ do
      let (_, opened) = answer base (startCall uSP_Admin Nothing Nothing) st0
          (_, later)  = answer base call opened
      map sesHostProps (sessionsOn base later) `shouldBe` [initialHostProperties]
      Map.lookup maxComPacketSizeName (hostPropsOn base later) `shouldBe` Just 4096

  describe "the properties it and the jig read" $
    it "are named as Core Table 167 names them, and Opal Table 17's host requirements as well" $ do
      filter (`notElem` map propertyName properties)
        [maxComPacketSizeName, maxResponseComPacketSizeName, maxPacketSizeName, maxPacketsName, maxSubpacketsName, maxMethodsName, maxSessionsName]
        `shouldBe` []
      filter (`notElem` hostNames) (map fst hostRequirements) `shouldBe` []
      map fst hostRequirements `shouldBe` supportedNames

  describe "what is no answer, the TPer unchanged (Core 3.3.7.1.3, 3.2.4.2 MAY)" $ do
    let call = invocation uSMUID (Methods.Properties Nothing)
        unanswered c = answer base c st0 `shouldBe` (Nothing, st0)
    it "a host's SyncSession, CloseSession or StartTrustedSession, the TPer's to send or unsupported (Opal 4.1.1)" $ do
      unanswered (invocation uSMUID (Methods.SyncSession (value (1 :: Natural)) (value (4096 :: Natural)) Nothing Nothing Nothing Nothing Nothing Nothing))
      unanswered (invocation uSMUID (Methods.CloseSession (value (1 :: Natural)) (value (4096 :: Natural))))
      unanswered (invocation uSMUID (Methods.StartTrustedSession (value (1 :: Natural)) (value (4096 :: Natural)) Nothing Nothing Nothing Nothing))
      unanswered call { _header = MethodHeader uSMUID uStartSession }
    it "Properties or StartSession invoked on another object than SMUID" $ do
      unanswered call { _header = MethodHeader uThisSP uProperties }
      unanswered (startCall uSP_Admin Nothing Nothing) { _header = MethodHeader uThisSP uStartSession }
    it "a required parameter, Properties having none (ETC-08's StartList after StartList)" $
      unanswered call { _parameters = Parameters [L (List [])] [] }
    it "an optional parameter other than HostProperties" $
      unanswered call { _parameters = Parameters [] [optionalParameter 1 (List [])] }
    it "HostProperties other than Named uintegers" $ do
      unanswered (invocation uSMUID (Methods.Properties (Just (D (Datum (Unsigned 3))))))
      unanswered (invocation uSMUID (Methods.Properties (Just (L (List [D (Datum (Unsigned 3))])))))
    it "a StartSession whose HostSessionID is no uinteger a 4-byte HSN holds (3.2.4.2: the identifying parameter invalid)" $ do
      unanswered (startCall uSP_Admin Nothing Nothing) { _parameters = Parameters [value ("one" :: ByteString), value (token uSP_Admin), value (1 :: Natural)] [] }
      unanswered (startCall uSP_Admin Nothing Nothing) { _parameters = Parameters [value (0x100000000 :: Natural), value (token uSP_Admin), value (1 :: Natural)] [] }
    it "a StartSession of other than its three required parameters" $
      unanswered (startCall uSP_Admin Nothing Nothing) { _parameters = Parameters [value (1 :: Natural), value (token uSP_Admin)] [] }

  describe "a host's abort, its status other than SUCCESS (Core 3.2.4.2 item 1; the lead's answer 4)" $ do
    let call = invocation uSMUID (Methods.Properties Nothing)
    it "Properties is answered in its normal form with that status and an empty list, the TPer unchanged" $
      forM_ [1, 0x10, 0x3F] $ \s ->
        answer base call { _statusCode = s } st0
          `shouldBe` (Just (MethodCall (MethodHeader uSMUID uProperties) (Parameters [L (List [])] []) s), st0)
    it "StartSession is answered SyncSession [HostSessionID, 0] with that status, no session opened" $ do
      let (r, st') = answer base (startCall uSP_Admin Nothing Nothing) { _statusCode = 1 } st0
      syncOf r `shouldBe` Just (1, 0, 1)
      st' `shouldBe` st0
    it "another method the host aborted is still no answer" $
      answer base call { _header = MethodHeader uSMUID uStartSession, _statusCode = 1 } st0 `shouldBe` (Nothing, st0)

  describe "SMUID.StartSession to the Admin SP (UCT-03 step 1a)" $ do
    let (r1, st1) = answer base (startCall uSP_Admin Nothing Nothing) st0
    it "with Write True and no authority opens a session: SyncSession [1, 4096] SUCCESS, the codec reading it back" $ do
      syncOf r1 `shouldBe` Just (1, 4096, statusValue SUCCESS)
      fmap (parseByteString . generate) r1 `shouldBe` fmap Right r1
      map (\s -> (sesComId s, sesTSN s, sesHSN s, sesSP s, sesWrite s, sesAuthorities s)) (sessionsOn base st1)
        `shouldBe` [(base, 4096, 1, uSP_Admin, True, [uAuthority_Anybody])]
      lifeOf base st1 `shouldBe` Just Associated
    it "a second session to the SP while one is open is SP_BUSY, [1, 0] (Core 5.1.5.3; UCT-03's extension)" $ do
      let (r2, st2) = answer base (startCall uSP_Admin Nothing Nothing) st1
      syncOf r2 `shouldBe` Just (1, 0, statusValue SP_BUSY)
      st2 `shouldBe` st1
    it "the Locking SP, Manufactured-Inactive at the factory, is INVALID_PARAMETER (Opal 5.2.2.1 item 1; ETC-20)" $
      statusOf (startCall uSP_Locking Nothing Nothing) st0 `shouldBe` Just INVALID_PARAMETER
    it "an SPID that is no row of the SP table, or no uid, is INVALID_PARAMETER (Core 5.1.5.11)" $ do
      statusOf (startCall (uid 0x00 0x00 0x02 0x05 0x00 0x00 0x00 0x09) Nothing Nothing) st0 `shouldBe` Just INVALID_PARAMETER
      statusOf (startCall uSP_Admin Nothing Nothing) { _parameters = Parameters [value (1 :: Natural), value (7 :: Natural), value (1 :: Natural)] [] } st0
        `shouldBe` Just INVALID_PARAMETER
    it "Write False, a read-only session, is INVALID_PARAMETER (Opal 4.1.1.2; the lead's answer 5), as is a Write of another value" $ do
      statusOf (startWith uSP_Admin 0 Nothing Nothing) st0 `shouldBe` Just INVALID_PARAMETER
      statusOf (startWith uSP_Admin 2 Nothing Nothing) st0 `shouldBe` Just INVALID_PARAMETER
    it "an optional parameter an Opal TPer does not support is INVALID_PARAMETER; SessionTimeout is accepted (Opal 4.1.1.2)" $ do
      let with f = (startCall uSP_Admin Nothing Nothing) { _parameters = Parameters [value (1 :: Natural), value (token uSP_Admin), value (1 :: Natural)] [f] }
      forM_ [1, 2, 4, 6, 7, 8] $ \i ->
        statusOf (with (optionalParameter i (value (1 :: Natural)))) st0 `shouldBe` Just INVALID_PARAMETER
      statusOf (with (optionalParameter 5 (value (60000 :: Natural)))) st0 `shouldBe` Just SUCCESS
    prop_distinctTSNs

  describe "SMUID.StartSession as SID (ETC-21/adminsp's authority)" $ do
    let msid = spMsid defaultSimParams
    it "with the right PIN opens a session with Anybody and SID authenticated, Tries 0" $ do
      let (r, st') = answer base (startCall uSP_Admin (Just uAuthority_SID) (Just msid)) st0
      syncOf r `shouldBe` Just (1, 4096, statusValue SUCCESS)
      map sesAuthorities (sessionsOn base st') `shouldBe` [[uAuthority_Anybody, uAuthority_SID]]
      triesOf st' `shouldBe` 0
    it "with a wrong PIN is NOT_AUTHORIZED, no session, Tries still 0 while TryLimit is 0 (Core 5.1.5.2; 5.3.4.1.1.2)" $ do
      let (r, st') = answer base (startCall uSP_Admin (Just uAuthority_SID) (Just (msid <> "!"))) st0
      syncOf r `shouldBe` Just (1, 0, statusValue NOT_AUTHORIZED)
      sessionsOn base st' `shouldBe` []
      triesOf st' `shouldBe` 0
      st' `shouldBe` st0
    it "with no challenge is NOT_AUTHORIZED too" $
      statusOf (startCall uSP_Admin (Just uAuthority_SID) Nothing) st0 `shouldBe` Just NOT_AUTHORIZED
    it "with a challenge that is no bytes is INVALID_PARAMETER (Core 5.1.5.11 d)" $ do
      let call = (startCall uSP_Admin (Just uAuthority_SID) Nothing)
                   { _parameters = Parameters [value (1 :: Natural), value (token uSP_Admin), value (1 :: Natural)]
                                              [optionalParameter 0 (value (3 :: Natural)), optionalParameter 3 (value (token uAuthority_SID))] }
      statusOf call st0 `shouldBe` Just INVALID_PARAMETER
    it "with TryLimit 3 counts Tries 1, 2, 3 and is then AUTHORITY_LOCKED_OUT, Tries never past 3 (Core 5.3.4.1.1.2; 5.1.5.15)" $ do
      let limited = initialState defaultSimParams { spSidTryLimit = 3 }
          wrong   = startCall uSP_Admin (Just uAuthority_SID) (Just "wrong")
          attempts = L.scanl (\(_, st) _ -> answer base wrong st) (Nothing, limited) [1 .. 5 :: Int]
      map (fmap (statusValue . statusOf') . fst) (drop 1 attempts)
        `shouldBe` map (Just . statusValue) [NOT_AUTHORIZED, NOT_AUTHORIZED, NOT_AUTHORIZED, AUTHORITY_LOCKED_OUT, AUTHORITY_LOCKED_OUT]
      map (triesOf . snd) attempts `shouldBe` [0, 1, 2, 3, 3, 3]
    it "a success resets Tries to 0 (Core 5.3.4.1.1.2)" $ do
      let limited = initialState defaultSimParams { spSidTryLimit = 3 }
          (_, failed) = answer base (startCall uSP_Admin (Just uAuthority_SID) (Just "wrong")) limited
          (r, st')    = answer base (startCall uSP_Admin (Just uAuthority_SID) (Just msid)) failed
      triesOf failed `shouldBe` 1
      syncOf r `shouldBe` Just (1, 4096, statusValue SUCCESS)
      triesOf st' `shouldBe` 0
    it "with the parameters' SID PIN authenticates by it, not the MSID" $ do
      let own = initialState defaultSimParams { spSidPin = Just "a SID PIN of its own" }
      statusOf (startCall uSP_Admin (Just uAuthority_SID) (Just "a SID PIN of its own")) own `shouldBe` Just SUCCESS
      statusOf (startCall uSP_Admin (Just uAuthority_SID) (Just msid)) own `shouldBe` Just NOT_AUTHORIZED

  describe "SMUID.StartSession with the other authorities" $ do
    it "Admins, a class, is INVALID_PARAMETER (Core 5.1.5.11)" $
      statusOf (startCall uSP_Admin (Just uAuthority_Admins) Nothing) st0 `shouldBe` Just INVALID_PARAMETER
    it "Admin1, disabled at the factory, is NOT_AUTHORIZED (Core 5.3.4.1.4; Opal Table 24)" $
      statusOf (startCall uSP_Admin (Just uAuthority_Admin1) (Just "")) st0 `shouldBe` Just NOT_AUTHORIZED
    it "an authority of no row is INVALID_PARAMETER" $
      statusOf (startCall uSP_Admin (Just (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x77)) Nothing) st0 `shouldBe` Just INVALID_PARAMETER
    it "Anybody with a challenge succeeds, the challenge ignored (Core 5.3.4.1.2.1)" $ do
      let (r, st') = answer base (startCall uSP_Admin (Just uAuthority_Anybody) (Just "ignored")) st0
      syncOf r `shouldBe` Just (1, 4096, statusValue SUCCESS)
      map sesAuthorities (sessionsOn base st') `shouldBe` [[uAuthority_Anybody]]
    it "with the Locking SP Manufactured, a Locking SP session while an Admin SP session is open is NO_SESSIONS_AVAILABLE at MaxSessions 1 (Core 5.1.5.7; ETC-22)" $ do
      let manufactured = initialState defaultSimParams { spLockingOFS = Manufactured }
          (_, opened)  = answer base (startCall uSP_Admin Nothing Nothing) manufactured
      statusOf (startCall uSP_Locking Nothing Nothing) opened `shouldBe` Just NO_SESSIONS_AVAILABLE
      statusOf (startCall uSP_Locking Nothing Nothing) manufactured `shouldBe` Just SUCCESS
    it "with MaxSessions 2 the Locking SP session opens beside the Admin SP's, TSN 4097, on its ComID" $ do
      let roomier     = initialState defaultSimParams { spLockingOFS = Manufactured, spTPerProperties = (maxSessionsName, 2) : filter ((/= maxSessionsName) . fst) (spTPerProperties defaultSimParams) }
          (_, opened) = answer base (startCall uSP_Admin Nothing Nothing) roomier
          (r, both)   = answer base (startCall uSP_Locking Nothing Nothing) opened
      syncOf r `shouldBe` Just (1, 4097, statusValue SUCCESS)
      map (\s -> (sesTSN s, sesSP s)) (sessionsOn base both) `shouldBe` [(4096, uSP_Admin), (4097, uSP_Locking)]
      statusOf (startCall uSP_Admin Nothing Nothing) both `shouldBe` Just SP_BUSY


-- | The base ComID, and the default TPer as it starts.
base :: Word16
base = spBaseComId defaultSimParams

st0 :: TPerState
st0 = initialState defaultSimParams

-- | The Session Manager's answer to a call on the base ComID of a TPer as it
--   starts with these parameters.
answerIn :: SimParams -> MethodCall -> Maybe MethodCall
answerIn p call = fst (answer (spBaseComId p) call (initialState p))

-- | Run an expectation on the answer to a call, failing if there is none.
withAnswer :: SimParams -> MethodCall -> (MethodCall -> Expectation) -> Expectation
withAnswer p call check = maybe (expectationFailure "no answer") check (answerIn p call)

-- | A Properties invocation with these HostProperties.
propertiesWith :: [(Text, Natural)] -> MethodCall
propertiesWith hs = invocation uSMUID (Methods.Properties (Just (L (List [ N (encodeUtf8 n .: v) | (n, v) <- hs ]))))

-- | A StartSession with HostSessionID 1 and Write True to an SP, with an
--   authority and a challenge or not (the jig's, Test Cases 4.1 e and h, PDF
--   26).
startCall :: UID -> Maybe UID -> Maybe ByteString -> MethodCall
startCall spid = startWith spid 1

-- | The same with a Write of any value.
startWith :: UID -> Natural -> Maybe UID -> Maybe ByteString -> MethodCall
startWith spid write auth challenge = invocation uSMUID Methods.StartSession
  { Methods.startSessionHostSessionID         = value (1 :: Natural)
  , Methods.startSessionSPID                  = value (token spid)
  , Methods.startSessionWrite                 = value write
  , Methods.startSessionHostChallenge         = value <$> challenge
  , Methods.startSessionHostExchangeAuthority = Nothing
  , Methods.startSessionHostExchangeCert      = Nothing
  , Methods.startSessionHostSigningAuthority  = value . token <$> auth
  , Methods.startSessionHostSigningCert       = Nothing
  , Methods.startSessionSessionTimeout        = Nothing
  , Methods.startSessionTransTimeout          = Nothing
  , Methods.startSessionInitialCredit         = Nothing
  , Methods.startSessionSignedHash            = Nothing
  }

-- | A SyncSession answer read: HostSessionID, SPSessionID and the status.
syncOf :: Maybe MethodCall -> Maybe (Natural, Natural, Natural)
syncOf r = do
  mc       <- r
  Right s  <- fromCall @Methods.SyncSession mc
  hsn      <- uinteger (Methods.syncSessionHostSessionID s)
  tsn      <- uinteger (Methods.syncSessionSPSessionID s)
  pure (hsn, tsn, _statusCode mc)
  where
    uinteger = \case
      D (Datum (Unsigned n)) -> Just n
      _                      -> Nothing

-- | The status a StartSession on a TPer is answered with, if it is answered.
statusOf :: MethodCall -> TPerState -> Maybe StatusCode
statusOf call st = statusOf' <$> fst (answer base call st)

statusOf' :: MethodCall -> StatusCode
statusOf' = StatusCode . _statusCode

-- | C_PIN_SID's Tries on the Admin SP.
triesOf :: TPerState -> Integer
triesOf st = case rowWithUID uSP_Admin uC_PINTable uC_PIN_SID st >>= \row -> columnOf c_PINTableDesc "Tries" >>= (`cellOf` row) of
  Just (UIntCell n) -> n
  _                 -> -1

-- | A ComID's host properties.
hostPropsOn :: Word16 -> TPerState -> Map Text Natural
hostPropsOn c st = maybe mempty cHostProps (Map.lookup c (tsComIds st))

-- | The TPer's properties a response reports: its required parameter, a list of Named values.
tperProperties :: MethodCall -> [(Text, Natural)]
tperProperties r = case _required (_parameters r) of
  [L (List vs)] -> mapMaybe namedUinteger vs
  _             -> []

-- | The HostProperties a response reports: its optional parameter named 0, a list of Named values.
hostProperties :: MethodCall -> Maybe [(Text, Natural)]
hostProperties r = case _optional (_parameters r) of
  [NamedValue (Name (Datum (Unsigned 0))) (L (List vs))] -> Just (mapMaybe namedUinteger vs)
  _                                                       -> Nothing

namedUinteger :: Value -> Maybe (Text, Natural)
namedUinteger = \case
  N (NamedValue (Name (Datum (Bytes n))) (D (Datum (Unsigned v)))) -> Just (decodeUtf8Lenient n, v)
  _                                                                 -> Nothing

-- | The least Opal 2.30 Table 17 has a TPer report of a property, from the generated requirements.
tperMinimum :: Text -> Maybe Natural
tperMinimum name = L.find ((== name) . reqName) propertyRequirements >>= reqTPer >>= tprMinimum

-- | The properties Core Table 167 gives the TPer, and the host.
tperNames, hostNames :: [Text]
tperNames = [ propertyName p | p <- properties, TPer `elem` appliesTo p ]
hostNames = [ propertyName p | p <- properties, Host `elem` appliesTo p ]

-- | The six host properties an Opal TPer supports, in Table 17's order.
supportedNames :: [Text]
supportedNames = [maxComPacketSizeName, maxPacketSizeName, maxIndTokenSizeName, maxPacketsName, maxSubpacketsName, maxMethodsName]

-- | Over a random run of StartSessions to the Admin SP, each followed or
--   not by the abort of every session, every SyncSession's SPSessionID is
--   4096 or more and no two are the same.
prop_distinctTSNs :: Spec
prop_distinctTSNs = prop "assigns TSNs from 4096, distinct, to every session it opens (Core 3.3.7.1.1)" $ forAll (listOf (elements [True, False])) $ \aborts ->
  let step (tsns, st) abort =
        let (r, st') = answer base (startCall uSP_Admin Nothing Nothing) st
            tsns'    = tsns <> [ tsn | Just (_, tsn, 0) <- [syncOf r] ]
        in (tsns', if abort then abortSessions (const True) st' else st')
      (opened, _) = L.foldl' step ([], st0) aborts
  in counterexample (show opened) (all (>= 4096) opened .&&. L.nub opened === opened)
