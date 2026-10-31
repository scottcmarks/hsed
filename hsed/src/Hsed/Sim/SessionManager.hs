{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Sim.SessionManager
Description : The simulated TPer's Session Manager: Properties, StartSession and CloseSession as behaviours, and its answer on the control session
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Session Manager's answers on the control session (Core 5.2), each a
'Behaviour' of its method's generated record ("Hsed.Sim.Behaviour"), bound by
a @behaviour@ comment on the method's heading (design 13.16): 'properties'
(5.2.2.1, PDF 140), 'startSession' (5.2.3.1, PDF 149, answered by
SyncSession, 5.2.3.2, PDF 150) and 'closeSession' (5.2.3.5, PDF 153). A
Session Manager response is formatted as a method invocation (Core 3.2.4.2;
5.2.2.1.2, PDF 140), so a reply is a 'MethodCall'; 'answer' is what the
synchronous protocol ("Hsed.Sim.Sync") calls for each invocation of a control
session packet, threading the TPer's state.

* Properties keeps the host's properties per ComID (5.2.2.1, PDF 140;
  5.2.2.3-4, PDF 143-145): each supported host property the host sends
  supersedes the value the ComID keeps, raised to the least Opal 2.30 Table
  17 allows (PDF 33-34, "Hsed.Gen.Opal.Properties") unless it is 0, no limit
  (Core Table 167, PDF 141-142: never raised, the lead's M4a decision, design
  13.30), and bounded by the most the run accepts ('spHostPropertyMax'); a
  property the host omits keeps its value, an unsupported name is ignored and
  not returned; sessions already open keep the properties they started with
  ("Hsed.Sim.State".@openSession@ snapshots them). The answer reports the
  TPer's properties, each a property Core Table 167 gives the TPer, from the
  parameters ("Hsed.Sim.Params".'SimParams': Table 17's minima by default,
  DefSessionTimeout 0 among them) and, where the host sent HostProperties,
  every host property the TPer supports with its current value (5.2.2.4, PDF
  144).
* StartSession opens a regular session or fails it with 5.1.5's statuses
  (PDF 137-139), the resources checked before the authentication (3.3.7.1.4,
  PDF 74), as 'startSession' lists; the answer is SyncSession with the
  host's HostSessionID and the TPer's SPSessionID, the session open once it
  is prepared (3.3.4.5, PDF 62), or, failing, with SPSessionID 0 and the
  status (5.3.4.1.5, PDF 215; 3.2.4.2, PDF 49-50). An Opal TPer supports
  HostSessionID, SPID, Write, HostChallenge and HostSigningAuthority (Opal
  2.30 4.1.1.2, PDF 34): Write must be True, read-only sessions being
  unsupported (and MaxReadSessions not reported, 5.2.2.2, PDF 141), the
  optional SessionTimeout is accepted and ignored (MaxSessionTimeout
  undefined), the other optional parameters are INVALID_PARAMETER (the
  lead's answer 5).
* A host's CloseSession is ignored, as it always is (only the TPer transmits
  the method, 5.2.3.5), and 'closeSessionCall' is the TPer's own, which a
  session abort stages (3.3.7.1.5, PDF 75; 3.3.10.7 b, PDF 86).

An invocation the host aborts, its first status other than SUCCESS, is
answered before its parameters are read, in the method's normal response
form with that status, its processing aborted and the TPer unchanged: a
non-0x00 first status SHALL cause the TPer to abort processing on that
method and return that value as the first value in the status list, and a
Session Manager method recognized but failing SHALL have its normal response
format (Core 3.2.4.2 item 5.1 and the paragraph after it, PDF 49; the lead's
answer 4; design 13.30 (4)): Properties with an empty list of the TPer's
properties and no HostProperties, whatever the parameters sent; StartSession
with SyncSession [HostSessionID, 0] where its first required parameter reads
as an HSN, else no answer (the identifying parameter invalid, which the
TPer MAY ignore, PDF 50). The status list's reserved values are ignored
where it is read (item 2, "Hsed.Sim.Sync").

Anything else is no answer, its payload discarded, the TPer unchanged
(3.3.7.1.3, PDF 73-74): a method invoked on another than SMUID, a Session
Manager method the TPer does not answer (the host's SyncSession,
CloseSession, the trusted-session pair: Opal 2.30 4.1.1 supports no
StartTrustedSession), a StartSession whose HostSessionID is no uinteger a
4-byte HSN holds (an invalid identifying parameter, PDF 50's MAY), and an
invocation with status 0 whose parameters are not its method's (a required
parameter missing or extra, an optional one misnamed, a HostProperties other
than Named byte-string names with uinteger values): a parameter-shape error
on the control session read as a streaming violation, whose payload an Opal
TPer discards (Opal 2.30 3.3.4.1.3, PDF 31; the lead's answer 8, over Core
3.2.2.4.2 item 4's INVALID_PARAMETER, PDF 39; Test Cases ETC-08 accepts the
discard, PDF 81; design 13.30 (4) and (8)).
-}
module Hsed.Sim.SessionManager
  ( -- * The answer on the control session
    answer
    -- * The behaviours
  , properties
  , startSession
  , closeSession
  , behaviours
  , closeSessionCall
  ) where

import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import           RIO

import           Hsed.Core.Call            (MethodCall (..), MethodHeader (..), Parameters (..))
import           Hsed.Core.Method          (MethodSignature (..), Param (..), SigResponse (..), Signature (..), invocation,
                                            invocationWith, responseParameters)
import           Hsed.Core.Properties      (Communicator (..), HostRequirement (..), Property (..), PropertyRequirement (..))
import           Hsed.Core.Table           (Cell (..), ColNum, Row, TableDesc)
import           Hsed.Core.Token           (IsToken (..), Token (..))
import           Hsed.Core.Types           (Enumerated, enumValue)
import           Hsed.Core.UID             (UID)
import           Hsed.Core.Value           (Datum (..), IsValue (..), List (..), Name (..), NamedValue (..), Value (..),
                                            (.:))
import           Hsed.Gen.Core.Codes       (StatusCode (..), pattern AUTHORITY_LOCKED_OUT, pattern INVALID_PARAMETER,
                                            pattern NOT_AUTHORIZED, pattern NO_SESSIONS_AVAILABLE, pattern SP_BUSY,
                                            pattern SUCCESS)
import           Hsed.Gen.Core.Enums       (Core_auth_method (..), Core_boolean (..))
import qualified Hsed.Gen.Core.Methods     as Methods
import qualified Hsed.Gen.Core.Properties  as P
import           Hsed.Gen.Core.TableDescs  (authorityTableDesc, c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs   (uAuthorityTable, uC_PINTable, uSPTable)
import           Hsed.Gen.Core.UIDs        (uAuthority_Anybody, uProperties, uSMUID, uSP_Admin, uStartSession)
import           Hsed.Jig.Catalogue        (CaseKey (..), TestId (..))
import           Hsed.Sim.Behaviour        (Behaviour (..), Invoked (..), Reply (..), SomeBehaviour, forMethod, runSome)
import           Hsed.Sim.State            (cellOf, columnOf, openSession, rowWithUID, tperProperty, updateRow)
import           Hsed.Sim.Types            (ComIdState (..), LifeCycle (..), SPState (..), Session (..), SimParams (..),
                                            TPerState (..), hostRequirements, initialHostProperties, maxSessionsName,
                                            supportedHostProperties, tperRequirements)


-- * The answer on the control session

-- | The Session Manager's answer to a method invocation on a ComID's
--   control session, and the TPer after it: an invocation of SMUID the host
--   aborted, its status other than SUCCESS, answered in its normal form
--   with that status before its parameters are read ('aborted'; Core
--   3.2.4.2 item 5.1, PDF 49); else one whose method a behaviour runs (its
--   parameters read against the record's signature, a ParamError leaving it
--   unanswered: Opal 2.30 3.3.4.1.3's discard, the lead's answer 8);
--   'Nothing', the TPer unchanged, for anything else (an invoker other than
--   SMUID, a method no behaviour answers, a behaviour that replies with
--   nothing or with results, which no control session carries).
answer :: Word16 -> MethodCall -> TPerState -> (Maybe MethodCall, TPerState)
answer c mc@(MethodCall (MethodHeader invoker method) params status) st
  | invoker /= uSMUID = (Nothing, st)
  | status /= success = (aborted, st)
  | otherwise = case mapMaybe (\b -> runSome b invoked mc st) behaviours of
      [Right (ReplyCall reply, st')] -> (Just reply, st')
      _                              -> (Nothing, st)
  where
    invoked = Invoked { ivComId = c, ivSession = Nothing, ivInvoker = invoker, ivStatus = status }
    -- a host's abort in the method's normal response form, the parameters unread: Properties with an empty list,
    -- StartSession as SyncSession [HostSessionID, 0] where its first required parameter is an HSN (else no answer,
    -- the identifying parameter invalid, PDF 50), no answer for a method the TPer does not answer
    aborted
      | method == uProperties   = Just (propertiesReply status [] Nothing)
      | method == uStartSession = (\hsn -> syncSession status hsn 0) <$> (listToMaybe (_required params) >>= hostSessionId)
      | otherwise               = Nothing

-- | The status of a method that succeeded (Core Table 166).
success :: Natural
success = statusValue SUCCESS


-- * The behaviours

-- | SMUID.Properties (Core 5.2.2.1, PDF 140; @behaviour
--   "SessionManager.properties"@): the TPer's properties and, where the host
--   sent its own, the host's it will use, kept on the invoking ComID
--   (5.2.2.3-4, PDF 143-145); a host's abort echoed with an empty list (which
--   'answer' does before the parameters are read; here for a run of the
--   behaviour alone); no answer where HostProperties is not a list of Named
--   byte-string names with uinteger values. UCT-02 reads it, ETC-02, ETC-03,
--   ETC-08 and ETC-15 invoke it.
properties :: Behaviour Methods.Properties
properties = Behaviour
  { behName  = "SessionManager.properties"
  , behHash  = "4096cbe61646"
  , behCases = [whole (UCT 2), whole (ETC 2), whole (ETC 3), whole (ETC 8), whole (ETC 15)]
  , behRun   = run
  }
  where
    run iv (Methods.Properties host) st
      | ivStatus iv /= success = (ReplyCall (propertiesReply (ivStatus iv) [] Nothing), st)
      | otherwise = case hostProperties host of
          Nothing          -> (NoReply, st)
          Just Nothing     -> (ReplyCall (propertiesReply success (tperProperties p) Nothing), st)
          Just (Just sent) ->
            let st'  = st { tsComIds = Map.adjust (\cs -> cs { cHostProps = acceptHostProperties p sent (cHostProps cs) }) (ivComId iv) (tsComIds st) }
                kept = maybe initialHostProperties cHostProps (Map.lookup (ivComId iv) (tsComIds st'))
            in (ReplyCall (propertiesReply success (tperProperties p) (Just (hostResult kept))), st')
      where p = tsParams st

-- | SMUID.StartSession (Core 5.2.3.1, PDF 149; @behaviour
--   "SessionManager.startSession"@), answered by SyncSession (5.2.3.2, PDF
--   150). UCT-03 step 1a, ETC-04 and ETC-21/adminsp invoke it. In order:
--
--   * HostSessionID a uinteger the packet header's 4-byte HSN holds
--     (5.2.3.1.1, PDF 149; 3.3.7.1.1, PDF 72), else no answer: a value of
--     another type, or one that does not fit the HSN, is an invalid
--     identifying parameter, which the TPer MAY ignore (3.2.4.2, PDF 50;
--     Core 5.1.5.11 f's INVALID_PARAMETER for a value out of range, PDF
--     139, would also do); then a host's abort echoed, SyncSession
--     [HostSessionID, 0] with its status (item 5.1, PDF 49; 'answer' does
--     it before the parameters are read, this branch serving a run of the
--     behaviour alone);
--   * SPID a row of the Admin SP's SP table (5.2.3.1.2), else
--     INVALID_PARAMETER (5.1.5.11, PDF 139); an SP Manufactured-Inactive,
--     to which no session can be opened (Opal 2.30 5.2.2.1 item 1, PDF 96;
--     5.2.2.3.1, PDF 97; Test Cases ETC-20), INVALID_PARAMETER;
--   * Write True (5.2.3.1.3), a read-only session unsupported (Opal 2.30
--     4.1.1.2, PDF 34: it may or may not be), else INVALID_PARAMETER;
--   * HostExchangeAuthority, HostExchangeCert, HostSigningCert, TransTimeout,
--     InitialCredit and SignedHash unsupported (Opal 2.30 4.1.1.2 lists the
--     five an Opal TPer supports), INVALID_PARAMETER; SessionTimeout accepted
--     and ignored (4.1.1.2 a-c: MaxSessionTimeout undefined);
--   * the resources, before the authentication (3.3.7.1.4, PDF 74): a
--     session open to the SP on any ComID, SP_BUSY (5.1.5.3, PDF 138: one
--     read-write session per SP, 3.3.7.1, PDF 72); as many sessions open
--     across the ComIDs as the TPer's MaxSessions (Core Table 167, PDF 142;
--     0 no limit), NO_SESSIONS_AVAILABLE (5.1.5.7);
--   * HostSigningAuthority: none or Anybody, the session's authority Anybody
--     and any HostChallenge ignored (5.3.4.1.2.1, PDF 211); no such row of
--     the SP's Authority table, or a class authority (5.1.5.11, PDF 139),
--     INVALID_PARAMETER; a disabled authority NOT_AUTHORIZED (5.3.4.1.4, PDF
--     214); a Password authority (5.3.4.1.3 b, PDF 213) authenticated with
--     the HostChallenge against its C_PIN credential's PIN (5.2.3.1.4, PDF
--     149; 5.1.5.2, PDF 138): its Tries at a TryLimit other than 0,
--     AUTHORITY_LOCKED_OUT (5.1.5.15, PDF 139, as the TPer MAY); the PIN,
--     Tries 0; another challenge NOT_AUTHORIZED, Tries one more while TryLimit
--     is not 0 and never past it (5.3.4.1.1.2, PDF 210-211), the C_PIN row
--     changed on the SP named (3.3.7.1.3, PDF 73); an authority with
--     Operation None, which 5.3.4.1.3 a lets a session start with as the
--     HostSigningAuthority with no credential to check (PDF 213), authenticated
--     as Anybody is, any HostChallenge ignored (no Opal row but Anybody's is
--     one, Opal 2.30 Tables 24 and 40, PDF 48 and 82); the challenge-response
--     Operations the simulator lacks (Sign, SymK, HMAC) and Exchange, no
--     signing authority (5.3.4.1.3 c-f), INVALID_PARAMETER;
--   * the session opened ("Hsed.Sim.State".@openSession@) with the authority
--     and its classes authenticated (3.3.7.1.4, PDF 75; 3.4.2.2, PDF 89;
--     5.3.4.1.2, PDF 211: a class's class too, one level), Anybody always
--     among them, and SyncSession [HostSessionID, SPSessionID] status 0
--     (3.3.4.5, PDF 62); failing, SyncSession [HostSessionID, 0] with the
--     status (5.3.4.1.5, PDF 215; SPSessionID 0 the lead's answer 7).
startSession :: Behaviour Methods.StartSession
startSession = Behaviour
  { behName  = "SessionManager.startSession"
  , behHash  = "24fca697f37f"
  , behCases = [CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing, whole (ETC 4), CaseKey (ETC 21) Nothing Nothing (Just "adminsp")]
  , behRun   = run
  }
  where
    run iv m st = case hostSessionId (Methods.startSessionHostSessionID m) of
      Nothing -> (NoReply, st)
      Just hsn
        | ivStatus iv /= success -> (ReplyCall (syncSession (ivStatus iv) hsn 0), st)
        | otherwise -> case admitted m st of
            Left status -> (ReplyCall (syncSession (statusValue status) hsn 0), st)
            Right spid  -> case authenticated spid m st of
              (Left status, st')       -> (ReplyCall (syncSession (statusValue status) hsn 0), st')
              (Right authorities, st') ->
                let (session, st'') = openSession (ivComId iv) hsn spid True authorities st'
                in (ReplyCall (syncSession success hsn (sesTSN session)), st'')

-- | SMUID.CloseSession (Core 5.2.3.5, PDF 153; @behaviour
--   "SessionManager.closeSession"@): a method only the TPer transmits, so a
--   host's invocation of it is ignored (3.3.7.1.3, PDF 73-74); the TPer's
--   own is 'closeSessionCall'. No Test Case invokes it.
closeSession :: Behaviour Methods.CloseSession
closeSession = Behaviour
  { behName  = "SessionManager.closeSession"
  , behHash  = "993210934add"
  , behCases = []
  , behRun   = \_ _ st -> (NoReply, st)
  }

-- | The Session Manager's behaviours, the ones its dispatch runs.
behaviours :: [SomeBehaviour]
behaviours = [forMethod properties, forMethod startSession, forMethod closeSession]

-- | The CloseSession the TPer transmits when it aborts a session (Core
--   3.3.7.1.5, PDF 75; 3.3.10.7 b, PDF 86; Opal 2.30 4.1.1.4 (O), PDF 35):
--   SMUID.CloseSession with the session's HSN as RemoteSessionNumber and
--   its TSN as LocalSessionNumber (5.2.3.5.1-2, PDF 153), status 0.
closeSessionCall :: Session -> MethodCall
closeSessionCall s = invocation uSMUID Methods.CloseSession
  { Methods.closeSessionRemoteSessionNumber = value (fromIntegral (sesHSN s) :: Natural)
  , Methods.closeSessionLocalSessionNumber  = value (fromIntegral (sesTSN s) :: Natural)
  }

-- | A test, all of it.
whole :: TestId -> CaseKey
whole t = CaseKey t Nothing Nothing Nothing


-- * Properties' parts

-- | The TPer's Properties invocation (5.2.2.1.2, PDF 140: the response is
--   itself a Properties invocation): its Properties, its HostProperties
--   where it answers them, and its status, the parameters placed as the
--   generated signature's response names them (@=> SMUID.Properties [
--   Properties : list, HostProperties = list ]@, HostProperties found by name
--   among the optional ones, 3.2.4.1).
propertiesReply :: Natural -> [Value] -> Maybe [Value] -> MethodCall
propertiesReply status tper host = MethodCall (MethodHeader uSMUID uProperties) params status
  where
    params = case sigResponse (signature @Methods.Properties) of
      Invocation _ reqs opts -> responseParameters reqs opts [L (List tper)]
                                  [ if paramName o == "HostProperties" then L . List <$> host else Nothing | o <- opts ]
      _                      -> Parameters [L (List tper)] []

-- | The TPer's properties as the response carries them: each a Named value,
--   a property Core Table 167 gives the TPer, a uinteger.
tperProperties :: SimParams -> [Value]
tperProperties p =
  [ N (encodeUtf8 name .: (fromInteger v :: Natural)) | (name, v) <- spTPerProperties p, name `elem` tper, v >= 0 ]
  where tper = [ propertyName pr | pr <- P.properties, TPer `elem` appliesTo pr ]

-- | The host's properties, if the optional parameter is Properties' encoding
--   of them: 'Just' 'Nothing' when the host sent none, 'Nothing' when they
--   are no HostProperties (not a list of Named byte-string names with
--   uinteger values).
hostProperties :: Maybe Value -> Maybe (Maybe [(Text, Natural)])
hostProperties = \case
  Nothing -> Just Nothing
  Just v  -> Just <$> namedUintegers v
  where
    namedUintegers (L (List vs)) = traverse namedUinteger vs
    namedUintegers _             = Nothing
    namedUinteger (N (NamedValue (Name (Datum (Bytes name))) (D (Datum (Unsigned n))))) =
      either (const Nothing) (\t -> Just (t, n)) (decodeUtf8' name)
    namedUinteger _ = Nothing

-- | A ComID's host properties after the HostProperties the host sent (Core
--   5.2.2.3-4, PDF 143-145): each supported property sent supersedes the
--   value kept, raised to the least Opal 2.30 Table 17 allows (its
--   generated @hrMinimum@) unless it is 0, which means no limit wherever
--   Core Table 167 says so (PDF 141-142) and is never raised (the lead's
--   M4a decision, design 13.30), then bounded by the most accepted, Table
--   17's where it gives one (@hrMaximum@, VU throughout) and the run's
--   ('spHostPropertyMax'), which bound a 0 too; a property not sent keeps its
--   value; an unsupported name is ignored (5.2.2.4, PDF 144).
acceptHostProperties :: SimParams -> [(Text, Natural)] -> Map Text Natural -> Map Text Natural
acceptHostProperties p sent kept = L.foldl' (\m (name, v) -> Map.insert name v m) kept
  [ (name, bounded (reqName r) h (raised h v)) | (name, v) <- sent, Just (r, h) <- [L.find ((== name) . reqName . fst) supportedHostProperties] ]
  where
    raised h v
      | v == 0    = 0
      | otherwise = maybe v (max v) (hrMinimum h)
    bounded name h v = case L.minimumMaybe (catMaybes [hrMaximum h, L.lookup name (spHostPropertyMax p)]) of
      Nothing   -> v
      Just most -> if v == 0 || v > most then most else v

-- | The HostProperties the TPer answers (5.2.2.4, PDF 144: every host
--   property it supports, with its current value, in Opal 2.30 Table 17's
--   order), from what the ComID keeps.
hostResult :: Map Text Natural -> [Value]
hostResult kept = [ N (encodeUtf8 name .: v) | (name, _) <- hostRequirements, Just v <- [Map.lookup name kept] ]


-- * StartSession's parts

-- | The TPer's SyncSession (5.2.3.2, PDF 150): HostSessionID the host's,
--   SPSessionID the TSN assigned (0 where none was), nothing optional, with
--   a status.
syncSession :: Natural -> Word32 -> Word32 -> MethodCall
syncSession status hsn tsn = invocationWith status uSMUID Methods.SyncSession
  { Methods.syncSessionHostSessionID  = value (fromIntegral hsn :: Natural)
  , Methods.syncSessionSPSessionID    = value (fromIntegral tsn :: Natural)
  , Methods.syncSessionSPChallenge    = Nothing
  , Methods.syncSessionSPExchangeCert = Nothing
  , Methods.syncSessionSPSigningCert  = Nothing
  , Methods.syncSessionTransTimeout   = Nothing
  , Methods.syncSessionInitialCredit  = Nothing
  , Methods.syncSessionSignedHash     = Nothing
  }

-- | The HostSessionID as the HSN it becomes (5.2.3.1.1, PDF 149): a
--   uinteger the packet header's 4-byte HSN holds (3.3.7.1.1, PDF 72);
--   anything else, a value of another type or one past 2^32 - 1, is no
--   session number.
hostSessionId :: Value -> Maybe Word32
hostSessionId = \case
  D (Datum (Unsigned n)) | n <= fromIntegral (maxBound :: Word32) -> Just (fromIntegral n)
  _                                                              -> Nothing

-- | A parameter as the UID it names (a uidref, the UID's eight bytes, Core
--   5.1.2, PDF 99), if it is one.
uidOf :: Value -> Maybe UID
uidOf v = mtoken v >>= fromToken

-- | Whether a cell is an enumeration's value (a boolean's True, an
--   auth_method's Password).
enumIs :: (Enumerated e, Enum e) => e -> Maybe Cell -> Bool
enumIs e = \case
  Just (EnumCell _ v) -> v == enumValue e
  _                   -> False

-- | A row's cell in a column named in a generated table description.
cellNamed :: TableDesc -> Text -> Row -> Maybe Cell
cellNamed desc name row = columnOf desc name >>= (`cellOf` row)

-- | The SP a StartSession may open a session to, admitted before the
--   authentication (3.3.7.1.4, PDF 74), or the status it fails with: the
--   SP, Write, the optional parameters and the resources, in that order (see
--   'startSession').
admitted :: Methods.StartSession -> TPerState -> Either StatusCode UID
admitted m st = do
  spid <- maybe (Left INVALID_PARAMETER) Right (uidOf (Methods.startSessionSPID m))
  -- a row of the Admin SP's SP table (5.2.3.1.2), and an SP in a life cycle state sessions open to: the SP's own
  -- life ("Hsed.Sim.Types".SPState) is read, which the SP-table row's LifeCycleState cell mirrors (nothing changes
  -- either at M4a; Activate and Revert, M5's, change both)
  unless (isJust (rowWithUID uSP_Admin uSPTable spid st)) (Left INVALID_PARAMETER)
  unless (fmap spsLife (Map.lookup spid (tsSPs st)) == Just Manufactured) (Left INVALID_PARAMETER)
  -- Write True (5.2.3.1.3): the boolean's value as the generated enumeration assigns it (Core Table 59, PDF 103)
  unless (Methods.startSessionWrite m == value (fromIntegral (enumValue Core_boolean_True) :: Natural)) (Left INVALID_PARAMETER)
  -- the optional parameters an Opal TPer does not support (Opal 2.30 4.1.1.2, PDF 34); SessionTimeout it accepts
  when (any isJust [ Methods.startSessionHostExchangeAuthority m, Methods.startSessionHostExchangeCert m
                   , Methods.startSessionHostSigningCert m, Methods.startSessionTransTimeout m
                   , Methods.startSessionInitialCredit m, Methods.startSessionSignedHash m ])
       (Left INVALID_PARAMETER)
  -- the resources: one read-write session per SP (3.3.7.1, PDF 72; 5.1.5.3), and the TPer's MaxSessions across
  -- its ComIDs (Core Table 167, PDF 142: 0 no limit; 5.1.5.7)
  when (any ((== spid) . sesSP) (Map.elems (tsSessions st))) (Left SP_BUSY)
  when (maxSessions /= 0 && toInteger (Map.size (tsSessions st)) >= maxSessions) (Left NO_SESSIONS_AVAILABLE)
  pure spid
  where
    maxSessions = fromMaybe (maybe 1 toInteger (L.lookup maxSessionsName tperRequirements)) (tperProperty maxSessionsName (tsParams st))

-- | The authorities a StartSession authenticates on an SP, by its
--   HostSigningAuthority and HostChallenge (see 'startSession'), or the
--   status it fails with; and the TPer after it, the credential's Tries
--   changed on the SP named (3.3.7.1.3, PDF 73).
authenticated :: UID -> Methods.StartSession -> TPerState -> (Either StatusCode [UID], TPerState)
authenticated spid m st = case Methods.startSessionHostSigningAuthority m of
  Nothing -> (Right [uAuthority_Anybody], st)
  Just v  -> case uidOf v of
    Nothing   -> (Left INVALID_PARAMETER, st)
    Just auth
      | auth == uAuthority_Anybody -> (Right [uAuthority_Anybody], st)
      | otherwise -> case rowWithUID spid uAuthorityTable auth st of
          Nothing -> (Left INVALID_PARAMETER, st)
          Just row
            | enumIs Core_boolean_True (cellNamed authorityTableDesc "IsClass" row)              -> (Left INVALID_PARAMETER, st)
            | not (enumIs Core_boolean_True (cellNamed authorityTableDesc "Enabled" row))        -> (Left NOT_AUTHORIZED, st)
            | enumIs Core_auth_method_Password (cellNamed authorityTableDesc "Operation" row)   -> password auth row
            | enumIs Core_auth_method_None (cellNamed authorityTableDesc "Operation" row)       -> (Right (L.nub (uAuthority_Anybody : auth : classesOf row)), st)
            | otherwise                                                                          -> (Left INVALID_PARAMETER, st)
  where
    -- a Password authority: its C_PIN credential (5.3.4.1.3 b, PDF 213; 5.2.3.1.7, PDF 149), the HostChallenge the
    -- password (5.2.3.1.4), the Tries rule (5.3.4.1.1.2, PDF 210-211)
    password auth row = case (cellNamed authorityTableDesc "Credential" row, challenge) of
      (_, Left status) -> (Left status, st)
      (Just (RefCell _ cred), Right given)
        | Just pin <- rowWithUID spid uC_PINTable cred st -> attempt auth row cred pin given
      _ -> (Left INVALID_PARAMETER, st)
    attempt auth row cred pin given
      | tryLimit /= 0 && tries >= tryLimit                 = (Left AUTHORITY_LOCKED_OUT, st)
      | Just (BytesCell secret) <- pinCell "PIN", secret == given = (Right (L.nub (uAuthority_Anybody : auth : classesOf row)), setTries cred 0)
      | tryLimit /= 0                                      = (Left NOT_AUTHORIZED, setTries cred (min tryLimit (tries + 1)))
      | otherwise                                          = (Left NOT_AUTHORIZED, st)
      where
        pinCell  = \name -> cellNamed c_PINTableDesc name pin
        tryLimit = uintOf (pinCell "TryLimit")
        tries    = uintOf (pinCell "Tries")
    -- the challenge as a password: bytes, none sent as a password of no bytes; a value of another type is no
    -- password (5.1.5.11 d, PDF 139)
    challenge = case Methods.startSessionHostChallenge m of
      Nothing                    -> Right mempty
      Just (D (Datum (Bytes b))) -> Right b
      Just _                     -> Left INVALID_PARAMETER
    uintOf = \case
      Just (UIntCell n) -> n
      _                 -> 0
    -- the Tries cell of a C_PIN row of this SP set
    setTries cred n = updateRow spid uC_PINTable cred (map (\(col, cell) -> (col, if Just col == triesColumn then UIntCell n else cell))) st
    triesColumn = columnOf c_PINTableDesc "Tries" :: Maybe ColNum
    -- the authority's classes, authenticated with it (3.4.2.2, PDF 89): its Class, and that class's, no further
    -- (5.3.4.1.2, PDF 211: a single level); Anybody is always authenticated (5.3.4.1.2.1)
    classesOf row = take 2 (L.unfoldr classAbove row)
    classAbove row = do
      RefCell _ cls <- cellNamed authorityTableDesc "Class" row
      above         <- rowWithUID spid uAuthorityTable cls st
      pure (cls, above)
