{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.SessionManagerSpec
Description : Test specifications for the simulated Session Manager: SMUID.Properties at M3
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Milestone M3's group G2: the simulator answers the invocation the jig's ETC-02
and ETC-03 send, SMUID.Properties with no HostProperties (Test Cases PDF
77-78), with a Properties invocation of its own (Core 5.2.2.1, PDF 140) that
the codec reads back: the TPer's properties, each a property Core Table 167
gives the TPer (MaxComPacketSize 2048, Opal 2.30 Table 17's least, PDF 33);
it echoes HostProperties, each at least what Opal Table 17 allows the host
and so at least Core Table 168's assumption (5.2.2.3, PDF 143); a Properties
invocation the host aborted it answers in the normal form with the host's
status and an empty list (Core 3.2.4.2 item 1, PDF 49); and it answers
nothing else. The property names it and the jig read are Core Table 167's.
-}
module Hsed.Sim.SessionManagerSpec (spec) where

import qualified RIO.List                 as L
import           RIO
import           Test.Hspec

import           Hsed.Core.Call           (MethodCall (..), MethodHeader (..), Parameters (..), optionalParameter)
import           Hsed.Core.Method         (invocation)
import           Hsed.Core.Properties     (Assumptions (..), Communicator (..), Property (..))
import           Hsed.Core.StreamItem     (StreamItem (..))
import           Hsed.Core.Token          (Token (..))
import           Hsed.Core.Value          (Datum (..), List (..), Name (..), NamedValue (..), Value (..), (.:))
import qualified Hsed.Gen.Core.Methods    as Methods
import           Hsed.Gen.Core.Properties (initialAssumptions, properties)
import           Hsed.Gen.Core.UIDs       (uProperties, uSMUID, uStartSession, uThisSP)
import           Hsed.Sim.SessionManager  (answer)
import           Hsed.Sim.Types           (SimParams (..), defaultSimParams, hostRequirements, maxComPacketSizeName,
                                           maxMethodsName, maxPacketSizeName, maxPacketsName, maxResponseComPacketSizeName,
                                           maxSubpacketsName)


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

  describe "SMUID.Properties with HostProperties" $ do
    let host = L (List [ N (("MaxComPacketSize" :: ByteString) .: (4096 :: Natural))
                       , N (("MaxPacketSize" :: ByteString) .: (1004 :: Natural))
                       , N (("MaxIndTokenSize" :: ByteString) .: (4040 :: Natural))
                       , N (("NoSuchProperty" :: ByteString) .: (7 :: Natural)) ])
        call = invocation uSMUID (Methods.Properties (Just host))
    it "echoes them as the response's optional HostProperties, raised to Opal Table 17's least" $
      withAnswer defaultSimParams call $ \r -> do
        (parseByteString (generate r) :: Either String MethodCall) `shouldBe` Right r
        hostProperties r `shouldBe` Just
          [ ("MaxComPacketSize", 4096), ("MaxPacketSize", 2028), ("MaxIndTokenSize", 4040)
          , ("MaxPackets", 1), ("MaxSubpackets", 1), ("MaxMethods", 1) ]
    it "reports host properties Core Table 167 gives the host, none below Core Table 168's assumption" $
      withAnswer defaultSimParams call $ \r -> do
        let reported    = fromMaybe [] (hostProperties r)
            tperAssumes = concat [ a | Assumptions TPer Host a <- initialAssumptions ]
        filter (`notElem` hostNames) (map fst reported) `shouldBe` []
        [ (n, v, a) | (n, v) <- reported, Just a <- [L.lookup n tperAssumes], v < a ] `shouldBe` []
        map fst reported `shouldSatisfy` all (`elem` map fst tperAssumes)

  describe "the properties it and the jig read" $
    it "are named as Core Table 167 names them, and Opal Table 17's host requirements as well" $ do
      filter (`notElem` map propertyName properties)
        [maxComPacketSizeName, maxResponseComPacketSizeName, maxPacketSizeName, maxPacketsName, maxSubpacketsName, maxMethodsName]
        `shouldBe` []
      filter (`notElem` hostNames) (map fst hostRequirements) `shouldBe` []

  describe "anything else" $ do
    let call = invocation uSMUID (Methods.Properties Nothing)
    it "another Session Manager method is no answer" $
      answer defaultSimParams call { _header = MethodHeader uSMUID uStartSession } `shouldBe` Nothing
    it "Properties invoked on another object is no answer" $
      answer defaultSimParams call { _header = MethodHeader uThisSP uProperties } `shouldBe` Nothing
    it "a Properties invocation the host aborted (status other than SUCCESS) is answered, aborted: its status, an empty list" $
      forM_ [1, 0x10, 0x3F] $ \s ->
        answer defaultSimParams call { _statusCode = s }
          `shouldBe` Just (MethodCall (MethodHeader uSMUID uProperties) (Parameters [L (List [])] []) s)
    it "another method the host aborted is still no answer" $
      answer defaultSimParams call { _header = MethodHeader uSMUID uStartSession, _statusCode = 1 } `shouldBe` Nothing
    it "a required parameter, Properties having none, is no answer (ETC-08's StartList after StartList)" $
      answer defaultSimParams call { _parameters = Parameters [L (List [])] [] } `shouldBe` Nothing
    it "an optional parameter other than HostProperties is no answer" $
      answer defaultSimParams call { _parameters = Parameters [] [optionalParameter 1 (List [])] } `shouldBe` Nothing
    it "HostProperties other than Named uintegers is no answer" $ do
      answer defaultSimParams (invocation uSMUID (Methods.Properties (Just (D (Datum (Unsigned 3)))))) `shouldBe` Nothing
      answer defaultSimParams (invocation uSMUID (Methods.Properties (Just (L (List [D (Datum (Unsigned 3))])))))
        `shouldBe` Nothing


-- | Run an expectation on the answer to a call, failing if there is none.
withAnswer :: SimParams -> MethodCall -> (MethodCall -> Expectation) -> Expectation
withAnswer p call check = maybe (expectationFailure "no answer") check (answer p call)

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

-- | The properties Core Table 167 gives the TPer, and the host.
tperNames, hostNames :: [Text]
tperNames = [ propertyName p | p <- properties, TPer `elem` appliesTo p ]
hostNames = [ propertyName p | p <- properties, Host `elem` appliesTo p ]
