{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Sim.BehaviourSpec
Description : Test specifications for Hsed.Sim.Behaviour and the behaviours of Hsed.Sim.SessionManager and Hsed.Sim.Base: names, hashes, cases, dispatch
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The behaviours the @behaviour@ and @fails@ comments of M4a bind (design
13.16): the Session Manager's three and the Base Template's Get, each named
as its comment names it, with the first 12 hex digits of its signature
block's hash in the IR (@spec/ir/core-2.01.json@: 5.2.2.1, 5.2.3.1, 5.2.3.5
and 5.3.3.6), the UID of its method, and cases the jig's catalogue has;
Get's Fails list, items a to d with their statuses, each item's property
run (the simulator fails Get so, "Hsed.Sim.Base"); and the dispatch,
'runSome', which runs a behaviour on its method's calls alone, reading the
parameters against the signature. The TPer's CloseSession carries the
session's HSN and TSN (Core 5.2.3.5, PDF 153).
-}
module Hsed.Sim.BehaviourSpec (spec) where

import qualified RIO.List                as L
import qualified RIO.Map                 as Map
import           RIO
import qualified RIO.Text                as T
import           Test.Hspec
import           Test.QuickCheck         (counterexample, property)

import           Hsed.Core.Call          (MethodCall (..), MethodHeader (..), Parameters (..))
import           Hsed.Core.Method        (ParamError (..), fromCall, invocation)
import           Hsed.Core.Token         (IsToken (..))
import           Hsed.Core.Value         (IsValue (..), List (..), Value (..))
import           Hsed.Gen.Core.Codes     (pattern INVALID_PARAMETER, pattern NOT_AUTHORIZED)
import qualified Hsed.Gen.Core.Methods   as Methods
import           Hsed.Gen.Core.UIDs      (uCloseSession, uMethodID_Get, uProperties, uSMUID, uStartSession, uSyncSession, uSP_Admin)
import           Hsed.Jig.Catalogue      (CatalogueEntry (..), catalogue, renderCaseKey)
import qualified Hsed.Sim.Base           as Base
import           Hsed.Sim.Behaviour
import qualified Hsed.Sim.SessionManager as SessionManager
import           Hsed.Sim.State          (openSession)
import           Hsed.Sim.Types          (TPerState (..), defaultSimParams, initialState, spBaseComId)


st0 :: TPerState
st0 = initialState defaultSimParams

base :: Word16
base = spBaseComId defaultSimParams

-- | A call on the control session.
invoked :: MethodCall -> Invoked
invoked mc = Invoked base Nothing (_invokingID (_header mc))

propertiesCall :: MethodCall
propertiesCall = invocation uSMUID (Methods.Properties Nothing)

spec :: Spec
spec = do
  describe "the behaviours the comments bind" $ do
    it "are named and hashed as the IR hashes their signature blocks (5.2.2.1, 5.2.3.1, 5.2.3.5, 5.3.3.6)" $ do
      [ (someName b, someHash b) | b <- SessionManager.behaviours ] `shouldBe`
        [ ("SessionManager.properties", "4096cbe61646"), ("SessionManager.startSession", "24fca697f37f"), ("SessionManager.closeSession", "993210934add") ]
      [ (someName b, someHash b) | b <- Base.behaviours ] `shouldBe` [("Base.get", "9d67d740e4cb")]
      [ T.length (someHash b) | b <- SessionManager.behaviours <> Base.behaviours ] `shouldSatisfy` all (== 12)
    it "run the methods their records name" $
      map someUID (SessionManager.behaviours <> Base.behaviours) `shouldBe` [uProperties, uStartSession, uCloseSession, uMethodID_Get]
    it "name cases the jig's catalogue has" $
      [ renderCaseKey k | SomeBehaviour b <- SessionManager.behaviours <> Base.behaviours, k <- behCases b, k `notElem` map ceKey catalogue ]
        `shouldBe` []
    it "name no name twice" $ do
      let names = map someName (SessionManager.behaviours <> Base.behaviours)
      L.nub names `shouldBe` names
    it "show their name, hash and cases, and their run as present" $
      show SessionManager.properties `shouldSatisfy` \s -> "SessionManager.properties" `L.isInfixOf` s && "behRun = <run>" `L.isInfixOf` s

  describe "Get's Fails list" $ do
    it "has items a to d with the IR's hashes: a NOT_AUTHORIZED, b to d INVALID_PARAMETER, each with its property" $ do
      failsName Base.getFails `shouldBe` "Base.getFails"
      [ (fiLabel i, fiHash i, fiStatus i) | i <- failsItems Base.getFails ] `shouldBe`
        [ ("a", "fbdda8fb3e3a", NOT_AUTHORIZED), ("b", "ca5584e1ccdf", INVALID_PARAMETER)
        , ("c", "46db34a6034d", INVALID_PARAMETER), ("d", "1b371d07bd3b", INVALID_PARAMETER) ]
      show (forFails Base.getFails) `shouldSatisfy` ("Base.getFails" `L.isInfixOf`)
      show (forFails Base.getFails) `shouldSatisfy` ("Just <property>" `L.isInfixOf`)
    forM_ (failsItems Base.getFails) $ \i ->
      it ("item " <> T.unpack (fiLabel i) <> " holds over the simulator, the method failing with " <> show (fiStatus i)) $
        fromMaybe (counterexample "the item has no property (Hsed.Sim.Base)" (property False)) (fiProp i)

  describe "runSome" $ do
    it "runs a behaviour on its method's call alone: Properties answered, StartSession not its; StartSession's own answers SyncSession and opens the session (Core 3.3.4.5, PDF 62)" $ do
      case runSome (forMethod SessionManager.properties) (invoked propertiesCall) propertiesCall st0 of
        Just (Right (ReplyCall r, st')) -> do
          _header r `shouldBe` MethodHeader uSMUID uProperties
          _statusCode r `shouldBe` 0
          st' `shouldBe` st0
        other -> expectationFailure (show other)
      let start = invocation uSMUID Methods.StartSession
            { Methods.startSessionHostSessionID         = value (1 :: Natural)
            , Methods.startSessionSPID                  = value (token uSP_Admin)
            , Methods.startSessionWrite                 = value (1 :: Natural)
            , Methods.startSessionHostChallenge         = Nothing
            , Methods.startSessionHostExchangeAuthority = Nothing
            , Methods.startSessionHostExchangeCert      = Nothing
            , Methods.startSessionHostSigningAuthority  = Nothing
            , Methods.startSessionHostSigningCert       = Nothing
            , Methods.startSessionSessionTimeout        = Nothing
            , Methods.startSessionTransTimeout          = Nothing
            , Methods.startSessionInitialCredit         = Nothing
            , Methods.startSessionSignedHash            = Nothing
            }
      isNothing (runSome (forMethod SessionManager.properties) (invoked start) start st0) `shouldBe` True
      case runSome (forMethod SessionManager.startSession) (invoked start) start st0 of
        Just (Right (ReplyCall r, st')) -> do
          _header r `shouldBe` MethodHeader uSMUID uSyncSession
          _statusCode r `shouldBe` 0
          Map.size (tsSessions st') `shouldBe` 1
        other -> expectationFailure (show other)
    it "reads the parameters against the signature: ETC-08's required list is a ParamError" $ do
      let etc08 = propertiesCall { _parameters = Parameters [L (List [])] [] }
      runSome (forMethod SessionManager.properties) (invoked etc08) etc08 st0 `shouldSatisfy` \r -> case r of
        Just (Left (RequiredCount 0 1)) -> True
        _                               -> False

  describe "closeSessionCall" $
    it "is the TPer's SMUID.CloseSession with the session's HSN as RemoteSessionNumber and its TSN as LocalSessionNumber (Core 5.2.3.5)" $ do
      let (s, _) = openSession base 0x51 uSP_Admin True [] st0
          mc = SessionManager.closeSessionCall s
      _header mc `shouldBe` MethodHeader uSMUID uCloseSession
      _statusCode mc `shouldBe` 0
      fmap (fmap (\c -> (Methods.closeSessionRemoteSessionNumber c, Methods.closeSessionLocalSessionNumber c))) (fromCall @Methods.CloseSession mc)
        `shouldBe` Just (Right (value (0x51 :: Natural), value (4096 :: Natural)))
