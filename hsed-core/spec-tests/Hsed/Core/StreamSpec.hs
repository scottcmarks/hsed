{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Core.StreamSpec
Description : Test specifications for Hsed.Core.Stream
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The total reading of a subpacket's payload (Core 3.2.2.4, PDF 38-39): every
token the codec writes is one atom; Empty atoms are dropped; the policies
refuse what Opal's TPer does not take (Table 15, PDF 31) and what is past
MaxIndTokenSize; invocations are read whole, M0's Properties and ETC-08's
among them, and a token out of place is a fault at 3.2.2.4.2's place, read
past to the next invocation in the header and the parameter list, stopping
everywhere else; End of Session and the transaction tokens by stream; the
host's reading of what came back; and the round trip: what the codec writes,
the stream reads.
-}
module Hsed.Core.StreamSpec (spec) where

import           Data.ByteString       (pack)
import qualified RIO.ByteString        as B
import           RIO
import qualified RIO.Text              as T
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck       hiding (generate)

import           Hsed.Core.Call
import           Hsed.Core.Stream
import           Hsed.Core.StreamItem  (StreamItem (..))
import           Hsed.Core.Token
import           Hsed.Core.UID         (UID, uid)
import           Hsed.Core.Value


-- | The SMUID and the Properties method UID (Core Spec Tables 239 and 241),
--   spelled out here so the codec's tests do not depend on the spec data package.
uSMUID, uProperties :: UID
uSMUID      = uid 0x00 0x00 0x00 0x00  0x00 0x00 0x00 0xFF
uProperties = uid 0x00 0x00 0x00 0x00  0x00 0x00 0xFF 0x01

propertiesHeader :: MethodHeader
propertiesHeader = MethodHeader uSMUID uProperties

-- | M0's SMUID.Properties[] (design section 11): Call, the two UIDs, an empty
--   parameter list, End of Data and the status list.
propertiesBytes :: ByteString
propertiesBytes = pack
  [ 0xF8, 0xA8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xA8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x01
  , 0xF0, 0xF1, 0xF9, 0xF0, 0x00, 0x00, 0x00, 0xF1 ]

-- | ETC-08's payload (Test Cases PDF 81): Properties with one required
--   parameter, an empty list, where the signature has none.
etc08Bytes :: ByteString
etc08Bytes = pack
  [ 0xF8, 0xA8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xA8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x01
  , 0xF0, 0xF0, 0xF1, 0xF1, 0xF9, 0xF0, 0x00, 0x00, 0x00, 0xF1 ]

-- | The tokens of a byte string under a policy taking whatever the codec writes.
tokensOf :: ByteString -> Either (Int, Text) [Token]
tokensOf = fmap (map atomToken) . atoms (AtomPolicy Nothing True False)

-- | The tokens of a byte string the tests know to read.
toks :: ByteString -> [Token]
toks = either (const []) id . tokensOf

smuid, properties :: Token
smuid      = token uSMUID
properties = token uProperties

-- | The tokens of a Properties invocation from its header to its End of Data.
callBody :: [Token]
callBody = [Call, smuid, properties, StartList, EndList, EndOfData]

status :: [Token]
status = [StartList, Unsigned 0, Unsigned 0, Unsigned 0, EndList]

-- | The place and index of each fault among the items, and of the one that stopped the reading.
summary :: ([Item], Maybe Fault) -> ([Either (Int, Place) Item], Maybe (Int, Place))
summary (is, f) = (map place is, at <$> f)
  where
    place (BadCall x) = Left (at x)
    place i           = Right i
    at x = (faultAt x, faultPlace x)

spec :: Spec
spec = do
  describe "atoms" $ do
    prop "reads every token the codec writes as one atom of the bytes written" $
      forAll (arbitrary `suchThat` plain) $ \t ->
        atoms hostPolicy (generate t) === Right [Atom 0 (B.length (generate t)) t]
    it "drops Empty atoms wherever they are, the offsets those of the atoms kept (Table 14)" $
      atoms hostPolicy (pack [0xFF, 0x01, 0xFF, 0xFF, 0x02, 0xFF]) `shouldBe` Right [Atom 1 1 (Unsigned 1), Atom 4 1 (Unsigned 2)]
    it "joins a continued byte sequence, under a policy that takes one, into one atom from its first segment to its last" $
      atoms (AtomPolicy Nothing True True) (generate (ContinuedBytes "ab") <> pack [0xFF] <> generate (Bytes "cd") <> generate (Unsigned 1))
        `shouldBe` Right [Atom 0 7 (Bytes "abcd"), Atom 7 1 (Unsigned 1)]
    it "refuses a continued byte sequence under the host's policy, at its offset" $
      atoms hostPolicy (generate (Unsigned 7) <> generate (ContinuedBytes "a")) `shouldSatisfy` failsAt 1 "continued"
    it "refuses a continued byte sequence that no byte sequence ends" $ do
      atoms (AtomPolicy Nothing True True) (generate (ContinuedBytes "a") <> generate (Unsigned 1)) `shouldSatisfy` failsAt 0 "no byte sequence ends"
      atoms (AtomPolicy Nothing True True) (generate (ContinuedBytes "a")) `shouldSatisfy` failsAt 0 "no byte sequence ends"
    it "refuses a signed integer under the TPer's policy (Opal Table 15), and takes it under the host's" $ do
      atoms (tperPolicy 1992) (generate (Signed (-1))) `shouldSatisfy` failsAt 0 "signed"
      atoms hostPolicy (generate (Signed (-1))) `shouldBe` Right [Atom 0 1 (Signed (-1))]
    it "refuses an atom larger than the TPer's MaxIndTokenSize, header and data counted; 0 means no limit (Table 167)" $ do
      atoms (tperPolicy 6) (generate (Bytes "12345")) `shouldBe` Right [Atom 0 6 (Bytes "12345")]
      atoms (tperPolicy 5) (generate (Unsigned 1) <> generate (Bytes "12345")) `shouldSatisfy` failsAt 1 "MaxIndTokenSize 5"
      atoms (tperPolicy 0) (generate (Bytes (B.replicate 3000 0))) `shouldBe` Right [Atom 0 3004 (Bytes (B.replicate 3000 0))]
    it "stops at a token that does not read: a TCG-reserved tag, a length of zero, bytes cut short" $ do
      atoms hostPolicy (pack [0x01, 0xE4]) `shouldSatisfy` failsAt 1 "does not read"
      atoms hostPolicy (pack [0x80]) `shouldSatisfy` failsAt 0 "does not read"
      atoms hostPolicy (pack [0x01, 0xA3, 0x01]) `shouldSatisfy` failsAt 1 "does not read"
    it "reads nothing from nothing" $
      atoms (tperPolicy 1992) mempty `shouldBe` Right []

  describe "items, a TPer's reading" $ do
    it "reads M0's SMUID.Properties[] as one invocation, on either stream" $ do
      let expected = ([Invoke (MethodCall propertiesHeader (Parameters [] []) 0)], Nothing)
      items ControlStream <$> tokensOf propertiesBytes `shouldBe` Right expected
      items SessionStream <$> tokensOf propertiesBytes `shouldBe` Right expected
    it "reads ETC-08's invocation, its one required parameter an empty list, as the host sent it" $
      items ControlStream <$> tokensOf etc08Bytes
        `shouldBe` Right ([Invoke (MethodCall propertiesHeader (Parameters [L (List [])] []) 0)], Nothing)
    it "reads the parameters as they are: required values, then the optional ones Named, and the host's status" $ do
      let ps = Parameters [D (Datum (Unsigned 1)), L (List [D (Datum (Bytes "ab"))])] [optionalParameter 0 (D (Datum (Unsigned 9)))]
      items SessionStream (toks (generate (MethodCall propertiesHeader ps 5)))
        `shouldBe` ([Invoke (MethodCall propertiesHeader ps 5)], Nothing)
    it "reads two invocations in order" $
      items ControlStream (toks (propertiesBytes <> propertiesBytes))
        `shouldBe` ([Invoke (MethodCall propertiesHeader (Parameters [] []) 0), Invoke (MethodCall propertiesHeader (Parameters [] []) 0)], Nothing)
    it "stops at a token outside an invocation, keeping the items before it (3.2.2.4.2 item 2)" $ do
      summary (items SessionStream (EndList : toks propertiesBytes)) `shouldBe` ([], Just (0, OutsideCall))
      summary (items ControlStream (toks propertiesBytes <> [EndName]))
        `shouldBe` ([Right (Invoke (MethodCall propertiesHeader (Parameters [] []) 0))], Just (11, OutsideCall))
    it "reads past a header that does not read, to its End of Data and status list: a BadCall in the header (item 3)" $ do
      summary (items SessionStream ([Call, EndList, EndOfData] <> status <> toks propertiesBytes))
        `shouldBe` ([Left (1, InHeader), Right (Invoke (MethodCall propertiesHeader (Parameters [] []) 0))], Nothing)
      summary (items ControlStream ([Call, smuid, Unsigned 1, StartList, EndList, EndOfData] <> status)) `shouldBe` ([Left (2, InHeader)], Nothing)
    it "reads past a token out of place in the parameter list: a BadCall in the parameters, with the header (item 4)" $ do
      summary (items SessionStream ([Call, smuid, properties, StartList, EndName, EndList, EndOfData] <> status))
        `shouldBe` ([Left (4, InParameters propertiesHeader)], Nothing)
      summary (items SessionStream ([Call, smuid, properties, StartList, StartName, Unsigned 0, Unsigned 1, EndName, Unsigned 2, EndList, EndOfData] <> status))
        `shouldBe` ([Left (8, InParameters propertiesHeader)], Nothing)
      summary (items SessionStream ([Call, smuid, properties, StartList, EndList, Unsigned 5, EndOfData] <> status))
        `shouldBe` ([Left (5, InParameters propertiesHeader)], Nothing)
    it "stops at a token after End of Data that is no Start List: outside the invocation (item 2)" $
      summary (items SessionStream (callBody <> [Unsigned 7] <> status)) `shouldBe` ([], Just (6, OutsideCall))
    it "stops in a status list of fewer than three values, or holding what is no uinteger (item 5), the header known" $ do
      summary (items SessionStream (callBody <> [StartList, Unsigned 0, Unsigned 0, EndList])) `shouldBe` ([], Just (9, InStatusList (Just propertiesHeader)))
      summary (items ControlStream (callBody <> [StartList, Unsigned 0, Signed (-1), Unsigned 0, EndList])) `shouldBe` ([], Just (8, InStatusList (Just propertiesHeader)))
    it "stops in the status list after a header that did not read, with no header" $
      summary (items SessionStream [Call, EndList, EndOfData, StartList, Unsigned 0, EndList]) `shouldBe` ([], Just (5, InStatusList Nothing))
    it "is unfinished, at the count of tokens, when the stream ends inside an invocation" $ do
      summary (items SessionStream [Call, smuid, properties, StartList]) `shouldBe` ([], Just (4, Unfinished))
      summary (items SessionStream [Call, EndList]) `shouldBe` ([], Just (2, Unfinished))
      summary (items SessionStream (callBody <> [StartList, Unsigned 0])) `shouldBe` ([], Just (8, Unfinished))
    it "ends a session stream at End of Session, what follows ignored, and skips it on the control stream (3.3.7.1.3, 3.2.2.3.4)" $ do
      items SessionStream (EndOfSession : toks propertiesBytes) `shouldBe` ([EndOfSessionItem], Nothing)
      items ControlStream (EndOfSession : toks propertiesBytes) `shouldBe` ([Invoke (MethodCall propertiesHeader (Parameters [] []) 0)], Nothing)
      items ControlStream [EndOfSession] `shouldBe` ([], Nothing)
    it "reads a transaction token with its status as an item of a session stream, and skips it on the control stream" $ do
      items SessionStream ([StartTransaction, Unsigned 0] <> toks propertiesBytes <> [EndTransaction, Unsigned 1])
        `shouldBe` ([StartTransactionItem 0, Invoke (MethodCall propertiesHeader (Parameters [] []) 0), EndTransactionItem 1], Nothing)
      items ControlStream ([StartTransaction, Unsigned 0] <> toks propertiesBytes <> [EndTransaction, Unsigned 1])
        `shouldBe` ([Invoke (MethodCall propertiesHeader (Parameters [] []) 0)], Nothing)
    it "stops a session stream at a transaction token without its uinteger status, outside an invocation" $ do
      summary (items SessionStream (EndTransaction : toks propertiesBytes)) `shouldBe` ([], Just (1, OutsideCall))
      summary (items SessionStream [StartTransaction]) `shouldBe` ([], Just (1, Unfinished))
    it "drops Empty atoms given among the tokens, the indexes those given" $
      summary (items SessionStream ([Empty, Call, Empty, EndList, EndOfData] <> status)) `shouldBe` ([Left (3, InHeader)], Nothing)
    prop "reads back the invocations the codec writes, each with its status" $
      \calls -> fmap (items SessionStream) (tokensOf (foldMap generate (calls :: [MethodCall]))) === Right (map Invoke calls, Nothing)

  describe "returnedItems, a host's reading" $ do
    it "reads a Session Manager response as the invocation it is, a method response with its status, the TPer's End of Session and a transaction token" $ do
      let response = MethodResponse [D (Datum (Unsigned 3)), N (optionalParameter 0 (D (Datum (Bytes "x"))))] 1
          answer   = MethodCall propertiesHeader (Parameters [L (List [])] []) 0
      returnedItems (toks (generate answer <> generate response <> generate EndOfSession <> generate EndTransaction <> generate (Unsigned 0)))
        `shouldBe` ([ReturnedCall answer, ReturnedResults response, ReturnedEndOfSession, ReturnedTransaction EndTransaction 0], Nothing)
    it "stops at a fault, keeping what came before it" $ do
      let at (rs, f) = (length rs, (\x -> (faultAt x, faultPlace x)) <$> f)
      at (returnedItems ([StartList, EndList, EndOfData, StartList, Unsigned 0, EndList] <> toks (generate EndOfSession))) `shouldBe` (0, Just (5, InResults))
      at (returnedItems [EndOfSession, StartList, EndList, Unsigned 7]) `shouldBe` (1, Just (3, InResults))
      at (returnedItems [EndName]) `shouldBe` (0, Just (0, OutsideCall))
      at (returnedItems ([Call, Unsigned 1] <> status)) `shouldBe` (0, Just (1, InHeader))
      at (returnedItems [StartList, EndList, EndOfData, StartList, Unsigned 0, Unsigned 0, Unsigned 0]) `shouldBe` (0, Just (7, Unfinished))
      at (returnedItems [StartTransaction]) `shouldBe` (0, Just (1, Unfinished))
    prop "reads back the responses the codec writes" $
      \rs -> fmap returnedItems (tokensOf (foldMap generate (rs :: [MethodResponse]))) === Right (map ReturnedResults rs, Nothing)
  where
    plain t = case t of
      Empty            -> False
      ContinuedBytes _ -> False
      _                -> True
    failsAt off what = \case
      Left (o, why) -> o == off && what `T.isInfixOf` why
      Right _       -> False
