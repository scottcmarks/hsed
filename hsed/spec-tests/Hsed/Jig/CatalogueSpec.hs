{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.CatalogueSpec
Description : Test specifications for Hsed.Jig.Catalogue and Hsed.Jig.Prereq
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The case keys @--tests@ takes and prints; the catalogue's ten cases of M4a
and eight of M5b with their titles and pages (Test Cases 1.02: UCT-01 PDF
29, UCT-02 PDF 31, UCT-03 PDF 32, SPF-02 cases 1 and 2 PDF 52, SPF-11 PDF
64, SPF-12 case 1 PDF 65, ETC-02 PDF 77, ETC-03 and ETC-04 PDF 78, ETC-07
PDF 80, ETC-08 and ETC-09 PDF 81, ETC-14 PDF 87, ETC-15 PDF 88, ETC-20 PDF
91, ETC-21 PDF 92; the PDF's outline), each with the case the runner runs,
its section, waivers, own prerequisites and needs, and the keys the laws
name (ETC-02, SPF-02:1, ETC-04, ETC-15); the common prerequisites of
sections 5.2 (PDF 29), 6.2 (PDF 49) and 7.2 (PDF 76), and the section each
family's tests are in.
-}
module Hsed.Jig.CatalogueSpec (spec) where

import           RIO
import           Test.Hspec

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue
import           Hsed.Jig.Prereq
import           Hsed.Jig.Run       (cases)


key :: TestId -> Maybe Int -> CaseKey
key t c = CaseKey t c Nothing Nothing

spec :: Spec
spec = do
  describe "parseCaseKeys and renderCaseKey" $ do
    it "read M3's four keys" $
      parseCaseKeys "UCT-01,ETC-02,ETC-03,SPF-02:1"
        `shouldBe` Right [key (UCT 1) Nothing, key (ETC 2) Nothing, key (ETC 3) Nothing, key (SPF 2) (Just 1)]
    it "round trip a step range, a variant, and a case's variant (M5b)" $ do
      fmap (map renderCaseKey) (parseCaseKeys "UCT-03:1a-1c") `shouldBe` Right ["UCT-03:1a-1c"]
      fmap (map renderCaseKey) (parseCaseKeys "ETC-21/adminsp") `shouldBe` Right ["ETC-21/adminsp"]
      parseCaseKeys "SPF-12:1/no-media" `shouldBe` Right [CaseKey (SPF 12) (Just 1) Nothing (Just "no-media")]
      fmap (map renderCaseKey) (parseCaseKeys "SPF-12:1/no-media") `shouldBe` Right ["SPF-12:1/no-media"]
      parseCaseKeys "UCT-03:1a-1c,ETC-21/adminsp"
        `shouldBe` Right [CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing, CaseKey (ETC 21) Nothing Nothing (Just "adminsp")]
    it "refuses what is no key" $
      map (isLeft . parseCaseKeys) ["", "UCT", "XYZ-01", "UCT-01:", "UCT-01/", "UCT-x1"] `shouldBe` replicate 6 True

  describe "catalogue" $ do
    it "holds M4a's ten cases and M5b's eight in the Test Cases' order, at their pages (the PDF's outline; printed one less)" $
      [ (renderCaseKey (ceKey e), cePdf e, cePrinted e) | e <- catalogue ]
        `shouldBe` [ ("UCT-01", 29, 28), ("UCT-02", 31, 30), ("UCT-03:1a-1c", 32, 31), ("SPF-02:1", 52, 51), ("SPF-02:2", 52, 51)
                   , ("SPF-11", 64, 63), ("SPF-12:1/no-media", 65, 64), ("ETC-02", 77, 76), ("ETC-03", 78, 77), ("ETC-04", 78, 77)
                   , ("ETC-07", 80, 79), ("ETC-08", 81, 80), ("ETC-09", 81, 80), ("ETC-14", 87, 86), ("ETC-15", 88, 87)
                   , ("ETC-20", 91, 90), ("ETC-21", 92, 91), ("ETC-21/adminsp", 92, 91) ]
    it "gives the pages of UCT-01, ETC-02, ETC-03 and SPF-02:1 as 29, 77, 78 and 52" $
      [ cePdf e | Right ks <- [parseCaseKeys "UCT-01,ETC-02,ETC-03,SPF-02:1"], k <- ks, e <- catalogue, ceKey e == k ]
        `shouldBe` [29, 77, 78, 52]
    it "titles them as the Test Cases head them (ETC-02's, ETC-07's, ETC-08's, ETC-14's and ETC-15's dashes en dashes, ETC-04's a hyphen)" $
      map ceTitle catalogue `shouldBe`
        [ "Level 0 Discovery"
        , "Properties"
        , "Taking Ownership of an SD, steps 1a to 1c: a session to the Admin SP, the MSID read, the session closed"
        , "IF-RECV Behavior Tests, Case 1: an IF-RECV while the SD is in Awaiting IF-SEND"
        , "IF-RECV Behavior Tests, Case 2: an IF-RECV with an insufficient transfer length"
        , "Stack Reset"
        , "TPer Reset, Case 1, steps 1 to 5: no media I/O"
        , "General \x2013 IF-SEND/IF-RECV Synchronous Protocol"
        , "Invalid IF-SEND Transfer Length"
        , "Invalid SessionID - Regular Session"
        , "Unexpected Token Outside of Method \x2013 Control Session"
        , "Unexpected Token in the Method Parameter List \x2013 Control Session"
        , "Exceeding Transaction Limit"
        , "Exceed TPer Properties \x2013 Regular Session"
        , "Exceed TPer Properties \x2013 Control Session"
        , "StartSession on Inactive Locking SP"
        , "StartSession with Incorrect HostChallenge"
        , "StartSession with Incorrect HostChallenge, the Admin SP variant: SID with a wrong HostChallenge" ]
    it "has a case for each entry, and an entry for each case, in its order" $
      map cKey cases `shouldBe` map ceKey catalogue
    it "holds the cases the synchronous protocol's laws name: ETC-02, SPF-02 case 1, ETC-04 and ETC-15" $
      [ k | k <- [key (ETC 2) Nothing, key (SPF 2) (Just 1), key (ETC 4) Nothing, key (ETC 15) Nothing], k `elem` map ceKey catalogue ]
        `shouldBe` [key (ETC 2) Nothing, key (SPF 2) (Just 1), key (ETC 4) Nothing, key (ETC 15) Nothing]
    it "puts each case in its family's section; M4a's ten waive LockingManufactured as never touching the Locking SP, of M5b's eight ETC-20 alone, its own prerequisite contradicting it (PDF 91)" $ do
      [ cSection c | c <- cases ] `shouldBe` [ sectionOf (ckTest (cKey c)) | c <- cases ]
      [ renderCaseKey (cKey c) | c <- cases, lockingSPUntouched `elem` cWaives c ]
        `shouldBe` ["UCT-01", "UCT-02", "UCT-03:1a-1c", "SPF-02:1", "ETC-02", "ETC-03", "ETC-04", "ETC-08", "ETC-15", "ETC-21/adminsp"]
      [ (renderCaseKey (cKey c), wReason w) | c <- cases, w <- cWaives c, wPrereq w == LockingManufactured, w /= lockingSPUntouched ]
        `shouldBe` [("ETC-20", "contradicted by the case's own prerequisite 1, the Locking SP Manufactured-Inactive (Test Cases ETC-20, PDF 91)")]
    it "gives ETC-20 alone a prerequisite of its own, the Locking SP Manufactured-Inactive (Test Cases PDF 91)" $
      [ (renderCaseKey (cKey c), cOwnPrereqs c) | c <- cases, not (null (cOwnPrereqs c)) ] `shouldBe` [("ETC-20", [LockingManufacturedInactive])]
    it "needs of a target what each case does: the error paths ETC-02, ETC-03, ETC-07, ETC-08, ETC-09, ETC-14 and ETC-15, the simulator alone ETC-21/adminsp and M5b's eight, a write SPF-11, SPF-12:1/no-media and ETC-14, IF-SEND UCT-02, UCT-03 and ETC-04" $ do
      [ renderCaseKey (cKey c) | c <- cases, NeedErrorPath `elem` cNeeds c ] `shouldBe` ["ETC-02", "ETC-03", "ETC-07", "ETC-08", "ETC-09", "ETC-14", "ETC-15"]
      [ renderCaseKey (cKey c) | c <- cases, NeedSimOnly `elem` cNeeds c ]
        `shouldBe` ["SPF-02:2", "SPF-11", "SPF-12:1/no-media", "ETC-07", "ETC-09", "ETC-14", "ETC-20", "ETC-21", "ETC-21/adminsp"]
      [ renderCaseKey (cKey c) | c <- cases, NeedWrite `elem` cNeeds c ] `shouldBe` ["SPF-11", "SPF-12:1/no-media", "ETC-14"]
      [ renderCaseKey (cKey c) | c <- cases, NeedIfSend `elem` cNeeds c ] `shouldBe` ["UCT-02", "UCT-03:1a-1c", "ETC-04"]
      [ renderCaseKey (cKey c) | c <- cases, cNeeds c == [NeedLevel0] ] `shouldBe` ["UCT-01"]

  describe "common" $ do
    it "lists 5.2's six prerequisites (PDF 29) and 6.2's, the same (PDF 49)" $ do
      common UseCases `shouldBe` zip [SyncIdle, LockingManufactured, CredentialsKnown, HostChallengeIsCurrentPin, ReadWriteSessions, NoSessions] [1 ..]
      common SpecificFunctionality `shouldBe` common UseCases
    it "lists 7.2's five, with no item about HostChallenge (PDF 76)" $
      common ErrorCases `shouldBe` zip [SyncIdle, LockingManufactured, CredentialsKnown, ReadWriteSessions, NoSessions] [1 ..]
    it "has each list at its page, each family in its section" $ do
      map listAt [minBound .. maxBound] `shouldBe` [("5.2", 29), ("6.2", 49), ("7.2", 76)]
      map sectionOf [UCT 1, SPF 2, ETC 2] `shouldBe` [UseCases, SpecificFunctionality, ErrorCases]
