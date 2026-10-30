{-|
Module      : Hsed.SpecialPurposeUIDs
Description : The special purpose UIDs and Session Manager method UIDs, generated from the PDF
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Since M2d a re-export of the Core Table 239 and Table 241 names of
"Hsed.Gen.Core.UIDs" (design docs/spec-as-source.md section 4.1). The hand
transcription this module held is the oracle's test fixture,
hsed/fixture/Hand/SpecialPurposeUIDs.hs (section 5.5); the names it gave UIDs
the PDF names nowhere (uNULL, uC_PINCharSetUnrestrictedUID) and uSMMethodUID
stay there.
-}
module Hsed.SpecialPurposeUIDs
  ( -- * Table 239 Special Purpose UIDs: the invoking IDs the PDF names
    specialPurposeUIDs
  , uSMUID
  , uThisSP
    -- * Table 241 Session Manager Method UIDs
  , sessionManagerMethodUIDs
  , uCloseSession
  , uProperties
  , uStartSession
  , uStartTrustedSession
  , uSyncSession
  , uSyncTrustedSession
  ) where

import           Hsed.Gen.Core.UIDs (sessionManagerMethodUIDs, specialPurposeUIDs, uCloseSession, uProperties, uSMUID,
                                     uStartSession, uStartTrustedSession, uSyncSession, uSyncTrustedSession, uThisSP)
