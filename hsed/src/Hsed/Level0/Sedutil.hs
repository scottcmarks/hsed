{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Level0.Sedutil
Description : sedutil's printed Level 0 (its --query text) read, compared with a decoded response, and rendered as one
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What @sedutil-cli --query@ (1.49.1) prints of a drive's Level 0 Discovery
response, the oracle the M3 fixture @spec/fixtures/disk6-query.txt@ holds
(design 10.2; sedutil has no @--json@): a line naming the device, then for
each feature it knows a heading, @TPer function (0x0001)@, and lines of
@name = value@ pairs separated by commas (one by a full stop), each value
@Y@ or @N@, a decimal or hex number, or a number with another in
parentheses (@8 (4096)@). Two things 1.49.1 prints otherwise are read and
not compared ('sqSkipped'): the Supported Data Removal Mechanism block
(@DataRemoval 1.n function (0x0404)@, its values not @name = value@,
sedutil a7eafd5's DtaDev.cpp:368-377) and the count of codes it does not know
(@**** n **** Unknown function codes IGNORED@, DtaDev.cpp:383). Later
sedutil's labels (136389c's @ACKNACK@) are outside 1.49.1's and this
module's scope.

Each label sedutil prints names a field of hsed's layout of the code
('compareQuery' and 'synthesize' share the map): Opal 2.30's TPer, Locking,
Geometry Reporting and Opal SSC V2 descriptors (generated) and the
DataStore and Block SID descriptors ("Hsed.Sim.FeatureSets"). sedutil prints
no line of a descriptor's version, of Locking's HW Reset for LOR/DOR
Supported or of Opal SSC V2's SSC Minor Version Number, and none at all of
the Configurable Namespace Locking descriptor (0x0403) that the kext reports
on disk6 (design 11, M3): 'synthesize' adds one, and the jig's @level0
decode --against@ asks for one, both for disk6, an assumption the commands'
help names.

'compareQuery' holds a decoded response to every value sedutil printed (its
@OPAL 2.15@ heading is sedutil's arithmetic, not the drive's version,
DtaDev.cpp:302); 'synthesize' renders a drive-like response from them, a
test aid until the drive's own is captured: each descriptor sedutil printed,
its printed values as printed (a pin of hsed's layout included), every other
field what the descriptor's allowances give it (the version an 'AnyVersion'
prints, the least a 'Minimum' allows, zero for the rest), and a Configurable
Namespace Locking descriptor of zeros but its pins, in increasing code order,
zero-padded to the transfer length a capture takes.
-}
module Hsed.Level0.Sedutil
  ( SedutilQuery (..)
  , SedutilFeature (..)
  , parseQuery
  , compareQuery
  , synthesize
  ) where

import           Numeric                (readHex, showHex)
import qualified RIO.ByteString         as B
import           RIO.Char               (isDigit, isHexDigit, isSpace)
import qualified RIO.List               as L
import qualified RIO.NonEmpty           as NE
import           RIO
import qualified RIO.Text               as T

import           Hsed.Core.Feature      (Allowance (..), Feature (..))
import           Hsed.Core.Layout       (Content (..), Decoded (..), Field (..), FieldKind (..), Layout (..),
                                         LayoutError, decode, encode)
import           Hsed.Core.Record       (LayoutRecord (..))
import           Hsed.Gen.Core.Layouts  (FeatureDescriptorTemplate (..), FeatureDescriptorTemplateHeader (..),
                                         featureDescriptorTemplateLayout)
import           Hsed.Gen.Opal.Features (Level0DiscoveryGeometryReportingFeature, Level0DiscoveryLockingFeature,
                                         Level0DiscoveryOpalSSCV2Feature, Level0DiscoverySupportedDataRemovalMechanismFeature,
                                         Level0DiscoveryTPerFeature)
import           Hsed.Jig.Options       (RunOptions (..), defaultRunOptions)
import           Hsed.Level0            (DecodedDescriptor (..), Descriptor (..), Level0 (..), decodeDescriptor,
                                         renderLevel0)
import           Hsed.Sim.FeatureSets   (BlockSIDFeature, DataStoreFeature, NamespaceLockingFeature)


-- | sedutil's printed Level 0: the device line as printed, each feature's
--   heading and values, and the lines read and not compared (the Data
--   Removal block, the count of unknown codes).
data SedutilQuery = SedutilQuery
  { sqDevice   :: !Text
  , sqFeatures :: ![SedutilFeature]
  , sqSkipped  :: ![Text]
  }
  deriving (Eq, Show)

-- | One feature as sedutil prints it: the code its heading names, the
--   heading's words before it, and each value by its printed name (@Y@ 1,
--   @N@ 0, a number as itself, @8 (4096)@ 8).
data SedutilFeature = SedutilFeature
  { sfCode   :: !Word16
  , sfName   :: !Text
  , sfValues :: ![(Text, Integer)]
  }
  deriving (Eq, Show)


-- * Reading

-- | sedutil's @--query@ text read: blank lines skipped, the first line the
--   device's, then each feature's heading (a line at the margin) and its
--   indented value lines; the Data Removal block (code 0x0404) and the
--   count of unknown codes kept as lines not compared. A line that is none
--   of these is refused, by its number.
parseQuery :: Text -> Either Text SedutilQuery
parseQuery text = case filter (not . T.null . T.strip . snd) (zip [1 :: Int ..] (T.lines text)) of
  [] -> Left "no device line"
  (n, device) : rest
    | indented device || isJust (heading device) -> Left (at n "the first line names no device")
    | otherwise -> do
        groups <- traverse feature (NE.groupBy (\_ next -> indented (snd next)) rest)
        pure (SedutilQuery (T.strip device) (lefts groups) (concat (rights groups)))
  where
    feature lines'
      | unknownCodes line = Right (Right (map (T.strip . snd) (NE.toList lines')))
      | otherwise = do
          (name, code) <- maybe (Left (at n ("no feature heading: " <> T.strip line))) Right (heading line)
          if code == dataRemovalCode
            then Right (Right (map (T.strip . snd) (NE.toList lines')))
            else Left . SedutilFeature code name . concat <$> traverse pairs (NE.tail lines')
      where (n, line) = NE.head lines'
    indented = maybe False (isSpace . fst) . T.uncons
    at n e = "line " <> tshow n <> ": " <> e
    -- @**** n **** Unknown function codes IGNORED@ (DtaDev.cpp:383 at a7eafd5)
    unknownCodes line = "****" `T.isPrefixOf` T.strip line && "Unknown function codes" `T.isInfixOf` line

-- | The code sedutil 1.49.1 prints its Data Removal block under, Opal's
--   Supported Data Removal Mechanism (Table 8, PDF 25; the generated
--   descriptor's code), whose values it prints other than as @name = value@.
dataRemovalCode :: Word16
dataRemovalCode = fromInteger (featureCode (Proxy :: Proxy Level0DiscoverySupportedDataRemovalMechanismFeature))

-- | A feature's heading, @TPer function (0x0001)@: its name and its code.
heading :: Text -> Maybe (Text, Word16)
heading line = do
  inner <- T.stripSuffix ")" (T.strip line)
  let parenthesized = T.takeWhileEnd (/= '(') inner
  name <- T.stripSuffix " function" (T.stripEnd (T.dropEnd (T.length parenthesized + 1) inner))
  code <- T.stripPrefix "0x" parenthesized >>= hexNumber
  guard (not (T.null name) && code <= toInteger (maxBound :: Word16))
  pure (name, fromInteger code)

-- | A value line's pairs, @name = value@, separated by commas or full stops
--   (no value sedutil prints holds either).
pairs :: (Int, Text) -> Either Text [(Text, Integer)]
pairs (n, line) = traverse pair (filter (not . T.null) (map T.strip (T.split (`elem` [',', '.']) line)))
  where
    pair piece = case T.break (== '=') piece of
      (name, rest)
        | not (T.null rest), not (T.null (T.strip name)), Just v <- printedValue (T.strip (T.drop 1 rest)) ->
            Right (T.strip name, v)
      _ -> Left ("line " <> tshow n <> ": no name = value: " <> piece)

-- | A value as sedutil prints it: @Y@ 1, @N@ 0, a hex number, a decimal, or a
--   decimal and another in parentheses (the first: @8 (4096)@ is 8).
printedValue :: Text -> Maybe Integer
printedValue t
  | t == "Y"                       = Just 1
  | t == "N"                       = Just 0
  | Just h <- T.stripPrefix "0x" t = hexNumber h
  | otherwise                      = case T.words t of
      [d] -> decimal d
      [d, aside]
        | Just inner <- T.stripPrefix "(" aside >>= T.stripSuffix ")", isJust (decimal inner) -> decimal d
      _   -> Nothing
  where
    decimal :: Text -> Maybe Integer
    decimal d = if not (T.null d) && T.all isDigit d then readMaybe (T.unpack d) else Nothing

hexNumber :: Text -> Maybe Integer
hexNumber h
  | not (T.null h), T.all isHexDigit h, [(v, "")] <- readHex (T.unpack h) = Just v
  | otherwise                                                             = Nothing


-- * What sedutil prints, and hsed's fields

-- | Each descriptor hsed reads sedutil's values of, by code: its layout, its
--   allowances, and each label sedutil prints (as
--   @spec/fixtures/disk6-query.txt@ has them) with the field of the layout
--   that holds its value.
printed :: [(Word16, (Layout, [(Text, Allowance)], [(Text, Text)]))]
printed =
  [ entry (Proxy :: Proxy Level0DiscoveryTPerFeature)
      [ ("ACKNAK", "ACK/NAK Supported"), ("ASYNC", "Async Supported"), ("BufferManagement", "Buffer Mgmt Supported")
      , ("comIDManagement", "ComID Mgmt Supported"), ("Streaming", "Streaming Supported"), ("SYNC", "Sync Supported") ]
  , entry (Proxy :: Proxy Level0DiscoveryLockingFeature)
      [ ("Locked", "Locked"), ("LockingEnabled", "Locking Enabled")
      , ("MBR shadowing Not Supported", "MBR Shadowing Not Supported"), ("MBRDone", "MBR Done")
      , ("MBREnabled", "MBR Enabled"), ("MediaEncrypt", "Media Encryption") ]
  , entry (Proxy :: Proxy Level0DiscoveryGeometryReportingFeature)
      [ ("Align", "ALIGN"), ("Alignment Granularity", "AlignmentGranularity"), ("Logical Block size", "LogicalBlockSize")
      , ("Lowest Aligned LBA", "LowestAlignedLBA") ]
  , entry (Proxy :: Proxy DataStoreFeature)
      [ ("Max Tables", "Maximum number of DataStore Tables"), ("Max Size Tables", "Maximum total size of DataStore Tables")
      , ("Table size alignment", "DataStore Table size alignment") ]
  , entry (Proxy :: Proxy Level0DiscoveryOpalSSCV2Feature)
      [ ("Base comID", "Base ComID"), ("Initial PIN", "Initial C_PIN_SID PIN Indicator")
      , ("Reverted PIN", "Behavior of C_PIN_SID PIN upon TPer Revert"), ("comIDs", "Number of ComIDs")
      , ("Locking Admins", "Number of Locking SP Admin Authorities Supported")
      , ("Locking Users", "Number of Locking SP User Authorities Supported"), ("Range Crossing", "Range Crossing Behavior") ]
  , entry (Proxy :: Proxy BlockSIDFeature)
      [ ("BlockSIDState", "SID Blocked State"), ("SIDvalueState", "SID Value State"), ("HardReset", "Hardware Reset") ]
  , entry namespaceLocking []
  ]
  where
    entry :: Feature f => Proxy f -> [(Text, Text)] -> (Word16, (Layout, [(Text, Allowance)], [(Text, Text)]))
    entry p labels = (fromInteger (featureCode p), (recordLayout p, featureAllowances p, labels))

-- | The Configurable Namespace Locking descriptor, which sedutil does not print.
namespaceLocking :: Proxy NamespaceLockingFeature
namespaceLocking = Proxy


-- * Comparing

-- | Each value sedutil printed that a decoded response does not hold, said
--   in words; none when they agree. A feature of a code the response lacks,
--   or whose descriptor hsed does not decode, and a label hsed maps to no
--   field, are each said too.
compareQuery :: SedutilQuery -> Level0 -> [Text]
compareQuery q l0 = concatMap feature (sqFeatures q)
  where
    feature sf = case (L.find ((== sfCode sf) . dCode) (l0Descriptors l0), L.lookup (sfCode sf) printed) of
      (Nothing, _)                 -> [named sf <> ": the response has no descriptor of that code"]
      (_, Nothing)                 -> [named sf <> ": hsed reads no value sedutil prints of that code"]
      (Just d, Just (_, _, labels)) -> case decodeDescriptor d of
        Nothing -> [named sf <> ": its descriptor is not decoded (" <> tshow (B.length (dBytes d)) <> " bytes)"]
        Just dd -> concatMap (value sf dd labels) (sfValues sf)
    value sf dd labels (label, v) = case L.lookup label labels of
      Nothing    -> [named sf <> ": sedutil's " <> label <> " names no field hsed knows"]
      Just field -> case L.lookup field (ddFields dd) of
        Just (Number n)
          | n == v    -> []
          | otherwise -> [named sf <> " " <> field <> ": sedutil printed " <> label <> " = " <> tshow v
                          <> ", the response holds " <> tshow n]
        _             -> [named sf <> " " <> field <> ": no such number in the decoded descriptor"]
    named sf = sfName sf <> " (0x" <> T.pack (showHex (sfCode sf) "") <> ")"


-- * Synthesizing

-- | A drive-like response from sedutil's values (and a Configurable
--   Namespace Locking descriptor, which sedutil does not print), as long as
--   a capture is (hsed-jig's @--level0-len@). A feature of a code hsed has
--   no layout of, or whose values its layout cannot hold, is left out.
synthesize :: SedutilQuery -> ByteString
synthesize q = renderLevel0 mempty (L.sortOn dCode (mapMaybe render codes)) (roLevel0Len defaultRunOptions)
  where
    codes = L.nub (map sfCode (sqFeatures q) <> [fromInteger (featureCode namespaceLocking)])
    render code = do
      (layout, allowances, labels) <- L.lookup code printed
      let values = maybe [] sfValues (L.find ((== code) . sfCode) (sqFeatures q))
          given  = [ (field, v) | (label, v) <- values, Just field <- [L.lookup label labels] ]
      either (const Nothing) Just $
        encode (unpinned (map fst given) layout) (fieldValues layout allowances given) >>= asDescriptor

-- | Every field of a layout that takes a value: what sedutil printed of it,
--   or what its allowance gives it (the version an 'AnyVersion' prints,
--   the least a 'Minimum' allows), or zero.
fieldValues :: Layout -> [(Text, Allowance)] -> [(Text, Integer)] -> [(Text, Content)]
fieldValues layout allowances given =
  [ (fieldName f, valueOf f) | f <- layoutFields layout, takesValue f ]
  where
    takesValue f = case fieldKind f of
      Reserved -> False
      Pinned _ -> fieldName f `elem` map fst given
      _        -> True
    valueOf f = case (fieldKind f, L.lookup (fieldName f) given, L.lookup (fieldName f) allowances) of
      (Bytes, _, _)                -> Octets (B.replicate (fieldWidth f `div` 8) 0)
      (_, Just v, _)               -> Number v
      (_, _, Just (AnyVersion v))  -> Number v
      (_, _, Just (Minimum v))     -> Number v
      _                            -> Number 0

-- | A layout whose pins on the named fields are lifted, so that they hold
--   what sedutil printed even where hsed's layout pins another value.
unpinned :: [Text] -> Layout -> Layout
unpinned names layout = layout { layoutFields = map unpin (layoutFields layout) }
  where
    unpin f = case fieldKind f of
      Pinned _ | fieldName f `elem` names -> f { fieldKind = Unsigned }
      _                                   -> f

-- | A descriptor's bytes with the code and version they hold (Core Table 41).
asDescriptor :: ByteString -> Either LayoutError Descriptor
asDescriptor bytes = do
  Decoded fields _ _ <- decode featureDescriptorTemplateLayout bytes
  FeatureDescriptorTemplate (FeatureDescriptorTemplateHeader code version _) _ <- fromFields fields
  pure (Descriptor code version bytes)
