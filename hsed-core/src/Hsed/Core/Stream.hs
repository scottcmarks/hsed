{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Core.Stream
Description : A subpacket's token stream read whole: its atoms under a policy, its invocations with the place of any fault (Core 3.2.2.4)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The reading of a data subpacket's payload, total and in one pass, which
"Hsed.Core.Call"'s parsers are not: they read one item from the front of a
byte string ('Hsed.Core.StreamItem.parseByteString' accepts a prefix) and say
nothing of where a stream goes wrong, where a TPer must (Core 3.2.2.4, PDF
38-39): what it does with an invalid or unexpected token depends on where in
the stream the token stands.

Two layers:

* 'atoms' reads the stream one token at a time ('Atom': its offset, its size
  with its header, its token), under an 'AtomPolicy': Empty atoms are dropped
  wherever they are (Table 14, PDF 36); an atom larger than the policy's
  MaxIndTokenSize (3.2.2.4.1 item 1, PDF 38; Table 167, PDF 142: a token's
  size is its header and its data), a signed integer or a continued byte
  sequence the policy does not take (Opal 2.30 3.3.4.1.1 and Table 15, PDF
  30-31: an Opal TPer supports the atoms whose S bit is zero) and a
  TCG-reserved tag (item 6) each stop the reading where they are; a continued
  byte sequence the policy takes is joined into one atom (3.2.2.3.1, PDF
  35-36).
* 'items' reads the tokens as a TPer does, by 'StreamMode': a control
  session's (End of Session and the transaction control tokens, each with its
  uinteger, ignored and not echoed, 3.3.7.1.3, PDF 73) or a regular
  session's (End of Session ends the reading, what follows ignored,
  3.2.2.3.4, PDF 38; 3.3.7.1.5, PDF 75). Each method invocation is read as
  3.2.4.2 encodes it (PDF 48-49): Call, the two eight-byte UIDs of the
  header, the parameter list (required values, then Named optional ones,
  3.2.4.1), End of Data, the status list of three or more uintegers, the
  first the host's status. A token out of place is a 'Fault' at a 'Place',
  3.2.2.4.2's items (PDF 39): in the header (item 3) or in the parameter
  list (item 4), the invocation is read to its End of Data and status list
  and given as a 'BadCall', which a regular session answers (NOT_AUTHORIZED,
  or INVALID_PARAMETER) and a control session ignores; outside an invocation
  (item 2), in the status list (item 5), or a stream ending inside an
  invocation ('Unfinished'; 3.3.10.3 a, PDF 83-84), the reading stops with
  the fault, the items before it kept (item 1: results for methods completed
  before the violating token are sent).
* 'returnedItems' is the host's reading of what a TPer sent: Session Manager
  responses, which are invocations ('ReturnedCall', 3.3.7.1.3), method
  responses ('ReturnedResults'), the TPer's End of Session and its
  transaction tokens; any fault stops it.

A 'Fault' is at a token's index among those given, 0 first: an 'Atom' knows
its offset, so a reader holding the atoms can say where in the bytes it lies.
-}
module Hsed.Core.Stream
  ( -- * Atoms
    Atom (..)
  , AtomPolicy (..)
  , hostPolicy
  , tperPolicy
  , atoms
    -- * The TPer's reading
  , StreamMode (..)
  , Place (..)
  , Fault (..)
  , Item (..)
  , items
    -- * The host's reading
  , Returned (..)
  , returnedItems
  ) where

import           Data.Attoparsec.ByteString (IResult (..), feed, parse)
import qualified RIO.ByteString             as B
import qualified RIO.List                   as L
import           RIO
import qualified RIO.Text                   as T

import           Hsed.Core.Call             (MethodCall (..), MethodHeader (..), MethodResponse (..), Parameters (..))
import           Hsed.Core.StreamItem       (Parser, StreamItem (..))
import           Hsed.Core.Token            (IsToken (..), Token (..))
import           Hsed.Core.UID              (UID)
import           Hsed.Core.Value            (Datum (..), List (..), Name (..), NamedValue (..), Value (..),
                                             maybeDatumToken)


-- * Atoms

-- | One token of a stream: where it starts, how many bytes it takes (its
--   header and its data, what Core Table 167's MaxIndTokenSize bounds), and
--   the token. A continued byte sequence joined is one atom, from its first
--   segment's start to its last segment's end.
data Atom = Atom
  { atomOffset :: !Int
  , atomSize   :: !Int
  , atomToken  :: !Token
  }
  deriving (Eq, Show)

-- | What atoms a reader takes.
data AtomPolicy = AtomPolicy
  { apMaxTokenSize :: !(Maybe Int)  -- ^ the reader's MaxIndTokenSize, if it has one (Core Table 167, PDF 142: 0 means no limit)
  , apSigned       :: !Bool         -- ^ signed integer atoms taken
  , apContinued    :: !Bool         -- ^ continued byte sequences taken, and joined
  }
  deriving (Eq, Show)

-- | The host's: every atom a TPer may send it. No MaxIndTokenSize (the jig's
--   host reads whatever came, to report it); signed integers (an integer
--   column's value); no continued byte sequence, the host's ContinuedTokens
--   assumption being False (Core Table 168, PDF 144) and the jig sending no
--   HostProperties that says otherwise.
hostPolicy :: AtomPolicy
hostPolicy = AtomPolicy { apMaxTokenSize = Nothing, apSigned = True, apContinued = False }

-- | An Opal TPer's, from its MaxIndTokenSize (0 meaning no limit, Core Table
--   167, PDF 142): the atoms whose S bit is zero, so neither signed integers
--   nor continued byte sequences (Opal 2.30 3.3.4.1.1, Table 15, PDF 30-31).
tperPolicy :: Int -> AtomPolicy
tperPolicy maxTokenSize = AtomPolicy
  { apMaxTokenSize = if maxTokenSize == 0 then Nothing else Just maxTokenSize
  , apSigned       = False
  , apContinued    = False }

-- | The atoms of a stream under a policy, in order, Empty atoms dropped; or
--   the offset where the reading stopped and why: a token that does not
--   read (a TCG-reserved tag, an illegal length, bytes short of a length),
--   one past the policy's MaxIndTokenSize, a signed integer or a continued
--   byte sequence the policy does not take, or a continued byte sequence no
--   byte sequence ends. Linear in the stream: each atom is read once, by
--   the codec's token parser on what is left.
atoms :: AtomPolicy -> ByteString -> Either (Int, Text) [Atom]
atoms policy stream = raw 0 stream >>= joined . filter ((/= Empty) . atomToken)
  where
    raw off bs
      | B.null bs = Right []
      | otherwise = case feed (parse (parser :: Parser Token) bs) B.empty of
          Done rest t -> do
            let a = Atom off (B.length bs - B.length rest) t
            admitted a
            (a :) <$> raw (off + atomSize a) rest
          Fail _ _ why -> Left (off, "a token that does not read: " <> T.pack why <> " (Core 3.2.2.3.1, PDF 33-36)")
          Partial _    -> Left (off, "a token cut short by the end of the stream (Core 3.2.2.3.1, PDF 33-36)")
    admitted (Atom off size t) = case t of
      Empty -> Right ()
      _ | Just limit <- apMaxTokenSize policy, size > limit ->
            Left (off, "an atom of " <> tshow size <> " bytes, more than MaxIndTokenSize " <> tshow limit
                       <> " (Core 3.2.2.4.1 item 1, PDF 38)")
      Signed _ | not (apSigned policy) ->
            Left (off, "a signed integer atom, its S bit set, which this reader does not take (Opal 2.30 Table 15, PDF 31; "
                       <> "Core 3.2.2.4.1 item 5, PDF 38)")
      ContinuedBytes _ | not (apContinued policy) ->
            Left (off, "a continued byte sequence, which this reader does not take (Core 3.2.2.4.1 item 3, PDF 38; "
                       <> "Opal 2.30 Table 15, PDF 31)")
      _ -> Right ()
    -- a continued byte sequence's segments and the byte sequence that ends it, as one atom
    joined = \case
      [] -> Right []
      a@(Atom off _ (ContinuedBytes _)) : rest ->
        let (segments, after) = L.span (isContinued . atomToken) rest
        in case after of
             Atom off' size' (Bytes final) : more ->
               (Atom off (off' + size' - off) (Bytes (B.concat (mapMaybe segment (a : segments)) <> final)) :) <$> joined more
             _ -> Left (off, "a continued byte sequence that no byte sequence ends (Core 3.2.2.3.1.2-3, PDF 35-36)")
      a : rest -> (a :) <$> joined rest
    isContinued = \case
      ContinuedBytes _ -> True
      _                -> False
    segment a = case atomToken a of
      ContinuedBytes bs -> Just bs
      _                 -> Nothing


-- * The TPer's reading

-- | Whose tokens a TPer is reading: the control session's, or a regular
--   session's (Core 3.3.7.1.2-3, PDF 73).
data StreamMode = ControlStream | SessionStream
  deriving (Eq, Show)

-- | Where in a stream a fault lies, by Core 3.2.2.4.2's items (PDF 39).
data Place
  = OutsideCall
    -- ^ outside a method invocation: before a Call, or after End of Data where the status list's Start List
    --   should be (item 2); a transaction control token without its uinteger status (3.2.2.3.3.4-5, PDF 37)
  | InHeader
    -- ^ after the Call, before the parameter list's Start List (item 3)
  | InParameters !MethodHeader
    -- ^ in the parameter list, after its Start List and before End of Data (item 4)
  | InStatusList !(Maybe MethodHeader)
    -- ^ in the status list (item 5); the header, when it read
  | InResults
    -- ^ in a method response, its result list, End of Data or status list: the host's reading alone
  | Unfinished
    -- ^ the stream ends inside an invocation or a response (3.3.10.3 a, PDF 83-84)
  deriving (Eq, Show)

-- | A token out of place: which token (its index among those given, 0
--   first; the count of tokens for a stream that ends too soon), where, and
--   why.
data Fault = Fault
  { faultAt    :: !Int
  , faultPlace :: !Place
  , faultWhy   :: !Text
  }
  deriving (Eq, Show)

-- | What a TPer reads from a stream.
data Item
  = Invoke !MethodCall
    -- ^ a method invocation read whole, its status the host's first status value
  | BadCall !Fault
    -- ^ an invocation with a fault in its header or its parameter list, read past its End of Data and status list
  | EndOfSessionItem
    -- ^ the host's End of Session, which ends a regular session's stream
  | StartTransactionItem !Natural
    -- ^ Start Transaction with its status, in a regular session
  | EndTransactionItem !Natural
    -- ^ End Transaction with its status, in a regular session
  deriving (Eq, Show)

-- | A TPer's reading of a stream's tokens: the items in order, and the fault
--   that stopped it, if one did. Empty atoms, should any be given, are
--   dropped first (3.2.2.4.2: they are never unexpected).
items :: StreamMode -> [Token] -> ([Item], Maybe Fault)
items mode toks = go (indexed toks)
  where
    count = length toks
    fault = faultOf count
    go = \case
      [] -> ([], Nothing)
      (i, t) : rest -> case t of
        Call             -> invocation rest
        EndOfSession     -> case mode of
          ControlStream -> go rest
          SessionStream -> ([EndOfSessionItem], Nothing)
        StartTransaction -> transaction StartTransactionItem rest
        EndTransaction   -> transaction EndTransactionItem rest
        _ -> ([], Just (Fault i OutsideCall (tshow t <> " outside a method invocation (Core 3.2.2.4.2 item 2, PDF 39)")))
    -- a transaction control token and the uinteger status after it: ignored on the control session, an item in a
    -- regular one
    transaction item rest = case (mode, rest) of
      (ControlStream, (_, Unsigned _) : more) -> go more
      (ControlStream, _)                      -> go rest
      (SessionStream, (_, Unsigned n) : more) -> first (item n :) (go more)
      (SessionStream, (j, t) : _)             ->
        ([], Just (Fault j OutsideCall (tshow t <> " where the transaction control token's uinteger status is expected "
                                        <> "(Core 3.2.2.3.3.4-5, PDF 37)")))
      (SessionStream, [])                     -> ([], Just (unfinishedAt count))
    -- a method invocation after its Call: read whole, or read to its End of Data and status list after a fault in
    -- its header or parameter list (a BadCall), or stopped
    invocation rest = case header rest of
      Left h -> resync (fault InHeader h) Nothing rest
      Right (hdr, afterHeader) -> case parameterList afterHeader of
        Left h -> resync (fault (InParameters hdr) h) (Just hdr) afterHeader
        Right (ps, afterParams) -> case afterParams of
          (_, EndOfData) : afterEod -> status (Just hdr) afterEod $ \st more -> first (Invoke (MethodCall hdr ps st) :) (go more)
          (j, t) : _ -> resync (Fault j (InParameters hdr) (tshow t <> " after the parameter list, before End of Data "
                                                              <> "(Core 3.2.2.4.2 item 4, PDF 39)")) (Just hdr) afterParams
          [] -> ([], Just (unfinishedAt count))
    -- after a fault in the header or the parameter list: the invocation's End of Data and status list, then the
    -- BadCall; a stream that ends first is unfinished
    resync f mhdr rest = case faultPlace f of
      Unfinished -> ([], Just f)
      _ -> case L.dropWhile ((/= EndOfData) . snd) rest of
        []           -> ([], Just (unfinishedAt count))
        _ : afterEod -> status mhdr afterEod $ \_ more -> first (BadCall f :) (go more)
    -- the status list after End of Data: its first value to the continuation, or the fault
    status mhdr afterEod k = case afterEod of
      (_, StartList) : values -> case statusValues values of
        Right (st, more) -> k st more
        Left h           -> ([], Just (fault (InStatusList mhdr) h))
      (j, t) : _ -> ([], Just (Fault j OutsideCall (tshow t <> " after End of Data, where the status list's Start List is "
                                                    <> "expected (Core 3.2.2.4.2 item 2, PDF 39)")))
      [] -> ([], Just (unfinishedAt count))


-- * The host's reading

-- | What a host reads from a TPer's response stream.
data Returned
  = ReturnedCall !MethodCall
    -- ^ a Session Manager response, formatted as an invocation (3.3.7.1.3, PDF 73)
  | ReturnedResults !MethodResponse
    -- ^ a method response: its results and its status (3.2.4.2, PDF 49)
  | ReturnedEndOfSession
    -- ^ the TPer's End of Session, answering the host's (3.2.2.3.3.3, PDF 37)
  | ReturnedTransaction !Token !Natural
    -- ^ Start Transaction or End Transaction with its status, mirroring the host's (3.2.2.3.3.4-5, PDF 37)
  deriving (Eq, Show)

-- | A host's reading of a TPer's tokens: what came back, in order, and the
--   fault that stopped the reading, if one did. Empty atoms are dropped
--   first.
returnedItems :: [Token] -> ([Returned], Maybe Fault)
returnedItems toks = go (indexed toks)
  where
    count = length toks
    fault = faultOf count
    go = \case
      [] -> ([], Nothing)
      (_, Call) : rest      -> either stop (\(mc, more) -> first (ReturnedCall mc :) (go more)) (call rest)
      (_, StartList) : rest -> either stop (\(mr, more) -> first (ReturnedResults mr :) (go more)) (response rest)
      (_, EndOfSession) : rest -> first (ReturnedEndOfSession :) (go rest)
      (_, t) : (_, Unsigned n) : rest | isTransaction t -> first (ReturnedTransaction t n :) (go rest)
      (_, t) : (j, u) : _ | isTransaction t ->
        stop (Fault j OutsideCall (tshow u <> " where the transaction control token's uinteger status is expected "
                                   <> "(Core 3.2.2.3.3.4-5, PDF 37)"))
      [(_, t)] | isTransaction t -> stop (unfinishedAt count)
      (i, t) : _ -> stop (Fault i OutsideCall (tshow t <> " outside a method invocation or response (Core 3.2.2.4.2 item 2, PDF 39)"))
    stop f = ([], Just f)
    isTransaction t = t == StartTransaction || t == EndTransaction
    -- an invocation after its Call, whole: any fault stops the host's reading
    call rest = do
      (hdr, s1) <- first (fault InHeader) (header rest)
      (ps, s2)  <- first (fault (InParameters hdr)) (parameterList s1)
      s3        <- endOfData (InParameters hdr) s2
      (st, s4)  <- statusList (InStatusList (Just hdr)) s3
      pure (MethodCall hdr ps st, s4)
    -- a response after its Start List: the results, End of Data and the status list
    response rest = do
      (results, s1) <- first (fault InResults) (listValues rest)
      s2            <- endOfData InResults s1
      (st, s3)      <- statusList InResults s2
      pure (MethodResponse (unList results) st, s3)
    unList (List vs) = vs
    endOfData place = \case
      (_, EndOfData) : s -> Right s
      (j, t) : _ -> Left (Fault j place (tshow t <> " where End of Data is expected (Core 3.2.4.2, PDF 48-49)"))
      [] -> Left (unfinishedAt count)
    statusList place = \case
      (_, StartList) : s -> first (fault place) (statusValues s)
      (j, t) : _ -> Left (Fault j OutsideCall (tshow t <> " after End of Data, where the status list's Start List is expected "
                                               <> "(Core 3.2.2.4.2 item 2, PDF 39)"))
      [] -> Left (unfinishedAt count)


-- * Reading tokens

-- | A stream's tokens with their indexes, Empty atoms dropped.
type Indexed = [(Int, Token)]

indexed :: [Token] -> Indexed
indexed = filter ((/= Empty) . snd) . zip [0 ..]

-- | Why a reading stops: at a token, or at the stream's end.
data Halt = HaltAt !Int !Text | HaltEnd

-- | The fault a halt is, at a place, in a stream of this many tokens.
faultOf :: Int -> Place -> Halt -> Fault
faultOf count place = \case
  HaltAt i why -> Fault i place why
  HaltEnd      -> unfinishedAt count

-- | The fault of a stream that ends inside what it was reading.
unfinishedAt :: Int -> Fault
unfinishedAt count =
  Fault count Unfinished "the stream ends inside a method invocation or response, which it leaves incomplete (Core 3.3.10.3 a, PDF 83-84)"

-- | The method header after a Call: the two eight-byte UIDs, then the
--   parameter list's Start List (Core 3.2.4.2 items 2-3, PDF 48).
header :: Indexed -> Either Halt (MethodHeader, Indexed)
header s0 = do
  (invoker, s1) <- uidAt "the InvokingID" s0
  (method, s2)  <- uidAt "the MethodID" s1
  case s2 of
    (_, StartList) : s3 -> Right (MethodHeader invoker method, s3)
    (j, t) : _          -> Left (HaltAt j (tshow t <> " where the parameter list's Start List is expected (Core 3.2.4.2 item 3, PDF 48)"))
    []                  -> Left HaltEnd
  where
    uidAt what = \case
      (_, t) : s | Just u <- fromToken t -> Right (u :: UID, s)
      (j, t) : _ -> Left (HaltAt j (tshow t <> " where " <> what <> ", an eight-byte UID, is expected (Core 3.2.4.2 item 2, PDF 48)"))
      []         -> Left HaltEnd

-- | A parameter list after its Start List, to its End List: the required
--   values first, then the optional ones, Named (Core 3.2.4.1, PDF 47-48).
parameterList :: Indexed -> Either Halt (Parameters, Indexed)
parameterList = loop [] []
  where
    loop req opt = \case
      (_, EndList) : s -> Right (Parameters (reverse req) (reverse opt), s)
      []               -> Left HaltEnd
      s@((j, _) : _)   -> do
        (v, s') <- value s
        case v of
          N nv -> loop req (nv : opt) s'
          _ | null opt  -> loop (v : req) opt s'
            | otherwise -> Left (HaltAt j "a required parameter after an optional one (Core 3.2.4.1, PDF 47-48)")

-- | One value: an atom, a Named value or a list (Core 3.2.2.3.2, PDF 36).
value :: Indexed -> Either Halt (Value, Indexed)
value = \case
  (_, t) : s | Just d <- maybeDatumToken t -> Right (D (Datum d), s)
  (_, StartName) : s -> do
    (nm, s1) <- nameAt s
    (v, s2)  <- value s1
    case s2 of
      (_, EndName) : s3 -> Right (N (NamedValue nm v), s3)
      (j, t) : _ -> Left (HaltAt j (tshow t <> " where the Named value's End Name is expected (Core 3.2.2.3.2.1, PDF 36)"))
      []         -> Left HaltEnd
  (_, StartList) : s -> first L <$> listValues s
  (j, t) : _ -> Left (HaltAt j (tshow t <> " where a value is expected (Core 3.2.2.3.2, PDF 36)"))
  []         -> Left HaltEnd
  where
    nameAt = \case
      (_, t) : s | Just d <- maybeDatumToken t -> Right (Name (Datum d), s)
      (j, t) : _ -> Left (HaltAt j (tshow t <> " where a Named value's name, a byte string or an integer, is expected "
                                    <> "(Core 3.2.2.3.2.1, PDF 36)"))
      []         -> Left HaltEnd

-- | A list's values after its Start List, to its End List.
listValues :: Indexed -> Either Halt (List, Indexed)
listValues = loop []
  where
    loop acc = \case
      (_, EndList) : s -> Right (List (reverse acc), s)
      []               -> Left HaltEnd
      s                -> value s >>= \(v, s') -> loop (v : acc) s'

-- | A status list's values after its Start List, to its End List: three or
--   more uintegers, the first the status, the rest ignored (Core 3.2.4.2
--   item 5, PDF 49; 3.2.2.4.2 item 5, PDF 39).
statusValues :: Indexed -> Either Halt (Natural, Indexed)
statusValues = loop []
  where
    loop acc = \case
      (j, EndList) : s -> case reverse acc of
        st : _ | length acc >= 3 -> Right (st, s)
        _ -> Left (HaltAt j ("a status list of " <> tshow (length acc) <> " values, fewer than three (Core 3.2.4.2 item 5, PDF 49)"))
      (_, Unsigned n) : s -> loop (n : acc) s
      (j, t) : _ -> Left (HaltAt j (tshow t <> " in the status list, a list of uintegers (Core 3.2.2.4.2 item 5, PDF 39)"))
      [] -> Left HaltEnd
