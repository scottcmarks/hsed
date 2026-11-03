{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.KextShimSpec
Description : Test specifications for cbits/hsed_kext.c itself, its code run against a stub IOKit
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The C shim's own code (@cbits/hsed_kext.c@, design 7.2 and 9.3), run: the
test suite compiles it against a stub of what it calls of Mach,
CoreFoundation and IOKit (@spec-tests/cbits/stub@, whose headers stand in
for the system's, every function the shim calls a macro for one named
@hsed_stub_*@), its five functions renamed @hsed_stubbed_kext_*@ and its
@geteuid@ the stub's (package.yaml's hsed-test), so that nothing of IOKit
is linked and no call reaches a kext. The stub is one drive's registry
(@spec-tests/cbits/hsed_kext_stub.c@): the whole disk over its block
storage driver and device over the kext's TPer, the TPer's properties and
its user clients; it records every call the shim makes. What
"Hsed.Transport.KextSourceSpec" pins in the source's tokens, this runs
(the M4c review's verification found one-line edits of the shim that
passed the token checks: the root gate's condition or its return made
void, the bind's checked answer or its user clients forced):

* @hsed_kext_open@ asks the effective user id first and, other than root,
  answers kIOReturnNotPrivileged having called nothing else; as root, it
  finds the TPer again by its registry id, opens its user client and calls
  the kext's Open (selector 0), and closes the connection again when that
  Open fails; it refuses an info already open, a TPer not found or of
  another class.
* @hsed_kext_locate@ reads the TPer above a whole disk, its interface, id,
  serial, kext build and user clients, and opens nothing.
* @hsed_kext_bind@ binds exactly where @hsed_cdb_bind_check@ takes the
  base, the base bound, the user clients it counts on the TPer and the
  serial it reads again (cbits/hsed_cdb.c, which
  "Hsed.Transport.CdbCSpec" runs), over 3 counts, 3 serials, 4 bases and 2
  bindings; it calls the kext nothing.
* @hsed_kext_command@ passes the kernel, by selector 2, exactly the six
  scalars @hsed_cdb_scalars@ gives for its arguments, echoes their first
  16 bytes, writes nothing into the buffer, and gives back the bytes the
  kext says it moved; a refusal of @hsed_cdb_scalars@ calls nothing, with
  the kern_return the shim maps it to.
* @hsed_kext_close@ calls the kext's Close (selector 1) and then closes the
  connection, whatever the first answers, and forgets the connection and
  the base.

Locate, open and bind leave no registry object and no CoreFoundation
object they were given unreleased.
-}
module Hsed.Transport.KextShimSpec (spec) where

import           Foreign.C.String      (CString, withCString)
import           Foreign.C.Types       (CInt (..))
import           Foreign.ForeignPtr    (withForeignPtr)
import           Foreign.Marshal.Alloc (allocaBytesAligned, alloca)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Marshal.Utils (copyBytes, fillBytes)
import           Foreign.Ptr           (Ptr, castPtr, nullPtr, plusPtr, ptrToWordPtr)
import           Foreign.Storable      (peek, peekByteOff, poke)
import           Data.Bits             (shiftL)
import qualified RIO.ByteString        as B
import           RIO
import qualified RIO.Text              as T
import           Test.Hspec

import           Hsed.Core.Record      (encodeRecord)
import           Hsed.Gen.Core.Layouts (STACK_RESETCommandRequest (..))
import           Hsed.Transport.Cdb    (Shape (..), shapeCode)
import           Hsed.Transport.CdbFFI (Scalars (..), cdbBindCheck, cdbEClients, cdbESerial, cdbOk, cdbScalarsAt, scalarCdb)
import           Hsed.Transport.Kext   (InfoLayout (..), infoLayout, ifRecvBuffer, kernReturn, pageBytes)


-- * The shim, as the test suite compiles it against the stub

foreign import ccall unsafe "hsed_stubbed_kext_locate"
  shimLocate :: CString -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "hsed_stubbed_kext_open"
  shimOpen :: Ptr Word8 -> IO CInt
foreign import ccall unsafe "hsed_stubbed_kext_bind"
  shimBind :: Ptr Word8 -> Word16 -> IO CInt
foreign import ccall unsafe "hsed_stubbed_kext_command"
  shimCommand :: Ptr Word8 -> CInt -> Word8 -> Ptr Word8 -> Word64 -> Ptr Word8 -> Ptr Word64 -> IO CInt
foreign import ccall unsafe "hsed_stubbed_kext_close"
  shimClose :: Ptr Word8 -> IO CInt


-- * The stub

foreign import ccall unsafe "hsed_stub_reset"       stubReset       :: IO ()
foreign import ccall unsafe "hsed_stub_set"         stubSet         :: CInt -> Int64 -> IO ()
foreign import ccall unsafe "hsed_stub_set_text"    stubSetText     :: CInt -> CString -> IO ()
foreign import ccall unsafe "hsed_stub_calls"       stubCalls       :: Ptr CInt -> CInt -> IO CInt
foreign import ccall unsafe "hsed_stub_scalar_call" stubScalarCall  :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word64 -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "hsed_stub_connection"  stubConnection  :: IO Word32
foreign import ccall unsafe "hsed_stub_clients"     stubClients     :: IO CInt
foreign import ccall unsafe "hsed_stub_cf_live"     stubCfLive      :: IO CInt
foreign import ccall unsafe "hsed_stub_io_live"     stubIoLive      :: IO CInt

-- | The stub's knobs, in the order of its @enum hsed_stub_knob@.
data Knob
  = Euid | BsdFound | MediaIsMedia | Chain | TPerIsTPer | TPerFound | RegistryId | Clients | IteratorFails | OpenResult
  | Selector0 | Selector1 | Selector2 | Outputs | Moved | CloseResult | BuildFound
  deriving (Eq, Show, Enum, Bounded)

-- | Its texts, in the order of its @enum hsed_stub_text@.
data StubText = BsdName | Interface | Serial | Build
  deriving (Eq, Show, Enum, Bounded)

-- | The calls it records, in the order of its @enum hsed_stub_call@ (from 1).
data Call
  = Geteuid | BsdMatching | IdMatching | GetMatching | Conforms | Parent | Property | EntryId | Children | Next | Release
  | KextInfo | ServiceOpen | ServiceClose | Selector0Call | Selector1Call | Selector2Call | OtherSelector
  deriving (Eq, Show, Enum, Bounded)

knob :: Knob -> Int64 -> IO ()
knob k = stubSet (fromIntegral (fromEnum k))

setText :: StubText -> Maybe Text -> IO ()
setText which = \case
  Nothing -> stubSetText (fromIntegral (fromEnum which)) nullPtr
  Just t  -> withCString (T.unpack t) (stubSetText (fromIntegral (fromEnum which)))

-- | Every call recorded since the reset, each by its number in the stub's enum (from 1).
recorded :: IO [Call]
recorded = do
  n <- stubCalls nullPtr 0
  allocaArray (fromIntegral n) $ \p -> do
    _ <- stubCalls p n
    concatMap (\c -> [ call | (k, call) <- zip [1 ..] [minBound .. maxBound], k == c ]) <$> peekArray (fromIntegral n) p

-- | An action's answer and the calls it made.
calling :: IO a -> IO (a, [Call])
calling act = do
  made <- length <$> recorded
  a <- act
  (,) a . drop made <$> recorded

-- | The last scalar method's connection, selector, inputs' count and inputs, and outputs offered; if one was called.
scalarCall :: IO (Maybe (Word32, Word32, Word32, [Word64], Word32))
scalarCall =
  alloca $ \pc -> alloca $ \ps -> alloca $ \pn -> allocaArray 6 $ \pi' -> alloca $ \po -> do
    seen <- stubScalarCall pc ps pn pi' po
    if seen == 0 then pure Nothing else
      (\c s n i o -> Just (c, s, n, i, o)) <$> peek pc <*> peek ps <*> peek pn <*> peekArray 6 pi' <*> peek po

-- | What the shim left unreleased: registry objects and CoreFoundation objects.
unreleased :: IO (CInt, CInt)
unreleased = (,) <$> stubIoLive <*> stubCfLive


-- * hsed_kext_info, as Hsed.Transport.Kext reads it

-- | A zeroed @hsed_kext_info@.
withInfo :: (Ptr Word8 -> IO a) -> IO a
withInfo act = allocaBytesAligned (ilBytes infoLayout) (ilAlignment infoLayout) $ \p -> fillBytes p 0 (ilBytes infoLayout) >> act p

conn :: Ptr Word8 -> IO Word32
conn p = peekByteOff p (ilConn infoLayout)

base :: Ptr Word8 -> IO Word16
base p = peekByteOff p (ilBaseComId infoLayout)

-- | A C string field of the info, to its NUL.
textAt :: Int -> Int -> Ptr Word8 -> IO ByteString
textAt offset size p = B.takeWhile (/= 0) <$> B.packCStringLen (castPtr (p `plusPtr` offset), size)

-- | What locate wrote: the interface flag, the registry id, the serial, the kext build, the other clients.
located :: Ptr Word8 -> IO (CInt, Word64, ByteString, ByteString, CInt)
located p = (,,,,) <$> peekByteOff p (ilSat l) <*> peekByteOff p (ilRegistryId l) <*> textAt (ilSerial l) (ilSerialBytes l) p
                   <*> textAt (ilKextBuild l) (ilKextBuildBytes l) p <*> peekByteOff p (ilOtherClients l)
  where l = infoLayout

-- | A kern_return as Hsed.Transport.Kext names it.
named :: CInt -> Text
named = kernReturn . fromIntegral

-- | The shim's calls, its answers named.
locate :: Ptr Word8 -> IO Text
locate p = withCString "disk6" (\bsd -> named <$> shimLocate bsd p)

open :: Ptr Word8 -> IO Text
open p = named <$> shimOpen p

bind :: Ptr Word8 -> Word16 -> IO Text
bind p b = named <$> shimBind p b

close :: Ptr Word8 -> IO Text
close p = named <$> shimClose p

success :: Text
success = "kIOReturnSuccess (0x00000000)"

-- | The stub reset, the drive located and its user client opened as root.
opened :: Ptr Word8 -> IO ()
opened p = do
  stubReset
  knob Euid 0
  (,) <$> locate p <*> open p >>= (`shouldBe` (success, success))


-- * What a command carries

-- | STACK_RESET's request for a ComID (Core Table 35, PDF 65), zero-padded to its one block.
stackResetRequest :: Word16 -> ByteString
stackResetRequest c = either (const "") (\r -> r <> B.replicate (512 - B.length r) 0)
                             (encodeRecord (STACK_RESETCommandRequest (fromIntegral c `shiftL` 16)))

-- | What a send of a shape carries on 0x1004, which the portable C takes: STACK_RESET's request, or a ComPacket's
--   header on the ComID (Core Table 17, PDF 40) and nothing after it; nothing for a receive.
carried :: Shape -> ByteString
carried = \case
  ResetSend  -> stackResetRequest 0x1004
  PacketSend -> B.pack [0, 0, 0, 0, 0x10, 0x04] <> B.replicate 14 0
  _          -> ""

-- | One command through the shim, on a page-aligned buffer of the blocks' bytes holding what the shape carries,
--   offset by some bytes and of a size given as some bytes fewer: the shim's answer named, the scalars
--   @hsed_cdb_scalars@ answers for the same arguments, the calls the shim made, the last scalar method's
--   arguments, the CDB it echoed, the bytes it says moved, and whether the buffer is as it was.
command :: Ptr Word8 -> Int -> Shape -> Word8 -> Int -> Int
        -> IO (Text, Scalars, [Call], Maybe (Word32, Word32, Word32, [Word64], Word32), ByteString, Word64, Bool)
command info sat s blocks offset short = do
  (fp, size) <- ifRecvBuffer (fromIntegral blocks * 512 + fromIntegral offset)
  withForeignPtr fp $ \p0 -> do
    let p = p0 `plusPtr` offset
        len = max 0 (fromIntegral blocks * 512 - short)
    B.useAsCStringLen (carried s) (\(src, n) -> copyBytes p (castPtr src) (min n len))
    was <- B.packCStringLen (castPtr p0, size)
    expected <- cdbScalarsAt sat (shapeCode s) 0x1004 blocks p (fromIntegral len) (fromIntegral pageBytes)
    allocaBytesAligned 16 8 $ \echo -> alloca $ \xfer -> do
      fillBytes echo 0xAA 16
      poke xfer 0xDEAD
      (answer, calls) <- calling (shimCommand info (fromIntegral (shapeCode s)) blocks p (fromIntegral len) echo xfer)
      call <- scalarCall
      echoed <- B.packCStringLen (castPtr echo, 16)
      moved <- peek xfer
      now <- B.packCStringLen (castPtr p0, size)
      pure (named answer, expected, calls, call, echoed, moved, was == now)


spec :: Spec
spec = describe "cbits/hsed_kext.c itself, compiled against the stub IOKit (spec-tests/cbits/stub) and run" $ do
  describe "hsed_kext_open" $ do
    it "refuses a process that is not root before anything else: kIOReturnNotPrivileged, its one call geteuid, nothing opened" $
      withInfo $ \p -> do
        stubReset
        locate p `shouldReturn` success
        for_ [501, 1, -1] $ \euid -> do
          knob Euid euid
          calling (open p) `shouldReturn` ("kIOReturnNotPrivileged (0xe00002c1)", [Geteuid])
          (,) <$> conn p <*> stubClients `shouldReturn` (0, 0)
        unreleased `shouldReturn` (0, 0)
    it "as root, finds the TPer again by its id, opens its user client and calls the kext's Open, selector 0, with no scalars" $
      withInfo $ \p -> do
        stubReset
        knob Euid 0
        locate p `shouldReturn` success
        calling (open p) `shouldReturn` (success, [Geteuid, IdMatching, GetMatching, Conforms, ServiceOpen, Release, Selector0Call])
        connection <- stubConnection
        (,,) <$> conn p <*> base p <*> stubClients `shouldReturn` (connection, 0, 1)
        scalarCall `shouldReturn` Just (connection, 0, 0, replicate 6 0, 0)
        unreleased `shouldReturn` (0, 0)
    it "refuses an info open already, after geteuid alone" $
      withInfo $ \p -> do
        opened p
        calling (open p) `shouldReturn` ("kIOReturnBadArgument (0xe00002c2)", [Geteuid])
        stubClients `shouldReturn` 1
    it "closes the connection where the kext's Open fails (another opener: kIOReturnExclusiveAccess), and keeps none" $
      withInfo $ \p -> do
        stubReset
        knob Euid 0
        locate p `shouldReturn` success
        knob Selector0 0xe00002c5
        (answer, calls) <- calling (open p)
        (answer, drop 4 calls) `shouldBe` ("kIOReturnExclusiveAccess (0xe00002c5)", [ServiceOpen, Release, Selector0Call, ServiceClose])
        (,) <$> conn p <*> stubClients `shouldReturn` (0, 0)
    it "opens nothing where IOServiceOpen fails, the TPer is not found again, or is not the kext's" $
      withInfo $ \p -> do
        for_ [ (OpenResult, 0xe00002c5, "kIOReturnExclusiveAccess (0xe00002c5)")
             , (TPerFound, 0, "kIOReturnNotFound (0xe00002f0)"), (TPerIsTPer, 0, "kIOReturnNoDevice (0xe00002c0)") ] $ \(k, v, said) -> do
          stubReset
          knob Euid 0
          locate p `shouldReturn` success
          knob k v
          (answer, calls) <- calling (open p)
          (k, answer, any (`elem` [Selector0Call, Selector1Call, Selector2Call, OtherSelector]) calls) `shouldBe` (k, said, False)
          (,) <$> conn p <*> stubClients `shouldReturn` (0, 0)
          unreleased `shouldReturn` (0, 0)

  describe "hsed_kext_locate" $ do
    it "reads the TPer above the whole disk, its interface, id, serial, kext build and user clients, and opens nothing" $
      withInfo $ \p -> do
        stubReset
        knob Clients 2
        (answer, calls) <- calling (locate p)
        answer `shouldBe` success
        located p `shouldReturn` (1, 0x100000abc, "TESTSERIAL0001", "625", 2)
        filter (`elem` [ServiceOpen, Selector0Call, Selector1Call, Selector2Call, OtherSelector, Geteuid]) calls `shouldBe` []
        (,) <$> conn p <*> base p `shouldReturn` (0, 0)
        unreleased `shouldReturn` (0, 0)
    it "reads a SCSI interface as 0, no serial or build as empty, a child iterator that fails as -1 clients" $
      withInfo $ \p -> do
        stubReset
        setText Interface (Just "SCSI")
        setText Serial Nothing
        knob BuildFound 0
        knob IteratorFails 1
        locate p `shouldReturn` success
        located p `shouldReturn` (0, 0x100000abc, "", "", -1)
        unreleased `shouldReturn` (0, 0)
    it "refuses another interface, a disk not found, an entry not a media, and a disk not directly on the kext's TPer" $
      withInfo $ \p -> do
        let refusals = [ (setText Interface (Just "ATA"), "kIOReturnUnsupported (0xe00002c7)"), (setText Interface Nothing, "kIOReturnUnsupported (0xe00002c7)")
                       , (knob BsdFound 0, "kIOReturnNotFound (0xe00002f0)"), (knob MediaIsMedia 0, "kIOReturnNoDevice (0xe00002c0)")
                       , (knob Chain 2, "kIOReturnNoDevice (0xe00002c0)"), (knob Chain 0, "kIOReturnNoDevice (0xe00002c0)")
                       , (knob TPerIsTPer 0, "kIOReturnNoDevice (0xe00002c0)") ]
        for_ (zip [0 :: Int ..] refusals) $ \(n, (setUp, said)) -> do
          stubReset
          setUp
          (answer, calls) <- calling (locate p)
          (n, answer, ServiceOpen `elem` calls) `shouldBe` (n, said, False)
          unreleased `shouldReturn` (0, 0)

  describe "hsed_kext_bind" $ do
    it "binds exactly where hsed_cdb_bind_check takes what it reads of the TPer again: 3 counts of user clients, 3 serials, 4 bases, 2 bindings" $ do
      let binds = [ (others, serial, b, already) | others <- [Nothing, Just 0, Just 1], serial <- [Just "TESTSERIAL0001", Just "OTHERSERIAL", Nothing]
                                               , b <- [0x0004, 0x0FFF, 0x1000, 0x1004], already <- [False, True] ]
      results <- for binds $ \(others, serial, b, already) -> withInfo $ \p -> do
        opened p
        when already (bind p 0x1004 `shouldReturn` success)
        bound <- base p
        maybe (knob IteratorFails 1) (knob Clients . (+ 1)) others   -- the opener's own user client and the others
        setText Serial serial
        (answer, calls) <- calling (bind p b)
        now <- base p
        counted <- stubClients
        let clients = maybe (-1) (const (fromIntegral counted)) others
        model <- cdbBindCheck b bound clients (maybe "" encodeUtf8 serial) "TESTSERIAL0001" 64
        let expected | model == cdbOk       = success
                     | model == cdbEClients = "kIOReturnExclusiveAccess (0xe00002c5)"
                     | model == cdbESerial  = "kIOReturnNoDevice (0xe00002c0)"
                     | otherwise            = "kIOReturnBadArgument (0xe00002c2)"
            kexted = filter (`elem` [ServiceOpen, ServiceClose, Selector0Call, Selector1Call, Selector2Call, OtherSelector]) calls
        live <- unreleased
        pure ((others, serial, b, already), (answer == expected, now == (if model == cdbOk then b else bound), kexted, live))
      [ c | (c, r) <- results, r /= (True, True, [], (0, 0)) ] `shouldBe` []
      length results `shouldBe` 72
    it "binds 0x1004 with the opener's user client alone and the serial unchanged" $
      withInfo $ \p -> do
        opened p
        bind p 0x1004 `shouldReturn` success
        base p `shouldReturn` 0x1004
    it "refuses the same with another user client on the TPer" $
      withInfo $ \p -> do
        opened p
        knob Clients 2
        bind p 0x1004 `shouldReturn` "kIOReturnExclusiveAccess (0xe00002c5)"
        base p `shouldReturn` 0
    it "refuses an info not open, calling nothing; a TPer not found again, or not the kext's" $
      withInfo $ \p -> do
        stubReset
        locate p `shouldReturn` success
        calling (bind p 0x1004) `shouldReturn` ("kIOReturnNotOpen (0xe00002cd)", [])
        for_ [(TPerFound, 0), (TPerIsTPer, 0)] $ \(k, v) -> withInfo $ \q -> do
          opened q
          knob k v
          bind q 0x1004 `shouldReturn` "kIOReturnNoDevice (0xe00002c0)"
          base q `shouldReturn` 0
          unreleased `shouldReturn` (0, 0)

  describe "hsed_kext_command" $ do
    it "passes the kernel, by selector 2, exactly the six scalars hsed_cdb_scalars gives, echoes their first 16 bytes, writes nothing into the buffer, and gives back the bytes moved: 5 shapes, 2 interfaces, 3 counts of blocks" $ do
      results <- for [ (sat, s, n) | sat <- [1, 0], s <- [minBound .. maxBound], n <- [1, 2, 255] ] $ \(sat, s, n) -> withInfo $ \p -> do
        stubReset
        knob Euid 0
        setText Interface (Just (if sat == 1 then "SAT" else "SCSI"))
        _ <- locate p
        _ <- open p
        _ <- bind p 0x1004
        connection <- stubConnection
        (answer, expected, calls, call, echoed, moved, untouched) <- command p sat s n 0 0
        cdb <- scalarCdb (sScalars expected)
        pure ( (sat, s, n)
             , (answer, calls, if Selector2Call `elem` calls then call else Nothing, echoed, moved, untouched)
             , if sCode expected == cdbOk
                 then (success, [Selector2Call], Just (connection, 2, 6, sScalars expected, 1), cdb, 512, True)
                 else ("kIOReturnBadArgument (0xe00002c2)", [], Nothing, B.replicate 16 0, 0, True) )
      [ (c, got) | (c, got, want) <- results, got /= want ] `shouldBe` []
      length [ () | (_, (a, _, _, _, _, _), _) <- results, a == success ] `shouldBe` 2 * (3 * 3 + 2 * 1)
    it "gives back the kext's own answer, and the bytes it says moved only where it gives an output" $
      withInfo $ \p -> do
        opened p
        bind p 0x1004 `shouldReturn` success
        knob Selector2 0xe00002ca
        (answer, _, _, _, _, moved, _) <- command p 1 PacketRecv 4 0 0
        (answer, moved) `shouldBe` ("kIOReturnIOError (0xe00002ca)", 512)
        knob Selector2 0
        knob Outputs 0
        (answer', _, _, _, _, moved', _) <- command p 1 PacketRecv 4 0 0
        (answer', moved') `shouldBe` (success, 0)
    it "calls nothing where hsed_cdb_scalars refuses: a buffer shorter than the length, off its page, a payload its shape does not carry, a command on the base before the bind" $
      withInfo $ \p -> do
        opened p
        (short, _, shortCalls, _, _, _, _) <- command p 1 PacketRecv 4 0 1
        (unaligned, _, unalignedCalls, _, _, _, _) <- command p 1 PacketRecv 4 8 0
        (short, unaligned, shortCalls, unalignedCalls)
          `shouldBe` ("kIOReturnNotReady (0xe00002d8)", "kIOReturnNotReady (0xe00002d8)", [], [])
        (level0, _, level0Calls, _, _, _, _) <- command p 1 Level0Recv 4 0 0
        (level0, level0Calls) `shouldBe` (success, [Selector2Call])
        bind p 0x1004 `shouldReturn` success
        (short', _, c1, _, _, _, _) <- command p 1 PacketRecv 4 0 1
        (unaligned', _, c2, _, _, _, _) <- command p 1 PacketRecv 4 8 0
        (short', unaligned', c1, c2) `shouldBe` ("kIOReturnNoSpace (0xe00002db)", "kIOReturnNotAligned (0xe00002d0)", [], [])
        fp <- fst <$> ifRecvBuffer 512
        withForeignPtr fp $ \buf -> allocaBytesAligned 16 8 $ \echo -> alloca $ \xfer -> do
          -- a ComPacket on another ComID than the base bound
          B.useAsCStringLen (B.pack [0, 0, 0, 0, 0x10, 0x05]) (\(src, n) -> copyBytes buf (castPtr src) n)
          calling (named <$> shimCommand p (fromIntegral (shapeCode PacketSend)) 1 buf 512 echo xfer)
            `shouldReturn` ("kIOReturnBadArgument (0xe00002c2)", [])
          calling (named <$> shimCommand p (fromIntegral (shapeCode PacketRecv)) 1 nullPtr 512 echo xfer)
            `shouldReturn` ("kIOReturnBadArgument (0xe00002c2)", [])
          calling (named <$> shimCommand p (fromIntegral (shapeCode PacketRecv)) 1 buf 512 nullPtr xfer)
            `shouldReturn` ("kIOReturnBadArgument (0xe00002c2)", [])
          ptrToWordPtr buf `mod` fromIntegral pageBytes `shouldBe` 0
    it "refuses an info not open, calling nothing" $
      withInfo $ \p -> do
        stubReset
        (answer, _, calls, _, _, _, _) <- command p 1 Level0Recv 1 0 0
        (answer, calls) `shouldBe` ("kIOReturnBadArgument (0xe00002c2)", [])

  describe "hsed_kext_close" $ do
    it "calls the kext's Close, selector 1, then closes the connection, and forgets both the connection and the base" $
      withInfo $ \p -> do
        opened p
        bind p 0x1004 `shouldReturn` success
        connection <- stubConnection
        calling (close p) `shouldReturn` (success, [Selector1Call, ServiceClose])
        scalarCall `shouldReturn` Just (connection, 1, 0, replicate 6 0, 0)
        (,,) <$> conn p <*> base p <*> stubClients `shouldReturn` (0, 0, 0)
        calling (close p) `shouldReturn` ("kIOReturnNotOpen (0xe00002cd)", [])
    it "closes the connection whatever the kext's Close answers, and gives back the Close's failure first" $
      withInfo $ \p -> do
        opened p
        knob Selector1 0xe00002cd
        knob CloseResult 0xe00002c2
        calling (close p) `shouldReturn` ("kIOReturnNotOpen (0xe00002cd)", [Selector1Call, ServiceClose])
        (,) <$> conn p <*> stubClients `shouldReturn` (0, 0)
    it "gives back the connection's close failing where the kext's Close succeeded" $
      withInfo $ \p -> do
        opened p
        knob CloseResult 0xe00002c2
        calling (close p) `shouldReturn` ("kIOReturnBadArgument (0xe00002c2)", [Selector1Call, ServiceClose])
