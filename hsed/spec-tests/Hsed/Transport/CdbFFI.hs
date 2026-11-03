{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.CdbFFI
Description : The portable C of the drive's CDBs (cbits/hsed_cdb.c) called from the tests
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The three functions of @cbits/hsed_cdb.h@ through FFI, for the tests alone:
no module under src, gen or app imports them (the kext shim calls them in
C, and hsed-doc-test, which interprets those modules, links no C), so the
test executable is the one build that links @hsed_cdb.o@ without the flag
@kext@. Unsafe calls: each is pure C of a few hundred instructions that
neither blocks nor calls back.

Each wrapper writes known bytes where the C is to write (the CDB 0xAA, the
direction and the length 'unwritten32' and 'unwritten64'), so that a test
sees what the C wrote and what it left.
-}
module Hsed.Transport.CdbFFI
  ( -- * The C functions
    hsedCdbBaseOk
  , hsedCdbBuild
  , hsedCdbCheckPayload
    -- * Their return codes (cbits/hsed_cdb.h)
  , cdbOk
  , cdbEArgument
  , cdbEShape
  , cdbEBlocks
  , cdbEComId
  , cdbEPayload
    -- * Wrapped
  , Built (..)
  , unwritten32
  , unwritten64
  , satFlag
  , cdbBuild
  , cdbBuildNulls
  , cdbBaseOk
  , cdbCheckPayload
  , cdbCheckPayloadAt
  , cdbCheckPayloadNull
  , cdbCheckEachByte
  ) where

import           Foreign.C.Types       (CInt (..))
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Marshal.Utils (copyBytes, fillBytes)
import           Foreign.Ptr           (Ptr, castPtr, nullPtr)
import           Foreign.Storable      (peek, peekByteOff, poke, pokeByteOff)
import qualified RIO.ByteString        as B
import           RIO

import           Hsed.Transport.Drive  (InterfaceType (..))


-- | @int hsed_cdb_base_ok(uint16_t base)@.
foreign import ccall unsafe "hsed_cdb_base_ok"
  hsedCdbBaseOk :: Word16 -> IO CInt

-- | @int hsed_cdb_build(int sat, int shape, uint16_t base, uint8_t blocks,
--   uint8_t cdb[16], uint32_t *direction, uint64_t *requested)@.
foreign import ccall unsafe "hsed_cdb_build"
  hsedCdbBuild :: CInt -> CInt -> Word16 -> Word8 -> Ptr Word8 -> Ptr Word32 -> Ptr Word64 -> IO CInt

-- | @int hsed_cdb_check_payload(int shape, uint16_t base, const uint8_t
--   *buf, uint64_t len)@.
foreign import ccall unsafe "hsed_cdb_check_payload"
  hsedCdbCheckPayload :: CInt -> Word16 -> Ptr Word8 -> Word64 -> IO CInt

-- | @HSED_CDB_OK@ and the refusals, as cbits/hsed_cdb.h defines them
--   ("Hsed.Transport.CdbCSpec" reads them from it).
cdbOk, cdbEArgument, cdbEShape, cdbEBlocks, cdbEComId, cdbEPayload :: Int
cdbOk        = 0
cdbEArgument = 1
cdbEShape    = 2
cdbEBlocks   = 3
cdbEComId    = 4
cdbEPayload  = 5

-- | What @hsed_cdb_build@ answered, and the CDB, direction and length as
--   it left them.
data Built = Built
  { bCode      :: !Int
  , bCdb       :: !ByteString  -- ^ 16 bytes
  , bDirection :: !Word32
  , bRequested :: !Word64
  }
  deriving (Eq, Show)

-- | What the direction and the length hold before the C writes them.
unwritten32 :: Word32
unwritten32 = 0xDEADBEEF

unwritten64 :: Word64
unwritten64 = 0xDEADBEEFDEADBEEF

-- | The C's @sat@: 1 for SAT, 0 for SCSI.
satFlag :: InterfaceType -> Int
satFlag IfSAT  = 1
satFlag IfSCSI = 0

-- | @hsed_cdb_build@ of a @sat@, a shape's number (any, so that the tests
--   can pass one outside the five), a base and blocks.
cdbBuild :: Int -> Int -> Word16 -> Word8 -> IO Built
cdbBuild sat shape base blocks =
  allocaBytes 16 $ \pCdb -> alloca $ \pDirection -> alloca $ \pRequested -> do
    fillBytes pCdb 0xAA 16
    poke pDirection unwritten32
    poke pRequested unwritten64
    code <- hsedCdbBuild (fromIntegral sat) (fromIntegral shape) base blocks pCdb pDirection pRequested
    Built (fromIntegral code) <$> B.packCStringLen (castPtr pCdb, 16) <*> peek pDirection <*> peek pRequested

-- | @hsed_cdb_build@ of a valid request (SAT, Level 0's shape, 4 blocks)
--   with NULL in place of the CDB, the direction or the length, as each
--   flag says: its answer, and the CDB as it was left where one was given.
cdbBuildNulls :: Bool -> Bool -> Bool -> IO (Int, Maybe ByteString)
cdbBuildNulls noCdb noDirection noRequested =
  allocaBytes 16 $ \pCdb -> alloca $ \pDirection -> alloca $ \pRequested -> do
    fillBytes pCdb 0xAA 16
    code <- hsedCdbBuild 1 0 0x1004 4 (if noCdb then nullPtr else pCdb) (if noDirection then nullPtr else pDirection)
                                      (if noRequested then nullPtr else pRequested)
    cdb <- if noCdb then pure Nothing else Just <$> B.packCStringLen (castPtr pCdb, 16)
    pure (fromIntegral code, cdb)

-- | @hsed_cdb_base_ok@.
cdbBaseOk :: Word16 -> IO Bool
cdbBaseOk base = (/= 0) <$> hsedCdbBaseOk base

-- | @hsed_cdb_check_payload@ of a shape's number and a base over bytes, in a
--   buffer of their own, their length the length checked.
cdbCheckPayload :: Int -> Word16 -> ByteString -> IO Int
cdbCheckPayload shape base bytes =
  allocaBytes (max 1 (B.length bytes)) $ \p -> do
    B.useAsCStringLen bytes (\(src, n) -> copyBytes p (castPtr src) n)
    cdbCheckPayloadAt shape base p (fromIntegral (B.length bytes))

-- | @hsed_cdb_check_payload@ on a buffer of at least the length given: the
--   very buffer a command passes on.
cdbCheckPayloadAt :: Int -> Word16 -> Ptr Word8 -> Word64 -> IO Int
cdbCheckPayloadAt shape base p len = fromIntegral <$> hsedCdbCheckPayload (fromIntegral shape) base p len

-- | @hsed_cdb_check_payload@ with NULL for the buffer.
cdbCheckPayloadNull :: Int -> Word16 -> Word64 -> IO Int
cdbCheckPayloadNull shape base len = fromIntegral <$> hsedCdbCheckPayload (fromIntegral shape) base nullPtr len

-- | Every change of one byte to a payload, each of its bytes set to each
--   of the 255 values it does not hold, checked in one buffer (a payload
--   of 512 bytes is 130,560 of them): the changes the C takes, by offset
--   and value.
cdbCheckEachByte :: Int -> Word16 -> ByteString -> IO [(Int, Word8)]
cdbCheckEachByte shape base bytes =
  allocaBytes (max 1 len) $ \p -> do
    B.useAsCStringLen bytes (\(src, n) -> copyBytes p (castPtr src) n)
    fmap concat . forM [0 .. len - 1] $ \i -> do
      was <- peekByteOff p i :: IO Word8
      taken <- filterM (\v -> do
                          pokeByteOff p i v
                          (== cdbOk) <$> cdbCheckPayloadAt shape base p (fromIntegral len))
                       [ v | v <- [minBound .. maxBound], v /= was ]
      pokeByteOff p i was
      pure (map ((,) i) taken)
  where len = B.length bytes
