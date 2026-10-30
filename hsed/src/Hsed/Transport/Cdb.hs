{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.Cdb
Description : The command descriptor block of a Level 0 IF-RECV, on SAT and on SCSI
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The CDB that carries IF-RECV of Level 0 Discovery (protocol 1, ComID 1) to a
drive (design 7.2): on SAT, ATA PASS-THROUGH (12) wrapping TRUSTED RECEIVE
(A1h ... 5Ch); on SCSI, SECURITY PROTOCOL IN (A2h, INC_512), each its
transfer counted in 512-byte blocks, 1 to 255 (the kext sizes an A1h
transfer from the count byte alone). The bytes are those the kext itself
sends when it attaches (TPerDriver.cpp:1506-1557), and the C shim builds the
same, the only CDB it can (cbits/hsed_kext.c); these are what the jig
records and the tests hold to the kext's.

'level0Cdb' is milestone M3's group G4's; until it lands it is a stub.
-}
module Hsed.Transport.Cdb
  ( level0Cdb
  ) where

import           RIO

import           Hsed.Transport.Drive (InterfaceType)


-- | The 12 bytes of a Level 0 IF-RECV of a number of 512-byte blocks, or why
--   not (no blocks, or more than a count byte holds).
level0Cdb :: InterfaceType -> Word32 -> Either Text ByteString
level0Cdb = error "G4: level0Cdb"
