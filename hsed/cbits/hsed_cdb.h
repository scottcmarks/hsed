/*
 * hsed_cdb.h: the five commands a drive transport carries, and the CDB of
 * each (design docs/spec-as-source.md 7.2 and 9.3, M4c).
 *
 * Portable C99, no IOKit: hsed_cdb.c is compiled into the hsed library in
 * every build, so that the tests run it through FFI against
 * Hsed.Transport.Cdb, which builds the same bytes in Haskell. The kext
 * shim (hsed_kext.c) is to take every CDB, direction and length it issues
 * from hsed_cdb_build, and to check every IF-SEND's payload with
 * hsed_cdb_check_payload on the very buffer it passes to the kernel.
 *
 * A transfer counts 512-byte blocks, 1 to 255 of them (SIIS 1.20 Tables
 * 12-13 for SCSI, PDF 30-31; Tables 16-17 for ATA, PDF 40-41; the kext
 * sizes an ATA PASS-THROUGH transfer from its count byte alone). The base
 * is the drive's base ComID, bound once from its own Level 0 Discovery.
 */
#ifndef HSED_CDB_H
#define HSED_CDB_H

#include <stdint.h>

/* The five commands, Hsed.Transport.Cdb's Shape, numbered as its shapeCode:
   - LEVEL0_RECV, IF-RECV of Level 0 Discovery: protocol 1, ComID 0x0001,
     1 to 255 blocks (Core 3.3.6.1-3.3.6.2, PDF 67);
   - PACKET_SEND and PACKET_RECV, a ComPacket's IF-SEND and IF-RECV:
     protocol 1, the base, 1 to 255 blocks;
   - RESET_SEND, STACK_RESET's HANDLE_COMID_REQUEST: IF-SEND, protocol 2,
     the base, one block (Core Table 29, PDF 62; its request Table 35,
     PDF 65);
   - RESET_RECV, its GET_COMID_RESPONSE: IF-RECV, protocol 2, the base,
     one block (Core Table 30, PDF 63). */
enum hsed_shape { HSED_SHAPE_LEVEL0_RECV = 0, HSED_SHAPE_PACKET_SEND = 1, HSED_SHAPE_PACKET_RECV = 2, HSED_SHAPE_RESET_SEND = 3, HSED_SHAPE_RESET_RECV = 4 };

/* The direction scalar of the kext's SCSI command, IOKit's IODirection
   (IOMemoryDescriptor.h; macos-driver UserClient.cpp at 8bf2635, which
   build 625 matches): the only two directions this code names. */
#define HSED_DIRECTION_IN  1u   /* kIODirectionIn: the kext copies its buffer out to the caller (UserClient.cpp:188-189) */
#define HSED_DIRECTION_OUT 2u   /* kIODirectionOut: the kext copies the caller's buffer in (UserClient.cpp:165-170) */

/* What hsed_cdb_build and hsed_cdb_check_payload return: OK, or why not. */
#define HSED_CDB_OK 0
#define HSED_CDB_E_ARGUMENT 1   /* a NULL pointer */
#define HSED_CDB_E_SHAPE 2      /* a shape outside the five */
#define HSED_CDB_E_BLOCKS 3     /* no blocks, or protocol 2's other than one */
#define HSED_CDB_E_COMID 4      /* a shape on the base, and the base not one hsed_cdb_base_ok takes */
#define HSED_CDB_E_PAYLOAD 5    /* an IF-SEND's payload other than its shape carries on the base */

int hsed_cdb_base_ok(uint16_t base);   /* 1 iff base >= 0x1000: Core Table 26's non-reserved ComIDs, PDF 55-56 */

/* A shape's CDB on an interface (sat 1: SAT's ATA PASS-THROUGH (12), SIIS
   1.20 Tables 16-17, PDF 40-41; sat 0: SCSI's SECURITY PROTOCOL OUT and
   IN, Tables 12-13, PDF 30-31), its ComID the base but for LEVEL0_RECV's,
   of a number of blocks: its 16 bytes (12, then 4 zeros), the direction
   scalar and the bytes asked, blocks * 512. On a refusal nothing is
   written but the CDB's zeros. */
int hsed_cdb_build(int sat, int shape, uint16_t base, uint8_t blocks, uint8_t cdb[16], uint32_t *direction, uint64_t *requested);

/* HSED_CDB_OK when the len bytes at buf are a payload a shape may carry
   on the base: for RESET_SEND, STACK_RESET's request for the base (Core
   Table 35, PDF 65), zeros to the block's end; for PACKET_SEND, a
   ComPacket on the base whose reserved bytes, extension, OutstandingData
   and MinTransfer are zero (Core 3.2.3.2.1, PDF 40-41; 3.3.3.1, PDF 57),
   zeros after its Length; the receive shapes send nothing to check. */
int hsed_cdb_check_payload(int shape, uint16_t base, const uint8_t *buf, uint64_t len);

#endif /* HSED_CDB_H */
