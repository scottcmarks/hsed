/*
 * hsed_cdb.h: the five commands a drive transport carries, and the CDB of
 * each (design docs/spec-as-source.md 7.2 and 9.3, M4c).
 *
 * Portable C99, no IOKit: hsed_cdb.c is compiled into the hsed library in
 * every build, so that the tests run it through FFI against
 * Hsed.Transport.Cdb, which builds the same bytes in Haskell. The kext
 * shim (hsed_kext.c) takes the six scalars of every command it issues, and
 * the CDB it echoes, from one hsed_cdb_scalars (which builds them with
 * hsed_cdb_build and checks an IF-SEND's payload with
 * hsed_cdb_check_payload on the very buffer it passes to the kernel), and
 * binds a base only where hsed_cdb_bind_check takes what it read of the
 * TPer: the decisions are here, where the tests run them, and the shim
 * holds the IOKit calls alone.
 *
 * A transfer counts 512-byte blocks, 1 to 255 of them (SIIS 1.20 Tables
 * 12-13 for SCSI, PDF 30-31; Tables 16-17 for ATA, PDF 40-41; the kext
 * sizes an ATA PASS-THROUGH transfer from its count byte alone). The base
 * is the drive's base ComID, bound once from its own Level 0 Discovery.
 */
#ifndef HSED_CDB_H
#define HSED_CDB_H

#include <stddef.h>
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

/* What hsed_cdb_build, hsed_cdb_check_payload, hsed_cdb_scalars and
   hsed_cdb_bind_check return: OK, or why not. */
#define HSED_CDB_OK 0
#define HSED_CDB_E_ARGUMENT 1   /* a NULL pointer, or no page size */
#define HSED_CDB_E_SHAPE 2      /* a shape outside the five */
#define HSED_CDB_E_BLOCKS 3     /* no blocks, or protocol 2's other than one */
#define HSED_CDB_E_COMID 4      /* a shape on the base, and the base not one hsed_cdb_base_ok takes; a bind of such a base, or of another than the one bound */
#define HSED_CDB_E_PAYLOAD 5    /* an IF-SEND's payload other than its shape carries on the base */
#define HSED_CDB_E_SPACE 6      /* a buffer shorter than the length asked */
#define HSED_CDB_E_ALIGN 7      /* a buffer not on a page boundary */
#define HSED_CDB_E_CLIENTS 8    /* a bind on a TPer whose user clients are other than exactly one, the shim's */
#define HSED_CDB_E_SERIAL 9     /* a bind on a TPer whose serial is none, or not the one read when it was located */

/* The six scalars of the kext's SCSI command, in the order its user client
   reads them (macos-driver UserClient.cpp:83-87, at 8bf2635, which build
   625 matches): the CDB as two native quads, its 16 bytes in memory; the
   buffer's address; its size; the direction; the length asked. */
enum hsed_scalar { HSED_SCALAR_CDB = 0, HSED_SCALAR_BUFFER = 2, HSED_SCALAR_SIZE = 3, HSED_SCALAR_DIRECTION = 4, HSED_SCALAR_LENGTH = 5, HSED_SCALARS = 6 };

int hsed_cdb_base_ok(uint16_t base);   /* 1 iff base >= 0x1000: Core Table 26's non-reserved ComIDs, PDF 55-56 */

/* A shape's CDB on an interface (sat 1: SAT's ATA PASS-THROUGH (12)
   carrying ACS's TRUSTED RECEIVE or SEND, SAT-5 and ACS-5, SIIS 1.20's
   references [21] and [22]; sat 0: SPC-5's SECURITY PROTOCOL IN and OUT,
   its reference [10]; the fields' values SIIS 1.20 Tables 12-13 and 16-17,
   PDF 30-31 and 40-41), its ComID the base but for LEVEL0_RECV's, of a
   number of blocks: its 16 bytes (12, then 4 zeros), the direction scalar
   and the bytes asked, blocks * 512. On a refusal nothing is written but
   the CDB's zeros. */
int hsed_cdb_build(int sat, int shape, uint16_t base, uint8_t blocks, uint8_t cdb[16], uint32_t *direction, uint64_t *requested);

/* HSED_CDB_OK when the len bytes at buf are a payload a shape may carry
   on the base: for RESET_SEND, STACK_RESET's request for the base (Core
   Table 35, PDF 65), zeros to the block's end; for PACKET_SEND, a
   ComPacket on the base whose reserved bytes, extension, OutstandingData
   and MinTransfer are zero (Core 3.2.3.2.1, PDF 40-41; 3.3.3.1, PDF 57),
   zeros after its Length; the receive shapes send nothing to check. */
int hsed_cdb_check_payload(int shape, uint16_t base, const uint8_t *buf, uint64_t len);

/* The six scalars of one command (enum hsed_scalar) and the CDB the shim
   echoes, from one hsed_cdb_build of a shape on the base of a number of
   blocks, for a buffer of bufsize bytes: the buffer at least as long as
   the length asked and on a boundary of the page size given, an IF-SEND's
   payload, its first bytes, as many as asked, taken by
   hsed_cdb_check_payload; then the CDB in scalars 0 and 1 as the kernel
   reads it, the buffer's address, its size, the direction and the length
   asked, and the echo the scalars' first 16 bytes, the very bytes the
   kernel is given. Both are zeroed first, so that a refusal leaves nothing
   else. */
int hsed_cdb_scalars(int sat, int shape, uint16_t base, uint8_t blocks, const void *buf, uint64_t bufsize, uint64_t page,
                     uint64_t scalars[6], uint8_t cdb_out[16]);

/* Whether a base may be bound, from what the shim read of the TPer it
   opened: HSED_CDB_OK, or why not: a base hsed_cdb_base_ok refuses, or
   another than the one bound already (bound 0 for none; the same again is
   bound already), HSED_CDB_E_COMID; its user clients counted other than
   exactly one, the shim's own (another could send to the drive between
   hsed's commands, and none counted, or -1 for none that could be, is a
   count that is not what it seems, design 13.29 (d)), HSED_CDB_E_CLIENTS;
   its IDENTIFY serial as the registry holds it now empty, or other than
   the one read when it was located, compared in at most size bytes,
   HSED_CDB_E_SERIAL. */
int hsed_cdb_bind_check(uint16_t base, uint16_t bound, int clients, const char *serial, const char *located, size_t size);

#endif /* HSED_CDB_H */
