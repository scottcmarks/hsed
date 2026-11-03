/*
 * hsed_cdb.c: the CDBs of the five commands a drive transport carries
 * (hsed_cdb.h; design docs/spec-as-source.md 7.2 and 9.3, M4c).
 *
 * Portable C99 with no IOKit, compiled into the hsed library in every
 * build, Linux CI's included, and run there through FFI by the tests
 * (Hsed.Transport.CdbCSpec), which hold every CDB it builds to
 * Hsed.Transport.Cdb.cdbFor, written apart from it in Haskell. It is the one
 * file of the shim that writes a CDB byte, a direction, a security protocol
 * or a ComID: four templates, one for each interface and direction, and a
 * table of the five shapes. The kext shim (hsed_kext.c) takes every CDB,
 * direction and length it issues from one hsed_cdb_build, and checks every
 * IF-SEND's payload with hsed_cdb_check_payload on the very buffer it
 * passes to the kernel.
 *
 * Each template's variable bytes (the security protocol, the count of
 * blocks, the ComID) are left zero, and hsed_cdb_build writes them at the
 * offsets below; Hsed.Transport.KextSourceSpec reads the templates and the
 * offsets from this file and holds them to cdbFor with those bytes zeroed.
 */
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "hsed_cdb.h"

/* The bytes of the block a transfer length counts: SCSI's with INC_512 set
   (SIIS 1.20 Tables 12-13, PDF 30-31), ATA's 512-byte data units (Tables
   16-17, PDF 40-41). */
#define HSED_BLOCK_BYTES 512u

/* Level 0 Discovery's ComID (Core Table 26, PDF 55; 3.3.6.2, PDF 67). */
#define HSED_LEVEL0_COMID 0x0001u

/* The first ComID Core Table 26 does not reserve (PDF 55-56): 0x0000 to
   0x0FFF are reserved (Level 0's among them, and TPER_RESET's 0x0004),
   0x1000 to 0xFFFF are the non-reserved ComIDs of protocols 1 and 2, and a
   drive's base ComID is one of them. */
#define HSED_FIRST_BASE_COMID 0x1000u

/* The security protocols the five carry (SIIS 1.20 Tables 12-13 and 16-17,
   PDF 30-31 and 40-41): a ComPacket's, which Level 0 Discovery's IF-RECV
   uses too (Core 3.3.6.2, PDF 67), and ComID management's, STACK_RESET's
   (Core 3.3.4.7, PDF 62-63). */
#define HSED_PROTOCOL_COMPACKET 0x01u
#define HSED_PROTOCOL_COMID_MANAGEMENT 0x02u

/* STACK_RESET's Request Code (Core Table 35, PDF 65). */
#define HSED_STACK_RESET_REQUEST_CODE 0x00000002u

/* A ComPacket's header, Reserved to Length (Core Table 17, PDF 40). */
#define HSED_COMPACKET_HEADER_BYTES 20u

/* Where the variable bytes lie in SAT's CDB: FEATURES (the security
   protocol), COUNT (the blocks), LBA 15 to 8 and LBA 23 to 16 (the ComID,
   low byte first) (SIIS 1.20 Tables 16-17, PDF 40-41). */
#define HSED_SAT_PROTOCOL 3
#define HSED_SAT_COUNT 4
#define HSED_SAT_COMID_LO 6
#define HSED_SAT_COMID_HI 7

/* Where they lie in SCSI's: SECURITY PROTOCOL, SECURITY PROTOCOL SPECIFIC
   (the ComID, high byte first), the length's low byte (SIIS 1.20 Tables
   12-13, PDF 30-31). */
#define HSED_SCSI_PROTOCOL 1
#define HSED_SCSI_COMID_HI 2
#define HSED_SCSI_COMID_LO 3
#define HSED_SCSI_LENGTH 9

/* SAT's ATA PASS-THROUGH (12), opcode A1h, carrying ACS's TRUSTED RECEIVE,
   5Ch, or TRUSTED SEND, 5Eh, PIO, the length counted in 512-byte blocks in
   COUNT (SIIS 1.20 Tables 17 and 16, PDF 41 and 40; sedutil DtaOS.h:258-268).
   The receive is the kext's own attach-time CDB with its count and its
   ComID left to be written (TPerDriver.cpp:1527-1557). The kext reads an
   A1h's direction from its PROTOCOL field alone, 4 In and 5 Out
   (ATAProtocolDirections.cpp), and its length from COUNT alone
   (CDBAccess.cpp:46-50), which is why a transfer is 1 to 255 blocks. */
static const uint8_t hsed_sat_in[12] = {
    0xA1,  /* ATA PASS-THROUGH (12) */
    0x08,  /* PROTOCOL 4, PIO Data-In, in bits 4 to 1 */
    0x0E,  /* T_DIR from the device, BYTE_BLOCK in blocks, T_LENGTH in COUNT */
    0x00,  /* FEATURES: the security protocol */
    0x00,  /* COUNT: the blocks */
    0x00,  /* LBA 7 to 0: the length's high byte, zero up to 255 blocks */
    0x00,  /* LBA 15 to 8: the ComID's low byte */
    0x00,  /* LBA 23 to 16: the ComID's high byte */
    0x00,  /* DEVICE */
    0x5C,  /* COMMAND: TRUSTED RECEIVE */
    0x00,  /* reserved */
    0x00,  /* CONTROL */
};
static const uint8_t hsed_sat_out[12] = {
    0xA1,  /* ATA PASS-THROUGH (12) */
    0x0A,  /* PROTOCOL 5, PIO Data-Out, in bits 4 to 1 */
    0x06,  /* T_DIR to the device, BYTE_BLOCK in blocks, T_LENGTH in COUNT */
    0x00,  /* FEATURES: the security protocol */
    0x00,  /* COUNT: the blocks */
    0x00,  /* LBA 7 to 0: the length's high byte, zero up to 255 blocks */
    0x00,  /* LBA 15 to 8: the ComID's low byte */
    0x00,  /* LBA 23 to 16: the ComID's high byte */
    0x00,  /* DEVICE */
    0x5E,  /* COMMAND: TRUSTED SEND */
    0x00,  /* reserved */
    0x00,  /* CONTROL */
};

/* SCSI's SECURITY PROTOCOL IN, opcode A2h, and SECURITY PROTOCOL OUT, opcode
   B5h, INC_512 set, the length counted in 512-byte blocks (SIIS 1.20
   Tables 13 and 12, PDF 31 and 30). The IN is the kext's own attach-time
   CDB with its count and its ComID left to be written
   (TPerDriver.cpp:1506-1524). The kext reads the direction from the opcode
   and the length from the length asked. */
static const uint8_t hsed_scsi_in[12] = {
    0xA2,  /* SECURITY PROTOCOL IN */
    0x00,  /* SECURITY PROTOCOL */
    0x00,  /* SECURITY PROTOCOL SPECIFIC: the ComID's high byte */
    0x00,  /*   and its low byte */
    0x80,  /* INC_512: the length counts 512-byte blocks */
    0x00,  /* reserved */
    0x00,  /* ALLOCATION LENGTH: its high bytes, zero up to 255 blocks */
    0x00,
    0x00,
    0x00,  /*   and its low byte: the blocks */
    0x00,  /* reserved */
    0x00,  /* CONTROL */
};
static const uint8_t hsed_scsi_out[12] = {
    0xB5,  /* SECURITY PROTOCOL OUT */
    0x00,  /* SECURITY PROTOCOL */
    0x00,  /* SECURITY PROTOCOL SPECIFIC: the ComID's high byte */
    0x00,  /*   and its low byte */
    0x80,  /* INC_512: the length counts 512-byte blocks */
    0x00,  /* reserved */
    0x00,  /* TRANSFER LENGTH: its high bytes, zero up to 255 blocks */
    0x00,
    0x00,
    0x00,  /*   and its low byte: the blocks */
    0x00,  /* reserved */
    0x00,  /* CONTROL */
};

/* What each of the five shapes is (hsed_cdb.h's enum hsed_shape): the
   direction scalar the kext is given, which also picks the template; the
   security protocol; whether its ComID is the base (else Level 0
   Discovery's); and the most blocks it moves, ComID management's one
   (Core Tables 29-30, PDF 62-63). */
struct hsed_shape_row {
    uint32_t direction;
    uint8_t  protocol;
    int      on_base;
    uint8_t  most_blocks;
};
static const struct hsed_shape_row hsed_shapes[5] = {
    [HSED_SHAPE_LEVEL0_RECV] = { HSED_DIRECTION_IN,  HSED_PROTOCOL_COMPACKET,        0, 255 },
    [HSED_SHAPE_PACKET_SEND] = { HSED_DIRECTION_OUT, HSED_PROTOCOL_COMPACKET,        1, 255 },
    [HSED_SHAPE_PACKET_RECV] = { HSED_DIRECTION_IN,  HSED_PROTOCOL_COMPACKET,        1, 255 },
    [HSED_SHAPE_RESET_SEND]  = { HSED_DIRECTION_OUT, HSED_PROTOCOL_COMID_MANAGEMENT, 1, 1 },
    [HSED_SHAPE_RESET_RECV]  = { HSED_DIRECTION_IN,  HSED_PROTOCOL_COMID_MANAGEMENT, 1, 1 },
};

/* Whether a shape is one of the five. */
static int shape_known(int shape)
{
    return shape >= HSED_SHAPE_LEVEL0_RECV && shape <= HSED_SHAPE_RESET_RECV;
}

/* Two and four bytes, most significant first, as Core writes every field
   of a ComPacket's header and of ComID management's requests. */
static uint16_t be16(const uint8_t *p)
{
    return (uint16_t)(((unsigned int)p[0] << 8) | (unsigned int)p[1]);
}
static uint32_t be32(const uint8_t *p)
{
    return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) | ((uint32_t)p[2] << 8) | (uint32_t)p[3];
}

/* Whether the bytes from one offset to another are all zero. */
static int zeros(const uint8_t *buf, uint64_t from, uint64_t to)
{
    uint64_t i = 0;
    for (i = from; i < to; i++)
        if (buf[i] != 0)
            return 0;
    return 1;
}

/* STACK_RESET's request for the base, as HANDLE_COMID_REQUEST carries it in
   its one block (Core Table 29, PDF 62): the Extended ComID, the base with
   a static ComID's extension, 0x0000 (3.3.3.1, PDF 57); the Request Code;
   reserved zeros to the end of the transfer (Table 35, PDF 65). */
static int reset_request_ok(uint16_t base, const uint8_t *buf, uint64_t len)
{
    return len == HSED_BLOCK_BYTES
        && be16(buf) == base
        && be16(buf + 2) == 0
        && be32(buf + 4) == HSED_STACK_RESET_REQUEST_CODE
        && zeros(buf, 8, len);
}

/* A ComPacket the host sends on the base (Core Table 17, PDF 40; 3.2.3.2.1,
   PDF 40-41): its Reserved bytes zero; its ComID the base, its extension a
   static ComID's, 0x0000 (3.3.3.1, PDF 57); OutstandingData and MinTransfer,
   reserved from the host, zero; its Length within the transfer; and zeros
   after its payload to the transfer's end. */
static int compacket_ok(uint16_t base, const uint8_t *buf, uint64_t len)
{
    uint64_t end = 0;
    if (len < HSED_COMPACKET_HEADER_BYTES)
        return 0;
    end = (uint64_t)HSED_COMPACKET_HEADER_BYTES + be32(buf + 16);
    return zeros(buf, 0, 4)
        && be16(buf + 4) == base
        && zeros(buf, 6, 16)
        && end <= len
        && zeros(buf, end, len);
}

int hsed_cdb_base_ok(uint16_t base)
{
    return (unsigned int)base >= HSED_FIRST_BASE_COMID;
}

int hsed_cdb_build(int sat, int shape, uint16_t base, uint8_t blocks, uint8_t cdb[16], uint32_t *direction, uint64_t *requested)
{
    const struct hsed_shape_row *row = NULL;
    unsigned int comid = 0;

    /* zeroed first, so that a refusal leaves nothing else */
    if (cdb != NULL)
        memset(cdb, 0, 16);
    if (cdb == NULL || direction == NULL || requested == NULL)
        return HSED_CDB_E_ARGUMENT;
    if (!shape_known(shape))
        return HSED_CDB_E_SHAPE;
    row = &hsed_shapes[shape];
    if (blocks == 0 || blocks > row->most_blocks)
        return HSED_CDB_E_BLOCKS;
    if (row->on_base && !hsed_cdb_base_ok(base))
        return HSED_CDB_E_COMID;
    comid = row->on_base ? (unsigned int)base : HSED_LEVEL0_COMID;

    /* the template of the shape's direction, its variable bytes written;
       bytes 12 to 15 stay zero */
    if (sat) {
        memcpy(cdb, row->direction == HSED_DIRECTION_OUT ? hsed_sat_out : hsed_sat_in, sizeof hsed_sat_in);
        cdb[HSED_SAT_PROTOCOL] = row->protocol;
        cdb[HSED_SAT_COUNT] = blocks;
        cdb[HSED_SAT_COMID_LO] = (uint8_t)(comid & 0xFFu);
        cdb[HSED_SAT_COMID_HI] = (uint8_t)(comid >> 8);
    } else {
        memcpy(cdb, row->direction == HSED_DIRECTION_OUT ? hsed_scsi_out : hsed_scsi_in, sizeof hsed_scsi_in);
        cdb[HSED_SCSI_PROTOCOL] = row->protocol;
        cdb[HSED_SCSI_COMID_HI] = (uint8_t)(comid >> 8);
        cdb[HSED_SCSI_COMID_LO] = (uint8_t)(comid & 0xFFu);
        cdb[HSED_SCSI_LENGTH] = blocks;
    }
    /* the direction from the same row as the template's direction bits, the
       length from the same count as the CDB's: the kext reads the one from
       the scalar and the other from the CDB, so they must agree */
    *direction = row->direction;
    *requested = (uint64_t)blocks * HSED_BLOCK_BYTES;
    return HSED_CDB_OK;
}

int hsed_cdb_check_payload(int shape, uint16_t base, const uint8_t *buf, uint64_t len)
{
    if (buf == NULL)
        return HSED_CDB_E_ARGUMENT;
    if (!shape_known(shape))
        return HSED_CDB_E_SHAPE;
    /* a receive sends nothing: the kext copies no buffer in for it */
    if (hsed_shapes[shape].direction != HSED_DIRECTION_OUT)
        return HSED_CDB_OK;
    if (!hsed_cdb_base_ok(base))
        return HSED_CDB_E_COMID;
    switch (shape) {
    case HSED_SHAPE_RESET_SEND:
        return reset_request_ok(base, buf, len) ? HSED_CDB_OK : HSED_CDB_E_PAYLOAD;
    case HSED_SHAPE_PACKET_SEND:
        return compacket_ok(base, buf, len) ? HSED_CDB_OK : HSED_CDB_E_PAYLOAD;
    default:
        return HSED_CDB_E_PAYLOAD;
    }
}
