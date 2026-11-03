/*
 * hsed_kext.h: the C shim between hsed and the BrightPlaza TPer kext's user
 * client (design docs/spec-as-source.md 7.2 and 9.3, M4c; compiled only
 * under the hsed package's manual flag kext, on macOS).
 *
 * Five functions over IOKit and CoreFoundation alone (no
 * SEDKernelInterface.framework: its header needs sedutil's uninstalled
 * DtaStructures.h). The shim issues the five commands of hsed_cdb.h and
 * nothing else: Level 0 Discovery's IF-RECV and, once the drive's base
 * ComID is bound, a ComPacket's IF-SEND and IF-RECV and STACK_RESET's two
 * on that ComID. The six scalars of every command it issues, and the CDB
 * it echoes, come from one hsed_cdb_scalars, which builds the CDB, the
 * direction and the length with hsed_cdb_build and checks an IF-SEND's
 * payload with hsed_cdb_check_payload on the very buffer, and its bind's
 * checks are hsed_cdb_bind_check's (hsed_cdb.c, portable C that every
 * build compiles and the tests run); this shim writes none of them.
 *
 * Each returns a kern_return_t: KERN_SUCCESS, or the IOKit error.
 */
#ifndef HSED_KEXT_H
#define HSED_KEXT_H

#include <stdint.h>

/* What hsed_kext_locate reads of a TPer from the IORegistry, the user
   client's connection once hsed_kext_open has opened it, and the drive's
   base ComID once hsed_kext_bind has bound it. */
typedef struct {
    uint32_t conn;            /* the io_connect_t; 0 until opened */
    uint64_t registry_id;     /* the TPer's IORegistry entry id */
    int      sat;             /* 1: 'TPer Interface Type' SAT; 0: SCSI */
    char     serial[64];      /* 'Identify Device Characteristics' -> 'Serial Number', as the registry holds it */
    char     kext_build[32];  /* the loaded kext's CFBundleVersion */
    int      other_clients;   /* user clients already open on the TPer */
    uint16_t base_comid;      /* the drive's base ComID; 0 until bound, and again once closed */
} hsed_kext_info;

/* Find the TPer under a BSD device, a whole disk (diskN), and read its
   registry entry; opens nothing. */
int hsed_kext_locate(const char *bsd, hsed_kext_info *out);

/* Open the TPer's user client, as root only (hsed's gate: the kext checks
   no privilege): IOServiceOpen, then the kext's Open (selector 0);
   exclusive, so a second opener gets kIOReturnExclusiveAccess. */
int hsed_kext_open(hsed_kext_info *info);

/* Bind the drive's base ComID, the one its own Level 0 Discovery names,
   once: the TPer opened found again by its registry id, then, as
   hsed_cdb_bind_check decides, a ComID hsed_cdb_base_ok takes, the TPer's
   user clients exactly this one, and its serial still the one read when it
   was located. */
int hsed_kext_bind(hsed_kext_info *info, uint16_t base);

/* One of the five commands (enum hsed_shape) of a number of 512-byte
   blocks, the base-carrying ones only once bound: its six scalars and the
   CDB echoed to cdb_out from one hsed_cdb_scalars (the buffer of bufsize
   bytes as long as asked and page-aligned, an IF-SEND's payload, its first
   bytes, checked); then the one call that issues it. The bytes the kext
   says it moved written to xfer. */
int hsed_kext_command(const hsed_kext_info *info, int shape, uint8_t blocks, void *buf, uint64_t bufsize,
                      uint8_t cdb_out[16], uint64_t *xfer);

/* Close the user client: the kext's Close (selector 1), then IOServiceClose. */
int hsed_kext_close(hsed_kext_info *info);

#endif /* HSED_KEXT_H */
