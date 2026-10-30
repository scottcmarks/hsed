/*
 * hsed_kext.h: the C shim between hsed and the BrightPlaza TPer kext's user
 * client (design docs/spec-as-source.md 7.2, M3; compiled only under the
 * hsed package's manual flag kext, on macOS).
 *
 * Four functions over IOKit and CoreFoundation alone (no
 * SEDKernelInterface.framework: its header needs sedutil's uninstalled
 * DtaStructures.h). The shim can issue one command, IF-RECV of Level 0
 * Discovery (protocol 1, ComID 1), whose CDB it builds itself: there is no
 * IF-SEND, no Security Protocol Out and no data-out direction here.
 *
 * Each returns a kern_return_t: KERN_SUCCESS, or the IOKit error.
 */
#ifndef HSED_KEXT_H
#define HSED_KEXT_H

#include <stdint.h>

/* What hsed_kext_locate reads of a TPer from the IORegistry, and the user
   client's connection once hsed_kext_open has opened it. */
typedef struct {
    uint32_t conn;            /* the io_connect_t; 0 until opened */
    uint64_t registry_id;     /* the TPer's IORegistry entry id */
    int      sat;             /* 1: 'TPer Interface Type' SAT; 0: SCSI */
    char     serial[64];      /* 'Identify Device Characteristics' -> 'Serial Number', as the registry holds it */
    char     kext_build[32];  /* the loaded kext's CFBundleVersion */
    int      other_clients;   /* user clients already open on the TPer */
} hsed_kext_info;

/* Find the TPer under a BSD device (disk6) and read its registry entry;
   opens nothing. */
int hsed_kext_locate(const char *bsd, hsed_kext_info *out);

/* Open the TPer's user client: IOServiceOpen, then the kext's Open
   (selector 0); exclusive, so a second opener gets kIOReturnExclusiveAccess. */
int hsed_kext_open(hsed_kext_info *info);

/* IF-RECV of Level 0 Discovery, blocks * 512 bytes (1 to 255 blocks) into a
   page-aligned buffer of bufsize bytes: the CDB the shim built written to
   cdb_out, the bytes transferred to xfer. */
int hsed_kext_level0(const hsed_kext_info *info, uint8_t blocks, void *buf, uint64_t bufsize,
                     uint8_t cdb_out[16], uint64_t *xfer);

/* Close the user client: the kext's Close (selector 1), then IOServiceClose. */
int hsed_kext_close(hsed_kext_info *info);

#endif /* HSED_KEXT_H */
