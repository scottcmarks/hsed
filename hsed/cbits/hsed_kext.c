/*
 * hsed_kext.c: the C shim between hsed and the BrightPlaza TPer kext's user
 * client (hsed_kext.h; design docs/spec-as-source.md 7.2, M3).
 *
 * IOKit and CoreFoundation alone, the kext's user-client ABI written out
 * here: its classes (macos-driver UserKernelShared.h) and three of its
 * methods (TPerDriverDispatchTable.inc, unchanged since its 2023 initial
 * commit). hsed_kext_locate reads the IORegistry and opens nothing;
 * hsed_kext_open and hsed_kext_close open and close the user client; and
 * hsed_kext_level0 issues the one command the shim can, IF-RECV of Level 0
 * Discovery, through a CDB it builds itself from the two below, with the
 * data moving in from the drive. Nothing in this file sends: no command
 * that carries data to the drive, no direction but In, no CDB a caller
 * hands in (Hsed.Transport.KextSourceSpec reads this file to hold it so).
 *
 * The loaded kext (build 625) checks no privilege: the gates are hsed's
 * (design 9.3), and this file is the one that holds whatever the Haskell
 * above it does.
 */
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

#include <mach/mach.h>
#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/kext/KextManager.h>
#include <IOKit/storage/IOBlockStorageDevice.h>
#include <IOKit/storage/IOBlockStorageDriver.h>
#include <IOKit/storage/IOMedia.h>

#include "hsed_kext.h"

/* hsed_kext_info as Hsed.Transport.MacKext reads it, by these offsets
   (Hsed.Transport.Kext.infoLayout; Hsed.Transport.KextSourceSpec compares the two). */
_Static_assert(sizeof(hsed_kext_info) == 120, "hsed_kext_info is 120 bytes");
_Static_assert(_Alignof(hsed_kext_info) == 8, "hsed_kext_info is aligned to 8");
_Static_assert(offsetof(hsed_kext_info, conn) == 0, "conn at 0");
_Static_assert(offsetof(hsed_kext_info, registry_id) == 8, "registry_id at 8");
_Static_assert(offsetof(hsed_kext_info, sat) == 16, "sat at 16");
_Static_assert(offsetof(hsed_kext_info, serial) == 20, "serial at 20");
_Static_assert(offsetof(hsed_kext_info, kext_build) == 84, "kext_build at 84");
_Static_assert(offsetof(hsed_kext_info, other_clients) == 116, "other_clients at 116");
_Static_assert(sizeof(((hsed_kext_info *)NULL)->serial) == 64, "serial is 64 bytes");
_Static_assert(sizeof(((hsed_kext_info *)NULL)->kext_build) == 32, "kext_build is 32 bytes");

/* The kext's driver and user-client classes (UserKernelShared.h). */
#define HSED_KEXT_TPER_CLASS        "com_brightplaza_BrightPlazaTPer"
#define HSED_KEXT_USER_CLIENT_CLASS "com_brightplaza_BrightPlazaTPerUserClient"

/* The user client's methods this file calls, by selector
   (TPerDriverDispatchTable.inc): Open and Close take no scalars; the SCSI
   command takes six in (the CDB as two native quads, the buffer, its size,
   the direction, the length asked) and gives one out (the bytes moved). */
#define HSED_KEXT_SELECTOR_OPEN  0u
#define HSED_KEXT_SELECTOR_CLOSE 1u
#define HSED_KEXT_SELECTOR_SCSI  2u

/* The direction scalar: IODirection's In, the user land's read
   (IOMemoryDescriptor.h; UserClient.cpp copies the kernel's buffer out to
   the caller's for it). The only direction this file names. */
#define HSED_KEXT_DIRECTION_IN 1u

/* An IF-RECV's transfer counts 512-byte blocks (SIIS 1.20 Table 17, PDF 41;
   Table 13 with INC_512 set, PDF 31). */
#define HSED_KEXT_BLOCK_BYTES 512u

/* IF-RECV of Level 0 Discovery: Protocol ID 1, ComID 1 (Core 2.01 3.3.6.2,
   PDF 67). Each is the kext's own attach-time CDB (TPerDriver.cpp:1506-1557)
   with its count of blocks left zero, which hsed_kext_level0 writes at
   HSED_SAT_COUNT or HSED_SCSI_LENGTH; Hsed.Transport.Cdb builds the same
   bytes, and Hsed.Transport.KextSourceSpec holds these two to it. */
static const uint8_t hsed_level0_sat[12] = {
    0xA1,  /* ATA PASS-THROUGH (12) */
    0x08,  /* PROTOCOL 4, PIO Data-In, in bits 4 to 1 */
    0x0E,  /* T_DIR from the device, BYTE_BLOCK in blocks, T_LENGTH in COUNT */
    0x01,  /* FEATURES: the security protocol */
    0x00,  /* COUNT: the blocks */
    0x00,  /* LBA 7 to 0: the transfer length's high byte, zero up to 255 blocks */
    0x01,  /* LBA 15 to 8: the ComID's low byte */
    0x00,  /* LBA 23 to 16: the ComID's high byte */
    0x00,  /* DEVICE */
    0x5C,  /* COMMAND: TRUSTED RECEIVE */
    0x00,  /* reserved */
    0x00,  /* CONTROL */
};
static const uint8_t hsed_level0_scsi[12] = {
    0xA2,  /* SECURITY PROTOCOL IN */
    0x01,  /* SECURITY PROTOCOL */
    0x00,  /* SECURITY PROTOCOL SPECIFIC: the ComID's high byte */
    0x01,  /*   and its low byte */
    0x80,  /* INC_512: the length counts 512-byte blocks */
    0x00,  /* reserved */
    0x00,  /* ALLOCATION LENGTH: its high bytes, zero up to 255 blocks */
    0x00,
    0x00,
    0x00,  /*   and its low byte: the blocks */
    0x00,  /* reserved */
    0x00,  /* CONTROL */
};
#define HSED_SAT_COUNT   4
#define HSED_SCSI_LENGTH 9
_Static_assert(sizeof hsed_level0_sat == 12 && sizeof hsed_level0_scsi == 12, "12-byte CDBs");


/* A registry entry's parent in the service plane, if it is of a class; the
   entry itself released either way. */
static io_registry_entry_t parent_of_class(io_registry_entry_t entry, const char *class_name)
{
    io_registry_entry_t parent = IO_OBJECT_NULL;
    if (entry == IO_OBJECT_NULL)
        return IO_OBJECT_NULL;
    if (IORegistryEntryGetParentEntry(entry, kIOServicePlane, &parent) != KERN_SUCCESS)
        parent = IO_OBJECT_NULL;
    IOObjectRelease(entry);
    if (parent != IO_OBJECT_NULL && !IOObjectConformsTo(parent, class_name)) {
        IOObjectRelease(parent);
        parent = IO_OBJECT_NULL;
    }
    return parent;
}

/* A CFString copied into a C buffer as UTF-8; empty when it is no string or
   does not fit. */
static void copy_string(CFTypeRef value, char *buf, size_t size)
{
    buf[0] = '\0';
    if (value != NULL && CFGetTypeID(value) == CFStringGetTypeID()
        && !CFStringGetCString((CFStringRef)value, buf, (CFIndex)size, kCFStringEncodingUTF8))
        buf[0] = '\0';
}

/* 'TPer Interface Type': SAT or SCSI; any other the shim cannot address. */
static int read_interface(io_registry_entry_t tper, int *sat)
{
    int ret = kIOReturnUnsupported;
    CFTypeRef type = IORegistryEntryCreateCFProperty(tper, CFSTR("TPer Interface Type"), kCFAllocatorDefault, 0);
    if (type != NULL && CFGetTypeID(type) == CFStringGetTypeID()) {
        if (CFStringCompare((CFStringRef)type, CFSTR("SAT"), 0) == kCFCompareEqualTo) {
            *sat = 1;
            ret = KERN_SUCCESS;
        } else if (CFStringCompare((CFStringRef)type, CFSTR("SCSI"), 0) == kCFCompareEqualTo) {
            *sat = 0;
            ret = KERN_SUCCESS;
        }
    }
    if (type != NULL)
        CFRelease(type);
    return ret;
}

/* 'Identify Device Characteristics' -> 'Serial Number', the IDENTIFY serial
   as the registry holds it (not the bridge's INQUIRY page 80 serial); empty
   when the TPer has none. */
static void read_serial(io_registry_entry_t tper, char *serial, size_t size)
{
    CFTypeRef identify = IORegistryEntryCreateCFProperty(tper, CFSTR("Identify Device Characteristics"),
                                                         kCFAllocatorDefault, 0);
    serial[0] = '\0';
    if (identify != NULL && CFGetTypeID(identify) == CFDictionaryGetTypeID())
        copy_string(CFDictionaryGetValue((CFDictionaryRef)identify, CFSTR("Serial Number")), serial, size);
    if (identify != NULL)
        CFRelease(identify);
}

/* The loaded kext's CFBundleVersion, found by the bundle identifier the
   TPer's entry carries; empty when the kext's information cannot be read. */
static void read_kext_build(io_registry_entry_t tper, char *build, size_t size)
{
    CFTypeRef bundle = IORegistryEntryCreateCFProperty(tper, kCFBundleIdentifierKey, kCFAllocatorDefault, 0);
    build[0] = '\0';
    if (bundle != NULL && CFGetTypeID(bundle) == CFStringGetTypeID()) {
        const void *version_key = kCFBundleVersionKey;
        CFArrayRef ids = CFArrayCreate(kCFAllocatorDefault, &bundle, 1, &kCFTypeArrayCallBacks);
        CFArrayRef keys = CFArrayCreate(kCFAllocatorDefault, &version_key, 1, &kCFTypeArrayCallBacks);
        CFDictionaryRef loaded = (ids != NULL && keys != NULL) ? KextManagerCopyLoadedKextInfo(ids, keys) : NULL;
        if (loaded != NULL) {
            CFTypeRef kext = CFDictionaryGetValue(loaded, bundle);
            if (kext != NULL && CFGetTypeID(kext) == CFDictionaryGetTypeID())
                copy_string(CFDictionaryGetValue((CFDictionaryRef)kext, kCFBundleVersionKey), build, size);
            CFRelease(loaded);
        }
        if (keys != NULL)
            CFRelease(keys);
        if (ids != NULL)
            CFRelease(ids);
    }
    if (bundle != NULL)
        CFRelease(bundle);
}

/* The kext's user clients already on the TPer, its children in the service
   plane; -1 when they cannot be counted. */
static int count_user_clients(io_registry_entry_t tper)
{
    io_iterator_t children = IO_OBJECT_NULL;
    io_registry_entry_t child = IO_OBJECT_NULL;
    int n = 0;
    if (IORegistryEntryGetChildIterator(tper, kIOServicePlane, &children) != KERN_SUCCESS)
        return -1;
    while ((child = IOIteratorNext(children)) != IO_OBJECT_NULL) {
        if (IOObjectConformsTo(child, HSED_KEXT_USER_CLIENT_CLASS))
            n++;
        IOObjectRelease(child);
    }
    IOObjectRelease(children);
    return n;
}

int hsed_kext_locate(const char *bsd, hsed_kext_info *out)
{
    CFMutableDictionaryRef matching = NULL;
    io_registry_entry_t media = IO_OBJECT_NULL;
    io_registry_entry_t tper = IO_OBJECT_NULL;
    int ret = KERN_SUCCESS;

    if (bsd == NULL || out == NULL)
        return kIOReturnBadArgument;
    memset(out, 0, sizeof *out);

    matching = IOBSDNameMatching(MACH_PORT_NULL, 0, bsd);
    if (matching == NULL)
        return kIOReturnNoMemory;
    media = IOServiceGetMatchingService(MACH_PORT_NULL, matching);  /* consumes matching */
    if (media == IO_OBJECT_NULL)
        return kIOReturnNotFound;
    if (!IOObjectConformsTo(media, kIOMediaClass)) {
        IOObjectRelease(media);
        return kIOReturnNoDevice;
    }
    /* IOMedia, then IOBlockStorageDriver, then IOBlockStorageDevice, then the
       kext's TPer (sedutil's DtaMacOS.cpp:37-114): a whole disk directly on
       the TPer, never a partition or a volume over it */
    tper = parent_of_class(parent_of_class(parent_of_class(media, kIOBlockStorageDriverClass),
                                           kIOBlockStorageDeviceClass),
                           HSED_KEXT_TPER_CLASS);
    if (tper == IO_OBJECT_NULL)
        return kIOReturnNoDevice;

    ret = read_interface(tper, &out->sat);
    if (ret == KERN_SUCCESS)
        ret = IORegistryEntryGetRegistryEntryID(tper, &out->registry_id);
    if (ret == KERN_SUCCESS) {
        read_serial(tper, out->serial, sizeof out->serial);
        read_kext_build(tper, out->kext_build, sizeof out->kext_build);
        out->other_clients = count_user_clients(tper);
    }
    IOObjectRelease(tper);
    return ret;
}

int hsed_kext_open(hsed_kext_info *info)
{
    CFMutableDictionaryRef matching = NULL;
    io_service_t tper = IO_OBJECT_NULL;
    io_connect_t conn = IO_OBJECT_NULL;
    kern_return_t ret = KERN_SUCCESS;

    if (info == NULL || info->conn != IO_OBJECT_NULL)
        return kIOReturnBadArgument;
    /* the entry hsed_kext_locate read, by its id, and still the kext's TPer */
    matching = IORegistryEntryIDMatching(info->registry_id);
    if (matching == NULL)
        return kIOReturnNoMemory;
    tper = IOServiceGetMatchingService(MACH_PORT_NULL, matching);  /* consumes matching */
    if (tper == IO_OBJECT_NULL)
        return kIOReturnNotFound;
    if (!IOObjectConformsTo(tper, HSED_KEXT_TPER_CLASS)) {
        IOObjectRelease(tper);
        return kIOReturnNoDevice;
    }
    ret = IOServiceOpen(tper, mach_task_self(), 0, &conn);
    IOObjectRelease(tper);
    if (ret != KERN_SUCCESS)
        return ret;
    /* the kext allows one opener: another already open gives kIOReturnExclusiveAccess */
    ret = IOConnectCallScalarMethod(conn, HSED_KEXT_SELECTOR_OPEN, NULL, 0, NULL, NULL);
    if (ret != KERN_SUCCESS) {
        IOServiceClose(conn);
        return ret;
    }
    info->conn = conn;
    return KERN_SUCCESS;
}

int hsed_kext_level0(const hsed_kext_info *info, uint8_t blocks, void *buf, uint64_t bufsize,
                     uint8_t cdb_out[16], uint64_t *xfer)
{
    uint8_t cdb[16] = { 0 };
    uint64_t quads[2] = { 0, 0 };
    uint64_t output[1] = { 0 };
    uint32_t outputs = 1;
    uint64_t requested = 0;
    long page = 0;
    kern_return_t ret = KERN_SUCCESS;

    if (info == NULL || info->conn == IO_OBJECT_NULL || buf == NULL || cdb_out == NULL || xfer == NULL || blocks == 0)
        return kIOReturnBadArgument;
    requested = (uint64_t)blocks * HSED_KEXT_BLOCK_BYTES;
    if (bufsize < requested)
        return kIOReturnNoSpace;
    /* page-aligned, as the kext's own user library requires (TPerKernelInterface.c) */
    page = sysconf(_SC_PAGESIZE);
    if (page <= 0 || (uintptr_t)buf % (uintptr_t)page != 0)
        return kIOReturnNotAligned;

    /* the CDB, built here and nowhere else: a template above, its count the
       blocks, 16 bytes with the last 4 zero; the kext sizes the transfer
       from the count (CDBAccess.cpp:46-50), so the length asked is the same */
    memcpy(cdb, info->sat ? hsed_level0_sat : hsed_level0_scsi, sizeof hsed_level0_sat);
    cdb[info->sat ? HSED_SAT_COUNT : HSED_SCSI_LENGTH] = blocks;
    memcpy(cdb_out, cdb, sizeof cdb);
    memcpy(quads, cdb, sizeof quads);

    {
        const uint64_t input[6] = {
            [0] = quads[0],
            [1] = quads[1],
            [2] = (uint64_t)(uintptr_t)buf,
            [3] = bufsize,
            [4] = HSED_KEXT_DIRECTION_IN,
            [5] = requested,
        };
        *xfer = 0;
        ret = IOConnectCallScalarMethod((io_connect_t)info->conn, HSED_KEXT_SELECTOR_SCSI, input, 6, output, &outputs);
        if (outputs >= 1)
            *xfer = output[0];
    }
    return ret;
}

int hsed_kext_close(hsed_kext_info *info)
{
    io_connect_t conn = IO_OBJECT_NULL;
    kern_return_t closed = KERN_SUCCESS;
    kern_return_t released = KERN_SUCCESS;

    if (info == NULL || info->conn == IO_OBJECT_NULL)
        return kIOReturnNotOpen;
    conn = (io_connect_t)info->conn;
    info->conn = IO_OBJECT_NULL;
    closed = IOConnectCallScalarMethod(conn, HSED_KEXT_SELECTOR_CLOSE, NULL, 0, NULL, NULL);
    released = IOServiceClose(conn);
    return closed != KERN_SUCCESS ? closed : released;
}
