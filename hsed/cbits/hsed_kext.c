/*
 * hsed_kext.c: the C shim between hsed and the BrightPlaza TPer kext's user
 * client (hsed_kext.h; design docs/spec-as-source.md 7.2 and 9.3, M4c).
 *
 * IOKit and CoreFoundation alone, the kext's user-client ABI written out
 * here: its classes (macos-driver UserKernelShared.h) and three of its
 * methods (TPerDriverDispatchTable.inc, unchanged since its 2023 initial
 * commit; build 625's dispatch table and the direction logic of its SCSI
 * command were read byte for byte on 2026-11-02). hsed_kext_locate reads
 * the IORegistry and opens nothing; hsed_kext_open and hsed_kext_close
 * open and close the user client; hsed_kext_bind binds the drive's base
 * ComID; and hsed_kext_command issues one of the five commands of
 * hsed_cdb.h. This file writes no CDB byte, no direction, no security
 * protocol, no ComID and no scalar, and decides no bind: a command's six
 * scalars (the CDB, the buffer, its size, the direction the kext copies
 * the buffer by and the length it moves) and the CDB it echoes come from
 * one hsed_cdb_scalars, which also checks the buffer and an IF-SEND's
 * payload in it, and a bind's checks are hsed_cdb_bind_check's, on what
 * this file reads of the TPer (hsed_cdb.c, portable C that every build
 * compiles and the tests run through FFI); this file makes the IOKit calls
 * around them. Hsed.Transport.KextSourceSpec reads this file as text, a
 * check of its tokens against drift (the calls, their arguments, their
 * order), not a proof of what it does: it never runs.
 *
 * The loaded kext (build 625) checks no privilege and no command: the
 * gates are hsed's (design 9.3), and this file is the one that holds
 * whatever the Haskell above it does: root before the user client is
 * opened; the base ComID, the user client alone on the TPer and the
 * drive's serial before the base is bound; the shape, the length, the
 * buffer and the payload before each command.
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

#include "hsed_cdb.h"
#include "hsed_kext.h"

/* hsed_kext_info as Hsed.Transport.MacKext reads it, by these offsets
   (Hsed.Transport.Kext.infoLayout; Hsed.Transport.KextSourceSpec compares the two). */
_Static_assert(sizeof(hsed_kext_info) == 128, "hsed_kext_info is 128 bytes");
_Static_assert(_Alignof(hsed_kext_info) == 8, "hsed_kext_info is aligned to 8");
_Static_assert(offsetof(hsed_kext_info, conn) == 0, "conn at 0");
_Static_assert(offsetof(hsed_kext_info, registry_id) == 8, "registry_id at 8");
_Static_assert(offsetof(hsed_kext_info, sat) == 16, "sat at 16");
_Static_assert(offsetof(hsed_kext_info, serial) == 20, "serial at 20");
_Static_assert(offsetof(hsed_kext_info, kext_build) == 84, "kext_build at 84");
_Static_assert(offsetof(hsed_kext_info, other_clients) == 116, "other_clients at 116");
_Static_assert(offsetof(hsed_kext_info, base_comid) == 120, "base_comid at 120");
_Static_assert(sizeof(((hsed_kext_info *)NULL)->serial) == 64, "serial is 64 bytes");
_Static_assert(sizeof(((hsed_kext_info *)NULL)->kext_build) == 32, "kext_build is 32 bytes");

/* The kext's driver and user-client classes (UserKernelShared.h). */
#define HSED_KEXT_TPER_CLASS        "com_brightplaza_BrightPlazaTPer"
#define HSED_KEXT_USER_CLIENT_CLASS "com_brightplaza_BrightPlazaTPerUserClient"

/* The user client's methods this file calls, by selector
   (TPerDriverDispatchTable.inc): Open and Close take no scalars; the SCSI
   command takes six in (the CDB as two native quads, the buffer, its size,
   the direction, the length asked; UserClient.cpp:83-88) and gives one out
   (the bytes moved). The fourth method, which has the kernel issue
   commands of its own, is never called. */
#define HSED_KEXT_SELECTOR_OPEN  0u
#define HSED_KEXT_SELECTOR_CLOSE 1u
#define HSED_KEXT_SELECTOR_SCSI  2u


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

    /* root first, hsed's own gate: build 625 grants its user client to any
       process (design 9.3), so this is the one check of privilege there is */
    if (geteuid() != 0)
        return kIOReturnNotPrivileged;
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
    info->base_comid = 0;
    return KERN_SUCCESS;
}

int hsed_kext_bind(hsed_kext_info *info, uint16_t base)
{
    CFMutableDictionaryRef matching = NULL;
    io_service_t tper = IO_OBJECT_NULL;
    char serial[sizeof ((hsed_kext_info *)NULL)->serial] = { 0 };
    int clients = -1;
    int checked = HSED_CDB_OK;

    if (info == NULL)
        return kIOReturnBadArgument;
    if (info->conn == IO_OBJECT_NULL)
        return kIOReturnNotOpen;
    /* the TPer this process opened, found again by its registry id, and
       still the kext's: its user clients counted, its serial read again */
    matching = IORegistryEntryIDMatching(info->registry_id);
    if (matching == NULL)
        return kIOReturnNoMemory;
    tper = IOServiceGetMatchingService(MACH_PORT_NULL, matching);  /* consumes matching */
    if (tper == IO_OBJECT_NULL)
        return kIOReturnNoDevice;
    if (!IOObjectConformsTo(tper, HSED_KEXT_TPER_CLASS)) {
        IOObjectRelease(tper);
        return kIOReturnNoDevice;
    }
    clients = count_user_clients(tper);
    read_serial(tper, serial, sizeof serial);
    IOObjectRelease(tper);
    /* the checks in the portable C (hsed_cdb.c): a base hsed_cdb_base_ok
       takes, bound once; the TPer's user clients exactly one, this
       process's (another could be sending to the drive between hsed's
       commands, and none counted means the count is not what it seems,
       design 13.29 (d)); its IDENTIFY serial the one read when it was
       located and admitted; each failing closed */
    checked = hsed_cdb_bind_check(base, info->base_comid, clients, serial, info->serial, sizeof serial);
    if (checked != HSED_CDB_OK)
        return checked == HSED_CDB_E_CLIENTS ? kIOReturnExclusiveAccess : checked == HSED_CDB_E_SERIAL ? kIOReturnNoDevice : kIOReturnBadArgument;
    info->base_comid = base;
    return KERN_SUCCESS;
}

int hsed_kext_command(const hsed_kext_info *info, int shape, uint8_t blocks, void *buf, uint64_t bufsize,
                      uint8_t cdb_out[16], uint64_t *xfer)
{
    uint64_t input[6] = { 0, 0, 0, 0, 0, 0 };
    uint64_t output[1] = { 0 };
    uint32_t outputs = 1;
    long page = 0;
    int built = HSED_CDB_OK;
    kern_return_t ret = KERN_SUCCESS;

    if (info == NULL || info->conn == IO_OBJECT_NULL || buf == NULL || cdb_out == NULL || xfer == NULL)
        return kIOReturnBadArgument;
    *xfer = 0;
    page = sysconf(_SC_PAGESIZE);
    if (page <= 0)
        return kIOReturnNotAligned;
    /* the six scalars and the CDB echoed, all from one build on the base
       bound, in the portable C (hsed_cdb.c): the buffer as long as asked
       and on a page, an IF-SEND's payload in it checked; the echo the
       scalars' own first bytes, the CDB the kernel is given */
    built = hsed_cdb_scalars(info->sat, shape, info->base_comid, blocks, buf, bufsize, (uint64_t)page, input, cdb_out);
    if (built != HSED_CDB_OK)
        return built == HSED_CDB_E_COMID && info->base_comid == 0 ? kIOReturnNotReady  /* a command on the base before the bind */
             : built == HSED_CDB_E_SPACE ? kIOReturnNoSpace
             : built == HSED_CDB_E_ALIGN ? kIOReturnNotAligned
             : kIOReturnBadArgument;
    ret = IOConnectCallScalarMethod((io_connect_t)info->conn, HSED_KEXT_SELECTOR_SCSI, input, 6, output, &outputs);
    if (outputs >= 1)
        *xfer = output[0];
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
    info->base_comid = 0;
    closed = IOConnectCallScalarMethod(conn, HSED_KEXT_SELECTOR_CLOSE, NULL, 0, NULL, NULL);
    released = IOServiceClose(conn);
    return closed != KERN_SUCCESS ? closed : released;
}
