/*
 * hsed_kext_stub.h: a stub of what cbits/hsed_kext.c calls of Mach, CoreFoundation and IOKit, for hsed-test
 * alone (Hsed.Transport.KextShimSpec; the M4c review's verification). The headers beside it stand in for
 * <mach/mach.h>, <CoreFoundation/CoreFoundation.h>, <IOKit/IOKitLib.h>, <IOKit/kext/KextManager.h> and the
 * three of <IOKit/storage/...> the shim includes, each including this one, so that the shim's own source
 * compiles on any platform against this stub: every function it calls is a macro for one named hsed_stub_*,
 * so its object references nothing of Mach, CoreFoundation or IOKit, and the test executable links none of
 * them (package.yaml's hsed-test; the shim's own functions are renamed hsed_stubbed_kext_* there, and its
 * geteuid hsed_stub_geteuid). The stub is a registry of one drive: an IOMedia whole disk over its block
 * storage driver and device, over the kext's TPer, with the TPer's properties and the user clients on it; a
 * test sets it up (hsed_stub_reset, hsed_stub_set, hsed_stub_set_text), calls the shim, and reads what the
 * shim called, in order (hsed_stub_calls), and the last scalar method's arguments (hsed_stub_scalar_call).
 *
 * The types are the IOKit and CoreFoundation ones in shape, not in layout: io objects are numbers, the CF
 * objects the stub's own. The kern_return codes are IOReturn.h's (iokit_common_err), so that
 * Hsed.Transport.Kext.kernReturn names them as it names the kext's.
 */
#ifndef HSED_KEXT_STUB_H
#define HSED_KEXT_STUB_H

#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>

/* --- Mach --- */
typedef unsigned int mach_port_t;
typedef mach_port_t task_port_t;
typedef int kern_return_t;
typedef int boolean_t;
#define KERN_SUCCESS   0
#define MACH_PORT_NULL ((mach_port_t)0)
#define mach_task_self() hsed_stub_mach_task_self()
mach_port_t hsed_stub_mach_task_self(void);

/* --- CoreFoundation --- */
struct hsed_stub_cf;
typedef const void *CFTypeRef;
typedef const struct hsed_stub_cf *CFStringRef;
typedef const struct hsed_stub_cf *CFDictionaryRef;
typedef struct hsed_stub_cf *CFMutableDictionaryRef;
typedef const struct hsed_stub_cf *CFArrayRef;
typedef const struct hsed_stub_cf *CFAllocatorRef;
typedef long CFIndex;
typedef unsigned long CFTypeID;
typedef unsigned char Boolean;
typedef uint32_t CFStringEncoding;
typedef long CFComparisonResult;
typedef unsigned long CFStringCompareFlags;
typedef struct {
    CFIndex version;
    const void *retain;
    const void *release;
    const void *copyDescription;
    const void *equal;
} CFArrayCallBacks;

#define kCFCompareEqualTo      0
#define kCFStringEncodingUTF8  0x08000100u
#define kCFAllocatorDefault    ((CFAllocatorRef)NULL)
#define kCFTypeArrayCallBacks  hsed_stub_kCFTypeArrayCallBacks
#define kCFBundleIdentifierKey CFSTR("CFBundleIdentifier")
#define kCFBundleVersionKey    CFSTR("CFBundleVersion")
extern const CFArrayCallBacks hsed_stub_kCFTypeArrayCallBacks;

#define CFSTR(text)                hsed_stub_CFSTR(text)
#define CFGetTypeID                hsed_stub_CFGetTypeID
#define CFStringGetTypeID          hsed_stub_CFStringGetTypeID
#define CFDictionaryGetTypeID      hsed_stub_CFDictionaryGetTypeID
#define CFStringGetCString         hsed_stub_CFStringGetCString
#define CFStringCompare            hsed_stub_CFStringCompare
#define CFDictionaryGetValue       hsed_stub_CFDictionaryGetValue
#define CFArrayCreate              hsed_stub_CFArrayCreate
#define CFRelease                  hsed_stub_CFRelease
CFStringRef hsed_stub_CFSTR(const char *text);
CFTypeID hsed_stub_CFGetTypeID(CFTypeRef value);
CFTypeID hsed_stub_CFStringGetTypeID(void);
CFTypeID hsed_stub_CFDictionaryGetTypeID(void);
Boolean hsed_stub_CFStringGetCString(CFStringRef value, char *buffer, CFIndex size, CFStringEncoding encoding);
CFComparisonResult hsed_stub_CFStringCompare(CFStringRef a, CFStringRef b, CFStringCompareFlags flags);
const void *hsed_stub_CFDictionaryGetValue(CFDictionaryRef dictionary, const void *key);
CFArrayRef hsed_stub_CFArrayCreate(CFAllocatorRef allocator, const void **values, CFIndex count, const CFArrayCallBacks *callbacks);
void hsed_stub_CFRelease(CFTypeRef value);

/* --- IOKit --- */
typedef mach_port_t io_object_t;
typedef io_object_t io_registry_entry_t;
typedef io_object_t io_service_t;
typedef io_object_t io_connect_t;
typedef io_object_t io_iterator_t;
typedef char io_name_t[128];
typedef uint32_t IOOptionBits;
#define IO_OBJECT_NULL  ((io_object_t)0)
#define kIOServicePlane "IOService"

/* IOReturn.h: iokit_common_err(code), sys_iokit (err_system(0x38)) | sub_iokit_common (err_sub(0)) | code */
#define HSED_STUB_IOKIT_ERR(code) ((kern_return_t)(0xe0000000u | (code)))
#define kIOReturnNoMemory        HSED_STUB_IOKIT_ERR(0x2bdu)
#define kIOReturnNoDevice        HSED_STUB_IOKIT_ERR(0x2c0u)
#define kIOReturnNotPrivileged   HSED_STUB_IOKIT_ERR(0x2c1u)
#define kIOReturnBadArgument     HSED_STUB_IOKIT_ERR(0x2c2u)
#define kIOReturnExclusiveAccess HSED_STUB_IOKIT_ERR(0x2c5u)
#define kIOReturnUnsupported     HSED_STUB_IOKIT_ERR(0x2c7u)
#define kIOReturnNotOpen         HSED_STUB_IOKIT_ERR(0x2cdu)
#define kIOReturnNotAligned      HSED_STUB_IOKIT_ERR(0x2d0u)
#define kIOReturnNotReady        HSED_STUB_IOKIT_ERR(0x2d8u)
#define kIOReturnNoSpace         HSED_STUB_IOKIT_ERR(0x2dbu)
#define kIOReturnNotFound        HSED_STUB_IOKIT_ERR(0x2f0u)

#define IORegistryEntryGetParentEntry     hsed_stub_IORegistryEntryGetParentEntry
#define IOObjectRelease                   hsed_stub_IOObjectRelease
#define IOObjectConformsTo                hsed_stub_IOObjectConformsTo
#define IORegistryEntryCreateCFProperty   hsed_stub_IORegistryEntryCreateCFProperty
#define IOBSDNameMatching                 hsed_stub_IOBSDNameMatching
#define IOServiceGetMatchingService       hsed_stub_IOServiceGetMatchingService
#define IORegistryEntryGetRegistryEntryID hsed_stub_IORegistryEntryGetRegistryEntryID
#define IORegistryEntryGetChildIterator   hsed_stub_IORegistryEntryGetChildIterator
#define IOIteratorNext                    hsed_stub_IOIteratorNext
#define IORegistryEntryIDMatching         hsed_stub_IORegistryEntryIDMatching
#define IOServiceOpen                     hsed_stub_IOServiceOpen
#define IOServiceClose                    hsed_stub_IOServiceClose
#define IOConnectCallScalarMethod         hsed_stub_IOConnectCallScalarMethod
#define KextManagerCopyLoadedKextInfo     hsed_stub_KextManagerCopyLoadedKextInfo
kern_return_t hsed_stub_IORegistryEntryGetParentEntry(io_registry_entry_t entry, const char *plane, io_registry_entry_t *parent);
kern_return_t hsed_stub_IOObjectRelease(io_object_t object);
boolean_t hsed_stub_IOObjectConformsTo(io_object_t object, const char *class_name);
CFTypeRef hsed_stub_IORegistryEntryCreateCFProperty(io_registry_entry_t entry, CFStringRef key, CFAllocatorRef allocator,
                                                    IOOptionBits options);
CFMutableDictionaryRef hsed_stub_IOBSDNameMatching(mach_port_t port, uint32_t options, const char *bsd);
io_service_t hsed_stub_IOServiceGetMatchingService(mach_port_t port, CFDictionaryRef matching);
kern_return_t hsed_stub_IORegistryEntryGetRegistryEntryID(io_registry_entry_t entry, uint64_t *id);
kern_return_t hsed_stub_IORegistryEntryGetChildIterator(io_registry_entry_t entry, const char *plane, io_iterator_t *iterator);
io_object_t hsed_stub_IOIteratorNext(io_iterator_t iterator);
CFMutableDictionaryRef hsed_stub_IORegistryEntryIDMatching(uint64_t id);
kern_return_t hsed_stub_IOServiceOpen(io_service_t service, task_port_t task, uint32_t type, io_connect_t *connect);
kern_return_t hsed_stub_IOServiceClose(io_connect_t connect);
kern_return_t hsed_stub_IOConnectCallScalarMethod(mach_port_t connection, uint32_t selector, const uint64_t *input,
                                                  uint32_t inputs, uint64_t *output, uint32_t *outputs);
CFDictionaryRef hsed_stub_KextManagerCopyLoadedKextInfo(CFArrayRef identifiers, CFArrayRef keys);

/* The classes the storage headers name (IOMedia.h, IOBlockStorageDriver.h, IOBlockStorageDevice.h). */
#define kIOMediaClass              "IOMedia"
#define kIOBlockStorageDriverClass "IOBlockStorageDriver"
#define kIOBlockStorageDeviceClass "IOBlockStorageDevice"

/* --- the shim's geteuid (package.yaml renames it) --- */
uid_t hsed_stub_geteuid(void);

/* --- what a test sets and reads (Hsed.Transport.KextShimSpec) --- */

/* The stub's knobs (hsed_stub_set), each an integer:
   EUID           the effective user id the shim reads (default 501, not root);
   BSD_FOUND      whether IOBSDNameMatching finds the BSD name set (default 1);
   MEDIA_IS_MEDIA whether the entry found is an IOMedia (1);
   CHAIN          how many of the media's ancestors stand, driver, device, TPer (3: all);
   TPER_IS_TPER   whether the TPer is of the kext's class (1);
   TPER_FOUND     whether IORegistryEntryIDMatching's service is found again (1);
   REGISTRY_ID    the TPer's registry entry id (0x100000abc);
   CLIENTS        the user clients on the TPer before any open (0); an open adds one, a close takes one;
   ITERATOR_FAILS whether the TPer's child iterator fails (0);
   OPEN_RESULT    what IOServiceOpen answers (KERN_SUCCESS);
   SELECTOR0      what the kext's Open (selector 0) answers (KERN_SUCCESS);
   SELECTOR1      what its Close (selector 1) answers (KERN_SUCCESS);
   SELECTOR2      what its SCSI command (selector 2) answers (KERN_SUCCESS);
   OUTPUTS        how many outputs the SCSI command gives back (1);
   MOVED          the bytes moved it gives back (512);
   CLOSE_RESULT   what IOServiceClose answers (KERN_SUCCESS);
   BUILD_FOUND    whether the loaded kext's information is found (1). */
enum hsed_stub_knob {
    HSED_STUB_EUID = 0, HSED_STUB_BSD_FOUND, HSED_STUB_MEDIA_IS_MEDIA, HSED_STUB_CHAIN, HSED_STUB_TPER_IS_TPER,
    HSED_STUB_TPER_FOUND, HSED_STUB_REGISTRY_ID, HSED_STUB_CLIENTS, HSED_STUB_ITERATOR_FAILS, HSED_STUB_OPEN_RESULT,
    HSED_STUB_SELECTOR0, HSED_STUB_SELECTOR1, HSED_STUB_SELECTOR2, HSED_STUB_OUTPUTS, HSED_STUB_MOVED,
    HSED_STUB_CLOSE_RESULT, HSED_STUB_BUILD_FOUND, HSED_STUB_KNOBS
};

/* Its texts (hsed_stub_set_text), each NULL for none: the BSD name the media answers to ("disk6"); the
   TPer's 'TPer Interface Type' ("SAT"); its IDENTIFY serial ("TESTSERIAL0001"); the loaded kext's
   CFBundleVersion ("625"). */
enum hsed_stub_text { HSED_STUB_BSD = 0, HSED_STUB_INTERFACE, HSED_STUB_SERIAL, HSED_STUB_BUILD, HSED_STUB_TEXTS };

/* The calls the stub records, in order (hsed_stub_calls). */
enum hsed_stub_call {
    HSED_STUB_CALL_GETEUID = 1, HSED_STUB_CALL_BSD_MATCHING, HSED_STUB_CALL_ID_MATCHING, HSED_STUB_CALL_GET_MATCHING,
    HSED_STUB_CALL_CONFORMS, HSED_STUB_CALL_PARENT, HSED_STUB_CALL_PROPERTY, HSED_STUB_CALL_ENTRY_ID,
    HSED_STUB_CALL_CHILDREN, HSED_STUB_CALL_NEXT, HSED_STUB_CALL_RELEASE, HSED_STUB_CALL_KEXT_INFO,
    HSED_STUB_CALL_SERVICE_OPEN, HSED_STUB_CALL_SERVICE_CLOSE, HSED_STUB_CALL_SELECTOR0, HSED_STUB_CALL_SELECTOR1,
    HSED_STUB_CALL_SELECTOR2, HSED_STUB_CALL_SELECTOR_OTHER
};

void hsed_stub_reset(void);
void hsed_stub_set(int knob, int64_t value);
void hsed_stub_set_text(int which, const char *text);
/* The calls recorded since the reset, at most max of them written to out; how many there were. */
int hsed_stub_calls(int *out, int max);
/* The last IOConnectCallScalarMethod's connection, selector, inputs (their count, and up to six of them), and
   the outputs it was offered (0 for a NULL output or count; else the count given); 0 if there was none, 1 if
   there was. */
int hsed_stub_scalar_call(uint32_t *connection, uint32_t *selector, uint32_t *inputs, uint64_t input[6], uint32_t *outputs);
/* The connection IOServiceOpen gives, and the user clients on the TPer now. */
uint32_t hsed_stub_connection(void);
int hsed_stub_clients(void);
/* CF objects the stub made that are still unreleased (the ones CFSTR makes, which CoreFoundation never frees,
   left out), and registry objects it handed out that are still unreleased. */
int hsed_stub_cf_live(void);
int hsed_stub_io_live(void);

#endif /* HSED_KEXT_STUB_H */
