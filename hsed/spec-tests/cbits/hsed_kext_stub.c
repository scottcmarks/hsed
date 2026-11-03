/*
 * hsed_kext_stub.c: the stub of Mach, CoreFoundation and IOKit that cbits/hsed_kext.c is compiled against in
 * hsed-test (spec-tests/cbits/stub/hsed_kext_stub.h says what it stands in for and how a test drives it). One
 * drive's registry, its entries numbered: the IOMedia whole disk, its block storage driver and device, the
 * kext's TPer, the TPer's user clients, and one child iterator; and the CoreFoundation objects the shim reads,
 * the stub's own. Every call the shim makes is recorded in order; nothing here reaches IOKit.
 */
#include <stdlib.h>
#include <string.h>

#include <hsed_kext_stub.h>

/* the registry's entries, and the connection IOServiceOpen gives */
enum { MEDIA = 1, DRIVER = 2, DEVICE = 3, TPER = 4, ITERATOR = 5, CLIENT = 16 };
#define CONNECTION 0x4d2u

#define TPER_CLASS        "com_brightplaza_BrightPlazaTPer"
#define USER_CLIENT_CLASS "com_brightplaza_BrightPlazaTPerUserClient"
#define BUNDLE_ID         "com.brightplaza.BrightPlazaTPer"

/* --- the CoreFoundation objects --- */

enum { CF_STRING = 1, CF_DICTIONARY = 2, CF_ARRAY = 3, CF_MATCHING_BSD = 4, CF_MATCHING_ID = 5 };
#define CF_ENTRIES 4

struct hsed_stub_cf {
    int kind;
    int constant;                                      /* a CFSTR's, never released */
    char text[80];                                     /* a string's bytes, a BSD matching's name */
    uint64_t id;                                       /* an id matching's registry entry id */
    int count;                                         /* a dictionary's entries, an array's values */
    const struct hsed_stub_cf *keys[CF_ENTRIES];       /* a dictionary's keys (strings) */
    const struct hsed_stub_cf *values[CF_ENTRIES];     /* its values, which it owns; an array's, which it does not */
};

const CFArrayCallBacks hsed_stub_kCFTypeArrayCallBacks = { 0, NULL, NULL, NULL, NULL };

static int cf_live;                                    /* objects made and not yet released, CFSTR's left out */

static struct hsed_stub_cf *cf_new(int kind)
{
    struct hsed_stub_cf *o = calloc(1, sizeof *o);
    if (o == NULL)
        abort();
    o->kind = kind;
    cf_live++;
    return o;
}

static struct hsed_stub_cf *cf_string(const char *text)
{
    struct hsed_stub_cf *o = cf_new(CF_STRING);
    strncpy(o->text, text, sizeof o->text - 1);
    return o;
}

/* a dictionary of one key, its value owned */
static struct hsed_stub_cf *cf_dictionary(const char *key, struct hsed_stub_cf *value)
{
    struct hsed_stub_cf *o = cf_new(CF_DICTIONARY);
    o->keys[0] = hsed_stub_CFSTR(key);
    o->values[0] = value;
    o->count = 1;
    return o;
}

static void cf_free(struct hsed_stub_cf *o)
{
    int i;
    if (o == NULL || o->constant)
        return;
    if (o->kind == CF_DICTIONARY)
        for (i = 0; i < o->count; i++)
            cf_free((struct hsed_stub_cf *)o->values[i]);
    cf_live--;
    free(o);
}

/* the CFSTR constants, made once each */
static struct hsed_stub_cf *constants[32];
static int nconstants;

CFStringRef hsed_stub_CFSTR(const char *text)
{
    int i;
    struct hsed_stub_cf *o = NULL;
    for (i = 0; i < nconstants; i++)
        if (strcmp(constants[i]->text, text) == 0)
            return constants[i];
    if (nconstants == (int)(sizeof constants / sizeof constants[0]))
        abort();
    o = calloc(1, sizeof *o);
    if (o == NULL)
        abort();
    o->kind = CF_STRING;
    o->constant = 1;
    strncpy(o->text, text, sizeof o->text - 1);
    constants[nconstants++] = o;
    return o;
}

CFTypeID hsed_stub_CFGetTypeID(CFTypeRef value)
{
    return value == NULL ? 0 : (CFTypeID)((const struct hsed_stub_cf *)value)->kind;
}

CFTypeID hsed_stub_CFStringGetTypeID(void)
{
    return CF_STRING;
}

CFTypeID hsed_stub_CFDictionaryGetTypeID(void)
{
    return CF_DICTIONARY;
}

Boolean hsed_stub_CFStringGetCString(CFStringRef value, char *buffer, CFIndex size, CFStringEncoding encoding)
{
    if (value == NULL || value->kind != CF_STRING || buffer == NULL || size <= 0 || encoding != kCFStringEncodingUTF8
        || strlen(value->text) >= (size_t)size)
        return 0;
    strcpy(buffer, value->text);
    return 1;
}

CFComparisonResult hsed_stub_CFStringCompare(CFStringRef a, CFStringRef b, CFStringCompareFlags flags)
{
    int c = 0;
    (void)flags;
    c = strcmp(a->text, b->text);
    return c < 0 ? -1 : c > 0 ? 1 : kCFCompareEqualTo;
}

const void *hsed_stub_CFDictionaryGetValue(CFDictionaryRef dictionary, const void *key)
{
    int i;
    const struct hsed_stub_cf *k = key;
    if (dictionary == NULL || dictionary->kind != CF_DICTIONARY || k == NULL || k->kind != CF_STRING)
        return NULL;
    for (i = 0; i < dictionary->count; i++)
        if (strcmp(dictionary->keys[i]->text, k->text) == 0)
            return dictionary->values[i];
    return NULL;
}

CFArrayRef hsed_stub_CFArrayCreate(CFAllocatorRef allocator, const void **values, CFIndex count, const CFArrayCallBacks *callbacks)
{
    CFIndex i;
    struct hsed_stub_cf *o = NULL;
    (void)allocator;
    (void)callbacks;
    if (count < 0 || count > CF_ENTRIES || (count > 0 && values == NULL))
        return NULL;
    o = cf_new(CF_ARRAY);
    for (i = 0; i < count; i++)
        o->values[i] = values[i];
    o->count = (int)count;
    return o;
}

void hsed_stub_CFRelease(CFTypeRef value)
{
    if (value == NULL)
        abort();   /* CoreFoundation's CFRelease(NULL) crashes as well */
    cf_free((struct hsed_stub_cf *)value);
}

/* --- the world a test sets up --- */

static int64_t knobs[HSED_STUB_KNOBS];
static char texts[HSED_STUB_TEXTS][80];
static int text_set[HSED_STUB_TEXTS];

static int calls[512];
static int ncalls;

static int scalar_seen;
static uint32_t scalar_connection, scalar_selector, scalar_inputs, scalar_outputs;
static uint64_t scalar_input[6];

static int clients_now;        /* the user clients on the TPer */
static int next_child;         /* the child iterator's place: the device, then each user client */
static int io_live;            /* registry objects handed out and not yet released */

static void record(int call)
{
    if (ncalls < (int)(sizeof calls / sizeof calls[0]))
        calls[ncalls] = call;
    ncalls++;
}

static void set_text(int which, const char *text)
{
    text_set[which] = text != NULL;
    texts[which][0] = '\0';
    if (text != NULL)
        strncpy(texts[which], text, sizeof texts[which] - 1);
}

void hsed_stub_reset(void)
{
    static const int64_t defaults[HSED_STUB_KNOBS] = {
        [HSED_STUB_EUID] = 501, [HSED_STUB_BSD_FOUND] = 1, [HSED_STUB_MEDIA_IS_MEDIA] = 1, [HSED_STUB_CHAIN] = 3,
        [HSED_STUB_TPER_IS_TPER] = 1, [HSED_STUB_TPER_FOUND] = 1, [HSED_STUB_REGISTRY_ID] = 0x100000abc,
        [HSED_STUB_CLIENTS] = 0, [HSED_STUB_ITERATOR_FAILS] = 0, [HSED_STUB_OPEN_RESULT] = KERN_SUCCESS,
        [HSED_STUB_SELECTOR0] = KERN_SUCCESS, [HSED_STUB_SELECTOR1] = KERN_SUCCESS, [HSED_STUB_SELECTOR2] = KERN_SUCCESS,
        [HSED_STUB_OUTPUTS] = 1, [HSED_STUB_MOVED] = 512, [HSED_STUB_CLOSE_RESULT] = KERN_SUCCESS, [HSED_STUB_BUILD_FOUND] = 1 };
    memcpy(knobs, defaults, sizeof knobs);
    set_text(HSED_STUB_BSD, "disk6");
    set_text(HSED_STUB_INTERFACE, "SAT");
    set_text(HSED_STUB_SERIAL, "TESTSERIAL0001");
    set_text(HSED_STUB_BUILD, "625");
    ncalls = 0;
    scalar_seen = 0;
    scalar_connection = scalar_selector = scalar_inputs = scalar_outputs = 0;
    memset(scalar_input, 0, sizeof scalar_input);
    clients_now = 0;
    next_child = 0;
    io_live = 0;
    cf_live = 0;
}

void hsed_stub_set(int knob, int64_t value)
{
    if (knob < 0 || knob >= HSED_STUB_KNOBS)
        abort();
    knobs[knob] = value;
    if (knob == HSED_STUB_CLIENTS)
        clients_now = (int)value;
}

void hsed_stub_set_text(int which, const char *text)
{
    if (which < 0 || which >= HSED_STUB_TEXTS)
        abort();
    set_text(which, text);
}

int hsed_stub_calls(int *out, int max)
{
    int i;
    for (i = 0; i < ncalls && i < max && i < (int)(sizeof calls / sizeof calls[0]); i++)
        out[i] = calls[i];
    return ncalls;
}

int hsed_stub_scalar_call(uint32_t *connection, uint32_t *selector, uint32_t *inputs, uint64_t input[6], uint32_t *outputs)
{
    *connection = scalar_connection;
    *selector = scalar_selector;
    *inputs = scalar_inputs;
    memcpy(input, scalar_input, sizeof scalar_input);
    *outputs = scalar_outputs;
    return scalar_seen;
}

uint32_t hsed_stub_connection(void)
{
    return CONNECTION;
}

int hsed_stub_clients(void)
{
    return clients_now;
}

int hsed_stub_cf_live(void)
{
    return cf_live;
}

int hsed_stub_io_live(void)
{
    return io_live;
}

/* --- Mach and the shim's geteuid --- */

mach_port_t hsed_stub_mach_task_self(void)
{
    return 0x103u;
}

uid_t hsed_stub_geteuid(void)
{
    record(HSED_STUB_CALL_GETEUID);
    return (uid_t)knobs[HSED_STUB_EUID];
}

/* --- IOKit --- */

static io_object_t handed(io_object_t object)
{
    if (object != IO_OBJECT_NULL)
        io_live++;
    return object;
}

CFMutableDictionaryRef hsed_stub_IOBSDNameMatching(mach_port_t port, uint32_t options, const char *bsd)
{
    struct hsed_stub_cf *o = NULL;
    (void)port;
    (void)options;
    record(HSED_STUB_CALL_BSD_MATCHING);
    o = cf_new(CF_MATCHING_BSD);
    strncpy(o->text, bsd, sizeof o->text - 1);
    return o;
}

CFMutableDictionaryRef hsed_stub_IORegistryEntryIDMatching(uint64_t id)
{
    struct hsed_stub_cf *o = NULL;
    record(HSED_STUB_CALL_ID_MATCHING);
    o = cf_new(CF_MATCHING_ID);
    o->id = id;
    return o;
}

io_service_t hsed_stub_IOServiceGetMatchingService(mach_port_t port, CFDictionaryRef matching)
{
    io_service_t found = IO_OBJECT_NULL;
    (void)port;
    record(HSED_STUB_CALL_GET_MATCHING);
    if (matching == NULL)
        return IO_OBJECT_NULL;
    if (matching->kind == CF_MATCHING_BSD && knobs[HSED_STUB_BSD_FOUND] && text_set[HSED_STUB_BSD]
        && strcmp(matching->text, texts[HSED_STUB_BSD]) == 0)
        found = MEDIA;
    else if (matching->kind == CF_MATCHING_ID && knobs[HSED_STUB_TPER_FOUND] && matching->id == (uint64_t)knobs[HSED_STUB_REGISTRY_ID])
        found = TPER;
    cf_free((struct hsed_stub_cf *)matching);   /* IOServiceGetMatchingService consumes its matching dictionary */
    return handed(found);
}

boolean_t hsed_stub_IOObjectConformsTo(io_object_t object, const char *class_name)
{
    record(HSED_STUB_CALL_CONFORMS);
    switch (object) {
    case MEDIA:  return knobs[HSED_STUB_MEDIA_IS_MEDIA] && strcmp(class_name, kIOMediaClass) == 0;
    case DRIVER: return strcmp(class_name, kIOBlockStorageDriverClass) == 0;
    case DEVICE: return strcmp(class_name, kIOBlockStorageDeviceClass) == 0;
    case TPER:   return knobs[HSED_STUB_TPER_IS_TPER] && strcmp(class_name, TPER_CLASS) == 0;
    default:     return object >= CLIENT && strcmp(class_name, USER_CLIENT_CLASS) == 0;
    }
}

kern_return_t hsed_stub_IORegistryEntryGetParentEntry(io_registry_entry_t entry, const char *plane, io_registry_entry_t *parent)
{
    record(HSED_STUB_CALL_PARENT);
    *parent = IO_OBJECT_NULL;
    if (strcmp(plane, kIOServicePlane) != 0)
        return kIOReturnBadArgument;
    if (entry == MEDIA && knobs[HSED_STUB_CHAIN] >= 1)
        *parent = handed(DRIVER);
    else if (entry == DRIVER && knobs[HSED_STUB_CHAIN] >= 2)
        *parent = handed(DEVICE);
    else if (entry == DEVICE && knobs[HSED_STUB_CHAIN] >= 3)
        *parent = handed(TPER);
    return *parent == IO_OBJECT_NULL ? kIOReturnNotFound : KERN_SUCCESS;
}

kern_return_t hsed_stub_IOObjectRelease(io_object_t object)
{
    record(HSED_STUB_CALL_RELEASE);
    if (object == IO_OBJECT_NULL)
        return kIOReturnBadArgument;
    io_live--;
    return KERN_SUCCESS;
}

CFTypeRef hsed_stub_IORegistryEntryCreateCFProperty(io_registry_entry_t entry, CFStringRef key, CFAllocatorRef allocator,
                                                    IOOptionBits options)
{
    (void)allocator;
    (void)options;
    record(HSED_STUB_CALL_PROPERTY);
    if (entry != TPER || key == NULL)
        return NULL;
    if (strcmp(key->text, "TPer Interface Type") == 0)
        return text_set[HSED_STUB_INTERFACE] ? cf_string(texts[HSED_STUB_INTERFACE]) : NULL;
    if (strcmp(key->text, "Identify Device Characteristics") == 0)
        return text_set[HSED_STUB_SERIAL] ? cf_dictionary("Serial Number", cf_string(texts[HSED_STUB_SERIAL])) : NULL;
    if (strcmp(key->text, "CFBundleIdentifier") == 0)
        return cf_string(BUNDLE_ID);
    return NULL;
}

kern_return_t hsed_stub_IORegistryEntryGetRegistryEntryID(io_registry_entry_t entry, uint64_t *id)
{
    record(HSED_STUB_CALL_ENTRY_ID);
    if (entry != TPER)
        return kIOReturnBadArgument;
    *id = (uint64_t)knobs[HSED_STUB_REGISTRY_ID];
    return KERN_SUCCESS;
}

kern_return_t hsed_stub_IORegistryEntryGetChildIterator(io_registry_entry_t entry, const char *plane, io_iterator_t *iterator)
{
    record(HSED_STUB_CALL_CHILDREN);
    *iterator = IO_OBJECT_NULL;
    if (knobs[HSED_STUB_ITERATOR_FAILS] || entry != TPER || strcmp(plane, kIOServicePlane) != 0)
        return kIOReturnNoMemory;
    next_child = 0;
    *iterator = handed(ITERATOR);
    return KERN_SUCCESS;
}

io_object_t hsed_stub_IOIteratorNext(io_iterator_t iterator)
{
    io_object_t child = IO_OBJECT_NULL;
    record(HSED_STUB_CALL_NEXT);
    if (iterator != ITERATOR)
        return IO_OBJECT_NULL;
    /* the TPer's children in the service plane: the block storage device, then its user clients */
    if (next_child == 0)
        child = DEVICE;
    else if (next_child <= clients_now)
        child = (io_object_t)(CLIENT + next_child - 1);
    if (child != IO_OBJECT_NULL)
        next_child++;
    return handed(child);
}

CFDictionaryRef hsed_stub_KextManagerCopyLoadedKextInfo(CFArrayRef identifiers, CFArrayRef keys)
{
    record(HSED_STUB_CALL_KEXT_INFO);
    if (identifiers == NULL || keys == NULL || identifiers->count != 1 || keys->count != 1 || !knobs[HSED_STUB_BUILD_FOUND]
        || !text_set[HSED_STUB_BUILD] || strcmp(identifiers->values[0]->text, BUNDLE_ID) != 0
        || strcmp(keys->values[0]->text, "CFBundleVersion") != 0)
        return NULL;
    return cf_dictionary(BUNDLE_ID, cf_dictionary("CFBundleVersion", cf_string(texts[HSED_STUB_BUILD])));
}

kern_return_t hsed_stub_IOServiceOpen(io_service_t service, task_port_t task, uint32_t type, io_connect_t *connect)
{
    (void)task;
    record(HSED_STUB_CALL_SERVICE_OPEN);
    *connect = IO_OBJECT_NULL;
    if (service != TPER || type != 0)
        return kIOReturnBadArgument;
    if (knobs[HSED_STUB_OPEN_RESULT] != KERN_SUCCESS)
        return (kern_return_t)knobs[HSED_STUB_OPEN_RESULT];
    clients_now++;   /* the kext's user client, a child of the TPer */
    *connect = CONNECTION;
    return KERN_SUCCESS;
}

kern_return_t hsed_stub_IOServiceClose(io_connect_t connect)
{
    record(HSED_STUB_CALL_SERVICE_CLOSE);
    if (connect != CONNECTION)
        return kIOReturnBadArgument;
    if (clients_now > 0)
        clients_now--;
    return (kern_return_t)knobs[HSED_STUB_CLOSE_RESULT];
}

kern_return_t hsed_stub_IOConnectCallScalarMethod(mach_port_t connection, uint32_t selector, const uint64_t *input,
                                                  uint32_t inputs, uint64_t *output, uint32_t *outputs)
{
    uint32_t i;
    record(selector == 0 ? HSED_STUB_CALL_SELECTOR0 : selector == 1 ? HSED_STUB_CALL_SELECTOR1
           : selector == 2 ? HSED_STUB_CALL_SELECTOR2 : HSED_STUB_CALL_SELECTOR_OTHER);
    scalar_seen = 1;
    scalar_connection = connection;
    scalar_selector = selector;
    scalar_inputs = inputs;
    memset(scalar_input, 0, sizeof scalar_input);
    for (i = 0; input != NULL && i < inputs && i < 6; i++)
        scalar_input[i] = input[i];
    scalar_outputs = (output == NULL || outputs == NULL) ? 0 : *outputs;
    if (connection != CONNECTION)
        return kIOReturnBadArgument;
    switch (selector) {
    case 0:
        return (kern_return_t)knobs[HSED_STUB_SELECTOR0];
    case 1:
        return (kern_return_t)knobs[HSED_STUB_SELECTOR1];
    case 2:
        if (output != NULL && outputs != NULL && *outputs >= 1) {
            output[0] = (uint64_t)knobs[HSED_STUB_MOVED];
            *outputs = (uint32_t)knobs[HSED_STUB_OUTPUTS];
        }
        return (kern_return_t)knobs[HSED_STUB_SELECTOR2];
    default:
        return kIOReturnUnsupported;
    }
}
