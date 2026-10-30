/*
 * hsed_kext.c: the C shim between hsed and the BrightPlaza TPer kext's user
 * client (hsed_kext.h; design docs/spec-as-source.md 7.2, M3).
 *
 * Until milestone M3's group G4 writes the bodies, each function refuses
 * with kIOReturnUnsupported and touches nothing: the file proves that the
 * shim compiles against the SDK's IOKit headers and that the package links
 * with the IOKit and CoreFoundation frameworks (stack build hsed --flag
 * hsed:kext).
 */
#include <IOKit/IOKitLib.h>

#include "hsed_kext.h"

int hsed_kext_locate(const char *bsd, hsed_kext_info *out)
{
    (void)bsd;
    (void)out;
    return kIOReturnUnsupported;
}

int hsed_kext_open(hsed_kext_info *info)
{
    (void)info;
    return kIOReturnUnsupported;
}

int hsed_kext_level0(const hsed_kext_info *info, uint8_t blocks, void *buf, uint64_t bufsize,
                     uint8_t cdb_out[16], uint64_t *xfer)
{
    (void)info;
    (void)blocks;
    (void)buf;
    (void)bufsize;
    (void)cdb_out;
    (void)xfer;
    return kIOReturnUnsupported;
}

int hsed_kext_close(hsed_kext_info *info)
{
    (void)info;
    return kIOReturnUnsupported;
}
