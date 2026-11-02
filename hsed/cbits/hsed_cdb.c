/*
 * hsed_cdb.c: the CDBs of the five commands a drive transport carries
 * (hsed_cdb.h; design docs/spec-as-source.md 7.2 and 9.3, M4c).
 *
 * Portable C99 with no IOKit, compiled into the hsed library in every
 * build, Linux CI's included. At M4c's scaffold each function refuses:
 * hsed_cdb_build and hsed_cdb_check_payload answer HSED_CDB_E_SHAPE and
 * hsed_cdb_base_ok 0, and nothing calls them. The bodies come with the
 * shim's send path (M4c's G1): the four templates, the per-shape table and
 * the payload checks, held through FFI to Hsed.Transport.Cdb.cdbFor.
 */
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "hsed_cdb.h"

/* No base is taken yet. */
int hsed_cdb_base_ok(uint16_t base)
{
    (void)base;
    return 0;
}

/* No CDB is built yet: the CDB zeroed, as on any refusal, and nothing else
   written. */
int hsed_cdb_build(int sat, int shape, uint16_t base, uint8_t blocks, uint8_t cdb[16], uint32_t *direction, uint64_t *requested)
{
    (void)sat;
    (void)shape;
    (void)base;
    (void)blocks;
    (void)direction;
    (void)requested;
    if (cdb != NULL)
        memset(cdb, 0, 16);
    return HSED_CDB_E_SHAPE;
}

/* No payload is taken yet. */
int hsed_cdb_check_payload(int shape, uint16_t base, const uint8_t *buf, uint64_t len)
{
    (void)shape;
    (void)base;
    (void)buf;
    (void)len;
    return HSED_CDB_E_SHAPE;
}
