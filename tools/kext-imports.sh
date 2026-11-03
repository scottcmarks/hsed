#!/bin/bash
# tools/kext-imports.sh: the kext shim's object held to what it may import (design 7.2): nm -u of every hsed_kext.o a
# flag-on build left under a work dir, each undefined symbol on the list below or one the compiler adds itself (the
# stack protector's, the memory builtins'). The one IOConnect call is IOConnectCallScalarMethod: no IOConnectCallMethod,
# struct, asynchronous, mapping or trap call; nothing of SEDKernelInterface.framework; and of the portable C
# (hsed_cdb.c) the scalars and the bind's checks alone, from which the shim takes every scalar it passes the kernel and
# every bind's decision. The build is compiled and linked, never run (CI's macOS step; the integration's check).
#
#   tools/kext-imports.sh WORKDIR     WORKDIR the flag-on build's (.stack-work-kext, .stack-work-kextcheck)
#
# Exit status: 0 when at least one hsed_kext.o is found and each imports only these, 1 otherwise, 2 a usage error.
set -euo pipefail

[ $# -eq 1 ] || { echo "usage: tools/kext-imports.sh WORKDIR" >&2; exit 2; }
cd "$(git -C "$(dirname "$0")/.." rev-parse --show-toplevel)"

allowed='_CFArrayCreate _CFDictionaryGetTypeID _CFDictionaryGetValue _CFGetTypeID _CFRelease _CFStringCompare
_CFStringGetCString _CFStringGetTypeID _IOBSDNameMatching _IOConnectCallScalarMethod _IOIteratorNext _IOObjectConformsTo
_IOObjectRelease _IORegistryEntryCreateCFProperty _IORegistryEntryGetChildIterator _IORegistryEntryGetParentEntry
_IORegistryEntryGetRegistryEntryID _IORegistryEntryIDMatching _IOServiceClose _IOServiceGetMatchingService _IOServiceOpen
_KextManagerCopyLoadedKextInfo ___CFConstantStringClassReference _geteuid _hsed_cdb_bind_check _hsed_cdb_scalars
_kCFAllocatorDefault _kCFBundleIdentifierKey _kCFBundleVersionKey _kCFTypeArrayCallBacks _mach_task_self_ _sysconf'
# what a compiler adds of its own accord: the stack protector, and the memory builtins it may call for a struct's
# initialiser or a memset
compilers='^(___stack_chk_(fail|guard)|_mem(set|cpy|move)|___bzero|_bzero|___mem(set|cpy|move)_chk)$'

objs=$(find . -path "*/$1/*" -name hsed_kext.o -type f 2>/dev/null || true)
[ -n "$objs" ] || { echo "kext-imports: no hsed_kext.o under $1: build it flag-on first" >&2; exit 1; }
failed=0
for o in $objs; do
    while read -r sym; do
        [ -n "$sym" ] || continue
        if grep -qxF -- "$sym" <<<"$(tr ' \n' '\n\n' <<<"$allowed")" || [[ $sym =~ $compilers ]]; then continue; fi
        echo "kext-imports: $o imports $sym, which the shim may not" >&2
        failed=1
    done <<<"$(nm -u "$o")"
    [ "$failed" -ne 0 ] || echo "kext-imports: $o imports only what the shim may"
done
exit "$failed"
