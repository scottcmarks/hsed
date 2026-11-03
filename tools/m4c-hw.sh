#!/bin/bash
# tools/m4c-hw.sh: the hardware half of milestone M4c (docs/spec-as-source.md section 11): the drive named by its
# serial, the pre-flight anyone may run as themselves, a snapshot of the drive's volume and the comparison of two, and
# Scott's hardware steps H0-H14, printed after the pre-flight with the drive filled in.
#
#   tools/m4c-hw.sh disk SERIAL            the whole disk (diskN) under the kext TPer whose trimmed IDENTIFY serial is
#                                          SERIAL, alone on stdout; exit 1 unless exactly one TPer carries it
#   tools/m4c-hw.sh [SERIAL]               the pre-flight, then the steps (SERIAL S59BNM0RA15802W if none is given)
#   tools/m4c-hw.sh snapshot NAME SERIAL   build/m4c/NAME.manifest, NAME.volume and NAME.writes (Scott's, H4 and H12:
#                                          it reads the files of the drive's mounted volumes)
#   tools/m4c-hw.sh compare A B            two snapshots compared: exit 1 unless the volume is unchanged
#
# It runs nothing privileged and nothing that reaches the drive: it reads the IORegistry (ioreg), the loaded kexts
# (kmutil showloaded), the installed kext's Info.plist, load commands and imports (PlistBuddy, otool, nm), the process
# table (pgrep), launchd (launchctl print), the mount table (/sbin/mount, no argument), DiskArbitration (diskutil info),
# and the symbols of build/bin/hsed-jig and of the hsed-jig the shared .stack-work holds (nm, which never runs them);
# snapshot also reads the files of the drive's mounted volumes (find, shasum: the FAT32 volume is mounted noatime, so
# reading writes nothing). Every command that needs root is printed for Scott in the steps, never run here; hsed-jig
# repeats its own pre-flights before it opens anything (Hsed.Transport.MacKext, Hsed.Jig.Preflight) and refuses
# without root.
#
# The drive is named by its serial, never by its BSD number, which is assigned at each attach (disk6 on 2026-10-25;
# disk22 on 2026-11-02, when disk6 was a disk image): hsed-jig refuses a disk whose TPer or serial is not the one it
# expects, but sedutil-cli does not. The fixture keeps the name spec/fixtures/disk6-level0.bin, the drive as first seen.
#
# The steps are written for the default M4c ships (Hsed.Jig.Options.roStackResetBeforeCases, Scott's decision of
# 2026-11-02, design 13.7): a drive run resets its base ComID with STACK_RESET before each case, logged.
#
# The checks of build/bin/hsed-jig, the kext-enabled jig Scott builds at H1, ask it for the kext shim's
# _hsed_kext_command and _hsed_kext_bind and the portable _hsed_cdb_build; of the shared .stack-work's hsed-jig, the
# flag-off build the agents run, only that it has no _hsed_kext_ symbol (a static link leaves the unreferenced
# hsed_cdb.o out of it).
#
# Exit status: 0 when every check passes, 1 when one fails (the pre-flight prints the steps either way), 2 a usage
# error.
set -euo pipefail

kext=com.brightplaza.BrightPlazaTPer
jig=build/bin/hsed-jig
default_serial=S59BNM0RA15802W
failed=0

ok()    { printf 'ok:   %s\n' "$*"; }
fail()  { printf 'FAIL: %s\n' "$*"; failed=1; }
note()  { printf 'note: %s\n' "$*"; }
die()   { printf 'm4c-hw.sh: %s\n' "$*" >&2; exit 2; }
usage() { die "usage: tools/m4c-hw.sh disk SERIAL | tools/m4c-hw.sh [SERIAL] | tools/m4c-hw.sh snapshot NAME SERIAL | tools/m4c-hw.sh compare A B"; }

# Each kext TPer whose trimmed IDENTIFY serial is $1, a line each: the whole disk under it, its interface type and its
# registry id, separated by '|' (a field may be empty). ioreg prints a TPer's own properties before its children's, so the serial is known by the
# time its whole disk's IOMedia ("BSD Name" = "diskN", never a slice's diskNsM) is met.
tpers_of() {
    ioreg -r -c com_brightplaza_BrightPlazaTPer -l -w0 2>/dev/null | awk -v want="$1" '
        function flush() { if (seen && serial == want) printf "%s|%s|%s\n", disk, iface, id }
        /^\+-o com_brightplaza_BrightPlazaTPer / { flush(); seen = 1; serial = ""; iface = ""; disk = ""
            id = $0; sub(/.*, id /, "", id); sub(/,.*/, "", id) }
        /"TPer Interface Type" = / && match($0, /"TPer Interface Type" = "[^"]*"/) {
            iface = substr($0, RSTART + 25, RLENGTH - 26) }
        /"Identify Device Characteristics" = / && match($0, /"Serial Number"="[^"]*"/) {
            serial = substr($0, RSTART + 17, RLENGTH - 18); gsub(/^ +| +$/, "", serial) }
        disk == "" && match($0, /"BSD Name" = "disk[0-9]+"/) {
            disk = substr($0, RSTART + 14, RLENGTH - 15) }
        END { flush() }'
}

# The slices (diskNsM) under the kext TPer whose trimmed IDENTIFY serial is $1, a line each.
slices_of() {
    ioreg -r -c com_brightplaza_BrightPlazaTPer -l -w0 2>/dev/null | awk -v want="$1" '
        /^\+-o com_brightplaza_BrightPlazaTPer / { serial = "" }
        /"Identify Device Characteristics" = / && match($0, /"Serial Number"="[^"]*"/) {
            serial = substr($0, RSTART + 17, RLENGTH - 18); gsub(/^ +| +$/, "", serial) }
        serial == want && match($0, /"BSD Name" = "disk[0-9]+s[0-9]+"/) { print substr($0, RSTART + 14, RLENGTH - 15) }'
}

# The block storage driver's write counters under the kext TPer whose trimmed IDENTIFY serial is $1: its
# "Operations (Write)" and "Bytes (Write)", a line each. TCG commands go through the kext's own SCSI path, never this
# driver, so equal counters before and after a run mean no host block write reached the drive in between.
writes_of() {
    ioreg -r -c com_brightplaza_BrightPlazaTPer -l -w0 2>/dev/null | awk -v want="$1" '
        /^\+-o com_brightplaza_BrightPlazaTPer / { serial = "" }
        /"Identify Device Characteristics" = / && match($0, /"Serial Number"="[^"]*"/) {
            serial = substr($0, RSTART + 17, RLENGTH - 18); gsub(/^ +| +$/, "", serial) }
        serial == want && /"Statistics" = [{]/ { s = $0; sub(/.*"Statistics" = [{]/, "", s); sub(/[}].*/, "", s); print s }' \
        | tr ',' '\n' | grep -E '^"(Operations|Bytes) \(Write\)"=' | LC_ALL=C sort || true
}

# The mounts of a whole disk ($1, diskN) and its slices, a line each: the device and the mount point, tab-separated.
# A mount line is "DEVICE on MOUNT POINT (TYPE, options)": the device exactly the disk or one of its slices, the mount
# point whatever lies between " on " and the last " (", spaces and all (Hsed.Jig.Preflight reads it the same way).
mounts_of() {
    /sbin/mount | sed -n -E "s#^(/dev/$1(s[0-9]+)*) on (.*) \([^()]*\)\$#\1"$'\t'"\3#p"
}

# The whole disk of the drive whose serial is $1 on stdout, or why not on stderr (exit 1): exactly one kext TPer must
# carry the serial, a whole disk under it.
disk_of() {
    local found n
    found=$(tpers_of "$1")
    n=$(grep -c . <<<"$found" || true)
    if [ "$n" -eq 1 ] && [ -n "$(cut -d'|' -f1 <<<"$found")" ]; then
        cut -d'|' -f1 <<<"$found"
    elif [ "$n" -eq 0 ]; then
        printf 'm4c-hw.sh: no kext TPer carries IDENTIFY serial %s: the drive is absent, or not on the kext\n' "$1" >&2
        return 1
    elif [ "$n" -eq 1 ]; then
        printf 'm4c-hw.sh: the kext TPer that carries IDENTIFY serial %s has no whole disk under it\n' "$1" >&2
        return 1
    else
        printf 'm4c-hw.sh: %s kext TPers carry IDENTIFY serial %s: none is named\n' "$n" "$1" >&2
        return 1
    fi
}

# What a loaded kext UUID was read to be (design 13.12): build 625's arm64e slice, signed 2024-05-07.
known_uuid() {
    case "$1" in
        373BA509-89F7-3B36-A977-BB65C7635F1B)
            echo "build 625: no privilege check, no command whitelist (disassembled 2026-10-30, re-read 2026-11-02): hsed's Guard and shim are the safety" ;;
        *) echo "" ;;
    esac
}

# The installed kext binary whose Mach-O carries the UUID $1, if one does.
installed_binary() {
    local k exe loads
    [ -n "$1" ] || return 0
    for k in /Library/Extensions/*.kext; do
        [ "$(/usr/libexec/PlistBuddy -c 'Print :CFBundleIdentifier' "$k/Contents/Info.plist" 2>/dev/null)" = "$kext" ] || continue
        exe=$(/usr/libexec/PlistBuddy -c 'Print :CFBundleExecutable' "$k/Contents/Info.plist" 2>/dev/null || true)
        loads=$(otool -arch all -l "$k/Contents/MacOS/$exe" 2>/dev/null || true)
        if grep -q "uuid $1" <<<"$loads"; then echo "$k/Contents/MacOS/$exe"; return 0; fi
    done
}

# The pre-flight of the drive whose serial is $1, then the steps.
preflight() {
    local serial=$1 found disk="" iface id loaded build uuid known binary imports clients creators pids missing symbols shared b
    printf 'M4c hardware pre-flight: the drive whose IDENTIFY serial is %s\n\n' "$serial"

    # The drive: exactly one kext TPer with this serial, a whole disk under it, its interface SAT.
    found=$(tpers_of "$serial")
    case "$(grep -c . <<<"$found" || true)" in
        0) fail "no kext TPer carries IDENTIFY serial $serial: the drive is absent, or not on the kext" ;;
        1) IFS='|' read -r disk iface id <<<"$found"
           if [ -n "$disk" ]; then ok "the drive with serial $serial is /dev/$disk, on the kext's TPer (registry id $id)"
           else fail "the kext TPer with serial $serial (registry id $id) has no whole disk under it"; fi
           if [ "$iface" = SAT ]; then ok "its interface is SAT"; else fail "its interface is '$iface', not SAT"; fi ;;
        *) fail "more than one kext TPer carries IDENTIFY serial $serial: none is named" ;;
    esac

    # The kext, and what its binary checks (design 9.3, 13.12). Its build number names no source revision, so the
    # binary itself is read: the loaded kext's UUID, the installed binary that carries it, and whether that binary
    # imports IOUserClient::clientHasPrivilege (macos-driver 2be586e). Its ATA-command whitelist (macos-driver 7856c9c)
    # shows in no symbol: only a disassembly, recorded in known_uuid once made, says it.
    loaded=$(kmutil showloaded --list-only --bundle-identifier "$kext" 2>/dev/null | grep -F " $kext (" | head -1 || true)
    build=$(sed -n "s/.*$kext (\([^)]*\)).*/\1/p" <<<"$loaded")
    uuid=$(sed -n "s/.*$kext ([^)]*) \([0-9A-F-]\{36\}\).*/\1/p" <<<"$loaded")
    if [ -z "$build" ]; then
        fail "the kext $kext is not loaded"
    else
        ok "the kext $kext is loaded, build $build, UUID ${uuid:-unread}"
        known=$(known_uuid "$uuid")
        if [ -n "$known" ]; then ok "UUID $uuid is $known"
        else note "UUID ${uuid:-unread} was never read before: treat the kext as checking nothing, and read it (design 13.12)"; fi
        binary=$(installed_binary "$uuid")
        if [ -z "$binary" ]; then
            note "no installed kext binary carries UUID ${uuid:-unread}: its imports are unread; treat it as checking nothing"
        else
            imports=$(nm -u "$binary" 2>/dev/null || true)
            if grep -q clientHasPrivilege <<<"$imports"; then
                note "$binary imports IOUserClient::clientHasPrivilege: a build newer than 625 (design 13.12); record its UUID above once read"
            else
                ok "$binary, the installed binary of UUID $uuid, imports no IOUserClient::clientHasPrivilege: any local process can open its user client, and root is hsed's own gate (design 9.3, 13.12)"
            fi
        fi
    fi

    # No other opener: the kext admits one at a time, hsed-jig refuses to share (Hsed.Transport.Kext.admit), and the
    # bind requires exactly one user client, its own. A holder is named by the process that created its user client.
    clients=$(ioreg -r -c com_brightplaza_BrightPlazaTPerUserClient 2>/dev/null | grep -c '^+-o' || true)
    creators=$(ioreg -r -c com_brightplaza_BrightPlazaTPerUserClient -l -w0 2>/dev/null \
               | sed -n 's/.*"IOUserClientCreator" = "\(.*\)"/\1/p' | paste -sd ';' - || true)
    if [ "${clients:-0}" -eq 0 ]; then ok "no user client of the kext is open"
    else fail "${clients} user client(s) of the kext open, by ${creators:-a creator unread}: close it before a run"; fi
    for p in sedutil-cli sedd seddaemon SEDAccess sedagent hsed-jig; do
        if pids=$(pgrep -x "$p"); then fail "$p is running (pid $(echo $pids)): no other SED tool may run beside hsed-jig"
        else ok "$p is not running"; fi
    done
    if launchctl print system/com.brightplaza.sedd >/dev/null 2>&1; then
        fail "the SED ToolBox's daemon com.brightplaza.sedd is loaded in launchd: unload it before a run"
    else
        ok "the SED ToolBox's daemon com.brightplaza.sedd is not loaded"
    fi

    # The kext-enabled jig, built as Scott at H1 (never run here): the shim's command and bind, the portable CDB builder.
    if [ ! -x "$jig" ]; then
        fail "$jig is not built: H1 builds it"
    else
        symbols=$(nm "$jig" 2>/dev/null || true)
        missing=""
        for s in _hsed_kext_command _hsed_kext_bind _hsed_cdb_build; do
            grep -Eq "^[0-9a-f]+ [Tt] $s\$" <<<"$symbols" || missing="$missing $s"
        done
        if [ -n "$missing" ]; then fail "$jig lacks$missing: built without --flag hsed:kext, or before M4c's shim (H1)"
        elif [ "$(stat -f %u "$jig")" != "$(id -u)" ]; then fail "$jig is $(stat -f %Su "$jig")'s, not yours: build it as yourself (H1)"
        else ok "$jig is yours and carries the kext shim's _hsed_kext_command and _hsed_kext_bind and _hsed_cdb_build"; fi
    fi

    # The shared .stack-work's hsed-jig, which stack exec hsed-jig and the agents run: flag-off, no kext code.
    shared=0
    for b in .stack-work/install/*/*/*/bin/hsed-jig hsed/.stack-work/dist/*/*/build/hsed-jig/hsed-jig; do
        [ -x "$b" ] || continue
        shared=$((shared + 1))
        symbols=$(nm "$b" 2>/dev/null || true)
        if grep -q '_hsed_kext_' <<<"$symbols"; then
            fail "$b carries the kext transport: rebuild it flag-off (stack build hsed) before anything else runs"
        else
            ok "$b, the shared .stack-work's, carries no _hsed_kext_ symbol"
        fi
    done
    [ "$shared" -gt 0 ] || note "the shared .stack-work holds no hsed-jig: none to check"

    # What of the drive is mounted, and the block driver's writes so far (a run that may send needs --allow-mounted
    # for a mounted volume, design 9.3 item 4; snapshot and compare hold the writes across a run).
    if [ -n "$disk" ]; then
        mounted=$(mounts_of "$disk" || true)
        if [ -z "$mounted" ]; then ok "nothing of /dev/$disk is mounted"
        else
            while IFS=$'\t' read -r dev mp; do
                note "$dev is mounted on $mp: a run that may send needs --allow-mounted (design 9.3 item 4), or unmount it (H4)"
            done <<<"$mounted"
        fi
        note "the block driver's writes since the drive attached: $(writes_of "$serial" | paste -sd ' ' - || true)"
    fi

    printf '\n'
    if [ "$failed" -eq 0 ]; then printf 'Every pre-flight passes. '; else printf 'A pre-flight failed: fix what it names first (H2). '; fi
    printf "Scott's steps follow.\n\n"
    steps "$serial" "$disk"
    exit "$failed"
}

# Scott's hardware steps H0-H14, the serial and the disk the pre-flight found filled in.
steps() {
    local serial=$1 disk=${2:-}
    cat <<EOF
Scott's hardware steps for M4c (docs/spec-as-source.md 11), written for the default M4c ships: a drive run resets its
base ComID with STACK_RESET before each case, automatic and logged (Scott's decision of 2026-11-02, design 13.7). In
~/hsed, in order, least exposure first. Never run sedutil-cli and hsed-jig at the same time.

H0. Name the drive by its serial, never by a number typed from memory: the BSD number is assigned at each attach, and
    hsed-jig refuses a disk whose TPer or serial is wrong, but sedutil-cli does not. Derive DISK again after any replug.
      cd ~/hsed && git pull
      SERIAL=${serial}
      DISK=\$(tools/m4c-hw.sh disk "\$SERIAL") && echo "\$DISK"
    This pre-flight found ${disk:-no drive with that serial}.
EOF
    cat <<'EOF'

H1. Build the kext-enabled jig as yourself, never with sudo, in a work dir of its own (the shared .stack-work keeps the
    flag-off hsed-jig the agents run):
      stack --work-dir .stack-work-kext build hsed:exe:hsed-jig --flag hsed:kext --copy-bins --local-bin-path build/bin
    On failure: keep the log for an agent. Nothing has reached the drive.

H2. The pre-flight, as yourself; every check should pass:
      tools/m4c-hw.sh "$SERIAL"
    On failure: fix what it names (a holder by its IOUserClientCreator, 'pid N, name'), then run it again.

H3. M3's Level 0 capture, while FixtureSpec's six examples still pend: one IF-RECV, the drive Level0Only throughout,
    nothing bound, no --allow-mounted needed:
      sudo build/bin/hsed-jig --target /dev/$DISK --tests UCT-01 --expect-serial "$SERIAL" \
          --capture-level0 spec/fixtures/disk6-level0.bin --report-dir build/report
    Expect exit 0, and on stderr the user client opened (SAT, kext build 625), one command line with the CDB
    a1 08 0e 01 04 00 01 00 00 5c 00 00, kIOReturnSuccess (0x00000000), 2048 bytes transferred, the capture's sha256, and
    the user client closed. Then, as yourself (both exit 0):
      R=$(ls -t build/report/jig-dev-$DISK-*.json | head -1)
      build/bin/hsed-jig verify-transcript "$R" --no-if-send --level0-only
      build/bin/hsed-jig level0 decode spec/fixtures/disk6-level0.bin --against spec/fixtures/disk6-query.txt
    Hand spec/fixtures/disk6-level0.bin and its .json sidecar to an agent to test (stack test hsed: FixtureSpec's six
    run) and commit. The fixture keeps the name disk6: the drive as first seen.
    On failure: 'moved N of the 2048 bytes': stop; the JSON report keeps the bytes; 13.29 (d) is yours.
    kIOReturnExclusiveAccess: H2 names the holder. kIOReturnNoDevice or a serial refused: derive DISK again.
    level0 decode failing only on 0x0403's Length is 13.29 (i): commit nothing, hand it to an agent.

H4. The volume's baseline, as yourself, the volume left mounted:
      mkdir -p build/m4c && tools/m4c-hw.sh snapshot before "$SERIAL"
    It writes build/m4c/before.manifest (the sha256 of every file on the drive's mounted volumes but macOS's metadata:
    .fseventsd, .Spotlight-V100, .Trashes, .TemporaryItems, .DocumentRevisions-V100, ._* and .DS_Store; the mount is
    noatime, so reading writes nothing), before.volume (VolumeUUID, VolumeName, FilesystemType, TotalSize, FreeSpace)
    and before.writes (the block driver's Operations (Write) and Bytes (Write)). Optionally, a baseline check:
      diskutil verifyVolume /dev/${DISK}s1
    expecting 'File system check exit code is 0' (it may unmount and remount the volume; never use repairVolume).
    Or unmount the volume instead, before this snapshot (the lead's answer 6): diskutil unmount /dev/${DISK}s1. Then
    H6, H7, H9, H10, H11 and H13 need no --allow-mounted, the manifest is empty and the write counters stay silent;
    mount it again after H12's snapshot (diskutil mount /dev/${DISK}s1).
    On failure: a verify error predates hsed. Stop.

H5. sedutil's MSID, the oracle: the first IF-SEND through this bridge and kext 625, by an established tool. Only its
    length and its digest's first 12 hex digits are written, never the MSID:
      sudo sedutil-cli --printDefaultPassword /dev/$DISK | sed -n 's/^MSID: //p' | tr -d '\n' \
          | python3 -c 'import sys,hashlib; b=sys.stdin.buffer.read(); print(len(b), hashlib.sha256(b).hexdigest()[:12])' \
          > build/m4c/msid-sedutil.txt; cat build/m4c/msid-sedutil.txt
      tools/m4c-hw.sh "$SERIAL"
    The pre-flight after it: no user client left open.
    On failure (an error, or '0 e3b0c44298fc', nothing printed): stop before hsed's first IF-SEND and keep the output:
    the bridge or the kext refuses TRUSTED SEND.

H6. STACK_RESET alone, on the idle drive: protocol 2's first crossing of the bridge (the drive reports ComID Management
    Supported = 0, a VU bit in Opal Table 3):
      sudo build/bin/hsed-jig --recover --target /dev/$DISK --expect-serial "$SERIAL" --allow-mounted --report-dir build/report
    Expect exit 0, and on stderr Level 0; HANDLE_COMID_REQUEST a1 0a 06 02 01 00 04 10 00 5e 00 00 (512 bytes);
    GET_COMID_RESPONSE a1 08 0e 02 01 00 04 10 00 5c 00 00; the idle IF-RECV a1 08 0e 01 04 00 04 10 00 5c 00 00;
    StartSession a1 0a 06 01 01 00 04 10 00 5e 00 00 and its IF-RECV; End of Session and its IF-RECV. The summary:
    'recover: STACK_RESET on 0x1004: Success; the ComID idle; the Admin SP took a read-write session (TSN n) and ended it'.
    On failure: a kern_return failure on the STACK_RESET, 'Success/Failure 1' or 'No Response Available': the transport
    latched for that run; replug the bridge, derive DISK again, keep the report, and STOP here: every drive case resets
    its ComID first, so H7 cannot pass. An agent then sets Hsed.Jig.Options.roStackResetBeforeCases back to False (the
    probe-and-waive mode, built and tested) and records the --recover clause as unmet on this bridge.
    'moved N of 512 bytes' on the IF-SEND: it reached the drive; 13.29 (d) is yours. Stop.

H7. The acceptance run:
      sudo build/bin/hsed-jig --target /dev/$DISK --tests UCT-02,UCT-03:1a-1c,SPF-02:1 --expect-serial "$SERIAL" \
          --allow-mounted --report-dir build/report
      R7=$(ls -t build/report/jig-dev-$DISK-*.json | head -1)
    Expect exit 0: 16 drive commands, 7 of them IF-SENDs, 3 of those of protocol 2 (Level 0 first; then before each
    case a STACK_RESET pair, HANDLE_COMID_REQUEST and GET_COMID_RESPONSE, noted 'SyncIdle and NoSessions: STACK_RESET on
    0x1004: Success/Failure 0 ...'). Waivers: LockingManufactured on all three cases; CredentialsKnown,
    HostChallengeIsCurrentPin and ReadWriteSessions, vacuous, on UCT-02 and SPF-02:1 (UCT-03's three are notes).
    UCT-02: the TPer's eleven properties at or above their minima (a 0 noted as no limit), the host's three within
    [2048, 4096], [2028, 4076] and [1992, 4040]. UCT-03: 1a SUCCESS; 1b 'the MSID read, N bytes (a secret: masked ...)';
    1c End of Session answered. SPF-02:1: (0, 0, 0).
    On failure: exit 1: keep the reports, do not rerun blindly. '1a ... SP_BUSY ...': a session the STACK_RESET before
    the case did not clear (Core 3.3.4.7.5 item 1 says it must): stop and report it. Exit 3: stop; a drive command outside
    its policy is an hsed bug. 'moved N of M bytes': 13.29 (d).

H8. The report's checks, as yourself:
      build/bin/hsed-jig verify-transcript "$R7" --allow-list
      jq '.blocked, (.outside_policy | length), .exit' "$R7"
      diff <(jq -r '.secrets[] | [.len, .sha256] | @tsv' "$R7" | tr '\t' ' ') build/m4c/msid-sedutil.txt
      jq '.cases[] | {key, verdict, waivers}' "$R7"
    verify-transcript exits 0: every drive command Level 0, Properties, StartSession to the Admin SP as Anybody, Get on
    C_PIN_MSID's UID and PIN, End of Session or STACK_RESET, on the base ComID the drive's Level 0 reported, nothing
    blocked. jq prints 0, 0, 0. diff prints nothing: the MSID equals sedutil's.
    On failure: digests that differ while the lengths agree may be sedutil's %s print stopping at a NUL; to accept the
    MSID in clear in one report, rerun H7 with --show-msid and compare by eye (your call).

H9. The deliberately interrupted run:
      sudo build/bin/hsed-jig --target /dev/$DISK --tests UCT-03:1a-1c --expect-serial "$SERIAL" --allow-mounted \
          --interrupt-in-session --report-dir build/report
      R9=$(ls -t build/report/jig-dev-$DISK-*.json | head -1); jq -r .interrupted "$R9"
      tools/m4c-hw.sh "$SERIAL"
    Expect exit 130, .interrupted naming the session left open on ComID 0x1004 (End of Session not sent) and
    hsed-jig --recover, and no user client left open. (H9b, H7 run between H9 and H10 to see the orphan answer
    SP_BUSY, does not apply: the STACK_RESET before each case clears the orphan first.)
    On failure: exit 0 means UCT-03 opened no session: read the report.

H10. Recovery: H6's command again. Expect exit 0: the StartSession after the reset answering SUCCESS shows the Admin SP
    free of the orphan, since a second read-write session would answer SP_BUSY (Core 5.1.5.3, PDF 138).
    On failure: STACK_RESET failing: replug (a power cycle aborts every session, Core PDF 76), derive DISK again, run
    H10 again for the record. SP_BUSY after Success: the TPer kept a session through STACK_RESET (Core 3.3.4.7.5 item 1
    says it must not): stop and report. If the drive timed the session out first (DefSessionTimeout, PDF 76), H10 still
    passes; run H9 and H10 back to back.

H11. The acceptance run again, H7's command: exit 0. The drive is fully usable after the recovery.

H12. Close the window:
      tools/m4c-hw.sh snapshot after "$SERIAL" && tools/m4c-hw.sh compare before after
      sudo sedutil-cli --query /dev/$DISK > build/m4c/query-after.txt; diff spec/fixtures/disk6-query.txt build/m4c/query-after.txt
      tools/m4c-hw.sh "$SERIAL"
    compare: the manifests equal; VolumeUUID, VolumeName, FilesystemType and TotalSize equal; FreeSpace's difference
    printed; the write counters equal (no host block write in the window: TCG commands never pass that driver). The
    query: only line 2's device path differs. The pre-flight: no user client left open. Optionally, last of all:
    diskutil verifyVolume /dev/${DISK}s1 (it may remount). If you unmounted the volume at H4, mount it again now.
    On failure: a manifest difference outside macOS's metadata: stop and keep everything (none of the five commands
    addresses an LBA). Write counters that moved: macOS wrote to the mounted volume in the window; compare the .writes
    files, and unmount for a rerun if you want them silent.

H13. Optional, the Pair's Properties, live:
      sudo build/bin/hsed-jig --target sim+/dev/$DISK --tests UCT-02 --expect-serial "$SERIAL" --allow-mounted \
          --profile spec/fixtures/disk6-level0.bin --report-dir build/report
    exit 0: Level 0 differences allowed or unchecked only, and pair_properties' every difference judged by Opal 2.30
    Table 17. Or offline, without sudo:
      build/bin/hsed-jig --target sim --tests UCT-02 --report-dir build/report
      build/bin/hsed-jig diff "$(ls -t build/report/jig-sim-*.json | head -1)" "$R7"
    On failure: a difference not allowed names its rule (a TPer value below Table 17's minimum, or a host value outside
    [minimum, sent]): the drive's answer, recorded; hand the report to an agent.

H14. Keep build/report and build/m4c. Send the lead the reports of H3, H6, H7, H9, H10 and H11, and compare's output.
    Committing them under docs/evidence/ is your call: the manifests name the volume's files. The kext rebuild (13.12)
    stays open; if you rebuild, add its UUID to tools/m4c-hw.sh's known list and repeat H2 and H6 before anything else.
EOF
}

# A snapshot of the drive's volumes, named: the manifest of every mounted volume's files, each volume's identity and
# space, and the block driver's write counters (build/m4c/NAME.manifest, .volume, .writes).
snapshot() {
    local name=$1 serial=$2 disk out mounted dev mp slice plist key
    [[ $name =~ ^[A-Za-z0-9._-]+$ ]] || die "snapshot NAME: letters, digits, '.', '_' and '-' alone"
    disk=$(disk_of "$serial") || exit 1
    out=build/m4c
    mkdir -p "$out"
    mounted=$(mounts_of "$disk" || true)
    {
        if [ -z "$mounted" ]; then
            printf '# nothing of /dev/%s is mounted\n' "$disk"
        else
            while IFS=$'\t' read -r dev mp; do
                printf '# %s on %s\n' "$dev" "$mp"
                ( cd "$mp" && find . \( -name .fseventsd -o -name .Spotlight-V100 -o -name .Trashes -o -name .TemporaryItems \
                        -o -name .DocumentRevisions-V100 \) -prune -o -type f ! -name '._*' ! -name .DS_Store -print0 \
                    | LC_ALL=C sort -z | xargs -0 shasum -a 256 )
            done <<<"$mounted"
        fi
    } > "$out/$name.manifest"
    for slice in $(slices_of "$serial"); do
        plist=$(diskutil info -plist "/dev/$slice" 2>/dev/null || true)
        for key in VolumeUUID VolumeName FilesystemType TotalSize FreeSpace; do
            printf '%s\t%s\t%s\n' "$slice" "$key" "$(plutil -extract "$key" raw -o - - <<<"$plist" 2>/dev/null || true)"
        done
    done > "$out/$name.volume"
    writes_of "$serial" > "$out/$name.writes"
    printf 'm4c-hw.sh: %s/%s.manifest (%s files), %s.volume and %s.writes, of /dev/%s\n' \
        "$out" "$name" "$(grep -vc '^#' "$out/$name.manifest" || true)" "$name" "$name" "$disk"
}

# Two snapshots compared: the manifests equal, each volume's identity equal, FreeSpace's difference printed, the write
# counters equal; exit 1 otherwise.
compare() {
    local a=build/m4c/$1 b=build/m4c/$2 f ext key before after slice value later
    for f in "$a" "$b"; do
        for ext in manifest volume writes; do
            [ -f "$f.$ext" ] || die "$f.$ext: no such snapshot (tools/m4c-hw.sh snapshot NAME SERIAL writes one)"
        done
    done
    printf 'M4c volume check: snapshot %s against %s\n\n' "$1" "$2"
    if cmp -s "$a.manifest" "$b.manifest"; then
        ok "the manifests are equal: $(grep -vc '^#' "$a.manifest" || true) files, each by its sha256"
    else
        fail "the manifests differ (macOS's own metadata left out of both):"
        diff "$a.manifest" "$b.manifest" | sed 's/^/      /' | head -40 || true
    fi
    for key in VolumeUUID VolumeName FilesystemType TotalSize; do
        before=$(awk -F'\t' -v k="$key" '$2 == k { print $1 "=" $3 }' "$a.volume" | paste -sd ' ' -)
        after=$(awk -F'\t' -v k="$key" '$2 == k { print $1 "=" $3 }' "$b.volume" | paste -sd ' ' -)
        if [ "$before" = "$after" ]; then ok "$key equal: ${before:-none read}"
        else fail "$key differs: ${before:-none read} before, ${after:-none read} after"; fi
    done
    while IFS=$'\t' read -r slice key value; do
        [ "$key" = FreeSpace ] || continue
        later=$(awk -F'\t' -v s="$slice" '$1 == s && $2 == "FreeSpace" { print $3 }' "$b.volume")
        if [[ $value =~ ^[0-9]+$ && $later =~ ^[0-9]+$ ]]; then
            note "$slice FreeSpace: $value before, $later after, a difference of $((later - value)) bytes (macOS's metadata may move it)"
        else
            note "$slice FreeSpace: ${value:-unread} before, ${later:-unread} after"
        fi
    done < "$a.volume"
    if cmp -s "$a.writes" "$b.writes"; then
        ok "the block driver's write counters are equal, no host block write in the window: $(paste -sd ' ' - < "$a.writes")"
    else
        fail "the block driver's write counters moved, macOS wrote to the volume in the window: $(paste -sd ' ' - < "$a.writes") before, $(paste -sd ' ' - < "$b.writes") after"
    fi
    exit "$failed"
}

[ "$(uname -s)" = Darwin ] || die "the kext transport is macOS's"
[ "$(id -u)" -ne 0 ] || die "run it as yourself: it only reads, and prints the commands that need root"
cd "$(git -C "$(dirname "$0")/.." rev-parse --show-toplevel)"

case "${1:-}" in
    disk)            [ $# -eq 2 ] || usage; disk_of "$2" ;;
    snapshot)        [ $# -eq 3 ] || usage; snapshot "$2" "$3" ;;
    compare)         [ $# -eq 3 ] || usage; compare "$2" "$3" ;;
    -h | --help | -*) usage ;;
    *)               [ $# -le 1 ] || usage; preflight "${1:-$default_serial}" ;;
esac
