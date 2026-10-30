#!/bin/bash
# tools/m3-hw.sh: the hardware half of milestone M3 (docs/spec-as-source.md
# section 11): the pre-flight checks anyone may run as themselves, then the
# commands Scott runs, the one privileged run among them.
#
#   tools/m3-hw.sh [DISK [SERIAL]]      (default: disk6 S59BNM0RA15802W)
#
# It runs nothing privileged and nothing that reaches the drive: it reads the
# IORegistry (ioreg), the process table (pgrep), the loaded kexts (kmutil),
# the installed kext's Info.plist and Mach-O headers and symbols (PlistBuddy,
# otool, nm), the mount table, and the symbols of build/bin/hsed-jig and of
# the hsed-jig stack installed in the shared .stack-work (nm, which never
# runs them), and prints. Every command below that needs root is printed for
# Scott to run, never run here; hsed-jig repeats each pre-flight itself before
# it opens anything (Hsed.Transport.MacKext), and refuses without root.
#
# The kext-enabled hsed-jig is built in a work dir of its own,
# .stack-work-kext (step 1), so that the hsed-jig of the shared .stack-work,
# which `stack exec hsed-jig` and the agents run, stays the flag-off build.
#
# Exit status: 0 when every pre-flight passes, 1 when one fails; the commands
# are printed either way.
set -euo pipefail

disk=${1:-disk6}
serial=${2:-S59BNM0RA15802W}
jig=build/bin/hsed-jig
kext=com.brightplaza.BrightPlazaTPer
failed=0

ok()   { printf 'ok:   %s\n' "$*"; }
fail() { printf 'FAIL: %s\n' "$*"; failed=1; }
note() { printf 'note: %s\n' "$*"; }
die()  { printf 'm3-hw.sh: %s\n' "$*" >&2; exit 2; }

cd "$(git -C "$(dirname "$0")/.." rev-parse --show-toplevel)"
[ "$(id -u)" -ne 0 ] || die "run it as yourself: it only reads, and prints the commands that need root"
[ "$(uname -s)" = Darwin ] || die "the kext transport is macOS's"
[[ $disk =~ ^disk[0-9]+$ ]] || die "$disk: not a whole disk (diskN)"

printf 'M3 hardware pre-flight: /dev/%s, serial %s\n\n' "$disk" "$serial"

# The kext, and what its binary checks (design 9.3, 13.12). Its build number names no source revision
# (macos-driver's SEDBase.xcodeproj says 625 in three configurations and 626 in one at every revision from
# 513e429 to 8bf2635), so the binary itself is read: the loaded kext's UUID, the installed kext whose
# Mach-O carries that UUID, and whether that binary imports IOUserClient::clientHasPrivilege (macos-driver
# 2be586e). Its ATA-command whitelist (switch on cdb[9], macos-driver 7856c9c) shows in no symbol: only a
# disassembly, or a UUID recorded below once one was disassembled, confirms it.
loaded=$(kmutil showloaded --list-only --bundle-identifier "$kext" 2>/dev/null | grep -F " $kext (" | head -1)
build=$(sed -n "s/.*$kext (\([^)]*\)).*/\1/p" <<<"$loaded")
uuid=$(sed -n "s/.*$kext ([^)]*) \([0-9A-F-]\{36\}\).*/\1/p" <<<"$loaded")
# UUIDs read before: arm64e slice of build 625 (2024-05-07), disassembled 2026-10-30, design 13.12
known_uuid() {
    case "$1" in
        373BA509-89F7-3B36-A977-BB65C7635F1B) echo "build 625: no privilege check, no command whitelist (disassembled 2026-10-30)" ;;
        *) echo "" ;;
    esac
}
if [ -z "$build" ]; then
    fail "the kext $kext is not loaded"
else
    ok "the kext $kext is loaded, build $build, UUID ${uuid:-unread}"
    binary=""
    for k in /Library/Extensions/*.kext; do
        [ "$(/usr/libexec/PlistBuddy -c 'Print :CFBundleIdentifier' "$k/Contents/Info.plist" 2>/dev/null)" = "$kext" ] || continue
        exe=$(/usr/libexec/PlistBuddy -c 'Print :CFBundleExecutable' "$k/Contents/Info.plist" 2>/dev/null)
        if [ -n "$uuid" ] && otool -arch all -l "$k/Contents/MacOS/$exe" 2>/dev/null | grep -q "uuid $uuid"; then
            binary="$k/Contents/MacOS/$exe"
        fi
    done
    if [ -z "$binary" ]; then
        note "no installed kext's binary carries the loaded UUID: its checks are unread; treat it as checking nothing"
    elif nm -u "$binary" 2>/dev/null | grep -q 'clientHasPrivilege'; then
        ok "the loaded kext ($binary) checks the caller's privilege (IOUserClient::clientHasPrivilege)"
        note "whether it also refuses ATA commands other than TRUSTED RECEIVE, TRUSTED SEND and IDENTIFY (macos-driver 7856c9c) shows only in a disassembly"
    else
        note "the loaded kext ($binary) imports no IOUserClient::clientHasPrivilege: any local process can open its user client and"
        note "  send any ATA command the bridge translates (A1h), not only IF-SEND; root is hsed's own gate (design 9.3, 13.12)"
    fi
    known=$(known_uuid "$uuid")
    [ -z "$known" ] || note "UUID $uuid: $known"
fi

# The TPer the disk sits on: its interface and its IDENTIFY serial, read from the registry.
tper=$(ioreg -r -c com_brightplaza_BrightPlazaTPer -l -w0 2>/dev/null | awk -v disk="$disk" '
    /^\+-o com_brightplaza_BrightPlazaTPer / { iface = ""; serial = ""; id = $0; sub(/.*, id /, "", id); sub(/,.*/, "", id) }
    /"TPer Interface Type" = / && match($0, /"TPer Interface Type" = "[^"]*"/) {
        iface = substr($0, RSTART + 25, RLENGTH - 26) }
    /"Identify Device Characteristics" = / && match($0, /"Serial Number"="[^"]*"/) {
        serial = substr($0, RSTART + 17, RLENGTH - 18); gsub(/^ +| +$/, "", serial) }
    index($0, "\"BSD Name\" = \"" disk "\"") { printf "%s\t%s\t%s\n", iface, serial, id; exit }')
if [ -z "$tper" ]; then
    fail "no kext TPer has /dev/$disk under it"
else
    IFS=$'\t' read -r iface found id <<<"$tper"
    ok "/dev/$disk is on the kext's TPer, registry id $id"
    if [ "$iface" = SAT ]; then ok "its interface is SAT"; else fail "its interface is '$iface', not SAT"; fi
    if [ "$found" = "$serial" ]; then ok "its IDENTIFY serial is the one expected"
    else fail "its IDENTIFY serial is '$found', not $serial"; fi
fi

# No other opener: the kext allows one, and hsed-jig refuses to share.
clients=$(ioreg -r -c com_brightplaza_BrightPlazaTPerUserClient 2>/dev/null | grep -c '^+-o' || true)
if [ "$clients" -eq 0 ]; then ok "no user client is open on the kext"; else fail "$clients user client(s) open on the kext"; fi
for p in sedutil-cli sedd; do
    if pgrep -x "$p" >/dev/null; then fail "$p is running"; else ok "$p is not running"; fi
done

# The jig: built as Scott, with the kext transport in it (never run here).
if [ ! -x "$jig" ]; then
    fail "$jig is not built (step 1)"
elif ! nm "$jig" 2>/dev/null | grep -q '_hsed_kext_level0$'; then
    fail "$jig has no kext transport: built without --flag hsed:kext, or before the jig opened the kext (step 1)"
elif [ "$(stat -f %u "$jig")" -eq 0 ]; then
    fail "$jig is root's: build as yourself, never with stack under root"
else
    ok "$jig carries the kext transport and is $(stat -f %Su "$jig")'s"
fi

# The shared .stack-work's hsed-jig, which `stack exec hsed-jig` and the agents run: flag-off, no kext code.
shared=0
for b in .stack-work/install/*/*/*/bin/hsed-jig hsed/.stack-work/dist/*/*/build/hsed-jig/hsed-jig; do
    [ -x "$b" ] || continue
    if nm "$b" 2>/dev/null | grep -q '_hsed_kext_'; then
        fail "$b carries the kext transport: rebuild it flag-off (stack build hsed) before anything else runs"
        shared=1
    fi
done
[ "$shared" -ne 0 ] || ok "the shared .stack-work holds no kext-enabled hsed-jig"

# The volume may stay mounted: Level 0 touches neither the media nor the sync machine (Core 3.3.6.2, PDF 67).
# A mount line is "DEVICE on MOUNT POINT (options)": the device exactly the disk or one of its slices, the
# mount point whatever lies between " on " and the last " (", spaces and all.
mounted=$(mount | sed -n -E "s#^(/dev/$disk(s[0-9]+)?) on (.*) \([^()]*\)\$#\1 on \3#p" | paste -sd ',' -)
[ -z "$mounted" ] || note "mounted: $mounted (it may stay so: Level 0 reads no media)"

printf '\n'
if [ "$failed" -eq 0 ]; then
    printf 'Every pre-flight passes. The commands, for Scott to run in order:\n\n'
else
    printf 'A pre-flight failed: fix it first. The commands, for Scott to run in order:\n\n'
fi

cat <<EOF
1. Build as yourself (never sudo stack), in a work dir of its own so that the shared .stack-work keeps the
   flag-off hsed-jig the agents run: the shim compiles and links against IOKit.
     cd ~/hsed && stack --work-dir .stack-work-kext build hsed:exe:hsed-jig --flag hsed:kext --copy-bins --local-bin-path build/bin
2. Run this script again: every pre-flight should pass.
     tools/m3-hw.sh ${disk} ${serial}
3. Optional oracle before the run (never at the same time as hsed-jig):
     sudo sedutil-cli --query /dev/${disk} > build/sedutil-query-before.txt
     diff spec/fixtures/${disk}-query.txt build/sedutil-query-before.txt
4. The drive run: one Level 0 IF-RECV, UCT-01, the capture. Expect exit 0, and on stderr the CDB
   a1 08 0e 01 04 00 01 00 00 5c 00 00, kIOReturnSuccess, 2048 bytes transferred, the sha256 and the
   Length of Parameter Data; the files written stay yours. --expect-serial is required. If it says the kext
   moved fewer bytes than asked, stop there: it captured nothing, its JSON report keeps the bytes, and
   design 13.29 (d) is yours to settle before another run.
     cd ~/hsed && sudo build/bin/hsed-jig --target /dev/${disk} --tests UCT-01 --expect-serial ${serial} \\
         --capture-level0 spec/fixtures/${disk}-level0.bin --report-dir build/report
5. Optional live Pair run, one more Level 0 IF-RECV: expect extra 0x0005 and 0x0404, none missing,
   allowed differences only.
     sudo build/bin/hsed-jig --target sim+/dev/${disk} --tests UCT-01 --profile spec/fixtures/${disk}-level0.bin \\
         --expect-serial ${serial} --report-dir build/report
6. Unprivileged checks: the drive issued Level 0 Discovery's IF-RECV alone, the decode equal to sedutil's,
   the replay Pair.
     build/bin/hsed-jig verify-transcript \$(ls -t build/report/jig-dev-${disk}-*.json | head -1) --no-if-send --level0-only
     build/bin/hsed-jig level0 decode spec/fixtures/${disk}-level0.bin --against spec/fixtures/${disk}-query.txt
     build/bin/hsed-jig --target sim+replay:spec/fixtures/${disk}-level0.bin --tests UCT-01 --profile spec/fixtures/${disk}-level0.bin
7. Optional oracle after the run: the drive is unchanged.
     sudo sedutil-cli --query /dev/${disk} > build/sedutil-query-after.txt
     diff build/sedutil-query-before.txt build/sedutil-query-after.txt
8. Run this script once more: no user client should remain open.
     tools/m3-hw.sh ${disk} ${serial}
9. Test the capture, then commit it (or have an agent do it): FixtureSpec stops pending, and holds the
   sidecar to the .bin. If FixtureSpec fails only on 0x0403 (a descriptor shorter than CNL 1.30's, design
   13.29 (i)), commit nothing and hand it to an agent.
     stack test hsed
     git add spec/fixtures/${disk}-level0.bin spec/fixtures/${disk}-level0.json
     git commit -m 'Fixture: Level 0 of /dev/${disk} through the kext'
     tools/land.sh
EOF

exit "$failed"
