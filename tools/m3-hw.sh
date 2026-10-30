#!/bin/bash
# tools/m3-hw.sh: the hardware half of milestone M3 (docs/spec-as-source.md
# section 11): the pre-flight checks anyone may run as themselves, then the
# commands Scott runs, the one privileged run among them.
#
#   tools/m3-hw.sh [DISK [SERIAL]]      (default: disk6 S59BNM0RA15802W)
#
# It runs nothing privileged and nothing that reaches the drive: it reads the
# IORegistry (ioreg), the process table (pgrep), the loaded kexts (kmutil),
# the mount table, and the symbols of build/bin/hsed-jig (nm, which never
# runs it), and prints. Every command below that needs root is printed for
# Scott to run, never run here; hsed-jig repeats each pre-flight itself before
# it opens anything (Hsed.Transport.MacKext), and refuses without root.
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

# The kext, and its build: 625 checks no privilege (design 9.3), so hsed's gates are the only ones.
build=$(kmutil showloaded --list-only --bundle-identifier "$kext" 2>/dev/null | sed -n "s/.*$kext (\([^)]*\)).*/\1/p" | head -1)
if [ -n "$build" ]; then
    ok "the kext $kext is loaded, build $build"
    [ "$build" -ge 626 ] 2>/dev/null || note "build $build predates the kext's privilege check (626): root is hsed's gate alone"
else
    fail "the kext $kext is not loaded"
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

# The volume may stay mounted: Level 0 touches neither the media nor the sync machine (Core 3.3.6.2, PDF 67).
mounted=$(mount | awk -v disk="/dev/$disk" 'index($1, disk) == 1 { print $1 " on " $3 }' | paste -sd ',' -)
[ -z "$mounted" ] || note "mounted: $mounted (it may stay so: Level 0 reads no media)"

printf '\n'
if [ "$failed" -eq 0 ]; then
    printf 'Every pre-flight passes. The commands, for Scott to run in order:\n\n'
else
    printf 'A pre-flight failed: fix it first. The commands, for Scott to run in order:\n\n'
fi

cat <<EOF
1. Build as yourself (never sudo stack): the shim compiles and links against IOKit.
     cd ~/hsed && stack build hsed:exe:hsed-jig --flag hsed:kext --copy-bins --local-bin-path build/bin
2. Run this script again: every pre-flight should pass.
     tools/m3-hw.sh ${disk} ${serial}
3. Optional oracle before the run (never at the same time as hsed-jig):
     sudo sedutil-cli --query /dev/${disk} > build/sedutil-query-before.txt
     diff spec/fixtures/${disk}-query.txt build/sedutil-query-before.txt
4. The drive run: one Level 0 IF-RECV, UCT-01, the capture. Expect exit 0, and on stderr the CDB
   a1 08 0e 01 04 00 01 00 00 5c 00 00, kIOReturnSuccess, 2048 bytes transferred, the sha256 and the
   Length of Parameter Data; the files written stay yours.
     cd ~/hsed && sudo build/bin/hsed-jig --target /dev/${disk} --tests UCT-01 --expect-serial ${serial} \\
         --capture-level0 spec/fixtures/${disk}-level0.bin --report-dir build/report
5. Optional live Pair run, one more Level 0 IF-RECV: expect extra 0x0005 and 0x0404, none missing,
   allowed differences only.
     sudo build/bin/hsed-jig --target sim+/dev/${disk} --tests UCT-01 --profile spec/fixtures/${disk}-level0.bin \\
         --expect-serial ${serial} --report-dir build/report
6. Unprivileged checks: no IF-SEND issued, the decode equal to sedutil's, the replay Pair.
     build/bin/hsed-jig verify-transcript \$(ls -t build/report/jig-dev-${disk}-*.json | head -1) --no-if-send
     build/bin/hsed-jig level0 decode spec/fixtures/${disk}-level0.bin --against spec/fixtures/${disk}-query.txt
     build/bin/hsed-jig --target sim+replay:spec/fixtures/${disk}-level0.bin --tests UCT-01 --profile spec/fixtures/${disk}-level0.bin
7. Optional oracle after the run: the drive is unchanged.
     sudo sedutil-cli --query /dev/${disk} > build/sedutil-query-after.txt
     diff build/sedutil-query-before.txt build/sedutil-query-after.txt
8. Run this script once more: no user client should remain open.
     tools/m3-hw.sh ${disk} ${serial}
9. Commit the fixture (or have an agent do it), then run the tests: FixtureSpec stops pending.
     git add spec/fixtures/${disk}-level0.bin spec/fixtures/${disk}-level0.json
     git commit -m 'Fixture: Level 0 of /dev/${disk} through the kext'
     stack test hsed && tools/land.sh
EOF

exit "$failed"
