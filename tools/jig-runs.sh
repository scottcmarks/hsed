#!/bin/bash
# tools/jig-runs.sh: the jig's standard runs, on the simulator and on a replay of a Level 0 response alone, each into a
# report directory of its own, their exits and transcript entries in a table: what a milestone compares before and
# after it changes the jig or the simulator (tools/compare_reports.py compares two directories this wrote).
#
#   tools/jig-runs.sh [-t KEYS] [-r RUNS] OUTDIR
#
#   -t, --tests KEYS   every run given --tests KEYS (default: none, the jig's own selection)
#   -r, --runs RUNS    the runs to make, comma-separated, in that order (default: all fifteen, 1-9 then 1m-6m)
#
# The runs (the record since M4a, design 11 and 14):
#   1   --target sim
#   2   --target sim --hold-processing
#   3   --target sim --units blocks512
#   4   --target sim --units blocks512 --hold-processing
#   5   --target sim --show-msid
#   6   --target sim --sim-sid-pin 53494421
#   7   --target replay:SYN
#   8   --target sim+replay:SYN
#   9   --target sim+replay:spec/fixtures/disk6-level0.bin --profile spec/fixtures/disk6-level0.bin
#   1m-6m   runs 1-6 with --locking-ofs manufactured
# SYN is build/jig-runs/disk6-synth.bin, the Level 0 response `hsed-jig level0 synthesize` renders from
# spec/fixtures/disk6-query.txt, made again by each invocation (it is deterministic) and never committed; its SHA-256
# is in OUTDIR/runs.info, and M4c's was 0cdb6d4bd71f985e5b65f53d3e6910ae4b549e277af850dbafe0b7d7744a25be.
#
# Written under OUTDIR, which must be new or empty:
#   runs.tsv      a row a run: run, exit (the jig's), entries (its JSON report's transcript), report (the JSON
#                 report's path under OUTDIR, - if it wrote none) and args (the jig's arguments, --report-dir aside)
#   runs.info     the jig, its version, the checkout's HEAD, the --tests given, SYN and its SHA-256
#   runN/         run N's reports (jig-*.json, .md and .xml), and its stdout.txt and stderr.txt
#
# Only the simulator and replay targets, ever: no run names a /dev target, and each run's target is checked to be
# sim, replay:FILE or sim+replay:FILE, FILE a regular file outside /dev, before the jig is started. The jig is the
# hsed-jig stack installed for this checkout (stack path --local-install-root; stack build hsed, the kext flag off),
# never build/bin/hsed-jig: refused if it carries a _hsed_kext_ symbol (nm, as tools/m4c-hw.sh checks it), and
# never run as root. It is not built here: the runs say which build they ran (its version, in runs.info and in every
# report), and a jig built from another commit than HEAD is noted. Run from anywhere in the checkout; where builds
# share a lock, under it, as the agents run every stack command: lockf -k ~/hsed/.claude/stack.lock
# tools/jig-runs.sh OUTDIR.
#
# Exit status: 0 every run wrote its reports, whatever its own exit; 1 a run wrote none, or its JSON report's exit
# is not the jig's; 2 a usage error (OUTDIR not empty, an unknown run, no jig, a jig with the kext transport).
set -euo pipefail
shopt -s nullglob

die()  { printf 'jig-runs.sh: %s\n' "$*" >&2; exit 2; }
note() { printf 'jig-runs.sh: %s\n' "$*" >&2; }

syn=build/jig-runs/disk6-synth.bin
query=spec/fixtures/disk6-query.txt
fixture=spec/fixtures/disk6-level0.bin
m4c_sha=0cdb6d4bd71f985e5b65f53d3e6910ae4b549e277af850dbafe0b7d7744a25be
all_runs=1,2,3,4,5,6,7,8,9,1m,2m,3m,4m,5m,6m

tests=''; runs=$all_runs
while [ $# -gt 0 ]; do
    case "$1" in
        -t|--tests) [ $# -ge 2 ] || die "$1 needs a list of case keys"; tests=$2; shift ;;
        -r|--runs)  [ $# -ge 2 ] || die "$1 needs a list of runs"; runs=$2; shift ;;
        --)         shift; break ;;
        -*)         die "unknown option $1" ;;
        *)          break ;;
    esac
    shift
done
[ $# -eq 1 ] || die "usage: tools/jig-runs.sh [-t KEYS] [-r RUNS] OUTDIR"
[ "$(id -u)" != 0 ] || die "never as root: the runs need no privilege, and a report written as root is not yours"

# A run's arguments, the target first.
flags_of() {
    case "$1" in
        1) echo "--target sim" ;;
        2) echo "--target sim --hold-processing" ;;
        3) echo "--target sim --units blocks512" ;;
        4) echo "--target sim --units blocks512 --hold-processing" ;;
        5) echo "--target sim --show-msid" ;;
        6) echo "--target sim --sim-sid-pin 53494421" ;;
        7) echo "--target replay:$syn" ;;
        8) echo "--target sim+replay:$syn" ;;
        9) echo "--target sim+replay:$fixture --profile $fixture" ;;
        [1-6]m) echo "$(flags_of "${1%m}") --locking-ofs manufactured" ;;
        *) return 1 ;;
    esac
}

# Whether a target is the simulator, a replay of a regular file outside /dev, or the pair of the two.
simulated_or_replayed() {
    local file
    case "$1" in
        sim)           return 0 ;;
        replay:*)      file=${1#replay:} ;;
        sim+replay:*)  file=${1#sim+replay:} ;;
        *)             return 1 ;;
    esac
    case "$file" in /dev|/dev/*) return 1 ;; esac
    [ -f "$file" ] && [ ! -b "$file" ] && [ ! -c "$file" ]
}

case ",$runs," in *,,*) die "--runs names an empty run (a leading, trailing or doubled comma)" ;; esac
IFS=, read -r -a names <<<"$runs"
[ ${#names[@]} -gt 0 ] || die "--runs names no run"
for name in "${names[@]}"; do flags_of "$name" >/dev/null || die "unknown run '$name' (runs: $all_runs)"; done
twice=$(printf '%s\n' "${names[@]}" | sort | uniq -d | paste -sd, -)
[ -z "$twice" ] || die "--runs names $twice more than once"

out=$1
mkdir -p "$out"
out=$(cd "$out" && pwd)
[ -z "$(ls -A "$out")" ] || die "$out is not empty: give each set of runs a directory of its own"

cd "$(git -C "$(dirname "$0")/.." rev-parse --show-toplevel)"
py=.venv/bin/python
[ -x "$py" ] || die "no $py: make venv, or link a checkout's .venv"

# The jig: the checkout's flag-off build, never build/bin/hsed-jig.
install=$(stack path --local-install-root 2>/dev/null) || die "stack path failed: is stack set up for this checkout?"
jig=$install/bin/hsed-jig
case "$jig" in */build/bin/*) die "$jig: build/bin/hsed-jig is the kext-enabled jig, never run here" ;; esac
[ -x "$jig" ] || die "$jig: no hsed-jig built (stack build hsed)"
symbols=$(nm "$jig" 2>/dev/null) || die "nm could not read $jig"
if grep -q '_hsed_kext_' <<<"$symbols"; then
    die "$jig carries the kext transport (_hsed_kext_ symbols): rebuild it flag-off (stack build hsed)"
fi
version=$("$jig" --version)
head=$(git rev-parse HEAD)
case "$version" in
    *" ${head:0:7}"*) ;;
    *) note "the jig is $version, built from another commit than HEAD ${head:0:7}: stack build hsed for this tree's" ;;
esac

mkdir -p "$(dirname "$syn")"
"$jig" level0 synthesize "$query" "$syn" >"$out/synthesize.txt" 2>&1 || die "level0 synthesize failed: $out/synthesize.txt"
sha=$(shasum -a 256 "$syn" | cut -d' ' -f1)
[ "$sha" = "$m4c_sha" ] || note "$syn's SHA-256 is $sha, not M4c's $m4c_sha"
printf 'jig\t%s\nversion\t%s\nhead\t%s\ntests\t%s\nreplay\t%s\nreplay_sha256\t%s\n' \
    "$jig" "$version" "$head" "${tests:--}" "$syn" "$sha" >"$out/runs.info"

printf 'run\texit\tentries\treport\targs\n' >"$out/runs.tsv"
failed=0
for name in "${names[@]}"; do
    read -r -a args <<<"$(flags_of "$name")"
    [ -z "$tests" ] || args+=(--tests "$tests")
    [ "${args[0]}" = --target ] && simulated_or_replayed "${args[1]}" \
        || die "run $name: target ${args[1]} is not the simulator or a replay of a file: refused"
    dir=$out/run$name
    mkdir "$dir"
    rc=0
    "$jig" run "${args[@]}" --report-dir "$dir" >"$dir/stdout.txt" 2>"$dir/stderr.txt" || rc=$?
    reports=("$dir"/jig-*.json)
    if [ ${#reports[@]} -ne 1 ]; then
        note "run $name (exit $rc) wrote ${#reports[@]} JSON reports, not one: $dir/stderr.txt"
        failed=1; report=-; entries=-
    else
        report=${reports[0]#"$out"/}
        if counts=$("$py" -I -c 'import json, sys
r = json.load(open(sys.argv[1], encoding="utf-8"))
print(len(r["transcript"]), r["exit"])' "${reports[0]}") && read -r entries said <<<"$counts"; then
            [ "$said" = "$rc" ] || { note "run $name: its report gives exit $said, the jig exited $rc"; failed=1; }
        else
            note "run $name: ${reports[0]} gives no transcript and exit"; failed=1; entries=-
        fi
    fi
    printf '%s\t%s\t%s\t%s\t%s\n' "$name" "$rc" "$entries" "$report" "${args[*]}" >>"$out/runs.tsv"
done

printf 'jig: %s\nreplay: %s %s\n' "$version" "$sha" "$syn"
column -t -s $'\t' "$out/runs.tsv"
exit "$failed"
