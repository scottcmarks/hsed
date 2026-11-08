#!/bin/bash
# tools/jig-runs.sh: the jig's standard runs, on the simulator and on a replay of a Level 0 response alone, each into a
# report directory of its own, their exits and transcript entries in a table: what a milestone compares before and
# after it changes the jig or the simulator (tools/compare_reports.py compares two directories this wrote).
#
#   tools/jig-runs.sh [-w] [-t KEYS] [-r RUNS] OUTDIR
#
#   -w, --ws           each run on the simulator served by hsed-sim over the LAN leg (design 11 M5c): the runs 1-6
#                      and 1m-6m alone (7, 8 or 9 with -w is a usage error), each against a hub of its own (below)
#   -t, --tests KEYS   every run given --tests KEYS (default: none, the jig's own selection)
#   -r, --runs RUNS    the runs to make, comma-separated, in that order (default: all fifteen, 1-9 then 1m-6m; with
#                      -w the twelve, 1-6 then 1m-6m)
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
# With -w a run's hub is the hsed-sim stack installed for this checkout beside the jig (never build/bin), refused as
# the jig is if it carries a _hsed_kext_ symbol, never run as root, started for the run alone with --listen
# 127.0.0.1:0, the run's simulator flags (--units, --sim-sid-pin, --locking-ofs; --hold-processing as --hold-ifrecvs 1;
# --show-msid is the jig's alone), --allow-remote-control and --allow-remote-reset; the port it prints (PORT=<n>, read
# from its stdout within 10 s, refused if it is 8765 or out of range) is the run's target, ws://127.0.0.1:PORT/ws, the
# jig given the same flags, which on a ws:// target assert the hub's setup; then the hub is stopped (SIGTERM, then
# SIGKILL after 10 s) and waited for, as it is by a trap on EXIT, INT and TERM, so that none outlives the script.
#
# Written under OUTDIR, which must be new or empty:
#   runs.tsv      a row a run: run, exit (the jig's), entries (its JSON report's transcript), report (the JSON
#                 report's path under OUTDIR, - if it wrote none) and args (the jig's arguments, --report-dir aside);
#                 with -w, hub_exit too (the hub's exit status once stopped: 0, it closed its peers and exited)
#   runs.info     the jig, its version, the checkout's HEAD, the --tests given, SYN and its SHA-256; with -w the hub
#                 and its version too
#   runN/         run N's reports (jig-*.json, .md and .xml), and its stdout.txt and stderr.txt; with -w the hub's
#                 hub-stdout.txt and hub-stderr.txt too
#
# Only the simulator and replay targets, ever: no run names a /dev target, and each run's target is checked to be
# sim, replay:FILE or sim+replay:FILE, FILE a regular file outside /dev (with -w, ws://127.0.0.1:PORT/ws, PORT the
# run's hub's), before the jig is started. The jig is the
# hsed-jig stack installed for this checkout (stack path --local-install-root; stack build hsed, the kext flag off),
# never build/bin/hsed-jig: refused if it carries a _hsed_kext_ symbol (nm, as tools/m4c-hw.sh checks it), and
# never run as root. It is not built here: the runs say which build they ran (its version, in runs.info and in every
# report), and a jig built from another commit than HEAD is noted. Run from anywhere in the checkout; where builds
# share a lock, under it, as the agents run every stack command: lockf -k ~/hsed/.claude/stack.lock
# tools/jig-runs.sh OUTDIR.
#
# Exit status: 0 every run wrote its reports, whatever its own exit (with -w, and every hub exited 0 once stopped); 1 a
# run wrote none, or its JSON report's exit is not the jig's, or (-w) a hub did not print its port in time, printed
# one refused, or did not exit 0 once stopped; 2 a usage error (OUTDIR not empty, an unknown run, a run 7, 8 or 9
# with -w, no jig or no hsed-sim, either with the kext transport).
set -euo pipefail
shopt -s nullglob

die()  { printf 'jig-runs.sh: %s\n' "$*" >&2; exit 2; }
fail() { printf 'jig-runs.sh: %s\n' "$*" >&2; exit 1; }
note() { printf 'jig-runs.sh: %s\n' "$*" >&2; }

syn=build/jig-runs/disk6-synth.bin
query=spec/fixtures/disk6-query.txt
fixture=spec/fixtures/disk6-level0.bin
m4c_sha=0cdb6d4bd71f985e5b65f53d3e6910ae4b549e277af850dbafe0b7d7744a25be
all_runs=1,2,3,4,5,6,7,8,9,1m,2m,3m,4m,5m,6m
ws_runs=1,2,3,4,5,6,1m,2m,3m,4m,5m,6m

tests=''; runs=''; ws=''
while [ $# -gt 0 ]; do
    case "$1" in
        -w|--ws)    ws=1 ;;
        -t|--tests) [ $# -ge 2 ] || die "$1 needs a list of case keys"; tests=$2; shift ;;
        -r|--runs)  [ $# -ge 2 ] || die "$1 needs a list of runs"; runs=$2; shift ;;
        --)         shift; break ;;
        -*)         die "unknown option $1" ;;
        *)          break ;;
    esac
    shift
done
[ $# -eq 1 ] || die "usage: tools/jig-runs.sh [-w] [-t KEYS] [-r RUNS] OUTDIR"
[ "$(id -u)" != 0 ] || die "never as root: the runs need no privilege, and a report written as root is not yours"
[ -n "$runs" ] || if [ -n "$ws" ]; then runs=$ws_runs; else runs=$all_runs; fi

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
if [ -n "$ws" ]; then
    for name in "${names[@]}"; do
        case "$name" in
            7|8|9) die "-w: run $name's target is a replay (runs 7, 8 and 9 are not the simulator alone): -w makes $ws_runs alone" ;;
        esac
    done
fi
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

# With -w, the hub: the checkout's flag-off hsed-sim beside the jig, never build/bin's.
if [ -n "$ws" ]; then
    sim=$install/bin/hsed-sim
    case "$sim" in */build/bin/*) die "$sim: build/bin is not this checkout's flag-off build, never run here" ;; esac
    [ -x "$sim" ] || die "$sim: no hsed-sim built (stack build hsed)"
    sim_symbols=$(nm "$sim" 2>/dev/null) || die "nm could not read $sim"
    if grep -q '_hsed_kext_' <<<"$sim_symbols"; then
        die "$sim carries the kext transport (_hsed_kext_ symbols): rebuild it flag-off (stack build hsed)"
    fi
    sim_version=$("$sim" --version) || die "$sim --version failed"
    case "$sim_version" in
        *" ${head:0:7}"*) ;;
        *) note "the hub is $sim_version, built from another commit than HEAD ${head:0:7}: stack build hsed for this tree's" ;;
    esac
fi

# The run's hub, while one runs: stopped (SIGTERM, SIGKILL after 10 s) and waited for, its exit status in hub_exit,
# by the run once its jig is done and by a trap on EXIT, INT and TERM, so that no hub outlives the script.
hub_pid=''; hub_exit=-
alive() {   # whether a process is there and not a zombie
    local stat
    stat=$(ps -o stat= -p "$1" 2>/dev/null) || return 1
    [ -n "$stat" ] && [ "${stat#Z}" = "$stat" ]
}
stop_hub() {
    [ -n "$hub_pid" ] || return 0
    local pid=$hub_pid tries=0
    hub_pid=''
    kill -TERM "$pid" 2>/dev/null || true
    while alive "$pid" && [ "$tries" -lt 100 ]; do sleep 0.1; tries=$((tries + 1)); done
    if alive "$pid"; then kill -KILL "$pid" 2>/dev/null || true; fi
    hub_exit=0
    wait "$pid" 2>/dev/null || hub_exit=$?
}
trap 'stop_hub' EXIT
trap 'stop_hub; exit 130' INT
trap 'stop_hub; exit 143' TERM

# Start a run's hub with its flags, its output into DIR; the port it listens on (PORT=<n>, a whole line, within 10 s),
# or fail, the hub stopped.
start_hub() {
    local dir=$1 line='' tries=0
    shift
    "$sim" --listen 127.0.0.1:0 "$@" --allow-remote-control --allow-remote-reset >"$dir/hub-stdout.txt" 2>"$dir/hub-stderr.txt" &
    hub_pid=$!
    while [ "$tries" -lt 100 ]; do
        if [ "$(wc -l <"$dir/hub-stdout.txt")" -ge 1 ]; then line=$(head -n 1 "$dir/hub-stdout.txt"); break; fi
        alive "$hub_pid" || break
        sleep 0.1; tries=$((tries + 1))
    done
    port=${line#PORT=}
    if [ -z "$line" ] || [ "$port" = "$line" ]; then
        stop_hub; fail "the hub printed no PORT=<n> line within 10 s (exit $hub_exit): $dir/hub-stderr.txt"
    fi
    if ! [[ "$port" =~ ^[1-9][0-9]{0,4}$ ]] || [ "$port" -gt 65535 ]; then
        stop_hub; fail "the hub printed $line: not a port of 1 to 65535"
    fi
    if [ "$port" = 8765 ]; then
        stop_hub; fail "the hub printed $line: 8765 is MHTX's own, never a test's (D6)"
    fi
}

# A run's hub flags, from its jig flags, into hub_args: the simulator's as they are, --hold-processing as
# --hold-ifrecvs 1, the jig's own (--target, --tests, --show-msid) left out.
hub_flags_from() {
    local arg value=''
    hub_args=()
    for arg in "$@"; do
        if [ -n "$value" ]; then
            [ "$value" = drop ] || hub_args+=("$arg")
            value=''
            continue
        fi
        case "$arg" in
            --target|--tests)                    value=drop ;;
            --show-msid)                         ;;
            --hold-processing)                   hub_args+=(--hold-ifrecvs 1) ;;
            --units|--sim-sid-pin|--locking-ofs) hub_args+=("$arg"); value=keep ;;
            *)                                   die "$arg: no hub flag stands for it" ;;
        esac
    done
}

mkdir -p "$(dirname "$syn")"
"$jig" level0 synthesize "$query" "$syn" >"$out/synthesize.txt" 2>&1 || die "level0 synthesize failed: $out/synthesize.txt"
sha=$(shasum -a 256 "$syn" | cut -d' ' -f1)
[ "$sha" = "$m4c_sha" ] || note "$syn's SHA-256 is $sha, not M4c's $m4c_sha"
printf 'jig\t%s\nversion\t%s\nhead\t%s\ntests\t%s\nreplay\t%s\nreplay_sha256\t%s\n' \
    "$jig" "$version" "$head" "${tests:--}" "$syn" "$sha" >"$out/runs.info"
[ -z "$ws" ] || printf 'hub\t%s\nhub_version\t%s\n' "$sim" "$sim_version" >>"$out/runs.info"

if [ -n "$ws" ]; then
    printf 'run\texit\tentries\treport\targs\thub_exit\n' >"$out/runs.tsv"
else
    printf 'run\texit\tentries\treport\targs\n' >"$out/runs.tsv"
fi
failed=0
for name in "${names[@]}"; do
    read -r -a args <<<"$(flags_of "$name")"
    [ -z "$tests" ] || args+=(--tests "$tests")
    [ "${args[0]}" = --target ] && simulated_or_replayed "${args[1]}" \
        || die "run $name: target ${args[1]} is not the simulator or a replay of a file: refused"
    dir=$out/run$name
    mkdir "$dir"
    if [ -n "$ws" ]; then
        [ "${args[1]}" = sim ] || die "run $name: -w makes runs on the simulator alone, not ${args[1]}"
        hub_flags_from "${args[@]}"
        start_hub "$dir" ${hub_args[@]+"${hub_args[@]}"}
        args[1]=ws://127.0.0.1:$port/ws
        [[ "${args[1]}" =~ ^ws://127\.0\.0\.1:[1-9][0-9]*/ws$ ]] || die "run $name: target ${args[1]} is not this run's hub: refused"
    fi
    rc=0
    "$jig" run "${args[@]}" --report-dir "$dir" >"$dir/stdout.txt" 2>"$dir/stderr.txt" || rc=$?
    if [ -n "$ws" ]; then
        stop_hub
        [ "$hub_exit" = 0 ] || { note "run $name: its hub exited $hub_exit once stopped: $dir/hub-stderr.txt"; failed=1; }
    fi
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
    if [ -n "$ws" ]; then
        printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$name" "$rc" "$entries" "$report" "${args[*]}" "$hub_exit" >>"$out/runs.tsv"
    else
        printf '%s\t%s\t%s\t%s\t%s\n' "$name" "$rc" "$entries" "$report" "${args[*]}" >>"$out/runs.tsv"
    fi
done

printf 'jig: %s\nreplay: %s %s\n' "$version" "$sha" "$syn"
[ -z "$ws" ] || printf 'hub: %s\n' "$sim_version"
column -t -s $'\t' "$out/runs.tsv"
exit "$failed"
