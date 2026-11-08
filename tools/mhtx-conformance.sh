#!/bin/bash
# tools/mhtx-conformance.sh: Scott's step of milestone M5c (docs/spec-as-source.md section 11 M5c; Scott's answer Q2):
# mh_transport's Dart conformance suite against hsed-sim, run from a throwaway copy of ~/mh_transport, after the agents'
# Haskell checks (hsed-lan-test's ConformanceSpec) pass.
#
#   tools/mhtx-conformance.sh       the recipe, this checkout's directory filled in
#
# It prints the commands and runs none of them, as tools/m4c-hw.sh prints its steps: no hsed-sim, no dart, no stack, no
# copy; it finds only its own checkout's directory (cd and pwd). The suite runs from a copy, so that nothing in
# ~/mh_transport changes, not even its ignored .dart_tool and pubspec.lock (dart run writes both). Read the printed last
# line, not the exit status: a run with skipped checks also exits 0 (bin/conformance.dart:85, conformance.dart:53).
#
# Exit status: 0; 2 a usage error.
set -euo pipefail

if [[ $# -ne 0 ]]; then
    printf 'mhtx-conformance.sh: usage: tools/mhtx-conformance.sh (it takes no argument, and runs nothing)\n' >&2
    exit 2
fi

case ${BASH_SOURCE[0]} in
    */*) here=${BASH_SOURCE[0]%/*} ;;
    *)   here=. ;;
esac
checkout=$(cd "$here/.." && pwd)

cat <<'EOF'
M5c: mh_transport's Dart conformance suite against hsed-sim (Scott's step; Scott's answer Q2). Nothing below has run:
type each command yourself.

1. In terminal 1, in this checkout, hsed-sim as stack installed it, its conformance hooks on, on 127.0.0.1 and an
   ephemeral port, at the suite's heartbeat of 200 ms and timeout of 600 ms:

EOF
printf '     cd %s\n' "$checkout"
cat <<'EOF'
     "$(stack path --local-install-root)/bin/hsed-sim" --conformance-hooks --listen 127.0.0.1:0 --heartbeat-interval-ms 200 --dead-timeout-ms 600

   Its first stdout line is PORT=<n>; put that <n> in the commands below.

2. In terminal 2, the suite from a throwaway copy of ~/mh_transport, so that nothing in it changes (dart run writes its
   ignored .dart_tool and pubspec.lock):

     d=$(mktemp -d) && cp -Rp ~/mh_transport "$d/" && cd "$d/mh_transport/packages/mh_transport"
     dart run mh_transport:conformance ws://127.0.0.1:<n>/ws --emit http://127.0.0.1:<n>/emit --count http://127.0.0.1:<n>/clients --interval-ms 200 --timeout-ms 600

3. Read its output, not its exit status (a run with skipped checks exits 0 too: bin/conformance.dart:85,
   conformance.dart:53). Expect eight PASS lines, among them
     PASS broadcast reaches every client with the next seq: two clients, seq 1 then 2
     PASS silent client is dropped, pinging client is kept: silent closed (code 4000) within 800ms, pinging client fine
     PASS client judges missed broadcasts from hello: unknown, then some, then none
   and the last line
     CONFORMANT: 8 passed, 0 failed, 0 skipped

4. Stop hsed-sim with Ctrl-C in terminal 1: it closes its clients with 1001 and exits 0. Then

     lsof -nP -iTCP:<n> -sTCP:LISTEN

   prints nothing. Paste the suite's nine lines to the lead, who records them in design section 14 as M5c's Dart
   evidence. The copy under $d is a throwaway, yours to remove when you are done with it.
EOF
