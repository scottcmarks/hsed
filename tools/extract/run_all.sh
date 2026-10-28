#!/bin/bash
# M1 round trip on three Core Spec tables (docs/spec-as-source.md §5.3, §11 M1).
# Run from the hsed checkout root. Writes only under build/m1/.
set -euo pipefail
cd "$(dirname "$0")/../.."
PY=./.venv/bin/python
ORIG=tcg/TCG_Storage_Architecture_Core_Spec_v2.01_r1.00.pdf
IR=spec/ir/core-2.01.tables.json
OUT=build/m1
mkdir -p "$OUT"
$PY -I tools/extract/annotate.py "$ORIG" "$IR" "$OUT"
$PY -I tools/extract/compile.py "$OUT/core_annotated.pdf" "$OUT" --compare-json "$IR"
echo "annotated PDF, reports and generated Haskell are under $OUT"
