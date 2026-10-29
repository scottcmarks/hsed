# Developer targets; `make check` is the local form of the CI gates
# (docs/spec-as-source.md section 5.4).

PY      := .venv/bin/python
EXTRACT := $(PY) -I tools/extract/extract_tables.py
CORE    := tcg/TCG_Storage_Architecture_Core_Spec_v2.01_r1.00.pdf
OPAL    := tcg/TCG_Storage-Opal_SSC_v2.30.pdf

.PHONY: check test extract regen-diff m1 venv

## everything CI runs
check: test regen-diff m1

## both Haskell test suites
test:
	stack test

## the Python venv the extraction tools run in
venv: $(PY)
$(PY):
	python3 -m venv .venv
	.venv/bin/pip install --quiet -r tools/extract/requirements.txt

## regenerate the table extractions under spec/ir from the pristine PDFs
extract: $(PY)
	mkdir -p build
	$(EXTRACT) $(CORE) spec/ir/core-2.01.tables.json build/core-index.txt --page-offset 17
	$(EXTRACT) $(OPAL) spec/ir/opal-2.30.tables.json build/opal-index.txt --page-offset 1

## the committed extractions must be what the extractor produces today
regen-diff: extract
	git diff --exit-code --stat spec/ir

## the M1 round trip: annotate, re-extract, generate, check (exit status is the verdict)
m1: $(PY)
	tools/extract/run_all.sh
