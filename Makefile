# Developer targets; `make check` is the local form of the CI gates
# (docs/spec-as-source.md section 5.4).

# the interpreter `make venv` builds .venv from: pikepdf and pillow need Python >= 3.11
PYTHON3 ?= python3
PY      := .venv/bin/python
# the venv with tools/extract/requirements.txt installed; remade when the requirements change
VENV    := build/venv.stamp
EXTRACT := $(PY) -I tools/extract/extract_tables.py
CORE    := tcg/TCG_Storage_Architecture_Core_Spec_v2.01_r1.00.pdf
OPAL    := tcg/TCG_Storage-Opal_SSC_v2.30.pdf
# the annotation round trip of docs/spec-as-source.md section 5.3 (M1: Core only)
ANNOT   := spec/annot
CORE_ANNOT_ARGS := --spec core-2.01 --pristine $(CORE) --ir spec/ir/core-2.01.json --hsa $(ANNOT)/core-2.01.hsa \
                   --out build/core-2.01.hsed.pdf --upd $(ANNOT)/core-2.01.upd --manifest $(ANNOT)/core-2.01.manifest.json

# the hsedc side of the pipeline (docs/spec-as-source.md section 5.2): the annotated PDF extracted,
# then bind, check and compile; what it writes under hsed-spec/bindings and hsed/gen is committed, and
# so is its ledger (docs/LEDGER.md: the binding and finding totals the tests do not pin)
HSEDC       := stack exec -- hsedc
CORE_IR     := build/core-2.01.hsed.json
BINDINGS    := hsed-spec/bindings/Hsed/Bindings/Core.hs
BIND_CMD    := $(HSEDC) bind $(CORE_IR) -o $(BINDINGS)
CHECK_CMD   := $(HSEDC) check $(CORE_IR) --errata scott
COMPILE_CMD := $(HSEDC) compile $(CORE_IR) --errata scott --target core -o hsed/gen --report build/report \
               && cp build/report/ledger.md docs/LEDGER.md

.PHONY: check test extract extract-test regen-diff annotate pdf-checks venv \
        hsedc ir-annotated bind hsedc-check compile gen gen-diff splices

# The targets share files (spec/ir is rewritten by extract and read by annotate; build/*.hsed.* by
# annotate, ir-annotated and the hsedc targets) and stack takes a lock: run them one at a time
# whatever -j says.
.NOTPARALLEL:

## everything CI runs (gen-diff before test, so the suites compile what gen wrote)
check: extract-test regen-diff annotate pdf-checks gen-diff test

## both Haskell test suites
test:
	stack test

## the Python venv the extraction tools run in: made with $(PYTHON3) (refused below 3.11), and the
## pinned wheels reinstalled whenever tools/extract/requirements.txt changes
venv: $(VENV)
$(PY):
	@$(PYTHON3) -c 'import sys; sys.exit(sys.version_info < (3, 11))' || \
	  { echo "venv: $(PYTHON3) is $$($(PYTHON3) --version 2>&1); the extraction wheels need Python >= 3.11 (make venv PYTHON3=python3.12)"; exit 1; }
	$(PYTHON3) -m venv .venv
$(VENV): tools/extract/requirements.txt $(PY)
	mkdir -p build
	$(PY) -m pip install --quiet -r tools/extract/requirements.txt
	touch $@

## regenerate the IR v1 extractions under spec/ir from the pristine PDFs (docs/ir-v1.md);
## the extractor's sanity invariants (design 5.1 step 9) make these fail on a FAIL
extract: $(VENV)
	mkdir -p build
	$(EXTRACT) $(CORE) spec/ir/core-2.01.json build/core-index.txt --spec core-2.01 --page-offset 17
	$(EXTRACT) $(OPAL) spec/ir/opal-2.30.json build/opal-index.txt --spec opal-2.30 --page-offset 1 --check-against spec/ir/core-2.01.json

## unit tests of the extractor's pure rules (ragged join, section numbers, item labels, hashing,
## paragraph characters, signature blocks, table references, List of Tables entries)
extract-test: $(VENV)
	$(PY) -I -m unittest discover -s tools/extract -p 'test_*.py'

## the committed extractions must be what the extractor produces today
regen-diff: extract
	git diff --exit-code --stat spec/ir

## build/core-2.01.hsed.pdf from the committed sidecar: pristine + ONE incremental section;
## rewrites the committed .upd and manifest (which must then diff clean, see pdf-checks)
annotate: $(VENV)
	mkdir -p build
	$(PY) -I tools/extract/annotate.py $(CORE_ANNOT_ARGS)

## the PDF gates (design 5.4 c-e): byte prefix, pristine + upd == annotated, tail budget, identical
## renders (gs, MuPDF, sips where present), three-reader read-back, dump(apply(upd)) == hsa,
## annotate(hsa) == upd; then the committed .upd and manifest must be what annotate produced
pdf-checks: annotate
	$(PY) -I tools/extract/pdf_checks.py $(CORE_ANNOT_ARGS) --work build/pdf-checks
	git diff --exit-code --stat $(ANNOT)

## the spec compiler
hsedc:
	stack build hsed-spec:exe:hsedc

## build/core-2.01.hsed.json: the annotated PDF extracted (pristine + the committed annotations); it
## differs from spec/ir/core-2.01.json in its annots alone, which is what hsedc reads
ir-annotated: annotate
	$(EXTRACT) build/core-2.01.hsed.pdf $(CORE_IR) build/core-2.01.hsed-index.txt --spec core-2.01 --page-offset 17 --pristine $(CORE)

## hsed-spec/bindings/Hsed/Bindings/Core.hs from the annotations of build/core-2.01.hsed.json (made
## afresh from the committed sidecar); only the hsed-spec:bindings test component compiles it, and
## an Error leaves it as it was
bind: ir-annotated hsedc
	$(BIND_CMD)

## classify every numbered table and run the checks under Scott's errata (non-zero exit on an Error)
hsedc-check: ir-annotated hsedc
	$(CHECK_CMD)

## hsed/gen/Hsed/Gen/Core/*.hs (any other module there removed), build/report/{findings,coverage,ERRATA,ledger}.md
## and docs/LEDGER.md, the committed copy of the ledger (nothing written on an Error)
compile: ir-annotated hsedc
	$(COMPILE_CMD)

## the whole chain, in order: annotate -> ir-annotated -> bind -> hsedc-check -> compile
gen: ir-annotated hsedc
	$(BIND_CMD)
	$(CHECK_CMD)
	$(COMPILE_CMD)

## the committed bindings, generated code and ledger must be what gen produces today (changed, removed
## or new files)
gen-diff: gen
	git diff --exit-code --stat hsed-spec/bindings hsed/gen docs/LEDGER.md
	@new=$$(git status --porcelain --untracked-files=all -- hsed-spec/bindings hsed/gen docs/LEDGER.md); \
	  if [ -n "$$new" ]; then echo "$$new"; echo "gen-diff: generated files are not committed"; exit 1; fi

## dev/TableUIDs.txt: GHCi's -ddump-splices transcript of hsed's hand-written TableUIDs, the
## definitions the t240 quasiquoter makes (design 5.5: secondary to the runtime oracle, regenerated
## at M2a); paths made relative to the checkout. Not part of check.
splices:
	printf ':set -ddump-splices\n:load System.SED.MCTP.Common.TableUIDs\n' \
	  | stack ghci hsed:lib --no-load --ghci-options='-ignore-dot-ghci -hide-package gdp' 2>&1 \
	  | sed -e 's|$(CURDIR)/||g' > dev/TableUIDs.txt
