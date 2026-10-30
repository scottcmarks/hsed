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
# the annotation round trip of docs/spec-as-source.md section 5.3, for each spec (Core since M1, Opal since
# M3); pdf-checks renders Opal's pages 1, 20 and 23 besides its anchors' (its default pages are Core's)
ANNOT   := spec/annot
CORE_ANNOT_ARGS := --spec core-2.01 --pristine $(CORE) --ir spec/ir/core-2.01.json --hsa $(ANNOT)/core-2.01.hsa \
                   --out build/core-2.01.hsed.pdf --upd $(ANNOT)/core-2.01.upd --manifest $(ANNOT)/core-2.01.manifest.json
OPAL_ANNOT_ARGS := --spec opal-2.30 --pristine $(OPAL) --ir spec/ir/opal-2.30.json --hsa $(ANNOT)/opal-2.30.hsa \
                   --out build/opal-2.30.hsed.pdf --upd $(ANNOT)/opal-2.30.upd --manifest $(ANNOT)/opal-2.30.manifest.json
OPAL_PAGES      := 1,20,23

# the hsedc side of the pipeline (docs/spec-as-source.md section 5.2): each annotated PDF extracted,
# then bind, check and compile; what it writes under hsed-spec/bindings and hsed/gen is committed, and
# so are its ledgers (docs/LEDGER.md for Core, docs/LEDGER-opal.md for Opal: the binding and finding
# totals the tests do not pin). Opal is read over Core, Core's Context its base (--base, design 13.26
# (g)), and annotated in part until M5, its units nothing binds yet Warnings (--partial, 13.26 (k)).
HSEDC       := stack exec -- hsedc
CORE_IR     := build/core-2.01.hsed.json
OPAL_IR     := build/opal-2.30.hsed.json
BINDINGS    := hsed-spec/bindings/Hsed/Bindings
OPAL_RUN    := --base $(CORE_IR) --partial opal-2.30
BIND_CMD    := $(HSEDC) bind $(CORE_IR) -o $(BINDINGS)/Core.hs \
               && $(HSEDC) bind $(OPAL_IR) $(OPAL_RUN) -o $(BINDINGS)/Opal.hs
CHECK_CMD   := $(HSEDC) check $(CORE_IR) --errata scott \
               && $(HSEDC) check $(OPAL_IR) $(OPAL_RUN) --errata scott
COMPILE_CMD := $(HSEDC) compile $(CORE_IR) --errata scott --target core -o hsed/gen --report build/report \
               && cp build/report/ledger.md docs/LEDGER.md \
               && $(HSEDC) compile $(OPAL_IR) $(OPAL_RUN) --errata scott --target opal230 -o hsed/gen --report build/report/opal \
               && cp build/report/opal/ledger.md docs/LEDGER-opal.md
# what gen writes and gen-diff holds to the committed files
GENERATED   := hsed-spec/bindings hsed/gen docs/LEDGER.md docs/LEDGER-opal.md

.PHONY: check test werror extract extract-test regen-diff annotate pdf-checks venv \
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

## CI's test command (gate a: every package's warnings errors) in a work dir of its own,
## .stack-work-werror, which no plain build fills: in .stack-work a module a plain build compiled
## is not compiled again for -Werror alone (design section 12). Not part of check.
werror:
	stack --work-dir .stack-work-werror --system-ghc --no-install-ghc test --ghc-options=-Werror

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
## paragraph characters, signature blocks, table references, List of Tables entries, byte-grid extents,
## word-complete sections)
extract-test: $(VENV)
	$(PY) -I -m unittest discover -s tools/extract -p 'test_*.py'

## the committed extractions must be what the extractor produces today
regen-diff: extract
	git diff --exit-code --stat spec/ir

## build/<spec>.hsed.pdf from each spec's committed sidecar: pristine + ONE incremental section;
## rewrites the committed .upd and manifest (which must then diff clean, see pdf-checks)
annotate: $(VENV)
	mkdir -p build
	$(PY) -I tools/extract/annotate.py $(CORE_ANNOT_ARGS)
	$(PY) -I tools/extract/annotate.py $(OPAL_ANNOT_ARGS)

## the PDF gates (design 5.4 c-e): byte prefix, pristine + upd == annotated, tail budget, identical
## renders (gs, MuPDF, sips where present), three-reader read-back, dump(apply(upd)) == hsa,
## annotate(hsa) == upd, for each spec (its work dir build/pdf-checks/<spec>); then the committed .upd
## and manifest must be what annotate produced
pdf-checks: annotate
	$(PY) -I tools/extract/pdf_checks.py $(CORE_ANNOT_ARGS) --work build/pdf-checks/core-2.01
	$(PY) -I tools/extract/pdf_checks.py $(OPAL_ANNOT_ARGS) --pages $(OPAL_PAGES) --work build/pdf-checks/opal-2.30
	git diff --exit-code --stat $(ANNOT)

## the spec compiler
hsedc:
	stack build hsed-spec:exe:hsedc

## build/<spec>.hsed.json: each annotated PDF extracted (pristine + the committed annotations); it
## differs from spec/ir/<spec>.json in its annots alone, which is what hsedc reads
ir-annotated: annotate
	$(EXTRACT) build/core-2.01.hsed.pdf $(CORE_IR) build/core-2.01.hsed-index.txt --spec core-2.01 --page-offset 17 --pristine $(CORE)
	$(EXTRACT) build/opal-2.30.hsed.pdf $(OPAL_IR) build/opal-2.30.hsed-index.txt --spec opal-2.30 --page-offset 1 --pristine $(OPAL) --check-against spec/ir/core-2.01.json

## hsed-spec/bindings/Hsed/Bindings/{Core,Opal}.hs from the annotations of build/<spec>.hsed.json (made
## afresh from the committed sidecars); only the hsed-spec:bindings test component compiles them, and
## an Error leaves them as they were
bind: ir-annotated hsedc
	$(BIND_CMD)

## classify every numbered table of each spec and run the checks under Scott's errata (non-zero exit on
## an Error; Opal's unbound units are Warnings until M5)
hsedc-check: ir-annotated hsedc
	$(CHECK_CMD)

## hsed/gen/Hsed/Gen/{Core,Opal}/*.hs (any other module there removed), the reports
## build/report/{findings,coverage,ERRATA,ledger}.md (Opal's under build/report/opal) and the committed
## copies of the ledgers, docs/LEDGER.md and docs/LEDGER-opal.md (nothing written on an Error)
compile: ir-annotated hsedc
	$(COMPILE_CMD)

## the whole chain, in order: annotate -> ir-annotated -> bind -> hsedc-check -> compile
gen: ir-annotated hsedc
	$(BIND_CMD)
	$(CHECK_CMD)
	$(COMPILE_CMD)

## the committed bindings, generated code and ledgers must be what gen produces today (changed, removed
## or new files)
gen-diff: gen
	git diff --exit-code --stat $(GENERATED)
	@new=$$(git status --porcelain --untracked-files=all -- $(GENERATED)); \
	  if [ -n "$$new" ]; then echo "$$new"; echo "gen-diff: generated files are not committed"; exit 1; fi

## dev/TableUIDs.txt: GHCi's -ddump-splices transcript of the hand-written TableUIDs (since M2d the
## oracle's test fixture hsed/fixture/Hand/TableUIDs.hs), the definitions the t240 quasiquoter makes
## (design 5.5: secondary to the runtime oracle, regenerated at M2a and M2d); paths made relative to
## the checkout, and the notices stack prints for a test component before 'Configuring GHCi' left
## out. hsed's library is built first: GHCi runs the test component against the installed library
## (-package hsed), and stack ghci builds hsed-core as a plain dependency, under another unit id
## than stack test registers it with, but not hsed's library, which after a stack test names an
## hsed-core no longer installed ("cannot satisfy -package hsed"). Written to build/TableUIDs.txt
## first and moved over dev/TableUIDs.txt only when it holds the splices and GHCi's 'Ok, ' line
## (GHCi exits 0 when a :load fails, after the splices it ran): a stack ghci or a load that fails
## leaves the committed file as it was, and make fails. Not part of check.
splices:
	mkdir -p build
	stack build hsed
	printf ':set -ddump-splices\n:load Hand.TableUIDs\n' \
	  | stack ghci hsed:test:hsed-test --no-load --ghci-options='-ignore-dot-ghci' 2>&1 \
	  | sed -e '/^Configuring GHCi/,$$!d' -e 's|$(CURDIR)/||g' > build/TableUIDs.txt
	@grep -q 'Splicing declarations' build/TableUIDs.txt && grep -q '^Ok, ' build/TableUIDs.txt || \
	  { echo "splices: build/TableUIDs.txt holds no splices of a load that succeeded (stack ghci or the load failed); dev/TableUIDs.txt left as it was"; exit 1; }
	mv build/TableUIDs.txt dev/TableUIDs.txt
