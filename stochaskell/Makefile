.PHONY: build
.PRECIOUS: %.prof

PROF = --profile --work-dir .stack-work.prof
NOPROF = --work-dir .stack-work.noconflict

build: env
	stack build --interleaved-output $(NOPROF)

trace: env
	stack build --interleaved-output $(PROF)

%-exe: build
	stack exec $(NOPROF) $@

%.trace: trace
	stack exec $(PROF) $* -- +RTS -xc

%.prof: trace
	stack exec $(PROF) $* -- +RTS -p
	grep '[0-9][0-9][.]' $@

%.hp: trace
	stack exec $(PROF) $* -- +RTS -h
	hp2ps -cg $@

env: edward/env pymc3/env

%/env: %/requirements.txt
	virtualenv $@
	$@/bin/pip install -Ur $<

doc: build
	stack haddock $(NOPROF)
	stack exec $(NOPROF) \
		--no-ghc-package-path -- standalone-haddock -o docs \
		--compiler-exe=$(shell stack path --compiler-exe) \
		--dist-dir=$(shell stack path --dist-dir) \
		--package-db=$(shell stack path --snapshot-pkg-db) \
		--package-db=$(shell stack path --local-pkg-db) .
