.PHONY: build

build: env
	stack build

trace: env
	stack build --trace

%-exe: build
	stack exec $@

%.trace: trace
	stack exec $* -- +RTS -xc

%.prof: trace
	stack exec $* -- +RTS -p
	grep '[0-9][0-9][.]' $@

%.hp: trace
	stack exec $* -- +RTS -h
	hp2ps -cg $@

env: edward/env pymc3/env

%/env: %/requirements.txt
	virtualenv $@
	$@/bin/pip install -Ur $<

doc: build
	standalone-haddock -o docs --package-db=$(shell stack path --snapshot-pkg-db) --package-db=$(shell stack path --local-pkg-db) .
