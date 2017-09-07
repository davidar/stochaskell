.PHONY: build

build: edward/env pymc3/env
	stack build --trace

%-exe: build
	stack exec $@ -- +RTS -xc

%.prof: build
	stack exec $* -- +RTS -p
	grep '[0-9][0-9][.]' $@

%.hp: build
	stack exec $* -- +RTS -h
	hp2ps -cg $@

%/env: %/requirements.txt
	virtualenv $@
	$@/bin/pip install -Ur $<
