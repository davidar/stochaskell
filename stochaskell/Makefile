.PHONY: build exec

build:
	stack build --trace

%-exe: build
	stack exec $@ -- +RTS -xc

%.prof: build
	stack exec $* -- +RTS -p
	grep '[0-9][0-9][.]' $@
