.PHONY: build exec

build:
	stack build --trace

%-exe: build
	stack exec $@ -- +RTS -xc
