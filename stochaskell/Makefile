.PHONY: build exec

GHC_OPTS += -rtsopts -fprof-auto -fprof-cafs
STACK_OPTS += --ghc-options="$(GHC_OPTS)"
STACK_OPTS += --enable-executable-profiling --enable-library-profiling

build:
	stack build $(STACK_OPTS)

exec: build
	stack exec stochaskell-exe -- +RTS -xc
