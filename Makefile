export STOCHASKELL_DIRECTORY = $(CURDIR)/stochaskell

IHASKELL = \
	ihaskell \
	ihaskell-aeson \
	ihaskell-blaze \
	ihaskell-charts \
	ihaskell-diagrams \
	ihaskell-gnuplot \
	ihaskell-hatex \
	ihaskell-juicypixels \
	ihaskell-magic \
	ihaskell-plot \
	ihaskell-static-canvas \
	ihaskell-widgets

IHASKELL_BIN = $(shell stack path --local-install-root)/bin/ihaskell

$(IHASKELL_BIN):
	stack build --trace $(IHASKELL)

build:
	stack build --interleaved-output --trace

install: build $(IHASKELL_BIN)
	stack exec -- ihaskell install --stack

jupyter: install
	jupyter notebook
