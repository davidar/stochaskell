export STOCHASKELL_DIRECTORY = $(CURDIR)/stochaskell

TRACE = # --trace

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
#	ihaskell-plot \
#	ihaskell-static-canvas \
#	ihaskell-widgets

IHASKELL_BIN = $(shell stack path --local-install-root)/bin/ihaskell

build:
	stack build --interleaved-output $(TRACE)

$(IHASKELL_BIN):
	stack build $(TRACE) $(IHASKELL)

install: build $(IHASKELL_BIN)
	stack exec -- ihaskell install --stack

uninstall:
	rm -f $(IHASKELL_BIN)

jupyter: install
	jupyter notebook
