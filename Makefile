export STOCHASKELL_DIRECTORY = $(CURDIR)/stochaskell

build:
	stack build --interleaved-output --trace

install: build
	stack exec -- ihaskell install --stack

jupyter: install
	stack exec -- jupyter notebook

docker:
	docker build -t stochaskell:latest .
