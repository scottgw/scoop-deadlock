all: demo

GHC_OPTS=-O2 -Wall

PROF_OPTS=${GHC_OPTS} -prof -auto-all

.PHONY: demo
demo:
	@cp cabals/demo.cabal .
	runhaskell Setup configure --user && \
	runhaskell Setup build && \
        runhaskell Setup install
	@rm demo.cabal



.PHONY: deadtest
deadtest:
	@cp cabals/deadtest.cabal .
	runhaskell Setup configure --user && \
	runhaskell Setup build && \
        runhaskell Setup install
	@rm deadtest.cabal

clean:
	@rm -rf dist
	@rm -f demo.cabal deadtest.cabal

