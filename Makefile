PACKAGE = commands-spiros
VERSION = 0.0.0

HC = cabal exec -- ghc

CODE = config # tests
EXECUTABLE = server


# # # # # # # # # # # # # # # # # # 

default: check

all: build test run document style

configure:
	cabal configure --enable-tests

build: configure
	cabal build $(EXECUTABLE) 

run:
	cabal run server

serve:
	echo 'curl  -X POST  -H "Content-Type: application/json"  -d '"'"'["par","round","grave","camel","lit","async","break","break","action"]'"'"' "http://localhost:'"'"'${PORT}'"'"'/recognition/"'
	cabal run server

bench:
	cabal configure --enable-benchmarks && cabal bench

test:
# cabal install ../commands-core --force-reinstall --enable-tests
# cabal exec -- ghc-pkg unregister commands-core-0.0.0
	cabal configure --enable-tests
	cabal test && echo && cat dist/test/*-tests.log


document:
	cabal haddock
	open dist/doc/html/$(PACKAGE)/index.html

hyperlink: 
	cabal haddock --with-haddock="$(HOME)/haddock/.cabal-sandbox/bin/haddock" --haddock-options="--hyperlinked-source"
	open dist/doc/html/$(PACKAGE)/index.html

style:
	hlint --hint=HLint.hs  *.hs $(CODE)

fix:
	git vogue fix --all

check:
#	cabal build --ghc-options="-fforce-recomp -fno-code"
	cabal build --ghc-options="-fno-code"


# # # # # # # # # # # # # # # # # # 

default: Haskell

clean:
	cabal clean 
	rm -f Main *.{o,hi,dyn_o,dyn_hi}

fresh: clean
	rm -fr dist

.PHONY: default clean fresh all build test document style check bench
