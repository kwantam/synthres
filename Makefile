
# if we're using GHC 7, enable rts options and add -N for parallelism
RTS_OPT = $(shell ghc --version | grep ' 7\.' $&>/dev/null && echo -rtsopts -with-rtsopts=-N)
GHC_OPTS = -O6 -threaded --make -outputdir build
PROF_OPTS =
SOURCE_FILES = $(wildcard *.hs)

.PHONY : all, profile, clean, force

all: build/synthres

profile : PROF_OPTS = -prof -auto-all -caf-all -fforce-recomp
profile : clean build/synthres

clean :
	rm -rf build

build/% : %.hs $(SOURCE_FILES)
	mkdir -p build
	ghc $(GHC_OPTS) $(PROF_OPTS) $(RTS_OPT) -o $@ $<

