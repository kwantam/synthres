
GHC_OPTS = -O6 -threaded --make -outputdir build
PROF_OPTS =

.PHONY : all, profile, clean, force

all: build/synthres

profile : PROF_OPTS = -prof -auto-all -caf-all -fforce-recomp
profile : clean build/synthres

clean :
	rm -rf build

build/% : %.hs
	mkdir -p build
	ghc $(GHC_OPTS) $(PROF_OPTS) -o $@ $<

