
# 
# this file is part of synthres
#
# synthres is free software.  It comes without any warranty, to
# to the extent permitted by applicable law.  You can redistribute it
# and/or modify it under the terms of the Do What The Fuck You Want To
# Public License, Version 2, as published by Sam Hocevar.  See
# http://sam.zoy.org/wtfpl/COPYING for more details
#

# if we're using GHC 7, enable rts options and add -N for parallelism
RTS_OPT = $(shell ghc --version | grep ' 7\.' $&>/dev/null && echo -rtsopts -with-rtsopts=-N)
BUILD_DIR = build
GHC_OPTS = -O6 -threaded --make -outputdir $(BUILD_DIR)
CGI_OPTS = -optl-static -optl-pthread -static
PROF_OPTS =
SOURCE_FILES = $(wildcard *.hs)

.PHONY : all, profile, clean, force

all: cli cgi cgi2

cli: BUILD_DIR = build/cli
cli: build/synthres

cgi: BUILD_DIR = build/cgi
cgi: build/SynthResCGI.cgi

cgi2: BUILD_DIR = build/cgi2
cgi2: build/AllResNetsCGI.cgi

profile : PROF_OPTS = -prof -auto-all -caf-all -fforce-recomp
profile : clean build/synthres

clean :
	rm -rf build

build/%.cgi : %.hs $(SOURCE_FILES)
	mkdir -p $(BUILD_DIR)
	ghc $(GHC_OPTS) $(PROF_OPTS) $(CGI_OPTS) $(RTS_OPT) -o $@ $<
	strip $@

build/% : %.hs $(SOURCE_FILES)
	mkdir -p $(BUILD_DIR)
	ghc $(GHC_OPTS) $(PROF_OPTS) $(RTS_OPT) -o $@ $<
	strip $@

