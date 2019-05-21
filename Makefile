Scheme = $(shell if [ -e /usr/local/bin/scheme ]; then echo /usr/local/bin/scheme; elif [ -e /usr/bin/scheme ]; then echo /usr/bin/scheme; elif [ -e /usr/local/bin/petite ]; then echo /usr/local/bin/petite; elif [ -e /usr/bin/petite ]; then echo /usr/bin/petite; else echo scheme; fi)
Schemebase = $(shell basename $(Scheme))
m := $(shell echo '(machine-type)' | $(Scheme) -q)

Tclsh = tclsh

include Mf-SWLVER

###############################################################################

# SWL_ROOT is the installation root for SWL
# The SWL boot file (swl.boot), heap (swl.heap), script (swl), and
# compiled applications (eg. repl.so) will be installed in the the
# directory named $(SWL_ROOT)/<arch>  where <arch> is the architecture
# reported by Scheme for (machine-type).
SWL_ROOT=lib

# Specify optimization level, inspector, and profiling flags
o=2
i=f
p=f

# Should not need to modify anything below here.

all: config.make
ifeq "$(Schemebase)" "petite"
	@if [ ! -e lib/$(m)/swl.boot -o ! -e lib/$(m)/swl.so ] ; then\
          echo "no libraries found; cannot rebuild with Petite Chez Scheme" ;\
          false ;\
        else\
          echo "using existing libraries; cannot rebuild with Petite Chez Scheme" ;\
        fi
else
	(cd src/swl; $(MAKE))
endif

install-doc: config.make
	(cd src/dc/stex; $(MAKE))
	(cd src/dc; $(MAKE) install)

tclversion.ss:
	$(Tclsh) tclversion.tcl

config.scheme config.make: tclversion.ss configure.ss Makefile
	echo "(define swl:Scheme \"$(Scheme)\")\
	      (define prefix \"$(SWL_ROOT)\")\
	      (set! swl:version \"$(SWLVER)\")\
	      (optimize-level $(o))\
	      (quote (compile-profile #$(p)))\
	      (generate-inspector-information #$(i))" > config.scheme
	$(Scheme) configure.ss

config: cleanconfig config.make

# clean cleans out files used during build process, leaves heaps and libraries
clean: cleanintermediate cleanconfig
	/bin/rm -f swl
	/bin/rm -f Make.out

# reallyclean removes heaps and libraries
reallyclean: cleanlib cleanstex clean

# distclean removes online documentation produced by document compiler
distclean: cleanref cleanlib cleanstex clean
	/bin/rm -f swl

cleanintermediate: config.make
	(cd src/oop; $(MAKE) clean)
	(cd src/swl; $(MAKE) clean)
	(cd src/dc/stex ; $(MAKE) clean)
	(cd src/dc ; $(MAKE) clean)
	(cd apps/repl ; $(MAKE) clean)
	(cd apps/edit ; $(MAKE) clean)
	(cd apps/htmlview ; $(MAKE) clean)

cleanconfig:
	rm -f tclversion.ss config.make config.scheme

cleanlib: config.make
	$(MAKE) -f Mf-cleanlib cleanlib

cleanref:
	(cd $(SWL_ROOT)/ref ; rm -f *.html ; rm -rf math)

cleanstex:
	(cd src/dc/stex ; $(MAKE) reallyclean)

lecture: all
	(cd apps/lecture ; $(MAKE))

install: config.make all
	$(MAKE) -f Mf-install SWLVER=$(SWLVER)

tarball:
	git archive HEAD | bzip2 > /tmp/swl-`date +"%Y-%m-%d-%H:%M"`.tar.bz2
