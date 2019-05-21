x = ref
CLASSFILE = tspl
SOURCES := $(wildcard ../swl/*.ss ../../tutorial/*.ss)
texsrc = title.tex preface.tex body.tex summary.tex threads.tex macros.tex faq.tex

latex = latex
Prep = stex/tspl-prep
Hprep = stex/html-prep

PS = ../../ref/$x.ps
HTML = ../../ref/$x.html

.SUFFIXES:
.SUFFIXES: .stex .tex

include ../../config.make

#${PS}: thirdrun $x.dvi
#	dvips -o ${PS} $x.dvi

$x.dvi: thirdrun

thirdrun: secondrun
	makeindex $x
	echo '(summary-make "$x")' | $(Scheme) setup.ss summary.ss
	${latex} $x
	touch thirdrun

secondrun: firstrun
	makeindex $x
	echo '(summary-make "$x")' | $(Scheme) setup.ss summary.ss
	${latex} $x
	touch secondrun

firstrun: ${texsrc} $x.tex faq.tex ${CLASSFILE}.cls
	touch ref.sfm
	${latex} $x.tex
	touch firstrun

.stex.tex:
	echo '(go "$*")' | ${Prep}
# don't even think about it!
#	chmod -w $*.tex

body.stex: dc.so ${SOURCES}
	echo "(reset-handler abort) (fsm \"body.stex\" (quote replace) (quote (${SOURCES})))" | ${Scheme} dc.so ../swl/documentation.ss

threads.tex:  ../threads/threads.stex
	rm -f threads.stex
	ln -s ../threads/threads.stex .
	echo '(go "threads")' | ${Prep}

generics.ss: dc.so
	echo "(fsm-generics \"generics.ss\" (quote (${SOURCES})))" |\
	   ${Scheme} dc.so
	mv generics.ss /tmp
	sort /tmp/generics.ss | uniq > generics.ss

dc.so: dc.ss
	echo "(define target (quote tex))\
	      (optimize-level 2)\
	      (compile-file \"dc.ss\")" | ${Scheme} 

$x.html: hsecondrun
#$x.html: hsecondrun gifsrun mathrun

gifsrun:
	@(cd gifs; $(MAKE})

mathrun: ${mathfiles}
	-(cd math; $(MAKE))

${mathfiles}: hsecondrun
	echo -n gifs= > ${mathfiles}
	(cd math; echo *.tex | sed -e "s/\.tex/.gif/g") >> ${mathfiles}

hsecondrun: hfirstrun
	echo '(go "$x")' | ${Hprep}
	chmod 444 *.html
	touch hsecondrun

hfirstrun: ${Hprep} $x.dvi
	echo '(go "$x")' | ${Hprep}
	touch hfirstrun

clean:
	rm -f *.so *.aux *.ox *.log *.toc *.lof *.idx *.ind *.ilg *~ *.raw
	rm -f firstrun secondrun thirdrun hfirstrun hsecondrun hthirdrun
	rm -f threads.stex
	rm -f ref.tex body.tex body.stex title.tex preface.tex summary.tex threads.tex
	rm -f code.ss Make.out *.dvi *.html $x.sfm $x.rfm *.haux

test.html: body.stex test.stex ${Hprep}
	echo '(go "test")' | ${Hprep}

include ../../config.make

install: $x.html
	/bin/rm -rf $(SWL_ROOT)/ref/*.html $(SWL_ROOT)/ref/math
	cp -p *.html $(SWL_ROOT)/ref
	-cp -rp math $(SWL_ROOT)/ref
