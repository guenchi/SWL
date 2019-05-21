scripts = tspl-prep scheme-prep index-prep html-prep
heaps = tspl-prep.heap scheme-prep.heap index-prep.heap html-prep.heap

include ../../../config.make

all: ${scripts}

tspl-prep: tspl-prep.heap
	echo '#! /bin/sh' > $@
	echo 'm='`arch` >> $@
	echo 'LIB='`pwd` >> $@
	/bin/echo 'exec ${Scheme} -h $$LIB/scheme-prep.heap.$$m '\
                                 '-h $$LIB/tspl-prep.heap.$$m $$*' >> $@
	chmod +x $@

tspl-prep.heap: tspl-prep.stex scheme-prep
	echo '(go "tspl-prep")' | ./scheme-prep -s+ $@.`arch`

scheme-prep: scheme-prep.heap
	echo '#! /bin/sh' > $@
	echo 'm='`arch` >> $@
	echo 'LIB='`pwd` >> $@
	/bin/echo 'exec ${Scheme} -h $$LIB/scheme-prep.heap.$$m $$*' >> $@
	chmod +x $@

index-prep: index-prep.heap
	echo '#! /bin/sh' > $@
	echo 'm='`arch` >> $@
	echo 'LIB='`pwd` >> $@
	/bin/echo 'exec ${Scheme} -h '`pwd`'/index-prep.heap.$$m $$*' >> $@
	chmod +x $@

html-prep: html-prep.heap
	echo '#! /bin/sh' > $@
	echo 'm='`arch` >> $@
	echo 'LIB='`pwd` >> $@
	/bin/echo 'exec ${Scheme} -h '`pwd`'/html-prep.heap.$$m $$*' >> $@
	chmod +x $@

scheme-prep.heap: scheme-prep.ss preplib.ss dsm.ss setup.ss
	echo | ${Scheme} -s+ $@.`arch` setup.ss scheme-prep.ss

index-prep.heap: index-prep.ss preplib.ss dsm.ss setup.ss
	echo | ${Scheme} -s+ $@.`arch` setup.ss index-prep.ss

html-prep.heap: html-prep.ss preplib.ss dsm.ss setup.ss html-prep.tex
	echo '(go "html-prep.tex")' | ${Scheme} -s+ $@.`arch` setup.ss html-prep.ss

clean:
	/bin/rm -f tspl-prep.tex Make.out

reallyclean: clean
	/bin/rm -f ${scripts} tspl-prep.heap.`arch` scheme-prep.heap.`arch`\
          index-prep.heap.`arch` html-prep.heap.`arch`
