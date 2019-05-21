include ../../config.make

.SUFFIXES:
.SUFFIXES: .c .o .h .ss .so

.c.o:
	$(CC) -c $(CFLAGS) $*.c

SOURCE = base1.ss base2.ss button.ss callback.ss canvas.ss canvasitem.ss \
         dialog.ss entry.ss event.ss foreign.ss frame.ss generics.ss \
         image.ss init.ss io.ss label.ss listbox.ss load.ss markup.ss menu.ss \
         option.ss proto.ss require.ss scale.ss scrollbar.ss syntax.ss \
         teventloop.ss text.ss tkstream.ss ../threads/threads.ss selection.ss \
	 preferences.ss ../../apps/common/scrollframe.ss \
         console.ss module-setup.ss server.ss \
	 ../oop/class.ss \
	 ../../apps/common/app-text.ss \
	 ../../apps/common/app.ss \
	 ../../apps/common/arrows.ss \
	 ../../apps/common/auxio.ss \
	 ../../apps/common/flex-button.ss \
	 ../../apps/common/fontmenu.ss \
	 ../../apps/common/help-text.ss \
	 ../../apps/common/popup.ss \
	 ../../apps/common/scheme-text.ss \
	 ../../apps/common/scrollframe.ss \
	 ../../apps/common/select-file.ss \
	 ../../apps/common/semaphore.ss \
	 ../../apps/common/warning-dialog.ss \
	 ../../apps/common/worrier.ss \
	 ../../apps/edit/edit-text.ss \
	 ../../apps/edit/edit.ss \
	 ../../apps/htmlview/html-text.ss \
	 ../../apps/htmlview/html.ss \
	 ../../apps/htmlview/htmlview.ss \
	 ../../apps/htmlview/www.ss \
	 ../../apps/repl/repl-text.ss \
	 ../../apps/repl/repl-text.ss \
	 ../../apps/repl/repl.ss 

COBJECT = foreign.o

all: $(prefix) $(prefix)/$(FOREIGN_OBJ) $(prefix)/swl

$(prefix):
	mkdir -p $(prefix)

$(prefix)/swl.boot: $(SOURCE) make.ss build-script
	echo $(prefix)/swl.boot | $(Scheme) make.ss

$(prefix)/$(FOREIGN_OBJ): $(COBJECT) ../../config.make
	$(LD) $(LDFLAGS) -o $(prefix)/$(FOREIGN_OBJ) $(COBJECT) $(SHLIBS)

# we don't use -b option here when saving a heap, since we don't want
# to run new scheme-start routine.
$(prefix)/swl: $(prefix)/swl.boot
	rm -f $(prefix)/swl
	echo "#!/bin/sh" > $(prefix)/swl
	echo "TCL_LIBRARY=$(TCL_LIBRARY)" >> $(prefix)/swl
	echo "TK_LIBRARY=$(TK_LIBRARY)" >> $(prefix)/swl
	echo "SWL_ROOT=$(SWL_ROOT)" >> $(prefix)/swl
	echo "SWL_LIBRARY=$(SWL_LIBRARY)" >> $(prefix)/swl
	echo "export TCL_LIBRARY; export TK_LIBRARY; export SWL_LIBRARY; export SWL_ROOT" >> $(prefix)/swl
	if [ "$(SCHEMEHEAPDIRS)" != "" ] ; then \
	  echo "SCHEMEHEAPDIRS=$(SCHEMEHEAPDIRS)" >> $(prefix)/swl ;\
	  echo "export SCHEMEHEAPDIRS" >> $(prefix)/swl ;\
	fi
	if [ "$(NOHEAP)" = "1" ] ; then \
	  echo "exec $(Scheme) -b $(prefix)/swl.boot "'$$@' >> $(prefix)/swl ;\
        else \
          echo "(exit 0)" | $(Scheme) -s+ $(prefix)/swl.heap $(prefix)/swl.boot ; \
	  echo "exec $(Scheme) -h $(prefix)/swl.heap "'$$@' >> $(prefix)/swl ;\
        fi
	chmod 755 $(prefix)/swl
	cp $(prefix)/swl ../..

clean:
	-sh ./cleanso
	rm -f cleanso build-script cache *.o *.obj so_locations
