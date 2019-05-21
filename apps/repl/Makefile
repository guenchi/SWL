include ../../config.make

repl: $(prefix) $(prefix)/repl.so

$(prefix)/repl.so: $(prefix)/swl.heap build-script \
                   ../common/semaphore.ss \
                   ../common/app.ss \
                   ../common/app-text.ss \
                   ../common/help-text.ss \
		   ../common/popup.ss \
                   ../common/scheme-text.ss \
		   ../common/scrollframe.ss \
                   ../common/fontmenu.ss \
		   ../common/arrows.ss \
                   ../common/select-file.ss \
		   ../common/auxio.ss \
                   repl-text.ss repl.ss
	../../swl - build-script
	mv repl.so $(prefix)

$(prefix):
	mkdir -p $(prefix)

clean:
