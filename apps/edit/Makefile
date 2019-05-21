include ../../config.make

edit: $(prefix) $(prefix)/edit.so

$(prefix)/edit.so: $(prefix)/swl.heap build-script \
                ../common/app.ss \
                ../common/app-text.ss \
                ../common/help-text.ss \
		../common/popup.ss \
                ../common/scheme-text.ss \
                ../common/scrollframe.ss \
                ../common/fontmenu.ss \
                ../common/select-file.ss \
                ../common/warning-dialog.ss \
                ../common/auxio.ss \
                ../common/worrier.ss \
                edit-text.ss edit.ss
	../../swl - build-script
	mv edit.so $(prefix)

$(prefix):
	mkdir -p $(prefix)

clean:
