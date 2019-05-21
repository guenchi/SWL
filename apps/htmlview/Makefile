include ../../config.make

htmlview: $(prefix) $(prefix)/htmlview.so

$(prefix)/htmlview.so: $(prefix)/swl.heap build-script \
                       html.ss www.ss \
                       ../common/flex-button.ss \
                       ../common/help.ss \
                       ../common/select-file.ss \
                       ../common/arrows.ss
	../../swl - build-script
	mv htmlview.so $(prefix)

$(prefix):
	mkdir -p $(prefix)

clean:
