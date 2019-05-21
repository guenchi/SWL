include ../../config.make

# OBJECT = dispatch.so defint.so class.so
OBJECT = class.so

all: $(prefix) $(prefix)/oop.boot

$(prefix):
	mkdir -p $(prefix)

$(prefix)/oop.boot: class.ss defint.ss dispatch.ss build-script
	$(Scheme) build-script
	cat $(OBJECT) > $(prefix)/oop.boot

clean:
	rm -f $(OBJECT) build-script

