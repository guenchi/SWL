include ../Mf-SWLVER
Scheme=scheme
M := $(shell echo '(machine-type)' | $(Scheme) -q)
R = 1

pkg = swl$(SWLVER)-$(M)-$(R).pkg.tar.gz

all: $(pkg)

swl$(SWLVER)-$(M)-$(R).pkg.tar.gz:
	sudo makepkg "$(Scheme)" $(SWLVER) $(M) $(R)

clean:
	/bin/rm -rf $(pkg)
