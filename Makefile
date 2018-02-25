.SUFFIXES: .pas
#get CPU and HOST
ifeq ($(FPC)a,a)
FPC=fpc
endif
ifeq ($(CPU)a,a)
CPU	:= $(shell $(FPC) -iSP)
endif
ifeq ($(OS)a,a)
OS	:= $(shell $(FPC) -iSO)
endif

ifeq ($(CPU)a$(OS),a)
CPU=m68k
OS=amiga
endif
ifeq ($(OS),linux)
FPC=fpc4aros.sh
CPU=i386
OS=aros
endif
BDIR=units
ODIR=$(BDIR)/$(CPU)-$(OS)
FPCFLAGS=-FU$(ODIR) -Fu$(ODIR) -T$(OS) -P$(CPU)
SOURCES=$(wildcard src/*.pas)
OBJECTS=$(patsubst src/%.pas, $(ODIR)/%.o, $(SOURCES))
EXSRC=$(wildcard examples/*.pas)
EXAMPLES=$(patsubst examples/%.pas, examples/%, $(EXSRC))

all: $(ODIR) $(OBJECTS)

ide: all MUIIDE

examples: all $(EXAMPLES)

zip: all MUIIDE examples
	-tar cvf MUIClass.$(CPU)-$(OS).tar --warning=no-file-changed $(ODIR) ide/MUIIDE $(EXAMPLES)
	gzip MUIClass.$(CPU)-$(OS).tar

$(ODIR)/%.o: src/%.pas
	$(FPC) $(FPCFLAGS) $<
	
examples/%: examples/%.pas
	$(FPC) $(FPCFLAGS) $<

$(ODIR):
	-mkdir -p $(ODIR)

MUIIDE:
	$(FPC) $(FPCFLAGS) -Fuide ide/MUIIDE.pas
	
.PHONY: clean

clean:
	rm -f $(ODIR)/*.o $(ODIR)/*.ppu examples/*.o examples/*.ppu ide/*.o ide/*.ppu MUIIDE $(EXAMPLES) *.tar.gz *.tar
distclean: clean
	rm -rf $(BDIR)