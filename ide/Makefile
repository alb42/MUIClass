.PHONY:	all docs
CPU	:= $(shell fpc -iSP)
OS	:= $(shell fpc -iSO)
SRC=../src
FILE=MUIIDE.pas
ifeq ($(CPU)a$(OS),a)
CPU=m68k
OS=amiga
endif

all:
ifeq ($(OS),linux)
	make crossaros
else
	-makedir lib/$(CPU)-$(OS)
	fpc -Fu$(SRC) -FUlib/$(CPU)-$(OS) $(FILE)
endif


crossaros:
	mkdir -p lib/i386-aros
	fpc4aros.sh -FUlib/i386-aros -Fu$(SRC) $(FILE)

crossos4:
	mkdir -p lib/powerpc-amiga
	fpc4os4.sh -FUlib/powerpc-amiga -Fu$(SRC) $(FILE)

crossmorphos:
	mkdir -p lib/powerpc-morphos
	fpc4mos.sh -FUlib/powerpc-morphos -Fu$(SRC) $(FILE)


crossarosarm:
	mkdir -p lib/arm-aros
	fpc4arosarm.sh -FUlib/arm-aros -Fu$(SRC) $(FILE)

crossaros64:
	mkdir -p lib/x86_64-aros
	fpc4aros64.sh -FUlib/x86_64-aros -Fu$(SRC) $(FILE)

crossamiga:
	mkdir -p lib/m68k-amiga
	fpc4amiga.sh -FUlib/m68k-amiga -Fu$(SRC) $(FILE)




