include ../../src/Makefile.inc

MOSMLINCLUDE=-I $(LIBDIR)

# Options for compilation of mgtk.c
OPTS=-O3 -fPIC
CINCLUDE=-I$(MOSMLRUNTIME) `$(GTKCONFIG) --cflags`
CFLAGS=$(ADDRUNCFLAGS) -Dunix $(OPTS) $(CINCLUDE)
LIBS=$(ADDRUNLIBS) `$(GTKCONFIG) --libs`

SMLOBJ=MosmlToplevel.uo Unix.uo Signal.uo mosIDE.uo
SIGOBJ=MosmlToplevel.ui Unix.ui Signal.ui mosIDE.ui

all: $(SMLOBJ) $(SIGOBJ) libmunix.so

libmunix.so: munix.o
	$(DYNLD) -o $@ munix.o

clean:
	$(REMOVE) *~
	$(REMOVE) *.u? 
	$(REMOVE) *.o *.so
	$(REMOVE) Makefile.bak

depend: 
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(CAMLRUNM) $(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(CAMLRUNM) $(MOSMLTOOLS)/mosmldep >> Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
Unix.uo: Unix.ui Signal.ui 
Unix.ui: Signal.ui 
Signal.uo: Signal.ui 
MosmlToplevel.uo: MosmlToplevel.ui Unix.ui 
mosIDE.uo: MosmlToplevel.ui 
