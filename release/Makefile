# mGtk - Makefile
# (c) Ken Friis Larsen and Henning Niss 1999, 2000.

include Makefile.inc

# Options for compilation of mgtk.c
OPTS=-O3 -fPIC
CINCLUDE=-I$(MOSMLRUNTIME) `$(GTKCONFIG) --cflags`
CFLAGS=$(ADDRUNCFLAGS) -Dunix $(OPTS) $(CINCLUDE)
LIBS=$(ADDRUNLIBS) `$(GTKCONFIG) --libs`

# Files
MGTKFILES=Gtk.sig Gtk.sml mgtk.c
MGTKOBJS=Gtk.ui Gtk.uo mgtk.so


all: $(MGTKOBJS)

mgtk.so: $(COBJS)
	$(DYNLD) -o $@ $(COBJS) $(LIBS)

clean:
	rm -f *.u? 
	rm -f *.o *.so
	rm -f Makefile.bak

depend : 
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak;	 \
         gcc -MM $(CINCLUDE) $(OPTS) *.c) > Makefile
	$(MOSMLHOME)/bin/camlrunm $(MOSMLHOME)/tools/mosmldep >> Makefile


### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
mgtk.o: mgtk.c
Gtk.uo: Gtk.ui 
