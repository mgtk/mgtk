# mGtk - Makefile
# (c) Ken Friis Larsen and Henning Niss 1999, 2000.

include Makefile.inc

# Files
MGTKFILES=Gtk.sig Gtk.sml mgtk.c
MGTKOBJS=Gtk.ui Gtk.uo mgtk.so
CFILES=mgtk.c
COBJS=mgtk.o

all: $(MGTKOBJS)

mgtk.so: $(COBJS)
	$(DYNLD) -o $@ $(COBJS) $(LIBS)

clean:
	$(REMOVE) *.u? 
	$(REMOVE) *~
	$(REMOVE) *.o *.so
	$(REMOVE) Makefile.bak

depend : 
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak;	 \
         gcc -MM $(CINCLUDE) $(OPTS) *.c) > Makefile
	$(MOSMLHOME)/bin/camlrunm $(MOSMLHOME)/tools/mosmldep >> Makefile


### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
mgtk.o: mgtk.c
Gtk.uo: Gtk.ui 
