# mGtk - Makefile
# (c) Ken Friis Larsen and Henning Niss 1999, 2000.

include ../src/Makefile.inc

# Options for compilation of mgtk.c
OPTS=-O3 -fPIC
CINCLUDE=-I$(MOSMLRUNTIME) `$(GTKCONFIG) --cflags`
CFLAGS=$(ADDRUNCFLAGS) -Dunix $(OPTS) $(CINCLUDE)
LIBS=$(ADDRUNLIBS) `$(GTKCONFIG) --libs`

# Files
MGTKFILES=Gtk.sig Gtk.sml mgtk.c
MGTKOBJS=Gtk.ui Gtk.uo mgtk.so

CFILES=mgtk.c
COBJS=mgtk.o

GENPROG=mgtkgen/mgtkgen

SUBDIRS=mgtkgen

all: mgtkobjs

mgtkfiles: $(MGTKFILES) $(GENPROG)
mgtkobjs: mgtkfiles $(MGTKOBJS)


Gtk.sig: header.sig gtk.defs
	$(GENPROG) -sig -o Gtk.sig gtk.defs

Gtk.sml: header.sml gtk.defs
	$(GENPROG) -sml -o Gtk.sml gtk.defs

mgtk.c: header.c gtk.defs
	$(GENPROG) -c -o mgtk.c gtk.defs

mgtk.so: $(COBJS)
	$(DYNLD) -o $@ $(COBJS) $(LIBS)

clean:
	$(REMOVE) *~
	$(REMOVE) *.u? 
	$(REMOVE) *.o *.so
	$(REMOVE) Makefile.bak

gtkclean: clean
	$(REMOVE) Gtk.sig Gtk.sml mgtk.c

realclean: gtkclean
	@dirs='$(SUBDIRS)'; for dir in $$dirs; do \
	   cd $$dir; \
	   $(MAKE) realclean; \
           cd ..; \
        done
