# mGtk - Makefile.inc
# Configuration for make under Unix
# (taken from Makefile.inc for Moscow ML)


# Toplevel directory of unpacked distribution
TOPDIR=$(HOME)/mgtk

# Where to install stuff
MGTKHOME=$(TOPDIR)

# Where MosML resides
MOSMLHOME=$(HOME)/mosml

# Where the source files for the MosML runtime reside
MOSMLRUNTIMESRC=$(MOSMLHOME)/src/runtime


# Where GTK is installed
GTKHOME=/usr

# Various utility programs
REMOVE=rm -f
MOVE=mv


# For Linux, use:
LD=gcc -rdynamic
DYNLD=gcc -shared
CC=gcc
ADDRUNLIBS=
ADDRUNCFLAGS=


SHELL=/bin/sh

# ---------- You shouldn't need to edit below this line ------------

BINDIR=$(TOPDIR)
LIBDIR=$(TOPDIR)

MOSMLTOOLS=$(MOSMLHOME)/tools
MOSMLRUNTIME=$(MOSMLRUNTIMESRC)

MOSMLC=$(MOSMLHOME)/bin/mosmlc
CAMLRUNM=$(MOSMLHOME)/bin/camlrunm

GTKCONFIG=$(GTKHOME)/bin/gtk-config

# Options for compilation of C files using the MosML runtime and Gtk+
OPTS=-O3 -fPIC
CINCLUDE=-I$(MOSMLRUNTIME) `$(GTKCONFIG) --cflags`
CFLAGS=$(ADDRUNCFLAGS) -Dunix $(OPTS) $(CINCLUDE)
LIBS=$(ADDRUNLIBS) `$(GTKCONFIG) --libs`


.SUFFIXES :
.SUFFIXES : .sml .sig .ui .uo .c .o

.sig.ui:
	$(MOSMLC) $(MOSMLINCLUDE) -c $<

.sml.uo:
	$(MOSMLC) $(MOSMLINCLUDE) -c $<

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $<
