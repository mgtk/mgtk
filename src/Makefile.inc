# mGtk - Makefile.inc
# Configuration for make under Unix
# (taken from Makefile.inc for Moscow ML)


# Toplevel directory of unpacked distribution
TOPDIR=$(HOME)/research/mgtk

# Where to install stuff
MGTKHOME=$(HOME)/mgtk

# Where MosML resides
MOSMLHOME=/usr/lib/mosml

# Where the source files for the MosML runtime reside
MOSMLRUNTIMESRC=$(MOSMLHOME)/include

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

BINDIR=/usr/bin
LIBDIR=$(TOPDIR)/src

GENPROG=$(TOPDIR)/src/mgtkgen/defs2sml

MOSMLTOOLS=$(MOSMLHOME)/tools
MOSMLRUNTIME=$(MOSMLRUNTIMESRC)

MOSMLC=$(BINDIR)/mosmlc
CAMLRUNM=$(MOSMLHOME)/bin/camlrunm
MOSMLPM=mosmlpm

GTKCONFIG=$(GTKHOME)/bin/gtk-config

.SUFFIXES :
.SUFFIXES : .sml .sig .ui .uo .c .o

.sig.ui:
	$(MOSMLC) $(MOSMLINCLUDE) -c $<

.sml.uo:
	$(MOSMLC) $(MOSMLINCLUDE) -c $<

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $<
