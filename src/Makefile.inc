# mGtk - Makefile.inc
# Configuration for make under Unix
# (taken from Makefile.inc for Moscow ML)


# Toplevel directory of unpacked distribution
TOPDIR=${HOME}/tmp/mgtk

# Where to install stuff
MGTKHOME=${HOME}/tmp/mgtk

# Where MosML resides
MOSMLHOME=${HOME}/utils/mosml2.0

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

BINDIR=$(TOPDIR)/bin
LIBDIR=$(TOPDIR)/src

GENPROG=../bin/mgtkgen

MOSMLTOOLS=${MOSMLHOME}/tools
MOSMLRUNTIME=${MOSMLRUNTIMESRC}

MOSMLC=${MOSMLHOME}/bin/mosmlc
CAMLRUNM=${MOSMLHOME}/bin/camlrunm

GTKCONFIG=${GTKHOME}/bin/gtk-config

.SUFFIXES :
.SUFFIXES : .sml .sig .ui .uo .c .o

.sig.ui:
	$(MOSMLC) $(MOSMLINCLUDE) -c $<

.sml.uo:
	$(MOSMLC) $(MOSMLINCLUDE) -c $<

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $<
