# mgtk --- an SML binding for GTK.                                          
# (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004.    

MAIN=defs2sml$(EXEEXT)

MOSMLC  = mosmlc
PERL    = perl
MOSMLFLAGS = -liberal
MOSMAKEDIRS = . util defs ast codegen
MOSMAKE = mosmake
include $(MOSMAKE)/Makefile.inc

MOSMLHOME=/usr/lib/mosml
MOSMLRUNTIME=$(MOSMLHOME)/include
CINCLUDE=-I$(MOSMLRUNTIME) `pkg-config --cflags gtk+-2.0`

all: $(MAIN) regression

# "Regression" test
REGDIR=test/regression
REGIN =$(REGDIR)/input
REGOUT=$(REGDIR)/output
REGTRS=$(REGDIR)/trusted

REGFILES=$(notdir $(wildcard $(REGIN)/*.defs))

REGOPTS=--separate-struct -q $(OPTIONS)
REGOPTSMOS=$(REGOPTS)
REGOPTSMLTON=--mlton $(REGOPTS)

REGCOMPOPTS=$(REGOPTS)
REGCOMPOPTSMOS=$(REGCOMPOPTS) -sp lib/preamble.sml -cp lib/preamble.c
REGCOMPOPTSMLTON=--mlton $(REGCOMPOPTS) -sp lib/preamble-mlton.sml -cp lib/preamble-mlton.c

reggen-%: $(MAIN)
	@echo -n "[REG]          $*.defs "
	@./$(MAIN) $(REGOPTSMOS) -o $(REGOUT)/$*.out $(REGIN)/$*.defs

reggenmlton-%: $(MAIN)
	@echo -n "[REG (MLton)]  $*.defs "
	@./$(MAIN) $(REGOPTSMLTON) -o $(REGOUT)/$*-mlton.out $(REGIN)/$*.defs

regdiff-%: reggen-%
	@if diff -Naur $(REGTRS)/$*.out $(REGOUT)/$*.out ; then \
           echo ok ;\
        fi

regdiffmlton-%: reggenmlton-%
	@if diff -Naur $(REGTRS)/$*-mlton.out $(REGOUT)/$*-mlton.out ; then \
           echo ok ;\
        fi

reg-%: regdiff-% regdiffmlton-%
	@echo -n

regression: $(MAIN) $(patsubst %.defs,regdiff-%,$(REGFILES)) $(patsubst %.defs,regdiffmlton-%,$(REGFILES))

regcompgen-%: $(MAIN)
	@echo "[GEN]          $*.defs"
	@./$(MAIN) $(REGCOMPOPTSMOS) -o $(REGDIR)/output/$* $(REGIN)/$*.defs 

regcomp-%: regcompgen-%
	@echo "[GCC]          $*.c"
	@gcc $(CINCLUDE) -c -o /dev/null $(REGOUT)/$*.c
	@echo "[MOSMLC]       $*.sml"
	@mosmlc -toplevel -liberal -c $(REGOUT)/$*.sml

reg-compile: $(MAIN) $(patsubst %.defs,regcomp-%,$(REGFILES))


# Generate binding
mgtk.c mgtk.sml: $(MAIN) api/pygtk/gtk.defs api/pygtk/gtk-types.defs lib/excludes.txt lib/preamble.sml lib/preamble.c
	./$(MAIN) -co mgtk.c -so mgtk.sml -cp lib/preamble.c -sp lib/preamble.sml api/pygtk/gtk.defs

mgtk-mlton.c mgtk-mlton.sml: $(MAIN) api/pygtk/gtk.defs api/pygtk/gtk-types.defs lib/excludes.txt
	./$(MAIN) --mlton -co mgtk-mlton.c -so mgtk-mlton.sml -cp lib/preamble-mlton.c -sp lib/preamble-mlton.sml api/pygtk/gtk.defs 

compile: $(MAIN) mgtk.c mgtk.sml
	mosmlc -toplevel -liberal -c mgtk.sml
	gcc $(CINCLUDE) -c mgtk.c

MLTONHOME=/usr/lib/mlton
MLTONINCLUDE=/usr/lib/mlton/include
MLTONBIN=/usr/bin/

mgtk-mlton.o: mgtk-mlton.c
	gcc -c -O2 -I$(MLTONINCLUDE) \
		   `pkg-config --cflags gtk+-2.0` mgtk-mlton.c

mltoncompile: $(MAIN) mgtk-mlton.sml mgtk-mlton.o
	mlton -link-opt "`pkg-config --libs-only-l gtk+-2.0`" mgtk.cm mgtk-mlton.o

realclean:
	rm -f mgtk.c mgtk.sml
	rm -f typetable.txt
