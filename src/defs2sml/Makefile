# mgtk --- an SML binding for GTK.                                          
# (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004.    

REGRESSIONDIR=test/regression
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

regression: $(MAIN)
	@files="$(wildcard $(REGRESSIONDIR)/input/*.defs)"; \
	echo "Regression test results ... "; \
        for file in $$files ; do \
	      base=`basename $$file .defs`; \
              ./$(MAIN) $$file -o $(REGRESSIONDIR)/output/$$base.out ; \
	      diff -Naur $(REGRESSIONDIR)/trusted/$$base.out $(REGRESSIONDIR)/output/$$base.out ; \
        done

test-compile: $(MAIN)
	@files="$(wildcard $(REGRESSIONDIR)/input/*.defs)"; \
	echo "Regression test results ... "; \
        for file in $$files ; do \
	      base=`basename $$file .defs`; \
              ./$(MAIN) $$file -cp lib/preamble.c -o $(REGRESSIONDIR)/output/$$base ; \
	      gcc $(CINCLUDE) -c -o /dev/null $(REGRESSIONDIR)/output/$$base.c ; \
        done


mgtk.c: $(MAIN) api/pygtk/gtk.defs api/pygtk/gtk-types.defs
	./$(MAIN) -co mgtk.c -so mGtk.sml -cp lib/preamble.c api/pygtk/gtk.defs 

ctest: $(MAIN) mgtk.c
	gcc $(CINCLUDE) -c -o /dev/null mgtk.c > c.out 2>&1 