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
              ./$(MAIN) -q $$file -o $(REGRESSIONDIR)/output/$$base.out ; \
	      diff -Naur $(REGRESSIONDIR)/trusted/$$base.out $(REGRESSIONDIR)/output/$$base.out ; \
        done

test-compile: $(MAIN)
	@files="$(wildcard $(REGRESSIONDIR)/input/*.defs)"; \
	echo "Regression test results ... "; \
        for file in $$files ; do \
	      base=`basename $$file .defs`; \
              ./$(MAIN) $$file -cp lib/preamble.c -sp lib/preamble.sml -o $(REGRESSIONDIR)/output/$$base ; \
	      mosmlc -toplevel -liberal -c -o /dev/null $(REGRESSIONDIR)/output/$$base.sml ; \
	      gcc $(CINCLUDE) -c -o /dev/null $(REGRESSIONDIR)/output/$$base.c ; \
        done


testgen-%: $(MAIN)
	./$(MAIN) $(OPTIONS) test/regression/input/$*.defs -cp lib/preamble.c -sp lib/preamble.sml -o $(REGRESSIONDIR)/output/$*

testcomp-%: testgen-%
	gcc $(CINCLUDE) -c -o /dev/null $(REGRESSIONDIR)/output/$*.c
	mosmlc -toplevel -liberal -c -o /dev/null $(REGRESSIONDIR)/output/$*.sml

reggen-%: $(MAIN)
	@./$(MAIN) -q $(OPTIONS) -o $(REGRESSIONDIR)/output/$*.out test/regression/input/$*.defs

regcomp-%: reggen-%
	@if diff -Naur $(REGRESSIONDIR)/trusted/$*.out $(REGRESSIONDIR)/output/$*.out ; then \
           echo Ok ;\
        fi

mgtk.c mgtk.sml: $(MAIN) api/pygtk/gtk.defs api/pygtk/gtk-types.defs lib/excludes.txt
	./$(MAIN) -co mgtk.c -so mgtk.sml -cp lib/preamble.c -sp lib/preamble.sml api/pygtk/gtk.defs 

compile: $(MAIN) mgtk.c mgtk.sml
	mosmlc -toplevel -liberal -c mgtk.sml
	gcc $(CINCLUDE) -c -o /dev/null mgtk.c > c.out 2>&1 

realclean:
	rm -f c.out mgtk.c mgtk.sml
	rm -f typetable.txt
