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
              ./$(MAIN) --separate-struct -q $$file -o $(REGRESSIONDIR)/output/$$base.out ; \
	      diff -Naur $(REGRESSIONDIR)/trusted/$$base.out $(REGRESSIONDIR)/output/$$base.out ; \
        done

test-compile: $(MAIN)
	@files="$(wildcard $(REGRESSIONDIR)/input/*.defs)"; \
	echo "Regression test results ... "; \
        for file in $$files ; do \
	      base=`basename $$file .defs`; \
              ./$(MAIN) --separate-struct $$file -cp lib/preamble.c -sp lib/preamble.sml -o $(REGRESSIONDIR)/output/$$base ; \
	      mosmlc -toplevel -liberal -c -o /dev/null $(REGRESSIONDIR)/output/$$base.sml ; \
	      gcc $(CINCLUDE) -c -o /dev/null $(REGRESSIONDIR)/output/$$base.c ; \
        done


testgen-%: $(MAIN)
	./$(MAIN) $(OPTIONS) --separate-struct test/regression/input/$*.defs -cp lib/preamble.c -sp lib/preamble.sml -o $(REGRESSIONDIR)/output/$*

testcomp-%: testgen-%
	gcc $(CINCLUDE) -c -o /dev/null $(REGRESSIONDIR)/output/$*.c
	mosmlc -toplevel -liberal -c -o /dev/null $(REGRESSIONDIR)/output/$*.sml

reggen-%: $(MAIN)
	@./$(MAIN) --separate-struct -q $(OPTIONS) -o $(REGRESSIONDIR)/output/$*.out test/regression/input/$*.defs

regcomp-%: reggen-%
	@if diff -Naur $(REGRESSIONDIR)/trusted/$*.out $(REGRESSIONDIR)/output/$*.out ; then \
           echo Ok ;\
        fi

mgtk.c mgtk.sml: $(MAIN) api/pygtk/gtk.defs api/pygtk/gtk-types.defs lib/excludes.txt
	./$(MAIN) -co mgtk.c -so mgtk.sml -cp lib/preamble.c -sp lib/preamble.sml api/pygtk/gtk.defs

mgtk-mlton.c mgtk-mlton.sml: $(MAIN) api/pygtk/gtk.defs api/pygtk/gtk-types.defs lib/excludes.txt
	./$(MAIN) --mlton -co mgtk-mlton.c -so mgtk-mlton.sml -cp lib/preamble-mlton.c -sp lib/preamble-mlton.sml api/pygtk/gtk.defs 

compile: $(MAIN) mgtk.c mgtk.sml
	mosmlc -toplevel -liberal -c mgtk.sml
	gcc $(CINCLUDE) -c -o /dev/null mgtk.c > c.out 2>&1 

MLTONHOME=/usr/lib/mlton
MLTONINCLUDE=/usr/lib/mlton/include
MLTONBIN=/usr/bin/

minimgtk-mlton.c minimgtk-mlton.sml: $(MAIN) test/regression/input/minigtk.defs
	@./$(MAIN) --mlton -cp lib/preamble-mlton.c -sp lib/preamble-mlton.sml -q $(OPTIONS) -co minimgtk-mlton.c -so minimgtk-mlton.sml test/regression/input/minigtk.defs

mgtk-mlton.o: mgtk-mlton.c
	gcc -c -O2 -I$(MLTONINCLUDE) \
		   `pkg-config --cflags gtk+-2.0` mgtk-mlton.c

minimgtk-mlton.o: minimgtk-mlton.c
	gcc -c -O2 -I$(MLTONINCLUDE) \
		   `pkg-config --cflags gtk+-2.0` minimgtk-mlton.c

minimgtk-mlton: minimgtk-mlton.cm minimgtk-mlton.o \
                      $(shell mlton -stop f minimgtk-mlton.cm)
	$(MLTONBIN)/mlton -output minimgtk-mlton \
			  -link-opt "`pkg-config --libs-only-l gtk+-2.0`" \
	                  minimgtk-mlton.cm minimgtk-mlton.o


mltoncompile: $(MAIN) mgtk-mlton.sml mgtk-mlton.o
	mlton -link-opt "`pkg-config --libs-only-l gtk+-2.0`" mgtk.cm mgtk-mlton.o

realclean:
	rm -f c.out mgtk.c mgtk.sml
	rm -f typetable.txt
