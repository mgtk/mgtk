# mgtk --- an SML binding for GTK.                                          
# (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004.    

MAIN=defs2sml$(EXEEXT)

MOSMLC  = mosmlc
PERL    = perl
MOSMLFLAGS = -liberal
MOSMAKEDIRS = . util util/pretty defs ast codegen
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

REGOPTS=--separate-struct -as -tl Gtk -q $(OPTIONS)
REGOPTSMOS=$(REGOPTS)
REGOPTSMLTON=--mlton $(REGOPTS)

REGCOMPOPTS=$(REGOPTS)
REGCOMPOPTSMOS=$(REGCOMPOPTS) -sp lib/preamble.sml -cp lib/preamble.c
REGCOMPOPTSMLTON=--mlton $(REGCOMPOPTS) -sp lib/preamble-mlton.sml -cp lib/preamble-mlton.c
REGRESULTS=regression.log

reggen-%: $(MAIN)
	@echo -n "[REG]          $*.defs "; \
        params=`sed -e '/@PARAMS/!d' -e 's/.*@PARAMS=\([^@]*\)@.*/\1/' $(REGIN)/$*.defs` ; \
	./$(MAIN) $(REGOPTSMOS) $$params -o $(REGOUT)/$*.out $(REGIN)/$*.defs

reggenmlton-%: $(MAIN)
	@echo -n "[REG (MLton)]  $*.defs "
	@./$(MAIN) $(REGOPTSMLTON) -o $(REGOUT)/$*-mlton.out $(REGIN)/$*.defs

regdiff-%: reggen-%
	@if diff -Naur $(DIFFOPTIONS) $(REGTRS)/$*.out $(REGOUT)/$*.out | tee -a $(REGRESULTS) ; then \
           echo ok ;\
        fi

regdiffmlton-%: reggenmlton-%
	@if diff -Naur $(DIFFOPTIONS) $(REGTRS)/$*-mlton.out $(REGOUT)/$*-mlton.out | tee -a $(REGRESULTS) ; then \
           echo ok ;\
        fi

reg-%: regdiff-% regdiffmlton-%
	@echo -n

regmosml: $(MAIN) $(patsubst %.defs,regdiff-%,$(REGFILES))
regmlton: $(MAIN) $(patsubst %.defs,regdiffmlton-%,$(REGFILES))

regclean:
	rm -f $(REGRESULTS)

regression: $(MAIN) regclean $(patsubst %.defs,regdiff-%,$(REGFILES)) $(patsubst %.defs,regdiffmlton-%,$(REGFILES))

regcompgen-%: $(MAIN)
	@echo "[GEN]          $*.defs"; \
        params=`sed -e '/@PARAMS/!d' -e 's/.*@PARAMS=\([^@]*\)@.*/\1/' $(REGIN)/$*.defs` ; \
	./$(MAIN) $(REGCOMPOPTSMOS) $$params -o $(REGDIR)/output/$* $(REGIN)/$*.defs 

regcomp-%: regcompgen-%
	@echo "[GCC]          $*.c"
	@gcc $(CINCLUDE) -c -o /dev/null $(REGOUT)/$*.c
	@echo "[MOSMLC]       $*.sml"
	@mosmlc -toplevel -liberal -c $(REGOUT)/$*.sml

regcompile: $(MAIN) $(patsubst %.defs,regcomp-%,$(REGFILES))

trusted:
	cp $(REGOUT)/*.out $(REGTRS)

regressionclean:
	rm -f $(REGRESULTS)
	rm -f $(REGOUT)/*.{out,sml,c,uo,ui}


# Generate meta data .defs files

GTKAPIXML=api/gtk-api.raw
GTKMETADATA=api/gtk.metadata
lib/overrides.defs: $(GTKAPIXML) $(GTKMETADATA) tools/metadata2defs.pl
	tools/metadata2defs.pl $(GTKAPIXML) $(GTKMETADATA) > $@

lib/metadata.defs: lib/overrides.defs

# Generate binding
RELDIR=release/mgtk

#DEFSOPTS=$(OPTIONS) --metadata-file lib/metadata.defs --separate-struct -as api/gtk.defs > defs.log 2>&1
DEFSOPTS=$(OPTIONS) --metadata-file lib/metadata.defs --separate-struct -as api/gtk.defs > defs.log 2>&1
DEFSOPTSMOSML=-co $(RELDIR)/mgtk.c -so $(RELDIR)/mgtk.sml -cp lib/preamble.c -sp lib/preamble.sml
DEFSOPTSMLTON=--mlton -co $(RELDIR)/mgtk-mlton.c -so $(RELDIR)/mgtk-mlton.sml -cp lib/preamble-mlton.c -sp lib/preamble-mlton.sml

$(RELDIR)/mgtk.c $(RELDIR)/mgtk.sml: $(MAIN) api/gtk.defs api/gtk-types.defs lib/excludes.txt lib/preamble.sml lib/preamble.c lib/overrides.defs
	./$(MAIN) $(DEFSOPTSMOSML) $(DEFSOPTS)

$(RELDIR)/mgtk-mlton.c $(RELDIR)/mgtk-mlton.sml: $(MAIN) api/gtk.defs api/gtk-types.defs lib/excludes.txt lib/preamble-mlton.sml lib/preamble-mlton.c
	./$(MAIN) $(DEFSOPTSMLTON) $(DEFSOPTS)

mosml: $(RELDIR)/mgtk.so $(RELDIR)/mgtk.uo

$(RELDIR)/mgtk.uo: $(RELDIR)/mgtk.sml
	mosmlc -toplevel -c $<

$(RELDIR)/mgtk.o: $(RELDIR)/mgtk.c
	gcc $(CINCLUDE) -o $@ -c $<

$(RELDIR)/mgtk.so: $(RELDIR)/mgtk.o
	gcc -shared -o $@ $< `pkg-config --libs gtk+-2.0`

MLTONHOME=/usr/lib/mlton
MLTONINCLUDE=/usr/lib/mlton/include
MLTONBIN=/usr/bin/

$(RELDIR)/mgtk-mlton.o: $(RELDIR)/mgtk-mlton.c
	gcc -c -O2 -I$(MLTONINCLUDE) -o $@ `pkg-config --cflags gtk+-2.0` $<

$(RELDIR)/mgtk-mlton.cm: $(shell mlton -stop f $(RELDIR)/mgtk-mlton.cm)

mlton: $(RELDIR)/mgtk-mlton.cm $(RELDIR)/mgtk-mlton.o
	mlton -link-opt "`pkg-config --libs-only-l gtk+-2.0`" $^


# Clean-up
realclean: regressionclean
	rm -f mgtk.c mgtk.sml mgtk.o mgtk
	rm -f mgtk-mlton.c mgtk-mlton.sml mgtk-mlton.o mgtk-mlton
	rm -f typetable.txt
