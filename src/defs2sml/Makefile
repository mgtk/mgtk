# mgtk --- an SML binding for GTK.                                          
# (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       

REGRESSIONDIR=test/regression
MAIN=defs2sml$(EXEEXT)

MOSMLC  = mosmlc
PERL    = perl
MOSMLFLAGS = -liberal
MOSMAKEDIRS = . util defs xml ast codegen
MOSMAKE = mosmake
include $(MOSMAKE)/Makefile.inc

all: regression

regression: $(MAIN)
	@files=$(wildcard $(REGRESSIONDIR)/input/*.defs); \
	echo "Regression test results ... "; \
        for file in $$files ; do \
	      base=`basename $$file .defs`; \
              ./$(MAIN) $$file > $(REGRESSIONDIR)/output/$$base.out 2> /dev/null ; \
	      diff -Naur $(REGRESSIONDIR)/trusted/$$base.out $(REGRESSIONDIR)/output/$$base.out ; \
        done
