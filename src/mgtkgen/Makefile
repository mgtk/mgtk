# defs2sml --- generate wrapper code from .defs file.
# (c) Ken Friis Larsen and Henning Niss 1999, 2000.

include ../Makefile.inc

OBJS=Parsercomb.uo WSeq.uo Util.uo NameUtil.uo ArgParse.uo \
     TypeExp.uo TypeInfo.uo Parse.uo ParseUtils.uo Parser.uo \
     AST.uo Translate.uo Messages.uo State.uo Main.uo

SRCS=Parsercomb.sig Parsercomb.sml WSeq.sig WSeq.sml Util.sml \
     NameUtil.sig NameUtil.sml ArgParse.sig ArgParse.sml \
     TypeExp.sig TypeExp.sml TypeInfo.sig TypeInfo.sml Parse.sml \
     ParseUtils.sig ParseUtils.sml Parser.sml \
     AST.sig AST.sml Translate.sig Translate.sml \
     Messages.sig Messages.sml State.sml Main.sml

SUBDIRS=regression tools

all: $(GENPROG)

#$(GENPROG): $(OBJS)
#	$(MOSMLC) -o $(GENPROG) Main.uo

$(GENPROG): $(SRCS)
	$(MOSMLPM) $(GENPROG).pm

test: $(GENPROG)
	cd regression && $(MAKE) fresh

quicktest: $(GENPROG)
	cd regression && $(MAKE) clean && $(MAKE) quick

clean:
	$(REMOVE) *~
	$(REMOVE) *.u? 
	$(REMOVE) Makefile.bak

realclean: clean
	@dirs='$(SUBDIRS)'; for dir in $$dirs; do \
	   cd $$dir; \
	   $(MAKE) realclean; \
           cd ..; \
        done

depend: 
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(CAMLRUNM) $(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(CAMLRUNM) $(MOSMLTOOLS)/mosmldep >> Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
