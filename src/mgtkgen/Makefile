# defs2sml --- generate wrapper code from .defs file.
# (c) Ken Friis Larsen and Henning Niss 1999, 2000.

include ../Makefile.inc

OBJS=Parsercomb.uo WSeq.uo Util.uo NameUtil.uo ArgParse.uo \
     TypeExp.uo TypeInfo.uo Parse.uo ParseUtils.uo Parser.uo \
     AST.uo Translate.uo Messages.uo State.uo Main.uo

SUBDIRS=regression tools

all: $(GENPROG)

$(GENPROG): $(OBJS)
	$(MOSMLC) -o $(GENPROG) Main.uo

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
Translate.ui: AST.ui State.uo 
TypeInfo.ui: TypeExp.ui NameUtil.ui WSeq.ui 
TypeInfo.uo: TypeInfo.ui WSeq.ui Util.uo 
TypeExp.uo: TypeExp.ui Util.uo 
WSeq.uo: WSeq.ui 
ArgParse.uo: ArgParse.ui 
ParseUtils.uo: ParseUtils.ui AST.ui TypeExp.ui NameUtil.ui Util.uo 
ParseUtils.ui: AST.ui TypeExp.ui NameUtil.ui 
Parsercomb.uo: Parsercomb.ui 
Parser.uo: Parsercomb.ui 
NameUtil.uo: NameUtil.ui WSeq.ui Util.uo 
TypeExp.ui: NameUtil.ui 
NameUtil.ui: WSeq.ui 
Messages.uo: Messages.ui State.uo WSeq.ui 
Parse.uo: Parsercomb.ui 
Messages.ui: State.uo 
Main.uo: Parser.uo Translate.ui State.uo Messages.ui ArgParse.ui Util.uo \
    Parse.uo 
AST.uo: AST.ui Util.uo 
Translate.uo: Translate.ui State.uo WSeq.ui 
AST.ui: TypeExp.ui NameUtil.ui 
