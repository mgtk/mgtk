# mgtkgen --- generate wrapper code from .defs file.
# (c) Ken Friis Larsen and Henning Niss 1999, 2000.

include ../Makefile.inc

OBJS=Parsercomb.uo WSeq.uo Util.uo NameUtil.uo ArgParse.uo \
     TypeExp.uo TypeInfo.uo Parse.uo Parser.uo AST.uo Translate.uo \
     Messages.uo Main.uo
PROG=mgtkgen

SUBDIRS=regression tools

all: $(PROG)

$(PROG): $(OBJS)
	$(MOSMLC) -o $(PROG) Main.uo

test: $(PROG)
	cd regression && $(MAKE) fresh

quicktest: $(PROG)
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
Translate.ui: AST.ui 
TypeInfo.uo: WSeq.ui 
TypeExp.uo: TypeExp.ui Util.uo 
WSeq.uo: WSeq.ui 
ArgParse.uo: ArgParse.ui 
Parsercomb.uo: Parsercomb.ui 
Parser.uo: AST.ui Util.uo Parsercomb.ui 
NameUtil.uo: NameUtil.ui 
Messages.uo: Messages.ui AST.ui WSeq.ui 
Parse.uo: Parsercomb.ui 
Messages.ui: AST.ui 
Main.uo: Parser.uo AST.ui Translate.ui Messages.ui ArgParse.ui Util.uo \
    Parse.uo 
AST.uo: AST.ui TypeExp.ui Util.uo 
Translate.uo: Translate.ui WSeq.ui 
AST.ui: TypeExp.ui 
