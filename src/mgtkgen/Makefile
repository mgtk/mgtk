# mgtkgen --- generate wrapper code from .defs file.
# (c) Ken Friis Larsen and Henning Niss 1999, 2000.

include ../Makefile.inc

OBJS=Combinators.uo Stream.uo WSeq.uo Util.uo NameUtil.uo \
     TypeExp.uo TypeInfo.uo Parse.uo Parser.uo Lexer.uo AST.uo Translate.uo \
     ArgParse.uo Messages.uo Main.uo
PROG=mgtkgen

SUBDIRS=regression tools

all: $(PROG)

$(PROG): $(OBJS)
	$(MOSMLC) -o $(PROG) Main.uo

test: $(PROG)
	cd regression && $(MAKE) fresh

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
Combinators.uo: Combinators.ui Lexer.ui Stream.ui 
TypeExp.uo: TypeExp.ui Util.uo 
WSeq.uo: WSeq.ui 
TypeInfo.uo: TypeExp.ui NameUtil.ui WSeq.ui Util.uo 
Translate.uo: Translate.ui AST.ui TypeInfo.uo TypeExp.ui NameUtil.ui \
    WSeq.ui Util.uo 
Translate.ui: AST.ui 
Stream.uo: Stream.ui 
Parser.uo: AST.ui TypeExp.ui Lexer.ui Combinators.ui Util.uo 
Parse.uo: Lexer.ui Combinators.ui Stream.ui 
NameUtil.uo: NameUtil.ui 
Messages.uo: Messages.ui AST.ui WSeq.ui 
Messages.ui: AST.ui 
Main.uo: Parser.uo AST.ui Translate.ui Messages.ui ArgParse.ui Util.uo \
    Parse.uo 
Lexer.uo: Lexer.ui Stream.ui 
Lexer.ui: Stream.ui 
Combinators.ui: Lexer.ui 
ArgParse.uo: ArgParse.ui 
AST.uo: AST.ui TypeExp.ui Lexer.ui Util.uo 
AST.ui: TypeExp.ui Lexer.ui 
