# mgtkgen --- generate wrapper code from .defs file.
# (c) Ken Friis Larsen and Henning Niss 1999, 2000.

include ../Makefile.inc

OBJS=Combinators.uo Stream.uo WSeq.uo Util.uo NameUtil.uo \
     TypeInfo.uo Parse.uo Parser.uo Lexer.uo AST.uo Translate.uo \
     ArgParse.uo Messages.uo Main.uo
PROG=mgtkgen

all: $(PROG)

$(PROG): $(OBJS)
	$(MOSMLC) -g -o $(PROG) Main.uo

test: $(PROG)
	cd regression && $(MAKE)

clean:
	$(REMOVE) *~
	$(REMOVE) *.u? 
	$(REMOVE) Makefile.bak

realclean: clean
	cd regression && $(MAKE) clean

depend: 
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(CAMLRUNM) $(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(CAMLRUNM) $(MOSMLTOOLS)/mosmldep >> Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
Translate.ui: AST.ui 
TypeInfo.uo: AST.ui NameUtil.ui WSeq.ui Util.uo 
WSeq.uo: WSeq.ui 
ArgParse.uo: ArgParse.ui 
Combinators.uo: Combinators.ui Lexer.ui 
Combinators.ui: Lexer.ui 
Stream.uo: Stream.ui 
Parser.uo: AST.ui Lexer.ui Combinators.ui 
Parse.uo: Lexer.ui Combinators.ui Stream.ui 
Main.uo: Parser.uo AST.ui Translate.ui Messages.ui ArgParse.ui Util.uo \
    Parse.uo 
NameUtil.uo: NameUtil.ui 
Messages.uo: Messages.ui AST.ui WSeq.ui 
Messages.ui: AST.ui 
Lexer.uo: Lexer.ui Stream.ui 
Lexer.ui: Stream.ui 
Translate.uo: Translate.ui AST.ui TypeInfo.uo NameUtil.ui WSeq.ui Util.uo 
AST.uo: AST.ui Lexer.ui 
AST.ui: Lexer.ui 
