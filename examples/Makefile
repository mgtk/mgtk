include ../src/Makefile.inc

MOSMLINCLUDE=-I $(LIBDIR)

OBJS= FunInput.uo valuta.uo helloworld2.uo helloworld.uo entrylabel.uo\
      fileview.uo fileedit.uo fileedit2.uo counter.uo comboexample.uo\
      treeexample.uo

all: tests

tests: $(OBJS)

clean:
	$(REMOVE) *~
	$(REMOVE) *.u? 
	$(REMOVE) Makefile.bak

depend: 
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(CAMLRUNM) $(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(CAMLRUNM) $(MOSMLTOOLS)/mosmldep >> Makefile


### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
FunInput.uo: FunInput.ui 
counter.uo: FunInput.ui 
