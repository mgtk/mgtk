include ../src/Makefile.inc

MOSMLINCLUDE=-I $(LIBDIR)

OBJS= FunInput.uo valuta.uo helloworld2.uo helloworld.uo entrylabel.uo\
      fileedit.uo fileedit2.uo counter.uo comboexample.uo\
      treeexample.uo treeexample2.uo drawing.uo twowindows.uo

SUBDIRS= moside

all: tests

tests: $(OBJS)

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
FunInput.uo: FunInput.ui 
counter.uo: FunInput.ui 
