include ../../Makefile.inc

MOSMLINCLUDE=-I .. -I ../..

OBJS=defsstat.uo defslist.uo defsfind.uo defsdiff.uo defscheck.uo
TOOLS=defsstat defslist defsfind defsdiff defscheck

all: $(TOOLS)

defsstat: defsstat.uo
	$(MOSMLC) $(MOSMLINCLUDE) -o $@ $<

defslist: defslist.uo
	$(MOSMLC) $(MOSMLINCLUDE) -o $@ $<

defscheck: defscheck.uo
	$(MOSMLC) $(MOSMLINCLUDE) -o $@ $<

defsfind: defsfind.uo
	$(MOSMLC) $(MOSMLINCLUDE) -o $@ $<

defsdiff: defsdiff.uo
	$(MOSMLC) $(MOSMLINCLUDE) -o $@ $<

clean:
	$(REMOVE) *~
	$(REMOVE) *.u? 
	$(REMOVE) $(TOOLS)
	$(REMOVE) Makefile.bak

realclean: clean

depend: 
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(CAMLRUNM) $(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(CAMLRUNM) $(MOSMLTOOLS)/mosmldep >> Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
