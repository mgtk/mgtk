include ../../Makefile.inc

MOSMLINCLUDE=-I ..

OBJS=defsstat.uo defslist.uo defsfind.uo
TOOLS=defsstat defslist defsfind

all: $(TOOLS)

defsstat: defsstat.uo
	$(MOSMLC) $(MOSMLINCLUDE) -o $@ $<

defslist: defslist.uo
	$(MOSMLC) $(MOSMLINCLUDE) -o $@ $<

defsfind: defsfind.uo
	$(MOSMLC) $(MOSMLINCLUDE) -o $@ $<


clean:
	$(REMOVE) *~
	$(REMOVE) *.u? 
	$(REMOVE) $(TOOLS)
	$(REMOVE) Makefile.bak

depend: 
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	camlrunm $(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	camlrunm $(MOSMLTOOLS)/mosmldep >> Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
