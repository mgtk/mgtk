# Debian setup (mosml packages for Debian by JP Secher)
MOSMLHOME=/usr/lib/mosml
MOSMLRUNTIME=$(MOSMLHOME)/include
MOSMLBIN=/usr/bin

# Debian setup
MLTONHOME=/usr/lib/mlton
MLTONINCLUDE=/usr/lib/mlton/include
MLTONBIN=/usr/bin/

MGTKHOME=../src/defs2sml/release/mgtk


.PHONY: all mosml mlton

all: mosml mlton

mosml: helloworld-new buttons-galore signup menus

mlton: helloworld-mlton buttons-galore-mlton signup-mlton


# mosml stuff
%: %.sml
	$(MOSMLBIN)/mosmlc -o $* -I $(MGTKHOME) mgtk.ui $*.sml


# MLton stuff
mgtk-mlton.o: mgtk-mlton.c
	gcc -c -O2 -I$(MLTONINCLUDE) `pkg-config --cflags gtk+-2.0` mgtk-mlton.c
#%-mlton: %-mlton.cm mgtk-mlton.o \
#                      $(shell mlton -stop f %-mlton.cm)
%-mlton: %-mlton.cm mgtk-mlton.o 
	$(MLTONBIN)/mlton -output $*-mlton \
			  -link-opt "`pkg-config --libs-only-l gtk+-2.0`" \
	                  $*-mlton.cm mgtk-mlton.o

# cleaning
clean:
	rm -f *.{o,so,uo,ui} *~
	rm -f helloworld-new helloworld-mlton
	rm -f buttons-galore buttons-galore-mlton
	rm -f signup signup-mlton

realclean: clean
