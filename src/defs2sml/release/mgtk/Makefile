# Debian setup (mosml packages for Debian by JP Secher)
MOSMLHOME=/usr/lib/mosml
MOSMLRUNTIME=$(MOSMLHOME)/include
MOSMLBIN=/usr/bin

# Debian setup
MLTONHOME=/usr/lib/mlton
MLTONINCLUDE=/usr/lib/mlton/include
MLTONBIN=/usr/bin/

.PHONY: all mosml mlton

all: mosml mlton

mosml: helloworld-new buttons-galore signup mgtk.so

mlton: helloworld-mlton buttons-galore-mlton signup-mlton


# mosml stuff
mgtk.uo mgtk.ui: mgtk.sml
	$(MOSMLBIN)/mosmlc -c -toplevel mgtk.sml

mgtk.so: mgtk.o
	gcc -shared -o mgtk.so mgtk.o `pkg-config --libs gtk+-2.0`

mgtk.o: mgtk.c
	gcc -c -O3 -fPIC -Wall -Dunix -I$(MOSMLRUNTIME) \
	                              `pkg-config --cflags gtk+-2.0` mgtk.c 

%: %.sml mgtk.ui mgtk.uo
	$(MOSMLBIN)/mosmlc -o $* mgtk.ui $*.sml


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

realclean: clean
