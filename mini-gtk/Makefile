# Ken's setup
#MOSMLHOME=/home/kfl/programming/ml/mosml
#MOSMLRUNTIME=$(MOSMLHOME)/include
#MOSMLBIN=$(MOSMLHOME)/bin

# Debian setup (mosml packages for Debian by JP Secher)
MOSMLHOME=/usr/lib/mosml
MOSMLRUNTIME=$(MOSMLHOME)/include
MOSMLBIN=/usr/bin

MLTONINCLUDE=/usr/lib/mlton/self/include

.PHONY: all mlton mosml

all: mosml mlton

mosml: helloworld-new helloworld-flat mgtk.so buttons-galore

mlton: helloworld-mlton buttons-galore-mlton


MiniGtk.uo MiniGtk.ui: MiniGtk.sml
	$(MOSMLBIN)/mosmlc -c -toplevel MiniGtk.sml

Gtk.ui: Gtk.sig
	$(MOSMLBIN)/mosmlc -c Gtk.sig

Gtk.uo: Gtk.ui Gtk.sml
	$(MOSMLBIN)/mosmlc -c Gtk.sml

helloworld-flat: Gtk.uo helloworld-flat.sml
	$(MOSMLBIN)/mosmlc -o helloworld-flat helloworld-flat.sml

helloworld-new: helloworld-new.sml MiniGtk.ui MiniGtk.uo
	$(MOSMLBIN)/mosmlc -o helloworld-new MiniGtk.ui helloworld-new.sml

mosml-interf: mosml-interf.sml MiniGtk.ui MiniGtk.uo
	$(MOSMLBIN)/mosmlc -o mosml-interf MiniGtk.ui mosml-interf.sml


mgtk.so: mgtk.o
#	gcc -shared -o mgtk.so mgtk.o `gtk-config --libs`
	gcc -shared -o mgtk.so mgtk.o `pkg-config --libs gtk+-2.0`

mgtk.o: mgtk.c
#	gcc -c -O3 -fPIC -Wall -Dunix -I$(MOSMLRUNTIME) `gtk-config --cflags` mgtk.c 
	gcc -c -O3 -fPIC -Wall -Dunix -I$(MOSMLRUNTIME) \
	                              `pkg-config --cflags gtk+-2.0` mgtk.c 


# MLton stuff
mgtk-mlton.o: mgtk-mlton.c
	gcc -c -O2 -I$(MLTONINCLUDE) \
		   `pkg-config --cflags gtk+-2.0` mgtk-mlton.c


buttons-galore-mlton: buttons-galore-mlton.cm mgtk-mlton.o \
                      $(shell mlton -stop f buttons-galore-mlton.cm)
	mlton -output buttons-galore-mlton -verbose 2 buttons-galore-mlton.cm \
	      mgtk-mlton.o `pkg-config --libs gtk+-2.0`


helloworld-mlton: helloworld-mlton.cm mgtk-mlton.o \
                  $(shell mlton -stop f helloworld-mlton.cm)
	mlton -output helloworld-mlton -verbose 2 helloworld-mlton.cm \
	      mgtk-mlton.o `pkg-config --libs gtk+-2.0`






clean:
	rm *.{o,so,uo,ui} helloworld-mlton buttons-galore-mlton
