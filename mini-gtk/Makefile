# Ken's setup
#MOSMLHOME=/home/kfl/programming/ml/mosml
#MOSMLRUNTIME=$(MOSMLHOME)/include
#MOSMLBIN=$(MOSMLHOME)/bin

# Debian setup (mosml packages for Debian by JP Secher)
MOSMLHOME=/usr/lib/mosml
MOSMLRUNTIME=$(MOSMLHOME)/include
MOSMLBIN=/usr/bin

all: helloworld-new helloworld-flat mgtk.so mosml-interf

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

clean:
	rm *.{o,so,uo,ui}
