MOSMLRUNTIME=/home/kfl/utils/mosml2.0/include

all: helloworld-new helloworld-flat mgtk.so

MiniGtk.uo MiniGtk.ui: MiniGtk.sml
	mosmlc -c -toplevel MiniGtk.sml

Gtk.ui: Gtk.sig
	mosmlc -c Gtk.sig

Gtk.uo: Gtk.ui Gtk.sml
	mosmlc -c Gtk.sml

helloworld-flat: Gtk.uo helloworld-flat.sml
	mosmlc -o helloworld-flat helloworld-flat.sml

helloworld-new: Gtk.uo helloworld-new.sml MiniGtk.ui MiniGtk.uo
	mosmlc -o helloworld-new MiniGtk.ui helloworld-new.sml


mgtk.so: mgtk.o
	gcc -shared -o mgtk.so mgtk.o `gtk-config --libs`

mgtk.o: mgtk.c
	gcc -c -O3 -fPIC -Wall -Dunix -I$(MOSMLRUNTIME) `gtk-config --cflags` mgtk.c 
