MOSMLRUNTIME=/home/kfl/utils/mosml2.0/include

all: MiniGtk.uo MiniGtk.ui mgtk.so

MiniGtk.uo MiniGtk.ui: MiniGtk.sml
	mosmlc -c -toplevel MiniGtk.sml

mgtk.so: mgtk.o
	gcc -shared -o mgtk.so mgtk.o `gtk-config --libs`

mgtk.o: mgtk.c
	gcc -c -O3 -fPIC -Wall -Dunix -I$(MOSMLRUNTIME) `gtk-config --cflags` mgtk.c 
