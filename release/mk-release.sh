#!/bin/sh

if [ "x$1" = "x" ]; then
    echo "Usage: ./mk-release.sh <release name>"
    exit 1
fi

# Configuration

# ... directories
MGTKHOME=$HOME/mgtk
EXAMPLES=$MGTKHOME/examples
MOSIDE=$EXAMPLES/moside
DOCS=$MGTKHOME/docs
SRC=$MGTKHOME/src
TARFILE=$1.tar

cd $MGTKHOME/release

echo Generating release $1
echo

if [ -d mgtk-alpha ]; then
    echo "The directory mgtk-alpha already exists!"
    echo "Rename and restart."
    exit 1
fi

if [ -f $TARFILE.gz ]; then
    echo "Release file already exists!"
    echo "Rename and restart."
    exit 1
fi

echo Making directory structure ...
mkdir mgtk-alpha
mkdir mgtk-alpha/examples
mkdir mgtk-alpha/examples/moside
mkdir mgtk-alpha/docs

echo Copying release specific files ...
cp Make* mgtk-alpha
cp INSTALL mgtk-alpha

echo Copying global files ...
cp $MGTKHOME/ANNOUNCEMENT mgtk-alpha
cp $MGTKHOME/CONTRIB mgtk-alpha
cp $MGTKHOME/Changes mgtk-alpha
cp $MGTKHOME/LICENSE mgtk-alpha
cp $MGTKHOME/README mgtk-alpha
cp $DOCS/README mgtk-alpha/docs

echo Copying core files ...
cp $SRC/Gtk.sig mgtk-alpha
cp $SRC/Gtk.sml mgtk-alpha
cp $SRC/mgtk.c mgtk-alpha

echo Copying examples ...
cp $EXAMPLES/README mgtk-alpha/examples
cp $EXAMPLES/Makefile.w32 mgtk-alpha/examples
cp $EXAMPLES/*.sig mgtk-alpha/examples
cp $EXAMPLES/*.sml mgtk-alpha/examples
#   We have to treat the Makefile specially since it
#   refers to the ../src/Makefile.inc
#   Wont do: cp $EXAMPLES/Makefile mgtk-alpha/examples
sed 's/^include.*Makefile\.inc$/include ..\/Makefile.inc/' $EXAMPLES/Makefile > mgtk-alpha/examples/Makefile

echo Copying mosIDE ...
cp $MOSIDE/README mgtk-alpha/examples/moside
cp $MOSIDE/*.sig mgtk-alpha/examples/moside
cp $MOSIDE/*.sml mgtk-alpha/examples/moside
cp $MOSIDE/*.c mgtk-alpha/examples/moside
#   We have to treat the Makefile specially since it
#   refers to the ../src/Makefile.inc
#   Wont do: cp $MOSIDE/Makefile mgtk-alpha/examples/moside
sed 's/^include.*Makefile\.inc$/include ..\/..\/Makefile.inc/' $MOSIDE/Makefile > mgtk-alpha/examples/moside/Makefile


echo Constructing .tar.gz file ...
tar cf $TARFILE mgtk-alpha/
gzip $TARFILE
