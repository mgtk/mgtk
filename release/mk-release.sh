#!/bin/sh

Usage () {
    exec cat << END_USAGE
Usage: ./mk-release.sh <options>

  Where options can be:
  -type <release type> ('alpha' or 'beta', for example)
  -version <version> ('0.02', for example)
  -release <suffix> ('pre2', for example)
  -o <filename> the output file (if omitted the filename is constructed
                     as 'mgtk<-release type>-<version><release>.tar.gz')
  -clean remove the directory constructed during generation

  Only -version is required.
END_USAGE
}

RELTYPE="alpha"
VERSION=""
RELEASE=""
CLEAN=""

while [ "$#" != "0" ]; do
    arg=$1; shift
    case $arg in
      -type)
         RELTYPE="$1"; TYPE="-$RELTYPE"; shift
         ;;
      -version)
         VERSION=$1; shift
         ;;
      -release)
         RELEASE=$1; shift
         ;;
      -o)
         TARFILE=$1; shift
	 ;;
      -clean)
         CLEAN="true"
	 ;;
      *)
	 Usage
         exit 1
         ;;
    esac
done

if [ "x$VERSION" = "x" ]; then
    echo "No version specified!\n"
    Usage
fi

if [ "x$TARFILE" = "x" ]; then
    TARFILE="mgtk$TYPE-$VERSION$RELEASE.tar"
else
    case $TARFILE in
      *\.tar\.gz)
         TARFILE=`echo $TARFILE | sed 's/\.tar\.gz/.tar/'`
	 ;;
      *\.tar)
	 ;;
      *)
         TARFILE="$TARFILE.tar"
	 ;;
    esac
fi

echo "Generating mGTK $RELTYPE release v. $VERSION$RELEASE."
echo "(dir= mgtk$TYPE; tar= $TARFILE)\n"

# ... directories
MGTKHOME=$HOME/mgtk
EXAMPLES=$MGTKHOME/examples
MOSIDE=$EXAMPLES/moside
DOCS=$MGTKHOME/docs
SRC=$MGTKHOME/src
TRGT=mgtk$TYPE

cd $MGTKHOME/release

if [ -d $TRGT ]; then
    echo "The directory $TRGT already exists!"
    echo "Rename and restart."
    exit 1
fi

if [ -f $TARFILE.gz ]; then
    echo "Release file already exists!"
    echo "Rename and restart."
    exit 1
fi

echo Making directory structure ...
mkdir $TRGT
mkdir $TRGT/examples
mkdir $TRGT/examples/moside
mkdir $TRGT/docs

echo Copying release specific files ...
cp Make* $TRGT
cp INSTALL $TRGT

echo Copying global files ...
cp $MGTKHOME/ANNOUNCEMENT $TRGT
cp $MGTKHOME/CONTRIB $TRGT
cp $MGTKHOME/Changes $TRGT
cp $MGTKHOME/LICENSE $TRGT
cp $MGTKHOME/README $TRGT
cp $DOCS/README $TRGT/docs

echo Copying core files ...
cp $SRC/Gtk.sig $TRGT
cp $SRC/Gtk.sml $TRGT
cp $SRC/mgtk.c $TRGT

echo Copying examples ...
cp $EXAMPLES/README $TRGT/examples
cp $EXAMPLES/Makefile.w32 $TRGT/examples
cp $EXAMPLES/*.sig $TRGT/examples
cp $EXAMPLES/*.sml $TRGT/examples
#   We have to treat the Makefile specially since it
#   refers to the ../src/Makefile.inc
#   Wont do: cp $EXAMPLES/Makefile $TRGT/examples
sed 's/^include.*Makefile\.inc$/include ..\/Makefile.inc/' $EXAMPLES/Makefile > $TRGT/examples/Makefile

echo Copying mosIDE ...
cp $MOSIDE/README $TRGT/examples/moside
cp $MOSIDE/*.sig $TRGT/examples/moside
cp $MOSIDE/*.sml $TRGT/examples/moside
cp $MOSIDE/*.c $TRGT/examples/moside
#   We have to treat the Makefile specially since it
#   refers to the ../src/Makefile.inc
#   Wont do: cp $MOSIDE/Makefile $TRGT/examples/moside
sed 's/^include.*Makefile\.inc$/include ..\/..\/Makefile.inc/' $MOSIDE/Makefile > $TRGT/examples/moside/Makefile


echo Constructing .tar.gz file ...
tar cf $TARFILE $TRGT/
gzip $TARFILE

if [ "$CLEAN" = "true" ]; then
    echo Removing directory $TRGT
    rm -rf $TRGT
fi
