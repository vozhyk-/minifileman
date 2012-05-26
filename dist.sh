#!/bin/bash

if [[ "$1" == "DEV" ]]
then
    suffix="dev`date +%Y%m%d.%H.%M`"
else
    suffix="$1"
fi
archive="rest/dist/minifileman-${suffix}.tar"

if [[ "$2" == "" ]]
then
    format="bz2"
else
    format="$2"
fi
case "$format" in
    gz)
	compressor=gzip
	;;
    bz2)
	compressor=bzip2
	;;
    lzma)
	compressor=lzma
	;;
    *)
	echo The compression format $format isn\'t supported.
	exit 1
esac

echo Creating distribution tarball ${archive}.${format}...

echo Tarring...
tar -cvf ${archive}\
 doc/{LICENSE,TODO}\
 icons/minifileman{,-notext}.svg\
 src/config{,-test}.lisp\
 src/ltk-ext.lisp\
 src/gui-lib.lisp\
 src/helpers.lisp\
 src/load.lisp\
 src/packages.lisp\
 src/macro-helpers.lisp\
 src/pathnames.lisp\
 src/filesystem.lisp\
 src/minifileman.lisp\
 dist.sh

echo Compressing...
${compressor} -9v ${archive}

echo Done.
