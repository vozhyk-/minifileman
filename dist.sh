#!/bin/sh

source ./dist-common.sh

echo Tarring...
tar -cvf "${archive}"\
 doc/{LICENSE,TODO}\
 icons/minifileman{,-notext}.svg\
 src/minifileman.asd\
 src/config{,-test}.lisp\
 src/ltk-ext.lisp\
 src/gui-lib.lisp\
 src/path-entry.lisp\
 src/utils.lisp\
 src/load.lisp\
 src/packages.lisp\
 src/macro-utils.lisp\
 src/pathnames.lisp\
 src/filesystem.lisp\
 src/minifileman.lisp\
 portage/app-misc/minifileman/minifileman-0.1.0.ebuild\
 dist.sh\
 darcs-dist.sh\
 dist-common.sh

dist_compress

dist_done
