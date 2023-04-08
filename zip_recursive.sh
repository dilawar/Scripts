#!/usr/bin/env bash
set -e
set -x
# ZIP the given directory.
DIRPATH=`realpath ${1-.}`
DIRNAME=$(basename $(realpath $DIRPATH))
ZIPNAME=${DIRPATH}.zip
zip -9 -r $ZIPNAME $DIRNAME -x *.git* -x *.svn* -x *.DS_Store*
echo "Created $ZIPNAME"
ls -lh $ZIPNAME
