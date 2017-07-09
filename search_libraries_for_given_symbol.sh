#!/bin/bash
SEARCHDIR=${2:-/usr}
echo "Searching for $1 in $SEARCHDIR"
if [ -d $SEARCHDIR ]; then
    find $SEARCHDIR -name "*.so" -name "*.a" -exec nm --print-file-name --defined-only \
        --dynamic {} \; | grep "$1"
else
    echo "$SEARCHDIR is not found"
fi
