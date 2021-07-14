#!/usr/bin/env bash

set -e

VERSION=7.13.3
MACHINE=`uname -m`
FILENAME="filebeat-${VERSION}-linux-$MACHINE.tar.gz"

DESTDIR="$HOME/.local/opt"
mkdir -p "$DESTDIR"

BUILDDIR=/tmp

(
    cd $BUILDDIR

    if [ ! -f "$BUILDDIR/$FILENAME" ]; then
        curl -L -O "https://artifacts.elastic.co/downloads/beats/filebeat/$FILENAME"
    else
        echo "$BUILDDIR/$FILENAME already exists. Unarchiving..."
    fi

    tar xzvf "$BUILDDIR/$FILENAME" -C "$DESTDIR"

    echo "Unarchived into $DESTDIR."
)
