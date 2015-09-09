#!/bin/bash
DOWNLOAD_DIR="$1=`pwd`"
echo "Downloading to $DOWNLOAD_DIR"
(
    cd $DOWNLOAD_DIR
    wget -m -d  ftp://student:student@files.ncbs.res.in
)
