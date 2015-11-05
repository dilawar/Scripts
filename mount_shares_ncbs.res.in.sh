#!/bin/bash
set -x
MOUNT_POINT=$HOME/External/shares.ncbs.res.in
if [ ! -d $MOUNT_POINT ]; then
    mkdir -p $MOUNT_POINT
fi
echo "Mouting shares.ncbs.res.in"
sshfs dilawars@shares.ncbs.res.in:/home/bhalla/dilawars $MOUNT_POINT
