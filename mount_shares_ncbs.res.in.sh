#!/bin/bash
set -x
MOUNT_POINT=$HOME/External/shares.ncbs.res.in
if [ ! -d $MOUNT_POINT ]; then
    mkdir -p $MOUNT_POINT
fi
echo "Mouting shares.ncbs.res.in"
#sshfs dilawars@shares.ncbs.res.in:/home/bhalla/dilawars $MOUNT_POINT
mount -t cifs //shares.ncbs.res.in/dilawars -o vers=3.0,rw,uid=50066,gid=50009,username=dilawars, $HOME/shares.ncbs.res.in

