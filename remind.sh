#!/usr/bin/env bash

LOCKFILE=/tmp/_remind.lock
if [ `flock -xn $LOCKFILE -c 'echo 1'` ]; then
    echo "Starting remind service"
else
   echo -n 'its already running with PID'
   cat $LOCKFILE
   exit 1 
fi

# if gcalcli is not found. do nothing.


if ! [ -x "$(command -v gcalcli)" ]; then
    echo "gcalcli is not installed."
    exit 1;
fi

while true; do
    gcalcli --calendar="dilawar" remind
    sleep 300
done &
