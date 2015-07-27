#!/bin/bash

# This script uses mairix to search and index and then mutt to browse those
# emails. We assume that mairix has been configured beforehand.
MAIRIX=`which mairix`
MAIRIX_DIR=$HOME/Mail/mfolder
if [ ! $MAIRIX ]; then
    echo "mairix is not found."
    exit
fi

if [ ! -d $MAIRIX_DIR ]; then
    echo "Did you setup mairix"
    echo "See the man page of mairixrc"
    mkdir -p $MAIRIX_DIR
fi

$MAIRIX $@
mutt -f ~/Mail/mfolder
