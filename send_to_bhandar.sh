#!/bin/bash

SCRIPTDIR=`dirname $0`
if [ ! "$1" ] ; then
    echo "No filename or dir given. Assuming $HOME/Work"
    echo "Usage: $0 path"
    SRC=$HOME/Work
else
    SRC="$1"
fi

pass=`gpg -d $SCRIPTDIR/shares.ncbs.res.in.gpg`
eval $pass

BACKUPDIR=$HOME/bhandar
USER=dilawars
HOST=shares.ncbs.res.in

#read -a log -p "Log: " 
#echo "${log[@]}"

## 
sshpass -e rsync -azLv "$SRC" $USER@$HOST:~/bhandar/
