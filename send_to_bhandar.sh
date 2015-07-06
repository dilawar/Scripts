#!/bin/bash

if [ ! "$1" ] ; then
    echo "No filename or dir given"
    echo "Usage: $0 path"
    exit
fi

#pass=`gpg -d ./shares.ncbs.res.in.gpg`
#eval $pass

BACKUPDIR=$HOME/bhandar
USER=dilawars
HOST=shares.ncbs.res.in

read -a log -p "Log: " 
echo "${log[@]}"

## 
#sshpass -e ssh -Y $USER@$HOST
