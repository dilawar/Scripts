#!/bin/bash
BACKUPDIR=$HOME/.backup
if [ ! -d $BACKUPDIR ]; then
    mkdir $BACKUPDIR 
    git init
fi
# Read the file and add the to backup
CURDIR=$(pwd)

for i in $@; do 
    if [ -f $i ]; then
        cp $i $BACKUPDIR/
        (
            cd $BACKUPDIR && git add $i && git commit -m "Backup" 1&> /dev/null &
        )
    fi
done
if [ $# -lt 1 ]; then
    vim ~/.vimrc
fi
vim $@
