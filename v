#!/bin/bash
FILES=""
OPTIONS=""

# If it's not a file, parse it as a option (all new files are OPTIONS)
for i in $FILES; do
    if [ -f $i ]; then
        FILES="$FILES $i"
    else
        OPTIONS="$OPTIONS $i"
    fi
done

BACKUPDIR=$HOME/.backup
if [ ! -d $BACKUPDIR ]; then
    mkdir $BACKUPDIR 
    git init
fi
# Read the file and add the to backup
CURDIR=$(pwd)

# Whatever list of files is send to this function, it takes it and creates their
# backup.
function createBackup 
{
    for file in "$@"; do 
        if [ -f $file ]; then
            cp $file --parents $BACKUPDIR
            (
                cd $BACKUPDIR && git add $file \
                    && git commit -m "Backing up $file on `date`" 
            )
        fi
    done
}
# check for each files. If they do not exists then search recusively for
# them.
createBackup $FILES
vim $OPTIONS $FILES
