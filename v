#!/bin/bash
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
    for i in $@; do 
        if [ -f $i ]; then
            file=`basename $i`
            cp $i $BACKUPDIR/$file
            (
                cd $BACKUPDIR && git add $file \
                    && git commit -m "Backing up $file" 
            )
        fi
    done
}
# if not file is given then open the vimrc file.
if [ $# -lt 1 ]; then
    vim ~/.vimrc
else
    # check for each files. If they do not exists then search recusively for
    # them.
    files=''
    for i in $@; do
        if [ ! -f $i ]; then
            files="$files `find . -type f -name "*$i*" \
                -exec sh -c "file {} | grep text >/dev/null" \; -print | head -n 4`"
        else
            files="$files $i"
        fi
    done
    echo $files
    createBackup $files
    vim -p $files
fi
