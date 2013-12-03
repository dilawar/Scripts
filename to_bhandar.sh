#!/bin/bash
BACKUP_DIR=$HOME/Bhandar/backup
if [ -d $BACKUP_DIR ]; then 
    echo "$BACKUP_DIR does not exists.. Creating one!"
    mkdir -p $BACKUP_DIR 
fi
rsync -azv $HOME/Scripts/ $BACKUP_DIR/
rsync -azv $HOME/.vim $BACKUP_DIR/

