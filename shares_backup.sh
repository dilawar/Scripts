#!/bin/bash

START=$(date +%s)
outfile=/tmp/"Backup_$(date '+%A,%d%B%Y,%T')"
sshpass -e rsync -ab --progress --delete --backup-dir=old_`date +%F` --delete \
    --exclude=old_* ~/Work dilawars@shares.ncbs.res.in:~
FINISH=$(date +%s)
echo "total time: $(( ($FINISH-$START) / 60 )) minutes, $(( ($FINISH-$START) % 60 )) seconds"
notify-send "Done backing up Work to shares.ncbs.res.in"
