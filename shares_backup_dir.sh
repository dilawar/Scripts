#!/bin/bash - 
#===============================================================================
#
#          FILE: shares_backup_dir.sh
# 
#         USAGE: ./shares_backup_dir.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 07/09/2016 01:59:51 PM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
if [[ $# -lt 1 ]]; then
    echo "USAGE: $0 dir_to_backup"
    exit
fi
DIRNAME=`readlink -f $1`
sshpass -e rsync -azv --progress $DIRNAME dilawars@shares.ncbs.res.in:~
notify-send "Done backing up $DIRNAME"
