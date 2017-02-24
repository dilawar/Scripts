#!/bin/bash - 
#===============================================================================
#
#          FILE: svn_sync_remote_with_local.sh
# 
#         USAGE: ./svn_sync_remote_with_local.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 01/10/2017 05:53:42 PM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
if [ $# != 1 ]
then
    echo  "usage: $0 DIR"
    exit 0
fi

ROOT=$1

for i in `find ${ROOT} -type d \! -path "*.svn*" `
do

    echo
    echo "--------------------------"
    ( cd $i ; 
    echo $i
    echo "--------------------------"


    svn status | awk '  
    /^[!]/ { system("svn rm " $2) }
    /^[?]/ { system("svn add " $2) }
    '
    )
    echo

done
